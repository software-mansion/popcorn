/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */
use std::collections::HashMap;

use log::debug;
use log::trace;
use rand::prelude::IteratorRandom;

use crate::ast::FunctionDeclaration;
use crate::ast::Module;
use crate::core_types::*;
use crate::environment::CallLocality::*;
use crate::environment::Determinism::*;
use crate::environment::GuardContext::*;
use crate::environment::MultiScopeKind::*;
use crate::environment::ReuseSafety::*;
use crate::environment::Scope::*;
use crate::environment::ScopeClosureBehavior::*;
use crate::environment::Shadowing::*;
use crate::stdlib::*;
use crate::types::*;

/* Erlang's scoping rules are maddeningly inconsistent. For examples:
 * - Patterns are matched in parallel
 *   Except for binaries that are matched left to right
 *     Except that the size of each element is computed before matching that element
 * - Using a bound-variable in a pattern results in a check that they are equal
 *     Except on the left side of "<-" in a generator for a comprehension, where it just shadows the previous definition
 * - A variable that is defined in all branches of a case can be used after that case
 *     But a variable that is only used in some branches can never again be mentioned in that function clause
 *       Except obviously in a construct that allows shadowing
 * - A variable defined in a list comprehension is not defined after it, and can be freely used
 * - A variable defined in a catch expression is not defined after it, and it is an error to use it
 *  (like if it had been defined in some branches of a case)
 * - A variable defined on the right side of a "<-" in a comprehension's generator can't be used in that comprehension's head expression
 *     But a variable defined in a filter in a comprehension can.
 *
 * This inconsistency is a major problem for erlfuzz, as it aims to be able to generate any valid Erlang program,
 *   without ever generating an invalid one.
 * This file is responsible for the machinery that keeps track of which variables are live, shadowed, unusable, etc..
 * In order to keep things readable and make it bearable to add support for new language constructs in erlfuzz, I picked a modular approach,
 *   where the generator can open a new scope at will, and specify the behavior of each scope by passing special arguments to open_scope and close_scope.
 * Additionally, some scopes can actually be collections of scopes executed in parallel, and these have an additional function "shift_to_sibling" to
 *   move to the next sub-scope (which can also be customized).
 *
 * Here is a summary of the arguments passed to these functions per language construct, the actual calls can be found through generator.rs
 *   Catch and right-side of andalso/orelse:
 *     open: Single, NoShadowing
 *     close: Discard(NotSafeToReuse)
 *   Comprehension generator expression:
 *     open: Single, NoShadowing
 *     close: Discard(SafeToReuse)
 *   Comprehension generator pattern:
 *     open: Single, Shadowing
 *     close: Overwrite
 *   Multi patterns (e.g. tuple pattern, map pattern):
 *     open: Multi(Pattern), NoShadowing
 *     shift: SafeToReuse
 *     close: KeepUnion
 *   Cases:
 *     open: Multi(Expr), NoShadowing
 *     shift: SafeToReuse
 *     close: KeepIntersection
 *   Multi expressions (e.g. tuple, function call):
 *     open: Multi(Expr), NoShadowing
 *     shift: NotSafeToReuse
 *     close: KeepUnion
 */

#[derive(Copy, Clone, Debug)]
pub enum ScopeKind {
    Single,
    Multi(MultiScopeKind),
}
#[derive(Copy, Clone, Debug)]
pub enum MultiScopeKind {
    Expr,
    Pattern,
}
#[derive(Copy, Clone, Debug)]
pub enum Shadowing {
    Shadowing,
    NoShadowing,
}
#[derive(Copy, Clone, Debug)]
pub enum ReuseSafety {
    SafeToReuse,
    NotSafeToReuse,
}
#[derive(Copy, Clone, Debug)]
pub enum ScopeClosureBehavior {
    Overwrite,
    KeepUnion,
    KeepIntersection,
    Discard(ReuseSafety),
}

type TypedVarSet = Vec<(VarNum, TypeApproximation)>;

#[derive(Clone, Debug)]
struct ScopeElement {
    vars: TypedVarSet,
    next_available: VarNum,
}
impl ScopeElement {
    fn push(&mut self, v: VarNum, t: TypeApproximation) {
        match self.vars.last_mut() {
            Some((v_old, t_old)) if *v_old == v => {
                trace!(
                    "push (refine case): v={}, next_available={}",
                    v, self.next_available,
                );
                t_old.refine(&t);
                return;
            }
            Some((v_old, _)) => {
                trace!(
                    "push: v_old={}, v={}, next_available={}",
                    *v_old, v, self.next_available
                );
                assert!(*v_old < v);
            }
            None => trace!(
                "push(empty case): v={}, next_available={}",
                v, self.next_available
            ),
        }
        assert!(v >= self.next_available);
        self.vars.push((v, t));
        self.next_available = v + 1;
    }
}

#[derive(Clone, Debug)]
enum Scope {
    Single {
        element: ScopeElement,
        scope_counter: usize,
    },
    Multi {
        kind: MultiScopeKind,
        elements: Vec<ScopeElement>,
        available_for_sibling: VarNum,
        scope_counter: usize,
    },
}
impl Scope {
    fn active_element(&self) -> &ScopeElement {
        match self {
            Single { element, .. } => element,
            Multi { elements, .. } => elements.last().unwrap(),
        }
    }
    fn active_element_mut(&mut self) -> &mut ScopeElement {
        match self {
            Single { element, .. } => element,
            Multi { elements, .. } => elements.last_mut().unwrap(),
        }
    }
    fn next_available(&self) -> VarNum {
        self.active_element().next_available
    }
    fn mark_unsafe_until(&mut self, next_safe: VarNum) {
        make_max(&mut self.active_element_mut().next_available, next_safe);
    }
    fn shadow(&mut self, new_vars: &[(VarNum, TypeApproximation)]) {
        match self {
            Multi { .. } => unreachable!(),
            Single { element, .. } => {
                // FIXME: this could be done more efficiently.
                let mut map = new_vars
                    .iter()
                    .cloned()
                    .collect::<HashMap<VarNum, TypeApproximation>>();
                for (v, t) in &element.vars {
                    let _ = map.entry(*v).or_insert(t.clone());
                }
                element.vars = map.into_iter().collect();
                element.vars.sort_by(|(v1, _t1), (v2, _t2)| v1.cmp(v2));
            }
        }
    }
    fn element_from_single_mut(&mut self) -> &mut ScopeElement {
        match self {
            Single { element, .. } => element,
            _ => unreachable!(),
        }
    }
    fn elements_from_multi_mut(&mut self) -> &mut Vec<ScopeElement> {
        match self {
            Multi { elements, .. } => elements,
            _ => unreachable!(),
        }
    }
    fn scope_counter(&self) -> usize {
        match self {
            Single { scope_counter, .. } => *scope_counter,
            Multi { scope_counter, .. } => *scope_counter,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    scopes: Vec<Scope>,
    scope_counter: usize,
    funs: Vec<FunctionInformation>,
    pub disable_shadowing: bool,
}
impl Environment {
    pub fn new(m: &Module, disable_shadowing: bool) -> Self {
        let mut funs = m
            .functions
            .iter()
            .map(
                |FunctionDeclaration {
                     name,
                     function_type,
                     ..
                 }| FunctionInformation {
                    name: name.to_string(),
                    module_name: "?MODULE".to_string(),
                    t: function_type.clone(),
                    determinism: DeterministicOnly,
                    guard_context: NotInGuard,
                    call_locality: Local,
                },
            )
            .collect::<Vec<_>>();
        get_erlang_functions().iter().for_each(
            |(name, determinism, guard_context, call_locality, return_type, arg_types)| {
                funs.push(FunctionInformation {
                    module_name: "erlang".to_string(),
                    name: name.to_string(),
                    determinism: *determinism,
                    guard_context: *guard_context,
                    call_locality: *call_locality,
                    t: FunctionTypeApproximation {
                        return_type: return_type.clone(),
                        arg_types: arg_types.to_vec(),
                    },
                })
            },
        );
        get_list_functions()
            .iter()
            .for_each(|(name, return_type, arg_types)| {
                funs.push(FunctionInformation {
                    module_name: "list".to_string(),
                    name: name.to_string(),
                    determinism: DeterministicOnly,
                    guard_context: NotInGuard,
                    call_locality: Remote,
                    t: FunctionTypeApproximation {
                        return_type: return_type.clone(),
                        arg_types: arg_types.to_vec(),
                    },
                })
            });
        get_ets_functions()
            .iter()
            .for_each(|(name, determinism, return_type, arg_types)| {
                funs.push(FunctionInformation {
                    module_name: "ets".to_string(),
                    name: name.to_string(),
                    determinism: *determinism,
                    guard_context: NotInGuard,
                    call_locality: Remote,
                    t: FunctionTypeApproximation {
                        return_type: return_type.clone(),
                        arg_types: arg_types.to_vec(),
                    },
                })
            });
        Environment {
            scopes: vec![Scope::Single {
                element: ScopeElement {
                    vars: Vec::new(),
                    next_available: 0,
                },
                scope_counter: 0,
            }],
            scope_counter: 0,
            funs,
            disable_shadowing,
        }
    }
    fn pick_bound_var_max_depth<RngType: rand::Rng>(
        &self,
        rng: &mut RngType,
        wanted_type: &TypeApproximation,
        max_depth: usize,
    ) -> Option<(VarNum, TypeApproximation)> {
        let mut result = None;
        let mut depth = max_depth;
        while depth > 0 {
            let scope = &self.scopes[depth - 1];
            result = scope
                .active_element()
                .vars
                .iter()
                .filter(|(_v, t)| t.is_subtype_of(wanted_type))
                .choose(rng)
                .cloned();
            if result.is_some() && rng.gen::<bool>() {
                return result;
            }
            depth -= 1;
        }
        // FIXME: the type may be wrong if the variable is shadowed
        // probably not very urgent to fix, I don't think that the types are sufficiently precise for it to matter
        result
    }
    pub fn pick_bound_var<RngType: rand::Rng>(
        &self,
        rng: &mut RngType,
        wanted_type: &TypeApproximation,
    ) -> Option<(VarNum, TypeApproximation)> {
        self.pick_bound_var_max_depth(rng, wanted_type, self.scopes.len())
    }
    pub fn pick_used_var_for_pattern<RngType: rand::Rng>(
        &self,
        rng: &mut RngType,
        wanted_type: &TypeApproximation,
    ) -> Option<(VarNum, TypeApproximation)> {
        let mut result: Option<(VarNum, TypeApproximation)>;
        let mut depth = self.scopes.len();
        while depth > 0 {
            if let Multi {
                kind: Pattern,
                elements,
                ..
            } = &self.scopes[depth - 1]
            {
                result = elements
                    .iter()
                    .map(|ScopeElement { vars, .. }| {
                        vars.iter().filter(|(_v, t)| t.is_subtype_of(wanted_type))
                    })
                    .flatten()
                    .choose(rng)
                    .cloned();
                if rng.gen::<bool>() {
                    return result;
                }
                depth -= 1;
            } else {
                break;
            }
        }
        self.pick_bound_var_max_depth(rng, wanted_type, depth)
    }
    fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    pub fn make_fresh_var<RngType: rand::Rng>(
        &mut self,
        rng: &mut RngType,
        t: TypeApproximation,
    ) -> VarNum {
        // Skip some variable numbers, to have different sets of variables used in different branches of patterns/cases
        let v = if rng.gen::<bool>() {
            self.current_scope().next_available()
        } else {
            self.current_scope().next_available() + 1
        };
        self.current_scope_mut().active_element_mut().push(v, t);
        trace!("make_fresh_var: v={}", v);
        v
    }
    fn open_scope(&mut self, scope_kind: ScopeKind, shadowing: Shadowing) {
        self.scope_counter += 1;
        debug!(
            "open_scope: kind={:?}, shadowing={:?}, counter={}",
            scope_kind, shadowing, self.scope_counter
        );
        let next_available = match shadowing {
            Shadowing if !self.disable_shadowing => 0,
            _ => self.current_scope().next_available(),
        };
        self.scopes.push(match scope_kind {
            ScopeKind::Single => Single {
                element: ScopeElement {
                    vars: Vec::new(),
                    next_available,
                },
                scope_counter: self.scope_counter,
            },
            ScopeKind::Multi(multi_kind) => Multi {
                kind: multi_kind,
                available_for_sibling: next_available,
                elements: vec![ScopeElement {
                    vars: Vec::new(),
                    next_available,
                }],
                scope_counter: self.scope_counter,
            },
        });
    }
    pub fn shift_to_sibling(&mut self, reuse_safety: ReuseSafety) {
        debug!(
            "shift_to_sibling: reuse_safety={:?}, counter={}",
            reuse_safety,
            self.current_scope().scope_counter()
        );
        match self.current_scope_mut() {
            Single { .. } => unreachable!(),
            Multi {
                available_for_sibling,
                elements,
                ..
            } => {
                match reuse_safety {
                    SafeToReuse => (),
                    NotSafeToReuse => {
                        *available_for_sibling = elements.last().unwrap().next_available
                    }
                };
                elements.push(ScopeElement {
                    next_available: *available_for_sibling,
                    vars: Vec::new(),
                });
            }
        }
    }
    fn close_scope(&mut self, behavior: ScopeClosureBehavior) {
        let mut scope = self.scopes.pop().unwrap();
        debug!(
            "close_scope: behavior={:?}, counter={}",
            behavior,
            scope.scope_counter()
        );
        match behavior {
            ScopeClosureBehavior::Discard(_) => (),
            Overwrite => {
                let element = scope.element_from_single_mut();
                self.current_scope_mut().shadow(&element.vars);
            }
            KeepUnion => {
                let elements = scope.elements_from_multi_mut();
                let mut mapping = elements
                    .iter()
                    .map(|ScopeElement { vars, .. }| vars.iter())
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>();
                mapping.sort_by(|(v1, _), (v2, _)| v1.cmp(v2));
                for (v, t) in mapping {
                    self.current_scope_mut().active_element_mut().push(v, t);
                }
            }
            KeepIntersection => {
                let elements = scope.elements_from_multi_mut();
                let last_element = elements.last().unwrap();
                for (v, t) in last_element.vars.iter() {
                    // This is quadratic, but I expect N to be very small here. It would be easy to do a binary search instead as the vectors are sorted
                    let mut refined_type = t.clone();
                    let mut found = false;
                    let num_elements = elements.len();
                    // Skipping the last element since we are already iterating over it
                    for i in 0..(num_elements - 1) {
                        found = false;
                        for (v2, t2) in elements[i].vars.iter() {
                            if *v2 == *v {
                                found = true;
                                refined_type.refine(t2);
                                break;
                            }
                        }
                        if !found {
                            break;
                        }
                    }
                    if found {
                        self.current_scope_mut()
                            .active_element_mut()
                            .push(*v, refined_type);
                    }
                }
            }
        }
        match behavior {
            ScopeClosureBehavior::Discard(SafeToReuse) => (),
            _ => {
                let next_safe = match scope {
                    Single {
                        element: ScopeElement { next_available, .. },
                        ..
                    } => next_available,
                    Multi { elements, .. } => {
                        let mut v = 0;
                        for ScopeElement { next_available, .. } in elements {
                            make_max(&mut v, next_available);
                        }
                        v
                    }
                };
                self.current_scope_mut().mark_unsafe_until(next_safe);
            }
        }
    }
    pub fn with_single_scope<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        shadowing: Shadowing,
        behavior: ScopeClosureBehavior,
        mut f: F,
    ) -> T {
        self.open_scope(ScopeKind::Single, shadowing);
        let result = f(self);
        self.close_scope(behavior);
        result
    }
    pub fn with_multi_scope_manual<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        kind: MultiScopeKind,
        shadowing: Shadowing,
        behavior: ScopeClosureBehavior,
        mut f: F,
    ) -> T {
        self.open_scope(ScopeKind::Multi(kind), shadowing);
        let result = f(self);
        self.close_scope(behavior);
        result
    }
    pub fn with_multi_scope_auto<T, F: FnMut(&mut Self, usize) -> T>(
        &mut self,
        kind: MultiScopeKind,
        shadowing: Shadowing,
        shift_safety: ReuseSafety,
        behavior: ScopeClosureBehavior,
        arity: usize,
        mut f: F,
    ) -> Vec<T> {
        self.with_multi_scope_manual(kind, shadowing, behavior, |env| {
            let mut result = Vec::new();
            for i in 0..arity {
                if i > 0 {
                    env.shift_to_sibling(shift_safety);
                }
                result.push(f(env, i));
            }
            result
        })
    }

    pub fn pick_function<RngType: rand::Rng>(
        &self,
        rng: &mut RngType,
        return_type: &TypeApproximation,
        determinism_arg: Determinism,
        guard_context_arg: GuardContext,
        call_locality_arg: CallLocality,
    ) -> Option<&FunctionInformation> {
        self.funs
            .iter()
            .filter(
                |FunctionInformation {
                     t,
                     determinism,
                     guard_context,
                     call_locality,
                     ..
                 }| {
                    ((determinism_arg == AnyDeterminism) || (*determinism == DeterministicOnly))
                        && ((guard_context_arg == NotInGuard) || (*guard_context == InGuard))
                        && ((call_locality_arg == Remote) || (*call_locality == Local))
                        && t.return_type.is_subtype_of(return_type)
                },
            )
            .choose(rng)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GuardContext {
    InGuard,
    NotInGuard,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Determinism {
    DeterministicOnly,
    AnyDeterminism,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CallLocality {
    Local,
    Remote,
}

#[derive(Clone, Debug)]
pub struct FunctionInformation {
    pub name: String,
    pub module_name: String,
    pub t: FunctionTypeApproximation,
    pub determinism: Determinism,
    pub guard_context: GuardContext,
    pub call_locality: CallLocality,
}

fn make_max<T>(old: &mut T, new: T)
where
    T: Ord + Copy,
{
    *old = std::cmp::max(*old, new);
}
