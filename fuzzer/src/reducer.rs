/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */
use std::collections::HashMap;
use std::mem;

use log::debug;
use log::info;

use crate::ast::*;
use crate::core_types::*;
use crate::generator::make_vec;
use crate::types::TypeApproximation;

/// run returns true iff its argument still exhibits the behavior that we want (typically a crash)
/// reduce mutates its first argument in-place while guaranteeing that run will always return true on it
pub fn reduce_module<F: Fn(&Module) -> bool>(module: &mut Module, run: &F) {
    let num_functions = module.functions.len();
    let mut func_decl_to_reduce = Vec::new();
    for i in 0..num_functions {
        if !try_trivialize_func_decl(module, run, i) {
            func_decl_to_reduce.push(i);
        }
    }
    for i in func_decl_to_reduce {
        reduce_func_decl(module, run, i);
    }

    info!("      Trying to eliminate records");
    let num_records = module.records.len();
    let mut records_to_reduce = Vec::new();
    for i in 0..num_records {
        // We can't actually remove the records, because they are referred to in expressions, and it breaks the ast printing.
        // So we just hide them instead.
        module.records[i].hidden = true;
        let record = module.records[i].clone();
        let text = format!("eliminating record {}", record.name);
        if !try_reduction_or_else(module, run, record.size(module), &text, |m: &mut Module| {
            m.records[i].hidden = false;
        }) {
            records_to_reduce.push(i);
        }
    }

    info!("      Reducing record fields");
    for i in records_to_reduce {
        reduce_record(module, run, i);
    }

    info!("      Trying to eliminate function declarations altogether");
    for i in (0..num_functions).rev() {
        let func_decl = module.functions.remove(i);
        let text = format!(
            "eliminating function declaration {}/{}",
            &func_decl.name, func_decl.arity
        );
        let _ = try_reduction_or_else(
            module,
            run,
            func_decl.size(module),
            &text,
            |m: &mut Module| m.functions.insert(i, func_decl),
        );
    }

    info!("      Trying to eliminate function parameters");
    let new_num_functions = module.functions.len();
    let mut fun_name_to_index = HashMap::new();
    for i in 0..new_num_functions {
        let func = &module.functions[i];
        fun_name_to_index.insert((func.name.clone(), func.arity), i);
    }
    let mut fun_to_calls = make_vec(new_num_functions, || Vec::new());
    for expr_id in module.all_expr_ids() {
        match module.expr(expr_id) {
            Expr::LocalCall(fun_name, args) => {
                if let Some(fun_index) = fun_name_to_index
                    .get(&(fun_name.clone(), args.len()))
                    .copied()
                {
                    fun_to_calls[fun_index].push(expr_id);
                }
            }
            _ => continue,
        }
    }
    for fun_index in 0..new_num_functions {
        let func_decl = &module.functions[fun_index];
        if func_decl.clauses.len() > 1 {
            continue;
        }
        let arity = func_decl.arity;
        let name = func_decl.name.clone();
        let clause_id = func_decl.clauses[0];
        for arg_index in (0..arity).rev() {
            let func_clause_ref = module.function_clause(clause_id);
            match module.pattern(func_clause_ref.args[arg_index]) {
                Pattern::Underscore() => (),
                _ => continue,
            }
            let module_snapshot = module.clone();
            let func_clause = module.function_clause_mut(clause_id);
            let _ = func_clause.args.remove(arg_index);
            module.functions[fun_index].arity -= 1;
            let _ = module.functions[fun_index]
                .function_type
                .arg_types
                .remove(arg_index);
            let _ = module.functions[fun_index].clause_types[0]
                .arg_types
                .remove(arg_index);
            for call_id in fun_to_calls[fun_index].iter().copied() {
                let call = module.expr_mut(call_id);
                match call {
                    Expr::LocalCall(_, ref mut call_args) => {
                        call_args.remove(arg_index);
                    }
                    _ => unreachable!(),
                }
            }
            let text = format!(
                "removing parameter #{} from function declaration {}/{}",
                arg_index, name, arity
            );
            let _ = try_reduction_or_else(
                module,
                run,
                // Note that this win size is an overestimation, since it also counts calls that have been eliminated.
                (fun_to_calls[fun_index].len() + 1) as ASTSize,
                &text,
                |m: &mut Module| *m = module_snapshot,
            );
        }
    }
}

fn try_reduction_or_else<F: Fn(&Module) -> bool, G: FnOnce(&mut Module)>(
    module: &mut Module,
    run: &F,
    size: ASTSize,
    text: &str,
    restore: G,
) -> bool {
    let success = run(module);
    if success {
        info!("[SUCCESS] Reduction of size {} by {}", size, text);
    } else {
        debug!("[FAIL] Reduction of size {} by {}", size, text);
        restore(module);
    }
    success
}

fn try_trivialize_func_decl<F: Fn(&Module) -> bool>(
    module: &mut Module,
    run: &F,
    func_decl_id: usize,
) -> bool {
    let name = module.functions[func_decl_id].name.clone();
    let arity = module.functions[func_decl_id].arity;
    let ok_expr = module.add_expr(Expr::Atom("ok".to_string()), TypeApproximation::Atom);
    let body = module.add_body(Body {
        exprs: vec![ok_expr],
    });
    let guard = module.add_guard(Guard::default());
    let args = make_vec(arity, || module.add_pattern(Pattern::Underscore()));
    let trivial_clause_id = module.add_function_clause(FunctionClause {
        name,
        body,
        guard,
        args,
    });
    let old_clauses = mem::replace(
        &mut module.functions[func_decl_id].clauses,
        vec![trivial_clause_id],
    );
    let old_visible_spec = mem::replace(&mut module.functions[func_decl_id].visible_spec, false);
    let text = format!(
        "replacing function declaration {}/{} by a trivial one",
        &module.functions[func_decl_id].name, arity
    );
    try_reduction_or_else(
        module,
        run,
        old_clauses.size(module),
        &text,
        |m: &mut Module| {
            m.functions[func_decl_id].clauses = old_clauses;
            m.functions[func_decl_id].visible_spec = old_visible_spec;
        },
    )
}

fn reduce_func_decl<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, func_decl_id: usize) {
    let name = module.functions[func_decl_id].name.clone();
    let arity = module.functions[func_decl_id].arity;
    info!("    Trying to reduce {}/{}", name, arity);

    let num_clauses = module.functions[func_decl_id].clauses.len();
    for i in (0..num_clauses).rev() {
        if module.functions[func_decl_id].clauses.len() == 1 {
            // Avoid eliminating the last clause of a function declaration for now.
            break;
        }
        let clause_id = module.functions[func_decl_id].clauses.remove(i);
        let _ = try_reduction_or_else(
            module,
            run,
            module.function_clause(clause_id).size(module),
            "eliminating a function clause",
            |m: &mut Module| m.functions[func_decl_id].clauses.insert(i, clause_id),
        );
    }

    if module.functions[func_decl_id].visible_spec {
        module.functions[func_decl_id].visible_spec = false;
        let _ = try_reduction_or_else(
            module,
            run,
            0,
            "eliminating a function spec",
            |m: &mut Module| m.functions[func_decl_id].visible_spec = true,
        );
    }

    // TODO: I don't think this clone() should be required, but I can't get the borrow checker to understand it.
    let clause_ids = module.functions[func_decl_id].clauses.clone();
    for clause_id in clause_ids {
        reduce_clause(module, run, clause_id);
    }
}

fn reduce_clause<F: Fn(&Module) -> bool>(
    module: &mut Module,
    run: &F,
    clause_id: FunctionClauseId,
) {
    let clause = module.function_clause(clause_id);
    info!(
        "  Reducing a clause of {}/{}",
        clause.name,
        clause.args.len()
    );

    // We reduce the guards and body before the arguments to minimize the number of variables that the arguments have to bind.
    reduce_guard(module, run, module.function_clause(clause_id).guard);
    reduce_body(module, run, module.function_clause(clause_id).body);

    // TODO: I don't think this clone() should be required, but I can't get the borrow checker to understand it.
    let args = module.function_clause(clause_id).args.clone();
    for pattern_id in args {
        reduce_pattern(module, run, pattern_id);
    }
}

fn reduce_guard<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, guard_id: GuardId) {
    let num_guard_seqs = module.guard(guard_id).guard_seqs.len();
    for i in (0..num_guard_seqs).rev() {
        let guard_seq_id = module.guard_mut(guard_id).guard_seqs.remove(i);
        let _ = try_reduction_or_else(
            module,
            run,
            module.guard_seq(guard_seq_id).size(module),
            "eliminating a guard sequence",
            |m: &mut Module| m.guard_mut(guard_id).guard_seqs.insert(i, guard_seq_id),
        );
    }

    // TODO: I don't think this clone() should be required, but I can't get the borrow checker to understand it.
    let guard_seqs = module.guard(guard_id).guard_seqs.clone();
    for guard_seq_id in guard_seqs {
        reduce_guard_seq(module, run, guard_seq_id);
    }
}

fn reduce_guard_seq<F: Fn(&Module) -> bool>(
    module: &mut Module,
    run: &F,
    guard_seq_id: GuardSeqId,
) {
    let num_guard_exprs = module.guard_seq(guard_seq_id).guard_exprs.len();
    for i in (0..num_guard_exprs).rev() {
        // Avoid eliminating the last expression of a guard sequence.
        if module.guard_seq(guard_seq_id).guard_exprs.len() == 1 {
            break;
        }
        let expr_id = module.guard_seq_mut(guard_seq_id).guard_exprs.remove(i);
        let _ = try_reduction_or_else(
            module,
            run,
            module.expr(expr_id).size(module),
            "eliminating a guard expression",
            |m: &mut Module| m.guard_seq_mut(guard_seq_id).guard_exprs.insert(i, expr_id),
        );
    }

    // TODO: I don't think this clone() should be required, but I can't get the borrow checker to understand it.
    let guard_exprs = module.guard_seq(guard_seq_id).guard_exprs.clone();
    for guard_expr_id in guard_exprs {
        reduce_expr(module, run, guard_expr_id);
    }
}

fn reduce_body<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, body_id: BodyId) {
    let num_exprs = module.body(body_id).exprs.len();
    for i in (0..num_exprs).rev() {
        if module.body(body_id).exprs.len() == 1 {
            // Avoid eliminating the last expression of a body.
            return reduce_expr(module, run, module.body(body_id).exprs[0]);
        }
        let expr_id = module.body_mut(body_id).exprs.remove(i);
        if !try_reduction_or_else(
            module,
            run,
            module.expr(expr_id).size(module),
            "eliminating an expression from a body",
            |m: &mut Module| m.body_mut(body_id).exprs.insert(i, expr_id),
        ) {
            reduce_expr(module, run, expr_id);
        }
    }
}

fn reduce_pattern<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, pattern_id: PatternId) {
    let underscore = Pattern::Underscore();
    let pattern = mem::replace(module.pattern_mut(pattern_id), underscore);
    if !try_reduction_or_else(
        module,
        run,
        pattern.size(module) - 1,
        "replacing a pattern by _",
        |m: &mut Module| *m.pattern_mut(pattern_id) = pattern,
    ) {
        recurse_pattern(module, run, pattern_id);
    }
}

fn try_replace_pattern<PatternGenerator: FnOnce() -> Pattern, CheckModule: Fn(&Module) -> bool>(
    module: &mut Module,
    run: &CheckModule,
    pattern_id: PatternId,
    text: &str,
    f: PatternGenerator,
) -> bool {
    let new_pattern = f();
    let new_size = new_pattern.size(module);
    let old_pattern = mem::replace(module.pattern_mut(pattern_id), new_pattern);
    let old_size = old_pattern.size(module);
    try_reduction_or_else(module, run, old_size - new_size, text, |m: &mut Module| {
        *m.pattern_mut(pattern_id) = old_pattern
    })
}

fn recurse_pattern<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, pattern_id: PatternId) {
    macro_rules! try_replace_pattern {
        ( $descr:expr, $replacement:expr ) => {
            try_replace_pattern(module, run, pattern_id, $descr, $replacement)
        };
    }
    match module.pattern(pattern_id).clone() {
        Pattern::Nil()
        | Pattern::Atom(_)
        | Pattern::Integer(_)
        | Pattern::Float(_)
        | Pattern::Underscore()
        | Pattern::NamedVar(_) => (),
        Pattern::EqualPatterns(p1_id, p2_id) => {
            let p1 = module.pattern(p1_id).clone();
            if try_replace_pattern!(
                "replacing a pattern equality by the left-side pattern",
                || p1
            ) {
                return recurse_pattern(module, run, pattern_id);
            }
            let p2 = module.pattern(p2_id).clone();
            if try_replace_pattern!(
                "replacing a pattern equality by the right-side pattern",
                || p2
            ) {
                return recurse_pattern(module, run, pattern_id);
            }
            reduce_pattern(module, run, p1_id);
            reduce_pattern(module, run, p2_id);
        }
        Pattern::Tuple(ref mut args) => {
            for i in (0..args.len()).rev() {
                let p = args.remove(i);
                if try_replace_pattern!("eliminating an element of a tuple pattern", || {
                    Pattern::Tuple(args.clone())
                }) {
                    continue;
                }
                args.insert(i, p);
            }
            if args.len() == 1 {
                let new_pattern = module.pattern(args[0]).clone();
                if try_replace_pattern!(
                    "replacing a tuple pattern with a single element by this element",
                    || new_pattern
                ) {
                    return recurse_pattern(module, run, pattern_id);
                }
            }
            for sub_pattern_id in args {
                reduce_pattern(module, run, *sub_pattern_id);
            }
        }
        Pattern::List(p1_id, p2_id) => {
            let p1 = module.pattern(p1_id).clone();
            if try_replace_pattern!("replacing a list pattern by the head pattern", || p1) {
                return recurse_pattern(module, run, pattern_id);
            }
            let p2 = module.pattern(p2_id).clone();
            if try_replace_pattern!("replacing a list pattern by the tail pattern", || p2) {
                return recurse_pattern(module, run, pattern_id);
            }
            reduce_pattern(module, run, p1_id);
            reduce_pattern(module, run, p2_id);
        }
        Pattern::Bitstring(ref mut elements) => {
            // TODO: add another reduction, that removes size/type-specifier.
            for i in (0..elements.len()).rev() {
                let (v, s, ts) = elements.remove(i);
                if try_replace_pattern!("eliminating an element of a bitstring match", || {
                    Pattern::Bitstring(elements.clone())
                }) {
                    continue;
                }
                elements.insert(i, (v, s, ts));
            }

            for (v, s, _) in elements {
                reduce_pattern(module, run, *v);
                match s {
                    None => (),
                    Some(e) => reduce_expr(module, run, *e),
                }
            }
        }
        Pattern::Map(ref mut args) => {
            for i in (0..args.len()).rev() {
                let (k, v) = args.remove(i);
                if try_replace_pattern!("eliminating a pair of a map pattern", || Pattern::Map(
                    args.clone()
                )) {
                    continue;
                }
                args.insert(i, (k, v));
            }
            if args.len() == 1 {
                let new_pattern = module.pattern(args[0].1).clone();
                if try_replace_pattern!("replacing a map pattern by the only pattern in it", || {
                    new_pattern
                }) {
                    return recurse_pattern(module, run, pattern_id);
                }
            }
            for (k, v) in args {
                reduce_expr(module, run, *k);
                reduce_pattern(module, run, *v);
            }
        }
        Pattern::Record(r, ref mut fields) => {
            for i in (0..fields.len()).rev() {
                let (f, p) = fields.remove(i);
                if try_replace_pattern!("eliminating a field of a record pattern", || {
                    Pattern::Record(r.clone(), fields.clone())
                }) {
                    continue;
                }
                fields.insert(i, (f, p));
            }
            if fields.len() == 1 {
                let new_pattern = module.pattern(fields[0].1).clone();
                if try_replace_pattern!(
                    "replacing a record pattern by the only field in it",
                    || new_pattern
                ) {
                    return recurse_pattern(module, run, pattern_id);
                }
            }
            for (_f, p) in fields {
                reduce_pattern(module, run, *p);
            }
        }
    }
}

fn reduce_expr<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, expr_id: ExprId) {
    let placeholder_expr = Expr::Atom("ok".to_string());
    let expr = mem::replace(module.expr_mut(expr_id), placeholder_expr);
    if !try_reduction_or_else(
        module,
        run,
        expr.size(module) - 1,
        "replacing an expression by ok",
        |m: &mut Module| *m.expr_mut(expr_id) = expr,
    ) {
        recurse_expr(module, run, expr_id);
    }
}

fn try_replace_expr<ExprGenerator: FnOnce() -> Expr, CheckModule: Fn(&Module) -> bool>(
    module: &mut Module,
    run: &CheckModule,
    expr_id: ExprId,
    text: &str,
    f: ExprGenerator,
) -> bool {
    let new_expr = f();
    let new_size = new_expr.size(module);
    let old_expr = mem::replace(module.expr_mut(expr_id), new_expr);
    let old_size = old_expr.size(module);
    try_reduction_or_else(module, run, old_size - new_size, text, |m: &mut Module| {
        *m.expr_mut(expr_id) = old_expr;
    })
}

fn recurse_expr<F: Fn(&Module) -> bool>(module: &mut Module, run: &F, expr_id: ExprId) {
    macro_rules! try_replace_expr {
        ( $descr:expr, $replacement:expr) => {
            try_replace_expr(module, run, expr_id, $descr, $replacement)
        };
    }
    match module.expr(expr_id).clone() {
        Expr::String(_) => {
            let _ = try_replace_expr!("replacing a string literal by []", || Expr::Nil());
        }
        Expr::LocalCall(_, ref args) => {
            if try_replace_expr!(
                "replacing a local function call by a tuple",
                || Expr::Tuple(args.clone())
            ) {
                return recurse_expr(module, run, expr_id);
            }
            for sub_expr_id in args {
                reduce_expr(module, run, *sub_expr_id);
            }
        }
        Expr::RemoteCall(_, _, ref args) => {
            if try_replace_expr!("replacing a remote function call by a tuple", || {
                Expr::Tuple(args.clone())
            }) {
                return recurse_expr(module, run, expr_id);
            }
            for sub_expr_id in args {
                reduce_expr(module, run, *sub_expr_id);
            }
        }
        Expr::Tuple(ref mut args) => {
            // FIXME: abstract away the following pattern:
            for i in (0..args.len()).rev() {
                let e = args.remove(i);
                if try_replace_expr!("eliminating an element of a tuple", || Expr::Tuple(
                    args.clone()
                )) {
                    continue;
                }
                args.insert(i, e);
            }
            if args.len() == 1 {
                let new_expr = module.expr(args[0]).clone();
                if try_replace_expr!(
                    "replacing a tuple with a single element by this element",
                    || new_expr
                ) {
                    return recurse_expr(module, run, expr_id);
                }
            }
            for sub_expr_id in args {
                reduce_expr(module, run, *sub_expr_id);
            }
        }
        Expr::List(ref mut args) => {
            // FIXME: abstract away the following pattern:
            for i in (0..args.len()).rev() {
                let e = args.remove(i);
                if try_replace_expr!("eliminating an element of a list", || Expr::List(
                    args.clone()
                )) {
                    continue;
                }
                args.insert(i, e);
            }
            if args.len() == 1 {
                let new_expr = module.expr(args[0]).clone();
                if try_replace_expr!(
                    "replacing a list with a single element by this element",
                    || new_expr
                ) {
                    return recurse_expr(module, run, expr_id);
                }
            }
            for sub_expr_id in args {
                reduce_expr(module, run, *sub_expr_id);
            }
        }
        Expr::Catch(e) => {
            let new_expr = module.expr(e).clone();
            if try_replace_expr!(
                "replacing a catch expression by the expression it wraps",
                || new_expr
            ) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_expr(module, run, e);
        }
        Expr::BinaryOperation(_op, e1_id, e2_id) => {
            let e1 = module.expr(e1_id).clone();
            if try_replace_expr!("replacing a binary operation by its left operand", || e1) {
                return recurse_expr(module, run, expr_id);
            }
            let e2 = module.expr(e2_id).clone();
            if try_replace_expr!("replacing a binary operation by its right operand", || e2) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_expr(module, run, e1_id);
            reduce_expr(module, run, e2_id);
        }
        Expr::UnaryOperation(_op, e_id) => {
            let e = module.expr(e_id).clone();
            if try_replace_expr!("replacing a unary operation by its operand", || e) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_expr(module, run, e_id);
        }
        Expr::Case(matched_expr, ref mut cases) => {
            let new_expr = module.expr(matched_expr).clone();
            if try_replace_expr!(
                "replacing a case expression by the expression it matched",
                || new_expr
            ) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_expr(module, run, matched_expr);

            for i in (0..cases.len()).rev() {
                if cases.len() == 1 {
                    break;
                }
                let (p, g, b) = cases.remove(i);
                if try_replace_expr!("eliminating a case from a case expression", || Expr::Case(
                    matched_expr,
                    cases.clone()
                )) {
                    continue;
                }
                cases.insert(i, (p, g, b));
            }

            if cases.len() == 1 {
                let (p, _g, _b) = cases[0];
                if try_replace_expr!("replacing a case expression by an assignment", || {
                    Expr::Assignment(p, matched_expr)
                }) {
                    return recurse_expr(module, run, expr_id);
                }
            }

            // TODO: replace by a tuple with all the bodies?
            // Very tricky as the patterns can bind variables
            for (p, g, b) in cases.iter() {
                reduce_guard(module, run, *g);
                reduce_body(module, run, *b);
                reduce_pattern(module, run, *p);
            }

            // We cannot merge this with the Case=>Assignment rewrite rule above,
            //   because we must first have reduced the body to a single expression.
            if cases.len() == 1 {
                let (p, g, b) = cases[0];
                if let Pattern::Underscore() = module.pattern(p) {
                    let Body { exprs, .. } = module.body(b);
                    let Guard { guard_seqs, .. } = module.guard(g);
                    if exprs.len() == 1 && guard_seqs.len() == 0 {
                        let new_expr = module.expr(exprs[0]).clone();
                        let _ = try_replace_expr!(
                            "replacing a trivial case expression by its only sub-expression",
                            || new_expr
                        );
                    }
                }
            }
        }
        Expr::Assignment(p_id, e_id) => {
            let new_expr = module.expr(e_id).clone();
            if try_replace_expr!("replacing an assignment by its right-hand side", || {
                new_expr
            }) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_pattern(module, run, p_id);
            reduce_expr(module, run, e_id);
        }
        Expr::MapLiteral(ref mut mappings) => {
            for i in (0..mappings.len()).rev() {
                let (k, v) = mappings.remove(i);
                if try_replace_expr!("eliminating an element of a map literal", || {
                    Expr::MapLiteral(mappings.clone())
                }) {
                    continue;
                }
                mappings.insert(i, (k, v));
            }
            if try_replace_expr!("replacing a map literal by a tuple", || {
                let mut args = Vec::new();
                for (e1, e2) in mappings.iter() {
                    args.push(*e1);
                    args.push(*e2);
                }
                Expr::Tuple(args)
            }) {
                return recurse_expr(module, run, expr_id);
            }
            for (e1, e2) in mappings {
                reduce_expr(module, run, *e1);
                reduce_expr(module, run, *e2);
            }
        }
        Expr::MapInsertion(e_id, k_id, v_id) | Expr::MapUpdate(e_id, k_id, v_id) => {
            let e = module.expr(e_id).clone();
            if try_replace_expr!(
                "replacing a map insertion/update by its first operand",
                || e
            ) {
                return recurse_expr(module, run, expr_id);
            }
            let map_literal_elements = vec![(k_id, v_id)];
            if try_replace_expr!(
                "replacing a map insertion/update by its second operand as a map literal",
                || Expr::MapLiteral(map_literal_elements)
            ) {
                return recurse_expr(module, run, expr_id);
            }
            // TODO: more reductions, e.g. doing the insertion
            reduce_expr(module, run, e_id);
            reduce_expr(module, run, k_id);
            reduce_expr(module, run, v_id);
        }
        Expr::RecordCreation(r_id, ref mut fields) => {
            for i in (0..fields.len()).rev() {
                let (f, v) = fields.remove(i);
                if try_replace_expr!("eliminating a field of a record creation", || {
                    Expr::RecordCreation(r_id, fields.clone())
                }) {
                    continue;
                }
                fields.insert(i, (f, v));
            }
            if try_replace_expr!("replacing a record creation by a tuple", || {
                let mut args = Vec::new();
                for (_, e2) in fields.iter() {
                    args.push(*e2);
                }
                Expr::Tuple(args)
            }) {
                return recurse_expr(module, run, expr_id);
            }
            for (_, e2) in fields {
                reduce_expr(module, run, *e2);
            }
        }
        Expr::RecordUpdate(e, r_id, ref mut fields) => {
            for i in (0..fields.len()).rev() {
                let (f, v) = fields.remove(i);
                if try_replace_expr!("eliminating a field of a record update", || {
                    Expr::RecordUpdate(e, r_id, fields.clone())
                }) {
                    continue;
                }
                fields.insert(i, (f, v));
            }
            if try_replace_expr!("replacing a record update by a tuple", || {
                let mut args = vec![e];
                for (_, e2) in fields.iter() {
                    args.push(*e2);
                }
                Expr::Tuple(args)
            }) {
                return recurse_expr(module, run, expr_id);
            }
            reduce_expr(module, run, e);
            for (_, e2) in fields {
                reduce_expr(module, run, *e2);
            }
        }
        Expr::BitstringConstruction(ref mut elements) => {
            // TODO: add another reduction, that removes size/type-specifier.
            for i in (0..elements.len()).rev() {
                let (v, s, ts) = elements.remove(i);
                if try_replace_expr!("eliminating an element of a bitstring construction", || {
                    Expr::BitstringConstruction(elements.clone())
                }) {
                    continue;
                }
                elements.insert(i, (v, s, ts));
            }
            if elements.len() == 1 {
                let (v, s, _) = elements[0];
                let new_expr = module.expr(v).clone();
                if try_replace_expr!(
                    "replacing a single element bitstring construction by that element",
                    || new_expr
                ) {
                    return recurse_expr(module, run, expr_id);
                }
                if let Some(actual_size) = s {
                    let new_expr = module.expr(actual_size).clone();
                    if try_replace_expr!(
                        "replacing a single element bitstring construction by that element's size",
                        || new_expr
                    ) {
                        return recurse_expr(module, run, expr_id);
                    }
                }
            }
            for (v, s, _) in elements {
                reduce_expr(module, run, *v);
                match s {
                    None => (),
                    Some(e) => reduce_expr(module, run, *e),
                }
            }
        }
        Expr::MapComprehension(key, value, ref mut elements) => {
            reduce_expr(module, run, key);
            reduce_expr(module, run, value);

            reduce_comprehension_elements(module, run, elements, expr_id, |elems| {
                Expr::MapComprehension(key, value, elems.clone())
            });
        }
        Expr::Comprehension(kind, value, ref mut elements) => {
            let value_clone = module.expr(value).clone();
            if try_replace_expr!("replacing a comprehension by its content", || value_clone) {
                return recurse_expr(module, run, expr_id);
            }

            reduce_expr(module, run, value);

            reduce_comprehension_elements(module, run, elements, expr_id, |elems| {
                Expr::Comprehension(kind, value, elems.clone())
            });
        }
        Expr::Fun(var_name, ref mut clauses) => {
            for i in (0..clauses.len()).rev() {
                if clauses.len() == 1 {
                    break;
                }
                let clause = clauses.remove(i);
                if try_replace_expr!("eliminating a clause from a fun expression", || Expr::Fun(
                    var_name,
                    clauses.clone()
                )) {
                    continue;
                }
                clauses.insert(i, clause);
            }
            for clause in clauses.iter() {
                reduce_clause(module, run, *clause);
            }
            // We do this after reducing the clauses, to maximize the likelyhood of the body having a single expression.
            if clauses.len() == 1 {
                let FunctionClause { guard, body, .. } = module.function_clause(clauses[0]);
                let Body { exprs, .. } = module.body(*body);
                let Guard { guard_seqs, .. } = module.guard(*guard);
                if exprs.len() == 1 && guard_seqs.len() == 0 {
                    let new_expr = module.expr(exprs[0]).clone();
                    let _ = try_replace_expr!(
                        "replacing a single-expression fun by that expression",
                        || new_expr
                    );
                }
            }
        }
        Expr::Try(exprs, ref mut of, ref mut catch, ref mut after) => {
            let new_expr = Expr::Block(exprs);
            if try_replace_expr!(
                "replacing a try expression by the expressions that are tried",
                || new_expr
            ) {
                return recurse_expr(module, run, expr_id);
            }

            if let Some(after_body) = after {
                let new_expr = Expr::Block(*after_body);
                if try_replace_expr!("replacing a try expression by its after block", || new_expr) {
                    return recurse_expr(module, run, expr_id);
                }
            }

            if catch.is_some() && after.is_some() {
                // FIXME: size
                let new_expr = Expr::Try(exprs.clone(), of.clone(), None, after.clone());
                if try_replace_expr!("removing the catch section of a try expression", || {
                    new_expr
                }) {
                    return recurse_expr(module, run, expr_id);
                }
                // FIXME: size
                let new_expr = Expr::Try(exprs.clone(), of.clone(), catch.clone(), None);
                if try_replace_expr!("removing the after section of a try expression", || {
                    new_expr
                }) {
                    return recurse_expr(module, run, expr_id);
                }
            }

            if let Some(ref mut of_cases) = of {
                let new_expr = Expr::Try(exprs.clone(), None, catch.clone(), after.clone());
                if try_replace_expr!("removing the of section of a try expression", || new_expr) {
                    return recurse_expr(module, run, expr_id);
                }
                for i in (0..of_cases.len()).rev() {
                    if of_cases.len() == 1 {
                        break;
                    }
                    let (p, g, b) = of_cases.remove(i);
                    if !try_replace_expr!(
                        "eliminating a case from the of section of a try expression",
                        || Expr::Try(
                            exprs.clone(),
                            Some(of_cases.clone()),
                            catch.clone(),
                            after.clone(),
                        )
                    ) {
                        of_cases.insert(i, (p, g, b));
                    }
                }
                for (p, g, b) in of_cases.iter() {
                    reduce_guard(module, run, *g);
                    reduce_body(module, run, *b);
                    reduce_pattern(module, run, *p);
                }
            }

            // after the `of` section, as it may bind variables used in it.
            reduce_body(module, run, exprs);

            if let Some(catch_cases) = catch {
                for (p, g, b) in catch_cases.iter() {
                    reduce_guard(module, run, *g);
                    reduce_body(module, run, *b);
                    reduce_pattern(module, run, *p);
                }
            }
            if let Some(after_body) = after {
                reduce_body(module, run, *after_body);
            }
        }
        Expr::Maybe(ref mut exprs, ref mut else_section) => {
            if let Some(else_cases) = else_section {
                if try_replace_expr!("removing the else section of a maybe", || Expr::Maybe(
                    exprs.clone(),
                    None
                )) {
                    *else_section = None;
                } else {
                    let trivial_expr_id =
                        module.add_expr(Expr::Atom("ok".to_string()), TypeApproximation::Atom);
                    let new_expr = Expr::Case(trivial_expr_id, else_cases.clone());
                    if try_replace_expr!(
                        "replacing a maybe by its else section converted to a case expression",
                        || new_expr
                    ) {
                        return recurse_expr(module, run, expr_id);
                    }
                    for (p, g, b) in else_cases.iter() {
                        reduce_guard(module, run, *g);
                        reduce_body(module, run, *b);
                        reduce_pattern(module, run, *p);
                    }
                }
            }
            let num_exprs = exprs.len();
            for i in (0..num_exprs).rev() {
                if exprs.len() > 1 {
                    // Avoid eliminating the last expression of a body.
                    let maybe_expr = exprs.remove(i);
                    if try_replace_expr!("eliminating an expression from a maybe", || Expr::Maybe(
                        exprs.clone(),
                        else_section.clone()
                    )) {
                        continue;
                    } else {
                        exprs.insert(i, maybe_expr);
                    }
                }
                match exprs[i].clone() {
                    MaybeExpr::Normal(e) => reduce_expr(module, run, e),
                    MaybeExpr::MaybeAssignment(p, e) => {
                        exprs[i] = MaybeExpr::Normal(e);
                        if try_replace_expr!(
                            "replacing a maybe assignment by its right-hand side",
                            || Expr::Maybe(exprs.clone(), else_section.clone())
                        ) {
                            reduce_expr(module, run, e);
                        } else {
                            exprs[i] = MaybeExpr::MaybeAssignment(p, e);
                            reduce_pattern(module, run, p);
                            reduce_expr(module, run, e);
                        }
                    }
                }
            }
            if else_section.is_none() && exprs.len() == 1 {
                if let MaybeExpr::Normal(e) = exprs[0] {
                    let new_expr = module.expr(e).clone();
                    let _ = try_replace_expr!(
                        "replacing a trivial maybe expression by its only sub-expression",
                        || new_expr
                    );
                }
            }
        }
        Expr::Block(b) => {
            reduce_body(module, run, b);
            if module.body(b).exprs.len() == 1 {
                let new_expr = module.expr(module.body(b).exprs[0]).clone();
                let _ =
                    try_replace_expr!("replacing a begin..end block by its single element", || {
                        new_expr
                    });
            }
        }
        Expr::Var(_)
        | Expr::Nil()
        | Expr::Atom(_)
        | Expr::Integer(_)
        | Expr::Float(_)
        | Expr::RecordIndex(_, _) => (),
    }
}

fn reduce_comprehension_elements<
    F: Fn(&Module) -> bool,
    MakeExpr: Fn(&Vec<ComprehensionElement>) -> Expr,
>(
    module: &mut Module,
    run: &F,
    elements: &mut Vec<ComprehensionElement>,
    expr_id: ExprId,
    make_expr: MakeExpr,
) {
    for i in (0..elements.len()).rev() {
        if elements.len() > 1 {
            let elem = elements.remove(i);
            if try_replace_expr(
                module,
                run,
                expr_id,
                "eliminating an element of a comprehension",
                || make_expr(elements),
            ) {
                continue;
            }
            elements.insert(i, elem);
        }
        match elements[i] {
            ComprehensionElement::Filter(_) => (),
            generator @ ComprehensionElement::ListGenerator(_, e)
            | generator @ ComprehensionElement::BitstringGenerator(_, e)
            | generator @ ComprehensionElement::MapGenerator(_, _, e) => {
                elements[i] = ComprehensionElement::Filter(e);
                if !try_replace_expr(
                    module,
                    run,
                    expr_id,
                    "replacing a generator by a filter in a comprehension",
                    || make_expr(elements),
                ) {
                    elements[i] = generator;
                }
            }
        }
        match elements[i] {
            ComprehensionElement::Filter(e) => reduce_expr(module, run, e),
            ComprehensionElement::ListGenerator(p, e)
            | ComprehensionElement::BitstringGenerator(p, e) => {
                reduce_pattern(module, run, p);
                reduce_expr(module, run, e);
            }
            ComprehensionElement::MapGenerator(k, v, e) => {
                reduce_pattern(module, run, k);
                reduce_pattern(module, run, v);
                reduce_expr(module, run, e);
            }
        }
    }

    if elements.len() == 1 {
        if let ComprehensionElement::Filter(filter) = elements[0] {
            let filter_clone = module.expr(filter).clone();
            // We already reduced the filter as much as possible before, so there is no point in recursing further.
            let _ = try_replace_expr(
                module,
                run,
                expr_id,
                "replacing a comprehension with a single filter by that filter",
                || filter_clone,
            );
        }
    }
}

fn reduce_record(module: &mut Module, run: &impl Fn(&Module) -> bool, record_index: usize) {
    // FIXME: this clone should not be necessary, but the borrow checker is ornery.
    for field_id in module.records[record_index].fields.clone() {
        if let Some(expr) = module.record_field(field_id).initializer {
            reduce_expr(module, run, expr);
        }
    }
}
