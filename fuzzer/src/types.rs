/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::iter::zip;

use TypeApproximation::*;

use crate::ast::RecordId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeApproximation {
    Any,
    Integer,
    Float,
    Number,
    AnyTuple,
    Tuple(Vec<TypeApproximation>),
    // Note that all the information is duplicated between the record in the module and here.
    // This is because we don't have access to the module when manipulating the type.
    RecordType(RecordId, String, Vec<(String, TypeApproximation)>),
    Atom,
    List(Box<TypeApproximation>),
    Map,
    Bitstring,
    Fun,
    Pid,
    Port,
    Ref,
    Bottom,
    EtsTableName,
    EtsTableId,
    SpecificAtom(String),
    Union(Vec<TypeApproximation>),
}
impl TypeApproximation {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (Union(ts1), _) => ts1.iter().all(|t| t.is_subtype_of(other)),
            (_, Union(ts2)) => ts2.iter().any(|t| self.is_subtype_of(t)),
            (Bottom, _) => true,
            (_, Any) => true,
            (Integer, Number) => true,
            (Float, Number) => true,
            (Tuple(_), AnyTuple) => true,
            (RecordType(_, _, _), AnyTuple) => true,
            (RecordType(_, name, fields), Tuple(v2)) if 1 + fields.len() == v2.len() => {
                record_type_to_tuple(name, fields).is_subtype_of(other)
            }
            (EtsTableName, Atom) => true,
            (SpecificAtom(_), Atom) => true,
            (Tuple(ts1), Tuple(ts2)) if ts1.len() == ts2.len() => ts1
                .iter()
                .zip(ts2.iter())
                .all(|(t1, t2)| t1.is_subtype_of(t2)),
            (List(t1), List(t2)) => t1.is_subtype_of(t2),
            _ => false,
        }
    }
    pub fn refine(&mut self, other: &Self) {
        if self.is_subtype_of(other) {
            return;
        }
        if other.is_subtype_of(self) {
            *self = other.clone();
            return;
        }
        match (self, other) {
            (u @ Union(_), _) => {
                // This weird structure is to appease the borrow checker
                let maybe_new_t = {
                    let Union(ts) = u else { unreachable!() };
                    ts.iter_mut().for_each(|t| t.refine(other));
                    ts.retain(|t| match t {
                        Bottom => false,
                        _ => true,
                    });
                    if ts.len() == 0 {
                        Some(Bottom)
                    } else if ts.len() == 1 {
                        Some(ts.pop().unwrap())
                    } else {
                        None
                    }
                };
                if let Some(new_t) = maybe_new_t {
                    *u = new_t;
                }
            }
            (List(ref mut t1), List(t2)) => {
                t1.refine(t2);
            }
            (Tuple(ref mut ts1), Tuple(ts2)) if ts1.len() == ts2.len() => {
                ts1.iter_mut()
                    .zip(ts2.iter())
                    .for_each(|(t1, t2)| t1.refine(t2));
            }
            (t, _) => {
                *t = Bottom;
            }
        }
    }
}

fn record_type_to_tuple(
    name: &String,
    fields: &Vec<(String, TypeApproximation)>,
) -> TypeApproximation {
    let mut v = vec![];
    v.push(SpecificAtom(name.clone()));
    v.extend(fields.iter().map(|(_, t)| t.clone()));
    Tuple(v)
}

pub fn ets_table_type() -> TypeApproximation {
    Union(vec![EtsTableId, EtsTableName])
}
pub fn boolean_type() -> TypeApproximation {
    Union(vec![
        SpecificAtom("true".into()),
        SpecificAtom("false".into()),
    ])
}

pub fn write_list_strings<I: Iterator<Item = String>>(
    f: &mut fmt::Formatter<'_>,
    l: I,
    separator: &str,
) -> fmt::Result {
    let mut is_first = true;
    for x in l {
        if !is_first {
            write!(f, "{}", separator)?;
        }
        is_first = false;
        write!(f, "{}", x)?;
    }
    Ok(())
}

impl fmt::Display for TypeApproximation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            List(t) if **t == Any => write!(f, "list()"),
            List(t) => write!(f, "[{}]", t),
            Tuple(ts) => {
                write!(f, "{{")?;
                write_list_strings(f, ts.iter().map(|t| t.to_string()), ", ")?;
                write!(f, "}}")
            }
            Any => write!(f, "term()"),
            Integer => write!(f, "integer()"),
            Float => write!(f, "float()"),
            Number => write!(f, "number()"),
            AnyTuple => write!(f, "tuple()"),
            Atom => write!(f, "atom()"),
            Map => write!(f, "map()"),
            Bitstring => write!(f, "bitstring()"),
            Fun => write!(f, "fun()"),
            Pid => write!(f, "pid()"),
            Port => write!(f, "port()"),
            Ref => write!(f, "reference()"),
            Bottom => write!(f, "none()"),
            SpecificAtom(a) => write!(f, "'{}'", a),
            RecordType(_, name, _) => write!(f, "#{}{{}}", name),
            EtsTableName => write!(f, "atom()"),
            EtsTableId => write!(f, "ets:tid()"),
            t if *t == boolean_type() => write!(f, "boolean()"),
            t if *t == ets_table_type() => write!(f, "ets:table()"),
            Union(ts) => {
                write!(f, "(")?;
                write_list_strings(f, ts.iter().map(|t| t.to_string()), " | ")?;
                write!(f, ")")
            }
        }
    }
}

pub fn type_union(left: &TypeApproximation, right: &TypeApproximation) -> TypeApproximation {
    match (left, right) {
        _ if left.is_subtype_of(right) => right.clone(),
        _ if right.is_subtype_of(left) => left.clone(),
        (Float, Integer) | (Integer, Float) => Number,
        (EtsTableId, EtsTableName) | (EtsTableName, EtsTableId) => {
            Union(vec![EtsTableId, EtsTableName])
        }
        (List(t1), List(t2)) => List(Box::new(type_union(t1, t2))),
        (Tuple(ts1), Tuple(ts2)) if ts1.len() == ts2.len() => Tuple(
            ts1.iter()
                .zip(ts2.iter())
                .map(|(t1, t2)| type_union(t1, t2))
                .collect::<Vec<_>>(),
        ),
        (Tuple(_ts1), Tuple(_ts2)) => AnyTuple,
        (SpecificAtom(_), SpecificAtom(_))
            if left.is_subtype_of(&boolean_type()) && right.is_subtype_of(&boolean_type()) =>
        {
            boolean_type()
        }
        (SpecificAtom(_), SpecificAtom(_)) => Atom,
        (RecordType(_, n, f), t2) | (t2, RecordType(_, n, f)) => {
            type_union(&record_type_to_tuple(n, f), t2)
        }
        // We could be more precise for Union, but it would risk blowing up the size of the types.
        _ => Any,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionTypeApproximation {
    pub return_type: TypeApproximation,
    pub arg_types: Vec<TypeApproximation>,
}

pub fn join_function_types(types: &[FunctionTypeApproximation]) -> FunctionTypeApproximation {
    types
        .iter()
        .cloned()
        .reduce(
            |ft1: FunctionTypeApproximation, ft2: FunctionTypeApproximation| {
                assert!(ft1.arg_types.len() == ft2.arg_types.len());
                FunctionTypeApproximation {
                    return_type: type_union(&ft1.return_type, &ft2.return_type),
                    arg_types: zip(ft1.arg_types.iter(), ft2.arg_types.iter())
                        .map(|(t1, t2)| type_union(t1, t2))
                        .collect::<Vec<_>>(),
                }
            },
        )
        .unwrap()
}
