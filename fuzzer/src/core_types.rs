/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

// variables are named _V1, _V2, etc..
pub type VarNum = u32;
pub type Atom = String;
pub type Arity = usize;
pub type ASTSize = i32; // i32 to make arithmetic easier, e.g. comparing allowed_size - ast_size to 0
