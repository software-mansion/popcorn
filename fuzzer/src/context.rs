/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use crate::core_types::*;

#[derive(Debug, Clone, Copy)]
pub struct Context {
    pub is_in_guard: bool,
    // "binary fields without size are not allowed in patterns of bit string generators"
    pub is_in_bitstring_generator: bool,
    // "The default value for a field is an arbitrary expression, except that it must not use any variables.""
    pub is_in_record_initializer: bool,
    pub recursion_depth: u16,
    pub max_recursion_depth: u16,
    pub allowed_size: ASTSize, // can be negative if we've gone a bit beyond the limit.
    // Useful in places to disable shadowing, which in turns allows testing eqWAlizer.
    pub no_bound_vars: bool,
    pub maybe_is_allowed: bool,
    pub map_comprehensions_are_allowed: bool,
    pub deterministic: bool,
}
impl Context {
    pub fn new() -> Self {
        Context {
            allowed_size: 0,
            recursion_depth: 0,
            max_recursion_depth: 0,
            is_in_guard: false,
            is_in_bitstring_generator: false,
            is_in_record_initializer: false,
            no_bound_vars: false,
            maybe_is_allowed: true,
            map_comprehensions_are_allowed: true,
            deterministic: false,
        }
    }
    pub fn from_config(config: &crate::Config) -> Self {
        Context {
            allowed_size: config.max_size,
            max_recursion_depth: config.max_recursion_depth,
            maybe_is_allowed: !config.disable_maybe,
            map_comprehensions_are_allowed: !config.disable_map_comprehensions,
            deterministic: config.deterministic,
            ..Context::new()
        }
    }
    pub fn may_recurse(&self) -> bool {
        self.recursion_depth < self.max_recursion_depth && self.allowed_size > 0
    }
    pub fn for_recursion_with_spent_size(&self, size_reduction: i32) -> Self {
        Context {
            recursion_depth: self.recursion_depth + 1,
            allowed_size: self.allowed_size - size_reduction,
            ..*self
        }
    }
    pub fn in_guard(&self) -> Self {
        Context {
            is_in_guard: true,
            ..*self
        }
    }
    pub fn in_bitstring_generator(&self) -> Self {
        Context {
            is_in_bitstring_generator: true,
            ..*self
        }
    }
    pub fn in_record_initializer(&self) -> Self {
        Context {
            is_in_record_initializer: true,
            ..*self
        }
    }
    pub fn ban_bound_vars(&self) -> Self {
        Context {
            no_bound_vars: true,
            ..*self
        }
    }
}
