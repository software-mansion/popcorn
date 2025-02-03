/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::sync::OnceLock;
// Patch: Commented functions and arguments that are not yet supported by AtomVM

use crate::environment::CallLocality;
use crate::environment::CallLocality::*;
use crate::environment::Determinism;
use crate::environment::Determinism::*;
use crate::environment::GuardContext;
use crate::environment::GuardContext::*;
use crate::types::TypeApproximation::*;
use crate::types::*;

static ERLANG_FUNCTIONS: OnceLock<
    Vec<(
        &'static str,
        Determinism,
        GuardContext,
        CallLocality,
        TypeApproximation,
        Vec<TypeApproximation>,
    )>,
> = OnceLock::new();

#[rustfmt::skip]
pub fn get_erlang_functions() -> &'static [(
        &'static str,
        Determinism,
        GuardContext,
        CallLocality,
        TypeApproximation,
        Vec<TypeApproximation>,)] {
    let alias_opts = || List(Box::new(Union(vec![
        SpecificAtom("explicit_unalias".to_string()),
        SpecificAtom("reply".to_string()),
    ])));
    let encoding_type = || Union(vec![SpecificAtom("latin1".to_string()), SpecificAtom("unicode".to_string()), SpecificAtom("utf8".to_string())]);
    // let binary_to_term_opts = || List(Box::new(Union(vec![SpecificAtom("safe".to_string()), SpecificAtom("used".to_string())])));
    let true_type = || SpecificAtom("true".to_string());
    // let time_unit = || Union(vec![
    //     Integer,
    //     SpecificAtom("second".to_string()),
    //     SpecificAtom("millisecond".to_string()),
    //     SpecificAtom("microsecond".to_string()),
    //     SpecificAtom("nanosecond".to_string()),
    //     SpecificAtom("native".to_string()),
    //     SpecificAtom("perf_counter".to_string()),
    // ]);
    let delete_module_return = || Union(vec![SpecificAtom("undefined".to_string()), SpecificAtom("true".to_string())]);
    // let external_size_opts = || List(Box::new(Union(vec![
    //     SpecificAtom("compressed".to_string()),
    //     SpecificAtom("deterministic".to_string()),
    //     SpecificAtom("local".to_string()),
    //     // {compressed, Level :: 0..9}
    //     // {minor_version, Version :: 0..2}
    // ])));
    let float_printing_opts = || List(Box::new(Union(vec![
        SpecificAtom("compact".to_string()),
        SpecificAtom("short".to_string()),
        // {decimals, Decimals :: 0..253}
        // {scientific, Decimals :: 0..249}
    ])));
    let fun_info_item = || Union(vec![
        SpecificAtom("arity".to_string()),
        SpecificAtom("env".to_string()),
        // SpecificAtom("index".to_string()),
        SpecificAtom("name".to_string()),
        SpecificAtom("module".to_string()),
        // SpecificAtom("new_index".to_string()),
        // SpecificAtom("new_uniq".to_string()),
        // SpecificAtom("pid".to_string()),
        SpecificAtom("type".to_string()),
        // SpecificAtom("uniq".to_string()),
    ]);
    let memory_type = || Union(vec![
        // SpecificAtom("total".to_string()),
        // SpecificAtom("processes".to_string()),
        // SpecificAtom("processes_used".to_string()),
        // SpecificAtom("system".to_string()),
        // SpecificAtom("atom".to_string()),
        // SpecificAtom("atom_used".to_string()),
        SpecificAtom("binary".to_string()),
        // SpecificAtom("code".to_string()),
        // SpecificAtom("ets".to_string()),
    ]);
    let unique_integer_opts = || List(Box::new(Union(vec![
        SpecificAtom("positive".to_string()),
        SpecificAtom("monotonic".to_string()),
    ])));
    // let trace_pid_port_spec = || Union(vec![
    //     Pid,
    //     Port,
    //     SpecificAtom("all".to_string()),
    //     SpecificAtom("processes".to_string()),
    //     SpecificAtom("ports".to_string()),
    //     SpecificAtom("existing".to_string()),
    //     SpecificAtom("existing_processes".to_string()),
    //     SpecificAtom("existing_ports".to_string()),
    //     SpecificAtom("new".to_string()),
    //     SpecificAtom("new_processes".to_string()),
    //     SpecificAtom("new_ports".to_string()),
    // ]);
    // let trace_flags = || Union(vec![
    //     SpecificAtom("all".to_string()),
    //     SpecificAtom("send".to_string()),
    //     SpecificAtom("'receive'".to_string()),
    //     SpecificAtom("procs".to_string()),
    //     SpecificAtom("ports".to_string()),
    //     SpecificAtom("call".to_string()),
    //     SpecificAtom("arity".to_string()),
    //     SpecificAtom("return_to".to_string()),
    //     SpecificAtom("silent".to_string()),
    //     SpecificAtom("running".to_string()),
    //     SpecificAtom("exiting".to_string()),
    //     SpecificAtom("running_procs".to_string()),
    //     SpecificAtom("running_ports".to_string()),
    //     SpecificAtom("garbage_collection".to_string()),
    //     SpecificAtom("timestamp".to_string()),
    //     SpecificAtom("cpu_timestamp".to_string()),
    //     SpecificAtom("monotonic_timestamp".to_string()),
    //     SpecificAtom("strict_monotonic_timestamp".to_string()),
    //     SpecificAtom("set_on_spawn".to_string()),
    //     SpecificAtom("set_on_first_spawn".to_string()),
    //     SpecificAtom("set_on_link".to_string()),
    //     SpecificAtom("set_on_first_link".to_string()),
    //     Tuple(vec![SpecificAtom("tracer".to_string()), Union(vec![Pid, Port])]),
    //     // {tracer, module(), term()}
    // ]);

    ERLANG_FUNCTIONS.get_or_init(|| vec![
    ("abs", DeterministicOnly, InGuard, Local, Float, vec![Float]),
    ("abs", DeterministicOnly, InGuard, Local, Integer, vec![Integer]),
    ("adler32", DeterministicOnly, NotInGuard, Remote, Integer, vec![List(Box::new(Any))]), // actually iodata
    ("adler32", DeterministicOnly, NotInGuard, Remote, Integer, vec![Integer, List(Box::new(Any))]), // List(Box::new(Any)) is actually iodata
    ("adler32_combine", DeterministicOnly, NotInGuard, Remote, Integer, vec![Integer, Integer, Integer]),
    ("alias", AnyDeterminism, NotInGuard, Local, Ref, vec![]),
    ("alias", AnyDeterminism, NotInGuard, Local, Ref, vec![alias_opts()]),
    // TODO: better type for append_element
    // ("append_element", DeterministicOnly, NotInGuard, Remote, AnyTuple, vec![AnyTuple, Any]),
    ("apply", DeterministicOnly, NotInGuard, Local, Any, vec![Fun, Any]),
    // ("apply", DeterministicOnly, NotInGuard, Local, Any, vec![Atom, Atom, Any]),
    ("atom_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![Atom]),
    ("atom_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![Atom, encoding_type()]),
    ("atom_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Integer)), vec![Atom]),
    // ("binary_part", DeterministicOnly, InGuard, Local, Bitstring, vec![Bitstring, Tuple(vec![Integer, Integer])]),
    ("binary_part", DeterministicOnly, InGuard, Local, Bitstring, vec![Bitstring, Integer, Integer]),
    ("binary_to_atom", DeterministicOnly, NotInGuard, Local, Atom, vec![Bitstring]),
    ("binary_to_atom", DeterministicOnly, NotInGuard, Local, Atom, vec![Bitstring, encoding_type()]),
    ("binary_to_existing_atom", AnyDeterminism, NotInGuard, Local, Atom, vec![Bitstring]),
    ("binary_to_existing_atom", AnyDeterminism, NotInGuard, Local, Atom, vec![Bitstring, encoding_type()]),
    ("binary_to_float", DeterministicOnly, NotInGuard, Local, Float, vec![Bitstring]),
    ("binary_to_integer", DeterministicOnly, NotInGuard, Local, Integer, vec![Bitstring]),
    ("binary_to_integer", DeterministicOnly, NotInGuard, Local, Atom, vec![Bitstring, Integer]),
    ("binary_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Integer)), vec![Bitstring]),
    // ("binary_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Integer)), vec![Bitstring, Integer, Integer]),
    // ("binary_to_term", DeterministicOnly, NotInGuard, Local, Any, vec![Bitstring]),
    // ("binary_to_term", DeterministicOnly, NotInGuard, Local, Any, vec![Bitstring, binary_to_term_opts()]),
    ("bit_size", DeterministicOnly, NotInGuard, Local, Integer, vec![Bitstring]),
    // ("bitstring_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Union(vec![Integer, Bitstring]))), vec![Bitstring]),
    // ("bump_reductions", DeterministicOnly, NotInGuard, Remote, true_type(), vec![Integer]),
    ("byte_size", DeterministicOnly, InGuard, Local, Integer, vec![Bitstring]),
    // cancel_timer
    ("ceil", DeterministicOnly, InGuard, Local, Integer, vec![Number]),
    // check_old_code, check_process_code
    // ("convert_time_unit", DeterministicOnly, NotInGuard, Remote, Integer, vec![Integer, time_unit(), time_unit()]),
    // crc32, crc32_combine
    ("date", AnyDeterminism, NotInGuard, Local, Tuple(vec![Integer, Integer, Integer]), vec![]),
    // decode_packet: ridiculously complex, would need a dedicated fuzzer !
    // TODO: better type for delete_element, element
    ("delete_element", DeterministicOnly, NotInGuard, Remote, AnyTuple, vec![AnyTuple, Integer]),
    ("delete_module", DeterministicOnly, NotInGuard, Local, delete_module_return(), vec![Atom]),
    // demonitor, disconnect_node, display_term, dist_ctrl_..., 
    ("element", DeterministicOnly, InGuard, Local, Any, vec![AnyTuple, Integer]),
    // ("erase", AnyDeterminism, NotInGuard, Local, List(Box::new(Tuple(vec![Any, Any]))), vec![]),
    ("erase", DeterministicOnly, NotInGuard, Local, Any, vec![Any]), // Value | undefined
    // error, exit
    // ("external_size", AnyDeterminism, NotInGuard, Remote, Integer, vec![Any]),
    // ("external_size", AnyDeterminism, NotInGuard, Remote, Integer, vec![Any, external_size_opts()]),
    ("float", DeterministicOnly, InGuard, Local, Float, vec![Number]),
    ("float_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![Float]),
    ("float_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![Float, float_printing_opts()]),
    ("float_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Integer)), vec![Float]),
    ("float_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Integer)), vec![Float, float_printing_opts()]),
    ("floor", DeterministicOnly, InGuard, Local, Integer, vec![Number]),
    // ("fun_info", AnyDeterminism, NotInGuard, Remote, List(Box::new(Tuple(vec![fun_info_item(), Any]))), vec![Fun]),
    ("fun_info", AnyDeterminism, NotInGuard, Remote, Tuple(vec![fun_info_item(), Any]), vec![Fun, fun_info_item()]),
    ("fun_to_list", AnyDeterminism, NotInGuard, Remote, List(Box::new(Integer)), vec![Fun]),
    ("function_exported", DeterministicOnly, NotInGuard, Remote, boolean_type(), vec![Atom, Atom, Integer]), // M, F, A
    ("garbage_collect", DeterministicOnly, NotInGuard, Local, true_type(), vec![]),
    ("garbage_collect", DeterministicOnly, NotInGuard, Local, true_type(), vec![Pid]),
    // ("garbage_collect", DeterministicOnly, NotInGuard, Local, boolean_type(), vec![Pid, List]),
    // ("get", AnyDeterminism, NotInGuard, Local, List(Box::new(Tuple(vec![Any, Any]))), vec![]),
    // ("get", AnyDeterminism, NotInGuard, Local, Any, vec![Any]),
    // ("get_cookie", DeterministicOnly, NotInGuard, Remote, Atom, vec![]),
    // ("get_cookie", DeterministicOnly, NotInGuard, Remote, Atom, vec![Node]),
    // ("get_keys", AnyDeterminism, NotInGuard, Local, List(Box::new(Any)), vec![]),
    // ("get_keys", AnyDeterminism, NotInGuard, Local, List(Box::new(Any)), vec![Any]),
    ("group_leader", AnyDeterminism, NotInGuard, Local, Pid, vec![]),
    ("group_leader", DeterministicOnly, NotInGuard, Local, boolean_type(), vec![Pid, Pid]),
    // "halt", we're not interested in stopping the VM during test
    ("hd", DeterministicOnly, InGuard, Local, Any, vec![List(Box::new(Any))]),
    // hibernate
    ("insert_element", DeterministicOnly, NotInGuard, Remote, AnyTuple, vec![Integer, AnyTuple, Any]),
    ("integer_to_binary", DeterministicOnly, NotInGuard, Remote, Bitstring, vec![Integer]),
    ("integer_to_binary", DeterministicOnly, NotInGuard, Remote, Bitstring, vec![Integer, Integer]),
    ("integer_to_list", DeterministicOnly, NotInGuard, Remote, List(Box::new(Integer)), vec![Integer]),
    ("integer_to_list", DeterministicOnly, NotInGuard, Remote, List(Box::new(Integer)), vec![Integer, Integer]),
    ("iolist_size", DeterministicOnly, NotInGuard, Local, Integer, vec![List(Box::new(Any))]),
    ("iolist_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![List(Box::new(Any))]),
    // ("iolist_to_iovec", DeterministicOnly, NotInGuard, Remote, Bitstring, vec![List(Box::new(Any))]),
    // ("is_alive", AnyDeterminism, NotInGuard, Local, boolean_type(), vec![]),
    ("is_atom", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_binary", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_bitstring", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_boolean", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    // is_builtin
    ("is_float", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_function", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    // ("is_function", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any, Integer]),
    ("is_integer", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_list", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_map", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_map_key", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any, Map]),
    ("is_number", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_pid", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    // ("is_port", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_process_alive", AnyDeterminism, NotInGuard, Local, boolean_type(), vec![Pid]),
    // These two are rejected by the compiler if the record name is a literal that does not match any record.
    // ("is_record", AnyDeterminism, NotInGuard, Local, boolean_type(), vec![Any, Atom]),
    // ("is_record", AnyDeterminism, NotInGuard, Local, boolean_type(), vec![Any, Atom, Integer]),
    ("is_reference", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("is_tuple", DeterministicOnly, InGuard, Local, boolean_type(), vec![Any]),
    ("length", DeterministicOnly, InGuard, Local, Integer, vec![List(Box::new(Any))]),
    // link
    ("list_to_atom", DeterministicOnly, NotInGuard, Local, Atom, vec![List(Box::new(Integer))]),
    ("list_to_binary", DeterministicOnly, NotInGuard, Local, Bitstring, vec![List(Box::new(Any))]), // IoList
    // ("list_to_bitstring", DeterministicOnly, NotInGuard, Local, Bitstring, vec![List(Box::new(Any))]), // IoList, last one may be a bitstring
    // list_to_existing_atom, list_to_float/integer/pid/port/ref/tuple, load_module, load_nif
    // ("loaded", AnyDeterminism, NotInGuard, Remote, List(Box::new(Atom)), vec![]),
    ("localtime", AnyDeterminism, NotInGuard, Remote, Tuple(vec![Tuple(vec![Integer, Integer, Integer]), Tuple(vec![Integer, Integer, Integer])]), vec![]),
    // localtime_to_universaltime
    ("make_ref", AnyDeterminism, NotInGuard, Local, Ref, vec![]),
    ("make_tuple", DeterministicOnly, NotInGuard, Remote, AnyTuple, vec![Integer, Any]),
    // ("make_tuple", DeterministicOnly, NotInGuard, Remote, AnyTuple, vec![Integer, Any, List(Box::new(Tuple(vec![Integer, Any])))]),
    // map_get
    ("map_size", DeterministicOnly, InGuard, Local, Integer, vec![Map]),
    // match_spec_test
    ("max", DeterministicOnly, InGuard, Local, Any, vec![Any, Any]),
    ("md5", DeterministicOnly, NotInGuard, Remote, Bitstring, vec![Bitstring]), // actually takes an iolist
    // various incremental md5 functions
    // ("memory", AnyDeterminism, NotInGuard, Remote, List(Box::new(Tuple(vec![memory_type(), Integer]))), vec![]),
    ("memory", AnyDeterminism, NotInGuard, Remote, Integer, vec![memory_type()]),
    // ("memory", AnyDeterminism, NotInGuard, Remote, List(Box::new(Tuple(vec![memory_type(), Integer]))), vec![List(Box::new(memory_type()))]),
    ("min", DeterministicOnly, InGuard, Local, Any, vec![Any, Any]),
    // module_loaded, monitor, monitor_node
    ("monotonic_time", AnyDeterminism, NotInGuard, Remote, Integer, vec![]),
    // monotonic_time/1, nif_error
    // ("node", DeterministicOnly, InGuard, Local, Atom, vec![]),
    // ("node", DeterministicOnly, InGuard, Local, Atom, vec![Union(vec![Pid, Port, Ref])]),
    // ("nodes", AnyDeterminism, NotInGuard, Local, List(Box::new(Atom)), vec![]),
    // nodes/1, nodes/2
    // ("now", AnyDeterminism, NotInGuard, Local, Tuple(vec![Integer, Integer, Integer]), vec![]),
    // open_port, phash
    // ("phash2", AnyDeterminism, NotInGuard, Remote, Integer, vec![Any]),
    // ("phash2", AnyDeterminism, NotInGuard, Remote, Integer, vec![Any, Integer]),
    ("pid_to_list", AnyDeterminism, NotInGuard, Local, List(Box::new(Integer)), vec![Pid]),
    // port_call, port_command, port_close, port_connect, port_info
    // ("port_to_list", AnyDeterminism, NotInGuard, Local, List(Box::new(Integer)), vec![Port]),
    // ("ports", AnyDeterminism, NotInGuard, Remote, List(Box::new(Port)), vec![]),
    // ("pre_loaded", AnyDeterminism, NotInGuard, Local, List(Box::new(Atom)), vec![]),
    // process_display, process_flag
    ("process_info", AnyDeterminism, NotInGuard, Local, List(Box::new(AnyTuple)), vec![Pid]),
    // process_info/2
    ("processes", AnyDeterminism, NotInGuard, Local, List(Box::new(Pid)), vec![]),
    // purge_module
    ("put", DeterministicOnly, NotInGuard, Local, Any, vec![Any, Any]),
    // raise, read_timer,
    ("ref_to_list", AnyDeterminism, NotInGuard, Local, List(Box::new(Integer)), vec![Ref]),
    ("register", DeterministicOnly, NotInGuard, Local, true_type(), vec![Atom, Union(vec![Port, Pid])]),
    // ("registered", AnyDeterminism, NotInGuard, Local, List(Box::new(Atom)), vec![]),
    // resume_process
    ("round", DeterministicOnly, InGuard, Local, Integer, vec![Number]),
    // send/2, send/3, send_after, send_nosuspend
    ("self", AnyDeterminism, InGuard, Local, Pid, vec![]),
    // ("set_cookie", DeterministicOnly, NotInGuard, Remote, boolean_type(), vec![Atom]),
    // set_cookie/2
    ("setelement", DeterministicOnly, NotInGuard, Local, AnyTuple, vec![Integer, AnyTuple, Any]),
    ("size", DeterministicOnly, NotInGuard, Local, Integer, vec![AnyTuple]),
    ("size", DeterministicOnly, NotInGuard, Local, Integer, vec![List(Box::new(Any))]),
    // spawn, spawn_link, spawn_monitor, spawn_opt, spawn_request, spawn_request_abandon
    ("split_binary", DeterministicOnly, NotInGuard, Local, Tuple(vec![Bitstring, Bitstring]), vec![Bitstring, Integer]),
    // start_timer, statistics, suspend_process, system_flag, system_info, system_monitor, system_profile
    ("term_to_binary", AnyDeterminism, NotInGuard, Local, Bitstring, vec![Any]),
    // ("term_to_iovec", AnyDeterminism, NotInGuard, Local, List(Box::new(Any)), vec![Any]),
    // term_to_binary/2, term_to_iovec/2
    ("throw", DeterministicOnly, NotInGuard, Local, Any, vec![Any]), // TODO: get cleaner handling of Bottom vs Any
    // ("time", AnyDeterminism, NotInGuard, Local, Tuple(vec![Integer, Integer, Integer]), vec![]),
    // ("time_offset", AnyDeterminism, NotInGuard, Remote, Integer, vec![]),
    // ("time_offset", AnyDeterminism, NotInGuard, Remote, Integer, vec![time_unit()]),
    ("timestamp", AnyDeterminism, NotInGuard, Remote, Tuple(vec![Integer, Integer, Integer]), vec![]),
    ("tl", DeterministicOnly, InGuard, Local, Any, vec![List(Box::new(Any))]),
    // ("trace", AnyDeterminism, NotInGuard, Remote, Integer, vec![trace_pid_port_spec(), boolean_type(), trace_flags()]),
    // ("trace_delivered", AnyDeterminism, NotInGuard, Remote, Ref, vec![Union(vec![Pid, SpecificAtom("all".to_string())])]),
    // trace_info, trace_pattern
    ("trunc", DeterministicOnly, InGuard, Local, Integer, vec![Number]),
    ("tuple_size", DeterministicOnly, InGuard, Local, Integer, vec![AnyTuple]),
    ("tuple_to_list", DeterministicOnly, NotInGuard, Local, List(Box::new(Any)), vec![AnyTuple]),
    // ("unalias", AnyDeterminism, NotInGuard, Local, boolean_type(), vec![Ref]),
    ("unique_integer", AnyDeterminism, NotInGuard, Remote, Integer, vec![]),
    ("unique_integer", AnyDeterminism, NotInGuard, Remote, Integer, vec![unique_integer_opts()]),
    ("universaltime", AnyDeterminism, NotInGuard, Remote, Tuple(vec![Tuple(vec![Integer, Integer, Integer]), Tuple(vec![Integer, Integer, Integer])]), vec![]),
    // universaltime_to_localtime, unlink, 
    ("whereis", AnyDeterminism, NotInGuard, Local, Union(vec![SpecificAtom("undefined".to_string()), Port, Pid]), vec![Atom]),
    // ("yield", DeterministicOnly, NotInGuard, Remote, boolean_type(), vec![]),
    ])
}

static LIST_FUNCTIONS: OnceLock<Vec<(&'static str, TypeApproximation, Vec<TypeApproximation>)>> =
    OnceLock::new();

#[rustfmt::skip]
pub fn get_list_functions() -> &'static [(&'static str, TypeApproximation, Vec<TypeApproximation>)] {
    LIST_FUNCTIONS.get_or_init(|| vec![
        // all, any
        ("append", List(Box::new(Any)), vec![List(Box::new(List(Box::new(Any))))]),
        // ("append", List(Box::new(Any)), vec![List(Box::new(Any)), List(Box::new(Any))]),
        // Actual type is [atom() | integer() | float() | string()] -> string()
        ("concat", List(Box::new(Integer)), vec![List(Box::new(Any))]),
        ("delete", List(Box::new(Any)), vec![List(Box::new(Any)), Any]),
        ("droplast", List(Box::new(Any)), vec![List(Box::new(Any))]),
        // dropwhile
        ("duplicate", List(Box::new(Any)), vec![Integer, Any]),
        // ("enumerate", List(Box::new(Tuple(vec![Integer, Any]))), vec![List(Box::new(Any))]),
        // ("enumerate", List(Box::new(Tuple(vec![Integer, Any]))), vec![List(Box::new(Any)), Integer]),
        // ("enumerate", List(Box::new(Tuple(vec![Integer, Any]))), vec![List(Box::new(Any)), Integer, Integer]),
        // filter, filtermap
        // ("flatlength", Integer, vec![List(Box::new(Any))]),
        // flatmap
        ("flatten", List(Box::new(Any)), vec![List(Box::new(Any))]),
        // ("flatten", List(Box::new(Any)), vec![List(Box::new(Any)), List(Box::new(Any))]),
        // foldl, foldr
        ("join", List(Box::new(Any)), vec![Any, List(Box::new(Any))]),
        // foreach, keydelete, keyfind, keymap, key...
        ("last", Any, vec![List(Box::new(Any))]),
        // map, mapfoldl, mapfoldr
        // ("max", Any, vec![List(Box::new(Any))]),
        ("member", boolean_type(), vec![Any, List(Box::new(Any))]),
        // merge, merge3
        // ("min", Any, vec![List(Box::new(Any))]),
        ("nth", Any, vec![Integer, List(Box::new(Any))]),
        ("nthtail", List(Box::new(Any)), vec![Integer, List(Box::new(Any))]),
        // partition
        // ("prefix", boolean_type(), vec![List(Box::new(Any)), List(Box::new(Any))]),
        ("reverse", List(Box::new(Any)), vec![List(Box::new(Any))]),
        ("reverse", List(Box::new(Any)), vec![List(Box::new(Any)), List(Box::new(Any))]),
        // search
        ("seq", List(Box::new(Integer)), vec![Integer, Integer]),
        ("seq", List(Box::new(Integer)), vec![Integer, Integer, Integer]),
        ("sort", List(Box::new(Any)), vec![List(Box::new(Any))]),
        // sort/2
        ("split", Tuple(vec![List(Box::new(Any)), List(Box::new(Any))]), vec![Integer, List(Box::new(Any))]),
        // splitwith
        ("sublist", List(Box::new(Any)), vec![List(Box::new(Any)), Integer]),
        // ("sublist", List(Box::new(Any)), vec![List(Box::new(Any)), Integer, Integer]),
        // ("substract", List(Box::new(Any)), vec![List(Box::new(Any)), List(Box::new(Any))]),
        // ("suffix", boolean_type(), vec![List(Box::new(Any)), List(Box::new(Any))]),
        // ("sum", Number, vec![List(Box::new(Number))]),
        // takewhile, ukeymerge, ukeysort, umerge, umerge3
        ("unzip", Tuple(vec![List(Box::new(Any)), List(Box::new(Any))]), vec![List(Box::new(Tuple(vec![Any, Any])))]),
        // ("unzip3", Tuple(vec![List(Box::new(Any)), List(Box::new(Any)), List(Box::new(Any))]), vec![List(Box::new(Tuple(vec![Any, Any, Any])))]),
        ("usort", List(Box::new(Any)), vec![List(Box::new(Any))]),
        // usort/2, zip, zip3, zipwith, zipwith3
        // ("uniq", List(Box::new(Any)), vec![List(Box::new(Any))]),
        // uniq/2
    ])
}

static ETS_FUNCTIONS: OnceLock<
    Vec<(
        &'static str,
        Determinism,
        TypeApproximation,
        Vec<TypeApproximation>,
    )>,
> = OnceLock::new();

#[rustfmt::skip]
pub fn get_ets_functions() -> &'static [(&'static str, Determinism, TypeApproximation, Vec<TypeApproximation>)] {
    let true_type = || SpecificAtom("true".to_string());
    // let info_item = || Union(vec![
    //     SpecificAtom("binary".to_string()),
    //     SpecificAtom("compressed".to_string()),
    //     SpecificAtom("decentralized_counters".to_string()),
    //     SpecificAtom("fixed".to_string()),
    //     SpecificAtom("heir".to_string()),
    //     SpecificAtom("id".to_string()),
    //     SpecificAtom("keypos".to_string()),
    //     SpecificAtom("memory".to_string()),
    //     SpecificAtom("name".to_string()),
    //     SpecificAtom("named_table".to_string()),
    //     SpecificAtom("node".to_string()),
    //     SpecificAtom("owner".to_string()),
    //     SpecificAtom("protection".to_string()),
    //     SpecificAtom("safe_fixed".to_string()),
    //     SpecificAtom("safe_fixed_monotonic_time".to_string()),
    //     SpecificAtom("size".to_string()),
    //     SpecificAtom("stats".to_string()),
    //     SpecificAtom("type".to_string()),
    //     SpecificAtom("write_concurrency".to_string()),
    //     SpecificAtom("read_concurrency".to_string()),
    // ]);
    let new_opts = List(Box::new(Union(vec![
        // SpecificAtom("set".to_string()), SpecificAtom("ordered_set".to_string()), SpecificAtom("bag".to_string()), SpecificAtom("duplicate_bag".to_string()),
        SpecificAtom("set".to_string()), SpecificAtom("duplicate_bag".to_string()),
        SpecificAtom("public".to_string()), SpecificAtom("protected".to_string()), SpecificAtom("private".to_string()),
        // SpecificAtom("named_table".to_string()),
        Tuple(vec![SpecificAtom("keypos".to_string()), Integer]),
        // Tuple(vec![SpecificAtom("heir".to_string()), Pid, Any]),
        // Tuple(vec![SpecificAtom("heir".to_string()), SpecificAtom("none".to_string())]),
        // Tuple(vec![SpecificAtom("write_concurrency".to_string()), boolean_type()]),
        // Tuple(vec![SpecificAtom("write_concurrency".to_string()), SpecificAtom("auto".to_string())]),
        // Tuple(vec![SpecificAtom("read_concurrency".to_string()), boolean_type()]),
        // Tuple(vec![SpecificAtom("decentralized_counters".to_string()), boolean_type()]),
        // SpecificAtom("compressed".to_string()),
    ])));
    ETS_FUNCTIONS.get_or_init(|| vec![
        // ("all", AnyDeterminism, List(Box::new(ets_table_type())), vec![]),
        ("delete", DeterministicOnly, true_type(), vec![ets_table_type()]),
        ("delete", DeterministicOnly, true_type(), vec![ets_table_type(), Any]),
        // ("delete_all_objects", DeterministicOnly, true_type(), vec![ets_table_type()]),
        ("delete_object", DeterministicOnly, true_type(), vec![ets_table_type(), AnyTuple]),
        // file2tab
        // ("first", AnyDeterminism, Any, vec![ets_table_type()]),
        // foldl, folr,
        // from_dets
        // fun2ms
        // ("give_away", DeterministicOnly, true_type(), vec![ets_table_type(), Pid, Any]),
        // i
        // this could be made even more precise, but I don't think it's worth the effort
        // ("info", AnyDeterminism, Union(vec![SpecificAtom("undefined".to_string()), List(Box::new(Tuple(vec![info_item(), Any])))]), vec![ets_table_type()]),
        // ("info", AnyDeterminism, Any, vec![ets_table_type(), info_item()]),
        // init_table
        ("insert", DeterministicOnly, boolean_type(), vec![ets_table_type(), AnyTuple]),
        ("insert", DeterministicOnly, boolean_type(), vec![ets_table_type(), List(Box::new(AnyTuple))]),
        ("insert_new", DeterministicOnly, boolean_type(), vec![ets_table_type(), AnyTuple]),
        ("insert_new", DeterministicOnly, boolean_type(), vec![ets_table_type(), List(Box::new(AnyTuple))]),
        // ("is_compiled_ms", DeterministicOnly, boolean_type(), vec![Any]),
        // ("last", AnyDeterminism, Any, vec![ets_table_type()]),
        ("lookup", AnyDeterminism, AnyTuple, vec![ets_table_type(), Any]),
        ("lookup_element", AnyDeterminism, Any, vec![ets_table_type(), Any, Integer]),
        ("lookup_element", AnyDeterminism, Any, vec![ets_table_type(), Any, Integer, Any]),
        // match/1, match/2, match/3, match_delete, match_object, match_spec_compile, match_spec_run
        ("lookup", DeterministicOnly, boolean_type(), vec![ets_table_type(), Any]),
        ("new", DeterministicOnly, ets_table_type(), vec![Atom, new_opts]),
        ("next", AnyDeterminism, Any, vec![ets_table_type(), Any]),
        // ("prev", AnyDeterminism, Any, vec![ets_table_type(), Any]),
        // ("rename", DeterministicOnly, Atom, vec![ets_table_type(), Atom]),
        // repair_continuation
        // ("safe_fixtable", DeterministicOnly, boolean_type(), vec![ets_table_type(), boolean_type()]),
        // select, select_count, select_delete, select_replace, select_reverse
        // setopts
        // ("slot", AnyDeterminism, List(Box::new(Any)), vec![ets_table_type(), Integer]),
        // tab2file
        // ("tab2list", AnyDeterminism, List(Box::new(AnyTuple)), vec![ets_table_type()]),
        // tabfile_info, table/1, table/2
        ("take", AnyDeterminism, List(Box::new(AnyTuple)), vec![ets_table_type(), Any]),
        // test_ms, to_dets, update_counter
        ("update_element", DeterministicOnly, boolean_type(), vec![ets_table_type(), Any, Tuple(vec![Integer, Any])]),
        ("update_element", DeterministicOnly, boolean_type(), vec![ets_table_type(), Any, List(Box::new(Tuple(vec![Integer, Any])))]),
        // ("whereis", AnyDeterminism, Pid, vec![ets_table_type()]),
    ])
}
