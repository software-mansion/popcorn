-define(key(M, K), maps:get(K, M)).
-define(ann(Meta), elixir_erl:get_ann(Meta)).
-define(line(Meta), elixir_utils:get_line(Meta)).
-define(generated(Meta), elixir_utils:generated(Meta)).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args),
    {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}
).

-record(elixir_ex, {
    %% stores if __CALLER__ is allowed
    caller = false,
    %% TODO: Remove warn and everywhere it is set in v2.0

    %% {Read, Counter, {bitsize, Original} | none} | warn | raise | pin
    prematch = raise,
    %% stores if __STACKTRACE__ is allowed
    stacktrace = false,
    %% a map of unused vars and a version counter for vars
    unused = {#{}, 0},
    %% a list of modules defined in functions (runtime)
    runtime_modules = [],
    %% a tuple with maps of read and optional write current vars
    vars = {#{}, false}
}).

-record(elixir_erl, {
    %% can be match, guards or nil
    context = nil,
    %% extra information about the context, like pin_guard and map_key
    extra = nil,
    %% when true, it means caller was invoked
    caller = false,
    %% maps of defined variables and their alias
    var_names = #{},
    %% extra guards from args expansion
    extra_guards = [],
    %% a map counting the variables defined
    counter = #{},
    %% a boolean to control if captures should be expanded
    expand_captures = false,
    %% holds information about the stacktrace variable
    stacktrace = nil
}).

-record(elixir_tokenizer, {
    terminators = [],
    unescape = true,
    cursor_completion = false,
    existing_atoms_only = false,
    static_atoms_encoder = nil,
    preserve_comments = nil,
    identifier_tokenizer = elixir_tokenizer,
    ascii_identifiers_only = true,
    indentation = 0,
    column = 1,
    mismatch_hints = [],
    warnings = []
}).
