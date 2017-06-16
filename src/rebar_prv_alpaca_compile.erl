-module(rebar_prv_alpaca_compile).

-export([init/1, do/1, format_error/1, compile_dir/6]).

-define(PROVIDER, compile).
-define(NAMESPACE, alpaca).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 alpaca compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Alpaca rebar3 compiler plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Locate Alpaca compiler
    AlpacaPaths = [
                   os:getenv("ALPACA_ROOT"),
                   "/usr/lib/alpaca",
                   "/usr/local/lib/alpaca"],
    AlpacaHome = get_best_path(AlpacaPaths),
    code:add_path(AlpacaHome ++ "/beams"),
    AlpacaModules =
        [alpaca, alpaca_ast, alpaca_ast_gen, alpaca_codegen,
         alpaca_compiled_po, alpaca_error_format, alpaca_exhaustiveness,
         alpaca_parser, alpaca_scan, alpaca_scanner, alpaca_typer],
    case code:ensure_modules_loaded(AlpacaModules) of
        ok -> ok;
        _ -> abort_no_alpaca()
    end,
  
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
           end,
    TestsEnabled = [P || P <- rebar_state:current_profiles(State), P == test],
    [begin
         EBinDir = rebar_app_info:ebin_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         Opts = case dict:find(rebar_prv_alpaca, rebar_app_info:opts(AppInfo)) of
             {ok, O} -> O;
             error -> []
         end,
         SourceFiles = rebar_utils:find_files(SourceDir, ".*\\.alp\$"),
         Deps = rebar_state:all_deps(State),
         LocalBeamFiles = rebar_utils:find_files(EBinDir, ".*\\.beam\$"),
         DependencyBeamFiles = lists:flatmap(fun gather_beam_files/1, Deps),
         compile_dir(
           SourceFiles, 
           LocalBeamFiles, 
           DependencyBeamFiles, 
           EBinDir, 
           Opts,
           TestsEnabled)
     end || AppInfo <- Apps],
    {ok, State}.

compile_dir(SourceFiles, LocalBeamFiles, DependencyBeamFiles, 
            EBinDir, Opts, TestsEnabled) ->
    %% Get Alpaca version so we can display it when compiling source
    Version = proplists:get_value(version, alpaca:compiler_info()),
    
    %% Get User LOCALE so we can display translated error messages, if available
    Locale = case string:tokens(os:getenv("LANG", "en_US"), ".") of
                 [L, _] -> L;
                 L -> L
             end,
    
    %% Validate any options passed to us
    ValidOpts = is_list(Opts) andalso 
                lists:all(fun({Key, _}) when is_atom(Key) -> true;
                             (_) -> false end, Opts),
    
    case ValidOpts of
        true -> ok;
        false ->
            OptsError = io_lib:format("Invalid Alpaca options list: ~p", [Opts]),
            throw({error, {?MODULE, OptsError}})
    end,

    %% initial pass - iterate over source files, extract their dependencies,
    %% and figure out if (the files themselves) require compilation -
    %% i.e. BEAM file missing or mismatches the source hash
    FileGraph = lists:foldl(
        fun(F, Graph) ->
            {ok, Src} = file:read_file(F),
            {Mod, ModDeps} = alpaca:list_dependencies(Src),

            maps:put(Mod, {F, ModDeps, is_dirty(LocalBeamFiles, F, Src)}, Graph)
        end,
        maps:new(),
        SourceFiles),

    %% Next, we figure out which modules are dirty due to
    %% modules depended upon that require recompilation
    GetHasDirtyDeps = fun DirtyDeps(Mod, Map) ->
        {_, ModDeps, IsDirty} = maps:get(Mod, Map, {unknown, [], false}),
        case IsDirty of
            true -> true;
            false ->
                Dirty = lists:map(fun(M) -> DirtyDeps(M, Map) end, ModDeps),
                lists:any(fun(X) -> X =:= true end, Dirty)
    end
    end,

    %% We update the file graph with the list of dirty dependencies
    FileGraph2 = maps:map(
        fun(_, {_, _, true} = V) -> V;
        (Mod, {F, ModDeps, false}) ->
            {F, ModDeps, GetHasDirtyDeps(Mod, FileGraph)}
        end,
        FileGraph),

    %% Create a directed graph so we can topologically sort
    %% the modules in order of dependency
    DiGraph = digraph:new(),

    %% Each 'vertex' is a module
    lists:map(
    fun(Mod) ->
        digraph:add_vertex(DiGraph, Mod)
    end,
    maps:keys(FileGraph2)),

    %% Each 'edge' is a dependency relationship between them
    maps:map(
        fun(Mod, {_, ModDeps, _}) ->
            lists:map(fun(OtherMod) ->
                digraph:add_edge(DiGraph, OtherMod, Mod)
            end, ModDeps)
        end,
        FileGraph2),

    %% Generate the topological ordering
    Sorted = digraph_utils:topsort(DiGraph),

    %% Map the final list into .beam / .alp filepaths
    GatheredLocalFiles = lists:map(
        fun(Mod) ->
            {F, _, IsDirty} = maps:get(Mod, FileGraph2),
            case IsDirty of
            true -> F;
            false ->
                {ok, BF} = get_beam_file(F, LocalBeamFiles),
                BF
        end
        end,
        Sorted),

    %% Of course, if we don't have any .alp files in the final
    %% list, we don't even need to invoke compilation
    case lists:all(fun(F) ->
                    filename:extension(F) == ".beam"
                end,
                GatheredLocalFiles) of
        true -> ok;
        false ->
        CompileFiles = DependencyBeamFiles ++ GatheredLocalFiles,
        rebar_api:debug("Sending files to Alpaca compiler: ~p~n", [CompileFiles]),
        Sources = lists:filter(
            fun(F) ->
                filename:extension(F) == ".alp"
            end,
            GatheredLocalFiles),

        rebar_api:info(
            "Alpaca ~s: compiling ~s~n",
            [Version,
                string:join(
                    lists:map(fun filename:basename/1, Sources),
                    ", ")]),

        CompileOpts = TestsEnabled ++
                case proplists:get_value(default_imports, Opts) of
                    undefined -> [];
                    Imports -> [{default_imports, gather_imports(Imports)}]
                end,

        case alpaca:compile({files, CompileFiles}, CompileOpts) of
            {ok, Compiled} ->
                [file:write_file(filename:join(EBinDir, FileName), BeamBinary) ||
                {compiled_module, _, FileName, BeamBinary} <- Compiled];

            {error, _} = E ->
                Error = alpaca_error_format:fmt(E, Locale),
                throw({error, {?MODULE, Error}})
        end
     end.

get_beam_file(Filename, BeamFiles) ->
    [ModuleName, _] = string:tokens(filename:basename(Filename), "."),
    BeamName = "alpaca_" ++ ModuleName ++ ".beam",
    BFS = lists:filter(
        fun(F) ->
            filename:basename(F) == BeamName
        end, BeamFiles),
    case BFS of
        [BF] -> {ok, BF};
        [] -> {error, no_file};
        _ -> {error, duplicate_filename, BFS}
    end.

is_dirty(BeamFiles, Filename, SrcText) ->
    case get_beam_file(Filename, BeamFiles) of
        {ok, BeamMod} ->
            %% We have a BEAM File, compare the hashes
            SrcHash = alpaca:hash_source(unicode:characters_to_list(SrcText)),
            BeamHash = alpaca:retrieve_hash(BeamMod),
            case {SrcHash, BeamHash} of
                %% Hashes match, unchanged
                {_S, _S} -> false;
                %% Hashes differ, it has changed
                _ -> true
            end;

        {error, no_file} ->
            %% No BEAM file, compile the source file
            true
    end.

gather_beam_files(Dep) ->
    EbinDir = rebar_app_info:ebin_dir(Dep),
    PossibleAlpacaFiles = rebar_utils:find_files(EbinDir, "alpaca_.*\\.beam\$"),
    lists:filter(fun(Mod) -> case alpaca:retrieve_hash(Mod) of
                                 undefined -> false;
                                 _ -> true
                             end
                 end, PossibleAlpacaFiles).

gather_imports(Imports) ->
    ImportList = [begin
                    code:ensure_modules_loaded([Mod]),
                    case erlang:function_exported(Mod, Fun, 0) of
                        true -> 
                            erlang:apply(Mod, Fun, []);

                        false ->
                            Error = io_lib:format(
                                        "Default imports: '~p:~p/0' doesn't exist", [Mod, Fun]),
                            throw({error, {?MODULE, Error}})
                    end 
                  end|| {Mod, Fun} <- Imports],
    {Funs, Types} = lists:unzip(ImportList),
    {lists:flatten(Funs), lists:flatten(Types)}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("Alpaca compile error: \n\033[0m~s", [Reason]).


get_best_path([]) ->
    "/opt/alpaca";
get_best_path([Path | Rest]) ->
    case filelib:is_dir(Path) of
        true -> Path;
        false -> get_best_path(Rest)
    end.

abort_no_alpaca() ->
    io:put_chars(
      standard_error, "Error: Cannot find Alpaca. Please follow"
      " instructions at http://alpaca-lang.org\n"),
    halt(1, []).

    
