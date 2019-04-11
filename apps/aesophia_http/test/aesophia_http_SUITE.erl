-module(aesophia_http_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% Endpoint calls
-export([http_request/4]).

%% test case exports
%% external endpoints
-export([ identity_contract/1
        , identity_aci/1
        , include_contract/1
        , include_aci/1
        , legacy_decode_data/1
        , encode_calldata/1
        , decode_calldata_bytecode/1
        , decode_calldata_source/1
        , get_api/1
        , get_api_version/1
        , get_version/1
        ]).

all() ->
    [
     {group, contracts}
    ].

groups() ->
    [
     {contracts, [],
      [ identity_contract
      , identity_aci
      , include_contract
      , include_aci
      , legacy_decode_data
      , encode_calldata
      , decode_calldata_bytecode
      , decode_calldata_source
      , get_api
      , get_api_version
      , get_version
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Res = application:ensure_all_started(aesophia_http),
    io:format("RESULT: ~p\n", [Res]),
    Config.

end_per_suite(_Config) ->
    ok.


init_per_group(contracts, Config) -> Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% ============================================================
%% Test cases
%% ============================================================

identity_contract(_Config) ->
    %% Compile test contract "identity.aes"
    _Code = compile_test_contract("identity"),

    ok.

identity_aci(_Config) ->
    #{<<"encoded_aci">> := ACI, <<"interface">> := Prototype} =
        create_aci("identity"),

    ?assertMatch(<<"contract Identity =\n  function main : (int) => int\n">>, Prototype),

    ?assertMatch(#{<<"contract">> := _C}, ACI),

    ok.

include_contract(_Config) ->
    Dir = contract_dir(),
    Files = ["included.aes", "../contracts/included2.aes"],
    ExplicitFileSystem =
        maps:from_list(
            [ begin
                {ok, F} = file:read_file(filename:join(Dir, Name)),
                {list_to_binary(Name), F}
              end || Name <- Files ]),
    Opts = #{file_system => ExplicitFileSystem, src_file => <<"include.aes">>},

    _Code = compile_test_contract(Dir, "include", Opts),

    ok.

include_aci(_Config) ->
%% TODO: Implement this
%%     Dir = contract_dir(),
%%     Files = ["included.aes", "../contracts/included2.aes"],
%%     ExplicitFileSystem =
%%         maps:from_list(
%%             [ begin
%%                 {ok, F} = file:read_file(filename:join(Dir, Name)),
%%                 {list_to_binary(Name), F}
%%               end || Name <- Files ]),
%%     Opts = #{file_system => ExplicitFileSystem},

%%     _ACI = create_aci(Dir, "include", Opts),

    ok.

legacy_decode_data(_Config) ->
    Int42 = <<"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY">>,
    Type  = <<"int">>,
    ?assertMatch(#{ <<"type">> := <<"word">>, <<"value">> := 42},
                 decode_data(Type, Int42)),

    ok.

encode_calldata(_Config) ->
    Src = fun(Ts) ->
            lists:flatten(
                ["contract Dummy =\n",
                 "  type an_alias('a) = (string, 'a)\n"
                 "  record r = {x : an_alias(int), y : variant}\n"
                 "  datatype variant = Red | Blue(map(string, int))\n"
                 "  function foo : (", string:join(Ts, ", "), ") => int\n" ])
          end,

    encode_calldata(Src(["int", "string"]), "foo", ["42", "\"foo\""]),
    encode_calldata(Src(["variant", "r"]), "foo", ["Blue({[\"a\"] = 4})", "{x = (\"b\", 5), y = Red}"]),

    ok.

decode_calldata_bytecode(_Config) ->
    {ok, ContractSrcBin} = read_test_contract("calldata"),
    ContractSrc = binary_to_list(ContractSrcBin),
    Contract = compile_test_contract("calldata"),

    Data1 = encode_calldata(ContractSrc, "foo", ["42"]),
    Data2 = encode_calldata(ContractSrc, "bar", []),
    Data3 = encode_calldata(ContractSrc, "baz", ["(42, 43)", "\"hello\""]),

    DoDec = fun(Data) -> do_decode_calldata_bytecode(#{calldata => Data, bytecode => Contract}) end,

    {<<"foo">>, [#{<<"value">> := 42}]} = DoDec(Data1),
    {<<"bar">>, []} = DoDec(Data2),
    {<<"baz">>, [#{<<"value">> := [#{<<"value">> := 42}, #{<<"value">> := 43}]},
                 #{<<"value">> := <<"hello">>}]} = DoDec(Data3),

    ok.

decode_calldata_source(_Config) ->
    {ok, ContractSrcBin} = read_test_contract("calldata"),
    ContractSrc = binary_to_list(ContractSrcBin),

    Data1 = encode_calldata(ContractSrc, "foo", ["42"]),
    Data2 = encode_calldata(ContractSrc, "bar", []),
    Data3 = encode_calldata(ContractSrc, "baz", ["(42, 43)", "\"hello\""]),

    DoDec = fun(F, Data) -> do_decode_calldata_source(#{calldata => Data, function => F, source => ContractSrcBin}) end,

    {<<"foo">>, [#{<<"value">> := "42"}]} = DoDec(<<"foo">>, Data1),
    {<<"bar">>, []} = DoDec(<<"bar">>, Data2),
    {<<"baz">>, [#{<<"value">> := "(42, 43)"},
                 #{<<"value">> := "\"hello\""}]} = DoDec(<<"baz">>, Data3),

    ok.

get_api_version(_Config) ->
    {ok, 200, #{<<"api-version">> := Vsn}} = get_api_version(),
    ?assertMatch({X, X}, {Vsn, <<"2.1.0">>}),

    ok.

get_version(_Config) ->
    {ok, 200, #{<<"version">> := Vsn}} = get_version(),
    ?assertMatch({X, X}, {{ok, Vsn}, aeso_compiler:version()}),

    ok.

get_api(_Config) ->
    {ok, 200, #{<<"swagger">> := _Swagger}} = get_api(),

    ok.

%% Contract interface functions.
contract_dir() ->
    filename:join(code:lib_dir(aesophia_http), "../../extras/test/contracts").

compile_test_contract(Name) ->
    Dir = contract_dir(),
    compile_test_contract(Dir, Name).

compile_test_contract(Dir, Name) ->
    compile_test_contract(Dir, Name, #{}).

read_test_contract(Name) ->
    FileName = filename:join(contract_dir(), Name ++ ".aes"),
    file:read_file(FileName).

compile_test_contract(Dir, Name, Opts) ->
    FileName = filename:join(Dir, Name ++ ".aes"),
    {ok, SophiaCode} = file:read_file(FileName),
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode, Opts),
    Code.

create_aci(Name) ->
    Dir = contract_dir(),
    create_aci(Dir, Name).

create_aci(Dir, Name) ->
    create_aci(Dir, Name, #{}).

create_aci(Dir, Name, Opts) ->
    FileName = filename:join(Dir, Name ++ ".aes"),
    {ok, SophiaCode} = file:read_file(FileName),
    {ok, 200, ACI = #{}} = get_aci(SophiaCode, Opts),
    ACI.

decode_data(Type, EncodedData) ->
    {ok, 200, #{<<"data">> := DecodedData}} =
         get_contract_decode_data(#{'sophia-type' => Type,
                                    data => EncodedData}),
    DecodedData.

encode_calldata(Src, Fun, Args) ->
    {ok, 200, #{<<"calldata">> := Data}} =
         get_encode_calldata(#{source => list_to_binary(Src),
                               function => list_to_binary(Fun),
                               arguments => lists:map(fun list_to_binary/1, Args)}),
    Data.

do_decode_calldata_bytecode(Map) ->
    {ok, 200, #{<<"function">> := FName, <<"arguments">> := Args}} =
        get_decode_calldata_bytecode(Map),
    {FName, Args}.

do_decode_calldata_source(Map) ->
    {ok, 200, #{<<"function">> := FName, <<"arguments">> := Args}} =
        get_decode_calldata_source(Map),
    {FName, Args}.

%% ============================================================
%% HTTP Requests
%% Note that some are internal and some are external!
%% ============================================================

get_contract_bytecode(SourceCode, Opts) ->
    Host = internal_address(),
    http_request(Host, post, "compile",
                 #{ <<"code">> => SourceCode, <<"options">> => Opts }).

get_aci(SourceCode, Opts) ->
    Host = internal_address(),
    http_request(Host, post, "aci",
                 #{ <<"code">> => SourceCode, <<"options">> => Opts }).

get_contract_decode_data(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-data", Request).

get_encode_calldata(Request) ->
    Host = internal_address(),
    http_request(Host, post, "encode-calldata", Request).

get_decode_calldata_bytecode(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-calldata/bytecode", Request).

get_decode_calldata_source(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-calldata/source", Request).

get_api_version() ->
    Host = internal_address(),
    http_request(Host, get, "api-version", []).

get_version() ->
    Host = internal_address(),
    http_request(Host, get, "version", []).

get_api() ->
    Host = internal_address(),
    http_request(Host, get, "api", []).

%% ============================================================
%% private functions
%% ============================================================

internal_address() ->
    {ok, Port} = application:get_env(aesophia_http, port),
    "http://127.0.0.1:" ++ integer_to_list(Port).

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result = case iolist_to_binary(Body) of
                             <<>> -> #{};
                             BodyB ->
                                 jsx:decode(BodyB, [return_maps])
                         end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.
