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
        , faulty_contract/1
        , include_contract/1
        , include_aci/1
        , include_generate_calldata/1
        , encode_calldata/1
        , decode_calldata_bytecode/1
        , decode_calldata_source/1
        , decode_call_result/1
        , decode_call_result_bytecode/1
        , decode_call_result_bytecode_not_ok/1
        , validate_byte_code/1
        , get_api/1
        , get_api_version/1
        , get_version/1
        , compiler_version/1
        , fate_assembler/1
        ]).

all() ->
    [ {group, contracts} %% default == {group, fate}
    , {group, fate}
    , {group, admin}
    ].

groups() ->
    [
     {fate, [], [{group, contracts},
                  validate_byte_code,
                  fate_assembler
                ]},
     {contracts, [],
      [ identity_contract
      , identity_aci
      , faulty_contract
      , include_contract
      , include_aci
      , include_generate_calldata
      , encode_calldata
      , decode_calldata_bytecode
      , decode_calldata_source
      , decode_call_result
      , decode_call_result_bytecode
      , decode_call_result_bytecode_not_ok
      ]},
     {admin, [],
      [ get_api
      , get_api_version
      , get_version
      , compiler_version
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

init_per_group(_Group, _Config) ->
    ok.

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
    {ok, _Code, Aci} = compile_test_contract("identity"),

    ?assertMatch([_C], Aci),

    ok.

identity_aci(_Config) ->
    #{<<"encoded_aci">> := ACI, <<"external_encoded_aci">> := ExternalACI, <<"interface">> := Prototype} =
        create_aci("identity"),
    ?assertMatch(<<"main contract Identity =\n"
                   "  entrypoint main_ : (int) => int\n">>, Prototype),

    ?assertMatch(#{<<"contract">> := _C}, ACI),
    ?assertMatch([], ExternalACI),

    ok.

faulty_contract(_Config) ->
    {error, [Err]} = compile_test_contract("faulty"),
    ?assertMatch(#{ <<"type">> := <<"parse_error">> }, Err),

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

    {ok, _Code, _Aci} = compile_test_contract(Dir, "include", Opts),

    ok.

include_aci(_Config) ->
    Dir = contract_dir(),
    Files = ["included.aes", "../contracts/included2.aes"],
    ExplicitFileSystem =
        maps:from_list(
            [ begin
                {ok, F} = file:read_file(filename:join(Dir, Name)),
                {list_to_binary(Name), F}
              end || Name <- Files ]),
    Opts = #{file_system => ExplicitFileSystem},

    _ACI = create_aci(Dir, "include", Opts),

    ok.

include_generate_calldata(_Config) ->
    Dir = contract_dir(),
    Files = ["included.aes", "../contracts/included2.aes"],
    ExplicitFileSystem =
        maps:from_list(
            [ begin
                {ok, F} = file:read_file(filename:join(Dir, Name)),
                {list_to_binary(Name), F}
              end || Name <- Files ]),
    Opts = #{file_system => ExplicitFileSystem},

    {ok, ContractSrcBin} = read_test_contract("include"),
    ContractSrc = binary_to_list(ContractSrcBin),

    encode_calldata(ContractSrc, "foo", [], Opts),

    ok.


encode_calldata(_Config) ->
    Src = fun(Ts) ->
            lists:flatten(
                ["contract Dummy =\n",
                 "  type an_alias('a) = string * 'a\n"
                 "  record r = {x : an_alias(int), y : variant}\n"
                 "  datatype variant = Red | Blue(map(string, int))\n"
                 "  entrypoint foo : (", string:join(Ts, ", "), ") => int\n" ])
          end,

    encode_calldata(Src(["int", "string"]), "foo", ["42", "\"foo\""]),
    encode_calldata(Src(["variant", "r"]), "foo", ["Blue({[\"a\"] = 4})", "{x = (\"b\", 5), y = Red}"]),

    ok.

decode_calldata_bytecode(_Config) ->
    {ok, ContractSrcBin} = read_test_contract("calldata"),
    ContractSrc = binary_to_list(ContractSrcBin),
    {ok, Contract, _Aci} = compile_test_contract("calldata"),

    DoEnc = fun(F, V) -> encode_calldata(ContractSrc, binary_to_list(F), [V]) end,
    Datas = maps:map(DoEnc, test_data()),

    DoDec = fun(_F, Data) -> do_decode_calldata_bytecode(#{calldata => Data, bytecode => Contract}) end,
    Results = maps:map(DoDec, Datas),

    Expects = json_expect(),

    Check = fun(K) -> E = {K, maps:get(K, Expects)}, V = maps:get(K, Results), ?assertEqual(E, V) end,
    [ Check(K) || K <- maps:keys(Expects) ].

json_expect() ->
    #{ <<"a">> => [#{<<"type">> => <<"tuple">>,
                     <<"value">> => [#{<<"type">> => <<"int">>,<<"value">> => 42},
                                     #{<<"type">> => <<"bool">>,<<"value">> => true},
                                     #{<<"type">> => <<"string">>,<<"value">> => <<"Hello">>},
                                     #{<<"type">> => <<"unit">>,<<"value">> => <<>>}]}]
     , <<"b">> => [#{<<"type">> => <<"tuple">>,
                     <<"value">> => [#{<<"type">> => <<"variant">>, <<"value">> => [0, #{<<"type">> => <<"int">>,<<"value">> => 12},
                                                                                       #{<<"type">> => <<"int">>,<<"value">> => 18}]},
                                     #{<<"type">> => <<"variant">>,<<"value">> => [1]}]}]
     , <<"c">> => [#{<<"type">> => <<"tuple">>,
                     <<"value">> => [#{<<"type">> => <<"int">>,<<"value">> => 43},
                                     #{<<"type">> => <<"string">>,<<"value">> => <<"Foo">>},
                                     #{<<"type">> => <<"map">>, <<"value">> => [#{<<"key">> => #{<<"type">> => <<"int">>,<<"value">> => 1},
                                                                                  <<"val">> => #{<<"type">> => <<"int">>, <<"value">> => 2}}]}]}]
     , <<"d">> => [#{<<"type">> => <<"tuple">>,
                     <<"value">> => [#{<<"type">> => <<"bytes">>, <<"value">> => <<"ba_AAEYQB5m">>},
                                     #{<<"type">> => <<"bytes">>, <<"value">> => <<"ba_AAECAwQFBgcICQoLDA2sTJYu">>},
                                     #{<<"type">> => <<"bytes">>, <<"value">> => <<"ba_AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8vKHtN">>},
                                     #{<<"type">> => <<"bytes">>, <<"value">> => <<"ba_AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8AAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHwCpqpny">>}]}]
     , <<"e">> => [#{<<"type">> => <<"tuple">>,
                     <<"value">> => [#{<<"type">> => <<"oracle">>, <<"value">> => <<"ok_11111111111111111111111111111118qjnEr">>},
                                     #{<<"type">> => <<"oracle_query">>, <<"value">> => <<"oq_1111111111111111111111111111111Hrt6FG">>},
                                     #{<<"type">> => <<"contract">>, <<"value">> => <<"ct_1111111111111111111111111111111Rnzy1V">>},
                                     #{<<"type">> => <<"address">>, <<"value">> => <<"ak_1111111111111111111111111111111VcnZxy">>}]}]
     , <<"f">> => [#{<<"type">> => <<"list">>,
                     <<"value">> =>
                        [#{<<"type">> => <<"int">>,<<"value">> => 1},
                         #{<<"type">> => <<"int">>,<<"value">> => 2},
                         #{<<"type">> => <<"int">>,<<"value">> => 4},
                         #{<<"type">> => <<"int">>,<<"value">> => 8},
                         #{<<"type">> => <<"int">>,<<"value">> => 16}]}]
     }.

decode_calldata_source(_Config) ->
    {ok, ContractSrcBin} = read_test_contract("calldata"),
    ContractSrc = binary_to_list(ContractSrcBin),

    DoEnc = fun(F, V) -> encode_calldata(ContractSrc, binary_to_list(F), [V]) end,
    Datas = maps:map(DoEnc, test_data()),


    DoDec = fun(F, V) -> do_decode_calldata_source(#{calldata => V, function => F, source => ContractSrcBin}) end,
    Results = maps:map(DoDec, Datas),

    Expects =
        #{ <<"a">> => [#{<<"type">> => #{<<"tuple">> => [<<"int">>,<<"bool">>,<<"string">>, #{<<"tuple">> => []}]},
                         <<"value">> => [42,true,<<"Hello">>,[]]}]
         , <<"b">> => [#{<<"type">> => #{<<"tuple">> => [#{<<"Test.d">> => [<<"int">>]}, #{<<"Test.d">> => [<<"int">>]}]},
                         <<"value">> => [#{<<"One">> => [12,18]},<<"Two">>]}]
         , <<"c">> => [#{<<"type">> => #{<<"record">> => [#{<<"name">> => <<"x">>,<<"type">> => <<"int">>},
                                                          #{<<"name">> => <<"y">>,<<"type">> => <<"string">>},
                                                          #{<<"name">> => <<"z">>, <<"type">> => #{<<"map">> => [<<"int">>,<<"int">>]}}]},
                         <<"value">> => #{<<"x">> => 43,<<"y">> => <<"Foo">>, <<"z">> => [[1,2]]}}]
         , <<"d">> => [#{<<"type">> => #{<<"tuple">> => [#{<<"bytes">> => 2}, #{<<"bytes">> => 14}, #{<<"bytes">> => 32}, #{<<"bytes">> => 65}]},
                         <<"value">> => [<<"#0001">>,<<"#000102030405060708090a0b0c0d">>, <<"#000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f">>,
                                         <<"#000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f00">>]}]
         , <<"e">> => [#{<<"type">> => #{<<"tuple">> => [#{<<"oracle">> => [<<"int">>,<<"int">>]}, #{<<"oracle_query">> => [<<"int">>,<<"int">>]}, <<"Remote">>, <<"address">>]},
                         <<"value">> => [<<"ok_11111111111111111111111111111118qjnEr">>, <<"oq_1111111111111111111111111111111Hrt6FG">>,
                                         <<"ct_1111111111111111111111111111111Rnzy1V">>, <<"ak_1111111111111111111111111111111VcnZxy">>]}]
         , <<"f">> => [#{<<"type">> => #{<<"list">> => [<<"int">>]}, <<"value">> => [1,2,4,8,16]}]
         },

    Check = fun(K) -> E = {K, maps:get(K, Expects)}, V = maps:get(K, Results), ?assertEqual(E, V) end,
    [ Check(K) || K <- maps:keys(Expects) ].

decode_call_result(_Config) ->
    {ok, SrcBin} = read_test_contract("callresult"),

    Values = bin_test_data(),

    DoDec = fun(K, V) -> do_decode_call_result(#{source => SrcBin, function => K,
                                                                  'call-result' => <<"ok">>, 'call-value' => V}) end,
    Results = maps:map(DoDec, Values),
    Expects =
        #{ <<"a">> => [42,true,<<"Hello">>,[]]
         , <<"b">> => [#{<<"One">> => [12,18]},<<"Two">>]
         , <<"c">> => #{<<"x">> => 43,<<"y">> => <<"Foo">>,<<"z">> => [[1,2]]}
         , <<"d">> => [<<"#0001">>,<<"#000102030405060708090a0b0c0d">>,
                       <<"#000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f">>,
                       <<"#000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f00">>]
         , <<"e">> => [<<"ok_11111111111111111111111111111118qjnEr">>,
                       <<"oq_1111111111111111111111111111111Hrt6FG">>,
                       <<"ct_1111111111111111111111111111111Rnzy1V">>,
                       <<"ak_1111111111111111111111111111111VcnZxy">>]
         , <<"f">> => [1,2,4,8,16]
         },

    Check = fun(K) -> E = maps:get(K, Expects), V = maps:get(K, Results), ?assertEqual({K, E}, {K, V}) end,
    [ Check(K) || K <- maps:keys(Expects) ].

decode_call_result_bytecode(_Config) ->
    {ok, Contract, _Aci} = compile_test_contract("callresult"),

    Values = bin_test_data(),

    DoDec = fun(K, V) -> do_decode_call_result_bytecode(
                           #{bytecode => Contract, function => K,
                             'call-result' => <<"ok">>, 'call-value' => V}) end,
    Results = maps:map(DoDec, Values),
    Expects = json_expect(),

    Check = fun(K) -> [E] = maps:get(K, Expects), V = maps:get(K, Results), ?assertEqual({K, E}, V) end,
    [ Check(K) || K <- maps:keys(Expects) ].

decode_call_result_bytecode_not_ok(_Config) ->
    {ok, Contract, _Aci} = compile_test_contract("callresult"),
    Map0 = #{bytecode => Contract, function => <<"foo">>},

    ErrVal = aeser_api_encoder:encode(contract_bytearray, <<"An error happened!">>),
    {_, Res1} = do_decode_call_result_bytecode(Map0#{'call-result' => <<"error">>,
                                                                      'call-value'  => ErrVal}),

    ?assertMatch({X, X}, {#{<<"error">> => [<<"An error happened!">>]}, Res1}),

    RetVal0 = aeb_fate_encoding:serialize(<<"An error happened!">>),
    RetVal = aeser_api_encoder:encode(contract_bytearray, RetVal0),
    {_, Res2} = do_decode_call_result_bytecode(Map0#{'call-result' => <<"revert">>,
                                                                      'call-value'  => RetVal}),

    ?assertMatch({X, X}, {#{<<"abort">> => [<<"An error happened!">>]}, Res2}),
    ok.

validate_byte_code(_Config) ->
    {ok, IdByteCode, _Aci}  = compile_test_contract("identity"),
    {ok, IdSource}    = read_test_contract("identity"),
    {ok, NotIdSource} = read_test_contract("callresult"),
    ?assertMatch({ok, 200, #{}},
                 get_validate_byte_code(#{bytecode => IdByteCode,
                                          source   => IdSource,
                                          options  => #{} })),
    ?assertMatch({ok, 400, [#{<<"message">> := <<"Byte code does not match source code.\n", _/binary>> }]},
                 get_validate_byte_code(#{bytecode => IdByteCode,
                                          source   => NotIdSource,
                                          options  => #{} })),
    ok.

test_data() ->
    #{ <<"a">> => "(42, true, \"Hello\", ())"
     , <<"b">> => "(One(12, 18), Two)"
     , <<"c">> => "{x = 43, y = \"Foo\", z = { [1] = 2 }}"
     , <<"d">> => "(#0001, #000102030405060708090A0B0C0D,"
                  " #000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F,"
                  " #000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F00)"
     , <<"e">> => "(ok_11111111111111111111111111111118qjnEr, oq_1111111111111111111111111111111Hrt6FG,"
                  " ct_1111111111111111111111111111111Rnzy1V, ak_1111111111111111111111111111111VcnZxy)"
     , <<"f">> => "[1, 2, 4, 8, 16]" }.

bin_test_data() ->
    {ok, ContractSrcBin} = read_test_contract("calldata"),
    ContractSrc = binary_to_list(ContractSrcBin),

    Enc = fun(F, V) ->
              EncData = encode_calldata(ContractSrc, binary_to_list(F), [V]),
              {ok, Data} = aeser_api_encoder:safe_decode(contract_bytearray, EncData),
              {ok, [FateArgs]} = aeb_fate_abi:decode_calldata(binary_to_list(F), Data),
              aeser_api_encoder:encode(contract_bytearray, aeb_fate_encoding:serialize(FateArgs))
           end,
    maps:map(Enc, test_data()).

fate_assembler(_) ->
    C = <<"cb_+GZGA6CpNW171TSUfk88PoVv7YslUgxRcOJYKFPRxoGkXArWosC4OZ7+RNZEHwA3ADcAGg6CPwEDP/64F37sADcBBwcBAQCWLwIRRNZEHxFpbml0EbgXfuwRbWFpboIvAIU0LjEuMAANEx2r">>,
    _Res = do_get_fate_assembler(C).

-define(API_VERSION,      <<"7.1.1">>).
-define(COMPILER_VERSION, <<"7.0.1">>).

compiler_version(_) ->
    F = fun({ExpVer, CB}) ->
            Res = do_get_compiler_version(CB),
            ?assertEqual(ExpVer, Res)
        end,
    [ F(TD) || TD <- compiled_contracts() ].

get_api_version(_Config) ->
    {ok, 200, #{<<"api-version">> := Vsn}} = get_api_version(),
    ?assertMatch({X, X}, {Vsn, ?API_VERSION}),

    ok.

get_version(_Config) ->
    {ok, 200, #{<<"version">> := Vsn}} = get_version(),
    ?assertMatch({X, X}, {Vsn, ?COMPILER_VERSION}),

    ok.

get_api(_Config) ->
    {ok, 200, #{<<"swagger">> := _Swagger}} = get_api(),

    ok.

%% Contract interface functions.
contract_dir() ->
    filename:join(code:lib_dir(aesophia_http), "../../extras/test/contracts").

compile_test_contract(Name) ->
    Dir = contract_dir(),
    compile_test_contract(Dir, Name, #{}).

read_test_contract(Name) ->
    FileName = filename:join(contract_dir(), Name ++ ".aes"),
    file:read_file(FileName).

compile_test_contract(Dir, Name, Opts) ->
    FileName = filename:join(Dir, Name ++ ".aes"),
    {ok, SophiaCode} = file:read_file(FileName),
    case get_contract_bytecode(SophiaCode, Opts) of
        {ok, 200, #{<<"bytecode">> := Code, <<"aci">> := Aci}} -> {ok, Code, Aci};
        {ok, 400, Errors} -> {error, Errors}
    end.


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


encode_calldata(Src, Fun, Args) ->
    encode_calldata(Src, Fun, Args, #{}).

encode_calldata(Src, Fun, Args, Opts0) ->
    Opts = #{source => list_to_binary(Src),
             options => Opts0,
             function => list_to_binary(Fun),
             arguments => lists:map(fun list_to_binary/1, Args)},
    {ok, 200, #{<<"calldata">> := Data}} =
         get_encode_calldata(Opts),
    Data.

do_decode_calldata_bytecode(Map) ->
    {ok, 200, #{<<"function">> := FName, <<"arguments">> := Args}} =
        get_decode_calldata_bytecode(Map),
    {FName, Args}.

do_decode_calldata_source(Map0) ->
    Map = Map0#{ options => #{} },
    {ok, 200, #{<<"function">> := FName, <<"arguments">> := Args}} =
        get_decode_calldata_source(Map),
    {FName, Args}.

do_decode_call_result(Map0) ->
    Map = Map0#{ options => #{} },
    {ok, 200, JsonValue} =
        get_decode_call_result(Map),
    JsonValue.

do_decode_call_result_bytecode(Map) ->
    {ok, 200, #{<<"function">> := FName, <<"result">> := JsonValue}} =
        get_decode_call_result_bytecode(Map),
    {FName, JsonValue}.

do_get_compiler_version(CB) ->
    {ok, 200, #{<<"version">> := CVer}} = get_compiler_version(#{bytecode => CB}),
    CVer.

do_get_fate_assembler(CB) ->
    {ok, 200, #{<<"fate-assembler">> := Asm}} = get_fate_assembler(#{bytecode => CB}),
    Asm.

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


get_encode_calldata(Request) ->
    Host = internal_address(),
    http_request(Host, post, "encode-calldata", Request).

get_decode_calldata_bytecode(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-calldata/bytecode", Request).

get_decode_calldata_source(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-calldata/source", Request).

get_decode_call_result(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-call-result", Request).

get_decode_call_result_bytecode(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-call-result/bytecode", Request).

get_validate_byte_code(Request) ->
    Host = internal_address(),
    http_request(Host, post, "validate-byte-code", Request).

get_fate_assembler(Request) ->
    Host = internal_address(),
    http_request(Host, post, "fate-assembler", Request).

get_compiler_version(Request) ->
    Host = internal_address(),
    http_request(Host, post, "compiler-version", Request).

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
                            http_uri:quote(Path)}
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
    http_uri:quote(V).

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
                error:E:S ->
                    {error, {parse_error, [E, S]}}
            end;
        {error, _} = Error ->
            Error
    end.


compiled_contracts() ->
    [ {<<"undefined">>, <<"cb_+QPvRgGg/ukoFMi2RBUIDNHZ3pMMzHSrPs/uKkwO/vEf7cRnitr5Avv5ASqgaPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8WEbWFpbrjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QHLoLnJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqhGluaXS4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//////////////////////////////////////////7kBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uMxiAABkYgAAhJGAgIBRf7nJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqFGIAAMBXUIBRf2jyZ2M4/1CIOaukd0nv+ovofvKE8gf7PZmYcBzVOIfFFGIAAK9XUGABGVEAW2AAGVlgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tZWWAgAZCBUmAgkANgABlZYCABkIFSYCCQA2ADgVKBUpBWW2AgAVFRWVCAkVBQgJBQkFZbUFCCkVBQYgAAjFbiASYE">>}
    , {<<"2.1.0">>, <<"cb_+QP1RgKg/ukoFMi2RBUIDNHZ3pMMzHSrPs/uKkwO/vEf7cRnitr5Avv5ASqgaPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8WEbWFpbrjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QHLoLnJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqhGluaXS4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//////////////////////////////////////////7kBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uMxiAABkYgAAhJGAgIBRf7nJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqFGIAAMBXUIBRf2jyZ2M4/1CIOaukd0nv+ovofvKE8gf7PZmYcBzVOIfFFGIAAK9XUGABGVEAW2AAGVlgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tZWWAgAZCBUmAgkANgABlZYCABkIFSYCCQA2ADgVKBUpBWW2AgAVFRWVCAkVBQgJBQkFZbUFCCkVBQYgAAjFaFMi4xLjAhVoVW">>}
    , {<<"3.2.0">>, <<"cb_+QP1RgKgqTVte9U0lH5PPD6Fb+2LJVIMUXDiWChT0caBpFwK1qL5Avv5ASqgaPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8WEbWFpbrjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QHLoLnJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqhGluaXS4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//////////////////////////////////////////7kBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uMxiAABkYgAAhJGAgIBRf7nJVvKLMUmp9Zh6pQXz2hsiCcxXOSNABiu2wb2fn5nqFGIAAMBXUIBRf2jyZ2M4/1CIOaukd0nv+ovofvKE8gf7PZmYcBzVOIfFFGIAAK9XUGABGVEAW2AAGVlgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tZWWAgAZCBUmAgkANgABlZYCABkIFSYCCQA2ADgVKBUpBWW2AgAVFRWVCAkVBQgJBQkFZbUFCCkVBQYgAAjFaFMy4yLjC0dzuk">>}
    , {<<"4.0.0">>, <<"cb_+QP4RgOgqTVte9U0lH5PPD6Fb+2LJVIMUXDiWChT0caBpFwK1qL5Av35ASugaPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8WEbWFpbgC4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALhAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPkBzKC5yVbyizFJqfWYeqUF89obIgnMVzkjQAYrtsG9n5+Z6oRpbml0ALhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uQFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD//////////////////////////////////////////+4zGIAAGRiAACEkYCAgFF/uclW8osxSan1mHqlBfPaGyIJzFc5I0AGK7bBvZ+fmeoUYgAAwFdQgFF/aPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8UUYgAAr1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgA4FSkFlgAFFZUmAAUmAA81tgAIBSYADzW1lZYCABkIFSYCCQA2AAGVlgIAGQgVJgIJADYAOBUoFSkFZbYCABUVFZUICRUFCAkFCQVltQUIKRUFBiAACMVoU0LjAuMAA+Q6uo">>}
    , {<<"4.0.0">>, <<"cb_+GZGA6CpNW171TSUfk88PoVv7YslUgxRcOJYKFPRxoGkXArWosC4OZ7+RNZEHwA3ADcAGg6CPwEDP/64F37sADcBBwcBAQCWLwIRRNZEHxFpbml0EbgXfuwRbWFpboIvAIU0LjAuMADzYyEg">>}
    , {<<"4.1.0">>, <<"cb_+QP4RgOgqTVte9U0lH5PPD6Fb+2LJVIMUXDiWChT0caBpFwK1qL5Av35ASugaPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8WEbWFpbgC4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALhAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPkBzKC5yVbyizFJqfWYeqUF89obIgnMVzkjQAYrtsG9n5+Z6oRpbml0ALhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uQFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD//////////////////////////////////////////+4zGIAAGRiAACEkYCAgFF/uclW8osxSan1mHqlBfPaGyIJzFc5I0AGK7bBvZ+fmeoUYgAAwFdQgFF/aPJnYzj/UIg5q6R3Se/6i+h+8oTyB/s9mZhwHNU4h8UUYgAAr1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgA4FSkFlgAFFZUmAAUmAA81tgAIBSYADzW1lZYCABkIFSYCCQA2AAGVlgIAGQgVJgIJADYAOBUoFSkFZbYCABUVFZUICRUFCAkFCQVltQUIKRUFBiAACMVoU0LjEuMAAD3qDS">>}
    , {<<"4.1.0">>, <<"cb_+GZGA6CpNW171TSUfk88PoVv7YslUgxRcOJYKFPRxoGkXArWosC4OZ7+RNZEHwA3ADcAGg6CPwEDP/64F37sADcBBwcBAQCWLwIRRNZEHxFpbml0EbgXfuwRbWFpboIvAIU0LjEuMAANEx2r">>}
    ].
