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
        , legacy_decode_data/1
        , encode_calldata/1
        ]).

all() ->
    [
     {group, contracts}
    ].

groups() ->
    [
     {contracts, [],
      [ identity_contract
      , legacy_decode_data
      , encode_calldata
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

%% identity_contract(Config)
%%  Create the Identity contract by account acc_c and call by accounts
%%  acc_c and acc_d. Encode create and call data in server.

identity_contract(_Config) ->
    %% Node = proplists:get_value(node_name, Config),

    %% Compile test contract "identity.aes"
    _Code = compile_test_contract("identity"),

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

%% Contract interface functions.

compile_test_contract(Name) ->
    Dir = filename:join(code:lib_dir(aesophia_http), "../../extras/test/contracts"),
    compile_test_contract(Dir, Name).

compile_test_contract(Dir, Name) ->
    FileName = filename:join(Dir, Name ++ ".aes"),
    {ok, SophiaCode} = file:read_file(FileName),
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),
    Code.

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

%% ============================================================
%% HTTP Requests
%% Note that some are internal and some are external!
%% ============================================================

get_contract_bytecode(SourceCode) ->
    Host = internal_address(),
    http_request(Host, post, "compile",
                 #{ <<"code">> => SourceCode, <<"options">> => <<>> }).

get_contract_decode_data(Request) ->
    Host = internal_address(),
    http_request(Host, post, "decode-data", Request).

get_encode_calldata(Request) ->
    Host = internal_address(),
    http_request(Host, post, "encode-calldata", Request).

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

call_code(Fun, Args) ->
    Type    = ["(", string:join([ type_of_arg(Arg) || Arg <- Args ], ", "), ")"],
    BinArgs = args_to_list(Args),
    {code, list_to_binary(
        [ "contract Call =\n"
        , "  function ", Fun, " : ", Type, " => _\n"
        , "  function __call() = ", Fun, "(", BinArgs, ")\n" ])}.

type_of_arg(N) when is_integer(N) -> "int";
type_of_arg({string, _}) -> "string";
type_of_arg([H | _]) -> ["list(", type_of_arg(H), ")"];
type_of_arg([]) -> "list(int)"; %% Don't know the element type
type_of_arg(B) when is_binary(B), byte_size(B) == 32 -> "address";
type_of_arg(B) when is_binary(B), byte_size(B) == 64 -> "signature";
type_of_arg(T) when is_tuple(T) ->
    ["(", string:join([ type_of_arg(X) || X <- tuple_to_list(T) ], ","), ")"];
type_of_arg(M) when is_map(M) ->
    case maps:to_list(M) of
        []  -> %% empty map: can't infer type, default to int/int
            "map(int, int)";
        [{K, V} | _] ->
            ["map(", type_of_arg(K), ", ", type_of_arg(V), ")"]
    end.

%% args_to_binary(Args) -> binary_string().
%%  Take a list of arguments in "erlang format" and generate an
%%  argument binary string. Strings are handled naively now.

args_to_binary(Args) ->
    %% ct:pal("Args ~tp\n", [Args]),
    BinArgs = list_to_binary([$(,args_to_list(Args),$)]),
    %% ct:pal("BinArgs ~tp\n", [BinArgs]),
    BinArgs.

args_to_list([A]) -> [arg_to_list(A)];          %The last one
args_to_list([A1|Rest]) ->
    [arg_to_list(A1),$,|args_to_list(Rest)];
args_to_list([]) -> [].

%%arg_to_list(<<N:256>>) -> integer_to_list(N);
arg_to_list(N) when is_integer(N) -> integer_to_list(N);
arg_to_list(B) when is_binary(B) ->             %A key
    <<"0x", Enc/binary>> = aeu_hex:hexstring_encode(B),
    ["#", binary_to_list(Enc)];
arg_to_list({string,S}) -> ["\"",S,"\""];
arg_to_list(L) when is_list(L) ->
    [$[,args_to_list(L),$]];
arg_to_list(T) when is_tuple(T) ->
    [$(,args_to_list(tuple_to_list(T)),$)];
arg_to_list(M) when is_map(M) ->
    [${,map_to_list(maps:to_list(M)),$}].

map_to_list([{K,V}]) -> [$[,arg_to_list(K),"] = ",arg_to_list(V)];
map_to_list([{K,V},Fields]) ->
    [$[,arg_to_list(K),"] = ",arg_to_list(V),$,|map_to_list(Fields)];
map_to_list([]) -> [].
