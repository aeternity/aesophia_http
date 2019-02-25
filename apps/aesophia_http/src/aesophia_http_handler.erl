-module(aesophia_http_handler).

-export([init/2,
	 handle/2,content_types_provided/2,
	 allowed_methods/2,content_types_accepted/2
	]).

-compile(export_all).

init(Req0, Opts) ->
    %% Method = cowboy_req:method(Req0),
    %% io:format("Method: ~p\nReq: ~p\nOpts: ~p\n", [Method,Req0,Opts]),
    {cowboy_rest,Req0,Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>,<<"POST">>],
    {Methods,Req,State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>,<<"json">>,'*'}, handle},
      {{<<"text">>,<<"plain">>,'*'}, handle}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>,handle},
      {<<"test/plain">>,handle}
     ], Req, State}.

handle(Req, Opts) ->
    Method = cowboy_req:method(Req),
    case Method of
	<<"POST">> ->
	    handle_post(Req, Opts);
	_ ->
	    {stop,cowboy_req:reply(405, Req),Opts}
    end.

handle_post(Req0, Opts) ->
    {ok,Body,Req1} = cowboy_req:read_body(Req0, #{}),
    Path = cowboy_req:path(Req1),
    %% io:format("Body: ~p\n", [Body]),
    case Path of
	<<"/contracts/code/aci">> ->
	    #{<<"code">> := Code, <<"options">> := Options} =
		jsx:decode(Body, [return_maps]),
	    Headers = #{<<"content-type">> => <<"application/json">>},
	    ACI = aci_encode(Code, Options),
	    Req2 = cowboy_req:reply(200, Headers, ACI, Req1),
	    {stop,Req2,Opts};
	<<"/contracts/code/compile">> ->
	    #{<<"code">> := Code, <<"options">> := Options} =
		jsx:decode(Body, [return_maps]),
	    Headers = #{<<"content-type">> => <<"application/json">>},
	    Ret = compile_code(Code, Options),
	    Req2 = cowboy_req:reply(200, Headers, Ret, Req1),
	    {stop,Req2,Opts};
	_ ->
	    Req2 = cowboy_req:reply(400, #{}, <<"Bad command.">>, Req1),
	    {stop,Req2,Opts}
    end.

%% aci_encode(Contract, Options) -> ACI.

aci_encode(Contract, _Options) ->
    Enc = aeso_aci:encode(Contract),
    Dec = aeso_aci:decode(Enc),
    <<"{\"encoded-aci\":",Enc/binary,",",
      "\"decoded-aci\":\"",Dec/binary,"\"}">>.

%% compile_code(Contract, Options) -> ByteCode.
%%  Compile a contract code, then serialize it and encode it as a
%%  contract bytearray. The code for this has been plucked from
%%  aect_sophia:compile/2 and aehttp_dispatch_int:handle_request_/3
%%  for 'CompileContract'. Yes, it's a hack at the moment.

compile_code(Contract, Options) ->
    %% aect_sophia:compile/2
    Map = aeso_compiler:from_string(binary_to_list(Contract), []),
    Ser = serialize(Map),
    %% io:format("Ser: ~p\n", [Ser]),
    %% aehttp_dispatch_int:handle_request_('CompileContract', ...)
    ByteCode = aehttp_api_encoder:encode(contract_bytearray, Ser),
    <<"{\"bytecode\":\"",ByteCode/binary,"\"}">>.

%% From aect_sophia.erl for compile/2

-define(SOPHIA_CONTRACT_VSN, 1).

serialize(#{byte_code := ByteCode, type_info := TypeInfo, 
            contract_source := ContractString, compiler_version := _Version}) ->
    ContractBin = list_to_binary(ContractString),
    Fields = [ {source_hash, aec_hash:hash(sophia_source_code, ContractBin)}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode}
             ],
    aec_object_serialization:serialize(compiler_sophia,
                                       ?SOPHIA_CONTRACT_VSN,
                                       serialization_template(?SOPHIA_CONTRACT_VSN),
                                       Fields
                                      ).

serialization_template(?SOPHIA_CONTRACT_VSN) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}].
