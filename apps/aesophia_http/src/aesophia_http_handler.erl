-module(aesophia_http_handler).

-export([init/2,
	 handle_request_json/2,content_types_provided/2,
	 allowed_methods/2,content_types_accepted/2
	]).

-compile(export_all).

-record(state, { spec :: jsx:json_text()
               , validator :: jesse_state:state()
               , operation_id :: atom() }).

init(Req, OperationId) ->
    JsonSpec = aesophia_http_api_validate:json_spec(),
    Validator = aesophia_http_api_validate:validator(JsonSpec),
    State = #state{ spec = JsonSpec,
                    validator = Validator,
                    operation_id = OperationId },
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>,<<"POST">>],
    {Methods,Req,State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_request_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request_json}], Req, State}.

handle_request_json(Req0, State = #state{ validator = Validator,
                                         spec = Spec,
                                         operation_id = OperationId }) ->
    Method = cowboy_req:method(Req0),
    try aesophia_http_api_validate:request(OperationId, Method, Req0, Validator) of
        {ok, Params, Req1} ->
            Context = #{ spec => Spec },
            {Code, Headers, Body} = handle_request(OperationId, Params, Context),

            _ = aesophia_http_api_validate:response(OperationId, Method, Code, Body, Validator),

            Req = cowboy_req:reply(Code, to_headers(Headers), jsx:encode(Body), Req1),
            {stop, Req, State};
        {error, Reason, Req1} ->
            Body = jsx:encode(to_error(Reason)),
            Req = cowboy_req:reply(400, #{}, Body, Req1),
            {stop, Req, State}
    catch error:_Error ->
            Body = jsx:encode(to_error({validation_error, <<>>, <<>>})),
            {stop, cowboy_req:reply(400, #{}, Body, Req0), State}
    end.

handle_request('CompileContract', Req, _Context) ->
    case Req of
        #{'Contract' :=
              #{ <<"code">> := Code
               , <<"options">> := Options }} ->
            case compile_contract(Code, Options) of
                 {ok, ByteCode} ->
                     {200, [], #{bytecode => aeser_api_encoder:encode(contract_bytearray, ByteCode)}};
                 {error, ErrorMsg} ->
                     {403, [], #{reason => ErrorMsg}}
             end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('Api', _Req, #{ spec := Spec }) ->
    {200, [], Spec}.

%% handle_post(Req0, Opts) ->
%%     {ok,Body,Req1} = cowboy_req:read_body(Req0, #{}),
%%     Path = cowboy_req:path(Req1),
%%     %% io:format("Body: ~p\n", [Body]),
%%     case Path of
%% 	<<"/contracts/code/aci">> ->
%% 	    #{<<"code">> := Code, <<"options">> := Options} =
%% 		jsx:decode(Body, [return_maps]),
%% 	    Headers = #{<<"content-type">> => <<"application/json">>},
%% 	    ACI = aci_encode(Code, Options),
%% 	    Req2 = cowboy_req:reply(200, Headers, ACI, Req1),
%% 	    {stop,Req2,Opts};
%% 	<<"/contracts/code/compile">> ->
%% 	    #{<<"code">> := Code, <<"options">> := Options} =
%% 		jsx:decode(Body, [return_maps]),
%% 	    Headers = #{<<"content-type">> => <<"application/json">>},
%% 	    Ret = compile_code(Code, Options),
%% 	    Req2 = cowboy_req:reply(200, Headers, Ret, Req1),
%% 	    {stop,Req2,Opts};
%% 	_ ->
%% 	    Req2 = cowboy_req:reply(400, #{}, <<"Bad command.">>, Req1),
%% 	    {stop,Req2,Opts}
%%     end.

%% aci_encode(Contract, Options) -> ACI.

aci_encode(Contract, _Options) ->
    Enc = aeso_aci:encode(Contract),
    Dec = aeso_aci:decode(Enc),
    <<"{\"encoded-aci\":",Enc/binary,",",
      "\"decoded-aci\":\"",Dec/binary,"\"}">>.

compile_contract(Contract, _Options) ->
    case aeso_compiler:from_string(binary_to_list(Contract), []) of
        {ok, Map} ->
            {ok, serialize(Map)};
        Err = {error, _} ->
            Err
    end.

%% From aect_sophia.erl for compile/2

-define(SOPHIA_CONTRACT_VSN, 1).
-define(COMPILER_SOPHIA_TAG, 70).

serialize(#{byte_code := ByteCode, type_info := TypeInfo,
            contract_source := ContractString, compiler_version := _Version}) ->
    ContractBin = list_to_binary(ContractString),
    {ok, SourceHash} = eblake2:blake2b(32, ContractBin),
    Fields = [ {source_hash, SourceHash}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode} ],
    aeserialization:serialize(?COMPILER_SOPHIA_TAG,
                              ?SOPHIA_CONTRACT_VSN,
                              serialization_template(?SOPHIA_CONTRACT_VSN),
                              Fields).

serialization_template(?SOPHIA_CONTRACT_VSN) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}].

to_headers(Headers) when is_list(Headers) ->
    maps:from_list(Headers);
to_headers(Headers) ->
    Headers.

to_error({Reason, Name, Info}) ->
    #{ reason => Reason,
       parameter => Name,
       info => Info }.
