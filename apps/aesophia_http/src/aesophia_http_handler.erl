-module(aesophia_http_handler).

-export([init/2,
         handle_request_json/2,content_types_provided/2,
         allowed_methods/2,content_types_accepted/2
        ]).

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

handle_request('EncodeCalldata', Req, _Context) ->
    case Req of
        #{'FunctionCallInput' :=
              #{ <<"source">>    := ContractCode
               , <<"function">>  := FunctionName
               , <<"arguments">> := Arguments
               }} ->
            case encode_calldata(ContractCode, FunctionName, Arguments) of
                {ok, Result} ->
                    {200, [], #{calldata => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('DecodeData', Req, _Context) ->
    case Req of
        #{'SophiaBinaryData' :=
              #{ <<"sophia-type">>  := Type
               , <<"data">>  := Data
               }} ->
            case decode_data(Type, Data) of
                {ok, Result} ->
                    {200, [], #{data => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end
    end;

handle_request('GenerateACI', Req, _Context) ->
    case Req of
        #{'Contract' :=
              #{ <<"code">> := Code
               , <<"options">> := Options }} ->
            case generate_aci(Code, Options) of
                 {ok, EncACI, DecACI} ->
                     {200, [],
                      #{encoded_aci => jsx:decode(EncACI),
                        interface => DecACI}};
                 {error, ErrorMsg} ->
                     {403, [], #{reason => ErrorMsg}}
             end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('Version', _Req, _Context) ->
    case aeso_compiler:version() of
        {ok, Vsn} ->
            {200, [], #{version => Vsn}};
        _ ->
            {403, [], #{reason => <<"Internal error: Could not find the version!?">>}}
    end;

handle_request('APIVersion', _Req, #{ spec := Spec }) ->
    case jsx:decode(Spec, [return_maps]) of
        #{ <<"info">> := #{ <<"version">> := Vsn } } ->
            {200, [], #{'api-version' => Vsn}};
        _ ->
            {403, [], #{reason => <<"Internal error: Could not find the version!?">>}}
    end;

handle_request('Api', _Req, #{ spec := Spec }) ->
    {200, [], #{api => jsx:decode(Spec)}}.

generate_aci(Contract, _Options) ->
    case aeso_aci:encode(Contract) of
        {ok,Enc} ->
            Dec = aeso_aci:decode(Enc),
            {ok,Enc,Dec};
        {error,_} = Err ->
            Err
    end.

compile_contract(Contract, Options) ->
    Opts = compile_options(Options),
    case aeso_compiler:from_string(binary_to_list(Contract), Opts) of
        {ok, Map} ->
            {ok, serialize(Map)};
        Err = {error, _} ->
            Err
    end.

compile_options(Options) ->
    Map = maps:get(<<"file_system">>, Options, #{}),
    Map1 = maps:from_list([{binary_to_list(N), F} || {N, F} <- maps:to_list(Map)]),
    SrcFile = maps:get(<<"src_file">>, Options, no_file),
    [{include, {explicit_files, Map1}}]
      ++ [ {src_file, binary_to_list(SrcFile)} || SrcFile /= no_file ].

encode_calldata(Source, Function, Arguments) ->
    case aeso_compiler:create_calldata(binary_to_list(Source),
                                       binary_to_list(Function),
                                       lists:map(fun binary_to_list/1, Arguments)) of
        {ok, Calldata, _VMArgTypes, _VMRetType} ->
            {ok, aeser_api_encoder:encode(contract_bytearray, Calldata)};
        Err = {error, _} ->
            Err
    end.

decode_data(Type, Data) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, Data) of
        {error, _} ->
            {error, <<"Data must be encoded as a contract_bytearray">>};
        {ok, CallData} ->
            try decode_data_(Type, CallData) of
                {ok, _Result} = OK -> OK;
                {error, _ErrorMsg} = Err -> Err
            catch
                _T:_E ->
                    String = io_lib:format("~p:~p ~p", [_T,_E,erlang:get_stacktrace()]),
                    Error = << <<B>> || B <- "Bad argument: " ++ lists:flatten(String) >>,
                    {error, Error}
            end
    end.

decode_data_(Type, Data) ->
    case parse_type(Type) of
        {ok, VMType} ->
            try aeso_heap:from_binary(VMType, Data) of
                {ok, Term} ->
                    try prepare_for_json(VMType, Term) of
                        R -> {ok, R}
                    catch throw:R -> R
                    end;
                {error, _} -> {error, <<"bad type/data">>}
            catch _T:_E ->    {error, <<"bad argument">>}
            end;
        {error, _} = E -> E
    end.

parse_type(BinaryString) ->
    String = unicode:characters_to_list(BinaryString, utf8),
    case aeso_compiler:sophia_type_to_typerep(String) of
        {ok, _Type} = R -> R;
        {error, ErrorAtom} ->
            {error, unicode:characters_to_binary(atom_to_list(ErrorAtom))}
    end.

%% -- Helper functions -------------------------------------------------------

%% -- Contract serialization
-define(SOPHIA_CONTRACT_VSN, 2).
-define(COMPILER_SOPHIA_TAG, compiler_sophia).

serialize(#{byte_code := ByteCode, type_info := TypeInfo,
            contract_source := ContractString, compiler_version := Version}) ->
    ContractBin      = list_to_binary(ContractString),
    {ok, SourceHash} = eblake2:blake2b(32, ContractBin),
    Fields = [ {source_hash, SourceHash}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode}
             , {compiler_version, Version} ],
    aeser_chain_objects:serialize(?COMPILER_SOPHIA_TAG,
                                  ?SOPHIA_CONTRACT_VSN,
                                  serialization_template(?SOPHIA_CONTRACT_VSN),
                                  Fields).

serialization_template(?SOPHIA_CONTRACT_VSN) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}
    , {compiler_version, binary} ].

to_headers(Headers) when is_list(Headers) ->
    maps:from_list(Headers).

to_error({Reason, Name, Info}) ->
    #{ reason => Reason,
       parameter => Name,
       info => Info }.

%% -- JSON representation for typed VM-value
prepare_for_json(word, Integer) when is_integer(Integer) ->
    #{ <<"type">> => <<"word">>,
       <<"value">> => Integer};
prepare_for_json(string, String) when is_binary(String) ->
    #{ <<"type">> => <<"string">>,
       <<"value">> => String};
prepare_for_json({option, _T}, none) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => <<"None">>};
prepare_for_json({option, T}, {some, E}) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => prepare_for_json(T,E) };
prepare_for_json({tuple, Ts}, Es) ->
    #{ <<"type">> => <<"tuple">>,
       <<"value">> => [prepare_for_json(T,E)
                       || {T,E} <-
                              lists:zip(Ts, tuple_to_list(Es))] };
prepare_for_json({list, T}, Es) ->
    #{ <<"type">> => <<"list">>,
       <<"value">> => [prepare_for_json(T,E) || E <- Es]};
prepare_for_json(T = {variant, Cons}, R = {variant, Tag, Args}) when is_integer(Tag), Tag < length(Cons) ->
    Ts = lists:nth(Tag + 1, Cons),
    case length(Ts) == length(Args) of
        true ->
            #{ <<"type">> => <<"variant">>
             , <<"value">> => [Tag | [prepare_for_json(ArgT, Arg)
                                      || {ArgT, Arg} <- lists:zip(Ts, Args)]] };
        false ->
            String = io_lib:format("Type: ~p Res:~p", [T,R]),
            Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
            throw({error, Error})
    end;
prepare_for_json({map, KeyT, ValT}, Map) when is_map(Map) ->
    #{ <<"type">> => <<"map">>,
       <<"value">> => [ #{ <<"key">> => prepare_for_json(KeyT, K),
                           <<"val">> => prepare_for_json(ValT, V) }
                        || {K, V} <- maps:to_list(Map) ] };
prepare_for_json(T, R) ->
    String = io_lib:format("Type: ~p Res:~p", [T,R]),
    Error = << <<B>> || B <- "Invalid VM-type: " ++ lists:flatten(String) >>,
    throw({error, Error}).
