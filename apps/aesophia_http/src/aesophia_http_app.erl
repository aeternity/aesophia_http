%%%-------------------------------------------------------------------
%% @doc aesophia_http public API
%% @end
%%%-------------------------------------------------------------------

-module(aesophia_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Paths = get_paths(),
    {ok,Port} = application:get_env(port),      %Get the port
    io:format("Port: ~p\n", [Port]),
    Dispatch = cowboy_router:compile([
				      {'_',Paths}
				     ]),
    {ok,_} =  cowboy:start_clear(http, [{port,Port}],
				 #{env => #{dispatch => Dispatch}}),
    aesophia_http_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_paths() ->
    [{path(Path), aesophia_http_handler, OperationId}
     || {OperationId, Spec} <- maps:to_list(endpoints:operations()),
        {_Method, #{path := Path}} <- maps:to_list(Spec) ].

path(Path0) ->
    Path1 = binary:replace(Path0, <<"}">>, <<"">>, [global]),
    binary:replace(Path1, <<"{">>, <<":">>, [global]).

