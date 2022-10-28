%%%=============================================================================
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%%    Caching middleware for Cowboy
%%% @end
%%%=============================================================================
-module(aesophia_cache_middleware).

-behaviour(cowboy_middleware).

%% Behavior API
-export([execute/2]).

-define(CACHE_CONTROL , <<"public, max-age=604800, immutable">>).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec execute(cowboy_req:req(), cowboy_middleware:env()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}.
execute(Req, Env) ->
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, ?CACHE_CONTROL, Req),
    {ok, Req2, Env}.
