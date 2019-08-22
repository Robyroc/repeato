-module(repeato_sup).
-author("Robyroc").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    application:set_env(raboter, target, repeato_main),     %% <------ this line is important
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Son1 = {repeato_main, {repeato_main, start_link, []},
        permanent, 2000, worker, [repeato_main]},

    {ok, {SupFlags, [Son1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

