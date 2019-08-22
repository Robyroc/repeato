-module(repeato_main).
-author("Robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, run_command/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {last_msg}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run_command(ChatId, Text) ->
  gen_server:call(repeato_main, {command, ChatId, Text}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{last_msg = []}}.

handle_call({command, ChatId, Text}, _From, State) ->
  Previous = [Prev || {CID, Prev} <- State#state.last_msg, CID =:= ChatId],
  Others = [{CID, Prev} || {CID, Prev} <- State#state.last_msg, CID =/= ChatId],
  case Previous of
    [] -> raboter:send_message(ChatId, "Understood");
    [SomeText] -> raboter:send_message(ChatId, "Previous message was " ++ SomeText)
  end,
  {reply, ok, State#state{last_msg = [{ChatId, Text} | Others]}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
