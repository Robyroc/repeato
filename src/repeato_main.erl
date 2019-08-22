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

-record(state, {participants}).

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
  {ok, #state{participants = []}}.

handle_call({command, ChatId, "/join"}, _From, State) ->
  case [X || X <- State#state.participants, X =:= ChatId] of
	[] -> 
	  raboter:send_message(ChatId, "Joined"),
	  [raboter:send_message(CID, "Someone joined") || CID <- State#state.participants],
	  {reply, ok, State#state{participants = [ChatId | State#state.participants]}};
	_ ->
	  raboter:send_message(ChatId, "Already joined"),
	  {reply, ok, State}
  end;

handle_call({command, ChatId, "/leave"}, _From, State) ->
  case [X || X <- State#state.participants, X =:= ChatId] of
	[_] -> 
	  raboter:send_message(ChatId, "Left"),
	  [raboter:send_message(CID, "Someone left") || CID <- State#state.participants, CID =/= ChatId],
	  {reply, ok, State#state{participants = [CID || CID <- State#state.participants, CID =/= ChatId]}};
	[] ->
	  raboter:send_message(ChatId, "Already out of network"),
	  {reply, ok, State}
  end;
  
handle_call({command, ChatId, [47,109,32|Message]}, _From, State) ->
  case [X || X <- State#state.participants, X =:= ChatId] of
	[_] -> 
	  [raboter:send_message(CID, "Someone said: " ++ Message) || CID <- State#state.participants],
	  {reply, ok, State};
	[] ->
	  raboter:send_message(ChatId, "You must join to send messages"),
	  {reply, ok, State}
  end;
  
handle_call({command, ChatId, _Text}, _From, State) ->
  raboter:send_message(ChatId, "Command not understood. Use /join to join the anonymous chat, /leave to leave it, /m <TEXT> to write to everyone"),
  {reply, ok, State};

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
