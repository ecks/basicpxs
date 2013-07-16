-module(simple_state_machine).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-include("basicpxs_config.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    number
}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Msg = ping,
  timer:send_after(1000, Msg),
  {ok, #state{number = 0}}.

handle_call({hello, Message}, _From, #state{number = Number} = State) ->
  ?LINFO("Calling state machine"),
  ?LINFO("Message is ~p~n", [Message]),
  {reply, Number, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%%%handle_info(ping, State) ->
%%% Msg = ping,
%%%  receiver ! Msg,
%%%  timer:send_after(1000, Msg),
%%%  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
