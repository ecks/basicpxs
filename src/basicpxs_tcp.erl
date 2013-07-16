-module(basicpxs_tcp).

-compile([{parse_transform, lager_transform}]).

-include_lib("basicpxs_config.hrl").

-export([start/2]).

-define(CTRL_PORT, 6634).

start(Ctrl_Addr, Caller) ->
  ?LINFO("START::TCP client ~p", [self()]),
  Sock = connect(Ctrl_Addr),
  read(Sock, Caller).

connect(Ctrl_Addr) ->
  {ok, Address} = inet_parse:address(Ctrl_Addr),
  {ok, Sock} = gen_tcp:connect(Address, ?CTRL_PORT, [inet6]), 
  Sock.

read(Sock, Caller) ->
  receive
    {tcp, Sock, Data} ->
      ?LINFO("Got data ~p", [Data]),
      Caller ! {data, Data},
      read(Sock, Caller); 
    {tcp_closed, Sock} ->
      io:format("Socket ~w closed~n", [Sock]),
      ok;
    {tcp_error, Sock, Reason} ->
      io:format("Error on socket ~w, reason ~w~n", [Sock, Reason]),
      ok
  end.

