-module(basicpxs_run).

-compile([{parse_transform, lager_transform}]).

-include_lib("basicpxs_config.hrl").

-export([start/0, addr_of_controller/0]).

apps() ->
      %TODO: Lager fails if compiler, syntax_tools are not included. Find fix.
      [compiler, syntax_tools, lager].
      
start() ->
  lists:foreach (fun (App) ->
    case application:start (App) of
      {error, {already_started, App}} -> ok; 
      ok -> ok; 
      Other ->
        erlang:error ({error,
                       {?MODULE, ?LINE,
                       'could not start',
                       App,
                       'reason was', Other}})
    end 
  end,
  apps ()),
  error_logger:tty(false),

  %% first we find the number of controllers
  spawn(fun() ->
                register(basicpxs_run, self()),
                process_flag(trap_exit, true),
                Port = open_port({spawn, "/home/hasenov/zebralite/num_of_controllers"}, []),
                loop(Port)
        end),
  timer:sleep(1000), %% sleep for 100 milliseconds
  {ok, Address_String} = addr_of_controller(),
  My_Pid = self(),
  spawn(fun() ->
              basicpxs_tcp:start(Address_String, My_Pid)
        end),
  Acceptors = spawn_procs(acceptor, 3, null), 
  Replicas = spawn_procs(replica, 3, []),
  Leaders = spawn_procs(leader, 1, {Acceptors, Replicas}),
  basicpxs_client:start_link([]),
  basicpxs_client:set_replicas(Replicas),
  multi_exec(basicpxs_replica, set_leaders, Leaders, Replicas),
  propose_loop().

propose_loop() ->
  receive
    {data, Data} ->
      ?LINFO("Data received in propose loop ~p", [Data]),
      basicpxs_client:propose(Data),
      propose_loop()
  end.

%%% assume that there is only one controller for now
addr_of_controller() -> 
  basicpxs_run ! {call, self()},
  receive
    {num_of_controllers, Result} ->
      {ok, Result}
  end.

loop(Port) ->
  receive
    {call, Caller} ->
      Port ! {self(), {command, encode(1)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {num_of_controllers, decode(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit({port_terminated, Reason})
  end.

encode(Data) -> [Data]. 
decode(Data) -> Data.

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------

spawn_procs(Type, Count, Args) ->
    spawn_procs(Type, Count, Args, []).

spawn_procs(_Type, 0, _Args, Acc) ->
    Acc;
spawn_procs(Type, Count, Args, Acc) ->
    case Type of
      acceptor ->
        {ok, Pid} = basicpxs_acceptor:start_link();
      replica ->
        {ok, Pid} = basicpxs_replica:start_link(Args);
      leader ->
        {ok, Pid} = basicpxs_leader:start_link(Args)
    end,
    spawn_procs(Type, Count-1, Args, [Pid|Acc]).

multi_exec(_Mod, _Fun, _Val, []) ->
  ok;
multi_exec(Mod, Fun, Val, [Pid|Lst]) ->
  Mod:Fun(Pid, Val),
  multi_exec(Mod, Fun, Val, Lst).
