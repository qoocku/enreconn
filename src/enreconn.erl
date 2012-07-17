%%% ===========================================================================
%%% @doc The Application Utilities Functions.
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2012-07-17
%%% @end
%%% ===========================================================================
-module (enreconn).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-export ([start/0,
          start_link/0,
          server/0]).

%% @doc Starts the whole applications

-spec start () -> ok | {error, Reason::any()}.

start () ->
  application:start(?MODULE).

%% @doc Starts the main guard server.
%% The server is signleton, locally registered
%% under `enreconn_srv' name.

-spec start_link () -> {ok, pid()}.

start_link () ->
  gen_server:start_link({local, enreconn_srv}, enreconn_srv, [], []).

%% @doc Returns pid of the guard server process 
%% or rises `no_process' error if no such process.

-spec server () -> {ok, pid()}.

server () ->
  case erlang:whereis(enreconn_srv) of
    Pid when is_pid(Pid) ->
      {ok, Pid};
    _ ->
      error(no_process)
  end.

