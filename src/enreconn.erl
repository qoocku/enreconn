%%% ===========================================================================
%%% @doc The Application Utilities Functions.
%%% == License ==
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
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

