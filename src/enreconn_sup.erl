%%% ===========================================================================
%%% @doc The Application Main Supervisor Callbacks.
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
-module (enreconn_sup).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behavior (supervisor).

-export ([init/1]).

%% @doc Returns supervised gen_server process specification.

init (_) ->
  {ok,
   {restart_strategy(),
    child_specifications()}}.

%%% ========================== Local/internal functions ======================

restart_strategy () ->
  {one_for_one, 1000, 1}. %% restart period == 1 ms

child_specifications () ->
  [{guard_srv,
    {enreconn, start_link, []},
    permanent,
    5000,
    worker,
    [enreconn_srv]}].
