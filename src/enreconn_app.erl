%%% ===========================================================================
%%% @doc The Application Behavior Callbacks.
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
%%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%%% AB. All Rights Reserved.
%%%
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2012-07-17
%%% @end
%%% ===========================================================================
-module (enreconn_app).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behavior (application).

-export ([start/2,
          stop/1]).

start (_StartType, _Args) ->
  supervisor:start_link(enreconn_sup, []).

stop (_State) ->
  %% cast to all connected nodes message about stop.
  lists:foreach(fun (Node) ->
                    try gen_server:cast({enreconn_srv, Node}, {enreconn_node_stop, node()}) of
                        ok -> ok
                    catch
                      _:_ ->
                        ignore
                    end
                end, nodes(connected)),
  ok.

  
