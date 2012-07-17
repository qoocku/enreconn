%%% ===========================================================================
%%% @doc The Application Beehavior Callbacks.
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

  
