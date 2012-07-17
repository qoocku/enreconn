%%% ===========================================================================
%%% @doc The Application Main Supervisor Callbacks.
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
