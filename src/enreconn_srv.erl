%%% ===========================================================================
%%% @doc The Application Main Server Callbacks.
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2012-07-17
%%% @end
%%% ===========================================================================
-module (enreconn_srv).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behavior (gen_server).

-export ([init/1,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-export ([reconnect/2]).

-record (state, {tid :: ets:tid()}).
-type state () :: #state{}.

%% @doc The server process initialisation.
%% First, it registers itself as nodes monitoring
%% messages receiver (via {@std_doc net_kernel.html#monitor_nodes-2}).
%% Then it opens ETS table to hold dis-connected node names.

init (_) ->
  ok  = net_kernel:monitor_nodes(true, [{node_type, all}]),
  Tid = ets:new(?MODULE, []),
  {ok, #state{tid = Tid}}.

%% @doc Handles nodes notifications about supernormal exits (no need to reconnect).
%% The server casts this message if the application is about to be stopped.
%% @see enreconn_app:stop/1

handle_cast (Msg, State) ->
  {noreply, process_msg(Msg, State)}.

%% @doc Handles `{nodeup, ...}' and `{nodedown, ...}' messages.

handle_info (Info, State) ->
  {noreply, process_msg(Info, State)}.

terminate (_Reason, State) ->
  %% As a matter of fact its not necessary 'cause
  %% the process is just dying so the monitor automagically unregisters
  %% and the ETS table cease to exists BUT ordnung muss sein!!!!
  net_kernel:monitor_nodes(false, [{node_type, all}]),
  ets:delete(State#state.tid).

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ======================= Local/internal functions =========================

%% @doc Processes incoming messages.

-spec process_msg ({nodeup | nodedown, node(), list()} |
                   {enreconn_node_stop, node()}, state()) -> state().

process_msg ({enreconn_node_stop, Node}, State) ->
  unregister_node(Node, State);
process_msg ({nodeup, _Node, _InfoList}, State) ->
  State;
process_msg ({nodedown, Node, _InfoList}, State) ->
  node_down(Node, State).  

%% @doc Reaction when a node is down.

-spec node_down (node(), state()) -> state().

node_down (Node, State) ->
  reconnect_if_needed(ets:lookup(State#state.tid, Node) =:= [], 
                      Node, State).

reconnect_if_needed (false, Node, State) ->
  true = ets:delete(State#state.tid, Node),
  State;
reconnect_if_needed (true, Node, State) ->
  erlang:spawn(?MODULE, reconnect, [pang, Node]),
  State.

%% @doc Unregisters node and does not try to reconnect.

unregister_node (Node, State) ->
  true = ets:insert(State#state.tid, {Node}),
  State.

%% @doc Keep reconnecting a node via pinging till it answers with `pong'.

-spec reconnect (PreviousAnswer::ping|pong, node()) -> ok.

reconnect (pang, Node) ->
  try
    reconnect(net_adm:ping(Node), Node)
  catch
    E:R ->
     error_logger:error_report([{enreconn, node_reconnected},
                                {what, E},
                                {why, R},
                                {node, Node}])
  end;
reconnect (pong, Node) ->
  %% good, the node has answered
  error_logger:info_report([{enreconn, node_reconnected},
                            {node, Node}]),
  ok.
