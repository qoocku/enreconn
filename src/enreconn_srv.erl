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
          ping/1,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-export ([reconnect/3, 
          default_callback/1]).
                                 
-type callback () :: {module(), atom()}.
%% Reconnection callback.
-record (state, {tid      :: ets:tid(),
                 callback :: callback()}).
-type state () :: #state{}.

-export_type ([callback/0]).

%% @doc The server process initialisation.
%% First, it registers itself as nodes monitoring
%% messages receiver (via {@std_doc net_kernel.html#monitor_nodes-2}).
%% Then it opens ETS table to hold dis-connected node names.

init (_) ->
  ok           = net_kernel:monitor_nodes(true, [{node_type, all}]),
  Tid          = ets:new(?MODULE, []),
  {ok, {M, F}} = application:get_env(enreconn, callback),
  {ok, #state{tid      = Tid, 
              callback = {M, F}}}.

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
                   {forget, node()}                    |
                   {enreconn_node_stop, node()}, state()) -> state().

process_msg ({forget, Node}, State) ->
  ets:delete(State#state.tid, Node),
  State;
process_msg ({enreconn_node_stop, Node}, State) ->
  unregister_node(Node, State);
process_msg ({nodeup, _Node, _InfoList}, State) ->
  State;
process_msg ({nodedown, Node, _InfoList}, State) ->
  node_down(Node, State).  

%% @doc Reaction when a node is down.

-spec node_down (node(), state()) -> state().

node_down (Node, State) ->
  NodeStr       = erlang:atom_to_list(Node),
  {ok, Regexps} = application:get_env(exclude),
  case lists:any(fun (Regexp) ->
                     case re:run(NodeStr, Regexp) of
                       {match, _} ->
                         true;
                       nomatch ->
                         false
                     end
                 end, Regexps) of
    false ->
      reconnect_if_needed(ets:lookup(State#state.tid, Node), Node, State);
    true ->
      State
  end.

reconnect_if_needed ([{Node, unregistered}], Node, State) ->
  true = ets:delete(State#state.tid, Node),
  State;
reconnect_if_needed ([{Node, reconnecting}], Node, State) ->
  State;
reconnect_if_needed ([], Node, State) ->
  true = ets:insert(State#state.tid, {Node, reconnecting}),
  erlang:spawn(?MODULE, reconnect, [pang, Node, now()]),
  State.

%% @doc Unregisters node and does not try to reconnect.

unregister_node (Node, State) ->
  true = ets:insert(State#state.tid, {Node, unregistered}),
  State.

%% @doc Keep reconnecting a node via pinging till it answers with `pong'.

-spec reconnect (PreviousAnswer::ping|pong, node(), erlang:now()) -> ok.

reconnect (pang, Node, Started) ->
  try
    error_logger:info_report([{enreconn, reconnect},
                              {what, reconnection_try},
                              {lasts, timer:now_diff(now(), Started)/1000},
                              {node, Node}]),
    timer:sleep(cfg_var(reconnection_idle_time, 10000)),
    RT =  cfg_var(reconnection_timeout, 3600000),
    case RT of
      infinity ->
        ?MODULE:reconnect(net_adm:ping(Node), Node, Started);
      RT ->
        case (timer:now_diff(now(), Started)/1000 < RT) of
          true ->
            ?MODULE:reconnect(net_adm:ping(Node), Node, Started);
          false ->
            error_logger:info_report([{enreconn, reconnect},
                                      {what, reconnection_timeout},
                                      {node, Node}]),
            forget_node(Node)
        end
    end
  catch
    E:R ->
     error_logger:error_report([{enreconn, reconnect},
                                {what, E},
                                {why, R},
                                {node, Node}]),
      forget_node(Node)
  end;
reconnect (pong, Node, _Started) ->
  %% good, the node has answered
  error_logger:info_report([{enreconn, node_reconnected},
                            {node, Node}]),
  forget_node(Node),
  ok.

forget_node (Node) ->
  gen_server:cast(?MODULE, {forget, Node}).

%% @doc Special ping version (do not catch exceptions).

ping (Node) ->
  gen_server:call({net_kernel, Node}, {is_auth, node()}, infinity).

%% @doc Returns the application configuration variable value.
%% Return `Def' default value if no such variable is found in the apps config.

-spec cfg_var (atom(), any()) -> any().

cfg_var (Name, Def) ->
  case application:get_env(enreconn, Name) of 
    undefined -> Def; 
    {ok, Val} -> Val 
  end.

%% @doc Default callback applied when a node is reconnected.

-spec default_callback (node()) -> ok.

default_callback (Node) when is_atom(Node) ->
  ok.
