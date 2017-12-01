%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_discovery).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type state() :: #{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', state(), pos_integer()}.
init([]) -> {'ok', #{startup => kz_time:now_s()}, ?MILLISECONDS_IN_SECOND}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Msg, #{startup := Startup} = State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, next_timeout(kz_time:elapsed_s(Startup))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('timeout', #{startup := Startup} = State) ->
    _ = sbc_discovery(),
    {'noreply', State, next_timeout(kz_time:elapsed_s(Startup))};
handle_info(_Msg, #{startup := Startup} = State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State, next_timeout(kz_time:elapsed_s(Startup))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, _State) ->
    lager:debug("ecallmgr discovery terminating: ~p",[Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 5 -> ?MILLISECONDS_IN_SECOND;
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 10 -> ?MILLISECONDS_IN_SECOND * 3;
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 20 -> ?MILLISECONDS_IN_SECOND * 5;
next_timeout(_Elapsed) -> ?MILLISECONDS_IN_MINUTE * 5.

sbc_acl_filter({_K, V}) ->
    kz_json:get_ne_binary_value(<<"network-list-name">>, V) =:= <<"authoritative">>.

sbc_cidr(_, JObj, Acc) ->
    [kz_json:get_value(<<"cidr">>, JObj) | Acc].

sbc_cidrs(ACLs) ->
    SBCs = kz_json:filter(fun sbc_acl_filter/1, ACLs),
    lists:flatten(kz_json:foldl(fun sbc_cidr/3, [], SBCs)).

sbc_address_foldl(_, JObj, Acc) ->
    [kz_json:get_ne_binary_value(<<"address">>, JObj) | Acc].

sbc_addresses(#kz_node{roles=Roles}) ->
    Listeners = kz_json:get_json_value(<<"Listeners">>, props:get_value(<<"Proxy">>, Roles)),
    lists:usort(kz_json:foldl(fun sbc_address_foldl/3, [], Listeners)).

sbc_node(#kz_node{node=Name}=Node) ->
    {kz_term:to_binary(Name), sbc_addresses(Node)}.

sbc_verify_ip(IP, CIDRs) ->
    lists:any(fun(CIDR) -> kz_network_utils:verify_cidr(IP, CIDR) end, CIDRs).

sbc_discover({Node, IPs}, CIDRs, Acc) ->
    case lists:filter(fun(IP) -> not sbc_verify_ip(IP, CIDRs) end, IPs) of
        [] -> Acc;
        _ -> [{Node, IPs} | Acc]
    end.

-spec filter_acls(kz_json:object()) -> kz_json:object().
filter_acls(ACLs) ->
    kz_json:filter(fun filter_acls_fun/1, ACLs).

-spec filter_acls_fun({kz_json:path(), kz_json:json_term()}) -> boolean().
filter_acls_fun({_Name, ACL}) ->
    kz_json:get_value(<<"authorizing_type">>, ACL) =:= 'undefined'.

sbc_acl(IPs) ->
    kz_json:from_list([{<<"type">>, <<"allow">>}
                      ,{<<"network-list-name">>, ?FS_SBC_ACL_LIST}
                      ,{<<"cidr">>, [kz_network_utils:to_cidr(IP) || IP <- IPs]}
                      ]).

sbc_acls(Nodes) ->
    [{Node, sbc_acl(IPs)} || {Node, IPs} <- Nodes].

-spec sbc_discovery() -> any().
sbc_discovery() ->
    ACLs = filter_acls(ecallmgr_config:get_json(<<"acls">>, kz_json:new(), <<"default">>)),
    CIDRs = sbc_cidrs(ACLs),
    Nodes = [sbc_node(Node) || Node <- kz_nodes:with_role(<<"Proxy">>)],
    case lists:foldl(fun(A, C) -> sbc_discover(A, CIDRs, C) end, [], Nodes) of
        [] -> 'ok';
        Updates ->
            Names = lists:map(fun({Node, _}) -> Node end, Updates),
            lager:debug("adding authoritative acls for ~s", [kz_binary:join(Names)]),
            ecallmgr_config:set_node(<<"acls">>, kz_json:set_values(sbc_acls(Updates), ACLs), <<"default">>),
            ecallmgr_maintenance:reload_acls()
    end.
