%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm  
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(gen_mnesia). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("infra.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{}).

	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% server interface

-export([add_node/1,
	 delete_node/1,
	 add_table/3,
	 create_table/2,
	 sys_info/0,
	 glurk/0]).

-export([init_table_info/1,
	 delete_schema_file/0,
	 load_textfile/2,
	 load_textfile/1,
	 ping/0	 
	]).




-export([start/0,
	 stop/0
	 ]).
%% internal 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

glurk()->
    {ok,node(),?MODULE,?LINE}.
%C="https://"++Uid++":"++Pwd++"@github.com/"++Uid++"/"++SId++".git".

%% Asynchrounus Signals
%boot_strap()->
 %   PortStr=atom_to_list(PortArg),
 %   Port=list_to_integer(PortStr),
   % application:set_env([{boot_service,{port,Port}}]),
%    application:start(boot_service).
       
%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
sys_info()->
    gen_server:call(?MODULE,{sys_info},infinity).
add_table(Vm,Table,StorageType)->
      gen_server:call(?MODULE,{add_table,Vm,Table,StorageType},infinity).
create_table(Table,Args)->
    gen_server:call(?MODULE,{create_table,Table,Args},infinity).
add_node(Vm)->
    gen_server:call(?MODULE,{add_node,Vm},infinity).
delete_node(Vm)->
    gen_server:call(?MODULE,{delete_node,Vm},infinity).


init_table_info(Info)->
    gen_server:call(?MODULE,{init_table_info,Info},infinity).

delete_schema_file()->
    gen_server:call(?MODULE,{delete_schema_file},infinity).

load_textfile(Filename,Bin)->    
    gen_server:call(?MODULE,{load_textfile,Filename,Bin},infinity).
load_textfile(FileName)->    
    gen_server:call(?MODULE,{load_textfile,FileName},infinity).
    
ping()->
    gen_server:call(?MODULE,{ping},infinity).

%%___________________________________________________________________



%%-----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};


handle_call({sys_info}, _From, State) ->
    Reply=mnesia:system_info(),
    {reply, Reply, State};

handle_call({create_table,Table,Args}, _From, State) ->
    Reply=rpc:call(node(),gen_mnesia_lib,create_table,[Table,Args]),
    {reply, Reply, State};

handle_call({add_table,Vm,Table,StorageType}, _From, State) ->
    Reply=rpc:call(node(),gen_mnesia_lib,add_table,[Vm,Table,StorageType]),
    {reply, Reply, State};


handle_call({delete_node,Vm}, _From, State) ->
    Reply=rpc:call(Vm,application,stop,[gen_mnesia]),
    {reply, Reply, State};

handle_call({add_node,Vm}, _From, State) ->
    Reply=case net_adm:ping(Vm) of
	      pong->
		  rpc:call(Vm,application,stop,[gen_mnesia]),
		  ok=mnesia:delete_schema([Vm]),
		  case rpc:call(Vm,application,start,[gen_mnesia]) of
		      ok->
			  case mnesia:change_config(extra_db_nodes, [Vm]) of
			      {ok,[Vm]}->
				  ok;
			      Err->
				  {error,[Err,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
			  end;
		      Err->
			  {error,[Err,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
		  end;
	      pang ->
		  {error,[not_running,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
	   end,  
    {reply, Reply, State};

handle_call({init_table_info,Info}, _From, State) ->
    Reply=gen_mnesia_lib:create_table(Info),
    {reply, Reply, State};

handle_call({delete_schema_file}, _From, State) ->
    Reply=os:cmd("rm -rf Mne*"),
    {reply, Reply, State};

handle_call({load_textfile,FileName}, _From, State) ->
    Reply=mnesia:load_textfile(FileName),
 %   file:delete(Filename),
    {reply, Reply, State};

handle_call({load_textfile,Filename,Bin}, _From, State) ->
    file:delete(Filename),
    ok=file:write_file(Filename,Bin),
    Reply=mnesia:load_textfile(Filename),
 %   file:delete(Filename),
    {reply, Reply, State};


handle_call({stop}, _From, State) ->
    mnesia:stop(),
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({glurk}, State) ->

    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
