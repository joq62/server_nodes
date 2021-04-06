%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(gen_mnesia_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------


%% External exports
-export([initial_start/0,
	 create_table/2,
	 initiate_added_node/1,
	 add_table/3
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

initial_start()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start().

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

create_table(Table,Args)->
     {atomic,ok}=mnesia:create_table(Table,Args),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
initiate_added_node(Vm)->
    Result=case net_adm:ping(Vm) of
	       pong->
		   stopped=rpc:call(Vm,mnesia,stop,[]),
		   ok=mnesia:delete_schema([Vm]),
		   ok=rpc:call(Vm,mnesia,start,[]),
		   {ok,[Vm]}=mnesia:change_config(extra_db_nodes, [Vm]);
	       pang ->
		   {error,[not_running,Vm]}
	   end,    
    Result.


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

add_table(Vm,Table,StorageType)->
    mnesia:add_table_copy(Table, Vm,StorageType),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

