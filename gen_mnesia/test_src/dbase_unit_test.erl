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
-module(dbase_unit_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/db_passwd.hrl").
-include("test_src/db_shop.hrl").
-include("src/db_lock.hrl").
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
%% --------------------------------------------------------------------
-export([node_name/1]).

%% ====================================================================
%% External functions
%% ====================================================================
node_name(Name)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(Name++"@"++Host),    
    Node.


%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------

% Single Mnesia 
clean_start_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    rpc:call(NodeA,init,stop,[]),
    rpc:call(NodeB,init,stop,[]),
    timer:sleep(500),
    ok.
start_mnesia_test()->	  
    ?assertEqual(ok,application:start(gen_mnesia)),
  %  ?assertEqual([leader],db_lock:read_all()),
 %   ok=db_gen_mnesia:create_table(),
    ok.

create_table_lock_test()->
%    db_lock:create_table(),
 %   ?assertEqual({atomic,ok},db_lock:create()),
  %  ?assertEqual([leader],db_lock:read_all()),
  %  ?assertEqual(true,db_lock:is_open()),
   % ?assertEqual(false,db_lock:is_open()),
    ok.
create_table_passwd_test()->
    Table=passwd,
    Args=[{attributes, record_info(fields, passwd)}],
    ?assertEqual(ok,gen_mnesia:create_table(Table,Args)),

    
    ok.

%init_test()->
%    MnesiaConfig=[{passwd,[{attributes, record_info(fields, passwd)}
			  %,{disc_copies,[node()]}
%			  ]}],
%    ok=gen_mnesia_lib:start([],MnesiaConfig),
%    ok.
passwd_test()->
    ?assertEqual({atomic,ok},db_passwd:create("David",david1)),
    ?assertEqual({atomic,ok},db_passwd:create("David",david2)),
    ?assertEqual({atomic,ok},db_passwd:create("Joq",joq1)),
       ?assertEqual([{"Joq",joq1}],db_passwd:read("Joq")),  
    ?assertEqual([{"Joq",joq1},{"David",david2}],db_passwd:read_all()),    
    ok.

create_table_shop_test()->
    Table=shop,
    Args=[{attributes, record_info(fields, shop)}],
    ?assertEqual(ok,gen_mnesia:create_table(Table,Args)).

%init_test()->
%    MnesiaConfig=[{shop,[{attributes, record_info(fields, shop)}
			  %,{disc_copies,[node()]}
%			  ]}],
%    ok=gen_mnesia_lib:start([],MnesiaConfig),
%    ok.
shop_test()->
    ?assertEqual({atomic,ok},db_shop:create("asus_13",12500)),
    ?assertEqual({atomic,ok},db_shop:create("hp_15",14899)),
    ?assertEqual({atomic,ok},db_shop:create("MacPro",25999)),
    ?assertEqual([{"hp_15",14899},
		  {"asus_13",12500},
		  {"MacPro",25999}],db_shop:read_all()),    
    ok.


%%% Distributed mnesia 
start_nodes_to_add_test()->
    {ok,Host}=inet:gethostname(),




    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    ?assertEqual({ok,NodeA},slave:start(Host,a,"-pa ebin -pa test_ebin -setcookie abc")),
    ?assertEqual({ok,NodeB},slave:start(Host,b,"-pa ebin -pa test_ebin -setcookie abc")),
    ok.

add_nodes_test()->
   {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    ?assertEqual(ok,gen_mnesia:add_node(NodeA)),
    ?assertEqual(ok,gen_mnesia:add_node(NodeB)),
    ok.

add_table_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    ?assertEqual(ok,gen_mnesia:add_table(NodeA,passwd,ram_copies)),
    ?assertEqual(ok,gen_mnesia:add_table(NodeB,passwd,ram_copies)),
    ?assertEqual(ok,gen_mnesia:add_table(NodeA,shop,ram_copies)),
    ?assertEqual(ok,gen_mnesia:add_table(NodeB,shop,ram_copies)),
    ok.

node_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    ?assertEqual([{"Joq",joq1},
		 {"David",david2}],rpc:call(NodeB,db_passwd,read_all,[])),
    ?assertEqual({atomic,ok},rpc:call(NodeA,db_passwd,create,["Erika",123])),
    ?assertEqual([{"Joq",joq1},
		 {"Erika",123},
		 {"David",david2}],rpc:call(NodeB,db_passwd,read_all,[])),
    ok.
stop_restart_node_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    %-- check_running nodes
    ?assertEqual([],gen_mnesia_lib:check_stopped_db_nodes()),
    slave:stop(NodeA),
    timer:sleep(50),    
    ?assertEqual([NodeA],gen_mnesia_lib:check_stopped_db_nodes()), 
    ?assertMatch({badrpc,_},rpc:call(NodeA,db_passwd,read_all,[])),

    ?assertEqual({ok,NodeA},slave:start(Host,a,"-pa ebin -pa test_ebin -setcookie abc")),  
    timer:sleep(50), 
    ?assertEqual([],gen_mnesia_lib:check_stopped_db_nodes()),
    slave:stop(NodeA),
    slave:stop(NodeB),
    timer:sleep(50), 
    ?assertEqual([NodeB,NodeA],gen_mnesia_lib:check_stopped_db_nodes()), 

    ?assertEqual({ok,NodeA},slave:start(Host,a,"-pa ebin -pa test_ebin -setcookie abc")),  
    ?assertEqual({ok,NodeB},slave:start(Host,b,"-pa ebin -pa test_ebin -setcookie abc")), 
    timer:sleep(10), 
    ?assertEqual([],gen_mnesia_lib:check_stopped_db_nodes()),
    ?assertEqual([{"Joq",joq1},
		  {"Erika",123},
		 {"David",david2}],rpc:call(NodeA,db_passwd,read_all,[])),
    ?assertEqual([{"Joq",joq1}],rpc:call(NodeB,db_passwd,read,["Joq"])),
    ok.

some_test_xx()->
    Z=do(qlc:q([X || X <- mnesia:table(passwd)])),
    ?assertEqual(glurk,[{XId,XPwd}||{passwd,XId,XPwd}<-Z]),
   % X1=[X || X <- mnesia:table(passwd)],
   % Z=do(qlc:q(X1)),
   % ?assertEqual(glurk,Z),
    ok.
    
    
stop_test_xx()->
    init:stop().

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
