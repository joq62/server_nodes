-module(db_gen_mnesia).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_gen_mnesia.hrl").


-define(TABLE,db_info).
-define(RECORD,db_info).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create([?MODULE,Table,Args]) ->
    create(Table,Args).
create(Table,Args) ->
    Record=#?RECORD{table=Table,args=Args},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Table,Args}||{?RECORD,Table,Args}<-Z].

read(Table) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.table==Table])),
    [{XTable,Args}||{?RECORD,XTable,Args}<-Z].


update(Table,NewArgs)->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Table}),
			       X#?RECORD.table==Table],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{table=Table,args=NewArgs})
		end
	end,
    mnesia:transaction(F).

delete(Table) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Table}),
			    X#?RECORD.table==Table],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
