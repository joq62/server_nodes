-module(db_passwd).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_passwd.hrl").


-define(TABLE,passwd).
-define(RECORD,passwd).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create([?MODULE,Id,PassWd]) ->
    create(Id,PassWd).
create(Id,PassWd) ->
    Record=#?RECORD{user_id=Id,passwd=PassWd},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{XId,XPwd}||{?RECORD,XId,XPwd}<-Z].



read(Id) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.user_id==Id])),
    [{XId,XPwd}||{?RECORD,XId,XPwd}<-Z].

update(Id,NewPwd) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.user_id==Id],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{user_id=Id,passwd=NewPwd})
		end
	end,
    mnesia:transaction(F).

delete(Id) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.user_id==Id],
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
