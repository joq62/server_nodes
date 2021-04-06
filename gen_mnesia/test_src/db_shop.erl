-module(db_shop).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_shop.hrl").


-define(TABLE,shop).
-define(RECORD,shop).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create([?MODULE,Item,Price]) ->
    create(Item,Price).
create(Item,Price) ->
    Record=#?RECORD{item=Item,price=Price},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{XItem,XPrice}||{?RECORD,XItem,XPrice}<-Z].

read(Item) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.item==Item])),
    [{XItem,XPrice}||{?RECORD,XItem,XPrice}<-Z].

update(Item,NewPrice) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Item}),
			    X#?RECORD.item==Item],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{item=Item,price=NewPrice})
		end
	end,
    mnesia:transaction(F).

delete(Item) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Item}),
			    X#?RECORD.item==Item],
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
