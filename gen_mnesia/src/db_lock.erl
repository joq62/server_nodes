-module(db_lock).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_lock.hrl").

-define(LockTimeOut, 15). %% 30 sec 
-define(LeaderLock,leader).
-define(TABLE,lock).
-define(RECORD,lock).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).


create() ->
    create(?LeaderLock,0).
create({?MODULE,LockId}) ->
    create(LockId,0).
create(LockId,Time) ->
    F = fun() ->
		Record=#?RECORD{lock_id=LockId,time=Time},		
		mnesia:write(Record) end,
    mnesia:transaction(F).



read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{LockId,Time}||{?RECORD,LockId,Time}<-Z].

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [LockId||{?RECORD,LockId,_Time}<-Z].

	


read(LockId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.lock_id==LockId])),
    [{YLockId,Time}||{?RECORD,YLockId,Time}<-Z].

is_open()->
    is_open(?LeaderLock).

is_open(LockId)->
    is_open(LockId,?LockTimeOut).
is_open(LockId,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,LockId}) of
		  []->
		      mnesia:abort([]);
		  [LockInfo] ->
		      CurrentTime=erlang:system_time(seconds),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff == LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff < LockTimeOut->
			       mnesia:abort(LockId)
		      end
	      end
      end,
    IsOpen=case mnesia:transaction(F) of
	       {atomic,ok}->
		   true;
	       _->
		   false
	   end,
    IsOpen.
		      
	      
delete(LockId) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,LockId}),
			    X#?RECORD.lock_id==LockId],
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
