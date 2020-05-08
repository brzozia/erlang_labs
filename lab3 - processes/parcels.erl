%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2020 03:27
%%%-------------------------------------------------------------------
-module(parcels).
-author("Natalia Brzozowska").

%% API
-export([makeParcelLockers/0, makePeople/1, findMin/4,findMyParcelLocker/2,findMyParcelLockerAndSendMsg/2, helper/3, seqFind/0, main/0,veryConcur/0,littleConcur/1, catchMsg/2]).


makeParcelLockers() -> [{ rand:uniform(10001)-1, rand:uniform(10001)-1} || _ <- lists:seq(1,1000)].
makePeople(N) -> [{ rand:uniform(10001)-1, rand:uniform(10001)-1} || _ <- lists:seq(1,N)].


findMin( {X1,Y1}, [H | []] , MinVal, Min) ->
  {X2,Y2}=H,
  Dist = math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)),
  case MinVal<Dist of
    true -> Min;
    false -> H
  end;
findMin( {X1,Y1}, [H |T] , MinVal, Min) ->
  {X2,Y2}=H,
  Dist = math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)),
  case MinVal<Dist of
    true -> findMin( {X1,Y1}, T , MinVal, Min);
    false ->  findMin( {X1,Y1}, T , Dist, H)
  end.


findMyParcelLocker(PersonLocation, LockerLocations) ->
  {X1,Y1}=PersonLocation,
  [H |T]= LockerLocations,
  {X2,Y2}=H,
  Dist = math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)),
  findMin(PersonLocation,T,Dist ,H).



findMyParcelLockerAndSendMsg(PersonLocation, LockerLocations) ->
  {X1,Y1}=PersonLocation,
  [H |T]= LockerLocations,
  {X2,Y2}=H,
  Dist = math:sqrt(math:pow((X1-X2),2) + math:pow((Y1-Y2),2)),
  receive
    Pid -> Pid ! findMin(PersonLocation,T,Dist ,H)
  after
    20000 -> ok
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seqFind() ->
  Lockers = makeParcelLockers(),
  People = makePeople(10000),
  [ findMyParcelLocker(H, Lockers) || H <- People ].




veryConcur() ->
  Lockers = makeParcelLockers(),
  People = makePeople(10000),

  lists:foreach( fun(H) -> spawn(?MODULE, findMyParcelLockerAndSendMsg,[H,Lockers]) ! self() end, People),
  catchMsg([],10000).


catchMsg(List,0) ->
  List;
catchMsg(List,Count) ->
  receive
    Z -> catchMsg([Z | List], Count-1 )
  after
    60000 -> ok
  end.





littleConcur(N)->
  Lockers = makeParcelLockers(),
  spawn(?MODULE, helper,[makePeople(N),Lockers,2500]) ! self(),
  spawn(?MODULE, helper,[makePeople(N),Lockers,2500]) ! self(),
  spawn(?MODULE, helper,[makePeople(N),Lockers,2500]) ! self(),
  spawn(?MODULE, helper,[makePeople(N),Lockers,2500]) ! self(),
  catchMsg([],4).


helper(_,_,0) -> catchMsg([],2500);
helper([H|T],Lockers,PeopleCount) ->
  findMyParcelLockerAndSendMsg(H,Lockers),
  helper(T,Lockers, PeopleCount-1).


main() ->
  {Tqs,_v}=timer:tc(fun littleConcur/1,[2500]),
  io:format("Time with processes=number of cores ~b~n ",[Tqs]),
  {Fqs,_v}=timer:tc(fun seqFind/0),
  io:format("Time of sequence operations: ~b~n ",[Fqs]),
  {Sqs,_v}=timer:tc(fun veryConcur/0),
  io:format("Time with lot of processes: ~b~n ",[Sqs]).



