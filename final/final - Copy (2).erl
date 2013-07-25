-module(final).
-compile(export_all).

-define(RV_RANGE,30).
-define(ROADLEN,150).
-define(NUM_LANES,4).
-define(CRAZY_PROB,0.2).
-define(LANE_PROB,0.3).
-define(MAX_BRAKING_TIMESLOTS,5).
-define(BRAKING_SPEED,50).

main(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID) ->
    init_control(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID,self()).

%%      __      __
%%     /\ \    / /
%%    /  \ \  / / 
%%   / /\ \ \/ /  
%%  / ____ \  /   
%% /_/    \_\/    
%%                
%%             
avMain(X,Y,RvPid,ControlPid) ->
    receive
	{new_location,NewX,NewY,IsCrashed} ->               %Received a new location from the control unit  
	    case IsCrashed of                                  %If this vehicle has just crashed
		true -> tryToSend(RvPid,{rv_incoming_av,self(),NewX}); % If crashed, notify RV
		_ -> ok
	    end,
	    tryToSend(RvPid,{rv_where_are_you,self(),NewX,NewY}), % Ask RV where it is so that we can see if we're still in range
	    % Ask control to give us an RV if we don't have one :(
	    case is_pid(RvPid) of
		false -> ControlPid ! {av_gib_rv_plz,self()};
		_ -> ok
	    end,
	    avMain(NewX,NewY,RvPid,ControlPid);

	{av_range,false} -> % Reply from RV - we're not in range. Ask control for a new RV.
	    ControlPid ! {av_gib_rv_plz,self()},
	    tryToSend(RvPid,{rv_leave,self()}),
	    avMain(X,Y,0,ControlPid);

	{av_range,true} -> % Reply from RV - we're in range, yay.
	    avMain(X,Y,RvPid,ControlPid);

	{av_incoming_event,EventX} ->
	    % Check if our X is larger than the EventX - because that means we're going to be impacted by it (ahead of us)
	    case X>EventX of
		true ->
		    ControlPid ! {av_braking, self()};
		_ -> ok
	    end,
	    avMain(X,Y,RvPid,ControlPid);
	    
	{av_new_rv,NewRvPid} ->  % Got a new RV assignment from the control
	    tryToSend(NewRvPid,{rv_join,self()}),
	    avMain(X,Y,NewRvPid,ControlPid)
    end,
    ok.

%% Attempts to send to a sepcific PID. If it fails, doesn't crash anything :)
tryToSend(Pid,Content) ->
    case is_pid(Pid) of
	true ->
	    Pid ! Content;
	_ -> ok
    end.

%%
%%  _______      __
%% |  __ \ \    / /
%% | |__) \ \  / / 
%% |  _  / \ \/ /  
%% | | \ \  \  /   
%% |_|  \_\  \/    
%%                 
%%                 
%% RV Main entry point - sends a join to the BS and then continues to the next function
rvMain(X,Y,BSPid,ControlPid) ->
    BSPid ! {bs_join,self()},
    rvMain(X,Y,BSPid,ControlPid,[]).

%% RV method
rvMain(X,Y,BSPid,ControlPid,AvPids) ->
    receive
	{new_location,NewX,NewY,IsCrashed} ->               % Received a new location from the control unit  
	    case IsCrashed of                               % If this vehicle has just crashed
		true -> tryToSend(BSPid,{bs_incoming_event,self(),NewX}), % If crashed, notify base station
			[ Pid ! {av_incoming_event,NewX} || Pid <- AvPids ]; % send a crash event to all AVs
		_ -> ok
	    end,
	    % Now let's check if we need to ask for a new BS
	    case (X>=(?ROADLEN/2)) and (NewX=<(?ROADLEN/2)) of % Transitioning from left to right BS
		true -> BSPid!{bs_leave,self()};
		_ -> ok
	    end,
	    case NewX>=X of % Transitioning from right to left BS (modulu at the end of the road)
		true -> BSPid!{bs_leave,self()};
		_ -> ok
	    end,
	    rvMain(NewX,NewY,BSPid,ControlPid,AvPids);

	{rv_where_are_you,Pid,AvX,_} -> % AV asked us about our range to them
	    case abs(AvX-X)=<?RV_RANGE of
		true -> Pid ! {av_range,true}; % In range
		false -> Pid ! {av_range,false} % Not in range
	    end,
	    rvMain(X,Y,BSPid,ControlPid,AvPids);

	{rv_incoming_av,InPid,InX} -> % Crash event from an AV
	    [ Pid ! {av_incoming_event,InX} || Pid <- lists:delete(InPid,AvPids) ], % Send crash messages to all cars except the one that we got the message from
	    BSPid ! {bs_incoming_event,self(),InX}, % Notify the BS about the crash as well
	    case InX =< X of % Event affects this RV (is to the right of us) - let the control know we're braking
		true -> ControlPid ! {av_braking, self()};
		_ -> ok
	    end,
	    rvMain(X,Y,BSPid,ControlPid,AvPids);

	{rv_incoming_bs,InX} -> % Crash event from the base station
	    case X+?RV_RANGE >= InX of    % Check if the event is in the range of our interests (because we advance to the left)
		true -> [ Pid ! {av_incoming_event,InX} || Pid <- AvPids ]; % Send crash messages to all cars
		_ -> ok
	    end,
	    case InX =< X of % Event affects this RV (is to the right of us) - let the control know we're braking
		true -> ControlPid ! {av_braking, self()};
		_ -> ok
	    end,
	    rvMain(X,Y,BSPid,ControlPid,AvPids);
	    
	{rv_leave,Pid} ->  % Delete a PID from the list while calling ourselves recursively
	    rvMain(X,Y,BSPid,ControlPid,lists:delete(Pid,AvPids));

	{rv_join,Pid} -> % Insert a new PID to the list while calling ourselves recursively
	    rvMain(X,Y,BSPid,ControlPid,[Pid|AvPids]);
	
	{rv_new_bs,NewBSPid} ->
	    NewBSPid ! {bs_join,self()},
	    rvMain(X,Y,NewBSPid,ControlPid,AvPids)
    end.

%%  ____   _____ 
%% |  _ \ / ____|
%% | |_) | (___  
%% |  _ < \___ \ 
%% | |_) |____) |
%% |____/|_____/ 
%%               
               
%% The function that will start up the BS - waits for an init message and then moves on
bs_Init(IsRight,RvPids) ->
    receive
	{init,NewOtherBS} -> 
	    bs(IsRight,NewOtherBS,RvPids)
    end.

%% Base station main method, after init is done.
bs(IsRight,OtherBS,RvPids) ->
    receive
	{bs_incoming_event,InPid,InX} -> % Incoming event, send to all RVs
	    [ Pid ! {rv_incoming_bs,InX} || Pid <- lists:delete(InPid,RvPids) ], % Send crash messages to all RVs except the one that we got the message from

%%%%%%%%%%%% This might need some work later on, a lot of spare messages: %%%%%%%%%%%%%%%
	    case IsRight of
		true -> OtherBS ! {bs_incoming_event, InPid, InX}; % This will work on the other BS's list of RVs (the PID won't be deleted there)
		_ -> ok
	    end,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    bs(IsRight,OtherBS,RvPids);
	{bs_leave,RvPid} -> % Tell an RV it should swap to another BS. Remove him from our list.
	    RvPid ! {rv_new_bs, OtherBS},
	    bs(IsRight,OtherBS,lists:delete(RvPid,RvPids));

	{bs_join,RvPid} -> % Join message from an RV - add to list
	    bs(IsRight,OtherBS,[RvPid|RvPids])
    end.

%%
%%   _____ ____  _   _ _______ _____   ____  _      
%%  / ____/ __ \| \ | |__   __|  __ \ / __ \| |     
%% | |   | |  | |  \| |  | |  | |__) | |  | | |     
%% | |   | |  | | . ` |  | |  |  _  /| |  | | |     
%% | |___| |__| | |\  |  | |  | | \ \| |__| | |____ 
%%  \_____\____/|_| \_|  |_|  |_|  \_\\____/|______|
%%                                                  
%% Control main entry point. Inits some variables and shit...
init_control(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID,MainNodeID) ->
    % Set up the base stations and init them
    LeftBS = spawn(BSNodeID, ?MODULE, bs_Init, [false,[]]),
    RightBS = spawn(BSNodeID, ?MODULE, bs_Init, [true,[]]),
    LeftBS ! {init, RightBS},
    RightBS ! {init, LeftBS},

    % Create NUM_LANES ets tables with names lane1,lane2,...,laneN
    [ets:new(list_to_atom("lane"++[48+Id]), [ordered_set, named_table, public]) || Id <- lists:seq(1,?NUM_LANES)],

    % Spawn vehicles
    [ createVehicle(false,VehicleNodeID,{}) || _ <- lists:seq(1,NumOfAVs) ], % AVs
    [ createVehicle(true, VehicleNodeID,{LeftBS,RightBS}) || _ <- lists:seq(1,NumOfRVs) ], % RVs
    control().    % Call control with no RV list

control() -> % Pulls out RV list
    % The following magic line pulls out all of the RVs from all etses
    NewListOfRVs = [ ets:match(list_to_atom("lane"++[48+Id]),{'_','_','$1',true,'_','_','_'}) || Id <- lists:seq(1,?NUM_LANES) ],
    control(lists:flatten(NewListOfRVs)).

control(ListOfRVs) ->
    receive
	{av_gib_rv_plz,CarPid} -> % Respond to a new RV request
	    {A,_B,_C,_D,_E,_F,_G} = findVehicleByPid(CarPid),
	    NewRV = findRVInRange(A,ListOfRVs), % Get an RV in range
	    CarPid ! {av_new_rv,NewRV}, % Let the car know about the new PID we found :)
	    control(ListOfRVs); % Recurse
	{av_braking,CarPid} -> 
	    {A,B,C,D,E,_F,G} = findVehicleByPid(CarPid),
	    NewF = ?MAX_BRAKING_TIMESLOTS, % Update braking timeslot
	    ets:insert(list_to_atom("lane"++[48+B]),{A,B,C,D,E,NewF,G}), % Reinsert into ets (update only)
	    control(ListOfRVs) % Recurse
    after 1000 -> % Relocate vehicles
	    [ relocateVehiclesInLaneByX(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Move X
	    [ relocateVehiclesInLaneByY(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Move Y
	    [ updateALanesYValues(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Update lane Y values
	    [ sendUpdatesToLane(list_to_atom("lane"++[48+Id]),ets:first(list_to_atom("lane"++[48+Id]))) || Id <- lists:seq(1,?NUM_LANES) ], % Send messages to cars
	    io:format("~nSizes:~n"),
	    [ io:format("~w  | ",[lists:nth(6,ets:info(list_to_atom("lane"++[48+B])))]) || B<- lists:seq(1,4) ],

	    control()  % Will go and pull out the list of RVs again and start over.
    end.

%% No more cars to send updates to for this lane
sendUpdatesToLane(_,'$end_of_table') ->
    ok;

sendUpdatesToLane(B,Curr) ->
    {X,Y,Pid,_IsRV,_IsCrazy,BrakingTimeSlot,_Speed} = hd(ets:lookup(B,Curr)), % Pull out first entry
    IsCrashed = case BrakingTimeSlot of
		    ?MAX_BRAKING_TIMESLOTS -> true; % Car has crashed, update IsCrashed to true
		    _ -> false
		end,
    Pid ! {new_location,X,Y,IsCrashed}, % Send the car the new information
    sendUpdatesToLane(B,ets:next(B,Curr)). % Recurse
    
%%
%%  _____  ______ _      ____   _____       _______ _____ ____  _   _  _____ 
%% |  __ \|  ____| |    / __ \ / ____|   /\|__   __|_   _/ __ \| \ | |/ ____|
%% | |__) | |__  | |   | |  | | |       /  \  | |    | || |  | |  \| | (___  
%% |  _  /|  __| | |   | |  | | |      / /\ \ | |    | || |  | | . ` |\___ \ 
%% | | \ \| |____| |___| |__| | |____ / ____ \| |   _| || |__| | |\  |____) |
%% |_|  \_\______|______\____/ \_____/_/    \_\_|  |_____\____/|_| \_|_____/ 
%%                                                                          
%%                                                                           
%% We'll mark all moved vehicles by Y coordinate = 0. After this is all done, we'll second pass on them and correct their Y coordinate.
%% Relocate vehicles by Y coordinate - main entry point.
relocateVehiclesInLaneByY(B) ->
    {size,NumOfCarsInLane}=lists:nth(6,ets:info(list_to_atom("lane"++[48+B]))),
    relocateByY(B,NumOfCarsInLane,0).    

%% Will update Y values for this lane
updateALanesYValues(B) ->
    AllKeysInLane = [ Key || {Key,_,_,_,_,_,_} <- ets:tab2list(list_to_atom("lane"++[48+B])) ], % Get all keys for this lane
    [ ets:update_element(list_to_atom("lane"++[48+B]),Key,{2,B}) || Key <- AllKeysInLane ]. % Fix all Ys for this lane

relocateByY(_Lane,_,'$end_of_table') ->
    ok;

relocateByY(_Lane,0,_CurrentCar) ->
    ok;

%% Relocates cars by their Y value for a specific lane
relocateByY(Lane,NumOfCarsInLane,0) ->
    First = ets:first(list_to_atom("lane"++[48+Lane])),
    Next = ets:next(list_to_atom("lane"++[48+Lane]),First),
    {A,B,C,D,E,F,G} = findVehicleByKey(First,Lane),
    case B of
	0 -> relocateByY(Lane,NumOfCarsInLane-1,Next); % Don't move vehicle - it has already moved here
	_ -> % We might need to relocate
	    NewY = giveNewY(Lane),
	    case NewY of
		Lane -> relocateByY(Lane,NumOfCarsInLane-1,Next); % No need to relocate
		_ -> % We need to relocate. Check for crashes
		    Res = ets:lookup(list_to_atom("lane"++[48+NewY]),A),
		    case length(Res) of 
			0 -> % No crash, move the car over :)
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    case doesPosExist(A,NewY) of
				true -> io:format("Fuuuuuuck3~n");
				false -> ok
			    end,
			    ets:insert(list_to_atom("lane"++[48+NewY]),{A,0,C,D,E,F,G}),
			    relocateByY(Lane,NumOfCarsInLane-1,Next);
			_ -> % There is a crash. Keep car in same lane, but mark as crashed
			    ets:insert(list_to_atom("lane"++[48+Lane]),{A,Lane,C,D,E,?MAX_BRAKING_TIMESLOTS,?BRAKING_SPEED}),
			    relocateByY(Lane,NumOfCarsInLane-1,Next)
		    end
	    end
    end;

relocateByY(Lane,NumOfCarsInLane,CurrVehicle) ->
    Next = ets:next(list_to_atom("lane"++[48+Lane]),CurrVehicle),
    {A,B,C,D,E,F,G} = findVehicleByKey(CurrVehicle,Lane),
    case B of
	0 -> relocateByY(Lane,NumOfCarsInLane-1,Next); % Don't move vehicle - it has already moved here
	_ -> % We might need to relocate
	    NewY = giveNewY(Lane),
	    case NewY of
		Lane -> relocateByY(Lane,NumOfCarsInLane-1,Next); % No need to relocate
		_ -> % We need to relocate. Check for crashes
		    Res = ets:lookup(list_to_atom("lane"++[48+NewY]),A),
		    case length(Res) of 
			0 -> % No crash, move the car over :)
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    ets:insert(list_to_atom("lane"++[48+NewY]),{A,0,C,D,E,F,G}),
			    relocateByY(Lane,NumOfCarsInLane-1,Next);
			_ -> % There is a crash. Keep car in same lane, but mark as crashed
			    ets:insert(list_to_atom("lane"++[48+Lane]),{A,Lane,C,D,E,?MAX_BRAKING_TIMESLOTS,?BRAKING_SPEED}),
			    relocateByY(Lane,NumOfCarsInLane-1,Next)
		    end
	    end
    end.

%% Gives a new Y value for a specific lane number	
giveNewY(Lane) ->
    case random:uniform()=<?LANE_PROB of
	true -> 
	    case Lane of
		1 -> 2;
		?NUM_LANES -> ?NUM_LANES-1;
		_ -> case random:uniform()=<0.5 of % Move lanes
			 true -> Lane-1;
			 false -> Lane+1
		     end
	    end;
	false ->
	    Lane
    end.

relocateVehiclesInLaneByX(B) -> % Pulls out size and sends to the relocateByX method
    {size,NumOfCarsInLane}=lists:nth(6,ets:info(list_to_atom("lane"++[48+B]))),
    relocateByX(B,NumOfCarsInLane,ets:first(list_to_atom("lane"++[48+B]))).

relocateByX(_Lane,_,'$end_of_table') ->
    ok;

relocateByX(_Lane,0,_CurrentCar) ->
    ok;

%% Most recursions will end up here
relocateByX(Lane,NumOfCarsInLane,CurrentCar) ->
    Next = ets:next(list_to_atom("lane"++[48+Lane]),CurrentCar),

    {A,B,C,D,E,F,G} = findVehicleByKey(CurrentCar,Lane),
    {ProposedX,ProposedSpeed,ProposedSlot} = giveNewX(A,G,E,F),

    Prev = ets:prev(list_to_atom("lane"++[48+Lane]),A), % This is the car ahead of us
    NewPrev = case Prev of
	'$end_of_table' -> 
		      ets:last(list_to_atom("lane"++[48+Lane]));
		  _ -> 
		      Prev
    end,
    if
	ProposedX =< NewPrev -> % Crash :(
	    NewX = case NewPrev+1>=?ROADLEN of
		       true -> 0;
		       false -> NewPrev+1
		   end,
	    NewSpeed = ?BRAKING_SPEED,
	    NewSlot = ?MAX_BRAKING_TIMESLOTS,
	    ets:delete(list_to_atom("lane"++[48+Lane]),A),
	    case doesPosExist(NewX,B) of
		true -> io:format("Fuuuuuuck1~n");
		false -> ok
	    end,
	    ets:insert(list_to_atom("lane"++[48+Lane]),{NewX,B,C,D,E,NewSlot,NewSpeed});
	true ->  % No crash
	    ets:delete(list_to_atom("lane"++[48+Lane]),A),
	    case doesPosExist(ProposedX,B) of
		true -> io:format("Fuuuuuuck2~n");
		false -> ok
	    end,
	    ets:insert(list_to_atom("lane"++[48+Lane]),{ProposedX,B,C,D,E,ProposedSlot,ProposedSpeed})
    end,
    relocateByX(Lane,NumOfCarsInLane-1,Next).

giveNewX(OldX,_Speed,IsCrazy,BrakingWindow) ->
    if
	BrakingWindow =/= 0 ->
	    NewSpeed = ?BRAKING_SPEED,
	    NewSlot = BrakingWindow-1;
	true ->
	    NewSlot = 0,
	    Delta = case IsCrazy of
			true -> random:uniform(30)-15; % Crazy driver
			false -> random:uniform(10)-5 % Not a crazy driver
		    end,
	    NewSpeed = 110 + Delta % New speed +- delta
    end,
    NewX = modulo(erlang:trunc(OldX - NewSpeed/(3.6)), ?ROADLEN), % New location modulu roadlength
    {NewX,NewSpeed,NewSlot}.

%%  _    _ ______ _      _____  ______ _____    ______ _    _ _   _  _____ 
%% | |  | |  ____| |    |  __ \|  ____|  __ \  |  ____| |  | | \ | |/ ____|
%% | |__| | |__  | |    | |__) | |__  | |__) | | |__  | |  | |  \| | (___  
%% |  __  |  __| | |    |  ___/|  __| |  _  /  |  __| | |  | | . ` |\___ \ 
%% | |  | | |____| |____| |    | |____| | \ \  | |    | |__| | |\  |____) |
%% |_|  |_|______|______|_|    |______|_|  \_\ |_|     \____/|_| \_|_____/ 
%%                                                                        
%%                                                                         

findVehicleByKey(Key,Lane) -> % Finds a vehicle by pid
    NewListOfRVs = ets:match(list_to_atom("lane"++[48+Lane]),{Key,'$1','$2','$3','$4','$5','$6'}),
    {A,B,C,D,E,F} = hd(lists:map(fun(Z) -> list_to_tuple(Z) end, NewListOfRVs)),
    {Key,A,B,C,D,E,F}.

findVehicleByPid(Pid) -> % Finds a vehicle by pid
    NewListOfRVs = [ ets:match(list_to_atom("lane"++[48+Id]),{'$1','$2',Pid,'$3','$4','$5','$6'}) || Id <- lists:seq(1,?NUM_LANES) ],
    Res =  getNonEmpty(NewListOfRVs),
    case Res of 
	no_non_empty -> 
	    io:format("Haven't found ~w~n",[Pid]); %,
%	    [ io:format("~w~n::::::::::::::::::::::::~n",[ets:tab2list(list_to_atom("lane"++[48+Id]))]) || Id <- lists:seq(1,4) ];% Print all ETSs
	_ -> {A,B,C,D,E,F} = list_to_tuple(lists:flatten(Res)),
	     {A,B,Pid,C,D,E,F}
    end.

getNonEmpty([]) ->
    no_non_empty;

getNonEmpty([H|T]) ->
    case length(H)>0 of
	true ->
	    H;
	false -> getNonEmpty(T)
    end.

findRVInRange(_X,[]) ->
    0; % No RV found. Return 0 (not a Pid)

%% Returns an RV key for a specific given X
findRVInRange(X,[H|T]) ->
    {A,_,_,_,_,_,_} = findVehicleByPid(H), % Pull out first RV, get it's X coordinate
    case abs(X-A)=<?RV_RANGE of % Is it in range?
	true ->
	    H; % Return it. Stop looking
	false ->
	    findRVInRange(X,T) % Not in range, keep looking
    end.

%% Creates a vehicle and spawns it
createVehicle(IsRV,VehicleNodeID,BSPids) ->
    IsCrazy = (random:uniform()=<?CRAZY_PROB), % Assigns True/False to IsCrazy based on the probability predefined
    Y = random:uniform(?NUM_LANES), % Pick Y randomly
    X = gibValidX(Y),
    NewCarPid =  case IsRV of % pick the correct function (avMain or rvMain) to be spawned (מושרץ)
		     true -> 
			 {LeftBS,RightBS} = BSPids,
			 case X=<(?ROADLEN/2) of % Pick out the correct BS for the RV
			     true ->
				 BS = RightBS;
			     false ->
				 BS = LeftBS
			 end,
			 spawn(VehicleNodeID,?MODULE,rvMain,[X,Y,BS,self()]); % New RV
		     false -> 
			 spawn(VehicleNodeID,?MODULE,avMain,[X,Y,0,self()]) % Spawn a new avMain on NodeID with X,Y, base station and the control PID.
		 end,

    ets:insert(list_to_atom("lane"++[48+Y]),{X,Y,NewCarPid,IsRV,IsCrazy,0,110}), % Insert the tuple {X,Y,Car PID,Is RV,Is Crazy,Time slots for braking,Speed}
					 	                                 % into lane Y
    ok.

gibValidX(Y) ->
    X = random:uniform(?ROADLEN), % Pick X randomly
    Res = doesPosExist(X,Y),
    case Res of
	true ->
	    gibValidX(Y);
	false -> X
    end.
    

doesPosExist(X,Y) ->
    Res = ets:lookup(list_to_atom("lane"++[48+Y]),X),
    case length(Res) of
	0 ->
	    false;
	_ -> 
	    true
    end.

modulo(X,Y) when X > 0 ->   
   X rem Y;

modulo(X,Y) when X < 0 ->   
    K = (-X div Y)+1,
    PositiveX = X + K*Y,
    PositiveX rem Y;

modulo(0,_Y) -> 
    0.
