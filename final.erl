-module(final).
-compile(export_all).
-import(display,[spawn_slave/1,spawn_master/4]). % Import the spawning functions from the display unit

-define(RV_RANGE,500). % RV range in meters, needs to be identical to display
-define(ROADLEN,6600). % Road length in meters, identical to display
-define(NUM_LANES,6). % Number of lanes to draw
-define(CRAZY_PROB,0.2). % Probability of a crazy driver (more variance in speeds)
-define(LANE_PROB,0.05). % Probability of a car switching lanes
-define(MAX_BRAKING_TIMESLOTS,5). % Amount of time a car should brake
-define(BRAKING_SPEED,50). % Speed of a braking car

%%   _____ _______       _____ _______ 
%%  / ____|__   __|/\   |  __ \__   __|
%% | (___    | |  /  \  | |__) | | |   
%%  \___ \   | | / /\ \ |  _  /  | |   
%%  ____) |  | |/ ____ \| | \ \  | |   
%% |_____/   |_/_/    \_\_|  \_\ |_|   
%%

%% Starts up the entire system! (Names are rather descriptive, don't you think?)
start_system(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID,ControlNodeID,SlaveNodeID,MasterNodeID) ->
    K=display:spawn_slave(SlaveNodeID), % Start slave display
    L=main(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID,ControlNodeID), % Start control and all AVs/RVs/BSs
    display:spawn_master(L,MasterNodeID,6,K). % Now start the master display unit

%% Starts up only the simulation (control and all vehicles) - no display units
main(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID,ControlNodeID) ->
    spawn(ControlNodeID,?MODULE,init_control,[NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID]).

%%      __      __
%%     /\ \    / /
%%    /  \ \  / / 
%%   / /\ \ \/ /  
%%  / ____ \  /   
%% /_/    \_\/    
%%                
%%
%% Main AV method             
avMain(X,Y,RvPid,ControlPid) ->
    receive
	exit -> % Exit AV gracefully
	    ok;

	{new_location,NewX,NewY,IsCrashed} -> %Received a new location from the control unit  
	    case IsCrashed of  %If this vehicle has just crashed
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

	{av_are_you_in_range, FromWho} -> % Tell display if we're in range of an RV or not (when asked by display)
	    FromWho ! {av_range_response, self(), is_pid(RvPid)},
	    avMain(X,Y,RvPid,ControlPid); 

	{av_incoming_event,EventX} -> % Incoming event (braking) from an RV
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
	exit -> % Exit gracefully, but first tell the BS to exit as well
	    BSPid ! exit; 
	{new_location,NewX,NewY,IsCrashed} ->  % Received a new location from the control unit  
	    case IsCrashed of    % If this vehicle has just crashed
		true -> tryToSend(BSPid,{bs_incoming_event,self(),NewX}), % If crashed, notify base station
			[ Pid ! {av_incoming_event,NewX} || Pid <- AvPids ]; % And also send a crash event to all AVs we know
		_ -> ok
	    end,
	    % Now let's check if we need to ask for a new BS
	    case (X>=(?ROADLEN/2)) and (NewX=<(?ROADLEN/2)) of % Transitioning from left to right BS
		true -> BSPid ! {bs_leave,self()}; % A leave message to the BS will trigger a new BS pid being sent to us later.
		_ -> ok
	    end,
	    case NewX>=X of % Transitioning from right to left BS (modulu at the end of the road)
		true -> BSPid ! {bs_leave,self()}; % Likewise.
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
	    % Radius check:
	    case X+?RV_RANGE >= InX of    % Check if the event is in the range of us and all of our AVs 
		true -> [ Pid ! {av_incoming_event,InX} || Pid <- AvPids ]; % Send crash messages to all of my AVs
		_ -> ok
	    end,
	    % Specific self-affliction check:
	    case InX =< X of % Event affects this RV (is to the right of us) - let the control know we're braking
		true -> ControlPid ! {av_braking, self()};
		_ -> ok
	    end,
	    rvMain(X,Y,BSPid,ControlPid,AvPids);
	    
	{rv_leave,Pid} ->  % AV has left our range. Delete PID from the list while calling ourselves recursively
	    rvMain(X,Y,BSPid,ControlPid,lists:delete(Pid,AvPids));

	{rv_join,Pid} -> % AV has joined our range. Insert a new PID to the list while calling ourselves recursively
	    rvMain(X,Y,BSPid,ControlPid,[Pid|AvPids]);
	
	{rv_new_bs,NewBSPid} -> % Got a new BS assigned to us (we asked earlier, remember?)
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
	exit -> % Exit gracefully
	    OtherBS ! exit, % And tell the other BS to exit too
	    ok;

	{bs_incoming_event,InPid,InX} -> % Incoming event, send to all RVs
	    [ Pid ! {rv_incoming_bs,InX} || Pid <- lists:delete(InPid,RvPids) ], % Send crash messages to all RVs except the one that we got the message from

	    case IsRight of
		true -> OtherBS ! {bs_incoming_event, InPid, InX}; % This will work on the other BS's list of RVs (the PID won't be deleted there)
		_ -> ok
	    end,

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
init_control(NumOfAVs,NumOfRVs,VehicleNodeID,BSNodeID) ->
    % Set up the base stations and init them
    LeftBS = spawn(BSNodeID, ?MODULE, bs_Init, [false,[]]),
    RightBS = spawn(BSNodeID, ?MODULE, bs_Init, [true,[]]),
    LeftBS ! {init, RightBS}, % Tell each BS about the other one
    RightBS ! {init, LeftBS},

    % Create NUM_LANES ets tables with names lane1,lane2,...,laneN
    [ets:new(list_to_atom("lane"++[48+Id]), [ordered_set, named_table, public]) || Id <- lists:seq(1,?NUM_LANES)],

    % Spawn vehicles
    [ createVehicle(false,VehicleNodeID,{}) || _ <- lists:seq(1,NumOfAVs) ], % AVs
    [ createVehicle(true, VehicleNodeID,{LeftBS,RightBS}) || _ <- lists:seq(1,NumOfRVs) ], % RVs
    control().    % Call control with no RV list

%% When called with arity 0, pulls out an RV list from the ETS tables
control() ->
    % The following magic line pulls out all of the RVs from all etses
    NewListOfRVs = [ ets:match(list_to_atom("lane"++[48+Id]),{'_','_','$1',true,'_','_','_'}) || Id <- lists:seq(1,?NUM_LANES) ],
    control(lists:flatten(NewListOfRVs)).

%% Regular control method
control(ListOfRVs) ->
    receive
	exit -> % Exit gracefully
	    [ killVehiclesInLane(Id) || Id <- lists:seq(1,?NUM_LANES) ]; % Send exit messages to all vehicles in all lanes

	{av_gib_rv_plz,CarPid} -> % AV requests a new RV.
	    {A,_B,_C,_D,_E,_F,_G} = findVehicleByPid(CarPid), % Find the X value for the AV that asked
	    NewRV = findRVInRange(A,ListOfRVs), % Get an RV in range
	    CarPid ! {av_new_rv,NewRV}, % Let the car know about the new PID we found :)
	    control(ListOfRVs); % Recurse

	{av_braking,CarPid} -> % A vehicle let us know it has been affected by an event.
	    {A,B,C,D,E,_F,G} = findVehicleByPid(CarPid), % Find out where he is
	    NewF = ?MAX_BRAKING_TIMESLOTS, % Update his braking timeslot
	    ets:insert(list_to_atom("lane"++[48+B]),{A,B,C,D,E,NewF,G}), % Reinsert into ets (update only)
	    control(ListOfRVs); % Recurse

	{display_request,Pid,IsRight} -> % A display module has asked for the current state of his side of the road
	    TheList = lists:flatten([ gibLane(B,IsRight) || B <- lists:seq(1,?NUM_LANES) ]), % Pull out all information
	    Pid ! {display_response,TheList}, % Deliver the response
	    control(ListOfRVs)
    % After a specific timeframe, update all vehicles
    after 750 -> % Relocate vehicles
	    %% We relocate vehicles from right to left, first by X value and then Y value.
	    %% This way, there is no way we will move a vehicle twice by accident.
	    [ relocateVehiclesInLaneByX(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Move X
	    [ relocateVehiclesInLaneByY(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Move Y
	    [ updateALanesYValues(Id) || Id <- lists:seq(1,?NUM_LANES) ], % Update lane Y values (they were destroyed earlier)
	    [ sendUpdatesToLane(list_to_atom("lane"++[48+Id]),ets:first(list_to_atom("lane"++[48+Id]))) || Id <- lists:seq(1,?NUM_LANES) ], % Send messages to cars and let them know about their new locations
    
	    control()  % Will go and pull out the list of RVs again and start over.
    end.

%% Gives half of the lane (entire ETS entry that is... according to IsRight true or false)
gibLane(B,IsRight) ->
    Threshold = ?ROADLEN div 2,
    % Observe the magic of the ETS match specification!!!
    case IsRight of
	true ->  
	    Res = ets:select(list_to_atom("lane"++[48+B]),[{ {'$1','$2','$3','$4','$5','$6','$7'}, [ {'<','$1', Threshold}], ['$$']}]);
	false ->
	    Res = ets:select(list_to_atom("lane"++[48+B]),[{ {'$1','$2','$3','$4','$5','$6','$7'}, [ {'>=','$1', Threshold}], ['$$']}])
    end,
    lists:map(fun(X) -> list_to_tuple(X) end,Res). % Tuple it out because it's listed for some reason :S


%% No more cars to send updates to for this lane
sendUpdatesToLane(_,'$end_of_table') ->
    ok;

%% Sends the vehicles in the lane updates about their new locations and crashed status.
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
    {size,NumOfCarsInLane}=lists:nth(6,ets:info(list_to_atom("lane"++[48+B]))), % Pull out size
    relocateByY(B,NumOfCarsInLane,0).    

%% Will fix/update Y values for this lane
updateALanesYValues(B) ->
    AllKeysInLane = [ Key || {Key,_,_,_,_,_,_} <- ets:tab2list(list_to_atom("lane"++[48+B])) ], % Get all keys for this lane
    [ ets:update_element(list_to_atom("lane"++[48+B]),Key,{2,B}) || Key <- AllKeysInLane ]. % Fix all Ys for this lane

relocateByY(_Lane,_,'$end_of_table') ->
    ok;

relocateByY(_Lane,0,_CurrentCar) ->
    ok;

%% Relocates cars by their Y value for a specific lane. This is for the first entry only
relocateByY(Lane,NumOfCarsInLane,0) ->
    First = ets:first(list_to_atom("lane"++[48+Lane])),
    Next = ets:next(list_to_atom("lane"++[48+Lane]),First),
    {A,B,C,D,E,F,G} = findVehicleByKey(First,Lane),
    case B of
	0 -> relocateByY(Lane,NumOfCarsInLane-1,Next); % Don't move vehicle - it has already moved here
	_ -> % No 0 entry - we might need to relocate
	    NewY = giveNewY(Lane),
	    case NewY of
		Lane -> relocateByY(Lane,NumOfCarsInLane-1,Next); % No need to relocate
		_ -> % We need to relocate. Check for crashes
		    Res = ets:lookup(list_to_atom("lane"++[48+NewY]),A),
		    case length(Res) of 
			0 -> % No crash, move the car over :)
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    ets:insert(list_to_atom("lane"++[48+NewY]),{A,0,C,D,E,F,G}), % 0 in Y value means it has been moved.
			    relocateByY(Lane,NumOfCarsInLane-1,Next);
			_ -> % There is a crash. Keep car in same lane, but mark as crashed
			    ets:insert(list_to_atom("lane"++[48+Lane]),{A,Lane,C,D,E,?MAX_BRAKING_TIMESLOTS,?BRAKING_SPEED}),
			    relocateByY(Lane,NumOfCarsInLane-1,Next)
		    end
	    end
    end;

%% Relocate car by Y value, for all other entries.
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

%% Gives a new Y value for a specific lane number (+- 1 lane)
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

%% Relocates by X - wrapper function
relocateVehiclesInLaneByX(B) -> % Pulls out size and sends to the relocateByX method
    {size,NumOfCarsInLane}=lists:nth(6,ets:info(list_to_atom("lane"++[48+B]))), % Get size of lane
    relocateByX(B,NumOfCarsInLane,NumOfCarsInLane,ets:first(list_to_atom("lane"++[48+B]))). % Start working the lane

relocateByX(_Lane,_,_,'$end_of_table') ->
    ok;

relocateByX(_Lane,_,0,_CurrentCar) ->
    ok;

relocateByX(Lane,NumOfCarsInLane,M,CurrentCar) ->
    Next = ets:next(list_to_atom("lane"++[48+Lane]),CurrentCar), % Get next car (needed for nextb iteration)
    
    {A,_B,C,D,E,F,G} = findVehicleByKey(CurrentCar,Lane), % Get current vehicle
    {ProposedX,ProposedSpeed,ProposedSlot} = giveNewX(A,G,E,F), % Get a new proposed location for it (Y value not affected)
    
    Prev = ets:prev(list_to_atom("lane"++[48+Lane]),A), % This is the car ahead of us, MAYBE.
    % We might be the last on the road, and will need to compare against the first vehicle on the road. We will now check:
    NewPrev = case Prev of
		  '$end_of_table' -> 
		      ets:last(list_to_atom("lane"++[48+Lane])); % We are last. Get first vehicle on the road to compare against.
		  _ -> 
		      Prev % Assign previous to NewPrev - meaning there is a car ahead of us.
	      end,

    case NumOfCarsInLane<2 of
	true -> 
	    % Special case: We are the only car in this lane. Assign new location and exit this method.
	    ets:delete(list_to_atom("lane"++[48+Lane]),A),
	    ets:insert(list_to_atom("lane"++[48+Lane]),{ProposedX,Lane,C,D,E,ProposedSlot,ProposedSpeed});	    
	false -> 
	    % Check for modulu operation
	    case ProposedX > A of
		true -> % Modulu
		    case Prev of
			'$end_of_table' -> % We were last on the road
			    % Check with NewPrev if we crashed or not
			    % Newprev is the car we need to check against
			    case ProposedX =< NewPrev of
				true -> % Crashed
				    NewSpeed = ?BRAKING_SPEED,
				    NewSlot = ?MAX_BRAKING_TIMESLOTS,
				    ets:delete(list_to_atom("lane"++[48+Lane]),A),
				    case doesPosExist(NewPrev+1 ,Lane) of
					true ->
					    NewX = 0;
					false -> 
					    NewX = NewPrev+1						 
				    end,
				    ets:insert(list_to_atom("lane"++[48+Lane]),{NewX,Lane,C,D,E,NewSlot,NewSpeed});
				false -> % Relocate
				    ets:delete(list_to_atom("lane"++[48+Lane]),A),
				    ets:insert(list_to_atom("lane"++[48+Lane]),{ProposedX,Lane,C,D,E,ProposedSlot,ProposedSpeed})
				end;
			_ -> % We weren't first. Crashed fo sho.
			    NewX = NewPrev+1,
			    NewSpeed = ?BRAKING_SPEED,
			    NewSlot = ?MAX_BRAKING_TIMESLOTS,
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    ets:insert(list_to_atom("lane"++[48+Lane]),{NewX,Lane,C,D,E,NewSlot,NewSpeed})
		    end;
		false -> % No Modulu
		    % Check if we crashed
		    case ((ProposedX =< NewPrev) and (A > NewPrev)) of
			true -> % We crashed
			    NewX = NewPrev+1,
			    NewSpeed = ?BRAKING_SPEED,
			    NewSlot = ?MAX_BRAKING_TIMESLOTS,
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    ets:insert(list_to_atom("lane"++[48+Lane]),{NewX,Lane,C,D,E,NewSlot,NewSpeed});
			false -> % No crash
			    ets:delete(list_to_atom("lane"++[48+Lane]),A),
			    ets:insert(list_to_atom("lane"++[48+Lane]),{ProposedX,Lane,C,D,E,ProposedSlot,ProposedSpeed})
		    end
	    end,
	    relocateByX(Lane,NumOfCarsInLane,M-1,Next)
    end.

%% Propose a new X location for us based on our old location and our speed/braking status
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
%% Send an exit command to all vehicles in a specific lane                                                             
killVehiclesInLane(B) ->
    AllPidsInLane = [ Pid || {_,_,Pid,_,_,_,_} <- ets:tab2list(list_to_atom("lane"++[48+B])) ], % Get all PIDs for this lane
    [ AvPid ! exit || AvPid <- AllPidsInLane ]. % Send an exit message

%% Finds a vehicle by it's X location in a specific lane.
findVehicleByKey(Key,Lane) -> % Finds a vehicle by pid
    NewListOfRVs = ets:match(list_to_atom("lane"++[48+Lane]),{Key,'$1','$2','$3','$4','$5','$6'}),
    {A,B,C,D,E,F} = hd(lists:map(fun(Z) -> list_to_tuple(Z) end, NewListOfRVs)),
    {Key,A,B,C,D,E,F}.

%% Finds a vehicle by it's PID in all available lanes
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
    no_non_empty; % No non empty entry found :(((

%% Returns the first non empty entry in a list of lists.
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

%% Creates a vehicle and spawns it using random uniform variables
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

%% Returns a valid X for a specific lane, where there is no car. Used only in the initialisation stage when placing new vehicles.
gibValidX(Y) ->
    X = random:uniform(?ROADLEN), % Pick X randomly
    Res = doesPosExist(X,Y),
    case Res of
	true ->
	    gibValidX(Y);
	false -> X
    end.
    
%% Returns true if a position exists (to make sure we don't run any car over)
doesPosExist(X,Y) ->
    Res = ets:lookup(list_to_atom("lane"++[48+Y]),X),
    case length(Res) of
	0 ->
	    false;
	_ -> 
	    true
    end.

%% Attempts to send to a sepcific PID. If it fails, doesn't crash anything :)
tryToSend(Pid,Content) ->
    case is_pid(Pid) of
	true ->
	    Pid ! Content;
	_ -> ok
    end.

%% Performs a proper modulu with negatives working properly
modulo(X,Y) when X > 0 ->   
   X rem Y;

modulo(X,Y) when X < 0 ->   
    K = (-X div Y)+1,
    PositiveX = X + K*Y,
    PositiveX rem Y;

modulo(0,_Y) -> 
    0.
