-module(display).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-define(SCREEN_X,(1200)). % Fixed X size of the screen
-define(SCREEN_Y,(600)). % Fixed Y size of the screen
-define(INTERLANE_OFFSET,(10)). % Interlane offset in pixels
-define(SCREEN_OFFSET,(30)). % Offset from top/bottom of screen
-define(REFRESH_PERIOD,(1500)). % Time between refresh attempts in milliseconds
-define(ROAD_LEN,6600). % TOTAL road length in pixels
-define(RV_RANGE,500). % Range of RV in pixels

%% Spawns a slave (left monitor) on specified node
spawn_slave(Node) ->
    spawn(Node,?MODULE,start,[]).

%% Spawns a master (right monitor) on specified node. Also requires the slave PID returned from spawn_slave.
spawn_master(ControlPid, Node, NumOfLanes, SlavePid) ->
    spawn(Node,?MODULE,start,[ControlPid,SlavePid,true,NumOfLanes]).

%% Entry point for slave. Waits for an init message from the master unit.
start() ->
    receive
	{init,ControlPid, NumOfLanes, IsRight} -> % Init message from master unit
	    main(ControlPid,IsRight,NumOfLanes,nil)
    end.

%% Entry point for master. Sends an init message to the slave and continues
start(ControlPid,OtherPid,true,NumOfLanes) ->
    OtherPid ! {init,ControlPid, NumOfLanes, false},
    main(ControlPid,true,NumOfLanes,OtherPid).

%% Creates the window and menus etc. Same for both master/slave
main(ControlPid, IsRight, NumOfLanes, OtherPid)	->
    ets:new(cardb,[named_table,set,public]), % Local ETS table for managing the on screen vehicles

    Wx = wx:new(), % Init WX

    %% Make a nice title :)
    IsRightWords = case IsRight of
		       true -> "Right ->";
		       false -> "<- Left"
		   end,
    Title=io_lib:fwrite("Erlang Project: VANET - ~s", [IsRightWords]), 

    Frame = wxFrame:new(Wx, -1, Title, [{size, {?SCREEN_X, ?SCREEN_Y}},{pos,{20,20}}]), % Create a frame on the screen at 20,20
    Panel = wxPanel:new(Frame), % Create a new panel
    wxPanel:connect(Panel, left_down), % Attach mouse click events
    % Onpaint needed for first 
    OnPaint = fun(_Evt, _Obj) ->
		      Brush = wxBrush:new(),
		      Paint = wxPaintDC:new(Panel),
		      wxDC:setBrush(Paint, Brush),
		      wxDC:drawLabel(Paint,"Wait a few moments :)", {(?SCREEN_X) div 3, (?SCREEN_Y) div 3, 200, 200})
	      end,
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]),

    % Create menu bar
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar (Frame, MenuBar),
    wxFrame:getMenuBar (Frame),
    FileMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, FileMn, "&Menu"),
    Start = wxMenuItem:new ([{id, 200}, {text, "&Toggle Ranges"}]),
    wxMenu:append (FileMn, Start),
    Quit = wxMenuItem:new ([{id, 500}, {text, "&Quit"}]),
    wxMenu:append (FileMn, Quit),
    wxFrame:connect (Frame, command_menu_selected),
    wxFrame:show(Frame),
    
    % Precalculate the lane centers
    LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
    NewMiddles = [ getMiddle(?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
    loop(true,OtherPid, Frame, Panel, ControlPid, IsRight, NumOfLanes, NewMiddles). % Enter loop

%% Handles all the menu bar commands and drawing.
loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles) -> 
    receive 
	% Exit messages from the other screen
	exit ->
	    exit(1);
	% Capture Mouse event
	#wx{event = #wxMouse{type = left_down, x = X, y = Y}} ->
	    Res = checkForCarInPos(X,Y), % Check if a car exists
	    case length(Res)>0 of
		true ->	
		    % If a car exists, pull out first one that exists there
		    {A,B,C,D,E,F,G} = hd(Res),
		    Message = io_lib:fwrite("{X,Y}=~w,~w~n-------------~nCar PID: ~w~nCrazy: ~w~nIs it an RV? ~w~nBraking timeslots left: ~w~nSpeed: ~w", [A,B,C,E,D,F,G]), % Format a message
		    Modal = wxMessageDialog:new (Panel, Message), % Create modal for the message
		    wxMessageDialog:showModal(Modal), % Show modal
		    loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
		false ->    
		    % No car found. Ignore
		    loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
	    end;
	% Draw the range around the RV by Xpos given in pixels
	{draw_range,Xpos} -> 
	    % Check whether the ShowRanges flag is on or off
	    case ShowRanges of
		true -> % Draw range
		    Paint = wxWindowDC:new(Panel),
		    Brush = wxBrush:new(),
		    Pen = wxPen:new(),
		    wxBrush:setStyle(Brush,?wxTRANSPARENT),
		    wxPen:setStyle(Pen,?wxSOLID),
		    wxBrush:setColour(Brush, ?wxBLUE),
		    wxPen:setColour(Pen, ?wxBLUE),
		    wxWindowDC:setBrush(Paint,Brush),
		    wxWindowDC:setPen(Paint,Pen),
		    Range_In_Px=erlang:trunc(?RV_RANGE * (2*?SCREEN_X / ?ROAD_LEN)),
		    wxWindowDC:drawRectangle(Paint,{Xpos-Range_In_Px,?SCREEN_OFFSET-10},{2*Range_In_Px,?SCREEN_Y-80}),
		    wxBrush:destroy(Brush),
		    wxPen:destroy(Pen),
		    wxWindowDC:destroy(Paint);
		false -> ok % No range
	    end,
	    loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);	
	% Handle response from the control unit (as per our request in the "after" clause)
	{display_response,TheList} ->
	    ets:delete_all_objects(cardb), % Clear out old ETS table
	    parseControlListAndDrawCars(TheList,IsRight,Middles,Panel), % Parse message
	    loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
	% Handle message codes
	{_,X,_,_,_}-> % X is message code.
	    % Capture menu messages
	    case X of
		500 -> % Quit
		    case is_pid(OtherPid) of
			true ->
			    % We are the master, send to slave and control
			    OtherPid ! exit, % Send quit to other monitor
			    ControlPid ! exit, % Send quit to control unit
			    exit(1); % Quit ourselves
			false ->
			    % We are the slave. Just quit.
			    exit(1) 
		    end;
		300 -> % Redraw command
		    wxPanel:destroy(Panel), % Destroy old panel
		    NewPanel = wxPanel:new(Frame), % Create new panel
		    % Onpaint event draws the lanes
		    OnPaint1 = fun(_Evt, _Obj) ->
				       Paint = wxPaintDC:new(NewPanel),
				       Brush = wxBrush:new(),
				       wxBrush:setColour(Brush, ?wxBLACK),
				       wxDC:setBrush(Paint,Brush),
				       LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
				       [ draw_rectangle(NewPanel,Paint,Brush,0,?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
				       wxBrush:destroy(Brush),
				       wxPaintDC:destroy(Paint)
			       end,
		    wxFrame:connect(NewPanel, paint, [{callback, OnPaint1}]), % Connect onpaint to panel
		    wxPanel:connect(NewPanel, left_down), % Reconnect mouse event
		    Image = wxImage:new("bcn.bmp", []), % Draw beacon
		    Bitmap = wxBitmap:new(Image),
		    _StaticBitmap = wxStaticBitmap:new(NewPanel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]),
		    wxFrame:update(Frame), % Update
		    wxFrame:refresh(Frame),% Refresh.
		    % Since the previous update/refresh didn't work - we're going to attempt some voodoo
		    % Resize the screen by 1 pixel and resize it back. This forces a refresh in Windows.
		    wxWindow:setSize(Frame,{?SCREEN_X+1,?SCREEN_Y+1}),
		    wxWindow:setSize(Frame,{?SCREEN_X,?SCREEN_Y}),
		    loop(ShowRanges,OtherPid, Frame,NewPanel,ControlPid,IsRight,NumOfLanes,Middles);
		200 -> 
		    % Toggle Show Ranges
		    case ShowRanges of
			true ->
			    loop(false,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
			false ->
			    loop(true,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
		    end;
		_ -> 
		    loop(ShowRanges,OtherPid, Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
	    end
    % After the refresh period, we want to query the control about the new car situation...
    after ?REFRESH_PERIOD ->
	    ControlPid ! {display_request,self(),IsRight}, % Ask the control for some fresh information
	    self() ! {0,300,0,0,0}, % Send a redraw command to ourselves
	    loop(ShowRanges,OtherPid,Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
    end.

%% This method parses the list given from the control and puts cars in the correct place. Also updates the ETS.
parseControlListAndDrawCars([],_,_,_) ->
    ok; % Until empty....
parseControlListAndDrawCars([H|T],IsRight,Middles,Panel) ->
    {X,Y,Pid,IsRV,IsCrazy,Timeslot,Speed} = H, % Pull out first entry
    ets:insert(cardb,{meterToPixel(X,IsRight),lists:nth(Y,Middles),Pid,IsRV,IsCrazy,Timeslot,Speed}), % Insert into ETS
    % Find out what type the entry is
    Type = case IsRV of
	true -> rv;
	false ->
	    case Timeslot>0 of
		true -> 
		    av_braking;
		false ->
		    checkRange(Pid) % If it isn't braking, check if it's in range or not.
	    end
    end,
    putCar(Panel,meterToPixel(X,IsRight),lists:nth(Y,Middles),Type), % Put car in a specific X,Y location
    parseControlListAndDrawCars(T,IsRight,Middles,Panel). % Recurse over the entire list

%% This is used in the mouse handler. It checks if a car is in a specific position +- some margin.
%% Uses ETS match specification voodoo
checkForCarInPos(X,Y) ->
    Res = ets:select(cardb,[{{'$1','$2','$3','$4','$5','$6','$7'},
			      [{'andalso',{'=<','$1',X+35},
				{'andalso',{'>=','$1',X-35},
				 {'andalso',{'=<','$2',Y+20},
				  {'>=','$2',Y-20}}}}],
			     [{{'$1','$2','$3','$4','$5','$6','$7'}}]}]),
    Res.
    
%% Converts meters into pixels for master/slave display units with some math! (Dependant on road length and screen size)
meterToPixel(Meters,IsRight) ->
    AbsoluteX = ?ROAD_LEN - Meters,
    PreResult = erlang:trunc(AbsoluteX * (2*?SCREEN_X / ?ROAD_LEN)),
	
    case IsRight of
	true ->
	    PreResult - ?SCREEN_X;
	false -> 
	    PreResult
    end.

%% Queries an AV about it's RV status (connected/disconnected)
checkRange(Pid) ->
    Pid ! {av_are_you_in_range, self()},
    receive
	{av_range_response,_,Response} ->
	    case Response of
		true -> av_reg;
		false -> av_outofrange
	    end
    end.

%% Puts a car on the screen as if by MAGIC
putCar(Panel,X,Y,Type) -> 
    % Pick correct BMP
    File = case Type of
	       av_reg -> "av_inrange_100_49.bmp";
	       av_braking -> "av_braking_100_49.bmp";
	       av_outofrange -> "av_outofrange_100_49.bmp";
	       rv -> "rv_100_49.bmp";
	       _ -> "av_inrange_100_49.bmp"
	   end,

    Image = wxImage:new(File, []),
    Bitmap = wxBitmap:new(Image),
    _StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{40,20}},{pos,{X-20,Y-10}}]), % Puts in X-20,Y-10 (to center it)
    % If the car is of type RV, it causes a draw_range for afterwards
    case Type of
	rv -> 
	    self() ! {draw_range,X};
	_ -> ok
    end.

%% Draws a rectangle
draw_rectangle(_Panel,Paint,_Brush,X,Y,LaneWidth)        ->
    wxDC:drawRectangle(Paint,{X,Y},{?SCREEN_X,LaneWidth}).  % Draw rectangle

%% Returns the middle of a lane
getMiddle(Y,LaneWidth) ->
    Y+(LaneWidth div 2).
