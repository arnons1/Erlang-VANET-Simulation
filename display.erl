-module(display).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-define(SCREEN_X,(800)).
-define(SCREEN_Y,(600)).
-define(INTERLANE_OFFSET,(10)).
-define(SCREEN_OFFSET,(30)).
-define(REFRESH_PERIOD,(1500)). % in MS
-define(ROAD_LEN,6600).
-define(RV_RANGE,500).

spawn_slave(Node) ->
    spawn(Node,?MODULE,start,[]).

spawn_master(ControlPid, Node, NumOfLanes, SlavePid) ->
    spawn(Node,?MODULE,start,[ControlPid,SlavePid,true,NumOfLanes]).

start() ->
    receive
	{init,ControlPid, NumOfLanes, IsRight} ->
	    main(ControlPid,IsRight,NumOfLanes)
    end.

start(ControlPid,OtherPid,true,NumOfLanes) ->
    OtherPid ! {init,ControlPid, NumOfLanes, false},
    main(ControlPid,true,NumOfLanes).

%% Creates the window and menus etc.
main(ControlPid, IsRight, NumOfLanes)	->
    ets:new(cardb,[named_table,set,public]),
    Wx = wx:new(),
	IsRightWords = case IsRight of
						true -> "Right ->";
						false -> "<- Left"
	end,
	Title=io_lib:fwrite("Erlang Project: VANET - ~s", [IsRightWords]),
    Frame = wxFrame:new(Wx, -1, Title, [{size, {?SCREEN_X, ?SCREEN_Y}},{pos,{20,20}}]),
    Panel = wxPanel:new(Frame),
    wxPanel:connect(Panel, left_down),
    OnPaint = fun(_Evt, _Obj) ->
		      Brush = wxBrush:new(),
		      Paint = wxPaintDC:new(Panel),
		      wxDC:setBrush(Paint, Brush),
		      wxDC:drawLabel(Paint,"Press Menu -> Start to Begin .", {(?SCREEN_X) div 3, (?SCREEN_Y) div 3, 200, 200})
	      end,
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar (Frame, MenuBar),
    wxFrame:getMenuBar (Frame),
    FileMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, FileMn, "&Menu"),
    Start = wxMenuItem:new ([{id, 200}, {text, "&Start"}]),
    wxMenu:append (FileMn, Start),
    STP = wxMenuItem:new ([{id, 300}, {text, "&Draw a lane"}]),
    wxMenu:append (FileMn, STP),
    Less = wxMenuItem:new ([{id, 350}, {text, "&Draw one lane less"}]),
    wxMenu:append (FileMn, Less),
    About = wxMenuItem:new ([{id, 400}, {text, "&About"}]),
    wxMenu:append (FileMn, About),
    Quit = wxMenuItem:new ([{id, 500}, {text, "&Quit"}]),
    wxMenu:append (FileMn, Quit),
    
    wxFrame:connect (Frame, command_menu_selected),
    wxFrame:show(Frame),
    LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
    NewMiddles = [ getMiddle(?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
    loop(Frame, Panel, ControlPid, IsRight, NumOfLanes, NewMiddles).

%% Handles all the menu bar commands
loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles) -> 
    receive 
	#wx{event = #wxMouse{type = left_down, x = X, y = Y}} ->
	    Res = checkForCarInPos(X,Y),
	    case length(Res)>0 of
		true ->	
		    {A,B,C,D,E,F,G} = hd(Res),
		    Message = io_lib:fwrite("{X,Y}=~w,~w~n~n-------------~nCar PID: ~w~nCrazy: ~w~nIs it an RV? ~w~nBraking timeslots left: ~w~nSpeed: ~w", [A,B,C,E,D,F,G]),
		    Modal = wxMessageDialog:new (Panel, Message),
		    wxMessageDialog:showModal(Modal),
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
		false ->    
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
	    end;
	{_,X,_,_,_}-> % X is message code. Z is used for the mouse clicks
	    case X of
		500 ->
		    exit(1);
		%% About
		400 -> D = wxMessageDialog:new (Panel, "boop"),
		       wxMessageDialog:showModal(D),
		       loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
		%% Draw a lane
		300 -> 
		    wxPanel:destroy(Panel),
		    NewPanel = wxPanel:new(Frame),
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
		    LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
		    NewMiddles = [ getMiddle(?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
		    wxFrame:connect(NewPanel, paint, [{callback, OnPaint1}]),
		    wxPanel:connect(NewPanel, left_down),
		    Image = wxImage:new("bcn.bmp", []),
		    Bitmap = wxBitmap:new(Image),
		    StaticBitmap = wxStaticBitmap:new(NewPanel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]),
		    wxFrame:update(Frame),
		    wxFrame:refresh(Frame),
		    wxWindow:setSize(Frame,{?SCREEN_X+1,?SCREEN_Y+1}),
		    wxWindow:setSize(Frame,{?SCREEN_X,?SCREEN_Y}),
		    loop(Frame,NewPanel,ControlPid,IsRight,NumOfLanes,NewMiddles);
		200 -> 
		    Paint = wxPaintDC:new(Panel),
		    Brush = wxBrush:new(),
		    wxBrush:setColour(Brush, ?wxBLUE),
		    wxDC:setBrush(Paint,Brush),
		    draw_rectangle(Panel,Paint,Brush,0,random:uniform(700),20),
		    wxBrush:destroy(Brush),
		    wxPaintDC:destroy(Paint),
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
		_ -> 
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
	    end;
	{draw_range,Xpos} -> 
	    OnPaint2 = fun(_Evt, _Obj) ->
			       Paint = wxPaintDC:new(Panel),
			       Pen = wxPen:new(),
			       wxPen:setStyle(Pen,?wxSOLID),
			       wxPen:setColour(Pen, ?wxRED),
			       wxDC:setPen(Paint,Pen),
			       % Draw rectangle around the RV
			       wxDC:drawRectangle(Paint,{Xpos-(meterToPixel(?RV_RANGE,IsRight) div 2),?SCREEN_OFFSET},{meterToPixel(?RV_RANGE,IsRight),?SCREEN_Y-60}),

			       wxPen:destroy(Pen),
			       wxPaintDC:destroy(Paint)
		       end,
	    wxFrame:connect(Panel, paint, [{callback, OnPaint2}]),
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
	
	{click,_Mx,_My} -> % Only for mouse click events
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
	{display_response,TheList} -> % Response from the control unit
		ets:delete_all_objects(cardb),
	    parseControlListAndDrawCars(TheList,IsRight,Middles,Panel),
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
    after ?REFRESH_PERIOD ->
	    self() ! {0,300,0,0,0},
	    ControlPid ! {display_request,self(),IsRight}, % Ask the control for some fresh information
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
    end.

parseControlListAndDrawCars([],_,_,_) ->
    ok;
parseControlListAndDrawCars([H|T],IsRight,Middles,Panel) ->
    {X,Y,Pid,IsRV,IsCrazy,Timeslot,Speed} = H,
    ets:insert(cardb,{meterToPixel(X,IsRight),lists:nth(Y,Middles),Pid,IsRV,IsCrazy,Timeslot,Speed}),
    Type = case IsRV of
	true -> rv;
	false ->
	    case Timeslot>0 of
		true -> 
		    av_braking;
		false ->
		    checkRange(Pid)
	    end
    end,
    putCar(Panel,meterToPixel(X,IsRight),lists:nth(Y,Middles),Type,IsRight),
    parseControlListAndDrawCars(T,IsRight,Middles,Panel).

checkForCarInPos(X,Y) ->
    Res = ets:select(cardb,[{{'$1','$2','$3','$4','$5','$6','$7'},
			      [{'andalso',{'=<','$1',X+35},
				{'andalso',{'>=','$1',X-35},
				 {'andalso',{'=<','$2',Y+20},
				  {'>=','$2',Y-20}}}}],
			     [{{'$1','$2','$3','$4','$5','$6','$7'}}]}]),
    Res.
    

meterToPixel(Meters,IsRight) -> %% Converts meters to pixels depending on the right/left screen and road length
    %io:format("Transfer ~wm into p: ",[Meters]),
	AbsoluteX = ?ROAD_LEN - Meters,
    PreResult = erlang:trunc(AbsoluteX * (2*?SCREEN_X / ?ROAD_LEN)),
	
    case IsRight of
	true -> %io:format("~w~n~n",[PreResult-?SCREEN_X]),
		PreResult - ?SCREEN_X;
	false -> 
	%	io:format("~w~n~n",[PreResult]),
	    PreResult
    end.

checkRange(Pid) ->
    Pid ! {av_are_you_in_range, self()},
    receive
	{av_range_response,_,Response} ->
	    case Response of
		true -> av_reg;
		false -> av_outofrange
	    end
    end.

putCar(Panel,X,Y,Type,IsRight) -> % Puts a car.
    File = case Type of
	       av_reg -> "av_inrange_100_49.bmp";
	       av_braking -> "av_braking_100_49.bmp";
	       av_outofrange -> "av_outofrange_100_49.bmp";
	       rv -> "rv_100_49.bmp";
	       _ -> "av_inrange_100_49.bmp"
	   end,
    Image = wxImage:new(File, []),
    Bitmap = wxBitmap:new(Image),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{40,20}},{pos,{X,Y-10}}]),
    case Type of
	rv -> self() ! {draw_range,X},
	    draw_range(Panel,X,IsRight),
	    draw_circle(Panel,X,30,30);
	_ -> ok
    end.

%% Draw a line	
draw_line(Panel,Dot1,Dot2)       ->
    Paint = wxPaintDC:new(Panel),
    Brush = wxBrush:new(),
    wxBrush:setColour(Brush, ?wxRED),
    wxDC:setBrush(Paint,Brush),
    wxDC:drawLine(Paint,Dot1,Dot2), %draw line between two dots
    wxBrush:destroy(Brush),
    wxPaintDC:destroy(Paint).

%% Draw a circle and return the {X,Y,Radius} of the circle for storage later	
draw_circle(Panel,X,Y,Radius)    -> 
    Paint = wxPaintDC:new(Panel),
    Brush = wxBrush:new(),
    wxBrush:setColour(Brush, ?wxGREEN),
    wxDC:setBrush(Paint,Brush),
    wxDC:drawCircle(Paint,{X,Y},Radius),  % Craw circle center at {X,Y}
    wxBrush:destroy(Brush),
    wxPaintDC:destroy(Paint),
    {X,Y,Radius}.

%% Draw a square
draw_rectangle(_Panel,Paint,_Brush,X,Y,LaneWidth)        ->
    wxDC:drawRectangle(Paint,{X,Y},{?SCREEN_X,LaneWidth}).  % Draw square.

draw_range(Panel,Xpos,IsRight) ->
    Paint = wxPaintDC:new(Panel),
    Pen = wxPen:new(),
    wxPen:setStyle(Pen,?wxSOLID),
    wxPen:setColour(Pen, ?wxRED),
    wxDC:setPen(Paint,Pen),
    wxDC:drawRectangle(Paint,{Xpos-(meterToPixel(?RV_RANGE,IsRight) div 2),?SCREEN_OFFSET-10},{meterToPixel(?RV_RANGE,IsRight),?SCREEN_Y-80}),  % Draw rectangle around the RV
    wxPen:destroy(Pen),
    wxPaintDC:destroy(Paint).

getMiddle(Y,LaneWidth) ->
    Y+(LaneWidth div 2).
