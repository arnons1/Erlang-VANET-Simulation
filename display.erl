-module(display).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-define(SCREEN_X,(800)).
-define(SCREEN_Y,(600)).
-define(INTERLANE_OFFSET,(10)).
-define(SCREEN_OFFSET,(30)).
-define(REFRESH_PERIOD,(1000)). % in MS
-define(ROAD_LEN,1600).

%% Creeates the window and menus etc.
start(ControlPid, IsRight, NumOfLanes)	->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Vehicles! HOLY CRAP", [{size, {?SCREEN_X, ?SCREEN_Y}},{pos,{20,20}}]),
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
	{_,X,_,_,_}-> % X is message code. Z is used for the mouse clicks
	    io:fwrite("~p ~n", [X]),
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
		    Image = wxImage:new("bcn.bmp", []),
		    Bitmap = wxBitmap:new(Image),
		    StaticBitmap = wxStaticBitmap:new(NewPanel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]),
		    wxFrame:update(Frame),
		    wxFrame:refresh(Frame),
		    wxWindow:setSize(Frame,{?SCREEN_X+1,?SCREEN_Y+1}),
		    wxWindow:setSize(Frame,{?SCREEN_X,?SCREEN_Y}),
		    loop(Frame,NewPanel,ControlPid,IsRight,NumOfLanes,NewMiddles);
		%% Draw a circle
		350 ->
		    wxPanel:destroy(Panel),
		    NewPanel = wxPanel:new(Frame),
		    wxFrame:update(Frame),
		    wxFrame:refresh(Frame),
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
		    wxFrame:connect(NewPanel, paint, [{callback, OnPaint1}]),
		    Image = wxImage:new("bcn.bmp", []),
		    Bitmap = wxBitmap:new(Image),
		    StaticBitmap = wxStaticBitmap:new(NewPanel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]),
		    LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
		    NewMiddles = [ getMiddle(?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
		    [ putCar(NewPanel,random:uniform(?SCREEN_X),random:uniform(?SCREEN_Y),rv) || _ <- lists:seq(1,20) ],
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
	{click,_Mx,_My} -> % Only for mouse click events
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles);
	{display_response,TheList} -> % Response from the control unit
    	    io:format("TheList:~w~n",[TheList]),
	    parseControlListAndDrawCars(TheList,IsRight,Middles,Panel),
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
    after ?REFRESH_PERIOD ->
	    self() ! {0,300,0,0,0},
	    ControlPid ! {display_request,self(),IsRight}, % Ask the control for some fresh information
	    io:format("Sent req.~n"),
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes,Middles)
    end.

parseControlListAndDrawCars([],_,_,_) ->
    io:format("ParseControl finished~n~n"),
    ok;
parseControlListAndDrawCars([H|T],IsRight,Middles,Panel) ->
    io:format("ParseControl got ~w~n",[H]),
    {X,Y,Pid,IsRV,_IsCrazy,Timeslot,_Speed} = H,
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
    io:format("Putting a ~w at ~w ~w~n",[Type,meterToPixel(X,IsRight),lists:nth(Y,Middles)]),
    putCar(Panel,meterToPixel(X,IsRight),lists:nth(Y,Middles),Type),
    parseControlListAndDrawCars(T,IsRight,Middles,Panel).

meterToPixel(Meters,IsRight) -> %% Converts meters to pixels depending on the right/left screen and road length
    AbsoluteX = ?ROAD_LEN - Meters,
    PreResult = AbsoluteX * (2*?SCREEN_X div ?ROAD_LEN),
    case IsRight of
	true -> PreResult - ?SCREEN_X;
	false -> 
	    PreResult
    end.

checkRange(Pid) ->
    io:format("Gonna ask for range~n"),
    Pid ! {av_are_you_in_range, self()},
    receive
	{av_range_response,_,Response} ->
	    io:format("Got range ~w~n",[Response]),
	    case Response of
		true -> av_reg;
		false -> av_outofrange
	    end
    end.

putCar(Panel,X,Y,Type) -> % Puts a car.
    File = case Type of
	       av_reg -> "av_inrange_100_49.bmp";
	       av_braking -> "av_braking_100_49.bmp";
	       av_outofrange -> "av_outofrange_100_49.bmp";
	       rv -> "rv_100_49.bmp";
	       _ -> "av_inrange_100_49.bmp"
	   end,
    Image = wxImage:new(File, []),
    Bitmap = wxBitmap:new(Image),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{100,49}},{pos,{X,Y}}]).

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
draw_rectangle(Panel,Paint,Brush,X,Y,LaneWidth)        ->
    wxDC:drawRectangle(Paint,{X,Y},{?SCREEN_X,LaneWidth}).  % Draw square.


getMiddle(Y,LaneWidth) ->
    Y+(LaneWidth div 2).
