-module(display).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-define(SCREEN_X,(820)). % For testing git
-define(SCREEN_Y,(700)).
-define(INTERLANE_OFFSET,(10)).
-define(SCREEN_OFFSET,(30)).
  
%% Creates the window and menus etc.
start(ControlPid, IsRight, NumOfLanes)	->
    Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "Vehicles! HOLY CRAP", [{size, {?SCREEN_X, ?SCREEN_Y}},{pos,{20,20}}]),
    Panel = wxPanel:new(Frame),
    wxPanel:connect(Panel, left_down),
    MainSizer  = wxBoxSizer:new(?wxVERTICAL),
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
	About = wxMenuItem:new ([{id, 400}, {text, "&About"}]),
	wxMenu:append (FileMn, About),
	Quit = wxMenuItem:new ([{id, 500}, {text, "&Quit"}]),
	wxMenu:append (FileMn, Quit),

	wxSizer:addSpacer(MainSizer, ?SCREEN_X+5),  

	wxPanel:setSizer(Panel, MainSizer),
				Image = wxImage:new("bcn.bmp", []),
				Bitmap = wxBitmap:new(Image),
				StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]),

	wxFrame:connect (Frame, command_menu_selected),
	wxFrame:show(Frame),

	loop(Frame, Panel, ControlPid, IsRight, NumOfLanes).

%% Handles all the menu bar commands
loop(Frame,Panel,ControlPid,IsRight,NumOfLanes) -> 
    receive 
	{refresh}->
		  		Paint = wxPaintDC:new(Panel),
		        Brush = wxBrush:new(),
				LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
				Middles = [ draw_rectangle(Panel,Paint,Brush,0,?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
				wxBrush:destroy(Brush),
		        wxPaintDC:destroy(Paint),

		loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
	{_,X,_,_,_}-> % X is message code. Z is used for the mouse clicks
	    io:fwrite("~p ~n", [X]),
	    case X of
		%% About
		400 -> D = wxMessageDialog:new (Panel, "Created by Arnon Shimoni"),
		       wxMessageDialog:showModal(D),
		       loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
		%% Draw a lane
		300 -> 
			%OnPaint1 = fun(_Evt, _Obj) ->
  		        Paint = wxPaintDC:new(Panel),
		        Brush = wxBrush:new(),
				wxBrush:setColour(Brush, ?wxBLACK),
				wxDC:setBrush(Paint,Brush),
				LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
				Middles = [ draw_rectangle(Panel,Paint,Brush,0,?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
				wxBrush:destroy(Brush),
		        wxPaintDC:destroy(Paint),
		    %end,
			%wxFrame:connect(Panel, paint, [{callback, OnPaint1}]),
			%wxFrame:refresh(Frame),
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
		%% Draw a circle
		200 -> 
			Paint = wxPaintDC:new(Panel),
		        Brush = wxBrush:new(),
				wxBrush:setColour(Brush, ?wxBLUE),
				wxDC:setBrush(Paint,Brush),
				draw_rectangle(Panel,Paint,Brush,0,random:uniform(700),20),
				wxBrush:destroy(Brush),
		        wxPaintDC:destroy(Paint),
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
		%% Create N circles or squares
		300 -> ok,
		       loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
		%% Capture all other events:
		_ -> 
		    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes)
	    end;
	{click,Mx,My} -> % Only for mouse click events
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes);
	Msg -> % Anything else
	    io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
	    loop(Frame,Panel,ControlPid,IsRight,NumOfLanes)
    end.

%draw_lane(Panel,NumOfLanes)       ->
%	LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
	%Middles = [ draw_rectangle(Panel,0,?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
    %Image = wxImage:new("bcn.bmp", []),
    %Bitmap = wxBitmap:new(Image),
    %StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}]).

	
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
	wxDC:drawRectangle(Paint,{X,Y},{?SCREEN_X,LaneWidth}),  % Draw square.
	Y-(LaneWidth div 2).
