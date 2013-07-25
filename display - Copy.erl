-module(display).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-define(SCREEN_X,(800)).
-define(SCREEN_Y,(600)).
-define(INTERLANE_OFFSET,(10)).
-define(SCREEN_OFFSET,(30)).
  
%% Creeates the window and menus etc.
start(ControlPid, IsRight, NumOfLanes)                       ->
    Wx = wx:new(), % New WX
    Frame = wxFrame:new(Wx, -1, "Vehicles! HOLY CRAP", [{size, {?SCREEN_X, ?SCREEN_Y}}]), % New frame
    Panel = wxPanel:new(Frame), % Create a panel inside
    OnPaint = fun(_Evt, _Obj) ->
		      Brush = wxBrush:new(),
		      Paint = wxPaintDC:new(Panel),
		      wxDC:setBrush(Paint, Brush),
		      wxDC:drawLabel(Paint,"Press File-> Start to Begin.",{(?SCREEN_X) div 3,(?SCREEN_Y) div 3,200,200})
	      end,

    Mouse = fun(Evt,_) -> % Event handler for mouse
		    {_A,_B,_C,_D,E}=Evt,
		    {wxMouse,left_up,Mx,My,false,false,false,false,false,false,false,0,0,0}=E, % Pulls out X and Y coordinates
		    loopP!{click,Mx,My} % And lets the main loop know about it
	    end,
    
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]), % Attach OnPaint to the panel 
    wxFrame:connect(Panel, left_up, [{callback, Mouse}]), % Attach the mouse down event to the panel
    
    %% Create a menu bar with items
    MenuBar = wxMenuBar:new(),
    wxFrame:setMenuBar (Frame, MenuBar),
    wxFrame:getMenuBar (Frame),
    FileMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, FileMn, "&File"),
    Start=wxMenuItem:new ([{id,300},{text, "&Start"}]),
    wxMenu:append (FileMn, Start),
    LanesD=wxMenuItem:new ([{id,600},{text, "&Draw Lanes"}]),
    wxMenu:append (FileMn, LanesD),
    Circ=wxMenuItem:new ([{id,700},{text, "&Draw Circle"}]),
    wxMenu:append (FileMn, Circ),
    Quit = wxMenuItem:new ([{id,400},{text, "&Quit"}]),
    wxMenu:append (FileMn, Quit),
    HelpMn = wxMenu:new(),
    wxMenuBar:append (MenuBar, HelpMn, "&Help"),
    About = wxMenuItem:new ([{id,500},{text,"About"}]),
    wxMenu:append (HelpMn, About),

    wxFrame:connect (Frame, command_menu_selected),
    wxFrame:show(Frame),

    %% Loop 
    loop1(Frame,Panel,NumOfLanes,[]). % Register the loop first though
       
loop1(Frame,Panel,NumOfLanes,[])     ->
	register(loopP,self()), % Register for mouse-down events
	loop(Frame,Panel,NumOfLanes,[]). % And then loop

%% Handles all the menu bar commands
loop(Frame,Panel,NumOfLanes,Circles) -> 
    receive 
	{_,X,_,_,_}-> % X is message code. Z is used for the mouse clicks
	    io:fwrite("~p ~n", [X]),
	    case X of
		%% About
		500 -> D = wxMessageDialog:new (Panel, "Created by Arnon Shimoni"),
		       wxMessageDialog:showModal(D),
		       loop(Frame,Panel,NumOfLanes,Circles);
		400 -> exit(ok); % Exit semi-gracefully
		%% Draw a line
		600 -> 
		    draw_lane(Panel,NumOfLanes),
		    loop(Frame,Panel,NumOfLanes,Circles);
		%% Draw a circle
		700 -> 
		    ok,
		    loop(Frame,Panel,NumOfLanes,[]);
		%% Create N circles or squares
		300 -> ok,
		       loop(Frame,Panel,NumOfLanes,[]);
		%% Capture all other events:
		_ -> 
		    loop(Frame,Panel,NumOfLanes,Circles)
	    end;
	{click,Mx,My} -> % Only for mouse click events
	    findCircles(Mx,My,Circles,Panel), % Find out if it's in a circle
	    loop(Frame,Panel,NumOfLanes,Circles);
	Msg -> % Anything else
	    io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
	    loop(Frame,Panel,NumOfLanes,Circles)
    end.

draw_lane(Panel,NumOfLanes)       ->
    LaneWidth = ((?SCREEN_Y-60 - ?SCREEN_OFFSET) div (NumOfLanes))-?INTERLANE_OFFSET, % Gives effective lane size
	Middles = [ draw_rectangle(Panel,0,?SCREEN_OFFSET + (I-1)*LaneWidth + (I-1)*?INTERLANE_OFFSET, LaneWidth) || I <- lists:seq(1,NumOfLanes) ],
    Image = wxImage:new("bcn.bmp", []),
    Bitmap = wxBitmap:new(Image),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap,[{size,{28,28}},{pos,{(?SCREEN_X) div 2,0}}])
	.


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
draw_rectangle(Panel,X,Y,LaneWidth)        ->
    Paint = wxPaintDC:new(Panel),
    Brush = wxBrush:new(),
    wxBrush:setColour(Brush, ?wxBLACK),
    wxDC:setBrush(Paint,Brush),
	wxDC:drawRectangle(Paint,{X,Y},{?SCREEN_X,LaneWidth}),  % Draw square.
    wxBrush:destroy(Brush),
    wxPaintDC:destroy(Paint),
	Y-(LaneWidth div 2).

%% No click in a circle, return OK
findCircles(_,_,[],_)            ->
    ok;

%% Hunt for X,Y in a specific distance - using euclidean distance
findCircles(X,Y,[H|T],Panel)     ->
    {Xc,Yc,Rc}=H,
    Dist = math:sqrt(math:pow((X-Xc),2)+math:pow((Y-Yc),2)), % Calculate distance from first element
    if 
	Dist=<Rc ->
	    D = wxMessageDialog:new (Panel, "Hello"), % Is in the distance - show a message
	    wxMessageDialog:showModal(D);
	true ->
	    findCircles(X,Y,T,Panel) % Not in the distance, keep looking
    end.
