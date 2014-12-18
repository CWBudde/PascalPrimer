// clear screen
Clear;

procedure DrawRegularConvexPolygon(Edges: Integer; Radius: Float);
begin
  // center cursor
  Home;

  // turn cursor to look to the north 
  TurnLeft(90);

  // go to start position (top)
  Go(Radius);
  
  CursorColor := ComposeColor(Sqrt(1 / Sqr(Edges)), 0, 1);

  // now draw regular convex polygon 
  TurnRight(90 + 180 / Edges);
  for var i := 0 to Edges - 1 do
  begin                
    Draw(2 * Radius * sin(Pi / Edges));
    TurnRight(360 / Edges);
  end;
end;  

// draw lower order polygons
for var n := 3 to 10 do
  DrawRegularConvexPolygon(n, 150);

// draw circle
DrawRegularConvexPolygon(100, 150);