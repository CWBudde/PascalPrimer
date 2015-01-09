Home;
Clear;
CursorColor := clBlack;

procedure DrawFractalLine(Level: Integer; Size: Float);
begin
  if Level = 1 then
    Draw(Size)
  else  
  begin
    DrawFractalLine(Level - 1, Size / 3);
    TurnLeft(60);
    DrawFractalLine(Level - 1, Size / 3);
    TurnRight(120); 
    DrawFractalLine(Level - 1, Size / 3);
    TurnLeft(60);
    DrawFractalLine(Level - 1, Size / 3);
  end;
end;

procedure DrawKochSnowflake(Level: Integer = 5; Size: Float = 330);
begin
  for var i := 0 to 2 do
  begin
    DrawFractalLine(Level, Size);
    TurnRight(120);
  end;
end;    

MoveTo(10, 300);
CursorAngle := 60;
DrawKochSnowflake;