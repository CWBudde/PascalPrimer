unit ExampleUnit;

uses
  TurtleBasic;

procedure North;
begin
  CursorAngle := 90;
end;

procedure East;
begin
  CursorAngle := 0;
end;

procedure West;
begin
  CursorAngle := 180;
end;

procedure South;
begin
  CursorAngle := -90;
end;