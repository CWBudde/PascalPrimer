Clear(clWhite);
Home;

for var j := 0 to 400 do
begin
  CursorColor := ComposeColorHSL(j / 200, 1, 0.5);  
  Draw(1 + Power(j, 0.9) + 0.1 * j);
  TurnLeft(61);
  Delay(10);
end;  
