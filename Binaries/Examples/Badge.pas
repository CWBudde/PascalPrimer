Clear(0);
Home;

(*
for var j := 0 to 420 do
begin
  CursorColor := ComposeColorHSL(j / 200, 1, 0.5);  
  Draw(0.5 * (1 + Power(j, 0.9) + 0.1 * j));
  TurnLeft(61);
  Delay(1);
end;  
*)

const Steps : Integer = 24;
var Toggle : Integer = 0; 

Turn(1.5 * 360 / Steps);

for var j := 0 to Steps - 1 do
begin
  Turn(360 / Steps);
  Draw(400 / Steps);

  if Toggle = 0 then
    Turn(-4 * 360 / Steps)
  else  
    Turn(4 * 360 / Steps);
  Toggle := 1 - Toggle;
  Delay(1);
end;  

  Turn(360 / Steps);
  Go(400 / Steps);
  Turn(-3 * 360 / Steps);
  Go(400 / Steps);
  CursorAngle := -90;
  Draw(50);
  LineTo(0.5 * ClientWidth, 0.5 * ClientHeight + 25);

  Home;
  Turn(1.5 * 360 / Steps);

  Turn(-360 / Steps);
  Go(-400 / Steps);
  Turn(-3 * 360 / Steps);
  Go(-400 / Steps);
  CursorAngle := -90;
  Draw(50);
  LineTo(0.5 * ClientWidth, 0.5 * ClientHeight + 25);
