uses
  TextUnit;

Clear;
Home;

CursorWidth := 3;

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

Home;
Turn;
Go(50);
TurnRight;
Go(-32);
CursorWidth := 2;
DrawText('Winner', 16);