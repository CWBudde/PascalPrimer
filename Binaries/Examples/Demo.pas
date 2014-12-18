Center;
Clear(clWhite);        

MoveTo(0.5 * ClientWidth - 10, 0.5 * ClientHeight);
CursorAngle := 0;     
AntiAliasedLine := True;

const Steps = 157;
const Interval = 10;

for var i := 0 to Steps - 1 do                                      
begin                                                                
  if i < (Steps div 2) then                                       
    CursorColor := ComposeColor(i / (Steps div 2), 0, (Steps div 2 - i) / (Steps div 2))
  else
    CursorColor := ComposeColor(
      1 - (i - (Steps div 2)) / (Steps div 2), 0, 
      1 - ((Steps div 2 - (i - Steps div 2)) / (Steps div 2)));

  Draw(90);
  Delay(10);
  TurnLeft(45);       
  Draw(60); 
  TurnRight(0.5 * 360 / Steps);
  TurnRight(180);
  Draw(76);
  TurnRight(45);
end;