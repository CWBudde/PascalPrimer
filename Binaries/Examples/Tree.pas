Home;
Clear;
CursorVisible := False;
CursorWidth := 3;

const Spread = 30;
const Scale = 0.75;
const StartPosX = 400;
const StartPosY = 300;
const Depth = 7;
const RandomSize = 0.05; 
const RandomDepth: Integer = 3; 
const RandomAngle = 5; 
 
procedure DrawBranch(Size, Angle: Float; Depth: Integer);
var
  CurrentAngle: Float;
  CurrentPosX: Float;
  CurrentPosY: Float;  
begin
  // get some nice color
  CursorColor := ComposeColorHSL(1.2 - 0.03 * Depth, 1, 0.5);
      
  TurnLeft(Angle);
  Draw(Size);
  
  if Depth + RandomInt(RandomDepth) > 0 then
  begin
    // store current start position
    CurrentAngle := CursorAngle;
    CurrentPosX := CursorPositionX;
    CurrentPosY := CursorPositionY;
    
    // draw next branch
    DrawBranch((Scale + RandomSize * Random) * Size, 
      -(Spread + RandomAngle * Random), Depth - 1);

    // recall recent start position
    CursorPositionX := CurrentPosX;
    CursorPositionY := CurrentPosY;
    CursorAngle := CurrentAngle;

    // draw next branch
    DrawBranch((Scale + RandomSize * Random) * Size, 
      Spread + RandomAngle * Random, Depth - 1);
  end;

  // small delay to see tree recursively painting
  Delay(1);  
end;
  
Turn;
Go(-64);
DrawBranch(64, 0, Depth);
