unit TextUnit;

interface

uses
  TurtleBasic, TurtleIntermediate, TurtleAdvanced;

procedure DrawText(Text: String; Height: Float = 10);

implementation

procedure DrawHalfCircle(Height: Float);
var
  InvSign: Integer;
  Edges: Integer;
begin
  InvSign := Sign(Height);
  Height := Abs(Height);
  Edges := Round(Height);
  Edges := Edges + (Edges mod 2);
  TurnRight(InvSign * -180 / Edges);
  for var i := 0 to (Edges div 2) - 1 do
  begin
    Draw(Height * sin(Pi / Edges));
    Turn(InvSign * 360 / Edges)
  end;
  TurnRight(InvSign * +180 / Edges);
end;  

procedure DrawCharacter(Char: String; Height: Float = 10);
begin
  case Char of
    ' ': 
      Go(0.5 * Height);
    '-':
      begin 
        Turn;
        Go(0.5 * Height);
        TurnRight;
        Draw(0.5 * Height);
        TurnRight;
        Go(0.5 * Height);
        Turn;
        Go(0.25 * Height);
      end;  
    'A':
      begin
        PushPosition;
        TurnLeft(75);
        Go(0.5 * Height);
        TurnRight(75);
        Draw(0.25 * Height);

        PopPosition;
        TurnLeft(75);
        Draw(Height);
        TurnRight(150);
        Draw(Height);
        TurnLeft(75);
        Go(0.25 * Height);
      end;
    'B':
      begin
        Turn;
        Draw(Height);
        TurnRight;
        Draw(0.125 * Height);
        DrawHalfCircle(-0.5 * Height);
        Go(0.125 * Height);
        Turn(180);
        Draw(0.25 * Height);
        DrawHalfCircle(-0.5 * Height);
        Draw(0.25 * Height);
        Turn(180);
        Go(0.75 * Height);
      end;
    'C':
      begin
        Turn;
        Go(Height);
        TurnRight;
        Go(0.5 * Height);
        Turn(180);
        DrawHalfCircle(Height);
        Go(0.25 * Height);
      end;
    'D':
      begin
        Turn;
        Draw(Height);
        TurnRight;
        DrawHalfCircle(-Height);
        Turn(180);
        Go(0.75 * Height);
      end;
    'E':
      begin
        PushPosition;
        Turn;
        Draw(0.5 * Height);
        PushPosition;
        Draw(0.5 * Height);
        TurnRight;
        Draw(0.5 * Height);
        PopPosition;
        Draw(0.5 * Height);
        PopPosition;
        Draw(0.5 * Height);
        Go(0.25 * Height);
      end;
    'F':
      begin
        PushPosition;
        Turn;
        Draw(0.5 * Height);
        PushPosition;
        Draw(0.5 * Height);
        TurnRight;
        Draw(0.5 * Height);
        PopPosition;
        Draw(0.5 * Height);
        PopPosition;
        Go(0.75 * Height);
      end;
    'G':
      begin
        PushPosition;
        Turn;
        Go(Height);
        TurnRight;
        Go(0.5 * Height);
        Turn(180);
        DrawHalfCircle(Height);
        Turn;
        Draw(0.5 * Height);
        Turn;
        Draw(0.25 * Height);
        PopPosition;
        Turn(180);        
        Go(0.75 * Height);
      end;
    'H':
      begin
        Turn;
        Draw(Height);
        Go(-0.5 * Height);
        TurnRight;
        Draw(0.5 * Height);
        Turn;
        Go(0.5 * Height);
        Draw(-Height);
        TurnRight;
        Go(0.25 * Height);
      end;
    'I':
      begin
        Turn;
        Draw(Height);
        Go(-Height);
        TurnRight;
        Go(0.25 * Height);
      end;
    'J':
      begin
        Go(0.25 * Height);
        Turn;
        Go(Height);
        Turn(180);
        Draw(0.875 * Height);
        DrawHalfCircle(-0.25 * Height);
        Go(-0.125 * Height);
        TurnRight;
        Go(0.5 * Height);
      end;
    'K':
      begin
        PushPosition;
        Turn;
        Draw(Height);
        Go(-0.5 * Height);
        TurnRight(45);
        Draw(0.5 * sqrt(2) * Height);
        Go(-0.5 * sqrt(2) * Height);
        TurnRight(90);
        Draw(0.5 * sqrt(2) * Height);
        PopPosition;
        Turn(45);

        Go(0.5 * sqrt(2) * Height);
      end;
    'L':
      begin
        PushPosition;
        Turn;
        Draw(Height);
        PopPosition;
        TurnRight;
        Draw(0.5 * Height);
        Go(0.25 * Height);
      end;
    'M':
      begin
        Turn;
        Draw(Height);
        TurnRight(150);
        Draw(0.5 * Height);
        Turn(120);
        Draw(0.5 * Height);
        TurnRight(150);
        Draw(Height);
        Turn;
        Go(0.25 * Height);
      end;
    'N':
      begin
        Turn;
        Draw(Height);
        TurnRight(150);
        Draw(2 / sqrt(3) * Height);
        Turn(150);
        Draw(Height);
        Go(-Height);
        TurnRight;
        Go(0.25 * Height);
      end;
    'O':
      begin
        PushPosition;
        Turn;
        Go(0.25 * Height);
        Turn(180);

        DrawHalfCircle(0.5 * Height);
        Draw(0.5 * Height);
        DrawHalfCircle(0.5 * Height);
        Draw(0.5 * Height);
        Turn;

        PopPosition;
        Go(0.75 * Height);
      end;
    'P':
      begin
        PushPosition;
        Turn;
        Draw(Height);
        TurnRight;
        Draw(0.25 * Height);
        DrawHalfCircle(-0.5 * Height);
        Draw(0.25 * Height);

        PopPosition;
        Turn(180);
        Go(0.75 * Height);
      end;
    'Q':
      begin
        PushPosition;
        Turn;
        Go(0.25 * Height);
        Turn(180);

        DrawHalfCircle(0.5 * Height);
        Draw(0.5 * Height);
        DrawHalfCircle(0.5 * Height);
        Draw(0.5 * Height);
        Turn;

        PopPosition;
        Go(0.5 * Height);
        TurnLeft(135);
        Draw(0.25 * Height);
        Go(-0.25 * Height);
        TurnRight(135);
        Go(0.25 * Height);
      end;
    'R':
      begin
        PushPosition;
        Turn;
        Draw(Height);
        TurnRight;
        Draw(0.25 * Height);
        DrawHalfCircle(-0.5 * Height);
        Draw(0.25 * Height);
        Turn(135);
        Draw(0.5 * sqrt(2) * Height);
        Turn(45);
        PopPosition;
        Go(0.75 * Height);
      end;
    'S':
      begin
        Turn;
        Go(Height);
        TurnRight;
        Go(0.5 * Height);
        Turn(180);
        Draw(0.25 * Height);
        DrawHalfCircle(0.5 * Height);
        DrawHalfCircle(-0.5 * Height);
        Draw(0.25 * Height);
        Turn(180);
        Go(0.75 * Height);
      end;
    'T':
      begin
        Go(0.25 * Height);
        PushPosition;
        Turn;
        Draw(Height);
        Turn;
        Go(0.4 * Height);
        Turn(180);
        Draw(0.8 * Height);
        PopPosition;
        Go(0.5 * Height);
      end;
    'U':
      begin
        PushPosition;
        Turn;
        Go(Height);
        Turn(180);
        Draw(0.75 * Height);

        DrawHalfCircle(0.5 * Height);
        Draw(0.75 * Height);
        TurnRight;

        PopPosition;
        Go(0.75 * Height);
      end;
    'V':
      begin
        Turn; 
        Go(Height);
        TurnRight(165);
        Draw(sqrt(1.125) * Height);
        Turn(150);
        Draw(sqrt(1.125) * Height);
        Turn(15);
        Go(-Height);
        TurnRight; 
        Go(0.25 * Height);
      end;
    'W':
      begin
        Turn; 
        Go(Height);
        TurnRight(165);
        Draw(sqrt(1.125) * Height);
        Turn(150);
        Draw(0.6 * Height);
        TurnRight(150);

        Draw(0.6 * Height);
        Turn(150);
        Draw(sqrt(1.125) * Height);
        Turn(15);

        Go(-Height);
        TurnRight; 
        Go(0.25 * Height);
      end;
    'X':
      begin
        PushPosition;
        Turn(60);        
        Draw(2 / sqrt(3) * Height);
        Turn(30);
        PopPosition;
        Go(Height);
        TurnRight(150);
        Draw(2 / sqrt(3) * Height);
        TurnRight(-150 + 90);
        Go(0.25 * Height);
      end;
    'Y':
      begin
        PushPosition;
        Turn(60);        
        Draw(2 / sqrt(3) * Height);
        Turn(30);
        PopPosition;
        PushPosition;
        Go(Height);
        TurnRight(150);
        Draw(1 / sqrt(3) * Height);
        TurnRight(-150 + 90);
        PopPosition;
        Go(0.75 * Height);
      end;
    'Z':
      begin
        Turn;        
        Go(Height);
        TurnRight;        
        Draw(0.5 * Height);
        TurnRight(120);        
        Draw(2 / sqrt(3) * Height);
        Turn(120);        
        Draw(0.5 * Height);
        Go(0.25 * Height);
      end;
  end;
end;

procedure DrawText(Text: String; Height: Float = 10);
var
  StartX, StartY: Float;
begin
  StartX := CursorPositionX;
  StartY := CursorPositionY;
  
  for var Index := 1 to Length(Text) do
  begin
    case Text[Index] of  
      #13:
        begin
          StartY := StartY + 1.5 * Height;
          CursorPositionX := StartX;
          CursorPositionY := StartY;
        end;
      else    
        DrawCharacter(UpperCase(Text[Index]), Height);
    end;    
  end;
end;

end.