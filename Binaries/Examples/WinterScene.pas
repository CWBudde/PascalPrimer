Clear(clBlack);
Home;

procedure DrawOutlineCircle(Size: Float = 100; Steps: Integer = 36);
begin
  var Counter: Integer = Steps;
  repeat
    Dec(Counter, 1);
  
    Draw(Pi * Size / Steps);
    Turn(360 / Steps);
  until Counter = 0;
end;   

procedure DrawSnowMan(Size: Integer = 80);
begin
  CursorColor := clWhite;
  DrawOutlineCircle(Size);

  TurnLeft;
  Go(Size);
  TurnRight;
  DrawOutlineCircle(0.75 * Size);

  TurnLeft;
  Go(0.75 * Size);
  TurnRight;
  DrawOutlineCircle(0.5 * Size);

  TurnLeft;
  Go(0.2 * Size);
  TurnRight;
  CursorColor := ComposeColor(1, 0.5, 0);
  DrawOutlineCircle(0.04 * Size);

  CursorColor := clBlue;
  Go(0.1 * Size);
  TurnLeft;
  Go(0.1 * Size);
  TurnRight;
  DrawOutlineCircle(0.04 * Size);
  Go(-0.2 * Size);
  DrawOutlineCircle(0.04 * Size);

  CursorColor := clWhite;
  TurnRight;
  Go(0.18 * Size);
  TurnLeft(65);
  Draw(5);
  TurnLeft(25);
  Draw(10);
  TurnLeft(25);
  Draw(5);      
end;   

procedure StarA(Corners: Integer; Size: Integer = 20);
var 
  Counter: Integer;
begin
  Counter := 2 * Corners;
  repeat
    Dec(Counter, 1);
  
    Draw(Size / Corners);
  
    if Counter mod 2 = 0 then
      TurnRight(99)
    else
      TurnLeft(99);
  
    Turn(180 / Corners);
  until Counter = 0;
end;   

procedure StarB(Corners: Integer; Size: Integer = 20);
var 
  Counter: Integer;
  Factor: Integer;
begin
  Counter := 2 * Corners;

  Factor := 1 + Integer((Corners mod 2) = 1);

  repeat
    Dec(Counter, 1);
  
    Draw(Factor * Size / Corners);
  
    if Counter mod 2 = 0 then
      TurnRight(99)
    else
      TurnLeft(99);
  
    Turn(Factor * 180 / Corners);
  until Counter = 0;
end;   

procedure DrawStars;
var 
  Counter: Integer;
  RedAmount: Float;
begin
  for Counter := 0 to 15 do
  begin
    MoveTo(ClientWidth * 0.5 * (Random + Random), 
      ClientHeight * 0.25 * (Random + Random));
    CursorAngle := Random * 360;            
    
    RedAmount := 0.8 + 0.2 * Random;                                                
    CursorColor := ComposeColor(RedAmount, Sqrt(Random) * RedAmount, 0); 
     
    if Random < 0.2 then
      StarB(7)
    else                 
      StarA(4 + Round(3 * Random));
  end;  
end;

procedure DrawTree(Factor: Float);
const
  Angle: Float = 150;
begin
  CursorAngle := 0;
  PushPosition;

  Draw(Factor * 10);
  TurnLeft;
  Draw(Factor * 10);
  TurnRight;
  
  for var i := 0 to 3 do
  begin
    Draw(3 / (3 + i) * Factor * 60);
    TurnLeft(Angle);
    Draw(3 / (3 + i) * Factor * 72);
    TurnRight(Angle);
  end;  
  
  PopPosition;
  CursorAngle := 0;
  TurnLeft;
  Draw(Factor * 10);
  TurnLeft;
  
  for var i := 0 to 3 do
  begin
    Draw(3 / (3 + i) * Factor * 60);
    TurnRight(Angle);
    Draw(3 / (3 + i) * Factor * 72);
    TurnLeft(Angle);                           
  end;  
end;                    

procedure DrawTrees;
var
  RelativeHeight: Float;
begin
  for var i := 0 to 9 do
  begin          
    RelativeHeight := (1 + Random); 
    MoveTo(Random * ClientWidth, 0.5 * ClientHeight * RelativeHeight);
    CursorColor := ComposeColor(0, 0.5 * (Random + 1), 0, 0.5 * (Random + 1));
    DrawTree(0.2 * (RelativeHeight + Random));                               
  end;                         
end;                  

procedure DrawGradient;
var
  Color: TColor;
  H2: Integer;
begin
  H2 := (ClientHeight div 2);
  for var Y := 0 to H2 - 1 do
  begin                        
    Color := ComposeColorHSL(0.5 * y / H2, 0.5, 0.1 * y / H2); 
    for var X := 0 to ClientWidth - 1 do
      SetPixelColor(X, Y, Color);
  end;    
end;

DrawGradient;
DrawTrees;
CursorPositionX := Random * ClientWidth;          
CursorPositionY := 0.5 * ClientHeight * (1 + Random);
CursorAngle := 0;
DrawSnowMan;
DrawStars;