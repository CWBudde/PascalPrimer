Clear(clWhite);
Home;

var X, Y: Integer;
var Color: TColor;

Randomize;
repeat
  X := RandomInt(ClientWidth);
  Y := RandomInt(ClientHeight);
  Color := ComposeColorHSL(Random);

  if RandomInt(2) = 0 then 
    DrawCircle(X, Y, RandomInt(100), Color)
  else  
    DrawRectangle(X - 50, Y - 50, X + 50, Y + 50, Color);

  Delay(100);
until ReadMouseButton;
