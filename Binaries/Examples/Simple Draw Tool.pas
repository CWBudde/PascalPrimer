Home;
Clear;
CursorVisible := True;

var Pos: TPoint;

repeat
  // wait for any mouse click
  repeat
    Sleep(10);
  until ReadMouseButton;

  // check if a key has been pressed in the meantime
  if ReadKey <> '' then
    Exit;

  // get a random color
  CursorColor := $FF shl 24 + RandomInt($FFFFFF);
  
  repeat  
    Pos := GetMousePosition;

    LookAt(Pos.X, Pos.Y);
    if Sqr(CursorPositionX - Pos.X) + Sqr(CursorPositionY - Pos.Y) > 100 then
      Draw(10);  

    Delay(10);
  until ReadMouseButton;
until ReadKey <> '';
