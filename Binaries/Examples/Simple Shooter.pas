type
  TStarPoint = record
    Pos: TPoint;
    Size: Integer;
    Color: TColor;
  end;

var Stars: array [0..5] of TStarPoint;
var Count: Integer := 0;
var MousePos: TPoint;

procedure RandomizeStar(var Star: TStarPoint);
begin
  Star.Pos.X := RandomInt(ClientWidth);
  Star.Pos.Y := RandomInt(ClientHeight);
  Star.Size := 3 + RandomInt(10);
  Star.Color := $FF shl 24 or RandomInt($FFFFFF);
end;

procedure DrawMark(Pos: TPoint; Size: Integer; Color: TColor);
begin
  for var sy := -Size div 2 to Size div 2 do 
    for var sx := -Size div 2 to Size div 2 do 
      SetPixelColor(Pos.X + sx, Pos.Y + sy, Color);
end;

procedure Tick;
begin
  if ReadMouseButton then
    for var i := Low(Stars) to High(Stars) do
    begin
      MousePos := GetMousePosition;
      if (Abs(MousePos.X - Stars[i].Pos.X) < Stars[i].Size div 2) and 
         (Abs(MousePos.Y - Stars[i].Pos.Y) < Stars[i].Size div 2) then
      begin
        Inc(Count);
        TextOutput.Text := 'Hit count: ' + IntToStr(Count) + ' / 10';
        DrawMark(Stars[i].Pos, Stars[i].Size, clBlack);
        RandomizeStar(Stars[i]);
        Break;
      end;  
    end;  

  for var i := Low(Stars) to High(Stars) do               
  begin
    DrawMark(Stars[i].Pos, Stars[i].Size, clBlack);

    Stars[i].Pos.X := Stars[i].Pos.X + 2 * RandomInt(2) - 1;
    Stars[i].Pos.Y := Stars[i].Pos.Y + 2 * RandomInt(2) - 1;
    
    if (Stars[i].Pos.X < 0) or (Stars[i].Pos.X >= ClientWidth) or
       (Stars[i].Pos.Y < 0) or (Stars[i].Pos.Y >= ClientHeight) then
      RandomizeStar(Stars[i]);

    DrawMark(Stars[i].Pos, Stars[i].Size, Stars[i].Color);
  end;
end;

procedure MainLoop;
begin
  repeat
    Tick;
    Delay(100, True);
  until (ReadKey <> '') or (Count > 9);
end;

procedure WinAnimation;
begin
  Home;
  for var i := 0 to Max(ClientWidth, ClientHeight) do
  begin
    CursorColor := ComposeColorHSL(i * 0.01, 1, 0.5);
    Draw(1 + 0.5 * i);
    Turn(40 + RandomInt(2));
    Delay(2);
  end;
  
  TextOutput.Text := 'You won!';
end;

// randomize the stars
for var i := Low(Stars) to High(Stars) do
  RandomizeStar(Stars[i]);

// clear the background to black
Clear(clBlack);

// show some instructions
TextOutput.Text := 'Shoot the items!';

// now run the main loop
MainLoop;

if Count > 9 then
  WinAnimation;