// clear screen
Clear;

procedure Polygon(CenterX, CenterY: Float; Corners: Integer; LineLength: Float = 100);
begin 
  MoveTo(CenterX + LineLength, CenterY + 0);
  for var i := 1 to Corners do
  begin
    LineTo(
      CenterX + LineLength * cos(2 * Pi * i / Corners), 
      CenterY + LineLength * sin(2 * Pi * i / Corners));
  end;  
end;

for var i := 3 to 10 do
  Polygon(100, 100, i, i * 10);  
