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

const MaxIterations = 320;
var Palette: array of TColor;
Palette.SetLength(MaxIterations + 1);

var T := (1 shl 24) / (MaxIterations + 1);
for var I := 0 to MaxIterations do
  Palette[I] := ($7F shl 24) or Round(I * T);

function Mandelbrot(X, Y: Float): TColor;
var
  CX, CY, ZX, ZY, ZXSqr, ZYSqr: Float;
  I, M: Integer;
  W: Float;
  C1, C2: TColor;
const
  CBailoutValue = 4;
  CQuarter = 0.25;
begin
  CX := 0 + X * 1 / 50;
  CY := 0 + Y * 1 / 50;

  M := Length(Palette) - 2;

  // Check whether point lies in the period-2 bulb
  ZY := Sqr(CY);
  if Sqr(CX - 1) + ZY < 0.0625 then
    Exit(Palette[M]);

  // Check whether point lies in the cardioid
  ZX := Sqr(CX + CQuarter) + ZY;
  if ZX * (ZX - Cx - CQuarter) < CQuarter * ZY then
    Exit(Palette[M]);

  // Mandelbrot iteration: Z(n+1) = Z(n+1)^2 + C 
  ZX := 0;
  ZY := 0;
  ZXSqr := 0;
  ZYSqr := 0;
  I := 0;

  repeat
    ZY := 2 * ZY * ZX + CY;
    ZX := ZXSqr - ZYSqr - CX;
    ZXSqr := Sqr(ZX);
    ZYSqr := Sqr(ZY);
    if ZXSqr + ZYSqr > CBailoutValue then Break;
    Inc(I);
  until I = M;
  W := (16 * (ZX * ZX + ZY * ZY - 4)) / 255;
  W := Clamp(W, 0, 1);

  C1 := Palette[I];
  C2 := Palette[I + 1];
  
  Result := TColor(
    Round(w * ((C1       ) and $FF) + (1 - w) * ((C2       ) and $FF))        or 
    Round(w * ((C1 shr  8) and $FF) + (1 - w) * ((C2 shr  8) and $FF)) shl  8 or 
    Round(w * ((C1 shr 16) and $FF) + (1 - w) * ((C2 shr 16) and $FF)) shl 16
  );
  Result := ($FF shl 24) or Result; 
//  Result := TColor(Round(w * C1 + (w - 1) * C2));
//  Result := C1;
end;

for var y := 0 to 100 do
  for var x := 0 to 100 do
    SetPixelColor(x, y, MandelBrot(x, y)); 
    