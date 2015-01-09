// clear screen
Clear;

var Palette: array of TColor;

function Mandelbrot(CX, CY: Float; MaxIterations: Integer): TColor;
var
  ZX, ZY, ZXSqr, ZYSqr: Float;
  I, M: Integer;
  W: Float;
  C1, C2: TColor;
const
  CBailoutValue = 4;
  CQuarter = 0.25;
begin
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
  W := (ZX * ZX + ZY * ZY - 4) / 16;
  W := Clamp(W, 0, 1);
  
  if I < M then
  begin
    var zn := sqrt(ZXSqr + ZYSqr);
    w := Clamp(log2(log2(zn)), 0, 1);
  end
  else
    w := 0;  
    
  C1 := Palette[I];
  C2 := Palette[I + 1];
  
  Result := TColor(
    Round(w * ((C1       ) and $FF) + (1 - w) * ((C2       ) and $FF))        or 
    Round(w * ((C1 shr  8) and $FF) + (1 - w) * ((C2 shr  8) and $FF)) shl  8 or 
    Round(w * ((C1 shr 16) and $FF) + (1 - w) * ((C2 shr 16) and $FF)) shl 16
  );
  Result := ($FF shl 24) or Result; 
//  Result := C1;
end;

procedure DrawMandelBrot(MaxIterations: Integer = 100; 
  Left: Float = -2; Right: Float = +2;
  Top: Float = -2; Bottom: Float = +2);
var
  CX, CY: Float;  
begin
  Palette.SetLength(MaxIterations + 1);
  
  var T := (1 shl 24) / (MaxIterations + 1);
  for var I := 0 to MaxIterations do
    Palette[I] := ($FF shl 24) or Round(I * T);

  for var y := 0 to ClientHeight - 1 do
  begin
    for var x := 0 to ClientWidth - 1 do
    begin
      CX := Left + X * (Right - Left) / ClientWidth;
      CY := Top + Y * (Bottom - Top) / ClientHeight;
      SetPixelColor(x, y, MandelBrot(CX, CY, MaxIterations));
    end;  
    Delay(0);  
  end;
end;       
    
DrawMandelBrot(100);
//DrawMandelBrot(100, 0.749, 0.750, 0.101, 0.1);
