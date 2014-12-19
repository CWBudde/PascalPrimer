unit PascalPrimer.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.PngImage, Vcl.ExtCtrls, Vcl.StdCtrls, GR32, GR32_Image;

type
  TFormAbout = class(TForm)
    LabelTitle: TLabel;
    LabelAuthor: TLabel;
    Image32: TImage32;
    Timer: TTimer;
    MemoThanks: TMemo;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FStartAngle: Single;
  end;

implementation

uses
  System.Math, GR32_Math, GR32_VectorUtils, GR32_Polygons, GR32_PNG,
  GR32_ColorGradients, GR32_Blend;

{$R *.dfm}

procedure DrawLogo(Bitmap: TBitmap32; StartAngle: Single = 0);
var
  j, f: Integer;
  Angle, c, s: Single;
  Pos: array [0..1] of TPointF;
begin
  Angle := StartAngle;
  Pos[1] := PointF(Bitmap.Width div 2, Bitmap.Height div 2);
  f := Max(1, 255 div (Bitmap.Width + Bitmap.Height));
  for j := 0 to (Bitmap.Width + Bitmap.Height) - 1  do
  begin
    SinCos(Angle, 0.5 * (1 + Power(j, 0.9) + 0.1 * j), s, c);
    Pos[0] := PointF(Pos[1].X + c, Pos[1].Y + s);
    Angle := Angle + 1.06465089321136;
    Bitmap.LineFS(Pos[0].X, Pos[0].Y, Pos[1].X, Pos[1].Y,
      SetAlpha(HSLtoRGB(j / 200 - StartAngle, 1, 0.5), 255 - f * j));
    Pos[1] := Pos[0];
  end;
end;

procedure BuildIconImages;
var
  X, Y: Integer;
  Bitmap: TBitmap32;
  Dimension, Index: Integer;
  Sampler: TRadialGradientSampler;
const
  CDimensions: array [0 .. 5] of Integer = (16, 24, 32, 48, 64, 256);
begin
  Bitmap := TBitmap32.Create;
  try
    for Index := Low(CDimensions) to High(CDimensions) do
    begin
      Dimension := CDimensions[Index];
      Bitmap.SetSize(Dimension, Dimension);
      Bitmap.Clear(0);

      Sampler := TRadialGradientSampler.Create(wmClamp);
      try
        Sampler.Center := PointF(0.5 * Bitmap.Width, 0.5 * Bitmap.Height);
        Sampler.Radius := 0.5 * Bitmap.Width;
        Sampler.Gradient.StartColor := clWhite32;
        Sampler.Gradient.EndColor := $FFFFFF;
        Sampler.PrepareSampling;
        for y := 0 to Bitmap.Height - 1 do
          for x := 0 to Bitmap.Width - 1 do
            Bitmap.Pixel[X, Y] := Sampler.GetSampleInt(X, Y);
      finally
        FreeAndNil(Sampler);
      end;

      DrawLogo(Bitmap);
      SaveBitmap32ToPNG(Bitmap, ExtractFilePath(ParamStr(0)) +
        IntToStr(Dimension) + 'x' + IntToStr(Dimension) + '.png');
    end;
  finally
    FreeAndNil(Bitmap);
  end;
end;


{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Image32.Bitmap.SetSize(64, 64);
  FStartAngle := 0;
  Image32.Bitmap.Clear(clWhite32);
  DrawLogo(Image32.Bitmap, FStartAngle);
  // BuildIconImages; <-- need sqrt adjustments in DrawLogo and radial gradient
end;

procedure TFormAbout.FormClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.TimerTimer(Sender: TObject);
begin
  FStartAngle := FStartAngle + 0.01;
  Image32.Bitmap.Clear(clWhite32);
  DrawLogo(Image32.Bitmap, FStartAngle);
end;

end.
