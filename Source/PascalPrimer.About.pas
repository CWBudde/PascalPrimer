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
    procedure DrawLogo;
  end;

implementation

uses
  System.Math, GR32_Math, GR32_VectorUtils, GR32_Polygons;

{$R *.dfm}

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Image32.Bitmap.SetSize(64, 64);
  FStartAngle := 0;
  DrawLogo;
end;

procedure TFormAbout.FormClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.TimerTimer(Sender: TObject);
begin
  FStartAngle := FStartAngle + 0.01;
  DrawLogo;
end;

procedure TFormAbout.DrawLogo;
var
  j: Integer;
  Angle, c, s: Single;
  Pos: array [0..1] of TPointF;
begin
  Image32.Bitmap.Clear(clWhite32);

  Pos[1] := PointF(32, 32);
  Angle := FStartAngle;

  for j := 0 to 127 do
  begin
    SinCos(Angle, 0.5 * (1 + Power(j, 0.9) + 0.1 * j), s, c);
    Pos[0] := PointF(Pos[1].X + c, Pos[1].Y + s);
    Angle := Angle + 1.06465089321136;
    Image32.Bitmap.LineFS(Pos[0].X, Pos[0].Y, Pos[1].X, Pos[1].Y,
      SetAlpha(HSLtoRGB(j / 200 - FStartAngle, 1, 0.5), 255 - 2 * j));
    Pos[1] := Pos[0];
  end;
end;

end.
