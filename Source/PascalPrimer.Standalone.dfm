object FormStandalone: TFormStandalone
  Left = 0
  Top = 0
  Caption = 'PascalPrimer - Standalone'
  ClientHeight = 619
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 1000
    Height = 619
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.OuterColor = -2830136
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnKeyPress = FormKeyPress
    OnPaintStage = Image32PaintStage
    OnResize = Image32Resize
  end
end
