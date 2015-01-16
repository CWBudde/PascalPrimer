object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 126
  ClientWidth = 357
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClick = FormClick
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTitle: TLabel
    Left = 0
    Top = 0
    Width = 357
    Height = 52
    Align = alTop
    Alignment = taCenter
    Caption = 'PascalPrimer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -43
    Font.Name = 'Tahoma'
    Font.Style = []
    Font.Quality = fqAntialiased
    ParentFont = False
    OnClick = FormClick
  end
  object LabelAuthor: TLabel
    Left = 78
    Top = 55
    Width = 273
    Height = 19
    Caption = 'Copyright 2014 by Christian-W. Budde'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = FormClick
  end
  object Image32: TImage32
    Left = 8
    Top = 55
    Width = 64
    Height = 64
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnClick = FormClick
  end
  object MemoThanks: TMemo
    Left = 78
    Top = 88
    Width = 273
    Height = 31
    BorderStyle = bsNone
    Lines.Strings = (
      'Contributors:'
      'Norman Morrison (inspiration, name, documentation)')
    ReadOnly = True
    TabOrder = 1
    OnClick = FormClick
  end
  object Timer: TTimer
    Interval = 33
    OnTimer = TimerTimer
    Left = 168
    Top = 64
  end
end
