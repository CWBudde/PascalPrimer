program PascalPrimer;

{$R *.res}

uses
  FastMM4,
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  PascalPrimer.Main in 'PascalPrimer.Main.pas' {FormMain},
  PascalPrimer.About in 'PascalPrimer.About.pas' {FormAbout};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
