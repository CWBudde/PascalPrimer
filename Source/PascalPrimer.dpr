program PascalPrimer;

{$R *.res}

uses
  FastMM4,
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  PascalPrimer.Main in 'PascalPrimer.Main.pas' {FormMain},
  PascalPrimer.About in 'PascalPrimer.About.pas' {FormAbout},
  PascalPrimer.Shared in 'PascalPrimer.Shared.pas' {DataModuleShared: TDataModule},
  PascalPrimer.Statistics in 'PascalPrimer.Statistics.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModuleShared, DataModuleShared);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
