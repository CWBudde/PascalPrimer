program Standalone;

{$R *.res}

uses
  FastMM4,
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  PascalPrimer.Shared in 'PascalPrimer.Shared.pas' {DataModuleShared},
  PascalPrimer.Standalone in 'PascalPrimer.Standalone.pas' {FormStandalone};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModuleShared, DataModuleShared);
  Application.CreateForm(TFormStandalone, FormStandalone);
  if not Application.Terminated then
    Application.Run;
end.
