program StandaloneFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  PascalPrimer.Shared in '..\PascalPrimer.Shared.pas' {DataModuleShared},
  PascalPrimer.StandaloneFMX in 'PascalPrimer.StandaloneFMX.pas' {FormStandalone};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModuleShared, DataModuleShared);
  Application.CreateForm(TFormStandalone, FormStandalone);
  if not Application.Terminated then
    Application.Run;
end.
