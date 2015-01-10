unit PascalPrimer.Statistics;

interface

uses
  PascalPrimer.Shared;

type
  TStatistics = class
  private
    FCall: array [TLogCall] of Integer;
    function GetTurnCalls: Integer;
    function GetTotalCalls: Integer;
  public
    procedure Reset; overload;
    procedure Reset(CallType: TLogCall); overload;
    procedure LogCall(CallType: TLogCall; ClassAccess: Boolean = False);

    property ClearCalls: Integer read FCall[lcClear];
    property CenterCalls: Integer read FCall[lcCenter];
    property Executions: Integer read FCall[lcExecution];
    property ComposeColorCalls: Integer read FCall[lcComposeColor];
    property ComposeColorHSLCalls: Integer read FCall[lcComposeColorHSL];
    property DelayCalls: Integer read FCall[lcDelay];
    property HomeCalls: Integer read FCall[lcHome];
    property GoCalls: Integer read FCall[lcGo];
    property DrawCalls: Integer read FCall[lcDraw];
    property TurnLeftCalls: Integer read FCall[lcTurnLeft];
    property TurnRightCalls: Integer read FCall[lcTurnRight];
    property TurnCalls: Integer read GetTurnCalls;
    property TotalCalls: Integer read GetTotalCalls;
    property ColorChanges: Integer read FCall[lcColorChange];
    property AngleChanges: Integer read FCall[lcAngleChange];
    property AntialiasedChanges: Integer read FCall[lcAntialiased];
    property PopPositionCalls: Integer read FCall[lcPopPosition];
    property PushPositionCalls: Integer read FCall[lcPushPosition];
    property SaveToFileCalls: Integer read FCall[lcSaveToFile];
    property SetPixelColorCalls: Integer read FCall[lcSetPixelColor];
    property GetPixelColorCalls: Integer read FCall[lcGetPixelColor];
  end;

implementation

{ TStatistics }

function TStatistics.GetTotalCalls: Integer;
var
  Call: TLogCall;
begin
  Result := 0;
  for Call := Low(FCall) to High(FCall) do
    Result := Result + FCall[Call];
end;

function TStatistics.GetTurnCalls: Integer;
begin
  Result := FCall[lcTurnLeft] + FCall[lcTurnRight];
end;

procedure TStatistics.LogCall(CallType: TLogCall; ClassAccess: Boolean = False);
begin
  Inc(FCall[CallType]);
end;

procedure TStatistics.Reset(CallType: TLogCall);
begin
  FCall[CallType] := 0;
end;

procedure TStatistics.Reset;
var
  Call: TLogCall;
begin
  for Call := Low(FCall) to High(FCall) do
    FCall[Call] := 0;
end;

end.
