unit PascalPrimer.Shared;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.Variants,

  (* GR32 *)
  GR32,

  (* DWS *)
  dwsComp, dwsExprs, dwsSymbols, dwsErrors, dwsSuggestions, dwsVCLGUIFunctions,
  {$IFNDEF WIN64} dwsJIT, dwsJITx86, {$ENDIF} dwsStrings, dwsUnitSymbols,
  dwsFunctions, dwsTokenizer;

type
  IImageCanvas = interface
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixelColor(X, Y: Integer): TColor32;
    procedure SetPixelColor(X, Y: Integer; Value: TColor32);

    procedure Clear(Color: TColor32);
    procedure DrawLine(A, B: TPoint; Color: TColor32);
    procedure DrawLineF(A, B: TPointF; Color: TColor32);
    procedure Invalidate;
    procedure SaveToFile(FileName: TFileName);

    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property PixelColor[X, Y: Integer]: TColor32 read GetPixelColor write SetPixelColor;
  end;

  TTurtleCanvas = class
  type
    TOutputLogMessage = procedure (Sender: TObject; const Text: string) of object;
  private
    FImageCanvas: IImageCanvas;
    FColor: TColor32;
    FOnOutputLogMessage: TOutputLogMessage;
    FOnInvalidate: TNotifyEvent;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixel(X, Y: Integer): TColor32; inline;
    procedure SetPixel(X, Y: Integer; Color: TColor32); inline;
  public
    constructor Create(ImageCanvas: IImageCanvas);

    procedure Clear; overload;
    procedure Clear(Color: TColor32); overload;

    procedure DrawLine(AX, AY, BX, BY: Integer; Color: TColor32); overload;
    procedure DrawLine(A, B: TPoint; Color: TColor32); overload;
    procedure DrawLineF(A, B: TPointF; Color: TColor32);

    procedure SaveToFile(FileName: TFileName);

    procedure Invalidate;

    property ImageCanvas: IImageCanvas read FImageCanvas;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Color: TColor32 read FColor write FColor;
    property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel;

    property OnOutputLogMessage: TOutputLogMessage read FOnOutputLogMessage write FOnOutputLogMessage;
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
  end;

  TTurtleCursor = class
  type
    TPointStack = TStack<TPointF>;
  private
    FAngle: Single;
    FAntiAliased: Boolean;
    FCanvas: TTurtleCanvas;
    FColor: TColor32;
    FPosition: TPointF;
    FPositionStack: TPointStack;
    FVisible: Boolean;
    FOnPositionChanged: TNotifyEvent;
    procedure SetPosition(const Value: TPointF);
  protected
    procedure DrawLine(A, B: TPointF);
  public
    constructor Create(TurtleCanvas: TTurtleCanvas);
    destructor Destroy; override;

    procedure PopPosition;
    procedure PushPosition;

    procedure Go(Distance: Single = 10);
    procedure Draw(Distance: Single = 10);
    procedure Turn(NewAngleInDegree: Single = 90);
    procedure MoveTo(X, Y: Single);
    procedure LineTo(X, Y: Single);
    procedure LookAt(X, Y: Single);

    procedure Home(TruncToInteger: Boolean = False);
    procedure Center(TruncToInteger: Boolean = False);

    property Angle: Single read FAngle write FAngle;
    property AntiAliased: Boolean read FAntiAliased write FAntiAliased;
    property Color: TColor32 read FColor write FColor;
    property Position: TPointF read FPosition write SetPosition;
    property PositionStack: TPointStack read FPositionStack;
    property Visible: Boolean read FVisible write FVisible;

    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
  end;

  TTextOutput = class
  private
    FStringList: TStringList;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create(StringList: TStringList);
    procedure Clear;

    property Text: string read GetText write SetText;
  end;

  TLogCall = (lcExecution, lcClear, lcCenter, lcComposeColor,
    lcComposeColorHSL, lcDelay, lcHome, lcLineTo, lcLookAt, lcMoveTo, lcGo,
    lcDraw, lcSaveToFile, lcTurnLeft, lcTurnRight, lcColorChange,
    lcAngleChange, lcAntialiased, lcPopPosition, lcPushPosition,
    lcSetPixelColor, lcGetPixelColor);

  TDataModuleShared = class(TDataModule)
    DelphiWebScript: TDelphiWebScript;
    dwsUnitBasic: TdwsUnit;
    dwsUnitIntermediate: TdwsUnit;
    dwsUnitAdvanced: TdwsUnit;

    procedure dwsClassesTCanvasMethodsClearEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCanvasMethodsGetColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCanvasMethodsGetPixelColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCanvasMethodsSetColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCanvasMethodsSetPixelColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCanvasSaveToFileEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsDrawEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsGetAngleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsGetColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsGetVisibleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsGoEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsLookAtEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsPopPositionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsPushPositionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsSetAngleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsSetColorEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsSetVisibleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsTurnLeftEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTCursorMethodsTurnRightEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsFunctionsCenterEval(info: TProgramInfo);
    procedure dwsFunctionsClearEval(info: TProgramInfo);
    procedure dwsFunctionsComposeColorEval(info: TProgramInfo);
    procedure dwsFunctionsComposeColorHSLEval(info: TProgramInfo);
    procedure dwsFunctionsCosineEval(info: TProgramInfo);
    procedure dwsFunctionsDelayEval(info: TProgramInfo);
    procedure dwsFunctionsDrawEval(info: TProgramInfo);
    procedure dwsFunctionsGetPixelColorEval(info: TProgramInfo);
    procedure dwsFunctionsGoEval(info: TProgramInfo);
    procedure dwsFunctionsHomeEval(info: TProgramInfo);
    procedure dwsFunctionsLineToEval(info: TProgramInfo);
    procedure dwsFunctionsLookAtEval(info: TProgramInfo);
    procedure dwsFunctionsMoveToEval(info: TProgramInfo);
    procedure dwsFunctionsPopPositionEval(info: TProgramInfo);
    procedure dwsFunctionsPushPositionEval(info: TProgramInfo);
    procedure dwsFunctionsSaveToFileEval(info: TProgramInfo);
    procedure dwsFunctionsSetPixelColorEval(info: TProgramInfo);
    procedure dwsFunctionsSineEval(info: TProgramInfo);
    procedure dwsFunctionsTangentEval(info: TProgramInfo);
    procedure dwsFunctionsTurnLeftEval(info: TProgramInfo);
    procedure dwsFunctionsTurnRightEval(info: TProgramInfo);
    procedure dwsInstanceCanvasInstantiate(info: TProgramInfo; var ExtObject: TObject);
    procedure dwsInstanceCursorInstantiate(info: TProgramInfo; var ExtObject: TObject);
    procedure dwsVariablesAntiAliasedLineReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesAntiAliasedLineWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesCanvasColorReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCanvasColorWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesClientHeightReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesClientWidthReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorAngleReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorAngleWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesCursorColorReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorColorWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesCursorPositionXReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorPositionXWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesCursorPositionYReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorPositionYWriteVar(info: TProgramInfo; const value: Variant);
    procedure dwsVariablesCursorReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorWriteVar(info: TProgramInfo; const value: Variant);
    procedure DelphiWebScriptInclude(const scriptName: string; var scriptSource: string);
    function DelphiWebScriptNeedUnit(const unitName: string; var unitSource: string): IdwsUnit;
  type
    TOnLogCallEvent = procedure(Sender: TObject; CallType: TLogCall;
      ClassAccess: Boolean = False) of object;
  private
    FTurtleCanvas: TTurtleCanvas;
    FTurtleCursor: TTurtleCursor;
    FTextOutput: TTextOutput;
    FOnLogCall: TOnLogCallEvent;
    procedure LogCall(CallType: TLogCall; ClassAccess: Boolean = False);
  public
    property TurtleCanvas: TTurtleCanvas read FTurtleCanvas write FTurtleCanvas;
    property TurtleCursor: TTurtleCursor read FTurtleCursor write FTurtleCursor;
    property TextOutput: TTextOutput read FTextOutput write FTextOutput;
    property OnLogCall: TOnLogCallEvent read FOnLogCall write FOnLogCall;
  end;

var
  DataModuleShared: TDataModuleShared;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Math, dwsXPlatform, GR32_Math, GR32_PortableNetworkGraphic;

{ TTurtleCanvas }

constructor TTurtleCanvas.Create(ImageCanvas: IImageCanvas);
begin
  FImageCanvas := ImageCanvas;
  FColor := 0;
end;

procedure TTurtleCanvas.DrawLine(AX, AY, BX, BY: Integer; Color: TColor32);
begin
  FImageCanvas.DrawLine(System.Types.Point(AX, AY), System.Types.Point(BX, BY),
    Color);
end;

procedure TTurtleCanvas.DrawLine(A, B: TPoint; Color: TColor32);
begin
  FImageCanvas.DrawLine(A, B, Color);
end;

procedure TTurtleCanvas.DrawLineF(A, B: TPointF; Color: TColor32);
begin
  FImageCanvas.DrawLineF(A, B, Color);
end;

function TTurtleCanvas.GetHeight: Integer;
begin
  Result := FImageCanvas.Height;
end;

function TTurtleCanvas.GetPixel(X, Y: Integer): TColor32;
begin
  Result := FImageCanvas.PixelColor[X, Y];
end;

function TTurtleCanvas.GetWidth: Integer;
begin
  Result := FImageCanvas.Width;
end;

procedure TTurtleCanvas.Invalidate;
begin
  FImageCanvas.Invalidate;
  if Assigned(FOnInvalidate) then
    FOnInvalidate(Self);
end;

procedure TTurtleCanvas.SaveToFile(FileName: TFileName);
begin
  FImageCanvas.SaveToFile(FileName);
end;

procedure TTurtleCanvas.SetPixel(X, Y: Integer; Color: TColor32);
begin
  FImageCanvas.PixelColor[X, Y] := Color;
end;

procedure TTurtleCanvas.Clear;
begin
  FImageCanvas.Clear(FColor);
end;

procedure TTurtleCanvas.Clear(Color: TColor32);
begin
  FColor := Color;
  FImageCanvas.Clear(Color);
end;


{ TTurtleCursor }

constructor TTurtleCursor.Create(TurtleCanvas: TTurtleCanvas);
begin
  inherited Create;

  FCanvas := TurtleCanvas;

  FColor := clBlack32;
  FAngle := 0;
  FAntiAliased := True;
  FVisible := True;

  FPositionStack := TStack<TPointF>.Create;
end;

destructor TTurtleCursor.Destroy;
begin
  FreeAndNil(FPositionStack);

  inherited;
end;

procedure TTurtleCursor.Go(Distance: Single = 10);
var
  c, s: Single;
begin
  SinCos(FAngle, Distance, s, c);
  Position := PointF(FPosition.X + c, FPosition.Y + s);
end;

procedure TTurtleCursor.Center(TruncToInteger: Boolean = False);
begin
  if TruncToInteger then
    Position := PointF(FCanvas.Width div 2, FCanvas.Height div 2)
  else
    Position := PointF(0.5 * FCanvas.Width, 0.5 * FCanvas.Height);
end;

procedure TTurtleCursor.Home(TruncToInteger: Boolean = False);
begin
  Center(TruncToInteger);
  FAngle := 0;
end;

procedure TTurtleCursor.Draw(Distance: Single);
var
  c, s: Single;
  NewPoint: TPointF;
begin
  SinCos(FAngle, Distance, s, c);
  NewPoint := PointF(FPosition.X + c, FPosition.Y + s);

  DrawLine(Position, NewPoint);

  Position := NewPoint;
end;

procedure TTurtleCursor.DrawLine(A, B: TPointF);
begin
  if AntiAliased then
    FCanvas.DrawLineF(A, B, Color)
  else
    FCanvas.DrawLine(Round(A.X), Round(A.Y), Round(B.X), Round(B.Y), Color);
end;

procedure TTurtleCursor.LineTo(X, Y: Single);
var
  NewPoint: TPointF;
begin
  NewPoint := PointF(X, Y);

  DrawLine(Position, NewPoint);

  FAngle := ArcTan2(Y - Position.Y, X - Position.X);
  Position := NewPoint;
end;

procedure TTurtleCursor.LookAt(X, Y: Single);
begin
  FAngle := ArcTan2(Y - FPosition.Y, X - FPosition.X);
end;

procedure TTurtleCursor.MoveTo(X, Y: Single);
begin
  FAngle := ArcTan2(Y - Position.Y, X - Position.X);
  Position := PointF(X, Y);
end;

procedure TTurtleCursor.PopPosition;
begin
  Position := FPositionStack.Pop;
end;

procedure TTurtleCursor.PushPosition;
begin
  FPositionStack.Push(FPosition);
end;

procedure TTurtleCursor.SetPosition(const Value: TPointF);
begin
  if (FPosition.X <> Value.X) or (FPosition.Y <> Value.Y) then
  begin
    FPosition := Value;

    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);
  end;
end;

procedure TTurtleCursor.Turn(NewAngleInDegree: Single);
begin
  FAngle := FloatMod(FAngle - DegToRad(NewAngleInDegree), 2 * Pi);
end;


{ TTextOutput }

constructor TTextOutput.Create(StringList: TStringList);
begin
  FStringList := StringList;
end;

procedure TTextOutput.Clear;
begin
  FStringList.Clear;
end;

function TTextOutput.GetText: string;
begin
  Result := FStringList.Text;
end;

procedure TTextOutput.SetText(const Value: string);
begin
  FStringList.Text := Value;
end;


{ TDataModuleShared }

procedure TDataModuleShared.dwsFunctionsCenterEval(info: TProgramInfo);
begin
  FTurtleCursor.Center;

  LogCall(lcCenter);
end;

procedure TDataModuleShared.dwsFunctionsClearEval(info: TProgramInfo);
begin
  // clear text output
//TODO  ListBoxOutput.Items.Clear;

  // clear bitmap
  if info.FuncSym.Params.Count = 1 then
    FTurtleCanvas.Clear(Info.ParamAsInteger[0])
  else
    FTurtleCanvas.Clear;

  LogCall(lcClear);
end;

procedure TDataModuleShared.dwsFunctionsComposeColorEval(
  info: TProgramInfo);
begin
  if info.FuncSym.Params.Count = 3 then
    info.ResultAsInteger := Color32(
      Round(255 * Info.ParamAsFloat[0]),
      Round(255 * Info.ParamAsFloat[1]),
      Round(255 * Info.ParamAsFloat[2]))
  else
    info.ResultAsInteger := Color32(
      Round(255 * Info.ParamAsFloat[0]),
      Round(255 * Info.ParamAsFloat[1]),
      Round(255 * Info.ParamAsFloat[2]),
      Round(255 * Info.ParamAsFloat[3]));

  LogCall(lcComposeColor);
end;

procedure TDataModuleShared.dwsFunctionsComposeColorHSLEval(
  info: TProgramInfo);
begin
  if info.FuncSym.Params.Count = 3 then
    info.ResultAsInteger := HSLtoRGB(
      Info.ParamAsFloat[0],
      Info.ParamAsFloat[1],
      Info.ParamAsFloat[2])
  else
    info.ResultAsInteger := SetAlpha(HSLtoRGB(
      Info.ParamAsFloat[0],
      Info.ParamAsFloat[1],
      Info.ParamAsFloat[2]),
      Round(255 * Info.ParamAsFloat[3]));

  LogCall(lcComposeColorHSL);
end;

procedure TDataModuleShared.dwsFunctionsDelayEval(info: TProgramInfo);
var
  DelayTime: Integer;
begin
  DelayTime := info.ParamAsInteger[0];
  if DelayTime > 0 then
    Sleep(info.ParamAsInteger[0]);

  FTurtleCanvas.Invalidate;

// TODO  ListBoxOutput.Items.Text := FExecutionThread.Output;

  LogCall(lcDelay);
end;

procedure TDataModuleShared.dwsFunctionsDrawEval(info: TProgramInfo);
begin
  FTurtleCursor.Draw(info.ParamAsFloat[0]);
  LogCall(lcDraw);
end;

procedure TDataModuleShared.dwsFunctionsGetPixelColorEval(
  info: TProgramInfo);
begin
  info.ResultAsInteger := FTurtleCanvas.Pixel[
    info.ParamAsInteger[0], info.ParamAsInteger[1]];
end;

procedure TDataModuleShared.dwsFunctionsGoEval(info: TProgramInfo);
begin
  FTurtleCursor.Go(info.ParamAsFloat[0]);
  LogCall(lcGo);
end;

procedure TDataModuleShared.dwsFunctionsHomeEval(info: TProgramInfo);
begin
  FTurtleCursor.Home;
  LogCall(lcHome);
end;

procedure TDataModuleShared.dwsFunctionsLineToEval(info: TProgramInfo);
begin
  FTurtleCursor.LineTo(info.ValueAsFloat['X'], info.ValueAsFloat['Y']);
  LogCall(lcLineTo);
end;

procedure TDataModuleShared.dwsFunctionsLookAtEval(info: TProgramInfo);
begin
  FTurtleCursor.LookAt(Info.ValueAsFloat['X'], Info.ValueAsFloat['Y']);
  LogCall(lcLookAt);
end;

procedure TDataModuleShared.dwsFunctionsMoveToEval(info: TProgramInfo);
begin
  FTurtleCursor.MoveTo(info.ValueAsFloat['X'], info.ValueAsFloat['Y']);
  LogCall(lcMoveTo);
end;

procedure TDataModuleShared.dwsFunctionsPopPositionEval(info: TProgramInfo);
begin
  FTurtleCursor.PopPosition;
  LogCall(lcPopPosition);
end;

procedure TDataModuleShared.dwsFunctionsPushPositionEval(
  info: TProgramInfo);
begin
  FTurtleCursor.PushPosition;
  LogCall(lcPushPosition);
end;

procedure TDataModuleShared.dwsFunctionsSaveToFileEval(info: TProgramInfo);
begin
  FTurtleCanvas.SaveToFile(info.ParamAsString[0]);
  LogCall(lcSaveToFile);
end;

procedure TDataModuleShared.dwsFunctionsSetPixelColorEval(
  info: TProgramInfo);
begin
  FTurtleCanvas.Pixel[info.ParamAsInteger[0],
    info.ParamAsInteger[1]] := info.ParamAsInteger[2];

  LogCall(lcSetPixelColor);
end;

procedure TDataModuleShared.dwsFunctionsTurnLeftEval(info: TProgramInfo);
begin
  FTurtleCursor.Turn(info.ParamAsFloat[0]);
  LogCall(lcTurnLeft);
end;

procedure TDataModuleShared.dwsFunctionsTurnRightEval(info: TProgramInfo);
begin
  FTurtleCursor.Turn(-info.ParamAsFloat[0]);
  LogCall(lcTurnRight);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsDrawEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Draw(info.ParamAsFloat[0]);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsSetAngleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Angle := info.ParamAsFloat[0];
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsLookAtEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).LookAt(info.ParamAsFloat[0], info.ParamAsFloat[1]);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsPushPositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).PushPosition;
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsPopPositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).PopPosition;
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsSetVisibleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Visible := info.ParamAsBoolean[0];
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsSetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Color := info.ParamAsInteger[0];
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsTurnLeftEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Turn(info.ParamAsInteger[0]);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsTurnRightEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Turn(-info.ParamAsInteger[0]);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsGoEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Go(info.ParamAsFloat[0]);
end;

procedure TDataModuleShared.DelphiWebScriptInclude(const scriptName: string;
  var scriptSource: string);
var
  FileName: TFileName;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'Library\' +  scriptName;

  if FileExists(FileName) then
    scriptSource := LoadTextFromFile(FileName);
end;

function TDataModuleShared.DelphiWebScriptNeedUnit(const unitName: string;
  var unitSource: string): IdwsUnit;
var
  UnitFileName: TFileName;
begin
  UnitFileName := ExtractFilePath(ParamStr(0)) + 'Library\' +  unitName + '.pas';

  if FileExists(UnitFileName) then
    unitSource := LoadTextFromFile(UnitFileName);
end;

procedure TDataModuleShared.dwsClassesTCanvasMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).Clear;
end;

procedure TDataModuleShared.dwsClassesTCanvasSaveToFileEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).SaveToFile(info.ParamAsString[0]);
  LogCall(lcSaveToFile, True);
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsGetAngleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsFloat := TTurtleCursor(ExtObject).Angle;
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsGetVisibleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsBoolean := TTurtleCursor(ExtObject).Visible;
end;

procedure TDataModuleShared.dwsClassesTCursorMethodsGetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsInteger := TTurtleCursor(ExtObject).Color;
end;

procedure TDataModuleShared.dwsInstanceCanvasInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := FTurtleCanvas;
end;

procedure TDataModuleShared.dwsInstanceCursorInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := FTurtleCursor;
end;

procedure TDataModuleShared.dwsFunctionsCosineEval(info: TProgramInfo);
begin
  info.ResultAsFloat := Cos(DegToRad(info.ParamAsFloat[0]));
end;

procedure TDataModuleShared.dwsFunctionsSineEval(info: TProgramInfo);
begin
  info.ResultAsFloat := Sin(DegToRad(info.ParamAsFloat[0]));
end;

procedure TDataModuleShared.dwsFunctionsTangentEval(info: TProgramInfo);
begin
  info.ResultAsFloat := Tan(DegToRad(info.ParamAsFloat[0]));
end;

procedure TDataModuleShared.dwsClassesTCanvasMethodsGetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  info.ResultAsInteger := TTurtleCanvas(ExtObject).Color;
end;

procedure TDataModuleShared.dwsClassesTCanvasMethodsSetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).Color := info.ParamAsInteger[0];
end;

procedure TDataModuleShared.dwsClassesTCanvasMethodsSetPixelColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).SetPixel(info.ParamAsInteger[0],
    info.ParamAsInteger[1], info.ParamAsInteger[2]);
end;

procedure TDataModuleShared.dwsClassesTCanvasMethodsGetPixelColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  info.ResultAsInteger := TTurtleCanvas(ExtObject).GetPixel(
    info.ParamAsInteger[0], info.ParamAsInteger[1]);
end;

procedure TDataModuleShared.dwsVariablesAntiAliasedLineReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.AntiAliased;
end;

procedure TDataModuleShared.dwsVariablesAntiAliasedLineWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  if FTurtleCursor.AntiAliased <> Value then
  begin
    FTurtleCursor.AntiAliased := Value;
    LogCall(lcAntialiased);
  end;
end;

procedure TDataModuleShared.dwsVariablesCanvasColorReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCanvas.Color;
end;

procedure TDataModuleShared.dwsVariablesCanvasColorWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCanvas.Color := Value;
end;

procedure TDataModuleShared.dwsVariablesClientHeightReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := Int64(FTurtleCanvas.Height);
end;

procedure TDataModuleShared.dwsVariablesClientWidthReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := Int64(FTurtleCanvas.Width);
end;

procedure TDataModuleShared.dwsVariablesCursorAngleReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := RadToDeg(-FTurtleCursor.Angle);
end;

procedure TDataModuleShared.dwsVariablesCursorAngleWriteVar(
  info: TProgramInfo; const value: Variant);
var
  NewAngle: Single;
begin
  NewAngle := FloatMod(DegToRad(-Value), 2 * Pi);
  if NewAngle <> FTurtleCursor.Angle then
  begin
    FTurtleCursor.Angle := NewAngle;
    LogCall(lcAngleChange);
  end;
end;

procedure TDataModuleShared.dwsVariablesCursorColorReadVar(
  info: TProgramInfo; var value: Variant);
begin
  value := FTurtleCursor.Color;
end;

procedure TDataModuleShared.dwsVariablesCursorColorWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  if FTurtleCursor.Color <> Value then
  begin
    FTurtleCursor.Color := Value;
    LogCall(lcColorChange);
  end;
end;

procedure TDataModuleShared.dwsVariablesCursorPositionXReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.Position.X;
end;

procedure TDataModuleShared.dwsVariablesCursorPositionXWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCursor.Position := PointF(Value, FTurtleCursor.Position.Y);
end;

procedure TDataModuleShared.dwsVariablesCursorPositionYReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.Position.Y;
end;

procedure TDataModuleShared.dwsVariablesCursorPositionYWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCursor.Position := PointF(FTurtleCursor.Position.X, Value);
end;

procedure TDataModuleShared.dwsVariablesCursorReadVar(info: TProgramInfo;
  var value: Variant);
begin
  value := FTurtleCursor.Visible;
end;

procedure TDataModuleShared.dwsVariablesCursorWriteVar(info: TProgramInfo;
  const value: Variant);
begin
  FTurtleCursor.Visible := value;
end;

procedure TDataModuleShared.LogCall(CallType: TLogCall;
  ClassAccess: Boolean = False);
begin
  if Assigned(FOnLogCall) then
    FOnLogCall(Self, CallType, ClassAccess);
end;

end.
