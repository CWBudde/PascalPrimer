unit PascalPrimer.Shared;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.Variants, System.UITypes,

  (* GR32 *)
  GR32,

  (* DWS *)
  dwsComp, dwsExprs, dwsSymbols, dwsErrors, dwsSuggestions, dwsVCLGUIFunctions,
  {$IFNDEF WIN64} dwsJIT, dwsJITx86, {$ENDIF} dwsStrings, dwsUnitSymbols,
  dwsFunctions, dwsTokenizer;

type
  IOutputGraphics = interface
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

  IOutputText = interface
    function GetText: string;
    procedure SetText(const Value: string);

    procedure AddString(Text: string);
    procedure AddLine(Text: string);
    procedure Clear;

    property Text: string read GetText write SetText;
  end;

  IInput = interface
    function ReadKey(LimitToLastKey: Boolean): string;
    function ReadMouseButton: Boolean; overload;
    function ReadMouseButton(MouseButton: TMouseButton): Boolean; overload;
    function GetMousePosition(LimitToBounds: Boolean): TPoint;
  end;

  TTurtleCanvas = class
  private
    FOutputGraphics: IOutputGraphics;
    FColor: TColor32;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixel(X, Y: Integer): TColor32; inline;
    procedure SetPixel(X, Y: Integer; Color: TColor32); inline;
  public
    constructor Create(OutputGraphics: IOutputGraphics);

    procedure Clear; overload;
    procedure Clear(Color: TColor32); overload;

    procedure DrawLine(AX, AY, BX, BY: Integer; Color: TColor32); overload;
    procedure DrawLine(A, B: TPoint; Color: TColor32); overload;
    procedure DrawLineF(A, B: TPointF; Color: TColor32);

    procedure SaveToFile(FileName: TFileName);

    procedure Invalidate;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Color: TColor32 read FColor write FColor;
    property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel;
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
    FOutputText: IOutputText;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create(OutputText: IOutputText);

    procedure AddString(Text: string);
    procedure AddLine(Text: string);
    procedure Clear;

    property Text: string read GetText write SetText;
    property OutputText: IOutputText read FOutputText;
  end;

  TTurtleInput = class
  private
    FInput: IInput;
  public
    constructor Create(Input: IInput);

    function ReadKey(LimitToLastKey: Boolean = True): string;
    function ReadMouseButton: Boolean; overload;
    function ReadMouseButton(MouseButton: TMouseButton): Boolean; overload;
    function GetMousePosition(LimitToBounds: Boolean = True): TPoint;
  end;

  TLogCall = (lcExecution, lcClear, lcCenter, lcComposeColor,
    lcComposeColorHSL, lcDelay, lcHome, lcLineTo, lcLookAt, lcMoveTo, lcGo,
    lcDraw, lcSaveToFile, lcTurnLeft, lcTurnRight, lcColorChange,
    lcAngleChange, lcAntialiased, lcPopPosition, lcPushPosition,
    lcSetPixelColor, lcGetPixelColor);

  TDataModuleShared = class(TDataModule)
    DelphiWebScript: TDelphiWebScript;
    dwsUnitText: TdwsUnit;
    dwsUnitIntermediate: TdwsUnit;
    dwsUnitAdvanced: TdwsUnit;
    dwsUnitBasic: TdwsUnit;
    dwsUnitInput: TdwsUnit;

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
    procedure dwsClassesTTextOutputMethodsClearEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTTextOutputMethodsGetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTTextOutputMethodsSetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTTextOutputMethodsWriteEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsClassesTTextOutputMethodsWriteLineEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsFunctionsCenterEval(info: TProgramInfo);
    procedure dwsFunctionsClearEval(info: TProgramInfo);
    procedure dwsFunctionsComposeColorEval(info: TProgramInfo);
    procedure dwsFunctionsComposeColorHSLEval(info: TProgramInfo);
    procedure dwsFunctionsCosineEval(info: TProgramInfo);
    procedure dwsFunctionsDelayEval(info: TProgramInfo);
    procedure dwsFunctionsDrawEval(info: TProgramInfo);
    procedure dwsFunctionsGetMousePositionEval(info: TProgramInfo);
    procedure dwsFunctionsGetMousePositionXEval(info: TProgramInfo);
    procedure dwsFunctionsGetMousePositionYEval(info: TProgramInfo);
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
    procedure dwsFunctionsWriteEval(info: TProgramInfo);
    procedure dwsFunctionsWriteLineEval(info: TProgramInfo);
    procedure dwsInstanceCanvasInstantiate(info: TProgramInfo; var ExtObject: TObject);
    procedure dwsInstanceCursorInstantiate(info: TProgramInfo; var ExtObject: TObject);
    procedure dwsInstancesTextOutputInstantiate(info: TProgramInfo; var ExtObject: TObject);
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
    procedure dwsVariablesCursorVisibleReadVar(info: TProgramInfo; var value: Variant);
    procedure dwsVariablesCursorVisibleWriteVar(info: TProgramInfo; const value: Variant);
    procedure DelphiWebScriptInclude(const scriptName: string; var scriptSource: string);
    function DelphiWebScriptNeedUnit(const unitName: string; var unitSource: string): IdwsUnit;
    procedure dwsFunctionsReadKeyEval(info: TProgramInfo);
    procedure dwsFunctionsReadMouseButtonEval(info: TProgramInfo);
  type
    TOnLogCallEvent = procedure(Sender: TObject; CallType: TLogCall;
      ClassAccess: Boolean = False) of object;
  private
    FTurtleCanvas: TTurtleCanvas;
    FTurtleCursor: TTurtleCursor;
    FTurtleInput: TTurtleInput;
    FTextOutput: TTextOutput;
    FOnLogCall: TOnLogCallEvent;
    FOutputGraphics: IOutputGraphics;
    FOutputText: IOutputText;
    FInput: IInput;
    procedure LogCall(CallType: TLogCall; ClassAccess: Boolean = False);
    procedure SetOutputGraphics(const Value: IOutputGraphics);
    procedure SetOutputText(const Value: IOutputText);
    procedure SetInput(const Value: IInput);
  public
    property OutputGraphics: IOutputGraphics read FOutputGraphics write SetOutputGraphics;
    property OutputText: IOutputText read FOutputText write SetOutputText;
    property Input: IInput read FInput write SetInput;

    property TurtleCanvas: TTurtleCanvas read FTurtleCanvas;
    property TurtleCursor: TTurtleCursor read FTurtleCursor;
    property TextOutput: TTextOutput read FTextOutput;

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

constructor TTurtleCanvas.Create(OutputGraphics: IOutputGraphics);
begin
  FOutputGraphics := OutputGraphics;
  FColor := 0;
end;

procedure TTurtleCanvas.DrawLine(AX, AY, BX, BY: Integer; Color: TColor32);
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.DrawLine(System.Types.Point(AX, AY),
      System.Types.Point(BX, BY), Color);
end;

procedure TTurtleCanvas.DrawLine(A, B: TPoint; Color: TColor32);
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.DrawLine(A, B, Color);
end;

procedure TTurtleCanvas.DrawLineF(A, B: TPointF; Color: TColor32);
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.DrawLineF(A, B, Color);
end;

function TTurtleCanvas.GetHeight: Integer;
begin
  Result := 0;
  if Assigned(FOutputGraphics) then
    Result := FOutputGraphics.Height;
end;

function TTurtleCanvas.GetPixel(X, Y: Integer): TColor32;
begin
  Result := 0;
  if Assigned(FOutputGraphics) then
    Result := FOutputGraphics.PixelColor[X, Y];
end;

function TTurtleCanvas.GetWidth: Integer;
begin
  Result := 0;
  if Assigned(FOutputGraphics) then
    Result := FOutputGraphics.Width;
end;

procedure TTurtleCanvas.Invalidate;
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.Invalidate;
end;

procedure TTurtleCanvas.SaveToFile(FileName: TFileName);
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.SaveToFile(FileName);
end;

procedure TTurtleCanvas.SetPixel(X, Y: Integer; Color: TColor32);
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.PixelColor[X, Y] := Color;
end;

procedure TTurtleCanvas.Clear;
begin
  if Assigned(FOutputGraphics) then
    FOutputGraphics.Clear(FColor);
end;

procedure TTurtleCanvas.Clear(Color: TColor32);
begin
  FColor := Color;
  if Assigned(FOutputGraphics) then
    FOutputGraphics.Clear(Color);
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

constructor TTextOutput.Create(OutputText: IOutputText);
begin
  FOutputText := OutputText;
end;

procedure TTextOutput.AddLine(Text: string);
begin
  FOutputText.AddLine(Text);
end;

procedure TTextOutput.AddString(Text: string);
begin
  FOutputText.AddString(Text);
end;

procedure TTextOutput.Clear;
begin
  FOutputText.Clear;
end;

function TTextOutput.GetText: string;
begin
  Result := FOutputText.Text;
end;

procedure TTextOutput.SetText(const Value: string);
begin
  FOutputText.Text := Value;
end;


{ TTurtleInput }

constructor TTurtleInput.Create(Input: IInput);
begin
  FInput := Input;
end;

function TTurtleInput.GetMousePosition(LimitToBounds: Boolean = True): TPoint;
begin
  Result := FInput.GetMousePosition(LimitToBounds);
end;

function TTurtleInput.ReadKey(LimitToLastKey: Boolean = True): string;
begin
  Result := FInput.ReadKey(LimitToLastKey);
end;

function TTurtleInput.ReadMouseButton: Boolean;
begin
  Result := FInput.ReadMouseButton;
end;

function TTurtleInput.ReadMouseButton(MouseButton: TMouseButton): Boolean;
begin
  Result := FInput.ReadMouseButton(MouseButton);
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
  FTextOutput.Clear;

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

procedure TDataModuleShared.dwsClassesTTextOutputMethodsWriteEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTextOutput);
  FTextOutput.AddString(Info.ParamAsString[0]);
end;

procedure TDataModuleShared.dwsClassesTTextOutputMethodsWriteLineEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTextOutput);
  FTextOutput.AddString(Info.ParamAsString[0]);
end;

procedure TDataModuleShared.dwsInstancesTextOutputInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := FTextOutput;
end;

procedure TDataModuleShared.dwsFunctionsReadKeyEval(
  info: TProgramInfo);
begin
  info.ResultAsString := FTurtleInput.ReadKey;
end;

procedure TDataModuleShared.dwsFunctionsReadMouseButtonEval(
  info: TProgramInfo);
begin
  if info.FuncSym.Params.Count = 0 then
    info.ResultAsBoolean := FTurtleInput.ReadMouseButton
  else
    info.ResultAsBoolean := FTurtleInput.ReadMouseButton(TMouseButton(
      info.ParamAsInteger[0]));
end;

procedure TDataModuleShared.dwsClassesTTextOutputMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTextOutput);
  FTextOutput.Clear;
end;

procedure TDataModuleShared.dwsClassesTTextOutputMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTextOutput);
  Info.ResultAsString := FTextOutput.Text;
end;

procedure TDataModuleShared.dwsClassesTTextOutputMethodsSetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTextOutput);
  FTextOutput.Text := Info.ParamAsString[0];
end;

procedure TDataModuleShared.dwsFunctionsGetMousePositionEval(
  info: TProgramInfo);
var
  Pos: TPoint;
begin
  Pos := Input.GetMousePosition(info.ParamAsBoolean[0]);
  info.ResultVars.Member['X'].ValueAsInteger := Pos.X;
  info.ResultVars.Member['Y'].ValueAsInteger := Pos.Y;
end;

procedure TDataModuleShared.dwsFunctionsGetMousePositionXEval(
  info: TProgramInfo);
begin
  info.ResultAsInteger := Input.GetMousePosition(info.ParamAsBoolean[0]).X;
end;

procedure TDataModuleShared.dwsFunctionsGetMousePositionYEval(
  info: TProgramInfo);
begin
  info.ResultAsInteger := Input.GetMousePosition(info.ParamAsBoolean[0]).Y;
end;

procedure TDataModuleShared.dwsFunctionsWriteEval(info: TProgramInfo);
begin
  FTextOutput.AddString(info.ParamAsString[0]);
end;

procedure TDataModuleShared.dwsFunctionsWriteLineEval(info: TProgramInfo);
begin
  FTextOutput.AddLine(info.ParamAsString[0]);
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

procedure TDataModuleShared.dwsVariablesCursorVisibleReadVar(info: TProgramInfo;
  var value: Variant);
begin
  value := FTurtleCursor.Visible;
end;

procedure TDataModuleShared.dwsVariablesCursorVisibleWriteVar(info: TProgramInfo;
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

procedure TDataModuleShared.SetInput(const Value: IInput);
begin
  if Value <> Input then
  begin
    FTurtleInput.Free;

    if Assigned(Value) then
      FTurtleInput := TTurtleInput.Create(Value);

    FInput := Value;
  end;
end;

procedure TDataModuleShared.SetOutputGraphics(const Value: IOutputGraphics);
begin
  if Value <> OutputGraphics then
  begin
    FTurtleCanvas.Free;
    FTurtleCursor.Free;

    if Assigned(Value) then
    begin
      FTurtleCanvas := TTurtleCanvas.Create(Value);
      FTurtleCursor := TTurtleCursor.Create(FTurtleCanvas);
    end;

    FOutputGraphics := Value;
  end;
end;

procedure TDataModuleShared.SetOutputText(const Value: IOutputText);
begin
  if Value <> OutputText then
  begin
    FTextOutput.Free;
    if Assigned(Value) then
      FTextOutput := TTextOutput.Create(Value);

    FOutputText := Value;
  end;
end;

end.
