unit PascalPrimer.StandaloneFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs,

  (* DWS *)
  dwsComp, dwsExprs, dwsSymbols, dwsErrors, dwsSuggestions,
  dwsStrings, dwsUnitSymbols, dwsFunctions, dwsTokenizer,
  {$IFNDEF WIN64} dwsJIT, dwsJITx86, {$ENDIF}

  (* Custom *)
  PascalPrimer.Shared, FMX.Objects;

type
  TOutputImage = class(TInterfacedObject, IOutputGraphics)
  private
    FImage: TImage;
    function GetHeight: Integer;
    function GetPixelColor(X, Y: Integer): TColor;
    function GetWidth: Integer;
    procedure SetPixelColor(X, Y: Integer; Value: TColor);
  public
    constructor Create(Image: TImage);

    procedure Clear(Color: TColor);
    function ComposeColor(R, G, B, A: Byte): TColor;
    procedure Invalidate(WaitForRefresh: Boolean);
    procedure SaveToFile(FileName: TFileName);

    procedure DrawRectangle(Left, Top, Right, Bottom: Float; Color: TColor);
    procedure DrawCircle(Center: TPointF; Radius: Double; Color: TColor);
    procedure DrawLine(A, B: TPoint; Color: TColor);
    procedure DrawLineF(A, B: TPointF; Color: TColor; StrokeWidth: Float);

    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property PixelColor[X, Y: Integer]: TColor read GetPixelColor write SetPixelColor;
  end;

  TOutputStrings = class(TInterfacedObject, IOutputText)
  private
    FStrings: TStrings;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create(Strings: TStrings);

    procedure AddLine(Text: string);
    procedure AddString(Text: string);
    procedure Clear;

    property Text: string read GetText write SetText;
  end;

  TScriptExecutionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFormStandalone = class(TForm, IInput)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ImageResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FKeysPressedHistory: string;
    FMousePos: TPoint;
    FMouseButton: array [TMouseButton] of Boolean;
    function ReadKey(LimitToLastKey: Boolean): string;
    function ReadMouseButton: Boolean; overload;
    function ReadMouseButton(MouseButton: TMouseButton): Boolean; overload;
    function GetMousePosition(LimitToBounds: Boolean): TPoint;
  private
    FProgramExecution: IdwsProgramExecution;
    FCompiledProgram: IdwsProgram;
    FScriptExecutionThread: TScriptExecutionThread;

    FSourceCode: string;
    procedure SetupScriptExecutionThread;
    procedure KillScriptExecutionThread;
    procedure ScriptExecutionThreadTerminated(Sender: TObject);
  public
    procedure CompileScript;
    procedure RunScript;
  end;

var
  FormStandalone: TFormStandalone;

implementation

{$R *.fmx}

uses
  dwsXPlatform;

{ TOutputImage }

constructor TOutputImage.Create(Image: TImage);
begin
  FImage := Image;
end;

procedure TOutputImage.Clear(Color: TColor);
begin
  FImage.Bitmap.Clear(Color);
end;

function TOutputImage.ComposeColor(R, G, B, A: Byte): TColor;
var
  AlphaColor: TAlphaColorRec;
begin
  AlphaColor.A := A;
  AlphaColor.R := R;
  AlphaColor.G := G;
  AlphaColor.B := B;

  Result := AlphaColor.Color;
end;

procedure TOutputImage.DrawCircle(Center: TPointF; Radius: Double;
  Color: TColor);
begin
  FImage.Bitmap.Canvas.Stroke.Color := Color;
  FImage.Bitmap.Canvas.DrawEllipse(RectF(Center.X - Radius, Center.Y - Radius,
    Center.X + Radius, Center.Y + Radius), 1);
end;

procedure TOutputImage.DrawLine(A, B: TPoint; Color: TColor);
begin
  FImage.Bitmap.Canvas.Stroke.Color := Color;
  FImage.Bitmap.Canvas.DrawLine(A, B, 1);
end;

procedure TOutputImage.DrawLineF(A, B: TPointF; Color: TColor; StrokeWidth: Float);
begin
  FImage.Bitmap.Canvas.Stroke.Color := Color;
  FImage.Bitmap.Canvas.Stroke.Thickness := StrokeWidth;
  FImage.Bitmap.Canvas.DrawLine(A, B, 1);
end;

procedure TOutputImage.DrawRectangle(Left, Top, Right, Bottom: Float;
  Color: TColor);
begin
  FImage.Bitmap.Canvas.DrawRect(RectF(Left, Top, Right, Bottom), 0, 0, [], 1);
end;

function TOutputImage.GetHeight: Integer;
begin
  Result := FImage.Bitmap.Height;
end;

function TOutputImage.GetPixelColor(X, Y: Integer): TColor;
var
  BitmapData: TBitmapData;
begin
  if FImage.Bitmap.Map(TMapAccess.Read, BitmapData) then
  try
    Result := BitmapData.GetPixel(X, Y);
  finally
    FImage.Bitmap.Unmap(BitmapData);
  end;
end;

function TOutputImage.GetWidth: Integer;
begin
  Result := FImage.Bitmap.Width;
end;

procedure TOutputImage.Invalidate(WaitForRefresh: Boolean);
begin
  FImage.InvalidateRect(FImage.BoundsRect);
end;

procedure TOutputImage.SaveToFile(FileName: TFileName);
begin
  FImage.Bitmap.SaveToFile(FileName);
end;

procedure TOutputImage.SetPixelColor(X, Y: Integer; Value: TColor);
var
  BitmapData: TBitmapData;
begin
  if FImage.Bitmap.Map(TMapAccess.Write, BitmapData) then
  try
    BitmapData.SetPixel(X, Y, Value);
  finally
    FImage.Bitmap.Unmap(BitmapData);
  end;
end;


{ TOutputStrings }

constructor TOutputStrings.Create(Strings: TStrings);
begin
  FStrings := Strings;
end;

procedure TOutputStrings.AddLine(Text: string);
begin
  if Assigned(FStrings) then
    FStrings.Add(Text);
end;

procedure TOutputStrings.AddString(Text: string);
begin
  if Assigned(FStrings) then
    FStrings.Add(Text);
end;

procedure TOutputStrings.Clear;
begin
  if Assigned(FStrings) then
    FStrings.Clear;
end;

function TOutputStrings.GetText: string;
begin
  Result := '';
  if Assigned(FStrings) then
    Result := FStrings.Text;
end;

procedure TOutputStrings.SetText(const Value: string);
begin
  if Assigned(FStrings) then
    FStrings.Text := Value;
end;


{ TScriptExecutionThread }

procedure TScriptExecutionThread.Execute;
begin
  inherited;

  FormStandalone.RunScript;
end;


{ TFormStandalone }

procedure TFormStandalone.SetupScriptExecutionThread;
begin
  FScriptExecutionThread := TScriptExecutionThread.Create(True);
  FScriptExecutionThread.OnTerminate := ScriptExecutionThreadTerminated;
  FScriptExecutionThread.FreeOnTerminate := True;
  FScriptExecutionThread.Start;
end;

procedure TFormStandalone.FormCreate(Sender: TObject);
begin
  DataModuleShared.OutputGraphics := TOutputImage.Create(Image);
  DataModuleShared.OutputText := TOutputStrings.Create(nil);
  DataModuleShared.Input := Self;

  Image.Bitmap.SetSize(Round(Image.Width), Round(Image.Height));

  // load source code
  if FileExists(ChangeFileExt(ParamStr(0), '.pas')) then
    FSourceCode := LoadTextFromFile(ChangeFileExt(ParamStr(0), '.pas'))
  else
    Application.Terminate;

  CompileScript;
end;

procedure TFormStandalone.FormDestroy(Sender: TObject);
begin
  KillScriptExecutionThread;

  // release interfaces
  DataModuleShared.OutputGraphics := nil;
  DataModuleShared.OutputText := nil;
  DataModuleShared.Input := nil;
end;

procedure TFormStandalone.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FMouseButton[Button] := True;
end;

procedure TFormStandalone.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FMousePos.X := Round(X);
  FMousePos.Y := Round(Y);
end;

procedure TFormStandalone.FormShow(Sender: TObject);
begin
  SetupScriptExecutionThread;
end;

function TFormStandalone.GetMousePosition(LimitToBounds: Boolean): TPoint;
begin
  Result := FMousePos;
end;

procedure TFormStandalone.ImageResize(Sender: TObject);
begin
  KillScriptExecutionThread;
  SetupScriptExecutionThread;
  Image.Bitmap.SetSize(Round(Image.Width), Round(Image.Height));
end;

procedure TFormStandalone.KillScriptExecutionThread;
var
  Thread: TThread;
begin
  if Assigned(FScriptExecutionThread) then
  begin
    FProgramExecution.Stop;
    Thread := FScriptExecutionThread;
    Thread.Terminate;
//    Thread.WaitFor;
  end;
end;

procedure TFormStandalone.ScriptExecutionThreadTerminated(Sender: TObject);
begin
  FScriptExecutionThread := nil;
end;

procedure TFormStandalone.CompileScript;
begin
  // compile program
  FCompiledProgram := DataModuleShared.DelphiWebScript.Compile(FSourceCode);

  if FCompiledProgram.Msgs.HasErrors then
  begin
    OutputDebugString(FCompiledProgram.Msgs.AsInfo);
    Application.Terminate;
  end;

  {$IFNDEF WIN64}
  with TdwsJITx86.Create do
  try
    Options := Options - [jitoNoBranchAlignment];
    GreedyJIT(FCompiledProgram.ProgramObject);
  finally
    Free;
  end;
  {$ENDIF}
end;

function TFormStandalone.ReadKey(LimitToLastKey: Boolean): string;
begin

end;

function TFormStandalone.ReadMouseButton: Boolean;
begin

end;

function TFormStandalone.ReadMouseButton(MouseButton: TMouseButton): Boolean;
begin

end;

procedure TFormStandalone.RunScript;
begin
  // abort if no compiled program is available or if it has errors
  if not Assigned(FCompiledProgram) or FCompiledProgram.Msgs.HasErrors then
    Exit;

  FProgramExecution := nil;

  FProgramExecution := FCompiledProgram.CreateNewExecution;
  Image.Bitmap.Canvas.BeginScene;
  FProgramExecution.Execute;
  Image.Bitmap.Canvas.EndScene;

  FProgramExecution := nil;
end;

end.
