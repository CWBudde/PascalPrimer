unit PascalPrimer.Standalone;

interface

uses
  System.Types, System.SyncObjs, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, WinApi.Windows, WinApi.Messages,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus, Vcl.StdActns, Vcl.ActnList,
  Vcl.ExtDlgs, Vcl.ComCtrls, Vcl.ImgList, Vcl.ToolWin, Vcl.Buttons,

  (* GR32 *)
  GR32, GR32_Image, GR32_Layers, GR32_Transforms, GR32_PNG, GR32_Polygons,

  (* DWS *)
  dwsComp, dwsExprs, dwsSymbols, dwsErrors, dwsSuggestions, dwsVCLGUIFunctions,
  dwsStrings, dwsUnitSymbols, dwsFunctions, dwsTokenizer,
  {$IFNDEF WIN64} dwsJIT, dwsJITx86, {$ENDIF}

  (* Custom *)
  PascalPrimer.Shared;

type
  TOnCompilationCompleted = procedure(Sender: TObject; CompiledProgram: IdwsProgram) of object;

  TOutputImage32 = class(TInterfacedObject, IOutputGraphics)
  private
    FImage32: TImage32;
    function GetHeight: Integer;
    function GetPixelColor(X, Y: Integer): TColor32;
    function GetWidth: Integer;
    procedure SetPixelColor(X, Y: Integer; Value: TColor32);
  public
    constructor Create(Image32: TImage32);

    procedure Clear(Color: TColor32);
    procedure DrawLine(A, B: TPoint; Color: TColor32);
    procedure DrawLineF(A, B: TPointF; Color: TColor32);
    procedure Invalidate(WaitForRefresh: Boolean);
    procedure SaveToFile(FileName: TFileName);

    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property PixelColor[X, Y: Integer]: TColor32 read GetPixelColor write SetPixelColor;
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

  TInputImage32 = class(TInterfacedObject, IInput)
  private
    FImage32: TImage32;
    FKeysPressedHistory: string;
    FMouseButton: array [TMouseButton] of Boolean;
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  public
    constructor Create(Image32: TImage32);

    function ReadKey(LimitToLastKey: Boolean): string;
    function ReadMouseButton: Boolean; overload;
    function ReadMouseButton(MouseButton: TMouseButton): Boolean; overload;
    function GetMousePosition(LimitToBounds: Boolean): TPoint;

    property KeysPressedHistory: string read FKeysPressedHistory write FKeysPressedHistory;
  end;

  TFormStandalone = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure Image32Resize(Sender: TObject);
  private
    FProgramExecution: IdwsProgramExecution;
    FCompiledProgram: IdwsProgram;
    FCriticalSection: TCriticalSection;

    FInput: TInputImage32;
    FPolygonInverter: TInvertPolygonFiller;

    FSourceCode: string;
  public
    procedure CompileScript;
    procedure RunScript;
  end;

var
  FormStandalone: TFormStandalone;

implementation

{$R *.dfm}

uses
  Math, Registry, StrUtils, ShellAPI, dwsUtils, dwsXPlatform,
  GR32_PortableNetworkGraphic, GR32_VPR, GR32_Paths, GR32_Math, GR32_Brushes;

{ TOutputImage32 }

constructor TOutputImage32.Create(Image32: TImage32);
begin
  FImage32 := Image32;
end;

procedure TOutputImage32.Clear(Color: TColor32);
begin
  FImage32.Bitmap.Clear(Color);
end;

procedure TOutputImage32.DrawLine(A, B: TPoint; Color: TColor32);
begin
  FImage32.Bitmap.LineTS(A.X, A.Y, B.X, B.Y, Color);
end;

procedure TOutputImage32.DrawLineF(A, B: TPointF; Color: TColor32);
begin
  FImage32.Bitmap.LineFS(A.X, A.Y, B.X, B.Y, Color);
end;

function TOutputImage32.GetHeight: Integer;
begin
  Result := FImage32.Bitmap.Height;
end;

function TOutputImage32.GetPixelColor(X, Y: Integer): TColor32;
begin
  Result := FImage32.Bitmap.PixelS[X, Y];
end;

function TOutputImage32.GetWidth: Integer;
begin
  Result := FImage32.Bitmap.Width;
end;

procedure TOutputImage32.Invalidate(WaitForRefresh: Boolean);
begin
  Application.ProcessMessages;
  if WaitForRefresh then
    FImage32.Refresh
  else
    FImage32.Invalidate;
end;

procedure TOutputImage32.SaveToFile(FileName: TFileName);
var
  FileExt: string;
begin
  if FileName = '' then
    Exit;

  if IsRelativePath(FileName) then
    FileName := ExtractFilePath(ParamStr(0)) + FileName;

  FileExt := LowerCase(ExtractFileExt(Filename));
  if FileExt = '' then
    SaveBitmap32ToPNG(FImage32.Bitmap, Filename + '.png')
  else if FileExt = '.bmp' then
    FImage32.Bitmap.SaveToFile(Filename)
  else if FileExt = '.png' then
    SaveBitmap32ToPNG(FImage32.Bitmap, Filename)
  else
    Exit;
end;

procedure TOutputImage32.SetPixelColor(X, Y: Integer; Value: TColor32);
begin
  FImage32.Bitmap.PixelS[X, Y] := Value;
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


{ TInputImage32 }

constructor TInputImage32.Create(Image32: TImage32);
begin
  FImage32 := Image32;
  FImage32.OnMouseDown := MouseDownHandler;
  FKeysPressedHistory := '';
end;

procedure TInputImage32.MouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FMouseButton[Button] := True;
  FImage32.SetFocus;
end;

function TInputImage32.ReadKey(LimitToLastKey: Boolean): string;
begin
  // Check if no key has been pressed
  if Length(FKeysPressedHistory) = 0 then
    Exit('');

  if LimitToLastKey then
  begin
    Result := FKeysPressedHistory[Length(FKeysPressedHistory)];
    FKeysPressedHistory := '';
  end
  else
  begin
    if Length(FKeysPressedHistory) > 0 then
    begin
      Result := FKeysPressedHistory[1];
      Delete(FKeysPressedHistory, 1, 1);
    end;
  end;
end;

function TInputImage32.ReadMouseButton: Boolean;
begin
  Result := FMouseButton[mbLeft] or FMouseButton[mbMiddle] or FMouseButton[mbRight];
  FMouseButton[mbLeft] := False;
  FMouseButton[mbMiddle] := False;
  FMouseButton[mbRight] := False;
end;

function TInputImage32.ReadMouseButton(MouseButton: TMouseButton): Boolean;
begin
  Result := FMouseButton[MouseButton];
  FMouseButton[MouseButton] := False;
end;

function TInputImage32.GetMousePosition(LimitToBounds: Boolean): TPoint;
var
  Pos: TPoint;
begin
  Pos := Mouse.CursorPos;
  Result := FImage32.ScreenToClient(Pos);
  if LimitToBounds then
  begin
    if Pos.X < 0 then
      Pos.X := 0;
    if Pos.Y < 0 then
      Pos.Y := 0;
    if Pos.X > FImage32.Width then
      Pos.X := FImage32.Width;
    if Pos.Y < FImage32.Height then
      Pos.Y := FImage32.Height;
  end;
end;


{ TFormStandalone }

procedure TFormStandalone.FormCreate(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);

  FPolygonInverter := TInvertPolygonFiller.Create;

  FCriticalSection := TCriticalSection.Create;

  FInput := TInputImage32.Create(Image32);

  DataModuleShared.OutputGraphics := TOutputImage32.Create(Image32);
  DataModuleShared.OutputText := TOutputStrings.Create(nil);
  DataModuleShared.Input := FInput;

  Image32.PaintStages.Add.Stage := PST_CUSTOM;
  Image32.PaintStages.Insert(1).Stage := PST_CUSTOM;

  // load source code
  if FileExists(ChangeFileExt(ParamStr(0), '.pas')) then
    FSourceCode := LoadTextFromFile(ChangeFileExt(ParamStr(0), '.pas'))
  else
    Application.Terminate;

  CompileScript;
end;

procedure TFormStandalone.FormDestroy(Sender: TObject);
begin
  // release interfaces
  FInput := nil;
  DataModuleShared.OutputGraphics := nil;
  DataModuleShared.OutputText := nil;
  DataModuleShared.Input := nil;

  FreeAndNil(FCriticalSection);
  FreeAndNil(FPolygonInverter);
end;

procedure TFormStandalone.FormShow(Sender: TObject);
begin
  DataModuleShared.TurtleCursor.Home;
  DataModuleShared.TurtleCursor.Visible := False;

  RunScript;
end;

procedure TFormStandalone.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // append key
  FInput.KeysPressedHistory := FInput.KeysPressedHistory + Key;
end;

procedure TFormStandalone.CompileScript;
begin
  // compile program
  FCompiledProgram := DataModuleShared.DelphiWebScript.Compile(FSourceCode);

  if FCompiledProgram.Msgs.HasErrors then
    Application.Terminate;

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

procedure TFormStandalone.RunScript;
begin
  // abort if no compiled program is available or if it has errors
  if not Assigned(FCompiledProgram) or FCompiledProgram.Msgs.HasErrors then
    Exit;

  FProgramExecution := nil;

  FProgramExecution := FCompiledProgram.CreateNewExecution;
  FProgramExecution.Execute;
  Image32.Invalidate;

  FProgramExecution := nil;
end;

procedure TFormStandalone.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);

var
  s, c: Single;
  Pnts: TArrayOfFloatPoint;
begin
  if DataModuleShared.TurtleCursor.Visible and (StageNum = 6) then
  begin
    SetLength(Pnts, 3);

    with DataModuleShared.TurtleCursor do
    begin
      SinCos(Angle, 6, s, c);
      Pnts[0] := PointF(Position.X + c, Position.Y + S);

      SinCos(Angle + 2.5, 6, s, c);
      Pnts[1] := PointF(Position.X + c, Position.Y + s);

      SinCos(Angle - 2.5, 6, s, c);
      Pnts[2] := PointF(Position.X + c, Position.Y + s);
    end;

    PolygonFS(Buffer, Pnts, FPolygonInverter);
    PolylineFS(Buffer, Pnts, DataModuleShared.TurtleCursor.Color, True);
  end;
end;

procedure TFormStandalone.Image32Resize(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
  DataModuleShared.TurtleCanvas.Clear;

  RunScript;
end;

initialization
  SetGamma(1);

end.
