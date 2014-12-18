unit PascalPrimer.Main;

interface

uses
  System.Types, System.SyncObjs, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, WinApi.Windows, WinApi.Messages,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus, Vcl.StdActns, Vcl.ActnList,
  Vcl.ExtDlgs, Vcl.ComCtrls, Vcl.ImgList, Vcl.ToolWin, Vcl.Buttons,

  (* GR32 *)
  GR32, GR32_Image, GR32_Transforms, GR32_PNG, GR32_Polygons,

  (* DWS *)
  dwsComp, dwsExprs, dwsSymbols, dwsErrors, dwsSuggestions, dwsVCLGUIFunctions,
  dwsStrings, dwsUnitSymbols, {$IFNDEF WIN64} dwsJIT, dwsJITx86, {$ENDIF}

  (* SynEdit *)
  SynEdit, SynEditHighlighter, SynHighlighterDWS, SynCompletionProposal,
  SynEditPlugins, SynEditMiscClasses, SynEditSearch, SynEditOptionsDialog,
  SynMacroRecorder,

  (* PNG *)
  PngImageList, PngButtonFunctions,

  (* WebUpdate *)
  WebUpdate.Classes.WebUpdate;

type
  TOnCompilationCompleted = procedure(Sender: TObject; CompiledProgram: IdwsProgram) of object;

  TBackgroundCompilationThread = class(TThread)
  private
    FCompiler: TDelphiWebScript;
    FCompilationIsDue: Boolean;
    FOnCompilationCompleted: TOnCompilationCompleted;
  public
    constructor Create(Compiler: TDelphiWebScript);
    procedure Execute; override;
    procedure ScheduleCompilation;

    property OnCompilationCompleted: TOnCompilationCompleted read FOnCompilationCompleted write FOnCompilationCompleted;
  end;

  TExecutionThread = class(TThread)
  private
    FCompiledProgram: IdwsProgram;
    FJustInTimeCompilation: Boolean;
    FProgramExecution: IdwsProgramExecution;
    FOnExecutionDone: TNotifyEvent;
    function GetOutput: string;
  public
    constructor Create(CompiledProgram: IdwsProgram; JustInTimeCompilation: Boolean = False);

    procedure Execute; override;
    procedure Abort;

    property Output: string read GetOutput;
    property OnExecutionDone: TNotifyEvent read FOnExecutionDone write FOnExecutionDone;
  end;

  TTurtleCanvas = class
  type
    TOutputLogMessage = procedure (Sender: TObject; const Text: string) of object;
  private
    FImage32: TImage32;
    FColor: TColor32;
    FOnOutputLogMessage: TOutputLogMessage;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixel(X, Y: Integer): TColor32; inline;
    procedure SetPixel(X, Y: Integer; Color: TColor32); inline;
  public
    constructor Create(Image32: TImage32);

    procedure Clear; overload;
    procedure Clear(Color: TColor32); overload;

    procedure DrawLine(AX, AY, BX, BY: Integer; Color: TColor32); overload;
    procedure DrawLine(A, B: TPoint; Color: TColor32); overload;
    procedure DrawLineF(A, B: TPointF; Color: TColor32);

    procedure SaveToFile(FileName: TFileName);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Color: TColor32 read FColor write FColor;
    property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel;

    property OnOutputLogMessage: TOutputLogMessage read FOnOutputLogMessage write FOnOutputLogMessage;
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

  TStatistics = class
  type
    TLogCall = (lcClear, lcCenter, lcComposeColor, lcComposeColorHSL, lcDelay,
      lcHome, lcLineTo, lcLookAt, lcMoveTo, lcGo, lcDraw, lcSaveToFile,
      lcTurnLeft, lcTurnRight, lcColorChange, lcAngleChange, lcAntialiased,
      lcPopPosition, lcPushPosition, lcSetPixelColor, lcGetPixelColor);
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

  TTargetArea = record
    Rect: TRect;
    Passed: Boolean;
  end;

  TFormMain = class(TForm)
    ActionEditCopy: TEditCopy;
    ActionEditCut: TEditCut;
    ActionEditDelete: TEditDelete;
    ActionEditPaste: TEditPaste;
    ActionEditSelectAll: TEditSelectAll;
    ActionEditUndo: TEditUndo;
    ActionFileExit: TFileExit;
    ActionFileNew: TAction;
    ActionFileOpen: TFileOpen;
    ActionFileSaveScriptAs: TFileSaveAs;
    ActionHelpAbout: TAction;
    ActionHelpDocumentation: TAction;
    ActionHelpTutorial: TAction;
    ActionList: TActionList;
    ActionOptions: TAction;
    ActionOutputAntialiased: TAction;
    ActionOutputCursorVisible: TAction;
    ActionOutputSaveOutputAs: TFileSaveAs;
    ActionOutputSizeVGA: TAction;
    ActionScriptAbort: TAction;
    ActionScriptAutoRun: TAction;
    ActionScriptCompile: TAction;
    ActionScriptJustInTime: TAction;
    ActionScriptRun: TAction;
    ActionSearchFind: TSearchFind;
    ActionSearchFindNext: TSearchFindNext;
    BadgeList: TBitmap32List;
    DelphiWebScript: TDelphiWebScript;
    dwsUnitAdvanced: TdwsUnit;
    dwsUnitBasic: TdwsUnit;
    dwsUnitIntermediate: TdwsUnit;
    Image32: TImage32;
    ImageListActions: TPngImageList;
    ImageListSuggestion: TPngImageList;
    ListBoxCompiler: TListBox;
    ListBoxOutput: TListBox;
    MainMenu: TMainMenu;
    MenuItemCursorVisible: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditDelete: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuItemHelpDocumentation: TMenuItem;
    MenuItemHelpTutorial: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemOutput: TMenuItem;
    MenuItemOutputAntialiased: TMenuItem;
    MenuItemOutputSaveAs: TMenuItem;
    MenuItemScript: TMenuItem;
    MenuItemScriptAbort: TMenuItem;
    MenuItemScriptJIT: TMenuItem;
    MenuItemScriptRun: TMenuItem;
    MenuItemSearch: TMenuItem;
    MenuItemSearchFind: TMenuItem;
    MenuItemSearchSearchAgain: TMenuItem;
    MenuItemVGA: TMenuItem;
    MenuSaveMessagesAs: TMenuItem;
    MenuSaveOutputAs: TMenuItem;
    MenuScriptAutomaticallyCompile: TMenuItem;
    MenuScriptCompile: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    PageControl: TPageControl;
    PanelEditOutput: TPanel;
    PanelOutput: TPanel;
    PopupMenuMessages: TPopupMenu;
    PopupMenuOutput: TPopupMenu;
    ScrollBox: TScrollBox;
    SplitterHorizontal: TSplitter;
    SplitterVertical: TSplitter;
    StatusBar: TStatusBar;
    SynCodeSuggestions: TSynCompletionProposal;
    SynDWSSyn: TSynDWSSyn;
    SynEdit: TSynEdit;
    SynEditOptionsDialog: TSynEditOptionsDialog;
    SynEditSearch: TSynEditSearch;
    SynMacroRecorder: TSynMacroRecorder;
    SynParameters: TSynCompletionProposal;
    TabSheetCompiler: TTabSheet;
    TabSheetOutput: TTabSheet;
    ToolBar: TToolBar;
    ToolButtonCompile: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonCut: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonFind: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonOptions: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonRun: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSelectAll: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonSeparator3: TToolButton;
    ToolButtonSeparator4: TToolButton;
    ToolButtonUndo: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveScriptAsAccept(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionHelpDocumentationExecute(Sender: TObject);
    procedure ActionHelpTutorialExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionOutputAntialiasedExecute(Sender: TObject);
    procedure ActionOutputAntialiasedUpdate(Sender: TObject);
    procedure ActionOutputCursorVisibleExecute(Sender: TObject);
    procedure ActionOutputCursorVisibleUpdate(Sender: TObject);
    procedure ActionOutputSaveOutputAsAccept(Sender: TObject);
    procedure ActionOutputSizeVGAExecute(Sender: TObject);
    procedure ActionOutputSizeVGAUpdate(Sender: TObject);
    procedure ActionScriptAbortExecute(Sender: TObject);
    procedure ActionScriptAbortUpdate(Sender: TObject);
    procedure ActionScriptAutoRunExecute(Sender: TObject);
    procedure ActionScriptCompileExecute(Sender: TObject);
    procedure ActionScriptJustInTimeExecute(Sender: TObject);
    procedure ActionScriptJustInTimeUpdate(Sender: TObject);
    procedure ActionScriptRunExecute(Sender: TObject);
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
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure Image32Resize(Sender: TObject);
    procedure ListBoxCompilerClick(Sender: TObject);
    procedure MenuSaveMessagesAsClick(Sender: TObject);
    procedure MnuScriptExitClick(Sender: TObject);
    procedure SynCodeSuggestionsClose(Sender: TObject);
    procedure SynCodeSuggestionsExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure SynCodeSuggestionsPaintItem(Sender: TObject; Index: Integer;
      TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
    procedure SynCodeSuggestionsShow(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynParametersExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure ActionFileOpenBeforeExecute(Sender: TObject);
  private
    FRecentScriptName: TFileName;
    FBackgroundCompilationThread: TBackgroundCompilationThread;
    FExecutionThread: TExecutionThread;
    FCompiledProgram: IdwsProgram;
    FCriticalSection: TCriticalSection;

    FPolygonInverter: TInvertPolygonFiller;

    FTurtleCanvas: TTurtleCanvas;
    FTurtleCursor: TTurtleCursor;
    FModified: Boolean;

    FTutorialIndex: Integer;
    FLastArea: TRect;
    FTargetAreas: array of TTargetArea;
    FTutorialText: string;
    FStatistics: TStatistics;
    FTutorialStatistics: TStatistics;
    FSuggestionWhiteList: TStringList;
    FSuggestions: IDWSSuggestions;

    FWebUpdate: TWebUpdate;

    procedure CursorPositionChangedHandler(Sender: TObject);
    procedure CompilationCompletedHandler(Sender: TObject; CompiledProgram: IdwsProgram);
    procedure ExecutionDoneHandler(Sender: TObject);

    procedure PrepareTutorial(Index: Integer);
    procedure LogCall(CallType: TStatistics.TLogCall; ClassAccess: Boolean = False);

    procedure ResetSuggestionWhitelist;
    procedure SetSuggestionWhitelist(Items: array of string);
    procedure BuildBadges;
  public
    procedure CompileScript;
    procedure RunScript(JIT: Boolean = False);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Math, Registry, StrUtils, ShellAPI, dwsUtils,
  GR32_PortableNetworkGraphic, GR32_VPR, GR32_Paths, GR32_Math, GR32_Brushes,
  PascalPrimer.About;

const
  CWebUpdateUrl = 'http://www.savioursofsoul.de/Christian/WebUpdate/PascalPrimer/';

{ TBackgroundCompilationThread }

constructor TBackgroundCompilationThread.Create(Compiler: TDelphiWebScript);
begin
  FCompiler := Compiler;

  inherited Create;
end;

procedure TBackgroundCompilationThread.Execute;
var
  SourceCode: string;
  CompiledProgram: IdwsProgram;
begin
  while not Terminated do
  begin
    if FCompilationIsDue then
    begin
      FCompilationIsDue := False;

      // fetch source code in a safe way
      Synchronize(procedure begin
        SourceCode := FormMain.SynEdit.Lines.Text;
      end);

      // compile source code
      CompiledProgram := FCompiler.Compile(SourceCode);

      // call compilation complete
      Synchronize(procedure begin
        if Assigned(FOnCompilationCompleted) then
          FOnCompilationCompleted(Self, CompiledProgram);
      end);
    end;
    Sleep(100);
  end;
end;

procedure TBackgroundCompilationThread.ScheduleCompilation;
begin
  FCompilationIsDue := True;
end;


{ TExecutionThread }

constructor TExecutionThread.Create(CompiledProgram: IdwsProgram;
  JustInTimeCompilation: Boolean = False);
begin
  FCompiledProgram := CompiledProgram;
  FJustInTimeCompilation := JustInTimeCompilation;
  FreeOnTerminate := True;
  inherited Create;
end;

procedure TExecutionThread.Abort;
begin
  if Assigned(FProgramExecution) then
    FProgramExecution.Stop;
end;

procedure TExecutionThread.Execute;
var
  Success: Boolean;
begin
  FProgramExecution := nil;

  // abort if no compiled program is available or if it has errors
  if not Assigned(FCompiledProgram) or FCompiledProgram.Msgs.HasErrors then
    Exit;

   {$ifndef WIN64}
   if FJustInTimeCompilation then
     with TdwsJITx86.Create do
     try
       Options := Options - [jitoNoBranchAlignment];
       GreedyJIT(FCompiledProgram.ProgramObject);
     finally
       Free;
     end;
   {$endif}

  try
    FProgramExecution := FCompiledProgram.CreateNewExecution;
    FProgramExecution.Execute(5 * 60 * 1000); // 5 minutes!

    Success := True;
  except
    Success := False
  end;

  if Terminated then
    Exit;

  Synchronize(procedure
  begin
    if Success then
    begin
      FormMain.ListBoxOutput.Items.Text := FProgramExecution.Result.ToString;
      FormMain.StatusBar.Panels[1].Text := 'Executed';
    end
    else
      FormMain.StatusBar.Panels[1].Text := 'Error';

    FormMain.Image32.Invalidate;

    if Assigned(FOnExecutionDone) then
      FOnExecutionDone(Self);
  end);

  FProgramExecution := nil;
end;

function TExecutionThread.GetOutput: string;
begin
  if Assigned(FProgramExecution) then
    Result := FProgramExecution.Result.ToString
  else
    Result := '';
end;


{ TTurtleCanvas }

constructor TTurtleCanvas.Create(Image32: TImage32);
begin
  FImage32 := Image32;
  FColor := 0;
end;

procedure TTurtleCanvas.DrawLine(AX, AY, BX, BY: Integer; Color: TColor32);
begin
  FImage32.Bitmap.LineTS(AX, AY, BX, BY, Color);
end;

procedure TTurtleCanvas.DrawLine(A, B: TPoint; Color: TColor32);
begin
  FImage32.Bitmap.LineTS(A.X, A.Y, B.X, B.Y, Color);
end;

procedure TTurtleCanvas.DrawLineF(A, B: TPointF; Color: TColor32);
begin
  FImage32.Bitmap.LineFS(A.X, A.Y, B.X, B.Y, Color);
end;

function TTurtleCanvas.GetHeight: Integer;
begin
  Result := FImage32.Bitmap.Height;
end;

function TTurtleCanvas.GetPixel(X, Y: Integer): TColor32;
begin
  Result := FImage32.Bitmap.Pixel[X, Y];
end;

function TTurtleCanvas.GetWidth: Integer;
begin
  Result := FImage32.Bitmap.Width;
end;

procedure TTurtleCanvas.SaveToFile(FileName: TFileName);
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
  begin
    SaveBitmap32ToPNG(FImage32.Bitmap, ChangeFileExt(Filename, '.png'));

    if Assigned(FOnOutputLogMessage) then
      FOnOutputLogMessage(Self, 'SaveToFile Warning: File is stored as PNG!');
  end;
end;

procedure TTurtleCanvas.SetPixel(X, Y: Integer; Color: TColor32);
begin
  FImage32.Bitmap.PixelS[X, Y] := Color;
end;

procedure TTurtleCanvas.Clear;
begin
  FImage32.Bitmap.Clear(FColor);
end;

procedure TTurtleCanvas.Clear(Color: TColor32);
begin
  FColor := Color;
  FImage32.Bitmap.Clear(Color);
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


{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);

  FStatistics := TStatistics.Create;

  FPolygonInverter := TInvertPolygonFiller.Create;

  FCriticalSection := TCriticalSection.Create;
  FBackgroundCompilationThread := TBackgroundCompilationThread.Create(DelphiWebScript);
  FBackgroundCompilationThread.OnCompilationCompleted := CompilationCompletedHandler;

  FTutorialIndex := -1;

  FSuggestionWhiteList := TStringList.Create;
  ResetSuggestionWhitelist;

  FTurtleCanvas := TTurtleCanvas.Create(Image32);
  FTurtleCursor := TTurtleCursor.Create(FTurtleCanvas);

  FWebUpdate := TWebUpdate.Create(CWebUpdateUrl);

  Image32.PaintStages.Add.Stage := PST_CUSTOM;
  Image32.PaintStages.Insert(1).Stage := PST_CUSTOM;

  BuildBadges;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FWebUpdate.Free;
  FTurtleCursor.Free;
  FTurtleCanvas.Free;
  FSuggestionWhiteList.Free;
  FStatistics.Free;

  if Assigned(FBackgroundCompilationThread) then
  begin
    FBackgroundCompilationThread.Terminate;
    FBackgroundCompilationThread.WaitFor;
    FreeAndNil(FBackgroundCompilationThread);
  end;

  FreeAndNil(FCriticalSection);
  FreeAndNil(FPolygonInverter);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FRecentScriptName := ChangeFileExt(Application.ExeName, '.dws');
  if FileExists(FRecentScriptName) then
    SynEdit.Lines.LoadFromFile(FRecentScriptName);

  with TRegistry.Create do
  try
    if OpenKey('Software\DWS\Interactive\', False) then
    begin
      SynEdit.CaretX := ReadInteger('CaretX');
      SynEdit.CaretY := ReadInteger('CaretY');

      // recall script options
      if ValueExists('AutoRun') then
        ActionScriptAutoRun.Checked := ReadBool('AutoRun');

      // recall output options
      if ValueExists('AntiAliasedLine') then
        FTurtleCursor.Antialiased := ReadBool('AntiAliasedLine');
      if ValueExists('CursorVisible') then
        FTurtleCursor.Visible := ReadBool('CursorVisible');
    end;
    CloseKey;
  finally
    Free;
  end;

  FTurtleCursor.Home;

  // schedule first compilation
  FBackgroundCompilationThread.ScheduleCompilation;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SynEdit.Lines.SaveToFile(FRecentScriptName);

  with TRegistry.Create do
  try
    OpenKey('Software\DWS\Interactive\', True);

    // store compile options
    WriteBool('AutoRun', ActionScriptAutoRun.Checked);

    // store output options
    WriteBool('AntiAliasedLine', FTurtleCursor.Antialiased);
    WriteBool('CursorVisible', FTurtleCursor.Visible);

    // store caret position
    WriteInteger('CaretX', SynEdit.CaretX);
    WriteInteger('CaretY', SynEdit.CaretY);

    // store options
    WriteBool('AltSetsColumnMode', eoAltSetsColumnMode in SynEdit.Options);
    WriteBool('AutoIndent', eoAutoIndent in SynEdit.Options);
    WriteBool('AutoSizeMaxScrollWidth', eoAutoSizeMaxScrollWidth in SynEdit.Options);
    WriteBool('DisableScrollArrows', eoDisableScrollArrows in SynEdit.Options);
    WriteBool('DragDropEditing', eoDragDropEditing in SynEdit.Options);
    WriteBool('DropFiles', eoDropFiles in SynEdit.Options);
    WriteBool('EnhanceHomeKey', eoEnhanceHomeKey in SynEdit.Options);
    WriteBool('EnhanceEndKey', eoEnhanceEndKey in SynEdit.Options);
    WriteBool('GroupUndo', eoGroupUndo in SynEdit.Options);
    WriteBool('HalfPageScroll', eoHalfPageScroll in SynEdit.Options);
    WriteBool('HideShowScrollbars', eoHideShowScrollbars in SynEdit.Options);
    WriteBool('KeepCaretX', eoKeepCaretX in SynEdit.Options);
    WriteBool('NoCaret', eoNoCaret in SynEdit.Options);
    WriteBool('NoSelection', eoNoSelection in SynEdit.Options);
    WriteBool('RightMouseMovesCursor', eoRightMouseMovesCursor in SynEdit.Options);
    WriteBool('ScrollByOneLess', eoScrollByOneLess in SynEdit.Options);
    WriteBool('ScrollHintFollows', eoScrollHintFollows in SynEdit.Options);
    WriteBool('ScrollPastEof', eoScrollPastEof in SynEdit.Options);
    WriteBool('ScrollPastEol', eoScrollPastEol in SynEdit.Options);
    WriteBool('ShowScrollHint', eoShowScrollHint in SynEdit.Options);
    WriteBool('ShowSpecialChars', eoShowSpecialChars in SynEdit.Options);
    WriteBool('SmartTabDelete', eoSmartTabDelete in SynEdit.Options);
    WriteBool('SmartTabs', eoSmartTabs in SynEdit.Options);
    WriteBool('SpecialLineDefaultFg', eoSpecialLineDefaultFg in SynEdit.Options);
    WriteBool('TabIndent', eoTabIndent in SynEdit.Options);
    WriteBool('TabsToSpaces', eoTabsToSpaces in SynEdit.Options);
    WriteBool('TrimTrailingSpaces', eoTrimTrailingSpaces in SynEdit.Options);

    CloseKey;
  finally
    Free;
  end;
end;

procedure TFormMain.dwsFunctionsCenterEval(info: TProgramInfo);
begin
  FTurtleCursor.Center;

  LogCall(lcCenter);
end;

procedure TFormMain.dwsFunctionsClearEval(info: TProgramInfo);
begin
  // clear text output
  ListBoxOutput.Items.Clear;

  // clear bitmap
  if info.FuncSym.Params.Count = 1 then
    FTurtleCanvas.Clear(Info.ParamAsInteger[0])
  else
    FTurtleCanvas.Clear;

  LogCall(lcClear);
end;

procedure TFormMain.dwsFunctionsComposeColorEval(
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

procedure TFormMain.dwsFunctionsComposeColorHSLEval(
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

procedure TFormMain.dwsFunctionsDelayEval(info: TProgramInfo);
var
  DelayTime: Integer;
begin
  DelayTime := info.ParamAsInteger[0];
  if DelayTime > 0 then
    Sleep(info.ParamAsInteger[0]);
  Image32.Invalidate;

  ListBoxOutput.Items.Text := FExecutionThread.Output;

  LogCall(lcDelay);
end;

procedure TFormMain.dwsFunctionsDrawEval(info: TProgramInfo);
begin
  FTurtleCursor.Draw(info.ParamAsFloat[0]);
  LogCall(lcDraw);
end;

procedure TFormMain.dwsFunctionsGetPixelColorEval(
  info: TProgramInfo);
begin
  info.ResultAsInteger := Image32.Bitmap.PixelS[
    info.ParamAsInteger[0], info.ParamAsInteger[1]];
end;

procedure TFormMain.dwsFunctionsGoEval(info: TProgramInfo);
begin
  FTurtleCursor.Go(info.ParamAsFloat[0]);
  LogCall(lcGo);
end;

procedure TFormMain.dwsFunctionsHomeEval(info: TProgramInfo);
begin
  FTurtleCursor.Home;
  LogCall(lcHome);
end;

procedure TFormMain.dwsFunctionsLineToEval(info: TProgramInfo);
begin
  FTurtleCursor.LineTo(info.ValueAsFloat['X'], info.ValueAsFloat['Y']);
  LogCall(lcLineTo);
end;

procedure TFormMain.dwsFunctionsLookAtEval(info: TProgramInfo);
begin
  FTurtleCursor.LookAt(Info.ValueAsFloat['X'], Info.ValueAsFloat['Y']);
  LogCall(lcLookAt);
end;

procedure TFormMain.dwsFunctionsMoveToEval(info: TProgramInfo);
begin
  FTurtleCursor.MoveTo(info.ValueAsFloat['X'], info.ValueAsFloat['Y']);
  LogCall(lcMoveTo);
end;

procedure TFormMain.dwsFunctionsPopPositionEval(info: TProgramInfo);
begin
  FTurtleCursor.PopPosition;
  LogCall(lcPopPosition);
end;

procedure TFormMain.dwsFunctionsPushPositionEval(
  info: TProgramInfo);
begin
  FTurtleCursor.PushPosition;
  LogCall(lcPushPosition);
end;

procedure TFormMain.dwsFunctionsSaveToFileEval(info: TProgramInfo);
begin
  FTurtleCanvas.SaveToFile(info.ParamAsString[0]);
  LogCall(lcSaveToFile);
end;

procedure TFormMain.dwsFunctionsSetPixelColorEval(
  info: TProgramInfo);
begin
  FTurtleCanvas.Pixel[info.ParamAsInteger[0],
    info.ParamAsInteger[1]] := info.ParamAsInteger[2];

  LogCall(lcSetPixelColor);
end;

procedure TFormMain.dwsFunctionsTurnLeftEval(info: TProgramInfo);
begin
  FTurtleCursor.Turn(info.ParamAsFloat[0]);
  LogCall(lcTurnLeft);
end;

procedure TFormMain.dwsFunctionsTurnRightEval(info: TProgramInfo);
begin
  FTurtleCursor.Turn(-info.ParamAsFloat[0]);
  LogCall(lcTurnRight);
end;

procedure TFormMain.dwsClassesTCursorMethodsDrawEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Draw(info.ParamAsFloat[0]);
end;

procedure TFormMain.dwsClassesTCursorMethodsSetAngleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Angle := info.ParamAsFloat[0];
end;

procedure TFormMain.dwsClassesTCursorMethodsLookAtEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).LookAt(info.ParamAsFloat[0], info.ParamAsFloat[1]);
end;

procedure TFormMain.dwsClassesTCursorMethodsPushPositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).PushPosition;
end;

procedure TFormMain.dwsClassesTCursorMethodsPopPositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).PopPosition;
end;

procedure TFormMain.dwsClassesTCursorMethodsSetVisibleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Visible := info.ParamAsBoolean[0];
end;

procedure TFormMain.dwsClassesTCursorMethodsSetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Color := info.ParamAsInteger[0];
end;

procedure TFormMain.dwsClassesTCursorMethodsTurnLeftEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Turn(info.ParamAsInteger[0]);
end;

procedure TFormMain.dwsClassesTCursorMethodsTurnRightEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Turn(-info.ParamAsInteger[0]);
end;

procedure TFormMain.dwsClassesTCursorMethodsGoEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  TTurtleCursor(ExtObject).Go(info.ParamAsFloat[0]);
end;

procedure TFormMain.dwsClassesTCanvasMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).Clear;
end;

procedure TFormMain.dwsClassesTCanvasSaveToFileEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  FTurtleCanvas.SaveToFile(info.ParamAsString[0]);
  LogCall(lcSaveToFile, True);
end;

procedure TFormMain.dwsClassesTCursorMethodsGetAngleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsFloat := TTurtleCursor(ExtObject).Angle;
end;

procedure TFormMain.dwsClassesTCursorMethodsGetVisibleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsBoolean := TTurtleCursor(ExtObject).Visible;
end;

procedure TFormMain.dwsClassesTCursorMethodsGetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCursor);
  Info.ResultAsInteger := TTurtleCursor(ExtObject).Color;
end;

procedure TFormMain.dwsInstanceCanvasInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := Image32.Bitmap;
end;

procedure TFormMain.dwsInstanceCursorInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := FTurtleCursor;
end;

procedure TFormMain.dwsClassesTCanvasMethodsGetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  info.ResultAsInteger := TTurtleCanvas(ExtObject).Color;
end;

procedure TFormMain.dwsClassesTCanvasMethodsSetColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).Color := info.ParamAsInteger[0];
end;

procedure TFormMain.dwsClassesTCanvasMethodsSetPixelColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  TTurtleCanvas(ExtObject).SetPixel(info.ParamAsInteger[0],
    info.ParamAsInteger[1], info.ParamAsInteger[2]);
end;

procedure TFormMain.dwsClassesTCanvasMethodsGetPixelColorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Assert(ExtObject is TTurtleCanvas);
  info.ResultAsInteger := TTurtleCanvas(ExtObject).GetPixel(
    info.ParamAsInteger[0], info.ParamAsInteger[1]);
end;

procedure TFormMain.dwsVariablesAntiAliasedLineReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.AntiAliased;
end;

procedure TFormMain.dwsVariablesAntiAliasedLineWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  if FTurtleCursor.AntiAliased <> Value then
  begin
    FTurtleCursor.AntiAliased := Value;
    LogCall(lcAntialiased);
  end;
end;

procedure TFormMain.dwsVariablesCanvasColorReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCanvas.Color;
end;

procedure TFormMain.dwsVariablesCanvasColorWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCanvas.Color := Value;
end;

procedure TFormMain.dwsVariablesClientHeightReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := Int64(FTurtleCanvas.Height);
end;

procedure TFormMain.dwsVariablesClientWidthReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := Int64(FTurtleCanvas.Width);
end;

procedure TFormMain.dwsVariablesCursorAngleReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := RadToDeg(-FTurtleCursor.Angle);
end;

procedure TFormMain.dwsVariablesCursorAngleWriteVar(
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

procedure TFormMain.dwsVariablesCursorColorReadVar(
  info: TProgramInfo; var value: Variant);
begin
  value := FTurtleCursor.Color;
end;

procedure TFormMain.dwsVariablesCursorColorWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  if FTurtleCursor.Color <> Value then
  begin
    FTurtleCursor.Color := Value;
    LogCall(lcColorChange);
  end;
end;

procedure TFormMain.dwsVariablesCursorPositionXReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.Position.X;
end;

procedure TFormMain.dwsVariablesCursorPositionXWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCursor.Position := PointF(Value, FTurtleCursor.Position.Y);
end;

procedure TFormMain.dwsVariablesCursorPositionYReadVar(
  info: TProgramInfo; var value: Variant);
begin
  Value := FTurtleCursor.Position.Y;
end;

procedure TFormMain.dwsVariablesCursorPositionYWriteVar(
  info: TProgramInfo; const value: Variant);
begin
  FTurtleCursor.Position := PointF(FTurtleCursor.Position.X, Value);
end;

procedure TFormMain.dwsVariablesCursorReadVar(info: TProgramInfo;
  var value: Variant);
begin
  value := ActionOutputCursorVisible.Checked;
end;

procedure TFormMain.dwsVariablesCursorWriteVar(info: TProgramInfo;
  const value: Variant);
begin
  ActionOutputCursorVisible.Checked := value;
end;

procedure TFormMain.CompilationCompletedHandler(Sender: TObject;
  CompiledProgram: IdwsProgram);
var
  Index: Integer;
begin
  StatusBar.Panels[1].Text := 'Compiled';

  FCompiledProgram := CompiledProgram;

  ListBoxCompiler.Items.Clear;
  for Index := 0 to FCompiledProgram.Msgs.Count - 1 do
    ListBoxCompiler.Items.AddObject(FCompiledProgram.Msgs[Index].AsInfo,
      FCompiledProgram.Msgs[Index]);

  if ListBoxCompiler.Count = 0 then
    PageControl.ActivePage := TabSheetOutput
  else
    PageControl.ActivePage := TabSheetCompiler;

  if ActionScriptAutoRun.Checked then
    RunScript;
end;

function BuildAreaTarget(Pt: TPointF; OffsetX: Integer = 0;
  OffsetY: Integer = 0): TTargetArea; overload;
begin
  Result.Rect := Rect(Round(Pt.X + OffsetX - 8), Round(Pt.Y + OffsetY - 8),
    Round(Pt.X + OffsetX + 8), Round(Pt.Y + OffsetY + 8));
  Result.Passed := False;
end;

function BuildAreaTarget(Pt: TPoint; OffsetX: Integer = 0;
  OffsetY: Integer = 0): TTargetArea; overload;
begin
  Result.Rect := Rect(Pt.X + OffsetX - 8, Pt.Y + OffsetY - 8,
    Pt.X + OffsetX + 8, Pt.Y + OffsetY + 8);
  Result.Passed := False;
end;

procedure TFormMain.ExecutionDoneHandler(Sender: TObject);
var
  Index: Integer;
  TargetAreasReached: Boolean;
begin
  FExecutionThread := nil;

  // check for tutorial conditions
  if (FTutorialIndex >= 0) and Assigned(FTutorialStatistics) then
  begin
    TargetAreasReached := True;
    for Index := Low(FTargetAreas) to High(FTargetAreas) do
      TargetAreasReached := TargetAreasReached and PtInRect(
        FTargetAreas[Index].Rect, FTurtleCursor.Position);

    // evaluate by tutorial
    case FTutorialIndex of
      0, 1:
        if TargetAreasReached and (FTutorialStatistics.GoCalls > 0) then
          PrepareTutorial(FTutorialIndex + 1);
      2:
        if TargetAreasReached and (FTutorialStatistics.DrawCalls > 0) then
          PrepareTutorial(FTutorialIndex + 1);
      3..4:
        if TargetAreasReached then
          PrepareTutorial(FTutorialIndex + 1);
      5:
        if TargetAreasReached and (FTutorialStatistics.AngleChanges > 0) then
          PrepareTutorial(FTutorialIndex + 1);
      6:
        if TargetAreasReached then
          PrepareTutorial(FTutorialIndex + 1)
        else
        begin
          SetLength(FTargetAreas, 0);
          PrepareTutorial(FTutorialIndex);
          Exit;
        end;
      7:
        if FTutorialStatistics.ColorChanges > 4 then
          PrepareTutorial(FTutorialIndex + 1);
      8, 9:
        if TargetAreasReached then
          PrepareTutorial(FTutorialIndex + 1);
      10:
        if TargetAreasReached and (FStatistics.AntialiasedChanges > 1) then
          PrepareTutorial(FTutorialIndex + 1);
      else
        PrepareTutorial(FTutorialIndex + 1);
    end;
  end;
end;

procedure TFormMain.CompileScript;
begin
  FBackgroundCompilationThread.ScheduleCompilation;
end;

procedure TFormMain.ResetSuggestionWhitelist;
begin
  FSuggestionWhiteList.Clear;
  FSuggestionWhiteList.Add('Center');
  FSuggestionWhiteList.Add('Clear');
  FSuggestionWhiteList.Add('ClientHeight');
  FSuggestionWhiteList.Add('ClientWidth');
  FSuggestionWhiteList.Add('ComposeColor');
  FSuggestionWhiteList.Add('CursorAngle');
  FSuggestionWhiteList.Add('CursorColor');
  FSuggestionWhiteList.Add('CursorVisible');
  FSuggestionWhiteList.Add('Delay');
  FSuggestionWhiteList.Add('Draw');
  FSuggestionWhiteList.Add('GetPixelColor');
  FSuggestionWhiteList.Add('Go');
  FSuggestionWhiteList.Add('Home');
  FSuggestionWhiteList.Add('LineTo');
  FSuggestionWhiteList.Add('LookAt');
  FSuggestionWhiteList.Add('MoveTo');
  FSuggestionWhiteList.Add('SaveToFile');
  FSuggestionWhiteList.Add('SetPixelColor');
  FSuggestionWhiteList.Add('Turn');
  FSuggestionWhiteList.Add('TurnLeft');
  FSuggestionWhiteList.Add('TurnRight');
end;

procedure TFormMain.RunScript(JIT: Boolean = False);
begin
  // abort if no compiled program is available or if it has errors
  if not Assigned(FCompiledProgram) or FCompiledProgram.Msgs.HasErrors then
    Exit;

  // check if execution is still running
  if Assigned(FExecutionThread) then
  begin
    FExecutionThread.Abort;
    FExecutionThread.Terminate;
  end;

  // start new execution in a separate thread
  FExecutionThread := TExecutionThread.Create(FCompiledProgram, JIT);
  FExecutionThread.OnExecutionDone := ExecutionDoneHandler;
end;

procedure TFormMain.MenuSaveMessagesAsClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Filter := 'Text (*.txt)|*.txt';
    if Execute then
      ListBoxOutput.Items.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TFormMain.MnuScriptExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.PrepareTutorial(Index: Integer);
var
  TargetPoint: TPoint;
  X, Y: Integer;
begin
  FTutorialIndex := Index;

  if not Assigned(FTutorialStatistics) then
    FTutorialStatistics := TStatistics.Create;

  if Index >= 0 then
    FTurtleCursor.OnPositionChanged := CursorPositionChangedHandler
  else
    FTurtleCursor.OnPositionChanged := nil;

  // configure tool buttons
  ToolButtonCopy.Visible := (Index < 0) or (Index >= 6);
  ToolButtonCut.Visible := (Index < 0) or (Index >= 6);
  ToolButtonPaste.Visible := (Index < 0) or (Index >= 6);
  ToolButtonSelectAll.Visible := (Index < 0) or (Index >= 6);
  ToolButtonUndo.Visible := (Index < 0) or (Index >= 6);
  ToolButtonFind.Visible := (Index < 0) or (Index >= 6);
  ToolButtonOptions.Visible := (Index < 0) or (Index >= 6);
  ToolButtonSeparator1.Visible := (Index < 0) or (Index >= 6);
  ToolButtonSeparator2.Visible := (Index < 0) or (Index >= 6);
  ToolButtonSeparator3.Visible := (Index < 0) or (Index >= 6);

  // configure actions
  ActionScriptAutoRun.Visible := (Index < 0) or (Index >= 8);
  ActionScriptAbort.Visible := (Index < 0) or (Index >= 8);
  ActionOutputSaveOutputAs.Visible := (Index < 0);
  ActionOutputCursorVisible.Visible := (Index < 0);
  ActionOutputAntialiased.Visible := (Index < 0) or (Index >= 10);

  // configure menus
  MenuItemOutput.Visible := (Index < 0) or (Index >= 10);
  MenuItemSearch.Visible := (Index < 0) or (Index >= 6);
  MenuItemEdit.Visible := (Index < 0) or (Index >= 6);

  Image32.Bitmap.Clear(0);

  // store last
  if Length(FTargetAreas) > 0 then
    FLastArea := FTargetAreas[Length(FTargetAreas) - 1].Rect;

  case Index of
    0:
      begin
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Home(True);
        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Home']);

        FTutorialText :=
          'Welcome to this interactive DWS environment!' + #10 + #10 +
          'On the left you can see the code editor. This will be the place ' +
          'where you can write executable scripts. The code is continously ' +
          'compiled. All errors (if present) will be shown below.' + #10 +
          'In this area you can see the cursor, a small black triangle. ' +
          'Using simple commands, you can control this in various ways. ' +
          #10#10#10 + 'Let''s start by writing ''Go'' to the source code!' +
          #10 + 'After that, just run the script by pressing F9.' + #10 +
          'If you reach the red square, this lesson is completed!';

        SetLength(FTargetAreas, 1);
        FTargetAreas[0] := BuildAreaTarget(FTurtleCursor.Position, 30);
      end;
    1:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Angle := 0;
        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Home']);

        FTutorialText :=
          'You can change the distance by adding an argument to the call.' +
          'To complete this lesson reach the next red square by passing ' +
          'the distance of 28 to the procedure directly.' + #10#10 +
          'Just write ''Go(28)'' and press F9!' + #10#10#10#10#10#10 +
          'Note: A negative distance is also possible (in case you missed ' +
          'the red target area).';

        SetLength(FTargetAreas, 1);
        FTargetAreas[0] := BuildAreaTarget(FTurtleCursor.Position, 24);
      end;
    2:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Angle := 0;
        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Draw', 'Home']);

        FTutorialText :=
          'Instead of ''Go'' you can also use ''Draw'' to draw a line from ' +
          'the last position to the new position. This helps you visually to ' +
          'keep track of the cursor.' + #10 +
          'Just write ''Draw(28)'' and press F9 to complete this lesson.';

        SetLength(FTargetAreas, 1);
        FTargetAreas[0] := BuildAreaTarget(FTurtleCursor.Position, 30);
      end;
    3:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Angle := 0;
        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Draw', 'Home', 'Turn',
          'TurnLeft', 'TurnRight']);

        FTutorialText :=
          'You can also turn the cursor into other directions. To do ' +
          'so you can use the commands ''TurnLeft'' and ''TurnRight''' + #10 +
          'Now try to reach the next red target square by using either ' +
          '''Go'' or ''Draw'' and ''TurnLeft'' or ''TurnRight''.' + #10#10 +
          'Note: In case you want to use more than one commands in the ' +
          'script, you need to terminate each command by a semicolon ('';'')';

        SetLength(FTargetAreas, 1);
        FTargetAreas[0] := BuildAreaTarget(FTurtleCursor.Position, 0, 30);
      end;
    4:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Draw', 'Home', 'Turn',
          'TurnLeft', 'TurnRight']);

        FTutorialText :=
          'Similar to the argument of ''Go'' and ''Draw'' you can also ' +
          'specify the angle to turn left/right as an argument.' + #10 +
          'For example ''TurnLeft(45)'' will turn the cursor to the left by ' +
          ' 45°.' + #10 + 'Again the goal is to reach the red square to ' +
          'complete this lesson.' + #10 +
          'Note: In case you "lost" your cursor beyond the boundaries, you ' +
          'can always move it back to the "home" position (center of the ' +
          'canvas) by calling ''home;'' or ''center;''.';

        TargetPoint.X := Image32.Bitmap.Width div 2;
        TargetPoint.Y := Image32.Bitmap.Height div 2;
        Image32.Bitmap.HorzLineTS(TargetPoint.X - 4, TargetPoint.Y,
          TargetPoint.X + 4, clTrGray32);
        Image32.Bitmap.VertLineTS(TargetPoint.X, TargetPoint.Y - 4,
          TargetPoint.Y + 4, clTrGray32);
        Image32.Bitmap.RenderText(TargetPoint.X + 2, TargetPoint.Y, 'Home', 4,
          clTrGray32);

        SetLength(FTargetAreas, 1);
        repeat
          TargetPoint.X := 8 + Random(Image32.Bitmap.Width - 16);
          TargetPoint.Y := 8 + Random(Image32.Bitmap.Height - 16);
        until not PtInRect(FTargetAreas[0].Rect, TargetPoint);

        FTargetAreas[0] := BuildAreaTarget(TargetPoint);
      end;
    5:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Draw', 'Home', 'Turn',
          'TurnLeft', 'TurnRight', 'CursorAngle']);

        FTutorialText :=
          'The state ''CursorAngle'' represents the angle of the ' +
          'cursor. This variable can be used to determine the current ' +
          'angle and to set a new absolute angle.' + #10 +
          'A value can be assigned by using the '':='' operator. ' + #10 +
          'For example ''CursorAngle := 45;'' turns the cursor to north-west ' +
          'independent of the previous direction.' + #10#10 +
          'Try this out to move the cursor to the red target square!';

        SetLength(FTargetAreas, 1);

        repeat
          TargetPoint.X := 8 + Random(Image32.Bitmap.Width - 16);
          TargetPoint.Y := 8 + Random(Image32.Bitmap.Height - 16);
        until not PtInRect(FTargetAreas[0].Rect, TargetPoint);

        FTargetAreas[0] := BuildAreaTarget(TargetPoint);
      end;
    6:
      begin
        // configure actions
        ActionScriptAutoRun.Checked := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Go', 'Draw', 'Home', 'Turn',
          'TurnLeft', 'TurnRight', 'CursorAngle']);

        FTutorialText :=
          'To make editing easier you can cut, copy & paste source ' +
          'code by using the typical shortcuts. In addition you can find ' +
          'buttons for this in the toolbar and in the main menu.' + #10 +
          'You can also find a certain code fragment by using the search ' +
          'dialog.' + #10 +
          'Now try to reach the destination with just one script call! ' +
          'While this is quite difficult, it is very important as it can ' +
          'only be achieved with one program and not by executing a snipped ' +
          'several times as the target will change randomly after each fail.' +
          #10#10 + 'Note: The faint 10x10 grid should help you here.' + #10;

        for Y := 0 to (Image32.Bitmap.Height div 10) - 1 do
          for X := 0 to (Image32.Bitmap.Width div 10) - 1 do
            Image32.Bitmap.Pixel[X * 10, Y * 10] := clTrGray32;

        SetLength(FTargetAreas, 1);

        repeat
          TargetPoint.X := 8 + Random(Image32.Bitmap.Width - 16);
          TargetPoint.Y := 8 + Random(Image32.Bitmap.Height - 16);
        until not PtInRect(FTargetAreas[0].Rect, TargetPoint);

        FTargetAreas[0] := BuildAreaTarget(TargetPoint);
      end;
    7:
      begin
        // configure actions
        ActionOutputSaveOutputAs.Visible := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputCursorVisible.Visible := False;
        ActionOutputAntialiased.Checked := True;

        FTurtleCursor.Color := clBlack32;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Home', 'Go', 'Draw', 'TurnLeft',
          'TurnRight', 'Turn', 'CursorAngle', 'CursorColor', 'ComposeColor']);

        FTutorialText :=
          'In addition to the ''CursorAngle'' there''s also ''CursorColor'' ' +
          'which allows you to set the color which is used by the cursor ' +
          'to draw the line.' + #10 +
          'There are several pre-defined colors available like ''clRed'', ' +
          '''clGreen'' and ''clBlue'' and so forth.' + #10 +
          'For example ''CursorColor := clRed'' sets the cursor color to red.' +
          #10#10 +
          'If you need a custom color you can use ''ComposeColor'' with the ' +
          'arguments (Red, Green, Blue: Float). All parameters are specified ' +
          'as real value between 0 and 1, with the convention of 0 being the ' +
          'darkest and 1 being the lightest.' + #10 +
          'Use ''CursorColor := ComposeColor(Random, 0, 1)'' to set a random ' +
          'color between blue and purple.' + #10#10 +
          'Change the cursor color at least five times and draw some lines ' +
          'to complete this lesson!';

        SetLength(FTargetAreas, 0);
      end;
    8:
      begin
        // configure actions
        ActionOutputSaveOutputAs.Visible := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputCursorVisible.Visible := False;
        ActionOutputAntialiased.Checked := True;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Home', 'Go', 'Draw', 'TurnLeft',
          'TurnRight', 'Turn', 'CursorAngle', 'CursorColor', 'ComposeColor']);

        FTutorialText :=
          'In order to enhance the interactive experience, the application ' +
          'can execute the script automatically after each change.' + #10 +
          'You can now enable the automatic run from the main menu ' +
          '(Script->Run always after changes). Once activated you need to ' +
          'press F9 only if you want to perform the script more than once.'#10 +
          'Since with this, dead locks might occur, you can abort execution ' +
          'by pressing CTRL + F2 or by waiting more than 5 minutes.'#10#10 +
          'Write a program that targets all the yellow areas and stops in ' +
          'the red target to complete this lesson!' + #10 +
          'Note: start with an absolute position by using ''Home;'' as first ' +
          'command.';

        SetLength(FTargetAreas, 4);

        TargetPoint.X := Image32.Bitmap.Width div 2;
        TargetPoint.Y := Image32.Bitmap.Height div 2;

        FTargetAreas[0] := BuildAreaTarget(TargetPoint);
        FTargetAreas[1] := BuildAreaTarget(TargetPoint, 40, 0);
        FTargetAreas[2] := BuildAreaTarget(TargetPoint, 50, 60);
        FTargetAreas[3] := BuildAreaTarget(TargetPoint, -50, 90);
      end;
    9:
      begin
        // configure actions
        ActionOutputSaveOutputAs.Visible := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputCursorVisible.Visible := False;
        ActionOutputAntialiased.Checked := True;

        // update suggestion whitelist
        SetSuggestionWhitelist(['Center', 'Home', 'Go', 'Draw', 'TurnLeft',
          'TurnRight', 'Turn', 'CursorAngle', 'CursorColor', 'ComposeColor']);

        FTutorialText :=
          'Now, let''s try to use the gained experience to draw a triangle!' +
          #10#10 + 'Note: The triangle must be drawn counter-clockwise.';

        SetLength(FTargetAreas, 4);

        TargetPoint.X := Image32.Bitmap.Width div 2;
        TargetPoint.Y := Image32.Bitmap.Height div 2;

        FTargetAreas[0] := BuildAreaTarget(TargetPoint, 0, 0);
        FTargetAreas[1] := BuildAreaTarget(TargetPoint,  50, -Round(sqrt(0.75) * 100));
        FTargetAreas[2] := BuildAreaTarget(TargetPoint, -50, -Round(sqrt(0.75) * 100));
        FTargetAreas[3] := BuildAreaTarget(TargetPoint, 0, 0);
      end;
    10:
      begin
        // configure actions
        ActionOutputSaveOutputAs.Visible := False;
        ActionOutputCursorVisible.Checked := True;
        ActionOutputCursorVisible.Visible := False;
        ActionOutputAntialiased.Checked := True;

        FTutorialText :=
          'While not being that important, it''s possible to change ' +
          'the line drawing quality. It can either be smooth (anti-aliased) ' +
          'or pixelized (anti-aliasing disabled).' + #10#10 +
          'This might become handy for later tutorials and can be set from ' +
          'the main menu (Output->Antialiasing Enabled/Disabled) or via ' +
          'code by using ''AntiAliasedLine''' + #10#10 +
          'To complete this lesson, disable anti-aliasing and redraw the ' +
          'triangle.';
        SetLength(FTargetAreas, 4);

        TargetPoint.X := Image32.Bitmap.Width div 2;
        TargetPoint.Y := Image32.Bitmap.Height div 2;

        FTargetAreas[0] := BuildAreaTarget(TargetPoint, 0, 0);
        FTargetAreas[1] := BuildAreaTarget(TargetPoint,  50, -Round(sqrt(0.75) * 100));
        FTargetAreas[2] := BuildAreaTarget(TargetPoint, -50, -Round(sqrt(0.75) * 100));
        FTargetAreas[3] := BuildAreaTarget(TargetPoint, 0, 0);
      end;
    11:
      begin
        FTurtleCursor.Home(True);

        ResetSuggestionWhitelist;

        // configure actions
        ActionOutputSaveOutputAs.Visible := True;
        ActionOutputCursorVisible.Visible := True;

        FTutorialText := 'Now you''re on your own.';

        SetLength(FTargetAreas, 0);
      end;
    else
      begin
        FreeAndNil(FTutorialStatistics);
        FTutorialIndex := -1;
        FTurtleCursor.Home(True);

        ResetSuggestionWhitelist;

        // configure actions
        ActionOutputSaveOutputAs.Visible := True;
        ActionOutputCursorVisible.Visible := True;
        ActionOutputAntialiased.Visible := True;

        FTutorialText := '';

        SetLength(FTargetAreas, 0);
      end;
  end;
end;

procedure TFormMain.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);

  procedure DrawFramedRect(Rect: TRect; Color: TColor32);
  begin
    Buffer.FillRectTS(Rect.Left + 1, Rect.Top + 1, Rect.Right - 1,
      Rect.Bottom - 1, SetAlpha(Color, Color shr 25));
    Buffer.FrameRectTS(Rect, Color);
  end;

var
  Canvas32: TCanvas32;
  Brush: TSolidBrush;
  Index: Integer;
  s, c: Single;
  Pnts: TArrayOfFloatPoint;
begin
  // eventually render tutorial stuff
  if (StageNum = 1) and (FTutorialIndex >= 0) then
  begin
    DrawFramedRect(FLastArea, clTrBlue32);

    // eventually render targer areas
    for Index := 0 to Length(FTargetAreas) - 1 do
      DrawFramedRect(FTargetAreas[Index].Rect, $7FFF0000);

    // eventually render tutorial text
    if (FTutorialText <> '') then
    begin
      Canvas32 := TCanvas32.Create(Buffer);
      try
        Canvas32.Renderer := TPolygonRenderer32LCD2.Create(Buffer);
        Brush := TSolidBrush(Canvas32.Brushes.Add(TSolidBrush));
        Brush.Visible := True;
        Brush.FillColor := SetAlpha(clDimGray32, $C0);
        Buffer.Font.Size := 12;

        Canvas32.RenderText(FloatRect(Buffer.ClipRect), FTutorialText,
          DT_LEFT + DT_WORDBREAK);
      finally
        Canvas32.Free;
      end;
    end;
  end
  else if FTurtleCursor.Visible and (StageNum = 6) then
  begin
    SetLength(Pnts, 3);

    with FTurtleCursor do
    begin
      SinCos(Angle, 6, s, c);
      Pnts[0] := PointF(Position.X + c, Position.Y + S);

      SinCos(Angle + 2.5, 6, s, c);
      Pnts[1] := PointF(Position.X + c, Position.Y + s);

      SinCos(Angle - 2.5, 6, s, c);
      Pnts[2] := PointF(Position.X + c, Position.Y + s);
    end;

    PolygonFS(Buffer, Pnts, FPolygonInverter);
    PolylineFS(Buffer, Pnts, FTurtleCursor.Color, True);
  end;
end;

procedure TFormMain.Image32Resize(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
  FTurtleCanvas.Clear;

  if ActionScriptAutoRun.Checked then
    RunScript;
end;

procedure TFormMain.ListBoxCompilerClick(Sender: TObject);
var
  CompilerMessage: TdwsMessage;
begin
  // check if item is selected at all
  if ListBoxCompiler.ItemIndex < 0 then
    Exit;

  with ListBoxCompiler do
    CompilerMessage := TdwsMessage(Items.Objects[ItemIndex]);

  if CompilerMessage is TScriptMessage then
  begin
    SynEdit.GotoLineAndCenter(TScriptMessage(CompilerMessage).Line);
    SynEdit.CaretX := TScriptMessage(CompilerMessage).Col;
  end;
end;

procedure TFormMain.LogCall(CallType: TStatistics.TLogCall; ClassAccess: Boolean = False);
begin
  FStatistics.LogCall(CallType, ClassAccess);
  if Assigned(FTutorialStatistics) then
    FTutorialStatistics.LogCall(CallType, ClassAccess);
end;

procedure TFormMain.ActionScriptAutoRunExecute(Sender: TObject);
begin
(*
  if ActionAutoRun.Checked then
    ActionAutoRun.ImageIndex := 23
  else
    ActionAutoRun.ImageIndex := 22;
*)
end;

procedure TFormMain.ActionFileNewExecute(Sender: TObject);
begin
  if FModified and (MessageDlg('The code has been modified.'#13#10#13#10 +
    'Do you want to saved it first?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    ActionFileSaveScriptAs.Execute;

  SynEdit.Text := '';
end;

procedure TFormMain.ActionFileOpenAccept(Sender: TObject);
begin
  SynEdit.Lines.LoadFromFile(ActionFileOpen.Dialog.Filename);
  FBackgroundCompilationThread.ScheduleCompilation;
end;

procedure TFormMain.ActionFileOpenBeforeExecute(Sender: TObject);
begin
  if FModified and (MessageDlg('The code has been modified.'#13#10#13#10 +
    'Do you want to saved it first?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    ActionFileSaveScriptAs.Execute;
end;

procedure TFormMain.ActionOutputSaveOutputAsAccept(Sender: TObject);
begin
  with ActionOutputSaveOutputAs do
    case Dialog.FilterIndex of
      1:
        if LowerCase(ExtractFileExt(Dialog.Filename)) = '.png' then
          SaveBitmap32ToPNG(Image32.Bitmap, Dialog.Filename)
        else
          Image32.Bitmap.SaveToFile(Dialog.Filename);
      3:
        SaveBitmap32ToPNG(Image32.Bitmap, Dialog.Filename);
      else
        Image32.Bitmap.SaveToFile(Dialog.Filename);
    end;
end;

procedure TFormMain.ActionFileSaveScriptAsAccept(Sender: TObject);
begin
  SynEdit.Lines.SaveToFile(ActionFileSaveScriptAs.Dialog.Filename);
  FModified := False;
end;

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormMain.ActionHelpDocumentationExecute(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'Documentation.pdf', nil, nil, 0);
end;

procedure TFormMain.ActionHelpTutorialExecute(Sender: TObject);
begin
  if FTutorialIndex < 0 then
    PrepareTutorial(0)
  else
    PrepareTutorial(-1);
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
var
  SynEditorOptionsContainer: TSynEditorOptionsContainer;
begin
  SynEditorOptionsContainer := TSynEditorOptionsContainer.Create(nil);
  SynEditorOptionsContainer.Assign(SynEdit);
  SynEditOptionsDialog.Execute(SynEditorOptionsContainer);
  SynEdit.Assign(SynEditorOptionsContainer);
end;

procedure TFormMain.ActionOutputAntialiasedExecute(Sender: TObject);
begin
  FTurtleCursor.AntiAliased := TAction(Sender).Checked;
  RunScript;
end;

procedure TFormMain.ActionOutputAntialiasedUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FTurtleCursor.AntiAliased;

  if TAction(Sender).Checked then
  begin
    TAction(Sender).ImageIndex := 18;
    TAction(Sender).Caption := 'Antialiasing Enabled';
  end
  else
  begin
    TAction(Sender).ImageIndex := 17;
    TAction(Sender).Caption := 'Antialiasing Disabled';
  end;
end;

procedure TFormMain.ActionOutputCursorVisibleExecute(Sender: TObject);
begin
  FTurtleCursor.Visible := TAction(Sender).Checked;
  Image32.Invalidate;
end;

procedure TFormMain.ActionOutputCursorVisibleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FTurtleCursor.Visible;
  if TAction(Sender).Checked then
    TAction(Sender).ImageIndex := 15
  else
    TAction(Sender).ImageIndex := 16;
end;

procedure TFormMain.ActionScriptAbortExecute(Sender: TObject);
begin
  if Assigned(FExecutionThread) then
  begin
    FExecutionThread.Abort;
    FExecutionThread.Terminate;
    FExecutionThread := nil;
  end;
end;

procedure TFormMain.ActionScriptAbortUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FExecutionThread);
end;

procedure TFormMain.ActionScriptCompileExecute(Sender: TObject);
begin
  CompileScript;
end;

procedure TFormMain.ActionScriptJustInTimeExecute(Sender: TObject);
begin
  //
end;

procedure TFormMain.ActionScriptJustInTimeUpdate(Sender: TObject);
begin
  {$IFDEF Debug}
  TAction(Sender).Visible := True;
  {$ELSE}
  TAction(Sender).Visible := False
  {$ENDIF}
end;

procedure TFormMain.ActionScriptRunExecute(Sender: TObject);
begin
  RunScript(ActionScriptJustInTime.Checked);
end;

procedure TFormMain.ActionOutputSizeVGAExecute(Sender: TObject);
begin
  SynEdit.Width := SynEdit.Width - (640 - Image32.Width);
  PageControl.Height := PageControl.Height - (480 - Image32.Height);
end;

procedure TFormMain.ActionOutputSizeVGAUpdate(Sender: TObject);
begin
  {$IFDEF Debug}
  TAction(Sender).Visible := True;
  TAction(Sender).Checked := (Image32.Width = 640) and (Image32.Height = 480);
  {$ELSE}
  TAction(Sender).Visible := False
  {$ENDIF}
end;

procedure TFormMain.CursorPositionChangedHandler(Sender: TObject);
var
  Index: Integer;
begin
  // perform extra tutorial checks
  if (FTutorialIndex >= 0) then
    for Index := Low(FTargetAreas) to High(FTargetAreas) do
      with FTargetAreas[Index] do
        Passed := PtInRect(Rect, FTurtleCursor.Position);
end;

procedure TFormMain.SetSuggestionWhitelist(Items: array of string);
var
  Index: Integer;
begin
  FSuggestionWhiteList.Clear;
  for Index := Low(Items) to High(Items) do
    FSuggestionWhiteList.Add(Items[Index]);
end;

procedure TFormMain.SynCodeSuggestionsClose(Sender: TObject);
begin
  FSuggestions := nil;
end;

procedure TFormMain.SynCodeSuggestionsExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
  function ContainsBlacklistedWord(const Text: string): Boolean;
  var
    Index: Integer;
  const
    CBlackList: array [0 .. 5] of string = ('Ansi', 'Cosh', 'Sinh', 'Tanh',
      'ISO8601', 'var');
  begin
    Result := False;
    for Index := Low(CBlackList) to High(CBlackList) do
      if Pos(CBlackList[Index], Text) > 0 then
        Exit(True);
  end;

  function ContainsWhitelistedWord(const Text: string): Boolean;
  var
    Index: Integer;
  begin
    Result := False;
    for Index := 0 to FSuggestionWhiteList.Count - 1 do
      if StartsStr(FSuggestionWhiteList[Index], Text) then
        Exit(True);
  end;

var
  CaretPos: TBufferCoord;
  Index, TopPos: Integer;
  Token: string;
  CodeSuggestions: TSynCompletionProposal;
  Attrib: TSynHighlighterAttributes;
  ScriptSourceItem: TScriptSourceItem;
  ScriptPos: TScriptPos;
begin
  Assert(Sender is TSynCompletionProposal);

  CanExecute := False;

  // use this handler only in case the kind is set to ctCode!
  Assert(Kind = ctCode);

  // clear code suggestions lists
  CodeSuggestions := TSynCompletionProposal(Sender);
  CodeSuggestions.InsertList.Clear;
  CodeSuggestions.ItemList.Clear;

  // update form
  if Assigned(CodeSuggestions.Form) then
  begin
    CodeSuggestions.Form.DoubleBuffered := True;
    CodeSuggestions.Resizeable := True;
    CodeSuggestions.Form.Resizeable := True;
    CodeSuggestions.Form.BorderStyle := bsSizeToolWin;
    if CodeSuggestions.Form.Visible then
      Exit;
  end;

  // get current editor position
  CaretPos := TSynCompletionProposal(Sender).Editor.CaretXY;

  // don't invoke code completion within a string / comment
  SynEdit.GetHighlighterAttriAtRowCol(CaretPos, Token, Attrib);
  if Assigned(Attrib) and (SameText(Attrib.Name, 'string') or
    SameText(Attrib.Name, 'comment')) then
    Exit;

  // ensure the source list is present
  if not Assigned(FCompiledProgram.SourceList) then
    Exit;

  // get script source item
  ScriptSourceItem := FCompiledProgram.SourceList.MainScript;
  if not Assigned(ScriptSourceItem) then
    Exit;

  // setup script position
  ScriptPos := TScriptPos.Create(ScriptSourceItem.SourceFile, CaretPos.Line,
    CaretPos.Char);

  // and create suggestions element
  FSuggestions := TdwsSuggestions.Create(FCompiledProgram, ScriptPos, [soNoUnits]);

  TopPos := 0;

  // now populate the suggestion box
  for Index := 0 to FSuggestions.Count - 1 do
    if (FSuggestions.Caption[Index] <> '') and (FSuggestions.Symbols[Index].Name <> '') then
    begin
      Assert(FSuggestions.Symbols[Index] is TSymbol);
      if ContainsWhitelistedWord(FSuggestions.Caption[Index]) then
      begin
        CodeSuggestions.ItemList.InsertObject(TopPos, FSuggestions.Caption[Index],
          TObject(FSuggestions.Category[Index]));
        CodeSuggestions.InsertList.InsertObject(TopPos, FSuggestions.Code[Index],
          FSuggestions.Symbols[Index]);
        Inc(TopPos);
      end
      else
      begin
        CodeSuggestions.ItemList.AddObject(FSuggestions.Caption[Index],
          TObject(FSuggestions.Category[Index]));
        CodeSuggestions.InsertList.AddObject(FSuggestions.Code[Index],
          FSuggestions.Symbols[Index]);
      end
    end;

  // allow execution if at least one item is available
  CanExecute := CodeSuggestions.ItemList.Count > 0;
end;

procedure TFormMain.SynParametersExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);

  procedure GetParameterInfosForCursor(const AProgram: IdwsProgram; Col,
    Line: Integer; var ParameterInfos: TStrings; InfoPosition: Integer = 0);

    procedure ParamsToInfo(const AParams: TParamsSymbolTable);
    var
      y: Integer;
      ParamsStr: string;
    begin
      ParamsStr := '';
      if (AParams <> nil) and (AParams.Count > 0) then
      begin
        if InfoPosition >= AParams.Count then
          Exit;

        ParamsStr := '"' + AParams[0].Description + ';"';
        for y := 1 to AParams.Count - 1 do
          ParamsStr := ParamsStr + ',"' + AParams[y].Description + ';"';
      end else
      if InfoPosition > 0 then
        Exit;

      if (ParameterInfos.IndexOf(ParamsStr) < 0) then
        ParameterInfos.Add(ParamsStr);
    end;

  var
    Overloads : TFuncSymbolList;

    procedure CollectMethodOverloads(methSym : TMethodSymbol);
    var
      Member: TSymbol;
      Struct: TCompositeTypeSymbol;
      LastOverloaded: TMethodSymbol;
    begin
      LastOverloaded := methSym;
      Struct := methSym.StructSymbol;
      repeat
        for Member in Struct.Members do
        begin
          if not UnicodeSameText(Member.Name, methSym.Name) then
            Continue;
          if not (Member is TMethodSymbol) then
            Continue;

          LastOverloaded := TMethodSymbol(Member);
          if not Overloads.ContainsChildMethodOf(LastOverloaded) then
            Overloads.Add(LastOverloaded);
        end;

        Struct := Struct.Parent;
      until (Struct = nil) or not LastOverloaded.IsOverloaded;
    end;

  var
    ItemIndex: Integer;
    FuncSymbol: TFuncSymbol;

    SymbolDictionary: TdwsSymbolDictionary;
    Symbol, TestSymbol: TSymbol;
  begin
    // make sure the string list is present
    Assert(Assigned(ParameterInfos));

    // ensure a compiled program is assigned
    if not Assigned(AProgram) then
      Exit;

    SymbolDictionary := AProgram.SymbolDictionary;
    Symbol := SymbolDictionary.FindSymbolAtPosition(Col, Line, MSG_MainModule);

    if (Symbol is TSourceMethodSymbol) then
    begin
      Overloads := TFuncSymbolList.Create;
      try
        CollectMethodOverloads(TSourceMethodSymbol(Symbol));
        for ItemIndex := 0 to Overloads.Count - 1 do
        begin
          FuncSymbol := Overloads[ItemIndex];
          ParamsToInfo(FuncSymbol.Params);
        end;
      finally
        Overloads.Free;
      end;
    end else
    if (Symbol is TFuncSymbol) then
    begin
      ParamsToInfo(TFuncSymbol(Symbol).Params);

      if TFuncSymbol(Symbol).IsOverloaded then
      begin
        for ItemIndex := 0 to SymbolDictionary.Count - 1 do
        begin
          TestSymbol := SymbolDictionary.Items[ItemIndex].Symbol;

          if (TestSymbol.ClassType = Symbol.ClassType) and
            SameText(TFuncSymbol(TestSymbol).Name, TFuncSymbol(Symbol).Name) and
            (TestSymbol <> Symbol) then
            ParamsToInfo(TFuncSymbol(TestSymbol).Params);
        end;
      end
    end;

    // check if no parameters at all is an option, if so: replace and move to top
    ItemIndex := ParameterInfos.IndexOf('');
    if ItemIndex >= 0 then
    begin
      ParameterInfos.Delete(ItemIndex);
      ParameterInfos.Insert(0, '"<no parameters required>"');
    end;
  end;


var
  LineText: String;
  Proposal: TSynCompletionProposal;
  LocLine: string;
  TmpX: Integer;
  TmpLocation, StartX, ParenCounter: Integer;
  ParameterInfoList: TStrings;
begin
  CanExecute := False;
  Assert(Kind = ctParams);

  (* check the proposal type *)
  if (Sender <> nil) and (Sender is TSynCompletionProposal) then
  begin
    Proposal := TSynCompletionProposal(Sender);
    Proposal.InsertList.Clear;
    Proposal.ItemList.Clear;
    ParameterInfoList := TStrings(Proposal.ItemList);

    (* get text @ current line *)
    LineText := SynEdit.LineText;

    with TSynCompletionProposal(Sender).Editor do
    begin
      (* First check if the active PageControl frame is actually
        a pascal unit. It could be a JS frame or a resource frame *)

      // get current compiled program
      if not Assigned(FCompiledProgram) then
        Exit;

      LocLine := LineText;

      //go back from the cursor and find the first open paren
      TmpX := CaretX;
      if TmpX > Length(LocLine) then
        TmpX := Length(LocLine)
      else Dec(TmpX);
      TmpLocation := 0;

      while (TmpX > 0) and not CanExecute do
      begin
        if LocLine[TmpX] = ',' then
        begin
          Inc(TmpLocation);
          Dec(TmpX);
        end else if LocLine[TmpX] = ')' then
        begin
          // we found a close, go till it's opening paren
          ParenCounter := 1;
          Dec(TmpX);
          while (TmpX > 0) and (ParenCounter > 0) do
          begin
            if LocLine[TmpX] = ')' then
              Inc(ParenCounter)
            else
            if LocLine[TmpX] = '(' then
              Dec(ParenCounter);
            Dec(TmpX);
          end;
          if TmpX > 0 then Dec(TmpX);  // eat the open paren
        end else if LocLine[TmpX] = '(' then
        begin
          // we have a valid open paren, lets see what the word before it is
          StartX := TmpX;
          while (TmpX > 0) and not IsIdentChar(LocLine[TmpX])do
            Dec(TmpX);
          if TmpX > 0 then
          begin
            while (TmpX > 0) and IsIdentChar(LocLine[TmpX]) do
              Dec(TmpX);
            Inc(TmpX);

            GetParameterInfosForCursor(FCompiledProgram, TmpX, SynEdit.CaretY,
              ParameterInfoList, TmpLocation);

            CanExecute := ParameterInfoList.Count > 0;

            if not CanExecute then
            begin
              TmpX := StartX;
              Dec(TmpX);
            end
            else
              TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
          end;
        end else Dec(TmpX)
      end;
    end;
  end;
end;

procedure TFormMain.SynCodeSuggestionsPaintItem(Sender: TObject;
  Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
  var CustomDraw: Boolean);
var
  Offset: TPoint;
  ItemName, ItemHighlight: string;
  Symbol: TSymbol;
  OldColor: TColor;
  Rct: TRect;
begin
  inherited;

  if Assigned(SynCodeSuggestions.Images) then
  begin
    TargetCanvas.FillRect(ItemRect);

    ItemHighlight := SynCodeSuggestions.InsertList[index];
    ItemName := SynCodeSuggestions.ItemList[index];

    case TdwsSuggestionCategory(SynCodeSuggestions.ItemList.Objects[index]) of
      scUnit:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 0);
      scType:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 1);
      scRecord, scClass:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 2);
      scInterface:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 3);
      scFunction:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 4);
      scProcedure:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 5);
      scDestructor, scMethod:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 6);
      scConstructor:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 7);
      scProperty:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 8);
      scEnum:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 9);
      scParameter:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 10);
      scConst, scVariable:
        begin
          Assert(SynCodeSuggestions.InsertList.Objects[index] is TSymbol);
          Symbol := TSymbol(SynCodeSuggestions.InsertList.Objects[index]);
          if Symbol is TConstSymbol then
          begin
            if Assigned(TConstSymbol(Symbol).Typ) then
              if (TConstSymbol(Symbol).Typ.Name = 'TColor') then
              begin
                Rct := Rect(ItemRect.Left + 1, ItemRect.Top + 1,
                  ItemRect.Left + ItemRect.Bottom - ItemRect.Top - 1,
                  ItemRect.Bottom - 1);

                OldColor := TargetCanvas.Brush.Color;
                TargetCanvas.Brush.Color := clBlack;
                TargetCanvas.FrameRect(Rct);
                TargetCanvas.Brush.Color := OldColor;

                InflateRect(Rct, -1, -1);
                OldColor := TargetCanvas.Brush.Color;
                TargetCanvas.Brush.Color := WinColor(Integer(TConstSymbol(Symbol).Data[0]));
                TargetCanvas.FillRect(Rct);
                TargetCanvas.Brush.Color := OldColor;
              end
          end;
        end;
      scReservedWord:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 12);
      scSpecialFunction:
        SynCodeSuggestions.Images.Draw(TargetCanvas, ItemRect.Left, ItemRect.Top, 13);
    end;

    Offset.x := ItemRect.Left + 18;
    Offset.y := ItemRect.Top;

    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    TargetCanvas.TextOut(Offset.X, Offset.Y, ItemHighlight);
    Delete(ItemName, 1, Length(ItemHighlight));
    Inc(Offset.X, TargetCanvas.TextWidth(ItemHighlight));
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
    TargetCanvas.TextOut(Offset.X, Offset.Y, ItemName);

    CustomDraw := True;
  end;
end;

procedure TFormMain.SynCodeSuggestionsShow(Sender: TObject);
var
  CompletionProposalForm: TSynBaseCompletionProposalForm;
begin
  inherited;

  if (Sender <> nil) and (Sender is TSynBaseCompletionProposalForm) then
  begin
    CompletionProposalForm := TSynBaseCompletionProposalForm(Sender);
    try
      CompletionProposalForm.DoubleBuffered := True;

      if CompletionProposalForm.Height > 300 then
        CompletionProposalForm.Height := 300
    except
      on Exception do;
    end;
  end;
end;

procedure TFormMain.SynEditChange(Sender: TObject);
begin
  FBackgroundCompilationThread.ScheduleCompilation;
  FModified := True;
end;

procedure TFormMain.SynEditGutterPaint(Sender: TObject; ALine, X, Y: Integer);
var
  StrLineNumber: string;
  LineNumberRect: TRect;
  GutterWidth, Offset: Integer;
  OldFont: TFont;
begin
  with TSynEdit(Sender), Canvas do
  begin
    Brush.Style := bsClear;
    GutterWidth := Gutter.Width - 5;
    if (ALine = 1) or (ALine = CaretY) or ((ALine mod 10) = 0) then
    begin
      StrLineNumber := IntToStr(ALine);
      LineNumberRect := Rect(x, y, GutterWidth, y + LineHeight);
      OldFont := TFont.Create;
      try
        OldFont.Assign(Canvas.Font);
        Canvas.Font := Gutter.Font;
        Canvas.TextRect(LineNumberRect, StrLineNumber, [tfVerticalCenter,
          tfSingleLine, tfRight]);
        Canvas.Font := OldFont;
      finally
        OldFont.Free;
      end;
    end
    else
    begin
      Canvas.Pen.Color := Gutter.Font.Color;
      if (ALine mod 5) = 0 then
        Offset := 5
      else
        Offset := 2;
      Inc(y, LineHeight div 2);
      Canvas.MoveTo(GutterWidth - Offset, y);
      Canvas.LineTo(GutterWidth, y);
    end;
  end;
end;

procedure TFormMain.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY] * Changes <> [] then
  begin
    StatusBar.Panels[0].Text := Format('%d : %d', [TSynEdit(Sender).CaretX,
      TSynEdit(Sender).CaretY]);
  end;
end;

procedure DrawBadge(Bitmap: TBitmap32; StarColor, BannerColor: TColor32;
  const Text: string = '');
var
  j, Size: Integer;
  Angle, c, s: Single;
  Star, Banner: TArrayOfFloatPoint;
  Brush: TSolidBrush;
  Canvas32: TCanvas32;
const
  Steps: Integer = 24;
begin
  Bitmap.Clear(0);

  Size := Min(Bitmap.Width, Bitmap.Height);

  SetLength(Star, Steps);
  Star[0] := FloatPoint(0.5 * Bitmap.Width, 0.68 * Bitmap.Height);
  Angle := 5 * Pi / Steps;
  for j := 0 to Steps - 2 do
  begin
    SinCos(-Angle, 2 * Size / Steps, s, c);
    Star[j + 1] := PointF(Star[j].X + c, Star[j].Y + s);
    Angle := Angle + ((2 * (j mod 2) - 1) * 4 + 1) * 2 * Pi / Steps;
  end;

  SetLength(Banner, 5);
  Banner[0] := FloatPoint(Star[2].X, Star[2].Y - 2 * Size / Steps);
  Banner[1] := FloatPoint(Banner[0].X, Banner[0].Y + 8 * Size / Steps);
  Banner[2] := FloatPoint(0.5 * Bitmap.Width, 0.5 * Size + 7 * Size / Steps);
  Banner[3] := FloatPoint(Bitmap.Width - Banner[1].X, Banner[1].Y);
  Banner[4] := FloatPoint(Bitmap.Width - Banner[0].X, Banner[0].Y);

  PolygonFS(Bitmap, Banner, BannerColor);
  PolylineFS(Bitmap, Banner, clBlack32, False, 2);
  PolygonFS(Bitmap, Star, StarColor);
  PolylineFS(Bitmap, Star, clBlack32, True, 2);

  if Text <> '' then
  begin
    Canvas32 := TCanvas32.Create(Bitmap);
    try
      Canvas32.Renderer := TPolygonRenderer32LCD2.Create(Bitmap);
      Brush := TSolidBrush(Canvas32.Brushes.Add(TSolidBrush));
      Brush.Visible := True;
      Brush.FillColor := clBlack32;
      Bitmap.Font.Size := 8;
      Bitmap.Font.Style := [fsBold];

      Canvas32.RenderText(12, 5, Text);
    finally
      FreeAndNil(Canvas32);
    end;
  end;
end;

procedure TFormMain.BuildBadges;
var
  Index: Integer;
  Bitmap: TBitmap32;
  Image32: TImage32;
begin
  ScrollBox.Visible := False;
  Exit;

  for Index := 0 to 10 do
  begin
    Bitmap := BadgeList.Bitmaps.Add.Bitmap;
    Bitmap.SetSize(32, 32);
    case Index of
      0:
        DrawBadge(Bitmap, clGreen32, clNavy32, 'C');
      else
        DrawBadge(Bitmap, clTeal32, clBlue32);
    end;
  end;

  Image32 := TImage32.Create(ScrollBox);
  Image32.Parent := ScrollBox;
  Image32.Bitmap.Assign(BadgeList.Bitmaps[0].Bitmap);
  Image32.Bitmap.DrawMode := dmBlend;
  Image32.Width := Image32.Bitmap.Width;
  Image32.Height := Image32.Bitmap.Height;
end;

initialization
  SetGamma(1);

end.
