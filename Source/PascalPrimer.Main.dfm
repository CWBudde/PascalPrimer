object FormMain: TFormMain
  Left = 0
  Top = 0
  ActiveControl = PageControl
  Caption = 'PascalPrimer'
  ClientHeight = 600
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 1000
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 64
      end
      item
        Width = 50
      end>
  end
  object PanelEditOutput: TPanel
    Left = 0
    Top = 22
    Width = 1000
    Height = 527
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterHorizontal: TSplitter
      Left = 593
      Top = 0
      Height = 527
    end
    object PanelOutput: TPanel
      Left = 596
      Top = 0
      Width = 404
      Height = 527
      Align = alClient
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      object SplitterVertical: TSplitter
        Left = 0
        Top = 331
        Width = 404
        Height = 3
        Cursor = crVSplit
        Align = alBottom
      end
      object PageControl: TPageControl
        Left = 0
        Top = 334
        Width = 404
        Height = 193
        ActivePage = TabSheetCompiler
        Align = alBottom
        TabOrder = 1
        object TabSheetCompiler: TTabSheet
          Caption = '&Compiler'
          object ListBoxCompiler: TListBox
            Left = 0
            Top = 0
            Width = 396
            Height = 165
            Align = alClient
            ItemHeight = 13
            PopupMenu = PopupMenuMessages
            TabOrder = 0
            OnClick = ListBoxCompilerClick
          end
        end
        object TabSheetOutput: TTabSheet
          Caption = '&Output'
          ImageIndex = 1
          object ListBoxOutput: TListBox
            Left = 0
            Top = 0
            Width = 396
            Height = 165
            Align = alClient
            ItemHeight = 13
            PopupMenu = PopupMenuMessages
            TabOrder = 0
          end
        end
      end
      object Image32: TImage32
        Left = 0
        Top = 0
        Width = 404
        Height = 331
        Align = alClient
        Bitmap.DrawMode = dmBlend
        Bitmap.OuterColor = -2830136
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        PopupMenu = PopupMenuOutput
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnPaintStage = Image32PaintStage
        OnResize = Image32Resize
      end
    end
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 593
      Height = 527
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 1
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = SynDWSSyn
      Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces]
      SearchEngine = SynEditSearch
      TabWidth = 2
      WantTabs = True
      OnChange = SynEditChange
      OnGutterPaint = SynEditGutterPaint
      OnSpecialLineColors = SynEditSpecialLineColors
      OnStatusChange = SynEditStatusChange
      FontSmoothing = fsmNone
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 549
    Width = 1000
    Height = 32
    VertScrollBar.Visible = False
    Align = alBottom
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 2
    Visible = False
    object Badge: TPaintBox32
      Left = 0
      Top = 0
      Width = 32
      Height = 32
      TabOrder = 0
      OnPaintBuffer = BadgePaintBuffer
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 1000
    Height = 22
    AutoSize = True
    DoubleBuffered = True
    DrawingStyle = dsGradient
    GradientEndColor = clBtnFace
    GradientStartColor = clBtnFace
    Images = ImageListActions
    ParentDoubleBuffered = False
    TabOrder = 3
    Transparent = True
    object ToolButtonOpen: TToolButton
      Left = 0
      Top = 0
      Action = ActionFileOpen
      AutoSize = True
    end
    object ToolButtonSave: TToolButton
      Left = 23
      Top = 0
      Action = ActionFileSaveScriptAs
      AutoSize = True
    end
    object ToolButtonExit: TToolButton
      Left = 46
      Top = 0
      AutoSize = True
      Caption = 'ToolButtonExit'
      ImageIndex = 2
    end
    object ToolButtonSeparator1: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator1'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButtonCopy: TToolButton
      Left = 77
      Top = 0
      Action = ActionEditCopy
      AutoSize = True
    end
    object ToolButtonCut: TToolButton
      Left = 100
      Top = 0
      Action = ActionEditCut
      AutoSize = True
    end
    object ToolButtonPaste: TToolButton
      Left = 123
      Top = 0
      Action = ActionEditPaste
      AutoSize = True
    end
    object ToolButtonSelectAll: TToolButton
      Left = 146
      Top = 0
      Action = ActionEditSelectAll
      AutoSize = True
    end
    object ToolButtonUndo: TToolButton
      Left = 169
      Top = 0
      Action = ActionEditUndo
      AutoSize = True
    end
    object ToolButtonSeparator2: TToolButton
      Left = 192
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator2'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButtonOptions: TToolButton
      Left = 200
      Top = 0
      Action = ActionOptions
      AutoSize = True
    end
    object ToolButtonSeparator3: TToolButton
      Left = 223
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator3'
      ImageIndex = 12
      Style = tbsSeparator
    end
    object ToolButtonFind: TToolButton
      Left = 231
      Top = 0
      Action = ActionSearchFind
      AutoSize = True
    end
    object ToolButtonSeparator4: TToolButton
      Left = 254
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator4'
      ImageIndex = 14
      Style = tbsSeparator
    end
    object ToolButtonCompile: TToolButton
      Left = 262
      Top = 0
      Action = ActionScriptCompile
      AutoSize = True
    end
    object ToolButtonRun: TToolButton
      Left = 285
      Top = 0
      Action = ActionScriptRun
    end
  end
  object DelphiWebScript: TDelphiWebScript
    Config.CompilerOptions = [coOptimize, coSymbolDictionary, coContextMap, coAssertions]
    Config.OnInclude = DelphiWebScriptInclude
    Config.OnNeedUnit = DelphiWebScriptNeedUnit
    Left = 72
    Top = 16
  end
  object SynDWSSyn: TSynDWSSyn
    DefaultFilter = 'DWScript Files (*.dws;*.pas;*.inc)|*.dws;*.pas;*.inc'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    StringAttri.Foreground = clPurple
    Left = 240
    Top = 16
  end
  object SynMacroRecorder: TSynMacroRecorder
    Editor = SynEdit
    RecordShortCut = 24658
    PlaybackShortCut = 24656
    Left = 328
    Top = 16
  end
  object SynEditOptionsDialog: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 240
    Top = 72
  end
  object SynEditSearch: TSynEditSearch
    Left = 328
    Top = 72
  end
  object SynCodeSuggestions: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoTitleIsCentered, scoUseInsertList, scoEndCharCompletion, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = ImageListSuggestion
    OnClose = SynCodeSuggestionsClose
    OnExecute = SynCodeSuggestionsExecute
    OnPaintItem = SynCodeSuggestionsPaintItem
    OnShow = SynCodeSuggestionsShow
    ShortCut = 16416
    Editor = SynEdit
    Left = 424
    Top = 72
  end
  object MainMenu: TMainMenu
    Images = ImageListActions
    Left = 152
    Top = 16
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemFileNew: TMenuItem
        Action = ActionFileNew
      end
      object MenuItemFileOpen: TMenuItem
        Action = ActionFileOpen
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = ActionFileSaveScript
      end
      object MenuItemFileSaveAs: TMenuItem
        Action = ActionFileSaveScriptAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemFileExit: TMenuItem
        Action = ActionFileExit
      end
    end
    object MenuItemEdit: TMenuItem
      Caption = '&Edit'
      object MenuItemEditCut: TMenuItem
        Action = ActionEditCut
      end
      object MenuItemEditCopy: TMenuItem
        Action = ActionEditCopy
      end
      object MenuItemEditPaste: TMenuItem
        Action = ActionEditPaste
      end
      object MenuItemEditDelete: TMenuItem
        Action = ActionEditDelete
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItemEditUndo: TMenuItem
        Action = ActionEditUndo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuItemEditSelectAll: TMenuItem
        Action = ActionEditSelectAll
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuItemOptions: TMenuItem
        Action = ActionOptions
      end
    end
    object MenuItemSearch: TMenuItem
      Caption = '&Search'
      object MenuItemSearchFind: TMenuItem
        Action = ActionSearchFind
      end
      object MenuItemSearchSearchAgain: TMenuItem
        Action = ActionSearchFindNext
      end
    end
    object MenuItemScript: TMenuItem
      Caption = '&Script'
      object MenuItemScriptRun: TMenuItem
        Action = ActionScriptRun
      end
      object MenuScriptAutomaticallyCompile: TMenuItem
        Action = ActionScriptAutoRun
        AutoCheck = True
      end
      object MenuItemScriptAbort: TMenuItem
        Action = ActionScriptAbort
      end
      object N7: TMenuItem
        Caption = '-'
        Visible = False
      end
      object MenuScriptCompile: TMenuItem
        Action = ActionScriptCompile
      end
      object MenuItemScriptJIT: TMenuItem
        Action = ActionScriptJustInTime
        AutoCheck = True
      end
    end
    object MenuItemOutput: TMenuItem
      Caption = '&Output'
      object MenuItemOutputAntialiased: TMenuItem
        Action = ActionOutputAntialiased
        AutoCheck = True
      end
      object MenuItemCursorVisible: TMenuItem
        Action = ActionOutputCursorVisible
        AutoCheck = True
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuItemOutputSaveAs: TMenuItem
        Action = ActionOutputSaveOutputAs
      end
      object MenuItemVGA: TMenuItem
        Action = ActionOutputSizeVGA
      end
    end
    object MenuItemHelp: TMenuItem
      Caption = '&Help'
      object MenuItemHelpTutorial: TMenuItem
        Action = ActionHelpTutorial
      end
      object MenuItemHelpDocumentation: TMenuItem
        Action = ActionHelpDocumentation
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MenuItemHelpAbout: TMenuItem
        Action = ActionHelpAbout
      end
    end
  end
  object ActionList: TActionList
    Images = ImageListActions
    Left = 152
    Top = 72
    object ActionEditCut: TEditCut
      Category = 'Edit'
      Caption = '&Cut'
      Hint = 'Cut|Cut selection to clipboard'
      ImageIndex = 3
      ShortCut = 16472
    end
    object ActionFileNew: TAction
      Category = 'File'
      Caption = 'New'
      ImageIndex = 26
      OnExecute = ActionFileNewExecute
    end
    object ActionEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copy selection to clipboard'
      ImageIndex = 4
      ShortCut = 16451
    end
    object ActionEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Paste content of clipboard'
      ImageIndex = 5
      ShortCut = 16470
    end
    object ActionEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &all'
      Hint = 'Select all|Select entire document'
      ImageIndex = 7
      ShortCut = 16449
    end
    object ActionEditDelete: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Delete selection'
      ImageIndex = 6
      ShortCut = 46
    end
    object ActionEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Undo recent changes'
      ImageIndex = 8
      ShortCut = 16474
    end
    object ActionSearchFind: TSearchFind
      Category = 'Search'
      Caption = '&Find...'
      Hint = 'Search|Search for text'
      ImageIndex = 13
      ShortCut = 16454
    end
    object ActionFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '.dws'
      Dialog.Filter = 
        'Scripts (*.dws; *.pas)|*.dws;*.pas|Pascal files (*.pas)|*.pas|De' +
        'lphi Web Script (*.dws)|*.dws'
      Dialog.Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
      Hint = 'Open|Open script'
      ImageIndex = 0
      ShortCut = 16463
      BeforeExecute = ActionFileOpenBeforeExecute
      OnAccept = ActionFileOpenAccept
    end
    object ActionFileSaveScript: TAction
      Category = 'File'
      Caption = '&Save'
      ImageIndex = 1
      ShortCut = 16467
      OnExecute = ActionFileSaveScriptExecute
    end
    object ActionFileSaveScriptAs: TFileSaveAs
      Category = 'File'
      Caption = 'Save &as...'
      Dialog.DefaultExt = '.pas'
      Dialog.Filter = 'Pascal files (*.pas)|*.pas|Delphi Web Script (*.dws)|*.dws'
      Hint = 'Save as|Save active script'
      ImageIndex = 1
      OnAccept = ActionFileSaveScriptAsAccept
    end
    object ActionFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Close Application'
      ImageIndex = 2
    end
    object ActionOutputCursorVisible: TAction
      Category = 'Output'
      AutoCheck = True
      Caption = 'Cursor Visible'
      Checked = True
      ImageIndex = 15
      OnExecute = ActionOutputCursorVisibleExecute
      OnUpdate = ActionOutputCursorVisibleUpdate
    end
    object ActionOutputSaveOutputAs: TFileSaveAs
      Category = 'Output'
      Caption = 'Save &as...'
      Dialog.DefaultExt = '.bmp'
      Dialog.Filter = 
        'All graphics (*.bmp; *.png)|*.bmp; *.png|Bitmap (*.bmp)|*.bmp|Po' +
        'rtable Network Graphics (*.png)|*.png'
      Hint = 'Save as|Save current output'
      OnAccept = ActionOutputSaveOutputAsAccept
    end
    object ActionOptions: TAction
      Category = 'Edit'
      Caption = '&Options'
      ImageIndex = 10
      ShortCut = 121
      OnExecute = ActionOptionsExecute
    end
    object ActionScriptAutoRun: TAction
      Category = 'Script'
      AutoCheck = True
      Caption = '&Run always after changes'
      OnExecute = ActionScriptAutoRunExecute
    end
    object ActionScriptCompile: TAction
      Category = 'Script'
      Caption = '&Compile'
      ImageIndex = 14
      ShortCut = 16504
      Visible = False
      OnExecute = ActionScriptCompileExecute
    end
    object ActionSearchFindNext: TSearchFindNext
      Category = 'Search'
      Caption = 'Search &Again'
      Hint = 'Search Next|Repeats the last search'
      ShortCut = 114
    end
    object ActionOutputAntialiased: TAction
      Category = 'Output'
      AutoCheck = True
      Caption = 'Antialiasing Enabled'
      Checked = True
      ImageIndex = 18
      OnExecute = ActionOutputAntialiasedExecute
      OnUpdate = ActionOutputAntialiasedUpdate
    end
    object ActionHelpAbout: TAction
      Category = 'Help'
      Caption = '&About'
      ImageIndex = 19
      OnExecute = ActionHelpAboutExecute
    end
    object ActionHelpDocumentation: TAction
      Category = 'Help'
      Caption = '&Documentation'
      ImageIndex = 20
      OnExecute = ActionHelpDocumentationExecute
    end
    object ActionScriptRun: TAction
      Category = 'Script'
      Caption = '&Run'
      ImageIndex = 21
      ShortCut = 120
      OnExecute = ActionScriptRunExecute
    end
    object ActionScriptAbort: TAction
      Category = 'Script'
      Caption = 'Abort Execution'
      ImageIndex = 24
      ShortCut = 16497
      OnExecute = ActionScriptAbortExecute
      OnUpdate = ActionScriptAbortUpdate
    end
    object ActionHelpTutorial: TAction
      Category = 'Help'
      Caption = 'Tutorial'
      ImageIndex = 25
      OnExecute = ActionHelpTutorialExecute
    end
    object ActionOutputSizeVGA: TAction
      Category = 'Output'
      Caption = '640 x 480'
      OnExecute = ActionOutputSizeVGAExecute
      OnUpdate = ActionOutputSizeVGAUpdate
    end
    object ActionScriptJustInTime: TAction
      Category = 'Script'
      AutoCheck = True
      Caption = 'JIT Compilation'
      OnExecute = ActionScriptJustInTimeExecute
      OnUpdate = ActionScriptJustInTimeUpdate
    end
  end
  object PopupMenuOutput: TPopupMenu
    Left = 704
    Top = 240
    object MenuSaveOutputAs: TMenuItem
      Action = ActionOutputSaveOutputAs
    end
  end
  object PopupMenuMessages: TPopupMenu
    Left = 704
    Top = 396
    object MenuSaveMessagesAs: TMenuItem
      Caption = 'Save &as...'
      Hint = 'Save as|Save current messages'
      ImageIndex = 30
      OnClick = MenuSaveMessagesAsClick
    end
  end
  object SynParameters: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer]
    ClBackground = clInfoBk
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = ImageListSuggestion
    OnExecute = SynParametersExecute
    ShortCut = 24608
    Editor = SynEdit
    Left = 424
    Top = 16
  end
  object dwsUnitBasic: TdwsUnit
    Script = DelphiWebScript
    Functions = <
      item
        Name = 'Clear'
        Overloaded = True
        OnEval = dwsFunctionsClearEval
      end
      item
        Name = 'Turn'
        Parameters = <
          item
            Name = 'Angle'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 90
          end>
        OnEval = dwsFunctionsTurnLeftEval
      end
      item
        Name = 'Go'
        Parameters = <
          item
            Name = 'Distance'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 10.000000000000000000
          end>
        OnEval = dwsFunctionsGoEval
      end
      item
        Name = 'Draw'
        Parameters = <
          item
            Name = 'Distance'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 10.000000000000000000
          end>
        OnEval = dwsFunctionsDrawEval
      end
      item
        Name = 'Center'
        OnEval = dwsFunctionsCenterEval
      end
      item
        Name = 'Home'
        OnEval = dwsFunctionsHomeEval
      end
      item
        Name = 'Sine'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = dwsFunctionsSineEval
      end
      item
        Name = 'Cosine'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = dwsFunctionsCosineEval
      end
      item
        Name = 'Tangent'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = dwsFunctionsTangentEval
      end>
    Synonyms = <
      item
        Name = 'Single'
        DataType = 'Float'
      end
      item
        Name = 'Double'
        DataType = 'Float'
      end
      item
        Name = 'Byte'
        DataType = 'Integer'
      end
      item
        Name = 'Word'
        DataType = 'Integer'
      end
      item
        Name = 'SmallInt'
        DataType = 'Integer'
      end
      item
        Name = 'LongInt'
        DataType = 'Integer'
      end
      item
        Name = 'ShortInt'
        DataType = 'Integer'
      end
      item
        Name = 'Real'
        DataType = 'Float'
      end>
    UnitName = 'TurtleBasic'
    Variables = <
      item
        Name = 'CursorVisible'
        DataType = 'Boolean'
        OnReadVar = dwsVariablesCursorReadVar
        OnWriteVar = dwsVariablesCursorWriteVar
      end
      item
        Name = 'CursorAngle'
        DataType = 'Float'
        OnReadVar = dwsVariablesCursorAngleReadVar
        OnWriteVar = dwsVariablesCursorAngleWriteVar
      end>
    StaticSymbols = False
    Left = 72
    Top = 136
  end
  object ImageListActions: TPngImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    PngImages = <
      item
        Background = clWindow
        Name = 'Open'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C18000002304944415478DA
          95D36F4813711807F0EFDDADB52BB7B53FAEE56838343D4B7353E84DAF4C6D2F
          8240A88850E8CF9BC217CE887A95D66B6B5084BD0DAA17F52E28675B45415939
          7033FF70ADCD9AB51AEDCF6D11F4C271D7CF53B71D44630F3C70DC3DBF0FF77C
          B9A3B05EFB2E3E9748C583633DF5A8A2A88D8BCE0BCFA416AE160B8B3FEF87BC
          BDFD5503CEF301E9EA1917DECFFDC0C474F25ED8DB3B5015D03EEC97464EB910
          4BA4904EFFC1E36086DC958A630C43CBC334B5D60C4D3D088E751F5700974F3A
          F1683A86063D83C1FDB530D795E278FBFA15986D3B104A16A0A20A18F7A53073
          BD872AAD40804B03ED980C7D463E2360B4CB08C7AE6668B55AD0348D37535368
          6E6CC0D3701CE91515EEBC1010F62A8080E439D18A977371E4B339029860B258
          61B3D9E4E73CFF118D04F89513C027B218BC9BC06C39E022219E3DB21BEFF8AF
          C811E0CA0113D46C0D388E83288AF25B44221144A3512CC778DCFEE2C4076FB7
          1238DDD782994FDF8AC026CD56D8ED76B02C0BF28DC808B57EA4D513C0C28D83
          4AA0FF3087F9A5EF04108A80C56281C16090818D6218067B86FC58BCE92E011D
          043876A80991E524848C505C41A7D3C16AB556063A877D89266E7B5D8DA6804C
          2A8BD132C06C36CB87FE0B906A730D4D3C71D4EB7652E26F8C749580D515D46A
          754540CEA6C3E39B341B69DBAD3E13366FD142AFD7CB0D954631D846429C2F0F
          B11CD97BEEE135B046B7284AF2EE6BEB9732A0C9316625EF9F1D3FEAFE17B05A
          0ED2C60AFF91407AE92F8E65DB115A4387890000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Save'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000000473424954080808087C086488000000097048597300000B1300000B
          1301009A9C18000001394944415478DA6364A0103052CD009319B71632FCFB1F
          6B21F89931419B83E1FBF71F60714D7555B8E2DAADB7184E7D12683C9BA9DA80
          69C0B49BFF189898182DF83F32B8897D6278FCE205583CDCDB1BC580D39F0518
          FEFF67801B82E482DBFF41B4ECFF970C35F6C20CCF5EBE60E064E76030D0D202
          CBFF03EA4A5E7699E139B304987F26439511AB013F3FBE6650E4F9CB202DC081
          E2D7271F7E303CF8CACCC0CE278ADF006201ED0C10E66466D01165C188606010
          305C79FD87E1DDF7BFF80D58E02BCC20C1CD8CD5D6679FFF32246D7D8BDF801D
          11620CAFDFBC65F8FEED3B8A664E6E2E06516121068F15AF8833E0EBB76F2806
          7073010D1011C66D80F1946BFF195958C106E0032003FEFDF9C5702E471BD500
          9D96BD2B189958C24A4D39F1E68FEED3DFFFFFFEF37BD98D3A97181403C80514
          1B0000F3AC8C11B854DB160000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Exit'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002464944415478DAA5D27B4853511C07F0EF75AFBB975BCBE5B225B188
          AC14AD30F19FFE88F5472F90FAA3C07FFAC31E188154183D082B738AA364D5C8
          1E968CA2FCA322A307864C121F930CA9202A918408E79DB1ED7A6B0F773A778D
          74EEFE1174E0CBB95C7EE7730EE7FC18FCE760868EAF23FF5A5CDA34CC480272
          731E8242646CF3D9AE83E2CFD80C42741252E16982B44E90047CB56B8990C842
          C1463B3E7351EFA67D971C52C076A74978AA9F020E200D619A6BF793F5D1412C
          B118612EDB8AFB9E7BDEEAB6911369C01538A1406572C57C60E05809E1A309E4
          E568A0CD5F850677275A7D5CF95FA01EDB60464B43593D4E0E9ECEBCC4BE23C5
          49603105F4CB8AE0703F9E054E610BF2D1786E431D42B120E28918740A2D9432
          15B8881FAE8E6B605ED714FD0116AAA1B516C079EBC52CD08A91BAD233E8F3F7
          623ACE432D67C1D268151AE4AA17C1F5E03A989EC385241C994902ACC506D75D
          6FC6092A57EE061F0FC3A0CA064B7767E52A1894069CF73483E9AE5E4D8222B0
          80C570D61A7C9DE0D1D4FE24E30E6A4A0EA1A5E36AE61D341EAD22B6492F2C14
          F84672D0F52582B667FDE592AFF009204E92FE0ACFAB56901FD371E45240A137
          C1D3FD11378602E900ED03FB856CE1D5542813E8DCBB9C04F818728D2A28347A
          3CF48DC33DC06500B44ED87167942124BDF393DAED9DD677269DA250A664F1F2
          0387CBFD93C56247D344E7CC01F15B12B85961B5E958D91BB54A6EEC190DE362
          AFDF9E5A18A7E168C6697E89B5928038DA772DAD502B658FDE7EFF098777624F
          EAF8EF69C6E62E980FFC065C5E066E739EEA980000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Cut'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000029B4944415478DA63644003D5D5D5B1ADADAD8B197080B56BD77AFFFD
          FB77775858D82F109F115DC1D4A9535F3332324ECCCACA6A41975BB56A95343B
          3BFBD59F3F7FA6030D5889D5808E8E8E2FB6B6B6AC4097081F3870E00BB2DCFE
          FDFB4F090808985EBA74A9233E3EBE12AB01858585E7B3B3B30DF6EDDBB72D2D
          2DCD1B26BE68D1222F1B1B9BAD2F5FBE64B87CF9723D50AE09AB01414141B955
          555593B8B8B818A64C99623D6DDAB4630D0D0D6C4E4E4EAF949595F98F1E3DCA
          F0ECD933B7828282DD580D707070E0011AF2CAD7D797F3CC99334F4243436581
          B64F747777CFFBF8F123C3BA75EBEE2A2929A903C3E02F560340C0DFDF3F0F08
          266A6868800275724C4C4C96989818F39E3D7B18AE5DBB16D1D4D4B412A616AB
          0120101515B5ABA8A8C8F5D7AF5FFF819A197FFCF8018A85CD40CD7EC8EA701A
          606969C90974C1D99C9C1C4D60B4319C3A756ADD870F1F2281E1F18B280380B6
          B1696B6BBFDEBC7933AF9F9FDF3F60C8EF080F0FF7415787D3808653DD0BCE7D
          BE161FF0D036F42FC717C537266FBA2E6FBB63BFAC60D9219C06843E2EE4FCFA
          F4F9EE3FEC7FD4DE737E10FA7FECFF8D33497B7540728197437FFD66FDFBE3DF
          BF1F1FFEFFFA3675BBC1814E0C036C7638AD7F74F0D98D47ED372A65772B7DFF
          B1FB6FE9EBAE87534072769B2C8EFF62FC354FF6BBF2BC4B8257DFE8BDD71659
          1DB6FA2F8A015ACB556F3D9AF336E0CBDE77D724660BBFFBBA96D1F2F38E3737
          41728E534C1B9F70BE53BD9D7C374A77AEE66D6626A6D00B89572FA01820DEC3
          BDEACF0B063146C1BFFBFFFF602A7F7BE31B2FC36A067082F16CB3157DC8F2FC
          16FB7F66ED3FFF194EFD63FBAA77B5E8C93B1403B45731B03D3BC5B6E9DF2F06
          81DFCF181BBFADF9B91D59DEA841BFF333C3A72C967F2C8BAF37DDCE02890100
          EF482E20DAA23BF00000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Copy'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002684944415478DA8D93CB6F125114C6BF3B53460A65281D7935926A8C
          4AD4F85AF82076C1865DDDD86581F24FF81734A976891B76848455355D3436D0
          DA260D2410D999184934CE064C9AA81490373383772E098FA64D3C9B3BE7CE39
          BFFB9D2FF712D04824125B74896022082193DFF1C160100F8542459C09924C26
          45BACA7EBF5FE2BE97D1F25C06CFF3301A8D100481ADA552A9532814DE0683C1
          57E702285DF6F97C92F9DB4FE0E14DD6787A7A8A72B98C4AA5025555078AA274
          687D73A257571367004DD3E4402020198B25084FEE30402E9783C3E180ADD262
          D5CA9273D4D9ED7661329994E3E3E3FC08B0BABA2AF19F7F40787A971565B3D9
          73017AB3D3E90455A4A452A93180CE27F50B5F61787C9B15663219D86C3690DF
          35966B92887EBF0F5114992F56AB750CA033CAD4E129009507B7DB3D52D074CD
          B393A97496D7EBF521201A8D52A82887C3E129C0D1D111EC763BB83F75DD4498
          961699377A701CC73C383C3CBC18F0F1E000573C1E88BFFEC2366F83E9DE8D91
          89BD5E0F2727274A9E0603582C16797D7D7D04A09E209D4EB1111655030C0601
          FC55176BD67D68B7DB989B9B53D2E9F410401339128948BD4F5F401EDD6292F7
          F7F7E1F57A21D4861E70CE05B6DFE97460369B410F55F6F6F6C680B5B53549A1
          0AD4FBD759A13EC2F3E565D86B3D0650AFB9D0683474F7595EAD5695DDDDDD3C
          BBF0B1586C8BDEC6885105D79D219ABED76EB72C2B2B2F2EE9266A14D8B59A30
          3B3B3B32511F6167E77D7EFC62CEC49BD79B1F82A17060A1D232B4A96CCB03EF
          D47F7AD59577DBDB17033637365E822711EB807F469F0C9A33D0F4174A084715
          1070842B5225F10B01FF1BFF00B0E22FF3B00EADDC0000000049454E44AE4260
          82}
      end
      item
        Background = clWindow
        Name = 'Paste'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002334944415478DA7DD25D6852611807F0BF1E9B47C7DC11AC4DBA6A82
          69CB201814D4A284D5E62EB63E480882D88D5422C12E8A20EAD62E82ECC22870
          B08BA282AC4818B26A35851AD5D85CF9D187A315D10E6AF36B1EF5F876301A6B
          C7B3070E2FEFFF9CF7C7CB791E19D6298FC733242CEDC233E572B9C61B7D236B
          14BADDEE168D46B3A0D7EB152A958A6659365728149E3B1C8EA3EB025EAF7753
          B55A3D2397CBB50683E1ACC562A1FEBD0B0683D962B178831032EB743AEF3704
          46AF5F26DA8E2E501405B55A2DBA593E9F47EE770A1BBE4FA215F34CCF85F1A5
          FF8091ABC364B1CA800705A952C878ECA8BC01D5541203812BFDE453AD03BBFA
          4EC068348A0E2712094C8DDDC1562A290DFC509AB177600866B3590444A35184
          1EFBB0998BA289E6C4C0934BFDE4A7CA8CEE416960F2910FFAE5284A658E39EE
          5E03F82FDAC8AFE66DD877581A78E5F7A1ADF01115BE2C061E9CEF236C4B27F6
          1F9106261EFAB031F7010415317077B897A499BF80C964AA6742DF118944108F
          C7EB7BF6CB0C74E969508AAA18183DD74B96B49DE81E38B9D285743A8D502804
          ABD58A4C2683E0533F5A1726402B1B0023CE4324A7DB8EDD363B8449AC67D96C
          16E1701876BB1D814000C999D760D86994CB3CE3B8B506B87DFA2029B4EFC41E
          DB31D0345DCF4AA51262B1587D32398EC362EC2D9AD939D4F89A08D872D3D1F3
          95D399C0AB989550F805C2C0CB209353207C058AE51494A9CFB8F732DEF522F6
          EDDD6AA0EDDAA903CF84557A8E5755F0FDFCE0D86C32FE07C411012054AFE980
          0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000025B4944415478DA7593DF6B925118C7BFAFAFB3FCC16A2D6B88AD2EB6
          D89DEBA2884485D560E028BC8820BA8E2EFA17822EBA19F48328FA03964CB211
          A6602069D128E62835A2652E925CF36DE0AFA99BECF5D5D3398227DFAD3D7038
          CF7B38DF0FDFE739CF2BA027BC5EAF8E6E26A3D1B81F7B84C7E3C9F77E0B3DE2
          098D4613D56AB5D0EBF50D4110C8FF00F4FC83DBED9E5401A8788A8A5FD9ED76
          A4D3698C8F8FC36C36772E10F28F532E97B1B8B8B8323D3D7D9203A8F82215BF
          74381CB05AAD8844221CD02B6679A552D90DF0F97CC4E974C262B1740E18C066
          B3EDE9201E8FAB017EBF9F984C267EA95AADB246211C0E4396659503E632994C
          AA01C160709D5A3E6230183A97FAFBFBC11AB9D33E5B0C4E4B58A580610E0885
          4292CBE51A8A46A368369BBB6C77727A6EF4CD83487F201E1F6EBA03CF752A00
          EDC150B78C9DE2E57B0F919B7F81A34E3B445144A35024C5444A41BB3D73E9CB
          A75B1C108BC578CD5D48DF9B77E84BA470E199172D494233FBABB3B73402E2B3
          BE6DD26ACD70009D3E55BD6C2D5CBE8A83A323189D9C80BCF2034A2E0745CA83
          CDD8CF72AD5A2F94121CC07AD0EB80B0BAEF3F86E5DC599CBA731BE2C0002F4D
          C9AF61E9FA8DCDF27AA9CA01DD57E82E16EFAF5C83890A8F19F741F99D43BB51
          039137211E30202B1B37EAA58D780740DF973BE88AE9CC43F77601DAA58F3833
          751E4AE62BDA5B65083A019AD131245EA7B6DA8A725708040205EA609039E8F9
          6178FEFDD113649FCE6170E40434ED26B65B84D456A5CD5CA33E7633BDBCC600
          9FE9F3F0C9DA195D47E2EC9C8E148A020E1FAA3FA8144E6732DFA4D55CBEFA17
          7D046211CE87835A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Select All'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000000F34944415478DA63FCFFFF3F03258031B9F535032323E3
          0120B607E274A08133C11204D8407C10881D1863EB9F80C4FF737272A59362F3
          F7EFDF6682CD4BEB7807E2A791E983598CAEE967C02E50545422C905F7EFDF83
          B8C0327A1FD8005D5D03920CB87CF90295BCA0E6B106EC02070727925C70E0C0
          3E880B945C56830D70717126C9803D7BF652C90BEA9E3BC02EB0B73723C90507
          0F9E82B840D7FF28D8004B4B4D920C387EFC3A95BCC02F6ECEA0EBBEE2BF9616
          3F492EB876EDE3CCCB3B231819816C3943AFF56B0424AD4C555559D36FDFFE0D
          CE3484D81F9E1F3B7D7E5B6008C8007E20E6006266129DFF17887F0000F29C76
          F92A5A7AD40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Undo'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002314944415478DA6364A010300E4E0354D7BEE0FFF98F211CC80C0162
          51201602E2B3407C81999161C1FD108947380D905DF1D4E93F03E36246464629
          262606063E366686AF7FFE3100110CFC00E2A247A112D3310C505DF1D8ECEB9F
          FF4718999858F9D999194C8459FEFDFAFCF513371F37F7B35F8CACD73FFE61F8
          F71FA296859121EB5E88C47446F90577577FFBFB7FD9EB6495F562F3EEDE6362
          61566461626430E1FAFDE1E0869D873FBEFB7003A85ED2D8D64C47DEC440EFCC
          FBBF4C6043FEFFFFAEC2CFAAC4A838F3DAEB4F8C2C224027AF6164620C91E266
          6590E561F97B62EDA659EF5EBFABF9B7A5FA1DC846269F5677250DE572690747
          FB7BDFFE3101D5330831FDE965549E72FEE53B264E31A0660610FEFBEECDB3BF
          4F1EDEFEFAE05E104C330C000DD136F1705CFB4C44411D6400FFDF6F171955FB
          4FDF79CDC2A5CC080A314688210C0CFF1EB3B0B04CD011E198BE2F40F63BB221
          4A755BDA7E6A185482D44AFDFD7A9B51A3F3D8E9A72C3C268CCC4C0C4CC03862
          65616690E76763501560FDC3CAC850B4C8537632B201012BAF36F08989D67FF8
          F59FE1EBD3A74718355B0E6C7DC824E0C5CEC6C4A022C2CEA0C0CFFAE7E5A327
          F7CF9F3877EDEBE7AFD381DED889E60D576E3E9E7C690519A14FEF3EAE64B4E8
          D83F97594822499C87E9D7B3878FEE5EBE70E5EAF7AFDFB602D5AE036AFE842D
          A1010DE10352EA407C9751247E4688A89868F9FD7BF76FFDFAF9733350700B50
          E317A29332D0341E206D0CC427811A7F50252F900200AAC3D14F7163C53E0000
          000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Redo'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002274944415478DA6364A010300E2E03E4563C95FBC7C098C0C8C86800
          943106D2EF80C2AF81780D0733E3CA5B41E21F711A20B9F8612650431F231313
          07032323032B3323030F2B33C3E73FFF18FE43943D636264887D1022B10FC300
          A905F733FF32304C6304AA6006626D417606194EC6DF5FBF7EFBCACECBC377EE
          FD3FA68FBF816E6364FCCDCDC268733D50FC14DC00CB550F246E7FF8758F9199
          8993998989C15C8CFDDFD34B972F9D3B7AF60A50FE39BF9080864390A7EDB99F
          1C02402318FEFFFB7BFF49A48C92DCEA178140C3A21855675FEF79FB97A91864
          BBAA00DBBF57278E1D7C70EB7EE7BF2DD53B413630F9B40A09890AB7588707A4
          3DF9C1C0FCF207D8256B806685F031FC79C3A834F9FC85F74C1CFA207FCB7F7D
          7EE3D2C1632140CD5791FD0932845749791DABA28A2ABBB88414482DD0100681
          DF5F5F316A4D3C7DEBC97F4E552660A009BEBCD779BFC5B70259B3D386C79CD7
          DEFFCEFCF7EF5F01132B8B2C23543330B019447E7DBACB68D577E4308798B88D
          08372BC3E7F7EF9AB72719D5211B10B7FD71EE1F46A6BE7B5FFEB03CFAF28FE1
          2F28E098208648FE787F86513C79563E1F3F5FF8F3672FDE7DFBF27522D0F9BB
          D19CEFCECDCB9D69646DA225A52CAFF8F8DB7F96FB9FFF32FC01C5DEB7D7DB18
          41FE03B29581F82650F3276CA90DA8860F480571F17079EB9B1B6ACBAB292ABF
          F9C3C4F6F3D99379242565A0413C40CA879D83DD57555B4DEDC5D3179D64E505
          A0411C40CA1C88CF529C9900AC2DC1632FCF8BBE0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Options'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000000473424954080808087C08648800000009704859730000049D000004
          9D017C346BA10000001974455874536F667477617265007777772E696E6B7363
          6170652E6F72679BEE3C1A000002AE4944415478DA75924B4C53411486CFCC2D
          487C441168085DB0F0C552774A696FA32EC4D70E7421A9048506EBA3EE4C485C
          B930EAC6A426C448A28921B494A7F2D0D2C48571671084446D13122289B410B9
          059ADEC78C67E6F28C3AB79333D37BFEEFFCE7B484730E4F2F9D1C3DEA2AAD02
          8500771010111C1CB8880A70AE00DE0138C53B150A069FBEFD4EB43D1B6D22E2
          9AB87B39E9557D0740888981D916269A78361122CEB81553464EF13DCB42E787
          5F097F5BEC94048CDDA9FFA1AADE8350202A30145A6BC9B6086005B891014E56
          11A4032924D0F99125FCF72236A0EB46FD77D5E73B241D506115C508B1F465C8
          677F82A92F01A0C82AA0A0ECC07E0A29C43F6B63C1FBD1D31230D8D7FD5655D5
          B3C0417C8020C7340DB0701320F20B823B9DCE405959198815EBED0D37078241
          0918E889BEC116CE49256ECB3080314B8AA4583C789400A70010E88EF584AFB7
          B4DA80D7AF3A863D359E3322CB307430745D0AD62BC39A8B85C545282D29910E
          4646DEB5DFBC1D0A48407FAC6B1067705EBCB04C538A0815626A9FD720E9741A
          9C4EA7E445A2B170D3B5E675072F86DCD535B5A23AE36C8B906EB8104F666101
          67502A1D0C0F8FB4B7066FD90EFA6291019FEABD20C45488E926606B9C470745
          4545904CA620994A86AF3636D90EFA7B22FD5E8FE7A2B0B609F81B245AC8E572
          905D5E81A9E9A9B0DFDF68035E763C1FAA3E71BC96CB59511B22016403448968
          6151FE8C791C72FC7DBCBD2510086C732013B754DF76C6389FC94056D36072F2
          2BE8BA1EBED2D0603B78F2E8E1E3AA23878F61118571A270CE14C6B883712E27
          8862AE381C2CB7BA0AC5C5FB75C5A158E3E35FE2A150E88104E0A21313133B53
          A9D4EED9D9D93D3333337BE7E6E6F661CFBBF2F9BC8283D3D1BA565151B15459
          59A9B95C2EADBCBC5C73BBDDC63AE09F2B1A8D8A3F00D4D5D5FD37E90FE1D751
          F1CC106E8A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Preferences'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000027D4944415478DA63648082095DCB630C0C3516CB2A897DBB7CF6CEBB
          1BD7EEB5543424CE64200018618C6913D671B33031EE49CD0DB000F13F7DFECC
          D0DDBA6C524B67463E5E03F2D27A4C26CD2A39031328CD9DB8A2AA29295C4080
          87E1C2D95B0CB3676E949A36BBEC394E03DA1B17FDBF73FB61D6DC25B5D34102
          4559FD2BEADBD3C2F9F8B8C00AF252BB9B27CF29ABC369C0E5CB0FFE7FFFFA93
          213FA73D4E47472978D6FC5A7F90C4BFBFFF18FEFCFBCB9093D457387B49C504
          9C0674B42E795A5E152DB57DDB71060343150621417E866FDF7E317072B3313C
          7AF492A124B92D6EF3E1198B711A10E857995A5A163BCBD25A93E1E3C72F0C7F
          7FFF6160636363B87AFD01C3F7EF7F187EDD7FC0D07EEA73EA1767CFA5674285
          BF638D0547FBCCC921416E390E0E060CFF80102CCCCCC4C0F9E70703BB101F43
          FE99DF0CEF1838EEFCFEF2D3E4689CDC47ACD168641023C5C2CCE2C4C4C4FA99
          859555ADA336B64BD7429321E3F46F86D59DDB19642438197422EC1EBE7DF145
          EB78AAF2370C03D081BB5743A2686EF6BC35D30F30480AB333FCFBCFC8C0CEC2
          C2A0E0A4F5F0EDB30FFA674BF43FE235C062F51B364E86EFD77F3F7AA3F4F1FD
          4F06266646869F5F7E32FCFEF88D41525FEEE1C7375FB42E379A7DC369000858
          CFBDCDC3C5CE7CE5EF9F7FF29FDFFF60600686CBEF1FBF18DEDD78CEC02B23F4
          E8E7C72F5A780D00874DC7797E2E76D68BDCD202F27FFEFC6360646260F8F5F5
          17C3B38B4F80B1C5B89DA00120A05D798C8B8583F99A8C89A23C0B3B3303D39F
          BF0CBF39D8186EEEB9CE40940120A092B59B9F999DF992ACA3B61C872027C3BB
          2FBF191E6D39F79E68034040316E23DB6F1696EDCC1202365F5F7EFCC8FAF6AD
          1B0054DDF457E572123D0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Find'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002E14944415478DA6364A01030A2F1556C6D6CB6383A38CC6D6A69E946
          96282B2B53E76063DBBBFFC0814D878F1CC9C265401BD0804A2D2DADFD3367CD
          724296888B8B8E5790535CB07BCF9EB7C74F9C10C130203131D1E9FAF5EBDDDF
          BE7ED5141111B927282828C4CAC6FA75D3A6CD4CCA4A8A3C3CBCBC4F85858434
          5FBF79FBD7C2C262F2C489132BE106585A5A72AAABAB7F565450606661666278
          FAEC19C38D9B371944444419B66CD9C2202828C0009467D000621E5E3E865FBF
          7E319C3C7952EFF8F1E397C10658595939A9A9A9EDB5B4B46078F3EA15D8805B
          B76F33484BCB30AC59B386414C4C94415151116C808E8E2E58EEE8D1A395A74F
          9FEE001B007452B58686468B95A525C3C3870F198E1D3BC6202E2EFE92998585
          65E3C68D0C8606FABF3E7FF9CAEBE4E4C4A3A9A9C170F5EA35909A1DA74E9DF2
          8479610BD089DE8E8E8E0C172E5C60D8B3670FC38D1B37927FFFFECD0D0DA237
          3A3A3ACD5E5E5ECADADADA0CE7CF9F0719F05D5E5E9E1766C05BA0014220032E
          5DBAC4B07FFF7E066080CA7FFFFE9D056AC02F2323A325AEAEAEF64A4A4A0C4F
          9E3C61D8B97327C39F3F7F4C188D8D8D75D9D8D82E393A7B3018E86B33ECDEB5
          13E48AEF40FF712147A38989C95A7E7EFE20592D17066939758663BB1731BC7B
          79BF8E5158447CB1A05E4C8CA8B43683908010C3ADAB0719B8DFED7F03344414
          D9002929A93DBC7A19CEC2BA860C9F189918BE5DB9C2F0E3E9D9578C7CEA21CF
          04ACBC25190CF5C00AB9AFDE60F87A7AF3A747E756F0231B20A1E9B347D239CE
          F9AD9A3244E0ED7B86DF7BF7BD6754B22FBFAA1BEFA3256BAA02167FBCFF3AC3
          D5552B9EDC39324B16D900559BF47C454BE7096A71B660FEB54DA7816A4F9D66
          3430774BFBC8633B3336D1858189819561FFAEF30C1F6EACC8B8786AEF4C6403
          424343993FF17BECE3E617B6FDF2F903E3D3C70FDFFDFB78CD1E1C0B46D67E41
          9C5296F10AF24A4FF87E5E5A3C7D72EB095CB94F257495A824D353D9C32B0BCF
          61CB4C2403008B2515CFFC8D08890000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Search'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000027F4944415478DA75935D68526118C79F738E47CF9C1FDB6CE1D622F6
          C11C91CB25B6167DB2AB6041318A06117D6CCE60D0FDEAB22EBA2B3675EB2ADA
          451F10EDAABA0A82CA4DF0CEDC476B4E2CE7D20DF538F578CED19E234770690F
          FC39EFFBF23EBFF7F93A0454987B6676822088BBA45A7398A45520E63200F9DC
          673C73DBC7465F430D232A9CA7EA9A8D1346DB6950E91BA158445F5E80782804
          69FF02907CEE11421ED404B8DC33F7B4AD079F749EBF00F8DA9E0B598E874824
          06ACEF135002D78F106F15C0E9722F5B2F5D33A9F50D55218A6201B676D210F4
          FB818C049E8FDBC76EED014C3B5DED8C46BB7EF6EAF55A2996005104AC052320
          AE7CF98D80B67F012646A35B1EBC32020A8AAC02643901C27F92100C4741FCE1
          A909A03071E1DCF0083435E880946B2015511045882733B0BE9980583808105D
          99738CDB6F54D500210F9BDB7BEE9B6DFDC0A81425088FCEEC6E1E3631FCAD9D
          14706B5E2004EE24023C5580A969A79E20A984A1F308EC3FD4554A85C316A632
          7948A658C8FF0A00CDEFCE8EDEB9EDF8EF1C20C4521AA23AAD9D526337940C14
          726928B2715C17C067F6C5B799843F4F9397DF9B5E25AA0065C374ACF81940B5
          2330108B6DBF5BE8FEBA78CAD6DD41D174D1930A8504256541085B1350CB7ADF
          F63EB6B6991C93C787756F425E61910D2F21E40442321240EA1D83AA47A96428
          F600781487CA496BDBFCB139736BC7C5A12E8BE643E43B1FE6B3DF3EF6CD0F4A
          97695423AA05258DA24276C63F09D2D228C87BF1E84BF38B032DFBCE18F41413
          CE14D8CC8678B31C8106D5244721ED45F97529124A7E44FA32A667A6A76A637D
          9F52AD56AC4E2E0D946B40CA17C88A14CAE77405408A4E631832F4701BF958DA
          CFFEFC0B284E0133EF4230350000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Compile'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002504944415478DA63FCFFFF3F0325801166C09617BD70C1A7CF5FF03C
          B8F7232BC2D2FAFBBF7FFFFF7EF9FD8DB97DF3FAC71921F61B805A18FE30FC66
          F8F7FF1F43887435C28025B7AB187E7CFBC5C0CCC4C2F0F2D52BE1F7EFD96705
          9A99B2FFFDCFF0FBF3F72F2C3D9B37EF56579498F4FBDF6F06511631065E167E
          860ABF6684010905A10CCED1060C4FBF5C6778F8F015132BBBC44D57233DF97F
          FF197E7EFCF29979E1EEFDBD7C7CECB54ADCB20CF6A2C10CCC3F1919BC83BC10
          06383838306F5CBD918D5F94FFBB7583F18E868822A7879F2EB232323231FCFA
          FD9B4189DFE865F9E2DE5361DAF6899612019FDF7D7EF32B3828186100085858
          58F8CC983163D9B9576779CF3E3BC81068EEC670F2DE6E060B25778685FB3730
          C4982631F0FCE6FD7AEDDAF5D5B2B2B239EEEEEE5F510C282D2DAD292B2B6BE6
          60E16478F1ED3943EFD666866CCF0C86B6D53D0C75C1AD0C7202720CBFFFFF61
          D8BC79D3D1D8D8581B94580081C58B174FF7F5F5CD60023A9B95998DE1D3EF8F
          0C6933731966A44E6010E11663F8FAE30B031B1B1BC3C68D1BAF444646EA6218
          B062C58A23212121D6BF7EFF6260024290DC87EF1F18C4F9C5197EFEFE095603
          3260FBF6EDAF7C7C7CC4310C3878F0E0277B7B7B5E4289E7C48913BF80E1C54E
          7D03BABABA5E9A989888FDFDFB1777D2656464B875EBD6F7ACAC2C2E0C03F6EC
          D9F3D5D6D6968B900B2E5CB8F0CBDCDC1CD305F3E6CD7B6F64642440C805B76F
          DFFE191E1ECE8161C0993367FE03BD40C8010C376FDE6450575767C430C0DADA
          BA594A4AEA3BC8165C00A4FE171000D3420F880F0008E321F0F8DD8D53000000
          0049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Cursor'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000DE4944415478DA6364A0103092A1871F882380581E88AB8835800988
          5D8138018803809803CA5E48C80075208E07E23820964612FF0AC41240FC059B
          013027826CB0C061F022A8C1F03000D14E409C08C44140CC49C065CE40BC0FD9
          8013406C4E64783C02620520FE8F6C0028603281580D88458098078F01CD405C
          07E3E00A449001D6403C19885591C4FF43F977091900032E40BC1B897F0888ED
          91151032801B88BF20F19380783E2906800028CEB9A00649A2194894014F8158
          0A88E702710ABA24310680FC6D0BC456407C9C1C03C2182009CC139B2439B911
          05000042B01E11B7A480090000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Cursor Disabled'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C180000000774494D4507DE
          0C010019080C777CB1000000F34944415478DA6364A0103092AAA1BCBC9C0D48
          A901316F6767E771A20C006A02A99305624D205606626620DE0334E03A23018D
          82404A03AA911B49EA3710CF051AF09B118B269813419A2470987D03A879373C
          0CA2A3A319646464909DC842C057EB81063C811B00B4350C488913198E9F9F3C
          79B260E9D2A50CC8062801295D201600624E2066C563C069A0ED27601CAC8108
          341064802410DB430D45068B80067CC46B009241A0700940127A06D4BC16590D
          2103402EC9401202C73DD106400DC96480C40A3CEE49352089019288AE0135EF
          459727C68060202505C4AB8106BC20C7005520A509D4BC099B3CC9B9111D0000
          5FA64911769952D80000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Antialiasing Disabled'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C180000000774494D4507DE
          0C0100122A3AE3E49E0000001D69545874436F6D6D656E740000000000437265
          6174656420776974682047494D50642E65070000005B4944415478DAADD3410A
          00200804C0F6FF8F363C0452AB92EA25489D8319446475021100685A5002B459
          CF36902114B0CD65C03645B39807BC62F77E14C8DE9DE5E7009A24CBF40C3802
          6EC8C6A9C5CF6762DBF905B0799481131B02BB8BE183C39C8F0000000049454E
          44AE426082}
      end
      item
        Background = clWindow
        Name = 'Antialiasing Enabled'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C180000000774494D4507DE
          0C01001217628BA88F0000001D69545874436F6D6D656E740000000000437265
          6174656420776974682047494D50642E6507000001324944415478DA9DD3CF2B
          446114C6F119224532294B591AC28635352BA52C9494343596E4C7C6C2C2CA66
          4AFE028AE52CEC6641CA9292EC48C9C81F203676747DAEEEE29AEE8C196F7D7B
          CE39EF394FF3BEF3DE741004A95A2B6D05F51AC29E5AFB66FBC9AAFDADFF1AEC
          91150CEA7969CAC0702F79C4093AF42C356BB04BFAB08E072CE8BB6AC8C0700F
          79C284BD8ABC20CE8B271B35D826C3EA8B51DE426EB0A356AE6BA0B9933C634A
          FD3E569F26FB1851FFAC67B046726AB309F7724E4AF60E120D34B447679F53BB
          4E301823E5E86FFD4832C89342FCB2D45AE55FB1BC446ED58ABF0CA28BBAC386
          FC2C367041DE7188530CE01243FA5EE3063971513C5EF5B3BBC83C96113EED23
          8CA2A277B3FA0819F15BAAC6B29F0D8F88F0556690FD79277F7C6C49466D6406
          DD668F9B36A85EDFB4D5A7E1C6077AA20000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'About'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000001FE4944415478DA63FCFFFF3F03258011640023232356C99BBDFC3341
          B47AF1C7746CF260BDF80C78B12BF73B8896709BCC49B20197AB046B356A9634
          3101A56E74C4B46B37BEAF22DA80E2304EF5A208EF3D62EEAE320CFF18185EED
          DEFDA46FC5D682DE55DFD76235A03599BB33448C83834BE4AFE5A7EF7A222C2C
          5FA595D322D8FE33FE04AA02A96467B8377BC5AF3FBFB99FF2715F7AF3FD1DD3
          F1D5CF7FFEA89EFBB51C6CC0CF55A273EF3E1148524BC962F8FFFB3D3068FF32
          30FCFB0DD408B4FE3FC87540CCC80A14636560641364B8356F2A83B2F2BB0DEC
          016F02E15EF8B64668D9DDCBFF23D5522C811AB8811AFF02F5C83030FCFDC0F0
          FFCF57209F19C87ECE706BC11506657D86E55CA1EFA330C2E0FB3CF1652F3E7F
          8A147766064A80C4402E6002BB02A4EEE5C13F0C12827CCB39E35E46E10CC43B
          6D0EDBA4BC2F7932323281F543C28001EC9D27DBB4D7A9D61E0EC6190B9F32E4
          245F707E58229FC0EE04F40103233344F3FF3F403650C9E3E53FCE897D110CE2
          9BF1E8212E030CDE8B7DDA2611CC26F9E7D35786D7FBC07633883A3230B2F072
          33BC58FFEBB9E02B3E2FA001177019E0F556E2F502A67F4C223F1F71EE949461
          CF0045C0F3C73F67B0CB7C776760FBC728F84CD41B68C0365C064433B0FCEB65
          D0F9B0952FE34B3272A2F93483672EC335016F865F4CC5400396A21840716EA4
          0400009EE315F0CA5B0F3A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Documentation'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000036D4944415478DA55530D685357183D3749F39AD7A4ABD6
          26261A9AA68C3A45A933B658B7BA056A22D54688459445D1EAE6D6EA36AAD516
          DA4C86ABE25ACC6AFCA93AC4B229D4C8A02DFE60AB8222145154D64D58AC69EC
          4A9A8625217969FE1AEF7B05E92E1CBECBE59DF3BE9FF3118BC5024208442291
          10290A283E02A0A2C8C5EC0953F83299CC5F147E0ACCCCCC808F648E00436399
          4EA75B5B5B5BBB5DAFD72F4FA552025B2291C0ED76BFECEDEDFDDDE3F13CA2C4
          612A109F2BC093D7D7D4D46C31994C3B06874378F14F1C236FD282C0529D18CB
          8BA530AE56E0F6ED3B57FAFBFBAF53F21D8A38A124FE9B7554A8CE6834DAECDD
          5348903C2895F9904AA5344D201E8F637232806C12425B5D1E0607877A060606
          2E51DE03525D5DAD2A2A2ADA7BF060E38F2D4E1F985C0D62B104AE1E2BC6DCF3
          C3792FDCFFA63113F309220E87A3756C6CEC02319BCDC6C3879B7E19F1E62C1B
          7C218742A180DF1F84A94C2C103FD4CDC3864FD5B874C383A1671984C3619857
          71289CEFFFD3E93C73805455557D71F6EC999E1397C7119714C2E70B0AC46432
          89689443E50A11B656B1E8BD1B846B28048D468992C5697CB53907CDCD2D3642
          EB6EE8E8E8E86AF839009DBE08E3E353088622E06231AC5A928D265B01464639
          D8BBFDC2B4147216326906C7F731B0DBEDFB496565E581CECE4E47FDC900949A
          C5708F4E22CDCF389DC2C029BD904DF5F7AF414412E1CE48259089A771E26B06
          6D6D6DDF928A8A8A5DB4965F3B7FF3C11354622A9878DFB8743C2C443193FBFE
          4D841456EA22A8DB28434BCB91DDA4BCBCDCD4D878E8D46860C1926BF73210C9
          D44824D34827383CEC9E9DC4275FBA2196B2C23D159DC0CEF562A858EFDFE7CE
          39BF23068341ADD516EE6F6D3DDADCE418857F5A0931BB10C9E9301E381709A4
          75F5E354201789C80434723F8E7DA3457BFBD1F6B76FBD5DA4B4B494F7B4D962
          B1EEB15AB759EB7F7A0EEF7F2C72F27590CA3E100412B110A28137D0CEE3D075
          64055CAEABAEBEBE1B1769536F11954A0596656559595956AB75BBD566DBB3B9
          EFFE181E3D9DC0C36713B325AC5463EDC76A6CFA4C8B9E9E8B7FF00274CC2E8E
          E36244E80B90CD30CC7CB95CFE7949C9D2B28686431B0C8635FFB3E293278FDD
          A74F9FBCF9EAD5C8702412B94FED1DA0CFD3BC006FB93C8A7C0A39CD248F6E5F
          31DDCE82B9EB4CB7CF4FB7D34DFFCC3B2D4A3145117C07D65D6EF23A0C1BA400
          00000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Execute'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000036D4944415478DA6D536D4C5357187ECFED07FDB2C8046A2535194B17
          83990622DB90C6010A26236144563E246EC9D09992816409244608A2B8680986
          4D9044025194F0031AE818FE1196451BFDA36459D9061A2B45A8154AE1DEF642
          4B7BEFF56DAD8924BEC993F7DC7BCEFB719EF739043E607575751F979494D47B
          BD5EBAB8B8F85C6F6F6F29CFF36C4747C7C4F4F4F4C6FB67C9BB85482482B6B6
          B6AFDC6EF76A4A4A4A5A7979F9603018641D0E8705BF8BC462B17A686828BFA1
          A1E1CF0F26686F6F3F525555351A0A85C2841029421E0E8721028EE3003B1066
          6666AEA30F3A9DCE872693C9B225416565E5A75D5D5D4FE472B98AF1F984DFFF
          9A8247FFB9A2FB5FEED142DE017DB44B2C00814060293B3B7B2F4DD31E2C4444
          C3C3C3CD5AAD362D3D3DFDD86B8F17AAAE58616A590632A59A84791E027E1A32
          B51CFC66CA856D4A19CCCECEDE2E2828F81E730BA4A2A222D56C36FFA344C364
          C2CFD7FE10ECCC4794E9880E3E4F55C398DD0713FFAEC2827B0572766F42F309
          03300CB3383F3F3FD6D7D77781646565A99B9A9A2E67646498965719EEF0451B
          75FAEBCFC8E91C2D840431DCB8BF044FDDEBF0F4951FE8E5459868CE816D7269
          8417C162B11411A9542AEFEEEEFE293F3FDFFC647691ABE871882C67F6C3EE1D
          0AF8E1A6035E7802C006C3100CF1B04BC543FF8F69B02F3559B0DBED16ABD57A
          9660F0F1BCBCBC3B481079E55D0FE7FCF258646B3190A9974168B13AC117781B
          BC5D2101D8F481ADE510ECDAA1829E9E9EEFEAEBEB07484D4D4D466969E96842
          42C24E9C80F898F90177B62C53BCC4F27061D40971120A947122407E402361E0
          DEA522E070B42B2B2B4C6D6DED01121BA56C6464E4965EAF37BA688EFFF5EE33
          72F594819CEC9B8139CF46540792CD35B0341C827D9FEC14161616165D2ED7F3
          DCDCDCA3D139A3D292070707FF56A9545AE404984D4A98767A2155A382FBFF7B
          0885A7BE35E841A7D90E6B6B6B340AEE8BC9C9C9978220ACBF1392A8BFBFBF53
          A3D11C44C98A131313D3241209C864B2A878288A8A4088ACD1F8CECECE6F5A5B
          5BC7B72851A7D3C561AC04F930188DC6BB78678E65594F52525232FE27369BED
          32AA90562814EAC6C6C68B737373812D096246959595E90B0B0BCFFBFD7EB6BA
          BAFA1C72331E1F1FAFC117691C1818781CE920862D8F0967040A8432E6E588B8
          C8065E438DDA8F54631011BF8E60633EF406CBEF7CE1F9264870000000004945
          4E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Sync'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000003554944415478DA5D935D685B6518C7FFEF79CF479293E4C4A469EAE2
          5CAD65AC54D78983B1B8E2366B99CA1C8AE05051C4AF9B81A02238BDD09B8182
          172A7EDC088217435C6F74885A715F886E6D6D676D5A93266967726CB2343649
          4F733EDE93E3494B8ADD73F17E3ECFEF7D9EFFC34B70837DFDFD58A858594D08
          82B887E3B8806DDB25C6CCCB3BBB6353C389FEB51BFD497B7171F25A9431F66E
          44F13E7453C81FE1799E9A968D95BA01F57ACDBABEBC3265188D0F6311FF57C7
          0EEEB6B60046C716E2415918897786EE12055EACEA4D10F7461629249EA0D974
          507141D9C2CABFC562718487F5CA9347F7D737015792EAD99BA3CA7DA9B2E9F9
          72B28E4CB509BB096C53440CF5CAB8B75B42CC4FA11B0C59B55A5FCCAB6799A9
          3FFBCCB18441462EE6023BE381792A79A26F8E5688E8F5E048AF173B148AB952
          0397160D283E118FF5FBB0A74B0063760B522A16973E78E4F0C029F2D395EC47
          BDB7C69EFF2659934ECFDAE4F541057BB709A01C594F2F5DD671E64F0D6593E2
          F86E3FFAA3149CCD9CB985D2623A9D3944CE8D67A7BB3A3BFAAE16192D1B1489
          ED02A23E6EBDB8960E2D4872A981D3D36B9828028938C1DB436164D45A359BCB
          7FD602A462D1488F2B38953DFCC6CB84AC07AF8BE4CE3FA6347CF25B1515378B
          C1DB7C383514447555379319354DCE8F677F0985943B99C3C9EF5C58E51E77D3
          DC1B172152B2096815C3DC4EFCB16460A260E0A57D0A74DD742653FFA0057835
          100C1E70081D3E79AEEE4B2FDB087B294EECF3E3C00E6913D02EA73570EEC666
          0C9767F21B6797A6163F0E29C1A75FFEAEE2CFD5088E0F04707FB780B087B8CE
          1B019AD5C4CF3913E7AFD9782DE1C72DFEA6F3FB6CDED9004C2E1CF5783C6F9D
          49D90399B2212DAC7238D823E3C15E11610960762B7D1D5F5CD541243FDE3B24
          B89DB08CE47C7EA60D506CDB792A1A514E580EBDFDDB644DB850704025097D1D
          02886D613C6FA2CA043CDA27E1E11E40D3B4F2DCFCDF6F90FF7F8C8959F54549
          925E907D527FAA64787F486BF8B5C050B1047447BC38D24371783B05338DDA5F
          397544F2059FDB02189BC907B486795210F841D92BED92BD6298A39438AE548E
          5BAD613247D3D60A6AA9FC69C3B0DE7FE281BB8D2D80B64DA7973A97AB8D5D6E
          F0B0C873FB79CA75B85DACAD99F6A820089FDF734757A1EDFB1FE0DB7549F9BC
          1BDD0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Sync (Disabled)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507DE0C010D052239637B690000018349
          44415478DA6593CB2F035114C6E74E1553EFA0A5341E51510B42585ADA889DFF
          151B8905DD2192862244BC92265EF1A8A86A7D577EB71963925F6672EF3DDFF9
          CEB9678CF7FF6912FDA243C4C49778168FA2163D6C228169E18B775165AD5DF4
          F27D2B6E443D2A1027F88D8CADAC7F2264709414DFA2C87A43605894454A2C92
          D160FD449C8B57D18C488B28D8920C75A6515E25EBB1781283629C920EC51DE7
          939CBBB20219F12166C58CD8A2CE1AB5A6D80BC4BE28E16EC03AB31F39F14236
          7BE8928C5EA859766F9EF7B5D8C0C5AFD234023EB65CE646A7F54C89256AB7FD
          581709EBC20A644585DA96C50159AA21813A0986287917B1ACC18A8FFD15D1C7
          8DEC888B9040D8518DB9C8B96B4CE3604D748B3D714673EB04D88C13624C6C73
          AD934EA093C039BADE85401111677F817237E95FD209D8EC3D4C649CEB1C652A
          4B0864D82B305C765ACBE17FC123738206A6B8E21126F05E9CD2179B30C8E7F3
          4751019FC080BA2BDEDF3F304E2F2A6ED8A202EE8921D24680A199764E1EC257
          FC039D4664DFC1284CA90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Abort'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002804944415478DA85935B4854511486BF3DE78CCDC54A7C10C47CA8A8
          F0A588C032EDC2A06541609A4619414F1194164216581215D1432065F8127681
          084C908A54488CD4C444280BC3CC22272F89D4A8338EE3CC99D96D1D89511317
          6CD867B1D677FEFDEFB505F3C2979D17D0D727EA8424187E109ACA8610660BC1
          DE7E23AAFA8939B25E447E78B373A5353F1D860740D715C098030FBA5D4CBD71
          62AF7D2616003C5939D27ED441F0E33BB4E23BF0BE0576EC87C61A701C848A52
          8CDF2398740D6FB393E5B5CFC53FC0F8C1437279DE6E825DED687B0E43D26650
          201C59F0EA2964E4625C2950127C886965EA48DEB64156D4BD10C29595E38FC9
          4F371B9DEDE8572BA1B916D66F844F0A909E03F55590991706043C08A10062FA
          BF1277C7504078CE9F9356DB2421AF17FDE6C33060C326758436D8AB00754AC1
          BEB0023D6E4DD88B1F9D08938909E71862ACA850465B27907E3FDA34E05139A4
          2A23DF36C0F1D3C88A1B885317C380CB652ADF8851F3405D4E14EEFE7184EBEC
          19691FE9C21874626DFC3A6BB7725FD3E7DCC00CA0F436B434E02D3C8696B88A
          516F34A26FEB1619173542C83785ADFD178B452460E2E46134BB85A16F4AC1F7
          9464192F07084E4E61FF30B2282074AD1853C9CD30E0C421349B9D7EA702F46E
          4B9609937D61C017174B4645199EEB256896687E0EBB114DF6187F72CA5A73B0
          BB1B5BA71362639700943371E902268B958E813F8199416AB0AD94A949F19877
          39D06EDD5DBCB9A5155F5101A19E1E5A47DD64CC4EC44CBC34DBA5237E0596C7
          D590B6FDBFCDC187F7F1DDABE4B59AC403B3533CE731D588653273DD6ACC3BD3
          16023EF7E06F6DA25E6DB323FAC4FCBA2ACC8104027A3022375D64524B39641C
          8139CFF92FD07AFFCF99A0DB7F0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Tutorial'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000002954944415478DAB5935B4854511486FF7319E7E864532998F7C49228
          B39A222219936E940441541666D84350115690164498A04616A5F41061E14346
          21618262520FBE544899498E65E1A5191B9DAB8E3A33672E67CE6ECF512A8442
          88F6E667C1E15FDF3E6BAFBD18FCE362E66B34773C8A15A222574884E8F487CE
          B77D359986FF0AB076342E6034C20615C3EA1890F5B22CAFF6FA3CE91AB54A5B
          70E17A61DBABAE0605505656C696E46F8E0BF84806CBB2596C48D6F9FCFECC69
          9F77F964505CE4F47BE008F9E142102E2E843D31F1787AFFF9CD8AFAA61205D0
          78A7BC7E67CEA6C2917107E7F4B961F3BB311AF04254D373D51CD8081E2A350F
          8E6521B1808E08B0B47F7C5970F5EE2E0570ADF858ED8EC3B9C52DAE11440A02
          543481E359502F08219089ACC4B0820C412AE1916C701AF527ABD650CB347370
          DB96A3D5D5A71E3E0BDAC2193FCD8A30038012094234461306DB2DB2B4AFF8B6
          6EC836D1CBAC4A4D58D75277E57D6BD414EB25A15F10CC8D40681676C0A34669
          79C391F6AEFE27E12E68BB1B2A3FF5A4F00946590447A0987FFF8BF00EC812D2
          D531D02FC984DA3188DA9AC755B5CDAF2F2B6D6CAA3CD381ECA4DCB7B21B2A32
          5B3BDD6152B8048948F0D30256BA05EC16D290949681F6176D6F8ACEDDC85300
          15C7F7D6E414649F6DA6CD52C933F54AB41C89123896477C541C92853818AD9F
          B1CCE2813E7A3162D3B39077BABC5001E4EBD7165D2CDD5FFF20C2059EDEBF86
          2625F21AA4C82C12A508C4881CC4293B4CA35667A7C164E81E30192C8E2953E7
          80F59E02C84C5EBAB1E5D6897776AD004194316976062CDFED635F86CD83BDDF
          AC7DFD667BAFD1E6EA77F9A4A1F023A592E6CE82F652FED6BA31DBC478CFA0A5
          BBCF64FB401D46FADD4125CF779816528954C1FF328D7F5A3F009AB05053D61C
          72040000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'New'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000030C4944415478DA6D936948545114C7FF6FF671986C233202512C0CAD
          ACD08612C9942C2DD3F6B22F9504596605A1F6215AA030A9D048C9165193D240
          8B960FDAF2417032A34D1D52A7A9B11C75D4199D378B33BD99D7BB379F4574E1
          CFB99CF7FEBF7BEE3D1C86E77990B52FE7649C10C2F19F15AA18D42E9E660C7D
          3212DF3999720A6AAA2C2BF6310490977FF6C889C3FBAFCD9C317DCA44F2A224
          43AD181FFA00656436388E83C3C1A2BAAEB1E3E299C2180AA8BCFBA065EFCE8C
          78B95C4ECD814000FC781F0260C06B42806F7518EE3740BBA2103ED60A7ECC88
          D76606E969EBE751406DFD437DD68E0C9D68A60021EF797500F2881430522B6C
          AC111A4D3224D64A7C911662D0E6C68694E4451470B7AE9102C85E34933861EB
          814A5103895480323E48193B5E3685217471168C263336A6A6FC01ECD9BE5947
          CDA35DE0D9CF80DA0646ED10AE310CA57601C08D62C2DE01B74B0E9ED1627080
          C36873C5310AA8B9DFA0DFBD2D9D56F0D331005B5F2B58B701DE9F7D9837DD80
          3973AC80570AE78804EFCDCBE1E16663786C16B273CF274C01766DDDA413CB27
          2F4D047737A43FD23066512348E226AD817BC621C8161C41A7A11BEB9213174D
          76A15EBF65538A8E74814088D92F4866CA41BB390A91211C5CC6C750C69681EF
          C847B0EE123E9A39AC5D93F01B70ABEA9E7E59F442DA050251281450F24EB01E
          0E73E78743C3B6C2FCE606C2321AE0F7FBC17E7F0FC310F8E4C484280AA8A8AC
          D5AF581249011289046AB59A4AA552D128E76CB0B617818BC887C3E982CBED42
          D3F35703E74F17C45240D9CD2AFDCAE5D153158880A0A0201AC97AFBF40A5665
          16C0EBF5A2ABCB809CDCBC736FDB5ACF524069F96DFDEAB8180A20E513A36856
          2A95B0582C181EB1236669347A7B7B71A1E8F2A3FBB555D9827784028A4BCA5B
          1257C7C6330C430DE2E9440468329930CEBA11ACD5E06AE9F5E6DB15650705DF
          377220051C3A7EEA68665A52C92C619864321985D0871422B9D2E7EE1E7C35F7
          FB6AAAABEFBC687E764EF00C8843C788E39C949A99EEF37A13027EBFE2DF7176
          3959FBA7776D4F783ED029FCEFF9FBDB2F6CDEA808655D98810000000049454E
          44AE426082}
      end>
    PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    Left = 424
    Top = 144
    Bitmap = {}
  end
  object dwsUnitIntermediate: TdwsUnit
    Script = DelphiWebScript
    Classes = <
      item
        Name = 'TCursor'
        Methods = <
          item
            Name = 'GetAngle'
            ResultType = 'Float'
            OnEval = dwsClassesTCursorMethodsGetAngleEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'GetVisible'
            ResultType = 'Boolean'
            OnEval = dwsClassesTCursorMethodsGetVisibleEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'GetColor'
            ResultType = 'TColor'
            OnEval = dwsClassesTCursorMethodsGetColorEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAngle'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Float'
              end>
            OnEval = dwsClassesTCursorMethodsSetAngleEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'SetVisible'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsClassesTCursorMethodsSetVisibleEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'SetColor'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TColor'
              end>
            OnEval = dwsClassesTCursorMethodsSetColorEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'Turn'
            Parameters = <
              item
                Name = 'Angle'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = 90.000000000000000000
              end>
            OnEval = dwsClassesTCursorMethodsTurnLeftEval
            Kind = mkProcedure
          end
          item
            Name = 'TurnLeft'
            Parameters = <
              item
                Name = 'Angle'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = 90.000000000000000000
              end>
            OnEval = dwsClassesTCursorMethodsTurnLeftEval
            Kind = mkProcedure
          end
          item
            Name = 'TurnRight'
            Parameters = <
              item
                Name = 'Angle'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = 90.000000000000000000
              end>
            OnEval = dwsClassesTCursorMethodsTurnRightEval
            Kind = mkProcedure
          end
          item
            Name = 'Go'
            Parameters = <
              item
                Name = 'Distance'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = 10.000000000000000000
              end>
            OnEval = dwsClassesTCursorMethodsGoEval
            Kind = mkProcedure
          end
          item
            Name = 'Draw'
            Parameters = <
              item
                Name = 'Distance'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = 10
              end>
            OnEval = dwsClassesTCursorMethodsDrawEval
            Kind = mkProcedure
          end
          item
            Name = 'LookAt'
            Parameters = <
              item
                Name = 'X'
                DataType = 'Float'
              end
              item
                Name = 'Y'
                DataType = 'Float'
              end>
            OnEval = dwsClassesTCursorMethodsLookAtEval
            Kind = mkProcedure
          end
          item
            Name = 'PushPosition'
            OnEval = dwsClassesTCursorMethodsPushPositionEval
            Kind = mkProcedure
          end
          item
            Name = 'PopPosition'
            OnEval = dwsClassesTCursorMethodsPopPositionEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Visible'
            DataType = 'Boolean'
            ReadAccess = 'GetVisible'
            WriteAccess = 'SetVisible'
          end
          item
            Name = 'Angle'
            DataType = 'Float'
            ReadAccess = 'GetAngle'
            WriteAccess = 'SetAngle'
          end
          item
            Name = 'Color'
            DataType = 'TColor'
            ReadAccess = 'GetColor'
            WriteAccess = 'SetColor'
          end>
      end
      item
        Name = 'TCanvas'
        Methods = <
          item
            Name = 'GetPixelColor'
            Parameters = <
              item
                Name = 'X'
                DataType = 'Integer'
              end
              item
                Name = 'Y'
                DataType = 'Integer'
              end>
            ResultType = 'TColor'
            OnEval = dwsClassesTCanvasMethodsGetPixelColorEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPixelColor'
            Parameters = <
              item
                Name = 'X'
                DataType = 'Integer'
              end
              item
                Name = 'Y'
                DataType = 'Integer'
              end
              item
                Name = 'Value'
                DataType = 'TColor'
              end>
            OnEval = dwsClassesTCanvasMethodsSetPixelColorEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'SetColor'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TColor'
              end>
            OnEval = dwsClassesTCanvasMethodsSetColorEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetColor'
            ResultType = 'TColor'
            OnEval = dwsClassesTCanvasMethodsGetColorEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'Clear'
            OnEval = dwsClassesTCanvasMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'SaveToFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'string'
              end>
            OnEval = dwsClassesTCanvasSaveToFileEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Pixel'
            DataType = 'TColor'
            ReadAccess = 'GetPixelColor'
            WriteAccess = 'SetPixelColor'
            Parameters = <
              item
                Name = 'X'
                DataType = 'Integer'
              end
              item
                Name = 'Y'
                DataType = 'Integer'
              end>
            IsDefault = True
          end
          item
            Name = 'Color'
            DataType = 'TColor'
            ReadAccess = 'GetColor'
            WriteAccess = 'SetColor'
          end>
      end>
    Constants = <
      item
        Name = 'clBlack'
        DataType = 'TColor'
        Value = -16777216
      end
      item
        Name = 'clDimGray'
        DataType = 'TColor'
        Value = -12632257
      end
      item
        Name = 'clGray'
        DataType = 'TColor'
        Value = -8421505
      end
      item
        Name = 'clLightGray'
        DataType = 'TColor'
        Value = -4210753
      end
      item
        Name = 'clWhite'
        DataType = 'TColor'
        Value = -1
      end
      item
        Name = 'clMaroon'
        DataType = 'TColor'
        Value = -8454144
      end
      item
        Name = 'clGreen'
        DataType = 'TColor'
        Value = -16744704
      end
      item
        Name = 'clOlive'
        DataType = 'TColor'
        Value = -8421632
      end
      item
        Name = 'clNavy'
        DataType = 'TColor'
        Value = -16777089
      end
      item
        Name = 'clPurple'
        DataType = 'TColor'
        Value = -8454017
      end
      item
        Name = 'clTeal'
        DataType = 'TColor'
        Value = -16744577
      end
      item
        Name = 'clRed'
        DataType = 'TColor'
        Value = -65536
      end
      item
        Name = 'clLime'
        DataType = 'TColor'
        Value = -16711936
      end
      item
        Name = 'clYellow'
        DataType = 'TColor'
        Value = -256
      end
      item
        Name = 'clBlue'
        DataType = 'TColor'
        Value = -16776961
      end
      item
        Name = 'clFuchsia'
        DataType = 'TColor'
        Value = -65281
      end>
    Dependencies.Strings = (
      'TurtleBasic')
    Functions = <
      item
        Name = 'ComposeColor'
        Parameters = <
          item
            Name = 'Red'
            DataType = 'Float'
          end
          item
            Name = 'Green'
            DataType = 'Float'
          end
          item
            Name = 'Blue'
            DataType = 'Float'
          end>
        ResultType = 'TColor'
        Overloaded = True
        OnEval = dwsFunctionsComposeColorEval
      end
      item
        Name = 'ComposeColor'
        Parameters = <
          item
            Name = 'Red'
            DataType = 'Float'
          end
          item
            Name = 'Green'
            DataType = 'Float'
          end
          item
            Name = 'Bliue'
            DataType = 'Float'
          end
          item
            Name = 'Alpha'
            DataType = 'Float'
          end>
        ResultType = 'TColor'
        Overloaded = True
        OnEval = dwsFunctionsComposeColorEval
      end
      item
        Name = 'Clear'
        Parameters = <
          item
            Name = 'Color'
            DataType = 'Integer'
          end>
        Overloaded = True
        OnEval = dwsFunctionsClearEval
      end
      item
        Name = 'LineTo'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Float'
          end
          item
            Name = 'Y'
            DataType = 'Float'
          end>
        OnEval = dwsFunctionsLineToEval
      end
      item
        Name = 'MoveTo'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Float'
          end
          item
            Name = 'Y'
            DataType = 'Float'
          end>
        OnEval = dwsFunctionsMoveToEval
      end
      item
        Name = 'TurnLeft'
        Parameters = <
          item
            Name = 'Angle'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 90.000000000000000000
          end>
        OnEval = dwsFunctionsTurnLeftEval
      end
      item
        Name = 'TurnRight'
        Parameters = <
          item
            Name = 'Angle'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 90.000000000000000000
          end>
        OnEval = dwsFunctionsTurnRightEval
      end
      item
        Name = 'SaveToFile'
        Parameters = <
          item
            Name = 'FileName'
            DataType = 'string'
          end>
        OnEval = dwsFunctionsSaveToFileEval
      end
      item
        Name = 'LookAt'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Float'
          end
          item
            Name = 'Y'
            DataType = 'Float'
          end>
        OnEval = dwsFunctionsLookAtEval
      end
      item
        Name = 'GetPixelColor'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Integer'
          end
          item
            Name = 'Y'
            DataType = 'Integer'
          end>
        ResultType = 'TColor'
        OnEval = dwsFunctionsGetPixelColorEval
      end
      item
        Name = 'SetPixelColor'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Integer'
          end
          item
            Name = 'Y'
            DataType = 'Integer'
          end
          item
            Name = 'Color'
            DataType = 'TColor'
          end>
        OnEval = dwsFunctionsSetPixelColorEval
      end
      item
        Name = 'Delay'
        Parameters = <
          item
            Name = 'Milliseconds'
            DataType = 'Integer'
          end>
        OnEval = dwsFunctionsDelayEval
      end
      item
        Name = 'PushPosition'
        OnEval = dwsFunctionsPushPositionEval
      end
      item
        Name = 'PopPosition'
        OnEval = dwsFunctionsPopPositionEval
      end>
    Instances = <
      item
        Name = 'Canvas'
        DataType = 'TCanvas'
        OnInstantiate = dwsInstanceCanvasInstantiate
      end
      item
        Name = 'Cursor'
        DataType = 'TCursor'
        OnInstantiate = dwsInstanceCursorInstantiate
      end>
    Synonyms = <
      item
        Name = 'TColor'
        DataType = 'Integer'
      end>
    UnitName = 'TurtleIntermediate'
    Variables = <
      item
        Name = 'CursorColor'
        DataType = 'TColor'
        OnReadVar = dwsVariablesCursorColorReadVar
        OnWriteVar = dwsVariablesCursorColorWriteVar
      end
      item
        Name = 'AntiAliasedLine'
        DataType = 'Boolean'
        OnReadVar = dwsVariablesAntiAliasedLineReadVar
        OnWriteVar = dwsVariablesAntiAliasedLineWriteVar
      end
      item
        Name = 'CanvasColor'
        DataType = 'TColor'
        OnReadVar = dwsVariablesCanvasColorReadVar
        OnWriteVar = dwsVariablesCanvasColorWriteVar
      end
      item
        Name = 'ClientWidth'
        DataType = 'Integer'
        OnReadVar = dwsVariablesClientWidthReadVar
      end
      item
        Name = 'ClientHeight'
        DataType = 'Integer'
        OnReadVar = dwsVariablesClientHeightReadVar
      end
      item
        Name = 'CursorPositionX'
        DataType = 'Float'
        OnReadVar = dwsVariablesCursorPositionXReadVar
        OnWriteVar = dwsVariablesCursorPositionXWriteVar
      end
      item
        Name = 'CursorPositionY'
        DataType = 'Float'
        OnReadVar = dwsVariablesCursorPositionYReadVar
        OnWriteVar = dwsVariablesCursorPositionYWriteVar
      end>
    StaticSymbols = False
    Left = 72
    Top = 192
  end
  object dwsUnitAdvanced: TdwsUnit
    Script = DelphiWebScript
    Dependencies.Strings = (
      'TurtleBasic'
      'TurtleIntermediate')
    Functions = <
      item
        Name = 'ComposeColorHSL'
        Parameters = <
          item
            Name = 'Hue'
            DataType = 'Float'
          end
          item
            Name = 'Saturation'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 1.000000000000000000
          end
          item
            Name = 'Luminance'
            DataType = 'Float'
            HasDefaultValue = True
            DefaultValue = 0.500000000000000000
          end>
        ResultType = 'TColor'
        Overloaded = True
        OnEval = dwsFunctionsComposeColorHSLEval
      end
      item
        Name = 'ComposeColorHSL'
        Parameters = <
          item
            Name = 'Hue'
            DataType = 'Float'
          end
          item
            Name = 'Saturation'
            DataType = 'Float'
          end
          item
            Name = 'Luminace'
            DataType = 'Float'
          end
          item
            Name = 'Alpha'
            DataType = 'Float'
          end>
        Overloaded = True
        OnEval = dwsFunctionsComposeColorHSLEval
      end>
    UnitName = 'TurtleAdvanced'
    StaticSymbols = False
    Left = 72
    Top = 248
  end
  object BadgeList: TBitmap32List
    Bitmaps = <>
    Left = 216
    Top = 144
  end
  object ImageListSuggestion: TPngImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    PngImages = <
      item
        Background = clWindow
        Name = 'Unit'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000001954944415478DA6364C0028466D858BFFBC57C04CC66FB6BF32EE3C8
          51061C8011ABE824FBFFF57ACFC1CCC64B920C0C790719893600663BB201F85C
          C1C830C1FE3FC37F10EB3F4214C8ACD27F0966B65D1287D8F31F28C808B5AF00
          E12246865EBBFFC5FAAF9014A1DBC00816065BF01FA8FC9218034331B2019D76
          FFB30DDEA0D88E620898CF086530304CBD28CCC0507618C98036DBFF49FAEF10
          BAC06AD14D4170E75D10641012FA0F0F134686261B7018B0B03130846A7F82D8
          83A61F62FF7FB077965FE0875A000C7071061BB83270E83FFB7FC4CFE02BA637
          300024DC379FE7614035E0C1FF23CE46DFC19A6191F21F8B765888EC3DCF0164
          9758C1D5581AFD82688219C0C88061CAF1F36C70BE900A13D00B0596FFF58DFF
          21A210A89B9111E263845F10F4C573CC0C42EACC48819863F15FCD8401252DDD
          3A0B893A35A3FFA86E86C94D3981148D9916FFE54C98E0063C3AFB8F41489FC5
          0624F9E9CEDF23521A8C287E787416C89E8E6C409AC57F711366B8829767FE32
          30CC822A4093C390071B90628E19D0734E421460934396070200807CABBC15D2
          16F90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Type'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000019D4944415478DA6364C0018A3A36BB333031348039FF181AFA2A7C77
          6253C788CB80C0E225BF24F8B8183EFCFECD70ECCE7386872B0AD84832C03D7B
          FE7F10BDEBCE3330FFFFCE6A469C0660732E3603B0A963C4E55CF7EC79EE40F3
          77EC02F2FF33FCF360D859B3139B3A4674E77A68C9336888099FF9C7CC9077E3
          E18B63BBEFBC62C875D6B362FACB30E9E9874F261FBF7C43711586014E8A120C
          E202DC7F457879196E3E7BC50C3220D346FBEF8947CF18AE3D7BC76CA7208ECD
          8079EEFF19FE37ECBEFD82818F83ADD74A4ED8554C8027F5C3D75F8C679EBC67
          9014E4F87BE9F1ABB906D242BB05D8D98A41EAFE33FE6F00790B2310FF3330EE
          FBF79739FAC9C78F32876F3F63B690156590E0E7FBCBC9C1F68489F9EF524686
          FF4E7803F1E4BDE74C8C4CACFFEFBDFE70DB485248F3D1C71F0C4652FCD779D9
          D954FF32FE6394E0E3F9873710C1AEF8CF90C7F0F3D774771D85DFA030B0B5F8
          C6CAFE462113A878127AD46237001AE7571FBFD90132A0C051CB0357DAC00844
          58E080BCC5C9CAC874FDCD1786775FBEFF83A50D7475D449CA343200D3B9D8D4
          010076422F2036031B220000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Class'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558744372656174696F6E2054696D6500446920313820466562
          20323030332032323A31383A3138202B30313030C02ACFB40000000774494D45
          07D605170D3134E2ACD590000000097048597300000B1100000B11017F645F91
          000003164944415478DA55935D4C935718C77FA76FFB96022D4509C3B496CAC7
          E617C2D4B1F87531773113352E1B59B660B832317AA35E796322C6182E965D98
          78217E642C734BE60573D1A08BA028A2308D16A6038151110404A49436B4A5E0
          BBA7D4BA70DEFCF39EF39CE7FF3FCF739EE72852E3C88865A5E5F5B15CBB2A9D
          3354416CDEC88FCCC4A3B1D9D8B839161B1B18987A1ABDFED509F10C0AE65234
          F55EA0AAA3BCE2DB75ED05D9E0D420CB02631311AEDCF31B3DFDE344DE8414CD
          072B99196C12EF3181B158E0CBE64FD9FA491BD6B4A4D56A62CBB49FD6E61E59
          CB81335183A6EF6A30E29764578CCC2F16D85DBF910D9B1EA2CBD14ACC669DB2
          F000BE47FDE22A02316D96DB7BCE8AE7394197E0ED6281ED374A29F9C847BA49
          AC129D45A728F88ABE7F5E4AC68908B429DA2AEAC4F3ECBB0892291835981E58
          D992A15359DED9B23F665A26E498C04C5EE015A3FE7190F4898CBCA6EFD0CFCC
          724178BDEF2378D6686A5FE975969B6627B978671BD577AB187A530619429A9A
          84E1490A773EC6FB7503A38189782838D731D61BB81CAD9E3F2FFCB01ABA4DC0
          B55939D13F9475160C8668ACED320E07BE501FACD9CC3707FBE5D01926829286
          A1194D0FFFA6F5CFE793FCC05E21B4AB67750C1516E0B258C1E4B6D376D34DC0
          D745EDFA1C966FF2908E9D25CEA5B872F23870E647C2A31198C04F2D89087E55
          ADA7E9F17828D6CCE070DA78DCED62CAD7C7A98F97A0E73BB06B1632CD1AF96E
          37DFD7364A7584F6824E7EE10F99D5A9AB2779E2CAA32C51BDF40C8C7F87DD84
          9E0EA903A516DEE658C9D67432751D6F6E1EF5F5BE64DD7AB9CFEF34C8EC92FA
          E930476D3AC73332B139E40A82618711F54FAB7DCBE50E1DE29FA6B05A35BC59
          B94677EBB05A68E2475C1189BB32BB9CD0CB5E91CBE79FAD61D786227638D295
          FDDA3DA3E1379D281BD94A365E6C60B7A519217F149E70951686A50B6E09F756
          4220919553E0D1CDACD6148591F882EDA5841B67156B2911A12C3C34CAD74F48
          F6FE12B40806539D98F8EB027BB296483B2E38269A459E178582E2773EDD820E
          C18820FE7F2B2F7E9D2AD5AA32E46D4A3593E229E170EA49FF07327B15C8B061
          AE340000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Interface'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558744372656174696F6E2054696D6500446920313820466562
          20323030332032323A31383A3138202B30313030C02ACFB40000000774494D45
          07D30212171138BA1C262E000000097048597300000B1200000B1201D2DD7EFC
          0000029E4944415478DAA5925B48D37114C7BFBFFF75FF5DDCDCDC52B7DAC85C
          142296E2A5A7E82D2A907A087AEA558302D37AE82DE84296D1FDF262D843D283
          BD4664145968A998E0D0BC65D6D4E136A79B9BBBF8EF6C0B445A2F75E007BFDF
          EF1C3EE79CEF390CFF696CD3EB3944B7B6F08233BFA05C616289C0E04AC4D763
          A958C29748A80BBD33DF86564E459BFF0E7886AA4B07EA3F5B8C4670A2088913
          3113F4E3C5C73E7564FC27124B49063DAC68C1626E4007F61E75EE1CD08A1244
          C6A0C8323CA108DEBEF364236350910F1DCE219A1BF004E57516C7178DC48367
          1C34028FF9B08AFEFE4920457E1E715C84866E6A6EC02DEC72D8F23C5A2A9FA7
          0A6491C76A88C357CF1C9024BF8020AEC0FCA70697C1516775165E3E1188C71B
          7991517601A2C0410E6A303FBD04AC52DC1AE660C40EDCCCBC3600FBDFEFEEAB
          2E2AAA8E45C2E81B9DC5F0B40FD118A5D4913348C70B94546C81ABD28A854028
          114EAE0D2E4C053AA22DC9FB19C0E19EB2E0A1AA4A9340A271EA3A86A6A770A7
          FD8D5A346E6465C7B7E258FD3EC4590A8BA1503AA7DADD3F8C0FAFC6FCB8066B
          06E0EEB4FCA828B6D97504D8EEB0E341772FBCE37ED4AA0ED49EDC0649956036
          99612F2844C3DD7684E76908514CA00DA51980F1B166CC6ED5BBF58284629301
          03A38B989DF4A3D46886A3260F0A44E869224E8703AD8F5EA7C50456F009B751
          9315F106063903DBA34802CC7A454D7A05CC7D0F30C526C2E49461E024E82509
          2E5B21BABA86B2CA45F012F770300BB88ED358C2554AA490CAC88B28EAB22FCA
          60499747F1328D54E6E132DAD4D11E2FCB8C344E0D3CC4D98D3D68450195D580
          101A99843C354913D72140814D9050422B0C8356A3AE4CC44089DAE0C3793C45
          6AF322A5AD89DC41AAA41DCBBF978BC3088E10E40CDDDCF4D34CBD77E6DEC47F
          B05F041BD9119A1FDA180000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Function'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001244944415478DAA5D3314B425114C071DD5D43E813B4B8
          484B0D41832D3A4568103868448358E420061A0421899143AB10A58BD0D0D050
          A38B4BF41D5A04957069EDBDFE47CE83CBED3D253CF0F3DE7BDEF3DC77AECFB0
          EBBAA165224CF8E5D34AA2A7FE84E338BE05D6718E3B8C7189DDFF14384202C7
          F89AF7F841055AD21A4E16F56F17682066DD53C6B5CEBBE8208A7B5953A0633F
          C186F65CC300593C601F79D491D3DC9B5F0B45A470808991DFC495CEB7E79D41
          5BC7BCD54A12159CE23DA8C00A9EF08C1BE3CBF23E9CE9FC02AF4105B6F41790
          5DFAF286724D0E3183383E708B479DC767058C9DAAD8C30E4628A835BDEEAD25
          4A78996D629C7E13533DC4A088E8F82D1F5E8191268738C4A7B467F8B1D6AE37
          4A4881553369DDBCB8C0B27FE75F70B37AE11AB1CC900000000049454E44AE42
          6082}
      end
      item
        Background = clWindow
        Name = 'Procedure (new)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507DE0C110A2133F76DFB270000014149
          44415478DA6364A010308E1A80DF80A6A6A6485656568FCF9F3FEBFDFAF54BF3
          FBF7EF37BE7EFDBAE3E5CB972BB76FDF7E1EC580D8D8D83C202500C4A640FC7A
          F1E2C5490B172EFCAFAAAACAF0E6CD1B86F7EFDF33003532000D03F3A74F9FAE
          07547719AF0BAAAAAA5ECAC8C888A9ABAB33AC5DBB9681919191E1DFBF7F2043
          DE2D59B22409A8642323D4F60820250FC4DE40FC11889F025D9051585878858B
          8B4B5B515191E1D4A95360CD7FFFFE6578F7EEDD838D1B372603D5EDC3EB82CC
          CCCC7DFCFCFC8EA2A2A20C57AE5C011BF0E7CF1F86172F5E5CDFBB776F3C50C9
          69E430880252207F6903F127A00BA2555454141C1C1CCE4B4B4B0B3C78F0006C
          C0A74F9F5E015D73E4F9F3E78540758F888946C5E8E8E883407F335EB870E1D4
          A3478FF602C57601F15D20FE4F6C3A1086C6CE75207E0CC4FFE89390E8620000
          3C5891113BBA3AB40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Method (new)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507DE0C110A231FF783F5460000014B49
          44415478DA6364A010308E1A80DF8078EF2D91ECECCC1E9FBF7CD2FBF3E787E6
          CFDF9F6FFCF8F561C7FBCF0F579EBE36FB3C8A01CEA60D79404A00884D81F8F5
          DED30D49ED15C7FFEB9B88313C7DFC99E1CDCBEF0C8FEE7D62F8FCE91BC3C72F
          4F18361FC9D503AABB8CD705916E2B5FCA2B8A8B19598A332C9EB38781919191
          E1FFFFFF0CDF7EBC79B7F74C631250C94646A8ED11404A1E88BD81F823103F05
          BA2023D871DE1576565E6D756D498693274F8235FFFBFF9BE1D3D7A70F8E5F9E
          920C54B70FAF0B7C6D26EFE3E6147614151565B875EF0CC3BF7F7FC0F8EDA73B
          D72FDC5A1A0F54721A390CA28014C85FDA40FC09E882684961030503B598F3A2
          026A02CFDF5E04DAFE17E4FC57371E6C39F2FEF38342A0BA47C444A3A2B349FD
          C1AF3FDE30DE7DB2EFD4EB0FD7F702C57601F15D20FE4F6C3A1086C6CE75207E
          0CC4FFE89390E862000020CC911160EE3F770000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Constructor'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002A744558744372656174696F6E2054696D65004D692035204D727A20
          323030332032333A33363A3031202B30313030BF3F034C0000000774494D4507
          D3030517123A8F915257000000097048597300000B1200000B1201D2DD7EFC00
          0002ED4944415478DA6364A010308288FF40FA8A1283BACC3D867B820C0CBF60
          92FF8D198ABE0BF3D45CDBFDA5C1E43FC324B8B810031FE33B864F7003BE38B0
          2DE552138ABAB6F0C5A5154DDDF3D99918B59E7CFEA175EDDA45938D7FB6B387
          33FB7ED65557DAF08B91F5F8E4A30D7EFF4484DC4FEF78D763F185A10C6CC03B
          4B8E776FE50404BBF982181859997EBCBB79656DC9F3031E6C9F18F8AE3F67F8
          B78D5FACE39DBD8DB3B6869635EFFB5B8CB53F77321CDAF8F183FD3B0641B001
          475819F25899193AE6720AEEFA9192C19A7D658B9B99E73F16860B2F18766FFB
          CEB0B7AAEB1FC3F3A7576E3C7CBCDB65DD128B004E06ABD3EF19660631306432
          C2FC553361D2FA13FBF62EDFB369E3AA37D20C8B84CDB863199E7F65D87F9AE1
          4BAC96BEEAD3CB175F80D4256664BA317EFA5C336FD9123B781880404B4BCBE4
          0B172EDC5BB3664DFF7526069587AC0C9B7EFE6690DEE9EEFF7BDAF68D223075
          256565C11F3F7D8A993D6346208A01F5F5F5150F1E3E145EB8604129889FD3D6
          91F2EBF2B9763D31069ED0C9AB0AC4FF31CC0489D7D5D525BC7DFBD661EAD4A9
          092806A496D7C67E7BFDDC7FE9BC39210141C17C6262A20D1FBF7C2D5CF16335
          43F11787270FC5C53DD62E5C78B5B2AD2BF7EDFBB72AB3BA3BF391D301D3C740
          91F3775904746F9EFF7D668BA3BB34DB9FDFB71999583413E7CE5EDD67627657
          C0C4244995E9F77DBF4F67554EBD606094DC73CEDE8B81E115D88047920CDCA2
          8A2CEF4F2A68B3EE782D7A7BED8B57C6F6565666AC3C3C73A7F7F62880D498DA
          D8B284B0FD581367F4CB5FE2C4458649471892814E9807F7C249068688470C0C
          6E5B8A8A158538B979D8AF9F5D7C4F46A7B16A76A7415F5A919DAAA850F9AB3F
          0C7F9CE7F47148BE7EF7F7DE4F06B7480686A770039000734C4868F762E6BD85
          FBC5351958971FFD3731BAF8FADBAB971AF6EFDEBD16E263B4BC800EFE4B3018
          32E8739F63E0FAC770F5F0F77F8FDF3088783230BCC79999B0891FE26298FB93
          95C1FFDD4786A9E1C0D8C39B1B29010048AF2D204AE1CEB90000000049454E44
          AE426082}
      end
      item
        Background = clWindow
        Name = 'Property'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002CE4944415478DA8D536B489351187EBEDD950D5410A172
          51242B4231339AB7A93469CB7651F187045AFE2875761F9A195210C3451752B3
          F2871448C1D22D2D6125A5B84F7FE4A5FCB335FD2142A66CE66233B4DCA5F37D
          641004FAC0C7790EEF779EF3BECFFB1EAAB4B4140CAAC43676A5281CA580026C
          820830F02B8837D486C036EE12D40207435B95579FD6CEB95CF02D2FE3606E2E
          42C120BCF3F3E0F278E0F3F9E00B04A05B0C6D9108EC7F0518247096904CB9B4
          F194579778E474E581AC2C0CF6F6225FA7836F71111E22228A8A82283A1A4291
          08C31DF59D8CC06E72F630F9248CC8CEC8ECBE4C29D4C7CAAB650EBB1D877272
          30E6702047A582CFE341C0E7834028C48FE505EF647FE77BAAB8B8F884D1686C
          888F8FDFEF750E61D53D00B9221796AE2E1CD7EB31383080FC82027C1A1F476A
          7A3ABE793C9119E7946F66ECDD14978327945EAFAF36180CEDE17018B3963A94
          559CC42B9B157E5E02027393A8385589E1A12128F2F2304AD3F8E2F98ED7831F
          5E28A4E10BD17CEA2B53C2198BC5F288497F66B417A3CF9B9159760549993AB8
          1C5638BA4CD068B5A0491971A93AC42465C06C365791DF1FEFF8E90455545454
          D5DDDDFD30422CFD1F3ED3368C3C6BC6AEBC72C4ECC9206DA6603299AA4988BD
          94D26834353D3D3D0F325EEEC588D60907B92936361622E2B280B44B2A95FE23
          C8E1705052526220B49D1550ABD5B556ABB535140A6D363B2CB85C2E88F16709
          6D6305944AE5399BCD76FF635323929B6E6062620251A4D71B585D5D65F71B6B
          4A4A0A48D9E749A8851550281417FBFAFAEEAEAFAF6F29036612B55AED2542EF
          B10272B9FC727F7FFFED86EE385CD72CC0C58C301916C607061B5C2C16233131
          114232448585854612BAE3F7FB41A5A5A5D5D9ED76F3DADADA963260CC55A954
          F52B2B2BB7A6A7A741C964B2469AA66F06C983D90A78E4416567675F73BBDD1D
          8C45944422A921CE6EDFD2E93F201DF3060281B7842EFF06C64E0EBA0D9E736D
          0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Enum'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000001654944415478DA9593BD8E824010C76789C48010A1E301AEBACE86C2
          C6C25C7C9093C6E65A2542CC4583B1BF2B7D06FA4BD0C677F0198C60207E6075
          B73B6445343965933FB31F33BF19868500C02BD53B9545A5C2F383F907843E7A
          8BC5E2CB344DDC258414BCF8FAD62E974BE8743A3DB6FA3D1C0EB0DD6EF1F05A
          8220FCBBA7EB3A5C006118E2E66C36837EBF8F8ED3E9B49099CF1DC7C1B9A669
          19E0783C228005B383C160F05435F57A3D07445174A980033CCF2B64E6D6755D
          04AAAA9A0376BBDDA56CDBB69FEA83A22839208EE33B475601CFCCDFFB1A58AB
          D532C0E97442C07ABD06DFF7A1DD6E43B3D97CD80759967340922498B1DBEDC2
          7C3E87E1700893C9E4EE0B303B1A8DD04A929401D23445C066B3C160CBB2C030
          8C877DA856AB19E07C3EC37EBFC78CD79D1E8FC738E7196F61A228E6007699CA
          DC42A64AA58280CF20089C46A3512A78B55A41ABD5FA608017AA37AAEF127F22
          1B3DAA1F5232E86EFC01EA2C90238BC65E4A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Parameter'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000001464944415478DA636400
          02131393AEFFFFFFA70099B3CE9E3D5B011233333353FBFBF7EF56101B28E779
          EEDCB93BD8D43282048D8D8D7F32313119FDFBF70F287696032466646454C5C8
          C828CD00014F80E2EDD8D4C20CF80FA4403677000519A136353020813367CE34
          6053CB081448810AAC01E210A0AD6D40C5F3B019804D2DC880FBCCCCCC4EA74E
          9DBA0FF4B722D0DF7B80262BE33000432DC880874086FD850B171E4005F7030D
          50C06100865A742F84029DD54AA417C06A290F4498208803A3610600A3CA1AC4
          0646DB51640390D56235C0DCDC9CEFCF9F3FC78009E61C481EE854430E0E0EAB
          A3478F7E26CA0060224A006A7200B213A0F20B81FCBD40572C22CA00A0F39380
          B6DB2119B000C83F004CCE0BE8E3057202F1273051E80053D965B4CC24018DC5
          E7C89909592DCC365016CD033227A065E75D90DCFCDF152D3BC3D50200891E70
          0B57D7524A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Field'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C180000000774494D4507DE
          0C110A06325AAF79D4000002B44944415478DA95936D48535118C7FF676F7A7D
          4BC7AC46A9999A4669A0592858D6873008A23E8898A995686ED76923C95E3E0D
          D1FA5253C4D2D46C8149F8B9A20F518241592B156D2A6926B5DCA6A96976A7F3
          74BCB695561F7A0E97C373EFF3FB9FE7F9732E199AA2E914985B58C022FE2364
          324808C011CB384D8B56E2C1FFC0EEE89FC041323449F786AFC193B28A1A2649
          B04829D806B679E2F7DC5D73F9028FE129A4128B9D2645ABF0EC5CB911572E95
          88451F1DD3D8A00AF008ACCEDDB5FD0E2493411BDD19198C4EBDE11A74FA1208
          8200397B1454CE4E96802CAD9F1D50B6944A05386F8928E45A441B19F8427744
          05E28DAEA21655F55A084EC0D64B91D51E004EEAB762E6A2D84A1C0ACB8139C1
          0FDF476691EC60E3BE9DA05B6382D0A73554A3B4AE5814F0B25084971378CB7F
          C155476A919F5408FF22336C0F13C0B552D8F24817191DA79B372AF1AEA0D480
          E2B37A368213B27927BCA16086B11158FF2A1587007F8508D769A39119E38BB9
          2882EE2966F0889D8684AAF0E1A4EE3C9AAA2BC5D35E740F62575CD48AF639CD
          2B34EA62B03DC217B9F5A3E830862E0B0C8FD1759BD6E2738EA6145ACD69389D
          4E48A512B898431121EBA10E0E8297A60BB774911EF83517025C37817666837C
          A2F3416AC82632F34AD0D260848BB9DDF1B2172989DB98FFECC6F17D30F1617F
          85593491593AE3E703DFAF19277868B45AD6810099548AD4DDB1A8C9255096CD
          FC0BC637CCFA13FADEAE40984A48CFD5E05E73ADF8C16A9F64AD078A023C7F14
          F1CF8D2BE0478FDB7160FF1E60C4E145B2F2CFE04EDD551A9F9206EB981D4BD7
          C53A605E866F5A593A06741B400A0F8BF0ED963654DD6882B9FD3E8E17E809A1
          0D77E53895E15CFDA3B805B253D5626EEAA0F8231A5B15043EC18990C88E8150
          A9FB3D9DB6F2EA2D09B8986C06DFBC0CC625EDEBE9E9B53CF5C094B8202C987E
          0023E624B74D717A890000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Key'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000006624B474400FF00FF00FFA0BDA793000000097048597300000B1300
          000B1301009A9C180000000774494D4507DE0C110A131CB1CC920F000000BD49
          44415478DA6364A0103012A1460788BB80D809CADF07C4A5407C951803B481F8
          1810F3A1897F02624B20BE46C8802D40EC8DC437056266203E01C49B81D88F90
          015F81980B896F0135E028548E879001DF80981387DC6790D7F019D00CC43578
          E4D7037110231ECD7F81B81E87FC07203607E25BD80C6802E27F5036B2019FA1
          86EE05E24A20BE0D124437A01188FF63D15C0F3518033012D04C30A1C11434A0
          D94694666445FFA12E204933BA0B609A1BD15C44940164038A0D000002872411
          55B58FC20000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'About'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000001FE4944415478DA63FCFFFF3F03258011640023232356C99BBDFC3341
          B47AF1C7746CF260BDF80C78B12BF73B8896709BCC49B20197AB046B356A9634
          3101A56E74C4B46B37BEAF22DA80E2304EF5A208EF3D62EEAE320CFF18185EED
          DEFDA46FC5D682DE55DFD76235A03599BB33448C83834BE4AFE5A7EF7A222C2C
          5FA595D322D8FE33FE04AA02A96467B8377BC5AF3FBFB99FF2715F7AF3FD1DD3
          F1D5CF7FFEA89EFBB51C6CC0CF55A273EF3E1148524BC962F8FFFB3D3068FF32
          30FCFB0DD408B4FE3FC87540CCC80A14636560641364B8356F2A83B2F2BB0DEC
          016F02E15EF8B64668D9DDCBFF23D5522C811AB8811AFF02F5C83030FCFDC0F0
          FFCF57209F19C87ECE706BC11506657D86E55CA1EFA330C2E0FB3CF1652F3E7F
          8A147766064A80C4402E6002BB02A4EEE5C13F0C12827CCB39E35E46E10CC43B
          6D0EDBA4BC2F7932323281F543C28001EC9D27DBB4D7A9D61E0EC6190B9F32E4
          245F707E58229FC0EE04F40103233344F3FF3F403650C9E3E53FCE897D110CE2
          9BF1E8212E030CDE8B7DDA2611CC26F9E7D35786D7FBC07633883A3230B2F072
          33BC58FFEBB9E02B3E2FA001177019E0F556E2F502A67F4C223F1F71EE949461
          CF0045C0F3C73F67B0CB7C776760FBC728F84CD41B68C0365C064433B0FCEB65
          D0F9B0952FE34B3272A2F93483672EC335016F865F4CC5400396A21840716EA4
          0400009EE315F0CA5B0F3A0000000049454E44AE426082}
      end>
    PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    Left = 312
    Top = 144
    Bitmap = {}
  end
  object dwsUnitTeacher: TdwsUnit
    Dependencies.Strings = (
      'TurtleBasic'
      'TurtleIntermediate')
    UnitName = 'Teacher'
    Variables = <
      item
        Name = 'TutorialText'
        DataType = 'string'
        OnReadVar = dwsUnitTeacherVariablesTutorialTextReadVar
        OnWriteVar = dwsUnitTeacherVariablesTutorialTextWriteVar
      end>
    StaticSymbols = False
    Left = 312
    Top = 248
  end
end
