object DataModuleShared: TDataModuleShared
  OldCreateOrder = False
  Height = 324
  Width = 461
  object DelphiWebScript: TDelphiWebScript
    Config.CompilerOptions = [coOptimize, coSymbolDictionary, coContextMap, coAssertions]
    Config.OnInclude = DelphiWebScriptInclude
    Config.OnNeedUnit = DelphiWebScriptNeedUnit
    Left = 72
    Top = 16
  end
  object dwsUnitText: TdwsUnit
    Script = DelphiWebScript
    Classes = <
      item
        Name = 'TTextOutput'
        Methods = <
          item
            Name = 'GetText'
            ResultType = 'String'
            OnEval = dwsClassesTTextOutputMethodsGetTextEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsClassesTTextOutputMethodsSetTextEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'Clear'
            OnEval = dwsClassesTTextOutputMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'Write'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsClassesTTextOutputMethodsWriteEval
            Kind = mkProcedure
          end
          item
            Name = 'WriteLine'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'string'
              end>
            OnEval = dwsClassesTTextOutputMethodsWriteLineEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Text'
            DataType = 'String'
            ReadAccess = 'GetText'
            WriteAccess = 'SetText'
          end>
      end>
    Functions = <
      item
        Name = 'ClearText'
        OnEval = dwsFunctionsCenterEval
      end
      item
        Name = 'WriteLine'
        Parameters = <
          item
            Name = 'Text'
            DataType = 'Variant'
          end>
        OnEval = dwsFunctionsWriteLineEval
      end
      item
        Name = 'Write'
        Parameters = <
          item
            Name = 'Text'
            DataType = 'Variant'
          end>
        OnEval = dwsFunctionsWriteEval
      end>
    Instances = <
      item
        Name = 'TextOutput'
        DataType = 'TTextOutput'
        OnInstantiate = dwsInstancesTextOutputInstantiate
      end>
    UnitName = 'Text'
    StaticSymbols = False
    Left = 72
    Top = 72
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
            Name = 'Blue'
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
            HasDefaultValue = True
            DefaultValue = 100
          end
          item
            Name = 'WaitForRefresh'
            DataType = 'Boolean'
            HasDefaultValue = True
            DefaultValue = False
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
      end
      item
        Name = 'CursorWidth'
        DataType = 'Float'
        OnReadVar = dwsUnitIntermediateVariablesCursorWidthReadVar
        OnWriteVar = dwsUnitIntermediateVariablesCursorWidthWriteVar
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
        ResultType = 'TColor'
        Overloaded = True
        OnEval = dwsFunctionsComposeColorHSLEval
      end>
    UnitName = 'TurtleAdvanced'
    StaticSymbols = False
    Left = 72
    Top = 248
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
        OnReadVar = dwsVariablesCursorVisibleReadVar
        OnWriteVar = dwsVariablesCursorVisibleWriteVar
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
  object dwsUnitInput: TdwsUnit
    Script = DelphiWebScript
    Dependencies.Strings = (
      'TurtleIntermediate')
    Enumerations = <
      item
        Name = 'TMouseButton'
        Elements = <
          item
            Name = 'mbLeft'
          end
          item
            Name = 'mbMiddle'
          end
          item
            Name = 'mbRight'
          end>
      end>
    Functions = <
      item
        Name = 'GetMousePositionX'
        Parameters = <
          item
            Name = 'LimitToBounds'
            DataType = 'Boolean'
            HasDefaultValue = True
            DefaultValue = False
          end>
        ResultType = 'Integer'
        OnEval = dwsFunctionsGetMousePositionXEval
      end
      item
        Name = 'GetMousePositionY'
        Parameters = <
          item
            Name = 'LimitToBounds'
            DataType = 'Boolean'
            HasDefaultValue = True
            DefaultValue = False
          end>
        ResultType = 'Integer'
        OnEval = dwsFunctionsGetMousePositionYEval
      end
      item
        Name = 'GetMousePosition'
        Parameters = <
          item
            Name = 'LimitToBounds'
            DataType = 'Boolean'
            HasDefaultValue = True
            DefaultValue = False
          end>
        ResultType = 'TPoint'
        OnEval = dwsFunctionsGetMousePositionEval
      end
      item
        Name = 'ReadKey'
        ResultType = 'String'
        OnEval = dwsFunctionsReadKeyEval
      end
      item
        Name = 'ReadMouseButton'
        Parameters = <
          item
            Name = 'MouseButton'
            DataType = 'TMouseButton'
          end>
        ResultType = 'Boolean'
        Overloaded = True
        OnEval = dwsFunctionsReadMouseButtonEval
      end
      item
        Name = 'ReadMouseButton'
        ResultType = 'Boolean'
        Overloaded = True
        OnEval = dwsFunctionsReadMouseButtonEval
      end>
    Records = <
      item
        Name = 'TPoint'
        Members = <
          item
            Name = 'X'
            DataType = 'Integer'
          end
          item
            Name = 'Y'
            DataType = 'Integer'
          end>
        Properties = <>
      end>
    UnitName = 'Input'
    StaticSymbols = False
    Left = 160
    Top = 72
  end
  object dwsUnitShapes: TdwsUnit
    Script = DelphiWebScript
    Dependencies.Strings = (
      'TurtleIntermediate')
    Functions = <
      item
        Name = 'DrawCircle'
        Parameters = <
          item
            Name = 'Radius'
            DataType = 'Float'
          end>
        Overloaded = True
        OnEval = dwsFunctionsDrawCircleEval
      end
      item
        Name = 'DrawCircle'
        Parameters = <
          item
            Name = 'Radius'
            DataType = 'Float'
          end
          item
            Name = 'Color'
            DataType = 'TColor'
          end>
        Overloaded = True
        OnEval = dwsFunctionsDrawCircleEval
      end
      item
        Name = 'DrawCircle'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Float'
          end
          item
            Name = 'Y'
            DataType = 'Float'
          end
          item
            Name = 'Radius'
            DataType = 'Float'
          end
          item
            Name = 'Color'
            DataType = 'TColor'
          end>
        Overloaded = True
        OnEval = dwsFunctionsDrawCircleEval
      end
      item
        Name = 'DrawCircle'
        Parameters = <
          item
            Name = 'X'
            DataType = 'Float'
          end
          item
            Name = 'Y'
            DataType = 'Float'
          end
          item
            Name = 'Radius'
            DataType = 'Float'
          end>
        Overloaded = True
        OnEval = dwsFunctionsDrawCircleEval
      end
      item
        Name = 'DrawRectangle'
        Parameters = <
          item
            Name = 'Left'
            DataType = 'Float'
          end
          item
            Name = 'Top'
            DataType = 'Float'
          end
          item
            Name = 'Right'
            DataType = 'Float'
          end
          item
            Name = 'Bottom'
            DataType = 'Float'
          end
          item
            Name = 'Color'
            DataType = 'TColor'
          end>
        Overloaded = True
        OnEval = dwsUnitShapesFunctionsDrawRectangleEval
      end
      item
        Name = 'DrawRectangle'
        Parameters = <
          item
            Name = 'Left'
            DataType = 'Float'
          end
          item
            Name = 'Top'
            DataType = 'Float'
          end
          item
            Name = 'Right'
            DataType = 'Float'
          end
          item
            Name = 'Bottom'
            DataType = 'Float'
          end>
        Overloaded = True
      end>
    UnitName = 'Shapes'
    StaticSymbols = False
    Left = 160
    Top = 136
  end
end
