object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Multy Environmental Demo '
  ClientHeight = 661
  ClientWidth = 624
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 0
    Width = 624
    Height = 0
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 341
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 661
    ActivePage = TabSheetVEnvSysInfo
    Align = alClient
    TabOrder = 0
    object TabSheetVEnvSysInfo: TTabSheet
      Caption = 'Virtual Environment'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 64
        Align = alTop
        TabOrder = 0
        object SpeedButtonSelectDll: TSpeedButton
          Left = 448
          Top = 6
          Width = 23
          Height = 22
          OnClick = SpeedButtonSelectDllClick
        end
        object btnRun: TButton
          Left = 535
          Top = 34
          Width = 75
          Height = 25
          Caption = 'Run'
          Enabled = False
          TabOrder = 0
          OnClick = btnRunClick
        end
        object ComboBoxPyVEnv: TComboBox
          Left = 17
          Top = 6
          Width = 264
          Height = 21
          Align = alCustom
          ItemIndex = 0
          TabOrder = 1
          Text = 'C:\ProgramData\Anaconda3'
          OnChange = ComboBoxPyVEnvChange
          Items.Strings = (
            'C:\ProgramData\Anaconda3'
            'c:\ProgramData\Anaconda3\envs\p_38_idera\')
        end
        object EditPythonDll: TEdit
          Left = 287
          Top = 6
          Width = 162
          Height = 21
          Enabled = False
          TabOrder = 2
          Text = 'Python 3.8'
          OnChange = EditPythonDllChange
        end
        object ToggleSwitchPythonLock: TToggleSwitch
          Left = 17
          Top = 33
          Width = 249
          Height = 20
          StateCaptions.CaptionOn = 'Python Locked (Active)'
          StateCaptions.CaptionOff = 'Python Environment Unlocked (Inactive)'
          TabOrder = 3
          OnClick = ToggleSwitchPythonLockClick
        end
        object CheckBoxAllowNumPy: TCheckBox
          Left = 287
          Top = 33
          Width = 184
          Height = 17
          Caption = 'Enable TNumPy (NumPy4Delphi)'
          TabOrder = 4
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 64
        Width = 616
        Height = 569
        Align = alClient
        TabOrder = 1
        object HeaderControl2: THeaderControl
          Left = 1
          Top = 224
          Width = 614
          Height = 17
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Sections = <
            item
              Alignment = taCenter
              AutoSize = True
              ImageIndex = -1
              Text = 'Python Output'
              Width = 614
            end>
          ParentFont = False
        end
        object mePythonOutput: TMemo
          Left = 1
          Top = 241
          Width = 614
          Height = 327
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object sePythonCode: TSynEdit
          Left = 1
          Top = 16
          Width = 614
          Height = 208
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 2
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Highlighter = SynPythonSyn
          Lines.Strings = (
            'import sys'
            'print(f'#39'Run #{shared_variable.Value}'#39')'
            'print("Python version")'
            'print (sys.version)'
            'print("Version info.")'
            'print (sys.version_info)'
            'print("Environment prefix.")'
            'print (sys.prefix)'
            'print("Environment base prefix.")'
            'print (sys.base_prefix)'
            'shared_variable.Value =  shared_variable.Value + 1'
            'print(f'#39'Next run -> #{shared_variable.Value}'#39')')
        end
        object HeaderControl1: THeaderControl
          Left = 1
          Top = 1
          Width = 614
          Height = 15
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Sections = <
            item
              Alignment = taCenter
              AutoSize = True
              ImageIndex = -1
              Text = 'Python Source code'
              Width = 614
            end>
          ParentFont = False
        end
      end
    end
    object TabSheetData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      object StringGridDataTable: TStringGrid
        Left = 64
        Top = 88
        Width = 549
        Height = 225
        ColCount = 1
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goFixedRowDefAlign]
        TabOrder = 0
      end
      object SpinEditGridRows: TSpinEdit
        Left = 25
        Top = 88
        Width = 33
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 1
        OnChange = SpinEditGridRowsChange
      end
      object SpinEditGridColumns: TSpinEdit
        Left = 64
        Top = 60
        Width = 33
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 2
        Value = 1
        OnChange = SpinEditGridColumnsChange
      end
      object ButtonPassTableData: TButton
        Left = 392
        Top = 377
        Width = 161
        Height = 25
        Caption = 'Pass Table Data to Python'
        TabOrder = 3
        OnClick = ButtonPassTableDataClick
      end
      object RadioGroupPassTableOption: TRadioGroup
        Left = 64
        Top = 336
        Width = 281
        Height = 105
        Caption = 'Pass Table Data Options'
        Items.Strings = (
          'Use Python script injections'
          'Use CPython Engine Instructions'
          'Use PyDelphiWrapper (wrap array as object field)'
          'Use NumPy4Delphi wrapping'
          'Use Shared Memory')
        TabOrder = 4
      end
      object LabeledEditTablePyIdentifier: TLabeledEdit
        Left = 152
        Top = 61
        Width = 217
        Height = 21
        EditLabel.Width = 110
        EditLabel.Height = 13
        EditLabel.Caption = 'Table Python Identifier'
        TabOrder = 5
        Text = 'delphi_pytable'
      end
      object ButtonTestNumPyShare: TButton
        Left = 0
        Top = 0
        Width = 613
        Height = 25
        Caption = 
          'Quck Test: Share (1, 2, 3, 4) Array as  NumPy ndarray '#39'delphi_py' +
          'array'#39' and its buffer as '#39'array_data'#39
        Enabled = False
        TabOrder = 6
        OnClick = ButtonTestNumPyShareClick
      end
    end
    object TabSheetAppScript: TTabSheet
      Caption = 'Python Application Script'
      ImageIndex = 2
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 41
        Align = alTop
        TabOrder = 0
        object SpeedButton1: TSpeedButton
          Left = 448
          Top = 6
          Width = 23
          Height = 22
          Enabled = False
          OnClick = SpeedButtonSelectDllClick
        end
        object ButtonRunPyAppScript: TButton
          Left = 534
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Run Script'
          Enabled = False
          TabOrder = 0
          OnClick = ButtonRunPyAppScriptClick
        end
        object ComboBoxPyVEnvView: TComboBox
          Left = 17
          Top = 5
          Width = 264
          Height = 21
          Align = alCustom
          Enabled = False
          ItemIndex = 0
          TabOrder = 1
          Text = 'C:\ProgramData\Anaconda3'
          OnChange = ComboBoxPyVEnvChange
          Items.Strings = (
            'C:\ProgramData\Anaconda3'
            'c:\ProgramData\Anaconda3\envs\p_38_idera\')
        end
        object EditPythonDllView: TEdit
          Left = 287
          Top = 6
          Width = 162
          Height = 21
          Enabled = False
          TabOrder = 2
          Text = 'Python 3.8'
        end
      end
      object MemoSysInfoOutput: TMemo
        Left = 0
        Top = 41
        Width = 616
        Height = 592
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object Panel4: TPanel
        Left = 0
        Top = 41
        Width = 616
        Height = 592
        Align = alClient
        TabOrder = 2
        object HeaderControl3: THeaderControl
          Left = 1
          Top = 243
          Width = 614
          Height = 17
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Sections = <
            item
              Alignment = taCenter
              AutoSize = True
              ImageIndex = -1
              Text = 'Python Output'
              Width = 614
            end>
          ParentFont = False
        end
        object MemoAppScriptOutput: TMemo
          Left = 1
          Top = 260
          Width = 614
          Height = 331
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object SynEditPythonScript: TSynEdit
          Left = 1
          Top = 1
          Width = 614
          Height = 242
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 2
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Highlighter = SynPythonSyn
          Lines.Strings = (
            'import delphi_module'
            'from delphi_module import array_data'
            'print(array_data)'
            'from delphi_module import delphi_pyarray'
            'print(delphi_pyarray)')
        end
      end
    end
  end
  object SynPythonSyn: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 760
    Top = 32
  end
  object SynEditPythonBehaviour: TSynEditPythonBehaviour
    Editor = sePythonCode
    Left = 760
    Top = 80
  end
  object PythonGUIInputOutput: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = mePythonOutput
    Left = 880
    Top = 80
  end
  object OpenDialogPythonDll: TOpenDialog
    Filter = 'Python Dynamic Link Library|python*.dll'
    Left = 488
    Top = 558
  end
  object PythonDelphiVar1: TPythonDelphiVar
    Module = '__main__'
    VarName = 'shared_variable0'
    OnChange = PythonDelphiVar1Change
    Left = 512
    Top = 553
  end
  object PythonModule1: TPythonModule
    ModuleName = 'delphi_module'
    Errors = <>
    Left = 472
    Top = 553
  end
  object PyDelphiWrapper1: TPyDelphiWrapper
    Module = PythonModule1
    Left = 428
    Top = 559
  end
end
