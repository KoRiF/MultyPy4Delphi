object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple P4D Demo'
  ClientHeight = 599
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
    Top = 257
    Width = 624
    Height = 0
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 341
  end
  object sePythonCode: TSynEdit
    Left = 0
    Top = 15
    Width = 624
    Height = 242
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Highlighter = SynPythonSyn
    Lines.Strings = (
      'import sys'
      'print("Python version")'
      'print (sys.version)'
      'print("Version info.")'
      'print (sys.version_info)'
      'print("Environment prefix.")'
      'print (sys.prefix)'
      'print("Environment base prefix.")'
      'print (sys.base_prefix)')
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 0
    Width = 624
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
        Width = 624
      end>
    ParentFont = False
    ExplicitWidth = 957
  end
  object Panel1: TPanel
    Left = 0
    Top = 558
    Width = 624
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitWidth = 957
    object SpeedButtonSelectDll: TSpeedButton
      Left = 448
      Top = 6
      Width = 23
      Height = 22
      OnClick = SpeedButtonSelectDllClick
    end
    object btnRun: TButton
      Left = 534
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
    object ComboBoxPyVEnv: TComboBox
      Left = 9
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
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 257
    Width = 624
    Height = 301
    Align = alClient
    TabOrder = 3
    ExplicitTop = 344
    ExplicitWidth = 957
    ExplicitHeight = 214
    object HeaderControl2: THeaderControl
      Left = 1
      Top = 1
      Width = 622
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
          Width = 622
        end>
      ParentFont = False
      ExplicitWidth = 955
    end
    object mePythonOutput: TMemo
      Left = 1
      Top = 18
      Width = 622
      Height = 282
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitWidth = 955
      ExplicitHeight = 195
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
end
