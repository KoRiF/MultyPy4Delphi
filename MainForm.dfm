object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple P4D Demo'
  ClientHeight = 599
  ClientWidth = 957
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
    Top = 341
    Width = 957
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 201
    ExplicitWidth = 383
  end
  object sePythonCode: TSynEdit
    Left = 0
    Top = 15
    Width = 957
    Height = 326
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
    ExplicitTop = 13
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 0
    Width = 957
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
        Width = 957
      end>
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 558
    Width = 957
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnRun: TButton
      Left = 24
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 344
    Width = 957
    Height = 214
    Align = alClient
    TabOrder = 3
    ExplicitTop = 348
    ExplicitHeight = 210
    object HeaderControl2: THeaderControl
      Left = 1
      Top = 1
      Width = 955
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
          Width = 955
        end>
      ParentFont = False
    end
    object mePythonOutput: TMemo
      Left = 1
      Top = 18
      Width = 955
      Height = 174
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitHeight = 191
    end
    object ComboBox1: TComboBox
      Left = 1
      Top = 192
      Width = 955
      Height = 21
      Align = alBottom
      ItemIndex = 0
      TabOrder = 2
      Text = 'C:\ProgramData\Anaconda3'
      OnChange = ComboBox1Change
      Items.Strings = (
        'C:\ProgramData\Anaconda3'
        'c:\ProgramData\Anaconda3\envs\p_38_idera\')
      ExplicitLeft = 128
      ExplicitTop = 193
      ExplicitWidth = 145
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
end
