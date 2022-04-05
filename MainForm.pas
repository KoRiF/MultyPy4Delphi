unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, PythonGUIInputOutput, SynEditPythonBehaviour,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    sePythonCode: TSynEdit;
    HeaderControl1: THeaderControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    HeaderControl2: THeaderControl;
    mePythonOutput: TMemo;
    SynPythonSyn: TSynPythonSyn;
    SynEditPythonBehaviour: TSynEditPythonBehaviour;
    PythonGUIInputOutput: TPythonGUIInputOutput;
    btnRun: TButton;
    ComboBox1: TComboBox;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    _PythonHome: String;
    _PythonEngine: TPythonEngine;
  public
    { Public declarations }
    property PythonHome: String read _PythonHome write _PythonHome;
    property PythonEngine: TPythonEngine read _PythonEngine;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.Threading,
  System.Math;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  _PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.IO :=  PythonGUIInputOutput;
  PythonEngine.SetPythonHome(PythonHome);
  PythonEngine.LoadDll();
  PythonEngine.ExecString(UTF8Encode(sePythonCode.Text));
  FreeAndNil(_PythonEngine);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  PythonHome := ComboBox1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PythonHome := 'C:\ProgramData\Anaconda3';
end;

begin
  MaskFPUExceptions(True);
end.
