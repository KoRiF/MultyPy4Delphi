unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, PythonGUIInputOutput, SynEditPythonBehaviour,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, Vcl.ExtCtrls,
  Vcl.Buttons;

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
    ComboBoxPyVEnv: TComboBox;
    EditPythonDll: TEdit;
    SpeedButtonSelectDll: TSpeedButton;
    OpenDialogPythonDll: TOpenDialog;
    PythonDelphiVar1: TPythonDelphiVar;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxPyVEnvChange(Sender: TObject);
    procedure SpeedButtonSelectDllClick(Sender: TObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
  private
    { Private declarations }
    _PythonEngine: TPythonEngine;  //Writhing
  public
    { Public declarations }

    property PythonEngine: TPythonEngine read _PythonEngine; //TWrithingPythonEngine;
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
  var WrithingPythonEngine := PythonEngine as TWrithingPythonEngine;
  WrithingPythonEngine.Attach();
  PythonDelphiVar1.Engine := PythonEngine;
  //var PyVar := TPythonDelphiVar.Create(nil);

  //if VarIsNull(PythonDelphiVar1.Value) then //!! "NULL" instead "Unassigned"

  (PythonDelphiVar1 as TPersistentPythonDelphiVar).RestoreValue;
  PythonEngine.ExecString(sePythonCode.Text);  //UTF8Encode()
  (PythonDelphiVar1 as TPersistentPythonDelphiVar).StoreValue;
end;

procedure TForm1.ComboBoxPyVEnvChange(Sender: TObject);
begin
  PythonEngine.DllPath := ComboBoxPyVEnv.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  _PythonEngine := TWrithingPythonEngine.Create(Self)
      .Configure('C:\ProgramData\Anaconda3', '3.8');

  PythonEngine.IO :=  PythonGUIInputOutput;

  PythonDelphiVar1 := TPersistentPythonDelphiVar.Create(Self);
  PythonDelphiVar1.Engine := PythonEngine;
  PythonDelphiVar1.VarName := 'shared_variable';
  (PythonDelphiVar1 as TPersistentPythonDelphiVar).Init(0);
end;

procedure TForm1.PythonDelphiVar1Change(Sender: TObject);
begin
  ShowMessage(PythonDelphiVar1.ValueAsString);
end;

procedure TForm1.SpeedButtonSelectDllClick(Sender: TObject);
begin
  OpenDialogPythonDll.InitialDir := ComboBoxPyVEnv.Text;
  if OpenDialogPythonDll.Execute() then
  begin
    (PythonEngine as TWrithingPythonEngine).Configure(OpenDialogPythonDll.FileName);
    EditPythonDll.Text := 'Python ' + PythonEngine.RegVersion;
  end;
end;

end.
