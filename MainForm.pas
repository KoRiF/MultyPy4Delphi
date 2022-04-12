unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, PythonGUIInputOutput, SynEditPythonBehaviour,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, Vcl.ExtCtrls,
  Vcl.Buttons, WrapDelphi, Vcl.Samples.Spin, Vcl.Grids, Vcl.Mask;

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
    PythonModule1: TPythonModule;
    PageControl1: TPageControl;
    TabSheetVEnvSysInfo: TTabSheet;
    TabSheetData: TTabSheet;
    TabSheetAppScript: TTabSheet;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    ButtonRunPyAppScript: TButton;
    ComboBoxPyVEnvView: TComboBox;
    EditPythonDllView: TEdit;
    MemoSysInfoOutput: TMemo;
    Panel4: TPanel;
    HeaderControl3: THeaderControl;
    MemoAppScriptOutput: TMemo;
    SynEditPythonScript: TSynEdit;
    StringGridDataTable: TStringGrid;
    SpinEditGridRows: TSpinEdit;
    SpinEditGridColumns: TSpinEdit;
    ButtonPassTableData: TButton;
    RadioGroupPassTableOption: TRadioGroup;
    LabeledEditTablePyIdentifier: TLabeledEdit;
    ButtonShareNumPy: TButton;
    PyDelphiWrapper1: TPyDelphiWrapper;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxPyVEnvChange(Sender: TObject);
    procedure SpeedButtonSelectDllClick(Sender: TObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
    procedure EditPythonDllChange(Sender: TObject);
    procedure ButtonShareNumPyClick(Sender: TObject);
    procedure ButtonRunPyAppScriptClick(Sender: TObject);
    procedure ButtonPassTableDataClick(Sender: TObject);
    procedure SpinEditGridColumnsChange(Sender: TObject);
    procedure SpinEditGridRowsChange(Sender: TObject);
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
  System.Math,
  np.Base, np.Api, np.Models,
  Python.Utils,
  UnitGridDataPy;

var np: TNumPy;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  var WrithingPythonEngine := PythonEngine as TWrithingPythonEngine;
  WrithingPythonEngine.Attach();

  PythonDelphiVar1.Engine := PythonEngine;

  (PythonDelphiVar1 as TPersistentPythonDelphiVar).RestoreValue;
  PythonEngine.ExecString(sePythonCode.Text);  //UTF8Encode()
  (PythonDelphiVar1 as TPersistentPythonDelphiVar).StoreValue;

  PyDelphiWrapper1.Engine := PythonEngine;

end;

procedure TForm1.ButtonPassTableDataClick(Sender: TObject);
const
  ITEM_IX_CODE_INJECTION = 0;
  ITEM_IX_CPYTHON_INSTRUCTIONS = 1;
  ITEM_IX_WRAP_AS_FIELD = 2;
  ITEM_IX_NUMPY4DELPHI_WRAPPING = 3;
  ITEM_IX_MEMORY_BUFFER_PROTOCOL = 4;
begin
  var tableIdentifier := LabeledEditTablePyIdentifier.Text;

  var optionIndex := RadioGroupPassTableOption.ItemIndex;
  case optionIndex of
  ITEM_IX_CODE_INJECTION:
    StringGridDataTable.InjectToNDArray(tableIdentifier);
  ITEM_IX_CPYTHON_INSTRUCTIONS:
    StringGridDataTable.FillToCPyNDArray(tableIdentifier);
  ITEM_IX_WRAP_AS_FIELD:
    StringGridDataTable.WrapAsObjectField(PyDelphiWrapper1, tableIdentifier);
  ITEM_IX_NUMPY4DELPHI_WRAPPING:
    StringGridDataTable.WrapToNDArray(tableIdentifier, PythonModule1);
  ITEM_IX_MEMORY_BUFFER_PROTOCOL:
    StringGridDataTable.ShareAsMemoryNumericData(PyDelphiWrapper1, tableIdentifier);
  else
    MessageDlg('Option is not selected', mtError, [mbCancel], 0);
    Exit;
  end;

  var tablePyIdentifier := LabeledEditTablePyIdentifier.Text;

  if UnitGridDataPy.PythonModuleName <> '' then
  begin
    var printline0 := String.Format('from %s import %s',[PythonModule1.ModuleName, tablePyIdentifier]);
    if pos(printline0, SynEditPythonScript.Text) = 0 then
      SynEditPythonScript.Lines.Add(printline0);
  end;

  var printline1 := String.Format('print(type(%s))', [tablePyIdentifier]);
  var printline2 := String.Format('print(%s)', [tablePyIdentifier]);

  if pos(printline1, SynEditPythonScript.Text) = 0 then
  begin
    SynEditPythonScript.Lines.Add(printline1);
    SynEditPythonScript.Lines.Add(printline2);
  end;
end;

procedure TForm1.ButtonRunPyAppScriptClick(Sender: TObject);
begin
  PythonGUIInputOutput.Output := MemoAppScriptOutput;
  PythonEngine.ExecString(SynEditPythonScript.Text);
  PythonGUIInputOutput.Output := MemoSysInfoOutput;
end;

procedure TForm1.ButtonShareNumPyClick(Sender: TObject);
var
  PyNDArray: TNDarray;
  PasNDArray: TNDArray<Double>;
begin
  try
    Python.Utils.g_MyPyEngine := PythonEngine;
    np := TNumPy.Init(true);

    PyNDArray := TNumPy.ConvertArrayToNDarray<Double>(TArray<Double>.Create(1, 2, 3, 4));
    PasNDArray := TNDArray<Double>.Create(PyNDArray);
    var pPyArrayData := PyNDArray.data;

    PythonModule1.Initialize(); //ForNewInterpreter
    UnitGridDataPy.PythonModuleName := PythonModule1.ModuleName;

    PythonModule1.SetVar('array_data', pPyArrayData);
    PythonModule1.SetVar('delphi_pyarray', PasNDArray.Handle);
    //PythonEngine.PyObject_SetAttrString(    ,PyNDArray.data);

    PythonEngine.Py_DECREF(pPyArrayData);

    PyDelphiWrapper1.Engine := PythonEngine;
    PyDelphiWrapper1.Module := PythonModule1;
    PyDelphiWrapper1.Initialize();
  finally
    TNumPy.FhModuleNumPy := nil;
    Python.Utils.g_MyPyEngine := nil;
  end;
end;

procedure TForm1.ComboBoxPyVEnvChange(Sender: TObject);
begin
  PythonEngine.DllPath := ComboBoxPyVEnv.Text;
  ComboBoxPyVEnvView.Text := ComboBoxPyVEnv.Text;
end;

procedure TForm1.EditPythonDllChange(Sender: TObject);
begin
  EditPythonDllView.Text := EditPythonDll.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  _PythonEngine := TWrithingPythonEngine.Create(Self)
      .Configure('C:\ProgramData\Anaconda3', '3.8');

  PythonEngine.IO :=  PythonGUIInputOutput;

  PythonModule1.Engine := PythonEngine;

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

procedure TForm1.SpinEditGridColumnsChange(Sender: TObject);
begin
  StringGridDataTable.ColCount := SpinEditGridColumns.Value;
end;

procedure TForm1.SpinEditGridRowsChange(Sender: TObject);
begin
  StringGridDataTable.RowCount := SpinEditGridRows.Value;
end;

end.
