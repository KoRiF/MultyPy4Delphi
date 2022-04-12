unit UnitGridDataPy;

interface
uses Grids, PythonEngine, WrapDelphi, SysUtils, np.Utils;

type
  ConverterToArray = TFunc<TArray<Double>>;
  ConverterTo2DArray = TFunc<TArray2D<Double>>;

  TStringGridArrayContainer = Class
    private
    const PYIDENTIFIER_TEMPLATE = 'tarray_%s';
    var
      _Grid: TStringGrid;
      _NumericArray: TArray<Double>;
      _PyContainerName: String;
      _CPython_id: Integer;  
    private
      function GetLineBreakSize(): Integer;
      property Breaksize: Integer read GetLineBreakSize;
      function GetValueByIx(i, j: Integer): Double;
      procedure SetValueByIx(i, j: Integer; value: Double);
      function GetPyDataPointer(): THandle;
    public
      constructor Create(Grid: TStringGrid);
      property numeric_array: TArray<Double> read _NumericArray;
      property Value[i, j: Integer]: Double read GetValueByIx write SetValueByIx; default;
      property cpython_id: Integer write _CPython_id;
    public
      property PyDataPointer: THandle read GetPyDataPointer;
      function WrapToPyObject(PyWrapper: TPyDelphiWrapper; PyIdentifier: string): TStringGridArrayContainer;
      procedure PyExecDataAsNDArray(Module: TPythonModule; PyIdentifier: string);
      function PyExecCreateNDArrayData(Module: TPythonModule; PyIdentifier: string): TStringGridArrayContainer;
  End;

  TStringGridPyHelper = Class Helper for TStringGrid
    procedure WrapAsObjectField(PyWrapper: TPyDelphiWrapper; PyIdentifier: string);
    function PreservePyMemory(PyWrapper: TPyDelphiWrapper; PyIdentifier: string): TStringGridArrayContainer;
    procedure ShareAsMemoryNumericData(PyWrapper: TPyDelphiWrapper;PyIdentifier: string);

    procedure InjectToPyList(PyIdentifier: string);
    procedure InjectToNDArray(PyIdentifier: string);
    procedure FillToCPyList(PyIdentifier: string);
    procedure FillToCPyNDArray(PyIdentifier: string);

    procedure Wrap1DToArray(Wrapper: TPyDelphiWrapper; PyIdentifier: string; dim: Integer = 0);
    //procedure Wrap2DToFlattenedArray(Wrapper: TPyDelphiWrapper; PyIdentifier: string);
    procedure WrapToNDArray(PyIdentifier: string; PyModule: TPythonModule);

  private
    procedure Wrap2DToFlattenedArray(Wrapper: TPyDelphiWrapper;
      PyIdentifier: string);

    function CoversionToArray(dim: Integer): ConverterToArray;
    function CoversionTo2DArray(): ConverterTo2DArray;
    function To2DArray: TArray2D<Double>;
  private
    function GetDataRows(): Integer;
    procedure SetDataRows(M: Integer);
    function GetDataColumns(): Integer;
    procedure SetDataColumns(N: Integer);
  public
    property NumDataRows: Integer read GetDataRows write SetDataRows;
    property NumDataColumns: Integer read GetDataColumns write SetDataColumns;
  End;



  procedure PyExecInjectGridTo1DPyList(Grid: TStringGrid; PyIdentifier: String; dim: integer = 0);
  procedure PyExecInjectGridTo2DPyList(Grid: TStringGrid; PyIdentifier: String);

  procedure PyExecFillGridTo1DCPyList(Grid: TStringGrid; PyIdentifier: String; dim: integer = 0);
  procedure PyExecFillGridTo2DCPyList(Grid: TStringGrid; PyIdentifier: String);

  function GridTo1DArray(Grid: TStringGrid; dim: integer = 0): TArray<Double>;
  function GridTo2DArray(Grid: TStringGrid): TArray2D<Double>;
  function GridTo2DFlattenedArray(Grid: TStringGrid): TArray<Double>;
  procedure GridTo2DFlattenedMemory(Grid: TStringGrid; pData: Pointer);

  function AllocateGridNumericMemory(Grid: TStringGrid): Pointer;

  var PythonModuleName: String;
implementation

uses Rtti, np.Base, np.Models;

function CurrentPyModule(): PPyObject;
begin
  var PyEngine := PythonEngine.GetPythonEngine();

  if (PythonModuleName = '') then
    RESULT := PyEngine.GetMainModule()
  else
    RESULT := PyEngine.ModuleByName(PythonModuleName);
end;

function AllocateGridNumericMemory(Grid: TStringGrid): Pointer;
begin
  RESULT := GetMemory(Grid.NumDataRows * Grid.NumDataColumns * sizeof(Double));
end;

procedure PyExecInjectGridTo1DPyList(Grid: TStringGrid; PyIdentifier: String; dim: Integer);
var PyEngine: TPythonEngine;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCol := Grid.FixedCols;

  var ListDataLength := 0;

  case dim of
  0:
    ListDataLength := Ndatacolumns;
  1:
    ListDataLength := Ndatarows;
  end;

  PyEngine := PythonEngine.GetPythonEngine();
  with PyEngine do
  begin

    ExecString(String.Format('%s = [None]* %d',[PyIdentifier, ListDataLength]));
    for var  ix := 0 to ListDataLength - 1 do
    begin
      var item: String;
      if dim = 1 then
        item := Grid.Cells[offsetCol, offsetRow + ix]
      else
        item := Grid.Cells[offsetCol + ix, offsetRow];
      ExecString(String.Format('%s[%d] = %s',[PyIdentifier, ix, item]));
    end;

  end;
end;


procedure PyExecInjectGridTo2DPyList(Grid: TStringGrid; PyIdentifier: String);
var PyEngine: TPythonEngine;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCols := Grid.FixedCols;

  PyEngine := PythonEngine.GetPythonEngine();
  with PyEngine do
  begin
    ExecString(String.Format('%s = [None]* %d',[PyIdentifier, Ndatarows]));
    for var  rowIx := 0 to Ndatarows - 1 do
    begin
      ExecString(String.Format('%s_ = [None]* %d',[PyIdentifier, Ndatacolumns]));
      for var colIx := 0 to Ndatacolumns - 1 do
        ExecString(String.Format('%s_[%d] = %s',[PyIdentifier, colIx, Grid.Cells[colIx + offsetCols, rowIx + offsetRow]]));

      ExecString(String.Format('%s[%d] = %s_',[PyIdentifier, rowIx, PyIdentifier]));
    end;
  end;
end;

procedure PyExecFillGridTo1DCPyList(Grid: TStringGrid; PyIdentifier: String; dim: Integer);
var PyEngine: TPythonEngine;

begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCol := Grid.FixedCols;

  var ListDataLength := 0;

  case dim of
  0:
    ListDataLength := Ndatacolumns;
  1:
    ListDataLength := Ndatarows;
  end;

  PyEngine := PythonEngine.GetPythonEngine();
  with PyEngine do
  begin
    var pyListObj := PyList_New(ListDataLength); //ExecString(String.Format('%s = [None]* %d',[PyListIdentifier, ListDataLength]));
    for var  ix := 0 to ListDataLength - 1 do
    begin
      var item: String;
      if dim = 1 then
        item := Grid.Cells[offsetCol, offsetRow + ix]
      else
        item := Grid.Cells[offsetCol + ix, offsetRow];
      var pyItem := PyEngine.PyFloat_FromString(@item[1]);
      PyList_SetItem(pyListObj, ix, pyItem); //ExecString(String.Format('%s[%d] = %s',[PyListIdentifier, ix, item]));
    end;

    var pyModule := CurrentPyModule();
    var pyListId: AnsiString := PyIdentifier;
    PyObject_SetAttrString(pyModule, PAnsiChar(pyListId), pyListObj);
  end;
end;


procedure PyExecFillGridTo2DCPyList(Grid: TStringGrid; PyIdentifier: string);
var PyEngine: TPythonEngine;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCols := Grid.FixedCols;

  PyEngine := PythonEngine.GetPythonEngine();
  with PyEngine do
  begin
    var pyTableObj := PyList_New(Ndatarows);//ExecString(String.Format('%s = [None]* %d',[PyListIdentifier, Ndatarows]));
    for var  rowIx := 0 to Ndatarows - 1 do
    begin
      var pyRowObj := PyList_New(Ndatacolumns); //ExecString(String.Format('%s_ = [None]* %d',[PyListIdentifier, Ndatacolumns]));
      for var colIx := 0 to Ndatacolumns - 1 do
      begin
        var item : AnsiString := Grid.Cells[colIx + offsetCols, rowIx + offsetRow];
        if item = '' then
          continue;
        var pyItem := PyEngine.PyUnicode_FromString(@item[1]);
        PyList_SetItem(pyRowObj, colIx, pyItem);
         //ExecString(String.Format('%s_[%d] = %s',[PyListIdentifier, colIx, Grid.Cells[colIx + offsetCols, rowIx + offsetRow]]));
      end;
      PyList_SetItem(pyTableObj, rowIx, pyRowObj); //ExecString(String.Format('%s[%d] = %s_',[PyListIdentifier, rowIx, PyListIdentifier]));

      var pyModule := CurrentPyModule();
      var pyListId: AnsiString := PyIdentifier;
      PyObject_SetAttrString(pyModule, PAnsiChar(pyListId), pyTableObj);
    end;
  end;
end;

function GridTo1DArray(Grid: TStringGrid; dim: integer = 0): TArray<Double>;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCol := Grid.FixedCols;

  var ListDataLength := 0;

  case dim of
  0:
    ListDataLength := Ndatacolumns;
  1:
    ListDataLength := Ndatarows;
  end;
  SetLength(RESULT, ListDataLength);

  for var  ix := 0 to ListDataLength - 1 do
  begin
    var item: String;
    if dim = 1 then
      item := Grid.Cells[offsetCol, offsetRow + ix]
    else
      item := Grid.Cells[offsetCol + ix, offsetRow];
    RESULT[ix] := StrToFloat(item);   //Pascal.Extended
  end;
end;

function GridTo2DArray(Grid: TStringGrid): TArray2D<Double>;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCols := Grid.FixedCols;

  SetLength(RESULT, Ndatarows);

  for var  rowIx := 0 to Ndatarows - 1 do
  begin
    var RowArray: TArray<Double>;
    SetLength(RowArray,  Ndatacolumns);
    for var colIx := 0 to Ndatacolumns - 1 do
    begin
      var item := Grid.Cells[colIx + offsetCols, rowIx + offsetRow];
      if item = '' then
        item := '0';
      RowArray[colIx] := StrToFloat(item);
    end;
    RESULT[rowIx] := RowArray;
  end;
end;

function GridTo2DFlattenedArray(Grid: TStringGrid): TArray<Double>;
begin
  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCols := Grid.FixedCols;

  SetLength(RESULT, Ndatarows * Ndatacolumns);

  for var  rowIx := 0 to Ndatarows - 1 do
  begin
    for var colIx := 0 to Ndatacolumns - 1 do
    begin
      var item := Grid.Cells[colIx + offsetCols, rowIx + offsetRow];
      if item = '' then
        continue;
      RESULT[rowIx * Ndatacolumns + colIx] := StrToFloat(item);
    end;
  end;
end;

procedure GridTo2DFlattenedMemory(Grid: TStringGrid; pData: Pointer);
var pValue0: PDouble;
begin
{$POINTERMATH ON}
  pValue0 := PDouble(pData);

  var Ndatarows := Grid.RowCount - Grid.FixedRows;
  var offsetRow := Grid.FixedRows;

  var Ndatacolumns := Grid.ColCount - Grid.FixedCols;
  var offsetCols := Grid.FixedCols;

  //SetLength(RESULT, Ndatarows * Ndatacolumns);

  for var  rowIx := 0 to Ndatarows - 1 do
  begin
    for var colIx := 0 to Ndatacolumns - 1 do
    begin
      var item := Grid.Cells[colIx + offsetCols, rowIx + offsetRow];
      if item = '' then
        continue;
      var offset := (rowIx * Ndatacolumns + colIx);// * SizeOf(Double)

      (pValue0 + offset)^ := Double(StrToFloat(item));
      //Grid.Cells[colIx + offsetCols, rowIx + offsetRow] := FloatToStr((pValue0 + offset)^);
    end;
  end;
{$POINTERMATH OFF}
end;

procedure PyWrapGridAsArray(Grid: TStringGrid; PyModule: TPythonModule; PyIdentifier: String;  CreateArrayFromGrid: ConverterToArray);
//var NumericArray: TRecNumericData;
begin
  //NumericArray.numeric_data := CreateArrayFromGrid();
  var GridArray := CreateArrayFromGrid();   //TArray<Double>
  var PyNDArray := TNumPy.ConvertArrayToNDarray<Double>(GridArray);
  var PasNDArray := TNDArray<Double>.Create(PyNDArray);
  var pPyArrayData := PyNDArray.data;

  //var pyWrappedArray := PyWrapper.WrapRecord(@NumericArray, TRttiContext.Create.GetType(TypeInfo(TRecNumericData)) as TRttiStructuredType);
  //var pyWrappedArray := PyWrapper.Wrap(PasNDArray);

  PyModule.SetVar(AnsiString(PyIdentifier), PasNDArray.Handle);
  //? PyWrapper.Engine.Py_DecRef(PasNDArray.Handle);

  PyModule.SetVar(AnsiString(PyIdentifier + '_data'), pPyArrayData);
  PyModule.Engine.Py_DecRef(pPyArrayData);
end;

//TODO: remove
procedure WrapGridAsNumPyNDArray(Grid: TStringGrid; Wrapper: TPyDelphiWrapper; PyIdentifier: String; GridToArray: ConverterToArray; npreshape: boolean);
begin
  var PyContainedRecIdentifier := '_delphi_' + PyIdentifier + '_rec';
  //PyWrapGridAsArray(Grid, Wrapper, PyContainedRecIdentifier, GridToArray);

  var PyEngine := Wrapper.Engine;
  PyEngine.ExecString('import numpy as np');
  var pycmd := String.Format('%s = np.frombuffer(%s.numeric_data)', [PyIdentifier, PyContainedRecIdentifier]);
  PyEngine.ExecString(pycmd);

  if npreshape then
  begin
    var Mrows := Grid.RowCount - Grid.FixedRows;
    var Ncols := Grid.ColCount - Grid.FixedCols;
    pycmd := String.Format('%s = np.reshape(%d,%d)', [PyIdentifier, Mrows, Ncols]);
    PyEngine.ExecString(pycmd);
  end;
end;




procedure WrapGridAsNumPyArray(PyModule: TPythonModule; PyIdentifier: String; GridToArray: ConverterToArray);
begin
  var GridArray := GridToArray();   //TArray<Double>
  var PyNDArray := TNumPy.ConvertArrayToNDarray<Double>(GridArray);
  var PasNDArray := TNDArray<Double>.Create(PyNDArray);

  PyModule.SetVar(AnsiString(PyIdentifier), PasNDArray.Handle);

  PyModule.Engine.Py_DecRef(PasNDArray.Handle);
end;

procedure WrapGridAsNumPy2NDArray(PyModule: TPythonModule; PyIdentifier: String; GridToArray: ConverterTo2DArray);
begin
  var GridArray := GridToArray();   //TArray2D<Double>
  var PyNDArray := TNumPy.ConvertArrayToNDarray<Double>(GridArray);
  var PasNDArray := TNDArray<Double>.Create(PyNDArray);

  PyModule.SetVar(AnsiString(PyIdentifier), PasNDArray.Handle);

  PyModule.Engine.Py_DecRef(PasNDArray.Handle);
end;



{ TStringGridPyHelper }

function TStringGridPyHelper.CoversionTo2DArray: ConverterTo2DArray;
begin
CoversionTo2DArray := function: TArray2D<Double>
  begin
    RESULT := GridTo2DArray(Self);
  end;
end;

function TStringGridPyHelper.CoversionToArray(dim: Integer): ConverterToArray;
begin
CoversionToArray := function: TArray<Double>
  begin
    RESULT :=  GridTo1DArray(Self, dim);  //dim's closure
  end;
end;

procedure TStringGridPyHelper.FillToCPyList(PyIdentifier: string);
begin
  if (Self.RowCount > 1) and (Self.ColCount > 1) then
    PyExecFillGridTo2DCPyList(Self, PyIdentifier)
  else if (Self.RowCount = 1) and (Self.ColCount > 1) then
    PyExecFillGridTo1DCPyList(Self, PyIdentifier, 0)
  else if (Self.RowCount > 1) and (Self.ColCount = 1) then
    PyExecFillGridTo1DCPyList(Self, PyIdentifier, 1)
  else
    PyExecFillGridTo2DCPyList(Self, PyIdentifier); // [ [Cell[0,0] ]]
end;

procedure TStringGridPyHelper.FillToCPyNDArray(PyIdentifier: String);
begin
  var PyEngine := PythonEngine.GetPythonEngine();
  PyEngine.ExecString('import numpy as np');

  Self.FillToCPyList(PyIdentifier);
  //PyEngine.ExecString(String.Format('import %s', [PythonModuleName]));

  var injScript := String.Format('%s = np.asarray(%s)', [PyIdentifier, PyIdentifier]);
  PyEngine.ExecString(injScript);
end;

function TStringGridPyHelper.GetDataColumns: Integer;
begin
  RESULT := Self.ColCount - Self.FixedCols;
end;

function TStringGridPyHelper.GetDataRows: Integer;
begin
  RESULT := Self.RowCount - Self.FixedCols;
end;

procedure TStringGridPyHelper.InjectToNDArray(PyIdentifier: string);
begin
  var PyEngine := PythonEngine.GetPythonEngine();

  PyEngine.ExecString('import numpy as np');

  Self.InjectToPyList(PyIdentifier);
  var injScript := String.Format('%s = np.asarray(%s)', [PyIdentifier, PyIdentifier]);
  PyEngine.ExecString(injScript);
end;

procedure TStringGridPyHelper.InjectToPyList(PyIdentifier: string);
begin
  if (Self.RowCount > 1) and (Self.ColCount > 1) then
    PyExecInjectGridTo2DPyList(Self, PyIdentifier)
  else if (Self.RowCount = 1) and (Self.ColCount > 1) then
    PyExecInjectGridTo1DPyList(Self, PyIdentifier, 0)
  else if (Self.RowCount > 1) and (Self.ColCount = 1) then
    PyExecInjectGridTo1DPyList(Self, PyIdentifier, 1)
  else
    PyExecInjectGridTo2DPyList(Self, PyIdentifier); // [ [Cell[0,0] ]]
end;

function TStringGridPyHelper.PreservePyMemory(PyWrapper: TPyDelphiWrapper;
  PyIdentifier: string): TStringGridArrayContainer;
begin
  RESULT := TStringGridArrayContainer.Create(Self)
    .WrapToPyObject(PyWrapper, PyIdentifier)
    .PyExecCreateNDArrayData(PyWrapper.Module, PyIdentifier);
end;

procedure TStringGridPyHelper.SetDataColumns(N: Integer);
begin
  Self.ColCount := Self.FixedCols + N;
end;

procedure TStringGridPyHelper.SetDataRows(M: Integer);
begin
  Self.RowCount := Self.FixedRows + M;
end;

procedure TStringGridPyHelper.ShareAsMemoryNumericData(PyWrapper: TPyDelphiWrapper; PyIdentifier: string);
var pMemBuf: Pointer;
begin
  var Container := PreservePyMemory(PyWrapper, PyIdentifier);
  pMemBuf := Ptr(Container.PyDataPointer);

  var PyEngine := PythonEngine.GetPythonEngine();
  var p0 := PyEngine.DllHandle;

  pMemBuf := Ptr(p0 + THandle(pMemBuf));
  GridTo2DFlattenedMemory(Self, pMemBuf);
end;

function TStringGridPyHelper.To2DArray: TArray2D<Double>;
begin
  RESULT := GridTo2DArray(Self);
end;

procedure TStringGridPyHelper.Wrap1DToArray(Wrapper: TPyDelphiWrapper;
  PyIdentifier: string; dim: Integer);
begin
  var GridToArray: ConverterToArray := function: TArray<Double>
  begin
    RESULT := GridTo1DArray(Self, dim);
  end;

  WrapGridAsNumPyNDArray(Self, Wrapper, PyIdentifier, GridToArray, False);
end;

procedure TStringGridPyHelper.Wrap2DToFlattenedArray(Wrapper: TPyDelphiWrapper;
  PyIdentifier: string);
begin
  var GridToArray: ConverterToArray := function: TArray<Double>
  begin
    RESULT := GridTo2DFlattenedArray(Self);
  end;

  WrapGridAsNumPyNDArray(Self, Wrapper, PyIdentifier, GridToArray, True);
end;

procedure TStringGridPyHelper.WrapAsObjectField(PyWrapper: TPyDelphiWrapper;
  PyIdentifier: string);
begin
  TStringGridArrayContainer.Create(Self)
    .WrapToPyObject(PyWrapper, PyIdentifier)
    .PyExecDataAsNDArray(PyWrapper.Module, PyIdentifier);
end;

procedure TStringGridPyHelper.WrapToNDArray(PyIdentifier: string;
  PyModule: TPythonModule);
begin
  if (Self.RowCount > 1) and (Self.ColCount > 1) then
    WrapGridAsNumPy2NDArray(PyModule,  PyIdentifier, Self.CoversionTo2DArray())
  else if (Self.RowCount = 1) and (Self.ColCount > 1) then
    WrapGridAsNumPyArray(PyModule, PyIdentifier, Self.CoversionToArray(0))
  else if (Self.RowCount > 1) and (Self.ColCount = 1) then
    WrapGridAsNumPyArray(PyModule, PyIdentifier, Self.CoversionToArray(1))
  else
    WrapGridAsNumPy2NDArray(PyModule,  PyIdentifier, Self.CoversionTo2DArray()); // [ [Cell[0,0] ]]
end;

//procedure TStringGridPyHelper.WrapToNDArray(Wrapper: TPyDelphiWrapper;
//  PyIdentifier: string);
//begin
//  if (Self.RowCount > 1) and (Self.ColCount > 1) then
//    Wrap2DToFlattenedArray(Wrapper, PyIdentifier)
//  else if (Self.RowCount = 1) and (Self.ColCount > 1) then
//    Wrap1DToArray(Wrapper, PyIdentifier, 0)
//  else if (Self.RowCount > 1) and (Self.ColCount = 1) then
//    Wrap1DToArray(Wrapper, PyIdentifier, 1)
//  else
//    Wrap2DToFlattenedArray(Wrapper, PyIdentifier); // [ [Cell[0,0] ]]
//end;

{ TStringGridArrayContiner }

constructor TStringGridArrayContainer.Create(Grid: TStringGrid);
begin
  _NumericArray := GridTo2DFlattenedArray(Grid);
  _Grid := Grid;
end;

function TStringGridArrayContainer.PyExecCreateNDArrayData(
  Module: TPythonModule; PyIdentifier: string): TStringGridArrayContainer;
begin
  var PyEngine := Module.Engine;
  PyEngine.ExecString('import numpy as np');
  var pycmdimport := String.Format('from %s import %s', [Module.ModuleName, _PyContainerName]);
  PyEngine.ExecString(pycmdimport);

  var pycmdcreate := String.Format('%s = np.zeros((%d, %d))', [PyIdentifier, _Grid.NumDataRows, _Grid.NumDataColumns]);

  PyEngine.ExecString(pycmdcreate);

  var pycmddataid := String.Format('%s.cpython_id = id(%s.data)', [_PyContainerName, PyIdentifier]);
  PyEngine.ExecString(pycmddataid);

  EXIT(Self);
end;

procedure TStringGridArrayContainer.PyExecDataAsNDArray(Module: TPythonModule;
  PyIdentifier: string);
begin

  var PyEngine := Module.Engine;
  PyEngine.ExecString('import numpy as np');
  var pyimport := String.Format('from %s import %s', [Module.ModuleName, _PyContainerName]);
  PyEngine.ExecString(pyimport);
  var pycmdasarray := String.Format('%s = np.asarray(%s.numeric_array)', [PyIdentifier, _PyContainerName]);
  PyEngine.ExecString(pycmdasarray);
  var pycmdreshape := String.Format('%s = %s.reshape(%d, %d)', [PyIdentifier, PyIdentifier, _Grid.NumDataRows, _Grid.NumDataColumns]);
  PyEngine.ExecString(pycmdreshape);
end;

function TStringGridArrayContainer.GetLineBreakSize: Integer;
begin
  RESULT := _Grid.ColCount - _Grid.FixedCols;
end;

function TStringGridArrayContainer.GetPyDataPointer: THandle;
begin
  RESULT := Cardinal(_CPython_id);
end;

function TStringGridArrayContainer.GetValueByIx(i, j: Integer): Double;
begin
  RESULT := _NumericArray[i * Breaksize + j];
end;

procedure TStringGridArrayContainer.SetValueByIx(i, j: Integer; value: Double);
begin
  _NumericArray[i * BreakSize + j] := value;
end;

function TStringGridArrayContainer.WrapToPyObject(PyWrapper: TPyDelphiWrapper;
  PyIdentifier: string): TStringGridArrayContainer;
begin
  //var pyContainerIdentifier: AnsiString := 'tarray_'+PyIdentifier;
  _PyContainerName := String.Format(Self.PYIDENTIFIER_TEMPLATE, [PyIdentifier]);

  var pyObj := PyWrapper.Wrap(Self, soOwned);
  PyWrapper.Module.SetVar(Self._PyContainerName, pyObj);
  PyWrapper.Engine.Py_DECREF(pyObj);

  EXIT(Self);
end;

end.
