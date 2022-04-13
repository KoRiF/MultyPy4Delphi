program MultyPy4Delphi;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  UnitGridDataPy in 'UnitGridDataPy.pas',
  UnitMemShare in 'UnitMemShare.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
