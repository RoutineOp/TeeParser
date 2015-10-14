program Test_Type;

uses
  Forms,
  Unit_Test_Type in 'Unit_Test_Type.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
