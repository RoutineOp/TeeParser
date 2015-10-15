program TeePascal_Demo;

uses
  Vcl.Forms,
  BasicDemoUnit in 'BasicDemoUnit.pas' {PascalDemo};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPascalDemo, PascalDemo);
  Application.Run;
end.
