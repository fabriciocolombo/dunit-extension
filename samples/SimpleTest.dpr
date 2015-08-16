program SimpleTest;

uses
  TestRunnerUtils,
  Vcl.Forms,
  TestDunitSample in 'TestDunitSample.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  ExitCode := TTestRunnerUtils.RunRegisteredTests;
end.
