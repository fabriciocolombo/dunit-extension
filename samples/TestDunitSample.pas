unit TestDunitSample;

interface

uses
  TestCaseExtension;

type
  TTestDunitSample = class(TTestCaseExtension)
  published
    procedure Dummy;
  end;

implementation

{ TTestDunitSample }

procedure TTestDunitSample.Dummy;
begin
  CheckTrue(True);
end;

initialization
  TTestDunitSample.RegisterTest();

end.
