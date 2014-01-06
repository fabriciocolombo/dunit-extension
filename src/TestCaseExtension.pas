unit TestCaseExtension;

interface

///	<summary>
///	  Uncomment this line to integrate with DSharp unit tests
///	</summary>
{.$DEFINE DSharp}

{$IFDEF DSharp}
uses TestFramework, DSharp.Testing.DUnit, TestExtensions, TypInfo, Classes;
{$ELSE}
uses TestFramework, TestExtensions, TypInfo, Classes;
{$ENDIF}

type
  TTestCaseExtension = class(TTestCase)
  private
  public
    class procedure RegisterTest(SuitePath: string);overload;
    class procedure RegisterTest();overload;
    class procedure RegisterRepeatedTest(AIterations: Integer; SuitePath: string='');

    procedure CheckEqualsDate(expected, actual: TDateTime; msg: string = ''); virtual;
    procedure CheckEqualsDouble(expected, actual: Double; msg: string = '';ErrorAddrs: Pointer = nil); overload;virtual;
    procedure CheckEqualsDouble(expected, actual, delta: Double; msg: string = '';ErrorAddrs: Pointer = nil); overload;virtual;
    procedure CheckGreaterThanExpected(expected, actual: Double; msg: string = '';ErrorAddrs: Pointer = nil); virtual;
    procedure CheckGreaterThanOrEqualsExpected(expected, actual: Double; msg: string = '';ErrorAddrs: Pointer = nil); virtual;
    procedure CheckIsEmptyString(actual: String; msg: string='';ErrorAddrs: Pointer = nil); virtual;
    procedure CheckNotIsEmptyString(actual: String; msg: string='';ErrorAddrs: Pointer = nil); virtual;
    procedure CheckEqualsEnum(expected, actual: Variant; typeinfo: PTypeInfo; msg: string='';ErrorAddrs: Pointer = nil); virtual;
    procedure CheckEqualsText(expected, actual: string; msg: string = ''); virtual;
  end;

implementation

uses SysUtils, Math, Types;

type
  TTestCaseEntries = class
  private
    FList: TStringList;

    procedure LoadTestCasesEntry;

    function FindCmdLineSwitchValue(const Switch: string; var Value: String): Boolean;
  public
    function IsEmpty: Boolean;

    function matchClass(AClassName: TTestCaseClass): Boolean;

    function CanRegister(AClassName: TTestCaseClass): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

var
  _TestCasesEntries: TTestCaseEntries = nil;

{ TestCaseExtended }

procedure TTestCaseExtension.CheckEqualsDate(expected, actual: TDateTime;msg: string);
begin
  if (expected <> actual) then
  begin
    FailNotEquals(DateTimeToStr(expected), DateTimeToStr(actual), msg, CallerAddr);
  end;
end;

procedure TTestCaseExtension.CheckEqualsDouble(expected, actual,delta: Double; msg: string; ErrorAddrs: Pointer);
begin
  if not SameValue(expected, actual, delta) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckEqualsDouble(expected, actual: Double;msg: string; ErrorAddrs: Pointer);
begin
  if (ErrorAddrs = nil) then
  begin
    ErrorAddrs := CallerAddr;
  end;

  CheckEqualsDouble(expected, actual, 0.001, msg, ErrorAddrs);
end;

procedure TTestCaseExtension.CheckEqualsEnum(expected, actual: Variant;typeinfo: PTypeInfo; msg: string; ErrorAddrs: Pointer);
begin
  if (expected <> actual) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    FailNotEquals(GetEnumName(typeinfo, expected), GetEnumName(typeinfo, actual), msg, ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckEqualsText(expected, actual,msg: string);
begin
  FCheckCalled := True;
  if AnsiUpperCase(expected) <> AnsiUpperCase(actual) then
  begin
    FailNotEquals(expected, actual, msg, CallerAddr);
  end;
end;

procedure TTestCaseExtension.CheckGreaterThanExpected(expected,actual: Double; msg: string; ErrorAddrs: Pointer);
begin
  if CompareValue(actual, expected, 0.001) <> GreaterThanValue then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    Fail(Format('%s actual <%f> must be greater than expected <%f>',[msg, actual, expected]), ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckGreaterThanOrEqualsExpected(expected,actual: Double; msg: string; ErrorAddrs: Pointer);
begin
  if CompareValue(actual, expected, 0.001) = LessThanValue then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    Fail(Format('%s actual <%f> must be greater than or equals to expected <%f>',[msg, actual, expected]), ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckIsEmptyString(actual, msg: string;ErrorAddrs: Pointer);
begin
  if (actual <> EmptyStr) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    Fail(Format('%s actual string <%s> must be empty',[msg, actual]), ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckNotIsEmptyString(actual, msg: string;ErrorAddrs: Pointer);
begin
  if (actual = EmptyStr) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := CallerAddr;
    end;

    Fail(Format('%s actual  string <%s> must not be empty',[msg, actual]), ErrorAddrs);
  end;
end;

class procedure TTestCaseExtension.RegisterRepeatedTest(AIterations: Integer; SuitePath: string);
begin
  if _TestCasesEntries.CanRegister(Self) then
  begin
    TestFramework.RegisterTest(SuitePath, TRepeatedTest.Create(Self.Suite, AIterations, SuitePath));
  end;
end;

class procedure TTestCaseExtension.RegisterTest;
begin
  Self.RegisterTest(EmptyStr);
end;

class procedure TTestCaseExtension.RegisterTest(SuitePath: string);
begin
  if _TestCasesEntries.CanRegister(Self) then
  begin
    TestFramework.RegisterTest(SuitePath, Self.Suite);
  end;
end;

{ TTestCaseEntries }

function TTestCaseEntries.CanRegister(AClassName: TTestCaseClass): Boolean;
begin
  Result := IsEmpty or matchClass(AClassName);
end;

constructor TTestCaseEntries.Create;
begin
  FList := TStringList.Create;

  LoadTestCasesEntry;
end;

destructor TTestCaseEntries.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTestCaseEntries.FindCmdLineSwitchValue(const Switch: string; var Value: String): Boolean;
var
  I: Integer;
  S: string;
begin
  Value := EmptyStr;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (S[1] in SwitchChars) then
    begin
      if (AnsiCompareText(Copy(S, 2, Maxint), Switch) = 0) then
      begin
        Result := True;

        if (I < ParamCount) then
        begin
          Value := ParamStr(I+1); 
        end;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TTestCaseEntries.IsEmpty: Boolean;
begin
  Result := (FList.Count = 0);
end;

procedure TTestCaseEntries.LoadTestCasesEntry;
var
  vClasses: String;
begin
  if Self.FindCmdLineSwitchValue('TestCases', vClasses) then
  begin
    FList.Text := StringReplace(vClasses, ';', sLineBreak, [rfReplaceAll]);
  end;
end;

function TTestCaseEntries.matchClass(AClassName: TTestCaseClass): Boolean;
begin
  Result := (FList.IndexOf(AClassName.ClassName) >= 0);
end;

initialization
  _TestCasesEntries := TTestCaseEntries.Create; 

finalization
  FreeAndNil(_TestCasesEntries);
  
end.
