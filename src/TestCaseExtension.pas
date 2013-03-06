unit TestCaseExtension;

interface

uses TestFramework, TypInfo, Classes;

type
  TTestCaseExtension = class(TTestCase)
  private
  public
    class procedure RegisterTest(SuitePath: string);overload;
    class procedure RegisterTest();overload;

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

  TestCaseExtensionClass = class of TTestCase;

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

    function matchClass(AClassName: TClass): Boolean;

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

class procedure TTestCaseExtension.RegisterTest;
begin
  Self.RegisterTest(EmptyStr);
end;

class procedure TTestCaseExtension.RegisterTest(SuitePath: string);
begin
  if _TestCasesEntries.IsEmpty or _TestCasesEntries.matchClass(Self) then
  begin
    TestFramework.RegisterTest(SuitePath, Self.Suite);
  end;
end;

{ TTestCaseEntries }

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

function TTestCaseEntries.matchClass(AClassName: TClass): Boolean;
begin
  Result := (FList.IndexOf(AClassName.ClassName) >= 0);
end;

initialization
  _TestCasesEntries := TTestCaseEntries.Create; 

finalization
  FreeAndNil(_TestCasesEntries);
  
end.
