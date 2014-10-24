unit TestCaseExtension;

interface

///	<summary>
///	  Uncomment this line to integrate with DSharp unit tests
///	</summary>
{$DEFINE DSharp}

{$IFDEF DSharp}
uses TestFramework, DSharp.Testing.DUnit, TestExtensions, TypInfo, Classes;
{$ELSE}
uses TestFramework, TestExtensions, TypInfo, Classes;
{$ENDIF}

type
{$IFDEF DSharp}
  TestCaseAttribute = DSharp.Testing.DUnit.TestCaseAttribute;
  ExpectedExceptionAttribute = DSharp.Testing.DUnit.ExpectedExceptionAttribute;
{$ENDIF}

  CategoryAttribute = class(TCustomAttribute)
  private
    FCategory : string;
  public
    constructor Create(const ACategory : string);
    property Category : string read FCategory;
  end;

  {$M+}
  TTestCaseExtension = class(TTestCase, ITest)
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
    procedure CheckContains(subtext, actual: string; msg: string = ''); virtual;
  end;
  {$M-}

implementation

uses SysUtils, Math, Types, StrUtils, System.Rtti, System.Generics.Collections;

const
  DELIMITER = ',';
  EXCLUDE = '-';

type
  TCategoryFilter = class
  private
    Category: string;
    Exclude: Boolean;
  end;

  TCategories = class
  private
    FActiveCategories: TDictionary<String,TCategoryFilter>;

    function IsAllFiltersExclusions: Boolean;
  public
    procedure AddCategory(const ACategory: string);

    function IsEmpty: Boolean;
    function Match(const ACategories: TStringList): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  TTestCaseEntries = class
  private
    FTestCases: TStringList;
    FActiveCategories: TCategories;

    procedure LoadTestCasesEntry;

    function FindCmdLineSwitchValue(const Switch: string; var Value: String): Boolean;
  public
    function IsEmpty: Boolean;

    function matchClass(AClass: TTestCaseClass): Boolean;
    function matchCategory(AClass: TTestCaseClass): Boolean;

    function CanRegister(AClass: TTestCaseClass): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

var
  _TestCasesEntries: TTestCaseEntries = nil;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (NOT DEFINED(CLR)) AND (CompilerVersion >= 23.0) }
    {$DEFINE HAS_BUILTIN_RETURNADDRESS} // Requires ReturnAddress intrinsic function(Delphi XE2)
  {$IFEND}
{$ENDIF}

{$IFNDEF HAS_BUILTIN_RETURNADDRESS}
type
  TReturnAddressFunc = function : Pointer;

var
  ReturnAddress: TReturnAddressFunc = CallerAddr;
{$ENDIF}

{ TestCaseExtended }

procedure TTestCaseExtension.CheckContains(subtext, actual, msg: string);
begin
  FCheckCalled := True;
  if not AnsiContainsText(actual, subtext) then
  begin
    Fail(Format('%s expect the string <%s> contains the substring <%s>',[msg, actual, subtext]), ReturnAddress);
  end;
end;

procedure TTestCaseExtension.CheckEqualsDate(expected, actual: TDateTime;msg: string);
begin
  if (expected <> actual) then
  begin
    FailNotEquals(DateTimeToStr(expected), DateTimeToStr(actual), msg, ReturnAddress);
  end;
end;

procedure TTestCaseExtension.CheckEqualsDouble(expected, actual,delta: Double; msg: string; ErrorAddrs: Pointer);
begin
  if not SameValue(expected, actual, delta) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := ReturnAddress;
    end;

    FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckEqualsDouble(expected, actual: Double;msg: string; ErrorAddrs: Pointer);
begin
  if (ErrorAddrs = nil) then
  begin
    ErrorAddrs := ReturnAddress;
  end;

  CheckEqualsDouble(expected, actual, 0.001, msg, ErrorAddrs);
end;

procedure TTestCaseExtension.CheckEqualsEnum(expected, actual: Variant;typeinfo: PTypeInfo; msg: string; ErrorAddrs: Pointer);
begin
  if (expected <> actual) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := ReturnAddress;
    end;

    FailNotEquals(GetEnumName(typeinfo, expected), GetEnumName(typeinfo, actual), Format('[%s] %s', [typeinfo^.Name, msg]), ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckEqualsText(expected, actual,msg: string);
begin
  FCheckCalled := True;
  if AnsiUpperCase(expected) <> AnsiUpperCase(actual) then
  begin
    FailNotEquals(expected, actual, msg, ReturnAddress);
  end;
end;

procedure TTestCaseExtension.CheckGreaterThanExpected(expected,actual: Double; msg: string; ErrorAddrs: Pointer);
begin
  if CompareValue(actual, expected, 0.001) <> GreaterThanValue then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := ReturnAddress;
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
      ErrorAddrs := ReturnAddress;
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
      ErrorAddrs := ReturnAddress;
    end;

    Fail(Format('%s Expected empty string but was <%s>.',[msg, actual]), ErrorAddrs);
  end;
end;

procedure TTestCaseExtension.CheckNotIsEmptyString(actual, msg: string;ErrorAddrs: Pointer);
begin
  if (actual = EmptyStr) then
  begin
    if (ErrorAddrs = nil) then
    begin
      ErrorAddrs := ReturnAddress;
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

function TTestCaseEntries.CanRegister(AClass: TTestCaseClass): Boolean;
begin
  Result := IsEmpty or matchClass(AClass) or matchCategory(AClass);
end;

constructor TTestCaseEntries.Create;
begin
  FTestCases := TStringList.Create;
  FActiveCategories := TCategories.Create;

  LoadTestCasesEntry;
end;

destructor TTestCaseEntries.Destroy;
begin
  FTestCases.Free;
  FActiveCategories.Free;
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
    if CharInSet(S[1], SwitchChars) then
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
  Result := (FTestCases.Count = 0) and (FActiveCategories.IsEmpty);
end;

procedure TTestCaseEntries.LoadTestCasesEntry;
var
  values: String;
  vList: TStringList;
  vItem: string;
begin
  if Self.FindCmdLineSwitchValue('TestCases', values) then
  begin
    FTestCases.Delimiter := DELIMITER;
    FTestCases.DelimitedText := values;
  end;

  if Self.FindCmdLineSwitchValue('Category', values) then
  begin
    vList := TStringList.Create;
    try
      vList.Delimiter := DELIMITER;
      vList.DelimitedText := values;

      for vItem in vList do
      begin
        FActiveCategories.AddCategory(vItem);
      end;
    finally
      vList.Free;
    end;
  end;
end;

function TTestCaseEntries.matchCategory(AClass: TTestCaseClass): Boolean;
var
  vRttiContext: TRttiContext;
  vAttribute: TCustomAttribute;
  vCategories: TStringList;
  vParent: TClass;
begin
  Result := FActiveCategories.IsAllFiltersExclusions;

  for vAttribute in vRttiContext.GetType(AClass).GetAttributes do
  begin
    if (vAttribute is CategoryAttribute) then
    begin
      vCategories := TStringList.Create;
      try
        vCategories.Delimiter := DELIMITER;
        vCategories.DelimitedText := CategoryAttribute(vAttribute).Category;

        Exit(FActiveCategories.Match(vCategories))
      finally
        vCategories.Free;
      end;
    end;
  end;

  vParent := AClass.ClassParent;
  while (not Result) and (vParent <> TTestCaseExtension) and (vParent.InheritsFrom(TTestCase)) do
  begin
    Result := matchCategory(TTestCaseClass(vParent));

    vParent := vParent.ClassParent;
  end;
end;

function TTestCaseEntries.matchClass(AClass: TTestCaseClass): Boolean;
begin
  Result := (FTestCases.IndexOf(AClass.ClassName) >= 0);
end;

{ CategoryAttribute }

constructor CategoryAttribute.Create(const ACategory: string);
begin
  FCategory := ACategory;
end;

{ TCategories }

procedure TCategories.AddCategory(const ACategory: string);
var
  vCategoryFilter: TCategoryFilter;
begin
  vCategoryFilter := TCategoryFilter.Create;

  if StartsStr(EXCLUDE, ACategory) then
  begin
    vCategoryFilter.Category := Copy(ACategory,2, MaxInt);
    vCategoryFilter.Exclude  := True;
  end
  else
  begin
    vCategoryFilter.Category := ACategory;
  end;

  FActiveCategories.Add(vCategoryFilter.Category, vCategoryFilter);
end;

constructor TCategories.Create;
begin
  FActiveCategories := TObjectDictionary<String, TCategoryFilter>.Create([doOwnsValues]);
end;

destructor TCategories.Destroy;
begin
  FActiveCategories.Free;
  inherited;
end;

function TCategories.IsAllFiltersExclusions: Boolean;
var
  vFilter: TCategoryFilter;
begin
  Result := not IsEmpty;
  for vFilter in FActiveCategories.Values do
  begin
    Result := Result and vFilter.Exclude;
  end;
end;

function TCategories.IsEmpty: Boolean;
begin
  Result := FActiveCategories.Count = 0;
end;

function TCategories.Match(const ACategories: TStringList): Boolean;
var
  vFilter: TCategoryFilter;
begin
  Result := True;

  for vFilter in FActiveCategories.Values do
  begin
    if vFilter.Exclude then
    begin
      Result := Result and not (ACategories.IndexOf(vFilter.Category) >= 0);
    end
    else
    begin
      Result := Result and (ACategories.IndexOf(vFilter.Category) >= 0);
    end;
  end;
end;

initialization
  _TestCasesEntries := TTestCaseEntries.Create; 

finalization
  FreeAndNil(_TestCasesEntries);
  
end.
