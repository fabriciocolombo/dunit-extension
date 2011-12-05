unit TestRunnerUtils;

interface
uses
  TestFramework,
  GUITestRunner,
  CIXMLTestRunner,
  TextTestRunner,
  Forms,
  Classes,
  Windows;

const
  SUREFIRE_OUTPUT_DIRECTORY = '..\target\surefire-reports\';
  SUREFIRE_TESTRESULT_PREFIX = 'TEST_';

type
  TRunMode = (rmGUI, rmText, rmXML);

  TTestRunnerUtils = class
  private
    class function MustRunAsXml: Boolean;
    class function MustRunAsText: Boolean;

    class function RunGUIMode: TTestResult;
    class function RunTextMode: TTestResult;
    class function RunXmlMode: TTestResult;
  public
    class function RunRegisteredTests(): Integer;overload;
    class function RunRegisteredTests(ARunMode: TRunMode): Integer;overload;
  end;

  TFileUtils = class
  public
    class function BuildFileList(const Path, AExtension: string; const Attr: Integer; const List: TStrings; ARecursive: Boolean): Boolean;
    class procedure DeleteAllFiles(const Path, AExtension: string);
  end;

implementation

uses SysUtils, TypInfo;

{ TTestRunnerUtils }

class function TTestRunnerUtils.RunRegisteredTests: Integer;
var
  vRunMode: TRunMode;
begin
  vRunMode := rmGUI;
  if MustRunAsXml then
  begin
    vRunMode := rmXML;
  end
  else if MustRunAsText then
  begin
    vRunMode := rmText;
  end;

  Result := Self.RunRegisteredTests(vRunMode);
end;

class function TTestRunnerUtils.RunGUIMode: TTestResult;
begin
  GuiTestRunner.RunRegisteredTests;

  Result := TTestResult.Create;
end;

class function TTestRunnerUtils.RunRegisteredTests(ARunMode: TRunMode): Integer;
var
  vTestResult: TTestResult;
begin
  case ARunMode of
    rmGUI : vTestResult := RunGUIMode;
    rmText: vTestResult := RunTextMode;
    rmXML : vTestResult := RunXmlMode;
  else
    raise Exception.CreateFmt('RunMode "%s" não suportado.', [GetEnumName(TypeInfo(TRunMode), Ord(ARunMode))]);
  end;

  try
    Result := vTestResult.ErrorCount + vTestResult.FailureCount;
  finally
    vTestResult.Free;
  end;
end;

class function TTestRunnerUtils.RunTextMode: TTestResult;
begin
  Result := TextTestRunner.RunRegisteredTests();
end;

class function TTestRunnerUtils.RunXmlMode: TTestResult;
var
  vXmlOutputPath: String;
begin
  vXmlOutputPath := ExtractFilePath(Application.ExeName) + SUREFIRE_OUTPUT_DIRECTORY;

  TFileUtils.DeleteAllFiles(vXmlOutputPath + '*.xml', '.xml');

  if ForceDirectories(vXmlOutputPath) then
  begin
    vXmlOutputPath := vXmlOutputPath + SUREFIRE_TESTRESULT_PREFIX;

    Result := CIXMLTestRunner.RunRegisteredTests(vXmlOutputPath);
  end else
  begin
    raise Exception.CreateFmt('Falha ao criar outputdirectory "%s". Erro: %s', [vXmlOutputPath, SysErrorMessage(GetLastError)]);
  end;
end;

class function TTestRunnerUtils.MustRunAsXml: Boolean;
begin
  Result := FindCmdLineSwitch('xml', ['-', '/'], True);
end;

class function TTestRunnerUtils.MustRunAsText: Boolean;
begin
  Result := FindCmdLineSwitch('text', ['-', '/'], True);
end;

{ TFileUtils }

class function TFileUtils.BuildFileList(const Path, AExtension: string;const Attr: Integer; const List: TStrings; ARecursive: Boolean): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
  vRealFilePath: String;
begin
  vRealFilePath := ExtractFilePath(Path);
  
  Assert(List <> nil);
  R := FindFirst(Path, Attr, SearchRec);
  Result := R = 0;
  try
    if Result then
    begin
      while R = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
         if ((SearchRec.Attr and Attr) = (SearchRec.Attr and faDirectory)) and ARecursive then
           BuildFileList(vRealFilePath + SearchRec.Name + '\' + ExtractFileName(Path) , AExtension, Attr, List, ARecursive)
         else if (ExtractFileExt(SearchRec.Name) = AExtension) then 
          List.Add(vRealFilePath + SearchRec.Name);
        end;

        R := FindNext(SearchRec);
      end;
      Result := R = ERROR_NO_MORE_FILES;
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

class procedure TFileUtils.DeleteAllFiles(const Path, AExtension: string);
var
  vFileList: TStringList;
  i: Integer;
begin
  vFileList := TStringList.Create;
  try
    Self.BuildFileList(Path, AExtension, faAnyFile, vFileList, False);

    for i := 0 to vFileList.Count-1 do
    begin
      DeleteFile(PChar(vFileList[i]));
    end;
  finally
    vFileList.Free;
  end;

end;

end.
