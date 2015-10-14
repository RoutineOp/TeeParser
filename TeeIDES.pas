unit TeeIDES;

interface

uses
  Classes, TeePascal;

type
  TIDE=class
  private
    RegCompany : String;

    Constructor CreateStatic;

    function GetParser:TParser;
    function IDEToString:String;
    function IsLazarus:Boolean;
    procedure Prepare(const AVersion:Integer);
    function SourcePath:String;
    procedure VersionError(const AMessage:String);
  public
    CompilerVersion : Integer;

    UserDefines,
    UserPaths,
    UserScopes,

    Defines,
    UnitScopes,
    Paths:TStringList;

    Company,
    Product,
    Version,
    Description,
    Source : String;

    Constructor Create(const ACompilerVersion:Integer);
    Destructor Destroy; override;

    class procedure Detect(const AList:TStrings);
    procedure GetPaths;
    function RootDir:String;

    function ParserFromFile(const AFile:String):TParser;
    function ParserFromText(const AText:String):TParser;
  end;

implementation

uses
  Windows, SysUtils, Registry;

const
  LazarusCompany='Lazarus';

{ TIDE }

Constructor TIDE.CreateStatic;
begin
  inherited Create;
end;

Constructor TIDE.Create(const ACompilerVersion:Integer);
begin
  inherited Create;

  CompilerVersion:=ACompilerVersion;

  Paths:=TStringList.Create;
  UnitScopes:=TStringList.Create;
  Defines:=TStringList.Create;

  UserPaths:=TStringList.Create;
  UserDefines:=TStringList.Create;
  UserScopes:=TStringList.Create;

  Prepare(CompilerVersion);

  Source:=SourcePath;

  if IsLazarus then
  begin
    Defines.Add('FPC');
    Defines.Add('LCL');
    Defines.Add('CPUI386');
  end
  else
  begin
    Defines.Add('MSWINDOWS');
    Defines.Add('CPUX86');
    Defines.Add('VER'+IntToStr(CompilerVersion)); // D7=150

    //  Defines.Add('BCB');

    if (Source='') or (not DirectoryExists(Source)) then
       VersionError('Cannot find IDE root path at registry or program files folder: '+IDEToString);

    if CompilerVersion<170 then
    else
    begin
      Defines.Add('CPU386');
      Defines.Add('WIN32');

      if CompilerVersion>=200 then
      begin
        Defines.Add('UNICODE');
        Defines.Add('CONDITIONALEXPRESSIONS');
      end;

      if CompilerVersion>=280 then
         Defines.Add('ASSEMBLER');
    end;
  end;
end;

function TIDE.GetParser:TParser;
begin
  result:=TParser.Create(Paths);

  if IsLazarus then
  begin
    result.IsFPC:=True;
    result.DefaultExtension:='.pp';
  end
  else
    result.DefaultExtension:='.pas';

  result.Defines.AddStrings(Defines);
  result.Defines.AddStrings(UserDefines);

  result.UnitScopes.AddStrings(UnitScopes);
  result.UnitScopes.AddStrings(UserScopes);
end;

function TIDE.RootDir:String;

  function TryOpenKeyReadOnly(const R:TRegistry; const AKey:String):Boolean;
  begin
    if R.KeyExists(AKey) then
       result:=R.OpenKeyReadOnly(AKey)
    else
       result:=False;
  end;

  function ProgramFilesPath:String;
  var R : TRegistry;
  begin
    result:='';

    R:=TRegistry.Create;
    with R do
    try
      RootKey:=HKEY_LOCAL_MACHINE;

      if TryOpenKeyReadOnly(R,'SOFTWARE\Microsoft\Windows\CurrentVersion') then
      begin
         if ValueExists('ProgramFilesDir') then
            result:=ReadString('ProgramFilesDir');
      end
      else
         raise Exception.Create('Cannot open registry key: LOCAL_MACHINE');
    finally
      Free;
    end;
  end;

  function MakeCompilerDir(const AName,ABDSName,AEnvVersion:String):String;
  begin
    result:=ProgramFilesPath+'\'+AName+'\';

    if CompilerVersion>150 then
       result:=result+ABDSName //'BDS' 'RAD Studio'
    else
       result:=result+'Delphi';

    result:=result+'\'+AEnvVersion+'.0';
  end;

  function GetRootDir(const R:TRegistry):String;
  var tmpKey: String;
  begin
    tmpKey:='\SOFTWARE\'+RegCompany+'\'+Product+'\'+Version+'.0';

    if TryOpenKeyReadOnly(R,tmpKey) and R.ValueExists('RootDir') then
       result:=R.ReadString('RootDir')
    else
       result:='';
  end;

var Main : String;
    R : TRegistry;
begin
  result:='';
  Main:='';

  if IsLazarus then
     result:='c:\lazarus\'
  else
  begin
    R:=TRegistry.Create(KEY_READ);
    try
      R.RootKey:=HKEY_CURRENT_USER;
      Main:=GetRootDir(R);

      if Main='' then
      begin
        R.RootKey:=HKEY_LOCAL_MACHINE;
        Main:=GetRootDir(R);
      end;
    finally
      R.Free;
    end;

    if Main='' then
       Main:=MakeCompilerDir(Company,Product,Version);

    if (Main<>'') and DirectoryExists(Main) then
    begin
      if Copy(Main,Length(Main),1)<>'\' then
         Main:=Main+'\';

      result:=Main;
    end;
  end;
end;

function TIDE.SourcePath:String;
begin
  if IsLazarus then
     result:=RootDir+'lcl'
  else
     result:=RootDir+'Source';
end;

function TIDE.ParserFromFile(const AFile: String): TParser;
begin
  result:=GetParser;
  result.FromPathFile(AFile);
end;

function TIDE.ParserFromText(const AText: String): TParser;
begin
  result:=GetParser;
  result.FromText(AText);
end;

procedure TIDE.Prepare(const AVersion:Integer);
const
  Company_Borland  = 'Borland';
  Company_CodeGear = 'CodeGear';
  Company_Embarcadero = 'Embarcadero';

  Product_Delphi = 'Delphi';
  Product_BDS = 'BDS';
  Product_RAD = 'RAD Studio';
  Product_Studio = 'Studio';

  procedure SetVersion(Num:Integer; const AName:String);
  begin
    Company:=Company_Embarcadero;
    Product:=Product_BDS;
    RegCompany:=Company;
    Version:=IntToStr(Num);

    if Num>=14 then
       Description:=Product_Studio+' '+AName
    else
       Description:=Product_RAD+' '+AName;

    if (Num>=9) and (UnitScopes<>nil) then
    begin
      UnitScopes.Add('System');
      UnitScopes.Add('System.Win');
      UnitScopes.Add('Winapi');
      UnitScopes.Add('Data');
      UnitScopes.Add('Data.Win');
//      UnitScopes.Add('Fmx');
      UnitScopes.Add('Vcl');
      UnitScopes.Add('VclTee');
      UnitScopes.Add('Vcl.Imaging');
      UnitScopes.Add('Vcl.Touch');
      UnitScopes.Add('Vcl.Shell');
      UnitScopes.Add('DataSnap');
      UnitScopes.Add('DataSnap.win');
      UnitScopes.Add('Web');
      UnitScopes.Add('Web.Win');
      UnitScopes.Add('Soap');
      UnitScopes.Add('Soap.Win');
      UnitScopes.Add('Xml');
      UnitScopes.Add('Xml.Win');
      UnitScopes.Add('Bde');
      UnitScopes.Add('Samples');

      if Num>=14 then
         UnitScopes.Add('Rest');

      if Num>=16 then
         UnitScopes.Add('DUnitX');
    end;
  end;

begin
  CompilerVersion:=AVersion;

  case CompilerVersion of
    126: begin Company:=LazarusCompany; Product:='Lazarus'; Description:=Product; RegCompany:=Company; Version:='1.2.6'; end;
    130: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='5'; end;
    140: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='6'; end;
    150: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='7'; end;
    160: VersionError('Delphi 8 .NET not supported');
    170: begin Company:=Company_Borland; Product:=Product_BDS; Description:=Product+' 2005'; RegCompany:=Company; Version:='3'; end;
    180: begin Company:=Company_Borland; Product:=Product_BDS; Description:=Product+' 2006'; RegCompany:=Company; Version:='4'; end;
    190: begin Company:=Company_CodeGear; Product:=Product_BDS; Description:=Product_RAD+' 2007'; RegCompany:=Company_Borland; Version:='5'; end;
    200: begin Company:=Company_CodeGear; Product:=Product_BDS; Description:=Product_RAD+' 2009'; RegCompany:=Company; Version:='6'; end;
    210: begin Company:=Company_Embarcadero; Product:=Product_BDS; Description:=Product_RAD+' 2010'; RegCompany:=Company_CodeGear; Version:='7'; end;

    220: SetVersion(8,'XE');
    230: SetVersion(9,'XE2');
    240: SetVersion(10,'XE3');
    250: SetVersion(11,'XE4');
    260: SetVersion(12,'XE5');
    270: SetVersion(14,'XE6');
    280: SetVersion(15,'XE7');
    290: SetVersion(16,'XE8');
  else
    VersionError('Unknown Compiler Version: '+IntToStr(AVersion));
  end;
end;

function LazarusFound(out APath:String):Boolean;
const
  LazarusPath='c:\lazarus';
begin
  result:=FileExists(LazarusPath+'\lazarus.exe');

  if result then
     APath:=LazarusPath;
end;

class procedure TIDE.Detect(const AList: TStrings);
var i : TIDE;
    t : Integer;
    tmpPath : String;
begin
  AList.Clear;

  i:=TIDE.CreateStatic;
  try
    for t:=29 downto 13 do
        if t<>16 then // D8.NET
        begin
          i.Prepare(t*10);

          if i.RootDir<>'' then
             AList.AddObject(i.IDEToString,TObject(t*10));
        end;

    if LazarusFound(tmpPath) then
       AList.AddObject('Lazarus',TObject(126));

  finally
    i.Free;
  end;
end;

function TIDE.IDEToString:String;
begin
  result:=Company+' '+Description+' v'+Version;
end;

function TIDE.IsLazarus: Boolean;
begin
  result:=RegCompany=LazarusCompany;
end;

procedure TIDE.VersionError(const AMessage:String);
begin
  raise Exception.Create(AMessage);
end;

destructor TIDE.Destroy;
var t : Integer;
begin
  Defines.Free;
  UserDefines.Free;

  UserScopes.Free;
  UnitScopes.Free;

  UserPaths.Free;

  if Paths<>nil then
  begin
    for t:=0 to Paths.Count-1 do
        Paths.Objects[t].Free;

    Paths.Free;
  end;

  inherited;
end;

procedure TIDE.GetPaths;

  procedure GetRecursiveFolders(const APath:String);
  var s : TSearchRec;
  begin
    Paths.Add(APath);

    if FindFirst(APath+'\*.*',faAnyFile,s)=0 then
    begin
      Repeat
        if (s.Attr and faDirectory)=faDirectory then
           if (s.Name<>'.') and (s.Name<>'..') then
           begin
             if (CompilerVersion=190) and SameText(s.Name,'DOTNET') then
             else
             if SameText(s.Name,'CPPRTL') then
             else
                GetRecursiveFolders(APath+'\'+s.Name);
           end;

      Until FindNext(s)<>0;

      FindClose(s);
    end;
  end;

var t : Integer;
begin
  GetRecursiveFolders(Source);

  if IsLazarus then
  begin
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\win32');
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\inc');
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\win');
  end;

  for t:=0 to UserPaths.Count-1 do
      Paths.Add(UserPaths[t]);
end;

end.
