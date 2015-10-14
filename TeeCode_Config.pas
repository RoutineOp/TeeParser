unit TeeCode_Config;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ExtCtrls, ComCtrls, TeePascal;

type
  TFormConfig = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    SearchPaths: TMemo;
    Label2: TLabel;
    ComboIDE: TComboBox;
    LCompiler: TLabel;
    Label3: TLabel;
    LRoot: TLabel;
    LScope: TLabel;
    EScopes: TEdit;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    LBDefines: TListBox;
    LUserScopes: TLabel;
    UserScopes: TEdit;
    Label6: TLabel;
    LBUserDefines: TListBox;
    Button3: TButton;
    BRemoveDefine: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboIDEChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LBUserDefinesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BRemoveDefineClick(Sender: TObject);
  private
    { Private declarations }

    function IDEDate(const IDE:TIDE):String;
  public
    { Public declarations }
    Config : TIniFile;

    class function Edit(const AConfig:TIniFile):Boolean;
    function IDEVersion:Integer;
    class procedure ReadItems(const AConfig:TIniFile; const ASection:String; const ALines:TStrings);
    procedure WriteItems(const ASection:String; const ALines:TStrings);
    procedure WriteConfig;
  end;

implementation

{$R *.dfm}

function TFormConfig.IDEVersion:Integer;
begin
  if ComboIDE.ItemIndex=-1 then
     result:=0
  else
     result:=Integer(ComboIDE.Items.Objects[ComboIDE.ItemIndex]);
end;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  TIDE.Detect(ComboIDE.Items);
end;

procedure TFormConfig.FormShow(Sender: TObject);
var t,
    Ver : Integer;
    tmp : TStringList;
begin
  if ComboIDE.Items.Count=0 then
     ShowMessage('No Pascal IDEs detected');

  Config.ReadSection('SearchPaths',SearchPaths.Lines);

  if Config.ValueExists('IDE','Version') then
  begin
    Ver:=Config.ReadInteger('IDE','Version',0);

    ReadItems(Config,'PATHS',SearchPaths.Lines);
    ReadItems(Config,'DEFINES',LBUserDefines.Items);

    tmp:=TStringList.Create;
    try
      tmp.Delimiter:=';';
      ReadItems(Config,'UNITSCOPES',tmp);
      UserScopes.Text:=tmp.DelimitedText;
    finally
      tmp.Free;
    end;

    if Ver>0 then
    for t:=0 to ComboIDE.Items.Count-1 do
        if Integer(ComboIDE.Items.Objects[t])=Ver then
        begin
          ComboIDE.ItemIndex:=t;
          ComboIDEChange(Self);
          break;
        end;
  end;
end;

function Join(const AList:TStrings; const ADelimiter:String):String;
var t : Integer;
begin
  result:='';

  for t:=0 to AList.Count-1 do
      if t<AList.Count-1 then
         result:=result+AList[t]+ADelimiter
      else
         result:=result+AList[t];
end;

function FileDate(const AFile:String):String;
var tmpDate : {$IFDEF D12}TDateTimeInfoRec{$ELSE}Integer{$ENDIF};
begin
  {$IFDEF D12}
  if FileGetDateTimeInfo(AFile,tmpDate) then
     result:=FormatDateTime('mmm yyyy',tmpDate.CreationTime)
  else
     result:='';
  {$ELSE}
  result:='';
  {$ENDIF}
end;

function TFormConfig.IDEDate(const IDE:TIDE):String;
begin
  result:=FileDate(ide.RootDir+'bin\dcc32.exe');

  if result<>'' then
     result:='('+result+')';
end;

procedure TFormConfig.ComboIDEChange(Sender: TObject);
var ide : TIDE;
begin
  ide:=TIDE.Create(IDEVersion);
  try
    LRoot.Caption:=ide.Source;

    LCompiler.Caption:='Compiler Version: '+
                       IntToStr(ide.CompilerVersion)+' '+
                       IDEDate(ide);

    LBDefines.Items.Clear;
    LBDefines.Items.AddStrings(ide.Defines);

    LScope.Visible:=ide.CompilerVersion>=230;
    EScopes.Visible:=LScope.Visible;

    LUserScopes.Visible:=LScope.Visible;
    UserScopes.Visible:=LScope.Visible;

    if EScopes.Visible then
       EScopes.Text:=Join(ide.UnitScopes,';');
  finally
    ide.Free;
  end;
end;

class procedure TFormConfig.ReadItems(const AConfig:TIniFile; const ASection:String; const ALines:TStrings);
var tmp,
    t : Integer;
begin
  ALines.Clear;

  tmp:=AConfig.ReadInteger(ASection,'Count',0);

  for t:=0 to tmp-1 do
      ALines.Add(AConfig.ReadString(ASection,'Item'+IntToStr(t),''));
end;

procedure TFormConfig.WriteConfig;
var Ver : Integer;
    tmp : TStringList;
begin
  Ver:=IDEVersion;

  Config.WriteInteger('IDE','Version',Ver);

  WriteItems('PATHS',SearchPaths.Lines);
  WriteItems('DEFINES',LBUserDefines.Items);

  tmp:=TStringList.Create;
  try
    tmp.Delimiter:=';';
    tmp.DelimitedText:=UserScopes.Text;
    WriteItems('UNITSCOPES',tmp);
  finally
    tmp.Free;
  end;
end;

procedure TFormConfig.WriteItems(const ASection:String; const ALines:TStrings);
var t : Integer;
begin
  Config.WriteInteger(ASection,'Count',ALines.Count);

  for t:=0 to ALines.Count-1 do
      Config.WriteString(ASection,'Item'+IntToStr(t),ALines[t]);
end;

procedure TFormConfig.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

class function TFormConfig.Edit(const AConfig:TIniFile):Boolean;
begin
  with TFormConfig.Create(nil) do
  try
    Config:=AConfig;

    result:=ShowModal=mrOk;

    if result then
       WriteConfig;
  finally
    Free;
  end;
end;

procedure TFormConfig.LBUserDefinesClick(Sender: TObject);
begin
  BRemoveDefine.Enabled:=LBUserDefines.ItemIndex<>-1;
end;

procedure TFormConfig.Button3Click(Sender: TObject);
var S : String;
begin
  if InputQuery('Add Custom Define','Define',S) then
  begin
    S:=UpperCase(Trim(S));

    if S<>'' then
    begin
      if LBUserDefines.Items.IndexOf(S)=-1 then
      begin
        LBUserDefines.Items.Add(S);

        LBUserDefines.ItemIndex:=LBUserDefines.Count-1;
        LBUserDefinesClick(Self);
      end;
    end;
  end;
end;

procedure TFormConfig.BRemoveDefineClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBUserDefines.ItemIndex;
  LBUserDefines.Items.Delete(tmp);

  if LBUserDefines.Items.Count<=tmp then
     LBUserDefines.ItemIndex:=LBUserDefines.Count-1
  else
     LBUserDefines.ItemIndex:=tmp;

  LBUserDefinesClick(Self);
end;

end.
