unit BasicDemoUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  TeePascal, TeeCodeModules, TeeCodeBrowser, TeePascalEmit;

type
  TPascalDemo = class(TForm)
    Panel1: TPanel;
    ListIDEs: TListBox;
    Panel2: TPanel;
    LTime: TLabel;
    BParseVCL: TButton;
    Log: TMemo;
    BModules: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListIDEsClick(Sender: TObject);
    procedure BParseVCLClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BModulesClick(Sender: TObject);
  private
    { Private declarations }

    Parser : TPascal;
  public
    { Public declarations }
  end;

var
  PascalDemo: TPascalDemo;

implementation

{$R *.dfm}

procedure TPascalDemo.BModulesClick(Sender: TObject);
begin
  TCodeBrowser.Edit(Self,Parser,TPascalEmit);
end;

procedure TPascalDemo.BParseVCLClick(Sender: TObject);
var Version : Integer;
    FormsUnit : String;
    t1 : Cardinal;
begin
  Version:=Integer(ListIDEs.Items.Objects[ListIDEs.ItemIndex]);

  Parser.Free;
  Parser:=TPascal.CreateVersion(Version);

  FormsUnit:=Parser.IDE.RootDir+'\Source\VCL\';

  if Parser.IDE.HasScopes then
     FormsUnit:=FormsUnit+'VCL.Forms.pas'
  else
     FormsUnit:=FormsUnit+'Forms.pas';

  Screen.Cursor:=crHourGlass;
  try
    t1:=GetTickCount;

    Parser.ParseUnit(FormsUnit);

    LTime.Caption:=IntToStr(GetTickCount-t1)+' msec';

    Log.Clear;
    Log.Lines.Add('Compiler Version: '+IntToStr(Parser.CompilerVersion));
    Log.Lines.Add('Lines: '+IntToStr(Parser.ParsedLines));
    Log.Lines.Add('Blocks: '+IntToStr(Parser.ParsedBlocks));

    BModules.Enabled:=True;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TPascalDemo.FormCreate(Sender: TObject);
begin
  TIDE.Detect(ListIDEs.Items);

  if ListIDEs.Count>0 then
  begin
    ListIDEs.ItemIndex:=ListIDEs.Count-1;
    ListIDEsClick(Self);
  end;
end;

procedure TPascalDemo.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TPascalDemo.ListIDEsClick(Sender: TObject);
begin
  BParseVCL.Enabled:=ListIDEs.ItemIndex<>-1;
end;

end.
