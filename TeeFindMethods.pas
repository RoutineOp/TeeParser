unit TeeFindMethods;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, TeeCode;

type
  TFindMethods = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FLanguage : TLanguage;
  public
    { Public declarations }
    class procedure Edit(const AOwner:TComponent; const ALanguage:TLanguage);
  end;

implementation

{$R *.dfm}

uses
  TeePascalEmit;

{$IFNDEF D16}
function StartsWith(const S,Sub:String):Boolean;
begin
  result:=Copy(UpperCase(S),1,Length(Sub))=UpperCase(Sub);
end;
{$ENDIF}

type
  TMethods=Array of TMethodDeclaration;

procedure FindMethods(const AModule:TModule; const S:String; var AMethods:TMethods);

  procedure FindIn(const M:TMethodDeclarations);
  var t,L : Integer;
      tmp : TMethodDeclaration;
  begin
    if M<>nil then
    for t:=0 to M.Count-1 do
    begin
      tmp:=M[t];

      if {$IFDEF D16}tmp.Name.StartsWith(S,True){$ELSE}StartsWith(tmp.Name,S){$ENDIF} then
         if TMethodSpecification(tmp.TypeIdentifier.Expression).Body<>nil then
         begin
           L:=Length(AMethods);
           SetLength(AMethods,L+1);
           AMethods[L]:=tmp;
         end;
    end;
  end;

  procedure FindInTypes(const Types:TTypeDeclarations); forward;

  procedure FindInRecord(const ARecord:TRecordSpecification);
  begin
    FindIn(ARecord.Methods);
    FindInTypes(ARecord.Types);
  end;

  procedure FindInTypes(const Types:TTypeDeclarations);
  var t : Integer;
  begin
    if Types<>nil then
    for t:=0 to Types.Count-1 do
        if Types[t].Expression is TRecordSpecification then
           FindInRecord(TRecordSpecification(Types[t].Expression));
  end;

begin
  if AModule is TUnit then
    if TUnit(AModule).UnitInterface<>nil then
    begin
      FindIn(TUnit(AModule).UnitInterface.Methods);
      FindInTypes(TUnit(AModule).UnitInterface.Types);
    end;

  if AModule is TImplementationModule then
    if TImplementationModule(AModule).ModuleImplementation<>nil then
    begin
      FindIn(TImplementationModule(AModule).ModuleImplementation.Methods);
      FindInTypes(TImplementationModule(AModule).ModuleImplementation.Types);
    end;
end;

procedure TFindMethods.Button1Click(Sender: TObject);
var tmp : TMethods;
    t : Integer;
begin
  for t:=0 to FLanguage.Modules.PackageContains.Count-1 do
      FindMethods(FLanguage.Modules.PackageContains[t],Edit1.Text,tmp);

  ListBox1.Clear;
  ListBox1.Items.BeginUpdate;
  try
    for t:=0 to High(tmp) do
        ListBox1.Items.AddObject(tmp[t].Qualified,tmp[t]);
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

class procedure TFindMethods.Edit(const AOwner: TComponent; const ALanguage:TLanguage);
begin
  with TFindMethods.Create(AOwner) do
  try
    FLanguage:=ALanguage;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFindMethods.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
  begin
    Key:=#0;
    Button1Click(Self);
  end;
end;

procedure TFindMethods.ListBox1Click(Sender: TObject);
begin
  Memo1.Text:=TPascalEmit.AsString(TBlock(ListBox1.Items.Objects[ListBox1.ItemIndex]));
end;

end.
