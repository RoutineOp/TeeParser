unit TeeCodeTypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, TeeCode;

type
  TSelectType=procedure(Sender:TObject; const AType:TTypeDeclaration) of object;

  TCodeTypes = class(TForm)
    LTypes: TListBox;
    procedure LTypesClick(Sender: TObject);
  private
    { Private declarations }
    FOnSelect : TSelectType;
  public
    { Public declarations }
    procedure Fill(const ABlock:TBlock);

    property OnSelect:TSelectType read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

{ TCodeTypes }

procedure TCodeTypes.Fill(const ABlock: TBlock);

  procedure FillTypes(const ATypes:TTypeDeclarations);
  var t: Integer;
  begin
    if ATypes<>nil then
       for t:=0 to ATypes.Count-1 do
           LTypes.Items.AddObject(ATypes[t].Name,ATypes[t]);
  end;

  procedure FillSection(const ASection:TSection);
  begin
    if ASection<>nil then
       FillTypes(ASection.Types);
  end;

begin
  LTypes.Items.BeginUpdate;
  try
    LTypes.Clear;
    LTypes.Sorted:=False;

    if ABlock is TUnit then
       FillSection(TUnit(ABlock).UnitInterface);

    if ABlock is TImplementationModule then
       FillSection(TImplementationModule(ABlock).ModuleImplementation);

    if ABlock is TTypeDeclaration then
       if TTypeDeclaration(ABlock).Expression is TRecordSpecification then
          FillTypes(TRecordSpecification(TTypeDeclaration(ABlock).Expression).Types);

    LTypes.Sorted:=True;
  finally
    LTypes.Items.EndUpdate;
  end;
end;

procedure TCodeTypes.LTypesClick(Sender: TObject);
var tmpType : TTypeDeclaration;
begin
  if LTypes.ItemIndex<>-1 then
  begin
    tmpType:=TTypeDeclaration(LTypes.Items.Objects[LTypes.ItemIndex]);

    Caption:=IntToStr(tmpType.Index);

    if Assigned(FOnSelect) then
       FOnSelect(Self,tmpType);
  end;
end;

end.
