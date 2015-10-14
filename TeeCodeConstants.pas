unit TeeCodeConstants;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, TeeCode;

type
  TSelectConstant=procedure(Sender:TObject; const AConst:TConstantDeclaration) of object;

  TCodeConstants = class(TForm)
    LBConstants: TListBox;
    procedure LBConstantsClick(Sender: TObject);
  private
    { Private declarations }

    FOnSelect : TSelectConstant;
  public
    { Public declarations }

    procedure Clear;
    procedure Fill(const ABlock:TBlock);

    property OnSelect:TSelectConstant read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

{ TCodeConstants }

procedure TCodeConstants.Clear;
begin
  LBConstants.Clear;
end;

procedure TCodeConstants.Fill(const ABlock: TBlock);

  procedure FillConstants(const AConsts:TConstantDeclarations);
  var t: Integer;
  begin
    if AConsts<>nil then
       for t:=0 to AConsts.Count-1 do
           LBConstants.Items.AddObject(AConsts[t].Name,AConsts[t]);
  end;

  procedure FillSection(const ASection:TSection);
  begin
    if ASection<>nil then
       FillConstants(ASection.Constants);
  end;

begin
  LBConstants.Items.BeginUpdate;
  try
    LBConstants.Clear;
    LBConstants.Sorted:=False;

    if ABlock is TUnit then
       FillSection(TUnit(ABlock).UnitInterface);

    if ABlock is TImplementationModule then
       FillSection(TImplementationModule(ABlock).ModuleImplementation);

    if ABlock is TTypeDeclaration then
       if TTypeDeclaration(ABlock).Expression is TRecordSpecification then
          FillConstants(TRecordSpecification(TTypeDeclaration(ABlock).Expression).Constants);

    LBConstants.Sorted:=True;
  finally
    LBConstants.Items.EndUpdate;
  end;
end;

procedure TCodeConstants.LBConstantsClick(Sender: TObject);
begin
  if (LBConstants.ItemIndex<>-1) and Assigned(FOnSelect) then
     FOnSelect(Self,TConstantDeclaration(LBConstants.Items.Objects[LBConstants.ItemIndex]));
end;

end.
