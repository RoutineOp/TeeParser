unit TeeCodeVariables;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, TeeCode;

type
  TSelectVariable=procedure(Sender:TObject; const AVar:TVariableDeclaration) of object;

  TCodeVariables = class(TForm)
    LBVariables: TListBox;
    procedure LBVariablesClick(Sender: TObject);
  private
    { Private declarations }

    FOnSelect : TSelectVariable;
  public
    { Public declarations }

    procedure Clear;
    procedure Fill(const ABlock:TBlock);

    property OnSelect:TSelectVariable read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

{ TCodeVariables }

procedure TCodeVariables.Clear;
begin
  LBVariables.Clear;
end;

procedure TCodeVariables.Fill(const ABlock: TBlock);

  procedure FillVariables(const AVars:TVariableDeclarations);
  var t: Integer;
  begin
    if AVars<>nil then
       for t:=0 to AVars.Count-1 do
           LBVariables.Items.AddObject(AVars[t].Name,AVars[t]);
  end;

  procedure FillSection(const ASection:TSection);
  begin
    if ASection<>nil then
       FillVariables(ASection.Variables);
  end;

begin
  LBVariables.Items.BeginUpdate;
  try
    LBVariables.Clear;
    LBVariables.Sorted:=False;

    if ABlock is TUnit then
       FillSection(TUnit(ABlock).UnitInterface);

    if ABlock is TImplementationModule then
       FillSection(TImplementationModule(ABlock).ModuleImplementation);

    if ABlock is TTypeDeclaration then
       if TTypeDeclaration(ABlock).Expression is TRecordSpecification then
          FillVariables(TRecordSpecification(TTypeDeclaration(ABlock).Expression).Fields);

    LBVariables.Sorted:=True;
  finally
    LBVariables.Items.EndUpdate;
  end;
end;

procedure TCodeVariables.LBVariablesClick(Sender: TObject);
begin
  if (LBVariables.ItemIndex<>-1) and Assigned(FOnSelect) then
     FOnSelect(Self,TVariableDeclaration(LBVariables.Items.Objects[LBVariables.ItemIndex]));
end;

end.
