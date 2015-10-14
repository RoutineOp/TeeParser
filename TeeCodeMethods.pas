unit TeeCodeMethods;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, TeeCode, StdCtrls;

type
  TSelectMethod=procedure(Sender:TObject; const AMethod:TMethodDeclaration) of object;

  TCodeMethods = class(TForm)
    LBMethods: TListBox;
    procedure LBMethodsClick(Sender: TObject);
  private
    { Private declarations }
    FOnSelect : TSelectMethod;
  public
    { Public declarations }

    procedure Clear;
    procedure Fill(const ABlock:TBlock);

    property OnSelect:TSelectMethod read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

{ TCodeMethods }

procedure TCodeMethods.Clear;
begin
  LBMethods.Clear;
end;

procedure TCodeMethods.Fill(const ABlock: TBlock);

  procedure FillMethods(const AMethods:TMethodDeclarations);
  var t: Integer;
      tmp : String;
  begin
    if AMethods<>nil then
       for t:=0 to AMethods.Count-1 do
       begin
         tmp:=AMethods[t].Name;

         if tmp='' then
            tmp:='(unnamed)';

         LBMethods.Items.AddObject(tmp,AMethods[t]);
       end;
  end;

  procedure FillSection(const ASection:TSection);
  begin
    if ASection<>nil then
       FillMethods(ASection.Methods);
  end;

begin
  LBMethods.Items.BeginUpdate;
  try
    LBMethods.Clear;
    LBMethods.Sorted:=False;

    if ABlock is TUnit then
       FillSection(TUnit(ABlock).UnitInterface);

    if ABlock is TImplementationModule then
       FillSection(TImplementationModule(ABlock).ModuleImplementation);

    if ABlock is TTypeDeclaration then
       if TTypeDeclaration(ABlock).Expression is TRecordSpecification then
          FillMethods(TRecordSpecification(TTypeDeclaration(ABlock).Expression).Methods);

    LBMethods.Sorted:=True;
  finally
    LBMethods.Items.EndUpdate;
  end;
end;

procedure TCodeMethods.LBMethodsClick(Sender: TObject);
begin
  if (LBMethods.ItemIndex<>-1) and Assigned(FOnSelect) then
     FOnSelect(Self,TMethodDeclaration(LBMethods.Items.Objects[LBMethods.ItemIndex]));
end;

end.
