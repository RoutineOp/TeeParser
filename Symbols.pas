unit Symbols;

interface

uses
  Classes, TeeCode, TeePascal;

procedure Filter(const Modules:TModules; S:String; const Lines:TStrings);

implementation

uses
  SysUtils;

function StartsWith(const S,Sub:String):Boolean;
var L : Integer;
begin
  L:=Length(Sub);
  result:=(Length(S) >= L) and SameText(Copy(S,1,L),Sub);
end;

type
  TIdentifiersAccess=class(TIdentifiers);

procedure Filter(const Modules:TModules; S:String; const Lines:TStrings);

  procedure FilterSymbols(const I:TIdentifiers);
  var t : Integer;
      tmp : TIdentifier;
      Q : String;
  begin
    if Assigned(I) then
    for t:=0 to I.Count-1 do
    begin
      tmp:=TIdentifiersAccess(I).Get(t);

      Q:=tmp.Qualified;

      if Pos(S,UpperCase(Q))>0 then
         Lines.AddObject(Q,tmp);
    end;
  end;

  procedure FilterRecord(const R:TRecordSpecification);
  begin
    FilterSymbols(R.Fields);
    FilterSymbols(R.Methods);
  end;

  procedure FilterSection(const ASection:TUsesSection);
  var tmp : TTypeDeclaration;
        t : Integer;
  begin
    FilterSymbols(ASection.Constants);
    FilterSymbols(ASection.Variables);
    FilterSymbols(ASection.Types);
    FilterSymbols(ASection.Methods);
    FilterSymbols(ASection.Labels);
    FilterSymbols(ASection.ResourceStrings);

    if ASection.Types<>nil then
    for t:=0 to ASection.Types.Count-1 do
    begin
      tmp:=ASection.Types[t];

      if tmp.Expression is TRecordSpecification then
         FilterRecord(TRecordSpecification(tmp.Expression));
    end;
  end;

  procedure FilterModule(const AModule:TModule);
  begin
    if StartsWith(AModule.Name,S) then
       Lines.Add(AModule.Qualified);

    if AModule is TUnit then
       FilterSection(TUnit(AModule).UnitInterface)
    else
    if AModule is TImplementationModule then
       FilterSection(TImplementationModule(AModule).ModuleImplementation);
  end;

var t : Integer;
begin
  Lines.BeginUpdate;
  try
    Lines.Clear;

    S:=UpperCase(Trim(S));

    if S<>'' then
       for t:=0 to Modules.PackageContains.Count-1 do
           FilterModule(Modules.PackageContains[t]);
  finally
    Lines.EndUpdate;
  end;
end;

end.
