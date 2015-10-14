unit TeeCodeTraverse;

interface

uses
  TeeCode;

type
  TBlockProc=procedure(const Sender:TBlock);

procedure Traverse(const ABlock:TBlock; const AProc:TBlockProc);

implementation

procedure Traverse(const ABlock:TBlock; const AProc:TBlockProc);

  procedure DoTraverse(const ABlock:TBlock);
  var t : Integer;
  begin
    if ABlock=nil then
       Exit;

    AProc(ABlock);

    if ABlock is TUnit then
    begin
      DoTraverse(TUnit(ABlock).UnitInterface);
      DoTraverse(TUnit(ABlock).UnitInitialization);
      DoTraverse(TUnit(ABlock).UnitFinalization);
      DoTraverse(TUnit(ABlock).ModuleImplementation);
    end
    else
    if ABlock is TUsesSection then
    begin
      for t:=0 to TUsesSection(ABlock).UsesUnits.Count-1 do
          DoTraverse(TUsesSection(ABlock).UsesUnits[t]);
    end
    else
    if ABlock is TStatements then
    begin
      for t:=Low(TStatements(ABlock).Items) to High(TStatements(ABlock).Items) do
          DoTraverse(TStatements(ABlock).Items[t]);
    end
    else
    if ABlock is TIdentifiers then
    begin
      for t:=0 to TIdentifiers(ABlock).Count-1 do
          DoTraverse(TIdentifiers(ABlock).Item[t]);
    end
    else
    if ABlock is TRecordSpecification then
    begin
      DoTraverse(TRecordSpecification(ABlock).Constants);
      DoTraverse(TRecordSpecification(ABlock).Fields);
      DoTraverse(TRecordSpecification(ABlock).Methods);
      DoTraverse(TRecordSpecification(ABlock).Types);
      //DoTraverse(TRecordSpecification(ABlock).ThreadVariables);
    end;
  end;

begin
  DoTraverse(ABlock);
end;

end.
