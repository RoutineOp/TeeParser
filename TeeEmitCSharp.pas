unit TeeEmitCSharp;
{$I TeeDefs.inc}

interface

uses
  TeeCode;

type
  TCSharp=class
  public
    class function Emit(const ABlock:TBlock):String;
    class function EmitUnit(const AUnit:TUnit):String;
    class function EmitUses(const AUses:TUses):String;
  end;

implementation

const
  CRLF=#13#10;

{ TCSharp }

class function TCSharp.Emit(const ABlock: TBlock): String;
begin
  if ABlock is TUnit then
     result:=EmitUnit(TUnit(ABlock))
  else
     result:='?';
end;

function TypeName(const AType:TTypeDeclaration):String;
begin
  if (AType=nil) or (AType.Name='') then
     result:='?'
  else
     result:=AType.Name;
end;

function ResultTypeName(const ASpec:TMethodSpecification):String;
begin
  if ASpec is TFunctionSpecification then
     result:=TypeName(TFunctionSpecification(ASpec).ResultValue.TypeIdentifier)
  else
     result:='void';
end;

function MethodParams(const ASpec:TMethodSpecification):String;
var tmp : TParameters;
    t : Integer;
begin
  if ASpec is TMethodParams then
  begin
    result:='(';

    tmp:=TMethodParams(ASpec).Parameters;

    if tmp<>nil then
    for t:=0 to tmp.Count-1 do
    begin
      if t>0 then
         result:=result+', ';

      result:=result+TypeName(tmp[t].TypeIdentifier)+' '+tmp[t].Name;
    end;

    result:=result+')';
  end
  else
     result:='()';
end;

function BlockToString(const B:TBlock):String;
begin
  if B=nil then
     result:='?'
  else
     result:={$IFDEF D9}B.ToString{$ELSE}(B as TExpression).ToString{$ENDIF};
end;

function OperatorToString(const AOperator: TOperator): String;
begin
  case AOperator of
     opEqual: result:='==';
  opNotEqual: result:='!=';
       opAnd: result:='&&';
        opOr: result:='||';
       opXor: result:='^';
       opShl: result:='<<';
       opShr: result:='>>';
     opLower: result:='<';
   opGreater: result:='>';
opLowerEqual: result:='<=';
       opAdd: result:='+';
  opSubtract: result:='-';
  opMultiply: result:='*';
    opDivide: result:='/';
       opNot: result:='!';
       opDiv: result:='div';
       opMod: result:='%';
        opIn: result:='in'; // !!!
        opIs: result:='is';
        opAs: result:='as';
        opAt: result:='at'; // !!!
  else
     result:='>=';
  end;
end;

class function TCSharp.EmitUnit(const AUnit: TUnit): String;

  function EmitConstants(const Ident:String; const AConstants:TConstantDeclarations):String;
  var t : Integer;
      tmp : TConstantDeclaration;
  begin
    result:='';

    if AConstants<>nil then
    for t:=0 to AConstants.Count-1 do
    begin
      tmp:=AConstants[t];

      result:=result+Ident+'public static const '+TypeName(tmp.TypeIdentifier)+' '+
              tmp.Name+'='+BlockToString(tmp.Value)+CRLF;
    end;
  end;

  function EmitVariables(const Ident:String; const AVariables:TVariableDeclarations):String;
  var t : Integer;
      tmp : TVariableDeclaration;
  begin
    result:='';

    if AVariables<>nil then
    for t:=0 to AVariables.Count-1 do
    begin
      tmp:=AVariables[t];

      result:=result+Ident+'public static '+TypeName(tmp.TypeIdentifier)+' '+
        tmp.Name+'()'+CRLF+Ident+'{'+CRLF+
        Ident+'  return '+BlockToString(tmp.Value)+CRLF+
        Ident+'}'+CRLF+CRLF;
    end;
  end;

  function EmitExpression(const AExp:TExpression):String;
  begin
    if AExp is TOperatorExpression then
       result:=EmitExpression(TOperatorExpression(AExp).Left)+
               OperatorToString(TOperatorExpression(AExp).Operat)+
               EmitExpression(TOperatorExpression(AExp).Right)
    else
       result:=AExp.ToString;
  end;

  function EmitStatements(const Ident:String; const AStatements:TStatements):String; forward;

  function EmitStatement(const Ident:String; const AStatement:TStatement):String;
  begin
    if AStatement is TIf then
    begin
      result:='if ('+EmitExpression(TIf(AStatement).Expression)+')'+CRLF+
              EmitStatements(Ident+'  ',TIf(AStatement).ThenPart);

      if TIf(AStatement).ElsePart<>nil then
         result:=result+CRLF+'else'+CRLF+EmitStatements(Ident+'  ',TIf(AStatement).ElsePart);
    end
    else
       result:='?'+AStatement.ClassName;
  end;

  function EmitStatements(const Ident:String; const AStatements:TStatements):String;
  var t : Integer;
  begin
    for t:=Low(AStatements.Items) to High(AStatements.Items) do
        result:=result+EmitStatement(Ident,TStatement(AStatements.Items[t]))+CRLF;
  end;

  function BodyOfMethod(const ASpec:TMethodSpecification):TBody;
  begin
    result:=ASpec.Body;

    if result=nil then
    begin
      // Pending: Find body of method in aspec's Unit ModuleImplementation
    end;
  end;

  function EmitBody(const Ident:String; const ABody:TBody):String;
  begin
    // ABody.Constants..Variables..Types etc ?
    if ABody=nil then
       result:='?TBody'
    else
       result:=EmitStatements(Ident,ABody.Statements);
  end;

  function EmitMethods(const Ident:String; const AMethods:TMethodDeclarations):String;
  var t : Integer;
      tmp : TMethodDeclaration;
      tmpSpec : TMethodSpecification;
  begin
    result:='';

    if AMethods<>nil then
    for t:=0 to AMethods.Count-1 do
    begin
      tmp:=AMethods[t];
      tmpSpec:=TMethodSpecification(tmp.TypeIdentifier.Expression);

      result:=result+Ident+'public '+ResultTypeName(tmpSpec)+' '+
        tmp.Name+MethodParams(tmpSpec)+CRLF+Ident+'{'+CRLF+
           EmitBody(Ident+'  ',BodyOfMethod(tmpSpec))+CRLF+
        Ident+'}'+CRLF+CRLF;
    end;
  end;

  function EmitTypeSpec(const Ident:String; const ASpec:TTypeSpecification):String;
  begin
    if ASpec is TRecordSpecification then
    begin
      result:=EmitConstants(Ident+'  ',TRecordSpecification(ASpec).Constants)+
              EmitVariables(Ident+'  ',TRecordSpecification(ASpec).Fields)+
              EmitMethods(Ident+'  ',TRecordSpecification(ASpec).Methods)

              // ASpec.Types ?
    end
    else
      result:='?';
  end;

  function EmitType(const Ident:String; const AType:TTypeDeclaration):String;
  var tmpSpec : TTypeSpecification;
  begin
    tmpSpec:=AType.Expression;

    if tmpSpec is TInterfaceSpecification then
       result:='interface'
    else
    if tmpSpec is TClassSpecification then
       result:='class'
    else
    if tmpSpec is TRecordSpecification then
       result:='struct'
    else
       result:='?';

    result:=result+' '+AType.Name+CRLF+'{'+EmitTypeSpec(Ident,tmpSpec)+CRLF+Ident+'}';
  end;

  function EmitTypes(const Ident,Visible:String; const ATypes:TTypeDeclarations):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to ATypes.Count-1 do
        result:=result+Ident+Visible+' '+EmitType(Ident,ATypes[t])+CRLF;
  end;

  function EmitGlobals(const Ident,Visible,Name:String; const ASection:TSection):String;
  begin
    result:='';

    if (ASection.Variables<>nil) or (ASection.Constants<>nil) then
       result:=Ident+Visible+' class '+Name+CRLF+Ident+'{'+CRLF+
          EmitConstants(Ident+'  ',ASection.Constants)+
          EmitVariables(Ident+'  ',ASection.Variables)+
       Ident+'}'+CRLF;
  end;

  function EmitSection(const Ident,Visible,Name:String; ASection:TSection):String;
  begin
    if ASection=nil then
       result:=''
    else
    begin
      result:=EmitGlobals(Ident,Visible,Name,ASection);

      if ASection.Types<>nil then
         result:=result+EmitTypes(Ident,Visible,ASection.Types);
    end;
  end;

begin
  if AUnit.UnitInterface.UsesUnits=nil then
     result:=''
  else
     result:=EmitUses(AUnit.UnitInterface.UsesUnits)+CRLF;

  if AUnit.ModuleImplementation<>nil then
     if AUnit.ModuleImplementation.UsesUnits<>nil then
        result:=result+CRLF+EmitUses(AUnit.ModuleImplementation.UsesUnits)+CRLF;

  result:=result+'namespace '+AUnit.Name+CRLF+'{'+CRLF+
     EmitSection('  ','public','Globals',AUnit.UnitInterface)+
     EmitSection('  ','private','PrivateGlobals',AUnit.ModuleImplementation)+
  '}'+CRLF;
end;

class function TCSharp.EmitUses(const AUses: TUses): String;
var t : Integer;
begin
  result:='';

  for t:=0 to AUses.Count-1 do
      result:=result+'using '+AUses.Item[t].Name+';'+CRLF;
end;

end.
