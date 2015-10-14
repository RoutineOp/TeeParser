unit TreeCode;

interface

uses
  Classes, TeeTree, TeeCode, TeePascal;

procedure FillCode(const ATree:TTree; const ABlock:TBlock);

implementation

uses
  SysUtils;

function OperatorToString(const AOperator: TOperator): String;
begin
  case AOperator of
     opEqual: result:='=';
  opNotEqual: result:='<>';
       opAnd: result:='and';
        opOr: result:='or';
       opXor: result:='xor';
       opShl: result:='shl';
       opShr: result:='shr';
     opLower: result:='<';
   opGreater: result:='>';
opLowerEqual: result:='<=';
       opAdd: result:='+';
  opSubtract: result:='-';
  opMultiply: result:='*';
    opDivide: result:='/';
       opNot: result:='not';
       opDiv: result:='div';
       opMod: result:='mod';
        opIn: result:='in';
        opIs: result:='is';
        opAs: result:='as';
        opAt: result:='at';
  else
     result:='>=';
  end;
end;

procedure Fill(const AParent:TTreeNodeShape; const AName:String; const ABlock:TBlock); forward;

procedure AddParameters(const AParent:TTreeNodeShape; const ABlock:TBlock; const P:TParameters);
var tmp : TTreeNodeShape;
    t : Integer;
begin
  if (P<>nil) and (P.Count>0) then
  begin
    tmp:=AParent.AddChildObject('parameters',ABlock);

    for t:=0 to P.Count-1 do
        Fill(tmp,P[t].Name,P[t]);
  end;
end;

procedure FillExpression(const AParent: TTreeNodeShape; const ANode: TBlock); forward;

procedure FillRange(const AParent:TTreeNodeShape; const ARange:TRangeExpression);
var tmp : TTreeNodeShape;
begin
  tmp:=AParent.AddChildObject('start',ARange.StartRange);

  if ARange.StartRange<>nil then
     tmp.AddChildObject(ARange.StartRange.ToString,ARange.StartRange)
  else
     tmp.AddChild('?');

  tmp:=AParent.AddChildObject('end',ARange.EndRange);

  if ARange.EndRange<>nil then
     tmp.AddChildObject(ARange.EndRange.ToString,ARange.EndRange)
  else
     tmp.AddChild('?');
end;


procedure FillType(const AParent:TTreeNodeShape; const AType:TTypeDeclaration);

  procedure FillArray(const AParent:TTreeNodeShape; const AArray:TArraySpecification);
  var t : Integer;
      tmp : TTreeNodeShape;
  begin
    if AArray.Dimensions<>nil then
    begin
      tmp:=AParent.AddChildObject('dimensions',AArray);

      for t:=Low(AArray.Dimensions) to High(AArray.Dimensions) do
          FillExpression(tmp,AArray.Dimensions[t]);
    end;

    tmp:=AParent.AddChildObject('of',AArray.Expression);

    if AArray.Expression=nil then
       tmp.AddChild('?')
    else
       FillType(tmp,AArray.Expression);
  end;

var tmp : TTreeNodeShape;
begin
  if AType.Expression is TArraySpecification then
  begin
    tmp:=AParent.AddChildObject('array',AType);
    FillArray(tmp,TArraySpecification(AType.Expression));
  end
  else
     AParent.AddChildObject(AType.Name,AType);
end;

procedure FillExpressions(const AParent: TTreeNodeShape; const AParams:TExpressions);
var t : Integer;
begin
  for t:=Low(AParams) to High(AParams) do
      FillExpression(AParent,AParams[t]);
end;

procedure FillExpression(const AParent: TTreeNodeShape; const ANode: TBlock);
var tmp : TTreeNodeShape;
    tmpExp : TExpression;
begin
  if ANode is TOperatorExpression then
  begin
    tmp:=AParent.AddChildObject(OperatorToString(TOperatorExpression(ANode).Operat),ANode);

    FillExpression(tmp,TOperatorExpression(ANode).Left);
    FillExpression(tmp,TOperatorExpression(ANode).Right);
  end
  else
  if ANode is TCasting then
  begin
    tmp:=AParent.AddChildObject('casting',ANode);
    FillType(tmp,TCasting(ANode).TypeIdentifier);
    FillExpression(tmp,TCasting(ANode).Expression);
  end
  else
  if ANode is TCallExpression then
  begin
    tmpExp:=TCallExpression(ANode).Expression;

    if tmpExp is TMethodDeclaration then
       AParent.AddChildObject(TMethodDeclaration(tmpExp).Name,ANode);

    if Length(TCallExpression(ANode).Parameters)>0 then
       FillExpressions(AParent.AddChildObject('parameters',ANode),TCallExpression(ANode).Parameters);
  end
  else
  if ANode is TAddressOf then
  else
  if ANode is TFieldExpression then
  begin
    tmp:=AParent.AddChildObject('value',ANode);
    FillExpression(tmp,TFieldExpression(ANode).Value);

    if TFieldExpression(ANode).Field<>nil then
       FillExpression(tmp,TFieldExpression(ANode).Field);
  end
  else
  if ANode is TInheritedExpression then
  begin
    tmp:=AParent.AddChildObject('inherited',ANode);

    if TInheritedExpression(ANode).Expression<>nil then
       tmp.AddChildObject(TInheritedExpression(ANode).Expression.ToString,TInheritedExpression(ANode).Expression)
    else
       tmp.AddChildObject(TInheritedExpression(ANode).Method.Name,TInheritedExpression(ANode).Method)
  end
  else
  if ANode is TItemExpression then
  begin
    FillExpression(AParent,TItemExpression(ANode).Value);

    tmp:=AParent.AddChildObject('[]',TItemExpression(ANode).Items);
    FillExpressions(tmp,TItemExpression(ANode).Items);
  end
  else
  if ANode is TRangeExpression then
     FillRange(AParent,TRangeExpression(ANode))
  else
  if ANode is TExpression then
     AParent.AddChildObject(TExpression(ANode).ToString,ANode)
  else
  if ANode<>nil then
     AParent.AddChildObject(ANode.ClassName,ANode);
end;
(*
var tmp : TTreeNodeShape;
    S   : String;
    f   : TFunction;
    t   : Integer;
    a   : TArrayNode;
begin
  if ANode is TFunction then
     S:=TFunction(ANode).Name
  else
  if ANode is TExpressionNode then
     S:=TExpressionNode.OperatorToString(TExpressionNode(ANode).Operator)
  else
  if ANode is TArrayNode then
     S:='[]'
  else
     S:=ANode.Text;

  tmp:=AParent.AddChildObject(S,ANode);

  if ANode is TFunction then
  begin
    f:=TFunction(ANode);

    for t:=Low(f.Parameters) to High(f.Parameters) do
        FillExpression(tmp,f.Parameters[t]);
  end
  else
  if ANode is TExpressionNode then
  begin
    FillExpression(tmp,TExpressionNode(ANode).Left);

    if Assigned(TExpressionNode(ANode).Right) then
       FillExpression(tmp,TExpressionNode(ANode).Right);
  end
  else
  if ANode is TArrayNode then
  begin
    a:=TArrayNode(ANode);

    for t:=Low(a.Items) to High(a.Items) do
        FillExpression(tmp,a.Items[t]);
  end;
end;
*)

function VisibilityToString(const AIdent:TIdentifier):String;
begin
  case AIdent.Visibility of
    vPublic: result:='public';
    vStrictPrivate: result:='strict private';
    vPrivate: result:='private';
    vStrictProtected: result:='strict protected';
    vProtected: result:='protected';
  else
    result:='published';
  end;
end;

  procedure Fill(const AParent:TTreeNodeShape; const AName:String; const ABlock:TBlock);

    procedure AddStatements(const AParent:TTreeNodeShape; const S:TStatements);
    var t : Integer;
    begin
      if S<>nil then
      for t:=Low(S.Items) to High(S.Items) do
          Fill(AParent,S.Items[t].ClassName,S.Items[t]);
    end;

    procedure AddDirectives(const ANode:TTreeNodeShape; const D:TMethodDirectives);
    begin
      if mdStdCall in D then
         ANode.AddChildObject('stdcall',ABlock);

      if mdVirtual in D then
         ANode.AddChildObject('virtual',ABlock);

      if mdOverload in D then
         ANode.AddChildObject('overload',ABlock);

      if mdOverride in D then
         ANode.AddChildObject('override',ABlock);

      if mdForward in D then
         ANode.AddChildObject('forward',ABlock);

      if mdAssembler in D then
         ANode.AddChildObject('assembler',ABlock);

      if mdCDecl in D then
         ANode.AddChildObject('cdecl',ABlock);
    end;

  var tmp,
      tmp2,
      tmpUses : TTreeNodeShape;
      t   : Integer;
      tmpPrefix : String;
      //b : TBody;
      M : TMethodDeclaration;
      tmpSpec : TMethodSpecification;
  begin
    tmp:=AParent.AddChildObject(AName,ABlock);

    if ABlock=nil then
       Exit;

    if ABlock is TUsesSection then
    begin
      if (TUsesSection(ABlock).UsesUnits<>nil) and (TUsesSection(ABlock).UsesUnits.Count>0) then
      begin
        tmpUses:=tmp.AddChildObject('uses',TUsesSection(ABlock).UsesUnits);

        for t:=0 to TUsesSection(ABlock).UsesUnits.Count-1 do
            tmpUses.AddChildObject(TUsesSection(ABlock).UsesUnits[t].Name,TUsesSection(ABlock).UsesUnits[t]);
      end;
    end;

    if ABlock is TBody then
       AddStatements(tmp,TBody(ABlock).Statements)
    else
    if ABlock is TStatements then
       AddStatements(tmp,TStatements(ABlock));

    if ABlock is TSection then
    begin
      if TSection(ABlock).Constants<>nil then
         Fill(tmp,'const',TSection(ABlock).Constants);

      {
      if TSection(ABlock).ThreadVariables<>nil then
         Fill(tmp,'threadvar',TSection(ABlock).ThreadVariables);
      }

      if TSection(ABlock).Variables<>nil then
         Fill(tmp,'var',TSection(ABlock).Variables);

      if TSection(ABlock).Types<>nil then
         Fill(tmp,'type',TSection(ABlock).Types);

      if TSection(ABlock).Methods<>nil then
         if TSection(ABlock).Methods.Count>0 then
            Fill(tmp,'methods',TSection(ABlock).Methods);

      if TSection(ABlock).Labels<>nil then
         Fill(tmp,'label',TSection(ABlock).Labels);
    end
    else
    if ABlock is TMethodDeclarations then
    begin
      for t:=0 to TMethodDeclarations(ABlock).Count-1 do
      begin
        M:=TMethodDeclarations(ABlock).Item[t];

        tmpSpec:=TMethodSpecification(M.TypeIdentifier.Expression);

        if tmpSpec<>nil then
        begin
          if tmpSpec.IsClass then
             tmpPrefix:='class '
          else
             tmpPrefix:='';

          if tmpSpec is TFunctionSpecification then
             tmpPrefix:=tmpPrefix+'function'
          else
          if tmpSpec is TConstructorSpecification then
             tmpPrefix:=tmpPrefix+'constructor'
          else
          if tmpSpec is TDestructorSpecification then
             tmpPrefix:=tmpPrefix+'destructor'
          else
             tmpPrefix:=tmpPrefix+'procedure';

          Fill(tmp,tmpPrefix+' '+M.Name,M);
        end;
      end;
    end
    else
    if ABlock is TMethodDeclaration then
    begin
      tmp.AddChildObject(VisibilityToString(TMethodDeclaration(ABlock)),ABlock);

      if TMethodDeclaration(ABlock).TypeIdentifier.Expression is TMethodSpecification then
      begin
        tmpSpec:=TMethodSpecification(TMethodDeclaration(ABlock).TypeIdentifier.Expression);

        if tmpSpec is TFunctionSpecification then
        begin
          if TFunctionSpecification(tmpSpec).ResultValue=nil then
             tmp.AddChildObject('result',nil)
          else
          begin
            tmp2:=tmp.AddChildObject('result',TFunctionSpecification(tmpSpec).ResultValue);

            if TFunctionSpecification(tmpSpec).ResultValue.TypeIdentifier=nil then
               tmp2.AddChildObject('?',nil)
            else
               FillType(tmp2,TFunctionSpecification(tmpSpec).ResultValue.TypeIdentifier);
          end;
        end;

        if tmpSpec.Body<>nil then
           Fill(tmp,'begin',tmpSpec.Body);

        if tmpSpec is TMethodParams then
           AddParameters(tmp,ABlock,TMethodParams(tmpSpec).Parameters);

        AddDirectives(tmp,tmpSpec.Directives);

        if tmpSpec.ExternalDLL<>nil then
        begin
          tmp:=tmp.AddChildObject('external',ABlock);
          FillExpression(tmp,tmpSpec.ExternalDLL);

          if tmpSpec.ExternalName<>nil then
             FillExpression(tmp,tmpSpec.ExternalName);
        end;
      end;
    end
    else
    if ABlock is TTypeDeclarations then
    begin
      for t:=0 to TTypeDeclarations(ABlock).Count-1 do
          Fill(tmp,TTypeDeclarations(ABlock)[t].Name,TTypeDeclarations(ABlock)[t]);
    end
    else
    if ABlock is TVariableDeclarations then
    begin
      for t:=0 to TVariableDeclarations(ABlock).Count-1 do
          Fill(tmp,TVariableDeclarations(ABlock)[t].Name,TVariableDeclarations(ABlock)[t]);
    end
    else
    {
    if ABlock is TRecordSpecification then
    begin
      if TRecordSpecification(ABlock).Fields<>nil then
         Fill(tmp,'fields',TRecordDeclaration(ABlock).Fields);

      if TRecordDeclaration(ABlock).Methods<>nil then
         Fill(tmp,'methods',TRecordDeclaration(ABlock).Methods);
    end
    else
    }
    if ABlock is TVariableDeclaration then
    begin
      {
      if TVariableDeclaration(ABlock).PointerOf then
         tmp.AddChildObject('^',ABlock);
      }

      if TVariableDeclaration(ABlock).TypeIdentifier=nil then
         tmp.AddChildObject('?',ABlock)
      else
         FillType(tmp, TVariableDeclaration(ABlock).TypeIdentifier);

      if TVariableDeclaration(ABlock).Value<>nil then
         Fill(tmp,'=',TVariableDeclaration(ABlock).Value);

      tmp.AddChildObject(VisibilityToString(TVariableDeclaration(ABlock)),ABlock);
    end
    else
    if ABlock is TConstantDeclarations then
    begin
      for t:=0 to TConstantDeclarations(ABlock).Count-1 do
          Fill(tmp,TConstantDeclarations(ABlock)[t].Name,TConstantDeclarations(ABlock)[t]);
    end
    else
    if ABlock is TConstantDeclaration then
    begin
      {
      if TConstantDeclaration(ABlock).PointerOf then
         tmp.AddChildObject('^',ABlock);
      }

      if TConstantDeclaration(ABlock).TypeIdentifier<>nil then
         FillType(tmp,TConstantDeclaration(ABlock).TypeIdentifier);

      if TConstantDeclaration(ABlock).Value=nil then
      begin
        tmp.AddChildObject(TConstantDeclaration(ABlock).ToString,ABlock);
      end
      else
         Fill(tmp,'=',TConstantDeclaration(ABlock).Value);

      tmp.AddChildObject(VisibilityToString(TVariableDeclaration(ABlock)),ABlock);
    end
    else
    if ABlock is TTypeDeclaration then
    begin
      if TTypeDeclaration(ABlock).Expression=nil then
         tmp.AddChildObject(TTypeDeclaration(ABlock).Name,ABlock)
      else
         Fill(tmp,':',TTypeDeclaration(ABlock).Expression);
    end
    else
    if ABlock is TTypePointerOf then
       tmp.AddChildObject('^',ABlock)
    else
    if ABlock is TExpression then
       FillExpression(tmp,TExpression(ABlock))
    else
    if ABlock is TAssignment then
    begin
      tmp:=tmp.AddChildObject(':=',ABlock);
      Fill(tmp,'Left',TAssignment(ABlock).Left);
      Fill(tmp,'Right',TAssignment(ABlock).Right);
    end
    else
    if ABlock is TIf then
    begin
      tmp:=tmp.AddChildObject('if',ABlock);
      Fill(tmp,'Expression',TIf(ABlock).Expression);

      if TIf(ABlock).ThenPart<>nil then
      begin
        AddStatements(tmp,TIf(ABlock).ThenPart);

        if TIf(ABlock).ElsePart<>nil then
           AddStatements(tmp,TIf(ABlock).ElsePart);
      end;
    end
    else
    if ABlock is TCall then
       Fill(tmp.AddChildObject('call',ABlock),'Method',TCall(ABlock).Expression)
    else
    if ABlock is TCallExpression then
    begin
      tmp:=tmp.AddChildObject('parameters',ABlock);

      for t:=0 to High(TCallExpression(ABlock).Parameters) do
          Fill(tmp,IntToStr(t),TCallExpression(ABlock).Parameters[t]);
    end
    else
      tmp.AddChildObject(ABlock.ClassName,ABlock);
  end;

procedure FillCode(const ATree:TTree; const ABlock:TBlock);
var tmp : TTreeNodeShape;
begin
  tmp:=nil;

  if ABlock is TUnit then
  begin
    if tmp=nil then
       tmp:=ATree.AddRootObject(TUnit(ABlock).Name,ABlock);

    Fill(tmp,'interface',TUnit(ABlock).UnitInterface);
    Fill(tmp,'implementation',TUnit(ABlock).ModuleImplementation);

    if TUnit(ABlock).UnitInitialization<>nil then
       Fill(tmp,'initialization',TUnit(ABlock).UnitInitialization);

    if TUnit(ABlock).UnitFinalization<>nil then
       Fill(tmp,'finalization',TUnit(ABlock).UnitFinalization);
  end
  else
  if ABlock is TProgram then
  begin
    if tmp=nil then
       tmp:=ATree.AddRootObject(TProgram(ABlock).Name,ABlock);

    Fill(tmp,'section',TProgram(ABlock).ModuleImplementation);
    Fill(tmp,'main',TProgram(ABlock).Main);
  end
  else
  begin
    if ABlock=nil then
       Raise Exception.Create('Nil Block');

    if tmp=nil then
       tmp:=ATree.AddRootObject(ABlock.ClassName,ABlock);

    Fill(tmp,ABlock.ClassName,ABlock);
  end;
end;

procedure FillTree(AParent:TTreeNodeShape; const ANode:TBaseNode);
var tmp : TTreeNodeShape;
    S   : String;
    f   : TFunction;
    t   : Integer;
    a   : TArrayNode;
begin
  if ANode is TFunction then
     S:=TFunction(ANode).Name
  else
  if ANode is TExpressionNode then
     S:=TExpressionNode.OperatorToString(TExpressionNode(ANode).Operat)
  else
  if ANode is TArrayNode then
     S:='[]'
  else
     S:=ANode.Text;

  if AParent=nil then
     tmp:=Tree1.AddRootObject(S,ANode)
  else
     tmp:=AParent.AddChildObject(S,ANode);

  if ANode is TFunction then
  begin
    f:=TFunction(ANode);

    for t:=Low(f.Parameters) to High(f.Parameters) do
        FillTree(tmp,f.Parameters[t]);
  end
  else
  if ANode is TExpressionNode then
  begin
    FillTree(tmp,TExpressionNode(ANode).Left);

    if Assigned(TExpressionNode(ANode).Right) then
       FillTree(tmp,TExpressionNode(ANode).Right);
  end
  else
  if ANode is TArrayNode then
  begin
    a:=TArrayNode(ANode);

    for t:=Low(a.Items) to High(a.Items) do
        FillTree(tmp,a.Items[t]);
  end;
end;

procedure FillBlock(const ATree:TTree; AParent:TTreeNodeShape; const ABlock:TBlock);

  procedure AddUses(AParent:TTreeNodeShape; const AUses:TUses);
  var t : Integer;
  begin
    AParent:=AParent.AddChildObject('uses',AUses);

    for t:=0 to AUses.Count-1 do
        AParent.AddChildObject(AUses[t].Name,AUses[t]);
  end;

var S : String;
begin
  if ABlock is TIdentifier then
  begin
    S:=TIdentifier(ABlock).Name;

    if AParent=nil then
       AParent:=ATree.AddRootObject(S,ABlock)
    else
       AParent:=AParent.AddChildObject(S,ABlock);
  end;

  if ABlock is TUnit then
  begin
    AParent:=AParent.AddChildObject('interface',TUnit(ABlock).UnitInterface);
    AddUses(AParent,TUnit(ABlock).UnitInterface.UsesUnits);
  end;

  if ABlock is TImplementationModule then
  begin
    AParent:=AParent.AddChildObject('implementation',TImplementationModule(ABlock).ModuleImplementation);
    AddUses(AParent,TImplementationModule(ABlock).ModuleImplementation.UsesUnits);
  end
end;

procedure FillTreeBlock(const ABlock:TBlock);
begin
  Tree1.Clear;

  if ABlock<>nil then
  begin
    FillCode(Tree1,ABlock);

    if Tree1.Roots.Count>0 then
       Tree1.Roots[0].Expanded:=True;
  end;
end;

end.
