unit TeeCSharpEmit;

interface

uses
  Classes, TeeCode;

type
  TCSharpEmit=class(TEmit)
  private
    function AncestorsOf(const R:TRecordSpecification; const Exclude:String):String;
    function ArrayType(const A:TArraySpecification; const Indent:String):String;
    function Attributes(const ABlock:TBlock; const Indent:String):String;
    function CallExpression(const ACall:TCallExpression):String;
    function CallParameters(const P:TExpressions):String;
    function CaseVariable(const ACase:TCaseVariable; const Indent:String;
                          const OmitType:Boolean):String;
    function Constants(const AVisibility:TVisibility; const AConst:TConstantDeclarations; const Indent:String):String;
    function Directives(const AIdent:TIdentifier):String;
    function EmitASM(const S:TASMStatements; const Indent:String):String;
    function EmitCase(const C:TCase; const Indent:String):String;
    function EmitClass(const AClass:TRecordSpecification; const Indent:String;
                       IsForward:Boolean;
                       const AHelper:TTypeDeclaration;
                       const ADirectives:String):String;
    function EmitConst(const AConst:TConstantDeclaration; const Indent:String;
                       OmitType:Boolean=False):String;
    function EmitOperator(const AExp:TOperatorExpression):String;
    function EmitProgram(const AProgram:TProgram):String;
    function EmitProperty(const P:TPropertyDeclaration):String;
    function EmitRecord(const ARecord:TRecordSpecification; const Indent:String):String;
    function EmitType(const AType:TTypeDeclaration; const Indent:String):String;
    function EmitTypeSpec(const AType:TTypeDeclaration; const Indent:String):String;
    function EmitUnit(const AUnit:TUnit):String;
    function EmitUses(const AUses:TUses):String;
    function EmitUsing(const AUnit:String):String;
    function Expression(const AExp:TExpression; const AOwner:TExpression=nil):String;
    function Fields(const AVisibility:TVisibility;
                    const AFields:TVariableDeclarations;
                    const Indent:String;
                    OmitType:Boolean=False;
                    Properties:Boolean=False):String;
    function Method(const AMethod:TMethodDeclaration; Body:Boolean; const Indent:String):String;
    function MethodDirectives(const AMethod:TMethodSpecification):String;
    function Methods(const AVisibility:TVisibility; const AMethods:TMethodDeclarations;
                     Bodies:Boolean; const Indent:String):String;
    function MethodSpec(const ASpec:TMethodSpecification; const AName:String):String;
    function NameAndParams(const AType:TTypeDeclaration; Constraints:Boolean=False):String;
    function OperatorToString(const AOperator: TOperator): String;
    function Parameters(const P:TParameters; const Delimiter:String; const AsParams:Boolean=False):String;
    function Range(const ARange:TRangeExpression):String;
    function Section(const ASection:TSection; Bodies:Boolean;
                     const Indent:String):String;
    function Statement(const S:TStatement; const Indent:String):String;
    function Statements(const S:TStatements; const Indent:String; BeginEnd:Boolean=True):String;
    function TypeName(const AType:TTypeDeclaration; Constraints:Boolean=False):String;
    function TypeParams(const AParams:TTypeParameters; Constraints:Boolean=False):String;
    function Types(const AVisibility:TVisibility; const ATypes:TTypeDeclarations;
                   const Indent:String):String;
    function TypeSet(const AItems:TTypeDeclarations; const Indent:String):String;
    function TypeSpec(const AType:TTypeSpecification;
                      const IsForward:Boolean; const Indent,ADirectives:String):String;
    function UsesSection(const ASection:TUsesSection):String;
    function Value(const ABlock:TBlock):String;
    function Values(const V:TArrayValues):String;
    function Variables(const AVar:TVariableDeclarations; const Indent:String;
                       ThreadVars:Boolean=False):String;
  protected
    function Block(const ABlock:TBlock):String; override;
    function Expressions(const AItems:TExpressions):String; override;
  public
    Constructor Create; override;

    class function ModuleExtension:String; override;
  end;

  TCSharpEmitHTML=class(TCSharpEmit)
  protected
    function AnchorOf(const S:String):String; override;
    function Keyword(const S:String):String; override;
    function LinkTo(const ABlock:TBlock; const S:String):String; override;
    function StringToCode(const S:String):String; override;
    function UnitNameLink(const S:String):String; override;
  public
    Constructor Create; override;

    class function DefaultStyle:String;
    class function Page(const ABlock:TBlock):String;

    class function ModuleExtension:String; override;
  end;

implementation

uses
  SysUtils;

const
  MaxLineWidth=80;

{ TCSharpEmit }

Constructor TCSharpEmit.Create;
begin
  inherited;
  Quote:='"';
end;

function TCSharpEmit.Directives(const AIdent:TIdentifier):String;
var tmp : String;
begin
  result:='';

  {
  if AIdent.IsPlatform then
     result:=result+' '+Keyword('platform');
  }

  if AIdent.IsDeprecated then
  begin
    if AIdent.DeprecatedMessage=nil then
       tmp:='Deprecated'
    else
       tmp:=AIdent.DeprecatedMessage.ToString;

    result:='[Obsolete("'+tmp+'", false)]';
  end;

  {
  if AIdent.IsInline then
     [MethodImpl(MethodImplOptions.AggressiveInlining)]
     result:=result+' '+Keyword('inline');
  }
end;

function TCSharpEmit.Values(const V:TArrayValues):String;
var t, H: Integer;
begin
  result:='('+CRLF;

  H:=High(V.Items);

  for t:=Low(V.Items) to H do
  begin
    result:=result+Expression(V.Items[t]);

    if t<H then
       result:=result+',';

    result:=result+CRLF;
  end;

  result:=result+')'+CRLF;
end;

function TCSharpEmit.Value(const ABlock:TBlock):String;
begin
  if ABlock is TExpression then
     result:=Expression(TExpression(ABlock))
  else
     result:='?';
end;

function TCSharpEmit.TypeParams(const AParams:TTypeParameters; Constraints:Boolean=False):String;

  function TypeConstraints(const AConst:TTypeConstraints):String;
  var t,
      H : Integer;
      tmp : TTypeDeclaration;
  begin
    result:='';

    H:=High(AConst);

    for t:=Low(AConst) to H do
    begin
      tmp:=AConst[t];

      if tmp.Name='' then
         if tmp.Expression is TClassSpecification then
            result:=result+'class'
         else
         if tmp.Expression is TRecordSpecification then
            result:=result+'struct'
         else
         if tmp.Expression is TConstructorSpecification then
            //result:=result+'constructor'
         else
            result:=result+'?('+tmp.Expression.ClassName+')'
      else
         result:=result+AConst[t].Name;

      if t<H then
         result:=result+',';
    end;
  end;

var t, H : Integer;
    tmpType : TTypeDeclaration;
    tmpDelim : String;
begin
  result:=LessThan;

  H:=High(AParams);

  tmpDelim:=',';

  if Constraints then
  for t:=0 to H do
  begin
    tmpType:=TTypeDeclaration(AParams[t]);

    if tmpType is TTypeParameter then
       if TTypeParameter(tmpType).Constraints<>nil then
       begin
         tmpDelim:=';';
         break;
       end;
  end;

  for t:=0 to H do
  begin
    tmpType:=TTypeDeclaration(AParams[t]);
    result:=result+NameAndParams(tmpType);

    if Constraints and (tmpType is TTypeParameter) then
       if TTypeParameter(tmpType).Constraints<>nil then
          result:=result+':'+TypeConstraints(TTypeParameter(tmpType).Constraints);

    if t<H then
       result:=result+tmpDelim;
  end;

  result:=result+GreaterThan;
end;

function TCSharpEmit.NameAndParams(const AType:TTypeDeclaration; Constraints:Boolean=False):String;
begin
  result:=AType.Name;

  if AType.TypeParameters<>nil then
     result:=result+TypeParams(AType.TypeParameters,Constraints);
end;

function TCSharpEmit.TypeName(const AType:TTypeDeclaration; Constraints:Boolean=False):String;
begin
  if AType=nil then
     result:='?nil'
  else
  if AType is TScopedType then
     result:=Expression(TScopedType(AType).Scope)+'.'+
             TypeName(TScopedType(AType).TypeIdentifier)
  else
  if AType.Name='' then
     if AType.Expression=nil then
        result:='?'
     else
        result:=EmitTypeSpec(AType,'')
  else
  if AType.CodePage<>nil then
     result:=AType.Name+'('+Expression(AType.CodePage)+')'
  else
     result:=NameAndParams(AType,Constraints);
end;

function TCSharpEmit.ArrayType(const A:TArraySpecification; const Indent:String):String;
var t : Integer;
begin
  result:='';

  if A.Dimensions<>nil then
  begin
    result:=result+'[';

    for t:=Low(A.Dimensions) to High(A.Dimensions) do
    begin
      result:=result+Expression(A.Dimensions[t]);

      if t<High(A.Dimensions) then
         result:=result+', ';
    end;

    result:=result+']';
  end;

  result:=result+' '+Keyword('of')+' ';

  if A.Expression<>nil then // <-- ???
     result:=result+TypeName(A.Expression)
end;

function TCSharpEmit.AncestorsOf(const R:TRecordSpecification; const Exclude:String):String;
var t : Integer;
    tmp : TTypeDeclaration;
    DoExclude : Boolean;
    tmpName : String;
    tmpType : TTypeDeclaration;
begin
  result:='';

  if Length(R.Ancestors)>0 then
  begin
    tmp:=R.Ancestors[Low(R.Ancestors)];

    if (R is TClassSpecification) and
       (tmp.Expression is TInterfaceSpecification) then
    begin
      result:='TObject';
      DoExclude:=True;
    end
    else
       DoExclude:=Length(R.Ancestors)=1;

    for t:=Low(R.Ancestors) to High(R.Ancestors) do
       if (not DoExclude) or (R.Ancestors[t].Name<>Exclude) then
       begin
         if result<>'' then
            result:=result+', ';

         tmpName:=NameAndParams(R.Ancestors[t]);

         tmpType:=R.Ancestors[t].TypeIdentifier;

         if tmpName='' then
            result:=result+EmitTypeSpec(tmpType,'')
         else
            result:=result+LinkTo(tmpType,tmpName);
       end;

    if result<>'' then
       result:=' : '+result;
  end;
end;

function TCSharpEmit.TypeSet(const AItems:TTypeDeclarations; const Indent:String):String;
var tmp,
    LastWidth,
    t : Integer;
    tmpExp : TExpression;
begin
  result:='';
  LastWidth:=0;

  tmp:=AItems.Count-1;

  for t:=0 to tmp do
  begin
    result:=result+AItems[t].Name;

    if AItems[t] is TSetItem then
    begin
      tmpExp:=TSetItem(AItems[t]).Value;

      if tmpExp<>nil then
         result:=result+' = '+Value(tmpExp);
    end;

    if t<tmp then
       result:=result+', ';

    if Length(result)-LastWidth>MaxLineWidth then
    begin
      LastWidth:=Length(result);
      result:=result+CRLF+Indent;
    end;
  end;
end;

function TCSharpEmit.Parameters(const P:TParameters; const Delimiter:String; const AsParams:Boolean=False):String;
var t,H : Integer;
    tmp : TParameter;
    tmpV : TVariableDeclaration;
begin
  result:='';

  H:=P.Count-1;

  for t:=0 to H do
  begin
    tmpV:=P[t];

    if (Delimiter=',') and (not AsParams) then
       result:=result+Value(tmpV.Value)
    else
    begin
      if tmpV is TParameter then
      begin
        tmp:=TParameter(tmpV);

        if tmp.IsConst then
           //result:=result+Keyword('const')+' '
        else
        if tmp.IsVar then
           result:=result+Keyword('ref')+' '
        else
        if tmp.IsOut then
           result:=result+Keyword('out')+' ';
      end;

      result:=result+EmitConst(tmpV,'');
    end;

    if t<H then
       result:=result+Delimiter+' ';
  end;
end;

function TCSharpEmit.MethodSpec(const ASpec:TMethodSpecification; const AName:String):String;
var tmpType : TTypeDeclaration;
begin
  if ASpec is TRecordOperatorSpec then
     result:='operator'
  else
  if ASpec is TFunctionSpecification then
     result:=''
  else
  if ASpec is TProcedureSpecification then
     result:='void'
  else
  if ASpec is TConstructorSpecification then
     result:='constructor'
  else
     result:='destructor';

  result:=Keyword(result);

  if ASpec.IsClass then
     result:=Keyword('static')+' '+result;

  if ASpec is TFunctionSpecification then
     if TFunctionSpecification(ASpec).ResultValue=nil then
        result:=result+'?'
     else
     begin
       tmpType:=TFunctionSpecification(ASpec).ResultValue.TypeIdentifier;
       result:=result+LinkTo(tmpType,TypeName(tmpType));
     end;

  if AName<>'' then
     result:=result+' '+AName;

  if ASpec.TypeParameters<>nil then
     result:=result+TypeParams(ASpec.TypeParameters);

  result:=result+'(';

  if (ASpec is TMethodParams) and TMethodParams(ASpec).HasParameters then
     result:=result+Parameters(TMethodParams(ASpec).Parameters,',');

  result:=result+')';

  if ASpec.OfObject then
     result:=result+' '+Keyword('of object');
end;

class function TCSharpEmit.ModuleExtension: String;
begin
  result:='.cs';
end;

function TCSharpEmit.Range(const ARange:TRangeExpression):String;
begin
  result:=Expression(ARange.StartRange);

  if ARange.EndRange<>nil then
     result:=result+'..'+Expression(ARange.EndRange);
end;

function TCSharpEmit.EmitClass(const AClass:TRecordSpecification; const Indent:String; IsForward:Boolean;
                   const AHelper:TTypeDeclaration;
                   const ADirectives:String):String;

  function SpaceAndDirectives:String;
  begin
    if ADirectives='' then
       result:=''
    else
       result:=' '+ADirectives;
  end;

var tmp : String;
begin
  if AClass.IsPacked then
     result:='[StructLayout(LayoutKind.Sequential)]'
  else
     result:='';

  if not IsForward then
  begin
    if AClass is TClassSpecification then
    begin
      if TClassSpecification(AClass).IsSealed then
         result:=result+' '+Keyword('sealed')
      else
      if TClassSpecification(AClass).IsAbstract then
         result:=result+' '+Keyword('abstract');

      result:=result+Keyword('class');
    end;

    if AHelper<>nil then
       result:=result+' '+Keyword('helper for')+' '+TypeName(AHelper)
    else
    if Length(AClass.Ancestors)>0 then
       result:=result+AncestorsOf(AClass,'TObject');

    tmp:=EmitRecord(AClass,Indent);

    if tmp='' then
       if ADirectives='' then
       begin
         if IsForward then
            result:=result+CRLF
         else
            result:=result+IndentText+Keyword('}')+CRLF
       end
       else
          result:=result+IndentText+Keyword('}')+SpaceAndDirectives
    else
       result:=result+CRLF+tmp+Indent+IndentText+Keyword('}')+SpaceAndDirectives;
  end;
end;

function TCSharpEmit.MethodDirectives(const AMethod:TMethodSpecification):String;

  procedure Add(const S:String);
  begin
    if result='' then
       result:=' '+Keyword(S)
    else
       result:=result+'; '+Keyword(S);
  end;

var tmp : TMethodDirectives;
begin
  result:='';

  tmp:=AMethod.Directives;

  // Order here is important:

  if mdReintroduce in tmp then Add('reintroduce');
  if mdOverload in tmp then Add('overload');
  if mdVirtual in tmp then Add('virtual');
  if mdAssembler in tmp then Add('assembler');
  if mdCDecl in tmp then Add('cdecl');
  if mdDynamic in tmp then Add('dynamic');
  if mdExport in tmp then Add('export');
  if mdFar in tmp then Add('far');
  if mdFinal in tmp then Add('final');
  if mdForward in tmp then Add('forward');
  if mdInline in tmp then Add('inline');
  if mdLocal in tmp then Add('local');
  if mdNear in tmp then Add('near');
  if mdOverride in tmp then Add('override');
  if mdPascal in tmp then Add('pascal');
  if mdRegister in tmp then Add('register');
  if mdSafeCall in tmp then Add('safecall');
  if mdStdCall in tmp then Add('stdcall');
  if mdUnsafe in tmp then Add('unsafe');
  if mdStatic in tmp then Add('static');
  if mdAbstract in tmp then Add('abstract');
  if mdPlatform in tmp then Add('platform');

  if mdDeprecated in tmp then
  begin
    Add('deprecated');

    if AMethod.DeprecatedMessage<>nil then
       result:=result+' '+Value(AMethod.DeprecatedMessage);
  end;

  if mdVarArgs in tmp then Add('varargs'); // <-- VER210

  if AMethod.ExternalDLL<>nil then
  begin
    result:=result+' '+Keyword('external')+' '+Expression(AMethod.ExternalDLL);

    if AMethod.ExternalName<>nil then
       result:=result+' '+Keyword('name')+' '+Expression(AMethod.ExternalName);
  end;

  if AMethod.MessageField<>nil then
     result:=result+' '+Keyword('message')+' '+Expression(AMethod.MessageField);
end;

function TCSharpEmit.Attributes(const ABlock:TBlock; const Indent:String):String;
var t : Integer;
    tmp : TAttribute;
    tmpS : String;
begin
  result:='';

  for t:=0 to High(ABlock.Attributes) do
  begin
    tmp:=ABlock.Attributes[t];

    (*
    if tmp is TDirective then
       tmpS:='{$'+tmp.Text+'}'
    else
    *)
       tmpS:='['+tmp.Text+']';

    result:=result+Indent+tmpS+CRLF;
  end;
end;

function TCSharpEmit.TypeSpec(const AType:TTypeSpecification;
                              const IsForward:Boolean;
                              const Indent,ADirectives:String):String;
var tmp : TTypeDeclaration;
    tmpSpec : TTypeSpecification;
begin
  tmpSpec:=AType;

  if tmpSpec is TTypePointerOf then
  begin
    result:='^';

    tmp:=TTypePointerOf(tmpSpec).TypeIdentifier;

    if tmp=nil then
       result:=result+'?'
    else
       result:=result+tmp.Name;
  end
  else
  if tmpSpec is TArraySpecification then
     result:=Keyword('array')+ArrayType(TArraySpecification(tmpSpec),Indent)
  else
  if tmpSpec is TClassOf then
  begin
    if TClassOf(tmpSpec).IsPacked then
       result:='[StructLayout(LayoutKind.Sequential)]'+CRLF
    else
       result:='';

    result:=result+' '+Keyword('class of')+' '+TClassOf(tmpSpec).TypeIdentifier.Name;
  end
  else
  if tmpSpec is TClassHelperSpecification then
  begin
    result:=EmitClass(TClassHelperSpecification(tmpSpec),Indent,IsForward,
                     TClassHelperSpecification(tmpSpec).TypeIdentifier,
                     ADirectives);
    Exit;
  end
  else
  if tmpSpec is TClassSpecification then
  begin
    result:=EmitClass(TClassSpecification(tmpSpec),Indent,
                      IsForward,nil,ADirectives);
    Exit;
  end
  else
  if tmpSpec is TInterfaceSpecification then
  begin
    result:=Keyword('interface');

    if not IsForward then
    begin
      if Length(TInterfaceSpecification(tmpSpec).Ancestors)>0 then
         result:=result+AncestorsOf(TInterfaceSpecification(tmpSpec),'IInterface');

      result:=result+CRLF+Attributes(tmpSpec,Indent)+
              EmitRecord(TInterfaceSpecification(tmpSpec),Indent)+Indent+IndentText+Keyword('}');
    end;
  end
  else
  if tmpSpec is TRecordSpecification then
  begin
    if tmpSpec=VarRecSpec then
       result:=Keyword('const')
    else
    begin
      if TRecordSpecification(tmpSpec).IsPacked then
         result:='[StructLayout(LayoutKind.Sequential)]'+CRLF
      else
         result:='';

      result:=result+Keyword('struct');

      if tmpSpec is TRecordHelperSpecification then
         result:=result+' '+Keyword('helper for')+' '+
             TypeName(TRecordHelperSpecification(tmpSpec).TypeIdentifier);

      result:=result+CRLF+EmitRecord(TRecordSpecification(tmpSpec),Indent)+
              Indent+IndentText+Keyword('}');
    end;
  end
  else
  if tmpSpec is TSetSpecification then
     result:='('+TypeSet(TSetSpecification(tmpSpec).Items,Indent)+')'
  else
  if tmpSpec is TMethodSpecification then
     result:=MethodSpec(TMethodSpecification(tmpSpec),'')+
                    MethodDirectives(TMethodSpecification(tmpSpec))
  else
  if tmpSpec is TTypeTypeOf then
     result:=Keyword('type')+' '+TypeName(TTypeTypeOf(tmpSpec).TypeIdentifier)
  else
  if tmpSpec is TSetOfSpecification then
  begin
    result:=Keyword('set of')+' ';

    if TSetOfSpecification(tmpSpec).TypeIdentifier.Name='' then
       result:=result+EmitTypeSpec(TSetOfSpecification(tmpSpec).TypeIdentifier,'')
    else
       result:=result+TSetOfSpecification(tmpSpec).TypeIdentifier.Name;
  end
  else
  if tmpSpec is TSetRange then
     result:=Keyword('set of')+' '+Range(TSetRange(tmpSpec).Range)
  else
  if tmpSpec is TTypeRange then
     result:=Range(TTypeRange(tmpSpec).Range)
  else
  if tmpSpec is TReferenceSpecification then
     result:=Keyword('reference to')+' '+
             MethodSpec(TReferenceSpecification(tmpSpec).Method,'')
  else
  if tmpSpec is TTypeItem then
     result:=Expression(TTypeItem(tmpSpec).Item)
  else
     result:=' ?';

  if ADirectives<>'' then
     result:=result+' '+ADirectives;
end;

function TCSharpEmit.EmitType(const AType:TTypeDeclaration; const Indent:String):String;
begin
  result:=Attributes(AType,Indent)+AnchorOf(TypeName(AType,True))+' = ';

  if AType.Expression=nil then
     result:=result+' ?'
  else
  if AType.Alias=nil then
     result:=result+EmitTypeSpec(AType,Indent)+';'
  else
     result:=result+TypeName(AType.Alias)+Directives(AType)+';';
end;

function TCSharpEmit.EmitTypeSpec(const AType: TTypeDeclaration;
  const Indent: String): String;
begin
  result:=TypeSpec(AType.Expression,AType.IsForward,Indent,Directives(AType));
end;

function TCSharpEmit.EmitConst(const AConst:TConstantDeclaration; const Indent:String;
                   OmitType:Boolean=False):String;
begin
  if OmitType and (AConst.Value=nil) then
     result:=''
  else
  begin
    result:=AnchorOf(AConst.Name);

    if (not AConst.AutomaticType) and
       (AConst.TypeIdentifier<>nil) and
       (AConst.TypeIdentifier<>AnyType) then
    begin
      if result<>'' then
         result:=result+': ';

      if not OmitType then
         result:=result+LinkTo(AConst.TypeIdentifier,TypeName(AConst.TypeIdentifier));
    end
    else
    if AConst.NeedsType then
       result:=result+' ? ';

    if AConst.Value=nil then
    begin
      if not AConst.OptionalValue then
         result:=result+' = ?';
    end
    else
    begin
      if not OmitType then
         result:=result+' = ';

      result:=result+Value(AConst.Value)+Directives(AConst);
    end;
  end;
end;

function TCSharpEmit.Variables(const AVar:TVariableDeclarations; const Indent:String;
                               ThreadVars:Boolean=False):String;
var t : Integer;
    tmp : TConstantDeclaration;
begin
  result:='';

  for t:=0 to AVar.Count-1 do
  begin
    tmp:=AVar[t];

    if (not ThreadVars) or (tmp is TThreadVariable) then
    begin
      if (t<AVar.Count-1) and
         (tmp.TypeIdentifier=AVar[t+1].TypeIdentifier) then
         result:=result+tmp.Name+', '
      else
         result:=result+Indent+EmitConst(AVar[t],Indent)+';'+CRLF;
    end;
  end;
end;

function TCSharpEmit.CallParameters(const P:TExpressions):String;
var H,t : Integer;
begin
  result:='';

  H:=High(P);

  for t:=Low(P) to H do
  begin
    result:=result+Expression(P[t]);

    if t<H then
       result:=result+', ';
  end;
end;

function TCSharpEmit.EmitCase(const C:TCase; const Indent:String):String;

  function EmitItem(const I,Next:TCaseItem; const Indent:String):String;
  begin
    result:=Indent+Expression(I.Condition);

    if (Next=nil) or (I.Body<>Next.Body) then
    begin
      result:=result+': ';

      if I.Body<>nil then
         result:=result+Statements(I.Body,Indent+IndentText);

      result:=result+';';
    end
    else
      result:=result+',';
  end;

var t,H : Integer;
    Next : TCaseItem;
begin
  result:=Indent+Keyword('switch')+' '+Expression(C.Expression)+' '+
          Keyword('of')+' '+CRLF;

  H:=High(C.Cases);

  for t:=Low(C.Cases) to H do
      if C.Cases[t]=nil then
         result:=result+'?nil'+CRLF
      else
      begin
        if t=H then
           Next:=nil
        else
           Next:=C.Cases[t+1];

        result:=result+EmitItem(C.Cases[t],Next,Indent+IndentText)+CRLF;
      end;

  if C.ElsePart<>nil then
     result:=result+Indent+Keyword('else')+CRLF+Statements(C.ElsePart,IndentText)+CRLF;

  result:=result+Indent+Keyword('}');
end;

function TCSharpEmit.Expressions(const AItems:TExpressions):String;
var t, H : Integer;
begin
  result:='';

  H:=High(AItems);

  for t:=Low(AItems) to H do
  begin
    result:=result+Expression(AItems[t]);

    if t<H then
       result:=result+', ';
  end;
end;

function TCSharpEmit.EmitASM(const S:TASMStatements; const Indent:String):String;
var t : Integer;
begin
  result:=Indent+Keyword('asm')+CRLF;

  for t:=Low(S.Items) to High(S.Items) do
      result:=result+Indent+IndentText+S.Items[t]+CRLF;

  result:=result+Indent+Keyword('}');
end;

function TCSharpEmit.Statement(const S:TStatement; const Indent:String):String;

  function IteratorName(const AIterator:TVariableDeclaration):String;
  begin
    if AIterator=nil then
       result:='?(nil)'
    else
    if (AIterator.Owner is TFunctionSpecification) and
       (AIterator=TFunctionSpecification(AIterator.Owner).ResultValue) then
       result:='return'
    else
       result:=AIterator.Name;
  end;

begin
  if S is TAssignment then
     result:=Expression(TAssignment(S).Left)+' = '+Expression(TAssignment(S).Right)
  else
  if S is TCall then
     result:=Expression(TCall(S).Expression)
  else
  if S is TIf then
  begin
    result:=Keyword('if')+' ('+Expression(TIf(S).Expression)+') '+{Keyword('then')+}CRLF+
            Statements(TIf(S).ThenPart,Indent+' ');

    if TIf(S).ElsePart<>nil then
       result:=result+CRLF+Indent+Keyword('else')+CRLF+Statements(TIf(S).ElsePart,Indent);
  end
  else
  if S is TRepeat then
  begin
    result:=Keyword('repeat')+CRLF+Statements(TRepeat(S).RepeatPart,Indent+IndentText)+CRLF+
            Keyword('until')+' ('+Expression(TRepeat(S).Expression)+')';
  end
  else
  if S is TWhile then
  begin
    result:=Keyword('while')+' ('+Expression(TWhile(S).Expression)+') '+
            Keyword('do')+' '+CRLF+
            Statements(TWhile(S).WhilePart,IndentText);
  end
  else
  if S is TWith then
  begin
    result:=Keyword('using')+' ('+Expressions(TWith(S).Items)+') '+
            Keyword('do')+CRLF+
            Statements(TWith(S).WithPart,IndentText);
  end
  else
  if S is TFor then
  begin
    result:=Keyword('for')+' ('+IteratorName(TFor(S).Iterator);

    if TFor(s).Enumerated=nil then
    begin
      result:=result+' := '+Expression(TFor(S).Start);

      if TFor(S).ToValue=nil then
         result:=result+' '+Keyword('downto')+' '+Expression(TFor(S).DownToValue)
      else
         result:=result+' '+Keyword('to')+' '+Expression(TFor(S).ToValue);
    end
    else
      result:=result+' '+Keyword('in')+' '+Expression(TFor(s).Enumerated);

    result:=result+') '+Keyword('do')+CRLF+Statements(TFor(S).Loop,IndentText);
  end
  else
  if S is TTry then
  begin
    result:=Keyword('try')+CRLF+Statements(TTry(S).Block,IndentText)+CRLF;

    if TTry(S).FinallyPart<>nil then
       result:=result+IndentText+Keyword('finally')+CRLF+Statements(TTry(S).FinallyPart,IndentText)+CRLF;

    if TTry(S).ExceptPart<>nil then
       result:=result+IndentText+Keyword('except')+CRLF+Statements(TTry(S).ExceptPart,IndentText)+CRLF;

    if TTry(S).ElsePart<>nil then
       result:=result+IndentText+Keyword('else')+CRLF+Statements(TTry(S).ElsePart,IndentText)+CRLF;

    result:=result+IndentText+Keyword('}');
  end
  else
  if S is TCase then
     result:=EmitCase(TCase(S),Indent)
  else
  if S is TRaise then
  begin
    result:=Keyword('raise');

    if TRaise(S).Expression<>nil then
       result:=result+' '+Expression(TRaise(S).Expression);
  end
  else
  { No equivalent
  if S is TGoto then
     result:=Keyword('goto')+' '+TGoto(S).TargetLabel.Name
  else
  }
  if S is TOn then
  begin
    result:=Keyword('on')+' '+EmitConst(TOn(S).Exception,Indent)+' '+
       Keyword('do')+CRLF+
       Indent+Statements(TOn(S).DoPart,Indent);

    if TOn(S).ElsePart<>nil then
       result:=result+CRLF+Indent+Keyword('else')+' '+Statements(TOn(S).ElsePart,Indent);
  end
  else
  if S is TASMStatements then
     result:=EmitASM(TASMStatements(S),Indent)
  else
  if S is TStatements then
     result:=Statements(TStatements(S),Indent)
  else
  if S is TLabelPlace then
     result:=TLabelPlace(S).LabelIdentifier.Name+':'
  else
     result:='? '+S.ClassName;
end;

function TCSharpEmit.Method(const AMethod:TMethodDeclaration; Body:Boolean; const Indent:String):String;

  function FullMethodName(ARecord:TTypeDeclaration):String;
  var tmpSpec : TRecordSpecification;
  begin
    result:=LinkTo(AMethod,AMethod.Name);

    repeat
      if ARecord<>nil then
      begin
        result:=LinkTo(ARecord,NameAndParams(ARecord))+'.'+result;

        tmpSpec:=TRecordSpecification(ARecord.Expression).Outer;

        if tmpSpec=nil then
           ARecord:=nil
        else
           ARecord:=tmpSpec.RecordType;
      end;

    until ARecord=nil;
  end;

var tmpSpec : TMethodSpecification;
    tmpName : String;
    tmpDir : String;
    tmpAnonymous : Boolean;
begin
  tmpAnonymous:=AMethod.TypeIdentifier.Expression is TReferenceSpecification;

  if tmpAnonymous then
     tmpSpec:=TReferenceSpecification(AMethod.TypeIdentifier.Expression).Method
  else
     tmpSpec:=TMethodSpecification(AMethod.TypeIdentifier.Expression);

  if tmpSpec=nil then
  begin
    result:='?';
    Exit;
  end;

  if Body then
     tmpName:=FullMethodName(tmpSpec.OfRecord)
  else
     tmpName:=AnchorOf(AMethod.Name);

  result:=Indent+MethodSpec(tmpSpec,tmpName);

  if (tmpSpec.OfRecord=nil) or (not Body) then
  begin
    tmpDir:=MethodDirectives(tmpSpec);

    if tmpDir<>'' then
       result:=result+tmpDir+';';
  end;

  if Body and (tmpSpec.Body<>nil) then
     result:=result+CRLF+Section(tmpSpec.Body,Body,Indent+IndentText)+
             Statements(tmpSpec.Body.Statements,Indent)+CRLF;
end;

function TCSharpEmit.Methods(const AVisibility:TVisibility;
                             const AMethods:TMethodDeclarations;
                             Bodies:Boolean; const Indent:String):String;
var t : Integer;
    tmp : TMethodDeclaration;
begin
  result:='';

  for t:=0 to AMethods.Count-1 do
  begin
    tmp:=AMethods[t] as TMethodDeclaration;

    if tmp.Visibility=AVisibility then
       result:=result+Indent+Method(tmp,Bodies,Indent)+CRLF;
  end;
end;

function TCSharpEmit.EmitProperty(const P:TPropertyDeclaration):String;

  procedure Add(const AKeyword:String; const AExp:TExpression);
  begin
    if AExp<>nil then
       result:=result+' '+Keyword(AKeyword)+' '+Expression(AExp);
  end;

begin
  if P.IsClass then
     result:='class '
  else
     result:='';

  result:=result+Keyword('property')+' '+P.Name;

  // No type for republished properties:

  if (P.ReadPart<>nil) or (P.WritePart<>nil) then
  begin
    if P.IsIndexed then
       result:=result+'['+Parameters(P.Indexes,';',True)+']';

    result:=result+' : ';

    if P.PropertyType=nil then
       result:=result+'?'
    else
       result:=result+NameAndParams(P.PropertyType);
  end;

  Add('index',P.Index);
  Add('read',P.ReadPart);
  Add('write',P.WritePart);
  Add('stored',P.Stored);

  if P.IsDefault then
     result:=result+'; '+Keyword('default');

  if P.NoDefault then
     result:=result+' '+Keyword('nodefault');

  result:=result+';';
end;

function TCSharpEmit.CaseVariable(const ACase:TCaseVariable; const Indent:String;
                          const OmitType:Boolean):String;
var t : Integer;
    tmp : TVariableDeclarations;
    tmpItem : TCaseVariableItem;
    tmpS : String;
begin
  if OmitType then
     result:=''
  else
     result:=Indent+Keyword('case')+' '+EmitConst(ACase,'')+' '+Keyword('of')+CRLF;

  tmp:=TVariableDeclarations(ACase.Fields);

  for t:=0 to tmp.Count-1 do
  begin
    tmpItem:=TCaseVariableItem(tmp.Item[t]);

    if not OmitType then
       result:=result+Indent+IndentText+Expression(tmpItem.Expression)+': ';

    tmpS:=Fields(vPublic,TVariableDeclarations(tmpItem.Value),'',OmitType,False);

    if (not OmitType) or (tmpS<>'') then
    begin
      result:=result+' ('+tmpS+')';

      if not OmitType then
         result:=result+';'+CRLF;
    end;
  end;
end;

function TCSharpEmit.Fields(const AVisibility:TVisibility;
                   const AFields:TVariableDeclarations;
                   const Indent:String;
                   OmitType:Boolean=False;
                   Properties:Boolean=False):String;
var t : Integer;
    tmp : TConstantDeclaration;
    tmpS : String;
begin
  result:='';

  for t:=0 to AFields.Count-1 do
  begin
    tmp:=AFields[t];

    if tmp.Visibility=AVisibility then
       if Properties then
       begin
         if tmp is TPropertyDeclaration then
            result:=result+Indent+IndentText+EmitProperty(TPropertyDeclaration(tmp))+CRLF;
       end
       else
       if not (tmp is TPropertyDeclaration) then
          if tmp is TCaseVariable then
             result:=result+CaseVariable(TCaseVariable(tmp),Indent+IndentText,OmitType)+CRLF
          else
          begin
            tmpS:=EmitConst(tmp,'',OmitType);

            if tmpS<>'' then
               result:=result+Indent+IndentText+tmpS+';'+CRLF;
          end;
  end;
end;

function VisibilityToString(const AVisibility:TVisibility):String;
begin
  case AVisibility of
          vPublic: result:='public';
   vStrictPrivate: result:='strict private';
         vPrivate: result:='private';
 vStrictProtected: result:='strict protected';
       vProtected: result:='protected';
       vPublished: result:='published';

       // "internal"
       // "friends" ??
  end;
end;

function TCSharpEmit.Constants(const AVisibility:TVisibility; const AConst:TConstantDeclarations; const Indent:String):String;
var t : Integer;
    tmp : TConstantDeclaration;
begin
  result:='';

  for t:=0 to AConst.Count-1 do
  begin
    tmp:=AConst[t];

    if tmp.Visibility=AVisibility then
       result:=result+Indent+IndentText+EmitConst(tmp,'')+';'+CRLF;
  end;
end;

function TCSharpEmit.EmitRecord(const ARecord:TRecordSpecification; const Indent:String):String;
var S,
    tmp : String;
    t : Integer;
    tmpIdent : TBlock;
    NewClass,
    OldClass : TClass;
begin
  result:='';

  OldClass:=nil;

  for t:=0 to High(ARecord.Ordered) do
  begin
    tmpIdent:=ARecord.Ordered[t];

    if tmpIdent is TDirective then
       result:=result+'{$'+TDirective(tmpIdent).Text+'}'+CRLF
    else
    if tmpIdent is TVisibilityIdent then
    begin
      result:=result+Indent+IndentText+Keyword(VisibilityToString(TVisibilityIdent(tmpIdent).Visibility))+CRLF;

      OldClass:=nil;
    end
    else
    begin

      if tmpIdent.ClassType<>OldClass then
      begin
        NewClass:=tmpIdent.ClassType;

        if NewClass<>TMethodDeclaration then
        begin
          if NewClass=TThreadVariable then
             S:='threadvar'
          else
          if NewClass=TVariableDeclaration then
             if TVariableDeclaration(tmpIdent).IsClass then
                S:='class var'
             else
             if (OldClass<>nil) and (OldClass<>TVariableDeclaration) then
                S:='var'
             else
                S:=''
          else
          if NewClass=TConstantDeclaration then
             if TConstantDeclaration(tmpIdent).IsClass then
                S:='class const'
             else
                S:='const'
          else
          if NewClass=TTypeDeclaration then
             S:='type'
          else
          if (NewClass=TPropertyDeclaration) or (NewClass=TCaseVariable) then
             S:=''
          else
             S:='?';

          if S<>'' then
             result:=result+Indent+IndentText+Keyword(S)+CRLF;
        end;

        OldClass:=NewClass;
      end;

      if tmpIdent is TMethodDeclaration then
         tmp:=IndentText+Method(TMethodDeclaration(tmpIdent),False,Indent+IndentText)
      else
      if tmpIdent is TTypeDeclaration then
         tmp:=Indent+IndentText+IndentText+EmitType(TTypeDeclaration(tmpIdent),Indent+IndentText)
      else
      if tmpIdent is TPropertyDeclaration then
         tmp:=Indent+IndentText+IndentText+EmitProperty(TPropertyDeclaration(tmpIdent))
      else
      if tmpIdent is TCaseVariable then
         tmp:=Indent+IndentText+CaseVariable(TCaseVariable(tmpIdent),Indent+IndentText,False)
      else
      if tmpIdent is TConstantDeclaration then
         tmp:=Indent+IndentText+IndentText+EmitConst(TConstantDeclaration(tmpIdent),Indent+IndentText)+';'
      else
         tmp:='?('+tmpIdent.ClassName+')';

      result:=result+tmp+CRLF;
    end;
  end;
end;

function TCSharpEmit.OperatorToString(const AOperator: TOperator): String;
begin
  case AOperator of
     opEqual: result:='==';
  opNotEqual: result:='!=';
       opAnd: result:=Keyword('and');
        opOr: result:=Keyword('or');
       opXor: result:=Keyword('xor');
       opShl: result:=Keyword('<<');
       opShr: result:=Keyword('>>');
     opLower: result:=LessThan;
   opGreater: result:=GreaterThan;
opLowerEqual: result:=LessThan+'=';
       opAdd: result:='+';
  opSubtract: result:='-';
  opMultiply: result:='*';
    opDivide: result:='/';
       opNot: result:=Keyword('!');
       opDiv: result:=Keyword('/');
       opMod: result:=Keyword('%');
        opIn: result:=Keyword('in');
        opIs: result:=Keyword('is');
        opAs: result:=Keyword('as');
        opAt: result:=Keyword('at');
  else
     result:=GreaterThan+'=';
  end;
end;

function TCSharpEmit.CallExpression(const ACall:TCallExpression):String;
begin
  result:=Expression(ACall.Expression);

  if ACall.Parameters<>nil then
     result:=result+'('+CallParameters(ACall.Parameters)+')';
end;

function TCSharpEmit.EmitOperator(const AExp:TOperatorExpression):String;

  function EmitPart(const AExp:TExpression):String;
  begin
    if AExp=nil then
       result:='?'
    else
       result:=Expression(AExp);
  end;

var tmpLeft,
    tmpOp,
    tmpRight : String;
begin
  tmpLeft:=EmitPart(AExp.Left);
  tmpRight:=EmitPart(AExp.Right);
  tmpOp:=OperatorToString(AExp.Operat);

  if AExp.Operat=opNot then
     result:=' '+Keyword('not')+' '+tmpLeft
  else
  if AExp.Left=nil then
     result:=' '+tmpOp+' '+tmpRight
  else
  begin
    // Trick: Block exponent floats as: "1E-5" instead of: "1E - 5"

    if (AExp.Left is TFloatNumber) and
       TFloatNumber(AExp.Left).Exponent then
       result:=tmpLeft+tmpOp+tmpRight
    else
    begin
      if Length(tmpRight)>MaxLineWidth then
         tmpOp:=tmpOp+CRLF;

      result:=tmpLeft+' '+tmpOp+' '+tmpRight;
    end;
  end;
end;

function TCSharpEmit.Expression(const AExp:TExpression; const AOwner:TExpression=nil):String;

  function EmitConstName(const AConst:TConstantDeclaration):String;
  begin
    if AConst.TypeIdentifier=nil then
       result:=AConst.Name+' : ?'
    else
    if AConst.Name='' then
       result:=AConst.ToString
    else
       result:=LinkTo(AConst,AConst.Name);
  end;

  function IsAnonymousMethod:Boolean;
  begin
    result:=(AExp is TMethodDeclaration) and (AExp.TypeIdentifier.Expression is TReferenceSpecification);
  end;

var tmpS : String;
begin
  if AExp=nil then
     result:='?'
  else
  if AExp is TArrayValues then
     result:=Values(TArrayValues(AExp))
  else
  if AExp is TCallExpression then
     result:=CallExpression(TCallExpression(AExp))
  else
  if AExp is TFieldExpression then
  begin
    if (AOwner=nil) or (TFieldExpression(AExp).Value<>AOwner) then
       result:=Expression(TFieldExpression(AExp).Value)+'.'
    else
       result:='';

    result:=result+Expression(TFieldExpression(AExp).Field);
  end
  else
  if AExp is TItemExpression then
     result:=Expression(TItemExpression(AExp).Value)+'['+
             Expressions(TItemExpression(AExp).Items)+']'
  else
  if AExp is TDereferenceExpression then
     result:=Expression(TDereferenceExpression(AExp).Value)+'^'
  else
  if AExp is TRangeExpression then
     result:=Range(TRangeExpression(AExp))
  else
  if AExp is TNestedExpression then
     result:='('+Expression(TNestedExpression(AExp).Expression)+')'
  else
  if AExp is TOperatorExpression then
     result:=EmitOperator(TOperatorExpression(AExp))
  else
  if AExp is TChar then
     result:=StringToCode(AExp.ToString)
  else
  if AExp is TString then
     result:=StringToCode(AExp.ToString)
  else
  if AExp is TCasting then
     result:=LinkTo(TCasting(AExp).TypeIdentifier,TypeName(TCasting(AExp).TypeIdentifier))+'('+
               Expression(TCasting(AExp).Expression)+')'
  else
  if (AExp.Owner is TFunctionSpecification) and (AExp=TFunctionSpecification(AExp.Owner).ResultValue) then
     result:='return'
  else
  if AExp is TRecordInstance then
  begin
    // How to avoid checkin nested () ?
    tmpS:=Fields(vPublic,TRecordInstance(AExp).Fields,'',True);

    if Copy(Trim(tmpS),1,1)='(' then
       result:=tmpS
    else
       result:='('+tmpS+')';
  end
  else
  // Before TConstant
  if AExp is TAddressOf then
     result:='@'+Value(TAddressOf(AExp).Value)
  else
  if IsAnonymousMethod then // Before "is TConstant"
     result:=Method(TMethodDeclaration(AExp),True,'')
  else
  if AExp is TMethodDeclaration then
  begin
     result:=EmitConstName(TMethodDeclaration(AExp));

     if TMethodDeclaration(AExp).TypeIdentifier.TypeParameters<>nil then
        result:=result+TypeParams(AExp.TypeIdentifier.TypeParameters);
  end
  else
  if AExp is TConstantDeclaration then
     result:=EmitConstName(TConstantDeclaration(AExp))
  else
  if AExp is TInheritedExpression then
  begin
    result:=Keyword('base')+'.';

    if TInheritedExpression(AExp).Expression<>nil then
       result:=result+' '+Expression(TInheritedExpression(AExp).Expression);
  end
  else
  if AExp is TGenericTypeDeclaration then
     result:=LinkTo(AExp,TGenericTypeDeclaration(AExp).Name)+TypeParams(TGenericTypeDeclaration(AExp).TypeParameters)
  else
  if AExp is TTypeDeclaration then
     result:=LinkTo(AExp,TTypeDeclaration(AExp).Name)
  else
  if AExp is TTypedMethod then
     result:=Expression(TMethodDeclaration(TTypedMethod(AExp).Method))+
             TypeParams(TTypedMethod(AExp).TypeParameters)
  else
  if AExp is TGenericMethod then
     result:=Expression(TMethodDeclaration(TGenericMethod(AExp).Method))
  else
  if AExp is TExportDeclaration then
     result:=LinkTo(AExp,Expression(TExportDeclaration(AExp).Method))
  else
     result:='?'+AExp.ClassName;
end;

function TCSharpEmit.Statements(const S:TStatements; const Indent:String; BeginEnd:Boolean=True):String;

  function DoEmit(const ABlock:TBlock):String;
  begin
    if ABlock is TStatement then
       result:=Indent+IndentText+Statement(TStatement(ABlock),Indent)
    else
    if ABlock is TStatements then
       result:=Indent+IndentText+Statements(TStatements(ABlock),Indent)
    else
       raise ECodeException.Create('Invalid statement: '+ABlock.ClassName);
  end;

var t,H : Integer;
begin
  if S is TASMStatements then
     result:=EmitASM(TASMStatements(S),Indent)+CRLF
  else
  if S<>nil then
  begin
    if BeginEnd and S.BeginEnd then
       result:=Indent+Keyword('{')+CRLF
    else
       result:='';

    H:=Length(S.Items);

    if H>0 then
       if H=1 then
       begin
         result:=result+DoEmit(S.Items[0]);

         if BeginEnd and S.BeginEnd then
            result:=result+CRLF;
       end
       else
          for t:=Low(S.Items) to High(S.Items) do
              result:=result+DoEmit(S.Items[t])+';'+CRLF;

    if BeginEnd and S.BeginEnd then
       result:=result+Indent+Keyword('}');
  end;
end;

function TCSharpEmit.Types(const AVisibility:TVisibility; const ATypes:TTypeDeclarations;
                           const Indent:String):String;
var t : Integer;
    tmp : TTypeDeclaration;
begin
  result:='';

  for t:=0 to ATypes.Count-1 do
  begin
    tmp:=ATypes[t];

    if tmp.Visibility=AVisibility then
       result:=result+Indent+IndentText+EmitType(tmp,'')+CRLF+CRLF;
  end;
end;

function TCSharpEmit.Section(const ASection:TSection; Bodies:Boolean;
                             const Indent:String):String;
var S,
    tmp : String;
    t : Integer;
    tmpIdent : TBlock;
    OldClass : TClass;
begin
  result:='';

  OldClass:=nil;

  for t:=0 to High(ASection.Ordered) do
  begin
    tmpIdent:=ASection.Ordered[t];

    if tmpIdent.ClassType<>OldClass then
    begin
      OldClass:=tmpIdent.ClassType;

      if result<>'' then
         result:=result+CRLF;

      if OldClass<>TMethodDeclaration then
      begin
        if (tmpIdent is TIdentifier) and
           (TIdentifier(tmpIdent).TypeIdentifier=ResourceStringType) then
           S:=''
        else
        if OldClass=TThreadVariable then
           S:=''
        else
        if OldClass=TVariableDeclaration then
           S:=''
        else
        if OldClass=TConstantDeclaration then
           S:=''
        else
        if OldClass=TTypeDeclaration then
           S:=''
        else
        {
        if OldClass=TLabelDeclaration then // No equivalent
           S:='label'
        else
        }
        if (OldClass=TExportDeclaration) or (OldClass=TDirective) then
           S:=''
        else
           S:='?';

        if S<>'' then
           result:=result+Indent+Keyword(S)+CRLF;
      end;
    end;

    if tmpIdent is TMethodDeclaration then
       tmp:=Method(TMethodDeclaration(tmpIdent),Bodies,Indent)
    else
    if tmpIdent is TTypeDeclaration then
    begin
      tmp:=Indent+IndentText+EmitType(TTypeDeclaration(tmpIdent),Indent);

      if TTypeDeclaration(tmpIdent).Expression is TRecordSpecification then
         tmp:=tmp+CRLF;
    end
    else
    if tmpIdent is TConstantDeclaration then
       tmp:=Indent+IndentText+EmitConst(TConstantDeclaration(tmpIdent),Indent)+';'
    else
    if tmpIdent is TLabelDeclaration then
       tmp:=Indent+TLabelDeclaration(tmpIdent).Name
    else
    if tmpIdent is TExportDeclaration then
       tmp:=Indent+'exports '+LinkTo(TExportDeclaration(tmpIdent).Method,TExportDeclaration(tmpIdent).Method.Name)+';'
    else
    if tmpIdent is TDirective then
       tmp:='{$'+TDirective(tmpIdent).Text+'}'
    else
       tmp:='?';

    result:=result+tmp+CRLF;
  end;
end;

function TCSharpEmit.EmitUnit(const AUnit:TUnit):String;
begin
  if (AUnit.UnitInterface<>nil) and  (AUnit.UnitInterface.UsesUnits<>nil) then
     result:=EmitUses(AUnit.UnitInterface.UsesUnits)
  else
     result:=EmitUsing('System');

  if AUnit.ModuleImplementation<>nil then
     result:=result+EmitUses(AUnit.ModuleImplementation.UsesUnits);

  result:=Keyword('namespace')+' '+AUnit.Name+CRLF+'{'+CRLF+
          Section(AUnit.UnitInterface,True,'')+CRLF;

  if AUnit.ModuleImplementation<>nil then
     result:=result+Section(AUnit.ModuleImplementation,True,'');

  // .NET has .ctor module initializers, not avail in CSharp or VB.Net
  {
  if AUnit.UnitInitialization<>nil then
  begin
    result:=result+CRLF+
      Keyword('initialization')+CRLF+
         Statements(AUnit.UnitInitialization,'',False);

    if AUnit.UnitFinalization<>nil then
       result:=result+CRLF+
         Keyword('finalization')+CRLF+
            Statements(AUnit.UnitFinalization,'',False);
  end;
  }

  result:=result+CRLF+Keyword('}');
end;

function TCSharpEmit.EmitUsing(const AUnit:String):String;
begin
  result:=Keyword('using')+' '+UnitNameLink(AUnit)+';';
end;

function TCSharpEmit.EmitUses(const AUses:TUses):String;
var t : Integer;
begin
  result:='';

  for t:=0 to AUses.Count-1 do
      result:=result+EmitUsing(AUses[t].Name)+CRLF;
end;

function TCSharpEmit.UsesSection(const ASection:TUsesSection):String;
begin
  if ASection.UsesUnits<>nil then
     result:=Block(ASection.UsesUnits)+CRLF+CRLF;

  result:=result+Section(ASection,True,'');
end;

function TCSharpEmit.EmitProgram(const AProgram:TProgram):String;
begin
  result:=Keyword('namespace')+' '+AProgram.Name+';'+CRLF+
          Section(AProgram.ModuleImplementation,True,'')+CRLF+
          Keyword('{')+CRLF+
          Block(AProgram.Main)+CRLF+
          Keyword('}');
end;

function TCSharpEmit.Block(const ABlock:TBlock):String;
begin
  if ABlock is TUnit then
     result:=EmitUnit(TUnit(ABlock))
  else
  if ABlock is TProgram then
     result:=EmitProgram(TProgram(ABlock))
  else
  {
  if ABlock is TPackage then
     result:=EmitPackage(TPackage(ABlock)) // No equivalent
  else
  }
  if ABlock is TUsesSection then
     result:=UsesSection(TUsesSection(ABlock))
  else
  if ABlock is TUses then
     result:=EmitUses(TUses(ABlock))
  else
  if ABlock is TConstantDeclarations then
     result:=Constants(vPublic,TConstantDeclarations(ABlock),'')
  else
  if ABlock is TVariableDeclarations then
     result:=Variables(TVariableDeclarations(ABlock),'')
  else
  if ABlock is TMethodDeclaration then
     result:=Method(TMethodDeclaration(ABlock),True,'')
  else
  if ABlock is TMethodDeclarations then
     result:=Methods(vPublic,TMethodDeclarations(ABlock),True,'')
  else
  if ABlock is TTypeDeclarations then
     result:=Types(vPublic,TTypeDeclarations(ABlock),'')
  else
  if ABlock is TStatements then
     result:=Statements(TStatements(ABlock),'')
  else
  if ABlock is TCaseVariable then
     result:=CaseVariable(TCaseVariable(ABlock),'',False)
  else
  if ABlock is TConstantDeclaration then
     result:=EmitConst(TConstantDeclaration(ABlock),'')
  else
  if ABlock is TTypeDeclaration then
     result:=EmitType(TTypeDeclaration(ABlock),'')
  else
  if ABlock is TStatement then
     result:=Statement(TStatement(ABlock),'')
  else
  if ABlock is TExpression then
     result:=Expression(TExpression(ABlock))
  else
  if ABlock is TBody then
     result:=Statements(TBody(ABlock).Statements,'')
  else
  if ABlock is TTypeSpecification then
     result:=TypeSpec(TTypeSpecification(ABlock),False,'','')
  else
  if ABlock is TAttribute then
     result:='{$'+TAttribute(ABlock).Text+'}'
  else
  if ABlock=nil then
     result:='?nil'
  else
     result:='?'+ABlock.ClassName; // <-- error !

  if (ABlock<>nil) and (ABlock.Attributes<>nil) then
     result:=Attributes(ABlock,'')+result;
end;

{ TCSharpEmitHTML }

function TCSharpEmitHTML.AnchorOf(const S: String): String;
begin
  if S='' then
     result:=''
  else
     result:='<a name="'+S+'"></a>'+S;
end;

Constructor TCSharpEmitHTML.Create;
begin
  inherited;

  CRLF:='<br>';
  IndentText:='&nbsp;&nbsp;';
  LessThan:='&lt;';
  GreaterThan:='&gt;';
end;

function TCSharpEmitHTML.Keyword(const S: String): String;
begin
  result:='<span class="k">'+S+'</span>';
end;

function TCSharpEmitHTML.LinkTo(const ABlock:TBlock; const S: String): String;
begin
  result:={$IFDEF D9}ABlock.ToString{$ELSE}(ABlock as TExpression).ToString{$ENDIF};

  result:='<a href="#'+result+'">'+S+'</a>';
end;

class function TCSharpEmitHTML.ModuleExtension: String;
begin
  result:='.html';
end;

class function TCSharpEmitHTML.Page(const ABlock: TBlock): String;
begin
  result:='<html><head>'+DefaultStyle+'</head><body>'+AsString(ABlock)+'</body></html>';
end;

function TCSharpEmitHTML.StringToCode(const S: String): String;
begin
  result:='<span class="s">'+inherited {$IFNDEF D12}StringToCode(S){$ENDIF}+'</span>';
end;

function TCSharpEmitHTML.UnitNameLink(const S: String): String;
begin
  result:='<a href="'+S+'.html">'+S+'</a>';
end;

class function TCSharpEmitHTML.DefaultStyle:String;

  function None:String;
  begin
    result:='{color:#000000; background-color:transparent; text-decoration:none}'+#13#10;
  end;

  function Underline:String;
  begin
    result:='{color:#ff0000; background-color:transparent; text-decoration:underline}'+#13#10;
  end;

begin
  result:='<style>'+
    'body {font-family:courier new;}'+
    'span.k {color:navy; font-weight:bold;}'+
    'span.s {color:blue;}'+
    'span.n {color:blue;}'+#13#10+
    'a:link '+None+'a:visited '+None+
    'a:hover '+Underline+'a:active '+Underline+
    '</style>'+#13#10;
end;

end.
