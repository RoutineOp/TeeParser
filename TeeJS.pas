unit TeeJS;

interface

uses
  TeeCode, SysUtils;

type
  EJSParser=class(Exception);

  TJSModule=TImplementationModule;

  TJSParser=class(TBaseParser)
  private
    Module : TJSModule;

    UseStrict : Boolean;

    ArrayBlock : TRecordInstance;

    Globals : TSection;

    TypeOfBlock : TMethodDeclaration;
    UndefinedType : TTypeDeclaration;

    class function CreateRecord(const AOwner:TBlock; const AName:String):TRecordInstance;
    function IsAssignmentOperator(const C:Char; out AOperator:TAssignmentOperator):Boolean;
    class function Literal(const AOwner:TBlock; const S:String):TExpression;
    class function NewUndefined(const AName:String; const AOwner:TVariableDeclarations):TIdentifier;
    function NextJSString(const Delim:Char):String;

    function ParseCallParameters(const AOwner:TBlock):TExpressions;
    procedure ParseDo(const ABody:TBody; const ADo:TRepeat);
    procedure ParseFor(const ABody:TBody; const AFor:TForLoop);
    function ParseForBody(const AOwner:TBlock):TBody;
    procedure ParseFunctionBody(const AFunc:TFunctionSpecification);
    function ParseFunctionExpression(const AOwner:TBlock):TVariableDeclaration;
    procedure ParseIf(const ABody:TBody; const AIf:TIf);
    function ParseItemExpression(const AOwner:TBlock; const AValue:TExpression):TItemExpression;
    function ParseNewMethod(const AOwner:TBlock; const AName:String):TMethodDeclaration;
    procedure ParseObject(const AObject:TRecordInstance);
    procedure ParseParameters(const AFunc:TFunctionSpecification);
    function ParseBody(const ABody:TBody; IsMultiple:Boolean=False):TStatements;
    procedure ParseStatements(const ABody:TBody; const S:String;
                             const AStats:TStatements);
    procedure ParseSwitch(const ABody:TBody; const ASwitch:TCase);
    procedure ParseTry(const ABody:TBody; const ATry:TTry);
    procedure ParseVariables(const AVars:TVariableDeclarations);
    procedure ParseWhile(const ABody:TBody; const AWhile:TWhile);
    procedure ParseWith(const ABody:TBody; const AWith:TWith);

    function RequireCallOrCast(const AOwner:TBlock; const AExp:TExpression):TExpression;
    function RequireExpression(const AOwner:TBlock):TExpression;
  protected
    KeepComments : Boolean;

    procedure AddGlobals(const Globals:TBlock; const CompilerVersion:Integer); override;
    procedure Increment; override;

    procedure Parse(const AModule:TImplementationModule);
  end;

  TJavaScript=class(TLanguage)
  protected
    procedure DoParse(const AModule:TModule; const AFileName:String); override;
    procedure InternalParseModule(const AParser:TBaseParser; const AModule:TModule); override;
    function ParserFromFile(const AOwner:TBlock; const AFile:String):TBaseParser; override;
  public
    Constructor Create; override;

    function FullPathOf(var AName:String; const AExtension:String):String; override;
    function Parse(const AFileName: String): TImplementationModule;
  end;

  TJSEmit=class(TEmit)
  private
    function Body(const ABody:TBody):String;
    function CallExpression(const ACall:TCallExpression):String;
    function CallParameters(const P:TExpressions):String;
    function Expression(const AExp:TExpression; const AOwner:TExpression=nil):String;
    function EmitCase(const C:TCase; const Indent:String):String;
    function EmitIf(const AIf:TIf; const Indent:String):String;
    function EmitOperator(const AExp:TOperatorExpression):String;
    function ImplementationModule(const AModule:TImplementationModule):String;
    function MethodDeclaration(const AMethod:TMethodDeclaration):String;
    function MethodSpec(const AMethod:TMethodParams):String;
    function OperatorToString(const AOperator: TOperator): String;
    function Parameters(const AParams:TParameters):String;
    function RecordInstance(const ARecord:TRecordInstance):String;
    function Statement(const S:TStatement; const Indent:String):String;
    function Statements(const S:TStatements; const Indent:String; BeginEnd:Boolean=True):String;
    function Variable(const AVar:TVariableDeclaration):String;
    function Variables(const AVars:TVariableDeclarations):String;
  protected
    function Block(const ABlock:TBlock):String; override;
    function Expressions(const AItems:TExpressions):String; override;
  public
    class function ModuleExtension:String; override;
  end;

  TRegularExpressionConstant=class(TRecordInstance)
  public
    Text : String;
  end;

  TTernaryExpression=class(TExpression)
  public
    Condition,
    WhenTrue,
    WhenFalse : TExpression;
  end;

implementation

// For TJSEmit:
const
  MaxLineWidth=80;

{ TJavaScript }

constructor TJavaScript.Create;
begin
  inherited;

  CaseSentitive:=True;
  ModuleClass:=TJSModule;
  SelfKeyword:='this';
end;

procedure TJavaScript.DoParse(const AModule: TModule; const AFileName: String);
begin
  ParseModule(AModule,AFileName);
end;

function TJavaScript.FullPathOf(var AName: String; const AExtension: String): String;
begin
  result:=AName;
end;

type
  TBaseParserAccess=class(TBaseParser);

procedure TJavaScript.InternalParseModule(const AParser: TBaseParser; const AModule: TModule);
begin
  AModule.FileName:=AParser.CurrentFile;
  AModule.Name:=ExtractFileName(AModule.FileName);

  if AModule is TImplementationModule then
     TJSParser(AParser).Parse(TImplementationModule(AModule))
  else
     TBaseParserAccess(AParser).Error(AModule,'Module must be a TJSModule');
end;

function TJavaScript.Parse(const AFileName: String): TImplementationModule;
begin
  result:=TImplementationModule.Create(nil);
  Modules.PackageContains.Add(result);
  DoParse(result,AFileName);
end;

function TJavaScript.ParserFromFile(const AOwner: TBlock;
  const AFile: String): TBaseParser;
begin
  result:=TJSParser.Create(AOwner,Self);

  result.CurrentFile:=AFile;
  result.CurrentPath:=ExtractFilePath(AFile);

  TJSParser(result).Text:=TBaseParser.TextOf(AFile);
end;

{ TJSIntrinsics }

type
  TJSIntrinsics=class(TIntrinsics)
  private
    Parser : TJSParser;

    procedure AddGlobals;
  end;

procedure TJSIntrinsics.AddGlobals;

  function DateGetTimeBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('getTime',ResultOf(AnySpec)); // int
    result.IsClass:=True;
  end;

  function StringReplaceBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('replace',ResultOf(StringSpec));
    AddParam(result,AnyType,'searchvalue');
    AddParam(result,AnyType,'newvalue');

    result.IsClass:=True;
  end;

  function StringSliceBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('slice',ResultOf(StringSpec));
    AddParam(result,AnyType,'start');
    AddParam(result,AnyType,'end');

    result.IsClass:=True;
  end;

  function StringMatchBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('match',ResultOf(ArraySpec)); // <-- Array of String
    AddParam(result,AnyType,'regexp');

    result.IsClass:=True;
  end;

  function ArrayPopBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('pop',ResultOf(AnySpec));
    result.IsClass:=True;
  end;

var BreakBlock,
    ContinueBlock,
    DebuggerBlock : TMethodDeclaration;

    StringBlock,
    DateBlock: TRecordInstance;

    DateSpec : TClassSpecification;

    Infinity,
    NaN : TNumber;
begin
  Globals.Init;

  AnySpec:=TTypeSpecification.Create(Globals);
  AnyType:=GlobalType(AnySpec);

  BooleanType:=AnyType;

  Parser.UndefinedType:=GlobalType(AnySpec);
  Parser.UndefinedType.Name:='undefined';

  Infinity:=TNumber.CreateName(Globals,'Infinity');
  NaN:=TNumber.CreateName(Globals,'NaN');

  BreakBlock:=GlobalProcedure('Break');
  Globals.Methods.Add(BreakBlock);

  ContinueBlock:=GlobalProcedure('Continue');
  Globals.Methods.Add(ContinueBlock);

  DebuggerBlock:=GlobalProcedure('Debugger');
  Globals.Methods.Add(DebuggerBlock);

  Parser.TypeOfBlock:=GlobalFunction('typeof',ResultOf(AnySpec));
  Globals.Methods.Add(Parser.TypeOfBlock);

  ArraySpec:=TClassSpecification.Create(Globals);
  TClassSpecification(ArraySpec).Methods:=TMethodDeclarations.Create(ArraySpec);
  TClassSpecification(ArraySpec).Methods.Add(ArrayPopBlock);

  TClassSpecification(ArraySpec).PropertyDefault:=TPropertyDeclaration.CreateType(ArraySpec,AnyType);

  Parser.ArrayBlock:=TRecordInstance.CreateName(Globals,'Array');
  Parser.ArrayBlock.TypeIdentifier:=TTypeDeclaration.CreateSpec(Parser.ArrayBlock,ArraySpec);
  Globals.Variables.Add(Parser.ArrayBlock);

  DateSpec:=TClassSpecification.Create(Globals);
  DateSpec.Methods:=TMethodDeclarations.Create(DateSpec);
  DateSpec.Methods.Add(DateGetTimeBlock);

  DateBlock:=TRecordInstance.CreateName(Globals,'Date');
  DateBlock.TypeIdentifier:=TTypeDeclaration.CreateSpec(DateBlock,DateSpec);
  Globals.Variables.Add(DateBlock);

  StringSpec:=TClassSpecification.Create(Globals);
  TClassSpecification(StringSpec).Methods:=TMethodDeclarations.Create(StringSpec);

  TClassSpecification(StringSpec).Methods.Add(StringMatchBlock);
  TClassSpecification(StringSpec).Methods.Add(StringReplaceBlock);
  TClassSpecification(StringSpec).Methods.Add(StringSliceBlock);

  StringBlock:=TRecordInstance.CreateName(Globals,'String');
  StringBlock.TypeIdentifier:=TTypeDeclaration.CreateSpec(StringBlock,StringSpec);
  Globals.Variables.Add(StringBlock);

  {
  Pending:

  decodeURI()	Decodes a URI
  decodeURIComponent()	Decodes a URI component
  encodeURI()	Encodes a URI
  encodeURIComponent()	Encodes a URI component
  escape()	Deprecated in version 1.5. Use encodeURI() or encodeURIComponent() instead
  eval()		Evaluates a string and executes it as if it was script code
  isFinite()	Determines whether a value is a finite, legal number
  isNaN()		Determines whether a value is an illegal number
  Number()	Converts an object's value to a number
  parseFloat()	Parses a string and returns a floating point number
  parseInt()	Parses a string and returns an integer
  String()	Converts an object's value to a string
  unescape()	Deprecated in version 1.5. Use decodeURI() or decodeURIComponent() instead
  }
end;

{ TJSParser }

procedure TJSParser.AddGlobals(const Globals: TBlock; const CompilerVersion: Integer);
var tmp : TJSIntrinsics;
begin
  tmp:=TJSIntrinsics.Create;
  try
    tmp.Globals:=TSection(Globals);
    tmp.Parser:=Self;

    tmp.AddGlobals;
  finally
    tmp.Free;
  end;
end;

procedure TJSParser.Increment;
var C : Char;
begin
  repeat
    C:=Advance;

    if State.InLineComment then
    begin
      if (C=#13) or (C=#10) then
         State.InLineComment:=False;

      if KeepComments and State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if State.InBlockComment then
    begin
      if (C='/') and PrevCharIs('*') then
         State.InBlockComment:=False;

      if KeepComments and State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if (C='/') and NextCharIs('/') then
    begin
      State.InLineComment:=True;

      if KeepComments and State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if (C='/') and NextCharIs('*') and (not IgnoreStrings) then
       State.InBlockComment:=True
    else
    if State.DefinesOk then
    begin
      State.C:=C;
      Exit;
    end;

  until Eof;
end;

function TJSParser.IsAssignmentOperator(const C:Char; out AOperator:TAssignmentOperator):Boolean;
begin
  result:=False;

  case C of
    '=': begin
           AOperator:=aoAssign;
           result:=True;
         end;

    '+': if IsNextChar('=') then
         begin
           AOperator:=aoAddAssign;
           result:=True;
         end;

    '-': if IsNextChar('=') then
         begin
           AOperator:=aoSubtractAssign;
           result:=True;
         end;

    '*': if IsNextChar('=') then
         begin
           AOperator:=aoMultiplyAssign;
           result:=True;
         end;

    '/': if IsNextChar('=') then
         begin
           AOperator:=aoDivideAssign;
           result:=True;
         end;

    '%': if IsNextChar('=') then
         begin
           AOperator:=aoModAssign;
           result:=True;
         end;
  end;
end;

procedure TJSParser.ParseStatements(const ABody:TBody; const S:String;
                 const AStats:TStatements);
var tmpStat : TStatement;
    tmpExp : TExpression;
    tmpOp : TAssignmentOperator;
    tmp : String;
begin
  tmpStat:=nil;

  case TextInCase(S,['if','try','return','for','do','switch','while','with']) of
    0: begin
         NextToken(tmp);

         tmpStat:=TIf.Create(AStats);
         ParseIf(ABody,TIf(tmpStat));
       end;
    1: begin
         NextToken(tmp);

         tmpStat:=TTry.Create(AStats);
         ParseTry(ABody,TTry(tmpStat));
       end;

    2: begin
         NextToken(tmp);

         tmpStat:=TCall.Create(AStats);
         TCall(tmpStat).Expression:=RequireExpression(tmpStat);
       end;
    3: begin
         NextToken(tmp);

         tmpStat:=TForLoop.Create(AStats);
         ParseFor(ABody,TForLoop(tmpStat));
       end;
    4: begin
         NextToken(tmp);

         tmpStat:=TRepeat.Create(AStats);
         ParseDo(ABody,TRepeat(tmpStat));
       end;
    5: begin
         NextToken(tmp);

         tmpStat:=TCase.Create(AStats);
         ParseSwitch(ABody,TCase(tmpStat));
       end;
    6: begin
         NextToken(tmp);

         tmpStat:=TWhile.Create(AStats);
         ParseWhile(ABody,TWhile(tmpStat));
       end;
    7: begin
         if UseStrict then
            Error('With not allowed in strict mode');

         tmpStat:=TWith.Create(AStats);
         ParseWith(ABody,TWith(tmpStat));
       end;

  else
    begin
      tmpExp:=RequireExpression(AStats);

      if IsAssignmentOperator(PeekChar,tmpOp) then
      begin
        if tmpOp<>aoAssign then
           Increment;

        if IsChar('=') then
        begin
          tmpStat:=TAssignment.Create(AStats);
          TAssignment(tmpStat).Operat:=tmpOp;

          TAssignment(tmpStat).Left:=tmpExp;
          TAssignment(tmpStat).Right:=RequireExpression(AStats);
        end
        else
          Error('Expected assign operator: =');
      end
      else
      if tmpExp is TCallExpression then
      begin
        tmpStat:=TCall.Create(AStats);
        TCall(tmpStat).Expression:=tmpExp;
      end
      else
      if (tmpExp is TOperatorExpression) and
         (
           (TOperatorExpression(tmpExp).Operat=opIncrement)
           or
           (TOperatorExpression(tmpExp).Operat=opDecrement)
         ) then
      begin
        tmpStat:=TAssignment.Create(AStats);

        if TOperatorExpression(tmpExp).Operat=opIncrement then
           TAssignment(tmpStat).Operat:=aoIncrement
        else
           TAssignment(tmpStat).Operat:=aoDecrement;

        TAssignment(tmpStat).Left:=tmpExp;
      end
      else
        Error('Expected = or (');
    end;
  end;

  AStats.Add(tmpStat);
end;

function TJSParser.ParseForBody(const AOwner:TBlock):TBody;
var S : String;
    C : Char;
begin
  result:=nil;

  repeat
    C:=PeekChar;

    if C=';' then
    begin
      Increment;
      Exit;
    end
    else
    if C=')' then
       Exit;

    PeekToken(S);

    if S='var' then
    begin
       NextToken(S);

       if result=nil then
          result:=TBody.Create(AOwner);

       if result.Variables=nil then
          result.Variables:=TVariableDeclarations.Create(result);

       ParseVariables(result.Variables);
    end
    else
    begin
      if result=nil then
         result:=TBody.Create(AOwner);

      if result.Statements=nil then
         result.Statements:=TStatements.Create(result);

      ParseStatements(result,S,result.Statements);
    end;

  until False;
end;

function TJSParser.ParseBody(const ABody:TBody; IsMultiple:Boolean=False):TStatements;
var S : String;
    C : Char;
    tmpStrict : TAttribute;
    tmpExp : TExpression;
begin
  result:=nil;

  if not IsMultiple then
  begin
    C:=PeekChar;

    if C='{' then
    begin
      IsMultiple:=True;
      Increment;
    end;
  end;

  repeat
    C:=PeekChar;

    if IsMultiple and (C='}') then
    begin
      Increment;

      {
      if PeekChar=';' then
         OptionalSemicolon;
      }

      break;
    end
    else
    if C='"' then
    begin
      S:=NextJSString(C);

      if SameText(S,'USE STRICT') then
      begin
        tmpStrict:=TAttribute.Create(ABody);
        tmpStrict.Text:=S;
        ABody.AddOrderedSingle(tmpStrict);

        UseStrict:=True;

        OptionalSemicolon;
      end
      else
         Error('Invalid clause: '+S);
    end
    else
    begin
      PeekToken(S);

      if S='var' then
      begin
         NextToken(S);

         if ABody.Variables=nil then
            ABody.Variables:=TVariableDeclarations.Create(ABody);

         ParseVariables(ABody.Variables);

         OptionalSemicolon;
      end
      else
      if S='function' then
      begin
        NextIdentifier;
        tmpExp:=ParseFunctionExpression(Module.ModuleImplementation);
        //ABody.AddOrderedSingle(tmpExp);
      end
      else
      begin
        if result=nil then
           result:=TStatements.Create(ABody);

        ParseStatements(ABody,S,result);

        OptionalSemicolon;

        if not IsMultiple then
           Exit;
      end;
    end;

  until False;
end;

function TJSParser.ParseCallParameters(const AOwner:TBlock):TExpressions;
var tmp : TExpression;
    L : Integer;
begin
  result:=nil;

  if not IsChar(')') then
  begin
    L:=0;

    repeat
      tmp:=RequireExpression(AOwner);

      if tmp=nil then
         break
      else
      begin
        SetLength(result,L+1);
        result[L]:=tmp;
        Inc(L);

        IsChar(',');

      end;

    until False;

    if not IsChar(')') then
       Error('Expected: )');
  end;
end;

procedure TJSParser.ParseDo(const ABody:TBody; const ADo: TRepeat);
begin
  ADo.RepeatPart:=ParseBody(ABody);

  if NextIdentifier='while' then
     ADo.Expression:=RequireExpression(ADo)
  else
     Error('Expected: while');
end;

procedure TJSParser.Parse(const AModule: TImplementationModule);
var tmp : TBody;
begin
  Module:=AModule;
  Module.ModuleImplementation.Parser:=Self;

  tmp:=TBody.Create(Module);
  Module.ModuleImplementation.AddOrderedSingle(tmp);

  if AnyType=nil then
  begin
    Globals:=Module.ModuleImplementation;
    AddGlobals(Module.ModuleImplementation,Language.CompilerVersion);
  end;

  tmp.Statements:=ParseBody(tmp,True);
end;

procedure TJSParser.ParseFor(const ABody:TBody; const AFor: TForLoop);
begin
  if IsChar('(') then
  begin
    AFor.Init:=ParseForBody(AFor);

    if PeekChar<>';' then
       AFor.Condition:=RequireExpression(AFor);

    if not IsChar(';') then
       Error('Expected: ;');

    AFor.Step:=ParseForBody(AFor);

    if not IsChar(')') then
       Error('Expected: )');

    AFor.Loop:=ParseBody(ABody);
  end
  else
     Error('Expected: (');
end;

procedure TJSParser.ParseFunctionBody(const AFunc: TFunctionSpecification);
begin
  if AFunc.Body=nil then
     AFunc.Body:=TBody.Create(AFunc);

  AFunc.Body.Statements:=ParseBody(AFunc.Body,True);
end;

function TJSParser.NextJSString(const Delim:Char):String;

{
Pending, escape \ characters:

\'	single quote
\"	double quote
\\	backslash
\n	new line
\r	carriage return
\t	tab
\b	backspace
\f	form feed

}
var C,
    C2 : Char;
begin
  result:='';

  Increment;

  repeat
    C:=Next;

    Increment;

    if C=Delim then
    begin
      C2:=Next;

      if C2=Delim then
      begin
        result:=result+C+C2;
        Increment;
      end
      else
        break
    end
    else
      result:=result+C;

  until False;
end;

function TJSParser.ParseNewMethod(const AOwner:TBlock; const AName:String):TMethodDeclaration;
var tmpFunc : TFunctionSpecification;
begin
  result:=TMethodDeclaration.CreateName(AOwner{Module.ModuleImplementation},AName);

  if AOwner is TSection then
     TSection(AOwner).Methods.AddSorted(result); // Empty name allowed

  if IsChar('(') then
  begin
    tmpFunc:=TFunctionSpecification.Create(result);
    result.TypeIdentifier:=TTypeDeclaration.Create(result);
    result.TypeIdentifier.Expression:=tmpFunc;

    tmpFunc.ResultValue:=TVariableDeclaration.CreateType(tmpFunc,AnyType);

    ParseParameters(tmpFunc);

    if IsChar(')') then
    begin
      if IsChar('{') then
      begin
        ParseFunctionBody(tmpFunc);

        //if not IsChar('}') then
        //   Error('Expected: }');
      end
      else
        Error('Expected: {');
    end
    else
      Error('Expected: )');
  end
  else
    Error('Expected: (');
end;

function TJSParser.ParseFunctionExpression(const AOwner:TBlock):TVariableDeclaration;
var tmpName : String;
    tmpMethod : TMethodDeclaration;
begin
  tmpName:=PeekIdentifier;

  if tmpName<>'' then
     NextIdentifier;

  if tmpName='' then
     result:=ParseNewMethod(AOwner {Module.ModuleImplementation},'')
  else
  begin
    result:=CreateRecord(AOwner,tmpName);
    TRecordInstance(result).Fields:=TRecordFieldsDeclarations.Create(result);

    tmpMethod:=ParseNewMethod(AOwner {Module.ModuleImplementation},'');

    TRecordInstance(result).Fields.Add(tmpMethod);
  end;
end;

procedure TJSParser.ParseIf(const ABody:TBody; const AIf: TIf);
var S : String;
begin
  if IsChar('(') then
  begin
    AIf.Expression:=RequireExpression(AIf);

    if IsChar(')') then
    begin
      AIf.ThenPart:=ParseBody(ABody);

      S:=PeekIdentifier;

      if s='else' then
      begin
        NextIdentifier;
        AIf.ElsePart:=ParseBody(ABody);
      end;
    end
    else
      Error('Expected: )');
  end
  else
    Error('Expected: (');
end;

procedure TJSParser.ParseObject(const AObject: TRecordInstance);
var S : String;
    C : Char;
    tmp : TIdentifier;
begin
  repeat
    C:=PeekChar;

    if C='}' then
       break;

    S:=NextIdentifier;

    if S='' then
       Error('Identifier expected');

    if IsChar(':') then
    begin
      if AObject.Fields=nil then
         AObject.Fields:=TRecordFieldsDeclarations.Create(AObject);

      tmp:=NewUndefined(S,AObject.Fields);
      TRecordInstance(tmp).Value:=RequireExpression(tmp);

      IsChar(',');
    end
    else
      Error('Expected :');

  until S='';
end;

function TJSParser.ParseItemExpression(const AOwner:TBlock; const AValue:TExpression):TItemExpression;
var L : Integer;
    tmpExp : TExpression;
    C : Char;
begin
  result:=TItemExpression.Create(AOwner);
  result.TypeIdentifier:=UndefinedType;

  if AValue=nil then
     result.Value:=ArrayBlock
  else
     result.Value:=AValue;

  L:=0;

  repeat
    tmpExp:=RequireExpression(result);

    SetLength(result.Items,L+1);
    result.Items[L]:=tmpExp;
    Inc(L);

    C:=PeekChar;

    if C=',' then
       Increment
    else
    if C=']' then
       break
    else
       Error('Expected: , or ]');

  until False;

  Increment;
end;

procedure TJSParser.ParseParameters(const AFunc: TFunctionSpecification);
var C : Char;
    S : String;
    //tmpParam : TVariableDeclaration;
begin
  repeat
    C:=PeekChar;

    if C=')' then
       break
    else
    begin
      S:=NextIdentifier;

      if AFunc.Parameters=nil then
         AFunc.Parameters:=TParameters.Create(AFunc);

      NewUndefined(S,AFunc.Parameters);
      {
      tmpParam:=TVariableDeclaration.CreateName(AFunc,S);
      tmpParam.TypeIdentifier:=AnyType;

      AFunc.Parameters.Add(tmpParam);
      }

      C:=PeekChar;

      if C=',' then
         Increment;
    end;

  until False;
end;

procedure TJSParser.ParseSwitch(const ABody:TBody; const ASwitch: TCase);

  procedure ParseCaseItem(const ACase:TCaseItem);
  var L : Integer;
  begin
    if IsChar(':') then
    begin
      ACase.Body:=ParseBody(ABody);

      L:=Length(ASwitch.Cases);
      SetLength(ASwitch.Cases,L+1);
      ASwitch.Cases[L]:=ACase;
    end
    else
       Error('Expected: :');
  end;

var S : String;
    C : Char;
    tmpCase : TCaseItem;
begin
  if IsChar('(') then
  begin
    ASwitch.Expression:=RequireExpression(ASwitch);

    if IsChar('{') then
    begin
      repeat
        C:=PeekChar;

        if C='}' then
           break;

        PeekToken(S);

        if S='case' then
        begin
          tmpCase:=TCaseItem.Create(ASwitch);
          tmpCase.Condition:=RequireExpression(tmpCase);

          ParseCaseItem(tmpCase);
        end
        else
        if S='default' then
        begin
          tmpCase:=TCaseItem.Create(ASwitch);
          ParseCaseItem(tmpCase);
        end
        else
           Error('Expected: case or default');

      until False;

      if not IsChar('}') then
         Error('Expected: }');
    end
    else
      Error('Expected: {');

    if not IsChar(')') then
       Error('Expected: )');
  end
  else
     Error('Expected: (');
end;

procedure TJSParser.ParseTry(const ABody:TBody; const ATry: TTry);
var S : String;
    tmpOn : TOn;
begin
  ATry.Block:=ParseBody(ABody);

  S:=PeekIdentifier;

  if S='catch' then
  begin
    NextIdentifier;

    ATry.ExceptPart:=TTryExcept.Create(ATry);

    tmpOn:=TOn.Create(ATry.ExceptPart);
    ATry.ExceptPart.Add(tmpOn);

    if IsChar('(') then
    begin
      NextToken(S);

      tmpOn.Exception:=TVariableDeclaration.CreateName(tmpOn,S);
      tmpOn.DoPart:=ParseBody(ABody);
    end
    else
      Error('Expected: (');

    PeekToken(S);
  end;

  if s='finally' then
  begin
    NextToken(S);
    ATry.FinallyPart:=ParseBody(ABody);
  end;
end;

class function TJSParser.CreateRecord(const AOwner:TBlock; const AName:String):TRecordInstance;
var tmpSpec : TRecordSpecification;
begin
  result:=TRecordInstance.CreateName(AOwner,AName);
  tmpSpec:=TRecordSpecification.Create(result);

  tmpSpec.PropertyDefault:=TPropertyDeclaration.CreateType(tmpSpec,AnyType);
  tmpSpec.PropertyDefault.Indexes:=TParameters.Create(tmpSpec.PropertyDefault);
  tmpSpec.PropertyDefault.Indexes.Add(TVariableDeclaration.CreateType(tmpSpec.PropertyDefault,AnyType));

  result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,tmpSpec);
end;

class function TJSParser.NewUndefined(const AName:String; const AOwner:TVariableDeclarations):TIdentifier;
begin
  result:=CreateRecord(AOwner,AName);
  AOwner.Add(TRecordInstance(result));
end;

procedure TJSParser.ParseVariables(const AVars:TVariableDeclarations);
var ID : String;
    tmp : TIdentifier;
    C : Char;
begin
  repeat
    ID:=NextIdentifier;

    C:=PeekChar;

    tmp:=NewUndefined(ID,AVars);

    if C='=' then
    begin
      Increment;

      TRecordInstance(tmp).Value:=RequireExpression(tmp);

      //RequireSemicolon;

      C:=PeekChar;

      if C=',' then
         Increment;
    end
    else
    if C=',' then
       Increment;

  until C=';';
end;

procedure TJSParser.ParseWhile(const ABody: TBody; const AWhile: TWhile);
begin
  if IsChar('(') then
  begin
    AWhile.Expression:=RequireExpression(AWhile);

    if IsChar(')') then
       AWhile.WhilePart:=ParseBody(ABody)
    else
       Error('Expected: )');
  end
  else
    Error('Expected: (');
end;

procedure TJSParser.ParseWith(const ABody: TBody; const AWith: TWith);
var tmp : TExpression;
begin
  if IsChar('(') then
  begin
    tmp:=RequireExpression(AWith);

    SetLength(AWith.Items,1);
    AWith.Items[0]:=tmp;

    if IsChar(')') then
       AWith.WithPart:=ParseBody(ABody)
    else
       Error('Expected: )');
  end
  else
    Error('Expected: (');
end;

function TJSParser.RequireCallOrCast(const AOwner:TBlock; const AExp:TExpression):TExpression;
begin
  result:=TCallExpression.CreateCall(AOwner,AExp,ParseCallParameters(AOwner));
end;

class function TJSParser.Literal(const AOwner:TBlock; const S:String):TExpression;
var tmpFloat : Extended;
    tmpInt : Int64;
begin
  // Pending: 0x

  if S='true' then
  begin
    result:=TBoolean.Create(AOwner);
    TBoolean(result).Value:=True;
  end
  else
  if S='false' then
     result:=TBoolean.Create(AOwner)
  else
  if TryStrToInt64(S,tmpInt) then
  begin
    result:=TNumber.Create(AOwner);
    TNumber(result).SetValue(tmpInt);
  end
  else
  if TryStrToFloat(S,tmpFloat) then
  begin
    result:=TFloatNumber.Create(AOwner);

    TFloatNumber(result).ValueString:=S;
    TFloatNumber(result).SetValue(tmpFloat);
    TFloatNumber(result).Exponent:=SameText(Copy(S,Length(S),1),'E');
  end
  else
     result:=nil;
end;

function TJSParser.RequireExpression(const AOwner:TBlock): TExpression;

  function IsOperatorWord(const S:String; out AOperator:TOperator):Boolean;
  begin
    if s='||' then
    begin
      AOperator:=opOr;
      result:=True;
      Exit;
    end
    else
    if s='&&' then
    begin
      AOperator:=opAnd;
      result:=True;
      Exit;
    end
    else
    if s='in' then
    begin
      AOperator:=opIn;
      result:=True;
      Exit;
    end;

    result:=False;
  end;

  function IsOperator(const C:Char; out AOperator:TOperator):Boolean;
  begin
    result:=False;

    case C of
      '=' : if IsNextChar('=') then
            begin
              if IsNextChar('=') then
                 AOperator:=opEqualType
              else
                 AOperator:=opEqual
            end
            else
              Exit;

      '!' : if IsNextChar('=') then
            begin
              if IsNextChar('=') then
                 AOperator:=opNotEqualType
              else
                 AOperator:=opNotEqual;
            end
            else
              AOperator:=opNot;

      '<' : if IsNextChar('=') then
               AOperator:=opLowerEqual
            else
            if IsNextChar('<') then
               AOperator:=opShl
            else
               AOperator:=opLower;

      '>' : if IsNextChar('=') then
               AOperator:=opGreaterEqual
            else
            if IsNextChar('>') then
               AOperator:=opShr
            else
               AOperator:=opGreater;

      '+' : if IsNextChar('=') then
               Exit
            else
            if IsNextChar('+') then
               AOperator:=opIncrement
            else
               AOperator:=opAdd;

      '-' : if IsNextChar('=') then
               Exit
            else
            if IsNextChar('-') then
               AOperator:=opDecrement
            else
               AOperator:=opSubtract;

      '*' : if IsNextChar('=') then
               Exit
            else
               AOperator:=opMultiply;

      '/' : if IsNextChar('=') then
               Exit
            else
               AOperator:=opDivide;

      '%' : if IsNextChar('=') then
               Exit
            else
               AOperator:=opMod;
    else
      Exit;
    end;

    result:=True;
  end;

  function OperatorFrom(const AOperator:TOperator; const ALeft:TExpression):TExpression;
  begin
    result:=TOperatorExpression.Create(AOwner);
    TOperatorExpression(result).Operat:=AOperator;
    TOperatorExpression(result).Left:=ALeft;
    TOperatorExpression(result).Right:=RequireExpression(AOwner);

    result.TypeIdentifier:=TOperatorExpression(result).CalcType;
  end;

  function AddUndefined(const ARecord:TRecordInstance; const S:String):TExpression;

    procedure AddToSpec(const ASpec:TRecordSpecification; const AIdent:TIdentifier);
    begin
      if ASpec.Fields=nil then
         ASpec.Fields:=TRecordFieldsDeclarations.Create(ASpec);

      ASpec.Fields.AddSorted(AIdent);
    end;

  begin
    if ARecord.Fields=nil then
       ARecord.Fields:=TRecordFieldsDeclarations.Create(ARecord);

    result:=NewUndefined(S,ARecord.Fields);

    AddToSpec(TRecordSpecification(ARecord.TypeIdentifier.Expression),TIdentifier(result));
  end;

  function GuessToken(const AParent:TExpression):TExpression;
  var S : String;
  begin
    NextToken(IdentifierCharsNoDot,S);

    if S='' then
    begin
      result:=AParent;
      Exit;
    end;

    if AParent=nil then
       result:=Language.GuessIdentifier(skField,AOwner,S,False)
    else
       result:=Language.GuessIdentifier(skField,AParent,S,False,nil,[],[],AParent);

    if result=nil then
       if AParent=nil then
       begin
         if Globals.Variables=nil then
            Globals.Variables:=TVariableDeclarations.Create(Globals);

         result:=NewUndefined(S,Globals.Variables);
       end
       else
       begin
         result:=AParent;

         if result is TFieldExpression then
            result:=TFieldExpression(result).Field;

         if result is TRecordInstance then
            result:=AddUndefined(TRecordInstance(result),S)
         else
         if result is TCallExpression then
            result:=NewUndefined(S,Globals.Variables)
            //Error('Field not found: '+S)
         else
            Error('Not an instance: '+result.ClassName);
       end;
  end;

  function NextRegularExpression:String;
  var C : Char;
  begin
    result:='/';

    Increment;

    repeat
      C:=PeekChar;

      Increment;

      result:=result+C;

      if C='/' then
         break;

    until Eof;

    repeat
      C:=PeekChar;

      if (C='i') or (C='g') or (C='m') then
      begin
        result:=result+C;
        Increment;
      end
      else
      if (C='.') or (C=',') or (C=';') or (C=#13) or (C=#10) then
         break
      else
      begin
        Error('Wrong regexp modifier: '+C);
        break;
      end;

    until Eof;
  end;

var S : String;
    C : Char;
    tmpOp : TOperator;
    tmpExp : TExpression;
    tmpIdent : TExpression;
    tmpAssignOp : TAssignmentOperator;
begin
  result:=nil;

  repeat
    if Eof then
       break;

    C:=PeekChar;

    if (C=';') or (C=',') or (C=')') or (C=']') or (C='}') or (C=':') then
       break
    else
    if (C='"') or (C='''') then
    begin
      S:=NextJSString(C);
      result:=TConstantDeclaration.NewStringFrom(AOwner,S);
    end
    else
    if (C='/') and (result=nil) then
    begin
      S:=NextRegularExpression;
      result:=TRegularExpressionConstant.Create(AOwner);
      TRegularExpressionConstant(result).Text:=S;

      result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,TRecordSpecification.Create(result));
    end
    else
    if C='(' then
    begin
      Increment;

      S:=PeekIdentifier;

      if s='function' then
      begin
        NextIdentifier;
        result:=ParseFunctionExpression(Module.ModuleImplementation);

        if IsChar(')') then
        begin
          if IsChar('(') then
             result:=RequireCallOrCast(AOwner,result);
        end
        else
          Error('Expected: )');
      end
      else
      begin
        result:=RequireExpression(AOwner);

        if not IsChar(')') then
           Error('Expected: )');
      end;

    end
    else
    if C='{' then
    begin
      Increment;

      result:=CreateRecord(AOwner,'');
      ParseObject(TRecordInstance(result));

      if not IsChar('}') then
         Error('Expected: }');
    end
    else
    if IsOperator(C,tmpOp) then
    begin
      Increment;
      result:=OperatorFrom(tmpOp,result);
    end
    else
    begin
      if IsAssignmentOperator(C,tmpAssignOp) then
         Exit;

      PeekToken(S);

      if S='typeof' then
      begin
        NextToken(S);
        tmpExp:=RequireExpression(AOwner);
        result:=TCallExpression.CreateCall(AOwner,TypeOfBlock,[tmpExp]);
      end
      else
      if IsOperatorWord(S,tmpOp) then
      begin
        NextToken(S);
        result:=OperatorFrom(tmpOp,result);
      end
      else
      begin
        if C='?' then
        begin
          Increment;

          result:=TTernaryExpression.Create(AOwner);

          TTernaryExpression(result).TypeIdentifier:=UndefinedType;

          TTernaryExpression(result).Condition:=result;
          TTernaryExpression(result).WhenTrue:=RequireExpression(result);

          if IsChar(':') then
             TTernaryExpression(result).WhenFalse:=RequireExpression(result)
          else
             Error('Expected: :');
        end
        else
        if C<>'.' then
        begin
          S:=PeekIdentifier;

          result:=Literal(AOwner,S);

          if result<>nil then
             NextIdentifier
          else
          if S='function' then
          begin
            NextIdentifier;
            result:=ParseNewMethod(AOwner,'');
            Exit;
          end
          else
          if S='new' then
          begin
            NextIdentifier;
            NextToken(IdentifierCharsNoDot,S);

            tmpIdent:=Language.GuessIdentifier(skField,AOwner,S,False);

            if tmpIdent=nil then
               Error('Cannot find function: '+S);

            // Optional call and parameters
            if IsChar('(') then
               result:=RequireCallOrCast(AOwner,tmpIdent);
          end
          else
            result:=GuessToken(result);
        end;

        repeat
          C:=PeekChar;

          if C='.' then
          begin
            Increment;

            tmpIdent:=GuessToken(result);

            if IsChar('(') then
               tmpIdent:=RequireCallOrCast(AOwner,tmpIdent);

            result:=TFieldExpression.CreateField(AOwner,result,tmpIdent);
          end
          else
          if C='[' then
          begin
            Increment;
            result:=ParseItemExpression(AOwner,result);
            //S:='?'; // bad: to skip breaking repeat loop
          end
          else
          if C='(' then // call or casting
          begin
            Increment;
            result:=RequireCallorCast(AOwner,result);
          end
          else
             break;

        until False;
      end;
    end;

  until False {S=''};
end;

{ TJSEmit }

function TJSEmit.Block(const ABlock: TBlock): String;
begin
  if ABlock is TImplementationModule then
     result:=ImplementationModule(TImplementationModule(ABlock))
  else
  if ABlock is TBody then
     result:=Body(TBody(ABlock))
  else
  if ABlock is TStatement then
     result:=Statement(TStatement(ABlock),'')
  else
  if ABlock is TExpression then
     result:=Expression(TExpression(ABlock))
  else
  if ABlock is TCallExpression then
     result:=CallExpression(TCallExpression(ABlock))
  else
     result:='??'+ABlock.ToString;
end;

function TJSEmit.Expression(const AExp: TExpression; const AOwner:TExpression=nil): String;
begin
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
  if AExp is TMethodDeclaration then
     result:=MethodDeclaration(TMethodDeclaration(AExp))
  else
  if AExp is TRecordInstance then
     result:='{'+RecordInstance(TRecordInstance(AExp))+'}'
  else
  if AExp is TVariableDeclaration then
     result:=TVariableDeclaration(AExp).Name
  else
     result:='??'+AExp.ClassName;
end;

function TJSEmit.Body(const ABody: TBody): String;
var t : Integer;
begin
  result:='';

  if ABody<>nil then
  begin
    if (ABody.Variables<>nil) and (ABody.Variables.Count>0) then
        result:='var'+CRLF+Variables(ABody.Variables)+CRLF;

    if ABody.Methods<>nil then
    for t:=0 to ABody.Methods.Count-1 do
        result:=result+MethodDeclaration(ABody.Methods[t])+CRLF;

    result:=result+Statements(ABody.Statements,'');
  end;
end;

function TJSEmit.CallExpression(const ACall: TCallExpression): String;
begin
  result:=Expression(ACall.Expression);

  if ACall.Parameters<>nil then
     result:=result+'('+CallParameters(ACall.Parameters)+')';
end;

function TJSEmit.CallParameters(const P: TExpressions): String;
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

function TJSEmit.EmitCase(const C: TCase; const Indent: String): String;

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

function TJSEmit.EmitIf(const AIf: TIf; const Indent:String): String;
begin
  result:=Keyword('if')+' ('+Expression(AIf.Expression)+') '+CRLF+
            Statements(AIf.ThenPart,Indent+' ');

  if AIf.ElsePart<>nil then
     result:=result+CRLF+Indent+Keyword('else')+CRLF+Statements(AIf.ElsePart,Indent);
end;

function TJSEmit.EmitOperator(const AExp: TOperatorExpression): String;

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
     result:=' '+Keyword('!')+' '+tmpLeft
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

function TJSEmit.Expressions(const AItems: TExpressions): String;
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

function TJSEmit.ImplementationModule(const AModule: TImplementationModule): String;
var t : Integer;
begin
  // Pending: Uses
  result:='';

  for t:=0 to High(AModule.ModuleImplementation.Ordered) do
      result:=result+Block(AModule.ModuleImplementation.Ordered[t])+CRLF;
end;

function TJSEmit.MethodSpec(const AMethod:TMethodParams): String;
begin
  result:='('+Parameters(AMethod.Parameters)+')'+CRLF;

  if AMethod.Body<>nil then
     result:=result+'{'+CRLF+Body(AMethod.Body)+CRLF+'}'+CRLF;
end;

function TJSEmit.MethodDeclaration(const AMethod: TMethodDeclaration): String;
var tmp : TTypeSpecification;
begin
  result:='function';

  if AMethod.Name<>'' then
     result:=result+' '+AMethod.Name;

  tmp:=AMethod.TypeIdentifier.Expression;

  if tmp is TMethodParams then
     result:=result+MethodSpec(TMethodParams(tmp))
  else
     result:=result+'??'+tmp.ClassName;
end;

class function TJSEmit.ModuleExtension: String;
begin
  result:='.js';
end;

function TJSEmit.OperatorToString(const AOperator: TOperator): String;
begin
  case AOperator of
     opEqual: result:='==';
 opEqualType: result:='===';
  opNotEqual: result:='!=';
opNotEqualType: result:='!==';
       opAnd: result:=Keyword('&&');
        opOr: result:=Keyword('||');
       opXor: result:=Keyword('xor');
       opShl: result:=Keyword('<<');
       opShr: result:=Keyword('>>');
     opLower: result:=LessThan;
   opGreater: result:=GreaterThan;
opGreaterEqual: result:=GreaterThan+'=';
opLowerEqual: result:=LessThan+'=';
       opAdd: result:='+';
  opSubtract: result:='-';
  opMultiply: result:='*';
    opDivide: result:='/';
       opNot: result:=Keyword('!');
       opDiv: result:=Keyword('/');
       opMod: result:=Keyword('%');
        opIn: result:=Keyword('in');
 opIncrement: result:=Keyword('++');
 opDecrement: result:=Keyword('--');
  else
     result:='??';
  end;
end;

function TJSEmit.Parameters(const AParams: TParameters): String;
var t : Integer;
begin
  result:='';

  if AParams<>nil then
  for t:=0 to AParams.Count-1 do
  begin
    result:=result+AParams[t].Name;

    if t<AParams.Count then
       result:=result+', ';
  end;
end;

function TJSEmit.RecordInstance(const ARecord: TRecordInstance): String;
var t : Integer;
begin
  result:='';

  if ARecord.Fields<>nil then
  for t:=0 to ARecord.Fields.Count-1 do
      result:=result+Block(ARecord.Fields[t])+CRLF;
end;

function TJSEmit.Statement(const S: TStatement; const Indent: String): String;

  function ForBody(const ABody:TBody):String;
  begin
    if (ABody<>nil) and (ABody.Variables<>nil) and (ABody.Variables.Count>0) then
        result:='var'+CRLF+Variables(ABody.Variables)
    else
        result:='';

    result:=result+Statements(ABody.Statements,'');
  end;

  function AssignmentOperator(const AOperator:TAssignmentOperator):String;
  begin
    case AOperator of
         aoAssign: result:='=';
      aoAddAssign: result:='+=';
 aoSubtractAssign: result:='-=';
 aoMultiplyAssign: result:='*=';
   aoDivideAssign: result:='/=';
      aoModAssign: result:='%=';
    else
      result:='?=?';
    end;
  end;

begin
  if S is TAssignment then
  begin
    result:=Expression(TAssignment(S).Left)+' '+
            AssignmentOperator(TAssignment(S).Operat);

    if TAssignment(S).Right<>nil then
       result:=result+' '+Expression(TAssignment(S).Right)
    else
       result:=result+' ?? nil';
  end
  else
  if S is TCall then
     result:=Expression(TCall(S).Expression)
  else
  if S is TIf then
     result:=EmitIf(TIf(S),Indent)
  else
  if S is TRepeat then
  begin
    result:=Keyword('do')+CRLF+Statements(TRepeat(S).RepeatPart,Indent+IndentText)+CRLF+
            Keyword('while')+' ('+Expression(TRepeat(S).Expression)+')';
  end
  else
  if S is TWhile then
  begin
    result:=Keyword('while')+' ('+Expression(TWhile(S).Expression)+') '+CRLF+
            Statements(TWhile(S).WhilePart,IndentText);
  end
  else
  if S is TForLoop then
  begin
    result:=Keyword('for')+' ('+ForBody(TForLoop(S).Init)+';'+
            Expression(TForLoop(S).Condition)+';'+
            ForBody(TForLoop(S).Step)+')'+CRLF;

    result:=result+Statements(TForLoop(S).Loop,IndentText);
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
    result:=Keyword('throw');

    if TRaise(S).Expression<>nil then
       result:=result+' '+Expression(TRaise(S).Expression);
  end
  else
  if S is TOn then
  begin
    result:='('+AnchorOf(TOn(S).Exception.Name)+')'+CRLF+
       Indent+Statements(TOn(S).DoPart,Indent);

    if TOn(S).ElsePart<>nil then
       result:=result+CRLF+Indent+Keyword('else')+' '+Statements(TOn(S).ElsePart,Indent);
  end
  else
  if S is TStatements then
     result:=Statements(TStatements(S),Indent)
  else
     result:='? '+S.ClassName;
end;

function TJSEmit.Statements(const S: TStatements; const Indent: String;
  BeginEnd: Boolean): String;

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

function TJSEmit.Variable(const AVar: TVariableDeclaration): String;
begin
  result:=AVar.Name;

  if AVar.Value<>nil then
     result:=result+'='+Block(AVar.Value);
end;

function TJSEmit.Variables(const AVars: TVariableDeclarations): String;
var t : Integer;
begin
  result:='';

  for t:=0 to AVars.Count-1 do
  begin
    result:=result+Variable(AVars[t])+CRLF;

    if t<AVars.Count-1 then
       result:=result+','+CRLF;
  end;

  if result<>'' then
     result:=result+';';
end;

end.
