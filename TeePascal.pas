unit TeePascal;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0}
     {$DEFINE HAS_INLINE}
     {$DEFINE D9}
  {$ENDIF}
  {$IF CompilerVersion >= 20.0}
     {$DEFINE HAS_CHARINSET}
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUX64}
{$EXCESSPRECISION OFF}
{$ENDIF}

uses
  Classes, TeeEvaluate, TeeCode;

type
  TIncludeFile = record
    FileName : String;
    Content : String;
  end;

  TIncludeFiles=class
  public
    Includes : Array of TIncludeFile;

    function Add(const AFile,APath:String):String;
    procedure Clear;
    function Count:Integer;
    function Find(const AFile:String):String;
  end;

  TIDEPlatform=(ipWin32, ipWin64);

  TIDE=class
  private
    RegCompany : String;

    Constructor CreateStatic;

    function IDEToString:String;
    function IsLazarus:Boolean;
    procedure Prepare(const AVersion:Integer);
    function SourcePath:String;
    procedure VersionError(const AMessage:String);
  public
    CompilerVersion : Integer;
    IDEPlatform : TIDEPlatform;

    UserDefines,
    UserPaths,
    UserScopes,

    Defines,
    UnitScopes,
    Paths:TStringList;

    Company,
    Product,
    Version,
    Description,
    Source : String;

    Constructor Create(const ACompilerVersion:Integer;
                       const APlatform:TIDEPlatform=ipWin32);
    Destructor Destroy; override;

    class procedure Detect(const AList:TStrings);
    procedure GetPaths;
    function HasScopes:Boolean;
    function RootDir:String;
  end;

  TBasePascalParser=class(TBaseParser)
  private
    AlignSize,
    MinEnumSize : Integer;

    Eval : TEvaluator;
    FOnCall  : TCallEvent;

    //function AtPosition:String;
    function CompilerOptions(const AOpt,Arg:String):Boolean;
    function CompilerOptionsLong(const ADefine,Arg:String):Boolean;
    procedure EvalGet(Sender:TObject; const AIdentifier:String; out AValue:String);
    procedure EvalCall(Sender:TObject; const AFunction:TFunction; out AValue:Variant);
    function IsDefined(const D:String):Boolean;
    function IsExpressionTrue(const AExp:String):Boolean;
    function IsOption(const O:String):Boolean;
    procedure NewResource(const AResource:String);
    procedure ParseEnumSize(const AValue:String);
    function PeekDefine:Boolean;
    function PopDefine:Boolean;
    procedure ProcessDefine(const ADefine:String);
    procedure ProcessMessage(const AMessage:String);
    procedure PushDefine(const Value:Boolean);

    function ValidOption(const O:Char):Boolean;
  protected
    Defines,
    Options : TStrings;

    procedure AddGlobals(const Globals:TBlock; const CompilerVersion:Integer); override;
    procedure Increment; override;
  public
    MergeIncludes,
    KeepComments : Boolean;

    Constructor Create(const AOwner:TBlock; const ALanguage:TLanguage); override;
    Destructor Destroy; override;

    procedure FromPathFile(const AFile:String);
    function Parse:String; overload;
    function Parse(const AText:String):String; overload;

    property OnCall:TCallEvent read FOnCall write FOnCall;
  end;

  TPascalParse=class(TBasePascalParser)
  private
    {$IFNDEF D9}
    class function ExpressionRequire(const AOwner:TBlock):TExpression; overload;
    {$ENDIF}

    function ExpressionRequire(const AOwner:TBlock;
                           const Terminators:{$IFDEF D9}TStringArray=[]{$ELSE}Array of String{$ENDIF};
                           const AKind:TSymbolKind=skField;
                           const OnlyConstants:Boolean=False;
                           const AddressOf:Boolean=False;
                           const AResult:TTypeSpecification=nil):TExpression; overload;

    function GuessSymbol(const ASymbol:TFindSymbol):TExpression;
    function GuessTypeDeclaration(const AOwner:TBlock):TTypeDeclaration;
    function GuessTypeIdentifier(const AOwner:TBlock):TExpression;
    procedure GuessTypeParams(const AOwner:TBlock; out AParams:TTypeParameters; const Expand:Boolean=False);
    function GetTypedType(const AOwner:TBlock; const AKind:TSymbolKind; const S:String):TExpression;
    function InternalGetTypeIdentifier(const AOwner:TBlock; out AParams:TTypeParameters; Expand:Boolean=False):String;
    function InternalGetIdentifier(const AOwner:TBlock):String;

    function ParseArrayType(const AOwner:TBlock; const AParams:TTypeParameters):TArraySpecification;
    function ParseDefaultValueOf(const AOwner:TBlock; const AType:TTypeDeclaration; const AIndex:Integer=0):TExpression;

    function ParseExpressions(const AOwner:TBlock;
                          const Terminators:{$IFDEF D9}TStringArray=[]{$ELSE}Array of String{$ENDIF};
                          const OnlyConstants:Boolean=False):TExpressions; overload;

    procedure ParseTypeExpression(const AOwner:TBlock;
                          out ForwardExpression:String;
                          var AType:TTypeDeclaration;
                          const ATypeOwner:TBlock;
                          const AName:String;
                          const AParams:TExpressions=nil);

    {$IFNDEF D9}
    class function ParseExpressions(const AOwner:TBlock):TExpressions; overload;
    {$ENDIF}

    function ParseNewMethod(const AOwner:TBlock;
                        const Anonymous:Boolean;
                        const AIndex:Integer;
                        const AMethods:TMethodDeclarations;
                        const ABodies:Boolean;
                        out tmpInst:TMethodSpecification):TMethodDeclaration;

    function RequireCallOrCast(const AScope,AOwner:TBlock;
                           AParent:TExpression;
                           const AResult:TTypeSpecification;
                           const S:String;
                           const AVar:TExpression;
                           const tmpTypeParams:TTypeParameters):TExpression;

    function RequireIdentifier:String;

    function TryGuessTypeParams(const AScope:TBlock):TTypeParameters;

    procedure DoImplementation(const AModule:TImplementationModule);
    function FindScopeOrUnit(const S:String):TExpression;
    procedure Finish(const AUses:TUses);
    function ForceUseUnit(const AImpl:TImplementationModule; const AOwner:TUsesSection; const AName:String):TUnit;
    procedure GetName(const AModule:TModule; const APrefix:String);
    function GuessKind(const AName: String):TRecordOperatorKind;
    function RequireStatements(const AOwner:TBlock):TStatements;
    function SkipStringRange(const S:String):String;

    // TBlock parsers:

    procedure ParseASM(const ASMS:TASMStatements);
    procedure ParseAssignment(const Assignment:TAssignment);
    procedure ParseArray(const ArraySpec:TArraySpecification);
    procedure ParseArrayValues(const Values:TArrayValues);
    function ParseAttribute(const AOwner:TBlock):TAttribute;
    procedure ParseAttributeClass(const Attribute:TAttribute);
    procedure ParseBody(const Body:TBody);
    procedure ParseCase(const ACase:TCase);
    procedure ParseCaseVariable(const CaseVariable:TCaseVariable);
    procedure ParseClass(const ClassSpec:TClassSpecification);
    procedure ParseConstant(const Constant:TConstantDeclaration);
    procedure ParseConstants(const Constants:TConstantDeclarations);
    procedure ParseDestructor(const ADestructor:TDestructorSpecification);
    procedure ParseDirectives(const AIdentifier:TIdentifier);
    function ParseFields(const AOwner:TBlock;
                     const AFields:TVariableDeclarations;
                     OptionalType:Boolean;
                     const FieldKind:TParseFieldKind=fkField;
                     const AVisibility:TVisibility=vPublic;
                     const AIsClass:Boolean=False):Boolean;
    procedure ParseFor(const AFor:TFor);
    procedure ParseFunction(const AFunction:TFunctionSpecification);
    procedure ParseGoto(const AGoto:TGoto);
    procedure ParseIf(const AIf:TIf);
    procedure ParseImplementation(const AUnit:TUnit);
    procedure ParseItemExpression(const Item:TItemExpression);
    procedure ParseLabel(const ALabel:TLabelDeclaration);
    procedure ParseLabels(const Labels:TLabelDeclarations);
    procedure ParseLibrary(const ALibrary:TLibrary);
    procedure ParseMain(const AModule:TNativeModule);
    procedure ParseMethodDirectives(const AMethod:TMethodSpecification);
    procedure ParseMethodParams(const Method:TMethodParams);
    procedure ParseMethodSpec(const AMethod:TMethodSpecification);
    procedure ParseModuleSection(const AModule:TNativeModule);
    procedure ParseOn(const AOn:TOn);
    procedure ParsePackage(const APackage:TPackage);
    procedure ParsePackages(const APackages:TPackages);
    procedure ParseProgram(const AProgram:TProgram);
    procedure ParseProperty(const Prop:TPropertyDeclaration);
    procedure ParseRaise(const ARaise:TRaise);
    procedure ParseRecord(const RecordSpec:TRecordSpecification);
    procedure ParseRecordOperator(const AOperator:TRecordOperatorSpec);
    procedure ParseRepeat(const ARepeat:TRepeat);
    procedure ParseResourceStrings(const Strings:TResourceStrings);
    procedure ParseSection(const Section:TSection);
    procedure ParseSetSpecification(const SetSpec:TSetSpecification);
    procedure ParseStatementItems(const Statements:TStatements; Max:Integer=0);
    procedure ParseStatements(const Statements:TStatements);
    procedure ParseStringFormat(const Format:TStringFormatExpression);
    procedure ParseTry(const ATry:TTry);
    procedure ParseTypeAndValue(const Constant:TConstantDeclaration);
    procedure ParseTypes(const Types:TTypeDeclarations);
    procedure ParseUnit(const AUnit:TUnit);
    procedure ParseUses(const AUses:TUses);
    procedure ParseVariables(const Variables:TVariableDeclarations);
    procedure ParseWhile(const AWhile:TWhile);
    procedure ParseWith(const AWith:TWith);
  public
    procedure GetDefine(Sender:TObject; const AIdentifier:String; out AValue:String); override;
  end;

  TPascal=class(TLanguage)
  private
    FIDE : TIDE;

    function GetIncludeFile(const AFile:String):String;
    function GetParser(const AOwner:TBlock):TBaseParser;
    function GuessFullPathOf(const AFile:String):String;
  protected
    procedure DoParse(const AModule:TModule; const AFileName:String); override;
    procedure InternalParseModule(const AParser:TBaseParser; const AModule:TModule); override;
    function ParserFromFile(const AOwner:TBlock; const AFile:String):TBaseParser; override;
    function ParserFromText(const AOwner:TBlock; const AText:String):TBaseParser;
  public
    Includes : TIncludeFiles;
    Resources : TStrings;

    Constructor CreateVersion(const ACompilerVersion:Integer=0);
    Destructor Destroy; override;

    procedure AddScopes;
    procedure Clear; override;
    function FindModuleOrScope(const AOrigin:TBlock; const AName:String):TExpression; override;
    function FullPathOf(var AName:String; const AExtension:String):String; override;

    function ParsePackage(const AName: String): TPackage;
    function ParseProgram(const AFileName: String): TProgram;
    function ParseUnit(const AFileName:String):TUnit;

    property IDE:TIDE read FIDE;
  end;

implementation

uses
  Windows, Variants, Registry, SysUtils;

{$IFOPT D+}
var
  CurrentUnit,
  CurrentUnitIMP : String;
{$ENDIF}

{ TBasePascalParser }

Constructor TBasePascalParser.Create(const AOwner:TBlock; const ALanguage:TLanguage);
var tmp : TIDE;
begin
  inherited;

  Eval:=TEvaluator.Create;
  Eval.OnCall:=EvalCall;
  Eval.OnGet:=EvalGet;

  AlignSize:=8;
  MinEnumSize:=4;

  MergeIncludes:=True;

  Options:=TStringList.Create;

  Defines:=TStringList.Create;
  TStringList(Defines).CaseSensitive:=False;

  tmp:=(ALanguage as TPascal).IDE;

  Defines.AddStrings(tmp.Defines);
  Defines.AddStrings(tmp.UserDefines);
end;

Destructor TBasePascalParser.Destroy;
begin
  Defines.Free;
  Options.Free;
  Eval.Free;

  inherited;
end;

{
function TBasePascalParser.AtPosition: String;
begin
  result:='At file: '+CurrentFile+' line: '+IntToStr(State.Position.Line)+
          ' column: '+IntToStr(State.Position.Column);
end;
}

procedure TBasePascalParser.EvalGet(Sender: TObject; const AIdentifier: String; out AValue: String);
var S: String;
begin
  S:=UpperCase(Trim(AIdentifier));

  // Pending to merge parsers !
  if (S='GENERICVARIANTS') or (S='GENERICSAFEARRAYS') or (S='GENERICOPERATIONS') then
     AValue:='False'
  else
    GetDefine(Self,AIdentifier,AValue);
end;

procedure TBasePascalParser.EvalCall(Sender: TObject; const AFunction:TFunction; out AValue: Variant);

(*
  function IsDeclared(const D:String):Boolean;
  var tmp : Variant;
  begin
    if Assigned(FOnGet) then
    begin
      FOnGet(Self,D,tmp);
      result:=not VarIsNull(tmp);
    end
    else
      result:=False;
  end;
*)

  // Pending: Allow SizeOf of external non-intrinsic types (ie: Records etc)
  function GetSizeOf(const S:String):Integer;
  begin
    if S='BYTE' then result:=1 else
    if S='WORD' then result:=2 else
    if S='INTEGER' then result:=4 else
    if S='INT64' then result:=8 else
    if S='NATIVEINT' then result:=4 else
    if S='NATIVEUINT' then result:=4 else
    if S='LONGINT' then result:=4 else
    if S='SINGLE' then result:=4 else
    if S='DOUBLE' then result:=8 else
    if S='EXTENDED' then result:=10 else
    if S='BOOLEAN' then result:=1 else
    if S='INTPTR' then result:=4 else
    if S='POINTER' then result:=4 // Platform !! 64bit = 8
    else
    if S='TVALUETYPE' then result:=1 // <--- Classes.pas, pending parsing
    else
    begin
      result:=0;
      Error('Cannot determine SizeOf of: '+S);
    end;
  end;

var S   : String;
    tmp : Variant;
begin
  if Length(AFunction.Parameters)<>1 then
     Error('Wrong number of parameters for function: '+AFunction.Name);

  S:=UpperCase(Trim(AFunction.Name));

  if S='DEFINED' then
     AValue:=IsDefined(Trim(AFunction.Parameters[0].Text))
  else
  if S='DECLARED' then
  begin
    tmp:=AFunction.Parameters[0].Value;

    AValue:=(not VarIsNull(tmp)) and (tmp<>'');
  end
  else
  if S='SIZEOF' then
     AValue:=GetSizeOf(UpperCase(Trim(AFunction.Parameters[0].Text)))
  else
     Error('Unknown function: '+AFunction.Name);
end;

function TBasePascalParser.IsDefined(const D:String):Boolean;
begin
  // Protection against superflous ")" at the end
  if Copy(D,Length(D),1)=')' then
     result:=Defines.IndexOf(Copy(D,1,Length(D)-1))<>-1
  else
     result:=Defines.IndexOf(D)<>-1;
end;

procedure GetFilesFromDir(const APath:String; const AList:TBaseList);

  procedure Add(const APath:String);
  var S : TSearchRec;
  begin
    if FindFirst(APath+'\*.*',faAnyFile,S)=0 then
    try
      repeat
        if (S.Attr and faDirectory)<>faDirectory then
            AList.AddItem(S.Name,nil);

      until FindNext(S)<>0;
    finally
      FindClose(S);
    end;
  end;

begin
  Add(APath);
end;

const
  NumOptions=25;
  NumLongOptions=32;

  AlignOption=14;
  DescriptionOption=26;
  DesignOnlyOption=27;
  ImplicitBuildOption=28;
  HintsOption=29;
  ResourceOption=30;
  RunOnlyOption=31;

  OptionsShort:Array[0..NumOptions-1] of Char=
          ('O','C','H','I','R','Q','S','D','T','X','W','P','Z','L','A','J',
           'B','M','N','F','K','G','Y','U','V');
  OptionsLong:Array[0..NumLongOptions-1] of String=
          ('OPTIMIZATION',
           'ASSERTIONS',
           'LONGSTRINGS',
           'IOCHECKS',
           'RANGECHECKS',
           'OVERFLOWCHECKS',
           '',
           'DEBUGINFO',
           'TYPEDADDRESS',
           'EXTENDEDSYNTAX',
           'STACKFRAMES',
           'OPENSTRINGS',
           '',
           'LOCALSYMBOLS',
           'ALIGN',
           'WRITEABLECONST',
           'BOOLEVAL',
           '',
           '',
           '',
           'STACKCHECK',
           'IMPORTEDDATA',
           'REFERENCEINFO',
           'SAFEDIVIDE',
           'VARSTRINGCHECKS',
           'EXCESSPRECISION',
           'DESCRIPTION',
           'DESIGNONLY',
           'IMPLICITBUILD',
           'HINTS',
           'RESOURCE',
           'RUNONLY'
           );

function TBasePascalParser.ValidOption(const O:Char):Boolean;
var t : Integer;
begin
  for t:=Low(OptionsShort) to High(OptionsShort) do
      if OptionsShort[t]=O then
      begin
        result:=True;
        Exit;
      end;

  result:=False; //  result:=O in OptionsShort;
end;

function TBasePascalParser.IsOption(const O:String):Boolean;
var tmp : String;
begin
  if Length(O)<>2 then
     Error('Wrong $OPT, length not two characters: '+O);

  tmp:=O[1];

  if not ValidOption(tmp[1]) then
     Error('Wrong $OPT: '+O);

  result:=Options.IndexOf(tmp)<>-1;

  if O[2]='-' then
     result:=not result;
end;

procedure TBasePascalParser.PushDefine(const Value:Boolean);
var tmp : TTriBoolean;
begin
  if Value then
     tmp:=triTrue
  else
     tmp:=triFalse;

  State.DefinesStack[State.DefinesStackCount]:=tmp;
  Inc(State.DefinesStackCount);

  // Pending: Check DefinesStackCount <= 9999
end;

function TBasePascalParser.PeekDefine:Boolean;
var tmp : TTriBoolean;
begin
  if State.DefinesStackCount=0 then
     Error('Wrong {$else}');

  tmp:=State.DefinesStack[State.DefinesStackCount-1];

  result:=(tmp=triTrue) or (tmp=triTrueElse);
end;

function TBasePascalParser.PopDefine:Boolean;
begin
  if State.DefinesStackCount=0 then
     Error('Wrong {$else}');

  case State.DefinesStack[State.DefinesStackCount-1] of
    triTrue,
    triFalseElse : result:=True;
  else
    result:=False;
  end;

  Dec(State.DefinesStackCount);
end;

function GetPart(var S:String; const Delimiter:String):String;
var p : Integer;
begin
  p:=Pos(Delimiter,s);

  if p>0 then
  begin
    result:=Copy(s,1,p-1);
    Delete(s,1,p);

    s:=Trim(s);
  end
  else
  begin
    result:=s;
    s:='';
  end;
end;

function TBasePascalParser.IsExpressionTrue(const AExp:String):Boolean;

  function EvalPart(const APart:String):Boolean;
  var tmpNode : TBaseNode;
      tmp : Variant;
  begin
    Eval.Line:=State.Position.Line;

    tmpNode:=Eval.Parse(APart);
    try
      try
        tmp:=tmpNode.Value;

        if VarIsStr(tmp) then
           result:=IsDefined(tmp)
        else
           result:=tmp;
      except
        on E:Exception do
        begin
          result:=False;
          Error('Eval exception: '+E.Message+' evaluating: '+APart);
        end;
      end;
    finally
      tmpNode.Free;
    end;
  end;

begin
  result:=EvalPart(AExp);
end;

procedure TBasePascalParser.ParseEnumSize(const AValue:String);
begin
  if not TryStrToInt(AValue,MinEnumSize) then
     Error('Wrong MinEnumSize: '+AValue);
end;

function TBasePascalParser.CompilerOptions(const AOpt,Arg:String):Boolean;
var s,
    tmp,
    tmpValue : String;
    C : Char;
    i : Integer;
begin
  result:=True;

  s:=Trim(AOpt);

  repeat
    tmp:=UpperCase(GetPart(s,','));

    if tmp<>'' then
    begin
      C:=tmp[1];

      if ValidOption(C) then
      begin
        tmpValue:=Copy(tmp,2,Length(tmp));

        if tmpValue='+' then
        begin
          if Options.IndexOf(tmp)=-1 then
             Options.Add(tmp);
        end
        else
        if tmpValue='-' then
        begin
          i:=Options.IndexOf(tmp);

          if i<>-1 then
             Options.Delete(i);
        end
        else
        if C='Z' then
        begin
          ParseEnumSize(tmpValue);
        end
        else
        if C='A' then
        begin
          if not TryStrToInt(tmpValue,AlignSize) then
             Error('Wrong AlignSize: '+tmpValue);
        end
        else
        if C='C' then
        begin
          if not SameText(Arg,'PRELOAD') then // ??
             Error('Wrong $C directive: '+tmpValue);
        end
        else
        begin
          result:=False;
          break;
        end;

        result:=True;
      end
      else
      begin
        result:=False;
        break;
      end;
    end;

  until tmp='';
end;

function TBasePascalParser.CompilerOptionsLong(const ADefine,Arg:String):Boolean;

  function ValidAlign(const S:String):Boolean;
  var tmpAlign : Integer;
  begin
    result:=TryStrToInt(Arg,tmpAlign);

    if result then
       result:=(tmpAlign=1) or (tmpAlign=2) or (tmpAlign=4) or (tmpAlign=8) or (tmpAlign=16);
  end;

var S : String;
    t,i : Integer;
begin
  result:=False;

  S:=UpperCase(ADefine);

  for t:=Low(OptionsLong) to High(OptionsLong) do
      if OptionsLong[t]=S then
      begin
        S:=UpperCase(Arg);

        if (t=AlignOption) and ValidAlign(Arg) then
        else
        if t=ResourceOption then
        begin
          if State.DefinesOk then
             NewResource(ADefine);
        end
        else
        if (t=DescriptionOption) or (t=DesignOnlyOption) or
           (t=ImplicitBuildOption) or (t=HintsOption) or
           (t=RunOnlyOption) then
        else
        if S='ON' then
        begin
          if Options.IndexOf(OptionsShort[t])=-1 then
             Options.Add(OptionsShort[t]);
        end
        else
        if S='OFF' then
        begin
          i:=Options.IndexOf(OptionsShort[t]);

          if i<>-1 then
             Options.Delete(i);
        end
        else
          Error('Unknown compiler option argument: '+ADefine+' '+Arg);

        result:=True;
        break;
      end;
end;

procedure TBasePascalParser.ProcessMessage(const AMessage:String);
var i : Integer;
begin
  i:=Pos(' ',AMessage);

  if i>0 then
  begin
    if SameText(Trim(Copy(AMessage,1,i-1)),'ERROR') then
       Error('Error: '+AMessage);
  end
  {
  else
  if Verbose then
     Verbose(AMessage);
  }
end;

function StripQuotes(const S:String):String;
var L : Integer;
    C : Char;
begin
  result:=S;

  if result<>'' then
  begin
    C:=result[1];

    if (C='"') or (C='''') then
       Delete(result,1,1);

    L:=Length(result);

    if L>0 then
    begin
      C:=result[L];

      if (C='"') or (C='''') then
         Delete(result,L,1);
    end;
  end;
end;

procedure TBasePascalParser.NewResource(const AResource:String);
var i : Integer;
begin
  NewDirective(Self,dkResource,AResource);
  (*
  if Assigned(FOnDirective) then
     FOnDirective(Self,dkResource,AResource {+' '+Arg});
  *)

  i:=Pos(' ',AResource);
  TPascal(Language).Resources.Add(Trim(Copy(AResource,i,Length(AResource))));
end;

procedure TBasePascalParser.ProcessDefine(const ADefine:String);
var
  tmp,
  tmpArg : String;

  procedure ProcessInclude;
  var tmpInc : String;
      tmpPos : Integer;
  begin
    if MergeIncludes then
    begin
      // *.inc
      if Copy(tmpArg,1,2)='*.' then
         tmpArg:=ChangeFileExt(ExtractFileName(CurrentFile),Copy(tmpArg,2,Length(tmpArg)));

      tmpInc:=TPascal(Language).GetIncludeFile(StripQuotes(tmpArg));

      tmpPos:=State.Position.Current;

      // Pending:
      // Process "tmpInc" text separately, to avoid incrementing State.Line
      // with the number of lines in tmpInc include.

      FText:=Copy(FText,1,State.StartDefine-1)+tmpInc+Copy(FText,tmpPos+1,L);
      L:=Length(FText);

      State.Position.Current:=State.StartDefine-1;
    end;
  end;

  procedure SplitDefine(const APos:Integer);
  begin
    tmp:=Copy(ADefine,1,APos-1);
    tmpArg:=Trim(Copy(ADefine,APos+1,Length(ADefine)));
  end;

  function OnOff:Boolean;
  begin
    result:=SameText(tmpArg,'ON');
  end;

var i : Integer;
    tmpIndex : Integer;
begin
  tmpArg:='';

  i:=Pos(' ',ADefine);

  if i=0 then
  begin
    i:=Pos(#9,ADefine);

    if i=0 then
       tmp:=ADefine
    else
       SplitDefine(i);
  end
  else
    SplitDefine(i);

  tmp:=UpperCase(tmp);

  //result:=result+'==='+tmp+'===';

  if (tmp='I') or (tmp='INCLUDE') then
  begin
    if State.DefinesOk then
    begin
      NewDirective(Self,dkInclude,ADefine);
      (*
      if Assigned(FOnDirective) then
         FOnDirective(Self,dkInclude,ADefine);
      *)

      ProcessInclude;
    end;
  end
  else
  if tmp='DEFINE' then
  begin
    if State.DefinesOk then
    begin
      tmpArg:=UpperCase(tmpArg);

      if Defines.IndexOf(tmpArg)=-1 then
         Defines.Add(tmpArg);
    end;
  end
  else
  if tmp='NODEFINE' then
     // C++ hpp
  else
  if tmp='UNDEF' then
  begin
    if State.DefinesOk then
    begin
      tmpArg:=UpperCase(tmpArg);

      tmpIndex:=Defines.IndexOf(tmpArg);

      if tmpIndex<>-1 then
         Defines.Delete(tmpIndex);
    end;
  end
  else
  if tmp='IFDEF' then
  begin
    PushDefine(State.DefinesOk);

    if State.DefinesOk then
       State.DefinesOk:=IsDefined(tmpArg);
  end
  else
  if tmp='IF' then
  begin
    PushDefine(State.DefinesOk);

    if State.DefinesOk then
       State.DefinesOk:=IsExpressionTrue(tmpArg);
  end
  else
  if tmp='IFOPT' then
  begin
    PushDefine(State.DefinesOk);

    if State.DefinesOk then
       State.DefinesOk:=IsOption(UpperCase(tmpArg));
  end
  else
  if tmp='IFNDEF' then
  begin
    PushDefine(State.DefinesOk);

    if State.DefinesOk then
       State.DefinesOk:=not IsDefined(tmpArg);
  end
  else
  if tmp='ELSEIF' then
  begin
    if PeekDefine then
    begin
      State.DefinesOk:=not State.DefinesOk;

      if State.DefinesOk then
         State.DefinesOk:=IsExpressionTrue(tmpArg)
      else
         State.DefinesStack[State.DefinesStackCount-1]:=triFalseElse;
    end;
  end
  else
  if tmp='ELSE' then
  begin
    if PeekDefine then
       State.DefinesOk:=not State.DefinesOk;
  end
  else
  if tmp='ENDIF' then
     State.DefinesOk:=PopDefine
  else
  if tmp='IFEND' then
     State.DefinesOk:=PopDefine
  else
  if tmp='LEGACYIFEND' then
  begin
    // Force old "IF <-> IFEND" instead of new "IF <--> ENDIF (or IFEND)"
  end
  else
  if tmp='R' then
  begin
    // Add resource to link
    if State.DefinesOk then
       NewResource(ADefine);
  end
  else
  if tmp='L' then
  begin
    // Link object file: tmpArg
    if State.DefinesOk then
       NewDirective(Self,dkLink,ADefine);
       (*
       if Assigned(FOnDirective) then
          FOnDirective(Self,dkLink,ADefine);
          *)
  end
  else
  begin
    if tmp='OBJTYPENAME' then
       // C++
    else
    if tmp='APPTYPE' then // CONSOLE .. GUI
    else
    if tmp='INLINE' then // AUTO ON OFF
    else
    if tmp='POINTERMATH' then // ON OFF
       PointerMath:=SameText(tmpArg,'ON')
    else
    if tmp='VARPROPSETTER' then // ON OFF, since Delphi 6
    else
    if tmp='RTTI' then // RAD Studio 7.0 2010
    else
    if tmp='EXTERNALSYM' then
    begin
      // C++ do not add symbol to hpp
    end
    else
    if tmp='NOINCLUDE' then
    begin
      // C++ hpp: do not add unit as include: tmpArg
    end
    else
    if tmp='HPPEMIT' then
    begin
      // C++ hpp: tmpArg
    end
    else
    if tmp='MINENUMSIZE' then
    begin
      if State.DefinesOk then
         ParseEnumSize(tmpArg);
    end
    else
    if tmp='WARNINGS' then
    begin
      // Enable / Disable all Warning messages: tmpArg
    end
    else
    if tmp='WARN' then
    begin
      // Enable / Disable Warning message: tmpArg
    end
    else
    if tmp='WEAKPACKAGEUNIT' then
    begin
      // ??
    end
    else
    if tmp='DENYPACKAGEUNIT' then
    begin
      // ??
    end
    else
    if tmp='PACKRECORDS' then
    begin
      // ??
    end
    else
    if tmp='IMAGEBASE' then
    begin
      // ??
    end
    else
    if tmp='MESSAGE' then
    begin
      if State.DefinesOk then
         ProcessMessage(tmpArg);
    end
    else
    if tmp='METHODINFO' then
    begin
      // ON / OFF
    end
    else
    if tmp='ZEROBASEDSTRINGS' then
    begin
      // ON / OFF
    end
    else
    if tmp='UNSAFECODE' then
    begin
      // ON / OFF
    end
    else
    if tmp='REGION' then
    begin
      // Editor
    end
    else
    if tmp='ENDREGION' then
    begin
      // Editor
    end
    else
    if tmp='SCOPEDENUMS' then
       ScopedEnums:=OnOff
    else
    if tmp='OBJEXPORTALL' then
    begin
      // BCB
    end
    else
    if IsFPC and (tmp='MODE') then
    else
    if IsFPC and (tmp='MAXFPUREGISTERS') then
    else
    (* CLR
    if tmp='FINITEFLOAT' then
    begin
      // Precision
    end
    else
    *)
    if not CompilerOptionsLong(tmp,tmpArg) then
       if not CompilerOptions(tmp,tmpArg) then
          if State.DefinesOk then
             Error('Unknown conditional directive: $'+tmp);

    if State.DefinesOk then
       NewDirective(Self,dkUnknown,ADefine);
       (*
       if Assigned(FOnDirective) then
          FOnDirective(Self,dkUnknown,ADefine);
       *)
  end;
end;

procedure TBasePascalParser.Increment;
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
      if (C=')') and PrevCharIs('*') then
         State.InBlockComment:=False;

      if KeepComments and State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if State.InComment then
    begin
      if C='}' then
         State.InComment:=False;

      if KeepComments and State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if State.StartDefine>0 then
    begin
      if C='}' then
      begin
        ProcessDefine(Trim(State.Define));

        State.StartDefine:=0;

        State.C:=' ';  // <-- ie:  "Width{$IFDEF FMX}div 2" (space before "d" of "div")
        Exit;
      end
      else
        State.Define:=State.Define+C;
    end
    else
    if State.InStringQuote then
    begin
      State.InStringQuote:=False;

      if State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if State.InString then
    begin
      if C='''' then
         if NextCharIs('''') then
            State.InStringQuote:=True
         else
            State.InString:=False;

      if State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if (C='''') and (not IgnoreStrings) then
    begin
      State.InString:=True;

      if State.DefinesOk then
      begin
        State.C:=C;
        Exit;
      end;
    end
    else
    if (C='{') and NextCharIs('$') then
    begin
      State.Define:='';
      State.StartDefine:=State.Position.Current;

      Inc(State.Position.Current);
      Inc(State.Position.Column);
    end
    else
    if C='{' then
    begin
      State.InComment:=True;

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
    if (C='(') and NextCharIs('*') and (not IgnoreStrings) then
       State.InBlockComment:=True
    else
    if State.DefinesOk then
    begin
      State.C:=C;
      Exit;
    end;

  until Eof;
end;

function TBasePascalParser.Parse:String;
begin
  result:='';
  Init;

  while State.Position.Current<=L do
  begin
    result:=result+Next;
    Increment;
  end;

  if State.DefinesStackCount<>0 then
     Error('Internal: Invalid defines stack');
end;

function TBasePascalParser.Parse(const AText:String):String;
begin
  Text:=AText;
  result:=Parse;
end;

procedure TBasePascalParser.FromPathFile(const AFile:String);
var S : String;
    tmp : String;
begin
  tmp:=AFile;
  S:=Language.FullPathOf(tmp,'');

  if S='' then
     Error('Cannot find '+AFile+' file in search paths')
  else
  begin
    CurrentFile:=AFile;
    CurrentPath:=S;

    Text:=TextOf(S);
  end;
end;

{ TIncludeFiles }

procedure TIncludeFiles.Clear;
begin
  Includes:=nil;
end;

function TIncludeFiles.Count: Integer;
begin
  result:=Length(Includes);
end;

function TIncludeFiles.Find(const AFile:String):String;
var t : Integer;
begin
  for t:=Low(Includes) to High(Includes) do
      if SameText(AFile,Includes[t].FileName) then
      begin
        result:=Includes[t].Content;
        Exit;
      end;

  result:='';
end;

function TIncludeFiles.Add(const AFile,APath:String):String;
var L : Integer;
begin
  L:=Length(Includes);
  SetLength(Includes,L+1);
  Includes[L].FileName:=AFile;

  result:=TBaseParser.TextOf(APath);
  Includes[L].Content:=result;
end;

const
  LazarusVersion=126;

type
  TPascalIntrinsics=class(TIntrinsics)
  private
    procedure AddIntrinsics(const CompilerVersion:Integer);
  end;

procedure TPascalIntrinsics.AddIntrinsics(const CompilerVersion:Integer);
var
  AnsiCharType,
  ArrayType,
  ArrayOrSetOrRangeType,
  ArrayOrStringOrPCharType,
  ArrayOrStringType,
  FileType,
  IntegerOrPointerType,
  PCharType,
  PointerOrMethodType,
  WideCharType : TTypeDeclaration;

  AnsiStringType,
  //ByteBool,
  //CompType,
  //CurrencyType,
  //LongBool,
  LongWordType,
  //NativeInt,
  //NativeUInt,
  //OleVariantType,
  //OpenStringType,
  //PAnsiCharType,
  //PWideCharType,
  RealType
  //ShortInt,
  //ShortStringType,
  //SmallInt,
  //UInt64,
  //UnicodeStringType,
  //WideStringType,
  //WordBool
  : TTypeDeclaration;

  TextSpec : TTypeSpecification;

var
  EmptyString : TString;
  One : TNumber;

var
  MaxIntConst,
  MaxLongintConst : TNumber;

  PiConst : TFloatNumber;

  HInstanceConst : TVariableDeclaration;

  //LongintType : TTypeDeclaration;

  function VariantResult:TVariableDeclaration;
  begin
    result:=ResultOf(VariantType);
  end;

  function IntegerResult:TVariableDeclaration;
  begin
    result:=ResultOf(IntegerType);
  end;

  function IntegerOrSetResult:TVariableDeclaration;
  begin
    result:=ResultOf(IntegerOrSetType);
  end;

  function BooleanResult:TVariableDeclaration;
  begin
    result:=ResultOf(BooleanType);
  end;

  function WordResult:TVariableDeclaration;
  begin
    result:=ResultOf(WordType);
  end;

  function OrdinalResult:TVariableDeclaration;
  begin
    result:=ResultOf(OrdinalSpec);
  end;

  function RealResult:TVariableDeclaration;
  begin
    result:=ResultOf(RealType);
  end;

  function StringResult:TVariableDeclaration;
  begin
    result:=ResultOf(StringType);
  end;

  function ExtendedResult:TVariableDeclaration;
  begin
    result:=ResultOf(ExtendedType);
  end;

  function PointerResult:TVariableDeclaration;
  begin
    result:=ResultOf(PointerSpec);
  end;

  function RunErrorBlock:TMethodDeclaration;
  begin
    // Method alias:
    // 'RunError',System.'_RunError'

    result:=GlobalProcedure('RunError');
    AddParam(result,WordType,'Code');
  end;

  function IncBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Inc');

    AddParam(result,OrdinalType,'Dest');
    AddParam(result,IntegerType,'Amount').Value:=One;
  end;

  function DecBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Dec');

    AddParam(result,OrdinalType,'Dest');
    AddParam(result,IntegerType,'Amount').Value:=One;
  end;

  function SliceBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Slice',ResultOf(ArraySpec));
    AddParam(result,ArrayType,'Address').IsVar:=True;
    AddParam(result,IntegerType,'Count');
  end;

  function SetLengthBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('SetLength');
    AddParam(result,ArrayOrStringType,'Dest');
    AddParam(result,IntegerType,'Amount');

    // For multi-dim arrays:
    AddParam(result,IntegerType,'Amount2').Value:=Zero;
    AddParam(result,IntegerType,'Amount3').Value:=Zero;
    AddParam(result,IntegerType,'Amount4').Value:=Zero;
    AddParam(result,IntegerType,'Amount5').Value:=Zero;
  end;

  function SetStringBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('SetString');

    AddParam(result,StringType,'S').IsVar:=True;
    AddParam(result,PCharType ,'Buffer');
    AddParam(result,IntegerType,'Length');
  end;

  function FillCharBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('FillChar');

    AddParam(result,AnyType,'X');
    AddParam(result,IntegerType,'Count');
    AddParam(result,OrdinalType,'Value');
  end;

  function StrBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Str');
    AddParam(result,IntegerType,'X'); // optional: [:Width:[Decimals]]
    AddParam(result,StringType,'S').IsVar:=True;
  end;

  function ValBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Val');

    AddParam(result,StringType,'S');
    AddParam(result,AnyType,'V').IsVar:=True;
    AddParam(result,IntegerType,'Code').IsVar:=True;
  end;

  function NewBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('New',ResultOf(PointerSpec));
    AddParam(result,PointerType,'X').IsVar:=True;
  end;

  function SizeOfBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('SizeOf',IntegerResult);
    AddParam(result,AnyType,'Target'); // <-- better: AnyTypeType
  end;

  function LengthBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Length',IntegerResult);
    AddParam(result,ArrayOrStringOrPCharType,'Target');
  end;

  function AssignedBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Assigned',BooleanResult);
    AddParam(result,PointerOrMethodType,'Target');
  end;

  function CharResult:TVariableDeclaration;
  begin
    result:=TVariableDeclaration.CreateType(Globals,CharType);
  end;

  function ChrBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Chr',CharResult);
    AddParam(result,Byte_Type,'X'); // Byte !
  end;

  function OrdBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Ord',IntegerResult);
    AddParam(result,OrdinalType,'Target');
  end;

  function AbsBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Abs',RealResult);
    AddParam(result,RealType,'X');
  end;

  function TruncBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Trunc',RealResult);
    AddParam(result,RealType,'X');
  end;

  function FracBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Frac',RealResult);
    AddParam(result,RealType,'X');
  end;

  function IntBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Int',IntegerResult);
    AddParam(result,RealType,'X');
  end;

  function OddBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Odd',BooleanResult);
    AddParam(result,IntegerType,'X');
  end;

  function PtrBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Ptr',PointerResult);
    AddParam(result,IntegerType,'Address');
  end;

  function AddrBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Addr',PointerResult);
    AddParam(result,AnyType,'X').IsVar:=True;
  end;

  function RoundBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Round',IntegerResult);
    AddParam(result,RealType,'X');
  end;

  function ExpBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Exp',ExtendedResult);
    AddParam(result,RealType,'X');
  end;

  function SqrtBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Sqrt',ExtendedResult);
    AddParam(result,RealType,'X');
  end;

  function LnBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Ln',ExtendedResult);
    AddParam(result,RealType,'X');
  end;

  function ArcTanBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('ArcTan',ExtendedResult);
    AddParam(result,RealType,'X');
  end;

  function SqrBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Sqr',RealResult);
    AddParam(result,RealType,'X');
  end;

  function RandomBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Random',IntegerResult);
    AddParam(result,IntegerType,'Range').Value:=One;
  end;

  function SinBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Sin',ExtendedResult);
    AddParam(result,ExtendedType,'X');
  end;

  function CosBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Cos',ExtendedResult);
    AddParam(result,ExtendedType,'X');
  end;

  function HiBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Hi',WordResult);
    AddParam(result,IntegerType,'X');
  end;

  function HighBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('High',IntegerOrSetResult); // also: ShortString
    AddParam(result,ArrayOrSetOrRangeType,'X');
  end;

  function LowBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Low',IntegerOrSetResult); // also: String
    AddParam(result,ArrayOrSetOrRangeType,'X');
  end;

  function LoBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Lo',WordResult);
    AddParam(result,IntegerType,'X');
  end;

  function ReallocMemBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('ReallocMem',PointerResult);

    AddParam(result,PointerType,'P').IsVar:=True;
    AddParam(result,IntegerType,'NewSize');
  end;

  function GetMemBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('GetMem',PointerResult);

    AddParam(result,PointerType,'P').IsVar:=True;
    AddParam(result,IntegerType,'Size'); // Int64 ?
  end;

  function AssertBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Assert');

    AddParam(result,BooleanType,'Condition');
    AddParam(result,StringType,'Message').Value:=EmptyString;
  end;

  function FreeMemBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('FreeMem');

    AddParam(result,PointerType,'P');
    AddParam(result,IntegerType,'Size').Value:=Zero; // Int64 ?
  end;

  function ChDirBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('ChDir');
    AddParam(result,StringType,'Directory');
  end;

  function DisposeBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Dispose');
    AddParam(result,PointerType,'P').IsVar:=True;
  end;

  function CopyBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Copy',ResultOf(ArrayOrStringOrPChar));

    AddParam(result,ArrayOrStringOrPCharType,'S');
    AddParam(result,IntegerType,'Index').Value:=Zero;
    AddParam(result,IntegerType,'Count').Value:=MinusOne;

    // Pending: overload for array simple Copy(Foo) without Index and Count parameters
  end;

  function PosBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Pos',IntegerResult);

    AddParam(result,StringType,'SubStr');
    AddParam(result,StringType,'S');
  end;

  function InsertBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Insert');

    AddParam(result,StringType,'Substr');
    AddParam(result,StringType,'Dest').IsVar:=True;
    AddParam(result,IntegerType,'Index');
  end;

  procedure SetOverload(const AMethod:TMethodDeclaration);
  begin
    Include(TMethodSpecification(AMethod.TypeIdentifier.Expression).Directives,mdOverload);
  end;

  function DeleteBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Delete');

    AddParam(result,StringType,'S').IsVar:=True;
    AddParam(result,IntegerType,'Index');
    AddParam(result,IntegerType,'Count');

    if CompilerVersion>=290 then
       SetOverload(result);
  end;

  function DeleteArrayBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Delete');

    AddParam(result,ArrayType,'A').IsVar:=True;
    AddParam(result,IntegerType,'Index');
    AddParam(result,IntegerType,'Count');

    SetOverload(result);
  end;

  function ConcatBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Concat',StringResult);

    AddParam(result,StringType,'S1');
    AddParam(result,StringType,'S2');

    // ... SN
    TFunctionSpecification(result.TypeIdentifier.Expression).VariableParameters:=True;
  end;

  function GetDirBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('GetDir');

    AddParam(result,Byte_Type,'Drive');
    AddParam(result,StringType,'S').IsVar:=True;
  end;

  function HaltBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Halt');
    AddParam(result,IntegerType,'ExitCode').Value:=Zero;
  end;

  function IncludeBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Include');

    AddParam(result,SetType,'S').IsVar:=True;
    AddParam(result,OrdinalType,'I');
  end;

  function ExcludeBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Exclude');

    AddParam(result,SetType,'S').IsVar:=True;
    AddParam(result,OrdinalType,'I');
  end;

  function VarClearBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('VarClear');
    AddParam(result,VariantType,'V').IsVar:=True;
  end;

  function TypeInfoBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('TypeInfo',ResultOf(AnySpec));
    AddParam(result,AnyType,'T');
  end;

  function InitializeBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Initialize');

    AddParam(result,AnyType,'V').IsVar:=True;
    AddParam(result,IntegerType,'Count').Value:=Zero;
  end;

  function FinalizeBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Finalize');

    AddParam(result,AnyType,'V').IsVar:=True;
    AddParam(result,IntegerType,'Count').Value:=Zero;
  end;

  function VarArrayRedimBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('VarArrayRedim');

    AddParam(result,VariantType,'A').IsVar:=True;
    AddParam(result,IntegerType,'HighBound');
  end;

  function CloseBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Close');
    AddParam(result,FileType,'F').IsVar:=True;
  end;

  function PredBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Pred',OrdinalResult);
    AddParam(result,OrdinalType,'X');
  end;

  function SuccBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Succ',OrdinalResult);
    AddParam(result,OrdinalType,'X');
  end;

  function VarCastBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('VarCast');
    AddParam(result,VariantType,'Dest').IsVar:=True;
    AddParam(result,VariantType,'Source');
    AddParam(result,IntegerType,'VarType');
  end;

  function AssignFileBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('AssignFile',IntegerResult);
    AddParam(result,FileType,'File').IsVar:=True;
    AddParam(result,StringType,'FileName');
    AddParam(result,WordType,'CodePage').Value:=Zero;
  end;

  function AssignBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Assign',IntegerResult);
    AddParam(result,FileType,'File').IsVar:=True;
    AddParam(result,StringType,'FileName');
    AddParam(result,WordType,'CodePage').Value:=Zero;
  end;

  function CloseFileBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('CloseFile');
    AddParam(result,FileType,'File').IsVar:=True;
  end;

  function ResetBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Reset');
    AddParam(result,FileType,'File').IsVar:=True;
    AddParam(result,IntegerType,'RecSize').Value:=Zero;
  end;

  function RewriteBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Rewrite');
    AddParam(result,FileType,'File').IsVar:=True;
    AddParam(result,IntegerType,'RecSize').Value:=Zero;
  end;

  function ReadBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Read',ResultOf(StringType));
  end;

  function BlockReadWriteBlock(const AName:String):TMethodDeclaration;
  var tmp : TParameter;
  begin
    result:=GlobalFunction(AName,IntegerResult);
    AddParam(result,FileType,'File').IsVar:=True;
    AddParam(result,AnyType,'Buf').IsVar:=True;
    AddParam(result,IntegerType,'Count');

    tmp:=AddParam(result,IntegerType,'Result');
    tmp.Value:=Zero;
    tmp.IsVar:=True;
  end;

  function FilePosBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('FilePos',IntegerResult); // Longint
    AddParam(result,FileType,'File').IsVar:=True;
  end;

  function FileSizeBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('FileSize',IntegerResult);
    AddParam(result,FileType,'File').IsVar:=True;
  end;

  function EofBlock:TMethodDeclaration;
  var tmp : TParameter;
  begin
    result:=GlobalFunction('Eof',BooleanResult);

    tmp:=AddParam(result,FileType,'F');
    tmp.IsVar:=True;
    tmp.Value:=NilConst;
  end;

  function SeekBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Seek');
    AddParam(result,FileType,'F').IsVar:=True;
    AddParam(result,IntegerType,'N');
  end;

  function SeekEofBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('SeekEof',BooleanResult);
    AddParam(result,FileType,'F').IsVar:=True;
  end;

  function SeekEolnBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('SeekEoln',BooleanResult);
    AddParam(result,FileType,'F').IsVar:=True;
  end;

  function ReadLnBlock:TMethodDeclaration;
  var tmp : TParameter;
  begin
    result:=GlobalFunction('ReadLn',ResultOf(StringType));

    tmp:=AddParam(result,FileType,'F');
    tmp.IsVar:=True;
    tmp.Value:=NilConst;

    // Pending: "N" parameters (VarArgs)
    AddParam(result,StringType,'S1').Value:=EmptyString;
    AddParam(result,StringType,'S2').Value:=EmptyString;
    AddParam(result,StringType,'S3').Value:=EmptyString;
    AddParam(result,StringType,'S4').Value:=EmptyString;
  end;

  function WriteBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Write');

    AddParam(result,StringType,'S1').Value:=EmptyString;
    AddParam(result,StringType,'S2').Value:=EmptyString;
    AddParam(result,StringType,'S3').Value:=EmptyString;
    AddParam(result,StringType,'S4').Value:=EmptyString;

    SetOverload(result);
  end;

  function WriteTextBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('Write');

    AddParam(result,FileType,'Text').IsVar:=True;
    AddParam(result,StringType,'S1');
    AddParam(result,StringType,'S2').Value:=EmptyString;
    AddParam(result,StringType,'S3').Value:=EmptyString;
    AddParam(result,StringType,'S4').Value:=EmptyString;

    SetOverload(result);
  end;

  function WriteLnBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('WriteLn');

    AddParam(result,StringType,'S1').Value:=EmptyString;
    AddParam(result,StringType,'S2').Value:=EmptyString;
    AddParam(result,StringType,'S3').Value:=EmptyString;
    AddParam(result,StringType,'S4').Value:=EmptyString;

    SetOverload(result);
  end;

  function WriteLnTextBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('WriteLn');

    AddParam(result,FileType,'Text').IsVar:=True;
    AddParam(result,StringType,'S1').Value:=EmptyString;
    AddParam(result,StringType,'S2').Value:=EmptyString;
    AddParam(result,StringType,'S3').Value:=EmptyString;
    AddParam(result,StringType,'S4').Value:=EmptyString;

    SetOverload(result);
  end;

  function DefaultBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Default',ResultOf(AnyType));
    AddParam(result,AnyType,'X');
  end;

  function AtomicCmpExchangeBlock:TMethodDeclaration;
  var tmp : TParameter;
  begin
    result:=GlobalFunction('AtomicCmpExchange',ResultOf(IntegerOrPointerType));
    AddParam(result,AnyType,'Target').IsVar:=True;
    AddParam(result,IntegerOrPointerType,'NewValue');
    AddParam(result,IntegerOrPointerType,'Comparand');

    tmp:=AddParam(result,BooleanType,'Succeeded');
    tmp.IsOut:=True;
    tmp.Value:=Zero;
  end;

  function AtomicExchangeBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('AtomicExchange',ResultOf(IntegerOrPointerType));
    AddParam(result,AnyType,'A').IsVar:=True;
    AddParam(result,AnyType,'B').IsVar:=True;
  end;

  function AtomicIncrementBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('AtomicIncrement',IntegerResult);
    AddParam(result,IntegerType,'Value').IsVar:=True;
    AddParam(result,IntegerType,'Amount').Value:=One;
  end;

  function AtomicDecrementBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('AtomicDecrement',IntegerResult);
    AddParam(result,IntegerType,'Value').IsVar:=True;
  end;

  function SwapBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('Swap',IntegerResult);
    AddParam(result,IntegerType,'X');
  end;

  function IsManagedTypeBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('IsManagedType',BooleanResult);
    AddParam(result,AnyType,'T'); // <-- better: AnyTypeType
  end;

  function IsConstValueBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('IsConstValue',BooleanResult);
    AddParam(result,AnyType,'Value');
  end;

  function HasWeakRefBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('HasWeakRef',BooleanResult);
    AddParam(result,AnyType,'T'); // <--- "TypeIdentifier"
  end;

  function ReturnAddressBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('ReturnAddress',PointerResult);
  end;

  function IsProxyClassBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('IsProxyClass',BooleanResult);
    AddParam(result,AnyType,'X');
  end;

  function StringOfCharBlock:TMethodDeclaration;
  begin
    result:=GlobalFunction('StringOfChar',StringResult);
    AddParam(result,CharType,'Ch');
    AddParam(result,IntegerType,'Count');
  end;

  function GetTypeKindBlock:TMethodDeclaration;
  begin
    TypeKindType:=AddGlobalType('TTypeKind'); // <-- Temporary, see TeeCode

    result:=GlobalFunction('GetTypeKind',ResultOf(TypeKindType));
    AddParam(result,AnyType,'T'); // <--- "TypeIdentifier"
  end;

var ExitBlock,
    ContinueBlock,
    BreakBlock : TMethodDeclaration;

    ArrayOfByte,
    ArrayOfChar : TArraySpecification;

    CompilerVersionConst : TConstantDeclaration;
begin
  GUIDSpec:=nil;
  ObjectType:=nil;
  IInterfaceType:=nil;
  VarRecSpec:=nil;

  Globals.Init;

  NumberSpec:=TTypeSpecification.Create(Globals);
  IntegerType:=AddGlobalType('Integer',NumberSpec);

  CharSpec:=TTypeSpecification.Create(Globals);
  CharType:=AddGlobalType('Char',CharSpec);

  StringSpec:=TTypeSpecification.Create(Globals);

  {
  StringSpec.Expression:=CharType;
  SetLength(StringSpec.Dimensions,1);
  StringSpec.Dimensions[0]:=IntegerType;
  }

  ByteSpec:=NumberSpec;

  ArraySpec:=TArraySpecification.Create(Globals);
  NumberOrSetSpec:=TTypeSpecification.Create(Globals);
  AnySpec:=TTypeSpecification.Create(Globals);
  OrdinalSpec:=TTypeSpecification.Create(Globals);
  SetSpec:=TTypeSpecification.Create(Globals);
  PointerSpec:=TTypeSpecification.Create(Globals);

  // Hidden
  ArrayType:=TTypeDeclaration.CreateSpec(Globals,ArraySpec);
  ArrayOrString:=TArraySpecification.Create(Globals);
  ArrayOrStringOrPChar:=TTypeSpecification.Create(Globals);
  ArrayOrSetOrRange:=TTypeSpecification.Create(Globals);

  AnyType:=GlobalType(AnySpec);
  ArrayOrStringType:=GlobalType(ArrayOrString);
  ArrayOrStringOrPCharType:=GlobalType(ArrayOrStringOrPChar);
  ArrayOrSetOrRangeType:=GlobalType(ArrayOrSetOrRange);

  Int64Type:=AddGlobalType('Int64',NumberSpec);

  // TNumber after Int64Type:

  MinusOne:=TNumber.Create(Globals);
  MinusOne.Value:=-1;

  One:=TNumber.Create(Globals);
  One.Value:=1;

  Zero:=TNumber.Create(Globals);

  BreakBlock:=GlobalProcedure('Break');
  Globals.Methods.Add(BreakBlock);

  ContinueBlock:=GlobalProcedure('Continue');
  Globals.Methods.Add(ContinueBlock);

  // Pending:
  // 1) How to check Code parameter is compatible with function result value
  // 2) Exit is called only from inside a function body
  ExitBlock:=GlobalProcedure('Exit');
  AddParam(ExitBlock,AnyType,'Code').Value:=Zero;
  Globals.Methods.Add(ExitBlock);

  AnsiCharType:=AddGlobalType('AnsiChar',CharSpec);
  WideCharType:=AddGlobalType('WideChar',CharSpec);

  StringType:=AddGlobalType('String',StringSpec);

  EmptyString:=TString.Create(Globals);
  EmptyString.TypeIdentifier:=StringType;

  {ShortStringType:=}AddGlobalType('ShortString',StringSpec);
  AnsiStringType:=AddGlobalType('AnsiString',StringSpec);

  if CompilerVersion>=200 then
     AnsiStringType.HasCodePage:=True;

  if CompilerVersion>=210 then
     {OpenStringType:=}AddGlobalType('OpenString',StringSpec);

  {WideStringType:=}AddGlobalType('WideString',StringSpec);

  if (CompilerVersion>=200) or (CompilerVersion=LazarusVersion) then
     {UnicodeStringType:=}AddGlobalType('UnicodeString',StringSpec);

  PCharType:=AddGlobalPointerType('PChar',CharType);
  {PAnsiCharType:=}AddGlobalPointerType('PAnsiChar',AnsiCharType);
  {PWideCharType:=}AddGlobalPointerType('PWideChar',WideCharType);

  WordType:=AddGlobalType('Word',NumberSpec);
  Byte_Type:=AddGlobalType('Byte',ByteSpec);
  IntegerOrSetType:=GlobalType(NumberOrSetSpec);
  {ShortInt:=}AddGlobalType('ShortInt',NumberSpec);
  {SmallInt:=}AddGlobalType('SmallInt',NumberSpec);
  {UInt64:=}AddGlobalType('UInt64',NumberSpec);

  {NativeInt:=}AddGlobalType('NativeInt',NumberSpec);

  if CompilerVersion>=220 then
  begin
    {NativeUInt:=}AddGlobalType('NativeUInt',NumberSpec);

    ArrayOfByte:=TArraySpecification.Create(Globals);
    ArrayOfByte.Expression:=Byte_Type;
    AddGlobalType('TBytes',ArrayOfByte);

    ArrayOfChar:=TArraySpecification.Create(Globals);
    ArrayOfByte.Expression:=CharType;
    AddGlobalType('TCharArray',ArrayOfChar);
  end;

  LongWordType:=AddGlobalAliasType('LongWord',IntegerType);

  AddGlobalAliasType('Longint',IntegerType);
  AddGlobalAliasType('Cardinal',IntegerType);

  FloatSpec:=TTypeSpecification.Create(Globals);
  RealSpec:=FloatSpec; //TTypeSpecification.Create(Globals);

  RealType:=AddGlobalType('Real',RealSpec);

  OrdinalType:=GlobalType(OrdinalSpec);

  {$IFOPT D+}
  //OrdinalType.Name:='__ORDINAL__';
  {$ENDIF}

  PointerOrMethod:=TTypeSpecification.Create(Globals);
  PointerOrMethodType:=GlobalType(PointerOrMethod);

  IntegerOrPointer:=TTypeSpecification.Create(Globals);
  IntegerOrPointerType:=GlobalType(IntegerOrPointer);

  PointerType:=AddGlobalType('Pointer',PointerSpec);

  SetType:=GlobalType(SetSpec);

  TextSpec:=TTypeSpecification.Create(Globals);
  FileType:=AddGlobalType('Text',TextSpec);

  AddGlobalAliasType('TextFile',FileType);
  AddGlobalAliasType('File',FileType);

  ResourceStringType:=TTypeDeclaration.Create(Globals);

  if CompilerVersion<>LazarusVersion then
  begin
    MaxIntConst:=TNumber.Create(Globals);
    MaxIntConst.Name:='MaxInt';
    MaxIntConst.Value:=MaxInt;
    Globals.Constants.Add(MaxIntConst);

    MaxLongintConst:=TNumber.Create(Globals);
    MaxLongintConst.Name:='MaxLongint';
    MaxLongintConst.Value:=MaxLongInt;
    Globals.Constants.Add(MaxLongintConst);
  end;

  NilConst:=TConstantDeclaration.Create(Globals);
  NilConst.Name:='nil';
  NilConst.TypeIdentifier:=PointerType;
  Globals.Constants.Add(NilConst);

  // Special case: HInstance is at SysInit.pas instead of System.pas
  HInstanceConst:=TVariableDeclaration.CreateType(Globals,LongWordType);
  HInstanceConst.Name:='HInstance';
  Globals.Variables.Add(HInstanceConst);

  BooleanSpec:=TTypeSpecification.Create(Globals);

  BooleanType:=AddGlobalType('Boolean',BooleanSpec);
  {WordBool:=}AddGlobalType('WordBool',BooleanSpec);
  {LongBool:=}AddGlobalType('LongBool',BooleanSpec);
  {ByteBool:=}AddGlobalType('ByteBool',BooleanSpec);

  DoubleType:=AddGlobalType('Double',FloatSpec);
  SingleType:=AddGlobalType('Single',FloatSpec);
  ExtendedType:=AddGlobalType('Extended',FloatSpec);
  {CompType:=}AddGlobalType('Comp',FloatSpec);
  {CurrencyType:=}AddGlobalType('Currency',FloatSpec);

  if CompilerVersion=LazarusVersion then
  begin
    AddGlobalType('QWord',NumberSpec); // UInt64
    AddGlobalType('PtrInt',PointerSpec); // deprecated
    AddGlobalType('PtrUInt',PointerSpec);
  end
  else
  begin
    CompilerVersionConst:=TConstantDeclaration.CreateName(Globals,'CompilerVersion');
    CompilerVersionConst.TypeIdentifier:=IntegerType;
    CompilerVersionConst.Value:=TNumber.Create(CompilerVersionConst);
    TNumber(CompilerVersionConst.Value).Value:=CompilerVersion;
    Globals.Constants.Add(CompilerVersionConst);
  end;

  VariantSpec:=TTypeSpecification.Create(Globals);

  VariantType:=AddGlobalType('Variant',VariantSpec);
  {OleVariantType:=}AddGlobalType('OleVariant',VariantSpec);

  // After ExtendedType !
  PiConst:=TFloatNumber.Create(Globals);
  PiConst.Name:='PI';
  PiConst.Value:=PI;
  Globals.Constants.Add(PiConst);

  Globals.Methods.Add(RunErrorBlock);
  Globals.Methods.Add(IncBlock);
  Globals.Methods.Add(DecBlock);
  Globals.Methods.Add(SizeOfBlock);
  Globals.Methods.Add(AssignedBlock);
  Globals.Methods.Add(ChrBlock);
  Globals.Methods.Add(OrdBlock);
  Globals.Methods.Add(SliceBlock);
  Globals.Methods.Add(SetLengthBlock);
  Globals.Methods.Add(SetStringBlock);
  Globals.Methods.Add(FillCharBlock);
  Globals.Methods.Add(LengthBlock);
  Globals.Methods.Add(StrBlock);
  Globals.Methods.Add(ValBlock);
  Globals.Methods.Add(NewBlock);
  Globals.Methods.Add(AbsBlock);
  Globals.Methods.Add(TruncBlock);

  if CompilerVersion<170 then
  begin
    Globals.Methods.Add(FracBlock);
    Globals.Methods.Add(IntBlock);
    Globals.Methods.Add(ExpBlock);
    Globals.Methods.Add(SinBlock);
    Globals.Methods.Add(CosBlock);
    Globals.Methods.Add(SqrtBlock);
    Globals.Methods.Add(LnBlock);
    Globals.Methods.Add(ArcTanBlock);

    if CompilerVersion<160 then
       Globals.Methods.Add(ChDirBlock);
  end;

  Globals.Methods.Add(OddBlock);
  Globals.Methods.Add(RoundBlock);
  Globals.Methods.Add(SqrBlock);
  Globals.Methods.Add(RandomBlock);

  Globals.Methods.Add(ReallocMemBlock);
  Globals.Methods.Add(GetMemBlock);
  Globals.Methods.Add(AssertBlock);
  Globals.Methods.Add(DisposeBlock);
  Globals.Methods.Add(HighBlock);
  Globals.Methods.Add(HiBlock);
  Globals.Methods.Add(LowBlock);
  Globals.Methods.Add(LoBlock);
  Globals.Methods.Add(FreeMemBlock);
  Globals.Methods.Add(CopyBlock);
  Globals.Methods.Add(InsertBlock);

  Globals.Methods.Add(DeleteBlock);

  if CompilerVersion>=290 then
     Globals.Methods.Add(DeleteArrayBlock);

  Globals.Methods.Add(ConcatBlock);
  Globals.Methods.Add(GetDirBlock);
  Globals.Methods.Add(PosBlock);
  Globals.Methods.Add(HaltBlock);
  Globals.Methods.Add(IncludeBlock);
  Globals.Methods.Add(ExcludeBlock);
  Globals.Methods.Add(VarClearBlock);
  Globals.Methods.Add(TypeInfoBlock);
  Globals.Methods.Add(InitializeBlock);
  Globals.Methods.Add(FinalizeBlock);
  Globals.Methods.Add(VarArrayRedimBlock);
  Globals.Methods.Add(PtrBlock);
  Globals.Methods.Add(AddrBlock);
  Globals.Methods.Add(CloseBlock);
  Globals.Methods.Add(StringOfCharBlock);
  Globals.Methods.Add(PredBlock);
  Globals.Methods.Add(SuccBlock);
  Globals.Methods.Add(VarCastBlock);

  Globals.Methods.Add(AssignBlock);
  Globals.Methods.Add(AssignFileBlock);
  Globals.Methods.Add(CloseFileBlock);
  Globals.Methods.Add(ReadBlock);
  Globals.Methods.Add(ReadLnBlock);
  Globals.Methods.Add(ResetBlock);
  Globals.Methods.Add(RewriteBlock);
  Globals.Methods.Add(WriteBlock);
  Globals.Methods.Add(WriteTextBlock);
  Globals.Methods.Add(WriteLnBlock);
  Globals.Methods.Add(WriteLnTextBlock);

  Globals.Methods.Add(BlockReadWriteBlock('BlockRead'));
  Globals.Methods.Add(BlockReadWriteBlock('BlockWrite'));

  Globals.Methods.Add(FilePosBlock);
  Globals.Methods.Add(FileSizeBlock);

  Globals.Methods.Add(EofBlock);
  Globals.Methods.Add(SeekBlock);
  Globals.Methods.Add(SeekEofBlock);
  Globals.Methods.Add(SeekEolnBlock);

  if CompilerVersion>=150 then
     Globals.Methods.Add(SwapBlock);

  if CompilerVersion>=210 then
  begin
    Globals.Methods.Add(DefaultBlock);

    if CompilerVersion>=230 then
    begin
      Globals.Methods.Add(AtomicCmpExchangeBlock);
      Globals.Methods.Add(ReturnAddressBlock);

      if CompilerVersion>=240 then
      begin
        Globals.Methods.Add(AtomicExchangeBlock);
        Globals.Methods.Add(AtomicDecrementBlock);
        Globals.Methods.Add(AtomicIncrementBlock);

        if CompilerVersion>=280 then
        begin
          Globals.Methods.Add(IsManagedTypeBlock);

          if CompilerVersion>=290 then
          begin
            Globals.Methods.Add(IsConstValueBlock);
            Globals.Methods.Add(GetTypeKindBlock);
            Globals.Methods.Add(HasWeakRefBlock);
          end;
        end;
      end;
    end;
  end;

  // Missing Proxies.pas
  Globals.Methods.Add(IsProxyClassBlock);
end;

procedure TBasePascalParser.AddGlobals(const Globals: TBlock; const CompilerVersion: Integer);
var tmp : TPascalIntrinsics;
begin
  tmp:=TPascalIntrinsics.Create;
  try
    tmp.Globals:=TSection(Globals);
    tmp.AddIntrinsics(Language.CompilerVersion);
  finally
    tmp.Free;
  end;
end;

const
  LazarusCompany='Lazarus';

{ TIDE }

Constructor TIDE.CreateStatic;
begin
  inherited Create;
end;

Constructor TIDE.Create(const ACompilerVersion:Integer;
                        const APlatform:TIDEPlatform=ipWin32);
begin
  inherited Create;

  CompilerVersion:=ACompilerVersion;
  IDEPlatform:=APlatform;

  Paths:=TStringList.Create;
  UnitScopes:=TStringList.Create;
  Defines:=TStringList.Create;

  UserPaths:=TStringList.Create;
  UserDefines:=TStringList.Create;
  UserScopes:=TStringList.Create;

  Prepare(CompilerVersion);

  Source:=SourcePath;

  if IsLazarus then
  begin
    Defines.Add('FPC');
    Defines.Add('LCL');
    Defines.Add('CPUI386');
  end
  else
  begin
    Defines.Add('MSWINDOWS');

    case IDEPlatform of
      ipWin32: Defines.Add('CPUX86');
    else
      Defines.Add('CPUX64');
    end;

    Defines.Add('VER'+IntToStr(CompilerVersion)); // D7=150

    //  Defines.Add('BCB');

    if (Source='') or (not DirectoryExists(Source)) then
       VersionError('Cannot find IDE root path at registry or program files folder: '+IDEToString);

    if CompilerVersion<170 then
    else
    begin
      if IDEPlatform=ipWin32 then
         Defines.Add('CPU386');

      if IDEPlatform=ipWin32 then
         Defines.Add('WIN32')
      else
         Defines.Add('WIN64');

      if CompilerVersion>=200 then
      begin
        Defines.Add('UNICODE');
        Defines.Add('CONDITIONALEXPRESSIONS');
      end;

      if CompilerVersion>=280 then
         Defines.Add('ASSEMBLER');
    end;
  end;
end;

function TIDE.RootDir:String;

  function TryOpenKeyReadOnly(const R:TRegistry; const AKey:String):Boolean;
  begin
    if R.KeyExists(AKey) then
       result:=R.OpenKeyReadOnly(AKey)
    else
       result:=False;
  end;

  function ProgramFilesPath:String;
  var R : TRegistry;
  begin
    result:='';

    R:=TRegistry.Create;
    with R do
    try
      RootKey:=HKEY_LOCAL_MACHINE;

      if TryOpenKeyReadOnly(R,'SOFTWARE\Microsoft\Windows\CurrentVersion') then
      begin
         if ValueExists('ProgramFilesDir') then
            result:=ReadString('ProgramFilesDir');
      end
      else
         raise Exception.Create('Cannot open registry key: LOCAL_MACHINE');
    finally
      Free;
    end;
  end;

  function MakeCompilerDir(const AName,ABDSName,AEnvVersion:String):String;
  begin
    result:=ProgramFilesPath+'\'+AName+'\';

    if CompilerVersion>150 then
       result:=result+ABDSName //'BDS' 'RAD Studio'
    else
       result:=result+'Delphi';

    result:=result+'\'+AEnvVersion+'.0';
  end;

  function GetRootDir(const R:TRegistry):String;
  var tmpKey: String;
  begin
    tmpKey:='\SOFTWARE\'+RegCompany+'\'+Product+'\'+Version+'.0';

    if TryOpenKeyReadOnly(R,tmpKey) and R.ValueExists('RootDir') then
       result:=R.ReadString('RootDir')
    else
       result:='';
  end;

var Main : String;
    R : TRegistry;
begin
  result:='';
  Main:='';

  if IsLazarus then
     result:='c:\lazarus\'
  else
  begin
    R:=TRegistry.Create(KEY_READ);
    try
      R.RootKey:=HKEY_CURRENT_USER;
      Main:=GetRootDir(R);

      if Main='' then
      begin
        R.RootKey:=HKEY_LOCAL_MACHINE;
        Main:=GetRootDir(R);
      end;
    finally
      R.Free;
    end;

    if Main='' then
       Main:=MakeCompilerDir(Company,Product,Version);

    if (Main<>'') and DirectoryExists(Main) then
    begin
      if Copy(Main,Length(Main),1)<>'\' then
         Main:=Main+'\';

      result:=Main;
    end;
  end;
end;

function TIDE.SourcePath:String;
begin
  if IsLazarus then
     result:=RootDir+'lcl'
  else
     result:=RootDir+'Source';
end;

procedure TIDE.Prepare(const AVersion:Integer);
const
  Company_Borland  = 'Borland';
  Company_CodeGear = 'CodeGear';
  Company_Embarcadero = 'Embarcadero';

  Product_Delphi = 'Delphi';
  Product_BDS = 'BDS';
  Product_RAD = 'RAD Studio';
  Product_Studio = 'Studio';

  procedure SetVersion(Num:Integer; const AName:String);
  begin
    Company:=Company_Embarcadero;
    Product:=Product_BDS;
    RegCompany:=Company;
    Version:=IntToStr(Num);

    if Num>=14 then
       Description:=Product_Studio+' '+AName
    else
       Description:=Product_RAD+' '+AName;

    if (Num>=9) and (UnitScopes<>nil) then
    begin
      UnitScopes.Add('System');
      UnitScopes.Add('System.Win');
      UnitScopes.Add('Winapi');
      UnitScopes.Add('Data');
      UnitScopes.Add('Data.Win');
//      UnitScopes.Add('Fmx');
      UnitScopes.Add('Vcl');
      UnitScopes.Add('VclTee');
      UnitScopes.Add('Vcl.Imaging');
      UnitScopes.Add('Vcl.Touch');
      UnitScopes.Add('Vcl.Shell');
      UnitScopes.Add('DataSnap');
      UnitScopes.Add('DataSnap.win');
      UnitScopes.Add('Web');
      UnitScopes.Add('Web.Win');
      UnitScopes.Add('Soap');
      UnitScopes.Add('Soap.Win');
      UnitScopes.Add('Xml');
      UnitScopes.Add('Xml.Win');
      UnitScopes.Add('Bde');
      UnitScopes.Add('Samples');

      if Num>=14 then
         UnitScopes.Add('Rest');

      if Num>=16 then
         UnitScopes.Add('DUnitX');
    end;
  end;

begin
  CompilerVersion:=AVersion;

  case CompilerVersion of
    126: begin Company:=LazarusCompany; Product:='Lazarus'; Description:=Product; RegCompany:=Company; Version:='1.2.6'; end;
    130: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='5'; end;
    140: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='6'; end;
    150: begin Company:=Company_Borland; Product:=Product_Delphi; Description:=Product; RegCompany:=Company; Version:='7'; end;
    160: VersionError('Delphi 8 .NET not supported');
    170: begin Company:=Company_Borland; Product:=Product_BDS; Description:=Product+' 2005'; RegCompany:=Company; Version:='3'; end;
    180: begin Company:=Company_Borland; Product:=Product_BDS; Description:=Product+' 2006'; RegCompany:=Company; Version:='4'; end;
    190: begin Company:=Company_CodeGear; Product:=Product_BDS; Description:=Product_RAD+' 2007'; RegCompany:=Company_Borland; Version:='5'; end;
    200: begin Company:=Company_CodeGear; Product:=Product_BDS; Description:=Product_RAD+' 2009'; RegCompany:=Company; Version:='6'; end;
    210: begin Company:=Company_Embarcadero; Product:=Product_BDS; Description:=Product_RAD+' 2010'; RegCompany:=Company_CodeGear; Version:='7'; end;

    220: SetVersion(8,'XE');
    230: SetVersion(9,'XE2');
    240: SetVersion(10,'XE3');
    250: SetVersion(11,'XE4');
    260: SetVersion(12,'XE5');
    270: SetVersion(14,'XE6');
    280: SetVersion(15,'XE7');
    290: SetVersion(16,'XE8');
    300: SetVersion(17,'10 Seattle');
  else
    VersionError('Unknown Compiler Version: '+IntToStr(AVersion));
  end;
end;

function LazarusFound(out APath:String):Boolean;
const
  LazarusPath='c:\lazarus';
begin
  result:=FileExists(LazarusPath+'\lazarus.exe');

  if result then
     APath:=LazarusPath;
end;

class procedure TIDE.Detect(const AList: TStrings);
var i : TIDE;
    t : Integer;
    tmpPath : String;
begin
  AList.Clear;

  i:=TIDE.CreateStatic;
  try
    for t:=30 downto 13 do
        if t<>16 then // D8.NET
        begin
          i.Prepare(t*10);

          if i.RootDir<>'' then
             AList.AddObject(i.IDEToString,TObject(t*10));
        end;

    if LazarusFound(tmpPath) then
       AList.AddObject('Lazarus',TObject(126));

  finally
    i.Free;
  end;
end;

function TIDE.IDEToString:String;
begin
  result:=Company+' '+Description+' v'+Version;
end;

function TIDE.IsLazarus: Boolean;
begin
  result:=RegCompany=LazarusCompany;
end;

procedure TIDE.VersionError(const AMessage:String);
begin
  raise Exception.Create(AMessage);
end;

destructor TIDE.Destroy;
var t : Integer;
begin
  Defines.Free;
  UserDefines.Free;

  UserScopes.Free;
  UnitScopes.Free;

  UserPaths.Free;

  if Paths<>nil then
  begin
    for t:=0 to Paths.Count-1 do
        Paths.Objects[t].Free;

    Paths.Free;
  end;

  inherited;
end;

procedure TIDE.GetPaths;

  procedure GetRecursiveFolders(const APath:String);
  var s : TSearchRec;
  begin
    Paths.Add(APath);

    if FindFirst(APath+'\*.*',faAnyFile,s)=0 then
    begin
      Repeat
        if (s.Attr and faDirectory)=faDirectory then
           if (s.Name<>'.') and (s.Name<>'..') then
           begin
             if (CompilerVersion=190) and SameText(s.Name,'DOTNET') then
             else
             if SameText(s.Name,'CPPRTL') then
             else
                GetRecursiveFolders(APath+'\'+s.Name);
           end;

      Until FindNext(s)<>0;

      FindClose(s);
    end;
  end;

//var t : Integer;
begin
  Paths.Clear;

  GetRecursiveFolders(Source);

  if IsLazarus then
  begin
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\win32');
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\inc');
    GetRecursiveFolders(RootDir+'fpc\2.6.4\source\rtl\win');
  end;

  {
  for t:=0 to UserPaths.Count-1 do
      Paths.Add(UserPaths[t]);
  }
end;

function TIDE.HasScopes: Boolean;
begin
  result:=UnitScopes.Count>0;
end;

{ TPascal }

procedure TPascal.Clear;
begin
  inherited;

  FIDE.GetPaths;
  Includes.Clear;
  Resources.Clear;
  AddScopes;
end;

constructor TPascal.CreateVersion(const ACompilerVersion:Integer=0);
begin
  Create;

  CaseSentitive:=False;

  ModuleClass:=TUnit;

  CompilerVersion:=ACompilerVersion;

  FIDE:=TIDE.Create(CompilerVersion);
  FIDE.GetPaths;
  AddScopes;

  Includes:=TIncludeFiles.Create;
  Resources:=TStringList.Create;
end;

destructor TPascal.Destroy;
begin
  Resources.Free;
  Includes.Free;

  IDE.Free;

  inherited;
end;

procedure TPascal.AddScopes;

  procedure DoAddScopes(const AList:TStrings);
  var t : Integer;
  begin
    for t:=0 to AList.Count-1 do
        if Modules.Scopes.Find(AList[t])=nil then
           Modules.Scopes.AddSorted(TUnitScope.CreateName(Modules.Scopes,AList[t]));
  end;

begin
  DoAddScopes(ide.UnitScopes);
  DoAddScopes(ide.UserScopes);
end;

function TPascal.FindModuleOrScope(const AOrigin:TBlock; const AName:String):TExpression;
begin
  result:=inherited;

  if (result=nil) and (CompilerVersion>=230) then
      result:=Modules.Scopes.Find(AName); // Is Unit Scope ?
end;

function TPascal.GuessFullPathOf(const AFile:String):String;

  function IsInPath(const AList:TStrings; const AIndex:Integer; MoveToTop:Boolean):String;
  var
    tmpList : TBaseList;
    tmpIndex : Integer;
    tmpPath : String;
  begin
    if AList.Objects[AIndex]=nil then
    begin
      tmpList:=TBaseList.Create;
      GetFilesFromDir(AList[AIndex],tmpList);
      AList.Objects[AIndex]:=tmpList;
    end
    else
      tmpList:=TBaseList(AList.Objects[AIndex]);

    if tmpList.Find(AFile,tmpIndex) then
    begin
      tmpPath:=AList[AIndex];
      result:=tmpPath+'\'+AFile;

      // Favor search path to top of list
      if MoveToTop and (AIndex>0) then
      begin
        AList.Delete(AIndex);
        AList.InsertObject(0,tmpPath,tmpList);
      end;

      Exit;
    end;
  end;

var t : Integer;
begin
  result:='';

  for t:=0 to IDE.Paths.Count-1 do
  begin
    result:=IsInPath(IDE.Paths,t,True);

    if result<>'' then
       Exit;
  end;

  for t:=0 to IDE.UserPaths.Count-1 do
  begin
    result:=IsInPath(IDE.UserPaths,t,False);

    if result<>'' then
       Exit;
  end;
end;

type
  TModuleAccess=class(TModule);

procedure TPascal.InternalParseModule(const AParser:TBaseParser; const AModule: TModule);
begin
  AModule.FileName:=AParser.CurrentPath;

  if AModule is TProgram then
     TPascalParse(AParser).ParseProgram(TProgram(AModule))
  else
  if AModule is TLibrary then
     TPascalParse(AParser).ParseLibrary(TLibrary(AModule))
  else
  if AModule is TUnit then
     TPascalParse(AParser).ParseUnit(TUnit(AModule))
  else
  if AModule is TPackage then
     TPascalParse(AParser).ParsePackage(TPackage(AModule))
  else
     TModuleAccess(AModule).Error('Cannot parse Module: '+AModule.ClassName);
end;

function TPascal.FullPathOf(var AName:String; const AExtension:String):String;

  function TryUnitAlias(const S:String):String;
  begin
    if SameText(S,'WinTypes') or SameText(S,'WinProcs') then
       result:='Windows'
    else
    if SameText(S,'DbiTypes') or SameText(S,'DbiProcs') or SameText(S,'DbiErrs') then
       result:='BDE'
    else
       result:=S;
  end;

var t : Integer;
    tmp : String;
begin
  if ExtractFilePath(AName)='' then
  begin
    result:=GuessFullPathOf(AName+AExtension);

    if result='' then
    begin
       for t:=0 to IDE.UnitScopes.Count-1 do
       begin
         tmp:=IDE.UnitScopes[t];

         result:=GuessFullPathOf(tmp+'.'+AName+AExtension); // TPath.Combine

         if result<>'' then
         begin
           AName:=tmp+'.'+AName;

           // Favor scope to top of list
           if t>0 then
           begin
             IDE.UnitScopes.Delete(t);
             IDE.UnitScopes.Insert(0,tmp);
           end;

           Exit;
         end;
       end;

       if result='' then
          result:=GuessFullPathOf(TryUnitAlias(AName)+AExtension);
    end;
  end
  else
    result:=AName;
end;

function TPascal.GetIncludeFile(const AFile:String):String;
var S,
    tmp : String;
begin
  result:=Includes.Find(AFile);

  if result='' then
  begin
    S:=AFile;
    tmp:=FullPathOf(S,'');

    if tmp='' then
       raise ECodeException.Create('Cannot find '+AFile+' file in search paths');

    result:=Includes.Add(AFile,tmp);
  end;
end;

function TPascal.GetParser(const AOwner:TBlock):TBaseParser;
begin
  result:=TPascalParse.Create(AOwner,Self);

  if IDE.IsLazarus then
  begin
    result.IsFPC:=True;
    result.DefaultExtension:='.pp';
  end
  else
    result.DefaultExtension:='.pas';
end;

function TPascal.ParserFromFile(const AOwner:TBlock; const AFile: String): TBaseParser;
begin
  result:=GetParser(AOwner);
  TPascalParse(result).FromPathFile(AFile);
end;

function TPascal.ParserFromText(const AOwner:TBlock; const AText: String): TBaseParser;
begin
  result:=GetParser(AOwner);
  result.Text:=AText;
end;

type
  TParserAccess=class(TBaseParser);

procedure TPascal.DoParse(const AModule:TModule; const AFileName:String);
var tmp : String;
begin
  tmp:=ExtractFilePath(AFileName);

  if (tmp<>'') and (IDE.Paths.IndexOf(tmp)=-1) then
     IDE.Paths.Add(tmp);

  ParseModule(AModule,AFileName);
end;

function TPascal.ParsePackage(const AName: String): TPackage;
begin
  result:=GetPackage(Modules.PackageRequires,AName);
  {
  result:=TPackage.Create(nil);
  IDE.GetPaths;
  AddScopes;
  DoParse(result,AFileName);
  }
end;

function TPascal.ParseUnit(const AFileName: String): TUnit;
begin
  result:=TUnit.Create(nil);
  {
  IDE.GetPaths;
  AddScopes;
  }
  DoParse(result,AFileName);
end;

function TPascal.ParseProgram(const AFileName: String): TProgram;
begin
  result:=TProgram.Create(nil);
  {
  IDE.GetPaths;
  AddScopes;
  }
  DoParse(result,AFileName);
end;

var
  DefaultTerminators : TStringArray=nil;
  DirectiveTerminators : TBaseList=nil;
  EscapeKeywords : TBaseList;

{ TPascalParse }

function TPascalParse.ParseAttribute(const AOwner:TBlock):TAttribute;
begin
  result:=TAttribute.Create(AOwner);
  ParseAttributeClass(result);

  AOwner.AddAttribute(result);
end;

procedure TPascalParse.ParseAttributeClass(const Attribute: TAttribute);
begin
  Attribute.Text:=NextTokenUpTo(']');

  if not IsChar(']') then
     Error('Missing ] in attribute');

  // Pending: Verify "Text" as attribute expression(s)

  if IsFPC then
     OptionalSemicolon;
end;

procedure TPascalParse.ParseConstant(const Constant:TConstantDeclaration);
begin
  //inherited;

  Constant.Name:=RequireIdentifier;

  ParseTypeAndValue(Constant);

  if not IsToken(';') then
     ParseDirectives(Constant);

  if IsFPC then
     ParseDirectives(Constant);
end;

function TPascalParse.GuessTypeDeclaration(const AOwner:TBlock):TTypeDeclaration;
var tmp : TExpression;
begin
  tmp:=GuessTypeIdentifier(AOwner);

  if tmp=nil then
     result:=nil
  else
  if tmp is TFieldExpression then
     result:=TScopedType.CreateScope(AOwner,TFieldExpression(tmp))
  else
     result:=tmp.GuessType(AOwner);
end;

procedure TPascalParse.ParseCaseVariable(const CaseVariable:TCaseVariable);

  procedure FindNameAndType;
  var S : String;
  begin
    S:=RequireIdentifier;

    CaseVariable.TypeIdentifier:=Language.GuessType(CaseVariable.Owner,S); // subtypes ??

    if CaseVariable.TypeIdentifier=nil then
       if IsChar(':') then
       begin
         CaseVariable.Name:=S;
         CaseVariable.TypeIdentifier:=GuessTypeDeclaration(CaseVariable.Owner);

         if CaseVariable.TypeIdentifier=nil then
            Error('Missing type for case variable');
       end;
  end;

  type
    TCaseVariableItems=Array of TCaseVariableItem;

  function CaseFields:TCaseVariableItems;
  var L : Integer;
  begin
    result:=nil;
    L:=0;

    repeat
      SetLength(result,L+1);

      result[L]:=TCaseVariableItem.Create(CaseVariable);
      result[L].Expression:=ExpressionRequire(CaseVariable,[],skField,True);

      if result[L].Expression=nil then
         Error('Wrong case expression');

    until not IsChar(',');
  end;

  function GetCaseItemValue:TRecordFieldsDeclarations;
  var tmp : TCaseVariable;
  begin
    if IsChar('(') then
    begin
      result:=TRecordFieldsDeclarations.Create(CaseVariable);

      repeat
        if IsToken('CASE') then
        begin
          tmp:=TCaseVariable.Create(CaseVariable);
          ParseCaseVariable(tmp);

          result.Add(tmp);
        end
        else
        begin
          ParseFields(CaseVariable,result,False);

          if PeekChar=')' then
             break
          else
             RequireSemicolon;
        end;

      until False;

      if not IsChar(')') then
         Error('Missing ) in record case: '+CaseVariable.Name);
    end
    else
    begin
      Error('Missing ( in record case: '+CaseVariable.Name);
      result:=nil;
    end;
  end;

  function TokenIsEnd:Boolean;
  var tmp : String;
  begin
    PeekToken(tmp);
    result:=SameText(tmp,'END') or SameText(tmp,'END;');
  end;

var tmp : TCaseVariableItems;
    L,t : Integer;
begin
  FindNameAndType;

  RequireToken('OF');

  repeat
    if (PeekChar=')') or TokenIsEnd then
       break
    else
    begin
      tmp:=CaseFields;

      if IsChar(':') then
      begin
        L:=Low(tmp);

        for t:=L to High(tmp) do
        begin
          if t=L then
             tmp[L].Value:=GetCaseItemValue
          else
             tmp[t].Value:=tmp[L].Value;

          TRecordFieldsDeclarations(CaseVariable.Fields).Add(tmp[t]);
        end;

        OptionalSemicolon;
      end
      else
         Error('Missing ":" in record case: '+CaseVariable.Name);
    end;

  until False;
end;

procedure TPascalParse.ParseVariables(const Variables:TVariableDeclarations);
var S : String;
begin
  //inherited;

  repeat
    if not ParseFields(Variables,Variables,False,Variables.ParseKind) then
       break;

    OptionalSemicolon;

    while IsChar('[') do
          ParseAttribute(Variables);

    PeekToken(S);

  until (S='') or EscapeKeywords.Exists(S);
end;

procedure TPascalParse.ParseSection(const Section:TSection);

  procedure AddFields(const AKind:TParseFieldKind);
  var Old : Integer;
  begin
    if Section.Variables=nil then
       Section.Variables:=TVariableDeclarations.Create(Section);

    Old:=Section.Variables.Count;

    Section.Variables.ParseKind:=AKind;
    ParseVariables(Section.Variables);

    Section.AddOrdered(Section.Variables,Old);
  end;

  function ParseExports:TExportDeclaration;
  var tmpExp : TExpression;
  begin
    result:=nil;

    // Wrong/Pending: Instead of TExpression.Require, it should be: GuessSymbol
    tmpExp:=ExpressionRequire(Section);

    if tmpExp is TCallExpression then
       if TCallExpression(tmpExp).Expression is TMethodDeclaration then
       begin
         result:=TExportDeclaration.Create(Section);
         result.Method:=TCallExpression(tmpExp).Expression as TMethodDeclaration

         // Should we destroy tmpExp here ?
       end
       else
         Error('Wrong exports expression clause: '+TCallExpression(tmpExp).Expression.ClassName)
    else
    if tmpExp is TVariableDeclaration then
    begin
      result:=TExportDeclaration.Create(Section);
      result.Method:=TVariableDeclaration(tmpExp);
    end
    else
       Error('Wrong exports clause: '+tmpExp.ClassName);
  end;

  procedure AddDirectives;
  var t : Integer;
      tmp : TAttribute;
  begin
    for t:=0 to High(Directives) do
    begin
      tmp:=Directives[t];
      tmp.SetOwner(Section);

      Section.AddOrderedSingle(tmp);
    end;

    Directives:=nil;
  end;

var S : String;
    tmp : Integer;
    tmpInst : TMethodSpecification;
    tmpMethod : TMethodDeclaration;
    Old : Integer;
begin
  //inherited;

  repeat
    if Directives<>nil then
       AddDirectives;

    while IsChar('[') do
          ParseAttribute(Section);

    PeekToken(S);

    tmp:=TextIn(S,['CONST','TYPE','THREADVAR','VAR','CLASS','FUNCTION',
                   'PROCEDURE','CONSTRUCTOR','DESTRUCTOR','OPERATOR',
                   'LABEL','EXPORTS','RESOURCESTRING']);

    if tmp=-1 then
       break
    else
    begin
      SkipToken;

      case tmp of
        0: begin
             if Section.Constants=nil then
                Section.Constants:=TConstantDeclarations.Create(Section);

             Old:=Section.Constants.Count;
             ParseConstants(Section.Constants);
             Section.AddOrdered(Section.Constants,Old);
             //Process(Section.Constants);
           end;

        1: begin
             if Section.Types=nil then
                Section.Types:=TTypeDeclarations.Create(Section);

             Old:=Section.Types.Count;
             ParseTypes(Section.Types);
             Section.AddOrdered(Section.Types,Old);
             //Process(Section.Types);
           end;

        2: AddFields(fkThreadVar);
        3: AddFields(fkField);

    4,5,6,
    7,8,9: begin
             if Section.Methods=nil then
                Section.Methods:=TMethodDeclarations.Create(Section);

             tmpMethod:=ParseNewMethod(Section,False,tmp,Section.Methods,Section.Bodies,tmpInst);

             if Section.Bodies and (not (mdForward in tmpInst.Directives)) and (not tmpInst.IsExternal) then
             begin
               tmpInst.Body:=TBody.Create(tmpInst);
               ParseBody(tmpInst.Body);

               if tmpInst.Body.Statements is TASMStatements then
                  OptionalSemicolon
               else
                  RequireSemicolon;
             end;

             Section.AddOrderedSingle(tmpMethod);
           end;

       10: begin
             if Section.Labels=nil then
                Section.Labels:=TLabelDeclarations.Create(Section);

             Old:=Section.Labels.Count;
             ParseLabels(Section.Labels);
             Section.AddOrdered(Section.Labels,Old);
             //Process(Section.Labels);
           end;
       11: begin
             // Necessary to maintain a Exported list?
             //if Exported=nil then
             //   Exported:=TExportDeclarations.Create(Self);

             repeat
               Section.AddOrderedSingle(ParseExports);

               if not IsChar(',') then
                  break;

             until False;

             RequireSemicolon;
           end;
      else
        begin
          if Section.ResourceStrings=nil then
             Section.ResourceStrings:=TResourceStrings.Create(Section);

          Old:=Section.ResourceStrings.Count;
          ParseResourceStrings(Section.ResourceStrings);
          Section.AddOrdered(Section.ResourceStrings,Old);
          //Process(Section.ResourceStrings);
        end;
      end;
    end;

  until S='';
end;

procedure TPascalParse.ParseArrayValues(const Values:TArrayValues);
var L : Integer;
    tmp : TExpression;
begin
  if IsChar('(') then
  begin
    L:=0;

    repeat
      tmp:=ParseDefaultValueOf(Values.Owner,Values.TypeIdentifier,Values.DimensionIndex+1);

      if tmp=nil then
         Error('Invalid array value expression')
      else
      begin
        SetLength(Values.Items,L+1);

        Values.Items[L]:=tmp;
        Inc(L);

        // Pending: Check CompatibleTypes(Self.TypeIdentifier, tmp)
      end;

      if not IsChar(',') then
         break;

    until False;

    if not IsChar(')') then
       Error('Missing ) in array const values');

    // Pending: Verify Length(Items) =  Self.Range count !
  end
  else
    Error('Missing ( in array const values');
end;

procedure TPascalParse.ParseItemExpression(const Item:TItemExpression);
begin
  Item.TypeIdentifier:=Item.Value.TypeIdentifier;
  Item.Items:=ParseExpressions(Item.Owner);
end;

procedure TPascalParse.ParseSetSpecification(const SetSpec:TSetSpecification);

  procedure ParseElements;
  var S : String;
      tmp : TIdentifier;
  begin
    repeat
      S:=RequireIdentifier;

      tmp:=TSetItem.CreateSpec(SetSpec,SetSpec);

      if IsChar('=') then
         TSetItem(tmp).Value:=ExpressionRequire(SetSpec,[],skField,True);

      tmp.Name:=S;

      SetSpec.Add(tmp);

    until not IsChar(',');
  end;

begin
  if IsChar('(') then
  begin
    ParseElements;

    if not IsChar(')') then
       Error('Missing ) in set declaration');
  end
  else
    Error('Missing ( in set declaration');
end;

type
  TConstantsAccess=class(TConstantDeclarations);

procedure TPascalParse.ParseConstants(const Constants:TConstantDeclarations);
var S : String;
    tmp : TConstantDeclaration;
begin
  //inherited;

  repeat
    while IsChar('[') do
          ParseAttribute(Constants);

    S:=PeekToken(IdentifierCharsNoDot);

    if EscapeKeywords.Exists(S) then
       break
    else
    begin
      tmp:=TConstantDeclaration.Create(Constants);

      tmp.IsClass:=TConstantsAccess(Constants).TempIsClass;
      ParseConstant(tmp);
      Constants.Add(tmp);
    end;

  until S='';

  OptionalSemicolon;
end;

procedure TPascalParse.ParseResourceStrings(const Strings:TResourceStrings);
var t : Integer;
begin
  ParseConstants(Strings);

  for t:=0 to Strings.Count-1 do
  begin
    Language.CurrentModule.AddUsage(ResourceStringType);
    Strings.Item[t].TypeIdentifier:=ResourceStringType;
  end;
end;

procedure TPascalParse.ParseUses(const AUses:TUses);
var S,
    tmp : String;
    C : Char;
    tmpExp : TExpression;
    tmpModule : TModule;
begin
  //inherited;

  repeat
    NextToken(IdentifierCharsNoDot+['.'],S);

    if S<>'' then
    begin
      if not AUses.Exclude(S) then
      begin
        if IsToken('IN') then
        begin
          tmpExp:=ExpressionRequire(AUses,[],skField,True);

          if tmpExp=nil then
             Error('String Path required after uses "in"')
          else
          begin
            tmp:=tmpExp.ToString;

            if (ExtractFilePath(tmp)<>'') and IsRelativePath(tmp) then
            begin
              tmpModule:=TModule.ModuleOf(AUses);

              if tmpModule<>nil then
                 tmp:=ExtractFilePath(tmpModule.FileName)+tmp;
            end;
          end;
        end
        else
           tmp:=Language.FullPathOf(S,DefaultExtension);

        if tmp='' then
           Error('Cannot find unit: '+S)
        else
           AUses.ParseModule(S,tmp);
      end;

      C:=PeekChar;

      if C=',' then
         Increment
      else
      if C=';' then
      begin
        Increment;
        break;
      end
      else
        Error('Expected , or ;');
    end
    else
      break;

  until False;
end;

procedure TPascalParse.ParseStringFormat(const Format:TStringFormatExpression);
begin
  //inherited;

  Format.TypeIdentifier:=Format.Expression.TypeIdentifier;

  Increment;
  Format.Width:=ExpressionRequire(Format);

  if (Next=':') and (GetNextNext<>'=') then
  begin
    Increment;
    Format.Decimals:=ExpressionRequire(Format);
  end;
end;

procedure TPascalParse.ParseProperty(const Prop:TPropertyDeclaration);
var
  tmpRecord : TRecordSpecification;

  procedure ParseDefault;
  var tmpDefault : TPropertyDeclaration;
  begin
    if IsChar(';') then
    begin
      tmpDefault:=tmpRecord.DefaultProperty(False);

      if tmpDefault=nil then
         Prop.IsDefault:=True
      else
      if Prop.SameType(tmpDefault) then
         Error('Record class already has a default Property');
    end
    else
    begin
      Prop.DefaultValue:=ExpressionRequire(Prop.Owner,['STORED']);

      if Prop.DefaultValue=nil then
         Error('Wrong property default value: '+Prop.Name);
    end;
  end;

var
  Terminators:TStringArray;

  C : Char;
  S : String;
//  DummyForward : String;
  tmpFind : TFindSymbol;
//    tmpParams : TExpressions;
begin
  // NO !! inherited;

  SetLength(Terminators,6);
  Terminators[0]:='READ';
  Terminators[1]:='WRITE';
  Terminators[2]:='DEFAULT';
  Terminators[3]:='NODEFAULT';
  Terminators[4]:='STORED';
  Terminators[5]:='IMPLEMENTS';

  Prop.Name:=RequireIdentifier;

  tmpRecord:=TRecordSpecification.RecordOf(Prop.Owner);

  if tmpRecord=nil then
     Error('Cannot obtain record of property: '+Prop.Name);

  Prop.RecordType:=tmpRecord.RecordType;

  if IsChar('[') then
  begin
    Prop.Indexes:=TParameters.Create(Prop);

    repeat
      ParseFields(Prop.Owner {Self},Prop.Indexes,True {False},fkParameter);

      C:=PeekChar;

      if C=';' then
         Increment
      else
      if C=']' then
      begin
        Increment;
        break;
      end
      else
         Error('Missing closing "]" in property indexes');

    until False;
  end;

  C:=PeekChar;

  if C=':' then
  begin
    Increment;

    Prop.PropertyType:=GuessTypeDeclaration(Prop.Owner {Self});

    if Prop.PropertyType=nil then
       Error('Missing Type of property: '+S);

    SkipStringRange(S);

    if Prop.IsIndexed then
       Prop.TypeIdentifier:=TTypeDeclaration.CreateSpec(Prop,Prop.ArrayTypeOf(Prop.PropertyType))
    else
       Prop.TypeIdentifier:=Prop.PropertyType;
  end
  else
    Prop.SearchInheritedType;

  if C=';' then
  begin
    // Re-published/promoted properties
    Increment;
    Exit;
  end;

  repeat
    OptionalSemicolon;

    S:=PeekIdentifier;

    if SameText(S,'INDEX') then
    begin
      NextIdentifier;
      Prop.Index:=ExpressionRequire(Prop.Owner,Terminators);
    end
    else
    if SameText(S,'READ') then
    begin
      NextIdentifier;

      S:=PeekToken(IdentifierCharsNoDot+['.']);

      tmpFind.Kind:=skField;
      tmpFind.Origin:=Prop;
      tmpFind.Owner:=Prop.Owner;
      tmpFind.Parent:=Prop.Owner;
      tmpFind.Name:=S;
      tmpFind.AResult:=Prop.PropertyType.Expression;
      tmpFind.TypeParams:=nil;

      if Prop.Indexes=nil then
      begin
        if Prop.Index=nil then
           tmpFind.AsPointer:=True
        else
        begin
          tmpFind.AsPointer:=False;
          tmpFind.Params:=Prop.IndexExpression(False);
        end;

        Prop.ReadPart:=tmpRecord.Find(tmpFind);

        if Prop.ReadPart=nil then
           Prop.ReadPart:=ExpressionRequire(Prop.Owner,Terminators)
        else
           NextIdentifier;
      end
      else
      begin
        tmpFind.Params:=Prop.IndexesExpressions(False);
        Prop.ReadPart:=tmpRecord.Find(tmpFind);

        if Prop.ReadPart<>nil then
           NextIdentifier;
      end;

      if Prop.ReadPart=nil then
         Error('Missing property getter: '+S);
    end
    else
    if SameText(S,'WRITE') then
    begin
      NextIdentifier;

      S:=PeekToken(IdentifierCharsNoDot+['.']);

      tmpFind.Kind:=skField;
      tmpFind.Origin:=Prop;
      tmpfind.Owner:=Prop.Owner;
      tmpFind.Parent:=Prop.Owner;
      tmpFind.Name:=S;
      tmpFind.AResult:=nil;
      tmpFind.TypeParams:=nil;

      if Prop.Indexes=nil then
      begin
        if Prop.Index=nil then
           tmpFind.Params:=Prop.TypeExpression
        else
           tmpFind.Params:=Prop.IndexExpression(True);

        Prop.WritePart:=tmpRecord.Find(tmpFind);

        if Prop.WritePart=nil then
           Prop.WritePart:=ExpressionRequire(Prop.Owner,Terminators)
        else
           NextIdentifier;
      end
      else
      begin
        tmpFind.Params:=Prop.IndexesExpressions(True);
        Prop.WritePart:=tmpRecord.Find(tmpFind);

        if Prop.WritePart<>nil then
           NextIdentifier;
      end;

      if Prop.WritePart=nil then
         Error('Missing property setter: '+S);

      // Pending: D6 {$VARPROPSETTER ON}
      // CheckParamsAreVar(WritePart)
    end
    else
    if SameText(S,'DEFAULT') then
    begin
      NextIdentifier;
      ParseDefault;
    end
    else
    if SameText(S,'NODEFAULT') then
    begin
      NextIdentifier;
      Prop.NoDefault:=True;
    end
    else
    if SameText(S,'STORED') then
    begin
      NextIdentifier;
      Prop.Stored:=ExpressionRequire(Prop.Owner,['DEFAULT','NODEFAULT']);
    end
    else
    // Dispid, Readonly and WriteOnly pertain to TDispatchInterfaceProperty
    if SameText(S,'DISPID') then
    begin
      NextIdentifier;
      Prop.DispatchID:=ExpressionRequire(Prop,[],skField,True);
    end
    else
    if SameText(S,'READONLY') then
    begin
      NextIdentifier;
      Prop.ReadOnly:=True;
    end
    else
    if SameText(S,'WRITEONLY') then
    begin
      NextIdentifier;
      Prop.WriteOnly:=True;
    end
    else
    if SameText(S,'IMPLEMENTS') then
    begin
      NextIdentifier;
      Prop.ImplementsInterfaces:=ParseExpressions(Prop.Owner,Terminators); // TExpression.Require(Owner,Terminators);
      Prop.CheckImplements;
    end
    else
       break;

  until False;
end;

var
  OperatorKinds : TBaseList=nil;

function TPascalParse.GuessKind(const AName: String):TRecordOperatorKind;
var tmp : Integer;
begin
  if OperatorKinds=nil then
     if IsFPC then
        OperatorKinds:=TBaseList.CreateStrings(
           [':=','+','-','*','/','**','=','<','<=','>','>=','><','IN','EXPLICIT'
           ])
     else
        OperatorKinds:=TBaseList.CreateStrings(
           ['ADD', 'BITWISEAND', 'BITWISEOR', 'BITWISEXOR', 'DEC','DIVIDE','EQUAL',
            'EXPLICIT','GREATERTHAN','GREATERTHANOREQUAL','IMPLICIT',
            'IN','INC','INTDIVIDE','LEFTSHIFT','LESSTHAN','LESSTHANOREQUAL',
            'LOGICALAND','LOGICALNOT','LOGICALOR','LOGICALXOR','MODULUS','MULTIPLY',
            'NEGATIVE','NOTEQUAL','POSITIVE','RIGHTSHIFT','ROUND','SUBTRACT','TRUNC'
           ]);

  if OperatorKinds.Find(AName,tmp) then
     result:=TRecordOperatorKind(tmp)
  else
  begin
    Error('Invalid class operator: '+AName);
    result:=TRecordOperatorKind.okAdd;
  end;
end;

procedure TPascalParse.ParseMethodSpec(const AMethod:TMethodSpecification);
var tmpClass : TMethodSpecificationClass;
begin
  tmpClass:=TMethodSpecificationClass(AMethod.ClassType);

  //if tmpClass is TConstructorSpecification then

  if tmpClass=TRecordOperatorSpec then
     ParseRecordOperator(TRecordOperatorSpec(AMethod))
  else
  if tmpClass=TDestructorSpecification then
     ParseDestructor(TDestructorSpecification(AMethod))
  else
  if tmpClass=TFunctionSpecification then
     ParseFunction(TFunctionSpecification(AMethod))
  else
     ParseMethodParams(TMethodParams(AMethod));

  //2: tmpClass:=TProcedureSpecification;
  //tmpFunc.Parse;
end;

function TPascalParse.RequireStatements(const AOwner:TBlock): TStatements;
begin
  result:=TStatements.Create(AOwner);
  ParseStatements(result);
end;

type
  TTypesAccess=class(TTypeDeclarations);

procedure TPascalParse.ParseRecord(const RecordSpec:TRecordSpecification);
var
  tmpVisibility : TVisibility;

  function IsMethod(const S:String; IsClassMethod:Boolean):Boolean;
  const
    MethodKeywords:Array[0..4] of String=(
                   'CONSTRUCTOR','DESTRUCTOR','PROCEDURE','FUNCTION','OPERATOR');

  var tmpClass : TMethodSpecificationClass;
      tmpFunc : TMethodSpecification;
      tmpMethod : TMethodDeclaration;
      tmpParams : TTypeParameters;
      tmpName : String;
  begin
    result:=True;

    case TextIn(S,MethodKeywords) of
      0: tmpClass:=TConstructorSpecification;
      1: tmpClass:=TDestructorSpecification;
      2: tmpClass:=TProcedureSpecification;
      3: tmpClass:=TFunctionSpecification;
      4: tmpClass:=TRecordOperatorSpec;
    else
      begin
        tmpClass:=nil;
        result:=False;
      end;
    end;

    if result then
    begin
      NextIdentifier;

      tmpMethod:=TMethodDeclaration.Create(RecordSpec);

      tmpFunc:=tmpClass.Create(RecordSpec);
      tmpFunc.OfRecord:=RecordSpec.RecordType;

      tmpName:=InternalGetTypeIdentifier(RecordSpec,tmpParams);

      if IsChar('.') then // Reference
      begin
        // TODO Pending: Find tmpName as tmpMethod.RecordType !!
        // Warning: This might produce duplicate error !
        // See: TDUnitXIEnumerator<T> GetCurrent
        tmpMethod.Name:=tmpName+'.'+NextIdentifier;
      end
      else
      begin
        tmpMethod.Name:=tmpName;
        tmpFunc.TypeParameters:=tmpParams;
      end;

      if tmpFunc is TRecordOperatorSpec then
      begin
        TRecordOperatorSpec(tmpFunc).Kind:=GuessKind(tmpMethod.Name);
        RecordSpec.AddOperator(TRecordOperatorSpec(tmpFunc),tmpMethod.Name);
      end;

      tmpFunc.IsClass:=IsClassMethod;

      ParseMethodSpec(tmpFunc);

      if RecordSpec.Methods=nil then
         RecordSpec.Methods:=TMethodDeclarations.Create(RecordSpec);

      if IsChar('=') then
         tmpFunc.Referenced:=RequireIdentifier;

      OptionalSemicolon;

      ParseMethodDirectives(tmpFunc);

      tmpMethod.Visibility:=tmpVisibility;
      tmpMethod.TypeIdentifier:=TTypeDeclaration.CreateSpec(tmpMethod,tmpFunc);

      ParseDirectives(tmpMethod);

      // Call Methods.Add after ParseDirectives, to recognize possible "overload"
      RecordSpec.Methods.Add(tmpMethod);

      RecordSpec.AddOrderedSingle(tmpMethod);
    end;
  end;

  procedure ParseAncestors;
  var C : Char;
      tmpType : TTypeDeclaration;
      tmpSpec : TTypeSpecification;
  begin
    repeat
      tmpType:=GuessTypeDeclaration(RecordSpec);

      if tmpType=nil then
         Error('Invalid Ancestor')
      else
      begin
        tmpSpec:=tmpType.Expression;

        if tmpSpec is TRecordSpecification then
        begin
          if (RecordSpec.Ancestors=nil) or (not (tmpSpec is TClassSpecification)) then
             RecordSpec.AddAncestor(tmpType)
          else
             Error('Only one ancestor can be a Class type: '+tmpType.Name);

        end
        else
           Error('Ancestor is not a Record or Interface type: '+tmpType.Name);

        C:=PeekChar;

        if C=')' then
           break
        else
        if C=',' then
           Increment;
      end;

    until False;
  end;

  function IsVisibility(S:String):Boolean;
  var tmpStrict : Boolean;
      tmpIdent : TVisibilityIdent;
  begin
    if (Language.CompilerVersion>=150) or (RecordSpec is TClassSpecification) then
    begin
      tmpStrict:=SameText(S,'STRICT');

      if tmpStrict then
      begin
        NextIdentifier;
        S:=PeekIdentifier;
      end;

      result:=True;

      if SameText(S,'PRIVATE') then
         if tmpStrict then
            tmpVisibility:=vStrictPrivate
         else
            tmpVisibility:=vPrivate
      else
      if SameText(S,'PROTECTED') then
         if tmpStrict then
            tmpVisibility:=vStrictProtected
         else
            tmpVisibility:=vProtected
      else
      if SameText(S,'PUBLIC') then
         tmpVisibility:=vPublic
      else
      if SameText(S,'PUBLISHED') then
         tmpVisibility:=vPublished
      else
      if tmpStrict then
         Error('Invalid Strict clause: '+S)
      else
         result:=False;

      if result then
      begin
        tmpIdent:=TVisibilityIdent.Create(RecordSpec);
        tmpIdent.Visibility:=tmpVisibility;
        RecordSpec.AddOrderedSingle(tmpIdent);
      end;
   end
   else
     result:=False;
  end;

  procedure AddFields(const IsClass:Boolean;
                      const AKind:TParseFieldKind);
  var Old : Integer;
      S : String;
  begin
    S:=PeekIdentifier;

    if IsVisibility(S) then
       NextIdentifier
    else
    begin
      RecordSpec.CheckFields;

      Old:=RecordSpec.Fields.Count;

      ParseFields(RecordSpec,RecordSpec.Fields,False,AKind,tmpVisibility,IsClass);

      RecordSpec.AddOrdered(RecordSpec.Fields,Old);

      OptionalSemicolon;
    end;
  end;

  procedure DoParseProperty(const IsClass:Boolean);
  var tmpProp : TPropertyDeclaration;
  begin
    NextIdentifier;

    // Pending: Requires(TDispatchInterfaceProperty) when owner is dispinterface
    tmpProp:=TPropertyDeclaration.Create(RecordSpec);
    ParseProperty(tmpProp);
    tmpProp.Visibility:=tmpVisibility;

    tmpProp.IsClass:=IsClass;

    RecordSpec.AddField(tmpProp);

    if tmpProp.IsDefault then
       RecordSpec.PropertyDefault:=tmpProp;
  end;

  procedure DoParseConstants(IsClass:Boolean);
  var Old : Integer;
      S : String;
  begin
    S:=PeekIdentifier;

    if IsVisibility(S) then
       NextIdentifier
    else
    begin
      if RecordSpec.Constants=nil then
         RecordSpec.Constants:=TConstantDeclarations.Create(RecordSpec);

      Old:=RecordSpec.Constants.Count;

      TConstantsAccess(RecordSpec.Constants).TempIsClass:=IsClass;
      ParseConstants(RecordSpec.Constants);

      RecordSpec.AddOrdered(RecordSpec.Constants,Old);
    end;
  end;

  procedure DoParseTypes;
  var Old : Integer;
  begin
    if RecordSpec.Types=nil then
       RecordSpec.Types:=TTypeDeclarations.Create(RecordSpec);

    Old:=RecordSpec.Types.Count;

    TTypesAccess(RecordSpec.Types).TempVisibility:=tmpVisibility;
    ParseTypes(RecordSpec.Types);

    RecordSpec.AddOrdered(RecordSpec.Types,Old);
  end;

  procedure AddDirectives;
  var t : Integer;
      tmp : TAttribute;
  begin
    for t:=0 to High(Directives) do
    begin
      tmp:=Directives[t];
      tmp.SetOwner(RecordSpec);

      RecordSpec.AddOrderedSingle(tmp);
    end;

    Directives:=nil;
  end;

var S : String;
    tmpCase : TCaseVariable;
begin
  if IsChar('(') then
  begin
    ParseAncestors;

    if not IsChar(')') then
       Error('Missing ) in class ancestors');
  end;

  // Default
  tmpVisibility:=vPublic;

  repeat
    if IsChar(';') then  // type c=class;
       break
    else
    begin
      if Directives<>nil then
         AddDirectives;

      while IsChar('[') do
            ParseAttribute(RecordSpec);
    end;

    if IsChar('&') then
       AddFields(False,fkField)
    else
    begin
      S:=PeekIdentifier;

      if S='' then
         Error('Wrong record declaration: '+S)
      else
      begin
        if IsVisibility(S) then
           NextIdentifier
        else
        if SameText(S,'VAR') then
           NextIdentifier
        else
        if SameText(S,'CASE') then
        begin
          NextIdentifier;

          tmpCase:=TCaseVariable.Create(RecordSpec);
          ParseCaseVariable(tmpCase);

          RecordSpec.AddField(tmpCase);
        end
        else
        if SameText(S,'END') then
        begin
          NextIdentifier;
          break;
        end
        else
        if IsMethod(S,False) then // Already created and added
        else
        if SameText(S,'PROPERTY') then
           DoParseProperty(False)
        else
        if SameText(S,'CLASS') then
        begin
          NextIdentifier;

          S:=PeekIdentifier;

          if not IsMethod(S,True) then
          begin
            if Language.CompilerVersion>=180 then
            begin
              if SameText(S,'VAR') then
              begin
                NextIdentifier;
                AddFields(True,fkField);
              end
              else
              if SameText(S,'THREADVAR') then
              begin
                NextIdentifier;
                AddFields(True,fkThreadVar);
              end
              else
              if SameText(S,'CONST') then
              begin
                NextIdentifier;
                DoParseConstants(True);
                OptionalSemicolon;
              end
              else
              if SameText(S,'PROPERTY') then
                 DoParseProperty(True)
              else
                 Error('Unknown class section: '+S);
            end
            else
               Error('Class method not valid: '+S);
          end;
        end
        else
        if SameText(S,'TYPE') then
        begin
          NextIdentifier;

          if Language.CompilerVersion>=190 then
             DoParseTypes
          else
             Error('Types in records only allowed in Compiler Version >= 190');
        end
        else
        if SameText(S,'CONST') then
        begin
          NextIdentifier;

          if Language.CompilerVersion>=200 then
             DoParseConstants(False)
          else
             Error('Const in records only allowed in Compiler Version >= 200');
        end
        else
          AddFields(False,fkField);
      end;
    end;

  until SameText(S,'END');
end;

procedure TPascalParse.ParseClass(const ClassSpec:TClassSpecification);
var S : String;
begin
  S:=PeekIdentifier;

  if SameText(S,'ABSTRACT') then // CompilerVersion >= 190 D2007
  begin
    ClassSpec.IsAbstract:=True;
    NextIdentifier;
  end
  else
  if SameText(S,'SEALED') then // CompilerVersion >= 200 D2009
  begin
    ClassSpec.IsSealed:=True;
    NextIdentifier;
  end;

  ParseRecord(ClassSpec);
end;

procedure TPascalParse.ParseArray(const ArraySpec:TArraySpecification);

  procedure ParseDimensions;
  var tmp : TExpression;
  begin
    repeat
       tmp:=ExpressionRequire(ArraySpec);

       if tmp<>nil then
       begin
         ArraySpec.AddDimension(tmp);

         if not IsChar(',') then
            break;
       end
       else
         break;

    until False;
  end;

var S : String;
    tmpIsPacked : Boolean;
    DummyForward : String;
begin
  if IsChar('[') then
  begin
    ParseDimensions;

    if not IsChar(']') then
       Error('Missing ] in array ranges');
  end;

  if SameText(NextIdentifier,'OF') then
  begin
    if IsChar('^') then
    begin
      ArraySpec.Expression:=TTypeDeclaration.CreateSpec(ArraySpec,
                                   TTypePointerOf.Create(ArraySpec));
      ParseTypeExpression(ArraySpec.Owner,DummyForward,
         TTypePointerOf(ArraySpec.Expression.Expression).TypeIdentifier,ArraySpec,'');
    end
    else
    begin
      S:=PeekIdentifier;

      tmpIsPacked:=IsToken('PACKED');

      ArraySpec.Expression:=TTypeDeclaration.Create(ArraySpec);

      if SameText(S,'CONST') then
         ArraySpec.Expression.Expression:=VarRecSpec { TArrayOfConst.Create(Self)}
      else
      // To remove ! Procedure and Function, see 'ARRAY' below
      if SameText(S,'PROCEDURE') then
         ArraySpec.Expression.Expression:=TProcedureSpecification.Create(ArraySpec)
      else
      if SameText(S,'FUNCTION') then
         ArraySpec.Expression.Expression:=TFunctionSpecification.Create(ArraySpec)
      else
      // 'ARRAY' might be removed, leaving the default ParseTypeExpression
      if SameText(S,'ARRAY') then
      begin
        NextIdentifier;
        ArraySpec.Expression.Expression:=ParseArrayType(ArraySpec,ArraySpec.TypeParameters);
        TArraySpecification(ArraySpec.Expression.Expression).IsPacked:=tmpIsPacked;
        Exit;
      end
      else
      begin
        ParseTypeExpression(ArraySpec {Owner},DummyForward,ArraySpec.Expression,ArraySpec,'');
        Exit;
      end;
    end;

    NextIdentifier;
  end
  else
    Error('Missing "of"');
end;

type
  TTypeAccess=class(TTypeDeclaration);

procedure TPascalParse.ParseTypes(const Types:TTypeDeclarations);

  procedure DoParseType(var AType:TTypeDeclaration;
                        const AParams:TExpressions);
  begin
    ParseTypeExpression(Types,TTypeAccess(AType).ForwardExpression,AType,
                        Types,AType.Name,AParams);

    if (GUIDSpec=nil) and SameText(AType.Name,'TGUID') then
       GUIDSpec:=TRecordSpecification(AType.Expression)
    else
    if (VarRecSpec=nil) and SameText(AType.Name,'TVarRec') then
       VarRecSpec:=TRecordSpecification(AType.Expression);
  end;

  procedure TryAddAncestor(const ASpec:TRecordSpecification);
  begin
    if ASpec.Ancestors=nil then
       if ASpec is TInterfaceSpecification then
       begin
         if ASpec.RecordType<>IInterfaceType then
            ASpec.AddAncestor(IInterfaceType);
       end
       else
       if ASpec is TClassSpecification then
       begin
         if ASpec.RecordType<>ObjectType then
            ASpec.AddAncestor(ObjectType);
       end;
  end;

  procedure AddDirectives(const AType:TTypeDeclaration);
  var t : Integer;
      tmpDir : TAttribute;
  begin
    for t:=0 to High(Directives) do
    begin
      tmpDir:=Directives[t];
      tmpDir.SetOwner(AType);

      AType.AddAttribute(tmpDir);
    end;

    Directives:=nil;
  end;

var S : String;
    tmp : TTypeDeclaration;
    StartType : Integer;
    tmpFind : TFindSymbol;
    tmpIsAmp : Boolean;
begin
  StartType:=Types.Count-1;

  tmpFind.Kind:=skTypeOnly;
  tmpFind.Origin:=Types;
  tmpFind.Owner:=Types;
  tmpFind.Parent:=Types;
  tmpFind.AsPointer:=True;
  tmpFind.AResult:=nil;
  tmpFind.Params:=nil;

  repeat
    while IsChar('[') do
          ParseAttribute(Types);

    tmpIsAmp:=IsChar('&');

    PeekToken(S);

    if (not tmpIsAmp) and EscapeKeywords.Exists(S) then
       break
    else
    if S<>'' then
    begin
      tmpFind.Name:=InternalGetTypeIdentifier(Types,tmpFind.TypeParams);

      if not IsChar('=') then
         Error('Missing = in type declaration: '+tmpFind.Name);

      tmp:=TTypeDeclaration.CreateName(Types,tmpFind.Name);
      tmp.Visibility:=TTypesAccess(Types).TempVisibility;

      tmp.TypeParameters:=tmpFind.TypeParams;

      // Pending:
      // Check there is no other type with the same generic parameters

      if Directives<>nil then
         AddDirectives(tmp);

      Types.AddSorted(tmp);

      DoParseType(tmp,tmpFind.TypeParams);

      ParseDirectives(tmp);

      if tmp.Alias=nil then
         if tmp.Expression is TRecordSpecification then
            if TTypeAccess(tmp).ForwardExpression='' then
               TryAddAncestor(TRecordSpecification(tmp.Expression));

      if TTypeAccess(tmp).ForwardExpression='' then
         Types.RelinkForward(StartType,tmp);

      OptionalSemicolon;
    end;

  until S='';

  Types.VerifyForward(StartType);
end;

procedure TPascalParse.ParseStatements(const Statements:TStatements);
var S : String;
begin
  //inherited;

  S:=PeekIdentifier;

  Statements.BeginEnd:=SameText(S,'BEGIN');

  if Statements.BeginEnd then
  begin
    NextIdentifier;

    ParseStatementItems(Statements);

    if not SameText(NextIdentifier,'END') then
       Error('Missing END');
  end
  else
    ParseStatementItems(Statements,1);
end;

procedure TPascalParse.ParseStatementItems(const Statements:TStatements; Max:Integer=0);
type
  TStatementClass=class of TStatement;

  function IsStatement(const S:String; out AClass:TStatementClass):Boolean;
  begin
    result:=True;

    if S='IF' then AClass:=TIf else
    if S='FOR' then AClass:=TFor else
    if S='WHILE' then AClass:=TWhile else
    if S='WITH' then AClass:=TWith else
    if S='REPEAT' then AClass:=TRepeat else
    if S='TRY' then AClass:=TTry else
    if S='CASE' then AClass:=TCase else
    if S='GOTO' then AClass:=TGoto else
    if S='ASM' then AClass:=TASMStatements else
    if S='ON' then AClass:=TOn else
    if S='RAISE' then AClass:=TRaise else
    if S='BEGIN' then AClass:=TStatements else
       result:=False;
  end;

  function TryParse(out ABlock:TStatement):Boolean;
  var S: String;
      tmpClass : TStatementClass;
      tmpLabel : TLabelDeclaration;
      tmpExpression : TExpression;
  begin
    if IsChar(';') then
    begin
      result:=True;
      Exit;
    end
    else
      S:=UpperCase(PeekIdentifier);

    result:=False;

    if S<>'' then
    begin
      if TextIn(S,DefaultTerminators)<>-1 then
         Exit
      else
      if IsStatement(S,tmpClass) then
      begin
        if tmpClass<>TStatements then
           NextIdentifier;

        ABlock:=tmpClass.Create(Statements);
      end
      else
      begin
        S:=PeekIdentifier;

        tmpLabel:=TLabelDeclaration.FindLabel(Statements,S);

        if tmpLabel<>nil then
        begin
          NextIdentifier;

          if not IsChar(':') then
             Error('Expected ":"');

          ABlock:=TLabelPlace.Create(Statements);
          TLabelPlace(ABlock).LabelIdentifier:=tmpLabel;
          tmpClass:=TLabelPlace;
        end;
      end;
    end;

    if ABlock=nil then
    begin
      tmpExpression:=ExpressionRequire(Statements,DefaultTerminators);

      //if IsChar(':') and IsChar('=') then
      if PeekTwo=':=' then
      begin
        if tmpExpression=nil then
           Error('Unknown Left destination expression variable in assignment');

        if tmpExpression is TParameter then
           if TParameter(tmpExpression).IsConst then
              Error('Constant parameter cannot be assigned');

        ABlock:=TAssignment.Create(Statements);

        if tmpExpression.Owner=Statements then
           tmpExpression.Owner:=ABlock;

        TAssignment(ABlock).Left:=tmpExpression;
        tmpClass:=TAssignment;
      end
      else
      begin
        if tmpExpression=nil then
           Error('Method not found');

        ABlock:=TCall.Create(Statements);
        TCall(ABlock).Expression:=tmpExpression;
        tmpClass:=TCall;
      end;
    end;

    if Assigned(ABlock) then
    begin
      if tmpClass=TAssignment then ParseAssignment(TAssignment(ABlock)) else
      if tmpClass=TIf then ParseIf(TIf(ABlock)) else
      if tmpClass=TFor then ParseFor(TFor(ABlock)) else
      if tmpClass=TWhile then ParseWhile(TWhile(ABlock)) else
      if tmpClass=TRepeat then ParseRepeat(TRepeat(ABlock)) else
      if tmpClass=TASMStatements then ParseASM(TASMStatements(ABlock)) else
      if tmpClass=TStatements then ParseStatements(TStatements(ABlock)) else
      if tmpClass=TCase then ParseCase(TCase(ABlock)) else
      if tmpClass=TOn then ParseOn(TOn(ABlock)) else
      if tmpClass=TWith then ParseWith(TWith(ABlock)) else
      if tmpClass=TTry then ParseTry(TTry(ABlock)) else
      if tmpClass=TRaise then ParseRaise(TRaise(ABlock)) else
      if tmpClass=TGoto then ParseGoto(TGoto(ABlock)) else
      if (tmpClass<>TCall) and (tmpClass<>TLabelPlace) then
         Error('Internal error, ParseStatement');

      result:=True;
    end;
  end;

var
  L: Integer;
  tmp : TStatement;
begin
  L:=0;

  repeat
    tmp:=nil;

    if TryParse(tmp) then
    begin
      if tmp<>nil then
      begin
        Statements.Add(tmp);
        Inc(L);

        if Max>0 then
           if Max=L then
              break;
      end;
    end
    else
       break;

  until Eof;
end;

procedure TPascalParse.ParseASM(const ASMS:TASMStatements);
var S : String;
    L : Integer;
begin
  L:=0;

  IgnoreStrings:=True;
  try
    repeat
      S:=Trim(NextLine);

      if S<>'' then
      begin
        if SameText(Copy(S,1,3),'END') or
           SameText(Copy(S,Length(S)-4,4),' END') then
           break
        else
        begin
          SetLength(ASMS.Items,L+1);
          ASMS.Items[L]:=S;
          Inc(L);
        end;
      end;

    until Eof;

  finally
    IgnoreStrings:=False;
  end;
end;

procedure TPascalParse.ParseAssignment(const Assignment:TAssignment);
var tmpLeft,
    tmpRight : TTypeDeclaration;
    tmpIsMethod,
    Dummy : Boolean;
    tmpSpec : TTypeSpecification;
    tmpFunc : TFunctionSpecification;
begin
  //inherited;

  if IsChar(':') and IsChar('=') then
  begin
    tmpIsMethod:=False;

    // How to eliminate this check? ( function Foo; ... foo:=123 )
    tmpSpec:=Assignment.Left.TypeIdentifier.Expression;

    if tmpSpec is TFunctionSpecification then
    begin
      tmpFunc:=TFunctionSpecification.FunctionOf(Assignment);

      tmpIsMethod:=tmpFunc=nil; // <-- owner is a procedure ?

      if not tmpIsMethod then
      begin
        if tmpFunc.OfRecord=nil then
           tmpIsMethod:=tmpFunc<>TFunctionSpecification(tmpSpec) // not same function
        else
        begin
          if tmpFunc.OfRecord<>TFunctionSpecification(tmpSpec).OfRecord then
             tmpIsMethod:=True;
        end;
      end;
    end
    else
    if tmpSpec is TReferenceSpecification then
       tmpIsMethod:=True;

    Language.ValueFinal(Assignment.Left,tmpLeft,not tmpIsMethod);

    while tmpLeft.Alias<>nil do
          tmpLeft:=tmpLeft.Alias;

    if tmpLeft.Generic=nil then
       Error('Internal: Missing expression at left side of assignment');

    Assignment.Right:=ExpressionRequire(Assignment,
                                DefaultTerminators,
                                skField,
                                False,
                                (tmpLeft.Expression=PointerSpec) or
                                Language.IsMethodPointer(tmpLeft.Expression),
                                tmpLeft.Expression);

    if Assignment.Right=nil then
       Error('Missing right side of assignment')
    else
    if not Language.CompatibleTypes(tmpLeft.Generic,Assignment.Right,Dummy,True) then
    begin
      Language.ValueFinal(Assignment.Right,tmpRight,not Language.IsMethodPointer(tmpLeft.Generic));

      Error('Assignment not compatible. Target: '+tmpLeft.Qualified+
            ' Source: '+tmpRight.Qualified);
    end;

    OptionalSemicolon;
  end
  else
     Error('Missing := in assignment');
end;

procedure TPascalParse.ParseIf(const AIf:TIf);
begin
  //inherited;

  AIf.Expression:=ExpressionRequire(AIf,['THEN']);

  // Pending: CompatibleTypes(Expression,BooleanSpec)

  RequireToken('THEN');
  AIf.ThenPart:=RequireStatements(AIf);

  if IsToken('ELSE') then
     AIf.ElsePart:=RequireStatements(AIf);
end;

{ TWhile }

procedure TPascalParse.ParseWhile(const AWhile:TWhile);
var S : String;
begin
  //inherited;

  AWhile.Expression:=ExpressionRequire(AWhile,['DO']);

  // Pending: CompatibleTypes(Expression,BooleanSpec)

  NextToken(S);

  if SameText(S,'DO') then
     AWhile.WhilePart:=RequireStatements(AWhile)
  else
  if not SameText(S,'DO;') then
     Error('Expected: DO');
end;

procedure TPascalParse.ParseWith(const AWith:TWith);

  // This is necessary (instead of ParseExpressions), as With items can be nested
  procedure ParseItems;
  var L : Integer;
      tmp : TExpression;
  begin
    L:=0;

    repeat
      tmp:=ExpressionRequire(AWith,['DO']);

      if tmp=nil then
         break
      else
      begin
        SetLength(AWith.Items,L+1);
        AWith.Items[L]:=tmp;
        Inc(L);

        IsChar(',');
      end;

    until False;
  end;

var S : String;
begin
  //inherited;

  ParseItems;

  NextToken(S);

  if SameText(S,'DO') then
     AWith.WithPart:=RequireStatements(AWith)
  else
  if not SameText(S,'DO;') then
     Error('Expected: DO');
end;

{ TFor }

procedure TPascalParse.ParseFor(const AFor:TFor);
var S: String;
    tmp : TExpression;
    tmpOwner : TBlock;
begin
  //inherited;

  S:=RequireIdentifier;

  tmpOwner:=AFor.Owner;

  repeat
    if tmpOwner is TBody then
       break
    else
       tmpOwner:=tmpOwner.Owner;
  until tmpOwner=nil;

  if tmpOwner=nil then
     Error('Cannot obtain Body owner of TFor');

  tmp:=Language.GuessIdentifier(skField,tmpOwner {Self},S,True);

  if tmp is TVariableDeclaration then
     AFor.Iterator:=TVariableDeclaration(tmp)
  else
     Error('Local field '+S+' must be a variable');

  if AFor.Iterator=nil then
     Error('Local variable not found, for iterator identifier: '+S);

  if IsChar(':') and IsChar('=') then
  begin
    AFor.Start:=ExpressionRequire(AFor,['TO','DOWNTO']);

    if SameText(NextIdentifier,'TO') then
       AFor.ToValue:=ExpressionRequire(AFor,['DO'])
    else
       AFor.DownToValue:=ExpressionRequire(AFor,['DO']);

    // Pending: Validate Start etc are numeric
  end
  else
  begin
    RequireToken('IN');
    AFor.Enumerated:=ExpressionRequire(AFor,['DO']);

    // Pending: Validate "Enumerated" is enumerable of type "Iterator"
  end;

  NextToken(S);

  if SameText(S,'DO') then
     AFor.Loop:=RequireStatements(AFor)
  else
  if not SameText(S,'DO;') then
     Error('Expected: DO');
end;

procedure TPascalParse.ParseRepeat(const ARepeat:TRepeat);
begin
  //inherited;

  ARepeat.RepeatPart:=TStatements.Create(ARepeat);
  ParseStatementItems(ARepeat.RepeatPart);

  RequireToken('UNTIL');
  ARepeat.Expression:=ExpressionRequire(ARepeat,DefaultTerminators);

  // Pending: CompatibleTypes(Expression,BooleanSpec)
end;

{ TTry }

procedure TPascalParse.ParseTry(const ATry:TTry);
var S : String;
begin
  //inherited;

  ATry.Block:=TStatements.Create(ATry);
  ParseStatementItems(ATry.Block);

  S:=NextIdentifier;

  if SameText(S,'FINALLY') then
  begin
    ATry.FinallyPart:=TStatements.Create(ATry);
    ParseStatementItems(ATry.FinallyPart);
  end
  else
  if SameText(S,'EXCEPT') then
  begin
    ATry.ExceptPart:=TTryExcept.Create(ATry);
    ParseStatementItems(ATry.ExceptPart);
  end
  else
     Error('Missing EXCEPT or FINALLY');

  S:=NextIdentifier;

  if SameText(S,'ELSE') then
  begin
    ATry.ElsePart:=TStatements.Create(ATry);
    ParseStatementItems(ATry.ElsePart);
    S:=NextIdentifier;
  end;

  if SameText(S,'END') then
     OptionalSemicolon
  else
     Error('Missing ELSE or END at Try');
end;

{ TRaise }

procedure TPascalParse.ParseRaise(const ARaise:TRaise);
begin
  ARaise.Expression:=ExpressionRequire(ARaise,DefaultTerminators);
end;

procedure TPascalParse.ParseBody(const Body:TBody);
var S : String;
begin
  ParseSection(Body);

  S:=PeekIdentifier;

  if SameText(S,'BEGIN') then
  begin
    Body.Statements:=TStatements.Create(Body);
    ParseStatements(Body.Statements);
  end
  else
  if SameText(S,'ASM') then
  begin
    Body.Statements:=TASMStatements.Create(Body);
    SkipToken;
    ParseASM(TASMStatements(Body.Statements));
  end
  else
     Error('BEGIN or ASM expected. Found: '+S);
end;

procedure TPascalParse.ParseMethodParams(const Method:TMethodParams);
var C : Char;
begin
  //inherited;

  if IsChar('(') and (not IsChar(')')) then
  begin
    Method.Parameters:=TParameters.Create(Method);

    repeat
      ParseFields(Method,Method.Parameters,True,fkParameter);

      C:=PeekChar;

      if C=';' then
         Increment
      else
      begin
        if C=')' then
        begin
          Increment;
          break;
        end
        else
           Error('Missing closing ")" in parameters');
      end;

    until False;

    Method.CalcFixedParameters;
  end;
end;

procedure TPascalParse.ParseDestructor(const ADestructor:TDestructorSpecification);
begin
  //inherited;

  if IsChar('(') and (not IsChar(')')) then
     Error('Destructors cannot have parameters');
end;

procedure TPascalParse.ParseFunction(const AFunction:TFunctionSpecification);
var tmpType : TTypeDeclaration;
begin
  ParseMethodParams(AFunction);

  if IsChar('=') then
     AFunction.Referenced:=RequireIdentifier // TExpression.Require ?
  else
  if IsChar(':') then
  begin
    tmpType:=GuessTypeDeclaration(AFunction);

    if tmpType=nil then
       Error('Missing type for function result');

    AFunction.ResultValue:=TVariableDeclaration.CreateType(AFunction,tmpType);
    AFunction.ResultValue.Name:='Result';
  end
  else
  if Next<>';' then
     Error('Missing : and result type');
end;

procedure TPascalParse.ParseRecordOperator(const AOperator:TRecordOperatorSpec);
var tmp : Integer;
begin
  ParseFunction(AOperator);

  case AOperator.Kind of
    okImplicit,
    okExplicit,
    okNegative,
    okPositive,
    okInc,
    okDec,
    okLogicalNot,
    okTrunc,
    okRound: tmp:=1;
  else
    tmp:=2;
  end;

  if AOperator.Parameters.Count<>tmp then
     Error('Method Operator parameter count must be: '+IntToStr(tmp));
end;

function TPascalParse.ParseExpressions(const AOwner:TBlock;
                          const Terminators:{$IFDEF D9}TStringArray=[]{$ELSE}Array of String{$ENDIF};
                          const OnlyConstants:Boolean=False):TExpressions;
var L : Integer;
    tmp : TExpression;
    C : Char;
begin
  L:=0;

  repeat
    tmp:=ExpressionRequire(AOwner,[],skField,OnlyConstants);

    if tmp<>nil then
    begin
      SetLength(result,L+1);
      result[L]:=tmp;
      Inc(L);

      C:=PeekChar;

      if OnlyConstants and (C=':') then // <-- Case items
         break
      else
      if C=',' then
         Increment;
    end;

  until tmp=nil;
end;

{$IFNDEF D9}
function ParseExpressions(const AOwner:TBlock):TExpressions; overload;
begin
  result:=ParseExpressions(AOwner,[]);
end;
{$ENDIF}

procedure TPascalParse.ParseCase(const ACase:TCase);

  procedure ParseCase;
  var
    tmpFields : TExpressions;
    LFields : Integer;

    t : Integer;
    L : Integer;
    tmp : TCaseItem;
  begin
    tmpFields:=ParseExpressions(ACase,[],True);
    LFields:=Length(tmpFields);

    L:=Length(ACase.Cases);
    SetLength(ACase.Cases,L+LFields);

    for t:=0 to LFields-1 do
    begin
      tmp:=TCaseItem.Create(ACase);

      tmp.Condition:=tmpFields[t];

      if t=0 then
         if IsChar(':') then
         begin
           if not IsChar(';') then
              tmp.Body:=RequireStatements(tmp);
         end
         else
            Error('Missing ":" in case')
      else
         tmp.Body:=ACase.Cases[L].Body;

      ACase.Cases[L+t]:=tmp;
    end;
  end;

var S : String;
begin
  //inherited;

  ACase.Expression:=ExpressionRequire(ACase,['OF']);

  RequireToken('OF');

  repeat
    ParseCase;

    OptionalSemicolon;

    PeekToken(S);

    if TextIn(S,['ELSE','END','END;'])<>-1 then
       break;

  until S='';

  if SameText(S,'ELSE') then
  begin
    SkipToken;

    ACase.ElsePart:=TStatements.Create(ACase);
    ParseStatementItems(ACase.ElsePart);

    OptionalSemicolon; // ?
  end;

  if not SameText(NextIdentifier,'END') then
     Error('Missing END in case');
end;

{ TLabelDeclaration }

procedure TPascalParse.ParseLabel(const ALabel:TLabelDeclaration);
begin
  //inherited;
  ALabel.Name:=RequireIdentifier;
  OptionalSemicolon;
end;

procedure TPascalParse.ParseLabels(const Labels:TLabelDeclarations);
var S : String;
    tmp : TLabelDeclaration;
begin
  repeat
    tmp:=TLabelDeclaration.Create(Labels);
    ParseLabel(tmp);

    Labels.AddSorted(tmp);

    if not IsChar(',') then
    begin
      if IsChar(';') then
         break
      else
      begin
        PeekToken(S);

        if EscapeKeywords.Exists(S) then
           break
      end;
    end;

  until False;
end;

{ TGoto }

procedure TPascalParse.ParseGoto(const AGoto:TGoto);
var tmp : String;
begin
  //inherited;

  tmp:=NextIdentifier;
  AGoto.TargetLabel:=TLabelDeclaration.FindLabel(AGoto.Owner,tmp);

  if AGoto.TargetLabel=nil then
     Error('Label not found: '+tmp);
end;

function TPascalParse.ForceUseUnit(const AImpl:TImplementationModule; const AOwner:TUsesSection; const AName:String):TUnit;
var S : String;
begin
  if SameText(AImpl.Name,AName) then
     result:=nil
  else
  begin
    S:=AName;

    result:=TUnit(Language.FindModule(S));

    if result=nil then
    begin
      S:=Language.FullPathOf(S,DefaultExtension);

      if S<>'' then
         result:=TUnit(Language.Modules.ParseNewModule(AName,S,Language.ModuleClass));
    end;

    if result=nil then
       Error('Cannot find file: '+AName)
    else
       AOwner.UsesUnits.Add(result);
  end;
end;

type
  TLanguageAccess=class(TLanguage);

procedure TPascalParse.ParseUnit(const AUnit:TUnit);
begin
  GetName(AUnit,'UNIT');

  // Special case for top-root parsed unit:
  if Language.FindModule(AUnit.Name)=nil then
  begin
    Language.Modules.PackageContains.Add(AUnit);
//    Language.Units.NewBlock(AUnit);
  end;

  {$IFOPT D+}
  Language.BreakOn.Stopped:=False;
  CurrentUnit:=AUnit.Name;

  Language.BreakOn.IsUnitImpl:=SameText(AUnit.Name,Language.BreakOn.UnitImpl);
  {$ENDIF}

  RequireToken('INTERFACE');

  AUnit.UnitInterface:=TInterface.Create(AUnit);

  if SameText(AUnit.Name,'System') then
     AddGlobals(AUnit.UnitInterface,Language.CompilerVersion)
  else
     ForceUseUnit(AUnit,AUnit.UnitInterface,'System');

  if IsToken('USES') then
     ParseUses(AUnit.UnitInterface.UsesUnits);

  ParseSection(AUnit.UnitInterface);

  AUnit.InterfaceLines:=State.Position.Line;
  TLanguageAccess(Language).AddParsedLines(AUnit,AUnit.InterfaceLines);

  RequireToken('IMPLEMENTATION');

  if not AUnit.OnlyInterface then
  begin
    ParseImplementation(AUnit);
    AUnit.ModuleImplementation.Empty:=False;
  end;
end;

procedure TPascalParse.ParseProgram(const AProgram:TProgram);
begin
  GetName(AProgram,'PROGRAM');
  ParseModuleSection(AProgram);
  ParseMain(AProgram);
end;

procedure TPascalParse.ParsePackages(const APackages:TPackages);
var S : String;
    tmp : TPackage;
begin
  //inherited;

  repeat
    S:=PeekIdentifier;

    if SameText(S,'CONTAINS') or SameText(S,'END') then
       Exit
    else
    begin
      S:=RequireIdentifier;
      tmp:=TLanguageAccess(Language).GetPackage(APackages,S);
      APackages.AddSorted(tmp);
    end;

  until not IsChar(',');

  RequireSemicolon;
end;

procedure TPascalParse.ParsePackage(const APackage:TPackage);
begin
  //inherited;

  GetName(APackage,'PACKAGE');

  if IsToken('REQUIRES') then
  begin
    APackage.PackageRequires.Parser:=Self;
    ParsePackages(APackage.PackageRequires);
  end;

  if IsToken('CONTAINS') then
  begin
    APackage.PackageContains.Parser:=Self;
    ParseUses(APackage.PackageContains);
    Finish(APackage.PackageContains);
  end;

  RequireToken('END.');
end;

procedure TPascalParse.ParseLibrary(const ALibrary:TLibrary);
begin
  GetName(ALibrary,'LIBRARY');
  ParseModuleSection(ALibrary);
  ParseMain(ALibrary);
end;

procedure TPascalParse.ParseTypeAndValue(const Constant:TConstantDeclaration);

  procedure ParseType;
  var ForwardExpression : String;
  begin
    if PeekChar<>':' then
       if not Constant.NeedsType then
          Exit
       else
          Error('Missing :');

    Increment;

    ParseTypeExpression(Constant.Owner,ForwardExpression,Constant.TypeIdentifier,Constant,'');

    if ForwardExpression<>'' then
       Error('Missing type: '+ForwardExpression);

    if Constant.TypeIdentifier=nil then
       Error('Missing variable type');

    ParseDirectives(Constant);
  end;

var Dummy : Boolean;
begin
  ParseType;

  Constant.AutomaticType:=Constant.TypeIdentifier=nil;

  if IsChar('=') then
  begin
    if Constant.TypeIdentifier=nil then
    begin
      Constant.Value:=ExpressionRequire(Constant.Owner,['DEPRECATED','PLATFORM']);

      if Constant.Value=nil then
         Error('Cannot find default value of '+Constant.Qualified)
      else
      begin
        if Constant.Value is TTypeDeclaration then
           Constant.TypeIdentifier:=TTypeDeclaration(Constant.Value)
        else
        if Constant.Value is TExpression then
           Language.ValueFinal(TExpression(Constant.Value),Constant.TypeIdentifier);

        if Constant.TypeIdentifier=nil then
           Error('Cannot determine type from value: '+Constant.Value.ClassName);
      end;
    end
    else
       Constant.Value:=ParseDefaultValueOf(Constant {Owner},Constant.TypeIdentifier);

    if Constant.Value is TExpression then
       if not Language.CompatibleTypes(Constant.TypeIdentifier.Generic,TExpression(Constant.Value),Dummy) then
          Error('Value '+Constant.Value.ClassName+' of variable: '+Constant.Name+' is not compatible with type: '+Constant.TypeIdentifier.ClassName);

    ParseDirectives(Constant);
  end
  else
  if not Constant.OptionalValue then
     Error('Missing =');
end;

procedure TPascalParse.ParseDirectives(const AIdentifier:TIdentifier);
var S : String;
begin
  //FPC:
  while IsChar('[') do
        ParseAttribute(AIdentifier);

  repeat
    S:=PeekIdentifier;

    if SameText(S,'DEPRECATED') then
    begin
      AIdentifier.IsDeprecated:=True;
      NextIdentifier;

      if PeekChar='''' then
         AIdentifier.DeprecatedMessage:=ExpressionRequire(AIdentifier,[],skField,True); // Skip deprecated message
    end
    else
    if SameText(S,'PLATFORM') then
    begin
      AIdentifier.IsPlatform:=True;
      NextIdentifier;
    end
    else
    if SameText(S,'INLINE') then
    begin
      AIdentifier.IsInline:=True;
      NextIdentifier;
    end
    else
    if SameText(S,'ABSOLUTE') then
    begin
      NextIdentifier;
      AIdentifier.AbsoluteRef:=NextIdentifier;
    end
    else
    if IsFPC and SameText(S,'PUBLIC') then
    begin
      NextIdentifier;

      if IsToken('NAME') then
         ExpressionRequire(AIdentifier,[],skField,True); // Skip NAME string
    end
    else
      break;

    if IsToken(';') then ;

  until False;
end;

function TPascalParse.ParseFields(const AOwner:TBlock;
                     const AFields:TVariableDeclarations;
                     OptionalType:Boolean;
                     const FieldKind:TParseFieldKind=fkField;
                     const AVisibility:TVisibility=vPublic;
                     const AIsClass:Boolean=False):Boolean;
var tmp,
    tmp0 : TVariableDeclaration;
    tmpFields : Array of String;
    t,
    LFields : Integer;
    S : String;

    tmpIsAmp,

    IsConst,
    IsVar,
    IsOut : Boolean;
begin
  tmpFields:=nil;
  LFields:=0;

  IsConst:=False;
  IsVar:=False;
  IsOut:=False;

  repeat
    // Pending: Parent.Directives here !

    while IsChar('[') do
          ParseAttribute(AOwner);

    tmpIsAmp:=IsChar('&'); // skip, VER210

    S:=PeekIdentifier;

    if S='' then
       break
    else
    begin
      NextIdentifier;

      if (LFields=0) and (not tmpIsAmp) then
      begin
        if SameText(S,'CONST') then
           IsConst:=True
        else
        if SameText(S,'VAR') then
           IsVar:=True
        else
        if SameText(S,'OUT') then
           IsOut:=True;

        if IsConst or IsVar or IsOut then
        begin
          while IsChar('[') do
                ParseAttribute(AOwner);

          S:=RequireIdentifier;
        end;
      end;

      SetLength(tmpFields,LFields+1);
      tmpFields[LFields]:=S;
      Inc(LFields);

      if not IsChar(',') then
         break;
    end;

  until False;

  tmp0:=nil;

  for t:=0 to High(tmpFields) do
  begin
    case FieldKind of
      fkField: tmp:=TVariableDeclaration.Create(AOwner);

      fkParameter:  begin
                      tmp:=TParameter.Create(AOwner);

                      TParameter(tmp).IsConst:=IsConst;
                      TParameter(tmp).IsVar:=IsVar;
                      TParameter(tmp).IsOut:=IsOut;
                    end
    else
      tmp:=TThreadVariable.Create(AOwner);
    end;

    tmp.Name:=tmpFields[t];

    tmp.IsClass:=AIsClass;
    tmp.Visibility:=AVisibility;

    if OptionalType then
       tmp.NeedsType:=False;

    if t=0 then
    begin
      if (not OptionalType) or (PeekChar=':') then
         ParseTypeAndValue(tmp)
      else
         tmp.TypeIdentifier:=AnyType;

      tmp0:=tmp;
    end
    else
    begin
      tmp.TypeIdentifier:=tmp0.TypeIdentifier;
      tmp.Value:=tmp0.Value;
    end;

    AFields.Add(tmp);
  end;

  result:=Length(tmpFields)>0;

  (*
    //FPC:
    if result and (FieldKind=fkField) then
    begin
      tmp.Parent.OptionalSemicolon;
      tmp.ParseDirectives;
    end;
  *)
end;

function TPascalParse.ParseNewMethod(const AOwner:TBlock;
                        const Anonymous:Boolean;
                        const AIndex:Integer;
                        const AMethods:TMethodDeclarations;
                        const ABodies:Boolean;
                        out tmpInst:TMethodSpecification):TMethodDeclaration;

  function GetDeclaredMethods(const S:String; var ARecordType:TTypeDeclaration;
             const ATypeParams:TTypeParameters):TMethodDeclarations;
  var tmp : TExpression;
      tmpFind : TFindSymbol;
  begin
    tmpFind.Kind:=skTypeOnly;
    tmpFind.Origin:=AOwner;
    tmpFind.Owner:=AOwner;
    tmpFind.Parent:=AOwner;

    tmpFind.Name:=S;

    tmpFind.AsPointer:=True;
    tmpFind.AResult:=nil;
    tmpFind.Params:=nil;
    tmpFind.TypeParams:=ATypeParams;

    if ARecordType=nil then
    begin
      ARecordType:=TTypeDeclaration(TSection(AOwner).Find(tmpFind));

      if (ARecordType=nil) and (AOwner is TImplementation) then
         ARecordType:=TTypeDeclaration(TUnit(AOwner.Owner).UnitInterface.Find(tmpFind));
    end
    else
    begin
      tmp:=ARecordType.Find(tmpFind); // nested types

      if tmp is TTypeDeclaration then
         ARecordType:=TTypeDeclaration(tmp)
      else
         ARecordType:=nil;
    end;

    if ARecordType=nil then
       Error('Type not found: '+tmpFind.Name);

    if ARecordType.Expression is TRecordSpecification then
       result:=TRecordSpecification(ARecordType.Expression).Methods
    else
       result:=nil;
  end;

  procedure GetFromInterface(const AMethods:TMethodSpecifications; const AName:String);
  var tmpNoParams : Boolean;
  begin
    tmpInst.Directives:=tmpInst.Directives+AMethods[0].Directives;

    tmpNoParams:=(tmpInst is TMethodParams) and
                 (not TMethodParams(tmpInst).HasParameters);

    // Fetch parameters from interface first declaration, only when
    // the interface method(s) aren't overload:

    if (not (mdOverload in tmpInst.Directives)) or tmpNoParams then
    begin
      if tmpNoParams then
      begin
         if Length(AMethods)=1 then
         begin
           if AMethods[0] is TMethodParams then
           begin
             if not (mdOverload in tmpInst.Directives) then
                TMethodParams(tmpInst).Parameters:=TMethodParams(AMethods[0]).Parameters;
           end
           else
              Error('Wrong method declaration at interface: '+AName)
         end;
      end;

      if not (mdOverload in tmpInst.Directives) then
         if tmpInst is TMethodParams then
            TMethodParams(tmpInst).MatchParameters(AMethods);
    end;
  end;

var tmpClass : TMethodSpecificationClass;
    IsClass : Boolean;
    S : String;
    tmpRecordType : TTypeDeclaration;
    tmpMethods : TMethodSpecifications;
    tmpParent : TMethodDeclarations;
    tmpTypeParameters : TTypeParameters;
    tmpName : String;
    C : Char;
begin
  tmpClass:=nil;

  IsClass:=AIndex=4;

  if IsClass then
  begin
    S:=NextIdentifier;

    if SameText(S,'FUNCTION') then
       tmpClass:=TFunctionSpecification
    else
    if SameText(S,'PROCEDURE') then
       tmpClass:=TProcedureSpecification
    else
    if SameText(S,'CONSTRUCTOR') then
       tmpClass:=TConstructorSpecification
    else
    if SameText(S,'DESTRUCTOR') then
       tmpClass:=TDestructorSpecification
    else
    if SameText(S,'OPERATOR') then
       tmpClass:=TRecordOperatorSpec
    else
       Error('Class method unknown: '+S);
  end
  else
    case AIndex of
      5: tmpClass:=TFunctionSpecification;
      6: tmpClass:=TProcedureSpecification;
      7: tmpClass:=TConstructorSpecification;
      8: tmpClass:=TDestructorSpecification;
    else
      if IsFPC then
         tmpClass:=TRecordOperatorSpec
      else
         Error('Operator method must be a class method');
    end;

  if Anonymous then
     tmpInst:=tmpClass.Create(AOwner)
  else
     tmpInst:=tmpClass.Create(AMethods);

  tmpParent:=nil;

  if not Anonymous then
  begin
    tmpRecordType:=nil;

    repeat
      IsChar('&'); // skip
      NextToken(IdentifierCharsNoDot,S);

      SkipStringRange(S);

      C:=PeekChar;

      tmpTypeParameters:=nil;

      if C='<' then
      begin
        Increment;
        GuessTypeParams(AOwner,tmpTypeParameters);

        if PeekChar='.' then
        begin
          Increment;
          tmpParent:=GetDeclaredMethods(S,tmpRecordType,tmpTypeParameters);
        end
        else
        begin
          tmpInst.TypeParameters:=tmpTypeParameters;
          tmpName:=S;
          break;
        end;
      end
      else
      if C='.' then
      begin
        Increment;
        tmpParent:=GetDeclaredMethods(S,tmpRecordType,nil);
      end
      else
      if (C='(') or (C=':') or (C=';') or (C='<') then
      begin
        tmpName:=S;
        break;
      end
      else
      begin
        Error('Internal: Unknown character in method declaration: '+S);
        break;
      end;

    until False;

    if tmpRecordType=nil then
    begin
      if (AOwner is TImplementation) and (AOwner.Owner is TUnit) then
         tmpParent:=TUnit(AOwner.Owner).UnitInterface.Methods
      else
         tmpParent:=AMethods;
    end
    else
    begin
      tmpInst.OfRecord:=tmpRecordType;

      if tmpRecordType.Expression is TRecordSpecification then
         tmpParent:=TRecordSpecification(tmpRecordType.Expression).Methods
      else
        Error('Internal: Invalid Record specification');
    end;
  end;

  tmpInst.IsClass:=IsClass;

  result:=TMethodDeclaration.Create(AOwner);

  result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,tmpInst);

  if not Anonymous then
  begin
    result.Name:=tmpName;

    if tmpInst is TRecordOperatorSpec then
       TRecordOperatorSpec(tmpInst).Kind:=GuessKind(result.Name);
  end;

  ParseMethodSpec(tmpInst);

  if IsChar('=') then
     tmpInst.Referenced:=RequireIdentifier;

  OptionalSemicolon;

  if not Anonymous then
  begin
    ParseMethodDirectives(tmpInst);
    ParseDirectives(result);

    // Obtain method from Unit interface, to get its ResultValue and Parameters

    if ABodies and (tmpParent<>nil) and (AOwner is TImplementation) then
    begin
      // Any method in unit interface ?
      tmpMethods:=nil;

      tmpParent.FindAll(result.Name,tmpMethods);

      if tmpMethods<>nil then
         GetFromInterface(tmpMethods,result.Name);
    end;

    if tmpParent=AMethods then
       AMethods.Add(result)
    else
       AMethods.AddSorted(result);
  end;
end;

procedure TPascalParse.GuessTypeParams(const AOwner:TBlock; out AParams:TTypeParameters; const Expand:Boolean=False);

  // Ugly. "S" should be already a final subtype, without "." in it.
  function GuessSubType(const AOwner:TBlock; S:String; const tmpParams:TTypeParameters):TTypeDeclaration;
  var i : Integer;
      tmpParent : TBlock;
      tmpS : String;
      tmpExp : TExpression;
  begin
    tmpParent:=AOwner;

    repeat
      i:=Pos('.',S);

      if i=0 then
         result:=Language.GuessType(tmpParent,S,tmpParams)
      else
      begin
        tmpS:=Copy(S,1,i-1);
        Delete(S,1,i);

        if tmpParent is TUnitScope then
           result:=nil
        else
           result:=Language.GuessType(tmpParent,tmpS,tmpParams);

        if result=nil then
        begin
          tmpExp:=Language.FindModuleOrScope(AOwner,tmpS); // ie: <System.Rtti.TValue>

          if tmpExp is TUnitScope then
          begin
            result:=GuessSubType(tmpExp,S,tmpParams);

            if result=nil then
               Error('Type not found: '+S);

            Exit;
          end
          else
          begin
            if tmpExp is TTypeDeclaration then
               result:=TTypeDeclaration(tmpExp);

            if result=nil then
               Error('Type not found: '+tmpS);
          end;
        end;

        tmpParent:=result;
      end;

    until i=0;
  end;

  function GuessConstraint(const AParams:TTypeParameters):TTypeDeclaration;
  var tmpS : String;
      tmpClass : TClassSpecification;
  begin
    tmpS:=NextIdentifier;

    if SameText(tmpS,'CLASS') then
    begin
      tmpClass:=TClassSpecification.Create(AOwner);
      tmpClass.AddAncestor(ObjectType);
      result:=TTypeDeclaration.CreateSpec(AOwner,tmpClass);
    end
    else
    if SameText(tmpS,'CONSTRUCTOR') then
    begin
      result:=TTypeDeclaration.CreateSpec(AOwner,TConstructorSpecification.Create(AOwner));
    end
    else
    if SameText(tmpS,'RECORD') then
    begin
      result:=TTypeDeclaration.CreateSpec(AOwner,TRecordSpecification.Create(AOwner));
    end
    else
    begin
      result:=Language.GuessType(AOwner,tmpS,AParams);

      if result=nil then
         Error('Unknown Type parameter constraint: '+tmpS);
    end;
  end;

  // Pending: Multiple constraints (: x,y)
  function DoGuessConstraints(const AParams:TTypeParameters):TTypeConstraints;
  var L : Integer;
  begin
    L:=0;

    repeat
      SetLength(result,L+1);
      result[L]:=GuessConstraint(AParams);
      Inc(L);

    until not IsChar(',');

    // Pending check invalid: "record" and ("class" or "constructor")
  end;

  function ParseTypes:TTypeParameters;
  var L : Integer;
      C : Char;
      S : String;
      tmpParams : TTypeParameters;
      tmpSpec : TTypeSpecification;
      tmpType : TTypeDeclaration;
      tmpParam : TTypeParameter;
      tmpConstraints : TTypeConstraints;
  begin
    result:=nil;
    L:=0;

    repeat
      S:=InternalGetTypeIdentifier(AOwner,tmpParams,Expand);

      if S='' then
         break
      else
      begin
        SetLength(result,L+1);

        tmpSpec:=nil;
        tmpConstraints:=nil;

        if tmpParams=nil then
        begin
          if IsChar(':') then  // <-- Type constraints Since VER210
          begin
            tmpConstraints:=DoGuessConstraints(tmpParams);
            tmpSpec:=tmpConstraints[0].Expression;
          end
          else
            tmpSpec:=AnyType.Expression;

        end
        else
        if not Expand then
        begin
          tmpSpec:=TTypeSpecification.Create(AOwner);
          //tmpSpec.TypeParameters:=tmpParams;
        end;

        if Expand then
        begin
          tmpType:=GuessSubType(AOwner,S,tmpParams);

          if tmpParams=nil then
             result[L]:=tmpType
          else
             result[L]:=TGenericTypeDeclaration.CreateTyped(AOwner,tmpType,tmpParams);

          if result[L]=nil then
             Error('Unknown type: '+S);
        end
        else
        begin
          tmpParam:=TTypeParameter.CreateSpec(AOwner,tmpSpec);
          tmpParam.Visibility:=vStrictProtected;
          tmpParam.Name:=S;
          tmpParam.Index:=L;
          tmpParam.Constraints:=tmpConstraints;

          result[L]:=tmpParam;
        end;

        Inc(L);

        C:=PeekChar;

        if (C=',') or (C=';') then
           Increment
        else
           break;
      end;

    until False;
  end;

begin
  Next;

  if Language.CompilerVersion>=200 then
  begin
    AParams:=ParseTypes;

    if not IsChar('>') then
       Error('Missing > in generic type declaration');
  end
  else
    Error('Generic types allowed only in CompilerVersion >= 200');
end;

function TPascalParse.InternalGetTypeIdentifier(const AOwner:TBlock; out AParams:TTypeParameters; Expand:Boolean=False):String;
begin
  result:=InternalGetIdentifier(AOwner);

  if IsChar('<') then
     GuessTypeParams(AOwner,AParams,Expand)
  else
     AParams:=nil;
end;

procedure TPascalParse.GetDefine(Sender: TObject; const AIdentifier: String;
  out AValue: String);
begin
  if SameText(AIdentifier,'COMPILERVERSION') then
     AValue:=IntToStr(Language.CompilerVersion div 10)
  else
     inherited;
end;

procedure TPascalParse.GetName(const AModule:TModule; const APrefix:String);
begin
  RequireToken(APrefix);

  AModule.Name:=InternalGetIdentifier(AModule);

  // Pending: Verify Name = File name

  ParseDirectives(AModule);
  OptionalSemicolon;
end;

{ TNativeModule }

procedure TPascalParse.DoImplementation(const AModule:TImplementationModule);
begin
  AModule.ModuleImplementation.Parser:=Self;
  AModule.ModuleImplementation.UsesUnits.Parser:=Self;

  ForceUseUnit(AModule,AModule.ModuleImplementation,'SysInit');

  if IsToken('USES') then
     ParseUses(AModule.ModuleImplementation.UsesUnits);

  {$IFOPT D+}
  Language.BreakOn.Stopped:=False;
  CurrentUnit:=AModule.Name;

  Language.BreakOn.IsUnitImpl:=SameText(AModule.Name,Language.BreakOn.UnitImpl);
  {$ENDIF}

  ParseSection(AModule.ModuleImplementation);

  AModule.ModuleImplementation.Empty:=False;

  Finish(AModule.ModuleImplementation.UsesUnits);
end;

procedure TPascalParse.ParseModuleSection(const AModule:TNativeModule);
begin
  ForceUseUnit(AModule,AModule.ModuleImplementation,'System');

  DoImplementation(AModule);

  Finish(AModule.ModuleImplementation.UsesUnits);
end;

procedure TPascalParse.ParseMain(const AModule:TNativeModule);
begin
  RequireToken('BEGIN');

  AModule.Main:=TModuleBody.Create(AModule);
  ParseStatementItems(AModule.Main);

  RequireToken('END.');
end;

procedure TPascalParse.ParseImplementation(const AUnit:TUnit);
var Old : TModule;
begin
  if Assigned(Verbose) then
     Verbose(AUnit,'Implementation');

  Finish(AUnit.UnitInterface.UsesUnits);

  {$IFOPT D+}
  CurrentUnitIMP:=AUnit.Name;
  {$ENDIF}

  if AUnit.ModuleImplementation.Empty then
  begin
    Old:=Language.CurrentModule;
    Language.CurrentModule:=AUnit;
    try
      DoImplementation(AUnit);

      if IsToken('INITIALIZATION') or IsToken('BEGIN') then
      begin
        AUnit.UnitInitialization:=TInitialization.Create(AUnit);
        ParseStatementItems(AUnit.UnitInitialization);
      end;

      if IsToken('FINALIZATION') then
      begin
        AUnit.UnitFinalization:=TFinalization.Create(AUnit);
        ParseStatementItems(AUnit.UnitFinalization);
      end;

      RequireToken('END.');

    finally
      Language.CurrentModule:=Old;
    end;

    Inc(Language.ParsedBlocks,AUnit.BlockCount);
    TLanguageAccess(Language).AddParsedLines(AUnit,State.Position.Line-AUnit.InterfaceLines);
  end;
end;

{$IFNDEF D9}
class function TPascalParse.ExpressionRequire(const AOwner:TBlock):TExpression;
begin
  result:=ExpressionRequire(AOwner,[]);
end;
{$ENDIF}

function ChainField(const AOwner:TBlock; const AValue,AField:TExpression):TFieldExpression;
begin
  result:=TFieldExpression.CreateField(AOwner,AValue,AField);
end;

function TPascalParse.RequireCallOrCast(const AScope,AOwner:TBlock;
                           AParent:TExpression;
                           const AResult:TTypeSpecification;
                           const S:String;
                           const AVar:TExpression;
                           const tmpTypeParams:TTypeParameters):TExpression;

  procedure DoTryExpand(var AType:TTypeDeclaration);
  var tmp : TTypeDeclaration;
  begin
    Language.ValueFinal(AParent,tmp);

    repeat
      if tmp.TypeParameters<>nil then
      begin
        TTypeDeclaration.TryExpand(AType,AType,tmp.TypeParameters);
        break;
      end
      else
         tmp:=tmp.Alias;

    until tmp=nil;
  end;

  function CallOrCast(const AScope:TBlock):TExpression;
  var tmpMethod : TMethodSpecification;
      IsVariant : Boolean;
      tmpSpec : TMethodSpecification;
      tmpVar : TExpression;
      tmpParams : TExpressions;
      tmpExp : TTypeSpecification;
      tmpRecord : TRecordSpecification;
      tmpType : TTypeDeclaration;
  begin
    IsVariant:=(AParent<>nil) and (AParent.TypeIdentifier.Expression=VariantSpec);

    tmpParams:=ParseExpressions(AOwner);

    if IsVariant then
    begin
      tmpVar:=TMethodDeclaration.Create(AParent);

      if tmpParams=nil then
         tmpSpec:=TProcedureSpecification.Create(AParent)
      else
      begin
        tmpSpec:=TFunctionSpecification.Create(AParent);
        TFunctionSpecification(tmpSpec).ResultValue:=TVariableDeclaration.CreateType(tmpVar,VariantType);
      end;

      TMethodDeclaration(tmpVar).TypeIdentifier:=TTypeDeclaration.CreateSpec(tmpVar,tmpSpec);

      TMethodDeclaration(tmpVar).Name:=S;

      result:=TCallExpression.CreateCall(AOwner,
                    ChainField(AOwner,AParent,tmpVar),tmpParams);
    end
    else
    begin
      result:=nil;

      if AVar=nil then
      begin
        tmpVar:=Language.GuessIdentifier(skField,AScope,S,False,AResult,tmpParams,tmpTypeParams,AParent);

        if tmpVar=nil then
        begin
          Error('Missing identifier: '+S);
          Exit;
        end;

        if tmpVar is TFieldExpression then
        begin
          AParent:=TFieldExpression(tmpVar).Value;
          tmpVar:=TFieldExpression(tmpVar).Field;
        end;
      end
      else
        tmpVar:=AVar;

      if tmpVar is TTypeDeclaration then
      begin
        if Length(tmpParams)<>1 then
           Error('Casting to '+S+' needs a single parameter');

        if tmpParams[0]=nil then
           Error('Internal: Unknown casting expression: '+S);

        result:=TCasting.CreateType(AOwner,TTypeDeclaration(tmpVar),tmpParams[0]);
      end
      else
      begin
        if tmpVar is TConstantDeclaration then
           tmpType:=tmpVar.TypeIdentifier
        else
        if AVar<>nil then
           Language.ValueFinal(AVar,tmpType,False)
        else
           Error('Internal: Unknown var type: '+tmpVar.ClassName);

        //tmp:=TVariableDeclaration.CreateType(AOwner,tmpType);

        if (tmpType is TTypeParameter) and (AParent<>nil) then
           DoTryExpand(tmpType);

        tmpExp:=tmpType.Expression;

        if tmpExp is TMethodSpecification then
           tmpMethod:=TMethodSpecification(tmpExp)
        else
        if tmpExp is TReferenceSpecification then
           tmpMethod:=TReferenceSpecification(tmpExp).Method
        else
           tmpMethod:=nil;

        if tmpMethod<>nil then
        begin
          if (tmpParams<>nil) and (tmpMethod is TMethodParams) then
             TMethodParams(tmpMethod).CheckConstantParameters(tmpParams);

          if tmpTypeParams<>nil then
             if tmpVar is TMethodDeclaration then
                tmpVar:=TTypedMethod.CreateMethod(tmpVar,TMethodDeclaration(tmpVar),tmpTypeParams);

          if AParent=nil then
          begin
            if tmpMethod.OfRecord<>nil then
            begin
              tmpRecord:=TRecordSpecification.RecordOf(AScope);

              if (tmpRecord<>nil) and (tmpRecord.RecordType<>tmpMethod.OfRecord) then
              begin
                if tmpRecord.RecordType.TypeParameters<>nil then
                begin
                  if tmpVar is TMethodDeclaration then
                  begin
                    tmpType:=tmpRecord.FindAncestor(tmpMethod.OfRecord.Expression);

                    if (tmpType<>nil) and (tmpType.TypeParameters<>nil) then
                       tmpVar:=TGenericMethod.CreateMethod(tmpVar,TMethodDeclaration(tmpVar),tmpType.TypeParameters);
                  end;
                end;
              end;
            end;
          end
          else
          begin
            if AParent.TypeIdentifier.TypeParameters<>nil then
               if tmpVar is TMethodDeclaration then
                  tmpVar:=TGenericMethod.CreateMethod(tmpVar,TMethodDeclaration(tmpVar),AParent.TypeIdentifier.TypeParameters);

            if tmpVar<>AParent then
               tmpVar:=ChainField(AOwner,AParent,tmpVar);
          end;

          result:=TCallExpression.CreateCall(AOwner,tmpVar,tmpParams);
        end;
      end;

      if result=nil then
         Error('Symbol is not method or type: '+S+' of '+tmpVar.ClassName);
    end;
  end;

var tmpType : TTypeDeclaration;
    tmpParams : TExpressions;
    tmpIdentical : Boolean;
    tmpValid : TFindSymbol;
begin
  if AParent=nil then
     result:=CallOrCast(AScope)
  else
     result:=CallOrCast(AParent);

  if not IsChar(')') then
     Error('Missing ) in parameters');

  if result is TCasting then
  begin
    tmpType:=TCasting(result).TypeIdentifier;

    if tmpType.Expression is TMethodSpecification then
       if PeekChar<>'(' then
       else
       begin
         Increment;

         if PeekChar=')' then // Empty call ()
            Increment
         else
         if tmpType.Expression is TMethodParams then
         begin
           tmpParams:=ParseExpressions(AOwner);

           tmpValid.Owner:=AScope;
           tmpValid.Origin:=AScope;
           tmpValid.Parent:=AScope;
           tmpValid.AResult:=AResult;
           tmpValid.Params:=tmpParams;

           if TMethodParams(tmpType.Expression).ValidParameters(tmpValid,tmpIdentical) then
           begin
             TMethodParams(tmpType.Expression).CheckConstantParameters(tmpParams);

             // Call after casting
             result:=TCallExpression.CreateCall(AOwner,result,tmpParams);
           end
           else
             Error('Invalid params for casting method call');

           if not IsChar(')') then
              Error('Missing ) in casting call');
         end
         else
            Error('Casting type is not a method');
       end;
  end;
end;

function TPascalParse.GuessSymbol(const ASymbol:TFindSymbol):TExpression;
var tmpType : TTypeDeclaration;
    tmpName : String;
begin
  if IsChar('(') then // call or casting
     result:=RequireCallorCast(ASymbol.Origin,ASymbol.Owner,nil,ASymbol.AResult,ASymbol.Name,nil,ASymbol.TypeParams)
  else
  begin
    result:=Language.GuessIdentifier(ASymbol.Kind,ASymbol.Origin,ASymbol.Name,True {AsPointer},
                      ASymbol.AResult,nil,ASymbol.TypeParams);

    if result=nil then
    begin
      result:=Language.FindModuleOrScope(ASymbol.Origin,ASymbol.Name);

      if result=nil then
         if Language.CompilerVersion>=190 then
            // Is inner type or set ?
            result:=Language.GuessInnerTypes(ASymbol.Kind,ASymbol.Origin,
                     ASymbol.Owner,ASymbol.Name,True {AsPointer},ASymbol.AResult,nil);
    end;

    // No parameters call
    if result is TMethodDeclaration then
    begin
      if not ASymbol.AsPointer then
      begin
        if ASymbol.TypeParams<>nil then
        begin
          tmpType:=TGenericTypeDeclaration.CreateTyped(result,TMethodDeclaration(result).TypeIdentifier,ASymbol.TypeParams);
          tmpName:=TMethodDeclaration(result).Name;
          result:=TMethodDeclaration.CreateType(result,tmpType);
          TMethodDeclaration(result).Name:=tmpName;
        end;

        result:=TCallExpression.CreateCall(ASymbol.Owner,result,nil);
      end;
    end
    else
    if ASymbol.TypeParams<>nil then
       if (ASymbol.TypeParams[0].TypeIdentifier.Expression<>AnySpec) and
          (result is TTypeDeclaration) then
            if TTypeDeclaration(result).TypeIdentifier.Expression is TGenericSpec then
                 result:=TGenericTypeDeclaration.CreateTyped(result,
                                  result.TypeIdentifier,ASymbol.TypeParams);
  end;
end;

function TPascalParse.GetTypedType(const AOwner:TBlock; const AKind:TSymbolKind; const S:String):TExpression;
var tmpParams : TTypeParameters;
    tmp : TExpression;
begin
  GuessTypeParams(AOwner,tmpParams,True);
  tmp:=Language.GuessIdentifier(AKind,AOwner,S,True,nil,nil,tmpParams);

  if tmp=nil then
     Error('Internal: cannot find type: '+S);

  result:=TGenericTypeDeclaration.CreateTyped(AOwner,TTypeDeclaration(tmp),tmpParams)
end;

function TPascalParse.RequireIdentifier:String;
begin
  IsChar('&'); // skip

  result:=NextIdentifier;

  if result='' then
     Error('Identifier expected');
end;

// Pending: How to eliminate this?
function TPascalParse.TryGuessTypeParams(const AScope:TBlock):TTypeParameters;
const
  TypedTypeChars=IdentifierCharsNoDot+['<','.',',',':','>',''''];

var tmpS : String;
    L : Integer;
    DoGuess : Boolean;
    PosComma,
    PosGreater : Integer;
begin
  if PeekChar='<' then
  begin
    tmpS:=PeekToken(TypedTypeChars);

    L:=Length(tmpS);

    if (L>2) and (tmpS[1]='<') and
        (Copy(tmpS,1,2)<>'<>') then
    begin
      PosComma:=Pos('''',tmpS);

      if ({$IFDEF HAS_CHARINSET}CharInSet(tmpS[L],['>',',']){$ELSE}tmpS[L] in ['>',',']{$ENDIF}) then
         DoGuess:=(PosComma=0)
      else
      begin
        PosGreater:=Pos('>',tmpS);

        DoGuess:=(PosGreater>2) and ((PosComma=0) or (PosComma>PosGreater));
      end;

      if DoGuess then
      begin
        Increment;
        GuessTypeParams(AScope,result,True);
      end;
    end;
  end
  else
    result:=nil;
end;

// Faster using a SortedList ?
function IsOperatorWord(const S:String; out AOperator:TOperator):Boolean;
begin
  result:=True;

  if S='AND' then AOperator:=opAnd else
  if S='OR' then AOperator:=opOr else
  if S='NOT' then AOperator:=opNot else
  if S='DIV' then AOperator:=opDiv else
  if S='MOD' then AOperator:=opMod else
  if S='IN' then AOperator:=opIn else
  if S='IS' then AOperator:=opIs else
  if S='AS' then AOperator:=opAs else
  if S='XOR' then AOperator:=opXor else
  if S='SHL' then AOperator:=opShl else
  if S='SHR' then AOperator:=opShr else
  if S='AT' then AOperator:=opAt
  else
    result:=False;
end;

function Literal(const AOwner:TBlock; const S:String):TExpression;
var tmpFloat : Extended;
    tmpInt : Int64;
    C : Char;
begin
  if Length(S)>0 then
  begin
    C:=S[1];

    if (C='X') or (C='x') then
    begin
      result:=nil;
      Exit;
    end;
  end;

  if SameText(S,'TRUE') then
  begin
    result:=TBoolean.Create(AOwner);
    TBoolean(result).Value:=True;
  end
  else
  if SameText(S,'FALSE') then
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

function TPascalParse.ExpressionRequire(const AOwner:TBlock;
                           const Terminators:{$IFDEF D9}TStringArray=[]{$ELSE}Array of String{$ENDIF};
                           const AKind:TSymbolKind=skField;
                           const OnlyConstants:Boolean=False;
                           const AddressOf:Boolean=False;
                           const AResult:TTypeSpecification=nil):TExpression;

  function IsAnonymousMethod(const S:String):TMethodDeclaration;
  var tmpInst : TMethodSpecification;
      tmpType : Integer;
      tmpRef : TReferenceSpecification;
  begin
    if SameText(S,'FUNCTION') then
       tmpType:=5
    else
    if SameText(S,'PROCEDURE') then
       tmpType:=6
    else
    begin
      result:=nil;
      Exit;
    end;

    result:=ParseNewMethod(AOwner,True,tmpType,nil,True,{tmpParent,}tmpInst);

    tmpInst.Body:=TBody.Create(tmpInst);
    ParseBody(tmpInst.Body);

    tmpRef:=TReferenceSpecification.Create(result);
    tmpRef.Method:=tmpInst;

    // Switch expression from method to reference
    result.TypeIdentifier.Expression:=tmpRef;
  end;

  function IsOperator(const C:Char; out AOperator:TOperator):Boolean;
  begin
    case C of
      '=' : AOperator:=opEqual;

      '<' : if IsNextChar('>') then
               AOperator:=opNotEqual
            else
            if IsNextChar('=') then
               AOperator:=opLowerEqual
            else
               AOperator:=opLower;

      '>' : if IsNextChar('=') then
               AOperator:=opGreaterEqual
            else
               AOperator:=opGreater;

      '+' : AOperator:=opAdd;
      '-' : AOperator:=opSubtract;
      '*' : AOperator:=opMultiply;
      '/' : AOperator:=opDivide;
    else
    begin
      result:=False;
      Exit;
    end;
    end;

    result:=True;
  end;

  function OperatorOf(const AOwner:TBlock; const ALeft:TExpression; const AOperator:TOperator):TOperatorExpression;
  var tmp : TExpression;
      tmpOp : TOperatorExpression;
      //tmpParent,
      tmpPred : Boolean;
  begin
    tmpOp:=TOperatorExpression.Create(AOwner);
    tmpOp.Operat:=AOperator;

    //tmpParent:=AOwner.Parent.PeekChar='(';

    tmp:=ExpressionRequire(AOwner,Terminators,skField,OnlyConstants);

    if tmp=nil then
       Error('Missing right part expression of operator');

    if tmp is TOperatorExpression then
       if ALeft is TOperatorExpression then
          tmpPred:=TOperatorExpression(tmp).HasPrecedence(TOperatorExpression(ALeft).Operat)
       else
          tmpPred:=TOperatorExpression(tmp).HasPrecedence(tmpOp.Operat)
    else
       tmpPred:=False;

    if (tmp is TOperatorExpression) and
       tmpPred then
    begin
      result:=TOperatorExpression(tmp);

      tmpOp.Left:=ALeft;
      tmpOp.Right:=TOperatorExpression(tmp).Left;
      tmpOp.TypeIdentifier:=tmpOp.CalcType;

      TOperatorExpression(result).Left:=tmpOp;
    end
    else
    begin
      tmpOp.Left:=ALeft;
      tmpOp.Right:=tmp;
      tmpOp.TypeIdentifier:=tmpOp.CalcType;

      result:=tmpOp;
    end;

    result.TypeIdentifier:=result.CalcType;

    // Pending: CompatibleTypes(tmpLeft,tmpRight)
  end;

  function FindSelf(AOwner:TBlock):TConstantDeclaration;
  var tmpType : TTypeDeclaration;
  begin
    tmpType:=TRecordSpecification.OwnerRecordMethod(AOwner);

    if tmpType=nil then
       result:=nil
    else
    begin
      result:=TConstantDeclaration.CreateName(AOwner,Language.SelfKeyword);

      if tmpType.Expression is TRecordHelperSpecification then
         result.TypeIdentifier:=TRecordHelperSpecification(tmpType.Expression).TypeIdentifier
      else
         result.TypeIdentifier:=tmpType;
    end;
  end;

  function TryToFindUnit(APrefix:String):TExpression;
  var S : String;
  begin
    result:=nil;

    while IsChar('.') do
    begin
      NextToken(IdentifierCharsNoDot,S);

      APrefix:=APrefix+'.'+S;
      result:=Language.FindModule(APrefix);

      if result<>nil then
         break;
    end;
  end;

  function ParseInherited(var AExp:TInheritedExpression):Boolean;
  var Dummy,
      S : String;
      tmpSymbol: TFindSymbol;
      tmpType : TTypeDeclaration;
  begin
    // Optional
    if IsChar(';') then
    begin
      AExp.Method:=TMethodDeclaration.OwnerMethod(AOwner);
      AExp.TypeIdentifier:=AExp.Method.TypeIdentifier;

      result:=True;
    end
    else
    begin
      S:=PeekToken(IdentifierCharsNoDot);

      if (S='') or (TextIn(S,Terminators)<>-1) then
      begin
        AExp.Method:=TMethodDeclaration.OwnerMethod(AOwner); //OwnerRecordMethod(AOwner);
        AExp.TypeIdentifier:=AExp.Method.TypeIdentifier;

        result:=True;
      end
      else
      begin
        result:=False;

        NextToken(IdentifierCharsNoDot,Dummy);

        tmpSymbol.Kind:=AKind;
        tmpSymbol.Origin:=TRecordSpecification.AncestorOf(AOwner);
        tmpSymbol.Owner:=AOwner;
        tmpSymbol.Parent:=AOwner;
        tmpSymbol.AsPointer:=True;
        tmpSymbol.AResult:=AResult;
        tmpSymbol.Name:=S;

        tmpSymbol.TypeParams:=TryGuessTypeParams(tmpSymbol.Origin);

        AExp.Expression:=GuessSymbol(tmpSymbol);

        if AExp.Expression=nil then
           Error('Inherited identifier not found: '+S);

        AExp.TypeIdentifier:=AExp.Expression.TypeIdentifier;

        if tmpSymbol.Origin is TGenericTypeDeclaration then
         if not (AExp.Expression is TMethodDeclaration) then
         if AExp.Expression is TConstantDeclaration then
            if TGenericTypeDeclaration(tmpSymbol.Origin).TypeParameters<>nil then
               if AExp.TypeIdentifier.Expression is TArraySpecification then
                  if TTypeDeclaration.TryExpand(AExp.TypeIdentifier,tmpType,TGenericTypeDeclaration(tmpSymbol.Origin).TypeParameters) then
                  begin
                    AExp.Expression:=TConstantDeclaration.CreateName(AExp.Expression,
                           TConstantDeclaration(AExp.Expression).Name);
                    AExp.Expression.TypeIdentifier:=tmpType;
                    AExp.TypeIdentifier:=AExp.Expression.TypeIdentifier;
                  end;
      end;
    end;

  end;

var S : String;
    tmp : TExpression;
    C : Char;
    tmpOp : TOperator;
    i : Integer;
    tmpSymbol: TFindSymbol;
    //tmpType : TTypeDeclaration;
    tmpParams : TTypeParameters;
    tmpInt : Int64;
    tmpIsLiteral : Boolean;
begin
  result:=nil;

  repeat
    if Eof then
       break;

    C:=PeekChar;

    if (C=';') or (C=')') or (C=']') or (C=',') or
       ( (C=':') and (OnlyConstants or (GetNextNext='=')) ) then
       break
    else
    if (AKind<>skField) and (C='=') then // For "var a:b=c" to skip "=c"
       break
    else
    if C='@' then
    begin
      Increment;

      result:=TAddressOf.CreateValue(AOwner,
          ExpressionRequire(AOwner,Terminators,skField,OnlyConstants,True));
    end
    else
    if C='(' then  // <-- For simple nested expressions (no method calls)
    begin
      Increment;

      if result=nil then
      begin
        result:=TNestedExpression.Create(AOwner);
        TNestedExpression(result).Expression:=ExpressionRequire(AOwner,Terminators,skField,False,False,nil);
        TNestedExpression(result).TypeIdentifier:=TNestedExpression(result).Expression.TypeIdentifier;

        if not IsChar(')') then
           Error('Missing ) unbalanced parenthesis in expression');
      end
      else
        result:=RequireCallorCast(AOwner,AOwner,result,AResult,'',result,tmpSymbol.TypeParams);
    end
    else
    if C='[' then // pending !!   (.  .)  <-- equivalent to [ ]
    begin
      Increment;

      if result=nil then
         tmp:=SetType
      else
      if result.IsArray then
         tmp:=result
      else
      begin
        Error('Cannot access [] element of type: '+result.ClassName);
        tmp:=nil;
      end;

      result:=TItemExpression.Create(AOwner);
      TItemExpression(result).Value:=tmp;
      ParseItemExpression(TItemExpression(result));

      if not IsChar(']') then
         Error('Missing ] unbalanced bracket in array expression');
    end
    else
    begin
      if AddressOf and (result<>nil) then
         break;

      if IsOperator(C,tmpOp) then
      begin
        Increment;

        if result=nil then
        begin
          if (tmpOp=opSubtract) or (tmpOp=opAdd) then
             result:=OperatorOf(AOwner,nil,tmpOp)
          else
             Error('Operator missing left side');
        end
        else
           result:=OperatorOf(AOwner,result,tmpOp);

        // ie: -abc...fgh
        if TOperatorExpression(result).Right is TRangeExpression then
        begin
          tmp:=result;

          result:=TOperatorExpression(result).Right;
          TOperatorExpression(tmp).Right:=TRangeExpression(result).StartRange;
          TRangeExpression(result).StartRange:=tmp;

          //AOwner.Error('Internal: Invalid Type Expression');
        end;

        Continue; //break;
      end;

      tmpIsLiteral:=False;

      if C='$' then
      begin
        S:='$'+NextHexadecimal;

        if TryStrToInt64(S,tmpInt) then
        begin
          result:=TNumber.Create(AOwner);
          TNumber(result).Value:=tmpInt;
          TNumber(result).Hexadecimal:=True;
          tmpIsLiteral:=True;
        end
        else
          tmpIsLiteral:=False;
      end
      else
      if (C='#') or (C='''') or (C='^') then
      begin
        result:=TConstantDeclaration.NewStringFrom(AOwner,NextString);
        tmpIsLiteral:=result<>nil;
      end;

      // >=210 Helpers
      if tmpIsLiteral {and (P.CompilerVersion>=210)} then
         if (PeekChar<>'.') or (PeekTwo='..') then
            Continue;

      if (not tmpIsLiteral) or
         (PeekChar<>'.') or
         (PeekTwo='..') then
      begin
        S:=PeekIdentifier;

        i:=Pos('..',S);

        if i=0 then
        begin
          if TextIn(S,Terminators)<>-1 then
             break
          else
          if TChar.IsDigit(C) then
          begin
            S:=NextIdentifier;

            if (PeekChar='.') and (GetNextNext<>'.') then
            begin
              Increment;
              S:=S+'.'+NextIdentifier;
            end;
          end
          else
             NextToken(IdentifierCharsNoDot,S);
        end
        else
        begin
          S:=NextTokenUpTo('.');

          if (i=1) and (S='') and (result<>nil) then
          begin
            Increment;
            Increment;

            result:=TRangeExpression.CreateRange(AOwner,result,
                ExpressionRequire(AOwner,Terminators,AKind,OnlyConstants));

            break;
          end;
        end;

        if (result=nil) and (S='') then
           Error('Internal error. Empty token');

        if IsOperatorWord(UpperCase(S),tmpOp) then
        begin
          if tmpOp=opNot then
          begin
            if result=nil then
            begin
              result:=TOperatorExpression.Create(AOwner);
              TOperatorExpression(result).Operat:=tmpOp;
              TOperatorExpression(result).Left:=ExpressionRequire(AOwner,Terminators);

              result.TypeIdentifier:=TOperatorExpression(result).CalcType;
            end
            else
              Error('"not" operator after expression');

            Continue; //break;
          end
          else
          if (result<>nil) or ( (tmpOp<>opIn) and (tmpOp<>opIs) and (tmpOp<>opAt) ) then
          begin
            result:=OperatorOf(AOwner,result,tmpOp);
            Continue; //Exit; //break;
          end;
        end;

        if (NilConst<>nil) and SameText(S,NilConst.Name) then
           result:=NilConst
        else
        if SameText(S,Language.SelfKeyword) then
           result:=FindSelf(AOwner)
        else
        if SameText(S,'INHERITED') then
        begin
          result:=TInheritedExpression.Create(AOwner);

          if ParseInherited(TInheritedExpression(result)) then
             break;
        end
        else
           result:=Literal(AOwner,S);

        if result=nil then
        begin
          if Language.CompilerVersion>=210 then
             result:=IsAnonymousMethod(S);

          if result=nil then
          begin
             if (AKind=skTypeOnly) and (PeekChar='<') then
             begin
               Increment;
               result:=GetTypedType(AOwner,AKind,S);
             end
             else
             begin
               {
               // xml.xmldom.IDOMImplementation(....

               if P.CompilerVersion>=230 then
                  result:=UsedUnits.Scopes.SortedFind(S); // Is Unit Scope ?
               }

               if result=nil then
               begin
                 tmpSymbol.Origin:=AOwner;
                 tmpSymbol.Owner:=AOwner;
                 tmpSymbol.AsPointer:=AddressOf;
                 tmpSymbol.AResult:=AResult;
                 tmpSymbol.Name:=S;

                 if (AKind=skTypeOnly) and (PeekTwo='..') then
                    tmpSymbol.Kind:=skField
                 else
                    tmpSymbol.Kind:=AKind;

                 tmpSymbol.TypeParams:=TryGuessTypeParams(AOwner);

                 result:=GuessSymbol(tmpSymbol);
               end;

               if (result is TUnitScope) and SameText(TUnitScope(result).Name,'SYSTEM') then
                   result:=Language.FindModule('System');
             end;

             if result=nil then
             begin
               Error('Identifier not found: '+S);
               Exit;
             end;
          end;
        end;
      end;
    end;

    while IsChar('^') do
          result:=TDereferenceExpression.CreateValue(AOwner,result);

    C:=PeekChar;

    if C='.' then
    begin
      if GetNextNext='.' then
      begin
        Increment;
        Increment;

        result:=TRangeExpression.CreateRange(AOwner,result,
                    ExpressionRequire(AOwner,Terminators,skField,OnlyConstants));

        break;
      end;

      repeat
        Increment;

        IsChar('&'); // skip VER210

        NextToken(IdentifierCharsNoDot,S);
        tmpSymbol.TypeParams:=TryGuessTypeParams(AOwner);

        if IsChar('(') then // call or casting
           result:=RequireCallorCast(AOwner,AOwner,result,AResult,S,nil,tmpSymbol.TypeParams)
        else
        begin
          if result is TUnitScope then
             tmp:=FindScopeOrUnit(TUnitScope(result).Name+'.'+S)
          else
          if result is TImplementationModule then
             tmp:=Language.FindModule(TImplementationModule(result).Name+'.'+S)
          else
             tmp:=nil;

          if tmp=nil then
          begin
            tmp:=Language.GuessIdentifier(skField,result,S,True,AResult,nil,tmpSymbol.TypeParams,result);

            if tmp=nil then
               if result is TIdentifier then // TImplementationModule
               begin
                 tmp:=Language.FindModule(TIdentifier(result).Name+'.'+S);

                 if tmp=nil then
                    tmp:=TryToFindUnit(TIdentifier(result).Name+'.'+S);
               end;

            if tmp=nil then
               Error('Missing field or method: '+S+' of '+result.ClassName)
            else
            begin
              if (tmp is TMethodDeclaration) and (not AddressOf) then
              begin
                tmpParams:=tmpSymbol.TypeParams;

                if tmpParams<>nil then
                   tmp:=TTypedMethod.CreateMethod(tmp,TMethodDeclaration(tmp),tmpParams);
              end;

              result:=ChainField(AOwner,result,tmp);
            end;
          end
          else
            result:=tmp;
        end;

        while IsChar('^') do
              result:=TDereferenceExpression.CreateValue(AOwner,result);

        C:=PeekChar;

        if (C='.') and (GetNextNext='.') then
           Break;

      until C<>'.';

    end
    else
    if C=':' then
    begin
      if OnlyConstants then
         break
      else
      if GetNextNext<>'=' then // Special case for Str(X:w:d, ...)
      begin
        tmp:=result;
        result:=TStringFormatExpression.Create(result);
        TStringFormatExpression(result).Expression:=tmp;
        ParseStringFormat(TStringFormatExpression(result));
      end;
    end
    {
    else
    if (AKind=skTypeOnly) and (C<>'[') then
       break;
    }

  until False;
end;

function TPascalParse.ParseDefaultValueOf(const AOwner:TBlock; const AType:TTypeDeclaration; const AIndex:Integer=0):TExpression;

  procedure ParseRecordValues(const AFields:TRecordFieldsDeclarations);
  var tmpName : String;
      tmpField : TVariableDeclaration;
  begin
    if IsChar('(') then
    begin
      if PeekChar<>')' then
      repeat
        tmpName:=NextIdentifier;

        tmpField:=AFields.Find(tmpName);

        if tmpField=nil then
           Error('Invalid record field name: '+tmpName);

        if IsChar(':') then
        begin
          if tmpField.TypeIdentifier=nil then
             Error('Internal: Missing type: '+tmpField.ClassName);

          tmpField.Value:=ParseDefaultValueOf(AOwner,tmpField.TypeIdentifier);

          if not IsChar(';') then
             break;
        end
        else
          Error('Expected ":"');

        if PeekChar=')' then
           break;

      until False;

      if not IsChar(')') then
         Error('Missing ) in record constant values');
    end;
  end;

var tmpSpec : TTypeSpecification;
begin
  tmpSpec:=AType.Expression;

  // Special trick for TGUID as string, ie: '{959dc450-8d9e-11cf-8736-00aa00a485eb}'
  if (tmpSpec=GUIDSpec) and (PeekChar<>'(') then
     result:=ExpressionRequire(AOwner,['DEPRECATED','PLATFORM'],skField)
  else
  if tmpSpec is TArraySpecification then
  begin
    if PeekChar='(' then
    begin
      result:=TArraySpecification(tmpSpec).ValuesOfIndex(AOwner,AIndex);
      ParseArrayValues(TArrayValues(result));
    end
    else
       result:=ExpressionRequire(AOwner);
  end
  else
  if (not (tmpSpec is TClassSpecification)) and
     (not (tmpSpec is TInterfaceSpecification)) and
     (tmpSpec is TRecordSpecification) then
  begin
    result:=TRecordSpecification(tmpSpec).CloneFields(AOwner);
    ParseRecordValues(TRecordInstance(result).Fields);
  end
  else
    result:=ExpressionRequire(AOwner,['DEPRECATED','PLATFORM'],skField,False,(tmpSpec is TMethodSpecification) or (tmpSpec=PointerSpec));

  if result=nil then
     Error('Cannot get value of type: '+AType.Name);
end;

function TPascalParse.ParseArrayType(const AOwner:TBlock; const AParams:TTypeParameters):TArraySpecification;
var IsPacked : Boolean;
begin
  IsPacked:=IsToken('PACKED');

  result:=TArraySpecification.Create(AOwner);
  result.TypeParameters:=AParams;
  ParseArray(result);

  result.IsPacked:=IsPacked;
end;

function TPascalParse.GuessTypeIdentifier(const AOwner:TBlock):TExpression;
begin
  result:=ExpressionRequire(AOwner,
       ['BEGIN','OF','READ','WRITE','INDEX','STDCALL','READONLY',
        'ABSOLUTE','DISPID','FUNCTION','PROCEDURE',
        'WRITEONLY','REGISTER','CDECL','CONST','VAR','TYPE','THREADVAR','LABEL',
        'DO','END','DEPRECATED','PLATFORM','OVERLOAD','PASCAL'],
        skTypeOnly);
end;

procedure TPascalParse.ParseTypeExpression(const AOwner:TBlock;
            out ForwardExpression:String;
            var AType:TTypeDeclaration;
            const ATypeOwner:TBlock;
            const AName:String;
            const AParams:TExpressions=nil);

  function FindAType:TTypeDeclaration;
  var S : String;
      i : Integer;
      tmp : TTypeDeclaration;
  begin
    S:=PeekIdentifier;

    i:=Pos('.',S);

    if i>0 then
    begin
      result:=GuessTypeDeclaration(AOwner);
    end
    else
    begin
      S:=NextIdentifier;
      result:=Language.GuessType(AOwner,S,AParams);
    end;

    if result=nil then
       ForwardExpression:=S;

    SkipStringRange(S);

    if (result<>nil) and IsChar('(') then
       if result.HasCodePage then
       begin
         tmp:=result;

         result:=TTypeDeclaration.Create(AOwner);
         result.Name:=tmp.Name;
         result.Alias:=tmp;
         result.Expression:=tmp.Expression;
         result.CodePage:=ExpressionRequire(result);

         if not IsChar(')') then
            Error('Missing ) in type CodePage expression');
       end
       else
          Error('CodePage only allowed in AnsiString types, compiler version >= 200');
  end;

  function SetMethod(const AClass:TMethodSpecificationClass):TMethodSpecification;
  begin
    NextIdentifier;

    result:=AClass.Create(AOwner);
    result.TypeParameters:=AParams;
    ParseMethodSpec(result);

    OptionalSemicolon;

    if SameText(PeekIdentifier,'OF') then
    begin
      NextIdentifier;

      if SameText(NextIdentifier,'OBJECT') then
      begin
        result.OfObject:=True;
        OptionalSemicolon;
      end
      else
         Error('Expected: Object');
    end;

    ParseMethodDirectives(result);
  end;

  procedure ParseSetOf(var AType:TTypeDeclaration);
  var tmpSet : TSetSpecification;
      tmpExp : TExpression;
  begin
    if PeekChar='(' then
    begin
      tmpSet:=TSetSpecification.Create(AOwner);
      tmpSet.IsSetOf:=True;
      ParseSetSpecification(tmpSet);

      AType.Expression:=TSetOfSpecification.CreateType(AOwner,
                         TTypeDeclaration.CreateSpec(AOwner,tmpSet))
    end
    else
    begin
      tmpExp:=GuessTypeIdentifier(AType);

      //tmpExp:=TExpression.Require(AOwner,['PLATFORM','DEPRECATED'],skTypeOnly);

      if (tmpExp is TFieldExpression) and (TFieldExpression(tmpExp).Field is TTypeDeclaration) then
         AType.Expression:=TSetOfSpecification.CreateType(AOwner,TTypeDeclaration(TFieldExpression(tmpExp).Field))
      else
      if tmpExp is TTypeDeclaration then
         AType.Expression:=TSetOfSpecification.CreateType(AOwner,TTypeDeclaration(tmpExp))
      else
      if tmpExp is TRangeExpression then
         AType.Expression:=TSetRange.CreateRange(AOwner,TRangeExpression(tmpExp))
      else
      if (tmpExp is TNumber) or (tmpExp is TOperatorExpression) then
      begin
        if IsChar('.') and IsChar('.') then // Copy(P.PeekToken,1,2)='..' then
           AType.Expression:=TSetRange.CreateRange(AOwner,
                 TRangeExpression.CreateRange(AType,tmpExp,
                                            ExpressionRequire(AOwner)))
        else
          Error('Wrong set type range');
      end
      else
        Error('Wrong set type: '+tmpExp.ClassName);
    end;
  end;

  procedure NewType;
  begin
    if AName='' then
       AType:=TTypeDeclaration.Create(ATypeOwner);
  end;

var S : String;
    IsPacked : Boolean;
    tmpExp : TExpression;
    tmpType : TTypeDeclaration;
    tmpClassOf : TClassOf;
    C : Char;
    tmpSet : TSetSpecification;
begin
  IsPacked:=IsToken('PACKED');

  C:=PeekChar;

  if C='(' then
  begin
    NewType;

    tmpSet:=TSetSpecification.Create(AOwner);
    tmpSet.IsScoped:=ScopedEnums;
    AType.Expression:=tmpSet;
    ParseSetSpecification(tmpSet);

    // >=290
    if (TypeKindType<>nil) and SameText(AType.Name,'TTypeKind') then
       TypeKindType.Expression:=AType.Expression;
  end
  else
  if C='^' then
  begin
    Increment;

    NewType;

    AType.Expression:=TTypePointerOf.Create(AOwner);

    TTypePointerOf(AType.Expression).TypeIdentifier:=FindAType;
  end
  else
  begin
    S:=PeekIdentifier;

    if SameText(S,'TYPE') then
    begin
      NextIdentifier;

      NewType;

      AType.Expression:=TTypeTypeOf.Create(AOwner);

      IsToken('OF');

      TTypeTypeOf(AType.Expression).TypeIdentifier:=FindAType;

      ParseDirectives(AType);
    end
    else
    if SameText(S,'RECORD') then
    begin
      NextIdentifier;
      PeekToken(S);

      NewType;

      if SameText(S,'HELPER') then // class helper for ...
      begin
        if Language.CompilerVersion<200 then
           Error('Record Helper only allowed for Compiler Version >= 200');

        SkipToken;
        RequireToken('FOR');

        AType.Expression:=TRecordHelperSpecification.Create(AOwner);

        TRecordHelperSpecification(AType.Expression).SetType(FindAType);
      end
      else
        AType.Expression:=TRecordSpecification.Create(AOwner);

      TRecordSpecification(AType.Expression).SetRecordType(AType,AParams);

      ParseRecord(TRecordSpecification(AType.Expression));

      TRecordSpecification(AType.Expression).IsPacked:=IsPacked;

      ParseDirectives(AType);
    end
    else
    if SameText(S,'CLASS') then
    begin
      NextIdentifier;
      PeekToken(S);

      NewType;

      if S=';' then // Forward class
      begin
        TTypeAccess(AType).ForwardExpression:=AName; //??

        AType.Expression:=TClassSpecification.Create(AOwner);

        TRecordSpecification(AType.Expression).SetRecordType(AType,AParams);
      end
      else
      begin
        if (ObjectType=nil) and SameText(AType.Name,'TObject') then
            ObjectType:=AType;

        if SameText(S,'OF') then // class of
        begin
          SkipToken;

          S:=NextIdentifier;

          tmpClassOf:=TClassOf.Create(AType);
          AType.Expression:=tmpClassOf;
          tmpClassOf.IsPacked:=IsPacked;

          tmpType:=Language.GuessType(AOwner,S); // <-- subtypes not allowed

          if tmpType=nil then
          begin
            tmpClassOf.TypeIdentifier:=TTypeDeclaration.CreateName(AType,S);
            TTypeAccess(tmpClassOf.TypeIdentifier).ForwardExpression:=S;

            //AVOID DUPLICATE: TTypeDeclarations(AOwner).AddSorted(tmpClassOf.TypeIdentifier);
          end
          else
            tmpClassOf.TypeIdentifier:=tmpType;
        end
        else
        begin
          if SameText(S,'HELPER') then // class helper for ...
          begin
            if Language.CompilerVersion<190 then
               Error('Class Helper only allowed for Compiler Version >= 190');

            SkipToken;
            RequireToken('FOR');

            AType.Expression:=TClassHelperSpecification.Create(AOwner);

            TClassHelperSpecification(AType.Expression).SetType(FindAType);
          end
          else
            AType.Expression:=TClassSpecification.Create(AOwner);

          TRecordSpecification(AType.Expression).SetRecordType(AType,AParams);

          ParseClass(TClassSpecification(AType.Expression));

          TClassSpecification(AType.Expression).IsPacked:=IsPacked;

          ParseDirectives(AType);
        end;
      end;
    end
    else
    if SameText(S,'OBJECT') then
    begin
      NextIdentifier;

      NewType;

      AType.Expression:=TObjectSpecification.Create(AOwner);
      TObjectSpecification(AType.Expression).SetRecordType(AType,nil);

      ParseRecord(TObjectSpecification(AType.Expression));
      TObjectSpecification(AType.Expression).IsPacked:=IsPacked;

      ParseDirectives(AType);
    end
    else
    if SameText(S,'INTERFACE') then
    begin
      NextIdentifier;

      PeekToken(S);

      NewType;

      AType.Expression:=TInterfaceSpecification.Create(AOwner);

      TRecordSpecification(AType.Expression).SetRecordType(AType,AParams);

      if S=';' then // Forward class
         TTypeAccess(AType).ForwardExpression:=AName //??
      else
      begin
        ParseRecord(TRecordSpecification(AType.Expression));
        ParseDirectives(AType);

        if IInterfaceType=nil then
        begin
           if Language.CompilerVersion<140 then // D5
           begin
             if SameText(AType.Name,'IUnknown') then
                IInterfaceType:=AType;
           end
           else
           if SameText(AType.Name,'IInterface') then
              IInterfaceType:=AType;
        end;
      end;
    end
    else
    if SameText(S,'DISPINTERFACE') then
    begin
      NextIdentifier;

      NewType;

      AType.Expression:=TDispatchInterfaceSpecification.Create(AOwner);
      TDispatchInterfaceSpecification(AType.Expression).SetRecordType(AType,nil);
      ParseRecord(TDispatchInterfaceSpecification(AType.Expression));

      ParseDirectives(AType);
    end
    else
    if SameText(S,'SET') then // type bar = set of foo;
    begin
      NextIdentifier;
      NewType;

      RequireToken('OF');
      ParseSetOf(AType);
    end
    else
    if SameText(S,'ARRAY') then
    begin
      NextIdentifier;
      NewType;

      AType.Expression:=ParseArrayType(AType {AOwner},AParams);
      TArraySpecification(AType.Expression).IsPacked:=IsPacked;
    end
    else
    if SameText(S,'PROCEDURE') then
    begin
      NewType;
      AType.Expression:=SetMethod(TProcedureSpecification);
    end
    else
    if SameText(S,'FUNCTION') then
    begin
      NewType;
      AType.Expression:=SetMethod(TFunctionSpecification);
    end
    else
    if SameText(S,'REFERENCE') then
    begin
      if Language.CompilerVersion<200 then
         Error('Method reference types only allowed for Compiler Version >= 200');

      SkipToken;
      RequireToken('TO');

      NewType;
      AType.Expression:=TReferenceSpecification.Create(AOwner);
      TReferenceSpecification(AType.Expression).TypeParameters:=AParams;

      S:=NextIdentifier;

      if SameText(S,'PROCEDURE') then
         TReferenceSpecification(AType.Expression).Method:=SetMethod(TProcedureSpecification)
      else
      if SameText(S,'FUNCTION') then
         TReferenceSpecification(AType.Expression).Method:=SetMethod(TFunctionSpecification)
      else
         Error('Reference type must be to Procedure or Function');
    end
    else
    begin
      if AType=nil then
         tmpExp:=GuessTypeIdentifier(AOwner)
      else
         tmpExp:=GuessTypeIdentifier(AType);

      if tmpExp is TFieldExpression then
         tmpExp:=TScopedType.CreateScope(AOwner,TFieldExpression(tmpExp));

      if tmpExp=nil then
         Error('Missing Type specification');

      if tmpExp is TRangeExpression then
      begin
        NewType;

        AType.Expression:=TTypeRange.Create(AOwner);
        TTypeRange(AType.Expression).Range:=TRangeExpression(tmpExp);
      end
      else
      if tmpExp is TItemExpression then
      begin
        NewType;

        AType.Expression:=TTypeItem.Create(AOwner);
        TTypeItem(AType.Expression).Item:=TItemExpression(tmpExp);
      end
      else
      if AName='' then
         AType:=TTypeDeclaration(tmpExp)
      else
      begin
        if tmpExp is TTypeDeclaration then
        begin
          AType.Alias:=TTypeDeclaration(tmpExp);

          //if AType.Alias.Expression.TypeParameters=nil then
             AType.Expression:=AType.Alias.Expression;
        end
        else
          Error('Internal: Expression not a type: '+tmpExp.ClassName);
      end;
    end;
  end;

  if (AType=nil) or ((AType.Alias=nil) and (AType.Expression=nil)) then
     Error('Type specification not valid');
end;

function TPascalParse.FindScopeOrUnit(const S:String):TExpression;
var tmp : String;
begin
  result:=Language.Modules.Scopes.Find(S);

  if result=nil then
  begin
    result:=Language.FindModule(S);

    // Find unit with partial name
    if result=nil then
    begin
      if IsChar('.') then
      begin
        NextToken(IdentifierCharsNoDot,tmp);
        result:=FindScopeOrUnit(S+'.'+tmp);
      end;
    end;
  end;
end;

procedure TPascalParse.Finish(const AUses:TUses);
var t : Integer;
    tmp : TUnit;
begin
  for t:=0 to AUses.Count-1 do
  begin
    tmp:=TUnit(AUses.Item[t]);

    if tmp.ModuleImplementation.Empty then
       TPascalParse(tmp.Parser).ParseImplementation(tmp);
  end;
end;

procedure TPascalParse.ParseOn(const AOn:TOn);
var S : String;
    tmpState : TParserState;
    tmpIsColon : Boolean;
begin
  //inherited;

  // Pending: Check Owner is a TTryExcept clause

  tmpState:=State;
  S:=NextIdentifier;
  tmpIsColon:=PeekChar=':';
  State:=tmpState;

  if tmpIsColon then
  begin
    AOn.Exception:=TVariableDeclaration.Create(AOn);
    ParseConstant(AOn.Exception);
  end
  else
    AOn.Exception:=TVariableDeclaration.CreateType(AOn,GuessTypeDeclaration(AOn.Owner));

  if AOn.Exception=nil then
     Error('Invalid expression at except ON clause');

  // Pending: Verify Exception is a TException-derived type !

  NextToken(S);

  if SameText(S,'DO') then
     AOn.DoPart:=RequireStatements(AOn)
  else
  if not SameText(S,'DO;') then
     Error('Expected: DO');

  if IsToken('ELSE') then
     AOn.ElsePart:=RequireStatements(AOn);
end;

function TPascalParse.SkipStringRange(const S:String):String;
begin
  result:='';

  if SameText(S,'STRING') then
     if IsChar('[') then
     begin
       result:=NextTokenUpTo(']');

       if not IsChar(']') then
          Error('Missing ] in string range');
     end;
end;

function TPascalParse.InternalGetIdentifier(const AOwner:TBlock):String;
var S : String;
begin
  result:='';

  repeat
    IsChar('&'); // skip
    NextToken(IdentifierCharsNoDot,S);

    result:=result+S;

    SkipStringRange(S);

    if IsChar('.') then
       result:=result+'.'
    else
       break;

  until False;
end;

procedure TPascalParse.ParseMethodDirectives(const AMethod:TMethodSpecification);

  procedure ParseExternal;
  var tmp : String;
  begin
    if not IsChar(';') then // <-- Since VER210
    begin
      AMethod.ExternalDLL:=ExpressionRequire(AMethod,['NAME','DELAYED'],skField,True);

      tmp:=PeekIdentifier;

      if SameText(tmp,'NAME') then
      begin
        NextIdentifier;
        AMethod.ExternalName:=ExpressionRequire(AMethod,['FUNCTION','PROCEDURE','END',
                     'END.','VAR','CONST','TYPE','DELAYED'],skField,True);

        tmp:=PeekIdentifier;
      end;

      if SameText(tmp,'DELAYED') then
      begin
        AMethod.IsDelayed:=True;
        NextIdentifier;
        OptionalSemicolon;
      end;
    end;
  end;

const
  Msg_DynamicAndVirtual='Dynamic and Virtual directives are mutually exclusive';

  procedure ProcessDirective(var Directives:TMethodDirectives; const AIndex:Integer);
  begin
    case AIndex of
      0: Include(Directives,mdAbstract);
      1: Include(Directives,mdAssembler);
      2: Include(Directives,mdCDecl);
      3: begin
           Include(Directives,mdDeprecated);

           if PeekChar='''' then
              AMethod.DeprecatedMessage:=TConstantDeclaration.NewStringFrom(AMethod,NextString);

           OptionalSemicolon;
         end;
      4: begin
          AMethod.DispatchID:=ExpressionRequire(AMethod,[],skField,True);
          OptionalSemicolon;
         end;
      5: begin
           if mdVirtual in Directives then
              Error(Msg_DynamicAndVirtual);

           Include(Directives,mdDynamic);
         end;
      6: Include(Directives,mdExport);

      7: begin
           AMethod.IsExternal:=True;
           ParseExternal;
         end;

      8: Include(Directives,mdFar);
      9: Include(Directives,mdFinal);
     10: Include(Directives,mdForward);
     11: Include(Directives,mdInline);
     12: Include(Directives,mdLocal);

     13: begin
          AMethod.MessageField:=ExpressionRequire(AMethod,[],skField,True);
          OptionalSemicolon;
         end;

     14: Include(Directives,mdNear);
     15: ; // OF
     16: Include(Directives,mdOverload);
     17: Include(Directives,mdOverride);
     18: Include(Directives,mdPascal);
     19: Include(Directives,mdPlatform);
     20: Include(Directives,mdRegister);
     21: Include(Directives,mdReintroduce);
     22: Include(Directives,mdSafeCall);

     23: if AMethod.IsClass then
            Include(Directives,mdStatic)
         else
            Error('Static directive only allowed for class methods');

     24: Include(Directives,mdStdCall);
     25: Include(Directives,mdUnsafe);
     26: Include(Directives,mdVarArgs);

     27: begin
           if mdDynamic in Directives then
              Error(Msg_DynamicAndVirtual);

           Include(Directives,mdVirtual);
         end;
    end;
  end;

var S : String;
    tmp : Integer;
begin
  repeat
    S:=PeekToken(IdentifierCharsNoDot);

    if DirectiveTerminators.Find(S,tmp) then
    begin
      NextIdentifier;

      ProcessDirective(AMethod.Directives,tmp);

      OptionalSemicolon;
    end
    else
      break;

  until False;
end;

initialization
{
  if OperatorKinds=nil then
     if Parent.IsFPC then
        OperatorKinds:=TBaseList.CreateStrings(
           [':=','+','-','*','/','**','=','<','<=','>','>=','><','IN','EXPLICIT'
           ])
     else
}
        OperatorKinds:=TBaseList.CreateStrings(
           ['ADD', 'BITWISEAND', 'BITWISEOR', 'BITWISEXOR', 'DEC','DIVIDE','EQUAL',
            'EXPLICIT','GREATERTHAN','GREATERTHANOREQUAL','IMPLICIT',
            'IN','INC','INTDIVIDE','LEFTSHIFT','LESSTHAN','LESSTHANOREQUAL',
            'LOGICALAND','LOGICALNOT','LOGICALOR','LOGICALXOR','MODULUS','MULTIPLY',
            'NEGATIVE','NOTEQUAL','POSITIVE','RIGHTSHIFT','ROUND','SUBTRACT','TRUNC'
           ]);

  {$IFDEF HAS_ARRAYCREATE}
  DefaultTerminators:=['END','ELSE','UNTIL','FINALLY','EXCEPT','FINALIZATION','END.'];
  {$ELSE}
  SetLength(DefaultTerminators,7);
  DefaultTerminators[0]:='END';
  DefaultTerminators[1]:='ELSE';
  DefaultTerminators[2]:='UNTIL';
  DefaultTerminators[3]:='FINALLY';
  DefaultTerminators[4]:='EXCEPT';
  DefaultTerminators[5]:='FINALIZATION';
  DefaultTerminators[6]:='END.';
  {$ENDIF}

  EscapeKeywords:=TBaseList.CreateStrings(
      ['ASM','BEGIN','CLASS','CONST','CONSTRUCTOR','DESTRUCTOR','END','END.',
       'END;','FUNCTION','IMPLEMENTATION','INITIALIZATION','LABEL',
       'PRIVATE','PROCEDURE','PROTECTED','PUBLIC','PUBLISHED','RESOURCESTRING',
       'STRICT','THREADVAR','TYPE','VAR']
      );

  DirectiveTerminators:=TBaseList.CreateStrings(
      ['ABSTRACT','ASSEMBLER','CDECL','DEPRECATED','DISPID','DYNAMIC',
       'EXPORT','EXTERNAL','FAR','FINAL','FORWARD','INLINE','LOCAL',
       'MESSAGE','NEAR','OF','OVERLOAD','OVERRIDE','PASCAL','PLATFORM',
       'REGISTER','REINTRODUCE','SAFECALL','STATIC','STDCALL',
       'UNSAFE',
       'VARARGS', // <-- VER210
       'VIRTUAL'
      ]);

finalization
  OperatorKinds.Free;
  EscapeKeywords.Free;
  DirectiveTerminators.Free;
end.
