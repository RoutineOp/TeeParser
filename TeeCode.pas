
// @davidberneda
// davidberneda@gmail.com
// December 2014

unit TeeCode;

{$IFDEF CPUX64}
{$EXCESSPRECISION OFF}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0}
     {$DEFINE HAS_INLINE}
     {$DEFINE D9}
  {$ENDIF}
  {$IF CompilerVersion >= 18.0}
     {$DEFINE HAS_RTTI}
  {$ENDIF}
  {$IF CompilerVersion >= 20.0}
     {$DEFINE HAS_CHARINSET}
     {$DEFINE HAS_TOSTRING}
     {$DEFINE D12}
  {$ENDIF}
  {$IF CompilerVersion >= 25.0}
     {$DEFINE HAS_TARRAY}
  {$ENDIF}
  {$IF CompilerVersion >= 25.0}
     {$DEFINE HAS_ARRAYCREATE}
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_RTTI}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

interface

uses
  SysUtils;

type
  // Helper record for debugging this unit when parsing.
  // See BreakOn global variable.
  {$IFOPT D+}
  TDebugBreak=record
    IsUnitImpl : Boolean;
    UnitImpl : String;
    Line : Integer;
    Stopped : Boolean;
  end;
  {$ENDIF}

  // Base Parser
  TDirectiveKind=(dkUnknown, dkInclude, dkResource, dkLink);

  TDirectiveEvent=procedure(Sender:TObject; const Kind:TDirectiveKind;
                            const ADirective:String) of object;

  TTriBoolean=(triTrue,triFalse,triTrueElse,triFalseElse);

  TParserPosition=record
    Line,
    Column,
    Current : Integer;
  end;

  TParserState=record
    C : Char;

    DefinesOk : Boolean;

    DefinesStackCount : Integer;
    DefinesStack : Array[0..99] of TTriBoolean;  // ? !!

    InComment,
    InBlockComment,
    InLineComment,
    InString,
    InStringQuote : Boolean;
    StartDefine : Integer;
    Define : String;
    Position : TParserPosition;
  end;

  TBlock=class;
  TAttribute=class;

  TCharset={$IFDEF HAS_CHARINSET}TSysCharSet{$ELSE}set of Char{$ENDIF};

  TLanguage=class;

  TBaseParser=class
  private
    procedure SetText(const AText:String);
  protected
    Directives : {$IFDEF HAS_TARRAY}TArray<TAttribute>{$ELSE}Array of TAttribute{$ENDIF};

    State : TParserState;

    L : Integer;
    FText : String;

    FLanguage : TLanguage;

    procedure AddGlobals(const Globals:TBlock; const CompilerVersion:Integer); virtual; abstract;
    function Advance:Char;
    procedure Error(const ABlock:TObject; const AMessage:String); overload;
    procedure Error(const AMessage:String); overload;
    procedure GetDefine(Sender:TObject; const AIdentifier:String; out AValue:String); virtual;
    function GetNextNext:Char;
    procedure Increment; virtual; abstract;
    procedure Init;
    procedure NewDirective(Sender:TObject; const Kind:TDirectiveKind; const ADirective:String);
  public
    CurrentPath : String;
    CurrentFile : String;

    ParserOwner : TBlock;

    IsUTF : Boolean;

    // TO REMOVE:
    PointerMath : Boolean;
    IsFPC, // <-- Lazarus
    IgnoreStrings : Boolean;
    ScopedEnums : Boolean;

    // Better a virtual function ?
    DefaultExtension : String; // Delphi = .pas, Lazarus = .pp, etc

    Constructor Create(const AOwner:TBlock; const ALanguage:TLanguage); virtual;

    function Eof:Boolean; {$IFDEF HAS_INLINE}inline;{$ENDIF}

    function IsChar(const C:Char):Boolean;
    function IsNextChar(const C:Char):Boolean;
    function IsToken(const S:String):Boolean;

    function PeekIdentifier:String; {$IFDEF HAS_INLINE}inline;{$ENDIF}

    function PeekChar:Char;

    function PeekTwo:String;
    procedure PeekToken(out S:String); overload;
    function PeekToken(out NewState:TParserState):String; overload;
    function PeekToken(const ValidChars:TCharset):String; overload;

    function Next:Char; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function NextHexadecimal:String;
    function NextIdentifier:String; {$IFDEF HAS_INLINE}inline;{$ENDIF}

    function NextCharIs(const C:Char):Boolean;
    function NextLine:String;
    function NextString:String;
    procedure NextToken(out S:String); overload; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure NextToken(const ValidChars:TCharset; out S:String); overload;
    function NextTokenUpTo(const Terminator:Char):String;

    procedure OptionalSemicolon; {$IFDEF HAS_INLINE}inline;{$ENDIF}

    function PrevCharIs(const C:Char):Boolean;
    procedure RequireSemicolon;
    procedure RequireToken(const AName:String);

    procedure SkipToken;

    class function TextInCase(const S:String; const Texts:Array of String):Integer;
    class function TextIn(const S:String; const Texts:Array of String):Integer;
    class function TextOf(const AFile:String):String;

    property Language:TLanguage read FLanguage;
    property Text:String read FText write SetText;
  end;

  // Parsing exception
  ECodeException=class(Exception)
    CurrentFile : String;
    Parser : TBaseParser;
    Position : TParserPosition;
    Block : TObject;
  end;

  TBlockClass=class of TBlock;

  TBlocks={$IFDEF HAS_TARRAY}TArray<TBlock>{$ELSE}Array of TBlock{$ENDIF};

  // Main base class
  TBlock=class
  private
    // Gathered during parsing, position of this block in its source text file
    Position : TParserPosition;

    // Internal, simply to destroy "children" in an ordered manner
    //Owned : TBlocks;

    //{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    Parent : TBaseParser;

    procedure AddDirectives;
    //procedure SetOwned;
  protected
    procedure Error(const S:String);
  public
    //{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    Owner : TBlock;

    //{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    Module : TBlock;

    // Index inside the Package.Blocks array
    Index : Integer;

    // [...]
    Attributes : {$IFDEF HAS_TARRAY}TArray<TAttribute>{$ELSE}Array of TAttribute{$ENDIF};

    Constructor Create(const AOwner:TBlock); virtual;

    {$IFOPT D+}
    Destructor Destroy; override;
    {$ENDIF}

    procedure AddAttribute(const Attr:TAttribute);
    procedure SetOwner(const AOwner:TBlock);

    property Parser:TBaseParser read Parent write Parent;
  end;

  // [...]
  TAttribute=class(TBlock)
  public
    Text : String;
  end;

  // {$...}
  TDirective=class(TAttribute)
  public
    Kind : TDirectiveKind;

    Constructor CreateKind(const AOwner:TBlock; const AKind:TDirectiveKind; const AText:String);
  end;

  TTypeDeclaration=class;

  TFieldExpression=class;

  TExpression=class;

  TExpressions={$IFDEF HAS_TARRAY}TArray<TExpression>{$ELSE}Array of TExpression{$ENDIF};

  TTypeParameters=TExpressions;

  TTypeSpecification=class;

  TConstantDeclaration=class;

  TSymbolKind=(skField, skType, skTypeOnly);

  TFindSymbol=record
    Kind: TSymbolKind;
    Origin: TBlock;
    Owner: TBlock;
    Name: String;
    AsPointer: Boolean;
    AResult: TTypeSpecification;
    Params: TExpressions;
    TypeParams: TTypeParameters;
    Parent : TBlock;
  end;

  TStringArray=Array of String;

  TExpression=class(TBlock) // abstract
  public
    TypeIdentifier : TTypeDeclaration;

    function AsBoolean:Boolean; virtual;

    function Find(ASymbol:TFindSymbol):TExpression;
    function GuessType(const AOwner:TBlock):TTypeDeclaration;
    function IsArray:Boolean;
    function ToString:String; {$IFDEF HAS_TOSTRING}override{$ELSE}virtual{$ENDIF};
    function Value:String; virtual;
  end;

  // Default = vPublic
  TVisibility=(vPublic, vStrictPrivate, vPrivate, vStrictProtected, vProtected,
               vPublished);

  TIdentifier=class(TExpression)
  public
    Name : String;

    Visibility : TVisibility; // Default = vPublic

    AbsoluteRef : String; // <-- Pending: Find it

    IsDeprecated : Boolean;
    DeprecatedMessage : TExpression;

    IsInline : Boolean;
    IsPlatform : Boolean;

    Usage : Integer;

    Constructor CreateName(const AOwner:TBlock; const AName:String);

    function Qualified:String;
    function ToString:String; override;
  end;

  TVisibilityIdent=class(TIdentifier);

  TConstantDeclaration=class(TIdentifier)
  public
    NeedsType,
    AutomaticType,
    OptionalValue : Boolean; // For params only

    Value : TBlock; // <-- TExpression ?

    IsClass : Boolean;

    class function NewStringFrom(const AOwner:TBlock; const S:String):TConstantDeclaration;
  end;

  TVariableDeclaration=class(TConstantDeclaration)
  public
    Constructor Create(const AOwner:TBlock); override;
    Constructor CreateType(const AOwner:TBlock; const AType:TTypeDeclaration);
  end;

  TThreadVariable=class(TVariableDeclaration);

  TListItem=record
    Name : String;
    O : TObject;
  end;

  TBaseList = class
  private
    FList : {$IFDEF HAS_TARRAY}TArray<TListItem>{$ELSE}Array of TListItem{$ENDIF};
    FCapacity : Integer;

    function Remove(const AObject:TObject):Integer;
  public
    Count : Integer;

    IgnoreCase,
    Duplicates : Boolean;

    Constructor CreateStrings(const S:Array of String);

    function AddItem(const AName:String; const AObject:TObject):Integer;
    procedure Clear;
    function Exists(const S:String):Boolean; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function Find(const S:string; out Index: Integer):Boolean;
    function Get(const Index:Integer):TListItem; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  end;

  TMethodSpecification=class;

  TMethodDeclaration=class(TVariableDeclaration)
  public
    class function OwnerMethod(ABlock:TBlock):TMethodDeclaration;
  end;

  TExportDeclaration=class(TIdentifier) // TExpression !!
  public
    Method : TVariableDeclaration;
  end;

  TIdentifiers=class(TBlock)
  private
    Sorted : TBaseList;

  protected
    Items : {$IFDEF HAS_TARRAY}TArray<TIdentifier>{$ELSE}Array of TIdentifier{$ENDIF}; // To preserve order

    function Get(Index:Integer):TIdentifier; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function SortedFind(const AName:String):TIdentifier;
  public
    // Pending: Visibility : TVisibility; // Default = vPublic

    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    procedure AddSorted(const AIdentifier:TIdentifier);
    procedure Clear;
    function Count:Integer; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function IndexOf(const AIdentifier:TIdentifier):Integer;
    procedure Remove(const AIdentifier:TIdentifier);

    property Item[Index:Integer]:TIdentifier read Get; default;
  end;

  TCaseVariableItem=class(TVariableDeclaration)
  public
    Expression : TExpression;
  end;

  TCaseVariable=class(TVariableDeclaration)
  public
    Fields : TIdentifiers;

    Constructor Create(const AOwner:TBlock); override;

    function Find(const AName:String):TVariableDeclaration;
  end;

  TTypeSpecification=class(TBlock)
  private
    Helper : TTypeSpecification;
  protected
    function IsCompatible(ASpec:TTypeSpecification):Boolean; virtual;
    function IsSet:Boolean;
    function Redirect:TTypeSpecification;
  public
    function Find(const ASymbol:TFindSymbol):TExpression; virtual;
  end;

  TParseFieldKind=(fkField, fkParameter,fkThreadVar);

  TVariableDeclarations=class(TIdentifiers)
  private
    function Get(Index:Integer):TVariableDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  public
    ParseKind : TParseFieldKind; // temporary during parsing

    procedure Add(const AItem:TVariableDeclaration); virtual;
    function Find(const AName:String):TVariableDeclaration; virtual;

    property Item[Index:Integer]:TVariableDeclaration read Get; default;
  end;

  TConstantDeclarations=class;
  TTypeDeclarations=class;
  TMethodDeclarations=class;
  TLabelDeclarations=class;
  TResourceStrings=class;

  TSection=class(TBlock)
  public
    Bodies : Boolean;

    Constants : TConstantDeclarations;
    Methods   : TMethodDeclarations;
    Types     : TTypeDeclarations;
    Variables : TVariableDeclarations;

    Labels    : TLabelDeclarations;
    ResourceStrings : TResourceStrings;

    Ordered : TBlocks;

    procedure AddOrdered(const AList:TIdentifiers; const Initial:Integer);
    procedure AddOrderedSingle(const ABlock:TBlock);
    function Find(const ASymbol:TFindSymbol):TIdentifier;

    procedure Init;
  end;

  TTypeDeclaration=class(TIdentifier)
  private
    GenericSpec : TTypeSpecification;
  protected
    ForwardExpression : String; // internal, temporary
  public
    Alias : TTypeDeclaration;  // type Foo = Bar

    CodePage : TExpression; // VER200 :  type a=ansistring(65000)
    HasCodePage : Boolean;

    IsForward : Boolean;

    Expression : TTypeSpecification;

    TypeParameters : TTypeParameters;

    Constructor Create(const AOwner:TBlock); override;
    Constructor CreateSpec(const AOwner:TBlock; const ASpec:TTypeSpecification);

    function Generic:TTypeSpecification;
    class function TryExpand(const AType:TTypeDeclaration; var AResult:TTypeDeclaration;
                             const AParams:TTypeParameters):Boolean;
  end;

  TTypeDeclarationArray={$IFDEF HAS_TARRAY}TArray<TTypeDeclaration>{$ELSE}Array of TTypeDeclaration{$ENDIF};

  // <...; T:class...>
  TTypeConstraints=TTypeDeclarationArray;

  // <T>
  TTypeParameter=class(TTypeDeclaration)
  public
    Index : Integer; // The position order in the <A,B,C...> expression

    Constraints : TTypeConstraints;
  end;

  // Internal base
  TTypeRedirect=class(TTypeSpecification)
  public
    TypeIdentifier : TTypeDeclaration;

    function Find(const ASymbol:TFindSymbol):TExpression; override;
  end;

  // type Foo = ^Bar
  TTypePointerOf=class(TTypeRedirect);

  // type Foo = type Bar
  TTypeTypeOf=class(TTypeRedirect);

  // var Foo:Array... = (.....) // [....] ?
  TArrayValues=class(TExpression)
  public
    DimensionIndex : Integer;
    Items : TExpressions;

    //procedure Parse; override;
  end;

  // Foo[a,b,c...]
  TItemExpression=class(TExpression)
  public
    Value : TExpression;
    Items : TExpressions;
  end;

  // type Foo = String[123]
  TTypeItem=class(TTypeSpecification)
  public
    Item : TItemExpression;
  end;

  // type Foo = (Bar = 123, ...)
  TSetItem=class(TTypeDeclaration) // Only from: CompilerVersion >= 180
  public
    Value : TExpression;
  end;

  // type Foo = (Bar,Joe)
  TSetSpecification=class(TTypeSpecification)
  public
    Items : TTypeDeclarations;

    IsScoped : Boolean; // $ScopedEnums On
    IsSetOf : Boolean; // ?? remove?

    Constructor Create(const AOwner:TBlock); override;

    procedure Add(const AIdent:TIdentifier);
  end;

  TRangeExpression=class(TExpression)
  public
    StartRange,
    EndRange : TExpression;

    Constructor CreateRange(const AOwner:TBlock; const AStart,AEnd:TExpression);
  end;

  // type Foo = set of Bar
  TSetOfSpecification=class(TTypeSpecification)
  public
    TypeIdentifier : TTypeDeclaration;

    Constructor CreateType(const AOwner:TBlock; const AType:TTypeDeclaration);
  end;

  // type Foo = set of 0..99
  TSetRange=class(TTypeSpecification)
  public
    Range : TRangeExpression;

    Constructor CreateRange(const AOwner:TBlock; const ARange:TRangeExpression);
  end;

  // type Foo = 0..99
  TTypeRange=class(TTypeSpecification)
  private
    function IsNumber:Boolean;
  public
    Range : TRangeExpression;
  end;

  TConstantDeclarations=class(TIdentifiers)
  private
    function Get(Index:Integer):TConstantDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  protected
    TempIsClass : Boolean;
  public
    procedure Add(const AConstant:TConstantDeclaration); overload;
    function Find(const AName:String):TConstantDeclaration;

    property Item[Index:Integer]:TConstantDeclaration read Get; default;
  end;

  TResourceStrings=class(TConstantDeclarations);

  TModule=class;

  TUses=class(TIdentifiers)
  private
    function Get(AIndex:Integer):TModule; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  public
    procedure Add(const AModule:TModule);
    function Exclude(const AName:String):Boolean;
    function Find(const AName:String):TModule; overload;
    function Find(const ASymbol:TFindSymbol):TIdentifier; overload;
    procedure ParseModule(const AName,AFileName:String); virtual;

    property Item[Index:Integer]:TModule read Get; default;
  end;

  TNumber=class(TConstantDeclaration)
  public
    Hexadecimal : Boolean;
    Value : Int64;

    Constructor Create(const AOwner:TBlock); override;

    procedure SetValue(const AValue:Int64);
    function ToString:String; override;
  end;

  TFloatNumber=class(TConstantDeclaration)
  public
    Exponent : Boolean; // 1E-5  (-5 in operator expression)
    Value : Extended;
    ValueString : String;

    Constructor Create(const AOwner:TBlock); override;

    procedure SetValue(const AValue:Extended);
    function ToString:String; override;
  end;

  TChar=class(TConstantDeclaration)
  public
    Value : Char;

    Constructor Create(const AOwner:TBlock); override;

    class function IsDigit(const C:Char):Boolean;
    function ToString:String; override;
  end;

  TString=class(TConstantDeclaration)
  public
    Value : String;

    Constructor Create(const AOwner:TBlock); override;
    function ToString:String; override;
  end;

  TBoolean=class(TConstantDeclaration)
  public
    Value : Boolean;

    Constructor Create(const AOwner:TBlock); override;
    function ToString:String; override;
  end;

  TOperator=(opEqual, opEqualType, opNotEqual, opNotEqualType,
             opAnd, opOr, opXor, opShl, opShr,
             opLower, opGreater, opLowerEqual, opGreaterEqual,
             opAdd, opSubtract, opMultiply, opDivide,
             opNot, opDiv, opMod,
             opIn, opIs, opAs, opAt,
             opIncrement, opDecrement);

  // a <> b
  TOperatorExpression=class(TExpression)
  private
  public
    Operat : TOperator;
    Left,
    Right : TExpression;

    function AsBoolean:Boolean; override;

    function CalcType:TTypeDeclaration;
    function HasPrecedence(const AOperator:TOperator):Boolean;
  end;

  // ( ... )
  TNestedExpression=class(TExpression)
  public
    Expression : TExpression;
  end;

  TStringFormatExpression=class(TExpression)
  public
    Expression,
    Width,
    Decimals : TExpression;
  end;

  TArraySpecification=class;

  TRecordSpecification=class;

  TParameters=class(TVariableDeclarations);

  TPropertyDeclaration=class(TVariableDeclaration)
  private
    function SearchInherited(const ARecord:TRecordSpecification):TPropertyDeclaration;
  public
    IsDefault : Boolean;
    NoDefault : Boolean;

    Index : TExpression;

    PropertyType : TTypeDeclaration;

    Indexes : TParameters; // <-- To remove ?

    ReadPart : TExpression;
    WritePart : TExpression;

    Stored,
    DefaultValue : TExpression;

    ImplementsInterfaces : TExpressions;

    Ancestor : TPropertyDeclaration; // For re-published properties

    // Pending: Move to TDispatchInterfaceProperty
    ReadOnly,
    WriteOnly : Boolean;
    DispatchId : TExpression;

    RecordType : TTypeDeclaration;

    function ArrayTypeOf(const AType:TTypeDeclaration):TArraySpecification;
    procedure CheckImplements;
    function IndexExpression(const AddSelf:Boolean):TExpressions;
    function IndexesExpressions(const AddSelf:Boolean):TExpressions;
    function IsArray:Boolean;
    function IsIndexed:Boolean;
    function SameType(const P:TPropertyDeclaration):Boolean;
    procedure SearchInheritedType;
    function TypeExpression:TExpressions;
  end;

  TDispatchInterfaceProperty=class(TPropertyDeclaration)
  // ReadOnly,
  // WriteOnly : Boolean;
  // DispatchId : TExpression;
  end;

  TRecordFieldsDeclarations=class(TVariableDeclarations)
  private
    CaseVariables : Array of TCaseVariable;
  public
    Constructor Clone(const AOwner:TBlock; const AFields:TIdentifiers;
                      const Duplicates:Boolean=False);

    procedure Add(const AItem:TVariableDeclaration); override;
    function Find(const AName:String):TVariableDeclaration; override;
  end;

  TRecordInstance=class(TVariableDeclaration)
  public
    Fields : TRecordFieldsDeclarations;
  end;

  TTypeSpecifications=Array of TTypeSpecification;

  TPackage=class;

  TModule=class(TTypeDeclaration)
  public
    BlockCount : Integer;
    Blocks : TBlocks;

    FileName : String;

    ModuleIndex : Integer; // Used in reader/writer
    Used : Array of TModule;

    Package : TPackage;

    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    procedure Clear; virtual;
    function FindSymbol(const ASymbol:TFindSymbol):TIdentifier; virtual; abstract;
    class function ModuleOf(const AOwner:TBlock):TModule;
    procedure NewBlock(const Sender: TBlock);

    // Internal use, for streaming
    procedure AddUsage(const AIdent:TIdentifier);
  end;

  TGenericSpec=class(TTypeSpecification)
  protected
    function Expand(const AParams:TTypeParameters):TTypeSpecification; virtual;
  public
    TypeParameters : TTypeParameters; // <T,U,V...>

    function Find(const ASymbol:TFindSymbol):TExpression; override;
  end;

  TGenericTypeDeclaration=class(TTypeDeclaration)
  public
    Constructor CreateTyped(const AOwner:TBlock; const AType:TTypeDeclaration;
        const AParams:TTypeParameters);
  end;

  TRecordOperatorSpec=class;

  TRecordOperatorKind=(
     okAdd, okBitwiseAnd, okBitwiseOr, okBitwiseXor, okDec, okDivide, okEqual,
     okExplicit, okGreaterThan, okGreaterThanOrEqual, okImplicit,
     okIn, okInc, okIntDivide, okLeftShift, okLessThan, okLessThanOrEqual,
     okLogicalAnd, okLogicalNot, okLogicalOr, okLogicalXor, okModulus,
     okMultiply, okNegative, okNotEqual, okPositive, okRightShift, okRound,
     okSubtract, okTrunc);

  // type Foo = record...
  TRecordSpecification=class(TGenericSpec)
  private
    function FindOperator(const MatchResult:Boolean;
                          const ASpec:TTypeSpecification;
                          const AKind:TRecordOperatorKind):Boolean;
  public
    PropertyDefault : TPropertyDeclaration;

    Operators : Array[TRecordOperatorKind] of TTypeSpecifications;

    IsPacked : Boolean;

    Ancestors : TTypeDeclarationArray;
    RecordType : TTypeDeclaration;
    Outer : TRecordSpecification;
    Module : TModule;

    // Common to TSection:
    Constants : TConstantDeclarations;
    Fields : TRecordFieldsDeclarations;
    Methods : TMethodDeclarations;
    Types : TTypeDeclarations;

    Ordered : TBlocks;

    procedure AddAncestor(const AType:TTypeDeclaration);
    procedure AddField(const AField:TVariableDeclaration);
    procedure AddOperator(const AOperator:TRecordOperatorSpec; const AName:String);
    procedure AddOrdered(const AList:TIdentifiers; const Initial:Integer);
    procedure AddOrderedSingle(const ABlock:TBlock);
    class function AncestorOf(const AOwner:TBlock):TExpression;
    procedure CheckFields;
    function CloneFields(const AOwner:TBlock):TRecordInstance;
    function DefaultProperty(InAncestors:Boolean):TPropertyDeclaration;
    function Find(const ASymbol:TFindSymbol):TExpression; override;
    function FindAncestor(const AType:TTypeSpecification):TTypeDeclaration;

    function IsClassMeOrAncestor(const ARecord:TRecordSpecification):Boolean;

    class function OwnerRecordMethod(ABlock:TBlock):TTypeDeclaration;
    class function RecordOf(AOwner:TBlock):TRecordSpecification; //overload;
    procedure SetRecordType(const AType:TTypeDeclaration; const AParams:TExpressions);

    function TestVisibility(const AOrigin:TBlock;
                            const AOwner:TBlock; const AField:TIdentifier):Boolean;
  end;

  // type Foo = interface...
  TInterfaceSpecification=class(TRecordSpecification)
  protected
    function IsCompatible(ASpec:TTypeSpecification):Boolean; override;
  public
    function IsMeOrAncestor(const AInterface:TInterfaceSpecification):Boolean;
  end;

  // type Foo = dispinterface...
  TDispatchInterfaceSpecification=class(TInterfaceSpecification);

  // type Foo = class...
  TClassSpecification=class(TRecordSpecification)
  public
    IsAbstract,
    IsSealed : Boolean;

    function SupportsInterface(const AInterface:TInterfaceSpecification):Boolean;
  end;

  // type Foo = record helper for Bar ...
  TRecordHelperSpecification=class(TRecordSpecification)
  private
    Searching : Boolean;
  public
    TypeIdentifier : TTypeDeclaration;

    function Find(const ASymbol:TFindSymbol):TExpression; override;
    procedure SetType(const AType:TTypeDeclaration);
  end;

  // type Foo = class helper for Bar ...
  TClassHelperSpecification=class(TRecordHelperSpecification);

  // type Foo = class of Bar
  TClassOf=class(TTypeSpecification)
  public
    IsPacked : Boolean;
    TypeIdentifier : TTypeDeclaration;
  end;

  // type Foo = object...
  TObjectSpecification=class(TRecordSpecification); // <-- Generics ?

  // type Foo = array...
  TArraySpecification=class(TRecordSpecification) // <-- Bad: Record for >=210 Create constructor
  protected
    function CompatibleArrayTypes(const B:TTypeSpecification):Boolean;
    function CompatibleArrayValues(const AValues:TArrayValues):Boolean;
    function Expand(const AParams:TTypeParameters):TTypeSpecification; override;
    function SameArrayTypes(const B:TArraySpecification):Boolean;
  public
    IsPacked : Boolean;

    CreateConstructor : TMethodDeclaration;

    Expression : TTypeDeclaration; // <-- inherit from TTypeRedirect ?

    Dimensions : TExpressions;

    procedure AddDimension(const ADimension:TExpression);
    function Find(const ASymbol:TFindSymbol):TExpression; override;
    function ValuesOfIndex(const AOwner: TBlock; const AIndex:Integer): TArrayValues;
  end;

  TTypeDeclarations=class(TIdentifiers)
  private
    // Caches:
    SetOfSpecs,
    SetSpecs : TBaseList; // Array of TEnumSpecification;

    function Get(Index:Integer):TTypeDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function GetSorted(Index:Integer):TTypeDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  protected
    TempVisibility : TVisibility; // temporary during parsing
  public
    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    procedure AddSet(const AItem:TIdentifier; IsSetOf:Boolean);
    function Find(const ASymbol:TFindSymbol):TTypeDeclaration;
    procedure RelinkForward(StartType:Integer; const AType:TTypeDeclaration);
    procedure VerifyForward(StartType:Integer);

    property Item[Index:Integer]:TTypeDeclaration read Get; default;
  end;

  TMethodSpecifications=Array of TMethodSpecification;

  TMethodDeclarations=class(TIdentifiers)
  private
    function FindSpec(const ASpec:TMethodSpecification):TMethodDeclaration;
    function Get(Index:Integer):TMethodDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    function GetSorted(Index:Integer):TMethodDeclaration; {$IFDEF HAS_INLINE}inline;{$ENDIF}
  public
    Constructor Create(const AOwner:TBlock); override;

    procedure Add(const AMethod:TMethodDeclaration); overload;
    function Find(const ASymbol:TFindSymbol):TMethodDeclaration; overload;
    procedure FindAll(const AName:String; var AItems:TMethodSpecifications);

    property Item[Index:Integer]:TMethodDeclaration read Get; default;
  end;

  TUsesSection=class(TSection)
  public
    UsesUnits : TUses;

    Constructor Create(const AOwner:TBlock); override;
  end;

  TInterface=class(TUsesSection);

  TImplementation=class(TUsesSection)
  public
    Empty : Boolean;

    Constructor Create(const AOwner:TBlock); override;
  end;

  TStatements=class;

  TStatement=class(TBlock);

  TStatementArray=Array of TStatement;

  TStatements=class(TStatement)
  public
    BeginEnd : Boolean;
    Items : TStatementArray;

    procedure Add(const AStatement:TStatement);
  end;

  TASMStatements=class(TStatements)
  public
    Items : TStringArray;
  end;

  // TFoo(x)
  TCasting=class(TExpression)
  public
    Expression : TExpression;

    Constructor CreateType(const AOwner:TBlock; const AType:TTypeDeclaration;
                           const AExpression:TExpression);
    function ExpressionValue(out AType:TTypeDeclaration):TExpression;
  end;

  // @Foo
  TAddressOf=class(TVariableDeclaration)
  public
    Constructor CreateValue(const AOwner:TBlock; const AValue:TExpression);
  end;

  TAssignmentOperator=(aoAssign, aoAddAssign, aoSubtractAssign,
                       aoMultiplyAssign, aoDivideAssign, aoModAssign,
                       aoIncrement, aoDecrement);

  // Foo := Bar
  TAssignment=class(TStatement)
  public
    Operat : TAssignmentOperator;
    Left : TExpression;
    Right : TExpression;
  end;

  TIf=class(TStatement)
  public
    Expression : TExpression;
    ThenPart : TStatements;
    ElsePart : TStatements;
  end;

  TOn=class(TStatement)
  public
    Exception : TVariableDeclaration;
    DoPart : TStatements;
    ElsePart : TStatements;
  end;

  TWhile=class(TStatement)
  public
    Expression : TExpression;
    WhilePart : TStatements;
  end;

  TWith=class(TStatement)
  public
    Items : TExpressions;
    WithPart : TStatements;

    function Find(const ASymbol:TFindSymbol):TExpression;
  end;

  // abstract base
  TForStatement=class(TStatement)
  public
    Loop : TStatements;
  end;

  // Pascalish: for t:=0 to N do
  //   for x in y do
  TFor=class(TForStatement)
  public
    Enumerated : TExpression;
    Iterator : TVariableDeclaration;

    Start,
    ToValue,
    DownToValue : TExpression;
  end;

  TRepeat=class(TStatement)
  public
    RepeatPart : TStatements;
    Expression : TExpression;
  end;

  TTryExcept=class(TStatements);

  TTry=class(TStatement)
  public
    Block : TStatements;
    FinallyPart : TStatements;
    ExceptPart : TTryExcept;
    ElsePart : TStatements;
  end;

  TInstance=class(TConstantDeclaration);

  TRaise=class(TStatement)
  public
    Expression : TExpression;
  end;

  TParameter=class(TVariableDeclaration)
  public
    IsConst,
    IsVar,
    IsOut : Boolean;
  end;

  TBody=class(TSection)
  public
    Statements : TStatements;

    Constructor Create(const AOwner:TBlock); override;
  end;

  // JScript for(xxx,yyy,zzz; aaa<bbb; ccc++)...
  TForLoop=class(TForStatement)
  public
    Init,
    Step : TBody;

    Condition : TExpression;
  end;

  TMethodDirective=( mdAbstract,
    mdAssembler,
    mdCDecl,
    mdDynamic,
    mdExport,
    mdFar,
    mdForward,
    mdLocal,
    mdNear,
    mdOverload,
    mdOverride,
    mdPascal,
    mdReintroduce,
    mdRegister,
    mdSafeCall,
    mdStdCall,
    mdUnsafe,
    mdVirtual,
    mdFinal,
    mdInline,
    mdStatic,
    mdPlatform,
    mdDeprecated,
    mdVarArgs // <-- VER210
    );

  TMethodDirectives=set of TMethodDirective;

  TMethodSpecification=class(TGenericSpec)
  public
    Directives : TMethodDirectives;
    DeprecatedMessage : TConstantDeclaration;

    OfRecord : TTypeDeclaration; // Method of record or class

    OfObject : Boolean; // "of object"
    IsClass : Boolean; // class method

    IsExternal,
    IsDelayed : Boolean;

    ExternalDLL,
    ExternalName : TExpression; // <-- Pending: Lazy-delay-load or find ExternalDLL !

    DispatchID   : TExpression;
    MessageField : TExpression; // message WM_SIZE

    Referenced : String;

    Body : TBody;

    function Find(const ASymbol:TFindSymbol):TExpression; override;

    procedure MatchParameters(const AInterface:TMethodSpecifications);
    function ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean; virtual;
  end;

  // Redirect
  TReferenceSpecification=class(TGenericSpec)
  protected
    function Expand(const AParams:TTypeParameters):TTypeSpecification; override;
  public
    Method : TMethodSpecification;
  end;

  TMethodSpecificationClass=class of TMethodSpecification;

  TMethodParams=class(TMethodSpecification)
  protected
    function AnyParamIsTyped:Boolean;
    function ExpandParameter(const AParameter:TVariableDeclaration; const AParams: TTypeParameters):TVariableDeclaration;
    procedure ExpandParameters(const AMethod:TMethodParams; const AParams:TTypeParameters);
    function Expand(const AParams:TTypeParameters):TTypeSpecification; override;
  public
    FixedParameters : Integer;
    VariableParameters : Boolean;
    Parameters : TParameters;

    procedure CalcFixedParameters;
    procedure CheckConstantParameters(const AParams:TExpressions);
    function HasParameters:Boolean;
    function Find(const ASymbol:TFindSymbol):TExpression; override;
    function ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean; override;
  end;

  TProcedureSpecification=class(TMethodParams)
  public
    function ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean; override;
  end;

  TConstructorSpecification=class(TMethodParams);

  TDestructorSpecification=class(TMethodSpecification);

  TFunctionSpecification=class(TMethodParams)
  protected
    function Expand(const AParams:TTypeParameters):TTypeSpecification; override;
  public
    ResultValue : TVariableDeclaration;

    function Find(const ASymbol:TFindSymbol):TExpression; override;
    class function FunctionOf(ABlock:TBlock):TFunctionSpecification;

    function ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean; override;
  end;

  TRecordOperatorSpec=class(TFunctionSpecification)
  public
    Kind : TRecordOperatorKind;
  end;

  // Foo(a,b,c)
  TCallExpression=class(TExpression)
  public
    Expression : TExpression;
    Parameters : TExpressions;

    Constructor CreateCall(const AOwner:TBlock; const AExpression:TExpression;
                           const AParameters:TExpressions);
  end;

  // Foo(...)
  TCall=class(TStatement)
  private
  public
    Expression : TExpression;
  end;

  // Foo^
  TDereferenceExpression=class(TExpression)
  private
    procedure SetValue(const AExp:TExpression);
  public
    Value : TExpression;

    Constructor CreateValue(const AOwner:TBlock; const AValue:TExpression);
  end;

  // Foo.Bar
  TFieldExpression=class(TExpression)
  public
    Value : TExpression;
    Field : TExpression;

    Constructor CreateField(const AOwner:TBlock; const AValue,AField:TExpression);
  end;

  TScopedType=class(TTypeDeclaration)
  private
    FField : TFieldExpression;

    procedure SetField(const Value: TFieldExpression);
  public
    Constructor CreateScope(const AOwner:TBlock; const AField:TFieldExpression);

    function Scope:TExpression;

    property Field:TFieldExpression read FField write SetField;
  end;

  //TImplicitSelf=class(TFieldExpression); <-- Hope it'll never be necessary

  // Internal. Typed call.
  TGenericMethod=class(TExpression)
  public
    Method : TMethodDeclaration;
    TypeParameters : TTypeParameters;

    Constructor CreateMethod(const AOwner:TBlock; const AMethod:TMethodDeclaration;
                             const AParams:TTypeParameters);
  end;

  // Foo<String>(...)
  TTypedMethod=class(TGenericMethod);

  // inherited ...
  TInheritedExpression=class(TExpression)
  public
    Method : TMethodDeclaration;
    Expression : TExpression;
  end;

  TCaseItem=class(TBlock)
  public
    Condition : TExpression;
    Body : TStatements;
  end;

  TCaseItems=Array of TCaseItem;

  // case foo of...
  TCase=class(TStatement)
  public
    Expression : TExpression;
    Cases : TCaseItems;
    ElsePart : TStatements;
  end;

  TLabelDeclaration=class(TIdentifier)
  public
    class function FindLabel(const AOwner:TBlock; const S:String):TLabelDeclaration;
  end;

  // label foo;
  TLabelDeclarations=class(TIdentifiers)
  public
    function Find(const AName:String):TLabelDeclaration;
  end;

  // foo:    <-- label
  TLabelPlace=class(TStatement)
  public
    LabelIdentifier : TLabelDeclaration;
  end;

  // goto label
  TGoto=class(TStatement)
  public
    TargetLabel : TLabelDeclaration;
  end;

  // Internal
  TImplementationModule=class(TModule)
  public
    ModuleImplementation : TImplementation;

    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    function FindSymbol(const ASymbol:TFindSymbol):TIdentifier; override;
  end;

  TModuleBody=class(TStatements);

  TInitialization=class(TModuleBody);
  TFinalization=class(TModuleBody);

  // *.pas
  TUnit=class(TImplementationModule)
  private
    function GetUsedUnit(const AName:String; const ASection:TBlock):TUnit;
  public
    OnlyInterface : Boolean;

    UnitInterface : TInterface;
    UnitInitialization : TInitialization;
    UnitFinalization : TFinalization;

    InterfaceLines : Integer;

    function FindSymbol(const ASymbol:TFindSymbol):TIdentifier; override;
    class function UnitOf(AOwner:TBlock):TUnit; overload;
    class function UnitOf(AOwner:TBlock; out ASection:TBlock):TUnit; overload;
  end;

  // internal
  TNativeModule=class(TImplementationModule)
  public
    Main : TModuleBody;
  end;

  // *.exe
  TProgram=class(TNativeModule);

  TPackages=class(TIdentifiers)
  private
    function Get(Index:Integer):TPackage;
  public
    Destructor Destroy; override;

    function Find(const AName:String):TPackage;

    property Item[Index:Integer]:TPackage read Get; default;
  end;

  TPackageContains=class(TUses)
  public
    procedure ParseModule(const AName,AFileName:String); override;
  end;

  // *.bpl
  TPackage=class(TModule)
  private
    function FindRecursive(const AName:String):TModule;
  public
    PackageRequires : TPackages;
    PackageContains : TPackageContains;

    Language : TLanguage;

    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    procedure AddRequired(const APackage:TPackage);
    procedure Clear; override;
  end;

  // *.dll
  TLibrary=class(TNativeModule);

  TExcludeModule=function(const AName:String):Boolean of object;
  TParseModule=procedure(const AModule:TModule) of object;
  TSilent=procedure(const E:ECodeException) of object;
  TVerbose=procedure(const ABlock:TBlock; const AMessage:String) of object;

  TUnitScope=class(TIdentifier);

  TUnitScopes=class(TIdentifiers)
  public
    function Find(const AScope:String):TUnitScope;
  end;

  TModules=class;

  TModuleClass=class of TModule;

  TLanguage=class
  private
    CurrentPackage : TPackage;

    FOnExcludeModule : TExcludeModule;
    FOnParsing : TParseModule;
  protected
    procedure AddParsedLines(const AModule:TModule; const ALines:Integer);
    procedure DoParse(const AModule:TModule; const AFileName:String); virtual; abstract;
    procedure InternalParseModule(const AParser:TBaseParser; const AModule:TModule); virtual; abstract;
    function ParserFromFile(const AOwner:TBlock; const AFile:String):TBaseParser; virtual; abstract;
  public
    CompilerVersion : Integer;

    ParsedBlocks,
    ParsedLines : Integer;

    Modules : TModules;

    {$IFOPT D+}
    BreakOn : TDebugBreak;
    {$ENDIF}

    CurrentModule : TModule;

    CaseSentitive : Boolean;

    ModuleClass : TModuleClass;

    SelfKeyword: String;

    Constructor Create; virtual;
    Destructor Destroy; override;

    function CheckCompatible(const Dest:TTypeSpecification; tmpFinal:TTypeSpecification;
                             out SameType:Boolean; const IsNil:Boolean=False):Boolean;

    procedure Clear; virtual;

    function CompatibleTypes(ADest:TTypeSpecification;
                         const SourceValue:TExpression;
                         out SameType:Boolean;
                         const RaiseException:Boolean=False):Boolean;

    class function FindBlock(const ASymbol:TFindSymbol; out DoBreak:Boolean):TExpression;
    function FindModule(const AName: String): TModule;
    function FindModuleOrScope(const AOrigin:TBlock; const AName:String):TExpression; virtual;
    function FullPathOf(var AName:String; const AExtension:String):String; virtual; abstract;

    function GuessIdentifier(const AKind:TSymbolKind;
                     const AOwner:TBlock;
                     AName:String;
                     const AsPointer:Boolean;
                     const AResult:TTypeSpecification=nil;
                     const AParams:TExpressions=nil;
                     const ATypeParams:TExpressions=nil;
                     const AParent:TExpression=nil):TExpression;

    function GuessInnerTypes(const AKind:TSymbolKind;
                     const AOrigin,AOwner:TBlock;
                     const AName:String;
                     const AsPointer:Boolean;
                     const AResult:TTypeSpecification;
                     const ATypeParams:TTypeParameters):TExpression;

    function GuessType(const AOwner:TBlock;
                   const AName:String;
                   const ATypeParams:TExpressions=nil):TTypeDeclaration;

    function GetPackage(const AOwner:TBlock; const AName:String):TPackage;

    function IsMethodPointer(const ASpec:TTypeSpecification):Boolean;

    procedure ParseModule(const AModule:TModule; const AFile:String);

    function ValueFinal(const AExp:TExpression; out AType:TTypeDeclaration;
                        ResolveFunction:Boolean=True):TExpression;

    property OnExcludeModule:TExcludeModule read FOnExcludeModule write FOnExcludeModule;
    property OnParsing:TParseModule read FOnParsing write FOnParsing;
  end;

  // All modules (that are not in any TPackage)
  TModules=class(TPackage)
  private
    function FindScoped(const AName:String):TModule;
  public
    Scopes : TUnitScopes;

    Constructor Create(const AOwner:TBlock); override;
    Destructor Destroy; override;

    procedure Clear; override;
    function NewModule(const AName:String; const AClass:TModuleClass):TModule;
    function ParseNewModule(const AName,AFile:String; const AClass:TModuleClass):TModule;
  end;

  TEmit=class
  protected
    CRLF,
    GreaterThan,
    IndentText,
    LessThan,
    Quote : String;

    function AnchorOf(const S:String):String; virtual;
    function Keyword(const S:String):String; virtual;
    function LinkTo(const ABlock:TBlock; const S:String):String; virtual;
    function StringToCode(const S:String):String; virtual;
    function UnitNameLink(const S:String):String; virtual;

    function Block(const ABlock:TBlock):String; virtual; abstract;
    function Expressions(const AItems:TExpressions):String; virtual; abstract;
  public
    Constructor Create; virtual;

    class function AsString(const ABlock:TBlock):String; overload;
    class function AsString(const AItems:TExpressions):String; overload;

    class function ModuleExtension:String; virtual; abstract;
  end;

  TEmitClass=class of TEmit;

  TIntrinsics=class
  public
    Globals : TSection;

    function GlobalMethod(const AClass:TMethodSpecificationClass):TMethodDeclaration;
    function GlobalFunction(const AName:String; const AResult:TVariableDeclaration):TMethodDeclaration;
    function GlobalProcedure(const AName:String):TMethodDeclaration;
    function AddParam(const AMethod:TMethodDeclaration; const AType:TTypeDeclaration;
                      const AName:String):TParameter;
    function ResultOf(const AType:TTypeSpecification):TVariableDeclaration; overload;
    function ResultOf(const AType:TTypeDeclaration):TVariableDeclaration; overload;
    function GlobalType(const ASpec:TTypeSpecification):TTypeDeclaration; overload;
    function GlobalType(const AName:String):TTypeDeclaration; overload;
    function AddGlobalType(const AName:String; const AExpression:TTypeSpecification=nil):TTypeDeclaration;
    function AddGlobalPointerType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
    function AddGlobalAliasType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
  end;

var
  Silent : TSilent;
  Verbose : TVerbose;

  AnyType,
  BooleanType,
  Byte_Type,
  CharType,
  DoubleType,
  ExtendedType,
  IntegerType,
  IntegerOrSetType,
  Int64Type,
  OrdinalType,
  PointerType,
  ResourceStringType,
  SetType,
  SingleType,
  StringType,
  VariantType,
  TypeKindType,
  WordType : TTypeDeclaration;

  ArrayOrSetOrRange,
  ArrayOrString,
  ArrayOrStringOrPChar,
  ArraySpec,
  AnySpec,
  BooleanSpec,
  ByteSpec,
  CharSpec,
  FloatSpec,
  IntegerOrPointer,
  NumberSpec,
  NumberOrSetSpec,
  OrdinalSpec,
  PointerOrMethod,
  PointerSpec,
  RealSpec,
  SetSpec,
  VariantSpec : TTypeSpecification;

  StringSpec : TTypeSpecification;

  GUIDSpec,
  VarRecSpec : TRecordSpecification;

  IInterfaceType,
  ObjectType : TTypeDeclaration;

  NilConst : TConstantDeclaration;

  MinusOne,
  Zero : TNumber;

const
  IdentifierCharsNoDot=['0'..'9','a'..'z','A'..'Z','_'];
  IdentifierChars=IdentifierCharsNoDot+['.'];

{$IFOPT D+}
var
  CreatedBlocks:Integer=0;
{$ENDIF}

implementation

uses
  Classes; // <-- To remove (depends TStringStream only)

{
const
  DebugStrongCheck=False;
}

(*
{$IFOPT D+}
{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}
{$ENDIF}

procedure Debug(const S:String);
begin
  {$IFOPT D+}
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(S));
  {$ENDIF}
  {$ENDIF}
end;
*)

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
     {$DEFINE HAS_STRINGSTREAM}
  {$ENDIF}
{$ENDIF}

// Returns text content from AFile
class function TBaseParser.TextOf(const AFile:String):String;
var m : TStringStream;
    {$IFNDEF HAS_STRINGSTREAM}
    f : TFileStream;
    {$ENDIF}
begin
  {$IFDEF HAS_STRINGSTREAM}
  m:=TStringStream.Create('');
  try
    m.LoadFromFile(AFile);
    result:=m.DataString;
  finally
    m.Free;
  end;
  {$ELSE}
  f:=TFileStream.Create(AFile,fmOpenRead+fmShareDenyNone);
  try
    m:=TStringStream.Create('');
    try
      m.CopyFrom(f,f.Size);
      result:=m.DataString;
    finally
      m.Free;
    end;
  finally
    f.Free;
  end;
  {$ENDIF}
end;

class function TBaseParser.TextInCase(const S:String; const Texts:Array of String):Integer;
var t : Integer;
begin
  for t:=Low(Texts) to High(Texts) do
      if Texts[t]=S then
      begin
        result:=t;
        Exit;
      end;

  result:=-1;
end;

class function TBaseParser.TextIn(const S:String; const Texts:Array of String):Integer;
var t : Integer;
begin
  for t:=Low(Texts) to High(Texts) do
      if SameText(Texts[t],S) then
      begin
        result:=t;
        Exit;
      end;

  result:=-1;
end;

{ TBaseParser }

{$IFOPT D+}
procedure DebugBreak; external 'kernel32.dll' name 'DebugBreak';
{$ENDIF}

function TBaseParser.Advance: Char;
begin
  Inc(State.Position.Current);

  result:=FText[State.Position.Current];

  if result=#13 then
  begin
    Inc(State.Position.Line);
    State.Position.Column:=1;

    {$IFOPT D+}
    if Language.BreakOn.Line=State.Position.Line then
    if (DebugHook<>0) and Language.BreakOn.IsUnitImpl and
       (not Language.BreakOn.Stopped) then
    begin
      Language.BreakOn.Stopped:=True;
      DebugBreak;
    end;
    {$ENDIF}
  end
  else
  if result<>#10 then
     Inc(State.Position.Column);
end;

Constructor TBaseParser.Create(const AOwner:TBlock; const ALanguage:TLanguage);
begin
  inherited Create;

  FLanguage:=ALanguage;

  ParserOwner:=AOwner;
  ParserOwner.Parent:=Self;
end;

{$IFDEF D21}
{.$DEFINE ARRAYCAT}
{$ENDIF}

procedure TBaseParser.NewDirective(Sender:TObject; const Kind:TDirectiveKind; const ADirective:String);
{$IFNDEF ARRAYCAT}
var L : Integer;
{$ENDIF}
var tmp : TDirective;
begin
  tmp:=TDirective.CreateKind(nil,Kind,ADirective);

  {$IFDEF ARRAYCAT}
  Directives:=Directives+[tmp];
  {$ELSE}
  L:=Length(Directives);
  SetLength(Directives,L+1);
  Directives[L]:=tmp;
  {$ENDIF}
end;

procedure TBaseParser.GetDefine(Sender:TObject; const AIdentifier:String; out AValue:String);
var tmp : TExpression;
    Dummy : Boolean;
    tmpFind : TFindSymbol;
begin
  tmpFind.Kind:=skField;
  tmpFind.Origin:=ParserOwner;
  tmpFind.Owner:=ParserOwner;
  tmpFind.Name:=AIdentifier;
  tmpFind.Parent:=ParserOwner;

  tmp:=TLanguage.FindBlock(tmpFind,Dummy);

  if tmp is TConstantDeclaration then
     {$IFDEF HAS_TOSTRING}
     AValue:=TConstantDeclaration(tmp).Value.ToString
     {$ELSE}
     begin
       if TConstantDeclaration(tmp).Value is TConstantDeclaration then
          AValue:=TConstantDeclaration(TConstantDeclaration(tmp).Value).ToString
       else
          AValue:=AIdentifier;
     end
     {$ENDIF}
  else
  if tmp is TTypeDeclaration then
     AValue:=TTypeDeclaration(tmp).Name
  else
     AValue:='';
end;

procedure TBaseParser.Error(const ABlock:TObject; const AMessage:String);
var E: ECodeException;
begin
  E:=ECodeException.Create(AMessage);
  E.Parser:=Self;
  E.Block:=ABlock;
  E.Position:=State.Position;
  E.CurrentFile:=CurrentFile;

  if Assigned(Silent) then
     Silent(E)
  else
     raise E;
end;

procedure TBaseParser.Error(const AMessage:String);
begin
  Error(nil,AMessage);
end;

function TBaseParser.NextIdentifier:String;
begin
  NextToken(IdentifierChars,result);
end;

function TBaseParser.NextHexadecimal:String;
const
  HexadecimalChars=['0'..'9','a'..'f','A'..'F'];

var C : Char;
begin
  result:='';

  Increment;

  repeat
    C:=Next;

    if {$IFDEF HAS_CHARINSET}CharInSet(C,HexadecimalChars){$ELSE}C in HexadecimalChars{$ENDIF} then
    begin
      result:=result+C;
      Increment;
    end
    else
       break;

  until Eof;
end;

class function TChar.IsDigit(const C:Char):Boolean;
const
  Digits=['0'..'9'];
begin
  result:={$IFDEF HAS_CHARINSET}CharInSet(C,Digits){$ELSE}C in Digits{$ENDIF}
end;

function TBaseParser.NextCharIs(const C:Char):Boolean;
begin
  result:=(State.Position.Current<L-1) and (Text[State.Position.Current+1]=C);
end;

function TBaseParser.PrevCharIs(const C:Char):Boolean;
begin
  result:=(State.Position.Current>0) and (Text[State.Position.Current-1]=C);
end;

function TBaseParser.Next:Char;
begin
  result:=State.C;
end;

function TBaseParser.NextLine:String;
var C : Char;
begin
  result:='';

  while not Eof do
  begin
    C:=Next;
    Increment;

    if (C=#13) or (C=#10) then
       Exit
    else
       result:=result+C;
  end;
end;

function TBaseParser.NextString:String;

  function NextString:String;
  var C,
      C2 : Char;
  begin
    result:='';

    Increment;

    repeat
      C:=Next;

      Increment;

      if C='''' then
      begin
        C2:=Next;

        if C2='''' then
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

  function NextChar:Char;
  begin
    Increment;
    result:=Next;
    Increment;
  end;

  function NextCharInt:Char;
  var C : Char;
      S : String;
      tmp : Integer;
  begin
    Increment;

    if Next='$' then
       S:='$'+NextHexadecimal
    else
    begin
      S:='';

      repeat
        C:=Next;

        if TChar.IsDigit(C) then
        begin
          S:=S+C;
          Increment;
        end
        else
           break;

      until False;
    end;

    if TryStrToInt(S,tmp) then
       result:=Chr(tmp)
    else
       raise Exception.Create('Not an integer for # char: '+S);
  end;

var C : Char;
begin
  result:='';

  repeat
    C:=Next;

    if C='#' then
       result:=result+NextCharInt
    else
    if C='^' then
       result:=result+NextChar
    else
       result:=result+NextString;

    C:=Next;

    if (C<>'#') and (C<>'''') and (C<>'^') then
       break;

  until False;
end;

procedure TBaseParser.NextToken(out S:String);
begin
  NextToken([],S);
end;

function TBaseParser.GetNextNext:Char;
begin
  Next;
  result:=Text[State.Position.Current+1];
end;

function TBaseParser.NextTokenUpTo(const Terminator:Char{; Included:Boolean}):String;
var C : Char;
begin
  result:='';

  while not Eof do
  begin
    C:=Next;

    if C=Terminator then
       break
    else
    begin
      if (C=#13) or (C=#10) or ( (C=' ') and (result='')) then
      else
         result:=result+C;

      Increment;
    end;
  end;
end;

procedure TBaseParser.NextToken(const ValidChars:TCharset; out S:String);
const
  StopChars=[#9,#10,#13,' '];

var C : Char;
    AllValid : Boolean;
begin
  S:='';

  AllValid:=ValidChars=[];

  while not Eof do
  begin
    C:=Next;

    if {$IFDEF HAS_CHARINSET}CharInSet(C,StopChars){$ELSE}C in StopChars{$ENDIF} then
    begin
      if S<>'' then
         break
      else
         Increment;
    end
    else
    if AllValid or {$IFDEF HAS_CHARINSET}CharInSet(C,ValidChars){$ELSE}(C in ValidChars){$ENDIF} then
    begin
      S:=S+C;
      Increment;
    end
    else
      break;
  end;
end;

function TBaseParser.PeekIdentifier:String;
begin
  result:=PeekToken(IdentifierChars);
end;

function TBaseParser.Eof:Boolean;
begin
  result:=State.Position.Current>L;
end;

function TBaseParser.IsChar(const C:Char):Boolean;
begin
  result:=PeekChar=C;

  if result then
     Increment;
end;

function TBaseParser.IsNextChar(const C:Char):Boolean;
begin
  result:=GetNextNext=C;

  if result then
     Increment;
end;

// Peeks a token, and if equals S, does NextToken
function TBaseParser.IsToken(const S:String):Boolean;
var tmp : TParserState;
begin
  result:=SameText(PeekToken(tmp),S);

  if result then
     State:=tmp; //NextToken;
end;

function TBaseParser.PeekTwo:String;
begin
  PeekToken(result);
  result:=Copy(result,1,2);
end;

procedure TBaseParser.PeekToken(out S:String);
var Old : TParserState;
begin
  Old:=State;
  try
    NextToken(S);
  finally
    State:=Old;
  end;
end;

function TBaseParser.PeekToken(out NewState:TParserState):String;
var Old : TParserState;
begin
  Old:=State;
  try
    NextToken(result);
    NewState:=State;
  finally
    State:=Old;
  end;
end;

function TBaseParser.PeekToken(const ValidChars:TCharset):String;
var Old : TParserState;
begin
  Old:=State;
  try
    NextToken(ValidChars,result);
  finally
    State:=Old;
  end;
end;

procedure TBaseParser.RequireToken(const AName:String);
var tmp : String;
begin
  NextToken(tmp);

  if not SameText(tmp,AName) then
     Error('Expected: '+AName);
end;

procedure TBaseParser.Init;
begin
  State.DefinesOk:=True;
  State.DefinesStackCount:=0;

  State.InString:=False;
  State.InStringQuote:=False;
  State.InBlockComment:=False;
  State.InLineComment:=False;
  State.InComment:=False;
  State.StartDefine:=0;

  State.Position.Line:=1;
  State.Position.Current:=0;

  IsUTF:=Copy(Text,1,3)=#$EF#$BB#$BF;

  if IsUTF then
     Delete(FText,1,3);

  L:=Length(FText);
  Increment;
end;

procedure TBaseParser.SetText(const AText: String);
begin
  FText:=AText;
  Init;
end;

procedure TBaseParser.SkipToken;
const
  StopChars=[#9,#10,#13,' '];

var C : Char;
    Empty : Boolean;
begin
  Empty:=True;

  while not Eof do
  begin
    C:=Next;

    if {$IFDEF HAS_CHARINSET}CharInSet(C,StopChars){$ELSE}C in StopChars{$ENDIF} then
    begin
      if not Empty then
         break
      else
         Increment;
    end
    else
    begin
      Empty:=False;
      Increment;
    end;
  end;
end;

function TBaseParser.PeekChar:Char;
const
  Skipped=[#9,#10,#13,' '];

begin
  result:=#0;

  while not Eof do
  begin
    result:=Next;

    if {$IFDEF HAS_CHARINSET}CharInSet(result,Skipped){$ELSE}result in Skipped{$ENDIF} then
       Increment
    else
       break;
  end;
end;

procedure TBaseParser.OptionalSemicolon;
begin
  IsChar(';');
end;

procedure TBaseParser.RequireSemicolon;
begin
  if not IsChar(';') then
     Error('Semicolon expected');
end;

{ TModule }

constructor TModule.Create(const AOwner: TBlock);
begin
  inherited;

  Module:=Self;

  SetLength(Used,1);
  Used[0]:=Self;
end;

Destructor TModule.Destroy;
begin
  Clear;
  inherited;
end;

procedure TModule.Clear;
var t : Integer;
begin
  for t:=0 to BlockCount-1 do
      Blocks[t].Free;

  BlockCount:=0;
  Blocks:=nil;
end;

procedure TModule.NewBlock(const Sender: TBlock);
var L : Integer;
begin
  L:=Length(Blocks);

  if BlockCount>=L then
     SetLength(Blocks,L+1024);

  Blocks[BlockCount]:=Sender;
  Sender.Index:=BlockCount;
  Sender.Module:=Self;

  Inc(BlockCount);
end;

procedure TModule.AddUsage(const AIdent:TIdentifier);

  procedure TryAddUsed(const AModule:TModule);

    function ModuleIsUsed:Boolean;
    var t : Integer;
    begin
      for t:=0 to High(Used) do
          if Used[t]=AModule then
          begin
            result:=True;
            Exit;
          end;

      result:=False;
    end;

  var N : Integer;
  begin
    if not ModuleIsUsed then
    begin
      N:=Length(Used);
      SetLength(Used,N+1);
      Used[N]:=AModule;
    end;
  end;

begin
  Inc(AIdent.Usage);
  TryAddUsed(TModule(AIdent.Module));
end;

class function TModule.ModuleOf(const AOwner:TBlock):TModule;
begin
  result:=TModule(AOwner.Module);
  {
  repeat
    if AOwner is TModule then
    begin
      result:=TModule(AOwner);
      Exit;
    end
    else
      AOwner:=AOwner.Owner;

  until AOwner=nil;

  result:=nil;
  }
end;

{ TUnits }

Constructor TModules.Create(const AOwner:TBlock);
begin
  inherited;
  Scopes:=TUnitScopes.Create(Self);
end;

Destructor TModules.Destroy;
begin
  Scopes.Free;
  inherited;
end;

function TModules.FindScoped(const AName:String):TModule;
var t : Integer;
begin
  for t:=0 to Scopes.Count-1 do
  begin
    result:=PackageContains.Find(Scopes.Get(t).Name+'.'+AName);

    if result<>nil then
       Exit;
  end;

  result:=nil;
end;

function TModules.NewModule(const AName: String; const AClass:TModuleClass): TModule;
begin
  result:=AClass.Create(nil);
  result.Name:=AName;
  result.Package:=Self;

  PackageContains.Add(result);
end;

procedure TModules.Clear;
begin
  inherited;
  Scopes.Clear;
end;

function TModules.ParseNewModule(const AName,AFile:String; const AClass:TModuleClass):TModule;
begin
  result:=PackageContains.Find(AName);

  if result=nil then
  begin
    result:=AClass.CreateName(nil,AName);
    result.FileName:=AFile;

    Language.CurrentPackage.NewBlock(result);

    // Remove this:
    if result is TUnit then
       TUnit(result).OnlyInterface:=True;

    PackageContains.Add(result);

    if Language.CurrentPackage<>Self then
       Language.CurrentPackage.PackageContains.Add(result);

    Language.ParseModule(result,AFile);
  end;
end;

class function TMethodDeclaration.OwnerMethod(ABlock:TBlock):TMethodDeclaration;
var tmpSpec : TMethodSpecification;
begin
  tmpSpec:=nil;

  repeat
    if ABlock is TMethodSpecification then
    begin
      tmpSpec:=TMethodSpecification(ABlock);
      break;
    end
    else
    if ABlock is TReferenceSpecification then
    begin
      tmpSpec:=TReferenceSpecification(ABlock).Method;
      break;
    end
    else
      ABlock:=ABlock.Owner;

  until ABlock=nil;

  if tmpSpec=nil then
     result:=nil
  else
     result:=TMethodDeclarations(tmpSpec.Owner).FindSpec(tmpSpec)
end;

// Returns the Record type that is owner of ABlock
class function TRecordSpecification.OwnerRecordMethod(ABlock:TBlock):TTypeDeclaration;
var tmpMethod : TMethodDeclaration;
begin
  repeat
    tmpMethod:=TMethodDeclaration.OwnerMethod(ABlock);

    if (tmpMethod<>nil) and
       (tmpMethod.TypeIdentifier.Expression is TMethodSpecification) then
    begin
      result:=TMethodSpecification(tmpMethod.TypeIdentifier.Expression).OfRecord;

      if result<>nil then
         Exit;
    end;

    ABlock:=ABlock.Owner;

  until ABlock=nil;

  result:=nil;
end;

function IsMeOrAncestor(const AOwner:TBlock; const ARecord:TRecordSpecification):Boolean;
var tmp : TRecordSpecification;
begin
  tmp:=TRecordSpecification.RecordOf(AOwner);

  result:=(tmp<>nil) and tmp.IsClassMeOrAncestor(ARecord);
end;

class function TUnit.UnitOf(AOwner:TBlock):TUnit;
var tmp : TModule;
begin
  tmp:=ModuleOf(AOwner);

  if tmp=nil then
     result:=nil
  else
     result:=TUnit(tmp);

  {
  repeat
    if AOwner is TUnit then
    begin
      result:=TUnit(AOwner);
      Exit;
    end
    else
      AOwner:=AOwner.Owner;

  until AOwner=nil;

  result:=nil;
  }
end;

class function TUnit.UnitOf(AOwner:TBlock; out ASection:TBlock):TUnit;
begin
  repeat
    if AOwner.Owner is TUnit then
    begin
      ASection:=AOwner;
      result:=TUnit(AOwner.Owner);
      Exit;
    end
    else
      AOwner:=AOwner.Owner;

  until AOwner=nil;

  result:=nil;
end;

function TUnit.FindSymbol(const ASymbol:TFindSymbol):TIdentifier;
begin
  if UnitInterface=nil then
     result:=nil
  else
     result:=UnitInterface.Find(ASymbol);
end;

function GetUnit(const AOwner:TBlock; const AName:String):TUnit;
var tmp : TUnit;
    tmpSection : TBlock;
begin
  tmp:=TUnit.UnitOf(AOwner,tmpSection);

  if tmp=nil then
     result:=nil // ?? Internal Error !
  else
  if SameText(tmp.Name,AName) then
     result:=tmp
  else
     result:=tmp.GetUsedUnit(AName,tmpSection);
end;

{ TUnitScopes }

function TUnitScopes.Find(const AScope: String): TUnitScope;
var tmp : TIdentifier;
begin
  tmp:=SortedFind(AScope);

  if tmp=nil then
     result:=nil
  else
     result:=TUnitScope(tmp);
end;

function AnyPChar(const AType:TTypeSpecification):Boolean;
begin
  result:=(AType is TTypePointerOf) and (TTypePointerOf(AType).TypeIdentifier.Expression=CharSpec);
end;

function SpecIsArray(ASpec:TTypeSpecification):Boolean;

  function RecordHasIndexed(const ARecord:TRecordSpecification):Boolean;
  var tmp : TPropertyDeclaration;
  begin
    tmp:=ARecord.DefaultProperty(True);

    result:=(tmp<>nil) and tmp.IsArray;
  end;

begin
  ASpec:=ASpec.Redirect;

  result:=(ASpec is TArraySpecification) or
          (ASpec=VariantSpec) or
          (ASpec=StringSpec) or
          (ASpec=CharSpec) or
          (ASpec=ByteSpec) or // <-- VER220 POINTERMATH ON Byte
          (ASpec=PointerSpec) or // <-- VER220 POINTERMATH ON Byte
          (
            (ASpec is TRecordSpecification)
            {
            and
            (
              CompilerOption.PointerMath
              or
              RecordHasIndexed(TRecordSpecification(ASpec))
            )
            }
          )
          or
          (ASpec is TTypeItem);
end;

class function TRecordSpecification.AncestorOf(const AOwner:TBlock):TExpression;
var tmpType : TTypeDeclaration;
begin
  tmpType:=TRecordSpecification.OwnerRecordMethod(AOwner);

  if tmpType=nil then
     result:=nil
  else
     result:=TRecordSpecification(tmpType.Expression).Ancestors[0];
end;

function FindATypeParam(const AName:String; const AParams:TTypeParameters):TTypeDeclaration;
var t : Integer;
begin
  for t:=Low(AParams) to High(AParams) do
      if SameText(TTypeDeclaration(AParams[t]).Name,AName) then
      begin
        result:=TTypeDeclaration(AParams[t]);
        Exit;
      end;

  result:=nil;
end;

function FindATypeParamIndex(const AName:String; const AParams:TTypeParameters):Integer;
var t : Integer;
begin
  for t:=Low(AParams) to High(AParams) do
      if SameText(TTypeDeclaration(AParams[t]).Name,AName) then
      begin
        result:=t;
        Exit;
      end;

  result:=-1; // <-- Raise Error !
end;

function TrySubstituteIndex(const AExp:TExpression; const AParams:TTypeParameters):TExpression;
begin
  if AExp is TTypeParameter then
     result:=AParams[TTypeParameter(AExp).Index]
  else
     result:=AExp;
end;

class function TTypeDeclaration.TryExpand(const AType:TTypeDeclaration; var AResult:TTypeDeclaration;
                   const AParams:TTypeParameters):Boolean;

  function AnyIsTypeParam(const P:TTypeParameters):Boolean;
  var t : Integer;
  begin
    for t:=Low(P) to High(P) do
        if P[t] is TTypeParameter then
        begin
          result:=True;
          Exit;
        end;

    result:=False;
  end;

var tmpArray,
    tmpSpec : TTypeSpecification;
    tmpHasParams : Boolean;
begin
  result:=AType is TTypeParameter;

  if result then
     AResult:=TrySubstituteIndex(AType,AParams) as TTypeDeclaration
  else
  if AType.Expression is TGenericSpec then
  begin
    tmpHasParams:=(AType.TypeParameters=nil) or AnyIsTypeParam(AType.TypeParameters);

    if tmpHasParams then
       tmpSpec:=TGenericSpec(AType.Expression).Expand(AParams)
    else
       tmpSpec:=AType.Expression;

    result:=tmpSpec<>AType.Expression;

    if result then
       AResult:=TTypeDeclaration.CreateSpec(AType,tmpSpec)
    else
    begin
      if AType.Expression is TClassSpecification then
      begin
        AResult:=TTypeDeclaration.CreateSpec(AType,tmpSpec);
        AResult.TypeParameters:=AParams;
        AResult.Alias:=AType;
        result:=True;
      end
      else
      if tmpHasParams and (AType.Expression is TArraySpecification) then
      begin
        tmpArray:=TArraySpecification(tmpSpec).Expand(AParams);

        if tmpArray<>tmpSpec then
        begin
          AResult:=TTypeDeclaration.CreateSpec(AType,tmpArray);
          AResult.TypeParameters:=AType.TypeParameters;
          result:=True;
        end;
      end;
    end;
  end
  else
     AResult:=AType;
end;

function FindTypesIn(AType:TTypeDeclaration):TTypeParameters;
begin
  repeat
    result:=AType.TypeParameters;

    if result=nil then
       AType:=AType.Alias
    else
       break;

  until AType=nil;
end;

// Pending: Change AType from param to result
function TLanguage.ValueFinal(const AExp:TExpression; out AType:TTypeDeclaration;
                    ResolveFunction:Boolean=True):TExpression;

  function IsFunction(ASpec:TTypeSpecification; out AFunc:TFunctionSpecification):Boolean;
  begin
    if ASpec is TFunctionSpecification then
    begin
      AFunc:=TFunctionSpecification(ASpec);
      result:=True;
    end
    else
    if (ASpec is TReferenceSpecification) and
       (TReferenceSpecification(ASpec).Method is TFunctionSpecification) then
    begin
      AFunc:=TFunctionSpecification(TReferenceSpecification(ASpec).Method);
      result:=True;
    end
    else
      result:=False;
  end;

  function ExpressionIsFunction(const AExp:TExpression;
                                const AType:TTypeDeclaration;
                                out AFunc:TFunctionSpecification):Boolean;
  var tmpMethod : TMethodDeclaration;
      tmpSpec : TTypeSpecification;
  begin
    if AExp is TGenericMethod then
    begin
      tmpMethod:=TGenericMethod(AExp).Method;

      tmpSpec:=tmpMethod.TypeIdentifier.Generic;

      result:=IsFunction(tmpSpec,AFunc);

      if result then
         AFunc:=TFunctionSpecification(AFunc.Expand(TGenericMethod(AExp).TypeParameters));
    end
    else
      result:=(AType<>nil) and IsFunction(AType.Generic,AFunc);
  end;

  function TryGetParamsFromRecord(AType:TTypeDeclaration):TTypeParameters;
  var tmpClass : TRecordSpecification;
  begin
    result:=nil;

    if AType.Expression is TRecordSpecification then
    begin
      tmpClass:=TRecordSpecification(AType.Expression);

      repeat
        if tmpClass.Ancestors<>nil then
        begin
          AType:=tmpClass.Ancestors[0];
          result:=AType.TypeParameters;

          if result=nil then
          begin
            if AType.Expression is TRecordSpecification then
               tmpClass:=TRecordSpecification(AType.Expression);
          end
          else
             break;
        end;

      until tmpClass.Ancestors=nil;
    end;
  end;

    function ReplaceParams(const A,B:TTypeParameters):TTypeParameters;

       function ReplaceParam(const P:TExpression):TExpression;
       begin
         if P is TTypeParameter then
            result:=A[TTypeParameter(P).Index]
         else
         if P is TGenericTypeDeclaration then
         begin
           result:=TGenericTypeDeclaration.Create(P);
           TGenericTypeDeclaration(result).Expression:=TGenericTypeDeclaration(P).Expression;
           TGenericTypeDeclaration(result).Name:=TGenericTypeDeclaration(P).Name;
           TGenericTypeDeclaration(result).TypeParameters:=ReplaceParams(A,TGenericTypeDeclaration(P).TypeParameters);
         end
         else
           result:=P;
       end;

    var t, L : Integer;
    begin
       // A: String,Integer
       // B: TPair<TKey,TValue> --> TPair<String,Integer>

       if A=B then
          result:=A
       else
       begin
         SetLength(result,Length(B));
         L:=0;

         for t:=Low(B) to High(B) do
         begin
           result[L]:=ReplaceParam(B[t]);
           Inc(L);
         end;
       end;
    end;

  function Loop(const AExp:TExpression):TExpression;

    function ArrayElement(const AExp:TItemExpression; var AType:TTypeDeclaration):TExpression;
    var tmpSpec : TTypeSpecification;
        tmpDim,
        tmpHigh,
        t : Integer;
        tmpParams : TTypeParameters;
        tmpDefault : TPropertyDeclaration;
        tmpType : TTypeDeclaration;
        tmpFunc : TFunctionSpecification;
    begin
      result:=Loop(AExp.Value);

      {
      while AType.Alias<>nil do
            AType:=AType.Alias;
      }

      tmpType:=AType;

      // NO (<t>) : tmpSpec:=RedirectSpecification(AType.Expression);

      tmpSpec:=AType.Generic;

      if AExp.Value is TItemExpression then
         tmpDim:=1
      else
         tmpDim:=0;

      tmpHigh:=High(AExp.Items);

      for t:=Low(AExp.Items) to tmpHigh do
      begin
        tmpSpec:=tmpSpec.Redirect;

        if tmpSpec<>VariantSpec then
        begin
          if tmpSpec is TArraySpecification then
          begin
            Inc(tmpDim);

            if Length(TArraySpecification(tmpSpec).Dimensions)>tmpDim then
               tmpSpec:=tmpSpec // ?
            else
            begin
              if AExp.Parent.Language.CompilerVersion>=210 then
              begin

                while AType.Expression is TTypePointerOf do
                begin
                  AType:=TTypePointerOf(AType.Expression).TypeIdentifier;

                  //if AType=nil then
                  //   AExp.Error('Internal: TypeOf type missing');
                end;

                tmpParams:=FindTypesIn(AType);

                if tmpParams<>nil then
                   tmpSpec:=TArraySpecification(tmpSpec).Expand(tmpParams);
              end;

              AType:=TArraySpecification(tmpSpec).Expression.TypeIdentifier;

              tmpSpec:=AType.Generic;
            end;
          end
          else
          begin
            tmpDim:=0;

            if tmpSpec is TRecordSpecification then
            begin
              //?? DefaultProperty should do a TryExpand with the Ancestor's
              // TypeParameters that owns the default property.
              tmpDefault:=TRecordSpecification(tmpSpec).DefaultProperty(True);

              if tmpDefault=nil then
              begin
                if (not AExp.Parser.PointerMath) or
                   (not SpecIsArray(tmpSpec)) then
                   AExp.Error('Record has no default property: '+tmpSpec.ClassName);
              end
              else
              begin
                if tmpDefault.IsIndexed then
                begin
                  result:=tmpDefault;

                  if (AExp.Parent.Language.CompilerVersion>=210) and
                     (tmpDefault.TypeIdentifier is TTypeParameter) and
                     (tmpDefault.TypeIdentifier.Expression is TGenericSpec) then
                  begin
                    tmpParams:=FindTypesIn(AType);

                    // Hack, to solve the above DefaultProperty expand
                    if tmpParams=nil then
                       if AType.Expression is TClassSpecification then
                          if TClassSpecification(AType.Expression).Ancestors<>nil then
                             tmpParams:=TClassSpecification(AType.Expression).Ancestors[0].TypeParameters;

                    if tmpParams=nil then
                       AType:=tmpDefault.TypeIdentifier
                    else
                       TTypeDeclaration.TryExpand(tmpDefault.TypeIdentifier,AType,tmpParams);
                  end
                  else
                    AType:=tmpDefault.TypeIdentifier;

                  if AType.Expression is TArraySpecification then
                  begin
                    AType:=TArraySpecification(AType.Expression).Expression.TypeIdentifier;
                  end;
                end
                else
                   AExp.Error('Cannot access [] item value from: '+tmpSpec.ClassName);
              end;
            end
            else
            // Extreme hack here !
            // StringSpec should finally be made as TArraySpecification of ChartSpec.
            // See Intrinsics StringSpec declaration.
            if tmpSpec=StringSpec then
               AType:=CharType
            else
            if (tmpSpec<>SetSpec) and (not SpecIsArray(tmpSpec)) then
               AExp.Error('Cannot access [] item value from: '+tmpSpec.ClassName);
          end;

          if AType is TTypeParameter then
          begin
            tmpParams:=FindTypesIn(tmpType);

            if tmpParams=nil then
            begin
              tmpParams:=TryGetParamsFromRecord(tmpType);

              {
              if tmpParams=nil then
                 if AExp.Value is TPropertyDeclaration then
                    tmpParams:=TryGetParamsFromRecord(TPropertyDeclaration(AExp.Value).RecordType);
              }
            end;

            if tmpParams<>nil then
               TTypeDeclaration.TryExpand(AType,AType,tmpParams);
          end;
        end;
      end;

      if ResolveFunction and
         ExpressionIsFunction(AExp.Value,AType,tmpFunc) then
      begin
        result:=tmpFunc.ResultValue;
        AType:=result.TypeIdentifier;
      end;
    end;

    function FieldElement(const AExp:TFieldExpression; var AType:TTypeDeclaration):TExpression;
    var tmpType : TTypeDeclaration;
        tmpFunc : TFunctionSpecification;
        tmpParams : TTypeParameters;
    begin
      Loop(AExp.Value);
      tmpType:=AType;

      result:=Loop(AExp.Field);

      if result.TypeIdentifier.Expression is TConstructorSpecification then
         AType:=tmpType;

      if AExp.Parent.Language.CompilerVersion>=210 then
      begin
        tmpParams:=FindTypesIn(tmpType);

        if tmpParams=nil then
           tmpParams:=TryGetParamsFromRecord(tmpType);

        if tmpParams<>nil then
        begin
          if AType.TypeParameters<>nil then
             tmpParams:=ReplaceParams(tmpParams,AType.TypeParameters);

          TTypeDeclaration.TryExpand(AType,AType,tmpParams);
        end;
      end;

      if ResolveFunction and
         ExpressionIsFunction(AExp.Field,AType,tmpFunc) then
      begin
        result:=tmpFunc.ResultValue;
        AType:=result.TypeIdentifier;
      end;
    end;

  var tmpType : TTypeDeclaration;
      tmpFunc : TFunctionSpecification;
  begin
    if AExp is TItemExpression then
       result:=ArrayElement(TItemExpression(AExp),AType)
    else
    if AExp is TCallExpression then
       result:=Loop(TCallExpression(AExp).Expression)
    else
    if AExp is TInheritedExpression then
       if TInheritedExpression(AExp).Expression=nil then
          result:=Loop(TInheritedExpression(AExp).Method)
       else
          result:=Loop(TInheritedExpression(AExp).Expression)
    else
    if AExp is TNestedExpression then
       result:=Loop(TNestedExpression(AExp).Expression)
    else
    if AExp is TFieldExpression then
       result:=FieldElement(TFieldExpression(AExp),AType)
    else
    if (AExp.TypeIdentifier<>nil) and
       (AExp.TypeIdentifier.Expression is TConstructorSpecification) then
    begin
      result:=AExp;

      AType:=TMethodSpecification(AExp.TypeIdentifier.Expression).OfRecord;

      if (AType=nil) or (not (AType.Expression is TRecordSpecification)) then
         AExp.Error('Internal: Constructor owner is not a record');
    end
    else
    // Move this outside this Loop ?
    if ResolveFunction and
       ExpressionIsFunction(AExp,AExp.TypeIdentifier,tmpFunc) then
    begin
      result:=tmpFunc.ResultValue;
      AType:=result.TypeIdentifier;
    end
    else
    if AExp is TDereferenceExpression then
    begin
      result:=Loop(TDereferenceExpression(AExp).Value);

      if AType.Expression is TTypePointerOf then
         AType:=TTypePointerOf(AType.Expression).TypeIdentifier;
    end
    else
    if AExp is TCasting then
    begin
      result:=Loop(TCasting(AExp).ExpressionValue(tmpType));
      AType:=tmpType;
    end
    else
    begin
      result:=AExp;
      AType:=result.TypeIdentifier;
    end;
  end;

begin
  result:=Loop(AExp);

  if result=nil then
     AExp.Error('Internal: Cannot evaluate expression');
end;

function TLanguage.IsMethodPointer(const ASpec:TTypeSpecification):Boolean;
begin
  result:=(ASpec is TMethodSpecification) or (ASpec is TReferenceSpecification);
end;

function IsPointer(const AExp:TTypeSpecification):Boolean;
begin
  result:=(AExp=PointerSpec) or
          (AExp=VariantSpec) or
          (AExp=NumberSpec) or
          (AExp is TConstructorSpecification) or
          (AExp is TClassSpecification) or
          (AExp is TArraySpecification) or
          (AExp=IntegerOrPointer);
end;

  // Pending: Return nil or false if ASpec can't be returned modified:
  function FinalTypeOf(const ASpec:TTypeSpecification):TTypeSpecification;
  begin
    result:=ASpec;

    while result is TTypeTypeOf do
          result:={NonAlias}(TTypeTypeOf(result).TypeIdentifier.Expression);

    if result is TReferenceSpecification then
       if TReferenceSpecification(result).Method is TFunctionSpecification then
          result:=TFunctionSpecification(TReferenceSpecification(result).Method).ResultValue.TypeIdentifier.Expression;
  end;

function TLanguage.CheckCompatible(const Dest:TTypeSpecification; tmpFinal:TTypeSpecification;
                          out SameType:Boolean; const IsNil:Boolean=False):Boolean;

  function IsSameClassOf(const ARecord:TRecordSpecification):Boolean;
  var tmp : TClassSpecification;
      tmpRec : TTypeSpecification;
  begin
    result:=TTypeDeclaration(ARecord.Owner).Expression={TClassSpecification}(Dest){.Oldexpression};

    if not result then
    begin
      tmpRec:=ARecord;

      if tmpRec is TClassSpecification then
      begin
        tmp:=TClassSpecification(tmpRec);

        if Length(tmp.Ancestors)>0 then
        begin
          tmpRec:=tmp.Ancestors[0].Expression;

          if tmpRec is TRecordSpecification then
             result:=IsSameClassOf(TRecordSpecification(tmpRec));
        end;
      end;
    end;
  end;

  function GetRecord(tmpType:TTypeSpecification):TRecordSpecification;
  begin
    if tmpType is TConstructorSpecification then
       tmpType:=TConstructorSpecification(tmpType).OfRecord.Expression;

    if tmpType is TRecordSpecification then
       result:=TRecordSpecification(tmpType)
    else
       result:=nil;
  end;

  function IsSameRecord(const AType:TTypeSpecification; const ADest:TRecordSpecification):Boolean;
  var tmp : TRecordSpecification;
  begin
    result:=(AType=ADest) or (AType=VariantSpec);

    if not result then
    begin
      tmp:=GetRecord(AType);
      result:=(tmp<>nil) and (ADest=tmp);
    end;
  end;

  function IsSameClass(const AType:TTypeSpecification; const ADest:TClassSpecification):Boolean;
  var tmp : TRecordSpecification;
      tmpType : TTypeSpecification;
  begin
    tmpType:=AType;

    if tmpType is TClassOf then
       tmpType:=TClassOf(tmpType).TypeIdentifier.Expression;

    result:=(tmpType=ADest) or (tmpType=VariantSpec);

    if not result then
    begin
      tmp:=GetRecord(tmpType);

      if tmp<>nil then
         result:=(Dest=tmp) or IsMeOrAncestor(tmp,ADest);
    end;
  end;

  function IsResourceString(const Source:TTypeSpecification):Boolean;
  begin
    result:=Source=ResourceStringType.Expression
  end;

  function IsPointerOrMethod(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=PointerSpec) or
            (ASpec is TTypePointerOf) or
            (ASpec is TClassSpecification) or
            (ASpec is TInterfaceSpecification) or
            IsMethodPointer(ASpec) or
            (ASpec is TArraySpecification) or
            (ASpec is TClassOf);
  end;

  function CompatibleArray(const Dest:TArraySpecification):Boolean;
  begin
     result:=(
               (tmpFinal is TArraySpecification)
               and
               TArraySpecification(tmpFinal).CompatibleArrayTypes(Dest)
             )
             or
             (
               (tmpFinal=SetSpec) //and (SourceValue is TItemExpression) { PENDING: and Compatible TItemExpression(SourceValue).Items[0] !! }
             )
             or
             (tmpFinal=ArrayOrStringOrPChar) // Pending: Compatible type from "Copy" intrinsic etc
             or
             (
               (tmpFinal is TArraySpecification)
               and
               TArraySpecification(Dest).SameArrayTypes(TArraySpecification(tmpFinal))
             )
  end;

  function IsStringTypeItem(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec is TTypeItem)  // String[123]
            and
            (TTypeItem(ASpec).Item.TypeIdentifier.Expression=StringSpec);
  end;

  function IsArrayOfChar(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(
              (ASpec is TArraySpecification)
              and
              (TArraySpecification(ASpec).Expression.Expression=CharSpec)
            )
            or
              IsStringTypeItem(ASpec);
  end;

  function FinalIsTypeOf(const AFinal,ASpec:TTypeSpecification):Boolean;
  begin
    result:=(AFinal is TTypeTypeOf)
            and
            (TTypeTypeOf(AFinal).TypeIdentifier.Expression=ASpec)
  end;

  function IsSameTypeItem:Boolean;
  var tmp : TTypeSpecification;
  begin
    tmp:=TTypeItem(Dest).Item.TypeIdentifier.Expression;

    if tmpFinal is TTypeItem then
    begin
      // Pending: Check tmpFinal.Item.Items = compatible with Dest.Item.Items
      tmpFinal:=TTypeItem(tmpFinal).Item.TypeIdentifier.Expression;
    end;

    result:=(tmp=tmpFinal) or
             (
               (tmp=StringSpec) and
               (
                 (tmpFinal=CharSpec)
                 or
                 (tmpFinal=ArrayOrStringOrPChar)
               )
             )
             or
             (
               (tmpFinal is TArraySpecification)
               and
               CheckCompatible(tmp,TArraySpecification(tmpFinal).Expression.Expression,SameType,IsNil)
             )
             or
             (
               (tmpFinal is TTypeTypeOf) and
               CheckCompatible(tmp,TTypeTypeOf(tmpFinal).TypeIdentifier.Expression,SameType,IsNil)
             )
             or
             (
               (tmpFinal is TTypePointerOf) and
               CheckCompatible(tmp,TTypePointerOf(tmpFinal).TypeIdentifier.Expression,SameType,IsNil)
             )
  end;

  function IsArrayOrStringOrPChar(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=CharSpec) or
            (ASpec is TArraySpecification) or
            (ASpec=StringSpec) or
            AnyPChar(ASpec {tmpExp.TypeOf.Expression}) or
            IsArrayOfChar(ASpec) or
            FinalIsTypeOf(ASpec,StringSpec);
  end;

  function IsArrayOrString(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec is TArraySpecification) or
            (ASpec=StringSpec) or
            IsStringTypeItem(ASpec) or
            (FinalTypeOf(ASpec)=StringSpec);
  end;

  function IsString(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=ArrayOrString) or
            (ASpec=ArrayOrStringOrPChar) or
            (ASpec=CharSpec) or
            AnyPChar(ASpec) or
            IsArrayOfChar(ASpec) or
            FinalIsTypeOf(ASpec,StringSpec) or
            IsResourceString(ASpec);
  end;

  function IsArrayOrSetOrRange(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec is TArraySpecification) or
             (ASpec is TTypeRange) or
             (ASpec is TSetSpecification) or
             (ASpec=NumberSpec) or
             (ASpec=CharSpec) or

             // Pending: High and Low intrinsics
             (ASpec=StringSpec);
  end;

  function IsClassOf:Boolean;
  var tmpClass : TClassSpecification;
  begin
    result:=(tmpFinal=PointerSpec) {or (SourceValue=NilConst)};

    if not result then
    begin
      tmpClass:=TClassSpecification(TClassOf(Dest).TypeIdentifier.Expression);
      result:=IsSameClass(tmpFinal,tmpClass);
    end;
  end;

  function IsOrdinal(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=NumberSpec) or
            (ASpec=CharSpec) or
            (ASpec=BooleanSpec) or
            (ASpec=NumberOrSetSpec) or
            (ASpec is TSetSpecification) or
            (ASpec is TTypeRange) or
            (ASpec is TTypePointerOf) or
            (ASpec=StringSpec) // <-- String = Array of Char ??
  end;

  function IsSet(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=Dest) or (ASpec.Owner=Dest);

    if not result then
       result:=(ASpec=SetSpec) or
               (ASpec=OrdinalSpec) or
               (ASpec=NumberOrSetSpec); // <-- Pending: Succ Pred etc should return same type as passed param !

    SameType:=result;

    if not result then
       if ASpec is TTypeRange then
          result:=TTypeRange(ASpec).Range.TypeIdentifier.Expression=Dest;
  end;

  // Pending: 4 possibilities (set,set) (set,type of set) etc
  function SameSetOf(const A,B:TTypeSpecification):Boolean;
  begin
    result:=A=B;

    if not result then
    begin
      result:=A=FinalTypeOf(B);

      if not result then
         result:=FinalTypeOf(A)=B;
    end;
  end;

  function IsSetOf(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=Dest) or (ASpec=SetSpec) or
            (
              (ASpec is TSetOfSpecification) and
              SameSetOf(TSetOfSpecification(Dest).TypeIdentifier.Expression,TSetOfSpecification(ASpec).TypeIdentifier.Expression)
            )
  end;

  function IsNumber(const ASpec:TTypeSpecification):Boolean;
  begin
    result:=(ASpec=NumberSpec) or (ASpec=RealSpec) or
            (ASpec=OrdinalSpec) or (ASpec=NumberOrSetSpec) or
            (ASpec=IntegerOrPointer);
  end;

  function IsFloat(const ASpec:TTypeSpecification; out SameType:Boolean):Boolean;
  begin
    // better??? result:=IsNumber(ASpec);

    result:=(ASpec=FloatSpec) or (ASpec=RealSpec);

    SameType:=result;

    if not result then
       result:=(ASpec=NumberSpec) or
               (ASpec=OrdinalSpec) or
               (ASpec=NumberOrSetSpec);
  end;

  function IsTypeRange(const ASpec:TTypeSpecification):Boolean;
  var tmpType : TTypeDeclaration;
  begin
    //Pending: "and SameSet"
    result:=(ASpec=Dest) or
            (ASpec=NumberSpec) or
            (ASpec=NumberOrSetSpec) or
            (ASpec is TSetSpecification);

    if (not result) and (ASpec is TTypeRange) then
    begin
      ValueFinal(TTypeRange(ASpec).Range.StartRange,tmpType);

      result:=CheckCompatible(TTypeRange(Dest).Range.TypeIdentifier.Expression,
               tmpType.Expression,
               SameType,
               IsNil);
    end;
  end;

var tmpType : TTypeDeclaration;
begin
  // Pending: Put this here?
  if tmpFinal is TRecordHelperSpecification then
     tmpFinal:=TRecordHelperSpecification(tmpFinal).TypeIdentifier.Expression;

  SameType:=tmpFinal=Dest;

  if SameType then
     result:=True
  else
  if Dest=VariantSpec then
  begin
    SameType:=tmpFinal=VariantSpec;
    result:=True;
  end
  else
  if tmpFinal=VariantSpec then
     result:=True
  else
  if Dest=FloatSpec then
  begin
    result:=IsFloat(tmpFinal,SameType);

    if not result then
    begin
      tmpFinal:=FinalTypeOf(tmpFinal);

      result:=IsFloat(tmpFinal,SameType);

      if not result then
         result:=(tmpFinal is TTypeRange) and TTypeRange(tmpFinal).IsNumber;
    end;
  end
  else
  if Dest=ArrayOrStringOrPChar then
  begin
    result:=IsArrayOrStringOrPChar(tmpFinal);

    SameType:=result;
  end
  else
  if Dest=ArrayOrString then // BEFORE "is TArraySpecification" below
  begin
    result:=IsArrayOrString(tmpFinal);

    SameType:=result;
  end
  else
  if Dest=ArraySpec then
     result:=(tmpFinal=ArraySpec) or (tmpFinal is TArraySpecification)
  else
  if Dest is TArraySpecification then
  begin
    result:={(SourceValue=NilConst) or} (tmpFinal=PointerSpec) or (tmpFinal=ArraySpec);

    if not result then
    begin
      (*
      if SourceValue is TArrayValues then
         result:=CompatibleArrayValues(TArraySpecification(Dest),TArrayValues(SourceValue))
      else
      *)
      begin
        tmpFinal:=FinalTypeOf(tmpFinal);

        tmpType:=TArraySpecification(Dest).Expression;

        if tmpType<>nil then
        begin
          result:=(tmpType.Expression=tmpFinal) // ?
                    or
                    CompatibleArray(TArraySpecification(Dest))
                    or
                    (
                      (tmpType.Expression=CharSpec)
                      and
                       (
                         (tmpFinal=StringSpec) or
                         (
                           (tmpFinal is TTypePointerOf) and
                           (TTypePointerOf(tmpFinal).TypeIdentifier.Expression=CharSpec)
                         )
                       )
                    );
        end;
      end;
    end;
  end
  else
  if Dest=BooleanSpec then
  begin
    result:=(tmpFinal=BooleanSpec) or (FinalTypeOf(tmpFinal)=BooleanSpec);

    SameType:=result;
  end
  else
  if Dest=NumberSpec then
  begin
    result:=IsNumber(tmpFinal);

    if result then
       SameType:=True
    else
    begin
      result:=( { PointerArithmetic and } (tmpFinal=CharSpec) )
               or
               (tmpFinal is TTypePointerOf)
               or
               (IsNumber(FinalTypeOf(tmpFinal)))
               or
               (
                 (tmpFinal is TTypeRange)
                 and
                 TTypeRange(tmpFinal).IsNumber
               )
               or
               (
                 {
                 SourceValue.Parent.PreParser.PointerMath
                 and
                 }
                 (tmpFinal is TArraySpecification)
               )

    end;
  end
  else
  if Dest=NumberOrSetSpec then
  begin
    result:=(tmpFinal=NumberSpec) or (tmpFinal=FloatSpec) or
            (tmpFinal=OrdinalSpec) or (tmpFinal is TSetSpecification);

    if result then
       SameType:=True
    else
       result:=( { PointerArithmetic and } (tmpFinal=CharSpec) )
               or
               (tmpFinal is TTypePointerOf)
               or
               (FinalTypeOf(tmpFinal)=NumberSpec)
               or
               (FinalTypeOf(tmpFinal) is TSetSpecification)
               or
               (
                 (tmpFinal is TTypeRange)
                 and
                 TTypeRange(tmpFinal).IsNumber
               )
  end
  else
  if Dest=FloatSpec then
  begin
    result:=tmpFinal=FloatSpec;

    if result then
       SameType:=True
    else
    begin
      result:=(tmpFinal=NumberSpec);

      if not result then
      begin
        tmpFinal:=FinalTypeOf(tmpFinal);

        result:=(tmpFinal=FloatSpec);

        SameType:=result;

        if not result then
           result:=(tmpFinal=NumberSpec) or
                   (
                     (tmpFinal is TTypeRange)
                     and
                     TTypeRange(tmpFinal).IsNumber
                   );
      end;
    end;
  end
  else
  if Dest=PointerOrMethod then
  begin
    result:={(SourceValue=NilConst) or} IsPointerOrMethod(tmpFinal);

    SameType:=result;
  end
  else
  if Dest=IntegerOrPointer then
  begin
    result:={(SourceValue=NilConst) or}
            (tmpFinal=NumberSpec) or
            IsPointerOrMethod(tmpFinal);

    SameType:=result;
  end
  else
  if Dest is TTypePointerOf then
     result:=(tmpFinal=PointerSpec) or (tmpFinal=AnySpec) or IsPointer(tmpFinal) or
             (
               (tmpFinal is TArraySpecification)
               and
               (TTypePointerOf(Dest).TypeIdentifier.Expression=
                tarrayspecification(tmpfinal).expression.expression)
             )
  else
  if Dest=PointerSpec then
  begin
    result:=(tmpFinal=AnySpec) or
            IsPointer(tmpFinal) or
            (tmpFinal is TTypePointerOf) or
            (tmpFinal is TClassOf);

    SameType:=result;
  end
  else
  if Dest=ArrayOrSetOrRange then
  begin
    result:=IsArrayOrSetOrRange(tmpFinal);

    SameType:=result;
  end
  else
  if Dest=OrdinalSpec then
     result:=IsOrdinal(tmpFinal) or IsOrdinal(FinalTypeOf(tmpFinal))
  else
  if Dest=CharSpec then
  begin
    result:=(tmpFinal=CharSpec) or (tmpFinal=StringSpec) or (tmpFinal=OrdinalSpec) or
         (
           (tmpFinal is TTypePointerOf)
           and
           (TTypePointerOf(tmpFinal).TypeIdentifier.Expression=CharSpec)
         );

    if not result then
    begin
      tmpFinal:=FinalTypeOf(tmpFinal);

      result:=(tmpFinal=CharSpec) or (tmpFinal=StringSpec) or (tmpFinal=OrdinalSpec);
    end;
  end
  else
  if Dest=StringSpec then
  begin
    result:=IsString(tmpFinal);

    if not result then
    begin
      // Pending to investigate this case:
      if tmpFinal is TFunctionSpecification then
         result:=IsString(TFunctionSpecification(tmpFinal).ResultValue.TypeIdentifier.Expression);
    end;

    if result then
       SameType:=True;
  end
  else
  if Dest=SetSpec then
     result:=tmpFinal.IsSet
  else
  if Dest is TTypeItem then
     result:=IsSameTypeItem
  else
  if Dest is TSetSpecification then
     result:=IsSet(tmpFinal) or IsSet(FinalTypeOf(tmpFinal))
  else
  if (Dest=PointerOrMethod) or IsMethodPointer(Dest) then
     result:=(tmpFinal=PointerSpec) or IsMethodPointer(tmpFinal) // <-- Pending: Compatible method signature !
  else
  if Dest is TClassSpecification then
  begin
    result:=(tmpFinal=PointerSpec) or {(SourceValue=NilConst) or}
            (tmpFinal=IntegerOrPointer) or
            IsSameClass(tmpFinal,TClassSpecification(Dest));

    if not result then
       result:=FinalTypeOf(tmpFinal)=PointerSpec;
  end
  else
  if Dest is TClassOf then
     result:=IsClassOf
  else
  if Dest is TSetOfSpecification then
     result:=IsSetOf(tmpFinal) or IsSetOf(FinalTypeOf(tmpFinal))
  else
  if Dest is TInterfaceSpecification then
  begin
    // Pending to investigate/remove this "if" case:
    if tmpFinal is TFunctionSpecification then
       tmpFinal:=TFunctionSpecification(tmpFinal).ResultValue.TypeIdentifier.Expression;

    result:=IsNil or TInterfaceSpecification(Dest).IsCompatible(tmpFinal);

  end
  else
  if (GUIDSpec<>nil) and (Dest=GUIDSpec) then
  begin
    result:=IsNil;

    if not result then
       result:=(tmpFinal=GUIDSpec) or (tmpFinal is TInterfaceSpecification) or
               (tmpFinal=StringSpec);

    SameType:=result;
  end
  else
  if Dest is TTypeRange then
     result:=IsTypeRange(tmpFinal) or IsTypeRange(FinalTypeOf(tmpFinal))
  else
  if Dest is TRecordHelperSpecification then
     result:=Dest=tmpFinal
  else
  if Dest is TRecordSpecification then
  begin
    result:=(tmpFinal=PointerSpec) or
             IsSameRecord(tmpFinal,TRecordSpecification(Dest));
  end
  else
     result:=False;
end;

  function TryFindImplicit(const ADest,ASpec:TTypeSpecification):Boolean;
  begin
    // ASpec -> Dest  implicit here ??? (maybe not necessary)
    result:=(ASpec is TRecordSpecification) and
            TRecordSpecification(ASpec).FindOperator(True,ADest,okImplicit);

    if (not result) and (ADest is TRecordSpecification) then
       result:=TRecordSpecification(ADest).FindOperator(False,ASpec,okImplicit);
  end;

function TLanguage.CompatibleTypes(ADest:TTypeSpecification;
                         const SourceValue:TExpression;
                         out SameType:Boolean;
                         const RaiseException:Boolean=False):Boolean;
var IsDestMethod : Boolean;
    tmpType : TTypeDeclaration;
begin
  if ADest is TRecordHelperSpecification then
     ADest:=TRecordHelperSpecification(ADest).TypeIdentifier.Expression;

  IsDestMethod:=(ADest=PointerOrMethod) or
                IsMethodPointer(ADest); // <-- Assigned

  ValueFinal(SourceValue,tmpType,not IsDestMethod);

  if tmpType=nil then
     ADest.Error('Internal: Nil type');

  result:=CheckCompatible(ADest,tmpType.Generic,SameType,SourceValue=NilConst);

  if not result then
  begin
    if ADest is TTypeTypeOf then
       result:=CompatibleTypes(FinalTypeOf(ADest),SourceValue,SameType,SourceValue=NilConst)
    else
    if ADest is TArraySpecification then
    begin
      if SourceValue is TArrayValues then
         result:=TArraySpecification(ADest).CompatibleArrayValues(TArrayValues(SourceValue));
    end
    else
    if ADest is TTypePointerOf then
       result:=CompatibleTypes(TTypePointerOf(ADest).TypeIdentifier.Expression,SourceValue,SameType)
  end;

  if (not result) and (tmpType.Parent.Language.CompilerVersion>=210) then // Record Operators
  begin
    result:=TryFindImplicit(ADest,tmpType.Expression);

    if not result then
       if tmpType.Expression is TTypeTypeOf then
          result:=TryFindImplicit(ADest,TTypeTypeOf(tmpType.Expression).TypeIdentifier.Expression);
  end;

  {
  if (not result) and RaiseException then
     SourceValue.Error('Not compatible types: '+ADest.ClassName);
  }
end;

function SameParameters(const A,B:TParameters; DoError:Boolean; out AError:String):Boolean;

  procedure SetError(const AText:String; const P:TParameter; const Index:Integer);
  begin
    AError:=AText+P.Qualified+' ('+IntToStr(Index)+')';
  end;

var t,
    L : Integer;
    AP, BP : TParameter;
    Dummy : Boolean;
begin
  L:=A.Count;

  result:=L=B.Count;

  if result then
  begin
    for t:=0 to L-1 do
    begin
      AP:=TParameter(A[t]);
      BP:=TParameter(B[t]);

      if not SameText(AP.Name,BP.Name) then
      begin
        if DoError then
           SetError('Different Parameter name: ',AP,t);

        result:=False;
        Exit;
      end
      else
      if AP.OptionalValue<>BP.OptionalValue then
      begin
        if DoError then
           SetError('Not equal optional parameter: ',AP,t);

        result:=False;
        Exit;
      end
      else
      if AP.TypeIdentifier<>BP.TypeIdentifier then
      begin
        if (AP.TypeIdentifier<>nil) and (BP.TypeIdentifier<>nil) and
           AP.Parser.Language.CompatibleTypes(AP.TypeIdentifier.Generic,BP,Dummy) then
        else
        begin
          if DoError then
             SetError('Different parameter types: ',AP,t);

          result:=False;
          Exit;
        end;
      end
      else
      if AP.IsConst<>BP.IsConst then
      begin
        if DoError then
           SetError('Not equal const parameter: ',AP,t);

        result:=False;
        Exit;
      end
      else
      if AP.IsVar<>BP.IsVar then
      begin
        if DoError then
           SetError('Not equal var parameter: ',AP,t);

        result:=False;
        Exit;
      end
      else
      if AP.IsOut<>BP.IsOut then
      begin
        if DoError then
           SetError('Not equal out parameter: ',AP,t);

        result:=False;
        Exit;
      end;
    end;
  end
  else
  if DoError then
     AError:='Different number of parameters';
end;

procedure TBlock.AddAttribute(const Attr:TAttribute);
{$IFNDEF ARRAYCAT}
var L : Integer;
{$ENDIF}
begin
  {$IFDEF ARRAYCAT}
  Attributes:=Attributes+[Attr];
  {$ELSE}
  L:=Length(Attributes);
  SetLength(Attributes,L+1);
  Attributes[L]:=Attr;
  {$ENDIF}
end;

class function TConstantDeclaration.NewStringFrom(const AOwner:TBlock; const S:String):TConstantDeclaration;
begin
  if Length(S)=1 then
  begin
    result:=TChar.Create(AOwner);
    TChar(result).Value:=S[1]
  end
  else
  begin
    // Pending: Merge identical strings into a pool? (per unit/module?)
    result:=TString.Create(AOwner);
    TString(result).Value:=S;
  end;
end;

procedure TSection.AddOrdered(const AList:TIdentifiers; const Initial:Integer);
var t,L : Integer;
begin
  L:=Length(Ordered);
  SetLength(Ordered,L+AList.Count-Initial);

  for t:=Initial to AList.Count-1 do
      Ordered[L+t-Initial]:=AList[t];
end;

procedure TSection.AddOrderedSingle(const ABlock:TBlock);
var L : Integer;
begin
  {$IFDEF ARRAYCAT}
  Ordered:=Ordered+[ABlock];
  {$ELSE}
  L:=Length(Ordered);
  SetLength(Ordered,L+1);
  Ordered[L]:=ABlock;
  {$ENDIF}
end;

function TSection.Find(const ASymbol:TFindSymbol):TIdentifier;
begin
  if ASymbol.Kind<>skField then
  begin
    if Types=nil then
       result:=nil
    else
       result:=Types.Find(ASymbol);
  end
  else
  begin
    if Constants=nil then
       result:=nil
    else
       result:=Constants.Find(ASymbol.Name);

    if (result=nil) and (Variables<>nil) then
       result:=Variables.Find(ASymbol.Name);

    if (result=nil) and (Methods<>nil) then
       result:=Methods.Find(ASymbol);

    if (result=nil) and (Types<>nil) then
       result:=Types.Find(ASymbol);

    if (result=nil) and (ResourceStrings<>nil) then
       result:=ResourceStrings.Find(ASymbol.Name);
  end;
end;

procedure TSection.Init;
begin
  if Methods=nil then
     Methods:=TMethodDeclarations.Create(Self);

  if Constants=nil then
     Constants:=TConstantDeclarations.Create(Self);

  if Variables=nil then
     Variables:=TVariableDeclarations.Create(Self);

  if Types=nil then
     Types:=TTypeDeclarations.Create(Self);
end;

class function TLanguage.FindBlock(const ASymbol:TFindSymbol; out DoBreak:Boolean):TExpression;

  function GuessInUses(const AUses:TUsesSection):TExpression;
  begin
    if AUses.UsesUnits=nil then
       result:=nil
    else
       result:=AUses.UsesUnits.Find(ASymbol);
  end;

  function GuessInSection(const ASection:TSection):TExpression;
  begin
    result:=ASection.Find(ASymbol);

    if (result=nil) and (ASection is TUsesSection) then
       result:=GuessInUses(TUsesSection(ASection));
  end;

var tmpInterface : TInterface;
begin
  DoBreak:=False;

  if ASymbol.Owner is TImplementation then
  begin
    result:=TImplementation(ASymbol.Owner).Find(ASymbol);

    if result=nil then
    begin
      if ASymbol.Owner.Owner is TUnit then
      begin
        tmpInterface:=TUnit(ASymbol.Owner.Owner).UnitInterface;
        result:=tmpInterface.Find(ASymbol);
      end;

      if result=nil then
      begin
        result:=GuessInUses(TUsesSection(ASymbol.Owner));

        if (result=nil) and (ASymbol.Owner.Owner is TUnit) then
        begin
          tmpInterface:=TUnit(ASymbol.Owner.Owner).UnitInterface;
          result:=GuessInUses(tmpInterface);
        end;
      end;
    end;

    DoBreak:=True;
  end
  else
  if ASymbol.Owner is TSection then
     result:=GuessInSection(TSection(ASymbol.Owner))
  else
  if ASymbol.Owner is TModuleBody then
  begin
    result:=GuessInSection(TImplementationModule(ASymbol.Owner.Owner).ModuleImplementation);

    if (result=nil) and (ASymbol.Owner.Owner is TUnit) then
       result:=GuessInSection(TUnit(ASymbol.Owner.Owner).UnitInterface);

    DoBreak:=True;
  end
  else
  if ASymbol.Owner is TImplementationModule then
  begin
    {
    if TUnit(ASymbol.Owner).ModuleImplementation.Empty then
       result:=nil
    else
    }
       result:=GuessInSection(TImplementationModule(ASymbol.Owner).ModuleImplementation);

    if result=nil then
       if ASymbol.Owner is TUnit then
          if TUnit(ASymbol.Owner).UnitInterface<>nil then
             result:=GuessInSection(TUnit(ASymbol.Owner).UnitInterface);

    DoBreak:=True;
  end
  else
  if ASymbol.Owner is TTypeSpecification then
     result:=TTypeSpecification(ASymbol.Owner).Find(ASymbol)
  else
  if (ASymbol.Parent=nil) and (ASymbol.Owner is TConstantDeclaration) and
     SameText(TConstantDeclaration(ASymbol.Owner).Name,ASymbol.Name) then
     result:=TConstantDeclaration(ASymbol.Owner)
  else
  if ASymbol.Owner is TExpression then
     result:=TExpression(ASymbol.Owner).Find(ASymbol)
  else
  if (ASymbol.Owner is TOn) and
     SameText(TOn(ASymbol.Owner).Exception.Name,ASymbol.Name) then
     result:=TOn(ASymbol.Owner).Exception
  else
  if ASymbol.Owner is TWith then
     result:=TWith(ASymbol.Owner).Find(ASymbol)
  else
     result:=nil;
end;

function TLanguage.GuessIdentifier(const AKind:TSymbolKind;
                     const AOwner:TBlock;
                     AName:String;
                     const AsPointer:Boolean;
                     const AResult:TTypeSpecification=nil;
                     const AParams:TExpressions=nil;
                     const ATypeParams:TExpressions=nil;
                     const AParent:TExpression=nil):TExpression;

var DoBreak : Boolean;
    tmpFind : TFindSymbol;
begin
  tmpFind.Owner:=AOwner;
  tmpFind.Origin:=AOwner;
  tmpFind.Kind:=AKind;
  tmpFind.Parent:=AParent;

  tmpFind.Name:=AName;
  tmpFind.AsPointer:=AsPointer;
  tmpFind.AResult:=AResult;
  tmpFind.Params:=AParams;
  tmpFind.TypeParams:=ATypeParams;

  if AParent=nil then
  begin
    repeat
      result:=FindBlock(tmpFind,DoBreak);

      if (result<>nil) or DoBreak then
         break
      else
         tmpFind.Owner:=tmpFind.Owner.Owner;

    until tmpFind.Owner=nil;
  end
  else
    result:=FindBlock(tmpFind,DoBreak);

  if result is TIdentifier then
     CurrentModule.AddUsage(TIdentifier(result));
end;

function TLanguage.GuessType(const AOwner:TBlock;
                   const AName:String;
                   const ATypeParams:TExpressions=nil):TTypeDeclaration;
var tmp : TExpression;
begin
  tmp:=GuessIdentifier(skTypeOnly,AOwner,AName,True,nil,nil,ATypeParams);

  if tmp is TTypeDeclaration then
     result:=TTypeDeclaration(tmp)
  else
     result:=nil;
end;

function TLanguage.FindModuleOrScope(const AOrigin:TBlock; const AName:String):TExpression;
begin
  result:=GetUnit(AOrigin,AName);

  if result=nil then
     result:={AOrigin.Parent.Language.}Modules.FindScoped(AName); // Is Unit ?
end;

{ TImplementationModule }

constructor TImplementationModule.Create(const AOwner: TBlock);
begin
  inherited;
  ModuleImplementation:=TImplementation.Create(Self);
end;

Destructor TImplementationModule.Destroy;
begin
  Parent.Free;
  inherited;
end;

function TImplementationModule.FindSymbol(const ASymbol:TFindSymbol):TIdentifier;
begin
  result:=ModuleImplementation.Find(ASymbol);
end;

function TUnit.GetUsedUnit(const AName:String; const ASection: TBlock): TUnit;
begin
  if ASection=UnitInterface then
     result:=TUnit(UnitInterface.UsesUnits.Find(AName))
  else
  {
  if ModuleImplementation.Empty then
     result:=nil
  else
  }
  begin
    result:=TUnit(ModuleImplementation.UsesUnits.Find(AName));

    if result=nil then
       result:=TUnit(UnitInterface.UsesUnits.Find(AName));
  end;
end;

{ TPackage }

Constructor TPackage.Create(const AOwner:TBlock);
begin
  inherited;
  PackageRequires:=TPackages.Create(Self);
  PackageContains:=TPackageContains.Create(Self);
end;

Destructor TPackage.Destroy;
begin
  PackageRequires.Free;
  PackageContains.Free;
  inherited;
end;

function TPackage.FindRecursive(const AName: String): TModule;
var t : Integer;
begin
  result:=PackageContains.Find(AName);

  if result=nil then
     for t:=0 to PackageRequires.Count-1 do
     begin
       result:=PackageRequires[t].FindRecursive(AName);

       if result<>nil then
          break;
     end;
end;

procedure TPackage.AddRequired(const APackage: TPackage);
begin
  if PackageRequires=nil then
     PackageRequires:=TPackages.Create(Self);

  PackageRequires.AddSorted(APackage);
end;

procedure TPackage.Clear;
begin
  inherited;
  PackageRequires.Clear;
  PackageContains.Clear;
end;

function GetOwnerOfClass(ABlock:TBlock; const AClass:TBlockClass):TBlock;
begin
  repeat
    if ABlock is AClass then
    begin
      result:=ABlock;
      Exit;
    end
    else
       ABlock:=ABlock.Owner;

  until ABlock=nil;

  result:=nil;
end;

function TExpression.AsBoolean: Boolean;
begin
  Error('Expression cannot be evaluated');
  result:=False;
end;

function TExpression.Find(ASymbol:TFindSymbol):TExpression;
var tmpType : TTypeDeclaration;
    tmpSpec : TTypeSpecification;
    DummyBreak : Boolean;
begin
  Parser.Language.ValueFinal(Self,tmpType);

  if tmpType=nil then
  begin
    Error('Internal: Missing Expression Type');
    result:=nil;
  end
  else
  if (tmpType is TImplementationModule) and (ASymbol.Owner<>tmpType) then
  begin
    ASymbol.Owner:=tmpType;
    result:=TLanguage.FindBlock(ASymbol,DummyBreak);
  end
  else
  begin
    while tmpType.Alias<>nil do
          tmpType:=tmpType.Alias;

    while tmpType.Expression is TTypeRedirect do
          tmpType:=TTypeRedirect(tmpType.Expression).TypeIdentifier;

    tmpSpec:=tmpType.Expression;

    if tmpSpec is TClassOf then
       tmpType:=TClassOf(tmpSpec).TypeIdentifier;

    tmpSpec:=tmpType.Expression;

    if tmpSpec=nil then
       result:=nil  // <-- Error ??? !!!
    else
    if tmpSpec=VariantSpec then
    begin
      result:=TVariableDeclaration.CreateType(Self,tmpType);
      TVariableDeclaration(result).Name:=ASymbol.Name;
    end
    else
    if tmpSpec is TRecordSpecification then
    begin
      ASymbol.Owner:=Self;
      ASymbol.Parent:=tmpType; // <-- to obtain TypeParameters

      result:=TRecordSpecification(tmpSpec).Find(ASymbol);
    end
    else
    if tmpSpec is TSetSpecification then
       result:=TSetSpecification(tmpSpec).Items.Find(ASymbol)
    else
    if tmpSpec.Helper<>nil then
       result:=tmpSpec.Helper.Find(ASymbol)
    else
       result:=nil;
  end;
end;

function TExpression.GuessType(const AOwner:TBlock):TTypeDeclaration;
begin
  if Self is TTypeDeclaration then
     result:=TTypeDeclaration(Self)
  else
  if (Self is TCasting) and TCasting(Self).TypeIdentifier.HasCodePage then
  begin
    result:=TCasting(Self).TypeIdentifier;
    result.CodePage:=TCasting(Self).Expression;
  end
  else
  if (Self is TRangeExpression) and Parent.IsFPC then // FPC
  begin
    // type TFoo=type 0..99
    result:=TTypeDeclaration.CreateSpec(Self,
       TSetRange.CreateRange(AOwner,TRangeExpression(Self)));
  end
  else
  begin
    AOwner.Error('Invalid type: '+ClassName);
    result:=nil;
  end;
end;

function TExpression.IsArray:Boolean;
var tmpType : TTypeDeclaration;
begin
  Parser.Language.ValueFinal(Self,tmpType);
  result:=SpecIsArray(tmpType.Expression);
end;

// Nested types inside records or classes.
// Warning: Inefficient and slow.
// Solution? : Global per-unit sorted list
function TLanguage.GuessInnerTypes(const AKind:TSymbolKind;
                     const AOrigin,AOwner:TBlock;
                     const AName:String;
                     const AsPointer:Boolean;
                     const AResult:TTypeSpecification;
                     const ATypeParams:TTypeParameters):TExpression;

var
  tmpSymbol : TFindSymbol;

  function FindInSectionTypes(const ATypes:TTypeDeclarations):TExpression;
  var t : Integer;
      tmp : TTypeDeclaration;
      tmpExp : TExpression;
  begin
    if ATypes<>nil then
     for t:=0 to ATypes.Count-1 do
     begin
       tmp:=ATypes.Get(t);

       if tmp.Expression is TRecordSpecification then
          if TRecordSpecification(tmp.Expression).Types<>nil then
          begin
            tmpExp:=TRecordSpecification(tmp.Expression).Find(tmpSymbol);

            if tmpExp is TTypeDeclaration then
            begin
              result:=TTypeDeclaration(tmpExp);
              Exit;
            end;
          end;
     end;


     result:=nil;
  end;

  function FindInSection(const ASection:TUsesSection):TExpression;
  var t : Integer;
  begin
    result:=FindInSectionTypes(ASection.Types);

    if result=nil then
       for t:=0 to ASection.UsesUnits.Count-1 do
       begin
         result:=FindInSectionTypes(TUnit(ASection.UsesUnits[t]).UnitInterface.Types);

         if result<>nil then
            Exit;
       end;
  end;

var tmp : TUnit;
    tmpSection : TBlock;
begin
  tmp:=TUnit.UnitOf(AOwner,tmpSection);

  if tmp=nil then
     result:=nil // <-- Internal error !
  else
  begin
    tmpSymbol.Kind:=AKind;
    tmpSymbol.Origin:=AOrigin;
    tmpSymbol.Owner:=AOwner;
    tmpSymbol.Name:=AName;
    tmpSymbol.AsPointer:=AsPointer;
    tmpSymbol.AResult:=AResult;
    tmpSymbol.TypeParams:=ATypeParams;
    tmpSymbol.Parent:=AOwner;

    if tmpSection=tmp.ModuleImplementation then
       result:=FindInSection(tmp.ModuleImplementation)
    else
       result:=nil;

    if result=nil then
       result:=FindInSection(tmp.UnitInterface);
  end;
end;

function TTypeRedirect.Find(const ASymbol:TFindSymbol):TExpression;
begin
  result:=TypeIdentifier.Expression.Find(ASymbol);
end;

{ TTypeRange }

function TTypeRange.IsNumber:Boolean;
var tmp : TTypeSpecification;
begin
  tmp:=Range.TypeIdentifier.Expression;

  result:=(tmp=NumberSpec) or (tmp=NumberOrSetSpec);
end;

{ TConstantDeclarations }

procedure TConstantDeclarations.Add(const AConstant:TConstantDeclaration);
begin
  AddSorted(AConstant);
end;

function TConstantDeclarations.Find(const AName:String):TConstantDeclaration;
var tmp : TIdentifier;
begin
  tmp:=SortedFind(AName);

  if tmp=nil then result:=nil
             else result:=TConstantDeclaration(tmp);
end;

function TConstantDeclarations.Get(Index: Integer): TConstantDeclaration;
begin
  result:=TConstantDeclaration(Items[Index]);
end;

Constructor TRecordFieldsDeclarations.Clone(const AOwner:TBlock;
                                            const AFields:TIdentifiers;
                                            const Duplicates:Boolean=False);

  function CloneField(const AOwner:TBlock; const AField:TVariableDeclaration):TVariableDeclaration;
  begin
    if AField is TCaseVariable then
    begin
      result:=TCaseVariable.CreateName(AOwner,AField.Name);
      result.TypeIdentifier:=AField.TypeIdentifier;
      TCaseVariable(result).Fields:=TRecordFieldsDeclarations.Clone(AOwner,TCaseVariable(AField).Fields,True);
    end
    else
    if AField is TCaseVariableItem then
    begin
      result:=TCaseVariableItem.CreateName(AOwner,AField.Name);
      result.TypeIdentifier:=AField.TypeIdentifier;

      TCaseVariableItem(result).Expression:=TCaseVariableItem(AField).Expression;

      if TCaseVariableItem(AField).Value is TRecordFieldsDeclarations then
         TCaseVariableItem(result).Value:=TRecordFieldsDeclarations.Clone(AOwner,TRecordFieldsDeclarations(TCaseVariableItem(AField).Value))
      else
         AOwner.Error('Cannot not clone');
    end
    else
    begin
      result:=TVariableDeclaration.CreateType(AOwner,AField.TypeIdentifier);
      result.Name:=AField.Name;
    end;
  end;

var t : Integer;
begin
  Create(AOwner);
  Sorted.Duplicates:=Duplicates;

  if AFields<>nil then
     for t:=0 to AFields.Count-1 do
         Add(CloneField(AOwner,TVariableDeclaration(AFields.Get(t))));
end;

{ TRangeExpression }

Constructor TRangeExpression.CreateRange(const AOwner:TBlock; const AStart,AEnd:TExpression);
begin
  inherited Create(AOwner);

  StartRange:=AStart;
  EndRange:=AEnd;

  Parser.Language.ValueFinal(StartRange,TypeIdentifier);
end;

{ TArraySpecification }

procedure TArraySpecification.AddDimension(const ADimension: TExpression);
{$IFNDEF ARRAYCAT}
var L : Integer;
{$ENDIF}
begin
  {$IFDEF ARRAYCAT}
  Dimensions:=Dimensions+[ADimension];
  {$ELSE}
  L:=Length(Dimensions);
  SetLength(Dimensions,L+1);
  Dimensions[L]:=ADimension;
  {$ENDIF}
end;

function TArraySpecification.SameArrayTypes(const B:TArraySpecification):Boolean;
var LA,LB : Integer;
begin
  result:=Expression.TypeIdentifier.Expression=B.Expression.TypeIdentifier.Expression;

  if result then
  begin
    LA:=Length(Dimensions);
    LB:=Length(B.Dimensions);

    result:=(LA=LB) or ((LA=1) and (LB=0));
  end;
end;

function TArraySpecification.CompatibleArrayTypes(const B:TTypeSpecification):Boolean;
begin
  if B is TArraySpecification then
     result:=SameArrayTypes(TArraySpecification(B))
  else
     result:=Expression.Expression=B;
end;

function TArraySpecification.CompatibleArrayValues(const AValues:TArrayValues):Boolean;
var t : Integer;
    tmp : TExpression;
begin
  result:=True;
  Exit;

  // Pending !
  if (Dimensions=nil) or (Length(Dimensions)=1) then
     result:=CompatibleArrayTypes(AValues.TypeIdentifier.Expression)
  else
  begin
    tmp:=AValues;

    result:=True;

    for t:=Low(Dimensions) to High(Dimensions) do
    begin
      if not CompatibleArrayTypes(tmp.TypeIdentifier.Expression) then
         result:=False;
    end;
  end;
end;

function TArraySpecification.Expand(const AParams:TTypeParameters): TTypeSpecification;
var t,
    L : Integer;
    tmpGeneric : Boolean;
begin
  tmpGeneric:=Expression is TTypeParameter;

  L:=Length(Dimensions);

  if not tmpGeneric then
     for t:=0 to L-1 do
         if Dimensions[t] is TTypeParameter then
         begin
           tmpGeneric:=True;
           break;
         end;

  if tmpGeneric then
  begin
    result:=TArraySpecification.Create(Self);

    TArraySpecification(result).Expression:=TrySubstituteIndex(Expression,AParams) as TTypeDeclaration;

    if L>0 then
    begin
      SetLength(TArraySpecification(result).Dimensions,L);

      for t:=0 to L-1 do
          TArraySpecification(result).Dimensions[t]:=TrySubstituteIndex(Dimensions[t],AParams);
    end;
  end
  else
    result:=Self;
end;

function TArraySpecification.Find(const ASymbol:TFindSymbol):TExpression;
var tmpType : TTypeDeclaration;
    tmpSpec : TConstructorSpecification;
    tmpParam : TVariableDeclaration;
begin
  result:=inherited {$IFNDEF D12}Find(ASymbol){$ENDIF};

  if (result=nil) and (Parent.Language.CompilerVersion>=210) then
     if SameText(ASymbol.Name,'CREATE') then
     begin
       if CreateConstructor=nil then
       begin
         CreateConstructor:=TMethodDeclaration.Create(Self);

         tmpSpec:=TConstructorSpecification.Create(CreateConstructor);
         tmpSpec.OfRecord:=TTypeDeclaration(Owner);

         tmpSpec.VariableParameters:=True; // Any number of parameters
         tmpSpec.FixedParameters:=1; // At least one

         tmpParam:=TVariableDeclaration.CreateType(CreateConstructor,Expression);

         tmpSpec.Parameters:=TParameters.Create(CreateConstructor);
         tmpSpec.Parameters.Add(tmpParam);

         tmpType:=TTypeDeclaration.CreateSpec(CreateConstructor,tmpSpec);
         CreateConstructor.TypeIdentifier:=tmpType;
       end;

       result:=CreateConstructor;
     end;
end;

function TArraySpecification.ValuesOfIndex(const AOwner: TBlock; const AIndex:Integer): TArrayValues;
begin
  result:=TArrayValues.Create(AOwner);

  if (Dimensions=nil) or (AIndex<High(Dimensions)) then
     result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,Self)
  else
     result.TypeIdentifier:=Expression;

  result.DimensionIndex:=AIndex;
end;

{ TTypeDeclarations }

Constructor TTypeDeclarations.Create(const AOwner:TBlock);
begin
  inherited;
  Sorted.Duplicates:=True; //<--Forward // Parent.CompilerVersion>=200;
end;

destructor TTypeDeclarations.Destroy;
begin
  SetSpecs.Free;
  SetOfSpecs.Free;

  inherited;
end;

function TTypeDeclarations.Find(const ASymbol:TFindSymbol):TTypeDeclaration;
var tmp : TTypeDeclaration;
    L,
    t : Integer;
    tmpIndex : Integer;
    tmpParams : TTypeParameters;
begin
  if Sorted.Find(ASymbol.Name,tmpIndex) then
  begin
    result:=nil;

    L:=Length(ASymbol.TypeParams);

    tmp:=GetSorted(tmpIndex);

    if tmp=ASymbol.Origin then
       result:=nil
    else
    for t:=tmpIndex to Count-1 do
    begin
      tmp:=GetSorted(t);

      if SameText(tmp.Name,ASymbol.Name) then
      begin
        tmpParams:=tmp.TypeParameters; //(tmp.Expression);

        if Length(tmpParams)=L then
        begin
          result:=tmp;
          break;
        end;
      end
      else
        break;
    end;
  end
  else
    result:=nil;

  // TFoo=(a,b,c)
  if (result=nil) {and (not Parent.ScopedEnums)} then
    if (SetSpecs<>nil) and SetSpecs.Find(ASymbol.Name,tmpIndex) then
        result:=TTypeDeclaration(SetSpecs.FList[tmpIndex].O);

  // TFoo=set of (a,b,c)  <-- No ScopedEnums check, always try to find.
  if (result=nil) and (SetOfSpecs<>nil) and SetOfSpecs.Find(ASymbol.Name,tmpIndex) then
        result:=TTypeDeclaration(SetOfSpecs.FList[tmpIndex].O);
end;

procedure TTypeDeclarations.AddSet(const AItem:TIdentifier; IsSetOf:Boolean);

  procedure AddToList(var AList:TBaseList);
  begin
    if AList=nil then
    begin
      AList:=TBaseList.Create;
      AList.Duplicates:=AItem.Parent.ScopedEnums;
    end;

    AList.AddItem(AItem.Name,AItem);
  end;

begin
  if IsSetOf then
     AddToList(SetOfSpecs)
  else
     AddToList(SetSpecs);
end;

procedure TTypeDeclarations.RelinkForward(StartType:Integer; const AType:TTypeDeclaration);
var t : Integer;
    tmp : TTypeDeclaration;
begin
  for t:=1+StartType to Count-1 do
  begin
    tmp:=Get(t);

    if tmp<>AType then
    begin
      if SameText(tmp.ForwardExpression,AType.Name)
         or
           (
             // Special case: type PFoo=^TFoo (when TFoo existed already)
             // and appears later
             (tmp.Expression is TTypeRedirect{ TTypePointerOf})
             and
             (TTypeRedirect(tmp.Expression).TypeIdentifier<>nil)
             and
             SameText(TTypeRedirect(tmp.Expression).TypeIdentifier.Name,AType.Name)
           )
         then
      begin
        if tmp.Expression is TTypeRedirect then
        begin
          TTypeRedirect(tmp.Expression).TypeIdentifier:=AType;
      //    TTypeRedirect(tmp.Expression).TypeIdentifier.Expression:=AType.Expression;
          TTypeRedirect(tmp.Expression).TypeIdentifier.GenericSpec:=nil;
        end
        else
        begin
          tmp.Expression:=AType.Expression;
          tmp.GenericSpec:=nil;
        end;

        tmp.ForwardExpression:='';
        tmp.IsForward:=True;

        //RelinkForwardClassOf(StartType,tmp);
        RelinkForward(StartType,tmp);

      end
      else
      if tmp.Expression is TClassOf then
      begin
        if SameText(TClassOf(tmp.Expression).TypeIdentifier.ForwardExpression,AType.Name) then
        begin
          TClassOf(tmp.Expression).TypeIdentifier.Expression:=AType.Expression;
          TClassOf(tmp.Expression).TypeIdentifier.GenericSpec:=nil;
          TClassOf(tmp.Expression).TypeIdentifier.ForwardExpression:='';
          TClassOf(tmp.Expression).TypeIdentifier.IsForward:=True;

          RelinkForward(StartType,TClassOf(tmp.Expression).TypeIdentifier);
        end;
      end;
    end;
  end;
end;

procedure TTypeDeclarations.VerifyForward(StartType:Integer);
var t : Integer;
    tmp : TTypeDeclaration;
begin
  for t:=1+StartType to Count-1 do
  begin
    tmp:=Get(t);

    if (tmp.ForwardExpression='') and (tmp.Expression is TClassOf) then
       tmp:=TClassOf(tmp.Expression).TypeIdentifier;

    if tmp.ForwardExpression<>'' then
       Error('Declaration of Type '+tmp.ForwardExpression+' not yet finalized');
  end;
end;

function TTypeDeclarations.Get(Index: Integer): TTypeDeclaration;
begin
  result:=TTypeDeclaration(Items[Index]);
end;

function TTypeDeclarations.GetSorted(Index:Integer):TTypeDeclaration;
begin
  result:=TTypeDeclaration(Sorted.FList[Index].O);
end;

{ TMethodDeclarations }

Constructor TMethodDeclarations.Create(const AOwner:TBlock);
begin
  inherited;
  Sorted.Duplicates:=True; // overloads
end;

procedure TMethodDeclarations.Add(const AMethod:TMethodDeclaration);

  function CanBeDuplicate(const A,B:TMethodDeclaration):Boolean;

    function Specification(const M:TMethodDeclaration):TMethodSpecification;
    begin
      result:=TMethodSpecification(M.TypeIdentifier.Expression);
    end;

  var EA,
      EB : TMethodSpecification;
      DA,
      DB : TMethodDirectives;
  begin
    if A.Parent.IsFPC then
    begin
      // Same-unit overloads do not need the "overload" keyword
      result:=True;
      Exit;
    end;

    EA:=Specification(A);

    // Since VER210:
    if EA.IsClass and
       (
         (EA is TConstructorSpecification)
         or
         (EA is TDestructorSpecification)
       ) then
    begin
      result:=True;
      Exit;
    end;

    DA:=EA.Directives;

    // Pending: For override methods, use its ancestor declaration to obtain "IsOverload"
    result:=(mdOverload in DA) or (mdForward in DA);

    if not result then
    begin
      EB:=Specification(B);

      // Since VER210:
      if EB.IsClass and
         (
           (EB is TConstructorSpecification)
           or
           (EB is TDestructorSpecification)
         ) then
      begin
        result:=True;
        Exit;
      end;

      if Parent.Language.CompilerVersion>=210 then
         if (EA is TRecordOperatorSpec) or (EB is TRecordOperatorSpec) then
         begin
           result:=True;
           Exit;
         end;

      DB:=EB.Directives;

      result:=((mdVirtual in DA) or (mdOverride in DA)) and (mdOverride in DB);

      if not result then
         result:=mdOverload in DB;
    end;
  end;

var tmp : TIdentifier;
begin
  if AMethod.Name='' then
     Error('Empty method name');

  tmp:=SortedFind(AMethod.Name);

  if tmp<>nil then
     if (not (tmp is TMethodDeclaration)) or
        (not CanBeDuplicate(TMethodDeclaration(tmp),AMethod)) then
           Error('Duplicate method: '+AMethod.Name);

  AddSorted(AMethod);
end;

function TMethodDeclarations.Find(const ASymbol:TFindSymbol):TMethodDeclaration;

  function CanUseClass(const AMethod:TMethodDeclaration):Boolean;
  var tmpSpec : TRecordSpecification;
      tmpType : TTypeDeclaration;
      tmpRecord : TRecordSpecification;
      tmpMethod : TMethodSpecification;
  begin
    if ASymbol.Owner is TExpression then
    begin
      Parser.Language.ValueFinal(TExpression(ASymbol.Owner),tmpType);

      if tmpType.Expression is TClassOf then
         tmpType:=TClassOf(tmpType.Expression).TypeIdentifier;

      tmpRecord:=TRecordSpecification.RecordOf(tmpType);

      if (tmpRecord=nil) and (tmpType.Expression<>nil) then
         if tmpType.Expression.Helper is TRecordSpecification then
            tmpRecord:=TRecordSpecification(tmpType.Expression.Helper);
    end
    else
      tmpRecord:=TRecordSpecification.RecordOf(ASymbol.Owner);

    if tmpRecord=nil then
       result:=False
    else
    begin
      tmpMethod:=TMethodSpecification(AMethod.TypeIdentifier.Expression);
      tmpSpec:=TRecordSpecification(tmpMethod.OfRecord.Expression);

      result:=tmpRecord.Helper=tmpSpec;

      if not result then
      repeat
        result:=tmpRecord.IsClassMeOrAncestor(tmpSpec);

        if not result then
           if tmpRecord.Outer=nil then
              break
           else
              tmpRecord:=tmpRecord.Outer;

      until result;
    end;
  end;

  function Specification(const M:TMethodDeclaration):TMethodSpecification;
  begin
    result:=TMethodSpecification(M.TypeIdentifier.Expression);
  end;

var t : Integer;
    tmp : TMethodDeclaration;
    tmpOk,
    tmpIdentical : Boolean;
    tmpIndex : Integer;
    tmpMethod : TMethodSpecification;
begin
  if Sorted.Find(ASymbol.Name,tmpIndex) then
  begin
    tmp:=GetSorted(tmpIndex);

    if ASymbol.AsPointer and
       ((not Specification(tmp).IsClass) or CanUseClass(tmp)) and
       (
         (ASymbol.AResult=nil)
         or
         Parser.Language.IsMethodPointer(ASymbol.AResult.Redirect)
       ) then
    begin
      result:=tmp;
      Exit;
    end
    else
    begin
      result:=nil;

      for t:=tmpIndex to Count-1 do
      begin
        tmp:=GetSorted(t);

        if SameText(tmp.Name,ASymbol.Name) then
        begin
          tmpMethod:=Specification(tmp);

          if tmpMethod.IsClass then
             tmpOk:=CanUseClass(tmp)
          else
             tmpOk:=True;

          if tmpOk and
             tmpMethod.ValidParameters(ASymbol,tmpIdentical) then
          begin
            result:=tmp;

            if tmpIdentical then
               Exit;
          end;
        end
        else
          break;
      end;
    end;
  end
  else
    result:=nil;
end;

function TMethodDeclarations.Get(Index:Integer):TMethodDeclaration;
begin
  result:=TMethodDeclaration(Items[Index]);
end;

function TMethodDeclarations.GetSorted(Index:Integer):TMethodDeclaration;
begin
  result:=TMethodDeclaration(Sorted.FList[Index].O);
end;

function TMethodDeclarations.FindSpec(const ASpec:TMethodSpecification):TMethodDeclaration;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Get(t).TypeIdentifier.Expression=ASpec then
      begin
        result:=Get(t);
        Exit;
      end;

  result:=nil;
end;

procedure TMethodDeclarations.FindAll(const AName:String; var AItems:TMethodSpecifications);

  function Specification(const M:TMethodDeclaration): TMethodSpecification;
  begin
    result:=TMethodSpecification(M.TypeIdentifier.Expression);
  end;

var t,
    tmpIndex,
    L : Integer;
    tmp : TMethodDeclaration;
begin
  if Sorted.Find(AName,tmpIndex) then
  begin
    L:=Length(AItems);
    SetLength(AItems,L+1);
    AItems[L]:=Specification(GetSorted(tmpIndex));

    Inc(L);

    for t:=tmpIndex+1 to Count-1 do
    begin
      tmp:=GetSorted(t);

      if SameText(tmp.Name,AName) then
      begin
        SetLength(AItems,L+1);
        AItems[L]:=Specification(tmp);
        Inc(L);
      end
      else
        break;
    end;
  end;
end;

{ TRecordSpecification }

procedure TRecordSpecification.AddOrdered(const AList:TIdentifiers; const Initial:Integer);
var t,L : Integer;
begin
  L:=Length(Ordered);
  SetLength(Ordered,L+AList.Count-Initial);

  for t:=Initial to AList.Count-1 do
      Ordered[L+t-Initial]:=AList.Get(t);
end;

procedure TRecordSpecification.AddOrderedSingle(const ABlock:TBlock);
var L : Integer;
begin
  {$IFDEF ARRAYCAT}
  Ordered:=Ordered+[ABlock];
  {$ELSE}
  L:=Length(Ordered);
  SetLength(Ordered,L+1);
  Ordered[L]:=ABlock;
  {$ENDIF}
end;

procedure TRecordSpecification.AddOperator(const AOperator:TRecordOperatorSpec; const AName:String);
var L : Integer;
    tmp : TRecordOperatorKind;
begin
  tmp:=AOperator.Kind;

  {$IFDEF ARRAYCAT}
  Operators[tmp]:=Operators[tmp]+[AOperator];
  {$ELSE}
  L:=Length(Operators[tmp]);
  SetLength(Operators[tmp],L+1);
  Operators[tmp,L]:=AOperator;
  {$ENDIF}
end;

procedure TRecordSpecification.AddField(const AField:TVariableDeclaration);
begin
  CheckFields;
  Fields.Add(AField);
  AddOrderedSingle(AField);
end;

procedure TRecordSpecification.CheckFields;
begin
  if Fields=nil then
  begin
    Fields:=TRecordFieldsDeclarations.Create(Self);
    //AFields.Visibility:=tmpVisibility;

    // Due to indexed properties (they can have the same name and different types)
    Fields.Sorted.Duplicates:=True;
  end;
end;

class function TRecordSpecification.RecordOf(AOwner:TBlock):TRecordSpecification; //overload;
begin
  if (AOwner is TTypeDeclaration) and
     (TTypeDeclaration(AOwner).Expression is TRecordSpecification) then
       result:=TRecordSpecification(TTypeDeclaration(AOwner).Expression)
  else
  if (AOwner is TCasting) and
     (TCasting(AOwner).TypeIdentifier.Expression is TRecordSpecification) then
        result:=TRecordSpecification(TCasting(AOwner).TypeIdentifier.Expression)
  else
  begin
    repeat
      if (AOwner is TMethodSpecification) and
         (TMethodSpecification(AOwner).OfRecord<>nil) then
      begin
        result:=TRecordSpecification(TMethodSpecification(AOwner).OfRecord.Expression);
        Exit;
      end
      else
      if AOwner is TRecordSpecification then
      begin
        result:=TRecordSpecification(AOwner);
        Exit;
      end
      else
        AOwner:=AOwner.Owner;

    until AOwner=nil;

    result:=nil;
  end;
end;

function TRecordSpecification.CloneFields(const AOwner:TBlock):TRecordInstance;
begin
  result:=TRecordInstance.Create(AOwner);
  result.Fields:=TRecordFieldsDeclarations.Clone(result,Fields);
  result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,Self);
end;

procedure TRecordSpecification.AddAncestor(const AType:TTypeDeclaration);
{$IFNDEF ARRAYCAT}
var L : Integer;
{$ENDIF}
begin
  {$IFDEF ARRAYCAT}
  Ancestors:=Ancestors+[AType];
  {$ELSE}
  L:=Length(Ancestors);
  SetLength(Ancestors,L+1);
  Ancestors[L]:=AType;
  {$ENDIF}
end;

function TRecordSpecification.DefaultProperty(InAncestors:Boolean):TPropertyDeclaration;
var tmpSpec : TTypeSpecification;
begin
  result:=PropertyDefault;

  if result=nil then
  begin
    if InAncestors and (Length(Ancestors)>0) then
    begin
      tmpSpec:=Ancestors[0].Expression;

      //if not (tmpSpec is TInterfaceSpecification) then
         result:=TRecordSpecification(tmpSpec).DefaultProperty(InAncestors);
    end;
  end;
end;

procedure TRecordSpecification.SetRecordType(const AType: TTypeDeclaration;
                                             const AParams: TExpressions);

  function CalcOuter:TRecordSpecification;
  var tmp : TBlock;
  begin
    tmp:=RecordType.Owner;

    if (tmp is TTypeDeclarations) and
       (tmp.Owner is TRecordSpecification) then
         result:=TRecordSpecification(tmp.Owner)
    else
       result:=nil;
  end;

begin
  Module:=TUnit.UnitOf(Owner);
  RecordType:=AType;
  TypeParameters:=AParams;

  Outer:=CalcOuter;
end;

function TRecordSpecification.Find(const ASymbol:TFindSymbol):TExpression;

  function FindInAncestors(const Ancestors:TTypeDeclarationArray):TExpression;
  var t : Integer;
      tmp : TTypeDeclaration;
      S : TFindSymbol;
      OldTypes : TTypeParameters;
      tmpType : TTypeDeclaration;
  begin
    OldTypes:=ASymbol.TypeParams;

    for t:=Low(Ancestors) to High(Ancestors) do
    begin
      tmp:=Ancestors[t];

      S:=ASymbol;

      if (Parent.Language.CompilerVersion>=210) and (S.TypeParams=nil) then
      begin
        if S.Parent is TTypeDeclaration then
           S.TypeParams:=FindTypesIn(TTypeDeclaration(S.Parent));

        if S.TypeParams=nil then
           if tmp.TypeParameters<>nil then
              S.TypeParams:=tmp.TypeParameters;
      end;

      if S.TypeParams=nil then
         result:=nil
      else
         result:=FindATypeParam(S.Name,S.TypeParams);

      if result=nil then
         result:=tmp.Expression.Find(S);

      if result<>nil then
         if not (result is TMethodDeclaration) then
         if result is TConstantDeclaration then
            if S.TypeParams<>nil then
               if result.TypeIdentifier.Expression is TArraySpecification then
                  if TTypeDeclaration.TryExpand(result.TypeIdentifier,tmpType,S.TypeParams) then
                  begin
                    result:=TConstantDeclaration.CreateName(result,TConstantDeclaration(result).Name);
                    result.TypeIdentifier:=tmpType;
                  end;

      S.TypeParams:=OldTypes;

      if result<>nil then
         Exit;
    end;

    result:=nil;
  end;

var tmp : TIdentifier;
begin
  result:=inherited {$IFNDEF D12}Find(ASymbol){$ENDIF};

  if result=nil then
  begin
    if ASymbol.Kind<>skField then
    begin
      if Types=nil then
         result:=nil
      else
         result:=Types.Find(ASymbol);
    end
    else
    begin
      if Fields<>nil then
         result:=Fields.Find(ASymbol.Name);

      if (result=nil) and (Methods<>nil) then
         result:=Methods.Find(ASymbol);

      if (result=nil) and (Types<>nil) then
         result:=Types.Find(ASymbol);

      if (result=nil) and (Constants<>nil) then
         result:=Constants.Find(ASymbol.Name);

      if (result<>nil) and (result is TIdentifier) then
      begin
        tmp:=TIdentifier(result);

        if (tmp.Visibility<>vPublic) and
           (tmp.Visibility<>vPublished) and

           (not TestVisibility(ASymbol.Origin {Self},ASymbol.Owner,tmp)) then
                result:=nil;
      end;

      if result=nil then
         result:=FindInAncestors(Ancestors);
    end;

    if (result=nil) and (RecordType<>nil) then
       if Outer<>nil then
          result:=Outer.Find(ASymbol);
  end;
end;

function TRecordSpecification.FindAncestor(const AType: TTypeSpecification): TTypeDeclaration;
var t : Integer;
begin
  for t:=Low(Ancestors) to High(Ancestors) do
      if Ancestors[t].Expression=AType then
      begin
        result:=Ancestors[t];
        Exit;
      end;

  result:=nil;
end;

function TRecordSpecification.FindOperator(const MatchResult:Boolean;
                             const ASpec:TTypeSpecification;
                             const AKind:TRecordOperatorKind
                             //; IsNilConst:Boolean=False ?
                             ): Boolean;
var t : Integer;
    tmpRes : TVariableDeclaration;
    tmpOp : TTypeSpecifications;
    tmpSpec : TRecordOperatorSpec;
    tmpSameType : Boolean;
begin
  tmpOp:=Operators[AKind];

  if Length(tmpOp)>0 then
  begin
    for t:=Low(tmpOp) to High(tmpOp) do
    begin
      tmpSpec:=TRecordOperatorSpec(tmpOp[t]);

      if MatchResult then
         tmpRes:=tmpSpec.ResultValue
      else
         tmpRes:=tmpSpec.Parameters[0]; // Pending: Check empty param !

      if Parser.Language.CheckCompatible(tmpRes.TypeIdentifier.Expression,ASpec,tmpSameType) then
      //if tmpRes.TypeIdentifier.Expression=ASpec then
      begin
        result:=True;
        Exit;
      end;
    end;

    //tmpExp.Free;
  end;

  result:=False;
end;

function TRecordSpecification.IsClassMeOrAncestor(const ARecord:TRecordSpecification):Boolean;
var t : Integer;
    tmpExp : TTypeSpecification;
    tmpRec : TRecordSpecification;
begin
  if Self=ARecord then
     result:=True
  else
  begin
    for t:=Low(Ancestors) to High(Ancestors) do
    begin
      tmpExp:=Ancestors[t].Generic;

      if (tmpExp<>nil) and
         (
           (tmpExp=ARecord.Owner) or

           (
             (tmpExp.Owner is TTypeDeclaration) and
             (TTypeDeclaration(tmpExp.Owner).Generic=ARecord)
           )
         ) then
      begin
        result:=True;
        Exit;
      end
      else
      begin
        if tmpExp is TRecordSpecification then
           tmpRec:=TRecordSpecification(tmpExp)
        else
           tmpRec:=nil;

        if (tmpRec<>nil) and tmpRec.IsClassMeOrAncestor(ARecord) then
        begin
          result:=True;
          Exit;
        end;
      end;
    end;

    result:=False;
  end;
end;

function TRecordSpecification.TestVisibility(const AOrigin:TBlock;
                                             const AOwner:TBlock;
                                             const AField:TIdentifier):Boolean;

  function SameOrOuterRecord(ARecord:TRecordSpecification):Boolean;
  begin
    result:=False;

    repeat
      if ARecord=Self then
         result:=True
      else
      begin
        ARecord:=ARecord.Outer;

        if ARecord=nil then
           break;
      end;

    until result;
  end;

var tmp : TTypeDeclaration;
    tmpRecord : TRecordSpecification;
begin
  case AField.Visibility of
   vStrictPrivate: result:=SameOrOuterRecord(RecordOf(AOwner));
 vStrictProtected: begin
            result:=IsMeOrAncestor(AOwner,Self);

            if not result then
            begin
              // Self is outer
              tmpRecord:=RecordOf(AOwner);

              while tmpRecord<>nil do
              begin
                result:=tmpRecord.Outer=Self;

                if result then
                   break
                else
                   tmpRecord:=tmpRecord.Outer;
              end;

            end;
         end;

         vPrivate: result:=TUnit.UnitOf(AOrigin)=Module;

       vProtected: begin
                     result:=(TUnit.UnitOf(AOrigin)=Module) or
                              IsMeOrAncestor(AOwner,Self);

        // Special relaxed protected:
        if (not result) and (AOwner is TExpression) then
        begin
          Parser.Language.ValueFinal(TExpression(AOwner),tmp);

          if tmp.Expression is TClassOf then
             tmp:=TClassOf(tmp.Expression).TypeIdentifier;

          if tmp.Expression is TRecordSpecification then
             result:=(TUnit.UnitOf(AOrigin)=TUnit.UnitOf(tmp)) and
                     IsMeOrAncestor(TRecordSpecification(tmp.Expression),Self);
        end;
      end;
  else
    result:=True;
  end;
end;

{ TClassSpecification }

function TClassSpecification.SupportsInterface(const AInterface:TInterfaceSpecification):Boolean;
var t : Integer;
    tmp : TTypeSpecification;
begin
  result:=False;

  if Length(Ancestors)>0 then
  begin
    for t:=Low(Ancestors) to High(Ancestors) do
    begin
      tmp:=Ancestors[t].TypeIdentifier.Expression;

      if (tmp is TInterfaceSpecification) and
         IsMeOrAncestor(Self,TInterfaceSpecification(tmp)) then
      begin
        result:=True;
        Exit;
      end;
    end;

    tmp:=Ancestors[0].TypeIdentifier.Expression;

    if tmp is TClassSpecification then
       result:=TClassSpecification(tmp).SupportsInterface(AInterface);
  end;
end;

procedure TRecordHelperSpecification.SetType(const AType:TTypeDeclaration);
begin
  TypeIdentifier:=AType;
  TypeIdentifier.Expression.Helper:=Self;
end;

function TRecordHelperSpecification.Find(const ASymbol:TFindSymbol):TExpression;
begin
  result:=inherited {$IFNDEF D12}Find(ASymbol){$ENDIF};

  if (result=nil) and (not Searching) then
  begin
    Searching:=True;
    try
      result:=TypeIdentifier.Find(ASymbol);
    finally
      Searching:=False;
    end;
  end;
end;

{ TSetSpecification }

constructor TSetSpecification.Create(const AOwner: TBlock);
begin
  inherited;
  Items:=TTypeDeclarations.Create(Self);
end;

procedure TSetSpecification.Add(const AIdent: TIdentifier);
begin
  Items.AddSorted(AIdent);

  if Owner is TTypeDeclarations then
     if IsSetOf or (not IsScoped) then
        TTypeDeclarations(Owner).AddSet(AIdent,IsSetOf);
end;

{ TGenericSpec }

function TGenericSpec.Expand(const AParams:TTypeParameters): TTypeSpecification;
begin
  result:=Self;
end;

function TGenericSpec.Find(const ASymbol:TFindSymbol):TExpression;
begin
  result:=inherited {$IFNDEF D12}Find(ASymbol){$ENDIF};

  if (result=nil) and (TypeParameters<>nil) then
     result:=FindATypeParam(ASymbol.Name,TypeParameters);
end;

{ TTypeSpecification }

function TTypeSpecification.IsCompatible(ASpec: TTypeSpecification): Boolean;
begin
  result:=Self=ASpec;
end;

function TTypeSpecification.IsSet:Boolean;
begin
  result:=(Self is TSetOfSpecification) or
          (Self is TSetRange) or
          (Self is TSetSpecification);
end;

function TTypeSpecification.Find(const ASymbol:TFindSymbol):TExpression;
begin
  if Helper=nil then
     result:=nil
  else
     result:=Helper.Find(ASymbol);
end;

function TTypeSpecification.Redirect:TTypeSpecification;
begin
  result:=Self;

  while result is TTypeRedirect do
        result:=TTypeRedirect(result).TypeIdentifier.Expression;
end;

{ TTypeDeclaration }

constructor TTypeDeclaration.Create(const AOwner: TBlock);
begin
  inherited;
  TypeIdentifier:=Self;
end;

Constructor TTypeDeclaration.CreateSpec(const AOwner:TBlock; const ASpec:TTypeSpecification);
begin
  Create(AOwner);
  Expression:=ASpec;
end;

function TTypeDeclaration.Generic: TTypeSpecification;
begin
  if Alias<>nil then
     result:=Alias.Generic
  else
  begin
    if GenericSpec=nil then
    begin
      if (Expression is TGenericSpec) and
         (TypeParameters<>nil) then
           GenericSpec:=TGenericSpec(Expression).Expand(TypeParameters)
      else
         GenericSpec:=Expression;
    end;

    result:=GenericSpec;
  end;
end;

{ TVariableDeclarations }

procedure TVariableDeclarations.Add(const AItem:TVariableDeclaration);
begin
  AddSorted(AItem);
end;

function TVariableDeclarations.Find(const AName:String):TVariableDeclaration;
var tmp : TIdentifier;
begin
  tmp:=SortedFind(AName);

  if tmp=nil then
     result:=nil
  else
     result:=TVariableDeclaration(tmp);
end;

function TVariableDeclarations.Get(Index: Integer): TVariableDeclaration;
begin
  result:=TVariableDeclaration(Items[Index]);
end;

{ TRecordFieldsDeclarations }

procedure TRecordFieldsDeclarations.Add(const AItem:TVariableDeclaration);
var L : Integer;
begin
  inherited {$IFNDEF D12}Add(AItem){$ENDIF};

  if AItem is TCaseVariable then
  begin
    {$IFDEF ARRAYCAT}
    CaseVariables:=CaseVariables+[TCaseVariable(AItem)];
    {$ELSE}
    L:=Length(CaseVariables);
    SetLength(CaseVariables,L+1);
    CaseVariables[L]:=TCaseVariable(AItem);
    {$ENDIF}
  end;
end;

function TRecordFieldsDeclarations.Find(const AName:String):TVariableDeclaration;
var t : Integer;
begin
  result:=inherited {$IFNDEF D12}Find(AName){$ENDIF};

  if result=nil then
    for t:=Low(CaseVariables) to High(CaseVariables) do
    begin
      result:=CaseVariables[t].Find(AName);

      if result<>nil then
         Exit;
    end;
end;

{ TOperatorExpression }

function TOperatorExpression.AsBoolean: Boolean;
begin
  case Operat of
    opEqual        : result:=Left.Value=Right.Value;
    opEqualType    : result:=Left.TypeIdentifier=Right.TypeIdentifier;
    opNotEqual     : result:=Left.Value<>Right.Value;
    opNotEqualType : result:=Left.TypeIdentifier<>Right.TypeIdentifier;
    opLower        : result:=Left.Value<Right.Value;
    opGreater      : result:=Left.Value>Right.Value;
    opLowerEqual   : result:=Left.Value<=Right.Value;
    opGreaterEqual : result:=Left.Value>=Right.Value;
    opIs           : result:=Left.TypeIdentifier.Expression=Right.TypeIdentifier.Expression;
    opAnd          : result:=Left.AsBoolean and Right.AsBoolean;
    opOr           : result:=Left.AsBoolean or Right.AsBoolean;
    opNot          : result:=not Left.AsBoolean;
  else
    begin
      Error('Operator cannot be evaluated as boolean');
      result:=False;
    end;
  end;
end;

function TOperatorExpression.HasPrecedence(const AOperator:TOperator):Boolean;

  function Precedence(const AOp:TOperator):Byte;
  begin
    case AOp of
        opEqual,
        opNotEqual,
        opLower,
        opGreater,
        opLowerEqual,
        opGreaterEqual,
        opIn,
        opIs  : result:=0;

        opAdd,
        opSubtract,
        opOr,
        opXor : result:=1;

        opAt,
        opNot : result:=3;

    else
      {
        opAnd: ;
        opShl: ;
        opShr: ;
        opMultiply: ;
        opDivide: ;
        opDiv: ;
        opMod: ;
        opAs: ;
        opEqualType: ;
        opNotEqualType: ;
      }
      result:=2;
    end;
  end;

begin
  result:=Precedence(Operat) < Precedence(AOperator);
end;

function TOperatorExpression.CalcType:TTypeDeclaration;
begin
  case Operat of
    opEqual,
    opNotEqual,
    opLower,
    opGreater,
    opLowerEqual,
    opGreaterEqual,
    opIn,
    opIs : result:=BooleanType;

    opAs : begin
             result:=Right.TypeIdentifier;

             // Workaround D7 bug:
             if Parent.Language.CompilerVersion<160 then
             begin
               if (result.Expression is TRecordSpecification) and
                 (Left.TypeIdentifier.Expression is TRecordSpecification) and
                 TRecordSpecification(Left.TypeIdentifier.Expression).IsClassMeOrAncestor(TRecordSpecification(result.Expression)) then
                     result:=Left.TypeIdentifier;
             end;
           end;

    opEqualType,
    opNotEqualType: result:=Right.TypeIdentifier;
  else
  begin
    if Left=nil then
       if Right=nil then
          Error('Internal: Missing operator expression Left and Right sides')
       else
         Parser.Language.ValueFinal(Right,result)
    else
      // Pending: Promote "bigger" type Left vs Right
      Parser.Language.ValueFinal(Left,result);
  end
  end;
end;

{ TVariableDeclaration }

Constructor TVariableDeclaration.Create(const AOwner:TBlock);
begin
  inherited;
  NeedsType:=True;
  OptionalValue:=True;
end;

Constructor TVariableDeclaration.CreateType(const AOwner:TBlock; const AType:TTypeDeclaration);
begin
  Create(AOwner);
  TypeIdentifier:=AType;
end;

{ TPropertyDeclaration }

function TPropertyDeclaration.SearchInherited(const ARecord:TRecordSpecification):TPropertyDeclaration;
var tmpField : TExpression;
    tmpFind : TFindSymbol;
begin
  tmpFind.Kind:=skField;
  tmpFind.Origin:=Self;
  tmpFind.Owner:=Self.Owner;
  tmpFind.Parent:=Self.Owner;
  tmpFind.Name:=Self.Name;
  tmpFind.AsPointer:=True;

  tmpField:=ARecord.Find(tmpFind);

  if tmpField is TPropertyDeclaration then
  begin
    result:=TPropertyDeclaration(tmpField);
    TModule(Module).AddUsage(result);
  end
  else
  begin
    Error('Cannot obtain inherited property: '+Name);
    result:=nil;
  end;
end;

procedure TPropertyDeclaration.SearchInheritedType;
begin
  Ancestor:=SearchInherited(TRecordSpecification(RecordType.Expression));
  TypeIdentifier:=Ancestor.TypeIdentifier;

  if TypeIdentifier=nil then
     Error('Missing Type of inherited property: '+Name);

  TModule(Module).AddUsage(TypeIdentifier);

  PropertyType:=TypeIdentifier;
end;

function TPropertyDeclaration.ArrayTypeOf(const AType:TTypeDeclaration):TArraySpecification;
var
  t,
  L : Integer;
begin
  result:=TArraySpecification.Create(Self);

  result.Expression:=AType;

  L:=Indexes.Count;
  SetLength(result.Dimensions,L);

  for t := 0 to L-1 do
      result.Dimensions[t]:=Indexes[t].TypeIdentifier;
end;

function TPropertyDeclaration.IndexExpression(const AddSelf:Boolean):TExpressions;
begin
  if AddSelf then
     SetLength(result,2)
  else
     SetLength(result,1);

  result[0]:=Index;

  if AddSelf then
     result[1]:=TypeIdentifier;
end;

function TPropertyDeclaration.IndexesExpressions(const AddSelf:Boolean):TExpressions;
var L,
    tmpIndex,
    t : Integer;
begin
  L:=Indexes.Count;

  if Index<>nil then
     tmpIndex:=1
  else
     tmpIndex:=0;

  if AddSelf then
     SetLength(result,tmpIndex+L+1)
  else
     SetLength(result,tmpIndex+L);

  for t:=0 to L-1 do
      result[t]:=Indexes[t];

  if Index<>nil then
     result[L]:=Index;

  if AddSelf then
     result[tmpIndex+L]:=PropertyType;
end;

function TPropertyDeclaration.TypeExpression:TExpressions;
begin
  SetLength(result,1);
  result[0]:=Self;
end;

procedure TPropertyDeclaration.CheckImplements;
var t : Integer;
begin
  if ImplementsInterfaces=nil then
     Error('Missing implements interfaces')
  else
  begin
    for t:=Low(ImplementsInterfaces) to High(ImplementsInterfaces) do
    begin
      if (not (ImplementsInterfaces[t] is TTypeDeclaration)) or
         (not (TTypeDeclaration(ImplementsInterfaces[t]).Expression is TInterfaceSpecification)) then
           Error('Implements expression is not a TInterfaceSpecification');
    end;
  end;
end;

function TPropertyDeclaration.SameType(const P:TPropertyDeclaration):Boolean;

  function SameIndexes(const I:TParameters):Boolean;
  var t : Integer;
  begin
    result:=(Indexes.Count=I.Count);

    if result then
       for t:=0 to Indexes.Count-1 do
           if Indexes[t].TypeIdentifier.Expression<>I[t].TypeIdentifier.Expression then
           begin
             result:=False;
             Exit;
           end;
  end;

begin
  result:=PropertyType.Expression=P.PropertyType.Expression;

  if result then
  begin
    if (Indexes<>nil) and (P.Indexes<>nil) then
       result:=SameIndexes(P.Indexes);
  end;
end;

function TPropertyDeclaration.IsIndexed:Boolean;
begin
  result:=(Indexes<>nil) and (Indexes.Count>0);
end;

function TPropertyDeclaration.IsArray:Boolean;
var tmp : TPropertyDeclaration;
begin
  tmp:=Self;

  while tmp.Ancestor<>nil do
        tmp:=tmp.Ancestor;

  result:=tmp.IsIndexed;
end;

{ TIdentifier }

Constructor TIdentifier.CreateName(const AOwner: TBlock; const AName: String);
begin
  Create(AOwner);
  Name:=AName;
end;

function TIdentifier.ToString: String;
begin
  result:=Name;
end;

function TIdentifier.Qualified:String;
var tmp : TBlock;
    tmpS : String;
begin
  result:=Name;

  tmp:=Owner;

  while tmp<>nil do
  begin
    if tmp is TIdentifier then
    begin
      tmpS:=TIdentifier(tmp).Name;

      if tmpS<>'' then
         if result='' then
            result:=tmpS
         else
            result:=tmpS+'.'+result;
    end;

    tmp:=tmp.Owner;
  end;
end;

{ TBlock }

Constructor TBlock.Create(const AOwner:TBlock);
begin
  inherited Create;

  {$IFOPT D+}
  Inc(CreatedBlocks);
  {$ENDIF}

  Owner:=AOwner;

  if Owner<>nil then
  begin
    //SetOwned;

    Parent:=Owner.Parent;

    if Parent<>nil then
    begin
      Parent.Language.CurrentModule.NewBlock(Self);

      if Parent.Directives<>nil then
         AddDirectives;

      Position:=Parent.State.Position;
    end;
  end;
end;

{$IFOPT D+}
Destructor TBlock.Destroy;
begin
  inherited;

  {$IFOPT D+}
  Dec(CreatedBlocks);
  {$ENDIF}
end;
{$ENDIF}

procedure TBlock.AddDirectives;
var tmp : TAttribute;
    t,L : Integer;
begin
  L:=Length(Parent.Directives);
  SetLength(Attributes,L);

  for t:=0 to L-1 do
  begin
    tmp:=Parent.Directives[t];
    tmp.SetOwner(Self);
    Attributes[t]:=tmp;
  end;

  Parent.Directives:=nil;
end;

(*
procedure TBlock.SetOwned;
var L : Integer;
begin
  {$IFDEF ARRAYCAT}
  Owner.Owned:=Owner.Owned+[Self];
  {$ELSE}
  L:=Length(Owner.Owned);
  SetLength(Owner.Owned,L+1);
  Owner.Owned[L]:=Self;
  {$ENDIF}
end;
*)

procedure TBlock.SetOwner(const AOwner: TBlock);
begin
  if Owner<>nil then
     Error('Internal: Already owned');

  Owner:=AOwner;
  Parent:=Owner.Parent;
  //SetOwned;
  Module:=Owner.Module;

  {Package.}Parent.Language.CurrentModule.NewBlock(Self);
end;

procedure TBlock.Error(const S:String);
begin
  if Parent=nil then // ---> Self is TUnitScope
     raise ECodeException.Create(S)
  else
     Parent.Error(Self,S);
end;

class function TLabelDeclaration.FindLabel(const AOwner:TBlock; const S:String):TLabelDeclaration;
begin
  result:=nil;

  if AOwner<>nil then
  begin
    if AOwner is TSection then
       if TSection(AOwner).Labels<>nil then
          result:=TSection(AOwner).Labels.Find(S);

    if result=nil then
       result:=FindLabel(AOwner.Owner,S);
  end;
end;

{ TUses }

procedure TUses.Add(const AModule: TModule);
begin
  if Find(AModule.Name)=nil then
     AddSorted(AModule);
end;

function TUses.Find(const AName: String): TModule;
var tmp : TIdentifier;
begin
  tmp:=SortedFind(AName);

  if tmp=nil then result:=nil
             else result:=TModule(tmp);
end;

function TUses.Get(AIndex:Integer):TModule;
begin
  result:=TModule(Items[AIndex]);
end;

function TUses.Exclude(const AName:String):Boolean;
begin
  result:=Assigned(Parent.Language.FOnExcludeModule) and
          Parent.Language.OnExcludeModule(AName);
end;

function TUses.Find(const ASymbol:TFindSymbol):TIdentifier;
var t : Integer;
begin
  for t:=Count-1 downto 0 do
  begin
    result:=Item[t].FindSymbol(ASymbol);

    if result<>nil then
       Exit;
  end;

  result:=nil;
end;

{$IFNDEF D12}
function IsRelativePath(const Path: string): Boolean;
var
  L: Integer;
begin
  L := Length(Path);
  Result := ((L = 0) or ((L > 0) and (Path[{$IFDEF D9}Low(string){$ELSE}1{$ENDIF}] <> PathDelim)))
    {$IFDEF MSWINDOWS} and ( (L <= 1) or (Path[{$IFDEF D9}Low(string){$ELSE}1{$ENDIF} + 1] <> ':') ); {$ENDIF MSWINDOWS};
end;
{$ENDIF}

procedure TUses.ParseModule(const AName,AFileName:String);
begin
  Add(Parent.Language.Modules.ParseNewModule(AName,AFileName,Parent.Language.ModuleClass));
end;

{ TPackages }

destructor TPackages.Destroy;
//var t : Integer;
begin
{  for t:=0 to Count-1 do
      Items[t].Free;}

  inherited;
end;

function TPackages.Find(const AName: String): TPackage;
var tmp : TBlock;
begin
  tmp:=SortedFind(AName);

  if tmp=nil then
     result:=nil
  else
     result:=TPackage(tmp);
end;

function TPackages.Get(Index: Integer): TPackage;
var tmp : TIdentifier;
begin
  tmp:=inherited Get(Index);

  if tmp=nil then
     result:=nil
  else
     result:=TPackage(tmp);
end;

{ TImplementation }

Constructor TImplementation.Create(const AOwner:TBlock);
begin
  inherited;
  Bodies:=True;
  Empty:=True;
end;

{ TWith }

function TWith.Find(const ASymbol:TFindSymbol):TExpression;
var t : Integer;
    S : TFindSymbol;
    OldParent : TBlock;
    tmpType : TTypeDeclaration;
begin
  S:=ASymbol;
  OldParent:=S.Owner;

  for t:=High(Items) downto Low(Items) do
  begin
    S.Owner:=Items[t]; // <-- restrict vStrictProtected ?

    result:=Items[t].Find(S {ASymbol});

    if result<>nil then
       if (Items[t].TypeIdentifier.TypeParameters<>nil) and
          (result is TMethodDeclaration) then
             result:=TFieldExpression.CreateField(result,Items[t],result)
       else
       if Items[t].TypeIdentifier.Expression is TMethodSpecification then
       begin
         tmpType:=TMethodSpecification(items[t].typeidentifier.expression).ofrecord;

         if tmpType.TypeParameters<>nil then
            result:=TFieldExpression.CreateField(result,Items[t],result)
       end;

    S.Owner:=OldParent;

    if result<>nil then
       Exit;
  end;

  result:=nil;
end;

{ TDereferenceExpression }

constructor TDereferenceExpression.CreateValue(const AOwner: TBlock; const AValue: TExpression);
begin
  Create(AOwner);
  SetValue(AValue);
end;

procedure TDereferenceExpression.SetValue(const AExp: TExpression);
var tmp : TTypeDeclaration;
    tmpExp : TTypeSpecification;
begin
  Value:=AExp;
  Parser.Language.ValueFinal(Value,TypeIdentifier);

  tmp:=TypeIdentifier;

  while tmp.Alias<>nil do
        tmp:=tmp.Alias;

  tmpExp:=tmp.Expression;

  if (tmpExp<>VariantSpec) and
     (tmpExp<>PointerSpec) and
     (not (tmpExp is TTypePointerOf)) then
       Error('Dereference Value type is not a Pointer or Variant');
end;

{ TFieldExpression }

Constructor TFieldExpression.CreateField(const AOwner:TBlock; const AValue,AField:TExpression);
begin
  inherited Create(AOwner);

  Value:=AValue;
  Field:=AField;

  TypeIdentifier:=Field.TypeIdentifier;

  TModule(Module).AddUsage(TypeIdentifier);
end;

{ TLabelDeclarations }

function TLabelDeclarations.Find(const AName:String):TLabelDeclaration;
var tmp : TIdentifier;
begin
  tmp:=SortedFind(AName);

  if tmp=nil then result:=nil
             else result:=TLabelDeclaration(tmp);
end;

{ TCasting }

constructor TCasting.CreateType(const AOwner:TBlock; const AType: TTypeDeclaration;
  const AExpression: TExpression);
begin
  Create(AOwner);
  TypeIdentifier:=AType;
  Expression:=AExpression;
end;

function TCasting.ExpressionValue(out AType: TTypeDeclaration): TExpression;
begin
  AType:=TypeIdentifier;

  {
  if (Parent.CompilerVersion>=230) and
     (TypeIdentifier.Expression is TRecordSpecification) then
  begin
    if TRecordSpecification(TypeIdentifier.Expression).FindOperator(Expression,okExplicit) then
  end
  else
  }
     result:=Expression;
end;

{ TCallExpression }

Constructor TCallExpression.CreateCall(const AOwner:TBlock; const AExpression:TExpression;
                                   const AParameters:TExpressions);
begin
  inherited Create(AOwner);

  Expression:=AExpression;
  Parameters:=AParameters;

  TypeIdentifier:=Expression.TypeIdentifier;

  if TypeIdentifier.Expression is TFunctionSpecification then
     TypeIdentifier:=TFunctionSpecification(TypeIdentifier.Expression).ResultValue.TypeIdentifier;

  TModule(Module).AddUsage(TypeIdentifier);
end;

{ TBody }

Constructor TBody.Create(const AOwner:TBlock);
begin
  inherited;
  Bodies:=True;
end;

{ TMethodSpecification }

function TMethodSpecification.ValidParameters(const ASymbol:TFindSymbol;
                                        out IdenticalTypes:Boolean):Boolean;
begin
  result:=(ASymbol.Params=nil) and (ASymbol.AResult=nil);
  IdenticalTypes:=result;
end;

function TMethodSpecification.Find(const ASymbol:TFindSymbol):TExpression;
begin
  // result:=inherited ??  (see TMethodParams.Find)

  if (OfRecord=nil) or (OfRecord.Expression=nil) then
     result:=nil
  else
     result:=OfRecord.Expression.Find(ASymbol);
end;

procedure TMethodSpecification.MatchParameters(const AInterface:TMethodSpecifications);
var
  tmpParams : TMethodParams;
  HasAParams : Boolean;

  function SameResult(const A,B:TFunctionSpecification; DoError:Boolean; out AError:String):Boolean;

    function FinalType(const AFunc:TFunctionSpecification):TTypeSpecification;
    begin
      result:=AFunc.ResultValue.TypeIdentifier.Expression;
    end;

  var tmpA,
      tmpB : TTypeSpecification;
  begin
    tmpA:=FinalType(A);
    tmpB:=FinalType(B);

    result:=tmpA=tmpB;

    if not result then
       if DoError then
          AError:='Different function result types';
  end;

  function SameSignature(const B:TMethodSpecification; DoError:Boolean; out AError:String):Boolean;
  var HasBParams : Boolean;
  begin
    result:=False;

    HasBParams:=(B is TMethodParams) and TMethodParams(B).HasParameters;

    if HasAParams and (not HasBParams) then
    begin
      if DoError then
         AError:='No parameters at implementation declaration';

      Exit;
    end;

    if (not HasAParams) and HasBParams then
    begin
      if DoError then
         AError:='No parameters at interface declaration';

      Exit;
    end;

    if HasAParams and HasBParams and
       (not SameParameters(tmpParams.Parameters,TMethodParams(B).Parameters,DoError,AError)) then
          Exit;

    if Self is TFunctionSpecification then
    begin
      if not (B is TFunctionSpecification) then
      begin
        if DoError then
           AError:='Not a function';

        Exit;
      end;

      if TFunctionSpecification(Self).ResultValue=nil then
      begin
        TFunctionSpecification(Self).ResultValue:=TFunctionSpecification(B).ResultValue;

        if TFunctionSpecification(Self).ResultValue=nil then
        begin
          if DoError then
             AError:='Function has not result';

          Exit;
        end;
      end
      else
      if not SameResult(TFunctionSpecification(Self),TFunctionSpecification(B),DoError,AError) then
         Exit;
    end
    else
      if B is TFunctionSpecification then
      begin
        if DoError then
           AError:='Is function';

        Exit;
      end;

    result:=True;
  end;

var t : Integer;
    tmpS,
    tmpError : String;
begin
  if Self is TMethodParams then
  begin
    tmpParams:=TMethodParams(Self);
    HasAParams:=tmpParams.HasParameters;
  end
  else
  begin
    tmpParams:=nil;
    HasAParams:=False;
  end;

  for t:=Low(AInterface) to High(AInterface) do
      if SameSignature(AInterface[t],False,tmpError) then
         Exit;

  tmpError:='';

  for t:=Low(AInterface) to High(AInterface) do
      if not SameSignature(AInterface[t],True,tmpS) then
         tmpError:=tmpError+#13#10+tmpS;

  Error('Method declaration mismatch at '+tmpError);
end;

{ TMethodParams }

function TMethodParams.AnyParamIsTyped: Boolean;

  function IsTyped(const AType:TTypeDeclaration):Boolean;
  begin
    result:=AType is TTypeParameter;

    if not result then
       result:=(AType.Expression is TGenericSpec) and
               (TGenericSpec(AType.Expression).TypeParameters<>nil);
  end;

var t : Integer;
begin
  if Parameters<>nil then
  for t:=0 to Parameters.Count-1 do
      if IsTyped(Parameters[t].TypeIdentifier) then
      begin
        result:=True;
        Exit;
      end;

  result:=False;
end;

procedure TMethodParams.CalcFixedParameters;
var t : Integer;
begin
  FixedParameters:=0;

  for t:=0 to Parameters.Count-1 do
      if Parameters[t].Value=nil then
         Inc(FixedParameters)
      else
         break;
end;

procedure TMethodParams.CheckConstantParameters(const AParams:TExpressions);

var t : Integer;
    L : Integer;
    tmp : TVariableDeclaration;
begin
  if VariableParameters or (Parameters=nil) then
     L:=0
  else
  begin
    L:=Length(AParams);

    if L>Parameters.Count then
       Error('Internal: Wrong method overload, too much parameters');
  end;

  for t:=0 to L-1 do
  begin
    tmp:=Parameters[t];

    if tmp is TParameter then
       if TParameter(tmp).IsVar or TParameter(tmp).IsOut then
          if (AParams[t] is TParameter) and TParameter(AParams[t]).IsConst then
             Error('Constant expression '+tmp.Name+
                   ' cannot be passed to var or out parameter: '+TParameter(AParams[t]).Name);
  end;
end;

function TMethodParams.HasParameters: Boolean;
begin
  result:=(Parameters<>nil) and (Parameters.Count>0);
end;

type
  TMethodParamsClass=class of TMethodParams;

function TMethodParams.Expand(const AParams: TTypeParameters): TTypeSpecification;
begin
  if AnyParamIsTyped then
  begin
    result:=TMethodParamsClass(ClassType).Create(Self);
    TMethodParams(result).ExpandParameters(Self,AParams);
  end
  else
    result:=Self;
end;

procedure TMethodParams.ExpandParameters(const AMethod: TMethodParams;
  const AParams: TTypeParameters);
var t : Integer;
begin
  FixedParameters:=AMethod.FixedParameters;
  VariableParameters:=AMethod.VariableParameters;

  if AMethod.Parameters<>nil then
  begin
    Parameters:=TParameters.Create(Self);

    for t:=0 to AMethod.Parameters.Count-1 do
        Parameters.Add(ExpandParameter(AMethod.Parameters[t],AParams));
  end;
end;

function TMethodParams.ExpandParameter(const AParameter:TVariableDeclaration;
  const AParams: TTypeParameters):TVariableDeclaration;
var tmpType : TTypeDeclaration;
begin
  result:=AParameter;

  if TTypeDeclaration.TryExpand(result.TypeIdentifier,tmpType,AParams) then
  begin
    result:=TVariableDeclaration.CreateType(Self,tmpType);
    result.Name:=AParameter.Name;
  end;
end;

function TMethodParams.Find(const ASymbol:TFindSymbol):TExpression;
var tmpIdentical : Boolean;
    tmp : TVariableDeclaration;
    tmpType : TTypeDeclaration;
begin
  result:=nil;

  if (ASymbol.Kind=skField) and (Parameters<>nil) then
  begin
    tmp:=Parameters.Find(ASymbol.Name);

    if tmp<>nil then
    begin
      tmpType:=tmp.TypeIdentifier;

      if (tmpType.Expression is TMethodSpecification) and
         (
          ASymbol.AsPointer
          or
          TMethodSpecification(tmpType.Expression).ValidParameters(ASymbol,tmpIdentical)
         ) then
             result:=tmp
      else
         result:=tmp;

    end;
  end;

  if result=nil then
  begin
    // Inverted priority, first TypeParameter, then inherited OfRecord
    if TypeParameters<>nil then
       result:=FindATypeParam(ASymbol.Name,TypeParameters);

    if result=nil then
       result:=inherited {$IFNDEF D9}Find(ASymbol){$ENDIF};
  end;
end;

function TMethodParams.ValidParameters(const ASymbol:TFindSymbol;
               out IdenticalTypes:Boolean):Boolean;
var
  tmpParams : TTypeParameters;

  function ValidParameter(const AValue:TExpression; AParam:TVariableDeclaration):Boolean;
  var tmp : Boolean;
  begin
    if AParam.TypeIdentifier=nil then
       Error('Internal: Missing Parameter type');

    if AParam.TypeIdentifier=AnyType then
       result:=True
    else
    begin
      if tmpParams<>nil then
         AParam:=ExpandParameter(AParam,tmpParams);

      result:=Parser.Language.CompatibleTypes(AParam.TypeIdentifier.Generic,AValue,tmp);

      if tmp then
         IdenticalTypes:=True;
    end;
  end;

  function InferTypes:TTypeParameters;
  var tmpIndex,
      tmp,
      t, L: Integer;
      tmpParam : TTypeDeclaration;
  begin
    L:=Length(TypeParameters);
    result:=nil;

    tmp:=0;

    for t:=Low(ASymbol.Params) to High(ASymbol.Params) do
    begin
      tmpParam:=Parameters[t].TypeIdentifier;

      if tmpParam is TTypeParameter then
      begin
        tmpIndex:=FindATypeParamIndex(TTypeParameter(tmpParam).Name,TypeParameters);
        //tmpIndex:=TTypeParameter(tmpParam).Index;

        if result=nil then
           SetLength(result,L);

        if result[tmpIndex]=nil then
        begin
          result[tmpIndex]:=ASymbol.Params[t].TypeIdentifier;

          Inc(tmp);

          if tmp=L then
             break;  // Speed opt
        end;
      end;
    end;
  end;

var t,
    L : Integer;
begin
  L:=Length(ASymbol.Params);

  if Parameters=nil then
  begin
    result:=L=0;
    IdenticalTypes:=result;
  end
  else
  begin
    result:=(L>=FixedParameters) and
            (VariableParameters or (L<=Parameters.Count));

    if result then
    begin
      IdenticalTypes:=False;

      if Parent.Language.CompilerVersion>=210 then
      begin
        tmpParams:=ASymbol.TypeParams;

        if tmpParams=nil then
          if ASymbol.Owner is TConstantDeclaration then
             tmpParams:=FindTypesIn(TConstantDeclaration(ASymbol.Owner).TypeIdentifier);

        if tmpParams=nil then
           if ASymbol.Parent is TTypeDeclaration then
              tmpParams:=FindTypesIn(TTypeDeclaration(ASymbol.Parent));

        if (tmpParams=nil) and (TypeParameters<>nil) then
           tmpParams:=InferTypes; // <-- Speed bottleneck
      end;

      if VariableParameters then
         if L>Parameters.Count then
            L:=Parameters.Count;

      for t:=Low(ASymbol.Params) to L-1 do
          if not ValidParameter(ASymbol.Params[t],Parameters[t]) then
          begin
            result:=False;
            Exit;
          end;
    end;
  end;
end;

{ TProcedureSpecification }

function TProcedureSpecification.ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean;
begin
  result:=(ASymbol.AResult=nil) and
          (inherited {$IFNDEF D9}ValidParameters(ASymbol,IdenticalTypes){$ENDIF});
end;

{ TFunctionSpecification }

function TFunctionSpecification.Expand(const AParams: TTypeParameters): TTypeSpecification;
var tmp : TVariableDeclaration;
    tmpTyped : Boolean;
begin
  tmpTyped:=AnyParamIsTyped;
  tmp:=ExpandParameter(ResultValue,AParams);

  if tmpTyped or (tmp<>ResultValue) then
  begin
    result:=TFunctionSpecification.Create(Self);
    TFunctionSpecification(result).ResultValue:=tmp;

    if tmpTyped then
       TFunctionSpecification(result).ExpandParameters(Self,AParams);
  end
  else
    result:=Self;
end;

class function TFunctionSpecification.FunctionOf(ABlock:TBlock):TFunctionSpecification;
begin
  repeat
    if ABlock is TFunctionSpecification then
    begin
      result:=TFunctionSpecification(ABlock);
      Exit;
    end
    else
       ABlock:=ABlock.Owner;

  until ABlock=nil;

  result:=nil;
end;

function TFunctionSpecification.Find(const ASymbol:TFindSymbol):TExpression;
begin
  if (ASymbol.Kind=skField) and (ResultValue<>nil) and SameText(ASymbol.Name,ResultValue.Name) then
     result:=ResultValue
  else
     result:=inherited {$IFNDEF D9}Find(ASymbol){$ENDIF};
end;

function TFunctionSpecification.ValidParameters(const ASymbol:TFindSymbol;
                             out IdenticalTypes:Boolean):Boolean;
var tmpExp : TExpression;
begin
  result:=inherited {$IFNDEF D9}ValidParameters(ASymbol,IdenticalTypes){$ENDIF};

  if result and (ASymbol.AResult<>nil) then
  begin
    tmpExp:=ResultValue;

    if not Parser.Language.CompatibleTypes(ASymbol.AResult,tmpExp,IdenticalTypes) then
       IdenticalTypes:=False;
  end;
end;

{ TUsesSection }

Constructor TUsesSection.Create(const AOwner:TBlock);
begin
  inherited;
  UsesUnits:=TUses.Create(Self);
end;

{ TLabelPlace }

function TExpression.ToString: String;
begin
  result:='? ('+ClassName+')';
end;

function TExpression.Value: String;
begin
  result:='';
end;

{ TNumber }

Constructor TNumber.Create(const AOwner:TBlock);
begin
  inherited;
  TypeIdentifier:=Int64Type;
end;

procedure TNumber.SetValue(const AValue:Int64);
var tmpType : TTypeDeclaration;
    tmp : Int64;
begin
  Value:=AValue;
  tmp:=Abs(AValue);

  if tmp>MaxLongint then
     tmpType:=Int64Type
  else
  if (tmp>65535) or (Value<0) then
     tmpType:=IntegerType
  else
  if AValue>255 then
     tmpType:=WordType
  else
     tmpType:=Byte_Type;

  // Pending: if AInteger<0 then tmpType:=U...Type unsigned !!

  TypeIdentifier:=tmpType;
end;

function TNumber.ToString: String;
begin
  if Hexadecimal then
     result:='$'+IntToHex(Value,0)
  else
     result:=IntToStr(Value);
end;

{ TFloatNumber }

Constructor TFloatNumber.Create(const AOwner:TBlock);
begin
  inherited;
  TypeIdentifier:=ExtendedType;
end;

procedure TFloatNumber.SetValue(const AValue:Extended);
 // From Math unit, to avoid using it
const
  MaxDouble   =  1.7976931348623157081e+308;
  MaxSingle   =  340282346638528859811704183484516925440.0;

var tmp : Extended;
begin
  Value:=AValue;
  tmp:=Abs(Value);

  if tmp>MaxDouble then
     TypeIdentifier:=ExtendedType
  else
  if tmp>MaxSingle then
     TypeIdentifier:=DoubleType
  else
     TypeIdentifier:=SingleType;
end;

function TFloatNumber.ToString: String;
begin
  result:=ValueString;
end;

{ TChar }

Constructor TChar.Create(const AOwner:TBlock);
begin
  inherited;
  TypeIdentifier:=CharType;
end;

function TChar.ToString:String;
begin
  result:=Value;
end;

{ TString }

Constructor TString.Create(const AOwner:TBlock);
begin
  inherited;
  TypeIdentifier:=StringType;
end;

function TString.ToString: String;
begin
  result:=Value;
end;

{ TBoolean }

Constructor TBoolean.Create(const AOwner:TBlock);
begin
  inherited;
  TypeIdentifier:=BooleanType;
end;

function TBoolean.ToString: String;
begin
  result:=BoolToStr(Value,True);
end;

{ TSetOfSpecification }

Constructor TSetOfSpecification.CreateType(const AOwner:TBlock; const AType:TTypeDeclaration);
begin
  Create(AOwner);
  TypeIdentifier:=AType;
end;

{ TSetRange }

Constructor TSetRange.CreateRange(const AOwner:TBlock; const ARange:TRangeExpression);
begin
  Create(AOwner);
  Range:=ARange;
end;

{ TCaseVariable }

constructor TCaseVariable.Create(const AOwner: TBlock);
begin
  inherited;
  Fields:=TRecordFieldsDeclarations.Create(Self);
  Fields.Sorted.Duplicates:=True; // <-- for unnamed case variables
end;

function TCaseVariable.Find(const AName: String): TVariableDeclaration;
var t : Integer;
    tmp : TBlock;
begin
  result:=nil;

  for t:=0 to Fields.Count-1 do
      if Fields.Get(t) is TCaseVariableItem then
      begin
        tmp:=TCaseVariableItem(Fields.Get(t)).Value;

        if tmp is TRecordFieldsDeclarations then
           result:=TRecordFieldsDeclarations(tmp).Find(AName)
        else
        if tmp is TVariableDeclaration then
           if SameText(TVariableDeclaration(tmp).Name,AName) then
              result:=TVariableDeclaration(tmp);

        if result<>nil then
           Exit;
      end;
end;

{ TBaseList }

// Adapted from Classes unit to avoid linking it

Constructor TBaseList.CreateStrings(const S:Array of String);
var t : Integer;
begin
  Count:=Length(S);
  FCapacity:=Count;

  SetLength(FList,Count);

  for t:=0 to Count-1 do
      FList[t].Name:=S[t];
end;

function TBaseList.Find(const S:String; out Index:Integer):Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Pred(Count);

  while L <= H do
  begin
    I := (L + H) shr 1;

    //C := AnsiCompareText(FList[I].Name, S);  D6=14sec

    if IgnoreCase then
       C := CompareStr(FList[I].Name,S)
    else
       C := CompareText(FList[I].Name, S);   //   D6=9sec

    if C < 0 then
       L := I + 1
    else
    begin
      H := I - 1;

      if C = 0 then
      begin
        Result := True;

        if not Duplicates then
        begin
          //L := I;
          Index := I;
          Exit;
        end;
      end;
    end;
  end;

  Index := L;
end;

function TBaseList.Get(const Index: Integer): TListItem;
begin
  result:=FList[Index];
end;

function TBaseList.Exists(const S:String):Boolean;
var Dummy : Integer;
begin
  result:=Find(S,Dummy);
end;

function TBaseList.AddItem(const AName:String; const AObject:TObject):Integer;

  procedure Grow;
  var NewCapacity,
      Delta: Integer;
  begin
    if FCapacity > 64 then
       Delta := FCapacity div 4
    else
    if FCapacity > 8 then
       Delta := 16
    else
       Delta := 4;

    NewCapacity:=FCapacity+Delta;

    if NewCapacity <> FCapacity then
    begin
      SetLength(FList,NewCapacity);
      FCapacity:=NewCapacity;
    end;
  end;

begin
  if Count=FCapacity then
     Grow;

  if Find(AName, result) then
     if not Duplicates then
        if AObject is TBlock then
           TBlock(AObject).Error('Duplicates not allowed. Adding: '+AName)
        else
           raise ECodeException.Create('Duplicates not allowed. Adding: '+AName);

  if result < Count then
     System.Move(FList[result],FList[result+1],(Count-result)*SizeOf(TListItem));

  Pointer(FList[result].Name) := nil;
  Pointer(FList[result].O) := nil;

  FList[result].Name:=AName;
  FList[result].O:=AObject;

  Inc(Count);
end;

procedure TBaseList.Clear;
begin
  Count:=0;
  FList:=nil;  // OwnsObjects ?
  FCapacity:=0;
end;

function TBaseList.Remove(const AObject:TObject):Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if FList[t].O=AObject then
      begin
        Dec(Count);

        result:=t;

        if t<Count then
           // FList.Delete
           System.Move(FList[t+1], FList[t], (Count-t) * SizeOf(TListItem));

        Exit;
      end;

  result:=-1;
end;

{ TIdentifiers }

constructor TIdentifiers.Create(const AOwner: TBlock);
begin
  inherited;
  Sorted:=TBaseList.Create;

  if (AOwner<>nil) and (AOwner.Parent<>nil) then
      Sorted.IgnoreCase:=AOwner.Parent.Language.CaseSentitive;
end;

destructor TIdentifiers.Destroy;
begin
  Sorted.Free;

  inherited;
end;

procedure TIdentifiers.AddSorted(const AIdentifier: TIdentifier);
begin
  Sorted.AddItem(AIdentifier.Name,AIdentifier);

  SetLength(Items,Sorted.FCapacity);
  Items[Count-1]:=AIdentifier;
end;

procedure TIdentifiers.Clear;
begin
  Sorted.Clear;
  Items:=nil;
end;

function TIdentifiers.Count: Integer;
var t : Integer;
begin
  // Reset Sorted list. Add Items.
  // This occurs when loading TIdentifiers from a stream.
  if (Items<>nil) and (Sorted.Count=0) then
     for t:=Low(Items) to High(Items) do
         Sorted.AddItem(Items[t].Name,Items[t]);

  result:=Sorted.Count;
end;

procedure TIdentifiers.Remove(const AIdentifier:TIdentifier);
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t]=AIdentifier then
      begin
        if t<Count then
           System.Move(Items[t+1], Items[t], (Count-t) * SizeOf(TIdentifier));

        SetLength(Items,Count-1);
        break;
      end;

  Sorted.Remove(AIdentifier);
end;

function TIdentifiers.SortedFind(const AName: String): TIdentifier;
var t : Integer;
begin
  if Sorted.Find(AName,t) then
     result:=TIdentifier(Sorted.FList[t].O)
  else
     result:=nil;
end;

function TIdentifiers.Get(Index:Integer):TIdentifier;
begin
  result:=Items[Index];
end;

function TIdentifiers.IndexOf(const AIdentifier: TIdentifier): Integer;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      if Items[t]=AIdentifier then
      begin
        result:=t;
        Exit;
      end;

  result:=-1;
end;

{ TInterfaceSpecification }

function TInterfaceSpecification.IsCompatible(ASpec: TTypeSpecification): Boolean;
begin
  result:=inherited {$IFNDEF D12}IsCompatible(ASpec){$ENDIF};

  if not result then
  begin
    if ASpec is TClassOf then
       ASpec:=TClassOf(ASpec).TypeIdentifier.Expression;

    if ASpec is TClassSpecification then
       result:=TClassSpecification(ASpec).SupportsInterface(Self)
    else
    if ASpec is TInterfaceSpecification then
       result:=TInterfaceSpecification(ASpec).IsMeOrAncestor(Self);

    if not result then
       result:=ASpec.Parser.PointerMath and
               (
                 (ASpec is TTypePointerOf) or IsPointer(ASpec)
               );
  end;
end;

function TInterfaceSpecification.IsMeOrAncestor(const AInterface:TInterfaceSpecification):Boolean;
var tmp : TTypeSpecification;
begin
  result:=Self=AInterface;

  if (not result) and (Ancestors<>nil) then
  begin
    tmp:=Ancestors[0].Expression;

    if tmp is TInterfaceSpecification then
       result:=TInterfaceSpecification(tmp).IsMeOrAncestor(AInterface);
  end;
end;

{ TAddressOf }

Constructor TAddressOf.CreateValue(const AOwner: TBlock; const AValue: TExpression);
begin
  inherited Create(AOwner);

  Value:=AValue;
  TypeIdentifier:=PointerType;

  if Value=nil then
     Error('Address of expression not found');
end;

{ TScopedType }

constructor TScopedType.CreateScope(const AOwner: TBlock; const AField: TFieldExpression);
begin
  inherited Create(AOwner);
  Field:=AField;
end;

function TScopedType.Scope: TExpression;
begin
  result:=Field.Value;
end;

procedure TScopedType.SetField(const Value: TFieldExpression);
begin
  FField:=Value;
  TypeIdentifier:=FField.Field as TTypeDeclaration;
  Expression:=TypeIdentifier.Expression;
  Alias:=TypeIdentifier.Alias;
end;

{ TGenericMethod }

Constructor TGenericMethod.CreateMethod(const AOwner: TBlock;
  const AMethod: TMethodDeclaration; const AParams: TTypeParameters);
begin
  inherited Create(AOwner);
  Method:=AMethod;
  TypeIdentifier:=Method.TypeIdentifier;
  TypeParameters:=AParams;
end;

{ TGenericTypeDeclaration }

constructor TGenericTypeDeclaration.CreateTyped(const AOwner: TBlock;
  const AType: TTypeDeclaration; const AParams: TTypeParameters);
begin
  CreateSpec(AOwner,AType.Expression);
  Name:=AType.Name;
  TypeParameters:=AParams;
end;

{ TReferenceSpecification }

function TReferenceSpecification.Expand(const AParams: TTypeParameters): TTypeSpecification;
begin
  result:=TReferenceSpecification.Create(Self);
  TReferenceSpecification(result).Method:=TMethodSpecification(Method.Expand(AParams));
end;

{ TDirective }

constructor TDirective.CreateKind(const AOwner: TBlock; const AKind: TDirectiveKind;
  const AText: String);
begin
  Create(AOwner);
  Kind:=AKind;
  Text:=AText;
end;

{ TLanguage }

procedure TLanguage.AddParsedLines(const AModule: TModule;
  const ALines: Integer);
begin
  Inc(ParsedLines,ALines);

  if Assigned(FOnParsing) then
     FOnParsing(AModule);
end;

procedure TLanguage.Clear;
begin
  ParsedBlocks:=0;
  ParsedLines:=0;

  Modules.Clear;
end;

constructor TLanguage.Create;
begin
  inherited {Create};

  SelfKeyword:='Self';

  Modules:=TModules.Create(nil);
  Modules.Language:=Self;

  ModuleClass:=TImplementationModule;

  CurrentPackage:=Modules;
end;

destructor TLanguage.Destroy;
begin
  Modules.Free;
  inherited;
end;

function TLanguage.FindModule(const AName: String): TModule;
begin
  result:=Modules.PackageContains.Find(AName);
end;

function RemoveFileExtension(const AFileName:String):String;
var tmpExt : String;
    L : Integer;
begin
  result:=AFileName;
  tmpExt:=ExtractFileExt(result);

  if tmpExt<>'' then
  begin
    L:=Length(tmpExt);
    Delete(result,Length(result)-L+1,L);
  end;
end;

function TLanguage.GetPackage(const AOwner:TBlock; const AName: String): TPackage;
var Old : TPackage;
    tmpName : String;
begin
  tmpName:=AName;

  if ExtractFileExt(tmpName)='' then
     tmpName:=tmpName+'.dpk';

  result:=Modules.PackageRequires.Find(RemoveFileExtension(tmpName));

  if result=nil then
  begin
    result:=TPackage.CreateName(AOwner,tmpName);
    result.Language:=Self;

    // if FileExist(...) then
    //    TCodeReader.ReadFromFile(result.Units,AName+IntToStr(FIDE.CompilerVersion))
    // else:

    Old:=CurrentPackage;
    CurrentPackage:=result;
    try
      DoParse(result,tmpName);

      Modules.AddRequired(result);

      if Assigned(FOnParsing) then
         FOnParsing(result);

    finally
      CurrentPackage:=Old;
    end;
  end;
end;

procedure TLanguage.ParseModule(const AModule:TModule; const AFile:String);
var Old : TModule;
begin
  Old:=CurrentModule;
  CurrentModule:=AModule;
  try
    InternalParseModule(ParserFromFile(AModule,AFile),AModule);
  finally
    CurrentModule:=Old;
  end;
end;

{ TPackageContains }

procedure TPackageContains.ParseModule(const AName, AFileName: String);
var tmp : TModule;
begin
  if (Module=nil) or (TModule(Module).Package=nil) or
     (TModule(Module).Package=Parent.Language.Modules) then
     inherited
  else
  begin
    tmp:=TModule(Module).Package.FindRecursive(AName);

    if (tmp=nil) or (TModule(tmp.Module).Package=TModule(Module).Package) then
       inherited
    else
       Error('Module '+AName+' already in package: '+TModule(tmp.Module).Package.Name);
  end;
end;

{ TEmit }

Constructor TEmit.Create;
begin
  inherited;

  CRLF:=#13#10;
  IndentText:='  ';
  LessThan:='<';
  GreaterThan:='>';
end;

function TEmit.AnchorOf(const S: String): String;
begin
  result:=S;
end;

function TEmit.Keyword(const S: String): String;
begin
  result:=S;
end;

function TEmit.LinkTo(const ABlock: TBlock; const S: String): String;
begin
  result:=S;
end;

function TEmit.StringToCode(const S: String): String;
var t, L : Integer;
    InString : Boolean;
begin
  result:='';

  L:=Length(S);

  if L>0 then
  begin
    t:=1;

    InString:=False;

    repeat
      if Ord(S[t])<32 then
      begin
        if InString then
        begin
          result:=result+Quote;
          InString:=False;
        end;

        result:='#'+IntToStr(Ord(S[t]));
      end
      else
      begin
        if not InString then
        begin
          result:=result+Quote;
          InString:=True;
        end;

        result:=result+S[t];
      end;

      Inc(t);
    until t>L;

    if InString then
       result:=result+Quote;

  end
  else
    result:=Quote+Quote;
end;

function TEmit.UnitNameLink(const S: String): String;
begin
  result:=S;
end;

class function TEmit.AsString(const ABlock: TBlock): String;
begin
  with Create do
  try
    result:=Block(ABlock);
  finally
    Free;
  end;
end;

class function TEmit.AsString(const AItems: TExpressions): String;
begin
  with Create do
  try
    result:=Expressions(AItems);
  finally
    Free;
  end;
end;

{ TStatements }

procedure TStatements.Add(const AStatement: TStatement);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=AStatement;
end;

{ TIntrinsics }

function TIntrinsics.GlobalMethod(const AClass:TMethodSpecificationClass):TMethodDeclaration;
begin
  result:=TMethodDeclaration.Create(Globals);
  result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,AClass.Create(result));
end;

function TIntrinsics.GlobalFunction(const AName:String; const AResult:TVariableDeclaration):TMethodDeclaration;
begin
  result:=GlobalMethod(TFunctionSpecification);
  result.Name:=AName;
  TFunctionSpecification(result.TypeIdentifier.Expression).ResultValue:=AResult;
end;

function TIntrinsics.GlobalProcedure(const AName:String):TMethodDeclaration;
begin
  result:=GlobalMethod(TProcedureSpecification);
  result.Name:=AName;
end;

function TIntrinsics.AddParam(const AMethod:TMethodDeclaration; const AType:TTypeDeclaration;
                  const AName:String):TParameter;
var tmp : TMethodParams;
begin
  tmp:=AMethod.TypeIdentifier.Expression as TMethodParams;

  result:=TParameter.CreateType(tmp,AType);
  result.Name:=AName;

  if tmp.Parameters=nil then
     tmp.Parameters:=TParameters.Create(tmp);

  tmp.Parameters.Add(result);
end;

function TIntrinsics.ResultOf(const AType:TTypeSpecification):TVariableDeclaration;
begin
  result:=TVariableDeclaration.CreateType(Globals,TTypeDeclaration.CreateSpec(Globals,AType));
end;

function TIntrinsics.ResultOf(const AType:TTypeDeclaration):TVariableDeclaration;
begin
  result:=TVariableDeclaration.CreateType(Globals,AType);
end;

function TIntrinsics.GlobalType(const ASpec:TTypeSpecification):TTypeDeclaration;
begin
  result:=TTypeDeclaration.CreateSpec(Globals,ASpec);
end;

function TIntrinsics.GlobalType(const AName:String):TTypeDeclaration;
begin
  result:=TTypeDeclaration.Create(Globals);
  result.Name:=AName;
end;

type
  TIdentifiersAccess = class(TIdentifiers);

function TIntrinsics.AddGlobalType(const AName:String; const AExpression:TTypeSpecification=nil):TTypeDeclaration;
begin
  result:=GlobalType(AName);
  result.Expression:=AExpression;
  TIdentifiersAccess(Globals.Types).AddSorted(result);
end;

function TIntrinsics.AddGlobalPointerType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
var tmp : TTypePointerOf;
begin
  result:=GlobalType(AName);

  tmp:=TTypePointerOf.Create(Globals);
  tmp.TypeIdentifier:=AType;
  tmp.TypeIdentifier.Expression:=AType.Expression;

  result.Expression:=tmp;

  TIdentifiersAccess(Globals.Types).AddSorted(result);
end;

function TIntrinsics.AddGlobalAliasType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
begin
  result:=GlobalType(AName);

  result.Alias:=AType;
  result.Expression:=AType.Expression;

  TIdentifiersAccess(Globals.Types).AddSorted(result);
end;

end.
