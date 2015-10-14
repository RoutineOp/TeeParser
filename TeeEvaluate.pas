{***********************************}
{ Expression evaluator              }
{ Copyright 2014 by Steema Software }
{ www.steema.com david@steema.com   }
{***********************************}
unit TeeEvaluate;

// Notes:
//
//   Sets arent supported (for example the "in" operator). Pointers not supported.
//   Operator precedence is currently applied to left part of the expression only.

interface

uses
  SysUtils;

{$DEFINE USEMATH}

type
  EEvaluator=class(Exception);

  TBaseNode=class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    Parent : TObject;
  public
    Tag : TObject;
    Text : String;

    procedure Validate; virtual; abstract;

    function Value:Variant; virtual; abstract;
  end;

  TValueNode=class(TBaseNode)
  private
    CustomValue : Boolean;
    CachedValue : Variant;
  protected
    Evaluated : Boolean;

    procedure TryCache;
  public
    procedure Validate; override;
    function Value:Variant; override;
  end;

  TOperator=(opEqual, opNotEqual,
             opAnd, opOr, opAndNot, opOrNot, opXor,
             opShl, opShr,
             opLower, opGreater, opLowerEqual, opGreaterEqual,
             opAdd, opSubtract, opMultiply, opDivide,
             opUnaryNot, opDiv, opMod
             {$IFDEF USEMATH}
             , opPower
             {$ENDIF}
             );

  TExpressionNode=class(TBaseNode)
  public
    Left,
    Right : TBaseNode;

    Operat : TOperator;

    Destructor Destroy; override;

    class function OperatorToString(const AOperator:TOperator):String;

    procedure Validate; override;
    function Value:Variant; override;
  end;

  TBaseNodes=Array of TBaseNode;

  TParameters=TBaseNodes;

  TFunction=class(TValueNode)
  private
    CachedFunction : Integer;

    procedure TryCache;
  public
    Name : String;
    Parameters : TParameters;

    Destructor Destroy; override;

    procedure Validate; override;
    function Value:Variant; override;
  end;

  TArrayItems=TBaseNodes;

  TArrayNode=class(TBaseNode)
  public
    Items : TArrayItems;

    Destructor Destroy; override;

    procedure Validate; override;
    function Value:Variant; override;
  end;

  TGetEvent=procedure(Sender:TObject; const AIdentifier:String; out AValue:String) of object;
  TGetTagEvent=procedure(Sender:TObject; const AIdentifier:String; out AValue:TObject) of object;

  TCallEvent=procedure(Sender:TObject; const AFunction:TFunction; out AValue:Variant) of object;
  TParseErrorEvent=procedure(Sender:TObject; const AMessage,AExpression:String; APosition:Integer) of object;

  TEvaluator=class
  private
    FOnCall  : TCallEvent;
    FOnGetCall : TGetTagEvent;

    FOnError : TParseErrorEvent;

    FOnGet   : TGetEvent;
    FOnGetTag: TGetTagEvent;

    ICurrentExp : String;
    IParentPos,
    IPos : Integer;

    procedure Error(const AMessage:String);
    function InternalParse(const AExpression:String):TBaseNode;
    function StringToOperator(S:String; out AOperator:TOperator):Boolean;
  protected
    function Call(const AFunction:TFunction):Variant;
    function Get(const AIdentifier:String):String;
    function GetTag(const AIdentifier:String):TObject;
    function WhichFunction(const AName:String):Integer;
  public
    Line : Integer;
    FileName : String;

    class function AsString(const ARoot:TBaseNode):String; overload;
    function Evaluate(const AExpression:String):Variant;
    function Parse(const AExpression:String):TBaseNode;

    property OnCall:TCallEvent read FOnCall write FOnCall;
    property OnGet:TGetEvent read FOnGet write FOnGet;

    property OnGetCall:TGetTagEvent read FOnGetCall write FOnGetCall;
    property OnGetTag:TGetTagEvent read FOnGetTag write FOnGetTag;

    property OnParseError:TParseErrorEvent read FOnError write FOnError;
  end;

implementation

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
     {$DEFINE HAS_CHARINSET}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF USEMATH}
  Math,
  {$ENDIF}
  Variants, Classes;

const
  EvaluatorMsg_UnknownOperator = 'Unknown operator: %s';
  EvaluatorMsg_WrongNotOperator = 'Wrong "NOT" operator.';
  EvaluatorMsg_IdentifierNotFound = 'Identifier not found: %s';
  EvaluatorMsg_FunctionNotFound = 'Function not found: %s';

{ TValueNode }

procedure TValueNode.TryCache;

  // Returns True when the first and last characters are simple (single) quotes: '...'
  function IsString:Boolean;
  begin
    result:=(Copy(Text,1,1)='''') and (Copy(Text,Length(Text),1)='''');
  end;

  function IsChars:Boolean;
  begin
    result:=Copy(Text,1,1)='#';
  end;

var tmpFloat : Extended;
    tmpInt : Int64;
    tmpBool : Boolean;
begin
  CustomValue:=False;

  if SameText(Text,'PI') then
     CachedValue:=Pi
  else
  if TryStrToInt64(Text,tmpInt) then
     CachedValue:=tmpInt
  else
  if TryStrToFloat(Text,tmpFloat) then
     CachedValue:=tmpFloat
  else
  if IsString then
     CachedValue:=Copy(Text,2,Length(Text)-2) // Remove quotes
  else
  if IsChars then
     CachedValue:=Text
  else
  if TryStrToBool(Text,tmpBool) then
     CachedValue:=tmpBool
  else
     CustomValue:=True;

  Evaluated:=True;
end;

procedure TValueNode.Validate;
begin
  if not Evaluated then
     TryCache;

  if CustomValue then
     Tag:=TEvaluator(Parent).GetTag(Text);
end;

function TValueNode.Value:Variant;
begin
  if not Evaluated then
     TryCache;

  if CustomValue then
     result:=TEvaluator(Parent).Get(Text)
  else
     result:=CachedValue;
end;

{ TExpressionNode }

Destructor TExpressionNode.Destroy;
begin
  Left.Free;
  Right.Free;
  inherited;
end;

class function TExpressionNode.OperatorToString(const AOperator: TOperator): String;
begin
  case AOperator of
     opEqual: result:='=';
  opNotEqual: result:='<>';
       opAnd: result:='and';
        opOr: result:='or';
    opAndNot: result:='and not';
     opOrNot: result:='or not';
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
  opUnaryNot: result:='not';
       opDiv: result:='div';
       opMod: result:='mod';
  {$IFDEF USEMATH}
     opPower: result:='^';
  {$ENDIF}
  else
     result:='>=';
  end;
end;

procedure TExpressionNode.Validate;
begin
  Left.Validate;

  if Right<>nil then
     Right.Validate;
end;

function TExpressionNode.Value:Variant;

  // 8x performance speed achieved by moving this "case" here in a nested
  // sub-procedure instead of leaving it at caller.
  procedure CalcOperator;
  begin
    case Operat of
       opEqual: result:=Left.Value=Right.Value;
    opNotEqual: result:=Left.Value<>Right.Value;
         opAnd: result:=Left.Value and Right.Value;
          opOr: result:=Left.Value or Right.Value;
      opAndNot: result:=Left.Value and not Right.Value;
       opOrNot: result:=Left.Value or not Right.Value;
         opXor: result:=Left.Value xor Right.Value;
         opShl: result:=Left.Value shl Right.Value;
         opShr: result:=Left.Value shr Right.Value;
       opLower: result:=Left.Value<Right.Value;
     opGreater: result:=Left.Value>Right.Value;
  opLowerEqual: result:=Left.Value<=Right.Value;

         opAdd: result:=Left.Value+Right.Value;
    opSubtract: result:=Left.Value-Right.Value;
    opMultiply: result:=Left.Value*Right.Value;
      opDivide: result:=Left.Value/Right.Value;
      {$IFDEF USEMATH}
       opPower: result:=Power(Left.Value,Right.Value);
      {$ENDIF}
         opDiv: result:=Left.Value div Right.Value;
         opMod: result:=Left.Value mod Right.Value;
     else
       result:=Left.Value>=Right.Value; // opGreaterEqual
    end;
  end;

begin
  if Right=nil then
     if Operat=opUnaryNot then
        result:=not Left.Value
     else
     if Operat=opSubtract then
        result:=-Left.Value
     else
        result:=Left.Value // <-- Error !
  else
    CalcOperator;
end;

{ TFunction }

destructor TFunction.Destroy;
var t : Integer;
begin
  for t:=Low(Parameters) to High(Parameters) do
      Parameters[t].Free;

  inherited;
end;

procedure TFunction.TryCache;
begin
  if not Evaluated then
  begin
    CachedFunction:=TEvaluator(Parent).WhichFunction(Name);
    Evaluated:=True;
  end;
end;

procedure TFunction.Validate;
var t : Integer;
begin
  TryCache;

  if CachedFunction=-1 then
  begin
    if Tag=nil then
    begin
      if Assigned(TEvaluator(Parent).FOnGetCall) then
         TEvaluator(Parent).FOnGetCall(Parent,Name,Tag);
    end;

  end;

  for t:=Low(Parameters) to High(Parameters) do
      Parameters[t].Validate;
end;

function TFunction.Value:Variant;

  // Default preset functions
  function InternalFunction:Variant;
  var AParam : Variant;
      tmp : Integer;
  begin
    if CachedFunction=17 then
       tmp:=2
    else
       tmp:=1;

    if Length(Parameters)<>tmp then
       TEvaluator(Parent).Error('Wrong number of parameters for function: '+Name+'. Expected: '+IntToStr(tmp));

    AParam:=Parameters[0].Value;

    case CachedFunction of
      0: Result:=Abs(AParam);
      1: Result:=Cos(AParam);
      2: Result:=Exp(AParam);
      3: Result:=LN(AParam);

      4: Result:=Log10(AParam);

      5: Result:=Sin(AParam);
      6: Result:=Sqr(AParam);
      7: Result:=Sqrt(AParam);

      8: Result:=Tan(AParam);

      9: Result:=Round(AParam);
     10: Result:=Trunc(AParam);

     11: Result:=Floor(AParam);
     12: Result:=Ceil(AParam);

     13: Result:=Length(AParam);
     14: Result:=LowerCase(AParam);
     15: Result:=Trim(AParam);
     16: Result:=UpperCase(AParam);

     17: Result:=Power(AParam,Parameters[1].Value);
    end;
  end;

begin
  TryCache;

  if CachedFunction=-1 then
     result:=TEvaluator(Parent).Call(Self)
  else
     result:=InternalFunction;
end;

{ TArrayNode }

destructor TArrayNode.Destroy;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      Items[t].Free;

  inherited;
end;

procedure TArrayNode.Validate;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      Items[t].Validate;
end;

function TArrayNode.Value:Variant;
var tmp : Array of Variant;
    t, L : Integer;
begin
  L:=Length(Items);

  if L=0 then
     result:=Null
  else
  begin
    SetLength(tmp,L);

    for t:=Low(Items) to High(Items) do
        tmp[t]:=Items[t].Value;

    result:=VarArrayOf(tmp);
  end;
end;

type
  TSortedOperators=class(TStringList)
  public
    constructor Create;
  end;

var
  SortedOp : TSortedOperators;

{ TEvaluator }

function TEvaluator.StringToOperator(S:String; out AOperator:TOperator):Boolean;
var IPos : Integer;
    tmp : Integer;
begin
  S:=Trim(S);

  // Delete middle spaces, in case there are more than one consecutive
  IPos:=Pos(' ',S);

  if IPos>0 then
  begin
    while IPos<Length(S) do
      if S[IPos+1]=' ' then
         Delete(S,IPos+1,1)
      else
         break;
  end;

  result:=SortedOp.Find(S,tmp);

  if result then
     AOperator:=TOperator(SortedOp.Objects[tmp]);
end;

function TEvaluator.Evaluate(const AExpression: String): Variant;
var Root : TBaseNode;
begin
  Root:=Parse(AExpression);
  try
    result:=Root.Value;
  finally
    Root.Free;
  end;
end;

function TEvaluator.InternalParse(const AExpression: String): TBaseNode;

  // Search for an operator in S, and split and return the left and right parts
  procedure Split(S:String; out ALeft,ARight:String; out AOp:TOperator;
                  out IsFunction,OperatorOk,IsArray:Boolean);
  var tmp : String;
      L : Integer;

    procedure GetPart;
    var tmpValue : {$IFDEF NEXTGEN}Double{$ELSE}Extended{$ENDIF};
        tmpOp : TOperator;
    begin
      if tmp<>'' then
      begin
        if ALeft='' then
        begin
          if UpperCase(tmp)='NOT' then
          begin
            AOp:=opUnaryNot;
            OperatorOk:=True;
          end
          else
          if (Copy(tmp,1,1)='-') and (not OperatorOk) and (not TryStrToFloat(tmp,tmpValue)) then
          begin
            AOp:=opSubtract;
            OperatorOk:=True;
            ALeft:=Copy(tmp,2,Length(tmp));
          end
          else
          if (Copy(tmp,1,1)='+') and (not OperatorOk) and (not TryStrToFloat(tmp,tmpValue)) then
          begin
            AOp:=opAdd;
            OperatorOk:=True;
            ALeft:=Copy(tmp,2,Length(tmp));
          end
          else
             ALeft:=tmp;
        end
        else
        if OperatorOk then
        begin
          if UpperCase(tmp)='NOT' then
          begin
            if AOp=opAnd then
               AOp:=opAndNot
            else
            if AOp=opOr then
               AOp:=opOrNot
            else
               ARight:=ARight+' '+tmp;
          end
          else
          if StringToOperator(tmp,tmpOp) and (AOp=opUnaryNot) then
          begin
            ALeft:='not '+ALeft;
            AOp:=tmpOp;
          end
          else
             ARight:=ARight+' '+tmp;
        end
        else
        begin
          if StringToOperator(tmp,AOp) then
             OperatorOk:=True
          else
             ALeft:=ALeft+tmp;
        end;

        tmp:='';
      end;

      while (IPos<L) and (S[IPos+1]=' ') do
        Inc(IPos);
    end;

    function NextCharIs(C:Char):Boolean;
    begin
      if IPos<L then
         result:=S[IPos+1]=C
      else
         result:=False;
    end;

    function IsNumberE(const S:String):Boolean;
    var tmp : Extended;
        L : Integer;
    begin
      L:=Length(S);

      result:=(L>0) and (UpperCase(Copy(S,L,1))='E') and
               TryStrToFloat(Copy(S,1,L-1),tmp);
    end;

    function ParentCount(const S:String):Integer;
    var t : Integer;
        C : Char;
    begin
      result:=0;

      for t:=1 to Length(S) do
      begin
       C:=S[t];

       if C='(' then
          Inc(result)
       else
       if C=')' then
       begin
         Dec(result);

         if result<0 then
            Exit;
       end;
      end;
    end;

  var C : Char;
      Bracket,
      Parent : Integer;
      InString : Boolean;
      tmpS : String;
  begin
    S:=Trim(S);

    while (Copy(S,1,1)='(') and
          (ParentCount(Copy(S,2,Length(S)-2))=0) do
          S:=Trim(Copy(S,2,Length(S)-2));

    ALeft:='';
    ARight:='';

    IsArray:=False;
    IsFunction:=False;
    OperatorOk:=False;

    InString:=False;
    Parent:=0;
    Bracket:=0;

    tmp:='';
    IPos:=1;
    L:=Length(S);

    while IPos<=L do
    begin
      C:=S[IPos];

      if InString then
      begin
        tmpS:=tmpS+C;

        if C='''' then
        begin
          InString:=False;

          tmp:=tmp+tmpS
        end;
      end
      else
      if C='(' then
      begin
        if SameText(tmp,'NOT') then
           GetPart;

        tmp:=tmp+C;
        Inc(Parent);
      end
      else
      if C=')' then
      begin
        if Parent>0 then
        begin
          Dec(Parent);

          if Parent=0 then
          begin
            if (AOp<>opUnaryNot) or (ALeft='') then
               tmp:=tmp+C;

            if ALeft='' then
               ALeft:=tmp
            else
               ARight:=tmp;

            tmp:='';
          end
          else
            tmp:=tmp+C;
        end;
      end
      else
      if C='[' then
      begin
        Inc(Bracket);

        IsArray:=True;

        if tmp<>'' then
        begin
          if ALeft='' then
          begin
            ALeft:=tmp;
            tmp:='';
          end;

          tmp:=tmp+C;
        end;
      end
      else
      if C=']' then
      begin
        Dec(Bracket);

        if Bracket=0 then
        begin
          if Pos('[',tmp)>0 then
             tmp:=tmp+C;

          if ALeft='' then
             ALeft:=tmp
          else
             ARight:=tmp;
        end
        else
        if Copy(tmp,1,1)<>'[' then
           tmp:=tmp+C;
      end
      else
      if C='''' then
      begin
        InString:=True;
        tmpS:=C;
      end
      else
        if Parent=0 then
        begin
          if C=' ' then
          begin
            GetPart;
          end
          else
          if C='=' then
          begin
            GetPart;
            tmp:=C;
            GetPart;
          end
          else
          if C='>' then
          begin
            GetPart;
            tmp:=C;

            if NextCharIs('=') then
            begin
              tmp:=tmp+'=';
              Inc(IPos);
            end;

            GetPart;
          end
          else
          if C='<' then
          begin
            GetPart;
            tmp:=C;

            if NextCharIs('=') then
            begin
              tmp:=tmp+'=';
              Inc(IPos);
            end
            else
            if NextCharIs('>') then
            begin
              tmp:=tmp+'>';
              Inc(IPos);
            end;

            GetPart;
          end
          else
          if (C='+') or (C='-') or (C='*') or (C='/') or (C='^') then
          begin
            if not OperatorOk then
            begin
              if ((C='+') or (C='-')) and (ALeft='') and (tmp='') then
                 tmp:=tmp+C
              else
              begin
                if IsNumberE(tmp) then
                   tmp:=tmp+C
                else
                begin
                  GetPart;
                  tmp:=C;
                  GetPart;
                end;
              end;
            end
            else
              tmp:=tmp+C;
          end
          else
            tmp:=tmp+C;
        end
        else
          tmp:=tmp+C;

      Inc(IPos);
    end;

    if (not IsArray) and (tmp<>'') then
       GetPart;

    // Simple function calls:  Foo(Bar)
    if not OperatorOk then
    begin
      IPos:=Pos('(',ALeft);

      if IPos>1 then
      begin
        if Copy(ALeft,Length(ALeft),1)=')' then
        begin
          IsFunction:=True;

          ARight:=Copy(ALeft,IPos+1,Length(ALeft)-IPos-1);
          ALeft:=Copy(ALeft,1,IPos-1);
        end;
      end
      else
      begin
        ARight:=Trim(ARight);

        if Copy(ARight,1,1)='(' then
        begin
          if Copy(ARight,Length(ARight),1)=')' then
          begin
            IsFunction:=True;

            ARight:=Copy(ARight,2,Length(ARight)-2);
          end;
        end;
      end;
    end;
  end;

  function NextTokenIsNumber(const S:String; AStart:Integer):Boolean;
  const
    Digits=['0'..'9'];
    Prefixes=['$','-','+'];

  var L,
      i : Integer;
      C : Char;
  begin
    result:=False;
    L:=Length(S);

    i:=AStart;

    while i<=L do
    begin
      C:=S[AStart];

      if not ({$IFDEF HAS_CHARINSET}CharInSet(C,Digits){$ELSE}C in Digits{$ENDIF}) then
      begin
        if ({$IFDEF HAS_CHARINSET}CharInSet(C,Prefixes){$ELSE}C in Prefixes{$ENDIF}) and
           (i<>AStart) then
           Exit
        else
        if i>AStart then
        begin
          result:=True;
          Exit;
        end;
      end;

      Inc(i);
    end;
  end;

  function ParseItems(S:String):TBaseNodes;
  var L : Integer;

    procedure Add(const S:String);
    begin
      SetLength(result,L+1);
      result[L]:=InternalParse(S);
      Inc(L);
    end;

  var i : Integer;
  begin
    result:=nil;
    L:=0;

    repeat
      if Copy(S,1,1)='[' then
      begin
        i:=Pos(']',S);

        if i>0 then
        begin
          Add(Copy(S,1,i));
          Delete(S,1,i);

          S:=Trim(S);
          if Copy(S,1,1)=',' then
             Delete(S,1,1);
        end
        else
          Error('Missing matching bracket.');
      end
      else
      begin
        i:=Pos(',',S);

        if i>0 then
        begin
          Add(Copy(S,1,i-1));
          Delete(S,1,i);
        end;
      end;
    until i=0;

    if S<>'' then
       Add(S);
  end;

var SLeft,
    SRight : String;

    Operat : TOperator;

    S : String;
    OperatorOk,
    tmpArray,
    tmpFunction : Boolean;
begin
  S:=Trim(AExpression);

  // Special case for unary minus
  if Copy(S,1,1)='-' then
     if not NextTokenIsNumber(S,2) then
     begin
       result:=TExpressionNode.Create; // <-- Future: TNegationCode
       result.Parent:=Self;
       result.Text:=AExpression;

       TExpressionNode(result).Operat:=opSubtract;
       TExpressionNode(result).Left:=InternalParse(Copy(S,2,Length(S)));

       Exit;
     end;

  Split(S,SLeft,SRight,Operat,tmpFunction,OperatorOk,tmpArray);

  if SLeft='' then
     result:=nil
  else
  begin
    if tmpFunction then
    begin
      result:=TFunction.Create;
      TFunction(result).Name:=SLeft;

      TFunction(result).Parameters:=ParseItems(SRight);
    end
    else
    if tmpArray then
    begin
      result:=TArrayNode.Create;

      if SLeft='' then
         TArrayNode(result).Items:=ParseItems(SLeft)
      else
      begin
         result.Text:=SLeft;
         TArrayNode(result).Items:=ParseItems(SRight);
      end;
    end
    else
    begin
      if OperatorOk then
      begin
        IParentPos:=IPos;

        result:=TExpressionNode.Create;
        TExpressionNode(result).Operat:=Operat;
        TExpressionNode(result).Left:=InternalParse(SLeft);

        if SRight<>'' then
           TExpressionNode(result).Right:=InternalParse(SRight);
      end
      else
        result:=TValueNode.Create;
    end;

    result.Parent:=Self;
    result.Text:=S;
  end;
end;

function TEvaluator.Parse(const AExpression: String): TBaseNode;
begin
  ICurrentExp:=AExpression;
  IParentPos:=0;

  result:=InternalParse(AExpression);
end;

const
  Functions:Array[0..17] of String=(
    'ABS',
    'COS',
    'EXP',
    'LN',
    'LOG10',
    'SIN',
    'SQR',
    'SQRT',
    'TAN',
    'ROUND',
    'TRUNC',
    'FLOOR',
    'CEIL',
    'LENGTH',
    'LOWERCASE',
    'TRIM',
    'UPPERCASE',
    'POWER'
  );

function TEvaluator.WhichFunction(const AName:String):Integer;
var t : Integer;
    S : String;
begin
  S:=UpperCase(Trim(AName));

  for t:=Low(Functions) to High(Functions) do
    if Functions[t]=S then
    begin
      result:=t;
      Exit;
    end;

  result:=-1;
end;

// Callback to obtain a custom function result
function TEvaluator.Call(const AFunction:TFunction):Variant;
begin
  result:=Unassigned;

  if Assigned(FOnCall) then
     FOnCall(Self,AFunction,result);

  if VarIsEmpty(result) then
     Error(Format(EvaluatorMsg_FunctionNotFound,[AFunction.Name]));
end;

// Callback to obtain a tag object for an Identifier
function TEvaluator.GetTag(const AIdentifier:String):TObject;
begin
  if Assigned(FOnGetTag) then
     FOnGetTag(Self,AIdentifier,result)
  else
     result:=nil;

//  if result=nil then
//     Error(Format(EvaluatorMsg_IdentifierNotFound,[AIdentifier]));
end;

// Callback to obtain an identifier
function TEvaluator.Get(const AIdentifier:String):String;
begin
  result:='';

  if Assigned(FOnGet) then
     FOnGet(Self,AIdentifier,result);

  //if result='' then
  //   Error(Format(EvaluatorMsg_IdentifierNotFound,[AIdentifier]));
end;

procedure TEvaluator.Error(const AMessage:String);
begin
  if Assigned(FOnError) then
  begin
    FOnError(Self,AMessage,ICurrentExp,IParentPos+IPos);
    Abort;
  end
  else
     raise EEvaluator.Create(AMessage+' '+#13+
                 'Expression: '+ICurrentExp+#13+
                 'Position: '+IntToStr(IParentPos+IPos)+#13+
                 'File: '+FileName+#13+
                 'Line: '+IntToStr(Line));
end;

class function TEvaluator.AsString(const ARoot:TBaseNode):String;

  function ItemsToString(const AItems:TBaseNodes):String;
  var t : Integer;
  begin
    result:='';

    if Length(AItems)>0 then
       for t:=Low(AItems) to High(AItems) do
       begin
         if t>Low(AItems) then
            result:=result+',';

         result:=result+AsString(AItems[t]);
       end;
  end;

begin
  if ARoot is TExpressionNode then
  begin
    result:=AsString(TExpressionNode(ARoot).Left);

    if TExpressionNode(ARoot).Right<>nil then
       result:=TExpressionNode.OperatorToString(TExpressionNode(ARoot).Operat)+
               AsString(TExpressionNode(ARoot).Right);
  end
  else
  if ARoot is TFunction then
     result:=TFunction(ARoot).Name+'('+ItemsToString(TFunction(ARoot).Parameters)+')'
  else
  if ARoot is TArrayNode then
     result:='['+ItemsToString(TArrayNode(ARoot).Items)+']'
  else
     result:=ARoot.Text;
end;

constructor TSortedOperators.Create;
begin
  inherited Create;
  Duplicates:=dupError;
  Sorted:=True;

  AddObject('=',TObject(opEqual));
  AddObject('<>',TObject(opNotEqual));
  AddObject('AND',TObject(opAnd));
  AddObject('OR',TObject(opOr));
  AddObject('AND NOT',TObject(opAndNot));
  AddObject('OR NOT',TObject(opOrNot));
  AddObject('XOR',TObject(opXor));
  AddObject('SHL',TObject(opShl));
  AddObject('SHR',TObject(opShr));
  AddObject('<',TObject(opLower));
  AddObject('>',TObject(opGreater));
  AddObject('<=',TObject(opLowerEqual));
  AddObject('>=',TObject(opGreaterEqual));
  AddObject('+',TObject(opAdd));
  AddObject('-',TObject(opSubtract));
  AddObject('*',TObject(opMultiply));
  AddObject('/',TObject(opDivide));
  AddObject('NOT',TObject(opUnaryNot));
  AddObject('DIV',TObject(opDiv));
  AddObject('MOD',TObject(opMod));

  {$IFDEF USEMATH}
  AddObject('^',TObject(opPower));
  {$ENDIF}
end;

initialization
  SortedOp:=TSortedOperators.Create;
finalization
  SortedOp.Free;
end.
