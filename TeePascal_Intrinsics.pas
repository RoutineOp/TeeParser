unit TeePascal_Intrinsics;

{$IFDEF D10}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

interface

implementation

uses
  TeeCode;

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
  ByteBool,
  CompType,
  CurrencyType,
  LongBool,
  LongWordType,
  NativeInt,
  NativeUInt,
  OleVariantType,
  OpenStringType,
  PAnsiCharType,
  PWideCharType,
  RealType,
  ShortInt,
  ShortStringType,
  SmallInt,
  UInt64,
  UnicodeStringType,
  WideStringType,
  WordBool : TTypeDeclaration;

  TextSpec : TTypeSpecification;

var
  EmptyString : TString;
  One : TNumber;

type
  TIdentifiersAccess = class(TIdentifiers);

const
  LazarusVersion=126;

procedure AddIntrinsics(const Globals:TSection; const CompilerVersion:Integer);

  function GlobalMethod(const AClass:TMethodSpecificationClass):TMethodDeclaration;
  begin
    result:=TMethodDeclaration.Create(Globals);
    result.TypeIdentifier:=TTypeDeclaration.CreateSpec(result,AClass.Create(result));
  end;

  function GlobalFunction(const AName:String; const AResult:TVariableDeclaration):TMethodDeclaration;
  begin
    result:=GlobalMethod(TFunctionSpecification);
    result.Name:=AName;
    TFunctionSpecification(result.TypeIdentifier.Expression).ResultValue:=AResult;
  end;

  function GlobalProcedure(const AName:String):TMethodDeclaration;
  begin
    result:=GlobalMethod(TProcedureSpecification);
    result.Name:=AName;
  end;

  function AddParam(const AMethod:TMethodDeclaration; const AType:TTypeDeclaration;
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

var
  MaxIntConst,
  MaxLongintConst : TNumber;

  PiConst : TFloatNumber;

  HInstanceConst : TVariableDeclaration;

  //LongintType : TTypeDeclaration;

  function ResultOf(const AType:TTypeSpecification):TVariableDeclaration; overload;
  begin
    result:=TVariableDeclaration.CreateType(Globals,TTypeDeclaration.CreateSpec(Globals,AType));
  end;

  function ResultOf(const AType:TTypeDeclaration):TVariableDeclaration; overload;
  begin
    result:=TVariableDeclaration.CreateType(Globals,AType);
  end;

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
    AddParam(result,ByteType,'X'); // Byte !
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
  end;

  function GetDirBlock:TMethodDeclaration;
  begin
    result:=GlobalProcedure('GetDir');

    AddParam(result,ByteType,'Drive');
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

  function GlobalType(const ASpec:TTypeSpecification):TTypeDeclaration; overload;
  begin
    result:=TTypeDeclaration.CreateSpec(Globals,ASpec);
  end;

  function GlobalType(const AName:String):TTypeDeclaration; overload;
  begin
    result:=TTypeDeclaration.Create(Globals);
    result.Name:=AName;
  end;

  function AddGlobalType(const AName:String; const AExpression:TTypeSpecification=nil):TTypeDeclaration;
  begin
    result:=GlobalType(AName);
    result.Expression:=AExpression;
    TIdentifiersAccess(Globals.Types).AddSorted(result);
  end;

  function GetTypeKindBlock:TMethodDeclaration;
  begin
    TypeKindType:=AddGlobalType('TTypeKind'); // <-- Temporary, see TeeCode

    result:=GlobalFunction('GetTypeKind',ResultOf(TypeKindType));
    AddParam(result,AnyType,'T'); // <--- "TypeIdentifier"
  end;

  function AddGlobalPointerType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
  var tmp : TTypePointerOf;
  begin
    result:=GlobalType(AName);

    tmp:=TTypePointerOf.Create(Globals);
    tmp.TypeIdentifier:=AType;
    tmp.TypeIdentifier.Expression:=AType.Expression;

    result.Expression:=tmp;

    TIdentifiersAccess(Globals.Types).AddSorted(result);
  end;

  function AddGlobalAliasType(const AName:String; const AType:TTypeDeclaration):TTypeDeclaration;
  begin
    result:=GlobalType(AName);

    result.Alias:=AType;
    result.Expression:=AType.Expression;

    TIdentifiersAccess(Globals.Types).AddSorted(result);
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

  if Globals.Methods=nil then
     Globals.Methods:=TMethodDeclarations.Create(Globals);

  if Globals.Constants=nil then
     Globals.Constants:=TConstantDeclarations.Create(Globals);

  if Globals.Variables=nil then
     Globals.Variables:=TVariableDeclarations.Create(Globals);

  if Globals.Types=nil then
     Globals.Types:=TTypeDeclarations.Create(Globals);

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

  ShortStringType:=AddGlobalType('ShortString',StringSpec);
  AnsiStringType:=AddGlobalType('AnsiString',StringSpec);

  if CompilerVersion>=200 then
     AnsiStringType.HasCodePage:=True;

  if CompilerVersion>=210 then
     OpenStringType:=AddGlobalType('OpenString',StringSpec);

  WideStringType:=AddGlobalType('WideString',StringSpec);

  if (CompilerVersion>=200) or (CompilerVersion=LazarusVersion) then
     UnicodeStringType:=AddGlobalType('UnicodeString',StringSpec);

  PCharType:=AddGlobalPointerType('PChar',CharType);
  PAnsiCharType:=AddGlobalPointerType('PAnsiChar',AnsiCharType);
  PWideCharType:=AddGlobalPointerType('PWideChar',WideCharType);

  WordType:=AddGlobalType('Word',NumberSpec);
  ByteType:=AddGlobalType('Byte',ByteSpec);
  IntegerOrSetType:=GlobalType(NumberOrSetSpec);
  ShortInt:=AddGlobalType('ShortInt',NumberSpec);
  SmallInt:=AddGlobalType('SmallInt',NumberSpec);
  UInt64:=AddGlobalType('UInt64',NumberSpec);

  NativeInt:=AddGlobalType('NativeInt',NumberSpec);

  if CompilerVersion>=220 then
  begin
    NativeUInt:=AddGlobalType('NativeUInt',NumberSpec);

    ArrayOfByte:=TArraySpecification.Create(Globals);
    ArrayOfByte.Expression:=ByteType;
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
  WordBool:=AddGlobalType('WordBool',BooleanSpec);
  LongBool:=AddGlobalType('LongBool',BooleanSpec);
  ByteBool:=AddGlobalType('ByteBool',BooleanSpec);

  DoubleType:=AddGlobalType('Double',FloatSpec);
  SingleType:=AddGlobalType('Single',FloatSpec);
  ExtendedType:=AddGlobalType('Extended',FloatSpec);
  CompType:=AddGlobalType('Comp',FloatSpec);
  CurrencyType:=AddGlobalType('Currency',FloatSpec);

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
  OleVariantType:=AddGlobalType('OleVariant',VariantSpec);

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
          Globals.Methods.Add(SwapBlock);
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

initialization
  AddGlobals:=AddIntrinsics;
finalization
  AddGlobals:=nil;
end.
