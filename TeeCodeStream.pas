unit TeeCodeStream;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0}
     {$DEFINE HAS_INLINE}
     {$DEFINE D9}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes, TeeCode;

type
  TCodeWriter=class(TWriter)
  private
    Package : TPackage;

    procedure WriteAs(const ABlock: TBlock; const ID:Integer);
    procedure WriteBlocks(const AModule:TModule);
    procedure WriteIdentifierNames(const AIdents:TIdentifiers);
  public
    Constructor Create(const AStream: TStream);

    class procedure WriteToFile(const AModule:TModule; const AFileName:String); overload;
    class procedure WriteToFile(const APackage:TPackage; const AFileName:String); overload;
  end;

  TCodeReader=class(TReader)
  private
    Module : TModule;
    Package : TPackage;

    CurrentDir : String;

    procedure ReadAs(const ABlock: TBlock; const ID:Integer);
    procedure ReadBlocks(const AModule:TModule);
  public
    Constructor Create(const AStream: TStream);

    class procedure ReadFromFile(const AModule:TModule; const AFileName:String); overload;
    class procedure ReadFromFile(const APackage:TPackage; const AFileName:String); overload;
  end;

implementation

uses
  {$IFOPT D+}
  Windows,
  {$ENDIF}
  TeePascal, // <-- Due to TDirectiveKind
  SysUtils;

const
  Version=1;
  CompatibleWith=1;

  TeeSignature='TEE';
  DefaultBufferSize=16384;

var
  IDs : TBaseList=nil;

const
  TUnit_ID      =0;
  TInterface_ID =1;
  TUses_ID      =2;
  TConstantDeclarations_ID =3;
  TConstantDeclaration_ID  =4;
  TUnitScope_ID =5;
  TDirective_ID =6;
  TMethodDeclarations_ID = 7;
  TVariableDeclarations_ID = 8;
  TTypeDeclarations_ID = 9;
  TArraySpecification_ID = 10;
  TNumber_ID = 11;
  TString_ID = 12;
  TMethodDeclaration_ID = 13;
  TAttribute_ID = 14;
  TLibrary_ID = 15;
  TVariableDeclaration_ID = 16;
  TThreadVariable_ID = 17;
  TExportDeclaration_ID = 18;
  TCaseVariableItem_ID = 19;
  TCaseVariable_ID = 20;
  TScopedType_ID = 21;
  TTypeParameter_ID = 22;
  TTypePointerOf_ID = 23;
  TTypeTypeOf_ID = 24;
  TArrayValues_ID = 25;
  TItemExpression_ID = 26;
  TTypeItem_ID = 27;
  TSetItem_ID = 28;
  TSetSpecification_ID = 29;
  TRangeExpression_ID = 30;
  TSetOfSpecification_ID = 31;
  TSetRange_ID = 32;
  TTypeRange_ID = 33;
  TResourceStrings_ID = 34;
  TFloatNumber_ID = 35;
  TChar_ID = 36;
  TBoolean_ID = 37;
  TOperatorExpression_ID = 38;
  TNestedExpression_ID = 39;
  TStringFormatExpression_ID = 40;
  TPropertyDeclaration_ID = 41;
  TDispatchInterfaceProperty_ID = 42;
  TRecordInstance_ID = 43;
  TGenericSpec_ID = 44;
  TGenericTypeDeclaration_ID = 45;
  TRecordSpecification_ID = 46;
  TInterfaceSpecification_ID = 47;
  TDispatchInterfaceSpecification_ID = 48;
  TClassSpecification_ID = 49;
  TRecordHelperSpecification_ID = 50;
  TClassHelperSpecification_ID = 51;
  TClassOf_ID = 52;
  TObjectSpecification_ID = 53;
  TImplementation_ID = 54;
  TStatements_ID = 55;
  TASMStatements_ID = 56;
  TCasting_ID = 57;
  TAddressOf_ID = 58;
  TAssignment_ID = 59;
  TIf_ID = 60;
  TOn_ID = 61;
  TWhile_ID = 62;
  TWith_ID = 63;
  TFor_ID = 64;
  TRepeat_ID = 65;
  TTry_ID = 66;
  TRaise_ID = 67;
  TBody_ID = 68;
  TReferenceSpecification_ID = 69;
  TProcedureSpecification_ID = 70;
  TConstructorSpecification_ID = 71;
  TDestructorSpecification_ID = 72;
  TFunctionSpecification_ID = 73;
  TRecordOperatorSpec_ID = 74;
  TCallExpression_ID = 75;
  TCall_ID = 76;
  TDereferenceExpression_ID = 77;
  TFieldExpression_ID = 78;
  TTypedMethod_ID = 79;
  TInheritedExpression_ID = 80;
  TCaseItem_ID = 81;
  TCase_ID = 82;
  TLabelDeclaration_ID = 83;
  TLabelDeclarations_ID = 84;
  TLabelPlace_ID = 85;
  TGoto_ID = 86;
  TInitialization_ID = 87;
  TFinalization_ID = 88;
  TImplementationModule_ID = 89;
  TProgram_ID = 90;
  TPackages_ID = 91;
  TPackageContains_ID = 92;
  TPackage_ID = 93;
  TTypeSpecification_ID = 94;
  TTypeDeclaration_ID = 95;
  TParameter_ID = 96;
  TParameters_ID = 97;
  TVisibilityIdent_ID = 98;
  TRecordFieldsDeclarations_ID = 99;
  TTryExcept_ID = 100;
  TGenericMethod_ID = 101;

  MaxID=101;

  IDStrings:Array[0..MaxID] of String = (
  'TUnit',
  'TInterface',
  'TUses',
  'TConstantDeclarations',
  'TConstantDeclaration',
  'TUnitScope',
  'TDirective',
  'TMethodDeclarations',
  'TVariableDeclarations',
  'TTypeDeclarations',
  'TArraySpecification',
  'TNumber',
  'TString',
  'TMethodDeclaration',
  'TAttribute',
  'TLibrary',
  'TVariableDeclaration',
  'TThreadVariable',
  'TExportDeclaration',
  'TCaseVariableItem',
  'TCaseVariable',
  'TScopedType',
  'TTypeParameter',
  'TTypePointerOf',
  'TTypeTypeOf',
  'TArrayValues',
  'TItemExpression',
  'TTypeItem',
  'TSetItem',
  'TSetSpecification',
  'TRangeExpression',
  'TSetOfSpecification',
  'TSetRange',
  'TTypeRange',
  'TResourceStrings',
  'TFloatNumber',
  'TChar',
  'TBoolean',
  'TOperatorExpression',
  'TNestedExpression',
  'TStringFormatExpression',
  'TPropertyDeclaration',
  'TDispatchInterfaceProperty',
  'TRecordInstance',
  'TGenericSpec',
  'TGenericTypeDeclaration',
  'TRecordSpecification',
  'TInterfaceSpecification',
  'TDispatchInterfaceSpecification',
  'TClassSpecification',
  'TRecordHelperSpecification',
  'TClassHelperSpecification',
  'TClassOf',
  'TObjectSpecification',
  'TImplementation',
  'TStatements',
  'TASMStatements',
  'TCasting',
  'TAddressOf',
  'TAssignment',
  'TIf',
  'TOn',
  'TWhile',
  'TWith',
  'TFor',
  'TRepeat',
  'TTry',
  'TRaise',
  'TBody',
  'TReferenceSpecification',
  'TProcedureSpecification',
  'TConstructorSpecification',
  'TDestructorSpecification',
  'TFunctionSpecification',
  'TRecordOperatorSpec',
  'TCallExpression',
  'TCall',
  'TDereferenceExpression',
  'TFieldExpression',
  'TTypedMethod',
  'TInheritedExpression',
  'TCaseItem',
  'TCase',
  'TLabelDeclaration',
  'TLabelDeclarations',
  'TLabelPlace',
  'TGoto',
  'TInitialization',
  'TFinalization',
  'TImplementationModule',
  'TProgram',
  'TPackages',
  'TPackageContains',
  'TPackage',
  'TTypeSpecification',
  'TTypeDeclaration',
  'TParameter',
  'TParameters',
  'TVisibilityIdent',
  'TRecordFieldsDeclarations',
  'TTryExcept',
  'TGenericMethod'
  );

  IDClasses:Array[0..MaxID] of TBlockClass = (
  TUnit,
  TInterface,
  TUses,
  TConstantDeclarations,
  TConstantDeclaration,
  TUnitScope,
  TDirective,
  TMethodDeclarations,
  TVariableDeclarations,
  TTypeDeclarations,
  TArraySpecification,
  TNumber,
  TString,
  TMethodDeclaration,
  TAttribute,
  TLibrary,
  TVariableDeclaration,
  TThreadVariable,
  TExportDeclaration,
  TCaseVariableItem,
  TCaseVariable,
  TScopedType,
  TTypeParameter,
  TTypePointerOf,
  TTypeTypeOf,
  TArrayValues,
  TItemExpression,
  TTypeItem,
  TSetItem,
  TSetSpecification,
  TRangeExpression,
  TSetOfSpecification,
  TSetRange,
  TTypeRange,
  TResourceStrings,
  TFloatNumber,
  TChar,
  TBoolean,
  TOperatorExpression,
  TNestedExpression,
  TStringFormatExpression,
  TPropertyDeclaration,
  TDispatchInterfaceProperty,
  TRecordInstance,
  TGenericSpec,
  TGenericTypeDeclaration,
  TRecordSpecification,
  TInterfaceSpecification,
  TDispatchInterfaceSpecification,
  TClassSpecification,
  TRecordHelperSpecification,
  TClassHelperSpecification,
  TClassOf,
  TObjectSpecification,
  TImplementation,
  TStatements,
  TASMStatements,
  TCasting,
  TAddressOf,
  TAssignment,
  TIf,
  TOn,
  TWhile,
  TWith,
  TFor,
  TRepeat,
  TTry,
  TRaise,
  TBody,
  TReferenceSpecification,
  TProcedureSpecification,
  TConstructorSpecification,
  TDestructorSpecification,
  TFunctionSpecification,
  TRecordOperatorSpec,
  TCallExpression,
  TCall,
  TDereferenceExpression,
  TFieldExpression,
  TTypedMethod,
  TInheritedExpression,
  TCaseItem,
  TCase,
  TLabelDeclaration,
  TLabelDeclarations,
  TLabelPlace,
  TGoto,
  TInitialization,
  TFinalization,
  TImplementationModule,
  TProgram,
  TPackages,
  TPackageContains,
  TPackage,
  TTypeSpecification,
  TTypeDeclaration,
  TParameter,
  TParameters,
  TVisibilityIdent,
  TRecordFieldsDeclarations,
  TTryExcept,
  TGenericMethod
  );

procedure CheckIDs;
var t: Integer;
begin
  if IDs=nil then
  begin
    IDs:=TBaseList.Create;

    for t:=0 to MaxID do
        IDs.AddItem(IDStrings[t],TObject(t));
  end;
end;

function ClassID(const ABlock:TBlock):Integer;
begin
  CheckIDs;

  if IDs.Find(ABlock.ClassName,result) then
     result:=Integer(IDs.Get(result).O)
  else
     result:=-1; // Error !
end;

{ TCodeReader }

type
  TIdentifiersAccess=class(TIdentifiers);

procedure TCodeReader.ReadAs(const ABlock: TBlock; const ID:Integer);

  function ReadRef:TBlock;
  var tmp : Integer;
      tmpIndex : Integer;
  begin
    tmp:=ReadInteger;

    if tmp=-1 then
       result:=nil
    else
    begin
      tmpIndex:=ReadInteger;

      if tmp=-2 then
         result:=Module.Used[tmpIndex]
      else
         result:=Module.Used[tmp].Blocks[tmpIndex];
    end;
  end;

  function ReadRefExpression:TExpression; {$IFDEF D9}inline;{$ENDIF}
  begin
    result:=TExpression(ReadRef);
  end;

  procedure ReadAsBlock(const ABlock:TBlock);
  var t, H : Integer;
  begin
    // Position : TParserPosition;
    // Owner
    // Index
    // Owned ??
    // Parent ??

    H:=ReadInteger;

    if H<>-1 then
    begin
      SetLength(ABlock.Attributes,H+1);

      for t:=0 to H do
          ABlock.Attributes[t]:=TAttribute(ReadRef);
    end;
  end;

  procedure ReadAsExpression(const AExp:TExpression);
  begin
    ReadAsBlock(AExp);
    AExp.TypeIdentifier:=TTypeDeclaration(ReadRef);

    //  ValueType : TTypeDeclaration; ?
  end;

  procedure ReadAsIdentifier(const AIdent:TIdentifier);
  begin
    ReadAsExpression(AIdent);
    AIdent.Name:=ReadString;
    AIdent.Visibility:=TVisibility(ReadInteger);
    AIdent.AbsoluteRef:=ReadString;

    AIdent.IsDeprecated:=ReadBoolean;

    AIdent.DeprecatedMessage:=ReadRefExpression;

    AIdent.IsInline:=ReadBoolean;
    AIdent.IsPlatform:=ReadBoolean;

    // Usage : Integer; ??
  end;

  function ReadAsBlocks:TBlocks;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=ReadRef;
    end;
  end;

  procedure ReadAsSection(const ASection:TSection);
  begin
    ReadAsBlock(ASection);
    ASection.Bodies:=ReadBoolean;

    ASection.Constants:=TConstantDeclarations(ReadRef);
    ASection.Methods:=TMethodDeclarations(ReadRef);
    ASection.Types:=TTypeDeclarations(ReadRef);
    ASection.Variables:=TVariableDeclarations(ReadRef);
    ASection.Labels:=TLabelDeclarations(ReadRef);
    ASection.ResourceStrings:=TResourceStrings(ReadRef);

    ASection.Ordered:=ReadAsBlocks;
  end;

  procedure ReadAsUsesSection(const AUsesSection:TUsesSection);
  begin
    ReadAs(AUsesSection.UsesUnits,TUses_ID);
    ReadAsSection(AUsesSection);
  end;

  procedure ReadAsIdentifiers(const AIdents:TIdentifiers);
  var t,L : Integer;
  begin
    L:=ReadInteger;

    if L>0 then
    begin
      SetLength(TIdentifiersAccess(AIdents).Items,L);

      for t:=0 to L-1 do
          TIdentifiersAccess(AIdents).Items[t]:=TIdentifier(ReadRef);
      //    AIdents.AddSorted(TIdentifier(Units.Blocks[ReadInteger]));
    end;
  end;

  function ReadModuleReference(const DoLoad:Boolean):TModule;
  var tmp,
      tmpS : String;
  begin
    tmpS:=ReadString;

    result:=Module.Package.Language.FindModule(tmpS);

    if result=nil then
       result:=Module.Package.Language.Modules.NewModule(tmpS,Module.Package.Language.ModuleClass);

    if DoLoad and (result.BlockCount=0) then
    begin
      tmp:=CurrentDir;

      if Copy(tmp,Length(tmp),1)<>'\' then
         tmp:=tmp+'\';

      TCodeReader.ReadFromFile(result,tmp+tmpS+'.b');
    end;
  end;

  procedure ReadUsed(const AIdents:TIdentifiers);
  var t,
      L : Integer;
  begin
    L:=ReadInteger;

    for t:=0 to L-1 do
        AIdents.AddSorted(ReadModuleReference(True));
  end;

  procedure ReadAsNativeModule(const AModule:TNativeModule);
  begin
    ReadAs(AModule,TImplementationModule_ID);
    AModule.Main:=TModuleBody(ReadRef);
  end;

  procedure ReadAsTypeRedirect(const ATypeRedirect:TTypeRedirect);
  begin
    ReadAs(ATypeRedirect,TTypeSpecification_ID);
    ATypeRedirect.TypeIdentifier:=TTypeDeclaration(ReadRef);
  end;

  function ReadAsExpressions:TExpressions;
  var t,H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=ReadRefExpression;
    end;
  end;

  procedure ReadAsParameters(const AParams:TParameters);
  var t,L : Integer;
  begin
    L:=ReadInteger;

    for t:=0 to L-1 do
        AParams.Add(TVariableDeclaration(ReadRef));
  end;

  function ReadAsTypeSpecs:TTypeSpecifications;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=TTypeSpecification(ReadRef);
    end;
  end;

  function ReadAsTypes:TTypeDeclarationArray;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=TTypeDeclaration(ReadRef);
    end;
  end;

  procedure ReadAsRecordSpec(const ARecord:TRecordSpecification);
  var t : Integer;
  begin
    ReadAs(ARecord,TGenericSpec_ID);
    ARecord.PropertyDefault:=TPropertyDeclaration(ReadRef);

    for t:=Ord(Low(TRecordOperatorKind)) to Ord(High(TRecordOperatorKind)) do
        ARecord.Operators[TRecordOperatorKind(t)]:=ReadAsTypeSpecs;

    ARecord.IsPacked:=ReadBoolean;

    ARecord.Ancestors:=ReadAsTypes;
    ARecord.RecordType:=TTypeDeclaration(ReadRef);
    ARecord.Outer:=TRecordSpecification(ReadRef);
    ARecord.Module:=TModule(ReadRef);

    ARecord.Constants:=TConstantDeclarations(ReadRef);
    ARecord.Fields:=TRecordFieldsDeclarations(ReadRef);
    ARecord.Methods:=TMethodDeclarations(ReadRef);
    ARecord.Types:=TTypeDeclarations(ReadRef);

    ARecord.Ordered:=ReadAsBlocks;
  end;

  function ReadAsStatements:TStatementArray;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=TStatement(ReadRef);
    end;
  end;

  function ReadAsStrings:TStringArray;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=ReadString;
    end;
  end;

  procedure ReadAsStatement(const AStatement:TStatement);
  begin
    ReadAsBlock(AStatement);
  end;

  procedure ReadAsMethodSpec(const AMethod:TMethodSpecification);
  begin
    ReadAs(AMethod,TGenericSpec_ID);

    // !!!  ReadInteger( Directives : TMethodDirectives;

    AMethod.DeprecatedMessage:=TConstantDeclaration(ReadRef);
    AMethod.OfRecord:=TTypeDeclaration(ReadRef);

    AMethod.OfObject:=ReadBoolean;
    AMethod.IsClass:=ReadBoolean;

    AMethod.IsExternal:=ReadBoolean;
    AMethod.IsDelayed:=ReadBoolean;

    AMethod.ExternalDLL:=ReadRefExpression;
    AMethod.ExternalName:=ReadRefExpression;

    AMethod.DispatchID:=ReadRefExpression;
    AMethod.MessageField:=ReadRefExpression;

    AMethod.Referenced:=ReadString;

    AMethod.Body:=TBody(ReadRef);
  end;

  procedure ReadAsMethodParams(const AMethod:TMethodParams);
  begin
    ReadAsMethodSpec(AMethod);
    AMethod.FixedParameters:=ReadInteger;
    AMethod.VariableParameters:=ReadBoolean;
    AMethod.Parameters:=TParameters(ReadRef);
  end;

  procedure ReadAsGenericMethod(const AMethod:TGenericMethod);
  begin
    ReadAsExpression(AMethod);
    AMethod.Method:=TMethodDeclaration(ReadRef);
    AMethod.TypeParameters:=ReadAsExpressions;
  end;

  function ReadAsCaseArray:TCaseItems;
  var t, H : Integer;
  begin
    H:=ReadInteger;

    if H=-1 then
       result:=nil
    else
    begin
      SetLength(result,H+1);

      for t:=0 to H do
          result[t]:=TCaseItem(ReadRef);
    end;
  end;

  procedure ReadAsModule(const AModule:TModule);
  begin
    ReadAs(AModule,TTypeDeclaration_ID);
    AModule.FileName:=ReadString;
  end;

  procedure ReadUnit(const AUnit:TUnit);

    procedure ReadExtraUsed;
    var N, t : Integer;
    begin
      N:=ReadInteger;

      SetLength(AUnit.Used,1+N);

      AUnit.ModuleIndex:=0;
      AUnit.Used[0]:=AUnit;

      for t:=1 to N do
      begin
        ReadBoolean;
        AUnit.Used[t]:=ReadModuleReference(False{ReadBoolean});
        AUnit.Used[t].ModuleIndex:=t;
      end;
    end;

  begin
    Module:=AUnit;

    if Package<>nil then
       Module.Package:=Package;

    ReadExtraUsed;

    ReadBlocks(AUnit);

    ReadAs(AUnit,TImplementationModule_ID);
    AUnit.UnitInterface:=TInterface(ReadRef);
    AUnit.UnitInitialization:=TInitialization(ReadRef);
    AUnit.UnitFinalization:=TFinalization(ReadRef);
  end;

var t,tmp,
    H : Integer;
begin
  case ID  of
  TUnit_ID : ReadUnit(TUnit(ABlock));

  TInterface_ID : ReadAsUsesSection(TInterface(ABlock));
  TUses_ID : ReadUsed(TUses(ABlock));

  TConstantDeclarations_ID : ReadAsIdentifiers(TConstantDeclarations(ABlock));

  TConstantDeclaration_ID : begin
    ReadAsIdentifier(TConstantDeclaration(ABlock));
    TConstantDeclaration(ABlock).NeedsType:=ReadBoolean;
    TConstantDeclaration(ABlock).AutomaticType:=ReadBoolean;
    TConstantDeclaration(ABlock).OptionalValue:=ReadBoolean;
    TConstantDeclaration(ABlock).Value:=ReadRef;
    TConstantDeclaration(ABlock).IsClass:=ReadBoolean;
  end;

  TUnitScope_ID : ReadAsIdentifier(TUnitScope(ABlock));

  TDirective_ID : begin
    ReadAs(ABlock,TAttribute_ID);
    TDirective(ABlock).Kind:=TDirectiveKind(ReadInteger);
  end;

  TMethodDeclarations_ID : ReadAsIdentifiers(TMethodDeclarations(ABlock));
  TVariableDeclarations_ID : ReadAsIdentifiers(TVariableDeclarations(ABlock));
  TTypeDeclarations_ID :  ReadAsIdentifiers(TTypeDeclarations(ABlock));
  TArraySpecification_ID : begin
    ReadAs(ABlock,TRecordSpecification_ID);

    TArraySpecification(ABlock).IsPacked:=ReadBoolean;
    TArraySpecification(ABlock).CreateConstructor:=TMethodDeclaration(ReadRef);
    TArraySpecification(ABlock).Expression:=TTypeDeclaration(ReadRef);

    TArraySpecification(ABlock).Dimensions:=ReadAsExpressions;
  end;

  TNumber_ID : begin
    ReadAs(ABlock,TConstantDeclaration_ID);
    TNumber(ABlock).Hexadecimal:=ReadBoolean;
    TNumber(ABlock).Value:=ReadInt64;
  end;

  TString_ID : begin
    ReadAs(ABlock,TConstantDeclaration_ID);
    TString(ABlock).Value:=ReadString;
  end;

  TMethodDeclaration_ID : ReadAs(ABlock,TVariableDeclaration_ID);
  TAttribute_ID : begin
    ReadAsBlock(ABlock);
    TAttribute(ABlock).Text:=ReadString;
  end;

  TLibrary_ID : ReadAsNativeModule(TLibrary(ABlock));
  TVariableDeclaration_ID : ReadAs(ABlock,TConstantDeclaration_ID);
  TThreadVariable_ID : ReadAs(ABlock,TVariableDeclaration_ID);
  TExportDeclaration_ID : begin
    ReadAsIdentifier(TExportDeclaration(ABlock));
    TExportDeclaration(ABlock).Method:=TMethodDeclaration(ReadRef);
  end;

  TCaseVariableItem_ID : begin
    ReadAs(ABlock,TVariableDeclaration_ID);
    TCaseVariableItem(ABlock).Expression:=ReadRefExpression;
  end;

  TCaseVariable_ID : begin
    ReadAs(ABlock,TVariableDeclaration_ID);
    ReadAsIdentifiers(TCaseVariable(ABlock).Fields);
  end;

  TScopedType_ID : begin
    ReadAs(ABlock,TTypeDeclaration_ID);
    TScopedType(ABlock).Field:=TFieldExpression(ReadRef);
  end;

  TTypeParameter_ID : begin
    ReadAs(ABlock,TTypeDeclaration_ID);
    TTypeParameter(ABlock).Index:=ReadInteger;

    H:=ReadInteger;

    if H<>-1 then
    begin
      SetLength(TTypeParameter(ABlock).Constraints,H+1);

      for t:=0 to H do
          TTypeParameter(ABlock).Constraints[t]:=TTypeDeclaration(ReadRef);
    end;
  end;

  TTypePointerOf_ID : ReadAsTypeRedirect(TTypePointerOf(ABlock));
  TTypeTypeOf_ID :  ReadAsTypeRedirect(TTypePointerOf(ABlock));

  TArrayValues_ID : begin
    ReadAsExpression(TArrayValues(ABlock));
    TArrayValues(ABlock).DimensionIndex:=ReadInteger;
    TArrayValues(ABlock).Items:=ReadAsExpressions;
  end;

  TItemExpression_ID : begin
    ReadAsExpression(TItemExpression(ABlock));
    TItemExpression(ABlock).Value:=ReadRefExpression;
    TItemExpression(ABlock).Items:=ReadAsExpressions;
  end;

  TTypeItem_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TTypeItem(ABlock).Item:=TItemExpression(ReadRef);
  end;

  TSetItem_ID : begin
    ReadAs(ABlock,TTypeDeclaration_ID);
    TSetItem(ABlock).Value:=ReadRefExpression;
  end;

  TSetSpecification_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TSetSpecification(ABlock).IsScoped:=ReadBoolean;
    TSetSpecification(ABlock).IsSetOf:=ReadBoolean;
    ReadAs(TSetSpecification(ABlock).Items,TTypeDeclarations_ID);
  end;

  TRangeExpression_ID : begin
    ReadAsExpression(TRangeExpression(ABlock));
    TRangeExpression(ABlock).StartRange:=ReadRefExpression;
    TRangeExpression(ABlock).EndRange:=ReadRefExpression;
  end;

  TSetOfSpecification_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TSetOfSpecification(ABlock).TypeIdentifier:=TTypeDeclaration(ReadRef);
  end;

  TSetRange_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TSetRange(ABlock).Range:=TRangeExpression(ReadRef);
  end;

  TTypeRange_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TTypeRange(ABlock).Range:=TRangeExpression(ReadRef);
  end;

  TResourceStrings_ID : ReadAs(ABlock,TConstantDeclarations_ID);

  TFloatNumber_ID : begin
    ReadAs(ABlock,TConstantDeclaration_ID);
    TFloatNumber(ABlock).Exponent:=ReadBoolean;
    TFloatNumber(ABlock).Value:=ReadFloat;
    TFloatNumber(ABlock).ValueString:=ReadString;
  end;

  TChar_ID : begin
    ReadAs(ABlock,TConstantDeclaration_ID);
    TChar(ABlock).Value:=ReadChar;
  end;

  TBoolean_ID : begin
    ReadAs(ABlock,TConstantDeclaration_ID);
    TBoolean(ABlock).Value:=ReadBoolean;
  end;

  TOperatorExpression_ID : begin
    ReadAsExpression(TOperatorExpression(ABlock));
    TOperatorExpression(ABlock).Operat:=TOperator(ReadInteger);
    TOperatorExpression(ABlock).Left:=ReadRefExpression;
    TOperatorExpression(ABlock).Right:=ReadRefExpression;
  end;

  TNestedExpression_ID : begin
    ReadAsExpression(TNestedExpression(ABlock));
    TNestedExpression(ABlock).Expression:=ReadRefExpression;
  end;

  TStringFormatExpression_ID : begin
    ReadAsExpression(TStringFormatExpression(ABlock));
    TStringFormatExpression(ABlock).Expression:=ReadRefExpression;
    TStringFormatExpression(ABlock).Width:=ReadRefExpression;
    TStringFormatExpression(ABlock).Decimals:=ReadRefExpression;
  end;

  TPropertyDeclaration_ID : begin
    ReadAs(ABlock,TVariableDeclaration_ID);
    TPropertyDeclaration(ABlock).IsDefault:=ReadBoolean;
    TPropertyDeclaration(ABlock).NoDefault:=ReadBoolean;
    TPropertyDeclaration(ABlock).Index:=ReadRefExpression;
    TPropertyDeclaration(ABlock).PropertyType:=TTypeDeclaration(ReadRef);
    TPropertyDeclaration(ABlock).Indexes:=TParameters(ReadRef);

    TPropertyDeclaration(ABlock).ReadPart:=ReadRefExpression;
    TPropertyDeclaration(ABlock).WritePart:=ReadRefExpression;

    TPropertyDeclaration(ABlock).Stored:=ReadRefExpression;
    TPropertyDeclaration(ABlock).DefaultValue:=ReadRefExpression;

    TPropertyDeclaration(ABlock).ImplementsInterfaces:=ReadAsExpressions;

    TPropertyDeclaration(ABlock).Ancestor:=TPropertyDeclaration(ReadRef);

    // Pending: Move to TDispatchInterfaceProperty
    TPropertyDeclaration(ABlock).ReadOnly:=ReadBoolean;
    TPropertyDeclaration(ABlock).ReadOnly:=ReadBoolean;
    TPropertyDeclaration(ABlock).DispatchId:=ReadRefExpression;

    TPropertyDeclaration(ABlock).RecordType:=TTypeDeclaration(ReadRef);
  end;

  TDispatchInterfaceProperty_ID : ReadAs(ABlock,TPropertyDeclaration_ID);

  TRecordInstance_ID : begin
    ReadAs(ABlock,TVariableDeclaration_ID);
    TRecordInstance(ABlock).Fields:=TRecordFieldsDeclarations(ReadRef);
  end;

  TGenericSpec_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TGenericSpec(ABlock).TypeParameters:=ReadAsExpressions;
  end;

  TGenericTypeDeclaration_ID : ReadAs(ABlock,TTypeDeclaration_ID);

  TRecordSpecification_ID : ReadAsRecordSpec(TRecordSpecification(ABlock));

  TInterfaceSpecification_ID : ReadAs(ABlock,TRecordSpecification_ID);
  TDispatchInterfaceSpecification_ID : ReadAs(ABlock,TInterfaceSpecification_ID);

  TClassSpecification_ID : begin;
    ReadAs(ABlock,TRecordSpecification_ID);
    TClassSpecification(ABlock).IsAbstract:=ReadBoolean;
    TClassSpecification(ABlock).IsSealed:=ReadBoolean;
  end;

  TRecordHelperSpecification_ID : begin
    ReadAs(ABlock,TRecordSpecification_ID);
    TRecordHelperSpecification(ABlock).TypeIdentifier:=TTypeDeclaration(ReadRef);
  end;

  TClassHelperSpecification_ID : ReadAs(ABlock,TRecordHelperSpecification_ID);

  TClassOf_ID : begin
    ReadAs(ABlock,TTypeSpecification_ID);
    TClassOf(ABlock).IsPacked:=ReadBoolean;
    TClassOf(ABlock).TypeIdentifier:=TTypeDeclaration(ReadRef);
  end;

  TObjectSpecification_ID : ReadAs(ABlock,TRecordSpecification_ID);

  TImplementation_ID : ReadAsUsesSection(TImplementation(ABlock));

  TStatements_ID : begin;
    ReadAsStatement(TStatements(ABlock));
    TStatements(ABlock).BeginEnd:=ReadBoolean;
    TStatements(ABlock).Items:=ReadAsStatements;
  end;

  TASMStatements_ID : begin
    ReadAs(ABlock,TStatements_ID);
    TASMStatements(ABlock).Items:=ReadAsStrings;
  end;

  TCasting_ID : begin
    ReadAsExpression(TCasting(ABlock));
    TCasting(ABlock).Expression:=ReadRefExpression;
  end;

  TAddressOf_ID : ReadAs(ABlock,TVariableDeclaration_ID);
  TAssignment_ID : begin
    ReadAsStatement(TAssignment(ABlock));
    TAssignment(ABlock).Left:=ReadRefExpression;
    TAssignment(ABlock).Right:=ReadRefExpression;
  end;

  TIf_ID : begin
    ReadAsStatement(TIf(ABlock));
    TIf(ABlock).Expression:=ReadRefExpression;
    TIf(ABlock).ThenPart:=TStatements(ReadRef);
    TIf(ABlock).ElsePart:=TStatements(ReadRef);
  end;

  TOn_ID : begin
    ReadAsStatement(TOn(ABlock));
    TOn(ABlock).Exception:=TVariableDeclaration(ReadRef);
    TOn(ABlock).DoPart:=TStatements(ReadRef);
    TOn(ABlock).ElsePart:=TStatements(ReadRef);
  end;

  TWhile_ID : begin
    ReadAsStatement(TWhile(ABlock));
    TWhile(ABlock).Expression:=ReadRefExpression;
    TWhile(ABlock).WhilePart:=TStatements(ReadRef);
  end;

  TWith_ID : begin
    ReadAsStatement(TWith(ABlock));
    TWith(ABlock).Items:=ReadAsExpressions;
    TWith(ABlock).WithPart:=TStatements(ReadRef);
  end;

  TFor_ID : begin
    ReadAsStatement(TFor(ABlock));
    TFor(ABlock).Enumerated:=ReadRefExpression;
    TFor(ABlock).Iterator:=TVariableDeclaration(ReadRef);
    TFor(ABlock).Start:=ReadRefExpression;
    TFor(ABlock).ToValue:=ReadRefExpression;
    TFor(ABlock).DownToValue:=ReadRefExpression;
    TFor(ABlock).Loop:=TStatements(ReadRef);
  end;

  TRepeat_ID : begin
    ReadAsStatement(TRepeat(ABlock));
    TRepeat(ABlock).RepeatPart:=TStatements(ReadRef);
    TRepeat(ABlock).Expression:=ReadRefExpression;
  end;

  TTry_ID : begin
    ReadAsStatement(TTry(ABlock));
    TTry(ABlock).Block:=TStatements(ReadRef);
    TTry(ABlock).FinallyPart:=TStatements(ReadRef);
    TTry(ABlock).ExceptPart:=TTryExcept(ReadRef);
    TTry(ABlock).ElsePart:=TStatements(ReadRef);
  end;

  TRaise_ID : begin
    ReadAsStatement(TRaise(ABlock));
    TRaise(ABlock).Expression:=ReadRefExpression;
  end;

  TBody_ID : begin
    ReadAsSection(TBody(ABlock));
    TBody(ABlock).Statements:=TStatements(ReadRef);
  end;

  TReferenceSpecification_ID : begin
    ReadAs(ABlock,TGenericSpec_ID);
    TReferenceSpecification(ABlock).Method:=TMethodSpecification(ReadRef);
  end;

  TProcedureSpecification_ID : ReadAsMethodParams(TProcedureSpecification(ABlock));

  TConstructorSpecification_ID : ReadAsMethodParams(TConstructorSpecification(ABlock));
  TDestructorSpecification_ID : ReadAsMethodSpec(TDestructorSpecification(ABlock));

  TFunctionSpecification_ID : begin
    ReadAsMethodParams(TFunctionSpecification(ABlock));
    TFunctionSpecification(ABlock).ResultValue:=TVariableDeclaration(ReadRef);
  end;

  TRecordOperatorSpec_ID : begin
    ReadAs(ABlock,TFunctionSpecification_ID);
    TRecordOperatorSpec(ABlock).Kind:=TRecordOperatorKind(ReadInteger);
  end;

  TCallExpression_ID : begin
    ReadAsExpression(TCallExpression(ABlock));
    TCallExpression(ABlock).Expression:=ReadRefExpression;
    TCallExpression(ABlock).Parameters:=ReadAsExpressions;
  end;

  TCall_ID : begin
    ReadAsStatement(TCall(ABlock));
    TCall(ABlock).Expression:=ReadRefExpression;
  end;

  TDereferenceExpression_ID : begin
    ReadAsExpression(TDereferenceExpression(ABlock));
    TDereferenceExpression(ABlock).Value:=ReadRefExpression;
  end;

  TFieldExpression_ID : begin
    ReadAsExpression(TFieldExpression(ABlock));
    TFieldExpression(ABlock).Value:=ReadRefExpression;
    TFieldExpression(ABlock).Field:=ReadRefExpression;
  end;

  TTypedMethod_ID : ReadAsGenericMethod(TTypedMethod(ABlock));

  TInheritedExpression_ID : begin
    ReadAsExpression(TInheritedExpression(ABlock));
    TInheritedExpression(ABlock).Method:=TMethodDeclaration(ReadRef);
    TInheritedExpression(ABlock).Expression:=ReadRefExpression;
  end;

  TCaseItem_ID : begin
    ReadAsBlock(ABlock);
    TCaseItem(ABlock).Condition:=ReadRefExpression;
    TCaseItem(ABlock).Body:=TStatements(ReadRef);
  end;

  TCase_ID : begin
    ReadAsStatement(TCase(ABlock));
    TCase(ABlock).Expression:=ReadRefExpression;
    TCase(ABlock).Cases:=ReadAsCaseArray;
    TCase(ABlock).ElsePart:=TStatements(ReadRef);
  end;

  TLabelDeclaration_ID : ReadAsIdentifier(TLabelDeclaration(ABlock));
  TLabelDeclarations_ID : ReadAsIdentifiers(TLabelDeclarations(ABlock));

  TLabelPlace_ID : begin
    ReadAsStatement(TLabelPlace(ABlock));
    TLabelPlace(ABlock).LabelIdentifier:=TLabelDeclaration(ReadRef);
  end;

  TGoto_ID : begin
    ReadAsStatement(TGoto(ABlock));
    TGoto(ABlock).TargetLabel:=TLabelDeclaration(ReadRef);
  end;

  TInitialization_ID : ReadAs(ABlock,TStatements_ID);
  TFinalization_ID :  ReadAs(ABlock,TStatements_ID);

  TImplementationModule_ID : begin
    ReadAs(ABlock,TTypeDeclaration_ID);
    TImplementationModule(ABlock).FileName:=ReadString;
    TImplementationModule(ABlock).ModuleImplementation:=TImplementation(ReadRef);
  end;

  TProgram_ID : ReadAsNativeModule(TProgram(ABlock));

//  TPackage_ID : ReadAsIdentifier(TPackage(ABlock));

  TPackages_ID : begin
    //ReadAsBlock(ABlock);
    ReadAsIdentifiers(TPackages(ABlock));
    //TPackageRequires(ABlock).Items:=ReadAsPackageRequires;
  end;

  TPackageContains_ID : ReadAs(ABlock,TUses_ID);

  TPackage_ID : begin
    ReadAsModule(TPackage(ABlock));
    ReadAs(TPackage(ABlock).PackageRequires,TPackages_ID);
    TPackage(ABlock).PackageContains:=TPackageContains(ReadRef);
  end;

  TTypeSpecification_ID : ReadAsBlock(ABlock); // Helper??

  TTypeDeclaration_ID : begin
    ReadAsIdentifier(TIdentifier(ABlock));

    TTypeDeclaration(ABlock).Alias:=TTypeDeclaration(ReadRef);

    TTypeDeclaration(ABlock).CodePage:=ReadRefExpression;

    TTypeDeclaration(ABlock).HasCodePage:=ReadBoolean;
    TTypeDeclaration(ABlock).IsForward:=ReadBoolean;

    if TTypeDeclaration(ABlock).Alias=nil then
       TTypeDeclaration(ABlock).Expression:=TTypeSpecification(ReadRef)
    else
    begin
      ReadRef; // nil
      TTypeDeclaration(ABlock).Expression:=TTypeDeclaration(ABlock).Alias.Expression;
    end;

    TTypeDeclaration(ABlock).TypeParameters:=ReadAsExpressions;
  end;

  TParameter_ID : begin
    ReadAs(ABlock,TVariableDeclaration_ID);
    TParameter(ABlock).IsConst:=ReadBoolean;
    TParameter(ABlock).IsVar:=ReadBoolean;
    TParameter(ABlock).IsOut:=ReadBoolean;
  end;

  TParameters_ID : ReadAs(ABlock,TVariableDeclarations_ID);
  TVisibilityIdent_ID : ReadAsIdentifier(TVisibilityIdent(ABlock));
  TRecordFieldsDeclarations_ID : ReadAs(ABlock,TVariableDeclarations_ID);
  TTryExcept_ID : ReadAs(ABlock,TStatements_ID);

  TGenericMethod_ID : begin
    ReadAsExpression(TGenericMethod(ABlock));
    TGenericMethod(ABlock).Method:=TMethodDeclaration(ReadRef);
    TGenericMethod(ABlock).TypeParameters:=ReadAsExpressions;
  end;
  end;
end;

procedure TCodeReader.ReadBlocks(const AModule: TModule);
var t : Integer;
    tmpID : Array of Integer;
    tmpClass : TBlockClass;
begin
  // Read BlockCount
  AModule.BlockCount:=ReadInteger;
  SetLength(AModule.Blocks,AModule.BlockCount);

  SetLength(tmpID,AModule.BlockCount);
  try
    // Read Block class IDs
    for t:=0 to AModule.BlockCount-1 do
    begin
      tmpID[t]:=ReadInteger;

      tmpClass:=IDClasses[tmpID[t]];

      if tmpClass=nil then
         raise Exception.Create('Wrong class (nil)');

      // Create each block
      AModule.Blocks[t]:=tmpClass.Create(nil);
      AModule.Blocks[t].Index:=t;
      AModule.Blocks[t].Module:=AModule;
    end;

//    tmpID[0]:=tmpID[0];

    // Read each Block
    for t:=0 to AModule.BlockCount-1 do
        ReadAs(AModule.Blocks[t],tmpID[t]);
  finally
    tmpID:=nil;
  end;
end;

class procedure TCodeReader.ReadFromFile(const AModule: TModule; const AFileName: String);
var f : TFileStream;
    c : TCodeReader;
begin
  f:=TFileStream.Create(AFileName,fmOpenRead+fmShareDenyWrite);
  try
    c:=TCodeReader.Create(f);
    try
      c.CurrentDir:=ExtractFilePath(AFileName);
      c.Module:=AModule;

      if AModule is TUnit then
         c.ReadAs(AModule,TUnit_ID)
      else
         c.ReadAs(AModule,TImplementationModule_ID);

    finally
      c.Free;
    end;
  finally
    f.Free;
  end;
end;

Constructor TCodeReader.Create(const AStream: TStream);
var V : Integer;
begin
  inherited Create(AStream,DefaultBufferSize);

  if ReadString<>TeeSignature then
     Raise Exception.Create('Bad signature read from stream');

  V:=ReadInteger;

  if V<CompatibleWith then
     Raise Exception.Create('Version is not compatible: '+IntToStr(V));
end;

class procedure TCodeReader.ReadFromFile(const APackage:TPackage; const AFileName: String);
var f : TFileStream;
    c : TCodeReader;
    tmpUnits : Array of Integer;
    t,L : Integer;
    tmpName : String;
    tmpPackage : TPackage;
    tmpUnit : TUnit;
begin
  f:=TFileStream.Create(AFileName,fmOpenRead+fmShareDenyWrite);
  try
    c:=TCodeReader.Create(f);
    try
      APackage.Clear;

      if APackage.Name='' then
         APackage.Name:=ExtractFileName(AFileName);

      // Required Packages
      L:=c.ReadInteger;

      for t:=0 to L-1 do
      begin
        tmpName:=c.ReadString;

        tmpPackage:=APackage.Language.Modules.PackageRequires.Find(tmpName);

        if tmpPackage=nil then
        begin
          tmpPackage:=TPackage.CreateName(APackage.PackageRequires,tmpName);
          tmpPackage.Language:=APackage.Language;

          tmpName:=tmpName+IntToStr(APackage.Language.CompilerVersion);

          if FileExists(ExtractFilePath(AFileName)+'\'+tmpName) then
             tmpName:=ExtractFilePath(AFileName)+'\'+tmpName
          else
             tmpName:=APackage.Language.FullPathOf(tmpName,'');

          if tmpName='' then
             raise Exception.Create('Cannot find package: '+tmpName)
          else
          begin
            //tmpPackage.Name:=ExtractFileName(tmpName);
            TCodeReader.ReadFromFile(tmpPackage,tmpName);
            APackage.Language.Modules.AddRequired(tmpPackage);
          end;
        end;

        APackage.AddRequired(tmpPackage);
      end;

      // Read Units indexes
      L:=c.ReadInteger;
      SetLength(tmpUnits,L);

      {
      for t:=0 to L-1 do
          tmpUnits[t]:=c.ReadInteger;
      }

      c.Package:=APackage;

      c.ReadBlocks(APackage);

      // Add Units to Package "Contains"
      for t:=0 to L-1 do
      begin
        tmpUnit:=TUnit(APackage.Blocks[tmpUnits[t]]);

        APackage.PackageContains.Add(tmpUnit);
        APackage.Language.Modules.PackageContains.Add(tmpUnit);
      end;

      //APackage.Language.LinkGlobals;

      // Scopes ??
    finally
      c.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TCodeWriter }

constructor TCodeWriter.Create(const AStream: TStream);
begin
  inherited Create(AStream,DefaultBufferSize);

  WriteString(TeeSignature);
  WriteInteger(Version);
end;

procedure TCodeWriter.WriteIdentifierNames(const AIdents:TIdentifiers);
var t : Integer;
begin
  WriteInteger(AIdents.Count);

  for t:=0 to AIdents.Count-1 do
      WriteString(AIdents[t].Name);
end;

procedure TCodeWriter.WriteAs(const ABlock: TBlock; const ID:Integer);

  procedure WriteRef(const ABlock:TBlock);
  var tmp : Integer;
  begin
    if ABlock=nil then
       WriteInteger(-1)
    else
    begin
      if ABlock is TModule then
      begin
        WriteInteger(-2);
        WriteInteger(TModule(ABlock).ModuleIndex);
      end
      else
      begin
        tmp:=TModule(ABlock.Module).ModuleIndex;
        WriteInteger(tmp);
        WriteInteger(ABlock.Index);
      end;
    end;
  end;

  procedure WriteAsBlock(const ABlock:TBlock);
  var t, H : Integer;
  begin
    // Position : TParserPosition;
    // Owner
    // Index
    // Owned ??
    // Parent ??

    if ABlock.Attributes=nil then
       H:=-1
    else
       H:=High(ABlock.Attributes);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(ABlock.Attributes[t]);
  end;

  procedure WriteAsExpression(const AExp:TExpression);
  begin
    WriteAsBlock(AExp);
    WriteRef(AExp.TypeIdentifier);

    //  ValueType : TTypeDeclaration; ?
  end;

  procedure WriteAsIdentifier(const AIdent:TIdentifier);
  begin
    WriteAsExpression(AIdent);
    WriteString(AIdent.Name);
    WriteInteger(Ord(AIdent.Visibility));
    WriteString(AIdent.AbsoluteRef);

    WriteBoolean(AIdent.IsDeprecated);

    WriteRef(AIdent.DeprecatedMessage);

    WriteBoolean(AIdent.IsInline);
    WriteBoolean(AIdent.IsPlatform);

    // Usage : Integer; ??
  end;

  procedure WriteAsBlocks(const ABlocks:Array of TBlock);
  var t, H : Integer;
  begin
    H:=High(ABlocks);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(ABlocks[t]);
  end;

  procedure WriteAsSection(const ASection:TSection);
  begin
    WriteAsBlock(ASection);
    WriteBoolean(ASection.Bodies);

    WriteRef(ASection.Constants);
    WriteRef(ASection.Methods);
    WriteRef(ASection.Types);
    WriteRef(ASection.Variables);
    WriteRef(ASection.Labels);
    WriteRef(ASection.ResourceStrings);

    WriteAsBlocks(ASection.Ordered);
  end;

  procedure WriteAsUsesSection(const AUsesSection:TUsesSection);
  begin
    WriteAs(AUsesSection.UsesUnits,TUses_ID);
    WriteAsSection(AUsesSection);
  end;

  procedure WriteAsIdentifiers(const AIdents:TIdentifiers);
  var t : Integer;
  begin
    WriteInteger(AIdents.Count);

    for t:=0 to AIdents.Count-1 do
        WriteRef(AIdents[t]);
  end;

  procedure WriteAsUsedNames(const AUses:TUses);
  var t : Integer;
  begin
    WriteInteger(AUses.Count);

    for t:=0 to AUses.Count-1 do
        WriteString(AUses[t].Name);
  end;

  procedure WriteAsNativeModule(const AModule:TNativeModule);
  begin
    WriteAs(AModule,TImplementationModule_ID);
    WriteRef(AModule.Main);
  end;

  procedure WriteAsTypeRedirect(const ATypeRedirect:TTypeRedirect);
  begin
    WriteAs(ATypeRedirect,TTypeSpecification_ID);
    WriteRef(ATypeRedirect.TypeIdentifier);
  end;

  procedure WriteAsExpressions(const AExp:Array of TExpression);
  var t,H : Integer;
  begin
    H:=High(AExp);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(AExp[t]);
  end;

  procedure WriteAsParameters(const AParams:TParameters);
  var t,L : Integer;
  begin
    L:=AParams.Count;

    WriteInteger(L);

    for t:=0 to L-1 do
        WriteRef(AParams[t]);
  end;

  procedure WriteAsTypeSpecs(const ASpecs:TTypeSpecifications);
  var t, H : Integer;
  begin
    H:=High(ASpecs);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(ASpecs[t]);
  end;

  procedure WriteAsTypes(const ATypes:TTypeDeclarationArray);
  var t, H : Integer;
  begin
    H:=High(ATypes);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(ATypes[t]);
  end;

  procedure WriteAsRecordSpec(const ARecord:TRecordSpecification);
  var t : Integer;
  begin
    WriteAs(ARecord,TGenericSpec_ID);
    WriteRef(ARecord.PropertyDefault);

    for t:=Ord(Low(TRecordOperatorKind)) to Ord(High(TRecordOperatorKind)) do
        WriteAsTypeSpecs(ARecord.Operators[TRecordOperatorKind(t)]);

    WriteBoolean(ARecord.IsPacked);

    WriteAsTypes(ARecord.Ancestors);
    WriteRef(ARecord.RecordType);
    WriteRef(ARecord.Outer);
    WriteRef(ARecord.Module);

    WriteRef(ARecord.Constants);
    WriteRef(ARecord.Fields);
    WriteRef(ARecord.Methods);
    WriteRef(ARecord.Types);

    WriteAsBlocks(ARecord.Ordered);
  end;

  procedure WriteAsStatements(const AItems:TStatementArray);
  var t, H : Integer;
  begin
    H:=High(AItems);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(AItems[t]);
  end;

  procedure WriteAsStrings(const AItems:Array of String);
  var t, H : Integer;
  begin
    H:=High(AItems);

    WriteInteger(H);

    for t:=0 to H do
        WriteString(AItems[t]);
  end;

  procedure WriteAsStatement(const AStatement:TStatement);
  begin
    WriteAsBlock(AStatement);
  end;

  procedure WriteAsMethodSpec(const AMethod:TMethodSpecification);
  begin
    WriteAs(AMethod,TGenericSpec_ID);

    // !!!  WriteInteger( Directives : TMethodDirectives;

    WriteRef(AMethod.DeprecatedMessage);
    WriteRef(AMethod.OfRecord);

    WriteBoolean(AMethod.OfObject);
    WriteBoolean(AMethod.IsClass);

    WriteBoolean(AMethod.IsExternal);
    WriteBoolean(AMethod.IsDelayed);

    WriteRef(AMethod.ExternalDLL);
    WriteRef(AMethod.ExternalName);

    WriteRef(AMethod.DispatchID);
    WriteRef(AMethod.MessageField);

    WriteString(AMethod.Referenced);

    WriteRef(AMethod.Body);
  end;

  procedure WriteAsMethodParams(const AMethod:TMethodParams);
  begin
    WriteAsMethodSpec(AMethod);
    WriteInteger(AMethod.FixedParameters);
    WriteBoolean(AMethod.VariableParameters);
    WriteRef(AMethod.Parameters);
  end;

  procedure WriteAsGenericMethod(const AMethod:TGenericMethod);
  begin
    WriteAsExpression(AMethod);
    WriteRef(AMethod.Method);
    WriteAsExpressions(AMethod.TypeParameters);
  end;

  procedure WriteAsCaseArray(const ACases:Array of TCaseItem);
  var t, H : Integer;
  begin
    H:=High(ACases);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(ACases[t]);
  end;

  {
  procedure WriteAsPackageRequires(const AItems:Array of TPackageRequire);
  var t, H : Integer;
  begin
    H:=High(AItems);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(AItems[t]);
  end;
  }

  procedure WriteAsModule(const AModule:TModule);
  begin
    WriteAs(AModule,TTypeDeclaration_ID);
    WriteString(AModule.FileName);
  end;

  procedure WriteUnit(const AUnit:TUnit);

    function InUses(const AModule:TModule):Boolean;
    begin
      result:=(AUnit.UnitInterface.UsesUnits.IndexOf(AModule)<>-1) or
              (AUnit.ModuleImplementation.UsesUnits.IndexOf(AModule)<>-1);
    end;

  var t,N : Integer;
  begin
    AUnit.ModuleIndex:=0;

    N:=Length(AUnit.Used);
    WriteInteger(N-1);

    for t:=1 to N-1 do
    begin
      WriteBoolean(not InUses(AUnit.Used[t]));
      WriteString(AUnit.Used[t].Name);
      AUnit.Used[t].ModuleIndex:=t;
    end;

    WriteBlocks(AUnit);

    WriteAs(AUnit,TImplementationModule_ID);
    WriteRef(AUnit.UnitInterface);
    WriteRef(AUnit.UnitInitialization);
    WriteRef(AUnit.UnitFinalization);
  end;

var t,H : Integer;
begin
  case ID  of
  TUnit_ID : WriteUnit(TUnit(ABlock));

  TInterface_ID : WriteAsUsesSection(TInterface(ABlock));
  TUses_ID : WriteAsUsedNames(TUses(ABlock));

  TConstantDeclarations_ID : WriteAsIdentifiers(TConstantDeclarations(ABlock));

  TConstantDeclaration_ID : begin
    WriteAsIdentifier(TConstantDeclaration(ABlock));
    WriteBoolean(TConstantDeclaration(ABlock).NeedsType);
    WriteBoolean(TConstantDeclaration(ABlock).AutomaticType);
    WriteBoolean(TConstantDeclaration(ABlock).OptionalValue);
    WriteRef(TConstantDeclaration(ABlock).Value);
    WriteBoolean(TConstantDeclaration(ABlock).IsClass);
  end;

  TUnitScope_ID : WriteAsIdentifier(TUnitScope(ABlock));

  TDirective_ID : begin
    WriteAs(ABlock,TAttribute_ID);
    WriteInteger(Ord(TDirective(ABlock).Kind));
  end;

  TMethodDeclarations_ID : WriteAsIdentifiers(TMethodDeclarations(ABlock));
  TVariableDeclarations_ID : WriteAsIdentifiers(TVariableDeclarations(ABlock));
  TTypeDeclarations_ID :  WriteAsIdentifiers(TTypeDeclarations(ABlock));
  TArraySpecification_ID : begin
    WriteAs(ABlock,TRecordSpecification_ID);

    WriteBoolean(TArraySpecification(ABlock).IsPacked);
    WriteRef(TArraySpecification(ABlock).CreateConstructor);
    WriteRef(TArraySpecification(ABlock).Expression);

    WriteAsExpressions(TArraySpecification(ABlock).Dimensions);
  end;

  TNumber_ID : begin
    WriteAs(ABlock,TConstantDeclaration_ID);
    WriteBoolean(TNumber(ABlock).Hexadecimal);
    WriteInteger(TNumber(ABlock).Value);
  end;

  TString_ID : begin
    WriteAs(ABlock,TConstantDeclaration_ID);
    WriteString(TString(ABlock).Value);
  end;

  TMethodDeclaration_ID : WriteAs(ABlock,TVariableDeclaration_ID);

  TAttribute_ID : begin
    WriteAsBlock(ABlock);
    WriteString(TAttribute(ABlock).Text);
  end;

  TLibrary_ID : WriteAsNativeModule(TLibrary(ABlock));
  TVariableDeclaration_ID : WriteAs(ABlock,TConstantDeclaration_ID);
  TThreadVariable_ID : WriteAs(ABlock,TVariableDeclaration_ID);
  TExportDeclaration_ID : begin
    WriteAsIdentifier(TExportDeclaration(ABlock));
    WriteRef(TExportDeclaration(ABlock).Method);
  end;

  TCaseVariableItem_ID : begin
    WriteAs(ABlock,TVariableDeclaration_ID);
    WriteRef(TCaseVariableItem(ABlock).Expression);
  end;

  TCaseVariable_ID : begin
    WriteAs(ABlock,TVariableDeclaration_ID);
    WriteAsIdentifiers(TCaseVariable(ABlock).Fields);
  end;

  TScopedType_ID : begin
    WriteAs(ABlock,TTypeDeclaration_ID);
    WriteRef(TScopedType(ABlock).Field);
  end;

  TTypeParameter_ID : begin
    WriteAs(ABlock,TTypeDeclaration_ID);
    WriteInteger(TTypeParameter(ABlock).Index);

    H:=High(TTypeParameter(ABlock).Constraints);

    WriteInteger(H);

    for t:=0 to H do
        WriteRef(TTypeParameter(ABlock).Constraints[t]);
  end;

  TTypePointerOf_ID : WriteAsTypeRedirect(TTypePointerOf(ABlock));
  TTypeTypeOf_ID :  WriteAsTypeRedirect(TTypePointerOf(ABlock));

  TArrayValues_ID : begin
    WriteAsExpression(TArrayValues(ABlock));
    WriteInteger(TArrayValues(ABlock).DimensionIndex);
    WriteAsExpressions(TArrayValues(ABlock).Items);
  end;

  TItemExpression_ID : begin
    WriteAsExpression(TItemExpression(ABlock));
    WriteRef(TItemExpression(ABlock).Value);
    WriteAsExpressions(TItemExpression(ABlock).Items);
  end;

  TTypeItem_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteRef(TTypeItem(ABlock).Item);
  end;

  TSetItem_ID : begin
    WriteAs(ABlock,TTypeDeclaration_ID);
    WriteRef(TSetItem(ABlock).Value);
  end;

  TSetSpecification_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteBoolean(TSetSpecification(ABlock).IsScoped);
    WriteBoolean(TSetSpecification(ABlock).IsSetOf);
    WriteAs(TSetSpecification(ABlock).Items,TTypeDeclarations_ID);
  end;

  TRangeExpression_ID : begin
    WriteAsExpression(TRangeExpression(ABlock));
    WriteRef(TRangeExpression(ABlock).StartRange);
    WriteRef(TRangeExpression(ABlock).EndRange);
  end;

  TSetOfSpecification_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteRef(TSetOfSpecification(ABlock).TypeIdentifier);
  end;

  TSetRange_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteRef(TSetRange(ABlock).Range);
  end;

  TTypeRange_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteRef(TTypeRange(ABlock).Range);
  end;

  TResourceStrings_ID : WriteAs(ABlock,TConstantDeclarations_ID);

  TFloatNumber_ID : begin
    WriteAs(ABlock,TConstantDeclaration_ID);
    WriteBoolean(TFloatNumber(ABlock).Exponent);
    WriteFloat(TFloatNumber(ABlock).Value);
    WriteString(TFloatNumber(ABlock).ValueString);
  end;

  TChar_ID : begin
    WriteAs(ABlock,TConstantDeclaration_ID);
    WriteChar(TChar(ABlock).Value);
  end;

  TBoolean_ID : begin
    WriteAs(ABlock,TConstantDeclaration_ID);
    WriteBoolean(TBoolean(ABlock).Value);
  end;

  TOperatorExpression_ID : begin
    WriteAsExpression(TOperatorExpression(ABlock));
    WriteInteger(Ord(TOperatorExpression(ABlock).Operat));
    WriteRef(TOperatorExpression(ABlock).Left);
    WriteRef(TOperatorExpression(ABlock).Right);
  end;

  TNestedExpression_ID : begin
    WriteAsExpression(TNestedExpression(ABlock));
    WriteRef(TNestedExpression(ABlock).Expression);
  end;

  TStringFormatExpression_ID : begin
    WriteAsExpression(TStringFormatExpression(ABlock));
    WriteRef(TStringFormatExpression(ABlock).Expression);
    WriteRef(TStringFormatExpression(ABlock).Width);
    WriteRef(TStringFormatExpression(ABlock).Decimals);
  end;

  TPropertyDeclaration_ID : begin
    WriteAs(ABlock,TVariableDeclaration_ID);
    WriteBoolean(TPropertyDeclaration(ABlock).IsDefault);
    WriteBoolean(TPropertyDeclaration(ABlock).NoDefault);
    WriteRef(TPropertyDeclaration(ABlock).Index);
    WriteRef(TPropertyDeclaration(ABlock).PropertyType);
    WriteRef(TPropertyDeclaration(ABlock).Indexes);

    WriteRef(TPropertyDeclaration(ABlock).ReadPart);
    WriteRef(TPropertyDeclaration(ABlock).WritePart);

    WriteRef(TPropertyDeclaration(ABlock).Stored);
    WriteRef(TPropertyDeclaration(ABlock).DefaultValue);

    WriteAsExpressions(TPropertyDeclaration(ABlock).ImplementsInterfaces);

    WriteRef(TPropertyDeclaration(ABlock).Ancestor);

    // Pending: Move to TDispatchInterfaceProperty
    WriteBoolean(TPropertyDeclaration(ABlock).ReadOnly);
    WriteBoolean(TPropertyDeclaration(ABlock).WriteOnly);
    WriteRef(TPropertyDeclaration(ABlock).DispatchId);

    WriteRef(TPropertyDeclaration(ABlock).RecordType);
  end;

  TDispatchInterfaceProperty_ID : WriteAs(ABlock,TPropertyDeclaration_ID);

  TRecordInstance_ID : begin
    WriteAs(ABlock,TVariableDeclaration_ID);
    WriteRef(TRecordInstance(ABlock).Fields);
  end;

  TGenericSpec_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteAsExpressions(TGenericSpec(ABlock).TypeParameters);
  end;

  TGenericTypeDeclaration_ID : WriteAs(ABlock,TTypeDeclaration_ID);

  TRecordSpecification_ID : WriteAsRecordSpec(TRecordSpecification(ABlock));

  TInterfaceSpecification_ID : WriteAs(ABlock,TRecordSpecification_ID);
  TDispatchInterfaceSpecification_ID : WriteAs(ABlock,TInterfaceSpecification_ID);

  TClassSpecification_ID : begin;
    WriteAs(ABlock,TRecordSpecification_ID);
    WriteBoolean(TClassSpecification(ABlock).IsAbstract);
    WriteBoolean(TClassSpecification(ABlock).IsSealed);
  end;

  TRecordHelperSpecification_ID : begin
    WriteAs(ABlock,TRecordSpecification_ID);
    WriteRef(TRecordHelperSpecification(ABlock).TypeIdentifier);
  end;

  TClassHelperSpecification_ID : WriteAs(ABlock,TRecordHelperSpecification_ID);

  TClassOf_ID : begin
    WriteAs(ABlock,TTypeSpecification_ID);
    WriteBoolean(TClassOf(ABlock).IsPacked);
    WriteRef(TClassOf(ABlock).TypeIdentifier);
  end;

  TObjectSpecification_ID : WriteAs(ABlock,TRecordSpecification_ID);

  TImplementation_ID : WriteAsUsesSection(TImplementation(ABlock));

  TStatements_ID : begin;
    WriteAsStatement(TStatements(ABlock));
    WriteBoolean(TStatements(ABlock).BeginEnd);
    WriteAsStatements(TStatements(ABlock).Items);
  end;

  TASMStatements_ID : begin
    WriteAs(ABlock,TStatements_ID);
    WriteAsStrings(TASMStatements(ABlock).Items);
  end;

  TCasting_ID : begin
    WriteAsExpression(TCasting(ABlock));
    WriteRef(TCasting(ABlock).Expression);
  end;

  TAddressOf_ID : WriteAs(ABlock,TVariableDeclaration_ID);
  TAssignment_ID : begin
    WriteAsStatement(TAssignment(ABlock));
    WriteRef(TAssignment(ABlock).Left);
    WriteRef(TAssignment(ABlock).Right);
  end;

  TIf_ID : begin
    WriteAsStatement(TIf(ABlock));
    WriteRef(TIf(ABlock).Expression);
    WriteRef(TIf(ABlock).ThenPart);
    WriteRef(TIf(ABlock).ElsePart);
  end;

  TOn_ID : begin
    WriteAsStatement(TOn(ABlock));
    WriteRef(TOn(ABlock).Exception);
    WriteRef(TOn(ABlock).DoPart);
    WriteRef(TOn(ABlock).ElsePart);
  end;

  TWhile_ID : begin
    WriteAsStatement(TWhile(ABlock));
    WriteRef(TWhile(ABlock).Expression);
    WriteRef(TWhile(ABlock).WhilePart);
  end;

  TWith_ID : begin
    WriteAsStatement(TWith(ABlock));
    WriteAsExpressions(TWith(ABlock).Items);
    WriteRef(TWith(ABlock).WithPart);
  end;

  TFor_ID : begin
    WriteAsStatement(TFor(ABlock));
    WriteRef(TFor(ABlock).Enumerated);
    WriteRef(TFor(ABlock).Iterator);
    WriteRef(TFor(ABlock).Start);
    WriteRef(TFor(ABlock).ToValue);
    WriteRef(TFor(ABlock).DownToValue);
    WriteRef(TFor(ABlock).Loop);
  end;

  TRepeat_ID : begin
    WriteAsStatement(TRepeat(ABlock));
    WriteRef(TRepeat(ABlock).RepeatPart);
    WriteRef(TRepeat(ABlock).Expression);
  end;

  TTry_ID : begin
    WriteAsStatement(TTry(ABlock));
    WriteRef(TTry(ABlock).Block);
    WriteRef(TTry(ABlock).FinallyPart);
    WriteRef(TTry(ABlock).ExceptPart);
    WriteRef(TTry(ABlock).ElsePart);
  end;

  TRaise_ID : begin
    WriteAsStatement(TRaise(ABlock));
    WriteRef(TRaise(ABlock).Expression);
  end;

  TBody_ID : begin
    WriteAsSection(TBody(ABlock));
    WriteRef(TBody(ABlock).Statements);
  end;

  TReferenceSpecification_ID : begin
    WriteAs(ABlock,TGenericSpec_ID);
    WriteRef(TReferenceSpecification(ABlock).Method);
  end;

  TProcedureSpecification_ID : WriteAsMethodParams(TProcedureSpecification(ABlock));

  TConstructorSpecification_ID : WriteAsMethodParams(TConstructorSpecification(ABlock));
  TDestructorSpecification_ID : WriteAsMethodSpec(TDestructorSpecification(ABlock));

  TFunctionSpecification_ID : begin
    WriteAsMethodParams(TFunctionSpecification(ABlock));
    WriteRef(TFunctionSpecification(ABlock).ResultValue);
  end;

  TRecordOperatorSpec_ID : begin
    WriteAs(ABlock,TFunctionSpecification_ID);
    WriteInteger(Ord(TRecordOperatorSpec(ABlock).Kind));
  end;

  TCallExpression_ID : begin
    WriteAsExpression(TCallExpression(ABlock));
    WriteRef(TCallExpression(ABlock).Expression);
    WriteAsExpressions(TCallExpression(ABlock).Parameters);
  end;

  TCall_ID : begin
    WriteAsStatement(TCall(ABlock));
    WriteRef(TCall(ABlock).Expression);
  end;

  TDereferenceExpression_ID : begin
    WriteAsExpression(TDereferenceExpression(ABlock));
    WriteRef(TDereferenceExpression(ABlock).Value);
  end;

  TFieldExpression_ID : begin
    WriteAsExpression(TFieldExpression(ABlock));
    WriteRef(TFieldExpression(ABlock).Value);
    WriteRef(TFieldExpression(ABlock).Field);
  end;

  TTypedMethod_ID : WriteAsGenericMethod(TTypedMethod(ABlock));

  TInheritedExpression_ID : begin
    WriteAsExpression(TInheritedExpression(ABlock));
    WriteRef(TInheritedExpression(ABlock).Method);
    WriteRef(TInheritedExpression(ABlock).Expression);
  end;

  TCaseItem_ID : begin
    WriteAsBlock(ABlock);
    WriteRef(TCaseItem(ABlock).Condition);
    WriteRef(TCaseItem(ABlock).Body);
  end;

  TCase_ID : begin
    WriteAsStatement(TCase(ABlock));
    WriteRef(TCase(ABlock).Expression);
    WriteAsCaseArray(TCase(ABlock).Cases);
    WriteRef(TCase(ABlock).ElsePart);
  end;

  TLabelDeclaration_ID : WriteAsIdentifier(TLabelDeclaration(ABlock));
  TLabelDeclarations_ID : WriteAsIdentifiers(TLabelDeclarations(ABlock));

  TLabelPlace_ID : begin
    WriteAsStatement(TLabelPlace(ABlock));
    WriteRef(TLabelPlace(ABlock).LabelIdentifier);
  end;

  TGoto_ID : begin
    WriteAsStatement(TGoto(ABlock));
    WriteRef(TGoto(ABlock).TargetLabel);
  end;

  TInitialization_ID : WriteAs(ABlock,TStatements_ID);
  TFinalization_ID   : WriteAs(ABlock,TStatements_ID);

  TImplementationModule_ID : begin
    WriteAs(ABlock,TTypeDeclaration_ID);
    WriteString(TImplementationModule(ABlock).FileName);
    WriteRef(TImplementationModule(ABlock).ModuleImplementation);
  end;

  TProgram_ID : WriteAsNativeModule(TProgram(ABlock));
//  TPackage_ID : WriteAsIdentifier(TPackageRequire(ABlock));

  TPackages_ID : begin
//    WriteAsBlock(ABlock);
    WriteAsIdentifiers(TPackages(ABlock));
//    WriteAsPackageRequires(TPackageRequires(ABlock).Items);
  end;

  TPackageContains_ID : WriteAs(ABlock,TUses_ID);

  TPackage_ID : begin
    WriteAsModule(TPackage(ABlock));
    WriteAs(TPackage(ABlock).PackageRequires,TPackages_ID);
    WriteRef(TPackage(ABlock).PackageContains);
  end;

  TTypeSpecification_ID : WriteAsBlock(ABlock); // Helper??

  TTypeDeclaration_ID : begin
    WriteAsIdentifier(TIdentifier(ABlock));

    WriteRef(TTypeDeclaration(ABlock).Alias);

    WriteRef(TTypeDeclaration(ABlock).CodePage);

    WriteBoolean(TTypeDeclaration(ABlock).HasCodePage);
    WriteBoolean(TTypeDeclaration(ABlock).IsForward);

    if TTypeDeclaration(ABlock).Alias=nil then
       WriteRef(TTypeDeclaration(ABlock).Expression)
    else
       WriteRef(nil);

    WriteAsExpressions(TTypeDeclaration(ABlock).TypeParameters);
  end;

  TParameter_ID : begin
    WriteAs(ABlock,TVariableDeclaration_ID);
    WriteBoolean(TParameter(ABlock).IsConst);
    WriteBoolean(TParameter(ABlock).IsVar);
    WriteBoolean(TParameter(ABlock).IsOut);
  end;

  TParameters_ID : WriteAs(ABlock,TVariableDeclarations_ID);
  TVisibilityIdent_ID : WriteAsIdentifier(TVisibilityIdent(ABlock));
  TRecordFieldsDeclarations_ID : WriteAs(ABlock,TVariableDeclarations_ID);
  TTryExcept_ID : WriteAs(ABlock,TStatements_ID);
  TGenericMethod_ID : begin
    WriteAsExpression(TGenericMethod(ABlock));
    WriteRef(TGenericMethod(ABlock).Method);
    WriteAsExpressions(TGenericMethod(ABlock).TypeParameters);
  end;
  end;
end;

procedure TCodeWriter.WriteBlocks(const AModule: TModule);
var t,
    L : Integer;
    tmpID : Array of Integer;
    S : String;
begin
  // Block count
  L:=AModule.BlockCount;
  WriteInteger(L);

  SetLength(tmpID,L);
  try
    // Block class IDs
    for t:=0 to L-1 do
    begin
      tmpID[t]:=ClassID(AModule.Blocks[t]);

      if tmpID[t]=-1 then
      begin
        S:='TCodeWriter Block ID missing: '+AModule.Blocks[t].ClassName;
        //OutputDebugString(PWideChar(S));
        raise Exception.Create(S);
      end;

      WriteInteger(tmpID[t]);
    end;

    // Blocks
    for t:=0 to L-1 do
        WriteAs(AModule.Blocks[t],tmpID[t]);

  finally
    tmpID:=nil;
  end;
end;

class procedure TCodeWriter.WriteToFile(const AModule: TModule; const AFileName: String);
var f : TFileStream;
    c : TCodeWriter;
begin
  f:=TFileStream.Create(AFileName,fmCreate+fmShareDenyWrite);
  try
    c:=TCodeWriter.Create(f);
    try
      if AModule is TUnit then
         c.WriteAs(AModule,TUnit_ID)
      else
         c.WriteAs(AModule,TImplementationModule_ID);

    finally
      c.Free;
    end;
  finally
    f.Free;
  end;
end;

class procedure TCodeWriter.WriteToFile(const APackage: TPackage; const AFileName: String);
var f : TFileStream;
    c : TCodeWriter;
    //t,
    L : Integer;
begin
  f:=TFileStream.Create(AFileName,fmCreate+fmShareDenyWrite);
  try
    c:=TCodeWriter.Create(f);
    try
      c.Package:=APackage;

      c.WriteIdentifierNames(APackage.PackageRequires);

      // Units
      L:=APackage.PackageContains.Count;
      c.WriteInteger(L);

      (*
      for t:=0 to L-1 do
          c.WriteAs(APackage.PackageContains[t],TUnit_ID);
          //c.WriteInteger(APackage.PackageContains[t].Index);
      *)

      c.WriteBlocks(APackage);

    finally
      c.Free;
    end;
  finally
    f.Free;
  end;
end;

initialization
finalization
  IDs.Free;
end.
