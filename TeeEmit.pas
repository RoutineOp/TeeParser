unit TeeEmit;

interface

uses
  TeeCode;

type
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

implementation

uses
  SysUtils;

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

end.
