unit TeeRunner;

interface

uses
  TeePascal;

type
  TRunner=class
  public
    Current : TBlock;

    procedure Next;
    procedure Pause;
    procedure Start(const ABlock:TBlock);
    procedure Stop;
  end;

implementation

{ TRunner }

procedure TRunner.Next;
begin
//  Current:=Current.Next;
end;

procedure TRunner.Pause;
begin

end;

procedure TRunner.Start(const ABlock:TBlock);
begin
  Current:=ABlock;
end;

procedure TRunner.Stop;
begin
  Current:=nil;
end;

end.
