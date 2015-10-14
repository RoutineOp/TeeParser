unit DiagramSupport;

interface

uses
  Classes, DB, TreeIntf;

type
  TIsland = class
  public
    Sprig : TComponentSprig;
    function VisibleTreeParent: Boolean; virtual; abstract;
  end;

  TIslandClass = class of TIsland;

  TDatasetIsland=class(TIsland)
  end;

  TMasterDetailBridge = class
  public
    Omega : TComponentSprig;

    class function GetOmegaSource(AItem: TPersistent): TDataSource; virtual; abstract;
    class procedure SetOmegaSource(AItem: TPersistent; ADataSource: TDataSource); virtual; abstract;
    function Caption: string; virtual; abstract;
    class function OmegaIslandClass: TIslandClass; virtual; abstract;
  end;

  TMasterDetailBridgeClass=class of TMasterDetailBridge;

procedure RegisterIslandType(Sprig:TComponentSprig; Island:TIslandClass);
procedure RegisterBridgeType(A,B:TIslandClass; C:TMasterDetailBridgeClass);

implementation

procedure RegisterIslandType(Sprig:TComponentSprig; Island:TIsland);
begin
end;

procedure RegisterBridgeType(A,B:TIslandClass; C:TMasterDetailBridgeClass);
begin
end;

end.
