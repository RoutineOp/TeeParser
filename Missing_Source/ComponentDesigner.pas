unit ComponentDesigner;

interface

uses
  Windows, Classes, DesignIntf;

type
  TEnvironment=class
  public
    GetMainWindowSize:TRect;
    GetBaseRegKey:String;
    procedure ModalEdit(C:Char; AOwner:TComponent);
  end;

  IComponentDesigner=interface
    Environment : TEnvironment;
    procedure SetSelection(Designer:IDesigner; O:TObject; Components:IDesignerSelections);
  end;
  
function ActiveDesigner:IComponentDesigner;

implementation

function ActiveDesigner:IComponentDesigner;
begin
  result:=nil;
end;

end.
