unit ToolWnds;

interface

uses
  DsgnWnds, DesignEditors, ExtCtrls, ComCtrls;

type
  TToolbarDesignWindow=class(TDesignWindow)
    Splitter1: TSplitter;
    Toolbar1: TToolbar;
  public
    ActiveRoot:TComponentEditor;
    LargeButtons : Boolean;
  end;
  
implementation

end.
