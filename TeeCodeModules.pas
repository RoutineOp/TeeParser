unit TeeCodeModules;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, TeeCode;

type
  TSelectModule=procedure(Sender:TObject; const AModule:TModule) of object;

  TCodeModules = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    LBUnits: TListBox;
    ByUses: TTreeView;
    UsedBy: TTreeView;
    CBSection: TComboBox;
    TreePackages: TTreeView;
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBSectionChange(Sender: TObject);
    procedure UsedByExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure LBUnitsClick(Sender: TObject);
    procedure ByUsesChange(Sender: TObject; Node: TTreeNode);
    procedure UsedByChange(Sender: TObject; Node: TTreeNode);
    procedure TreePackagesClick(Sender: TObject);
  private
    { Private declarations }

    FOnSelect : TSelectModule;

    procedure FillContains(const AContains:TPackageContains; const AList:TListBox);
    procedure FillRequires(const ARequires:TPackages; const ATree:TTreeView);
    procedure FillUses(const AContains:TPackageContains; const ATree:TTreeView);
    procedure FillUsedBy(const AContains:TPackageContains; const ATree:TTreeView);
  public
    { Public declarations }

    Language : TLanguage;

    class procedure Edit(const AOwner:TComponent; const ALanguage:TLanguage);

    property OnSelect:TSelectModule read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

procedure TCodeModules.ByUsesChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node.Data<>nil) and Assigned(FOnSelect) then
     FOnSelect(Self,TUnit(Node.Data));
end;

procedure TCodeModules.CBSectionChange(Sender: TObject);
begin
  ByUses.Items.Clear;
  UsedBy.Items.Clear;

  PageControl1Change(Self);
end;

procedure TCodeModules.FillUsedBy(const AContains:TPackageContains; const ATree:TTreeView);
var t : Integer;
    tmp : TModule;
    tmpNode : TTreeNode;
begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    for t:=0 to AContains.Count-1 do
    begin
      tmp:=AContains[t];

      tmpNode:=ATree.Items.AddChildObject(nil,tmp.Name,tmp);
      ATree.Items.AddChild(tmpNode,'');
    end;
  finally
    ATree.Items.EndUpdate;
  end;
end;

procedure TCodeModules.FillUses(const AContains:TPackageContains; const ATree:TTreeView);

  procedure AddUses(const ANode:TTreeNode; const AModule:TModule);

    procedure AddUnits(const ANode:TTreeNode; const AUses:TUses);
    var t : Integer;
        tmp : TModule;
    begin
      for t:=0 to AUses.Count-1 do
      begin
        tmp:=AUses[t];
        ByUses.Items.AddChildObject(ANode,tmp.Name,tmp);
      end;
    end;

    procedure TryAdd(const ASection:TUsesSection; const AName:String);
    begin
      if (ASection<>nil) and (ASection.UsesUnits<>nil) and (ASection.UsesUnits.Count>0) then
          AddUnits(ByUses.Items.AddChild(ANode,AName),ASection.UsesUnits);
    end;

  var tmp : TUses;
  begin
    if CBSection.ItemIndex=0 then
    begin
      if AModule is TImplementationModule then
         TryAdd(TImplementationModule(AModule).ModuleImplementation,'implementation');

      if AModule is TUnit then
         TryAdd(TUnit(AModule).UnitInterface,'interface');
    end
    else
    begin
      if (CBSection.ItemIndex=1) or (CBSection.ItemIndex=3) then
      begin
        if AModule is TUnit then
        begin
          tmp:=TUnit(AModule).UnitInterface.UsesUnits;

          if (tmp<>nil) and (tmp.Count>0) then
              AddUnits(ANode,tmp);
        end;
      end;

      if (CBSection.ItemIndex=2) or (CBSection.ItemIndex=3) then
      if AModule is TImplementationModule then
      begin
        if TImplementationModule(AModule).ModuleImplementation<>nil then
        begin
          tmp:=TImplementationModule(AModule).ModuleImplementation.UsesUnits;

          if (tmp<>nil) and (tmp.Count>0) then
              AddUnits(ANode,tmp);
        end;
      end;
    end;
  end;

var t : Integer;
    tmp : TModule;
    tmpNode : TTreeNode;
begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    for t:=0 to AContains.Count-1 do
    begin
      tmp:=AContains[t];

      tmpNode:=ATree.Items.AddChildObject(nil,tmp.Name,tmp);

      AddUses(tmpNode,tmp);
    end;
  finally
    ATree.Items.EndUpdate;
  end;
end;

procedure TCodeModules.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;
end;

procedure TCodeModules.FillContains(const AContains:TPackageContains; const AList:TListBox);
var t : Integer;
    tmp : TModule;
begin
  AList.Items.BeginUpdate;
  try
    AList.Items.Clear;

    for t:=0 to AContains.Count-1 do
    begin
      tmp:=AContains[t];
      AList.Items.AddObject(tmp.Name,tmp);
    end;

    AList.Sorted:=True;
  finally
    AList.Items.EndUpdate;
  end;
end;

procedure TCodeModules.FillRequires(const ARequires: TPackages;
  const ATree: TTreeView);

  procedure DoFill(const ANode:TTreeNode; const ARequires:TPackages);
  var t : Integer;
      tmp : TTreeNode;
      tmpPackage : TPackage;
  begin
    if ARequires<>nil then
    for t:=0 to ARequires.Count-1 do
    begin
      tmpPackage:=ARequires[t];
      tmp:=ATree.Items.AddChildObject(ANode,tmpPackage.Name,tmpPackage);
      DoFill(tmp,tmpPackage.PackageRequires);
    end;
  end;

begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    DoFill(nil,ARequires);
  finally
    ATree.Items.EndUpdate;
  end;
end;

procedure TCodeModules.FormShow(Sender: TObject);
begin
  FillRequires(Language.Modules.PackageRequires,TreePackages);
  FillContains(Language.Modules.PackageContains,LBUnits);
end;

procedure TCodeModules.LBUnitsClick(Sender: TObject);
begin
  if (LBUnits.ItemIndex<>-1) and Assigned(FOnSelect) then
     FOnSelect(Self,TUnit(LBUnits.Items.Objects[LBUnits.ItemIndex]));
end;

procedure TCodeModules.PageControl1Change(Sender: TObject);
begin
  if (PageControl1.ActivePage=TabSheet2) and (ByUses.Items.Count=0) then
     FillUses(Language.Modules.PackageContains,ByUses)
  else
  if (PageControl1.ActivePage=TabSheet3) and (UsedBy.Items.Count=0) then
     FillUsedBy(Language.Modules.PackageContains,UsedBy);
end;

procedure TCodeModules.TreePackagesClick(Sender: TObject);
begin
  if TreePackages.Selected<>nil then
  begin
    FillContains(TPackage(TreePackages.Selected.Data).PackageContains,LBUnits);
    CBSectionChange(Self);
  end;
end;

class procedure TCodeModules.Edit(const AOwner: TComponent; const ALanguage:TLanguage);
begin
  with TCodeModules.Create(AOwner) do
  try
    Language:=ALanguage;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TCodeModules.UsedByChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node.Data<>nil) and Assigned(FOnSelect) then
     FOnSelect(Self,TUnit(Node.Data));
end;

procedure TCodeModules.UsedByExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);

  function UsesModule(const AModule,AChild:TModule):Boolean;

    function UsesSection(const ASection:TUsesSection):Boolean;
    begin
      result:=(ASection<>nil) and (ASection.UsesUnits.IndexOf(AChild)<>-1);
    end;

  begin
    result:=False;

    if AModule is TImplementationModule then
       result:=UsesSection(TImplementationModule(AModule).ModuleImplementation);

    if not result then
       if AModule is TUnit then
          result:=UsesSection(TUnit(AModule).UnitInterface);
  end;

  procedure AddUsedBy(const ANode:TTreeNode; const ATree:TTreeView; const AModule:TModule);
  var t : Integer;
      tmp : TModule;
  begin
    ATree.Items.BeginUpdate;
    try
      for t:=0 to Language.Modules.PackageContains.Count-1 do
      begin
        tmp:=Language.Modules.PackageContains[t];

        if UsesModule(tmp,AModule) then
           ATree.Items.AddChildObject(ANode,tmp.Name,tmp);
      end;
    finally
      ATree.Items.EndUpdate;
    end;
  end;

begin
  if (Node.Count=1) and (Node.Item[0].Text='') then
  begin
    Node[0].Free;
    AddUsedBy(Node,TTreeView(Node.TreeView),TUnit(Node.Data));
  end;

  AllowExpansion:=Node.Count>0;
end;

end.
