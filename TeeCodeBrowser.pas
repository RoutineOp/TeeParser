unit TeeCodeBrowser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, TeeCodeModules, TeeCode, TeeCodeTypes, StdCtrls,
  ExtCtrls, TeeCodeVariables, TeeCodeConstants,
  TeeCodeMethods;

type
  TCodeBrowser = class(TForm)
    Panel1: TPanel;
    CodeMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    FLanguage : TLanguage;
    FEmitter : TEmitClass;

    Constants : TCodeConstants;
    Methods : TCodeMethods;
    Modules : TCodeModules;
    Types : TCodeTypes;
    Variables : TCodeVariables;

    procedure AddForm(const AForm:TForm);
    procedure AddPanels;

    procedure SelectConstant(Sender:TObject; const AConst:TConstantDeclaration);
    procedure SelectMethod(Sender:TObject; const AMethod:TMethodDeclaration);
    procedure SelectType(Sender:TObject; const AType:TTypeDeclaration);
    procedure SelectModule(Sender:TObject; const AModule:TModule);
    procedure SelectVariable(Sender:TObject; const AVar:TVariableDeclaration);
  public
    { Public declarations }
    class procedure Edit(const AOwner:TComponent; const ALanguage:TLanguage;
                         const AEmitter:TEmitClass);
  end;

implementation

{$R *.dfm}

{ TCodeBrowser }

procedure TCodeBrowser.AddForm(const AForm:TForm);
var s : TSplitter;
begin
  AForm.Parent:=Self;
  AForm.Position:=poDesigned;
  AForm.BorderStyle:=bsNone;
  AForm.BorderIcons:=[];
  AForm.Width:=200;

  s:=TSplitter.Create(Self);
  s.Align:=alLeft;
  s.Parent:=Self;
end;

procedure TCodeBrowser.FormCreate(Sender: TObject);
begin
  Panel1.Align:=alClient;
end;

procedure TCodeBrowser.AddPanels;
begin
  Modules:=TCodeModules.Create(Self);
  Modules.Language:=FLanguage;
  Modules.Align:=alLeft;

  AddForm(Modules);

  Modules.OnSelect:=SelectModule;

  Types:=TCodeTypes.Create(Self);
  Types.Align:=alLeft;

  AddForm(Types);

  Types.OnSelect:=SelectType;

  Variables:=TCodeVariables.Create(Self);
  Variables.Align:=alLeft;

  AddForm(Variables);

  Variables.OnSelect:=SelectVariable;

  Constants:=TCodeConstants.Create(Self);
  Constants.Align:=alLeft;

  AddForm(Constants);

  Constants.OnSelect:=SelectConstant;

  Methods:=TCodeMethods.Create(Self);
  Methods.Align:=alLeft;

  AddForm(Methods);

  Methods.OnSelect:=SelectMethod;
end;

procedure TCodeBrowser.FormShow(Sender: TObject);
begin
  Modules.Show;
  Types.Show;
  Variables.Show;
  Constants.Show;
  Methods.Show;
end;

procedure TCodeBrowser.SelectConstant(Sender: TObject;
  const AConst: TConstantDeclaration);
begin
  CodeMemo.Text:=FEmitter.AsString(AConst);
end;

procedure TCodeBrowser.SelectMethod(Sender: TObject;
  const AMethod: TMethodDeclaration);
begin
  CodeMemo.Text:=FEmitter.AsString(AMethod);
end;

procedure TCodeBrowser.SelectType(Sender: TObject;
  const AType: TTypeDeclaration);
begin
  Constants.Fill(AType);
  Methods.Fill(AType);
  Variables.Fill(AType);

  CodeMemo.Text:=FEmitter.AsString(AType);
end;

procedure TCodeBrowser.SelectModule(Sender: TObject; const AModule:TModule);
begin
  Types.Fill(AModule);
  Constants.Fill(AModule);
  Methods.Fill(AModule);
  Variables.Fill(AModule);

  CodeMemo.Text:=FEmitter.AsString(AModule);
end;

procedure TCodeBrowser.SelectVariable(Sender: TObject;
  const AVar: TVariableDeclaration);
begin
  CodeMemo.Text:=FEmitter.AsString(AVar);
end;

class procedure TCodeBrowser.Edit(const AOwner: TComponent; const ALanguage:TLanguage;
                                  const AEmitter:TEmitClass);
begin
  with TCodeBrowser.Create(AOwner) do
  try
    FLanguage:=ALanguage;
    FEmitter:=AEmitter;

    AddPanels;

    ShowModal;
  finally
    Free;
  end;
end;

end.
