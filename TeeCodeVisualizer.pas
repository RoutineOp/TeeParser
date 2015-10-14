{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit TeeCodeVisualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolsAPI, Vcl.OleCtrls, SHDocVw;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TBlockViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater,
    IOTAThreadNotifier, IOTAThreadNotifier160)
    WebBrowser1: TWebBrowser;
    procedure StringListViewData(Sender: TObject; Item: TListItem);
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FItems: TStrings;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
    function GetDelimiter: string;
    function GetStrictDelimiter: Boolean;
    function GetDelimitedText: string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure AddStringListItems(const Expression, TypeName, EvalResult: string);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);

    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);
  end;

procedure Register;

implementation

uses
  DesignIntf, Actnlist, ImgList, Menus, IniFiles;

{$R *.dfm}

resourcestring
  sBlockVisualizerName = 'TBlock Visualize for Delphi';
  sBlockVisualizerDescription = 'Displays a TBlock as Object Pascal';
  sMenuText = 'Show Object Pascal';
  sFormCaption = 'TBlock Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TBlockVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TBlockViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerBlockVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerBlockVisualizer }

function TDebuggerBlockVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerBlockVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TBlock';
  AllDescendants := True;
end;

function TDebuggerBlockVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerBlockVisualizer.GetVisualizerDescription: string;
begin
  Result := sBlockVisualizerDescription;
end;

function TDebuggerBlockVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerBlockVisualizer.GetVisualizerName: string;
begin
  Result := sBlockVisualizerName;
end;

function TDebuggerBlockVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TBlockViewerFrame;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TBlockVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TBlockViewerFrame;
  AFrame.AddStringListItems(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TBlockViewerFrame }

procedure TBlockViewerFrame.AddStringListItems(const Expression, TypeName,
  EvalResult: string);
var
  Delim, DelimText: string;
begin
  FAvailableState := asAvailable;
  FExpression := Expression;
  if FItems = nil then
    FItems := TStringList.Create
  else
    FItems.Clear;

  Delim := GetDelimiter;
  if Length(Delim) > 1 then
  begin
    FItems.Delimiter := Delim[2];
    FItems.StrictDelimiter := GetStrictDelimiter;
    DelimText := GetDelimitedText;
    if DelimText <> '' then
    begin
      FItems.DelimitedText := DelimText;
      StringListView.Items.Count := FItems.Count;
    end else
      StringListView.Items.Count := 0;
    StringListView.Invalidate;
  end else
  begin
    FAvailableState := asNotAvailable;
    StringListView.Invalidate;
  end;
end;

procedure TBlockViewerFrame.AfterSave;
begin
end;

procedure TBlockViewerFrame.BeforeSave;
begin
end;

procedure TBlockViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
     FOwningForm.Close;
end;

procedure TBlockViewerFrame.Destroyed;
begin
end;

function TBlockViewerFrame.Evaluate(Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;
end;

procedure TBlockViewerFrame.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress), ResultSize, ReturnCode);
end;

procedure TBlockViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

function TBlockViewerFrame.GetDelimiter: string;
begin
  Result := Evaluate(Format('%s.Delimiter', [FExpression]));
end;

function TBlockViewerFrame.GetStrictDelimiter: Boolean;
begin
  Result := StrToBool(Evaluate(Format('%s.StrictDelimiter', [FExpression])));
end;

function TBlockViewerFrame.GetDelimitedText: string;
begin
  Result := Evaluate(Format('%s.DelimitedText', [FExpression]));
  Result := Copy(Result, 2, Length(Result) -2);
end;

procedure SetHTML(const Web:TWebBrowser; const HTML:String);
var
  Doc: Variant;
begin
  Doc := Web.Document;
  Doc.Clear;
  Doc.Write(HTML);
  Doc.Close;
end;

procedure TBlockViewerFrame.MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;

  SetHTML(WebBrowser1,'');
end;

procedure TBlockViewerFrame.Modified;
begin
end;

procedure TBlockViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
end;

procedure TBlockViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  AddStringListItems(Expression, TypeName, EvalResult);
end;

procedure TBlockViewerFrame.SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TBlockViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TBlockViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FreeAndNil(FItems);
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TBlockViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin
end;

{ TBlockVisualizerForm }

constructor TBlockVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TBlockVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TBlockVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TBlockVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TBlockVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame := TBlockViewerFrame(AFrame);
end;

function TBlockVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TBlockVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TBlockVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TBlockVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TBlockVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TBlockViewerFrame;
end;

function TBlockVisualizerForm.GetIdentifier: string;
begin
  Result := 'BlockDebugVisualizer';
end;

function TBlockVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBlockVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TBlockVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBlockVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TBlockVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TBlockVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TBlockVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TBlockVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TBlockViewerFrame(Frame);
end;

var
  BlockVis: IOTADebuggerVisualizer;

procedure Register;
begin
  BlockVis := TDebuggerBlockVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(BlockVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(BlockVis);
    BlockVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

