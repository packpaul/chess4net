unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntForms, TntMenus, Menus, Classes, Controls, ExtCtrls, Messages,
  ComCtrls, Dialogs, ActnList, ImgList, AppEvnts, SysUtils,
  //
  ChessBoardUnit, PosBaseChessBoardUnit, ChessEngineInfoUnit, ChessEngine,
  MoveListFormUnit, PlysTreeUnit, PlysProviderIntfUnit, URLVersionQueryUnit,
  SelectLineFormUnit, OpeningsDBManagerFormUnit, OpeningsDBManagerUnit;

type
  TAnalyseChessBoard = class(TTntForm, IPlysProvider)
    MainMenu: TTntMainMenu;
    FileMenuItem: TTntMenuItem;
    FileOpenMenuItem: TTntMenuItem;
    FileSaveAsMenuItem: TTntMenuItem;
    N2: TTntMenuItem;
    FileCopyMenuItem: TTntMenuItem;
    FilePasteMenuItem: TTntMenuItem;
    N1: TTntMenuItem;
    FileExitMenuItem: TTntMenuItem;
    HelpMenuItem: TTntMenuItem;
    HelpContentsMenuItem: TTntMenuItem;
    N3: TTntMenuItem;
    HelpAboutMenuItem: TTntMenuItem;
    ChessBoardPanel: TPanel;
    StatusBar: TStatusBar;
    ViewMenuItem: TTntMenuItem;
    ViewMoveListMenuItem: TTntMenuItem;
    ViewOpeningsDBManagerMenuItem: TTntMenuItem;
    PositionMenuItem: TTntMenuItem;
    PositionInitialMenuItem: TTntMenuItem;
    N4: TTntMenuItem;
    PositionTakebackMoveMenuItem: TTntMenuItem;
    PositionForwardMoveMenuItem: TTntMenuItem;
    PopupMenu: TTntPopupMenu;
    PopupTakebackMoveMenuItem: TTntMenuItem;
    PopupForwardMoveMenuItem: TTntMenuItem;
    ViewFlipBoardMenuItem: TTntMenuItem;
    N5: TTntMenuItem;
    OpenDialog: TOpenDialog;
    ViewChessEngineInfoMenuItem: TTntMenuItem;
    ActionList: TActionList;
    ChessEngineInfoAction: TAction;
    MoveListAction: TAction;
    TakebackMoveAction: TAction;
    ForwardMoveAction: TAction;
    ImageList: TImageList;
    ApplicationEvents: TApplicationEvents;
    PositionSelectLineMenuItem: TTntMenuItem;
    SelectLineAction: TAction;
    SelectLineFromMoveListAction: TAction;
    PopupSelectLineMenuItem: TTntMenuItem;
    OpeningsDBManagerAction: TAction;
    InitialPositionAction: TAction;
    PopupInitialPositionMenuItem: TTntMenuItem;
    FileNewMenuItem: TTntMenuItem;
    N6: TTntMenuItem;
    PositionReturnFromLineMenuItem: TTntMenuItem;
    ReturnFromLineAction: TAction;
    FileSaveMenuItem: TTntMenuItem;
    SaveAction: TAction;
    SaveAsAction: TAction;
    SaveDialog: TSaveDialog;
    EditMenuItem: TTntMenuItem;
    DeleteLineAction: TAction;
    EditDeleteLineMenuItem: TTntMenuItem;
    N7: TTntMenuItem;
    PopupDeleteLineMenuItem: TTntMenuItem;
    EditSetLineToMainMenuItem: TTntMenuItem;
    SetLineToMainAction: TAction;
    procedure FileExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ViewFlipBoardMenuItemClick(Sender: TObject);
    procedure FileOpenMenuItemClick(Sender: TObject);
    procedure FilePasteMenuItemClick(Sender: TObject);
    procedure ChessEngineInfoActionExecute(Sender: TObject);
    procedure ChessEngineInfoActionUpdate(Sender: TObject);
    procedure MoveListActionUpdate(Sender: TObject);
    procedure MoveListActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TakebackMoveActionExecute(Sender: TObject);
    procedure ForwardMoveActionExecute(Sender: TObject);
    procedure TakebackMoveActionUpdate(Sender: TObject);
    procedure ForwardMoveActionUpdate(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure SelectLineActionExecute(Sender: TObject);
    procedure SelectLineActionUpdate(Sender: TObject);
    procedure SelectLineFromMoveListActionExecute(Sender: TObject);
    procedure SelectLineFromMoveListActionUpdate(Sender: TObject);
    procedure OpeningsDBManagerActionExecute(Sender: TObject);
    procedure OpeningsDBManagerActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure InitialPositionActionExecute(Sender: TObject);
    procedure InitialPositionActionUpdate(Sender: TObject);
    procedure FileNewMenuItemClick(Sender: TObject);
    procedure ReturnFromLineActionExecute(Sender: TObject);
    procedure ReturnFromLineActionUpdate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure SaveAsActionUpdate(Sender: TObject);
    procedure DeleteLineActionExecute(Sender: TObject);
    procedure DeleteLineActionUpdate(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure SetLineToMainActionUpdate(Sender: TObject);
    procedure SetLineToMainActionExecute(Sender: TObject);
    procedure HelpAboutMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    m_ChessBoard: TPosBaseChessBoard;
    m_ResizingType: (rtNo, rtHoriz, rtVert);

    m_OpeningsDBManager: TOpeningsDBManager;

    m_ChessEngine: TChessEngine;
    m_ChessEngineInfoForm: TChessEngineInfoForm;

    m_OpeningsDBManagerForm: TOpeningsDBManagerForm;
    m_MoveListForm: TMoveListForm;
    m_SelectLineForm: TSelectLineForm;

    m_PlysTree: TPlysTree;
    m_iCurrentPlyIndex: integer;

    m_lwPlysListUpdateID: LongWord;

    m_bGameChanged: boolean;

    m_GameFileName: TFileName;
    m_bGameFileInC4NFormat: boolean;

    procedure FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
      d2: pointer = nil);

    procedure WMSizing(var Msg: TMessage); message WM_SIZING;

    procedure FCreateChessBoard;
    procedure FInitPosition;

    procedure FCreateChessEngineInfoForm;
    procedure FOnChessEngineInfoFormShow(Sender: TObject);
    procedure FOnChessEngineInfoFormHide(Sender: TObject);

    procedure FCreateChessEngine;
    procedure FDestroyChessEngine;
    procedure FOnChessEngineCalculationInfo(Sender: TObject; rEvaluation: real; strMovesLine: string);
    procedure FSynchronizeChessEngineWithChessBoardAndStartEvaluation;

    procedure FLoadPGNDataFromFile(AFileName: TFileName);
    function FLoadPGNData(const PGNData: TStrings): boolean;
    function FSavePGNData: boolean;
    procedure FSavePGNDataAs;
    function FAskAndSavePGNData: boolean;

    function IPlysProvider.GetWhiteStarts = FGetWhiteStarts;
    function FGetWhiteStarts: boolean;

    function IPlysProvider.GetPlysCount = FGetPlysCount;
    function FGetPlysCount: integer;

    function IPlysProvider.GetPly = FGetPly;
    function FGetPly(iIndex: integer): string;

    function IPlysProvider.GetInvalidationID = FGetInvalidationID;
    function FGetInvalidationID: LongWord;

    function IPlysProvider.GetCurrentPlyIndex = FGetCurrentPlyIndex;
    function FGetCurrentPlyIndex: integer;

    procedure IPlysProvider.SetCurrentPlyIndex = FSetCurrentPlyIndex;
    procedure FSetCurrentPlyIndex(iValue: integer);

    function IPlysProvider.HasSeveralPlysForPlyIndex = FHasSeveralPlysForPlyIndex;
    function FHasSeveralPlysForPlyIndex(iPlyIndex: integer): boolean;

    procedure IPlysProvider.GetPlysForPlyIndex = FGetPlysForPlyIndex;
    procedure FGetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);

    function IPlysProvider.SetPlyForPlyIndex = FSetPlyForPlyIndex;
    function FSetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    function IPlysProvider.GetPlyStatus = FGetPlyStatus;
    function FGetPlyStatus(iPlyIndex: integer): TPlyStatuses;

    procedure FSetToInitialPosition;
    procedure FTakebackMove;
    procedure FForwardMove;
    procedure FReturnFromCurrentLine;
    procedure FDeleteLine;
    procedure FSetLineToMain;

    procedure FRefreshMoveListForm;

    procedure FOnURLQueryReady(Sender: TURLVersionQuery);
    procedure FOnOpeningsDBManagerChanged(Sender: TObject);

    procedure FSetGameFileName(const AGameFileName: TFileName);
  end;

implementation

uses
  Windows, Clipbrd,
  //
  PGNParserUnit, ChessRulesEngine, GlobalsLocalUnit, DontShowMessageDlgUnit,
  IniSettingsUnit, PGNWriterUnit, SplashFormUnit;

{$R *.dfm}

const
  MSG_GAME_CHANGED_SAVE_CHANGES = 'Current game was modified. Save the changes?';
  MSG_INCORRECT_FILE_FORMAT = 'Incorrect file format encountered or data is broken!';
  MSG_FILE_EXISTS_OVERWRITE = 'File %s already exists. Do you want it to be overwritten?';
  MSG_LINE_TO_BE_DELETED = 'Are you sure you want to delete current line?';
  MSG_SET_LINE_TO_MAIN = 'Are you sure you want current line be set to main?';

  LBL_CHESS4NET_ANALYZER_VER = 'Chess4Net Analyzer %s';

////////////////////////////////////////////////////////////////////////////////
// TAnalyseChessBoard

procedure TAnalyseChessBoard.FileExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TAnalyseChessBoard.FormCreate(Sender: TObject);
begin
  Caption := Format(LBL_CHESS4NET_ANALYZER_VER, [CHESS4NET_VERSION_TXT]);

  m_PlysTree := TPlysTree.Create;

  m_OpeningsDBManager := TOpeningsDBManager.Create;
  m_OpeningsDBManager.OnChanged := FOnOpeningsDBManagerChanged;

  FCreateChessBoard;
  FInitPosition;
end;


procedure TAnalyseChessBoard.FormDestroy(Sender: TObject);
begin
  FDestroyChessEngine;

  m_MoveListForm.PlysProvider := nil;
  m_OpeningsDBManagerForm.OpeningsDBManagerProvider := nil;
   
  m_OpeningsDBManager.Free;
  m_PlysTree.Free;
end;


procedure TAnalyseChessBoard.FCreateChessBoard;
begin
  m_ChessBoard := TPosBaseChessBoard.Create(self, FChessBoardHandler, '');
  m_ChessBoard.MoveNotationFormat := mnfCh4NEx;

  with ChessBoardPanel do
    SetBounds(Left, Top, m_ChessBoard.ClientWidth, m_ChessBoard.ClientHeight);

  m_ChessBoard.BorderStyle := bsNone;

  m_ChessBoard.Align := alClient;
  m_ChessBoard.Parent := ChessBoardPanel;
  m_ChessBoard.Visible := TRUE;

  m_ChessBoard.Mode := mAnalyse;

  m_ChessBoard.InitPosition;

  FOnOpeningsDBManagerChanged(nil);
end;


procedure TAnalyseChessBoard.FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
  d2: pointer = nil);

  procedure NAdjustPlysList;
  var
    strMove: string;
    iPly: integer;
  begin
    strMove := PString(d1)^;

    iPly := FGetCurrentPlyIndex;

    if (FGetPlysCount >= iPly) then
    begin
      if (m_PlysTree[iPly] = strMove) then
      begin
        FRefreshMoveListForm; // cursor moved
        exit;
      end;
    end;

    m_PlysTree.Add(iPly, m_ChessBoard.GetPosition, strMove, [psUserLine]);
    m_bGameChanged := TRUE;

    inc(m_lwPlysListUpdateID);
    FRefreshMoveListForm;
  end;

begin // .FChessBoardHandler
  case e of
    cbeMoved:
    begin
      inc(m_iCurrentPlyIndex);
      NAdjustPlysList;
      FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
    end;
  end;
end;


procedure TAnalyseChessBoard.WMSizing(var Msg: TMessage);
begin
  m_ChessBoard.Perform(Msg.Msg, Msg.WParam, Msg.LParam);

  case Msg.WParam of
    WMSZ_RIGHT, WMSZ_LEFT, WMSZ_BOTTOMRIGHT, WMSZ_TOPLEFT:
      m_ResizingType := rtHoriz;
    WMSZ_BOTTOM, WMSZ_TOP:
      m_ResizingType := rtVert;
  else
    begin
      m_ResizingType := rtNo;
      PRect(Msg.LParam).Left := Left;
      PRect(Msg.LParam).Top := Top;
    end;
  end; // case
end;


procedure TAnalyseChessBoard.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
  iNewChessBoardWidth, iNewChessBoardHeight: integer;
begin
  Resize := (m_ResizingType <> rtNo);
  if (not Resize) then
    exit;

  Resize := ((m_ChessBoard.Width > 196) and (m_ChessBoard.Height > 187)); // TODO: handle menu resizing in Vista
  if (not Resize) then
  begin
    case m_ResizingType of
      rtHoriz:
        Resize := (NewWidth >= Width);
      rtVert:
        Resize := (NewHeight >= Height);
    end;
  end;
  if (not Resize) then
    exit;

  iNewChessBoardWidth := m_ChessBoard.Width + (NewWidth - self.Width);
  iNewChessBoardHeight := m_ChessBoard.Height + (NewHeight - self.Height);

  m_ChessBoard.FormCanResize(self, iNewChessBoardWidth, iNewChessBoardHeight, Resize);
  if (Resize) then
  begin
    NewWidth := self.Width + (iNewChessBoardWidth - m_ChessBoard.Width);
    NewHeight := self.Height + (iNewChessBoardHeight - m_ChessBoard.Height);
  end;

end;


procedure TAnalyseChessBoard.FTakebackMove;
var
  iPly: integer;
begin
  iPly := FGetCurrentPlyIndex;
  if (iPly > 0) then
    FSetCurrentPlyIndex(iPly - 1);
end;


procedure TAnalyseChessBoard.FSetToInitialPosition;
begin
  if (FGetCurrentPlyIndex > 0) then
    FSetCurrentPlyIndex(0);
end;


procedure TAnalyseChessBoard.FInitPosition;
begin
  m_ChessBoard.InitPosition;

  m_iCurrentPlyIndex := 0;

  inc(m_lwPlysListUpdateID);

  m_PlysTree.Clear;
  m_PlysTree.Add(m_ChessBoard.GetPosition);

  m_bGameChanged := FALSE;
  FSetGameFileName('');

  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
end;


procedure TAnalyseChessBoard.FForwardMove;
var
  bRes: boolean;
begin
  if (FGetPlysCount <= FGetCurrentPlyIndex) then
    exit;

  bRes := m_ChessBoard.DoMove(m_PlysTree[FGetCurrentPlyIndex + 1]);
  Assert(bRes);
end;


procedure TAnalyseChessBoard.ViewFlipBoardMenuItemClick(Sender: TObject);
begin
  m_ChessBoard.Flipped := (not m_ChessBoard.Flipped);
  ViewFlipBoardMenuItem.Checked := m_ChessBoard.Flipped;
end;


procedure TAnalyseChessBoard.FileOpenMenuItemClick(Sender: TObject);
begin
  if (not FAskAndSavePGNData) then
    exit;

  if (OpenDialog.Execute) then
    FLoadPGNDataFromFile(OpenDialog.FileName);
end;


procedure TAnalyseChessBoard.FLoadPGNDataFromFile(AFileName: TFileName);
var
  strlData: TStringList;
begin
  strlData := TStringList.Create;
  try
    strlData.LoadFromFile(AFileName);
    if (not FLoadPGNData(strlData)) then
    begin
      MessageDlg(MSG_INCORRECT_FILE_FORMAT, mtError, [mbOK], 0);
      exit;
    end;

    FSetGameFileName(AFileName);

  finally
    strlData.Free;
  end;
end;


procedure TAnalyseChessBoard.FilePasteMenuItemClick(Sender: TObject);
var
  strlData: TStringList;
begin
  if (not Clipboard.HasFormat(CF_TEXT)) then
    exit;

  if (not FAskAndSavePGNData) then
    exit;

  strlData := TStringList.Create;
  try
    strlData.Text := Clipboard.AsText;
    FLoadPGNData(strlData);
  finally
    strlData.Free;
  end;

end;


function TAnalyseChessBoard.FLoadPGNData(const PGNData: TStrings): boolean;
var
  PGNParser: TPGNParser;
begin
  Result := FALSE;

  PGNParser := TPGNParser.Create;
  try
    if (not PGNParser.Parse(PGNData)) then
      exit;

    FInitPosition;

    m_PlysTree.Assign(PGNParser.Tree);

    m_bGameFileInC4NFormat := PGNParser.InC4NFormat;

  finally
    PGNParser.Free;
  end;

  FRefreshMoveListForm;

  Result := TRUE;
end;


procedure TAnalyseChessBoard.ChessEngineInfoActionExecute(Sender: TObject);
begin
  if (not Assigned(m_ChessEngineInfoForm)) then
    FCreateChessEngineInfoForm;

  if (m_ChessEngineInfoForm.Showing) then
    m_ChessEngineInfoForm.Hide
  else
    m_ChessEngineInfoForm.Show;
end;


procedure TAnalyseChessBoard.FCreateChessEngineInfoForm;
begin
  m_ChessEngineInfoForm := TChessEngineInfoForm.Create(self);
  m_ChessEngineInfoForm.OnShow := FOnChessEngineInfoFormShow;
  m_ChessEngineInfoForm.OnHide := FOnChessEngineInfoFormHide;
end;


procedure TAnalyseChessBoard.FOnChessEngineInfoFormShow(Sender: TObject);
begin
  FCreateChessEngine;
  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
end;


procedure TAnalyseChessBoard.FOnChessEngineInfoFormHide(Sender: TObject);
begin
  FDestroyChessEngine;
end;


procedure TAnalyseChessBoard.ChessEngineInfoActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (Assigned(m_ChessEngineInfoForm) and m_ChessEngineInfoForm.Showing);
end;


procedure TAnalyseChessBoard.FCreateChessEngine;
begin
  if (Assigned(m_ChessEngine)) then
    exit;
  m_ChessEngine := TChessEngine.Create;
  m_ChessEngine.OnCalculationInfo := FOnChessEngineCalculationInfo;
end;


procedure TAnalyseChessBoard.FDestroyChessEngine;
begin
  if (Assigned(m_ChessEngine)) then
    m_ChessEngine.StopCalculation;
  FreeAndNil(m_ChessEngine);
end;


procedure TAnalyseChessBoard.FOnChessEngineCalculationInfo(Sender: TObject;
  rEvaluation: real; strMovesLine: string);
begin
  if (not Assigned(m_ChessEngineInfoForm)) then
    exit;

  m_ChessEngineInfoForm.SetInfo(rEvaluation, strMovesLine);
end;


procedure TAnalyseChessBoard.FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
begin
  if (not Assigned(m_ChessEngine)) then
    exit;
  m_ChessEngine.SetPosition(m_ChessBoard.GetPosition);
  m_ChessEngine.CalculateReplyNonBlocking;
end;


procedure TAnalyseChessBoard.MoveListActionExecute(Sender: TObject);
begin
  if (not Assigned(m_MoveListForm)) then
  begin
    m_MoveListForm := TMoveListForm.Create(self);
    m_MoveListForm.PlysProvider := self;
  end;

  if (m_MoveListForm.Showing) then
    m_MoveListForm.Hide
  else
    m_MoveListForm.Show;
end;


procedure TAnalyseChessBoard.MoveListActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (Assigned(m_MoveListForm) and m_MoveListForm.Showing);
end;


procedure TAnalyseChessBoard.FormShow(Sender: TObject);
begin
  MoveListAction.Execute;
  OpeningsDBManagerAction.Execute;
{$IFDEF RELEASE}
  with TURLVersionQuery.Create do
  begin
    OnQueryReady := FOnURLQueryReady;
    Query(aidAnalyzer, CHESS4NET_VERSION, osidWindows);
  end;
{$ENDIF}
end;


procedure TAnalyseChessBoard.FOnURLQueryReady(Sender: TURLVersionQuery);
var
  bDontShowFlag: boolean;
begin
  if (not Assigned(Sender)) then
    exit;

  try
    if ((Sender.LastVersion <= TIniSettings.Instance.DontShowLastVersion)) then
      exit;

    bDontShowFlag := FALSE;

    if (Sender.Info <> '') then
      TDontShowMessageDlg.Show(Sender.Info, bDontShowFlag);

    if (bDontShowFlag) then
      TIniSettings.Instance.DontShowLastVersion := Sender.LastVersion;

  finally
    Sender.Free;
  end;
end;


procedure TAnalyseChessBoard.FOnOpeningsDBManagerChanged(Sender: TObject);
begin
  m_ChessBoard.SetExternalBase(m_OpeningsDBManager.DB);
  m_ChessBoard.pTrainingMode := ((m_OpeningsDBManager.DB <> '') and (m_OpeningsDBManager.Enabled));  
end;


function TAnalyseChessBoard.FGetPlysCount: integer;
begin
  Result := m_PlysTree.Count - 1;
end;


function TAnalyseChessBoard.FGetWhiteStarts: boolean;
begin
  Result := m_PlysTree.WhiteStarts;
end;


function TAnalyseChessBoard.FGetPly(iIndex: integer): string;
begin
  Result := m_PlysTree[iIndex];
end;


procedure TAnalyseChessBoard.TakebackMoveActionExecute(Sender: TObject);
begin
  FTakebackMove;
end;


procedure TAnalyseChessBoard.ForwardMoveActionExecute(Sender: TObject);
begin
  FForwardMove;
end;


procedure TAnalyseChessBoard.TakebackMoveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetCurrentPlyIndex > 0)
end;


procedure TAnalyseChessBoard.ForwardMoveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((FGetPlysCount) > FGetCurrentPlyIndex);
end;


function TAnalyseChessBoard.FGetInvalidationID: LongWord;
begin
  Result := m_lwPlysListUpdateID;
end;


function TAnalyseChessBoard.FGetCurrentPlyIndex: integer;
begin
  Result := m_iCurrentPlyIndex;
end;


procedure TAnalyseChessBoard.FSetCurrentPlyIndex(iValue: integer);
begin
  if ((iValue - FGetCurrentPlyIndex) = 1) then
    FForwardMove
  else if ((iValue >= 0) and (iValue <= FGetPlysCount)) then
  begin
    m_ChessBoard.SetPosition(m_PlysTree.Position[iValue]);
    m_iCurrentPlyIndex := iValue;
    FRefreshMoveListForm;
    FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
  end;
end;


procedure TAnalyseChessBoard.FRefreshMoveListForm;
begin
  m_MoveListForm.Refresh;
end;


procedure TAnalyseChessBoard.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  // Because TSpeedButton actions do not work on Vista
  TakebackMoveAction.Update;
  ForwardMoveAction.Update;
  SelectLineAction.Update;
  ReturnFromLineAction.Update;
  DeleteLineAction.Update;
end;


function TAnalyseChessBoard.FHasSeveralPlysForPlyIndex(iPlyIndex: integer): boolean;
begin
  Result := (m_PlysTree.GetPlysCountForPlyIndex(iPlyIndex) > 1);
end;


procedure TAnalyseChessBoard.FGetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);
begin
  m_PlysTree.GetPlysForPlyIndex(iPlyIndex, List);
end;


function TAnalyseChessBoard.FSetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;
begin
  Result := m_PlysTree.SetPlyForPlyIndex(iPlyIndex, strPly);
  if (Result) then
    inc(m_lwPlysListUpdateID);

  FSetCurrentPlyIndex(iPlyIndex);  
end;


procedure TAnalyseChessBoard.FReturnFromCurrentLine;
var
  iPlyIndex: integer;
  strlPlysList: TStringList;
begin
  iPlyIndex := FGetCurrentPlyIndex;
  if (iPlyIndex = 0) then
    exit;

  while ((iPlyIndex > 0) and (not FHasSeveralPlysForPlyIndex(iPlyIndex))) do
  begin
    dec(iPlyIndex);
  end;

  strlPlysList := TStringList.Create;
  try
    FGetPlysForPlyIndex(iPlyIndex, TStrings(strlPlysList));
    Assert(strlPlysList.Count > 0);

    if (m_PlysTree.SetPlyForPlyIndex(iPlyIndex, strlPlysList[0])) then
      inc(m_lwPlysListUpdateID);

  finally
    strlPlysList.Free;
  end;

  FSetCurrentPlyIndex(iPlyIndex - 1);
end;


procedure TAnalyseChessBoard.SelectLineActionExecute(Sender: TObject);
begin
  if (not Assigned(m_SelectLineForm)) then
  begin
    m_SelectLineForm := TSelectLineForm.Create(self);
    m_SelectLineForm.PlysProvider := self;
  end;

  m_SelectLineForm.ShowModal;
end;


procedure TAnalyseChessBoard.SelectLineFromMoveListActionExecute(
  Sender: TObject);
begin
  if (Assigned(m_MoveListForm)) then
    m_MoveListForm.SelectLine;
end;


procedure TAnalyseChessBoard.SelectLineActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FHasSeveralPlysForPlyIndex(FGetCurrentPlyIndex + 1);
end;


procedure TAnalyseChessBoard.SelectLineFromMoveListActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := ((FGetCurrentPlyIndex > 0) and
    (FHasSeveralPlysForPlyIndex(FGetCurrentPlyIndex)));
end;


function TAnalyseChessBoard.FGetPlyStatus(iPlyIndex: integer): TPlyStatuses;
begin
  Result := m_PlysTree.GetPlyStatus(iPlyIndex);
end;


procedure TAnalyseChessBoard.OpeningsDBManagerActionExecute(
  Sender: TObject);
begin
  if (not Assigned(m_OpeningsDBManagerForm)) then
  begin
    m_OpeningsDBManagerForm := TOpeningsDBManagerForm.Create(self);
    m_OpeningsDBManagerForm.OpeningsDBManagerProvider := m_OpeningsDBManager;
  end;

  if (m_OpeningsDBManagerForm.Showing) then
    m_OpeningsDBManagerForm.Hide
  else
    m_OpeningsDBManagerForm.Show;
end;


procedure TAnalyseChessBoard.OpeningsDBManagerActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Checked := (Assigned(m_OpeningsDBManagerForm) and
    m_OpeningsDBManagerForm.Showing);
end;


procedure TAnalyseChessBoard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not FAskAndSavePGNData) then
    CanClose := FALSE;
end;


function TAnalyseChessBoard.FAskAndSavePGNData: boolean;
var
  iRes: integer;
begin
  Result := TRUE;

  SaveAction.Update;
  if (not SaveAction.Enabled) then
    exit;

  iRes := MessageDlg(MSG_GAME_CHANGED_SAVE_CHANGES, mtConfirmation, mbYesNoCancel, 0);
  case iRes of
    mrCancel:
      Result := FALSE;
    mrYes:
      SaveAction.Execute;
    mrNo:
      ;
  end;
end;


function TAnalyseChessBoard.FSavePGNData: boolean;
var
  Writer: TPGNWriter;
begin
  Result := FALSE;

  if ((m_GameFileName = '') or (not m_bGameFileInC4NFormat)) then
    exit;

  Writer := TPGNWriter.Create;
  try
    Writer.WriteInChess4NetFormat(m_PlysTree);
    Writer.Data.SaveToFile(m_GameFileName);
  finally
    Writer.Free;
  end;

  m_bGameChanged := FALSE;

  Result := TRUE;
end;


procedure TAnalyseChessBoard.FSavePGNDataAs;
var
  strMsg: string;
begin
  if (m_bGameFileInC4NFormat) then
    SaveDialog.FileName := m_GameFileName
  else
    SaveDialog.FileName := ChangeFileExt(m_GameFileName, '');  

  if (not SaveDialog.Execute) then
    exit;

  if (FileExists(SaveDialog.FileName)) then
  begin
    strMsg := Format(MSG_FILE_EXISTS_OVERWRITE, [SaveDialog.FileName]);
    if (MessageDlg(strMsg, mtWarning, [mbYes, mbNo], 0) = mrNo) then
      exit;
  end;

  FSetGameFileName(SaveDialog.FileName);
  m_bGameFileInC4NFormat := TRUE;

  FSavePGNData;
end;


procedure TAnalyseChessBoard.InitialPositionActionExecute(Sender: TObject);
begin
  FSetToInitialPosition;
end;


procedure TAnalyseChessBoard.InitialPositionActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetCurrentPlyIndex > 0);
end;


procedure TAnalyseChessBoard.FileNewMenuItemClick(Sender: TObject);
begin
  if (not FAskAndSavePGNData) then
    exit;

  FInitPosition;
  FRefreshMoveListForm;
end;


procedure TAnalyseChessBoard.ReturnFromLineActionExecute(Sender: TObject);
begin
  FReturnFromCurrentLine;
end;


procedure TAnalyseChessBoard.ReturnFromLineActionUpdate(Sender: TObject);
var
  iPly: integer;
begin
  iPly := FGetCurrentPlyIndex;
  (Sender as TAction).Enabled :=
    ((iPly > 0) and (not (psMainLine in m_PlysTree.GetPlyStatus(iPly))));
end;


procedure TAnalyseChessBoard.SaveActionExecute(Sender: TObject);
begin
  if (not FSavePGNData) then
    FSavePGNDataAs;
end;


procedure TAnalyseChessBoard.SaveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    ((FGetPlysCount > 0) and m_bGameChanged);
end;


procedure TAnalyseChessBoard.SaveAsActionExecute(Sender: TObject);
begin
  FSavePGNDataAs;
end;


procedure TAnalyseChessBoard.SaveAsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetPlysCount > 0);
end;


procedure TAnalyseChessBoard.DeleteLineActionExecute(Sender: TObject);
begin
  FDeleteLine;
end;


procedure TAnalyseChessBoard.FDeleteLine;
var
  iPly: integer;
begin
  if (MessageDlg(MSG_LINE_TO_BE_DELETED, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
    exit;

  iPly := FGetCurrentPlyIndex;

  if (not m_PlysTree.Delete(iPly)) then
    exit;

  m_bGameChanged := TRUE;
  inc(m_lwPlysListUpdateID);

  FSetCurrentPlyIndex(iPly - 1);
end;


procedure TAnalyseChessBoard.DeleteLineActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetCurrentPlyIndex > 0);  
end;


procedure TAnalyseChessBoard.PopupMenuPopup(Sender: TObject);
var
  i: integer;
  Action: TAction;
begin
  for i := 0 to PopupMenu.Items.Count - 1 do
  begin
    Action := (PopupMenu.Items[i].Action as TAction);

    if (Assigned(Action)) then
      PopupMenu.Items[i].Visible := Action.Enabled
    else
      PopupMenu.Items[i].Visible := PopupMenu.Items[i].Enabled;
  end;

end;


procedure TAnalyseChessBoard.FSetGameFileName(const AGameFileName: TFileName);
begin
  if (AGameFileName = m_GameFileName) then
    exit;

  m_GameFileName := AGameFileName;

  StatusBar.SimpleText := ExtractFileName(AGameFileName);
  StatusBar.Hint := AGameFileName;
end;


procedure TAnalyseChessBoard.SetLineToMainActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetPlysCount > 0)
end;


procedure TAnalyseChessBoard.SetLineToMainActionExecute(Sender: TObject);
begin
  FSetLineToMain;
end;


procedure TAnalyseChessBoard.FSetLineToMain;
begin
  if (MessageDlg(MSG_SET_LINE_TO_MAIN, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
    exit;

  m_PlysTree.SetLineToMain;

  inc(m_lwPlysListUpdateID);
  FRefreshMoveListForm;
end;


procedure TAnalyseChessBoard.HelpAboutMenuItemClick(Sender: TObject);
begin
  with TSplashForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TAnalyseChessBoard.FormActivate(Sender: TObject);
const
  FIRST_ACTIVATED: boolean = FALSE;
begin
  if (FIRST_ACTIVATED) then
    exit;

  FIRST_ACTIVATED := TRUE;

  if (ParamCount > 0) then
    FLoadPGNDataFromFile(ParamStr(1));
end;

end.
