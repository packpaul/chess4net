////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntMenus, Menus, Classes, TntClasses, Controls, ExtCtrls, Messages,
  ComCtrls, Dialogs, ActnList, ImgList, AppEvnts, SysUtils,
  //
  ChessBoardUnit, PosBaseChessBoardLayerUnit, ChessEngineInfoUnit, ChessEngine,
  MoveListFormUnit, PlysTreeUnit, PlysProviderIntfUnit, URLVersionQueryUnit,
  SelectLineFormUnit, OpeningsDBManagerFormUnit, OpeningsDBManagerUnit,
  PositionEditingFormUnit, ChessRulesEngine, PGNParserUnit, FloatingFormsUnit,
  CommentsFormUnit, GamesManagerUnit, GamesListFormUnit;

type
  TAnalyseChessBoard = class(TMainFloatingForm, IPlysProvider, IPositionEditable)
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
    FileNewStandardMenuItem: TTntMenuItem;
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
    FileNewMenuItem: TTntMenuItem;
    FileNewCustomMenuItem: TTntMenuItem;
    FileCopyFENMenuItem: TTntMenuItem;
    CopyAction: TAction;
    NewStandardAction: TAction;
    NewCustomAction: TAction;
    EditPopupMenu: TTntPopupMenu;
    EditPopupWhiteMenuItem: TTntMenuItem;
    EditPopupWhiteKingMenuItem: TTntMenuItem;
    EditPopupWhiteQueenMenuItem: TTntMenuItem;
    EditPopupWhiteRookMenuItem: TTntMenuItem;
    EditPopupWhiteBishopMenuItem: TTntMenuItem;
    EditPopupWhitePawnMenuItem: TTntMenuItem;
    EditPopupBlackMenuItem: TTntMenuItem;
    EditPopupBlackKingMenuItem: TTntMenuItem;
    EditPopupBlackQueenMenuItem: TTntMenuItem;
    EditPopupBlackRookMenuItem: TTntMenuItem;
    EditPopupBlackBishopMenuItem: TTntMenuItem;
    EditPopupBlackPawnMenuItem: TTntMenuItem;
    N8: TTntMenuItem;
    N9: TTntMenuItem;
    EditPopupWhiteKnightMenuItem: TTntMenuItem;
    EditPopupBlackKnightMenuItem: TTntMenuItem;
    CommentsMenuItem: TTntMenuItem;
    CommentsAction: TAction;
    ReturnFromLine: TTntMenuItem;
    EndPositionAction: TAction;
    PositionEndMenuItem: TTntMenuItem;
    EditCommentAction: TAction;
    N10: TTntMenuItem;
    EditCommentMenuItem: TTntMenuItem;
    GamesListAction: TAction;
    GamesListMenuItem: TTntMenuItem;
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
    procedure FileCopyFENMenuItemClick(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CopyActionUpdate(Sender: TObject);
    procedure NewStandardActionExecute(Sender: TObject);
    procedure NewCustomActionExecute(Sender: TObject);
    procedure NewActionUpdate(Sender: TObject);
    procedure EditPopupColorMenuItemClick(Sender: TObject);
    procedure EditPopupMenuPopup(Sender: TObject);
    procedure EditPopupPieceMenuItemClick(Sender: TObject);
    procedure CommentsActionExecute(Sender: TObject);
    procedure CommentsActionUpdate(Sender: TObject);
    procedure StatusBarHint(Sender: TObject);
    procedure EndPositionActionExecute(Sender: TObject);
    procedure EndPositionActionUpdate(Sender: TObject);
    procedure EditCommentActionExecute(Sender: TObject);
    procedure EditCommentActionUpdate(Sender: TObject);
    procedure GamesListActionExecute(Sender: TObject);
    procedure GamesListActionUpdate(Sender: TObject);
  private
    m_ChessBoard: TChessBoard;
    m_PosBaseChessBoardLayer: TPosBaseChessBoardLayer;

    m_ResizingType: (rtNo, rtHoriz, rtVert);

    m_OpeningsDBManager: TOpeningsDBManager;

    m_ChessEngine: TChessEngine;
    m_ChessEngineInfoForm: TChessEngineInfoForm;

    m_OpeningsDBManagerForm: TOpeningsDBManagerForm;
    m_MoveListForm: TMoveListForm;
    m_CommentsForm: TCommentsForm;
    m_SelectLineForm: TSelectLineForm;

    m_PositionEditingForm: TPositionEditingForm;

    m_PlysTree: TPlysTree;
    m_iCurrentPlyIndex: integer;

    m_lwPlysListUpdateID: LongWord;

    m_bGameChanged: boolean;
    m_bPlysTreeChanged: boolean;    

    m_CurrentGameItem: TGameItem;

    m_GameFileName: TFileName;
    m_bGameFileInC4NFormat: boolean;

    m_GamesManager: TGamesManager;
    m_GamesListForm: TGamesListForm;

    procedure FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
      d2: pointer = nil);

    procedure WMSizing(var Msg: TMessage); message WM_SIZING;
    procedure WMDropFiles(var Msg : TMessage); message WM_DROPFILES;

    procedure FCreateChessBoard;
    procedure FDestroyChessBoard;
    procedure FInitPosition;

    procedure FCreateChessEngineInfoForm;
    procedure FOnChessEngineInfoFormShow(Sender: TObject);
    procedure FOnChessEngineInfoFormHide(Sender: TObject);

    procedure FCreateChessEngine;
    procedure FDestroyChessEngine;
    procedure FOnChessEngineCalculationInfo(Sender: TObject; rEvaluation: real; strMovesLine: string);
    procedure FSynchronizeChessEngineWithChessBoardAndStartEvaluation;

    procedure FLoadPGNDataFromFile(const AFileName: TFileName);
    procedure FLoadPGNDataFromParser(const PGNParser: TPGNParser);

    function FLoadDataForCurrentGameInGameList: boolean;

    function FSavePGNData: boolean;
    procedure FSavePGNDataAs;
    function FAskAndSavePGNData: boolean;

    function IPlysProvider.GetWhiteStarts = FGetWhiteStarts;
    function FGetWhiteStarts: boolean;

    function IPlysProvider.GetPlysCount = FGetPlysCount;
    function FGetPlysCount: integer;

    function IPlysProvider.GetPlysOffset = FGetPlysOffset;
    function FGetPlysOffset: integer;

    function IPlysProvider.GetPly = FGetPly;
    function FGetPly(iIndex: integer): string;

    function IPlysProvider.GetComments = FGetComments;
    function FGetComments(iIndex: integer): WideString;

    procedure IPlysProvider.SetComments = FSetComments;
    procedure FSetComments(iIndex: integer; const wstrValue: WideString);

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

    procedure IPositionEditable.SetEditPiece = FSetEditPiece;
    procedure FSetEditPiece(Piece: TFigure);

    function IPositionEditable.SetPosition = FSetPosition;
    function FSetPosition(const strFEN: string): boolean;

    procedure IPositionEditable.StopEditing = FStopEditing;

    procedure FSetToInitialPosition;
    procedure FSetToEndPosition;
    procedure FTakebackMove;
    procedure FForwardMove;
    procedure FReturnFromCurrentLine;
    procedure FDeleteLine;
    procedure FSetLineToMain;

    procedure FRefreshMoveListForm;

    procedure FOnURLQueryReady(Sender: TURLVersionQuery);
    procedure FOnOpeningsDBManagerChanged(Sender: TObject);

    procedure FSetGameFileName(const AGameFileName: TFileName);

    procedure FStartEditing;
    procedure FStopEditing;
    function FIsEditing: boolean;

    procedure FSetGameChanged(bValue: boolean);

    function FSetNewStandard: boolean;
    procedure FSetGameToGameList;

    function FGetChessBoardFlipped: boolean;
    procedure FSetChessBoardFlipped(bValue: boolean);

    procedure FRefreshStatusBar;

    procedure FOnGamesManagerChanged(Sender: TObject);

    procedure FSaveGameContextData;

    property ChessBoardFlipped: boolean read FGetChessBoardFlipped write FSetChessBoardFlipped;
  end;

implementation

uses
  Windows, TntClipbrd, ShellAPI,
  //
  GlobalsLocalUnit, DontShowMessageDlgUnit,
  IniSettingsUnit, PGNWriterUnit, SplashFormUnit, CommentsEditFormUnit;

{$R *.dfm}

type
  TGameContextData = class
  private
    m_iCurrentPlyIndex: integer;
    m_bGameChanged: boolean;
    m_PlysTree: TPlysTree;
    procedure FSetPlysTree(const Value: TPlysTree);
  public
    destructor Destroy; override;
    class function GetNullInstance: TGameContextData;
    property CurrentPlyIndex: integer read m_iCurrentPlyIndex write m_iCurrentPlyIndex;
    property GameChanged: boolean read m_bGameChanged write m_bGameChanged;
    property PlysTree: TPlysTree read m_PlysTree write FSetPlysTree;
  end;

const
  MSG_GAME_CHANGED_SAVE_CHANGES = 'Current game was modified. Save the changes?';
  MSG_INCORRECT_FILE_FORMAT = 'Incorrect file format encountered or data is broken!';
  MSG_FILE_EXISTS_OVERWRITE = 'File %s already exists. Do you want it to be overwritten?';
  MSG_LINE_TO_BE_DELETED = 'Are you sure you want to delete current line?';
  MSG_SET_LINE_TO_MAIN = 'Are you sure you want current line be set to main?';

  LBL_CHESS4NET_ANALYZER_VER = 'Chess4Net Analyzer %s';

  LBL_EDITING = 'Editing';
  LBL_CHANGED = 'Changed';

var
  g_NullGameContextDataInstance: TGameContextData = nil;

////////////////////////////////////////////////////////////////////////////////
// TAnalyseChessBoard

procedure TAnalyseChessBoard.FileExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TAnalyseChessBoard.FormCreate(Sender: TObject);
begin
  Caption := Format(LBL_CHESS4NET_ANALYZER_VER, [CHESS4NET_VERSION_TXT]);

  m_GamesManager := TGamesManager.Create;
  m_GamesManager.OnChanged := FOnGamesManagerChanged;

  m_PlysTree := TPlysTree.Create;

  m_OpeningsDBManager := TOpeningsDBManager.Create;
  m_OpeningsDBManager.OnChanged := FOnOpeningsDBManagerChanged;

  FCreateChessBoard;
  FInitPosition;

  FRefreshStatusBar;

  DragAcceptFiles(Handle, TRUE);
end;


procedure TAnalyseChessBoard.FRefreshStatusBar;
begin
  if (FIsEditing) then
    StatusBar.Panels[1].Text := LBL_EDITING
  else
  begin
    if (m_bGameChanged) then
      StatusBar.Panels[1].Text := LBL_CHANGED
    else
      StatusBar.Panels[1].Text := '';
  end;
end;


procedure TAnalyseChessBoard.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, FALSE);

  FDestroyChessEngine;

  m_GamesListForm.GamesListProvider := nil;
  m_GamesManager.OnChanged := nil;
  
  m_MoveListForm.PlysProvider := nil;
  m_OpeningsDBManagerForm.OpeningsDBManagerProvider := nil;

  m_CurrentGameItem := nil;
  m_GamesManager.Free;

  m_OpeningsDBManager.Free;
  m_PlysTree.Free;

  FDestroyChessBoard;
end;


procedure TAnalyseChessBoard.FCreateChessBoard;
begin
  m_ChessBoard := TChessBoard.Create(self, FChessBoardHandler);
  m_PosBaseChessBoardLayer := TPosBaseChessBoardLayer.Create;

  m_ChessBoard.AddLayer(m_PosBaseChessBoardLayer);

  m_ChessBoard.MoveNotationFormat := mnfCh4NEx;
  m_ChessBoard.FENFormat := TRUE;

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


procedure TAnalyseChessBoard.FDestroyChessBoard;
begin
  m_ChessBoard.RemoveLayer(m_PosBaseChessBoardLayer); // m_ChessBoard is destroyed by its parent
  FreeAndNil(m_PosBaseChessBoardLayer);
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
    FSetGameChanged(TRUE);

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

    cbePosSet:
    begin
      if (FIsEditing and Assigned(m_PositionEditingForm)) then
        m_PositionEditingForm.FEN := PString(d1)^;
    end;

    cbeMenu:
    begin
      if (FIsEditing) then
        EditPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y)
      else
        PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end;
end;


procedure TAnalyseChessBoard.FSetGameChanged(bValue: boolean);
begin
  if (m_bGameChanged = bValue) then
    exit;

  m_bGameChanged := bValue;
  if (m_bGameChanged) then
    m_bPlysTreeChanged := TRUE;

  FRefreshStatusBar;
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

    StatusBar.Panels[0].Width := StatusBar.Panels[0].Width + (iNewChessBoardWidth - m_ChessBoard.Width);
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
  FSetCurrentPlyIndex(0);
end;


procedure TAnalyseChessBoard.FSetToEndPosition;
begin
  FSetCurrentPlyIndex(FGetPlysCount);
end;


procedure TAnalyseChessBoard.FInitPosition;
begin
  FStopEditing;

  m_ChessBoard.InitPosition;
  ChessBoardFlipped := FALSE;

  m_iCurrentPlyIndex := 0;

  inc(m_lwPlysListUpdateID);

  m_PlysTree.Clear;
  m_PlysTree.WhiteStarts := (m_ChessBoard.PositionColor = fcWhite);
  m_PlysTree.Add(m_ChessBoard.GetPosition);
  m_bPlysTreeChanged := FALSE;

  FSetGameFileName('');
  FSetGameChanged(FALSE);

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
  ChessBoardFlipped := (not ChessBoardFlipped);
end;


function TAnalyseChessBoard.FGetChessBoardFlipped: boolean;
begin
  Result := m_ChessBoard.Flipped;
end;


procedure TAnalyseChessBoard.FSetChessBoardFlipped(bValue: boolean);
begin
  m_ChessBoard.Flipped := bValue;
  ViewFlipBoardMenuItem.Checked := bValue;
end;


procedure TAnalyseChessBoard.FileOpenMenuItemClick(Sender: TObject);
begin
  if (not FAskAndSavePGNData) then
    exit;

  if (OpenDialog.Execute) then
    FLoadPGNDataFromFile(OpenDialog.FileName);
end;


procedure TAnalyseChessBoard.FLoadPGNDataFromFile(const AFileName: TFileName);
var
  bResult: boolean;
begin
  Screen.Cursor := crHourGlass;
  try
    bResult := m_GamesManager.LoadFromFile(AFileName);
  finally
    Screen.Cursor := crDefault;
  end;

  if (Assigned(m_GamesListForm)) then
    m_GamesListForm.Refresh;

  if (m_GamesManager.GamesCount > 1) then
  begin
    GamesListAction.Update;
    if (not GamesListAction.Checked) then
      GamesListAction.Execute;
  end;

  if ((not bResult) or
      ((m_GamesManager.GetGamesCount = 1) and (m_CurrentGameItem.DataError))) then
    MessageDlg(MSG_INCORRECT_FILE_FORMAT, mtError, [mbOK], 0);
end;


procedure TAnalyseChessBoard.FilePasteMenuItemClick(Sender: TObject);
var
  wstrlData: TTntStringList;
  PGNParser: TPGNParser;
begin
  if (not TntClipboard.HasFormat(CF_TEXT)) then // TODO: CF_UNICODETEXT - ?
    exit;

  PGNParser := nil;

  wstrlData := TTntStringList.Create;
  try
    wstrlData.Text := TntClipboard.AsText;

    PGNParser := TPGNParser.Create;
    if (not PGNParser.Parse(wstrlData)) then
      exit;

    if (not FAskAndSavePGNData) then
      exit;

    FLoadPGNDataFromParser(PGNParser);

  finally
    PGNParser.Free;
    wstrlData.Free;
  end;

end;


function TAnalyseChessBoard.FLoadDataForCurrentGameInGameList: boolean;

  function NLoadPGNData: boolean;
  var
    PGNParser: TPGNParser;
  begin
    Result := FALSE;

    PGNParser := TPGNParser.Create;
    try
      if (not m_GamesManager.ParseGame(PGNParser, m_GamesManager.CurrentGameIndex)) then
        exit;

      FLoadPGNDataFromParser(PGNParser);

    finally
      PGNParser.Free;
    end;

    Result := TRUE;
  end;

var
  AGameContextData: TGameContextData;
begin // .FLoadDataForCurrentGameInGameList
  Result := FALSE;

  AGameContextData := TGameContextData.GetNullInstance;

  if (Assigned(m_CurrentGameItem) and Assigned(m_CurrentGameItem.Data)) then
    AGameContextData := m_CurrentGameItem.Data as TGameContextData;

  if (Assigned(AGameContextData.PlysTree)) then
  begin
    m_PlysTree.Assign(AGameContextData.PlysTree);
    m_bPlysTreeChanged := FALSE;
  end
  else
  begin
    if (not NLoadPGNData) then
      exit;
  end;

  m_iCurrentPlyIndex := AGameContextData.CurrentPlyIndex;
  m_bGameChanged := AGameContextData.GameChanged; 

  Result := TRUE;
end;


procedure TAnalyseChessBoard.FLoadPGNDataFromParser(const PGNParser: TPGNParser);
begin
  m_ChessBoard.BeginUpdate;
  try
    FInitPosition;

    m_PlysTree.Assign(PGNParser.Tree);

    m_bGameFileInC4NFormat := PGNParser.InC4NFormat;

    FSetToInitialPosition;
    FRefreshMoveListForm;

  finally
    m_ChessBoard.EndUpdate;
  end;
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
  m_ChessEngineInfoForm := TChessEngineInfoForm.Create(self, self);
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
  if (FIsEditing or (not Assigned(m_ChessEngine))) then
    exit;
  m_ChessEngine.SetPosition(m_ChessBoard.GetPosition);
  m_ChessEngine.CalculateReplyNonBlocking;
end;


procedure TAnalyseChessBoard.MoveListActionExecute(Sender: TObject);
begin
  if (not Assigned(m_MoveListForm)) then
  begin
    m_MoveListForm := TMoveListForm.Create(self, self);
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
  CommentsAction.Execute;

  FSetGameToGameList;

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
  m_PosBaseChessBoardLayer.SetExternalBase(m_OpeningsDBManager.DB);
  m_PosBaseChessBoardLayer.TrainingMode := ((m_OpeningsDBManager.DB <> '') and
    (m_OpeningsDBManager.Enabled));  
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


function TAnalyseChessBoard.FGetComments(iIndex: integer): WideString;
begin
  Result := m_PlysTree.Comments[iIndex];
end;


procedure TAnalyseChessBoard.FSetComments(iIndex: integer; const wstrValue: WideString);
begin
  m_PlysTree.Comments[iIndex] := wstrValue;
  FSetGameChanged(TRUE);
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
  if (Assigned(m_CommentsForm)) then
    m_CommentsForm.Refresh;
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
  InitialPositionAction.Update;
  EndPositionAction.Update;
  CommentsAction.Update; // TODO: In XP this line is not needed. What about Vista?
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
  begin
    m_bPlysTreeChanged := TRUE;
    inc(m_lwPlysListUpdateID);
  end;

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
    m_OpeningsDBManagerForm := TOpeningsDBManagerForm.Create(self, self);
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

  procedure NMakeBackup;
  var
    strExt: string;
    FileName: TFileName;
  begin
    strExt := ExtractFileExt(m_GameFileName);
    if (strExt = '') then
      strExt := '.~'
    else
      Insert('~', strExt, 2);

    FileName := ChangeFileExt(m_GameFileName, strExt);

    CopyFile(PChar(m_GameFileName), PChar(FileName), FALSE);
  end;

begin // .FSavePGNData
  Result := FALSE;

  if ((m_GameFileName = '') or (not m_bGameFileInC4NFormat)) then
    exit;

  with TPGNWriter.Create do
  try
    WriteInChess4NetFormat(m_PlysTree);
    if (FileExists(m_GameFileName)) then
      NMakeBackup;
    Data.SaveToFile(m_GameFileName);
  finally
    Free;
  end;

  FSetGameChanged(FALSE);

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


function TAnalyseChessBoard.FSetNewStandard: boolean;
begin
  Result := FAskAndSavePGNData;
  if (not Result) then
    exit;

  FInitPosition;
  FRefreshMoveListForm;

  FSetGameToGameList;
  if (Assigned(m_GamesListForm)) then
    m_GamesListForm.Refresh;
end;


procedure TAnalyseChessBoard.FSetGameToGameList;
var
  PGNWriter: TPGNWriter;
begin
  PGNWriter := TPGNWriter.Create;
  try
    PGNWriter.WriteInChess4NetFormat(m_PlysTree);
    m_GamesManager.Clear;
    m_GamesManager.AddGame(PGNWriter);
  finally
    PGNWriter.Free;
  end;
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

  FSetGameChanged(TRUE);
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

  StatusBar.Panels[0].Text := ExtractFileName(m_GameFileName);
  StatusBar.Hint := m_GameFileName;
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
  FSetGameChanged(TRUE);  

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


procedure TAnalyseChessBoard.FStartEditing;
begin
  if (Assigned(m_ChessEngine)) then
    m_ChessEngine.StopCalculation;

  if (Assigned(m_ChessEngine)) then
    m_ChessEngineInfoForm.Clear;

  if (not Assigned(m_PositionEditingForm)) then
  begin
    m_PositionEditingForm := TPositionEditingForm.Create(self, self);
    m_PositionEditingForm.PositionEditable := self;
  end;

  m_ChessBoard.Mode := mEdit;

  m_PositionEditingForm.Show;
  m_PositionEditingForm.FEN := m_ChessBoard.GetPosition;

  FRefreshStatusBar;    
end;


procedure TAnalyseChessBoard.FSetEditPiece(Piece: TFigure);
begin
  m_ChessBoard.EditPiece := Piece; 
end;


function TAnalyseChessBoard.FSetPosition(const strFEN: string): boolean;
begin
  Result := m_ChessBoard.SetPosition(strFEN);
end;


procedure TAnalyseChessBoard.FStopEditing;
begin
  if (Assigned(m_PositionEditingForm)) then
    m_PositionEditingForm.Hide;

  if (not FIsEditing) then
    exit;

  m_ChessBoard.Mode := mAnalyse;

  m_PlysTree.Clear;
  m_PlysTree.WhiteStarts := (m_ChessBoard.PositionColor = fcWhite);
  m_PlysTree.PlysOffset := 2 * m_ChessBoard.MovesOffset; 
  m_PlysTree.Add(m_ChessBoard.GetPosition);

  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;

  FRefreshStatusBar;

  FSetGameToGameList;
  if (Assigned(m_GamesListForm)) then
    m_GamesListForm.Refresh;
end;


function TAnalyseChessBoard.FIsEditing: boolean;
begin
  Result := (m_ChessBoard.Mode = mEdit);
end;


procedure TAnalyseChessBoard.FileCopyFENMenuItemClick(Sender: TObject);
begin
  TntClipboard.AsText := m_ChessBoard.GetPosition;
end;


procedure TAnalyseChessBoard.CopyActionExecute(Sender: TObject);
begin
  with TPGNWriter.Create do
  try
    WriteInChess4NetFormat(m_PlysTree);
    TntClipboard.AsText := Data.Text;
  finally
    Free;
  end;
end;


procedure TAnalyseChessBoard.CopyActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetPlysCount > 0);
end;


procedure TAnalyseChessBoard.NewStandardActionExecute(Sender: TObject);
begin
  FSetNewStandard;
end;


procedure TAnalyseChessBoard.NewCustomActionExecute(Sender: TObject);
begin
  if (not FSetNewStandard) then
    exit;

  FStartEditing;
end;


procedure TAnalyseChessBoard.NewActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not FIsEditing);
end;


procedure TAnalyseChessBoard.EditPopupColorMenuItemClick(Sender: TObject);
begin
  if (not Assigned(m_PositionEditingForm)) then
    exit;

  case (Sender as TMenuItem).Tag of
    -1:
    begin
      m_PositionEditingForm.PositionColor := fcWhite;
      EditPopupWhiteMenuItem.Checked := TRUE;
      EditPopupBlackMenuItem.Checked := FALSE;
    end;

    -2:
    begin
      m_PositionEditingForm.PositionColor := fcBlack;
      EditPopupWhiteMenuItem.Checked := FALSE;
      EditPopupBlackMenuItem.Checked := TRUE;
    end;

  else
    Assert(FALSE);
  end;

end;


procedure TAnalyseChessBoard.EditPopupMenuPopup(Sender: TObject);

  function NFindMenuItemByTag(iTag: integer): TMenuItem;
  var
    i: integer;
  begin
    with Sender as TPopupMenu do
    begin
      for i := 0 to Items.Count - 1 do
      begin
        Result := Items[i];
        if (Result.Tag = iTag) then
          exit;
      end;
    end;

    Result := nil;
  end;

const
  PIECES_TAGS: array[TFigure] of integer = (
    1, 2, 3, 4, 5, 6, 0, 7, 8, 9, 10, 11, 12);
begin
  if (not Assigned(m_PositionEditingForm)) then
    exit;

  NFindMenuItemByTag(PIECES_TAGS[m_PositionEditingForm.SelectedPiece]).Checked := TRUE;

  case m_PositionEditingForm.PositionColor of
    fcWhite:
    begin
      NFindMenuItemByTag(-1).Checked := TRUE;
      NFindMenuItemByTag(-2).Checked := FALSE;
    end;

    fcBlack:
    begin
      NFindMenuItemByTag(-1).Checked := FALSE;
      NFindMenuItemByTag(-2).Checked := TRUE;
    end;
  end;

end;


procedure TAnalyseChessBoard.EditPopupPieceMenuItemClick(
  Sender: TObject);
const
  PIECES: array[1..12] of TFigure = (
    WK, WQ, WR, WB, WN, WP, BK, BQ, BR, BB, BN, BP);
begin
  if (not Assigned(m_PositionEditingForm)) then
    exit;

  with Sender as TMenuItem do
  begin
    Checked := TRUE;
    m_PositionEditingForm.SelectedPiece := PIECES[Tag];
  end;
end;


function TAnalyseChessBoard.FGetPlysOffset: integer;
begin
  Result := m_PlysTree.PlysOffset;
end;


procedure TAnalyseChessBoard.CommentsActionExecute(Sender: TObject);
begin
  if (not Assigned(m_CommentsForm)) then
  begin
    m_CommentsForm := TCommentsForm.Create(self, self);
    m_CommentsForm.PlysProvider := self;
  end;

  if (m_CommentsForm.Showing) then
    m_CommentsForm.Hide
  else
    m_CommentsForm.Show;
end;


procedure TAnalyseChessBoard.CommentsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (Assigned(m_CommentsForm) and
    m_CommentsForm.Showing);
end;


procedure TAnalyseChessBoard.StatusBarHint(Sender: TObject);
var
  Pos: TPoint;
begin
  Pos := StatusBar.ScreenToClient(Mouse.CursorPos);

  if (Pos.X < StatusBar.Panels[0].Width) then
  begin
    StatusBar.Hint := m_GameFileName;
    StatusBar.ShowHint := TRUE;
  end
  else
    StatusBar.ShowHint := FALSE;
end;


procedure TAnalyseChessBoard.EndPositionActionExecute(Sender: TObject);
begin
  FSetToEndPosition;
end;


procedure TAnalyseChessBoard.EndPositionActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FGetCurrentPlyIndex < FGetPlysCount);
end;


procedure TAnalyseChessBoard.EditCommentActionExecute(Sender: TObject);
var
  wstrComments: WideString;
begin
  wstrComments := FGetComments(FGetCurrentPlyIndex);

  if (TCommentsEditForm.Edit(wstrComments)) then
  begin
    FSetComments(FGetCurrentPlyIndex, wstrComments);
    if (Assigned(m_CommentsForm)) then
      m_CommentsForm.Refresh;
  end;
end;


procedure TAnalyseChessBoard.EditCommentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not FIsEditing);
end;


procedure TAnalyseChessBoard.WMDropFiles(var Msg : TMessage);
var
  hDrop: THandle;

  procedure NProcessDroppedFile;
  var
    iCount: integer;
    iLen: integer;
    FileName: TFileName;
  begin
    iCount := DragQueryFile(hDrop, Cardinal(not 0), nil, 0);
    if (iCount <> 1) then
      exit;

    iLen := DragQueryFile(hDrop, 0, nil, 0);

    SetLength(FileName, iLen);
    DragQueryFile(hDrop, 0, @FileName[1], iLen + 1);

    if (not FAskAndSavePGNData) then
      exit;

    FLoadPGNDataFromFile(FileName);
  end;

begin // .WMDropFiles
  hDrop := Msg.WParam;
  try
    Msg.Result := 0;
    NProcessDroppedFile;
  finally
    DragFinish(hDrop);
  end;

  inherited;
end;


procedure TAnalyseChessBoard.GamesListActionExecute(Sender: TObject);
begin
  if (not Assigned(m_GamesListForm)) then
  begin
    m_GamesListForm := TGamesListForm.Create(self, self);
    m_GamesListForm.GamesListProvider := m_GamesManager;
  end;

  if (m_GamesListForm.Showing) then
    m_GamesListForm.Hide
  else
    m_GamesListForm.Show;
end;


procedure TAnalyseChessBoard.GamesListActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (Assigned(m_GamesListForm) and
    m_GamesListForm.Showing);
end;


procedure TAnalyseChessBoard.FOnGamesManagerChanged(Sender: TObject);
begin
  FSaveGameContextData;

  if (m_GamesManager.CurrentGameIndex >= 0) then
    m_CurrentGameItem := m_GamesManager.Games[m_GamesManager.CurrentGameIndex];

  if (FLoadDataForCurrentGameInGameList) then
    FSetGameFileName(m_CurrentGameItem.FileName);
  else
  begin
    m_CurrentGameItem := nil;
    FInitPosition;
  end;

  FRefreshMoveListForm;
end;


procedure TAnalyseChessBoard.FSaveGameContextData;
var
  AGameContextData: TGameContextData;
begin
  if (not Assigned(m_CurrentGameItem)) then
    exit;

  if ((m_iCurrentPlyIndex <> 0) or m_bGameChanged or m_bPlysTreeChanged) then
  begin
    AGameContextData := m_CurrentGameItem.Data as TGameContextData;
    if (not Assigned(AGameContextData)) then
    begin
      AGameContextData := TGameContextData.Create;
      m_CurrentGameItem.SetData(TObject(AGameContextData));
    end;

    AGameContextData.CurrentPlyIndex := m_iCurrentPlyIndex;
    AGameContextData.GameChanged := m_bGameChanged;
    if (m_bPlysTreeChanged) then
      AGameContextData.PlysTree := m_PlysTree;
  end;
  
end;

////////////////////////////////////////////////////////////////////////////////
// TGameContextData

destructor TGameContextData.Destroy;
begin
  m_PlysTree.Free;
  inherited;
end;


procedure TGameContextData.FSetPlysTree(const Value: TPlysTree);
begin
  if (not Assigned(m_PlysTree)) then
    m_PlysTree := TPlysTree.Create;
  m_PlysTree.Assign(Value);
end;


class function TGameContextData.GetNullInstance: TGameContextData;
begin
  if (not Assigned(g_NullGameContextDataInstance)) then
    g_NullGameContextDataInstance := TGameContextData.Create;
  Result := g_NullGameContextDataInstance;
end;

initialization

finalization
  FreeAndNil(g_NullGameContextDataInstance);

end.
