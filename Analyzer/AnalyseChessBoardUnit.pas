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
  TMode = (modAnalysis, modTraining);

  TAnalyseChessBoard = class;

  TModeStrategyBase = class
  private
    m_ChessBoard: TAnalyseChessBoard;
    function FGetMode: TMode;
  protected
    constructor RCreate(AChessBoard: TAnalyseChessBoard); virtual;
    property ChessBoard: TAnalyseChessBoard read m_ChessBoard;
  public
    procedure OnMove(const strMove: string); virtual; abstract;
    procedure OnSetPosition(const strPos: string); virtual; abstract;
    procedure OnForwardingMove; virtual;
    procedure OnLoadGameFrom; virtual;
  end;

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
    PopupReturnFromLine: TTntMenuItem;
    EndPositionAction: TAction;
    PositionEndMenuItem: TTntMenuItem;
    EditCommentAction: TAction;
    N10: TTntMenuItem;
    EditCommentMenuItem: TTntMenuItem;
    GamesListAction: TAction;
    GamesListMenuItem: TTntMenuItem;
    ModeMenuItem: TTntMenuItem;
    ModeAnalysisMenuItem: TTntMenuItem;
    ModeTrainingMenuItem: TTntMenuItem;
    AnalysisModeAction: TAction;
    TrainingModeAction: TAction;
    NextGameAction: TAction;
    PreviousGameAction: TAction;
    N11: TTntMenuItem;
    PreviousGame1: TTntMenuItem;
    NextGame1: TTntMenuItem;
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
    procedure AnalysisModeActionExecute(Sender: TObject);
    procedure TrainingModeActionExecute(Sender: TObject);
    procedure ModeActionUpdate(Sender: TObject);
    procedure PreviousGameActionExecute(Sender: TObject);
    procedure NextGameActionExecute(Sender: TObject);
    procedure PreviousGameActionUpdate(Sender: TObject);
    procedure NextGameActionUpdate(Sender: TObject);
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
    m_bGameChangedLocal: boolean;
    m_bPlysTreeChanged: boolean;

    m_bDontAskSaveGameOnGameSwitch: boolean;

    m_GameFileName: TFileName;
    m_bGameFileInC4NFormat: boolean;

    m_GamesManager: TGamesManager;
    m_GamesListForm: TGamesListForm;

    m_ModeStrategy: TModeStrategyBase;

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
    function FAskAndSavePGNDataOnGameSwitch: boolean;

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
    procedure FSetCurrentPlyIndex(iValue: integer); overload;
    procedure FSetCurrentPlyIndex(iValue: integer; bForwardMoveFlag: boolean); overload;

    function IPlysProvider.HasSeveralPlysForPlyIndex = FHasSeveralPlysForPlyIndex;
    function FHasSeveralPlysForPlyIndex(iPlyIndex: integer): boolean;

    procedure IPlysProvider.GetPlysForPlyIndex = FGetPlysForPlyIndex;
    procedure FGetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);

    function IPlysProvider.SetPlyForPlyIndex = FSetPlyForPlyIndex;
    function FSetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    function IPlysProvider.GetPlyStatus = FGetPlyStatus;
    function FGetPlyStatus(iPlyIndex: integer): TPlyStatuses;

    function IPlysProvider.IsPlyDisclosed = FIsPlyDisclosed;
    function FIsPlyDisclosed(iPlyIndex: integer): boolean;

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

    procedure FRefreshMoveListAndComments;
    procedure FAdjustPlysList(const strMove: string);

    procedure FOnURLQueryReady(Sender: TURLVersionQuery);
    procedure FOnOpeningsDBManagerChanged(Sender: TObject);

    procedure FSetGameFileName(const AGameFileName: TFileName);

    procedure FStartEditing;
    procedure FStopEditing;
    function FIsEditing: boolean;

    procedure FSetGameChanged(bValue: boolean);

    function FSetNewStandard: boolean;
    procedure FSetGameToGameList(iGameIndex: integer = -1);

    function FGetChessBoardFlipped: boolean;
    procedure FSetChessBoardFlipped(bValue: boolean);

    procedure FRefreshStatusBar;

    procedure FOnGamesManagerChanged(Sender: TObject);
    procedure FOnCurrentGameIndexChanged(iOldGameIndex: integer; var iNewGameIndex: integer);

    function FGetMode: TMode;

    procedure FSaveGameContextData(iGameIndex: integer);

    property ChessBoardFlipped: boolean read FGetChessBoardFlipped write FSetChessBoardFlipped;
  end;

implementation

uses
  Windows, TntClipbrd, ShellAPI,
  //
  GlobalsLocalUnit, DontShowMessageDlgUnit,
  IniSettingsUnit, PGNWriterUnit, SplashFormUnit, CommentsEditFormUnit,
  IncorrectMoveFormUnit;

{$R *.dfm}

type
  TGameContextData = class
  private
    m_iCurrentPlyIndex: integer;
    m_bGameChanged: boolean;
    m_PlysTree: TPlysTree;
    m_DontAskSaveGame: boolean;
    procedure FSetPlysTree(const Value: TPlysTree);
  public
    destructor Destroy; override;
    class function GetNullInstance: TGameContextData;
    property CurrentPlyIndex: integer read m_iCurrentPlyIndex write m_iCurrentPlyIndex;
    property GameChanged: boolean read m_bGameChanged write m_bGameChanged;
    property PlysTree: TPlysTree read m_PlysTree write FSetPlysTree;
    property DontAskSaveGame: boolean read m_DontAskSaveGame write m_DontAskSaveGame;
  end;


  TModeStrategyAnalysis = class(TModeStrategyBase)
  protected
    constructor RCreate(AChessBoard: TAnalyseChessBoard); override;
  public
    procedure OnMove(const strMove: string); override;
    procedure OnSetPosition(const strPos: string); override;
  end;


  TModeStrategyTraining = class(TModeStrategyBase)
  private
    m_bReplyingFlag: boolean;
    m_bForwardingFlag: boolean;
    m_ReplyDelayedTimer: TTimer;
    m_SwitchToNextGameTimer: TTimer;
    m_bDontSwitchToNextGame: boolean;
    procedure FOnReplyDelayedTimer(Sender: TObject);
    procedure FOnSwitchToNextGameTimer(Sender: TObject);
    procedure FResetPlysTree;
    procedure FDiscloseCurrentPly;
    function FIsCurrentPlyDisclosed: boolean;
    function FMayDoMove(const strMove: string): boolean;
    procedure FShowHint;
    function FIsLastPlyInLine: boolean;
    procedure FCreateTimers;
    procedure FDestroyTimers;
  protected
    constructor RCreate(AChessBoard: TAnalyseChessBoard); override;
  public
    destructor Destroy; override;
    procedure OnMove(const strMove: string); override;
    procedure OnSetPosition(const strPos: string); override;
    procedure OnForwardingMove; override;
    procedure OnLoadGameFrom; override;
  end;

const
  MSG_GAME_CHANGED_SAVE_CHANGES = 'Current game was modified. Save the changes?';
  MSG_INCORRECT_FILE_FORMAT = 'Incorrect file format encountered or data is broken!';
  MSG_FILE_EXISTS_OVERWRITE = 'File %s already exists. Do you want it to be overwritten?';
  MSG_LINE_TO_BE_DELETED = 'Are you sure you want to delete current line?';
  MSG_SET_LINE_TO_MAIN = 'Are you sure you want current line be set to main?';
  MSG_END_POSITION_SWITCH_NEXT_GAME = 'End position of line is reached. Do you want to switch to the next game?';

  LBL_CHESS4NET_ANALYZER_VER = 'Chess4Net Analyzer %s';

  LBL_EDITING = 'Editing';
  LBL_CHANGED = 'Changed';
  LBL_TRAINING = 'Training';

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
  m_GamesManager.OnCurrentGameIndexChange := FOnCurrentGameIndexChanged; 

  m_PlysTree := TPlysTree.Create;

  m_OpeningsDBManager := TOpeningsDBManager.Create;
  m_OpeningsDBManager.OnChanged := FOnOpeningsDBManagerChanged;

  m_ModeStrategy := TModeStrategyAnalysis.RCreate(self);

  FCreateChessBoard;
  FInitPosition;

  FRefreshStatusBar;

  DragAcceptFiles(Handle, TRUE);
end;


procedure TAnalyseChessBoard.FRefreshStatusBar;
begin
  case FGetMode of
    modAnalysis:
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

    modTraining:
    begin
      StatusBar.Panels[1].Text := LBL_TRAINING;
    end;

  end; // case

end;


procedure TAnalyseChessBoard.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, FALSE);

  FDestroyChessEngine;

  m_GamesListForm.GamesListProvider := nil;
  m_GamesManager.OnChanged := nil;
  m_GamesManager.OnCurrentGameIndexChange := nil;

  m_MoveListForm.PlysProvider := nil;
  m_OpeningsDBManagerForm.OpeningsDBManagerProvider := nil;

  m_GamesManager.Free;

  m_OpeningsDBManager.Free;
  m_PlysTree.Free;

  FDestroyChessBoard;

  m_ModeStrategy.Free;  

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
begin // .FChessBoardHandler
  case e of
    cbeMoved:
    begin
      m_ModeStrategy.OnMove(PString(d1)^);
    end;

    cbePosSet:
    begin
      m_ModeStrategy.OnSetPosition(PString(d1)^);
    end;

    cbeMenu:
    begin
      case FGetMode of
        modAnalysis:
        begin
          if (FIsEditing) then
            EditPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y)
          else
            PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;

        modTraining:
        begin
          PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;

      end; // case FGetMode

    end;

  end; // case e

end;


procedure TAnalyseChessBoard.FAdjustPlysList(const strMove: string);
var
  iPly: integer;
begin
  iPly := FGetCurrentPlyIndex;

  if (FGetPlysCount >= iPly) then
  begin
    if (m_PlysTree[iPly] = strMove) then
    begin
      FRefreshMoveListAndComments; // cursor moved
      exit;
    end;
  end;

  m_PlysTree.Add(iPly, m_ChessBoard.GetPosition, strMove, [psUserLine]);
  FSetGameChanged(TRUE);

  inc(m_lwPlysListUpdateID);
  FRefreshMoveListAndComments;
end;


procedure TAnalyseChessBoard.FSetGameChanged(bValue: boolean);
begin
  if (bValue) then
  begin
    m_bGameChangedLocal := TRUE;
    m_bPlysTreeChanged := TRUE;
  end;

  if (m_bGameChanged = bValue) then
    exit;

  m_bGameChanged := bValue;

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

  FSetGameFileName('');
  FSetGameChanged(FALSE);
  m_bPlysTreeChanged := FALSE;

  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
end;


procedure TAnalyseChessBoard.FForwardMove;
var
  bRes: boolean;
begin
  if (FGetPlysCount <= FGetCurrentPlyIndex) then
    exit;

  m_ModeStrategy.OnForwardingMove;

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
  ACurrentGameItem: TGameItem;
begin
  Screen.Cursor := crHourGlass;
  try
    bResult := m_GamesManager.LoadFromFile(AFileName);
  finally
    Screen.Cursor := crDefault;
  end;

  if (m_GamesManager.GamesCount > 1) then
  begin
    GamesListAction.Update;
    if (not GamesListAction.Checked) then
      GamesListAction.Execute;
  end;

  ACurrentGameItem := m_GamesManager.Games[m_GamesManager.CurrentGameIndex];
  if ((not bResult) or
      ((m_GamesManager.GamesCount = 1) and (ACurrentGameItem.DataError))) then
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
  GameContextData: TGameContextData;
  CurrentGameItem: TGameItem;
begin // .FLoadDataForCurrentGameInGameList
  Result := FALSE;

  CurrentGameItem := m_GamesManager.Games[m_GamesManager.CurrentGameIndex]; 

  if (Assigned(CurrentGameItem.Data)) then
    GameContextData := CurrentGameItem.Data as TGameContextData
  else
    GameContextData := TGameContextData.GetNullInstance;

  if (Assigned(GameContextData.PlysTree)) then
    m_PlysTree.Assign(GameContextData.PlysTree)
  else
  begin
    if (not NLoadPGNData) then
      exit;
  end;

  FSetCurrentPlyIndex(GameContextData.CurrentPlyIndex, FALSE);

  FSetGameChanged(GameContextData.GameChanged);
  m_bPlysTreeChanged := FALSE;
  m_bGameChangedLocal := FALSE;

  m_bDontAskSaveGameOnGameSwitch := GameContextData.DontAskSaveGame;

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
    FRefreshMoveListAndComments;

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
  FSetCurrentPlyIndex(iValue, TRUE);
end;


procedure TAnalyseChessBoard.FSetCurrentPlyIndex(iValue: integer; bForwardMoveFlag: boolean);
var
  iSavedPlyIndex: integer;
begin
  if (bForwardMoveFlag and ((iValue - FGetCurrentPlyIndex) = 1)) then
    FForwardMove
  else if ((iValue >= 0) and (iValue <= FGetPlysCount)) then
  begin
    iSavedPlyIndex := m_iCurrentPlyIndex;
    m_iCurrentPlyIndex := iValue;

    if (not m_ChessBoard.SetPosition(m_PlysTree.Position[iValue])) then
      m_iCurrentPlyIndex := iSavedPlyIndex;

    FRefreshMoveListAndComments;
    FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
  end;
end;


procedure TAnalyseChessBoard.FRefreshMoveListAndComments;
begin
  if (Assigned(m_MoveListForm)) then
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
  NextGameAction.Update;
  PreviousGameAction.Update;
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


function TAnalyseChessBoard.FIsPlyDisclosed(iPlyIndex: integer): boolean;
begin
  Result := m_PlysTree.IsDisclosed[iPlyIndex];
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
  Res: TModalResult;
begin
  Result := TRUE;

  SaveAction.Update;
  if (not SaveAction.Enabled) then
    exit;

  Res := MessageDlg(MSG_GAME_CHANGED_SAVE_CHANGES, mtConfirmation, mbYesNoCancel, 0);

  case Res of
    mrCancel:
      Result := FALSE;
    mrYes:
      SaveAction.Execute;
    mrNo:
      ;
  end;
end;


function TAnalyseChessBoard.FAskAndSavePGNDataOnGameSwitch: boolean;
var
  Res: TModalResult;
  bDontShowFlag: boolean;
begin
  Result := TRUE;

  SaveAction.Update;
  if (not SaveAction.Enabled) then
    exit;

  bDontShowFlag := FALSE;
  Res := TDontShowMessageDlg.Show(MSG_GAME_CHANGED_SAVE_CHANGES, mtConfirmation,
    mbYesNoCancel, bDontShowFlag);

  case Res of
    mrCancel:
      Result := FALSE;
    mrYes:
      SaveAction.Execute;
    mrNo:
      ;
  end;

  if (Result) then
    m_bDontAskSaveGameOnGameSwitch := bDontShowFlag;
    
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

  AnalysisModeAction.Execute;    

  FInitPosition;
  FRefreshMoveListAndComments;

  m_GamesManager.Clear;
  FSetGameToGameList;
end;


procedure TAnalyseChessBoard.FSetGameToGameList(iGameIndex: integer = -1);
var
  PGNWriter: TPGNWriter;
begin
  PGNWriter := TPGNWriter.Create;
  try
    PGNWriter.WriteInChess4NetFormat(m_PlysTree);
    if (iGameIndex >= 0) then
      m_GamesManager.ChangeGame(PGNWriter, iGameIndex)
    else
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
  (Sender as TAction).Enabled := ((FGetMode = modAnalysis) and (FGetCurrentPlyIndex > 0));  
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
  (Sender as TAction).Enabled := ((FGetMode = modAnalysis) and (FGetPlysCount > 0));
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
  FRefreshMoveListAndComments;
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
end;


function TAnalyseChessBoard.FIsEditing: boolean;
begin
  Result := ((FGetMode = modAnalysis) and (m_ChessBoard.Mode = mEdit));
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
  (Sender as TAction).Enabled := ((FGetMode = modAnalysis) and (not FIsEditing));
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
  if (Assigned(m_GamesListForm)) then
    m_GamesListForm.RefreshAll;
end;


procedure TAnalyseChessBoard.FOnCurrentGameIndexChanged(iOldGameIndex: integer;
  var iNewGameIndex: integer);
var
  ACurrentGameItem: TGameItem;
begin
  if (iOldGameIndex >= 0) then
  begin
    if (m_bGameChangedLocal and (not m_bDontAskSaveGameOnGameSwitch) and
        (not FAskAndSavePGNDataOnGameSwitch)) then
    begin
      iNewGameIndex := iOldGameIndex;
      exit;
    end;
  end;

  FSaveGameContextData(iOldGameIndex);

  if (FLoadDataForCurrentGameInGameList) then
  begin
    ACurrentGameItem := m_GamesManager.Games[m_GamesManager.CurrentGameIndex];
    FSetGameFileName(ACurrentGameItem.FileName);
    m_ModeStrategy.OnLoadGameFrom;
  end
  else
    FInitPosition;

  inc(m_lwPlysListUpdateID);
  FRefreshMoveListAndComments;

  if (Assigned(m_GamesListForm)) then
    m_GamesListForm.Refresh;
end;


procedure TAnalyseChessBoard.FSaveGameContextData(iGameIndex: integer);
var
  GameContextData: TGameContextData;
  OldGameItem: TGameItem;
begin
  if (iGameIndex < 0) then
    exit;

  if ((m_iCurrentPlyIndex <> 0) or m_bGameChanged or m_bPlysTreeChanged) then
  begin
    OldGameItem := m_GamesManager.Games[iGameIndex];

    GameContextData := OldGameItem.Data as TGameContextData;
    if (not Assigned(GameContextData)) then
    begin
      GameContextData := TGameContextData.Create;
      OldGameItem.SetData(TObject(GameContextData));
    end;

    GameContextData.CurrentPlyIndex := m_iCurrentPlyIndex;
    GameContextData.GameChanged := m_bGameChanged;
    if (m_bPlysTreeChanged) then
      GameContextData.PlysTree := m_PlysTree;
    GameContextData.DontAskSaveGame := m_bDontAskSaveGameOnGameSwitch;

    OldGameItem.FileName := m_GameFileName;
  end;

  if (m_bGameChanged and m_bPlysTreeChanged) then
    FSetGameToGameList(iGameIndex);
    
end;


procedure TAnalyseChessBoard.AnalysisModeActionExecute(Sender: TObject);
var
  OldModeStrategy: TModeStrategyBase;
begin
  (Sender as TAction).Checked := TRUE;
  ModeAnalysisMenuItem.Checked := TRUE; // Otherwise radio items won't work on Win7

  OldModeStrategy := m_ModeStrategy;
  m_ModeStrategy := TModeStrategyAnalysis.RCreate(self);
  OldModeStrategy.Free;

  FRefreshStatusBar;
end;


procedure TAnalyseChessBoard.TrainingModeActionExecute(Sender: TObject);
var
  OldModeStrategy: TModeStrategyBase;
begin
  (Sender as TAction).Checked := TRUE;
  ModeTrainingMenuItem.Checked := TRUE; // Otherwise radio items won't work on Win7

  OldModeStrategy := m_ModeStrategy;
  m_ModeStrategy := TModeStrategyTraining.RCreate(self);
  OldModeStrategy.Free;

  FRefreshStatusBar;
end;


procedure TAnalyseChessBoard.ModeActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not FIsEditing);
end;


function TAnalyseChessBoard.FGetMode: TMode;
begin
  Result := m_ModeStrategy.FGetMode;
end;


procedure TAnalyseChessBoard.PreviousGameActionExecute(Sender: TObject);
begin
  m_GamesManager.CurrentGameIndex := m_GamesManager.CurrentGameIndex - 1;
end;


procedure TAnalyseChessBoard.PreviousGameActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (m_GamesManager.CurrentGameIndex > 0);
end;


procedure TAnalyseChessBoard.NextGameActionExecute(Sender: TObject);
begin
  m_GamesManager.CurrentGameIndex := m_GamesManager.CurrentGameIndex + 1;
end;


procedure TAnalyseChessBoard.NextGameActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (m_GamesManager.CurrentGameIndex < (m_GamesManager.GamesCount - 1));
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

////////////////////////////////////////////////////////////////////////////////
// TModeStrategyBase

constructor TModeStrategyBase.RCreate(AChessBoard: TAnalyseChessBoard);
begin
  inherited Create;

  m_ChessBoard := AChessBoard;
end;


function TModeStrategyBase.FGetMode: TMode;
begin
  Result := modAnalysis; // To avoid warning

  if (self is TModeStrategyAnalysis) then
    Result := modAnalysis
  else if (self is TModeStrategyTraining) then
    Result := modTraining
  else
    Assert(FALSE);
end;


procedure TModeStrategyBase.OnForwardingMove;
begin
end;


procedure TModeStrategyBase.OnLoadGameFrom;
begin
end;

////////////////////////////////////////////////////////////////////////////////
// TModeStrategyAnalysis

constructor TModeStrategyAnalysis.RCreate(AChessBoard: TAnalyseChessBoard);
begin
  inherited;

  with ChessBoard do
  begin
    if (Assigned(m_MoveListForm)) then
      m_MoveListForm.TrainingMode := FALSE;
  end;
  
end;


procedure TModeStrategyAnalysis.OnMove(const strMove: string);
begin
  with ChessBoard do
  begin
    inc(m_iCurrentPlyIndex);
    FAdjustPlysList(strMove);
    FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
  end;
end;


procedure TModeStrategyAnalysis.OnSetPosition(const strPos: string);
begin
  with ChessBoard do
  begin
    if (FIsEditing and Assigned(m_PositionEditingForm)) then
      m_PositionEditingForm.FEN := strPos;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TModeStrategyTraining

constructor TModeStrategyTraining.RCreate(AChessBoard: TAnalyseChessBoard);
begin
  inherited;

  FCreateTimers;

  FResetPlysTree;

  with ChessBoard do
  begin
    if (Assigned(m_MoveListForm)) then
      m_MoveListForm.TrainingMode := TRUE;
    FDiscloseCurrentPly;
  end;

end;


destructor TModeStrategyTraining.Destroy;
begin
  FDestroyTimers;
  inherited;
end;


procedure TModeStrategyTraining.FCreateTimers;
begin
  m_ReplyDelayedTimer := TTimer.Create(nil);
  with m_ReplyDelayedTimer do
  begin
    Enabled := FALSE;
    Interval := 200;
    OnTimer := FOnReplyDelayedTimer;
  end;

  m_SwitchToNextGameTimer := TTimer.Create(nil);
  with m_SwitchToNextGameTimer do
  begin
    Enabled := FALSE;
    Interval := 500;
    OnTimer := FOnSwitchToNextGameTimer;
  end;

end;


procedure TModeStrategyTraining.FDestroyTimers;
begin
  FreeAndNil(m_SwitchToNextGameTimer);
  FreeAndNil(m_ReplyDelayedTimer);
end;


procedure TModeStrategyTraining.OnMove(const strMove: string);
var
  bPlyJustDisclosedFlag: boolean;
begin
  with ChessBoard do
  begin
    if (not self.FMayDoMove(strMove)) then
    begin
      m_ChessBoard.TakeBack;

      if (FIsLastPlyInLine) then
        exit;

      if (TIncorrectMoveForm.Show = immrShowHint) then
        FShowHint;

      exit;
    end;

    inc(m_iCurrentPlyIndex);
    FAdjustPlysList(strMove);

    bPlyJustDisclosedFlag := (not self.FIsCurrentPlyDisclosed);
    self.FDiscloseCurrentPly;

    FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
  end; // with

  try
    if (FIsLastPlyInLine) then
    begin
      if (m_bDontSwitchToNextGame or (not bPlyJustDisclosedFlag)) then
        exit;

      ChessBoard.NextGameAction.Update;
      if (ChessBoard.NextGameAction.Enabled) then
        m_SwitchToNextGameTimer.Enabled := TRUE;

      exit;
    end;

    if (m_bReplyingFlag or m_bForwardingFlag) then
      exit;

    m_bReplyingFlag := TRUE;

    m_ReplyDelayedTimer.Enabled := TRUE;

  finally
    m_bForwardingFlag := FALSE;
    m_bReplyingFlag := FALSE;
  end;

end;


function TModeStrategyTraining.FIsLastPlyInLine: boolean;
begin
  with ChessBoard do
    Result := (FGetCurrentPlyIndex >= FGetPlysCount);
end;


function TModeStrategyTraining.FMayDoMove(const strMove: string): boolean;
var
  m_strlPlys: TStrings;
begin
  m_strlPlys := TStringList.Create;
  try
    ChessBoard.FGetPlysForPlyIndex(ChessBoard.FGetCurrentPlyIndex + 1, m_strlPlys);
    Result := (m_strlPlys.IndexOf(strMove) >= 0);
  finally
    m_strlPlys.Free;
  end;
end;


procedure TModeStrategyTraining.OnSetPosition(const strPos: string);
begin
  m_bReplyingFlag := FALSE;
  FDiscloseCurrentPly;
end;


procedure TModeStrategyTraining.OnForwardingMove;
begin
  m_bForwardingFlag := TRUE;
end;


procedure TModeStrategyTraining.FOnReplyDelayedTimer(Sender: TObject);
begin
  if (ChessBoard.m_ChessBoard.IsMoveAnimating) then
    exit;

  m_ReplyDelayedTimer.Enabled := FALSE;
  ChessBoard.ForwardMoveAction.Execute;
end;


procedure TModeStrategyTraining.FOnSwitchToNextGameTimer(Sender: TObject);
var
  Res: TModalResult;
begin
  if (ChessBoard.m_ChessBoard.IsMoveAnimating) then
    exit;

  m_SwitchToNextGameTimer.Enabled := FALSE;

  Res := TDontShowMessageDlg.Show(MSG_END_POSITION_SWITCH_NEXT_GAME,
    mtConfirmation, [mbYes, mbNo], m_bDontSwitchToNextGame);
  if (Res = mrYes) then
    ChessBoard.NextGameAction.Execute;
end;


procedure TModeStrategyTraining.FResetPlysTree;
begin
  ChessBoard.m_PlysTree.ClearAllDisclosedPlys;
end;


function TModeStrategyTraining.FIsCurrentPlyDisclosed: boolean;
begin
  with ChessBoard do
    Result := m_PlysTree.IsDisclosed[FGetCurrentPlyIndex];
end;


procedure TModeStrategyTraining.FDiscloseCurrentPly;
begin
  with ChessBoard do
  begin
    m_PlysTree.IsDisclosed[FGetCurrentPlyIndex] := TRUE;
    if (Assigned(m_MoveListForm)) then
      m_MoveListForm.Refresh;
  end;
end;


procedure TModeStrategyTraining.OnLoadGameFrom;
begin
  with ChessBoard do
    m_ChessBoard.Flipped := (ChessBoardFlipped = FGetWhiteStarts);
end;


procedure TModeStrategyTraining.FShowHint;
var
  APlys: TStrings;
begin
  APlys := nil;

  with ChessBoard do
  try
    APlys := TStringList.Create;
    FGetPlysForPlyIndex(FGetCurrentPlyIndex + 1, APlys);
    MessageDlg(APlys.CommaText, mtCustom, [mbOk], 0);
  finally
    APlys.Free;
  end;

end;

initialization

finalization
  FreeAndNil(g_NullGameContextDataInstance);

end.
