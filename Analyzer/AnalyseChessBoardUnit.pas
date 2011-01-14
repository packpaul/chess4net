unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntForms, TntMenus, Menus, Classes, Controls, ExtCtrls, Messages,
  ComCtrls, Dialogs, ActnList, ImgList, AppEvnts,
  //
  ChessBoardUnit, PosBaseChessBoardUnit, ChessEngineInfoUnit, ChessEngine,
  MoveListFormUnit, PlysTreeUnit;

type
  TAnalyseChessBoard = class(TTntForm, IPlysProvider)
    MainMenu: TTntMainMenu;
    FileMenuItem: TTntMenuItem;
    OpenPGNMenuItem: TTntMenuItem;
    SavePGNMenuItem: TTntMenuItem;
    N2: TTntMenuItem;
    CopyPGNMenuItem: TTntMenuItem;
    PastePGNMenuItem: TTntMenuItem;
    N1: TTntMenuItem;
    ExitMenuItem: TTntMenuItem;
    HelpMenuItem: TTntMenuItem;
    ContentsMenuItem: TTntMenuItem;
    N3: TTntMenuItem;
    AboutMenuItem: TTntMenuItem;
    ChessBoardPanel: TPanel;
    StatusBar: TStatusBar;
    ViewMenuItem: TTntMenuItem;
    ViewMoveListMenuItem: TTntMenuItem;
    ViewPosDBManagerMenuItem: TTntMenuItem;
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
    OpenPGNDialog: TOpenDialog;
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
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure PositionInitialMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewFlipBoardMenuItemClick(Sender: TObject);
    procedure OpenPGNMenuItemClick(Sender: TObject);
    procedure PastePGNMenuItemClick(Sender: TObject);
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
  private
    m_ChessBoard: TPosBaseChessBoard;
    m_strPosBaseName: string;
    m_ResizingType: (rtNo, rtHoriz, rtVert);

    m_ChessEngine: TChessEngine;
    m_ChessEngineInfoForm: TChessEngineInfoForm;

    m_MoveListForm: TMoveListForm;

    m_PlysTree: TPlysTree;
    m_iCurrentPlyIndex: integer;

    m_lwPlysListUpdateID: LongWord;

    m_bNotRefreshFlag: boolean;

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

    function FLoadPGNData(const PGNData: TStrings): boolean;

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

    function IPlysProvider.GetPlysCountForPlyIndex = FGetPlysCountForPlyIndex;
    function FGetPlysCountForPlyIndex(iPlyIndex: integer): integer;

    procedure IPlysProvider.GetPlysForPlyIndex = FGetPlysForPlyIndex;
    procedure FGetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);

    function IPlysProvider.SetPlyForPlyIndex = FSetPlyForPlyIndex;
    function FSetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    procedure FTakebackMove;
    procedure FForwardMove;

    procedure FRefreshMoveListForm;
  end;

implementation

uses
  SysUtils, Windows, Clipbrd,
  //
  PGNParserUnit, ChessRulesEngine;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TAnalyseChessBoard

procedure TAnalyseChessBoard.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TAnalyseChessBoard.FormCreate(Sender: TObject);
begin
  m_strPosBaseName := 'Sicilian';

  m_PlysTree := TPlysTree.Create;

  FCreateChessBoard;
  FInitPosition;
end;


procedure TAnalyseChessBoard.FormDestroy(Sender: TObject);
begin
  FDestroyChessEngine;
  m_PlysTree.Free;
end;


procedure TAnalyseChessBoard.FCreateChessBoard;
begin
  m_ChessBoard := TPosBaseChessBoard.Create(self, FChessBoardHandler, '');

  with ChessBoardPanel do
    SetBounds(Left, Top, m_ChessBoard.ClientWidth, m_ChessBoard.ClientHeight);

  m_ChessBoard.BorderStyle := bsNone;

  m_ChessBoard.Align := alClient;
  m_ChessBoard.Parent := ChessBoardPanel;
  m_ChessBoard.Visible := TRUE;

  m_ChessBoard.Mode := mAnalyse;

  m_ChessBoard.SetExternalBase(m_strPosBaseName);
  m_ChessBoard.pTrainingMode := TRUE;

  m_ChessBoard.InitPosition;
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

    m_PlysTree.Add(iPly, m_ChessBoard.GetPosition, strMove);

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


procedure TAnalyseChessBoard.PositionInitialMenuItemClick(Sender: TObject);
begin
  FInitPosition;
  FRefreshMoveListForm;
end;


procedure TAnalyseChessBoard.FInitPosition;
begin
  m_ChessBoard.InitPosition;

  m_iCurrentPlyIndex := 0;

  inc(m_lwPlysListUpdateID);

  m_PlysTree.Clear;
  m_PlysTree.Add(m_ChessBoard.GetPosition);

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


procedure TAnalyseChessBoard.OpenPGNMenuItemClick(Sender: TObject);
var
  strlData: TStringList;
begin
  if (not OpenPGNDialog.Execute) then
    exit;

  strlData := TStringList.Create;
  try
    strlData.LoadFromFile(OpenPGNDialog.FileName);
    FLoadPGNData(strlData);
  finally
    strlData.Free;
  end;

end;


procedure TAnalyseChessBoard.PastePGNMenuItemClick(Sender: TObject);
var
  strlData: TStringList;
begin
  if (not Clipboard.HasFormat(CF_TEXT)) then
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
  ChessRulesEngine: TChessRulesEngine;
  i: integer;
begin
  Result := FALSE;

  PGNParser := TPGNParser.Create;
  try
    if (not PGNParser.Parse(PGNData)) then
      exit;

    FInitPosition;

    ChessRulesEngine := TChessRulesEngine.Create;
    try
      ChessRulesEngine.InitNewGame;
      for i := 0 to PGNParser.PGNMoveList.Count - 1 do
      begin
        if (ChessRulesEngine.DoMove(PGNParser.PGNMoveList[i])) then
          m_PlysTree.Add(ChessRulesEngine.GetPosition, PGNParser.PGNMoveList[i])
        else
          exit;
      end;

    finally
      ChessRulesEngine.Free;
    end;

  finally
    PGNParser.Free;
    FRefreshMoveListForm;    
  end;

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
end;


function TAnalyseChessBoard.FGetPlysCount: integer;
begin
  Result := m_PlysTree.Count - 1;
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
  if (not m_bNotRefreshFlag) then
    m_MoveListForm.Refresh;
end;


procedure TAnalyseChessBoard.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  // Because TSpeedButton actions do not work on Vista
  TakebackMoveAction.Update;
  ForwardMoveAction.Update;
end;


function TAnalyseChessBoard.FGetPlysCountForPlyIndex(iPlyIndex: integer): integer;
begin
  Result := m_PlysTree.GetPlysCountForPlyIndex(iPlyIndex);
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
    inc(m_lwPlysListUpdateID);
    FSetCurrentPlyIndex(FGetCurrentPlyIndex);
  end;
end;


procedure TAnalyseChessBoard.SelectLineActionExecute(Sender: TObject);
begin
  // TODO:
end;


procedure TAnalyseChessBoard.SelectLineActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((FGetCurrentPlyIndex > 0) and
    (FGetPlysCountForPlyIndex(FGetCurrentPlyIndex) > 1));
end;

end.
