unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntForms, TntMenus, Menus, Classes, Controls, ExtCtrls, Messages,
  ComCtrls, Dialogs, ActnList,
  //
  ChessBoardUnit, PosBaseChessBoardUnit, ChessEngineInfoUnit, ChessEngine,
  MoveListFormUnit, ImgList;

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
  private
    m_ChessBoard: TPosBaseChessBoard;
    m_strPosBaseName: string;
    m_ResizingType: (rtNo, rtHoriz, rtVert);

    m_ChessEngine: TChessEngine;
    m_ChessEngineInfoForm: TChessEngineInfoForm;

    m_MoveListForm: TMoveListForm;

    m_strlPlysList: TStringList;

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
{
    procedure IPlysProvider.ForwardPly = FForwardMove;
    procedure IPlysProvider.BackwardsPly = FTakebackMove;
}
    procedure FTakebackMove;
    procedure FForwardMove;
  end;

implementation

uses
  SysUtils, Windows, Clipbrd,
  //
  PGNParserUnit;

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

  m_strlPlysList := TStringList.Create;

  FCreateChessBoard;
end;


procedure TAnalyseChessBoard.FormDestroy(Sender: TObject);
begin
  FDestroyChessEngine;
  m_strlPlysList.Free;
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
    iPly := m_ChessBoard.NPlysDone;
    if (m_strlPlysList.Count >= iPly) then
    begin
      if (m_strlPlysList[iPly - 1] = strMove) then
        exit;
      while (m_strlPlysList.Count >= iPly) do
        m_strlPlysList.Delete(m_strlPlysList.Count - 1);
    end;
    m_strlPlysList.Add(strMove);
  end;

begin // .FChessBoardHandler
  case e of
    cbeMoved:
    begin
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
begin
  m_ChessBoard.TakeBack;
  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
end;


procedure TAnalyseChessBoard.PositionInitialMenuItemClick(Sender: TObject);
begin
  FInitPosition;
  m_MoveListForm.Refresh;
end;


procedure TAnalyseChessBoard.FInitPosition;
begin
  m_ChessBoard.InitPosition;
  m_strlPlysList.Clear;
  FSynchronizeChessEngineWithChessBoardAndStartEvaluation;
end;


procedure TAnalyseChessBoard.FForwardMove;
var
  bRes: boolean;
  iPly: integer;
begin
  iPly := m_ChessBoard.NPlysDone;

  if (m_strlPlysList.Count <= iPly) then
    exit;

  bRes := m_ChessBoard.DoMove(m_strlPlysList[iPly]);
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
begin
  Result := FALSE;

  PGNParser := TPGNParser.Create;
  try
    if (not PGNParser.Parse(PGNData)) then
      exit;

    FInitPosition;

    m_strlPlysList.Assign(PGNParser.PGNMoveList);

  finally
    PGNParser.Free;
  end;

  m_MoveListForm.Refresh;

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
  Result := m_strlPlysList.Count;
end;


function TAnalyseChessBoard.FGetPly(iIndex: integer): string;
begin
  Result := m_strlPlysList[iIndex];
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
  (Sender as TAction).Enabled := (m_ChessBoard.NPlysDone > 0)
end;


procedure TAnalyseChessBoard.ForwardMoveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (m_strlPlysList.Count > m_ChessBoard.NPlysDone);
end;

end.
