unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntForms, TntMenus, Menus, Classes, Controls, ExtCtrls, Messages,
  ComCtrls, Dialogs,
  //
  ChessBoardUnit, PosBaseChessBoardUnit;

type
  TAnalyseChessBoard = class(TTntForm)
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
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure PositionTakebackMoveMenuItemClick(Sender: TObject);
    procedure PositionInitialMenuItemClick(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure PositionForwardMoveMenuItemClick(Sender: TObject);
    procedure ViewFlipBoardMenuItemClick(Sender: TObject);
    procedure OpenPGNMenuItemClick(Sender: TObject);
    procedure PastePGNMenuItemClick(Sender: TObject);
  private
    m_ChessBoard: TPosBaseChessBoard;
    m_strPosBaseName: string;
    m_ResizingType: (rtNo, rtHoriz, rtVert);

    m_strlPlysList: TStringList;

    procedure FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
      d2: pointer = nil);

    procedure WMSizing(var Msg: TMessage); message WM_SIZING;
                  
    procedure FCreateChessBoard;
    procedure FInitPosition;
  end;

implementation

uses
  Windows, Clipbrd,
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


procedure TAnalyseChessBoard.TntFormDestroy(Sender: TObject);
begin
  m_strlPlysList.Free;
end;


procedure TAnalyseChessBoard.FCreateChessBoard;
begin
  m_ChessBoard := TPosBaseChessBoard.Create(self, FChessBoardHandler, '');

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
var
  strMove: string;
  iPly: integer;
begin
  case e of
    cbeMoved:
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

  iNewChessBoardWidth := m_ChessBoard.Width + (NewWidth - self.Width);
  iNewChessBoardHeight := m_ChessBoard.Height + (NewHeight - self.Height);

  m_ChessBoard.FormCanResize(self, iNewChessBoardWidth, iNewChessBoardHeight, Resize);
  if (Resize) then
  begin
    NewWidth := self.Width + (iNewChessBoardWidth - m_ChessBoard.Width);
    NewHeight := self.Height + (iNewChessBoardHeight - m_ChessBoard.Height);
  end;
end;


procedure TAnalyseChessBoard.PositionTakebackMoveMenuItemClick(
  Sender: TObject);
begin
  m_ChessBoard.TakeBack;
end;


procedure TAnalyseChessBoard.PositionInitialMenuItemClick(Sender: TObject);
begin
  FInitPosition;
end;


procedure TAnalyseChessBoard.FInitPosition;
begin
  m_ChessBoard.InitPosition;
  m_strlPlysList.Clear;
end;


procedure TAnalyseChessBoard.PositionForwardMoveMenuItemClick(
  Sender: TObject);
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
  PGNParser: TPGNParser;
begin
  if (not OpenPGNDialog.Execute) then
    exit;

  PGNParser := nil;
  strlData := TStringList.Create;
  try
    strlData.LoadFromFile(OpenPGNDialog.FileName);
    PGNParser := TPGNParser.Create;

    if (not PGNParser.Parse(strlData)) then
      exit;

    FInitPosition;

    m_strlPlysList.Assign(PGNParser.PGNMoveList);

  finally
    PGNParser.Free;
    strlData.Free;
  end;

end;


procedure TAnalyseChessBoard.PastePGNMenuItemClick(Sender: TObject);
var
  strlData: TStringList;
  PGNParser: TPGNParser;
begin
  if (not Clipboard.HasFormat(CF_TEXT)) then
    exit;

  PGNParser := nil;
  strlData := TStringList.Create;
  try
    strlData.Text := Clipboard.AsText;
    PGNParser := TPGNParser.Create;

    if (not PGNParser.Parse(strlData)) then
      exit;

    FInitPosition;

    m_strlPlysList.Assign(PGNParser.PGNMoveList);

  finally
    PGNParser.Free;
    strlData.Free;
  end;
  
end;

end.
