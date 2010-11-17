unit AnalyseChessBoardUnit;

interface

uses
  Forms, TntForms, TntMenus, Menus, Classes, Controls, ExtCtrls, Messages,
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
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    m_ChessBoard: TPosBaseChessBoard;
    m_strPosBaseName: string;
    m_ResizingType: (rtNo, rtHoriz, rtVert);

    procedure FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
      d2: pointer = nil);

    procedure WMSizing(var Msg: TMessage); message WM_SIZING;

    procedure FCreateChessBoard;
  end;

implementation

uses
  Windows;

{$R *.dfm}

procedure TAnalyseChessBoard.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TAnalyseChessBoard.FormCreate(Sender: TObject);
begin
  FCreateChessBoard;
end;


procedure TAnalyseChessBoard.FCreateChessBoard;
begin
  m_ChessBoard := TPosBaseChessBoard.Create(self, FChessBoardHandler, m_strPosBaseName);

  m_ChessBoard.BorderStyle := bsNone;
  m_ChessBoard.Align := alClient;
  m_ChessBoard.Parent := self;
  m_ChessBoard.Visible := TRUE;

  m_ChessBoard.InitPosition;
end;


procedure TAnalyseChessBoard.FChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil;
  d2: pointer = nil);
begin
  // TODO:
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


end.
