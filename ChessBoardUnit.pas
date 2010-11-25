unit ChessBoardUnit;

interface

uses
  Forms, ExtCtrls, Classes, Controls, Graphics, Types, Messages,
  //
  ChessRulesEngine, BitmapResUnit, PromotionUnit;

type
  TMode = (mView, mGame, mAnalyse); // Board mode

  TAnimation = (aNo, aSlow, aQuick);

  TChessBoardEvent = (cbeMate, cbeStaleMate, cbeMoved, cbeMenu);
  TChessBoardHandler = procedure(e: TChessBoardEvent;
                                 d1: pointer = nil; d2: pointer = nil) of object;

  TChessBoard = class(TForm, IChessRulesEngineable)
    PBoxBoard: TPaintBox;
    AnimateTimer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure AnimateTimerTimer(Sender: TObject);
    procedure PBoxBoardPaint(Sender: TObject);
    procedure PBoxBoardDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PBoxBoardDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PBoxBoardEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure PBoxBoardMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBoxBoardMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBoxBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBoxBoardStartDrag(Sender: TObject; var DragObject: TDragObject);

  private
    m_ChessRulesEngine: TChessRulesEngine;
    m_BitmapRes: TBitmapRes; // Manager for bitmaps

    FHandler: TChessBoardHandler;

    dx, dy: integer;  // Расстояние от курсора до верхнего левого угла
    x0, y0: integer; // Предыдущие координаты курсора
    _flipped: boolean; // Доска перевёрнута или нет
    hilighted: boolean; // Hilight the move that is being done

    m_i0, m_j0: integer;
    m_fig: TFigure;

    m_Mode: TMode;
    m_bViewGaming: boolean;

    m_bmChessBoard: TBitmap;
    m_bmFigure: array[TFigure] of TBitmap;
    m_bmBuf: TBitmap;

    m_animation: TAnimation; // Animation speed
    anim_step, anim_step_num: integer;
    anim_dx, anim_dy: real; // Variables for animation of a dragged piece
    m_bWillBeAnimatedFlag: boolean;

    m_PlayerColor: TFigureColor; // Color of player client
    dragged_moved: boolean; // Flag for switching of dragging
    last_hilight: boolean; // Flag for hilighting of the last move done
    coord_show: boolean; // Flag for showing coordinates

    // Resizing
    m_ResizingType: (rtNo, rtHoriz, rtVert);
    m_iDeltaWidthHeight: integer;
    m_bDeltaWidthHeightFlag: boolean;

    m_PromotionForm: TPromotionForm;

    procedure HilightLastMove;
    procedure Evaluate;

    function FGetLastMove: PMoveAbs;
    property lastMove: PMoveAbs read FGetLastMove;

    function FGetPosition: PChessPosition;
    property Position: PChessPosition read FGetPosition;

    function AskPromotionFigure(FigureColor: TFigureColor): TFigureName;

    procedure FOnAfterMoveDone;
    procedure FAnimate(const i, j: integer); // Animates a disposition of a piece from (i0,j0) to (i,j)

    procedure FWhatSquare(const P: TPoint; var i: Integer; var j: Integer);

    procedure FSetPlayerColor(const Value: TFigureColor);
    procedure FTogglePlayerColor;
    procedure FCancelAnimationDragging; // Caneling of animation and dragging for trace removal after draw
    procedure FSetFlipped(Value: boolean); // Flips chess position
    procedure FSetMode(const Value: TMode);
    procedure FSetCoordinatesShown(Value: boolean);
    procedure FSetLastMoveHilighted(Value: boolean);
    function FGetPositionsList: TList;
    function FGetPositionColor: TFigureColor;

    procedure WMSizing(var Msg: TMessage); message WM_SIZING;

    procedure FDoHandler(e: TChessBoardEvent; d1: pointer = nil; d2: pointer = nil);

    procedure FSetAnimateTimerEnabled(bValue: boolean);

    property AnimateTimerEnabled: boolean write FSetAnimateTimerEnabled;

  protected
    iSquareSize: integer; // Size of one chess board field
    bmHiddenBoard: TBitmap;

    procedure RDrawBoard;
    procedure RDrawHiddenBoard; virtual;
    procedure ROnAfterMoveDone; virtual;
    procedure ROnAfterSetPosition; virtual;
    function RDoMove(i, j: integer; prom_fig: TFigureName = K): boolean;
    function RIsAnimating: boolean;

    property PositionsList: TList read FGetPositionsList;

  public
    constructor Create(Owner: TComponent; AHandler: TChessBoardHandler = nil;
      const strPosBaseName: string = ''); reintroduce;

    function DoMove(const strMove: string): boolean;
    procedure ResetMoveList;
    function SetPosition(const strPosition: string): boolean;
    function GetPosition: string;
    procedure InitPosition;
    procedure PPRandom;
    procedure TakeBack;
    function NMoveDone: integer;
    function NPlysDone: integer;

    property PlayerColor: TFigureColor read m_PlayerColor write FSetPlayerColor;
    property Mode: TMode read m_Mode write FSetMode;
    property CoordinatesShown: boolean read coord_show write FSetCoordinatesShown;
    property Flipped: boolean read _flipped write FSetFlipped;
    property LastMoveHilighted: boolean read last_hilight write FSetLastMoveHilighted;
    property Animation: TAnimation read m_animation write m_animation;
    property ViewGaming: boolean read m_bViewGaming write m_bViewGaming;
    property PositionColor: TFigureColor read FGetPositionColor; // Whos move it is in the current position
  end;

var
  ChessBoard: TChessBoard;

implementation

{$R *.dfm}

uses
  Math, SysUtils, Windows,
  //
  ChessBoardHeaderUnit;

const
  HILIGHT_WIDTH = 1;
  HILIGHT_COLOR: TColor = clRed;
  HILIGHT_LAST_MOVE_WIDTH = 1;
  HILIGHT_LAST_MOVE_COLOR: TColor = clBlue;
  ANIMATION_SLOW = 30; // Time of animation in frames >= 1
  ANIMATION_QUICK = 9;
  CHB_WIDTH = 4;

////////////////////////////////////////////////////////////////////////////////
// TChessBoard

constructor TChessBoard.Create(Owner: TComponent; AHandler: TChessBoardHandler = nil;
  const strPosBaseName: string = '');
begin
  FHandler := AHandler;
  // TODO: strPosBaseName
  inherited Create(Owner);
end;


procedure TChessBoard.AnimateTimerTimer(Sender: TObject);
var
  X,Y: integer;
  rect: TRect;
begin
  inc(anim_step);
  if (anim_step < anim_step_num) then
  begin
    X := round(x0 + anim_dx * anim_step);
    Y := round(y0 + anim_dy * anim_step);
    dx := X - x0 - Round(anim_dx * (anim_step - 1));
    dy := Y - y0 - Round(anim_dy * (anim_step - 1));

    // Восстановить фрагмент на bmHiddenBoard
    bmHiddenBoard.Canvas.Draw(X - dx, Y - dy, m_bmBuf);
    // Копировать новый фрагмент в буфер
    m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
      bmHiddenBoard.Canvas, Bounds(X, Y, iSquareSize, iSquareSize));
    // Нарисовать перетаскиваемую фигуру в новой позиции
    bmHiddenBoard.Canvas.Draw(X, Y, m_bmFigure[m_fig]);
    // Перенести новый фрагмент на экран
    rect := Bounds(Min(X - dx, X), Min(Y - dy, Y),
      abs(dx) + iSquareSize, abs(dy) + iSquareSize);
    PBoxBoard.Canvas.CopyRect(rect, bmHiddenBoard.Canvas, rect);
  end
  else
  begin
    AnimateTimerEnabled := FALSE;
    RDrawBoard;
    HilightLastMove;
    Evaluate;
  end;
end;


procedure TChessBoard.RDrawBoard;
begin
  RDrawHiddenBoard;
  PBoxBoardPaint(nil);
end;


procedure TChessBoard.HilightLastMove;
var
  i, j, l,
  _i0, _j0, x, y: integer;
begin
  // вывод последнего сделанного хода
  if (last_hilight  and (lastMove.i0 <> 0)) then
  begin
    if (_flipped) then
    begin
      _i0 := 9 - lastMove.i0;
      _j0 := lastMove.j0;
      i := 9 - lastMove.i;
      j := lastMove.j;
    end
    else
    begin
      _i0 := lastMove.i0;
      _j0 := 9 - lastMove.j0;
      i := lastMove.i;
      j := 9 - lastMove.j;
    end;

    x := iSquareSize * (_i0 - 1) + CHB_X;
    y := iSquareSize * (_j0 - 1) + CHB_Y;
    bmHiddenBoard.Canvas.Pen.Color := HILIGHT_LAST_MOVE_COLOR;
    bmHiddenBoard.Canvas.Pen.Width := HILIGHT_LAST_MOVE_WIDTH;

    for l := 1 to 2 do
      with bmHiddenBoard.Canvas do
      begin
        MoveTo(x, y);
        LineTo(x + iSquareSize - 1, y);
        LineTo(x + iSquareSize - 1, y + iSquareSize - 1);
        LineTo(x, y + iSquareSize - 1);
        LineTo(x, y);

        x := iSquareSize * (i - 1) + CHB_X;
        y := iSquareSize * (j - 1) + CHB_Y;
      end;
    PBoxBoardPaint(nil);
  end;
end;


procedure TChessBoard.RDrawHiddenBoard;
var
  i, j: integer;
  x, y: integer;
begin
  if (not Assigned(bmHiddenBoard)) then
    exit;

  // Copy empty board to the hidden one
  with bmHiddenBoard do
  begin
    Canvas.CopyRect(Bounds(0,0, Width,Height), m_bmChessBoard.Canvas, Bounds(0,0, Width,Height));
  end;

  // Draw coordinates
  if (coord_show) then
    with bmHiddenBoard, bmHiddenBoard.Canvas do
    begin
      x:= CHB_X + iSquareSize div 2;
      y:= (bmHiddenBoard.Height + CHB_Y + 8 * iSquareSize + CHB_WIDTH) div 2;
      if _flipped then j := ord('h')
        else j:= ord('a');
      for i:= 1 to 8 do // буквы
        begin
          TextOut(x - TextWidth(chr(j)) div 2,
                  y + 1 - TextHeight(chr(j)) div 2 , chr(j));
          x := x + iSquareSize;
          if _flipped then dec(j)
            else inc(j);
        end;
      x:= (CHB_X - CHB_WIDTH) div 2;
      y:= CHB_Y + iSquareSize div 2;
      if _flipped then j:= ord('1')
        else j := ord('8');
      for i := 1 to 8 do // цифры
        begin
          TextOut(x - TextWidth(chr(j)) div 2,
                  y - TextHeight(chr(j)) div 2, chr(j));
          y:= y + iSquareSize;
          if _flipped then inc(j)
            else dec(j);
        end;
  end;

  // Draw pieces
  for i := 1 to 8 do
    for j := 1 to 8 do
      begin
        if ((Position.board[i,j] = ES)) then
          continue; // There's nothing to draw
        if not _flipped then // Загрузить нужную фигуру из ресурса и нарисовать
          bmHiddenBoard.Canvas.Draw(CHB_X + iSquareSize * (i-1),
                                    CHB_Y + iSquareSize * (8-j),
                                    m_bmFigure[Position.board[i,j]])
        else // Black is below
          bmHiddenBoard.Canvas.Draw(CHB_X + iSquareSize * (8-i),
                                    CHB_Y + iSquareSize * (j-1),
                                    m_bmFigure[Position.board[i,j]]);
      end;
end;


procedure TChessBoard.Evaluate;
begin
  case m_ChessRulesEngine.GetEvaluation of
    evMate:
      FDoHandler(cbeMate, self);
    evStaleMate:
      FDoHandler(cbeStaleMate, self);
  end;
end;


procedure TChessBoard.PBoxBoardPaint(Sender: TObject);
begin
  PBoxBoard.Canvas.Draw(0, 0, bmHiddenBoard); // Draw hidden board on the form
//  PBoxBoard.Canvas.StretchDraw(Bounds(0, 0, PBoxBoard.Width, PBoxBoard.Height), bmHiddenBoard);
end;


function TChessBoard.FGetLastMove: PMoveAbs;
begin
  Result := m_ChessRulesEngine.lastMove;
end;


function TChessBoard.FGetPosition: PChessPosition;
begin
  Result := m_ChessRulesEngine.Position;
end;


function TChessBoard.AskPromotionFigure(FigureColor: TFigureColor): TFigureName;
var
  frmOwner: TForm;
begin
  if (Owner is TForm) then
    frmOwner := TForm(Owner)
  else
    frmOwner := self;

  if (Showing) then
  begin
    m_PromotionForm := TPromotionForm.Create(frmOwner, m_BitmapRes);
    try
      Result := m_PromotionForm.ShowPromotion(FigureColor);
    finally
      FreeAndNil(m_PromotionForm);
    end;
  end
  else
    Result := Q;
end;


procedure TChessBoard.FSetPlayerColor(const Value: TFigureColor);
begin
  FCancelAnimationDragging;
  m_PlayerColor := Value;
  if (m_PlayerColor = fcWhite) then
    FSetFlipped(FALSE)
  else // fcBlack
    FSetFlipped(TRUE);
end;


procedure TChessBoard.FTogglePlayerColor;
begin
  if (m_PlayerColor = fcWhite) then
    m_PlayerColor := fcBlack
  else // fcBlack
    m_PlayerColor := fcWhite;  
end;


procedure TChessBoard.FCancelAnimationDragging;
begin
  // Cancel animation and dragging
  if (AnimateTimer.Enabled) then
  begin
    AnimateTimerEnabled := FALSE;
    // anim_step := anim_step_num;
    // AnimateTimerTimer(nil);
  end;
  if (PBoxBoard.Dragging) then
  begin
    dragged_moved := FALSE;
    PBoxBoard.EndDrag(FALSE);
  end;
end;


procedure TChessBoard.FSetFlipped(Value: boolean);
begin
  // TODO: ???
  _flipped := Value;
  RDrawBoard;
end;


procedure TChessBoard.FSetMode(const Value: TMode);
begin
  if (m_Mode = Value) then
    exit;

  m_Mode := Value;

  if ((m_Mode = mView) and (Assigned(m_PromotionForm))) then
    m_PromotionForm.Close;

  RDrawBoard;
  HilightLastMove;
end;


procedure TChessBoard.FSetCoordinatesShown(Value: boolean);
begin
  coord_show := Value;
  RDrawBoard;
  HilightLastMove;
end;


procedure TChessBoard.FSetLastMoveHilighted(Value: boolean);
begin
  last_hilight := Value;
  RDrawBoard;
  HilightLastMove;
end;


function TChessBoard.DoMove(const strMove: string): boolean;
begin
  // Animation canceling
  if (AnimateTimer.Enabled) then
  begin
    AnimateTimerEnabled := FALSE;
    anim_step := anim_step_num;
    AnimateTimerTimer(nil);
  end;

  Result := m_ChessRulesEngine.DoMove(strMove);

  if (Result) then
  begin
    FOnAfterMoveDone;
    FAnimate(lastMove.i, lastMove.j);
  end;
end;


procedure TChessBoard.FOnAfterMoveDone;
var
  _fig: TFigure;
begin
  m_i0 := lastMove.i0;
  m_j0 := lastMove.j0;

  _fig := Position.board[lastMove.i, lastMove.j];
  if (lastMove.prom_fig in [Q, R, B, N]) then
  begin
    if (_fig < ES) then
      m_fig := WP
    else
      m_fig := BP;
  end
  else
    m_fig := _fig;

  ROnAfterMoveDone;
end;


procedure TChessBoard.FAnimate(const i, j: integer);
var
  x, y: integer;
begin
  if (not Showing) then
    exit;

  case animation of
    aNo:
      anim_step_num := 1;
    aSlow:
      anim_step_num := ANIMATION_SLOW;
    aQuick:
      anim_step_num := ANIMATION_QUICK;
  end;

  if (_flipped) then
  begin
    x0 := (8 - m_i0) * iSquareSize + CHB_X;
    y0 := (m_j0 - 1) * iSquareSize + CHB_Y;
    x := (8 - i) * iSquareSize + CHB_X;
    y := (j - 1) * iSquareSize + CHB_Y;
  end
  else
  begin
    x0 := (m_i0 - 1) * iSquareSize + CHB_X;
    y0 := (8 - m_j0) * iSquareSize + CHB_Y;
    x := (i - 1) * iSquareSize + CHB_X;
    y := (8 - j) * iSquareSize + CHB_Y;
  end;

  anim_dx := (x-x0) / anim_step_num;
  anim_dy := (y-y0) / anim_step_num;

  anim_step:= 0;

  // Copy image of the empty square to m_bmBuf
  m_bmBuf.Width := iSquareSize;
  m_bmBuf.Height := iSquareSize;
  if (((m_i0 + m_j0) and 1) <> 0) then
    m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(0, 0, iSquareSize, iSquareSize))
  else
    m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(iSquareSize, 0, iSquareSize, iSquareSize));

  AnimateTimer.Enabled := TRUE;
end;


procedure TChessBoard.ROnAfterMoveDone;
var
  strLastMove: string;
begin
  strLastMove := m_ChessRulesEngine.LastMoveStr;
  FDoHandler(cbeMoved, @strLastMove, self);

  if (m_Mode = mAnalyse) then
    FTogglePlayerColor;
end;


procedure TChessBoard.ResetMoveList;
begin
  m_ChessRulesEngine.ResetMoveList;
end;


function TChessBoard.SetPosition(const strPosition: string): boolean;
begin
  Result := m_ChessRulesEngine.SetPosition(strPosition);
  if (Result) then
  begin
    FCancelAnimationDragging;
    ROnAfterSetPosition;
    RDrawBoard;
  end;
end;


function TChessBoard.GetPosition: string;
begin
  Result := m_ChessRulesEngine.GetPosition;
end;


procedure TChessBoard.ROnAfterSetPosition;
begin
end;


procedure TChessBoard.FormCreate(Sender: TObject);
begin
  // m_iDeltaWidthHeight := Width - Height;

  m_BitmapRes := TBitmapRes.Create(Size(PBoxBoard.Width, PBoxBoard.Height));

  coord_show:= TRUE;
  last_hilight:= FALSE;
  m_animation := aQuick;

  m_ChessRulesEngine := TChessRulesEngine.Create(self);
end;


procedure TChessBoard.FormDestroy(Sender: TObject);
var
  _fig: TFigure;
begin
  m_ChessRulesEngine.Free;

  bmHiddenBoard.Free;
  m_bmBuf.Free;

  for _fig := Low(TFigure) to High(TFigure) do
    m_bmFigure[_fig].Free;
  m_bmChessBoard.Free;

  m_BitmapRes.Free;
end;


procedure TChessBoard.PBoxBoardDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i, j: Integer;
begin
  FWhatSquare(Point(X, Y), i, j);
  if (Mode in [mGame, mAnalyse]) then
  begin
    if (RDoMove(i, j)) then
      dragged_moved := TRUE;
  end;
end;


procedure TChessBoard.FWhatSquare(const P: TPoint;
  var i: Integer; var j: Integer);
begin
  with P do
  begin
    i := (X - CHB_X + iSquareSize) div iSquareSize;
    j := 8 - (Y - CHB_Y) div iSquareSize;
    if (_flipped) then
    begin
      i := 9 - i;
      j := 9 - j;
    end;
  end;
end;


function TChessBoard.RDoMove(i, j: integer; prom_fig: TFigureName = K): boolean;
begin
  Result := m_ChessRulesEngine.DoMove(m_i0, m_j0, i, j, prom_fig);
  if (Result) then
    FOnAfterMoveDone;
end;


procedure TChessBoard.PBoxBoardDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  rect: TRect;
  i, j: integer;
begin
  case State of
    dsDragEnter:
      hilighted := FALSE;

    dsDragMove:
      begin
        // Repaint a fragment on bmHiddenBoard
        bmHiddenBoard.Canvas.Draw(x0 - dx, y0 - dy, m_bmBuf);
        // Copy new fragment to the buffer
        m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
          bmHiddenBoard.Canvas, Bounds(X - dx, Y - dy, iSquareSize, iSquareSize));
        // Draw the dragging piece in a new position
        bmHiddenBoard.Canvas.Draw(X - dx, Y - dy, m_bmFigure[m_fig]);
        // Copy the new fragment to the screen
        rect:= Bounds(Min(x0,X) - dx, Min(y0, Y) - dy,
          abs(X - x0) + iSquareSize, abs(Y - y0) + iSquareSize);
        PBoxBoard.Canvas.CopyRect(rect, bmHiddenBoard.Canvas, rect);

        x0 := X;
        y0 := Y;

        FWhatSquare(Point(X,Y), i, j);

        Accept := ((i in [1..8]) and (j in [1..8]));
      end;
  end;
end;


procedure TChessBoard.PBoxBoardEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if (hilighted) then
    with bmHiddenBoard.Canvas do
    begin
      Pen.Color:= HILIGHT_COLOR;
      Pen.Width := HILIGHT_WIDTH;
      x0:= x0 - dx;
      y0:= y0 - dy;
      MoveTo(x0,y0);
      LineTo(x0 + iSquareSize - 1, y0);
      LineTo(x0 + iSquareSize - 1, y0 + iSquareSize - 1);
      LineTo(x0, y0 + iSquareSize - 1);
      LineTo(x0, y0);

      PBoxBoardPaint(nil);
    end
  else
  begin
    RDrawBoard;
    if (dragged_moved) then
    begin
      HilightLastMove;
      Evaluate;
      dragged_moved:= FALSE;
    end;
  end;
end;


procedure TChessBoard.PBoxBoardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j: Integer;
  f: TFigure;
begin
  FWhatSquare(Point(X, Y), i, j);
  if (not ((i in [1..8]) and (j in [1..8]))) then
    exit;

  f := Position.board[i,j];

  case Mode of
    mGame, mAnalyse:
    begin
      if (m_bViewGaming) then
        exit;
      if (Button <> mbLeft) or (Position.color <> m_PlayerColor) or
         (((Position.color <> fcWhite) or (f >= ES)) and
          ((Position.color <> fcBlack) or (f <= ES))) then
        exit;
    end;
  else
    exit;
  end;

  if (anim_step < anim_step_num) then
    begin
      anim_step:= anim_step_num;
      AnimateTimerTimer(nil);
    end;

  if ((i = m_i0) and (j = m_j0)) then
    hilighted := (hilighted xor TRUE)
  else
    hilighted:= TRUE;

  m_fig := f;
  m_i0 := i;
  m_j0 := j;

  dx := (X - CHB_X) mod iSquareSize;
  dy := (Y - CHB_Y) mod iSquareSize;
  x0 := X;
  y0 := Y;

  dragged_moved := TRUE;
  PBoxBoard.BeginDrag(FALSE);
end;


procedure TChessBoard.PBoxBoardMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  f: TFigure;
  i,j: Integer;
begin
  FWhatSquare(Point(X,Y), i,j);
  if (not ((i in [1..8]) and (j in [1..8]))) then
  begin
    PBoxBoard.Cursor:= crDefault;
    exit;
  end;

  f := Position.board[i,j];

  case Mode of
    mGame, mAnalyse:
    begin
      if (m_bViewGaming) then
        exit;

      if (m_PlayerColor = Position.color) and
         (((Position.color = fcWhite) and (f < ES)) or
          ((Position.color = fcBlack) and (f > ES))) then
        PBoxBoard.Cursor:= crHandPoint
      else
        PBoxBoard.Cursor:= crDefault;
    end;

    else
      PBoxBoard.Cursor:= crDefault;
  end;
end;


function TChessBoard.FGetPositionsList: TList;
begin
  Result := m_ChessRulesEngine.PositionsList;
end;


procedure TChessBoard.PBoxBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  case Button of
    mbLeft:
    begin
      case Mode of
        mGame, mAnalyse:
        begin
          if (not hilighted) then
            exit;
          FWhatSquare(Point(X, Y), i, j);
          if (dragged_moved) then
            RDrawBoard
          else
          begin
            hilighted:= FALSE;
            if (RDoMove(i, j)) then
              FAnimate(i, j)
            else
              RDrawBoard;
          end;
        end;
      end; // case
    end;

    mbRight:
    begin
      FDoHandler(cbeMenu, self);
    end;

  end;
end;


procedure TChessBoard.PBoxBoardStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  // Copy image of an empty square to m_bmBuf
  m_bmBuf.Width := iSquareSize; m_bmBuf.Height:= iSquareSize;
  if (((m_i0 + m_j0) and 1) <> 0) then
      m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
        m_bmFigure[ES].Canvas, Bounds(0,0, iSquareSize, iSquareSize))
  else
    m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(iSquareSize,0, iSquareSize, iSquareSize));

  dragged_moved:= FALSE;
end;


procedure TChessBoard.InitPosition;
begin
  m_ChessRulesEngine.InitNewGame;

  if (m_Mode = mAnalyse) then
    m_PlayerColor := PositionColor;

  FCancelAnimationDragging;
  ROnAfterSetPosition;
  RDrawBoard;
end;


procedure TChessBoard.PPRandom;
begin
  m_ChessRulesEngine.InitNewPPRandomGame;
  RDrawBoard;
end;


procedure TChessBoard.TakeBack;
begin
  if (not m_ChessRulesEngine.TakeBack) then
    exit;

  if (m_Mode = mAnalyse) then
    FTogglePlayerColor;

  ROnAfterSetPosition;
  // TODO: animation
  RDrawBoard;
end;


function TChessBoard.NMoveDone: integer;
begin
  Result := m_ChessRulesEngine.NMovesDone;
end;


function TChessBoard.NPlysDone: integer;
begin
  Result := m_ChessRulesEngine.NPlysDone;
end;


function TChessBoard.FGetPositionColor: TFigureColor;
begin
  Result := Position.color;
end;


procedure TChessBoard.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
  NewBoardSize: TSize;
begin
  if (not m_bDeltaWidthHeightFlag) then
  begin
    m_iDeltaWidthHeight := Width - Height;
    m_bDeltaWidthHeightFlag := TRUE;
  end;

  Resize := (m_ResizingType <> rtNo);
  if (not Resize) then
    exit;

  if (m_ResizingType = rtVert) then
    NewWidth := NewHeight + m_iDeltaWidthHeight
  else // rtHoriz
    NewHeight := NewWidth - m_iDeltaWidthHeight;

  NewBoardSize := m_BitmapRes.GetOptimalBoardSize(
    Size(PBoxBoard.Width + (NewWidth - Width), PBoxBoard.Height + (NewHeight - Height)));

  Resize := (NewBoardSize.cx > 0) and (NewBoardSize.cy > 0) and
    ((NewBoardSize.cx <> PBoxBoard.Width) or (NewBoardSize.cy <> PBoxBoard.Height));
  if (Resize) then
  begin
    NewWidth := Width + (NewBoardSize.cx - PBoxBoard.Width);
    NewHeight := Height + (NewBoardSize.cy - PBoxBoard.Height);
  end;
end;


procedure TChessBoard.FormResize(Sender: TObject);
var
  _fig: TFigure;
begin
  FreeAndNil(m_bmChessBoard);
  m_BitmapRes.CreateBoardBitmap(Size(PBoxBoard.Width, PBoxBoard.Height), self.Color,
    m_bmChessBoard);
  iSquareSize := m_BitmapRes.SquareSize;

  for _fig := Low(TFigure) to High(TFigure) do
  begin
    FreeAndNil(m_bmFigure[_fig]);
    m_BitmapRes.CreateFigureBitmap(_fig, m_bmFigure[_fig]);
  end;

  // Graphics initialization
  if (not Assigned(bmHiddenBoard)) then
  begin
    bmHiddenBoard := Graphics.TBitmap.Create;
    bmHiddenBoard.Palette := m_bmChessBoard.Palette;
    bmHiddenBoard.Canvas.Font := PBoxBoard.Font; // Характеристики шрифта координат задаются в инспекторе
    bmHiddenBoard.Canvas.Brush.Style := bsClear;
  end;
  bmHiddenBoard.Width := m_bmChessBoard.Width;
  bmHiddenBoard.Height := m_bmChessBoard.Height;

  if (not Assigned(m_bmBuf)) then
  begin
    m_bmBuf := Graphics.TBitmap.Create;
    m_bmBuf.Palette:= m_bmChessBoard.Palette;
  end;

  RDrawBoard;
end;


procedure TChessBoard.WMSizing(var Msg: TMessage);
begin
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


procedure TChessBoard.FDoHandler(e: TChessBoardEvent; d1: pointer = nil; d2: pointer = nil);
begin
  if (Assigned(FHandler)) then
    FHandler(e, d1, d2);
end;


function TChessBoard.RIsAnimating: boolean;
begin
  Result := (AnimateTimer.Enabled or m_bWillBeAnimatedFlag);
end;


procedure TChessBoard.FSetAnimateTimerEnabled(bValue: boolean);
begin
  AnimateTimer.Enabled := bValue;
  if (not bValue) then
    m_bWillBeAnimatedFlag := FALSE;
end;

initialization

begin
  Randomize; // It's for PP Random
end;

finalization

end.
