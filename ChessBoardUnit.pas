unit ChessBoardUnit;

// Для Миранды следует определить дериктиву {$DEFINE THREADED_CHESSCLOCK}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TntForms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, Buttons,
  // Chess4net
  ChessBoardHeaderUnit, ChessRulesEngine, BitmapResUnit, LocalizerUnit;

type
  TMode = (mView, mGame, mEdit); // состояние доски
  TChessBoardEvent =
    (cbeMoved, cbeMate, cbeStaleMate, cbeInsuffMaterial, cbeKeyPressed,
     cbeClockSwitched, cbeTimeOut, cbeExit, cbeMenu, cbeActivate, cbeFormMoving,
     cbeRefreshAll); // возможно добавление новых событий
                                                                     // cbeRefreshAll сигнализирует, что были изменены глобальные опции.
  TChessBoardHandler = procedure(e: TChessBoardEvent;
                                 d1: pointer = nil; d2: pointer = nil) of object;
  TAnimation = (aNo, aSlow, aQuick);

{$IFDEF THREADED_CHESSCLOCK}
  TChessBoard = class;
  TTimeLabelThread = class(TThread)
  private
    ChessBoard: TChessBoard;
    player_time: array[TFigureColor] of TDateTime;
  protected
    procedure Execute; override;
  public
    WhiteTime, BlackTime: string;
    constructor Create(ChessBoard: TChessBoard);
  end;
{$ENDIF}

  TChessBoard = class(TTntForm, ILocalizable, IChessRulesEngineable)
    PBoxBoard: TPaintBox;
    TimePanel: TPanel;
    WhiteLabel: TTntLabel;
    WhiteTimeLabel: TLabel;
    BlackLabel: TTntLabel;
    BlackTimeLabel: TLabel;
    GameTimer: TTimer;
    AnimateTimer: TTimer;
    WhiteFlagButton: TSpeedButton;
    BlackFlagButton: TSpeedButton;
    WhitePanel: TPanel;
    BlackPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PBoxBoardPaint(Sender: TObject);
    procedure PBoxBoardDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PBoxBoardDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PBoxBoardEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure PBoxBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBoxBoardMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PBoxBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBoxBoardStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure AnimateTimerTimer(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FlagButtonClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure TimePanelResize(Sender: TObject);

  private
    m_ChessRulesEngine: TChessRulesEngine;

    mode_var: TMode;
    dx, dy: integer;  // Расстояние от курсора до верхнего левого угла
    x0,y0: integer; // Предыдущие координаты курсора
    _flipped: boolean; // Доска перевёрнута или нет
    hilighted: boolean; // Подсветка делаемого хода

    m_bmChessBoard: TBitmap;
    m_bmFigure: array[TFigure] of TBitmap;
    m_bmBuf: TBitmap;

    Handler: TChessBoardHandler;

    m_animation: TAnimation; // скорость анимации

    anim_dx, anim_dy: real; // переменные для анимации перемещения фигуры
    anim_step, anim_step_num: integer; // количество шагов в анимации
    player_color: TFigureColor; // цвет игрока клиента
    dragged_moved: boolean; // индикатор включения перетаскивания
    last_hilight: boolean; // флаг подсветки последнего хода
    m_bFlash_move: boolean; // flag for flashing on icoming move
    coord_show: boolean; // флаг координат

    auto_flag: boolean; // индикатор автофлага
    player_time: array[TFigureColor] of TDateTime; // время белых и чёрных
    past_time: TDateTime; // время начала обдумывания хода
    unlimited_var: array[TFigureColor] of boolean; // партия без временного контроля
    clock_color: TFigureColor; // цвет анимируемой фигуры

    shuted: boolean; // индикатор внешнего закрытия окна

    // Resizing
    m_ResizingType: (rtNo, rtHoriz, rtVert);
    m_iDeltaWidthHeight: integer;
    m_BitmapRes: TBitmapRes; // Manager for bitmaps
    m_iTimePanelInitialWidth: integer;
    m_iWhitePanelInitialLeft, m_iBlackPanelInitialLeft: integer;
    m_iWhitePanelInitialWidth, m_iBlackPanelInitialWidth: integer;
    m_TimeFont: TFont;

{$IFDEF THREADED_CHESSCLOCK}
    TimeLabelThread: TTimeLabelThread; // нить используется для борьбы с лагом в Миранде
{$ENDIF}
    procedure HilightLastMove;
    procedure DelPosList; // Удаляет текущую позицию из списка
    procedure WhatSquare(const P: TPoint; var i: Integer; var j: Integer);
    procedure Animate(const i,j: integer); // Анимирует перемещение фигуры с (i0,j0) до (i,j)
    procedure SetMode(const m: TMode);
    procedure ShowTime(const c: TFigureColor);
    procedure SetPlayerColor(const color: TFigureColor);
    procedure SetTime(color: TFigureColor; const tm: TDateTime);
    function GetTime(color: TFigureColor): TDateTime;
    procedure SetUnlimited(color: TFigureColor; const unl: boolean);
    function GetUnlimited(color: TFigureColor): boolean;
    procedure Evaluate;
    function CanMove(pos: TChessPosition): boolean;
    procedure SetHilightLastMove(const yes: boolean);
    procedure SetCoordinates(const yes: boolean);
    procedure SetFlipped(const f: boolean); // Переварачивает позицию при отображении
    function GetStayOnTop: boolean;
    procedure SetStayOnTop(onTop: boolean);
    procedure CancelAnimationDragging; // отмена анимации и перетаскивания для удаления грязи при прорисовки
    procedure SetAutoFlag(auto_flag: boolean);
    procedure FFlashWindow;
    // Localization
    procedure ILocalizable.Localize = FLocalize;
    procedure FLocalize;

    procedure WMMoving(var Msg: TWMMoving); message WM_MOVING;
    procedure WMSizing(var Msg: TMessage); message WM_SIZING;

    function FGetPositinoColor: TFigureColor;
    function FGetPosition: PChessPosition;
    function AskPromotionFigure(FigureColor: TFigureColor): TFigureName;

    function FGetI0J0(iIndex: integer): integer;
    procedure FSetI0J0(iIndex, iValue: integer);
    function FGetLastMove: PMoveAbs;

    function FGetPositionsList: TList;

    function FGetFig: TFigure;
    procedure FSetFig(Value: TFigure);

    property ChessRulesEngine: TChessRulesEngine read m_ChessRulesEngine;
    property Position: PChessPosition read FGetPosition;

    property i0: integer index 0 read FGetI0J0 write FSetI0J0;
    property j0: integer index 1 read FGetI0J0 write FSetI0J0;

    property fig: TFigure read FGetFig write FSetFig;

    property lastMove: PMoveAbs read FGetLastMove;

  protected
    iSquareSize: integer; // Size of a chess board field
    bmHiddenBoard: TBitmap;
    procedure RDrawBoard;
    procedure RDrawHiddenBoard; virtual;
    procedure RSetPositionRec(const pos: TChessPosition); virtual;
    function DoMove(i, j: integer; prom_fig: TFigureName = K): boolean; overload; virtual;

    property PositionsList: TList read FGetPositionsList;

  public
    constructor Create(Owner: TComponent; h: TChessBoardHandler = nil); reintroduce;

    procedure TakeBack; // взятие хода обратно
    procedure SwitchClock(clock_color: TFigureColor);
    procedure ResetMoveList; // очищает список позиций
    procedure Copy(cb: TChessBoard); // копирует поля cb в self
    procedure Refresh;
    property Unlimited[color: TFigureColor]: boolean read GetUnlimited write SetUnlimited;
    property Time[color: TFigureColor]: TDateTime read GetTime write SetTime;
    property PlayerColor: TFigureColor read player_color write SetPlayerColor;
    property PositionColor: TFigureColor read FGetPositinoColor; // чей ход в текущей позиции
    property ClockColor: TFigureColor read clock_color;
    property Mode: TMode read mode_var write SetMode;
    property CoordinatesShown: boolean read coord_show write SetCoordinates;
    procedure InitPosition;
    procedure PPRandom;
    procedure StopClock;

    function SetPosition(const posstr: string): boolean;
    function GetPosition: string;
    function GetPositionRec: TChessPosition;
    function GetLastMoveAbs: TMoveAbs; // Возвращает последний сделанный ход в абс. координатах
    function NMoveDone: integer; // количество сделанных ходов
    function DoMove(move_str: string): boolean; overload;
    procedure Shut;

    property flipped: boolean read _flipped write SetFlipped;
    property LastMoveHilighted: boolean read last_hilight write SetHilightLastMove;
    property FlashOnMove: boolean read m_bFlash_move write m_bFlash_move;
    property StayOnTop: boolean read GetStayOnTop write SetStayOnTop;
    property AutoFlag: boolean read auto_flag write SetAutoFlag;
    property animation: TAnimation read m_animation write m_animation;
  end;

const
  INITIAL_CHESS_POSITION = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -';
  EMPTY_CHESS_POSITION = '8/8/8/8/8/8/8/8 w - -';

implementation

{$J+}

{$R *.dfm}

uses
  StrUtils, Math, DateUtils,
  // Chess4Net
  PromotionUnit;

const
  HILIGHT_WIDTH = 1;
  HILIGHT_COLOR: TColor = clRed;
  HILIGHT_LAST_MOVE_WIDTH = 1;
  HILIGHT_LAST_MOVE_COLOR: TColor = clBlue;
  ANIMATION_SLOW = 30; // Время анимации хода в фреймах >= 1
  ANIMATION_QUICK = 9;
  CHB_WIDTH = 4;
  TIME_COLOR = clBlack;
//  FULL_TIME_FORMAT = 'h:n:s"."z';
  HOUR_TIME_FORMAT = 'h:nn:ss';
  MIN_TIME_FORMAT = 'n:ss';
  ZEITNOT_BOARDER = 10; // сек - цетйтнотная граница
  ZEITNOT_COLOR = clMaroon;
  ZEITNOT_FORMAT = 's"."zzz';
//  CHEAT_TIME_CONST = 1.5; // > 1
  WHITE_LONG_LABEL: WideString   =   'White   ';
  WHITE_MEDIUM_LABEL: WideString = 'White ';
  WHITE_SHORT_LABEL: WideString  =  'W ';
  BLACK_LONG_LABEL: WideString   =   'Black   ';
  BLACK_MEDIUM_LABEL: WideString = 'Black ';
  BLACK_SHORT_LABEL: WideString  =  'B ';

////////////////////////////////////////////////////////////////////////////////
// Globals

function CheckCheck(const pos: TChessPosition): boolean;
begin
  Result := TChessRulesEngine.CheckCheck(pos);
end;


function TChessBoard.CanMove(pos: TChessPosition): boolean;
begin
  Result := ChessRulesEngine.CanMove(pos);
end;


function TChessBoard.DoMove(move_str: string): boolean;
begin
  // Отмена анимации
  if (AnimateTimer.Enabled) then
  begin
    AnimateTimer.Enabled := FALSE;
    anim_step := anim_step_num;
    AnimateTimerTimer(nil);
  end;

  Result := ChessRulesEngine.DoMove(move_str);

  if (Result) then
  begin
    Animate(lastMove.i, lastMove.j);
    SwitchClock(PositionColor);
    if (m_bFlash_move and (mode_var = mGame)) then
      FFlashWindow;
  end;
end;


procedure TChessBoard.ShowTime(const c: TFigureColor);
var
  time_label: TLabel;
begin
  if c = fcWhite then time_label:= WhiteTimeLabel
    else time_label:= BlackTimeLabel;

  if unlimited_var[c] then
    begin
      time_label.Caption:= '';
      exit;
    end;

  time_label.Font.Color:= TIME_COLOR;

  LongTimeFormat:= MIN_TIME_FORMAT;
  if player_time[c] >= EncodeTime(1, 0, 0, 0) then
    LongTimeFormat:= HOUR_TIME_FORMAT
  else
    if (player_time[c] < EncodeTime(0, 0, ZEITNOT_BOARDER, 0)) and
       (player_time[c] > 0) then
      begin
        LongTimeFormat:= ZEITNOT_FORMAT;
        time_label.Font.Color:= ZEITNOT_COLOR;
      end;

  time_label.Caption:= TimeToStr(player_time[c]);
end;



procedure TChessBoard.SetFlipped(const f: boolean);
begin
  // TODO:
  _flipped:= f;
  RDrawBoard;
end;


function TChessBoard.SetPosition(const posstr: string): boolean;
var
  i, j, k: integer;
  l: byte;
  pos: TChessPosition;
begin
  Result:= FALSE;
  l := 1;
  for j := 8 downto 1 do
    begin
      i := 1;
      repeat
        case posstr[l] of
          'K': pos.board[i,j]:= WK;
          'Q': pos.board[i,j]:= WQ;
          'R': pos.board[i,j]:= WR;
          'B': pos.board[i,j]:= WB;
          'N': pos.board[i,j]:= WN;
          'P': pos.board[i,j]:= WP;

          'k': pos.board[i,j]:= BK;
          'q': pos.board[i,j]:= BQ;
          'r': pos.board[i,j]:= BR;
          'b': pos.board[i,j]:= BB;
          'n': pos.board[i,j]:= BN;
          'p': pos.board[i,j]:= BP;

          '1'..'8':      // Вставка пустых полей
            begin
              k:= StrToInt(posstr[l]);
              repeat
                pos.board[i,j]:= ES;
                dec(k); inc(i);
              until k = 0;
              dec(i);
            end;

          ' ': break;  // Позиция прочитана - выход из цикла

          else exit; // ошибка в posstr
        end;
        inc(i); inc(l);
      until (posstr[l] = '/') or (i > 8); // Повтор до появления '/' или пока на горизонтали
      inc(l);
    end;

  case posstr[l] of
    'w': pos.color:= fcWhite;
    'b': pos.color:= fcBlack;
    else exit;
  end;

  inc(l,2);
  pos.castling:= [];
  while posstr[l] <> ' ' do
    begin
      with pos do
        case posstr[l] of
          'K': castling:= castling + [WhiteKingSide];
          'Q': castling:= castling + [WhiteQueenSide];
          'k': castling:= castling + [BlackKingSide];
          'q': castling:= castling + [BlackQueenSide];
          '-':
            if castling <> [] then exit
              else
                begin
                  inc(l);
                  break;
                end;
          else exit;
        end;
      inc(l);
    end;

  inc(l);
  with pos do
    case posstr[l] of
      'a'..'h': en_passant:= ord(posstr[l]) - ord('a') + 1;
      '-': en_passant:= 0;
      else exit;
    end;

  if (Trim(RightStr(posstr, length(posstr) - l)) <> '') then
    exit;

  CancelAnimationDragging;
  RSetPositionRec(pos);
  clock_color:= Position.color;
  lastMove.i0 := 0; // предыдущего хода ещё не было
  Result:= TRUE;
  RDrawBoard;
end;


function TChessBoard.GetPosition: string;
var
  i,j: Integer;
  k: byte;
  chFig: char;
begin
  Result:= '';

  with Position^ do
    begin
      // Расстановка фигур
      for j:= 8 downto 1 do
        begin
          k:= 0;
          for i:= 1 to 8 do
            begin
              case board[i,j] of
                WK: chFig := 'K';
                WQ: chFig := 'Q';
                WR: chFig := 'R';
                WB: chFig := 'B';
                WN: chFig := 'N';
                WP: chFig := 'P';
                BK: chFig := 'k';
                BQ: chFig := 'q';
                BR: chFig := 'r';
                BB: chFig := 'b';
                BN: chFig := 'n';
                BP: chFig := 'p';
                ES:
                  begin
                    inc(k);
                    continue;
                  end;
              end;

              if k > 0 then
                begin
                  Result:= Result + IntToStr(k);
                  k:= 0;
                end;

              Result := Result + chFig;
            end;

          if k > 0 then Result:= Result + IntToStr(k);
          if j = 1 then Result:= Result + ' '
            else Result:= Result + '/'; // i <= 7
        end;

        if color = fcWhite then Result:= Result + 'w '
          else Result:= Result + 'b '; // color = fcBlack
        // Рокировка
        if castling = [] then Result:= Result + '-'
          else
            begin
              if WhiteKingSide in castling then Result:= Result + 'K';
              if WhiteQueenSide in castling then Result:= Result + 'Q';
              if BlackKingSide in castling then Result:= Result + 'k';
              if BlackQueenSide in castling then Result:= Result + 'q';
            end;
        // en-passant
        if en_passant = 0 then Result:= Result + ' -'
          else Result:= Result + ' ' + Chr(Ord('a')-1 + en_passant);
    end;
end;


function TChessBoard.GetPositionRec: TChessPosition;
begin
  Result := Position^;
end;


function TChessBoard.GetLastMoveAbs: TMoveAbs;
begin
  Result.i0 := lastMove.i0;
  Result.j0 := lastMove.j0;
  Result.i := lastMove.i;
  Result.j := lastMove.j;
  Result.prom_fig := lastMove.prom_fig;
end;


procedure TChessBoard.FormCreate(Sender: TObject);
begin
  m_iDeltaWidthHeight := Width - Height;

  m_iTimePanelInitialWidth := TimePanel.Width;
  m_iWhitePanelInitialLeft := WhitePanel.Left;
  m_iWhitePanelInitialWidth := WhitePanel.Width;
  m_iBlackPanelInitialLeft := BlackPanel.Left;
  m_iBlackPanelInitialWidth := BlackPanel.Width;

  m_TimeFont := TFont.Create;
  m_TimeFont.Assign(WhiteTimeLabel.Font);

  m_BitmapRes := TBitmapRes.Create(Size(PBoxBoard.Width, PBoxBoard.Height));

  BlackFlagButton.Glyph := WhiteFlagButton.Glyph; // чтоб не тащить лишнего
  coord_show:= TRUE;
  last_hilight:= FALSE;
  m_animation := aQuick;

  TLocalizer.Instance.AddSubscriber(self);
  FLocalize;

  m_ChessRulesEngine := TChessRulesEngine.Create(self);

  // Clock initialization
  SetUnlimited(fcWhite, TRUE); SetUnlimited(fcBlack, TRUE);

  InitPosition;
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
//      Canvas.Brush.Color := self.Color;
//      Canvas.FloodFill(0,0, Canvas.Pixels[0,0], fsSurface);
    end;

  // Draw coordinates
  if coord_show then
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


procedure TChessBoard.RDrawBoard;
begin
  RDrawHiddenBoard;
  PBoxBoardPaint(nil);
end;


procedure TChessBoard.PBoxBoardPaint(Sender: TObject);
begin
  PBoxBoard.Canvas.Draw(0,0, bmHiddenBoard); // Вывод скрытой доски на форму
//  PBoxBoard.Canvas.StretchDraw(Bounds(0, 0, PBoxBoard.Width, PBoxBoard.Height), bmHiddenBoard);
end;


constructor TChessBoard.Create(Owner: TComponent; h: TChessBoardHandler);
begin
  inherited Create(Owner);
  Handler:= h;
end;


procedure TChessBoard.FormDestroy(Sender: TObject);
var
  fig: TFigure;
begin
  m_ChessRulesEngine.Free;

  bmHiddenBoard.Free;
  m_bmBuf.Free;

  for fig:= low(fig) to high(fig) do
    m_bmFigure[fig].Free;
  m_bmChessBoard.Free;

  m_BitmapRes.Free;
  m_TimeFont.Free;

  TLocalizer.Instance.DeleteSubscriber(self);
end;


procedure TChessBoard.PBoxBoardDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i,j: Integer;
begin
  WhatSquare(Point(X,Y), i, j);
  case Mode of
    mGame:
    begin
      if (DoMove(i, j)) then
      begin
        SwitchClock(PositionColor);
        dragged_moved:= TRUE;
      end;
    end;

    mEdit:
      begin
        Position.board[i0, j0] := ES;
        Position.board[i, j] := fig;
      end;
  end; // case
end;


procedure TChessBoard.PBoxBoardDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  rect: TRect;
  i,j: Integer;
begin
  case State of
    dsDragEnter:
      hilighted:= FALSE;
    dsDragMove:
      begin
        // Восстановить фрагмент на bmHiddenBoard
        bmHiddenBoard.Canvas.Draw(x0-dx, y0-dy, m_bmBuf);
        // Копировать новый фрагмент в буфер
        m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
          bmHiddenBoard.Canvas, Bounds(X-dx, Y-dy, iSquareSize, iSquareSize));
        // Нарисовать перетаскиваемую фигуру в новой позиции
        bmHiddenBoard.Canvas.Draw(X-dx, Y-dy, m_bmFigure[fig]);
        // Перенести новый фрагмент на экран
        rect:= Bounds(Min(x0,X) - dx, Min(y0,Y) - dy,
          abs(X-x0) + iSquareSize, abs(Y-y0) + iSquareSize);
        PBoxBoard.Canvas.CopyRect(rect, bmHiddenBoard.Canvas, rect);

        x0:= X; y0:= Y;

        WhatSquare(Point(X,Y), i,j);
        if (i in [1..8]) and (j in [1..8]) then Accept:= TRUE
          else Accept:= FALSE;
      end;
  end;
end;

procedure TChessBoard.PBoxBoardEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  if hilighted then
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
        LineTo(x0,y0);

        PBoxBoardPaint(nil);
      end
  else
    begin
      RDrawBoard;
      if dragged_moved then
        begin
          HilightLastMove;
          Evaluate;
          dragged_moved:= FALSE;
        end;
    end;
end;


procedure TChessBoard.WhatSquare(const P: TPoint;
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


procedure TChessBoard.PBoxBoardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j: Integer;
  f: TFigure;
begin
  WhatSquare(Point(X,Y), i,j);
  if not (i in [1..8]) or not (j in [1..8]) then exit;
  f:= Position.board[i,j];
  case Mode of
    mGame:
      if (Button <> mbLeft) or (Position.color <> player_color) or
         (((Position.color <> fcWhite) or (f >= ES)) and
          ((Position.color <> fcBlack) or (f <= ES))) then exit;
    else exit;
  end;

  if anim_step < anim_step_num then
    begin
      anim_step:= anim_step_num;
      AnimateTimerTimer(nil);
    end;

  if (i = i0) and (j = j0) then
    hilighted := (hilighted xor TRUE)
  else
    hilighted:= TRUE;

  fig := f;
  i0 := i;
  j0 := j;

  dx := (X - CHB_X) mod iSquareSize;
  dy := (Y - CHB_Y) mod iSquareSize;
  x0 := X;
  y0:= Y;

  dragged_moved := TRUE;
  PBoxBoard.BeginDrag(FALSE);
end;


procedure TChessBoard.PBoxBoardMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  f: TFigure;
  i,j: Integer;
begin
  WhatSquare(Point(X,Y), i,j);
  if not ((i in [1..8]) and (j in [1..8])) then
    begin
      PBoxBoard.Cursor:= crDefault;
      exit;
    end;
  f:= Position.board[i,j];
  case Mode of
    mGame:
      if (player_color = Position.color) and
         (((Position.color = fcWhite) and (f < ES)) or
          ((Position.color = fcBlack) and (f > ES))) then
        PBoxBoard.Cursor:= crHandPoint
      else
        PBoxBoard.Cursor:= crDefault;

    else
      PBoxBoard.Cursor:= crDefault;
  end;
end;


function TChessBoard.DoMove(i,j: integer; prom_fig: TFigureName = K): boolean;
var
  strLastMove: string;
begin
  Result := ChessRulesEngine.DoMove(i, j, prom_fig);
  if (Result) then
  begin
    if (Assigned(Handler) and
        ((Mode = mGame) and (Position.color <> player_color))) then
    begin
      strLastMove := ChessRulesEngine.LastMoveStr;
      Handler(cbeMoved, @strLastMove, self);
    end;
  end;
end;


function TChessBoard.FGetPositionsList: TList;
begin
  Result := ChessRulesEngine.PositionsList;
end;


procedure TChessBoard.PBoxBoardMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i,j: integer;
begin
  case Button of
    mbLeft:
      case Mode of
        mGame:
          begin
            if not hilighted then exit;
            WhatSquare(Point(X,Y), i,j);
            if dragged_moved then
              RDrawBoard
            else
            begin
              hilighted:= FALSE;
              if DoMove(i,j) then
                begin
                  Animate(i,j);
                  SwitchClock(PositionColor);
                end
              else
                RDrawBoard;
            end;
          end;
        mEdit: ;
      end;
    mbRight:
      if Assigned(Handler) then Handler(cbeMenu, self);
  end;
end;

procedure TChessBoard.PBoxBoardStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  // Копировать изображение пустого поля в m_bmBuf
  m_bmBuf.Width:= iSquareSize; m_bmBuf.Height:= iSquareSize;
  if (((i0 + j0) and 1) <> 0) then
      m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
        m_bmFigure[ES].Canvas, Bounds(0,0, iSquareSize, iSquareSize))
  else
    m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(iSquareSize,0, iSquareSize, iSquareSize));

  dragged_moved:= FALSE;
end;


procedure TChessBoard.Animate(const i,j: integer);
var
  x,y: integer;
begin
  if not Showing then exit;

  case animation of
    aNo: anim_step_num:= 1;
    aSlow: anim_step_num:= ANIMATION_SLOW;
    aQuick: anim_step_num:= ANIMATION_QUICK;
  end;

  if _flipped then
    begin
      x0:= (8 - i0) * iSquareSize + CHB_X;
      y0:= (j0 - 1) * iSquareSize + CHB_Y;
      x:= (8 - i) * iSquareSize + CHB_X;
      y:= (j - 1) * iSquareSize + CHB_Y;
    end
  else
    begin
      x0:= (i0 - 1) * iSquareSize + CHB_X;
      y0:= (8 - j0) * iSquareSize + CHB_Y;
      x:= (i - 1) * iSquareSize + CHB_X;
      y:= (8 - j) * iSquareSize + CHB_Y;
    end;

  anim_dx:= (x-x0) / anim_step_num;
  anim_dy:= (y-y0) / anim_step_num;

  anim_step:= 0;

  // Копировать изображение пустого поля в m_bmBuf
  m_bmBuf.Width := iSquareSize;
  m_bmBuf.Height := iSquareSize;
  if (((i0 + j0) and 1) <> 0) then
    m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(0,0, iSquareSize, iSquareSize))
  else
    m_bmBuf.Canvas.CopyRect(Bounds(0,0, iSquareSize, iSquareSize),
      m_bmFigure[ES].Canvas, Bounds(iSquareSize,0, iSquareSize, iSquareSize));

  AnimateTimer.Enabled:= TRUE;
end;


procedure TChessBoard.AnimateTimerTimer(Sender: TObject);
var
  X,Y: integer;
  rect: TRect;
begin
  inc(anim_step);
  if anim_step < anim_step_num then
    begin
      X:= round(x0 + anim_dx * anim_step);
      Y:= round(y0 + anim_dy * anim_step);
      dx:= X - x0 - round(anim_dx * (anim_step - 1));
      dy:= Y - y0 - round(anim_dy * (anim_step - 1));

      // Восстановить фрагмент на bmHiddenBoard
      bmHiddenBoard.Canvas.Draw(X-dx, Y-dy, m_bmBuf);
      // Копировать новый фрагмент в буфер
      m_bmBuf.Canvas.CopyRect(Bounds(0, 0, iSquareSize, iSquareSize),
        bmHiddenBoard.Canvas, Bounds(X, Y, iSquareSize, iSquareSize));
      // Нарисовать перетаскиваемую фигуру в новой позиции
      bmHiddenBoard.Canvas.Draw(X, Y, m_bmFigure[fig]);
      // Перенести новый фрагмент на экран
      rect:= Bounds(Min(X-dx, X), Min(Y-dy, Y),
        abs(dx) + iSquareSize, abs(dy) + iSquareSize);
      PBoxBoard.Canvas.CopyRect(rect, bmHiddenBoard.Canvas, rect);
    end
  else
    begin
      AnimateTimer.Enabled := FALSE;
//    SwitchClock(PositionColor);
      RDrawBoard;
      HilightLastMove;
      Evaluate;
    end;
end;


procedure TChessBoard.InitPosition;
begin
  case Mode of
    mGame, mView:
      begin
        SetPosition(INITIAL_CHESS_POSITION);
        ResetMoveList;
      end;
    mEdit:
      SetPosition(EMPTY_CHESS_POSITION);
  end;
  RDrawBoard;
end;


procedure TChessBoard.SetMode(const m: TMode);
begin
  mode_var := m;
  RDrawBoard;
  HilightLastMove;
  if mode_var <> mGame then
    begin
      WhiteFlagButton.Visible := FALSE;
      BlackFlagButton.Visible := FALSE;
    end;
end;


procedure TChessBoard.SetTime(color: TFigureColor; const tm: TDateTime);
begin
  if not Unlimited[color] then
    begin
      if not auto_flag then
        case color of
          fcWhite:
            WhiteFlagButton.Visible := ((player_color = fcBlack) and (tm = 0.0));
          fcBlack:
            BlackFlagButton.Visible := ((player_color = fcWhite) and (tm = 0.0));
        end;
      player_time[color]:= tm;
      ShowTime(color);
    end;
end;


function TChessBoard.GetTime(color: TFigureColor): TDateTime;
begin
  Result:= player_time[color];
end;


procedure TChessBoard.GameTimerTimer(Sender: TObject);
begin
  if unlimited_var[clock_color] then
    begin
      GameTimer.Enabled := FALSE;
      exit;
    end;
  // ToDo - проверка на читерство
  player_time[clock_color] := player_time[clock_color] - (Now - past_time);
  if player_time[clock_color] <= 0.0 then
    begin
      player_time[clock_color] := 0.0;
      ShowTime(clock_color);
      if (not auto_flag) and (player_color <> clock_color) then
        case clock_color of
          fcWhite:
            WhiteFlagButton.Visible := TRUE;
          fcBlack:
            BlackFlagButton.Visible := TRUE;
        end;
      if (player_color <> clock_color) and Assigned(Handler) and (Mode = mGame) and (auto_flag) then
        Handler(cbeTimeOut, self);
      GameTimer.Enabled := FALSE;
    end;
{$IFNDEF THREADED_CHESSCLOCK}
  ShowTime(clock_color);
{$ENDIF}

  past_time:= Now;
end;


procedure TChessBoard.SetUnlimited(color: TFigureColor; const unl: boolean);
begin
  unlimited_var[color]:= unl;
  ShowTime(color);
end;


function TChessBoard.GetUnlimited(color: TFigureColor): boolean;
begin
  Result:= unlimited_var[color];
end;


procedure TChessBoard.SwitchClock(clock_color: TFigureColor);
begin
  self.clock_color := clock_color;
  if not GameTimer.Enabled then
    begin
      past_time := Now;
      GameTimer.Enabled := TRUE;
    end;
  if Assigned(Handler) and (Mode = mGame) then
    Handler(cbeClockSwitched, self);
  ShowTime(clock_color);

{$IFDEF THREADED_CHESSCLOCK}
  if not Assigned(TimeLabelThread) then
    TimeLabelThread := TTimeLabelThread.Create(self);
{$ENDIF}
end;


procedure TChessBoard.HilightLastMove;
var
  i, j, l,
  i0, j0, x, y: integer;
begin
  // вывод последнего сделанного хода
  if last_hilight  and (lastMove.i0 <> 0) and
     ((Mode = mGame) or (Mode = mView)) then
    begin
      if _flipped then
        begin
          i0 := 9 - lastMove.i0;
          j0:= lastMove.j0;
          i := 9 - lastMove.i;
          j:= lastMove.j;
        end
      else
        begin
          i0 := lastMove.i0;
          j0:= 9 - lastMove.j0;
          i := lastMove.i;
          j:= 9 - lastMove.j;
        end;

       x:= iSquareSize * (i0 - 1) + CHB_X;
       y:= iSquareSize * (j0 - 1) + CHB_Y;
       bmHiddenBoard.Canvas.Pen.Color := HILIGHT_LAST_MOVE_COLOR;
       bmHiddenBoard.Canvas.Pen.Width := HILIGHT_LAST_MOVE_WIDTH;

       for l:= 1 to 2 do
         with bmHiddenBoard.Canvas do
           begin
             MoveTo(x,y);
             LineTo(x + iSquareSize - 1, y);
             LineTo(x + iSquareSize - 1, y + iSquareSize - 1);
             LineTo(x,y + iSquareSize - 1);
             LineTo(x,y);

             x:= iSquareSize * (i - 1) + CHB_X;
             y:= iSquareSize * (j - 1) + CHB_Y;
           end;
      PBoxBoardPaint(nil);
    end;
end;


procedure TChessBoard.Copy(cb: TChessBoard);
begin
  Position^ := cb.Position^;
  _flipped:= cb._flipped;

  lastMove.i0:= cb.lastMove.i0; lastMove.j0:= cb.lastMove.j0;
  lastMove.i:= cb.lastMove.i; lastMove.j:= cb.lastMove.j;
{
  player_nick:= cb.player_nick;
}
  player_color:= cb.player_color;

  player_time:= cb.player_time;
  clock_color:= cb.clock_color;
  unlimited_var:= cb.unlimited_var;

  mode_var:= cb.mode_var;

  Refresh;
end;


procedure TChessBoard.Refresh;
begin
  RDrawBoard;
  HilightLastMove;
  ShowTime(fcWhite); ShowTime(fcBlack);
end;


procedure TChessBoard.SetPlayerColor(const color: TFigureColor);
begin
  CancelAnimationDragging;
  player_color:= color;
  if player_color = fcWhite then SetFlipped(FALSE)
    else SetFlipped(TRUE); // player_color = fcBlack
end;


procedure TChessBoard.StopClock;
begin
  GameTimer.Enabled := FALSE;
  WhiteFlagButton.Visible := FALSE;
  BlackFlagButton.Visible := FALSE;  
end;


procedure TChessBoard.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var

  NewBoardSize: TSize;
begin
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


procedure TChessBoard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ((not shuted) and Assigned(Handler)) then
    begin
      Handler(cbeExit, self);
      Action:= caNone;
    end
  else
    shuted := FALSE;
end;


procedure TChessBoard.Shut;
begin
  shuted:= TRUE;
  Close;
end;


procedure TChessBoard.Evaluate;
begin
  if Assigned(Handler) and not CanMove(Position^) then
     if CheckCheck(Position^) then
       Handler(cbeMate, self)
       else Handler(cbeStaleMate, self);
  // TODO: Оценка позиции на возможность технической ничьи
end;


procedure TChessBoard.PPRandom;
const
  FIG: array[0..5] of TFigureName = (B,B,Q,R,N,N);
  SQR: array[0..5] of byte = (2,3,4,6,7,0);
var
  rnd_sqr: array[0..5] of byte;
  i,j: integer;
  f: boolean;
begin
  InitPosition;
  If Random(2) = 0 then SQR[5] := 1 // с какой стороны оставляем ладью
    else SQR[5] := 8;
  for i := 0 to 5 do
    begin
      repeat
        rnd_sqr[i] := SQR[Random(6)];
        f := FALSE;
        for j := 0 to i-1 do f := f or (rnd_sqr[i] = rnd_sqr[j]);
      until not (f or ((i = 1) and (((rnd_sqr[0] xor rnd_sqr[1]) and 1) = 0)));
      Position.board[rnd_sqr[i], 1] := TFigure(ord(FIG[i]));
      Position.board[rnd_sqr[i], 8] := TFigure(ord(BK) + ord(FIG[i]));
    end;
  RDrawBoard;
end;


procedure TChessBoard.TakeBack;
begin
  if PositionsList.Count = 0 then
    exit;
  RSetPositionRec(PPosMove(PositionsList[PositionsList.Count - 1]).pos);
  DelPosList;
  // TODO: анимация
  RDrawBoard;
end;


procedure TChessBoard.DelPosList;
var
   i: integer;
begin
  i := PositionsList.Count - 1;
  if i >= 0 then
    begin
      Dispose(PositionsList[i]);
      PositionsList.Delete(i);
    end;
end;


procedure TChessBoard.ResetMoveList;
var
  i: integer;
begin
  for i := 0 to PositionsList.Count - 1 do
    Dispose(PositionsList[i]);
  PositionsList.Clear;
end;


procedure TChessBoard.SetHilightLastMove(const yes: boolean);
begin
  last_hilight := yes;
  RDrawBoard;
  HilightLastMove;
end;


procedure TChessBoard.SetCoordinates(const yes: boolean);
begin
  coord_show := yes;
  RDrawBoard;
  HilightLastMove;
end;


function TChessBoard.NMoveDone: integer;
begin
  Result := (PositionsList.Count + 1) shr 1; // div 2
end;


procedure TChessBoard.RSetPositionRec(const pos: TChessPosition);
begin
  Position^ := pos;
end;


{$IFDEF THREADED_CHESSCLOCK}
procedure TTimeLabelThread.Execute;
begin
  while ChessBoard.GameTimer.Enabled do
    begin
      if self.player_time[fcWhite] <> ChessBoard.player_time[fcWhite] then
        ChessBoard.ShowTime(fcWhite);
      if self.player_time[fcBlack] <> ChessBoard.player_time[fcBlack] then
        ChessBoard.ShowTime(fcBlack);
      Sleep(ChessBoard.GameTimer.Interval div 2);
    end;
  ChessBoard.TimeLabelThread := nil;  
end;


constructor TTimeLabelThread.Create(ChessBoard: TChessBoard);
begin
  self.ChessBoard := ChessBoard;
  self.player_time[fcWhite] := ChessBoard.player_time[fcWhite];
  self.player_time[fcBlack] := ChessBoard.player_time[fcBlack];

  inherited Create(TRUE);
//Priority := tpNormal;
  FreeOnTerminate := TRUE;
  Resume;
end;
{$ENDIF}

procedure TChessBoard.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Handler) then
    Handler(cbeKeyPressed, Pointer(Key), self);
end;


function TChessBoard.GetStayOnTop: boolean;
begin
  Result := (self.FormStyle = fsStayOnTop);
end;


procedure TChessBoard.SetStayOnTop(onTop: boolean);
begin
  if onTop then
    self.FormStyle := fsStayOnTop
  else
    self.FormStyle := fsNormal;
end;


procedure TChessBoard.FormActivate(Sender: TObject);
begin
  if Assigned(Handler) then
    Handler(cbeActivate, self);
end;


procedure TChessBoard.WMMoving(var Msg: TWMMoving);
begin
  // TODO: возможна обработка выхода формы за пределы экрана.
  if Assigned(Handler) then
    Handler(cbeFormMoving, Pointer(Msg.DragRect.Left - Left), Pointer(Msg.DragRect.Top - Top));
  inherited;
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
  end; { case }
end;


function TChessBoard.FGetPositinoColor: TFigureColor;
begin
  Result := Position.color;
end;


function TChessBoard.FGetPosition: PChessPosition;
begin
  Result := ChessRulesEngine.Position;
end;


function TChessBoard.AskPromotionFigure(FigureColor: TFigureColor): TFigureName;
begin
  if (Showing) then
  begin
    with TPromotionForm.Create(self, m_BitmapRes) do
    try
      Result := ShowPromotion(FigureColor);
    finally
      Free;
    end;
  end
  else
    Result := Q;
end;


function TChessBoard.FGetI0J0(iIndex: integer): integer;
begin
  case iIndex of
    0: Result := ChessRulesEngine.i0;
    1: Result := ChessRulesEngine.j0;
  else
    Result := 0;
  end;
end;


procedure TChessBoard.FSetI0J0(iIndex, iValue: integer);
begin
  case iIndex of
    0: ChessRulesEngine.i0 := iValue;
    1: ChessRulesEngine.j0 := iValue;
  end;
end;


function TChessBoard.FGetLastMove: PMoveAbs;
begin
  Result := ChessRulesEngine.lastMove;
end;


function TChessBoard.FGetFig: TFigure;
begin
  Result := m_ChessRulesEngine.fig;
end;


procedure TChessBoard.FSetFig(Value: TFigure);
begin
  m_ChessRulesEngine.fig := Value;
end;


procedure TChessBoard.CancelAnimationDragging;
begin
  // Отмена анимации и перетаскивания
  if AnimateTimer.Enabled then
    begin
      AnimateTimer.Enabled := FALSE;
      // anim_step := anim_step_num;
      // AnimateTimerTimer(nil);
    end;
  if PBoxBoard.Dragging then
    begin
      dragged_moved := FALSE;
      PBoxBoard.EndDrag(FALSE);
    end;
end;


procedure TChessBoard.FlagButtonClick(Sender: TObject);
begin
  if Assigned(Handler) and (Mode = mGame) then
    Handler(cbeTimeOut, self);
end;


procedure TChessBoard.SetAutoFlag(auto_flag: boolean);
begin
  self.auto_flag := auto_flag;
  if auto_flag then
    begin
      WhiteFlagButton.Visible := FALSE;
      BlackFlagButton.Visible := FALSE;
    end;
end;


procedure TChessBoard.FormResize(Sender: TObject);
var
  fig: TFigure;
begin
  FreeAndNil(m_bmChessBoard);
  m_BitmapRes.CreateBoardBitmap(Size(PBoxBoard.Width, PBoxBoard.Height), self.Color,
    m_bmChessBoard);
  iSquareSize := m_BitmapRes.SquareSize;

  for fig := low(fig) to high(fig) do
  begin
    FreeAndNil(m_bmFigure[fig]);
    m_BitmapRes.CreateFigureBitmap(fig, m_bmFigure[fig]);
  end;

  // Graphics initialization
  if (not Assigned(bmHiddenBoard)) then
  begin
    bmHiddenBoard := TBitmap.Create;
    bmHiddenBoard.Palette := m_bmChessBoard.Palette;
    bmHiddenBoard.Canvas.Font := PBoxBoard.Font; // Характеристики шрифта координат задаются в инспекторе
    bmHiddenBoard.Canvas.Brush.Style := bsClear;
  end;
  bmHiddenBoard.Width := m_bmChessBoard.Width;
  bmHiddenBoard.Height := m_bmChessBoard.Height;

  if (not Assigned(m_bmBuf)) then
  begin
    m_bmBuf := TBitmap.Create;
    m_bmBuf.Palette:= m_bmChessBoard.Palette;
  end;   

  RDrawBoard;
end;


procedure TChessBoard.TimePanelResize(Sender: TObject);
var
  rRatio: real;
begin
  // Adjust panels on TimePanel
  rRatio := TimePanel.Width / m_iTimePanelInitialWidth;
  WhitePanel.Left := Round(rRatio * m_iWhitePanelInitialLeft);
  WhitePanel.Width := m_iWhitePanelInitialWidth;
  BlackPanel.Left := Round(rRatio * m_iBlackPanelInitialLeft);
  BlackPanel.Width := m_iBlackPanelInitialWidth;


  WhiteTimeLabel.Font.Assign(m_TimeFont);
  BlackTimeLabel.Font.Assign(m_TimeFont);
  if (WhitePanel.Left + WhitePanel.Width < BlackPanel.Left) then
  begin
    WhiteLabel.Caption := WHITE_LONG_LABEL;
    BlackLabel.Caption := BLACK_LONG_LABEL;
  end
  else
  begin
    WhitePanel.Left := 4;
    WhitePanel.Width := TimePanel.Width div 2;
    BlackPanel.Left := TimePanel.Width div 2;
    BlackPanel.Width := TimePanel.Width div 2 - 4;

    WhiteLabel.Caption := WHITE_MEDIUM_LABEL;
    BlackLabel.Caption := BLACK_MEDIUM_LABEL;
  end;

  // Adjust color labels
  if ((WhiteTimeLabel.Left + WhiteTimeLabel.Width > WhitePanel.Width) or
      (BlackTimeLabel.Left + BlackTimeLabel.Width > BlackPanel.Width)) then
  begin
    WhiteTimeLabel.Font.Size := WhiteTimeLabel.Font.Size - 4;
    BlackTimeLabel.Font.Size := BlackTimeLabel.Font.Size - 4;
    WhiteLabel.Caption := WHITE_SHORT_LABEL;
    BlackLabel.Caption := BLACK_SHORT_LABEL;
  end;
end;


procedure TChessBoard.FFlashWindow;
var
  flushWindowInfo: TFlashWInfo;
begin
 // Flash with taskbar
 flushWindowInfo.cbSize := SizeOf(flushWindowInfo);
 flushWindowInfo.hwnd := Application.Handle;
 flushWindowInfo.dwflags := FLASHW_TRAY; // FLASHW_ALL; //FLASHW_TRAY;
 flushWindowInfo.ucount := 3; // Flash times
 flushWindowInfo.dwtimeout := 0; // speed in msec, 0 - frequency of cursor flashing
 FlashWindowEx(flushWindowInfo);

 if self.Focused then
   exit;
 // Flash window
 flushWindowInfo.hwnd := self.Handle; // handle of the flashing window
 flushWindowInfo.dwflags := FLASHW_CAPTION; // FLASHW_TRAY; // FLASHW_ALL; //FLASHW_TRAY;
 FlashWindowEx(flushWindowInfo);
end;


procedure TChessBoard.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    WHITE_LONG_LABEL := GetLabel(13);
    WHITE_MEDIUM_LABEL := GetLabel(14);
    WHITE_SHORT_LABEL := GetLabel(15);
    BLACK_LONG_LABEL := GetLabel(16);
    BLACK_MEDIUM_LABEL := GetLabel(17);
    BLACK_SHORT_LABEL := GetLabel(18);
  end;

  TimePanelResize(nil);  
end;

initialization

begin
  Randomize; // для PP Random
end;

finalization

end.
