////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ChessEngine;

interface

uses
  ChildProc;

type
  TReplyCalculatedEvent = procedure(Sender: TObject; strMove: string) of object;
  TCalculationInfoEvent = procedure(Sender: TObject; rEvaluation: real; strMovesLine: string) of object;

  TState = (sNONE, sUCIOK, sREADYOK, sGO);

  IChessEngineClockable = interface
    procedure GetWhiteTime(out lwTimeMSec, lwIncrementMSec: LongWord);
    procedure GetBlackTime(out lwTimeMSec, lwIncrementMSec: LongWord);
  end;

  TChessEngine = class
  private
    m_State: TState;
    m_bEngineLoaded: boolean;
    m_ChildProc: TChildProc;
    m_arrReplyNotifiers: array of string;
    m_strReplyMove: string;
    m_ChessClockController: IChessEngineClockable;

    FReplyCalculatedEvent: TReplyCalculatedEvent;
    FCalculationInfoEvent: TCalculationInfoEvent; 

    procedure FLoadEngine;
    procedure FUnloadEngine;

    procedure FOnTerminateChildProcProcess(Sender: TObject);
    procedure FOnReadFromChildProcPipe(Sender: TObject; strData: string);

    procedure FParseChildProcReply(strReply: string);
    procedure FOnReplyNotifier(strReplyLine: string);
    procedure FWaitForState(State: TState);

    procedure FDoReplyCalculated(const strMove: string);
    procedure FDoCalculationInfo(rEvaluation: real; const strMovesLine: string);

    function FGetReplyNotifiers(iIndex: integer): string;
    function FGetReplyNotifiersCount: integer;
    procedure FSetReplyNotifiers(arrData: array of string);

    property ChildProc: TChildProc read m_ChildProc;

    property ReplyNotifiers[iIndex: integer]: string read FGetReplyNotifiers;
    property ReplyNotifiersCount: integer read FGetReplyNotifiersCount;

    property ChessClockController: IChessEngineClockable read m_ChessClockController;

  public
    constructor Create; overload;
    constructor Create(const AChessClockController: IChessEngineClockable); overload;
    destructor Destroy; override;

    procedure InitNewGame;
    procedure Move(const strMove: string);
    procedure SetPosition(const strFEN: string);

    function CalculateReply: string;
    procedure CalculateReplyNonBlocking;
    procedure StopCalculation;

    property OnReplyCalculated: TReplyCalculatedEvent read FReplyCalculatedEvent
                                                      write FReplyCalculatedEvent;
    property OnCalculationInfo: TCalculationInfoEvent read FCalculationInfoEvent
                                                      write FCalculationInfoEvent;
  end;

implementation

uses
  Forms, SysUtils, StrUtils, Math;

const
  ENGINE_FILE = 'RobboLito.exe';

////////////////////////////////////////////////////////////////////////////////
// TChessEngine

constructor TChessEngine.Create(const AChessClockController: IChessEngineClockable);
begin
  m_ChessClockController := AChessClockController;

  inherited Create;

  m_ChildProc := TChildProc.Create;
  m_ChildProc.OnTerminateProcess := FOnTerminateChildProcProcess;
  m_ChildProc.OnReadFromPipe := FOnReadFromChildProcPipe;

  FLoadEngine;
end;


constructor TChessEngine.Create;
begin
  Create(nil);
end;


destructor TChessEngine.Destroy;
begin
  FUnloadEngine;
  Finalize(m_arrReplyNotifiers);
  m_ChildProc.Free;
  inherited;
end;


procedure TChessEngine.FLoadEngine;
var
  EngineFileName: TFileName;
begin
  if (m_bEngineLoaded) then
    exit;
  EngineFileName := ExtractFilePath(Application.ExeName) +
    'Engines' + PathDelim + ENGINE_FILE;
  ChildProc.CreateProcess(EngineFileName);

  if (not ChildProc.ProcessCreated) then
    exit;

  m_State := sNONE;
  FSetReplyNotifiers(['uciok']);
  ChildProc.WriteToPipe('uci');
  FWaitForState(sUCIOK);

  FSetReplyNotifiers(['readyok']);
  ChildProc.WriteToPipe('isready');
  FWaitForState(sREADYOK);

  m_bEngineLoaded := TRUE;
end;


procedure TChessEngine.FUnloadEngine;
begin
  if (not m_bEngineLoaded) then
    exit;
  ChildProc.WriteToPipe('quit');
  m_bEngineLoaded := FALSE;
end;


procedure TChessEngine.InitNewGame;
begin
  if (not m_bEngineLoaded) then
    exit;
  ChildProc.WriteToPipe('position initialpos');
end;


procedure TChessEngine.Move(const strMove: string);
begin
  if (not m_bEngineLoaded) then
    exit;
  ChildProc.WriteToPipe('position moves ' + strMove);
end;


procedure TChessEngine.SetPosition(const strFEN: string);
begin
  if (not m_bEngineLoaded) then
    exit;

  StopCalculation;
  ChildProc.WriteToPipe('position fen ' + strFEN);
end;


procedure TChessEngine.StopCalculation;
begin
  if (m_State = sGo) then
  begin
    FSetReplyNotifiers(['bestmove']);
    ChildProc.WriteToPipe('stop');
    FWaitForState(sREADYOK);
  end;
end;


function TChessEngine.CalculateReply: string;
begin
  if (not m_bEngineLoaded) then
    exit;

  CalculateReplyNonBlocking;

  FWaitForState(sREADYOK);

  Result := m_strReplyMove;
end;


procedure TChessEngine.CalculateReplyNonBlocking;

  function NGetTiming: string;
  var
    lwTimeMSec, lwIncrementMSec: LongWord;
  begin
    if (not Assigned(ChessClockController)) then
    begin
      Result := ' infinite';
      exit;
    end;

    Result := '';
    ChessClockController.GetWhiteTime(lwTimeMSec, lwIncrementMSec);
    if (lwTimeMSec > 0) then
    begin
      Result := Result + ' wtime ' + IntToStr(lwTimeMSec);
      if (lwIncrementMSec > 0) then
        Result := Result + ' winc ' + IntToStr(lwIncrementMSec);
    end;
    ChessClockController.GetBlackTime(lwTimeMSec, lwIncrementMSec);
    if (lwTimeMSec > 0) then
    begin
      Result := Result + ' btime ' + IntToStr(lwTimeMSec);
      if (lwIncrementMSec > 0) then
        Result := Result + ' binc ' + IntToStr(lwIncrementMSec);
    end;
  end;

begin // TChessEngine.CalculateReplyNonBlocking
  if (not m_bEngineLoaded) then
    exit;
  FSetReplyNotifiers(['info', 'bestmove']);
  ChildProc.WriteToPipe('go' + NGetTiming);
  m_State := sGO;
end;


procedure TChessEngine.FOnTerminateChildProcProcess(Sender: TObject);
begin
  m_bEngineLoaded := FALSE;
end;


procedure TChessEngine.FOnReadFromChildProcPipe(Sender: TObject; strData: string);
begin
  FParseChildProcReply(strData);
end;


procedure TChessEngine.FParseChildProcReply(strReply: string);
var
  k: integer;
  i, j: integer;
begin
  if (ReplyNotifiersCount = 0) then
    exit;

  strReply := sLineBreak + strReply;

  for k := 0 to ReplyNotifiersCount - 1 do
  begin
    i := Pos(sLineBreak + ReplyNotifiers[k] + sLineBreak, strReply);
    j := Pos(sLineBreak + ReplyNotifiers[k] + ' ', strReply);
    if ((i = 0) or ((j > 0) and (j < i))) then
      i := j;

    if (i = 0) then
      exit;

    inc(i, length(sLineBreak));
    j := PosEx(sLineBreak, strReply, i);
    Assert(i < j);

    FOnReplyNotifier(Copy(strReply, i, j - i));
  end; // for k
end;


procedure TChessEngine.FOnReplyNotifier(strReplyLine: string);

  function NNextToken(var str: string): string;
  var
    iPos: integer;
  begin
    str := TrimLeft(str);
    if (str = '') then
      Result := ''
    else
    begin
      iPos := Pos(' ', str);
      if (iPos > 0) then
      begin
        Result := LeftStr(str, Pred(iPos));
        str := Copy(str, iPos, MaxInt);
      end
      else
      begin
        Result := str;
        str := '';
      end;
    end;
  end;

  function NParseCalculationInfo(strData: string; out strEval, strLine: string): boolean;
  const
    _CP_ = ' cp ';
    _PV_ = ' pv ';
  var
    iPos: integer;
  begin
    iPos := Pos(_CP_, strData);
    if (iPos > 0) then
    begin
      strData := Copy(strData, iPos + Length(_CP_), MaxInt);
      strEval := NNextToken(strData);
    end
    else
      strEval := '';

    iPos := Pos(_PV_, strData);
    if (iPos > 0) then
    begin
      strLine := Copy(strData, iPos + Length(_PV_), MaxInt);
    end
    else
      strLine := '';

    Result := ((strEval <> '') or (strLine <> ''));
  end;

var
  str, strEval, strLine: string;
  iEval: integer;
  rEval: real;
begin // TChessEngine.FOnReplyNotifier
  case m_State of
    sNONE:
      m_State := sUCIOK;

    sUCIOK:
      m_State := sREADYOK;

    sREADYOK:
      ;

    sGO:
    begin
      str := strReplyLine;
      if (NNextToken(str) = 'bestmove') then
      begin
        m_State := sREADYOK;
        FDoReplyCalculated(NNextToken(str))
      end
      else
      begin
        str := strReplyLine;      
        if (NNextToken(str) = 'info') then
        begin
          if (NParseCalculationInfo(str, strEval, strLine)) then
          begin
            if (TryStrToInt(strEval, iEval)) then
              rEval := 0.01 * iEval
            else
              rEval := NaN;
            FDoCalculationInfo(rEval, strLine);
          end;
        end;
      end;
    end;
  end; // case
end;


procedure TChessEngine.FWaitForState(State: TState);
begin
  while (m_bEngineLoaded and (m_State <> State)) do
  begin
    if (Application.Terminated) then
      exit;
    Application.ProcessMessages;
    Sleep(1);
  end;
end;


procedure TChessEngine.FDoReplyCalculated(const strMove: string);
begin
  m_strReplyMove := strMove;
  Move(strMove);
  if (Assigned(FReplyCalculatedEvent)) then
    FReplyCalculatedEvent(self, strMove);
end;


procedure TChessEngine.FDoCalculationInfo(rEvaluation: real; const strMovesLine: string);
begin
  if (Assigned(FCalculationInfoEvent)) then
    FCalculationInfoEvent(self, rEvaluation, strMovesLine);
end;


function TChessEngine.FGetReplyNotifiers(iIndex: integer): string;
begin
  Result := m_arrReplyNotifiers[iIndex];
end;


function TChessEngine.FGetReplyNotifiersCount: integer;
begin
  Result := Length(m_arrReplyNotifiers);
end;


procedure TChessEngine.FSetReplyNotifiers(arrData: array of string);
var
  i: integer;
begin
  SetLength(m_arrReplyNotifiers, Length(arrData));
  for i := 0 to Length(arrData) - 1 do
    m_arrReplyNotifiers[i] := arrData[i];
end;

end.