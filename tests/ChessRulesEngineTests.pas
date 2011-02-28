unit ChessRulesEngineTests;

interface

uses
  TestFrameWork,
  //
  ChessRulesEngine;

type
  TChessRulesEngineTests = class(TTestCase)
  private
    m_ChessRulesEngine: TChessRulesEngine;
    procedure FCheckFENMoveNumber(iMoveNumber: integer);

  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestAfterCreationDoMove;

    procedure TestNMovesWhereWhiteStarts;
    procedure TestNMovesWhereBlackStarts;

    procedure TestMoveNumbersInFENInARowForWhite;
    procedure TestMoveNumbersInFENInARowForBlack;

    procedure TestNMovesAfterSettingFEN;

    procedure TestSettingFENMovesOffsetAndNMoves;
    procedure TestMoveNumbersInFENAfterSetPosition;
  end;

implementation

uses
  SysUtils, StrUtils;

////////////////////////////////////////////////////////////////////////////////
// TChessRulesEngineTests

class function TChessRulesEngineTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create('TChessRulesEngine');

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TChessRulesEngineTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TChessRulesEngineTests.SetUp;
begin
  m_ChessRulesEngine := TChessRulesEngine.Create;
end;


procedure TChessRulesEngineTests.TearDown;
begin
  FreeAndNil(m_ChessRulesEngine);
end;


procedure TChessRulesEngineTests.TestSettingFENMovesOffsetAndNMoves;
const
  MSG_WRONG_OFFSET = 'Wrong moves offset!';
  TESTING_FEN = 'rnbqkb1r/ppppp1pp/5p1n/8/3PPP2/8/PPP3PP/RNBQKBNR b KQkq e3 0 3';
var
  strFEN: string;
begin
  m_ChessRulesEngine.FENFormat := TRUE;

  m_ChessRulesEngine.InitNewGame;

  CheckEquals(0, m_ChessRulesEngine.MovesOffset, MSG_WRONG_OFFSET);
  CheckEquals(0, m_ChessRulesEngine.NMovesDone, MSG_WRONG_OFFSET);

  m_ChessRulesEngine.SetPosition(TESTING_FEN);
  strFEN := m_ChessRulesEngine.GetPosition;

  CheckEquals(2, m_ChessRulesEngine.MovesOffset, MSG_WRONG_OFFSET);
  CheckEquals(3, m_ChessRulesEngine.NMovesDone, MSG_WRONG_OFFSET);  

  CheckEqualsString(TESTING_FEN, strFEN);
end;


procedure TChessRulesEngineTests.TestAfterCreationDoMove;
begin
  CheckTrue(m_ChessRulesEngine.DoMove('e4'));
  CheckTrue(m_ChessRulesEngine.DoMove('e5'));  
end;


procedure TChessRulesEngineTests.FCheckFENMoveNumber(iMoveNumber: integer);
var
  strFEN: string;
  strMoveNumber: string;
begin
  strFEN := m_ChessRulesEngine.GetPosition;
  strMoveNumber := RightStr(strFEN, 1);
  CheckEqualsString(IntToStr(iMoveNumber), strMoveNumber, 'Wrong move number!');
end;


procedure TChessRulesEngineTests.TestNMovesWhereWhiteStarts;
begin
  CheckEquals(0, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('e4');
  CheckEquals(1, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('e5');
  CheckEquals(1, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('d4');
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('d5');
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);
end;


procedure TChessRulesEngineTests.TestNMovesWhereBlackStarts;
begin
  m_ChessRulesEngine.Position.color := fcBlack;

  CheckEquals(0, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('e5');
  CheckEquals(1, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('e4');
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('d5');
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.DoMove('d4');
  CheckEquals(3, m_ChessRulesEngine.NMovesDone);
end;


procedure TChessRulesEngineTests.TestNMovesAfterSettingFEN;
var
  arrFENs: array[0..4] of string;
begin
  // Set up

  m_ChessRulesEngine.FENFormat := TRUE;
  m_ChessRulesEngine.Position.color := fcBlack;

  arrFENs[0] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('e5');
  arrFENs[1] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('e4');
  arrFENs[2] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('d5');
  arrFENs[3] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('d4');
  arrFENs[4] := m_ChessRulesEngine.GetPosition;

  // Checking

  m_ChessRulesEngine.SetPosition(arrFENs[3]);
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.SetPosition(arrFENs[4]);
  CheckEquals(3, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.SetPosition(arrFENs[2]);
  CheckEquals(2, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.SetPosition(arrFENs[1]);
  CheckEquals(1, m_ChessRulesEngine.NMovesDone);

  m_ChessRulesEngine.SetPosition(arrFENs[0]);
  CheckEquals(0, m_ChessRulesEngine.NMovesDone);

end;


procedure TChessRulesEngineTests.TestMoveNumbersInFENInARowForWhite;
begin
  m_ChessRulesEngine.FENFormat := TRUE;

  m_ChessRulesEngine.GetPosition;
  FCheckFENMoveNumber(1);

  m_ChessRulesEngine.DoMove('e4');
  FCheckFENMoveNumber(1);

  m_ChessRulesEngine.DoMove('e5');
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.DoMove('d4');
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.DoMove('d5');
  FCheckFENMoveNumber(3);
end;


procedure TChessRulesEngineTests.TestMoveNumbersInFENAfterSetPosition;
var
  arrFENs: array[0..4] of string;
begin
  m_ChessRulesEngine.FENFormat := TRUE;

  arrFENs[0] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('e4');
  arrFENs[1] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('e5');
  arrFENs[2] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('d4');
  arrFENs[3] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.DoMove('d5');
  arrFENs[4] := m_ChessRulesEngine.GetPosition;

  m_ChessRulesEngine.SetPosition(arrFENS[4]);
  FCheckFENMoveNumber(3);

  m_ChessRulesEngine.SetPosition(arrFENs[2]);
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.SetPosition(arrFENs[3]);
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.SetPosition(arrFENs[1]);
  FCheckFENMoveNumber(1);

  m_ChessRulesEngine.SetPosition(arrFENs[0]);
  FCheckFENMoveNumber(1);
end;


procedure TChessRulesEngineTests.TestMoveNumbersInFENInARowForBlack;
begin
  m_ChessRulesEngine.FENFormat := TRUE;
  m_ChessRulesEngine.Position.color := fcBlack;

  m_ChessRulesEngine.GetPosition;
  FCheckFENMoveNumber(1);

  m_ChessRulesEngine.DoMove('e5');
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.DoMove('e4');
  FCheckFENMoveNumber(2);

  m_ChessRulesEngine.DoMove('d5');
  FCheckFENMoveNumber(3);

  m_ChessRulesEngine.DoMove('d4');
  FCheckFENMoveNumber(3);
end;


initialization
  TestFramework.RegisterTest(TChessRulesEngineTests.Suite);

end.
