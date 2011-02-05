unit PGNParserTestsUnit;

interface

uses
  TestFrameWork,
  //
  PGNParserUnit;


type
  TPGNParserTests = class(TTestCase)
  private
    m_PGNParser: TPGNParser;
  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure C4NLoadTest;
  end;

implementation

uses
  SysUtils, Classes,
  //
  PlysTreeUnit;

////////////////////////////////////////////////////////////////////////////////
// TPGNParserTests

class function TPGNParserTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TPGNParser.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TPGNWParser.Create(<method name>));

  Result := TestSuite;
end;

procedure TPGNParserTests.SetUp;
begin
  m_PGNParser := TPGNParser.Create;
end;


procedure TPGNParserTests.TearDown;
begin
  FreeAndNil(m_PGNParser);
end;


procedure TPGNParserTests.C4NLoadTest;

  procedure NFillInputData(var strlData: TStringList);
  begin
    strlData.Clear;

    strlData.Append('[C4N "1"]');
    strlData.Append('');
    strlData.Append('1. e4 c5 2. Nf3 d6 3. d4 cd 4. Nd4 Nf6 5. Nc3 g6 6. Be3 Bg7');
    strlData.Append('  (6. ... Ng4 7. Bb5+)');
    strlData.Append('');
    strlData.Append('7. f3 0-0 8. Qd2 Nc6 9. Bc4 Nd4');
    strlData.Append('  (9. ... a5 10. g4 Ne5 11. Be2 d5 12. g5)');
    strlData.Append('');
  end;

  procedure NCheckOutputTree(Tree: TPlysTree);
  const
    MAIN_LINE_MOVES: array[1..18] of string =
      ('e4', 'c5', 'Nf3', 'd6', 'd4', 'cd', 'Nd4', 'Nf6', 'Nc3', 'g6', 'Be3', 'Bg7',
       'f3', '0-0', 'Qd2', 'Nc6', 'Bc4', 'Nd4');
    FIRST_SUBLINE_MOVES: array[12..13] of string = ('Ng4', 'Bb5+');
    SECOND_SUBLINE_MOVES: array[18..23] of string = (
      'a5', 'g4', 'Ne5', 'Be2', 'd5', 'g5');
  var
    strMsg: string;
    i: integer;
    iPly: integer;
  begin
    // Main line
    CheckEquals(High(MAIN_LINE_MOVES), Tree.Count - 1,
      'Wrong number of moves in the main line!');

    for i := Low(MAIN_LINE_MOVES) to High(MAIN_LINE_MOVES) do
    begin
      strMsg := Format('Incorrect move on index %d in the main line!', [i]);
      CheckEqualsString(Tree.Plys[i], MAIN_LINE_MOVES[i], strMsg);
    end;

    // First subline
    iPly := Low(FIRST_SUBLINE_MOVES);

    CheckTrue(Tree.SetPlyForPlyIndex(iPly, FIRST_SUBLINE_MOVES[iPly]));

    CheckEquals(High(FIRST_SUBLINE_MOVES), Tree.Count - 1,
      'Wrong number of moves in the first subline!');

    for i := Low(FIRST_SUBLINE_MOVES) to High(FIRST_SUBLINE_MOVES) do
    begin
      strMsg := Format('Incorrect move on index %d in the first subline!', [i]);
      CheckEqualsString(Tree.Plys[i], FIRST_SUBLINE_MOVES[i], strMsg);
    end;

    CheckTrue(Tree.SetPlyForPlyIndex(iPly, MAIN_LINE_MOVES[iPly]));

    // Second subline
    iPly := Low(SECOND_SUBLINE_MOVES);

    CheckTrue(Tree.SetPlyForPlyIndex(iPly, SECOND_SUBLINE_MOVES[iPly]));

    CheckEquals(High(SECOND_SUBLINE_MOVES), Tree.Count - 1,
      'Wrong number of moves in the second subline!');

    for i := Low(SECOND_SUBLINE_MOVES) to High(SECOND_SUBLINE_MOVES) do
    begin
      strMsg := Format('Incorrect move on index %d in the second subline!', [i]);
      CheckEqualsString(Tree.Plys[i], SECOND_SUBLINE_MOVES[i], strMsg);
    end;

  end;

var
  strlData: TStringList;
begin // .C4NLoadTest
  strlData := TStringList.Create;
  try
    NFillInputData(strlData);
    CheckTrue(m_PGNParser.Parse(strlData));
    NCheckOutputTree(m_PGNParser.Tree);
  finally
    strlData.Free;
  end;
end;

initialization
  TestFramework.RegisterTest(TPGNParserTests.Suite);

end.
