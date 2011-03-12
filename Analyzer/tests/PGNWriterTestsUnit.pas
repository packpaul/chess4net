unit PGNWriterTestsUnit;

interface

uses
  Classes, TntClasses, TestFrameWork,
  //
  PGNWriterUnit;

type
  TPGNWriterTests = class(TTestCase)
  private
    m_PGNWriter: TPGNWriter;
    procedure FFillInputData(var wstrlData: TTntStringList);

  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure FormattedC4NOutputTest;
    procedure CommentsOutputTest;
  end;

implementation

uses
  SysUtils,
  //
  PGNParserUnit, PlysTreeUnit;

////////////////////////////////////////////////////////////////////////////////
// TPGNWriterTests

class function TPGNWriterTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TPGNWriter.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TPGNWriterTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TPGNWriterTests.SetUp;
begin
  m_PGNWriter := TPGNWriter.Create;
end;


procedure TPGNWriterTests.TearDown;
begin
  FreeAndNil(m_PGNWriter);
end;


procedure TPGNWriterTests.FFillInputData(var wstrlData: TTntStringList);
begin
  wstrlData.Clear;

  wstrlData.Append('1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5');
  wstrlData.Append('(3. ... Nf6 4. Ng5 d5 5. ed Na5 6. Bb5+ c6 7. dc bc 8. Be2 h6 9. Nf3 e4 10. Ne5 Bd6 11. d4 ed');
  wstrlData.Append('(11. ... Qc7 12. f4 (12. Bd2 Be5 13. de Qe5 14. Bc3 Qg5 15. Qd2 Qd2+');
  wstrlData.Append('(15. ... Qg2 16. Rf1 Nb7 17. Bf6 gf 18. Qf4)');
  wstrlData.Append('16. Bd2)');
  wstrlData.Append('12. ... g5 13. fg hg 14. Bg5 Be5 15. de Qe5 16. Bf6 Qf6 17. Nc3)');
  wstrlData.Append('12. Nd3 Qc7 13. h3 c5 14. Bf3 Rb8 15. b3 c4 16. bc Nc4)');
  wstrlData.Append('(3. ... Be7 4. d4 d6) 4. c3 Nf6 5. d4');
end;


procedure TPGNWriterTests.FormattedC4NOutputTest;

  procedure NCheckOutputData(const Data: TTntStrings);
  var
    strMsg: string;
  begin
    strMsg := 'Incorrect output data!';

    CheckEqualsWideString(Data[0],  '[C4N "2"]', strMsg);
    CheckEqualsWideString(Data[1],  '', strMsg);
    CheckEqualsWideString(Data[2],  '1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5', strMsg);
    CheckEqualsWideString(Data[3],  '  (3. ... Nf6 4. Ng5 d5 5. ed Na5 6. Bb5+ c6 7. dc bc 8. Be2 h6 9. Nf3 e4', strMsg);
    CheckEqualsWideString(Data[4],  '   10. Ne5 Bd6 11. d4 ed', strMsg);
    CheckEqualsWideString(Data[5],  '    (11. ... Qc7 12. f4', strMsg);
    CheckEqualsWideString(Data[6],  '      (12. Bd2 Be5 13. de Qe5 14. Bc3 Qg5 15. Qd2 Qd2+ (15. ... Qg2 16. Rf1 Nb7', strMsg);
    CheckEqualsWideString(Data[7],  '       17. Bf6 gf 18. Qf4) 16. Bd2)', strMsg);
    CheckEqualsWideString(Data[8],  '     12. ... g5 13. fg hg 14. Bg5 Be5 15. de Qe5 16. Bf6 Qf6 17. Nc3)', strMsg);
    CheckEqualsWideString(Data[9],  '   12. Nd3 Qc7 13. h3 c5 14. Bf3 Rb8 15. b3 c4 16. bc Nc4)', strMsg);
    CheckEqualsWideString(Data[10], '  (3. ... Be7 4. d4 d6)', strMsg);
    CheckEqualsWideString(Data[11], '', strMsg);
    CheckEqualsWideString(Data[12], '4. c3 Nf6 5. d4', strMsg);
  end;

var
  wstrlInData, wstrlOutData: TTntStringList;
begin // .FormattedC4NOutputTest
  wstrlInData := nil;
  wstrlOutData := nil;

  with TPGNParser.Create do
  try
    wstrlInData := TTntStringList.Create;
    FFillInputData(wstrlInData);

    CheckTrue(Parse(wstrlInData), 'Incorrect input data!');
    m_PGNWriter.WriteInChess4NetFormat(Tree);

    wstrlOutData := TTntStringList.Create;
    wstrlOutData.Assign(m_PGNWriter.Data);
{$IFDEF DEVELOP}
    wstrlOutData.SaveToFile('FormattedC4NOutputTest.txt');
{$ENDIF}

    NCheckOutputData(wstrlOutData);

  finally
    wstrlOutData.Free;
    wstrlInData.Free;
    Free;
  end;

end;


procedure TPGNWriterTests.CommentsOutputTest;

  procedure NExtendWithComments(Tree: TPlysTree);
  const
    MSG_CANNOT_SWITCH_LINE = 'Cannot switch the line!';
  begin
    Tree.Comments[0] := 'This is a comment for initial position!';
    Tree.Comments[Tree.Count - 1] := 'This is a comment for the last move of the main line!';

    // Switch to 3. ... Nf6 subline
    CheckTrue(Tree.SetPlyForPlyIndex(6,  'Nf6'), MSG_CANNOT_SWITCH_LINE);

    Tree.Comments[6] := 'This is a comment for 3. ... Nf6 move of the 1-st subline!';
    Tree.Comments[7] := 'This is a very very long comment for 4. Ng5 move of the 1-st subline! It should be indeed very long in order to check the formated output of it!';
    Tree.Comments[9] := 'This_is_a_very_long_comment_for_5._ed_move_of_the_1-st_subline!_It_has_no_spaces_in_order_to_check_splitting_functionality!';
    Tree.Comments[10] := '{ } is tested!';
    Tree.Comments[18] := 'Some_not_so_long_comment_but_without_any_spaces!';

    // Switch back to 3. ... Bc5 of main line
    CheckTrue(Tree.SetPlyForPlyIndex(6,  'Bc5'), MSG_CANNOT_SWITCH_LINE);
  end;

var
  wstrlInData, wstrlOutData: TTntStringList;
begin // .CommentsOutputTest
  wstrlInData := nil;
  wstrlOutData := nil;

  with TPGNParser.Create do
  try
    wstrlInData := TTntStringList.Create;
    FFillInputData(wstrlInData);

    CheckTrue(Parse(wstrlInData), 'Incorrect input data!');

    NExtendWithComments(Tree);
    m_PGNWriter.WriteInChess4NetFormat(Tree);

    wstrlOutData := TTntStringList.Create;
    wstrlOutData.Assign(m_PGNWriter.Data);
{$IFDEF DEVELOP}
    wstrlOutData.SaveToFile('CommentsOutputTest.txt');
{$ENDIF}

  finally
    wstrlOutData.Free;
    wstrlInData.Free;
    Free;
  end;

end;


initialization
  TestFramework.RegisterTest(TPGNWriterTests.Suite);

end.
