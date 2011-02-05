unit PGNWriterTestsUnit;

interface

uses
  TestFrameWork,
  //
  PGNWriterUnit;

type
  TPGNWriterTests = class(TTestCase)
  private
    m_PGNWriter: TPGNWriter;

  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure FormattedC4NOutputTest;
  end;

implementation

uses
  SysUtils, Classes,
  //
  PGNParserUnit;

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


procedure TPGNWriterTests.FormattedC4NOutputTest;

  procedure NFillInputData(var strlData: TStringList);
  begin
    strlData.Clear;
    
    strlData.Append('1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5');
    strlData.Append('(3. ... Nf6 4. Ng5 d5 5. ed Na5 6. Bb5+ c6 7. dc bc 8. Be2 h6 9. Nf3 e4 10. Ne5 Bd6 11. d4 ed');
    strlData.Append('(11. ... Qc7 12. f4 (12. Bd2 Be5 13. de Qe5 14. Bc3 Qg5 15. Qd2 Qd2+');
    strlData.Append('(15. ... Qg2 16. Rf1 Nb7 17. Bf6 gf 18. Qf4)');
    strlData.Append('16. Bd2)');
    strlData.Append('12. ... g5 13. fg hg 14. Bg5 Be5 15. de Qe5 16. Bf6 Qf6 17. Nc3)');
    strlData.Append('12. Nd3 Qc7 13. h3 c5 14. Bf3 Rb8 15. b3 c4 16. bc Nc4)');
    strlData.Append('(3. ... Be7 4. d4 d6) 4. c3 Nf6 5. d4');
  end;

  procedure NCheckOutputData(const strlData: TStringList);
  var
    strMsg: string;
  begin
    strMsg := 'Incorrect output data!';

    CheckEqualsString(strlData[0],  '[C4N "1"]', strMsg);
    CheckEqualsString(strlData[1],  '', strMsg);
    CheckEqualsString(strlData[2],  '1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5', strMsg);
    CheckEqualsString(strlData[3],  '  (3. ... Nf6 4. Ng5 d5 5. ed Na5 6. Bb5+ c6 7. dc bc 8. Be2 h6 9. Nf3 e4', strMsg);
    CheckEqualsString(strlData[4],  '   10. Ne5 Bd6 11. d4 ed', strMsg);
    CheckEqualsString(strlData[5],  '    (11. ... Qc7 12. f4', strMsg);
    CheckEqualsString(strlData[6],  '      (12. Bd2 Be5 13. de Qe5 14. Bc3 Qg5 15. Qd2 Qd2+ (15. ... Qg2 16. Rf1 Nb7', strMsg);
    CheckEqualsString(strlData[7],  '       17. Bf6 gf 18. Qf4) 16. Bd2)', strMsg);
    CheckEqualsString(strlData[8],  '     12. ... g5 13. fg hg 14. Bg5 Be5 15. de Qe5 16. Bf6 Qf6 17. Nc3)', strMsg);
    CheckEqualsString(strlData[9],  '   12. Nd3 Qc7 13. h3 c5 14. Bf3 Rb8 15. b3 c4 16. bc Nc4)', strMsg);
    CheckEqualsString(strlData[10],  '  (3. ... Be7 4. d4 d6)', strMsg);
    CheckEqualsString(strlData[11],  '', strMsg);
    CheckEqualsString(strlData[12], '4. c3 Nf6 5. d4', strMsg);
  end;

var
  strlData: TStringList;
begin // .FormattedC4NOutputTest
  strlData := nil;

  with TPGNParser.Create do
  try
    strlData := TStringList.Create;
    NFillInputData(strlData);

    CheckTrue(Parse(strlData), 'Incorrect input data!');
    m_PGNWriter.WriteInChess4NetFormat(Tree);
    strlData.Assign(m_PGNWriter.Data);
{$IFDEF DEVELOP}
    strlData.SaveToFile('FormattedC4NOutputTest.txt');
{$ENDIF}    

    NCheckOutputData(strlData);

  finally
    strlData.Free;
    Free;
  end;

end;


initialization
  TestFramework.RegisterTest(TPGNWriterTests.Suite);

end.
