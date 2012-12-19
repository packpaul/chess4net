////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseTestsUnit;

interface

uses
  TestFrameworkExUnit,
  //
  PosBaseUnit, MoveTreeBaseUnit;

type
  TPosBaseTests = class(TTestCaseEx)
  private
    class function FGetPosBase: TPosBase;
    class function FGetMoveTreeBase: TMoveTreeBase;
    property PosBase: TPosBase read FGetPosBase;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase;
  protected
    class procedure BeforeAllTests; override;
    class procedure AfterAllTests; override;
  published
    procedure TestAddAGame;
  end;

implementation

uses
  Classes, SysUtils, TestFramework,
  //
  PGNTraverserUnit, MoveTreeCollectorUnit, PosBaseCollectorUnit;

type
  TMoveTreeBaseEx = class(TMoveTreeBase);
  TPosBaseEx = class(TPosBase);
  TMoveTreeCollectorEx = class(TMoveTreeCollector);
  TPosBaseCollectorEx = class(TPosBaseCollector);

var
  g_PosBase: TPosBase = nil;
  g_MoveTreeBase: TMoveTreeBase = nil;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseTests

class procedure TPosBaseTests.BeforeAllTests;
begin
  g_MoveTreeBase := TMoveTreeBaseEx.CreateForTest;
  g_PosBase := TPosBaseEx.CreateForTest(g_MoveTreeBase);
end;


class procedure TPosBaseTests.AfterAllTests;
begin
  FreeAndNil(g_PosBase);
  FreeAndNil(g_MoveTreeBase);
end;


class function TPosBaseTests.FGetPosBase: TPosBase;
begin
  Result := g_PosBase;
end;


class function TPosBaseTests.FGetMoveTreeBase: TMoveTreeBase;
begin
  Result := g_MoveTreeBase;
end;


procedure TPosBaseTests.TestAddAGame;

  procedure NCreateInitialData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 Nf6 2. Nc3 Ng8 3. Nb1 Nc6 4. Nf3 Nb8 5. Ng1 e5');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 d6 3. d4 Bg4 4. de Bf3 5. Qf3 de 6. Bc4 Nf6');
    strlData.Append('7. Qb3 Qe7 8. Nc3 c6 9. Bg5 b5 10. Nb5 cb 11. Bb5+ Nbd7');
    strlData.Append('12. 0-0-0 Rd8 13. Rd7 Rd7 14. Rd1 Qe6 15. Bd7+ Nd7 16. Qb8+ Nb8 17. Rd8#');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. d4 d6 3. Nf3 Bg4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 d6 2. d4 e5 3. Nf3 Bg4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. Nf3 Nf6 2. Ng1 Ng8 3. e4 e5');
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollector;
begin // .TestAddAGame
  strlData := nil;
  PGNTraverser := nil;
  try
    NCreateInitialData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);
    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase);

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);

    PGNTraverser.Traverse;
  finally
    PGNTraverser.Free;
    strlData.Free;
  end;

end;

initialization
  TestFramework.RegisterTest(TTestSuiteEx.Create('TPosBase', [TPosBaseTests]));

end.
