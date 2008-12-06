unit ChessBoardHeaderUnit;

interface

uses
  Types;

type
  TFigureName = (K, Q, R, B, N, P);
  TFigure = (WK, WQ, WR, WB, WN, WP, ES,
              BK, BQ, BR, BB, BN, BP); // ES - Empty Square
  TFigureColor = (White, Black);

const
  CHB_X = 20; CHB_Y = 6;   // starting coordinates of A8 field

var
  SQUARE_SIZE: integer = 40;        // size of a chess board field

function Size(const iX, iY: integer): TSize;

implementation

function Size(const iX, iY: integer): TSize;
begin
  Result.cx := iX;
  Result.cy := iY;
end;

end.
