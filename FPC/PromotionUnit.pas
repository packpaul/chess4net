////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PromotionUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  // Chess4net
  ChessBoardUnit, LResources;

type
  TPromotionForm = class(TForm)
    PromFigImage: TImage;
    procedure FormShow(Sender: TObject);
    procedure PromFigImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    fig_color: TFigureColor;
    fig: TFigureName;
  public
    function ShowPromotion(color: TFigureColor): TFigureName;
    constructor Create(AOwner: TComponent); reintroduce;
  end;

implementation


procedure TPromotionForm.FormShow(Sender: TObject);
var
  k: byte;
begin
  // Установить окно в пределах курсора
  if Left + Width > Screen.Width then
    Left :=  Screen.Width - Width;

  with PromFigImage.Canvas do
    begin
      Brush.Color := self.Color;
      Brush.Style := bsSolid;
      FillRect(Rect(0,0, PromFigImage.Width, PromFigImage.Height));

      Brush.Color:= clWhite;
      for k := 0 to 3 do
        FillRect(Rect((SQUARE_SIZE + 2) * k, 0,
                      (SQUARE_SIZE + 2) * k + SQUARE_SIZE - 1 , SQUARE_SIZE - 1));

      case fig_color of
        White:
          begin
            Draw(0,0, bmFigure[WQ]);
            Draw(SQUARE_SIZE + 2, 0, bmFigure[WR]);
            Draw(2 * (SQUARE_SIZE + 2), 0, bmFigure[WB]);
            Draw(3 * (SQUARE_SIZE + 2), 0, bmFigure[WN]);
          end;
        Black:
          begin
            Draw(0, 0, bmFigure[BQ]);
            Draw(SQUARE_SIZE + 2, 0, bmFigure[BR]);
            Draw(2 * (SQUARE_SIZE + 2), 0, bmFigure[BB]);
            Draw(3 * (SQUARE_SIZE + 2), 0, bmFigure[BN]);
          end;
      end;
    end;
end;

function TPromotionForm.ShowPromotion(color: TFigureColor): TFigureName;
begin
  fig_color:= color;
  ShowModal;
  Result:= fig;
end;


procedure TPromotionForm.PromFigImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case X div (SQUARE_SIZE + 1) of
    1: fig:= R;
    2: fig:= B;
    3: fig:= N;
    else
      fig:= Q;
  end;
  Close;
end;

procedure TPromotionForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Key:= UpCase(Key);
  case Key of
    'Q', '1', ' ', #13: fig:= Q;
    'R', '2': fig:= R;
    'B', '3': fig:= B;
    'N', '4': fig:= N;
    else exit;
  end;
  Close;
end;


constructor TPromotionForm.Create(AOwner: TComponent);
begin
  FormStyle := (AOwner as TForm).FormStyle;
  inherited;
  Left:= Mouse.CursorPos.X - SQUARE_SIZE div 2;
  Top:= Mouse.CursorPos.Y - SQUARE_SIZE div 2;
end;

initialization
  {$i PromotionUnit.lrs}

end.
