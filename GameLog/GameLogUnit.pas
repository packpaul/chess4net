unit GameLogUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, ComCtrls, Menus,
  ChessBoardHeaderUnit, ModalForm;

type
  TGameLogForm = class(TModalForm)
    MovesListView: TListView;
    PopupMenu: TPopupMenu;
    CopyToClipboardItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopyToClipboardItemClick(Sender: TObject);
  public
    // Clears the log
    procedure Init;
    procedure TakeBack;
    procedure AddMove(wMoveNum: word; FigureColor: TFigureColor; const strMove: string);
  end;

implementation

{$R *.dfm}

uses
  Math;

////////////////////////////////////////////////////////////////////////////////
// TGameLogForm

procedure TGameLogForm.FormCreate(Sender: TObject);
begin
  with Constraints do
  begin
    MinWidth := Width;
    MaxWidth := MinWidth;
    MinHeight := Height;
  end;
end;


procedure TGameLogForm.FormShow(Sender: TObject);
var
  OwnerForm: TForm;
begin
  OwnerForm := Owner as TForm;

  Height := OwnerForm.Height;
  Left := OwnerForm.Left + OwnerForm.Width;
  Top := OwnerForm.Top;

  if ((Left + Width) > Screen.Width) then
    Left := OwnerForm.Left - Width;
end;


procedure TGameLogForm.AddMove(wMoveNum: word; FigureColor: TFigureColor; const strMove: string);
var
  ListItem: TListItem;
  iToDelete: integer;
begin
  with MovesListView do
  begin
    if (Items.Count >= wMoveNum) then
    begin
      iToDelete := Items.Count - wMoveNum + IfThen((FigureColor = White), 1);
      while (iToDelete > 0) do
      begin
        Items.Delete(Items.Count - 1);
        dec(iToDelete);
      end;
    end;

    if (Items.Count < wMoveNum) then
      ListItem := MovesListView.Items.Add
    else
      ListItem := Items[wMoveNum - 1];

    ListItem.Caption := IntToStr(wMoveNum) + '.';
    if (FigureColor = White) then
    begin
      ListItem.SubItems.Clear;
      ListItem.SubItems.Add(strMove);
    end
    else  // Black
    begin
      if (ListItem.SubItems.Count = 0) then
        ListItem.SubItems.Add('...');
      if (ListItem.SubItems.Count = 1) then
        ListItem.SubItems.Add(strMove)
      else // Count = 2
        ListItem.SubItems[1] := strMove;
    end;
  end;
end;


procedure TGameLogForm.Init;
begin
  MovesListView.Clear;
end;


procedure TGameLogForm.TakeBack;
var
  iItemIndex: integer;
begin
  with MovesListView do
  begin
    if (Items.Count = 0) then
      exit;
    iItemIndex := Items.Count - 1;
    if (Items[iItemIndex].SubItems.Count < 2) then
      Items[iItemIndex].Delete
    else
      Items[iItemIndex].SubItems.Delete(1);
  end;
end;

procedure TGameLogForm.CopyToClipboardItemClick(Sender: TObject);
begin
  // TODO: copy game to clipboard
end;

end.
