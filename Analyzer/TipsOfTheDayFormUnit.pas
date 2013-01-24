////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit TipsOfTheDayFormUnit;

interface

uses
  StdCtrls, TntStdCtrls, Classes, TntClasses, Controls,
  //
  FloatingFormsUnit;
type
  TTipsOfTheDayForm = class(TChildFloatingForm)
    PrevTipButton: TButton;
    NextTipButton: TButton;
    CloseButton: TButton;
    TipMemo: TTntMemo;
    ShowOnStartupCheckBox: TCheckBox;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NextTipButtonClick(Sender: TObject);
    procedure PrevTipButtonClick(Sender: TObject);
    procedure ShowOnStartupCheckBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    m_wstrlTips: TTntStringList;
    m_iTipIndex: integer;
    function FGetIniFileName: string;
    procedure FReadHints;
    procedure FRefresh;
    procedure FFixControlsPositioning;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Forms, SysUtils, TntIniFiles, StrUtils, Types,
  //
  IniSettingsUnit, GlobalsUnit;

const
  INI_FILE_NAME = 'TipsOfTheDay.ini';
  INI_SECTION = 'TipsOfTheDay';

////////////////////////////////////////////////////////////////////////////////
// TTipsOfTheDayForm

procedure TTipsOfTheDayForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;


function TTipsOfTheDayForm.FGetIniFileName: string;
begin
  Result := Chess4NetPath + INI_FILE_NAME;
end;


procedure TTipsOfTheDayForm.FormCreate(Sender: TObject);
begin
  m_wstrlTips := TTntStringList.Create;

  FReadHints;

  ShowOnStartupCheckBox.Checked := TIniSettings.Instance.ShowTipsOfTheDayOnStartup;
end;


procedure TTipsOfTheDayForm.FReadHints;

  function NInsertLineFeeds(const wstrSource: WideString): WideString;
  var
    iPos, iOffset: integer;
  begin
    Result := '';
    iOffset := 1;
    repeat
      iPos := PosEx('/n', wstrSource, iOffset);
      if (iPos = 0) then
        break;
      if ((iPos = 1) or (wstrSource[iPos - 1] <> '/')) then
        Result := Result + Copy(wstrSource, iOffset, iPos - iOffset) + sLineBreak
      else
        Result := Result + Copy(wstrSource, iOffset, iPos - iOffset) + 'n'; // '//n' -> '/n'
      iOffset := iPos + 2;
    until FALSE;

    Result := Result + Copy(wstrSource, iOffset, MaxInt);
  end;

var
  i: integer;
begin
  with TTntIniFile.Create(FGetIniFileName) do
  try
    ReadSectionValues(INI_SECTION, m_wstrlTips);
  finally
    Free;
  end;

  for i := 0 to m_wstrlTips.Count - 1 do
    m_wstrlTips[i] := NInsertLineFeeds(m_wstrlTips.ValueFromIndex[i]);
end;


procedure TTipsOfTheDayForm.FRefresh;
begin
  if ((m_iTipIndex >= 0) and (m_iTipIndex < m_wstrlTips.Count)) then
    TipMemo.Text := m_wstrlTips[m_iTipIndex]
  else
  begin
    TipMemo.Clear;
    m_iTipIndex := -1;
  end;

  PrevTipButton.Enabled := (m_wstrlTips.Count > 1);
  NextTipButton.Enabled := PrevTipButton.Enabled;
end;


procedure TTipsOfTheDayForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(m_wstrlTips);
end;


procedure TTipsOfTheDayForm.NextTipButtonClick(Sender: TObject);
begin
  if (m_iTipIndex < -1) then
    exit;

  inc(m_iTipIndex);
  if (m_iTipIndex >= m_wstrlTips.Count) then
    m_iTipIndex := 0;

  FRefresh;

  TIniSettings.Instance.TipsOfTheDayIndex := m_iTipIndex;
end;

procedure TTipsOfTheDayForm.PrevTipButtonClick(Sender: TObject);
begin
  if (m_iTipIndex < 0) then
    exit;

  dec(m_iTipIndex);
  if (m_iTipIndex < 0) then
    m_iTipIndex := m_wstrlTips.Count - 1;

  FRefresh;

  TIniSettings.Instance.TipsOfTheDayIndex := m_iTipIndex;      
end;


procedure TTipsOfTheDayForm.ShowOnStartupCheckBoxClick(Sender: TObject);
begin
  if (Showing) then
    TIniSettings.Instance.ShowTipsOfTheDayOnStartup := ShowOnStartupCheckBox.Checked;
end;


procedure TTipsOfTheDayForm.FormShow(Sender: TObject);
begin
  FFixControlsPositioning;

  m_iTipIndex := TIniSettings.Instance.TipsOfTheDayIndex;
  NextTipButton.Click;
end;


procedure TTipsOfTheDayForm.FFixControlsPositioning;

  function NGetAllRect: TRect;
  var
    i: integer;
    TmpRect: TRect;
  begin
    Result := Rect(0, 0, 1, 1);
    for i := 0 to ControlCount - 1 do
    begin
      if (Controls[i] is TWinControl) then
      begin
        UnionRect(TmpRect, Controls[i].BoundsRect, Result);
        Result := TmpRect;
      end;
    end;
  end;

  procedure NRebound(iDeltaX, iDeltaY: integer);
  var
    i: integer;
    AControl: TControl;
  begin
    for i := 0 to ControlCount - 1 do
    begin
      AControl := Controls[i];
      if (not (AControl is TWinControl)) then
        continue;
      if (iDeltaX < 0) then
      begin
        if (akLeft in AControl.Anchors) then
          AControl.Width := AControl.Width + iDeltaX
        else
          AControl.Left := AControl.Left + iDeltaX;
      end;
      if (iDeltaY < 0) then
      begin
        if (akTop in AControl.Anchors) then
          AControl.Height := AControl.Height + iDeltaY
        else
          AControl.Top := AControl.Top + iDeltaY;
      end;
    end;
  end;

var
  AllRect: TRect;
begin
  AllRect := NGetAllRect;
  NRebound(ClientRect.Right - AllRect.Right, ClientRect.Bottom - AllRect.Bottom);
end;

end.
