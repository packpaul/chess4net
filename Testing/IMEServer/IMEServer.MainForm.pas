////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IMEServer.MainForm;

interface

uses
  Forms,StdCtrls, Controls, ComCtrls, Classes,
  //
  IMEServer.Headers;

type
  TMainForm = class(TForm, IView)
    PortLabel: TLabel;
    Label2: TLabel;
    DelayTrackBar: TTrackBar;
    DelayLabel: TLabel;
    ClientsListBox: TListBox;
    LogMemo: TMemo;
    Label1: TLabel;
    procedure DelayTrackBarChange(Sender: TObject);

  private
    m_dbMaxDelay: Double;
    m_ViewEvents: IViewEvents;
//    m_bUpdating: boolean;

    function FGetDelay: Double;
    procedure FDoDelayChanged;
    procedure FAdjustTrackBar(dbNewDelay: Double);
    procedure FUpdateDelayLabel;

  protected
    procedure IView.SetEvents = RSetEvents;
    procedure RSetEvents(Value: IViewEvents);
    procedure IView.SetPort = RSetPort;
    procedure RSetPort(iValue: integer);
    procedure IView.SetDelay = RSetDelay;
    procedure RSetDelay(dbDelay: Double; dbMaxDelay: Double = 0.0);
    procedure IView.AddToLog = RAddToLog;
    procedure RAddToLog(const strData: string; LogEntryType: TLogEntryType);
    procedure IView.RemoveClient = RRemoveClient;
    procedure RRemoveClient(const strHandleName: string; iHandleID: integer);
    procedure IView.AddClient = RAddClient;
    procedure RAddClient(const strHandleName: string; iHandleID: integer);
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Math;

const
  LOG_ENTRY_TYPE_STR: array[TLogEntryType] of string =(
    '',
    'Wrong command'
  );

function TMainForm.FGetDelay: Double;
begin
  Result := m_dbMaxDelay * DelayTrackBar.Position / DelayTrackBar.Max;
end;


procedure TMainForm.DelayTrackBarChange(Sender: TObject);
begin
  FUpdateDelayLabel;
  FDoDelayChanged;
end;


procedure TMainForm.FUpdateDelayLabel;
begin
  DelayLabel.Caption := FloatToStrF(FGetDelay, ffFixed, 1, 2) + ' sec.';
end;


procedure TMainForm.RAddToLog(const strData: string; LogEntryType: TLogEntryType);
var
  strFormatedEntry: string;
begin
  if (LogEntryType = letNo) then
    strFormatedEntry := Format('%s: %s ', [TimeToStr(Now), strData])
  else
    strFormatedEntry := Format('%s: (%s) %s ', [TimeToStr(Now), LOG_ENTRY_TYPE_STR[LogEntryType], strData]);

  LogMemo.Lines.Add(strFormatedEntry);
  LogMemo.ScrollBy(0, MaxInt);
end;


procedure TMainForm.RSetEvents(Value: IViewEvents);
begin
  m_ViewEvents := Value;
end;

procedure TMainForm.RSetPort(iValue: integer);
begin
  PortLabel.Caption := 'Port#: ' + IntToStr(iValue);
end;


procedure TMainForm.RSetDelay(dbDelay: Double; dbMaxDelay: Double = 0.0);
begin
  Assert(not ((m_dbMaxDelay = 0.0) and (dbMaxDelay = 0.0)));
  if (dbMaxDelay > 0.0) then
    m_dbMaxDelay := dbMaxDelay;

  FAdjustTrackBar(Min(dbDelay, m_dbMaxDelay));
end;


procedure TMainForm.FAdjustTrackBar(dbNewDelay: Double);
begin
  DelayTrackBar.Position := Round(dbNewDelay * DelayTrackBar.Max / m_dbMaxDelay);
  FUpdateDelayLabel;
end;


procedure TMainForm.RRemoveClient(const strHandleName: string; iHandleID: integer);
var
  iIndex: integer;
begin
  iIndex := ClientsListBox.Items.IndexOfObject(Pointer(iHandleID));
  if (iIndex >= 0) then
    ClientsListBox.Items.Delete(iIndex);
end;


procedure TMainForm.RAddClient(const strHandleName: string; iHandleID: integer);
begin
  ClientsListBox.AddItem(Format('%s (%d)', [strHandleName, iHandleID]), Pointer(iHandleID));
end;


procedure TMainForm.FDoDelayChanged;
begin
  if Assigned(m_ViewEvents) then
    m_ViewEvents.OnDelayChanged(FGetDelay);
end;

end.
