////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ManagerUnit.Skype;

interface

uses
  Classes, TntClasses, ExtCtrls,
  //
  ManagerUnit, ConnectorUnit, ModalForm;

type
  TManagerSkype = class(TManager)
  protected
    procedure RSendData(const cmd: string = ''); override;
  end;

  TTransmitter = class;

  TGamingManagerSkype = class(TManagerSkype)
  private
    m_bSkypeConnectionError: boolean;
    m_lstTransmittingManagers: TList;
    m_UserContacts: TContactsList;
    procedure FStopAllTransmitters;
    procedure FAddTransmitter(ATransmitter: TTransmitter);
    function FRemoveTransmitter(ATransmitter: TTransmitter): boolean;
    procedure FSetGameContextToTransmitter(ATransmitter: TTransmitter);
    procedure FBuildUserContactsForTransmition;

  protected
    procedure ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil); override;

    procedure ROnCreate; override;
    procedure ROnDestroy; override;
    procedure RHandleConnectorDataCommand(sl: string); override;
    procedure RRetransmit(const strCmd: string); override;
    procedure RBroadcast; override;

    procedure DialogFormHandler(modSender: TModalForm; msgDlgID: TModalFormID); override;
  end;


  TTransmitter = class
  private
    m_DestroyTimer: TTimer;

    m_GamingManager: TGamingManagerSkype;
    m_wstrContactHandle: WideString;
    m_bReady: boolean;

    m_Connector: TConnector;

    m_lwOpponentClientVersion: LongWord;

    constructor FCreate(const AGamingManager: TGamingManagerSkype;
      const wstrContactHandle: WideString);

    procedure FSendData(const cmd: string = '');

    property Ready: boolean read m_bReady;
    property ContactHandle: WideString read m_wstrContactHandle;

    procedure ConnectorHandler(e: TConnectorEvent;
      d1: pointer = nil; d2: pointer = nil);
    procedure FHandleConnectorDataCommand(sl: string);

    class procedure FSplitStr(s: string; var strLeft: string; var strRight: string);

    procedure FOnCreate;
    procedure FOnDestroy;

    procedure FOnDestroyTimer(Sender: TObject);

  public
    destructor Destroy; override;
    procedure Free;
  end;

implementation

{$J+}

uses
  SysUtils, StrUtils, Dialogs, Forms, Controls,
  //
  LocalizerUnit, ChessBoardUnit, ConnectingUnit, SelectSkypeContactUnit,
  GlobalsLocalUnit;

////////////////////////////////////////////////////////////////////////////////
// TGamingManagerSkype

procedure TGamingManagerSkype.ROnCreate;
begin
  try
    TLocalizer.Instance.AddSubscriber(self);

    RCreateChessBoardAndDialogs;

    RLocalize;

    RSetChessBoardToView;
    RReadPrivateSettings;

    Connector := TConnector.Create(ConnectorHandler);

    RCreateAndPopulateExtBaseList;

    // Nick initialization
    if (not m_bSkypeConnectionError) then
      RShowConnectingForm;

  except
    Release;
    raise;
  end;
end;


procedure TGamingManagerSkype.ROnDestroy;
begin
  FStopAllTransmitters;

  if (Assigned(Connector)) then
  begin
    Connector.Close;
  end;

  inherited ROnDestroy;
end;


procedure TGamingManagerSkype.FStopAllTransmitters;
var
  i: integer;
begin
  if (not Assigned(m_lstTransmittingManagers)) then
    exit;

  with TList.Create do
  try
    Assign(m_lstTransmittingManagers);
    m_lstTransmittingManagers.Clear;
    for i := 0 to Count - 1 do
      TTransmitter(Items[i]).Free;
  finally
    Free;
  end;
end;


procedure TGamingManagerSkype.FAddTransmitter(ATransmitter: TTransmitter);
begin
  if (not Assigned(m_lstTransmittingManagers)) then
    m_lstTransmittingManagers := TList.Create;
  m_lstTransmittingManagers.Add(ATransmitter);
end;


function TGamingManagerSkype.FRemoveTransmitter(ATransmitter: TTransmitter): boolean;
var
  i: integer;
begin
  Result := FALSE;

  if (not Assigned(m_lstTransmittingManagers)) then
    exit;

  for i := 0 to m_lstTransmittingManagers.Count - 1 do
  begin
    if (m_lstTransmittingManagers[i] = ATransmitter) then
    begin
      m_lstTransmittingManagers.Delete(i);
      if (m_lstTransmittingManagers.Count = 0) then
        FreeAndNil(m_lstTransmittingManagers);

      Result := TRUE;
      exit;
    end;
  end; // for
end;


procedure TGamingManagerSkype.FSetGameContextToTransmitter(ATransmitter: TTransmitter);
begin
  if (not (Assigned(ATransmitter) and ATransmitter.Ready)) then
    exit;

  ATransmitter.FSendData(CMD_NICK_ID + ' ' + PlayerNickId + ' ' + OpponentNickId + ' ' + OpponentNick);
  ATransmitter.FSendData(CMD_GAME_CONTEXT + ' ' + RGetGameContextStr);

  if (ChessBoard.Mode = mGame) then
    ATransmitter.FSendData(CMD_CONTINUE_GAME);
end;


procedure TGamingManagerSkype.RHandleConnectorDataCommand(sl: string);
var
  strCmdSaved, sr: string;
begin
  strCmdSaved := sl;

  RSplitStr(sl, sl, sr);

  if (sl = CMD_TRANSMITTING) then
  begin
    Transmittable := TRUE;
    exit;
  end;

  if (not Transmittable) then
  begin
    inherited RHandleConnectorDataCommand(strCmdSaved);
    exit;
  end;

  if (sl = CMD_WELCOME) then
  begin
    if (Assigned(ChessBoard)) then
      ChessBoard.InitPosition;
    RSetConnectionOccured;
  end
  else if (sl = CMD_NICK_ID) then
  begin
    // sr ::= <PlayerNickId><OpponentNickId><OpponentNick>
    PlayerNick := OpponentNick; // change for transmittion

    RSplitStr(sr, sl, sr);
    PlayerNickId := sl;

    RSplitStr(sr, sl, sr);
    OpponentNickId := sl;

    RSplitStr(sr, sl, sr);
    OpponentNick := sl;

    ChessBoard.Caption := RGetGameName;
  end
  else if (sl = CMD_GAME_CONTEXT) then
  begin
    RSetGameContext(sr);
  end
  else
    inherited RHandleConnectorDataCommand(strCmdSaved);
end;


procedure TGamingManagerSkype.RRetransmit(const strCmd: string);
var
  i: integer;
  ATransmitter: TTransmitter;
begin
  if (Transmittable or (not Assigned(m_lstTransmittingManagers))) then
    exit;

  for i := 0 to m_lstTransmittingManagers.Count - 1 do
  begin
    ATransmitter := m_lstTransmittingManagers[i];
    if (Assigned(ATransmitter) and (ATransmitter.Ready)) then
      ATransmitter.FSendData(strCmd);
  end;
end;


procedure TGamingManagerSkype.RBroadcast;
begin
  FBuildUserContactsForTransmition;

  with pDialogs.CreateDialog(TSelectSkypeContactForm) as TSelectSkypeContactForm do
  begin
    Init(m_UserContacts);
    Show;
  end;
end;


procedure TGamingManagerSkype.FBuildUserContactsForTransmition;
var
  i, j: integer;
  ATransmittingManager: TTransmitter;
begin
  Assert(not Assigned(m_UserContacts));
  Connector.GetConnectableUsers(m_UserContacts);

  i := m_UserContacts.Count;

  while (i > 0) do
  begin
    dec(i);

    if (Connector.ContactHandle = m_UserContacts.Handle[i]) then
    begin
      m_UserContacts.Delete(i);
      continue;
    end;

    if (not Assigned(m_lstTransmittingManagers)) then
      continue;

    for j := 0 to m_lstTransmittingManagers.Count - 1 do
    begin
      ATransmittingManager := m_lstTransmittingManagers[j];
      if (Assigned(ATransmittingManager) and
          (ATransmittingManager.ContactHandle = m_UserContacts.Handle[i])) then
      begin
        m_UserContacts.Delete(i);
        break;
      end;
    end;

  end; // while

end;


procedure TGamingManagerSkype.ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
begin
  case e of
    ceConnected:
    begin
      PlayerNick := Connector.UserHandle;
      OpponentNick := Connector.ContactHandle;
      OpponentId := OpponentNick;
      inherited ConnectorHandler(e, d1, d2);
    end;

    ceDisconnected:
    begin
      if (not Connector.connected) then
      begin
{$IFDEF SKYPE}
        Application.Terminate; // KLUDGE
{$ENDIF}
        exit;
      end;
      inherited ConnectorHandler(e, d1, d2);
    end;

    ceSkypeError:
    begin
      m_bSkypeConnectionError := TRUE;
      // TODO: Localize
      pDialogs.MessageDlg('Chess4Net was unable to attach to your Skype application' + sLineBreak +
                          'This can happen due to the following reasons:' + sLineBreak +
                          '  1) You have an old version of Skype. OR' + sLineBreak +
                          '  2) Your Skype is blocking Chess4Net. OR' + sLineBreak +
                          '  3) Your Skype doesn''t support Skype applications. OR' + sLineBreak +
                          '  4) Other reasons.' + sLineBreak +
                          'Chess4Net won''t start.', mtWarning, [mbOk], mfMsgLeave);
    end;

    ceShowConnectableUsers:
    begin
      if (Assigned(ConnectingForm)) then
        ConnectingForm.ShowSkypeAcceptLogo := FALSE;
      with pDialogs.CreateDialog(TSelectSkypeContactForm) as TSelectSkypeContactForm do
      begin
        Connector.GetConnectableUsers(m_UserContacts);
        Init(m_UserContacts);
        Show;
      end;
    end;

  else
    inherited ConnectorHandler(e, d1, d2);
  end;
end;


procedure TGamingManagerSkype.DialogFormHandler(modSender: TModalForm; msgDlgID: TModalFormID);

  procedure NDoConnectForRetransmition;
  begin
    if (modSender.ModalResult = mrOk) then
    begin
      with modSender as TSelectSkypeContactForm do
      begin
        TTransmitter.FCreate(self,
          m_UserContacts.Handle[SelectedContactIndex]);
      end;
    end;
  end;

  procedure NDoConnectForGaming;
  begin
    if (modSender.ModalResult = mrOk) then
    begin
      with modSender as TSelectSkypeContactForm do
      begin
        Connector.ConnectToContact(m_UserContacts.Handle[SelectedContactIndex]);
      end;
    end
    else
    begin
      if (Assigned(ConnectingForm)) then
        ConnectingForm.Close
      else
        Close;
    end;
  end;

begin // .DialogFormHandler
  case msgDlgID of
    mfSelectSkypeContact:
    begin
      try
        if (Connector.connected) then
          NDoConnectForRetransmition
        else
          NDoConnectForGaming;
      finally
        FreeAndNil(m_UserContacts);
      end;
    end;

  else
    inherited DialogFormHandler(modSender, msgDlgID);
  end;

end;


////////////////////////////////////////////////////////////////////////////////
// TTransmitter

constructor TTransmitter.FCreate(const AGamingManager: TGamingManagerSkype;
  const wstrContactHandle: WideString);
begin
  m_DestroyTimer := TTimer.Create(nil);
  m_DestroyTimer.Enabled := FALSE;
  m_DestroyTimer.Interval := 100;
  m_DestroyTimer.OnTimer := FOnDestroyTimer;

  m_GamingManager := AGamingManager;
  m_wstrContactHandle := wstrContactHandle;

  FOnCreate;
end;


destructor TTransmitter.Destroy;
begin
  m_DestroyTimer.Free;
  inherited;
end;


procedure TTransmitter.Free;
begin
  if (m_DestroyTimer.Enabled) then
    exit;

  FOnDestroy;

  m_DestroyTimer.Enabled := TRUE;
end;


procedure TTransmitter.FOnCreate;
var
  AConnector: TConnector;
begin
//  PlayerNick := m_GamingManager.PlayerNick;
//  OpponentNick := m_GamingManager.OpponentNick;
//  OpponentId := m_GamingManager.OpponentId;

  m_GamingManager.Connector.CreateChildConnector(ConnectorHandler, AConnector);
  Assert(Assigned(AConnector));
  m_Connector := AConnector;

  m_GamingManager.FAddTransmitter(self);

  m_Connector.ConnectToContact(m_wstrContactHandle);
end;


procedure TTransmitter.FOnDestroy;
var
  AConnector: TConnector;
begin
  m_GamingManager.FRemoveTransmitter(self);

  AConnector := m_Connector;
  m_Connector := nil;
  if (Assigned(AConnector)) then
    AConnector.Free;
end;


procedure TTransmitter.FOnDestroyTimer(Sender: TObject);
begin
  m_DestroyTimer.Enabled := FALSE; 
  inherited Free;
end;


procedure TTransmitter.ConnectorHandler(
  e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
var
  strLeft, strCmd: string;
begin
  case e of
    ceConnected:
    begin
      FSendData(CMD_VERSION + ' ' + IntToStr(CHESS4NET_VERSION));
      FSendData(CMD_TRANSMITTING);
    end;

    ceError, ceDisconnected:
    begin
      Free;
    end;

    ceData:
    begin
      strCmd := PString(d1)^;
      repeat
        strLeft := LeftStr(strCmd, pos(CMD_DELIMITER, strCmd) - 1);
        strCmd := RightStr(strCmd, length(strCmd) - length(strLeft) - length(CMD_DELIMITER));

        FHandleConnectorDataCommand(strLeft);
      until (strCmd = '');

    end;

  else
    Assert(FALSE);
  end; // case
end;


procedure TTransmitter.FHandleConnectorDataCommand(sl: string);
var
  sr: string;
  lwOpponentClientVersion: Longword;
begin
  FSplitStr(sl, sl, sr);
  
  if (sl = CMD_VERSION) then
  begin
    FSplitStr(sr, sl, sr);
    lwOpponentClientVersion := StrToIntDef(sl, CHESS4NET_VERSION);

    if (lwOpponentClientVersion < 201101) then
    begin
      FSendData(CMD_GOODBYE);
      Free;
    end;

    m_lwOpponentClientVersion := lwOpponentClientVersion;
  end
  else if (sl = CMD_TRANSMITTING) then
  begin
    FSendData(CMD_GOODBYE); // TODO: some message or output to log
    Free;
  end
  else if (sl = CMD_GOODBYE) then
  begin
    Free;
  end
  else if (sl = CMD_WELCOME) then
  begin
    FSendData(CMD_WELCOME);
    m_bReady := TRUE;
    m_GamingManager.FSetGameContextToTransmitter(self);
  end;
end;


class procedure TTransmitter.FSplitStr(s: string; var strLeft: string; var strRight: string);
begin
  TManagerSkype.RSplitStr(s, strLeft, strRight);
end;


procedure TTransmitter.FSendData(const cmd: string = '');
const
  last_cmd: string = '';
begin
  if (cmd = '') then
    exit;
  last_cmd := cmd + CMD_DELIMITER;
  m_Connector.SendData(last_cmd);
end;

////////////////////////////////////////////////////////////////////////////////
// TManagerSkype

procedure TManagerSkype.RSendData(const cmd: string = '');
const
  last_cmd: string = '';
begin
  if (cmd = '') then
    exit;
  last_cmd := cmd + CMD_DELIMITER;
  Connector.SendData(last_cmd);
end;

end.
