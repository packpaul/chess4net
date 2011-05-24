////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ManagerUnit.Skype;

interface

uses
  Classes, TntClasses,
  //
  ManagerUnit, ConnectorUnit, ModalForm;

type
  TManagerSkype = class(TManager)
  protected
    procedure RSendData(const cmd: string = ''); override;
  end;

  TTransmittingManagerSkype = class;

  TGamingManagerSkype = class(TManagerSkype)
  private
    m_bSkypeConnectionError: boolean;
    m_lstTransmittingManagers: TList;
    m_UserContacts: TContactsList;
    procedure FStopAllTransmitters;
    procedure FAddTransmitter(ATransmitter: TTransmittingManagerSkype);
    function FRemoveTransmitter(ATransmitter: TTransmittingManagerSkype): boolean;
    procedure FSetGameContextToTransmitter(ATransmitter: TTransmittingManagerSkype);
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


  TTransmittingManagerSkype = class(TManagerSkype)
  private
    m_GamingManager: TGamingManagerSkype;
    m_wstrContactHandle: WideString;
    m_bReady: boolean;

    constructor FCreate(const AGamingManager: TGamingManagerSkype;
      const wstrContactHandle: WideString);

    property Ready: boolean read m_bReady;
    property ContactHandle: WideString read m_wstrContactHandle;

  protected
    procedure ConnectorHandler(e: TConnectorEvent;
      d1: pointer = nil; d2: pointer = nil); override;
    procedure RHandleConnectorDataCommand(sl: string); override;

    procedure ROnCreate; override;
    procedure ROnDestroy; override;
  end;

implementation

{$J+}

uses
  SysUtils, Dialogs, Forms, Controls,
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
      TTransmittingManagerSkype(Items[i]).RReleaseWithConnectorGracefully;
  finally
    Free;
  end;
end;


procedure TGamingManagerSkype.FAddTransmitter(ATransmitter: TTransmittingManagerSkype);
begin
  if (not Assigned(m_lstTransmittingManagers)) then
    m_lstTransmittingManagers := TList.Create;
  m_lstTransmittingManagers.Add(ATransmitter);
end;


function TGamingManagerSkype.FRemoveTransmitter(ATransmitter: TTransmittingManagerSkype): boolean;
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


procedure TGamingManagerSkype.FSetGameContextToTransmitter(ATransmitter: TTransmittingManagerSkype);
begin
  if (not (Assigned(ATransmitter) and ATransmitter.Ready)) then
    exit;

  ATransmitter.RSendData(CMD_NICK_ID + ' ' + PlayerNickId + ' ' + OpponentNickId + ' ' + OpponentNick);
  ATransmitter.RSendData(CMD_GAME_CONTEXT + ' ' + RGetGameContextStr);

  if (ChessBoard.Mode = mGame) then
    ATransmitter.RSendData(CMD_CONTINUE_GAME);
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
  ATransmitter: TTransmittingManagerSkype;
begin
  if (Transmittable or (not Assigned(m_lstTransmittingManagers))) then
    exit;

  for i := 0 to m_lstTransmittingManagers.Count - 1 do
  begin
    ATransmitter := m_lstTransmittingManagers[i];
    if (Assigned(ATransmitter) and (ATransmitter.Ready)) then
      ATransmitter.RSendData(strCmd);
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
  ATransmittingManager: TTransmittingManagerSkype;
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
        TTransmittingManagerSkype.FCreate(self,
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
// TTransmittingManagerSkype

constructor TTransmittingManagerSkype.FCreate(const AGamingManager: TGamingManagerSkype;
  const wstrContactHandle: WideString);
begin
  m_GamingManager := AGamingManager;
  m_wstrContactHandle := wstrContactHandle;
  
  RCreate;
end;


procedure TTransmittingManagerSkype.ROnCreate;
var
  AConnector: TConnector;
begin
//  PlayerNick := m_GamingManager.PlayerNick;
//  OpponentNick := m_GamingManager.OpponentNick;
//  OpponentId := m_GamingManager.OpponentId;

  m_GamingManager.Connector.CreateChildConnector(ConnectorHandler, AConnector);
  Assert(Assigned(AConnector));
  Connector := AConnector;

  m_GamingManager.FAddTransmitter(self);

  Connector.ConnectToContact(ContactHandle);   
end;


procedure TTransmittingManagerSkype.ROnDestroy;
var
  AConnector: TConnector;
begin // TTransmittingManagerMI.ROnDestroy
  m_GamingManager.FRemoveTransmitter(self);

  AConnector := Connector;
  Connector := nil;
  if (Assigned(AConnector)) then
    AConnector.Free;
end;


procedure TTransmittingManagerSkype.ConnectorHandler(
  e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
begin
  case e of
    ceConnected:
    begin
      RSendData(CMD_VERSION + ' ' + IntToStr(CHESS4NET_VERSION));
      RSendData(CMD_TRANSMITTING);
    end;

    ceError, ceDisconnected:
    begin
      RReleaseWithConnectorGracefully; // ???
    end;

  else
    inherited ConnectorHandler(e, d1, d2);
  end; // case
end;


procedure TTransmittingManagerSkype.RHandleConnectorDataCommand(sl: string);
var
  sr: string;
  lwOpponentClientVersion: Longword;
begin
  RSplitStr(sl, sl, sr);
  if (sl = CMD_VERSION) then
  begin
    RSplitStr(sr, sl, sr);
    lwOpponentClientVersion := StrToIntDef(sl, CHESS4NET_VERSION);

    if (lwOpponentClientVersion < 201101) then
    begin
      RSendData(CMD_GOODBYE);
      RReleaseWithConnectorGracefully;
    end;

    RSetOpponentClientVersion(lwOpponentClientVersion);
  end
  else if (sl = CMD_TRANSMITTING) then
  begin
    RSendData(CMD_GOODBYE); // TODO: some message or output to log
    RReleaseWithConnectorGracefully;
  end
  else if (sl = CMD_GOODBYE) then
  begin
    RReleaseWithConnectorGracefully;
  end
  else if (sl = CMD_WELCOME) then
  begin
    RSendData(CMD_WELCOME);
    m_bReady := TRUE;
    m_GamingManager.FSetGameContextToTransmitter(self);
  end;
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
