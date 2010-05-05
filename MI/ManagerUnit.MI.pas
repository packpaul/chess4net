unit ManagerUnit.MI;

interface

uses
  ControlUnit, ManagerUnit, ConnectorUnit, ModalForm;

type
  TNonRefInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TManagerMIFactory = class(TNonRefInterfacedObject, IMirandaPlugin)
  private
    m_Connector: TConnector;
    m_Manager: IMirandaPlugin;
    m_Dialogs: TDialogs;
    procedure FDialogsHandler(modSender: TModalForm; msgDlgID: TModalFormID);
    function FGetDialogs: TDialogs;
    procedure FStartGaming;
    procedure FStartTransmitting;
    function FCanStartTransmitting: boolean;
    property Dialogs: TDialogs read FGetDialogs;
  protected
    procedure Start;
    procedure Stop;
    procedure ConnectorHandler(ce: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
  public
    constructor Create(Connector: TConnector); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Types, StrUtils, SysUtils, Classes, Dialogs, Controls,
  //
  LocalizerUnit;

type
  TTransmittingManagerMI = class;

  EGamingManagerMI = class(Exception);
  TGamingManagerMI = class(TManager, IMirandaPlugin)
  private
    m_lstTransmittingManagers: TList;

    procedure FAddTransmitter(ATransmitter: TTransmittingManagerMI);
    function FRemoveTransmitter(ATransmitter: TTransmittingManagerMI): boolean;

  protected
    procedure Start;
    procedure Stop;
    procedure ROnCreate; override;
    procedure ROnDestroy; override;
    procedure ConnectorHandler(e: TConnectorEvent;
      d1: pointer = nil; d2: pointer = nil); override;
    procedure RSetOpponentClientVersion(lwVersion: LongWord); override;
    procedure RSendData(const cmd: string); override;
    procedure RSetConnectionOccured; override;
  public
    constructor Create(Connector: TConnector); reintroduce;
  end;

  ETransmittingManagerMI = class(Exception);
  TTransmittingManagerMI = class(TManager, IMirandaPlugin)
  protected
    procedure Start;
    procedure Stop;
    procedure ROnDestroy; override;
  end;


var
  g_lstGamingManagers: TList = nil;

////////////////////////////////////////////////////////////////////////////////
// TGamingManagerMI

constructor TGamingManagerMI.Create(Connector: TConnector);
begin
  self.Connector := Connector;

  RCreate;
end;


procedure TGamingManagerMI.FAddTransmitter(ATransmitter: TTransmittingManagerMI);
begin
  if (not Assigned(m_lstTransmittingManagers)) then
    m_lstTransmittingManagers := TList.Create;
  m_lstTransmittingManagers.Add(ATransmitter);
end;


function TGamingManagerMI.FRemoveTransmitter(ATransmitter: TTransmittingManagerMI): boolean;
var
  i: integer;
begin
  Result := FALSE;

  if (not Assigned(m_lstTransmittingManagers)) then
    exit;

  for i := 0 to m_lstTransmittingManagers.Count - 1 do
  begin
    if (m_lstTransmittingManagers[i] = self) then
    begin
      m_lstTransmittingManagers.Delete(i);
      if (m_lstTransmittingManagers.Count = 0) then
        FreeAndNil(m_lstTransmittingManagers);

      Result := TRUE;
      exit;
    end;
  end;
end;


procedure TGamingManagerMI.Start;
begin
  if (Assigned(ChessBoard)) then
  begin
    Show;
    exit;
  end;

  if (not Connector.Open(FALSE)) then
    raise EGamingManagerMI.Create('ERROR: Cannot open connector!');

  RCreateChessBoardAndDialogs;
  RSetChessBoardToView;

  RSetPrivateSettings;

  RShowConnectingForm;
end;


procedure TGamingManagerMI.Stop;
begin
  Release;
end;


procedure TGamingManagerMI.ROnCreate;
begin
  RCreateAndPopulateExtBaseList;

  // Nicks initialization
  PlayerNick := Connector.OwnerNick;

  OpponentNick := Connector.ContactNick;
  OpponentId := IntToStr(Connector.ContactID);

  TLocalizer.Instance.AddSubscriber(self); // TODO: -> TManager.ROnCreate
  RLocalize;
end;


procedure TGamingManagerMI.ROnDestroy;

  procedure NRemoveFromGamings;
  var
    lstTmp: TList;
    i: integer;
    ATransmitter: TTransmittingManagerMI;
  begin
    if (Assigned(g_lstGamingManagers)) then
      g_lstGamingManagers.Remove(self);

    if (not Assigned(m_lstTransmittingManagers)) then
      exit;

    lstTmp := m_lstTransmittingManagers;
    m_lstTransmittingManagers := nil;

    for i := 0 to lstTmp.Count - 1 do
    begin
      ATransmitter := lstTmp[i];
      if (Assigned(ATransmitter)) then
      begin
        lstTmp[i] := nil;
        ATransmitter.Stop;
      end;
    end;

    lstTmp.Free;
  end;

begin // TGamingManagerMI.ROnDestroy
  NRemoveFromGamings;

  if (Assigned(Connector)) then
  begin
    Connector.Free;
    Connector := nil;
  end;

  inherited ROnDestroy;
end;


procedure TGamingManagerMI.ConnectorHandler(e: TConnectorEvent;
  d1: pointer = nil; d2: pointer = nil);
var
  iData: integer;
  strCmd: string;
  strLeft: string;
begin
  case e of
    ceError:
    begin
      Connector.Close;
      inherited ConnectorHandler(e, d1, d2);
    end;

    ceData:
    begin
      Assert(High(TStringDynArray(d1)) >= 0);
      iData := 0;
      repeat
        strLeft := TStringDynArray(d1)[iData];
        inc(iData);
        strCmd := IfThen((iData <= High(TStringDynArray(d1))), '*');

        RHandleConnectorDataCommand(strLeft);
      until (strCmd = '');

    end; // ceData

  else
    inherited ConnectorHandler(e, d1, d2);
  end; // case
end;


procedure TGamingManagerMI.RSetOpponentClientVersion(lwVersion: LongWord);
begin
  inherited RSetOpponentClientVersion(lwVersion);

  if (lwVersion >= 200901) then
    Connector.MultiSession := TRUE;
end;


procedure TGamingManagerMI.RSendData(const cmd: string);
begin
  Connector.SendData(cmd);
end;


procedure TGamingManagerMI.RSetConnectionOccured;
var
  iIndex: integer;
begin
  inherited RSetConnectionOccured;

  if (not Assigned(g_lstGamingManagers)) then
    g_lstGamingManagers := TList.Create;

  iIndex := g_lstGamingManagers.IndexOf(self);
  if (iIndex < 0) then
    g_lstGamingManagers.Add(self);
end;

////////////////////////////////////////////////////////////////////////////////
// TManagerMIFactory

constructor TManagerMIFactory.Create(Connector: TConnector);
begin
  inherited Create;
  m_Connector := Connector;
end;


destructor TManagerMIFactory.Destroy;
begin
  m_Dialogs.Free;
  inherited;
end;


procedure TManagerMIFactory.Start;
begin
{
  // TODO: remove testing
  GM := NGetNeededGamingManager;
  GM.FAddTransmitter(self);
}

  if (FCanStartTransmitting) then
  begin
    Dialogs.MessageDlg('You are currently playing some games. Do you want to start broadcasting?',
      mtCustom, [mbYes, mbNo], mfTransmitting);
    exit;
  end;

  FStartGaming;
end;


function TManagerMIFactory.FCanStartTransmitting: boolean;
var
  i: integer;
  GM: TGamingManagerMI;
begin
  Result := FALSE;

  if (not (Assigned(g_lstGamingManagers) and Assigned(m_Connector))) then
    exit;

  for i := 0 to g_lstGamingManagers.Count - 1 do
  begin
    GM := g_lstGamingManagers[i];
    if (Assigned(GM)) then
    begin
      Result := (m_Connector.ContactID <> GM.Connector.ContactID);
      if (Result) then
        exit;
    end;
  end;
end;


procedure TManagerMIFactory.FStartGaming;
begin
  m_Manager := TGamingManagerMI.Create(m_Connector);
  m_Connector.SetPlugin(m_Manager);
  m_Connector := nil;
  m_Manager.Start;
  m_Manager := nil; // non-ref counted

  Stop;
end;


procedure TManagerMIFactory.FStartTransmitting;
begin
  ShowMessage('TODO:');
  // TODO:
  Stop;
end;


procedure TManagerMIFactory.Stop;
begin
  m_Connector.Free;
  if (Assigned(m_Manager)) then
    m_Manager.Stop;
  Free;
end;


procedure TManagerMIFactory.ConnectorHandler(ce: TConnectorEvent;
  d1: pointer = nil; d2: pointer = nil);
begin
end;


function TManagerMIFactory.FGetDialogs: TDialogs;
begin
  if (not Assigned(m_Dialogs)) then
    m_Dialogs := TDialogs.Create(nil, FDialogsHandler);
  Result := m_Dialogs;
end;


procedure TManagerMIFactory.FDialogsHandler(modSender: TModalForm; msgDlgID: TModalFormID);
var
  modRes: TModalResult;
begin
  modRes := modSender.ModalResult;
  case msgDlgID of
    mfNone:
      ;
    mfTransmitting:
    begin
      if (modRes = mrYes) then
        FStartTransmitting
      else // mrNo
        FStartGaming;
    end;
  end; // case
end;

////////////////////////////////////////////////////////////////////////////////
// TNonRefInterfacedObject

function TNonRefInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;


function TNonRefInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;


function TNonRefInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////
// TTransmittingManagerMI

procedure TTransmittingManagerMI.Start;
begin
  if (not Connector.Open(FALSE)) then
    raise ETransmittingManagerMI.Create('ERROR: Cannot open connector!');
end;


procedure TTransmittingManagerMI.Stop;
begin
  Release;
end;


procedure TTransmittingManagerMI.ROnDestroy;

  procedure NRemoveFromTransmittings;
  var
    i: integer;
    GM: TGamingManagerMI;
  begin
    if (not Assigned(g_lstGamingManagers)) then
      exit;

    for i := 0 to g_lstGamingManagers.Count - 1 do
    begin
      GM := g_lstGamingManagers[i];
      if (Assigned(GM) and GM.FRemoveTransmitter(self)) then
        break;
    end;
  end;

begin // TTransmittingManagerMI.ROnDestroy
  NRemoveFromTransmittings;

  if (Assigned(Connector)) then
  begin
    Connector.Free;
    Connector := nil;
  end;

  inherited ROnDestroy;
end;

initialization

finalization
  FreeAndNil(g_lstGamingManagers);

end.
