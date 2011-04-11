////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IMEClient.Headers;

interface

type
  IViewEvents = interface
    procedure OnClose;
    procedure OnSend;
    procedure OnConnect;
    procedure OnDisconnect;
    procedure OnChangeHandleID(iNewHandleID: integer);
    procedure OnChangeHandleName(const strNewHandleName: string);
    procedure OnChangeSendText(const strNewSendText: string);
    procedure OnChangeContactHandleID(iNewContactHandleID: integer);
    procedure OnStartPlugin;
    procedure OnClearPluginData;
  end;

  IView = interface
    procedure SetEvents(Value: IViewEvents);
    procedure SetConnected(bConnected: boolean);
    procedure AddContact(iHandleID: integer; const strHandleName: string);
    procedure DeleteContact(iHandleID: integer; const strHandleName: string);
    procedure SetInMessage(iFromHandleID: integer; const strFromHandleName, strMsg: string);
    procedure SetServerMessage(const strData: string);
    procedure OutputError(const strError: string);
    procedure SetHandleID(iHandleID: integer);
    procedure SetHandleName(const strHandleName: string);
    procedure SetSendText(const strValue: string);
    procedure AddPluginData(iHandleID: integer; const strHandleName, strData: string; bReceived: boolean);
    procedure ClearPluginData;
  end;

implementation

end.
