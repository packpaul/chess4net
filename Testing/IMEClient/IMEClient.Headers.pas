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
    function GetContactHandleID: integer;
  end;

implementation

end.
