unit IMEServer.Headers;

interface

type
  IViewEvents = interface
    procedure OnDelayChanged(dbNewValue: Double);
  end;

  TLogEntryType = (letNo, letWrongCommand);

  IView = interface
    procedure SetEvents(Value: IViewEvents);
    procedure SetPort(iValue: integer);
    procedure SetDelay(dbDelay: Double; dbMaxDelay: Double = 0.0);
    procedure AddToLog(const strData: string; LogEntryType: TLogEntryType);
    procedure RemoveClient(const strHandleName: string; iHandleID: integer);
    procedure AddClient(const strHandleName: string; iHandleID: integer);    
  end;

implementation

end.
