unit IMEClient.PluginSurrogate;

interface

type
  TPluginSurrogate = class(TObject)
  public
    function SendData(const strData: string; iContactID: integer): boolean; virtual; abstract;
    procedure SendError; virtual; abstract;
    procedure StartPlugin(iContactID: integer); virtual; abstract;
  end;

implementation

end.
