////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

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
