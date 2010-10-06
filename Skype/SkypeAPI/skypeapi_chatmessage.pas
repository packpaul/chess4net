unit SkypeAPI_ChatMessage;

interface

uses
  SkypeAPI_Skype, SkypeAPI_Command;

type
  TChatMessage = class(TInterfacedObject, IChatMessage)
  private
    function GetSender: IUser;
    function GetBody: WideString;
  public
    property Sender: IUser read GetSender;
    property Body: WideString read GetBody;
  end;


  TChatMessageReceivedListener = class(TListener)
  protected
    function RProcessCommand(const wstrCommand: WideString): boolean; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TChatMessage

function TChatMessage.GetSender: IUser;
begin
  Result := nil;
  // TODO:
end;


function TChatMessage.GetBody: WideString;
begin
  Result := '';
  // TODO:  
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessageReceivedListener

function TChatMessageReceivedListener.RProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := FALSE;
// TODO:  CHATMESSAGE 457097 STATUS RECEIVED
//  GET CHATMESSAGE 457097 FROM_HANDLE
end;

end.
