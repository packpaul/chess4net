////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeAPI_Chat;

interface

uses
  SkypeAPI_Skype, SkypeAPI_Object;

type
  TChat = class(TObjectInterfacedObject, IChat)
  private
    m_wstrName: WideString;
    m_strUserName: string;
    function Get_Name: WideString;
    procedure FCreateChat;
  public
    constructor Create(const strUserName: string);

    function SendMessage(const MessageText: WideString): IChatMessage;    

    property Name: WideString read Get_Name;
    property UserName: string read m_strUserName;
  end;

implementation

uses
  SysUtils,
  //
  SkypeAPI_Command, SkypeAPI_ChatMessage;

type
  TChatCommand = class(TCommand)
  private
    m_Chat: TChat;
  protected
    property Chat: TChat read m_Chat;
  public
    constructor Create(AChat: TChat);
  end;


  TChatCreateCommand = class(TChatCommand)
  private
    m_wstrChatName: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    property ChatName: WideString read m_wstrChatName;
  end;


  TChatChatMessage = class(TChatCommand)
  private
    m_ChatMessage: IChatMessage;
    m_wstrMessage: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(AChat: TChat; const wstrMessage: WideString);
    procedure GetChatMessage(out Result: IChatMessage);
  end;

const
  CMD_CHAT: WideString = 'CHAT';
  CMD_CHAT_CREATE: WideString = 'CHAT CREATE';
  CMD_DIALOG: WideString = 'DIALOG';

////////////////////////////////////////////////////////////////////////////////
// TChat

constructor TChat.Create(const strUserName: string);
begin
  inherited Create;
  m_strUserName := strUserName;
  FCreateChat;
end;


procedure TChat.FCreateChat;
var
  Command: TChatCreateCommand;
begin
  Command := TChatCreateCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      m_wstrName := Command.ChatName;
  finally
    Command.Free;
  end;
end;


function TChat.Get_Name: WideString;
begin
  Result := m_wstrName;
end;


function TChat.SendMessage(const MessageText: WideString): IChatMessage;
var
  Command: TChatChatMessage;
begin
  Result := nil;

  Command := TChatChatMessage.Create(self, MessageText);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Command.GetChatMessage(Result) 
  finally
    Command.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatCommand

constructor TChatCommand.Create(AChat: TChat);
begin
  inherited Create;
  m_Chat := AChat;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatCreateCommand

function TChatCreateCommand.RGetCommand: WideString;
begin
  Result := CMD_CHAT_CREATE + ' ' + Chat.UserName;
end;


function TChatCreateCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_CHAT) then
    exit;

  m_wstrChatName := RNextToken(wstrBody, wstrBody);

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_STATUS) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_DIALOG) then
    exit;

  Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatChatMessage

constructor TChatChatMessage.Create(AChat: TChat; const wstrMessage: WideString);
begin
  inherited Create(AChat);
  m_wstrMessage := wstrMessage;
end;


function TChatChatMessage.RGetCommand: WideString;
begin
  Result := CMD_CHATMESSAGE + ' ' + Chat.Name + ' ' +  m_wstrMessage;
end;


function TChatChatMessage.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  lwChatMessageID: LongWord;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_CHATMESSAGE) then
    exit;

  lwChatMessageID := StrToIntDef(RNextToken(wstrBody, wstrBody), 0);
  if (lwChatMessageID = 0) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_STATUS) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_SENDING) then
    exit;

  m_ChatMessage := TChatMessage.Create(lwChatMessageID, cmsSending);

  Result := TRUE;
end;


procedure TChatChatMessage.GetChatMessage(out Result: IChatMessage);
begin
  Result := m_ChatMessage;
  m_ChatMessage := nil;
end;

end.