unit SkypeAPI_ChatMessage;

interface

uses
  SkypeAPI_Skype, SkypeAPI_Command;

type
  TChatMessage = class(TInterfacedObject, IChatMessage)
  private
    m_lwID: LongWord;
    m_ChatMessageStatus: TChatMessageStatus;
    function GetSender: IUser;
    function GetBody: WideString;
    property ChatMessageStatus: TChatMessageStatus read m_ChatMessageStatus;
  public
    constructor Create(lwID: LongWord; AChatMessageStatus: TChatMessageStatus);
    property Sender: IUser read GetSender;
    property Body: WideString read GetBody;
    property ID: LongWord read m_lwID;
  end;


  TChatMessageStatusListener = class(TListener)
  private
    m_lwParsedChatMessageID: LongWord;
    m_ParsedChatMessageStatus: TChatMessageStatus;
    m_ChatMessage: TChatMessage;
  protected
    function RParseCommand(const wstrCommand: WideString): boolean; override;
    function RProcessCommand(const wstrCommand: WideString): boolean; override;
    procedure RDoNotify; override;
  end;

implementation

uses
  SysUtils;

type
  TChatMessageCommand = class(TCommand)
  private
    m_ChatMessage: TChatMessage;
  protected
    property ChatMessage: TChatMessage read m_ChatMessage;
  public
    constructor Create(AChatMessage: TChatMessage);
  end;

  TChatMessageSenderCommand = class(TChatMessageCommand)
  private
    m_Sender: IUser;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    procedure GetSender(out Sender: IUser);
  end;

  TChatMessageBodyCommand = class(TChatMessageCommand)
  private
    m_wstrBody: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    property Body: WideString read m_wstrBody;
  end;

const
  CMD_CHATMESSAGE = 'CHATMESSAGE';
  CMD_STATUS = 'STATUS';
  CMD_SENDING = 'SENDING';
  CMD_SENT = 'SENT';
  CMD_RECEIVED = 'RECEIVED';
  CMD_GET_CHATMESSAGE = 'GET CHATMESSAGE';
  CMD_FROM_HANDLE = 'FROM_HANDLE';
  CMD_BODY = 'BODY';

////////////////////////////////////////////////////////////////////////////////
// TChatMessage

constructor TChatMessage.Create(lwID: LongWord; AChatMessageStatus: TChatMessageStatus);
begin
  inherited Create;
  m_lwID := lwID;
  m_ChatMessageStatus := AChatMessageStatus;
end;


function TChatMessage.GetSender: IUser;
var
  Command: TChatMessageSenderCommand;
begin
  Command := TChatMessageSenderCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Command.GetSender(Result);
  finally
    Command.Free;
  end;
end;


function TChatMessage.GetBody: WideString;
var
  Command: TChatMessageBodyCommand;
begin
  Command := TChatMessageBodyCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Result := Command.Body;
  finally
    Command.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessageStatusListener

function TChatMessageStatusListener.RProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := RParseCommand(wstrCommand);

  if (not Result) then
    exit;

  m_ChatMessage := TChatMessage.Create(m_lwParsedChatMessageID, m_ParsedChatMessageStatus);

  Result := TRUE;
end;


function TChatMessageStatusListener.RParseCommand(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
begin
  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);

  if (wstrHead <> CMD_CHATMESSAGE) then
    exit;

  m_lwParsedChatMessageID := StrToIntDef(RNextToken(wstrBody, wstrBody), 0);
  if (m_lwParsedChatMessageID = 0) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_STATUS) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead = CMD_SENDING) then
    m_ParsedChatMessageStatus := cmsSending
  else if (wstrHead = CMD_SENT) then
    m_ParsedChatMessageStatus := cmsSent
  else if (wstrHead = CMD_RECEIVED) then
    m_ParsedChatMessageStatus := cmsReceived
  else
    m_ParsedChatMessageStatus := cmsUnknown;

  Result := TRUE;
end;


procedure TChatMessageStatusListener.RDoNotify;
var
  ChatMessage: IChatMessage;
  ChatMessageStatus: TChatMessageStatus;
begin
  ChatMessageStatus := m_ChatMessage.ChatMessageStatus;

  ChatMessage := m_ChatMessage;
  m_ChatMessage := nil;

  TSkype.Instance.DoMessageStatus(ChatMessage, ChatMessageStatus);
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessageCommand

constructor TChatMessageCommand.Create(AChatMessage: TChatMessage);
begin
  inherited Create;
  m_ChatMessage := AChatMessage;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessageSenderCommand

function TChatMessageSenderCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_CHATMESSAGE + ' ' + IntToStr(ChatMessage.ID) + ' ' + CMD_FROM_HANDLE;
end;


function TChatMessageSenderCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  lwChatMessageID: LongWord;
  wstrSenderHandle: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);

  if (wstrHead <> CMD_CHATMESSAGE) then
    exit;

  lwChatMessageID := StrToIntDef(RNextToken(wstrBody, wstrBody), 0);
  if (lwChatMessageID <> ChatMessage.ID) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_FROM_HANDLE) then
    exit;

  wstrSenderHandle := RNextToken(wstrBody, wstrBody);
  if (wstrSenderHandle = '') then
    exit;

  m_Sender := TSkype.Instance.GetUserByHandle(wstrSenderHandle);

  Result := TRUE;
end;


procedure TChatMessageSenderCommand.GetSender(out Sender: IUser);
begin
  Sender := m_Sender;
  m_Sender := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessageBodyCommand

function TChatMessageBodyCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_CHATMESSAGE + ' ' + IntToStr(ChatMessage.ID) + ' ' + CMD_BODY;
end;


function TChatMessageBodyCommand.RProcessResponse(const wstrCommand: WideString): boolean;
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
  if (lwChatMessageID <> ChatMessage.ID) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_BODY) then
    exit;

  m_wstrBody := wstrBody;

  Result := TRUE;
end;

end.
