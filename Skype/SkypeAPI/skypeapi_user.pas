unit SkypeAPI_User;

interface

uses
  Classes,
  //
  SkypeAPI_Skype;

type
  TUser = class(TInterfacedObject, IUser)
  private
    m_wstrHandle: WideString;
    function GetHandle: WideString;
    function GetFullName: WideString;
    function GetDisplayName: WideString;
  public
    constructor Create(const wstrHandle: WideString);
    property Handle: WideString read GetHandle;
    property FullName: WideString read GetFullName;
    property DisplayName: WideString read GetDisplayName;
  end;


  TUserCollection = class(TInterfaceList, IUserCollection)
  private
    function GetCount: Integer;
    function GetItem(iIndex: Integer): IUser;
  public
    property Count: Integer read GetCount;
    property Item[iIndex: Integer]: IUser read GetItem; default;
  end;

implementation

uses
  SysUtils,
  //
  SkypeAPI_Command;

type
  TUserCommand = class(TCommand)
  private
    m_User: TUser;
  protected
    property User: TUser read m_User;
  public
    constructor Create(AUser: TUser);
  end;


  TUserDisplayNameCommand = class(TUserCommand)
  private
    m_wstrDisplayName: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    property DisplayName: WideString read m_wstrDisplayName;
  end;


  TUserFullNameCommand = class(TUserCommand)
  private
    m_wstrFullName: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    property FullName: WideString read m_wstrFullName;
  end;

const
  CMD_USER: WideString = 'USER';
  CMD_GET_USER: WideString = 'GET USER';
  CMD_DISPLAY_NAME: WideString = 'DISPLAYNAME';
  CMD_FULL_NAME: WideString = 'FULLNAME';

////////////////////////////////////////////////////////////////////////////////
// TUser

constructor TUser.Create(const wstrHandle: WideString);
begin
  inherited Create;
  m_wstrHandle := wstrHandle;
end;


function TUser.GetHandle: WideString;
begin
  Result := m_wstrHandle;
end;


function TUser.GetFullName: WideString;
var
  Command: TUserFullNameCommand;
begin
  Result := '';

  Command := TUserFullNameCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Result := Command.FullName;
  finally
    Command.Free;
  end;
end;


function TUser.GetDisplayName: WideString;
var
  Command: TUserDisplayNameCommand;
begin
  Result := '';

  Command := TUserDisplayNameCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Result := Command.DisplayName;
  finally
    Command.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TUserCollection

function TUserCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TUserCollection.GetItem(iIndex: Integer): IUser;
begin
  Result := IUser(inherited Items[iIndex - 1]);
end;

////////////////////////////////////////////////////////////////////////////////
// TUserCommand

constructor TUserCommand.Create(AUser: TUser);
begin
  inherited Create;
  m_User := AUser;
end;

////////////////////////////////////////////////////////////////////////////////
// TUserDisplayNameCommand

function TUserDisplayNameCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_USER + ' ' + User.Handle + ' ' + CMD_DISPLAY_NAME;
end;


function TUserDisplayNameCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrUserHandle: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  TUserDisplayNameCommand.RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_USER) then
    exit;

  wstrUserHandle := TUserDisplayNameCommand.RNextToken(wstrBody, wstrBody);
  if (wstrUserHandle <> User.Handle) then
    exit;

  TUserDisplayNameCommand.RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_DISPLAY_NAME) then
    exit;

  m_wstrDisplayName := Trim(wstrBody);

  Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TUserFullNameCommand

function TUserFullNameCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_USER + ' ' + User.Handle + ' ' + CMD_FULL_NAME;
end;


function TUserFullNameCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrUserHandle: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  TUserDisplayNameCommand.RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_USER) then
    exit;

  wstrUserHandle := TUserDisplayNameCommand.RNextToken(wstrBody, wstrBody);
  if (wstrUserHandle <> User.Handle) then
    exit;

  TUserDisplayNameCommand.RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_FULL_NAME) then
    exit;

  m_wstrFullName := Trim(wstrBody);

  Result := TRUE;
end;

end.