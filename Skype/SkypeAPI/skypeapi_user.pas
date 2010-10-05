unit SkypeAPI_User;

interface

uses
  SkypeAPI_Skype;

type
  TUser = class(TInterfacedObject, IUser)
  private
    function GetHandle: WideString;
    function GetFullName: WideString;
    function GetDisplayName: WideString;
  public
    property Handle: WideString read GetHandle;
    property FullName: WideString read GetFullName;
    property DisplayName: WideString read GetDisplayName;
  end;


  TUserCollection = class(TInterfacedObject, IUserCollection)
  private
    function GetCount: Integer;
    function GetItem(iIndex: Integer): IUser;
  public
    property Count: Integer read GetCount;
    property Item[iIndex: Integer]: IUser read GetItem; default;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TUser

function TUser.GetHandle: WideString;
begin
  Result := '';
  // TODO:
end;


function TUser.GetFullName: WideString;
begin
  Result := '';
  // TODO:
end;


function TUser.GetDisplayName: WideString;
begin
  Result := '';
  // TODO:
end;

////////////////////////////////////////////////////////////////////////////////
// TUserCollection

function TUserCollection.GetCount: Integer;
begin
  Result := 0;
  // TODO:
end;

function TUserCollection.GetItem(iIndex: Integer): IUser;
begin
  Result := nil;
  // TODO:
end;

end.