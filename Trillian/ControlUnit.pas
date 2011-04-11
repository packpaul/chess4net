////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ControlUnit;

interface

uses
  plugin;

procedure PluginStart;
procedure PluginStop;
procedure StartFromAlias(vpMessage: PTtkMessage);

var
  PluginSend: TFNTtkPluginFunctionSend;

implementation

uses
  SysUtils, Classes,
  GlobalsLocalUnit, ManagerUnit, ConnectorUnit;

type
  TMenuAddThread = class(TThread)
    procedure Execute; override;
    class function Create: TMenuAddThread;
  end;

var
  gMenuAddThread: TMenuAddThread;
  bgPluginStopped: boolean;

function PluginMenuCallback(WindowID: Integer; SubWindow: PAnsiChar; Event: PAnsiChar;
                            Data: Pointer; UserData: Pointer): Integer; cdecl;
begin
  Result := 0;
  if Event = 'menu-destroy' then
    begin
      if Assigned(UserData) then
        Dispose(UserData);
      TMenuAddThread.Create;
    end
  else
  if Event = 'menu-select' then
    begin
      // Запуск плагина
      TManager.Create(PTtkContactListEntry(UserData)^);
    end;
end;


function CreatePluginMenu(const vContactlistEntry: TTtkContactListEntry;
                          var vpMyContactlistEntry: PTtkContactListEntry): PTtkMenuEntry;
var
  pMyMenu, pMenuEntry, pPrevMenuEntry: PTtkMenuEntry;
  pContactlistEntry: PTtkContactListEntry;
begin
  new(pContactlistEntry);
  pContactlistEntry^ := vContactlistEntry;

  new(pMenuEntry);
  pMyMenu := pMenuEntry;

  TrillianInitialize(pMenuEntry^);
  pMenuEntry._type := MENU_TEXT;
  pMenuEntry.text := PLUGIN_MENU_NAME;
  // TODO: присоединение иконки
  pMenuEntry.data := pContactlistEntry;
  pMenuEntry.callback := PluginMenuCallback;

  pPrevMenuEntry := pMenuEntry;

  new(pMenuEntry);
  TrillianInitialize(pMenuEntry^);
  pMenuEntry._type := MENU_SEPARATOR;
  pMenuEntry.callback := PluginMenuCallback;

  pPrevMenuEntry.next_menu := Pointer(pMenuEntry);

  vpMyContactlistEntry := pContactlistEntry;
  Result := pMyMenu;
end;


procedure CloneMenu(const vContactlistEntry: TTtkContactListEntry; var vpMyContactlistEntry: PTtkContactListEntry;
                    var vpMyMenuEntry: PTtkMenuEntry; vbAddTestMenu: boolean);
var
  pMenuEntry, pNextMenuEntry: PTtkMenuEntry;
  ppMyMenuEntry: ^PTtkMenuEntry;
begin
  vpMyMenuEntry := Pointer(vContactlistEntry.menu_entry);
  pMenuEntry := vpMyMenuEntry;
  while Assigned(pMenuEntry) do
    begin
      if (@pMenuEntry.callback = @PluginMenuCallback) then
        begin
          if vbAddTestMenu then
            exit
          else
            break;
        end;
      pMenuEntry := pMenuEntry.next_menu;
    end;

  if not (vbAddTestMenu or Assigned(pMenuEntry)) then
    exit;

  vpMyMenuEntry := nil;
  vpMyContactlistEntry := nil;
  ppMyMenuEntry := @vpMyMenuEntry;
   pMenuEntry := PTtkMenuEntry(vContactlistEntry.menu_entry);
  while Assigned(pMenuEntry) do
    begin
      pNextMenuEntry := pMenuEntry.next_menu;
      if (pMenuEntry.menu_id >= 0) and (vbAddTestMenu or (@pMenuEntry.callback <> @PluginMenuCallback)) then
        begin
          new(ppMyMenuEntry^);
          ppMyMenuEntry^^ := pMenuEntry^;
          TrillianInitialize(pMenuEntry^);
          pMenuEntry.struct_size := ppMyMenuEntry^.struct_size;
          pMenuEntry.num_copies := ppMyMenuEntry^.num_copies;
          pMenuEntry.next_menu := pNextMenuEntry;
          ppMyMenuEntry := @(ppMyMenuEntry^.next_menu);
        end;

      pMenuEntry := pNextMenuEntry;
    end;

  ppMyMenuEntry^ := nil;

  if vbAddTestMenu then
    begin
      pMenuEntry := CreatePluginMenu(vContactlistEntry, vpMyContactlistEntry);
      pNextMenuEntry := pMenuEntry;
      while Assigned(pNextMenuEntry.next_menu) do
        pNextMenuEntry := pNextMenuEntry.next_menu;
      pNextMenuEntry.next_menu := PTtkMenuEntry(vpMyMenuEntry);
      vpMyMenuEntry := pMenuEntry;
    end;
end;


procedure FreeLocalPluginMenu(var vpMenuEntry: PTtkMenuEntry);
var
  pNextMenuEntry: PTtkMenuEntry;
begin
  while Assigned(vpMenuEntry) do
    begin
      pNextMenuEntry := vpMenuEntry.next_menu;
      Dispose(vpMenuEntry);
      vpMenuEntry := pNextMenuEntry;
    end;
end;



function AddPluginMenuToContactsCallback(WindowID: Integer; SubWindow: PAnsiChar; Event: PAnsiChar;
  Data: Pointer; UserData: Pointer): Integer; cdecl;
var
  pContactlistEntry, pMyContactlistEntry: PTtkContactListEntry;
  pMyMenuEntry: PTtkMenuEntry;
begin
  Result := 0;
  if Event = 'enum_add' then
    begin
      pContactlistEntry := Data;
      if not Assigned(pContactlistEntry.uri) then
        exit;
      // добавлеие меню плагина в контакт для контактов, загруженных при старте (uri <> "")
      CloneMenu(pContactlistEntry^, pMyContactlistEntry, pMyMenuEntry, TRUE);
      if pContactlistEntry.menu_entry = Pointer(pMyMenuEntry) then
        exit;
      pContactlistEntry.menu_entry := Pointer(pMyMenuEntry);
      pContactlistEntry.data := pMyContactlistEntry;
      PluginSend(GUID, 'contactlistUpdate', pContactlistEntry);
      FreeLocalPluginMenu(pMyMenuEntry);
    end;
end;


procedure AddPluginMenuToContacts;
var
  contactListEnum: TTtkContactListEnum;
begin
  trillianInitialize(contactListEnum);

  contactListEnum.medium := 'all';
  contactListEnum.connection_id := -1;
  contactListEnum.callback := AddPluginMenuToContactsCallback;

  PluginSend(GUID, 'contactlistEnumerate', @contactListEnum);
end;


function RemovePluginMenuFromContactsCallback(WindowID: Integer; SubWindow: PAnsiChar; Event: PAnsiChar;
                                        Data: Pointer; UserData: Pointer): Integer; cdecl;
var
  contactlistEntry: TTtkContactListEntry;
  pMyContactlistEntry: PTtkContactListEntry;
  pMyMenuEntry: PTtkMenuEntry;
begin
  Result := 0;
  if Event = 'enum_add' then
    begin
      contactlistEntry := PTtkContactListEntry(Data)^;
      // удалить меню плагина из контакта
      CloneMenu(contactlistEntry, pMyContactlistEntry, pMyMenuEntry, FALSE);
      if contactlistEntry.menu_entry = Pointer(pMyMenuEntry) then
        exit;
      contactlistEntry.menu_entry := Pointer(pMyMenuEntry);
      PluginSend(GUID, 'contactlistUpdate', @contactlistEntry);
      FreeLocalPluginMenu(pMyMenuEntry);
    end;
end;


procedure RemovePluginMenuFromContacts;
var
  contactListEnum: TTtkContactListEnum;
begin
  trillianInitialize(contactListEnum);

  contactListEnum.medium := 'all';
  contactListEnum.connection_id := -1;
  contactListEnum.callback := RemovePluginMenuFromContactsCallback;

  PluginSend(GUID, 'contactlistEnumerate', @contactListEnum);
end;


procedure PluginStart;
begin
  gMenuAddThread := nil;
  bgPluginStopped := FALSE;

  if not EnableBroadcast then
    raise Exception.Create('ERROR: Broadcast setting fault. Plugin not started!');

  // добавление меню плагина в контакты
  TMenuAddThread.Create;
end;


procedure PluginStop;
begin
  // TODO: убить инстанцию главного объекта плагина
  bgPluginStopped := TRUE;
  while Assigned(gMenuAddThread) do
    Sleep(1);
  RemovePluginMenuFromContacts;

  DisableBroadcast;
end;


procedure StartFromAlias(vpMessage: PTtkMessage);
var
  contactListEntry: TTtkContactListEntry;
begin
  TrillianInitialize(contactListEntry);
  contactListEntry.real_name := vpMessage.name;
  contactListEntry.medium := vpMessage.medium;
  contactlistEntry.connection_id := vpMessage.connection_id;
  contactlistEntry.name := vpMessage.display_name;

  TManager.Create(contactlistEntry);
end;

{--------------------------- TMenuAddThread -------------------------------}

procedure TMenuAddThread.Execute;
begin
  Sleep(2000);
  if not bgPluginStopped then
    AddPluginMenuToContacts;
  gMenuAddThread := nil;
end;


class function TMenuAddThread.Create: TMenuAddThread;
begin
  if not (Assigned(gMenuAddThread) or bgPluginStopped) then
    begin
      gMenuAddThread := inherited Create(TRUE);
      gMenuAddThread.Priority := tpNormal;
      gMenuAddThread.FreeOnTerminate := TRUE;
      gMenuAddThread.Resume;
    end;

  Result := gMenuAddThread;
end;

end.

