unit CallExec;

interface
uses
 plugin,
 pluginutil,
 Types,
 Graphics,
 Windows;

type
 TContactInfo=record
   UIN:integer;
   Status:byte;
   Invisible:boolean;
   DisplayedName, First, Last:string
 end;
 TPluginInfo=record
   Handle:Cardinal;
   ScreenName:string;
   FileName:string;
 end;  

var
  callback:TpluginFun;      // &RQ callback function
  outBuffer:string;

function str2comm(s:string):pointer;
function callStr(s:string):pointer;

{####################################}
//           --===  Commands  ===--
procedure RQ_SendMsg(uin, Flag:integer; msg:string);
procedure RQ_SendContacts(uin, Flag:integer; contacts:TIntegerDynArray);
procedure RQ_SendAddedYou(uin:integer);
function  RQ_AddToList(List:integer; uins:TIntegerDynArray):integer;
function  RQ_RemoveFromList(List:integer; uins:TIntegerDynArray):integer;
procedure RQ_SetStatus(Status:byte);
procedure RQ_SetVisibility(Visibility:byte);
procedure RQ_Quit;
procedure RQ_Connect;
procedure RQ_Disconnect;
procedure RQ_SetAutoMessage(AutoMessage:string);
procedure RQ_SendAutoMessageRequest(uin:integer);
// - 5.0
function  RQ_CreateChatButton(buttonProc:pointer; buttonIcon: TIcon; buttonHint:string):integer;
procedure RQ_DeleteChatButton(var buttonAddr:Integer);
procedure RQ_ChangeChatButton(buttonAddr:Integer; buttonIcon: TIcon; buttonHint:string);
// - 6.0
procedure RQ_UnloadPlugin(pluginName:string);
procedure RQ_AddMessage2History(uin:integer; msg:string);
procedure RQ_SendHiddenMsg(uin, Flag:integer; msg:string);

{####################################}
//           --===  Get  ===--
function  RQ_GetTime:Double;
function  RQ_GetList(List:integer):TIntegerDynArray;
function  RQ_GetContactInfo(uin:integer):TContactInfo;
function  RQ_GetDisplayedName(uin:integer):string;
function  RQ_GetAwayTime:double;
function  RQ_GetAndrqPath:string;
function  RQ_GetUserPath:string;
function  RQ_GetAndrqVersion:integer;
function  RQ_GetAndrqVersionAsString:string;
function  RQ_GetCurrentUser:integer;
function  RQ_GetUserTime:double;
function  RQ_GetConnectionState:integer;
function  RQ_GetWindow(window:integer; var wnd:HWND; var left, top, width, height:integer):integer;
function  RQ_GetAutoMessage:string;
function  RQ_GetChatUIN:integer;
// - 6.0
function  RQ_GetPluginInfo(handle:Cardinal):TPluginInfo;
{####################################}
procedure RQ__ParseInitString(data:Pointer; var callback:TpluginFun; var apiVersion:integer;
                              var andrqPath, userPath:string; var currentUIN:integer);
procedure RQ__ParseMsgGotString(data:pointer; var uin, flags:integer; var when:TDateTime;
                                var msg:string);
procedure RQ__ParseMsgSentString(data:pointer; var uin, flags:integer; var msg:string);
procedure RQ__ParseURLGotString(data:Pointer; var uin, flags:integer; when:TDateTime; URL, text:string);
procedure RQ__ParseAddedYouSentString(data:Pointer; var uin:integer);
procedure RQ__ParseAddedYouGotString(data:Pointer; var uin, flags:integer; when:TDateTime);
procedure RQ__ParseContactsSentString(data:Pointer; var uin, flags:integer; contacts:TIntegerDynArray);
procedure RQ__ParseContactsGotString(data:Pointer; var uin, flags:integer; when:TDateTime; contacts:TIntegerDynArray);
procedure RQ__ParseAuthSentString(data:Pointer; var uin:integer);
procedure RQ__ParseAuthRequestGotString(data:Pointer; var uin, flags:integer; when:TDateTime; text:string);
procedure RQ__ParseAuthDeniedSentString(data:Pointer; var uin:integer; text:string);
procedure RQ__ParseAutoMessageSentString(data:Pointer; var uin:integer; text:string);
procedure RQ__ParseAutoMessageGotString(data:Pointer; var uin:integer; text:string);
procedure RQ__ParseAutoMessageRequestSentString(data:Pointer; var uin:integer);
procedure RQ__ParseAutoMessageRequestGotString(data:Pointer; var uin:integer);
procedure RQ__ParseVisibilityChanged(data:Pointer; var contact:integer);
procedure RQ__ParseUserinfoChanged(data:Pointer; var uin:integer);
procedure RQ__ParseStatusChanged(data:Pointer; var uin:integer; newStatus, oldStatus:byte;
                                 newInvisibleState, oldInvisibleState:Boolean);
procedure RQ__ParseListAddString(data:Pointer; var list:byte; uins:TIntegerDynArray);
procedure RQ__ParseListRemoveString(data:Pointer; var list:byte; uins:TIntegerDynArray);
{####################################}

implementation

uses
 SysUtils;

// convert a string to a "plugin communication"
function str2comm(s:string):pointer;
begin
outBuffer:=_int(length(s))+s;
result:=@outBuffer[1];
end; // str2comm

// execute callback on a string instead of pointer
function callStr(s:string):pointer;
begin
 result:=callback(str2comm( s ))
end;

{##############################################################################}

procedure RQ_SendMsg(uin, Flag:integer; msg:string);
{     Flag:
  Single  =0
  Multi   =1   }
begin
 callStr(char(PM_CMD)+char(PC_SEND_MSG)+_int(uin)+_int(Flag)+_istring(msg));
end;

procedure RQ_SendContacts(uin, Flag:integer; contacts:TIntegerDynArray);
begin
 callStr(char(PM_CMD)+char(PC_SEND_CONTACTS)+_int(uin)+_int(Flag)+_intlist(contacts));
end;

procedure RQ_SendAddedYou(uin:integer);
begin
 callStr(char(PM_CMD)+char(PC_SEND_ADDEDYOU)+_int(uin));
end;

function  RQ_AddToList(List:integer; uins:TIntegerDynArray):integer;
begin
 callStr(char(PM_CMD)+char(PC_LIST_ADD)+char(List)+_intlist(uins));
end;

function  RQ_RemoveFromList(List:integer; uins:TIntegerDynArray):integer;
begin
 callStr(char(PM_CMD)+char(PC_LIST_REMOVE)+char(List)+_intlist(uins));
end;

procedure RQ_SetStatus(Status:byte);
{   PS_ONLINE, PS_OCCUPIED, PS_DND
    PS_NA, PS_AWAY, PS_F4C, PS_OFFLINE, PS_UNKNOWN   }
begin
 callStr(char(PM_CMD)+char(PC_SET_STATUS)+char(Status));
end;

procedure RQ_SetVisibility(Visibility:byte);
{   PV_ALL, PV_NORMAL, PV_PRIVACY, PV_INVISIBLE   }
begin
 callStr(char(PM_CMD)+char(PC_SET_VISIBILITY)+char(Visibility));
end;

procedure RQ_Quit;
begin
 callStr(char(PM_CMD)+char(PC_QUIT));
end;

procedure RQ_Connect;
begin
 callStr(char(PM_CMD)+char(PC_CONNECT));
end;

procedure RQ_Disconnect;
begin
 callStr(char(PM_CMD)+char(PC_DISCONNECT));
end;

procedure RQ_SetAutoMessage(AutoMessage:string);
begin
 callStr(char(PM_CMD)+char(PC_SET_AUTOMSG)+_istring(AutoMessage));
end;

procedure RQ_SendAutoMessageRequest(uin:integer);
begin
 callStr(char(PM_CMD)+char(PC_SEND_AUTOMSG_REQ)+_int(uin));
end;

{##############################################################################}

function  RQ_GetTime:Double;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_TIME));
 Result:=_double(data,5);
end;

function RQ_GetList(List:integer):TIntegerDynArray;
{
  PL_ROASTER, PL_VISIBLELIST, PL_INVISIBLELIST, PL_TEMPVISIBLELIST,
  PL_IGNORELIST, PL_DB, PL_NIL.
}
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_LIST)+char(List));
 Result:=_intlist_at(data,5);
end;

function RQ_GetContactInfo(uin:integer):TContactInfo;
var
 data:Pointer;
 tempCI:TContactInfo;
 a:array[0..50]of char;
 i:integer;
begin
 data:=CallStr(char(PM_GET)+char(PG_CONTACTINFO)+_int(uin));
{ for i:=0 to 50 do begin
  a[i]:=Char(_byte_at(data,i));
 end;}
 with tempCI do begin
  UIN:=_int_at(data,9);
  Status:=_byte_at(data,13);
  Invisible:=boolean(_byte_at(data,14));
  DisplayedName:=_istring_at(data,15);
  i:=15+4+Length(DisplayedName);
  First:=_istring_at(data,i);
  i:=i+4+Length(First);
  Last:=_istring_at(data,i);
 end;
// Result.UIN:=length(a);
 Result:=tempCI;
end;

function  RQ_GetDisplayedName(uin:integer):string;
var
 data:Pointer;
 i:string;
begin
 data:=CallStr(char(PM_GET)+char(PG_DISPLAYED_NAME)+_int(uin));
 i:=_istring_at(data,5);
 Result:=i;
end;

function  RQ_GetAwayTime:double;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_AWAYTIME));
 result:=_double(data, 5);
end;

function  RQ_GetAndrqPath:string;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_ANDRQ_PATH));
 result:=_istring_at(data, 5);
end;

function  RQ_GetUserPath:string;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_USER_PATH));
 result:=_istring_at(data, 5);
end;

function  RQ_GetAndrqVersion:integer;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_ANDRQ_VER));
 result:=_int_at(data, 5);
end;

function  RQ_GetAndrqVersionAsString:string;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_ANDRQ_VER_STR));
 result:=_istring_at(data, 5);
end;

function RQ_GetCurrentUser:integer;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_USER));
 Result:=_int_at(data,5)
end;

function  RQ_GetUserTime:double;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_USERTIME));
 Result:=_int_at(data,5)
end;

function RQ_GetConnectionState:integer;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_CONNECTIONSTATE ));
 Result:=_int_at(data, 5);
end;

function  RQ_GetWindow(window:integer; var wnd:HWND; var left, top, width, height:integer):integer;
{   PW_ROASTER, PW_CHAT, PW_PREFERENCES   }
var
 data:Pointer;
begin
 data:=callStr( char(PM_GET)+char(PG_WINDOW)+char(window));
 if _byte_at(data,4) = PM_DATA then begin
  wnd:=_int_at(data, 5);
  left:=_int_at(data, 9);
  top:=_int_at(data, 13);
  width:=_int_at(data, 17);
  height:=_int_at(data, 21);
  result:=1;
 end
 else
  result:=0;
end;

function  RQ_GetAutoMessage:string;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_AUTOMSG));
 Result:=_istring_at(data,5)
end;

function  RQ_GetChatUIN:integer;
var
 data:Pointer;
begin
 data:=CallStr(char(PM_GET)+char(PG_CHAT_UIN));
 if _byte_at(data, 4)=7 then
  Result:=0
 else
  Result:=_int_at(data, 5);
end;

function  RQ_GetPluginInfo(handle:Cardinal):TPluginInfo;
var
 data:Pointer;
 res:TPluginInfo;
begin//
 data:=CallStr(char(PM_GET)+char(PG_PLUGININFO)+_int(handle));
 with res do begin
  Handle:=_int_at(data, 9);
  ScreenName:=_istring_at(data, 13);
  FileName:=_istring_at(data, 13+4+length(ScreenName));
 end;
 Result:=res;
end;

function RQ_CreateChatButton(buttonProc:Pointer; buttonIcon: TIcon; buttonHint:string):integer;
var
 data:Pointer;
 res:integer;
begin//
 data:=callStr(char(PM_CMD)+char(PC_ADDBUTTON)+
              _int(integer(buttonProc))+_int(integer(buttonIcon.Handle))+_istring(buttonHint));
 res:=_int_at(data, 4);
 Result:=res;
end;

procedure RQ_DeleteChatButton(var buttonAddr:Integer);
begin//
 callStr(char(PM_CMD)+char(PC_DELBUTTON)+_int(integer(buttonAddr)));
 buttonAddr:=0;
end;

procedure RQ_ChangeChatButton(buttonAddr:Integer; buttonIcon: TIcon; buttonHint:string);
begin//
 callStr(char(PM_CMD)+char(PC_MODIFY_BUTTON)
                       +_int(integer(buttonAddr))+_int(integer(buttonIcon.Handle))+_istring(buttonHint));
end;

// - 6.0

procedure RQ_UnloadPlugin(pluginName:string);
begin
 callStr(char(PM_CMD)+char(PC_UNLOAD)+_istring(pluginName));
end;

procedure RQ_AddMessage2History(uin:integer; msg:string);
begin
 callStr(char(PM_CMD)+char(PC_ADDMESSAGE)+_int(uin)+_istring(msg));
end;

procedure RQ_SendHiddenMsg(uin, Flag:integer; msg:string);
{     Flag:
  Single  =0
  Multi   =1   }
begin
 callStr(char(PM_CMD)+char(PC_SEND_HIDDEN_MSG)+_int(uin)+_int(Flag)+_istring(msg));
end;

{++++++++++++++++++++++++++++++++++++}

procedure RQ__ParseInitString(data:Pointer; var callback:TpluginFun; var apiVersion:integer;
                              var andrqPath, userPath:string; var currentUIN:integer);

var
 i:integer;
begin
 callback:=_ptr_at(data,6);
 apiVersion:=_int_at(data, 10);
 andrqPath:=_istring_at(data, 14);
 i:=14+4+length(andrqPath);
 userPath:=_istring_at(data, i);
 i:=i+4+length(userPath);
 currentUIN:=_int_at(data, i);
end;

procedure RQ__ParseMsgGotString(data:pointer; var uin, flags:integer; var when:TDateTime;
                                var msg:string);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 when:=_double(data, 14);
 msg:=_istring_at(data, 22);
end;

procedure RQ__ParseMsgSentString(data:pointer; var uin, flags:integer; var msg:string);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 msg:=_istring_at(data, 14);
end;

procedure RQ__ParseURLGotString(data:Pointer; var uin, flags:integer; when:TDateTime; URL, text:string);
var
 i:integer;
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 when:=_double(data, 14);
 URL:=_istring_at(data, 22);
 i:=22+4+length(URL);
 text:=_istring_at(data, i);
end;

procedure RQ__ParseAddedYouSentString(data:Pointer; var uin:integer);
begin
 uin:=_int_at(data, 6);
end;

procedure RQ__ParseAddedYouGotString(data:Pointer; var uin, flags:integer; when:TDateTime);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 when:=_double(data, 14);
end;

procedure RQ__ParseContactsSentString(data:pointer; var uin, flags:integer; contacts:TIntegerDynArray);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 contacts:=_intlist_at(data, 14);
end;

procedure RQ__ParseContactsGotString(data:Pointer; var uin, flags:integer; when:TDateTime; contacts:TIntegerDynArray);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 when:=_double(data, 14);
 contacts:=_intlist_at(data, 22);
end;

procedure RQ__ParseAuthSentString(data:Pointer; var uin:integer);
begin
 uin:=_int_at(data, 6);
end;

procedure RQ__ParseAuthRequestGotString(data:Pointer; var uin, flags:integer; when:TDateTime; text:string);
begin
 uin:=_int_at(data, 6);
 flags:=_int_at(data, 10);
 when:=_double(data, 14);
 text:=_istring_at(data, 22);
end;

procedure RQ__ParseAuthDeniedSentString(data:Pointer; var uin:integer; text:string);
begin
 uin:=_int_at(data, 6);
 text:=_istring_at(data, 10);
end;

procedure RQ__ParseAutoMessageSentString(data:Pointer; var uin:integer; text:string);
begin
 uin:=_int_at(data, 6);
 text:=_istring_at(data, 10);
end;

procedure RQ__ParseAutoMessageGotString(data:Pointer; var uin:integer; text:string);
begin
 uin:=_int_at(data, 6);
 text:=_istring_at(data, 10);
end;

procedure RQ__ParseAutoMessageRequestSentString(data:Pointer; var uin:integer);
begin
 uin:=_int_at(data, 6);
end;

procedure RQ__ParseAutoMessageRequestGotString(data:Pointer; var uin:integer);
begin
 uin:=_int_at(data, 6);
end;

procedure RQ__ParseVisibilityChanged(data:Pointer; var contact:integer);
{   if contact = 0 - all contacts   }
begin
 contact:=_int_at(data, 6);
end;

procedure RQ__ParseUserinfoChanged(data:Pointer; var uin:integer);
begin
 uin:=_int_at(data, 6);
end;

procedure RQ__ParseStatusChanged(data:Pointer; var uin:integer; newStatus, oldStatus:byte;
                                 newInvisibleState, oldInvisibleState:Boolean);
begin
 uin:=_int_at(data, 6);
 newStatus:=_byte_at(data, 10);
 oldStatus:=_byte_at(data, 11);
 newInvisibleState:=boolean(_byte_at(data, 12));
 oldInvisibleState:=boolean(_byte_at(data, 13));
end;

procedure RQ__ParseListAddString(data:Pointer; var list:byte; uins:TIntegerDynArray);
begin
 list:=_byte_at(data, 6);
 uins:=_intlist_at(data, 7);
end;

procedure RQ__ParseListRemoveString(data:Pointer; var list:byte; uins:TIntegerDynArray);
begin
 list:=_byte_at(data, 6);
 uins:=_intlist_at(data, 7);
end;

end.
