{*************************************}
{                                     }
{       QIP INFIUM SDK                }
{       Copyright(c) Ilham Z.         }
{       ilham@qip.ru                  }
{       http://www.qip.im             }
{                                     }
{*************************************}

unit u_plugin_info;

interface

uses Windows, u_plugin_msg, u_common;

const
  QIP_SDK_VER_MAJOR = 1;
  QIP_SDK_VER_MINOR = 3;

type
  {Plugin info}
  TPluginInfo = record
    DllHandle         : DWord;      //dll instance/handle will be updated by QIP after successful loading of plugin library
    DllPath           : PWideChar;  //this should be updated by plugin library after receiving PM_PLUGIN_LOAD_SUCCESS from QIP
    QipSdkVerMajor    : Word;       //major version of sdk for core compatibility check
    QipSdkVerMinor    : Word;       //minor version of sdk for core compatibility check
    PlugVerMajor      : Word;
    PlugVerMinor      : Word;
    PluginName        : PWideChar;
    PluginAuthor      : PWideChar;
  end;
  pPluginInfo = ^TPluginInfo;

  {QIP2Plugin instant message record}
  TQipMsgPlugin = record
    MsgType    : Byte;        //see below, MSG_TYPE_....
    MsgTime    : DWord;       //unix datetime, local time
    ProtoName  : PWideChar;
    SenderAcc  : PWideChar;
    SenderNick : PWideChar;
    RcvrAcc    : PWideChar;
    RcvrNick   : PWideChar;
    MsgText    : PWideChar;
    Blocked    : Boolean;     //received msg blocked by antispam
  end;
  pQipMsgPlugin = ^TQipMsgPlugin;

  {QIP gives to plugin this interface}
  IQIPPluginService = interface
    function  PluginOptions(DllHandle: LongInt): pPluginSpecific; stdcall;
    procedure OnPluginMessage(var PlugMsg: TPluginMessage); stdcall;
  end;
  pIQIPPluginService = ^IQIPPluginService;


  {Plugin gives to QIP this interface}
  IQIPPlugin = interface
    function  GetPluginInfo: pPluginInfo; stdcall;
    procedure OnQipMessage(var PlugMsg: TPluginMessage); stdcall;
  end;
  pIQIPPlugin = ^IQIPPlugin;

  
const
  {Messages qip <-> plugin}
  {All messages "plugin -> qip" have to be with actual PluginMsg.DllHandle parameter}
  {=== Plugin main messages =======}
  PM_PLUGIN_LOAD_SUCCESS    =  1; //qip -> plugin
  PM_PLUGIN_RUN             =  2; //qip -> plugin
  PM_PLUGIN_QUIT            =  3; //qip -> plugin
  PM_PLUGIN_ENABLE          =  4; //qip -> plugin
  PM_PLUGIN_DISABLE         =  5; //qip -> plugin
  PM_PLUGIN_OPTIONS         =  6; //qip -> plugin
  {=== Plugin specific messages ===}
  PM_PLUGIN_SPELL_CHECK     =  7; //qip -> plugin, WParam = PWideChar to checking word, LParam = MissSpelledColor (delphi TColor). Return LParam with own color if needed and Result = True if word misspelled.
  PM_PLUGIN_SPELL_POPUP     =  8; //qip -> plugin, WParam = PWideChar to misspelled word, LParam is PPoint where PopupMenu should be popuped (screen coordinates). Return Result = True to ignore qip default menu popup.
  PM_PLUGIN_SPELL_REPLACE   =  9; //plugin -> qip, WParam = PWideChar to right word which will replace old misspelled word. Qip will return Result = True if misspelled word was successfully replaced.
  PM_PLUGIN_XSTATUS_UPD     = 10; //plugin -> qip, WParam = custom status picture number (from 0 to 35 or more if new custom status pics added), LParam = PWideChar of Status text (max 20 chars), NParam = PWideChar of status description text (max 512 chars). If WParam = 0 then custom status picture will be removed.
  PM_PLUGIN_XSTATUS_GET     = 11; //plugin -> qip, core will return WParam = custom status picture number (from 0 to 35 or more if new custom status pics added), LParam = PWideChar of Status text (max 20 chars), NParam = PWideChar of status description text (max 512 chars). If WParam = 0 then custom status picture not set by user.
  PM_PLUGIN_XSTATUS_CHANGED = 12; //qip -> plugin, user manually changed custom status picture/text, WParam = custom status picture number (from 0 to 35 or more if new custom status pics added), LParam = PWideChar of Status text (max 20 chars), NParam = PWideChar of status description text (max 512 chars). If WParam = 0 then custom status picture was removed by user.
  PM_PLUGIN_SOUND_GET       = 13; //plugin -> qip, if core returned WParam = True then qip sound enabled else sound disabled.
  PM_PLUGIN_SOUND_SET       = 14; //plugin -> qip, if WParam = True then qip will enable sound else will disable.
  PM_PLUGIN_SOUND_CHANGED   = 15; //qip -> plugin, user manually switched sound On/Off. if WParam = True the sound enabled.
  PM_PLUGIN_MSG_RCVD        = 16; //qip -> plugin, WParam = pQipMsgPlugin, return result = True to allow core accept this msg else message will be IGNORED, CAREFUL here! If you will add to LParam pointer to own widestring then original msg text will be replaced by yours own text.
  PM_PLUGIN_MSG_SEND        = 17; //qip -> plugin, WParam = pQipMsgPlugin, return result = True to allow send this msg else user will not be able to send this message, CAREFUL here! If you will add to LParam pointer to own widestring then original msg text will be replaced by yours own text.
  PM_PLUGIN_SPELL_RECHECK   = 18; //plugin -> qip, rechecks spelling for all message editing boxes
  PM_PLUGIN_MSG_RCVD_NEW    = 19; //qip -> plugin, notifier, qip received new message and its still not read by user. WParam = PWideChar to accountname of sender, LParam = PWideChar to nickname of sender. Plugin will receive this message periodically (~400 msec) until user will open msg window and read this msg.
  PM_PLUGIN_MSG_RCVD_READ   = 20; //qip -> plugin, notifier, new received message has been read by user and qip core will stop notifing with PM_PLUGIN_MSG_RCVD_NEW message. WParam = PWideChar to accountname of sender, LParam = PWideChar to nickname of sender. Plugin will receive this message only once after message or event will be read by user.
  PM_PLUGIN_WRONG_SDK_VER   = 21; //qip -> plugin, qip sends this message if plugin sdk version higher than qip's sdk version, after this msg qip will send PM_PLUGIN_QUIT message.
  PM_PLUGIN_CAN_ADD_BTNS    = 22; //qip -> plugin, broadcasted to all plugins, core creates message window and plugin can add buttons to panel below avatars, this message will be sent always on message window creation or tabs changing. WParam is PWideChar of account name of msg tab or wnd, LParam is PWideChar of protocol name of account, NParam is dll handle of protocol(for spec plugin msg sending needs);
  PM_PLUGIN_ADD_BTN         = 23; //plugin -> qip, wParam is pAddBtnInfo, core will return Result = Uniquq Action ID, which plugin will receive on every click on this btn, if Result will be returned  = 0 then btn was not added;
  PM_PLUGIN_MSG_BTN_CLICK   = 24; //qip -> plugin, occurs when user clicked on msg button below avatar. WParam is Unique Action ID given by core on adding this btn, LParam is PWideChar of account name of msg tab or wnd, NParam is PWideChar of protocol name of account, Result is dll handle of protocol(for spec plugin msg sending needs);
  PM_PLUGIN_SPEC_SEND       = 25; //plugin -> qip, WParam is protocol dll handle, LParam is PWideChar of receiver account name, NParam is special msg text/data. if Result returned = True then special message was sent else failed to send.
  PM_PLUGIN_SPEC_RCVD       = 26; //qip -> plugin, broadcasted to all plugins, WParam is protocol dll handle, LParam is PWideChar of sender account name, NParam is special msg text/data, Result is protocol name. 
  //to be continued ...


  {QIP msg types}
  MSG_TYPE_UNK        = $00; //unknown msg type
  MSG_TYPE_TEXT       = $01;
  MSG_TYPE_CHAT       = $02;
  MSG_TYPE_FILE       = $03;
  MSG_TYPE_URL        = $04;
  MSG_TYPE_AUTH       = $05;
  MSG_TYPE_ADDED      = $06;
  MSG_TYPE_SERVER     = $07;
  MSG_TYPE_WEB        = $08;
  MSG_TYPE_CONTACTS   = $09;
  MSG_TYPE_AUTO       = $0A;
  MSG_TYPE_SERVICE    = $0B;
  MSG_TYPE_EMAIL      = $0C;
  MSG_TYPE_OFFMSG     = $0D;
  MSG_TYPE_AUTHREPLY  = $0E;



implementation

end.
