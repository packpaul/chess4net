Unit plugin;
interface
const
  APIversion = 4;

  // whatlist
  PL_ROASTER          =1;
  PL_VISIBLELIST      =2;
  PL_INVISIBLELIST    =3;
  PL_TEMPVISIBLELIST  =4;
  PL_IGNORELIST       =5;
  PL_DB               =6;
  PL_NIL              =7;  // not in list
  PL_CHAT             =8;
  PL_PLUGINS          =9;

  // connection state
  PCS_DISCONNECTED     =1;
  PCS_CONNECTED        =2;
  PCS_CONNECTING       =3;

  // whatwindow
  PW_ROASTER           =1;
  PW_CHAT              =2;
  PW_PREFERENCES       =3;

  // status
  PS_ONLINE            =0;
  PS_OCCUPIED          =1;
  PS_DND               =2;
  PS_NA                =3;
  PS_AWAY              =4;
  PS_F4C               =5;  // free for chat
  PS_OFFLINE           =6;
  PS_UNKNOWN           =7;

  // visibility
  PV_INVISIBLE         =0;
  PV_PRIVACY           =1;
  PV_NORMAL            =2;
  PV_ALL               =3;

  // messages
  PM_GET     =1;    // asking data
  PM_DATA    =2;    // posting datas (reply)
  PM_EVENT   =3;    // event notification
  PM_ABORT   =4;    // abort event (reply)
  PM_CMD     =5;    // exec command
  PM_ACK     =6;    // ack to request (reply)
  PM_ERROR   =7;    // error (reply)

  // events
  PE_INITIALIZE          =01;
  PE_FINALIZE            =02;
  PE_PREFERENCES         =03;
  PE_CONNECTED           =04;
  PE_DISCONNECTED        =05;
  PE_MSG_GOT             =06;
  PE_MSG_SENT            =07;
  PE_CONTACTS_GOT        =08;
  PE_CONTACTS_SENT       =09;
  PE_URL_GOT             =10;
  PE_URL_SENT            =11;
  PE_ADDEDYOU_GOT        =12;
  PE_ADDEDYOU_SENT       =13;
  PE_AUTHREQ_GOT         =14;
  PE_AUTHREQ_SENT        =15;
  PE_AUTH_GOT            =16;
  PE_AUTH_SENT           =17;
  PE_AUTHDENIED_GOT      =18;
  PE_AUTHDENIED_SENT     =19;
  PE_GCARD_GOT           =20;
  PE_GCARD_SENT          =21;
  PE_AUTOMSG_GOT         =22;
  PE_AUTOMSG_SENT        =23;
  PE_AUTOMSG_REQ_GOT     =24;
  PE_AUTOMSG_REQ_SENT    =25;
  PE_EMAILEXP_GOT        =26;
  PE_EMAILEXP_SENT       =27;
  PE_LIST_ADD            =28;
  PE_LIST_REMOVE         =29;
  PE_STATUS_CHANGED      =30;
  PE_USERINFO_CHANGED    =31;
  PE_VISIBILITY_CHANGED  =32;
  PE_WEBPAGER_GOT        =33;
  PE_WEBPAGER_SENT       =34;
  PE_FROM_MIRABILIS      =35;
  PE_UPDATE_INFO         =36;

  // get
  PG_USER               =01;
  PG_CONTACTINFO        =02;
  PG_DISPLAYED_NAME     =03;
  PG_TIME               =04;
  PG_LIST               =05;
  PG_NOF_UINLISTS       =06;
  PG_UINLIST            =07;
  PG_AWAYTIME           =08;
  PG_ANDRQ_PATH         =09;
  PG_USER_PATH          =10;
  PG_ANDRQ_VER          =11;
  PG_ANDRQ_VER_STR      =12;
  PG_USERTIME           =13;
  PG_CONNECTIONSTATE    =14;
  PG_WINDOW             =15;
  PG_AUTOMSG            =16;
{ Shyr }
  PG_CHAT_UIN           =201;
  PG_PLUGININFO         =202;
{ / Shyr } 

  // acks
  PA_OK             =01;
                    
  // errors
  PERR_ERROR          =01;
  PERR_BAD_REQ        =02;
  PERR_NOUSER         =03;
  PERR_UNEXISTENT     =04;
  PERR_FAILED_FOR     =05;
  PERR_UNK_REQ        =06;
                      
  // commands         
  PC_SEND_MSG         =01;
  PC_SEND_CONTACTS    =02;
  PC_SEND_ADDEDYOU    =03;
  PC_LIST_ADD         =04;  
  PC_LIST_REMOVE      =05;
  PC_SET_STATUS       =06;
  PC_SET_VISIBILITY   =07;
  PC_QUIT             =08;
  PC_CONNECT          =09;
  PC_DISCONNECT       =10;
  PC_SET_AUTOMSG      =11;
  PC_SEND_AUTOMSG_REQ =12;
{ Shyr }
  PC_ADDBUTTON        =201;
  PC_MODIFY_BUTTON    =202;
  PC_DELBUTTON        =203;
  PC_ADDMESSAGE       =204;
  PC_UNLOAD           =205;
  PC_SEND_HIDDEN_MSG  =206;
{ / Shyr }   

type
  TpluginFun=function(data:pointer):pointer; stdcall;
  TpluginFunC=function(data:pointer):pointer; cdecl;

implementation
end.
