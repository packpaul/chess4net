unit SkypeTS_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 26/01/2010 20:22:58 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\PP\MyProjects\Chess4Net\Current\bin\SkypeTS.exe (1)
// LIBID: {0B450383-2533-476F-B023-398C65839F2A}
// LCID: 0
// Helpfile: 
// HelpString: SkypeTS Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SkypeTSMajorVersion = 0;
  SkypeTSMinorVersion = 0;

  LIBID_SkypeTS: TGUID = '{0B450383-2533-476F-B023-398C65839F2A}';

  IID_ISkype: TGUID = '{8618CC27-13EF-4DC6-92C8-1EB7C6F11031}';
  DIID_ISkypeEvents: TGUID = '{E55935D9-34ED-4248-8A17-FE9C6B30F1E3}';
  CLASS_Skype: TGUID = '{6FA5F7DB-1A72-42BE-B66B-A38137470205}';
  IID_IApplication: TGUID = '{1A3FDED0-540E-49BA-885B-DB989FAA5013}';
  IID_IApplicationStreamCollection: TGUID = '{208F9D07-2B64-4741-B7FB-E5F21E09E8DD}';
  IID_IUserCollection: TGUID = '{BC509F57-3B67-4BA0-B0C4-636ECFEB6D44}';
  IID_IUser: TGUID = '{009E6253-AC63-4CC0-9B2C-C5FA3AB3BD05}';
  IID_IChatMessage: TGUID = '{B7AC5083-E161-4307-B49F-E35A4A80E2AD}';
  IID_IApplicationStream: TGUID = '{3453D1C9-CD9B-4704-9BBA-4B5FCFB1AA99}';
  IID_IClient: TGUID = '{A81A3BAF-66E6-46A5-B6E5-C568F1621853}';
  CLASS_ApplicationStreamCollection: TGUID = '{71AD8787-27D5-446F-A43B-4F9227925B1E}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TChatMessageStatus
type
  TChatMessageStatus = TOleEnum;
const
  cmsUnknown = $FFFFFFFF;
  cmsReceived = $00000002;

// Constants for enum TAttachmentStatus
type
  TAttachmentStatus = TOleEnum;
const
  apiAttachUnknown = $FFFFFFFF;
  apiAttachSuccess = $00000000;
  apiAttachAvailable = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISkype = interface;
  ISkypeDisp = dispinterface;
  ISkypeEvents = dispinterface;
  IApplication = interface;
  IApplicationDisp = dispinterface;
  IApplicationStreamCollection = interface;
  IApplicationStreamCollectionDisp = dispinterface;
  IUserCollection = interface;
  IUserCollectionDisp = dispinterface;
  IUser = interface;
  IUserDisp = dispinterface;
  IChatMessage = interface;
  IChatMessageDisp = dispinterface;
  IApplicationStream = interface;
  IApplicationStreamDisp = dispinterface;
  IClient = interface;
  IClientDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Skype = ISkype;


// *********************************************************************//
// Interface: ISkype
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8618CC27-13EF-4DC6-92C8-1EB7C6F11031}
// *********************************************************************//
  ISkype = interface(IDispatch)
    ['{8618CC27-13EF-4DC6-92C8-1EB7C6F11031}']
    procedure Attach(Protocol: Integer; Wait: WordBool); safecall;
    function Get_Application(const Name: WideString): IApplication; safecall;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; safecall;
    function Get_Client: IClient; safecall;
    function Get_CurrentUser: IUser; safecall;
    function Get_CurrentUserHandle: WideString; safecall;
    property Application[const Name: WideString]: IApplication read Get_Application;
    property Client: IClient read Get_Client;
    property CurrentUser: IUser read Get_CurrentUser;
    property CurrentUserHandle: WideString read Get_CurrentUserHandle;
  end;

// *********************************************************************//
// DispIntf:  ISkypeDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8618CC27-13EF-4DC6-92C8-1EB7C6F11031}
// *********************************************************************//
  ISkypeDisp = dispinterface
    ['{8618CC27-13EF-4DC6-92C8-1EB7C6F11031}']
    procedure Attach(Protocol: Integer; Wait: WordBool); dispid 21;
    property Application[const Name: WideString]: IApplication readonly dispid 47;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; dispid 23;
    property Client: IClient readonly dispid 58;
    property CurrentUser: IUser readonly dispid 10;
    property CurrentUserHandle: WideString readonly dispid 4;
  end;

// *********************************************************************//
// DispIntf:  ISkypeEvents
// Flags:     (4096) Dispatchable
// GUID:      {E55935D9-34ED-4248-8A17-FE9C6B30F1E3}
// *********************************************************************//
  ISkypeEvents = dispinterface
    ['{E55935D9-34ED-4248-8A17-FE9C6B30F1E3}']
    procedure MessageStatus(const pMessage: IChatMessage; Status: TChatMessageStatus); dispid 11;
    procedure AttachmentStatus(Status: TAttachmentStatus); dispid 4;
    procedure ApplicationDatagram(const pApp: IApplication; const pStream: IApplicationStream; 
                                  const Text: WideString); dispid 18;
    procedure ApplicationReceiving(const pApp: IApplication; 
                                   const pStreams: IApplicationStreamCollection); dispid 20;
  end;

// *********************************************************************//
// Interface: IApplication
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1A3FDED0-540E-49BA-885B-DB989FAA5013}
// *********************************************************************//
  IApplication = interface(IDispatch)
    ['{1A3FDED0-540E-49BA-885B-DB989FAA5013}']
    procedure Delete; safecall;
    procedure Create; safecall;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); safecall;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); safecall;
    function Get_Streams: IApplicationStreamCollection; safecall;
    function Get_ConnectableUsers: IUserCollection; safecall;
    function Get_ConnectingUsers: IUserCollection; safecall;
    function Get_Name: WideString; safecall;
    property Streams: IApplicationStreamCollection read Get_Streams;
    property ConnectableUsers: IUserCollection read Get_ConnectableUsers;
    property ConnectingUsers: IUserCollection read Get_ConnectingUsers;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  IApplicationDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1A3FDED0-540E-49BA-885B-DB989FAA5013}
// *********************************************************************//
  IApplicationDisp = dispinterface
    ['{1A3FDED0-540E-49BA-885B-DB989FAA5013}']
    procedure Delete; dispid 3;
    procedure Create; dispid 2;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); dispid 7;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); dispid 10;
    property Streams: IApplicationStreamCollection readonly dispid 4;
    property ConnectableUsers: IUserCollection readonly dispid 5;
    property ConnectingUsers: IUserCollection readonly dispid 6;
    property Name: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IApplicationStreamCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {208F9D07-2B64-4741-B7FB-E5F21E09E8DD}
// *********************************************************************//
  IApplicationStreamCollection = interface(IDispatch)
    ['{208F9D07-2B64-4741-B7FB-E5F21E09E8DD}']
    procedure Add(const pItem: IApplicationStream); safecall;
    function Get_Count: Integer; safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IApplicationStream; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IApplicationStream read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IApplicationStreamCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {208F9D07-2B64-4741-B7FB-E5F21E09E8DD}
// *********************************************************************//
  IApplicationStreamCollectionDisp = dispinterface
    ['{208F9D07-2B64-4741-B7FB-E5F21E09E8DD}']
    procedure Add(const pItem: IApplicationStream); dispid 2;
    property Count: Integer readonly dispid 1;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IApplicationStream readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IUserCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BC509F57-3B67-4BA0-B0C4-636ECFEB6D44}
// *********************************************************************//
  IUserCollection = interface(IDispatch)
    ['{BC509F57-3B67-4BA0-B0C4-636ECFEB6D44}']
    function Get_Count: Integer; safecall;
    function Get_Item(Param2: Integer): IUser; safecall;
    property Count: Integer read Get_Count;
    property Item[Param2: Integer]: IUser read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IUserCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BC509F57-3B67-4BA0-B0C4-636ECFEB6D44}
// *********************************************************************//
  IUserCollectionDisp = dispinterface
    ['{BC509F57-3B67-4BA0-B0C4-636ECFEB6D44}']
    property Count: Integer readonly dispid 1;
    property Item[Param2: Integer]: IUser readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IUser
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {009E6253-AC63-4CC0-9B2C-C5FA3AB3BD05}
// *********************************************************************//
  IUser = interface(IDispatch)
    ['{009E6253-AC63-4CC0-9B2C-C5FA3AB3BD05}']
    function Get_Handle: WideString; safecall;
    function Get_FullName: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    property Handle: WideString read Get_Handle;
    property FullName: WideString read Get_FullName;
    property DisplayName: WideString read Get_DisplayName;
  end;

// *********************************************************************//
// DispIntf:  IUserDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {009E6253-AC63-4CC0-9B2C-C5FA3AB3BD05}
// *********************************************************************//
  IUserDisp = dispinterface
    ['{009E6253-AC63-4CC0-9B2C-C5FA3AB3BD05}']
    property Handle: WideString readonly dispid 1;
    property FullName: WideString readonly dispid 2;
    property DisplayName: WideString readonly dispid 17;
  end;

// *********************************************************************//
// Interface: IChatMessage
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B7AC5083-E161-4307-B49F-E35A4A80E2AD}
// *********************************************************************//
  IChatMessage = interface(IDispatch)
    ['{B7AC5083-E161-4307-B49F-E35A4A80E2AD}']
    function Get_Sender: IUser; safecall;
    function Get_Body: WideString; safecall;
    property Sender: IUser read Get_Sender;
    property Body: WideString read Get_Body;
  end;

// *********************************************************************//
// DispIntf:  IChatMessageDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B7AC5083-E161-4307-B49F-E35A4A80E2AD}
// *********************************************************************//
  IChatMessageDisp = dispinterface
    ['{B7AC5083-E161-4307-B49F-E35A4A80E2AD}']
    property Sender: IUser readonly dispid 13;
    property Body: WideString readonly dispid 8;
  end;

// *********************************************************************//
// Interface: IApplicationStream
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3453D1C9-CD9B-4704-9BBA-4B5FCFB1AA99}
// *********************************************************************//
  IApplicationStream = interface(IDispatch)
    ['{3453D1C9-CD9B-4704-9BBA-4B5FCFB1AA99}']
    function Get_PartnerHandle: WideString; safecall;
    procedure SendDatagram(const Text: WideString); safecall;
    procedure Write(const Text: WideString); safecall;
    function Read: WideString; safecall;
    property PartnerHandle: WideString read Get_PartnerHandle;
  end;

// *********************************************************************//
// DispIntf:  IApplicationStreamDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3453D1C9-CD9B-4704-9BBA-4B5FCFB1AA99}
// *********************************************************************//
  IApplicationStreamDisp = dispinterface
    ['{3453D1C9-CD9B-4704-9BBA-4B5FCFB1AA99}']
    property PartnerHandle: WideString readonly dispid 8;
    procedure SendDatagram(const Text: WideString); dispid 5;
    procedure Write(const Text: WideString); dispid 4;
    function Read: WideString; dispid 3;
  end;

// *********************************************************************//
// Interface: IClient
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A81A3BAF-66E6-46A5-B6E5-C568F1621853}
// *********************************************************************//
  IClient = interface(IDispatch)
    ['{A81A3BAF-66E6-46A5-B6E5-C568F1621853}']
    function Get_IsRunning: WordBool; safecall;
    procedure Start(Minimized: WordBool; Nosplash: WordBool); safecall;
    property IsRunning: WordBool read Get_IsRunning;
  end;

// *********************************************************************//
// DispIntf:  IClientDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A81A3BAF-66E6-46A5-B6E5-C568F1621853}
// *********************************************************************//
  IClientDisp = dispinterface
    ['{A81A3BAF-66E6-46A5-B6E5-C568F1621853}']
    property IsRunning: WordBool readonly dispid 4;
    procedure Start(Minimized: WordBool; Nosplash: WordBool); dispid 1;
  end;

// *********************************************************************//
// The Class CoSkype provides a Create and CreateRemote method to          
// create instances of the default interface ISkype exposed by              
// the CoClass Skype. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSkype = class
    class function Create: ISkype;
    class function CreateRemote(const MachineName: string): ISkype;
  end;

// *********************************************************************//
// The Class CoApplicationStreamCollection provides a Create and CreateRemote method to          
// create instances of the default interface IApplicationStreamCollection exposed by              
// the CoClass ApplicationStreamCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoApplicationStreamCollection = class
    class function Create: IApplicationStreamCollection;
    class function CreateRemote(const MachineName: string): IApplicationStreamCollection;
  end;

  TSkypeMessageStatus = procedure(ASender: TObject; const pMessage: IChatMessage;
                                                    Status: TChatMessageStatus) of object;
  TSkypeAttachmentStatus = procedure(ASender: TObject; Status: TAttachmentStatus) of object;
  TSkypeApplicationDatagram = procedure(ASender: TObject; const pApp: IApplication;
                                                          const pStream: IApplicationStream;
                                                          const Text: WideString) of object;
  TApplicationReceiving = procedure(ASender: TObject; const pApp: IApplication;
                                                      const pStreams: IApplicationStreamCollection) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSkype
// Help String      : Skype testing class
// Default Interface: ISkype
// Def. Intf. DISP? : No
// Event   Interface: ISkypeEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSkypeProperties= class;
{$ENDIF}
  TSkype = class(TOleServer)
  private
    FOnMessageStatus: TSkypeMessageStatus;
    FOnAttachmentStatus: TSkypeAttachmentStatus;
    FOnApplicationDatagram: TSkypeApplicationDatagram;
    FOnApplicationReceiving: TApplicationReceiving;

    FIntf:        ISkype;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TSkypeProperties;
    function      GetServerProperties: TSkypeProperties;
{$ENDIF}
    function      GetDefaultInterface: ISkype;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Application(const Name: WideString): IApplication;
    function Get_Client: IClient;
    function Get_CurrentUser: IUser;
    function Get_CurrentUserHandle: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISkype);
    procedure Disconnect; override;
    procedure Attach(Protocol: Integer; Wait: WordBool);
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
    property DefaultInterface: ISkype read GetDefaultInterface;
    property Application[const Name: WideString]: IApplication read Get_Application;
    property Client: IClient read Get_Client;
    property CurrentUser: IUser read Get_CurrentUser;
    property CurrentUserHandle: WideString read Get_CurrentUserHandle;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSkypeProperties read GetServerProperties;
{$ENDIF}
    property OnMessageStatus: TSkypeMessageStatus read FOnMessageStatus write FOnMessageStatus;
    property OnAttachmentStatus: TSkypeAttachmentStatus read FOnAttachmentStatus write FOnAttachmentStatus;
    property OnApplicationDatagram: TSkypeApplicationDatagram read FOnApplicationDatagram write FOnApplicationDatagram;
    property OnApplicationReceiving: TApplicationReceiving read FOnApplicationReceiving write FOnApplicationReceiving;  
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSkype
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSkypeProperties = class(TPersistent)
  private
    FServer:    TSkype;
    function    GetDefaultInterface: ISkype;
    constructor Create(AServer: TSkype);
  protected
    function Get_Application(const Name: WideString): IApplication;
    function Get_Client: IClient;
    function Get_CurrentUser: IUser;
    function Get_CurrentUserHandle: WideString;
  public
    property DefaultInterface: ISkype read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoSkype.Create: ISkype;
begin
  Result := CreateComObject(CLASS_Skype) as ISkype;
end;

class function CoSkype.CreateRemote(const MachineName: string): ISkype;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Skype) as ISkype;
end;

class function CoApplicationStreamCollection.Create: IApplicationStreamCollection;
begin
  Result := CreateComObject(CLASS_ApplicationStreamCollection) as IApplicationStreamCollection;
end;

class function CoApplicationStreamCollection.CreateRemote(const MachineName: string): IApplicationStreamCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ApplicationStreamCollection) as IApplicationStreamCollection;
end;

procedure TSkype.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{6FA5F7DB-1A72-42BE-B66B-A38137470205}';
    IntfIID:   '{8618CC27-13EF-4DC6-92C8-1EB7C6F11031}';
    EventIID:  '{E55935D9-34ED-4248-8A17-FE9C6B30F1E3}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSkype.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ISkype;
  end;
end;

procedure TSkype.ConnectTo(svrIntf: ISkype);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TSkype.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TSkype.GetDefaultInterface: ISkype;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TSkype.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSkypeProperties.Create(Self);
{$ENDIF}
end;

destructor TSkype.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSkype.GetServerProperties: TSkypeProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TSkype.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    11: if Assigned(FOnMessageStatus) then
         FOnMessageStatus(Self,
                          IUnknown(TVarData(Params[0]).VPointer) as IChatMessage {const IChatMessage},
                          Params[1] {TChatMessageStatus});
    4: if Assigned(FOnAttachmentStatus) then
         FOnAttachmentStatus(Self, Params[0] {TAttachmentStatus});
    18: if Assigned(FOnApplicationDatagram) then
          FOnApplicationDatagram(Self,
                                IUnknown(TVarData(Params[0]).VPointer) as IApplication {const IApplication},
                                IUnknown(TVarData(Params[1]).VPointer) as IApplicationStream {const IApplicationStream},
                                Params[2] {const WideString});
    20: if Assigned(FOnApplicationReceiving) then
          FOnApplicationReceiving(Self,
          IUnknown(TVarData(Params[0]).VPointer) as IApplication {const IApplication},
          IUnknown(TVarData(Params[1]).VPointer) as IApplicationStreamCollection {const IApplicationStreamCollection});

  end; {case DispID}
end;

function TSkype.Get_Application(const Name: WideString): IApplication;
begin
    Result := DefaultInterface.Application[Name];
end;

function TSkype.Get_Client: IClient;
begin
    Result := DefaultInterface.Client;
end;

function TSkype.Get_CurrentUser: IUser;
begin
    Result := DefaultInterface.CurrentUser;
end;

function TSkype.Get_CurrentUserHandle: WideString;
begin
    Result := DefaultInterface.CurrentUserHandle;
end;

procedure TSkype.Attach(Protocol: Integer; Wait: WordBool);
begin
  DefaultInterface.Attach(Protocol, Wait);
end;

function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
begin
  Result := DefaultInterface.SendMessage(Username, Text);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSkypeProperties.Create(AServer: TSkype);
begin
  inherited Create;
  FServer := AServer;
end;

function TSkypeProperties.GetDefaultInterface: ISkype;
begin
  Result := FServer.DefaultInterface;
end;

function TSkypeProperties.Get_Application(const Name: WideString): IApplication;
begin
    Result := DefaultInterface.Application[Name];
end;

function TSkypeProperties.Get_Client: IClient;
begin
    Result := DefaultInterface.Client;
end;

function TSkypeProperties.Get_CurrentUser: IUser;
begin
    Result := DefaultInterface.CurrentUser;
end;

function TSkypeProperties.Get_CurrentUserHandle: WideString;
begin
    Result := DefaultInterface.CurrentUserHandle;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TSkype]);
end;

end.
