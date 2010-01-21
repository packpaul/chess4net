unit SKYPE4COMLib_TLB;

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
// File generated on 18/01/2010 14:29:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\PROGRA~1\COMMON~1\Skype\SKYPE4~1.DLL (1)
// LIBID: {03282B5D-B38F-469D-849A-09B0A7F4881B}
// LCID: 0
// Helpfile: C:\PROGRA~1\COMMON~1\Skype\Skype4COM.chm
// HelpString: Skype4COM 1.0 Type Library.
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Member 'Property' of 'ISkype' changed to 'Property_'
//   Hint: Parameter 'Property' of ISkype.Profile changed to 'Property_'
//   Hint: Parameter 'Property' of ISkype.Profile changed to 'Property_'
//   Hint: Parameter 'Type' of ISkype.ClearCallHistory changed to 'Type_'
//   Hint: Parameter 'Type' of IConversion.VoicemailTypeToText changed to 'Type_'
//   Hint: Parameter 'Type' of IConversion.ChatMessageTypeToText changed to 'Type_'
//   Hint: Parameter 'Type' of IConversion.GroupTypeToText changed to 'Type_'
//   Hint: Parameter 'Type' of IConversion.SmsMessageTypeToText changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SKYPE4COMLibMajorVersion = 1;
  SKYPE4COMLibMinorVersion = 0;

  LIBID_SKYPE4COMLib: TGUID = '{03282B5D-B38F-469D-849A-09B0A7F4881B}';

  IID_IApplication: TGUID = '{F9ADA565-2FAD-424D-878C-7AD12DF9E617}';
  IID_IApplicationStreamCollection: TGUID = '{D8F1BCC4-2A73-47B9-83B3-D4D299CA5BC6}';
  IID_IApplicationStream: TGUID = '{E072F615-F833-4BCA-B473-CEE44D302030}';
  IID_IUserCollection: TGUID = '{EC163200-44EB-483B-907F-A8C1CF56B8EE}';
  IID_IUser: TGUID = '{D0BC5168-A518-4192-9D20-0B907B23C4D1}';
  IID_ICall: TGUID = '{76036886-436C-405F-A96E-7835CCFB82F3}';
  IID_IParticipantCollection: TGUID = '{F825FE12-9826-4BD2-BF7E-1ED95B8480FE}';
  IID_IParticipant: TGUID = '{F1B2AD09-1961-4336-A6BF-93010B73DE72}';
  IID_ICallChannel: TGUID = '{B76C04C9-0EB1-4748-B0C0-D01DEB595906}';
  IID_ICallChannelManager: TGUID = '{30C27C77-525F-4A3F-B9DD-C5A77C67250D}';
  IID_ISkype: TGUID = '{B1878BFE-53D3-402E-8C86-190B19AF70D5}';
  IID_IConversion: TGUID = '{8D82F88A-B307-4074-8ED5-11C3BD213452}';
  IID_ICallCollection: TGUID = '{72E1EC13-0DF9-48AE-8A31-E2900D85BA55}';
  IID_IChatMessageCollection: TGUID = '{9511117A-7BCA-4669-BE96-6EA6866975C1}';
  IID_IChatMessage: TGUID = '{4CFF5C70-3C95-4566-824A-FA164586D535}';
  IID_IChat: TGUID = '{8C24106C-3368-4580-93E5-5ED4ACCDE306}';
  IID_IChatMemberCollection: TGUID = '{8FFC686B-2E5E-4763-8B03-FF4FD3A0C4DA}';
  IID_IChatMember: TGUID = '{9E9F0473-94C2-4FDD-85D2-43E3478765F9}';
  IID_ICommand: TGUID = '{48E046A8-31D7-4E5F-A611-47BF32B86405}';
  IID_IChatCollection: TGUID = '{052A4165-010A-457D-A874-E661B6322846}';
  IID_IConference: TGUID = '{AEB48497-5090-479C-8BE0-BD51597156A1}';
  IID_IConferenceCollection: TGUID = '{F3E0C529-7D4F-4AF5-B501-27D25D4D2825}';
  IID_IVoicemailCollection: TGUID = '{21494848-BBD5-4192-BE32-57D1A19D8E94}';
  IID_IVoicemail: TGUID = '{120B425E-6313-4924-B5A9-3E9F2E444A00}';
  IID_IProfile: TGUID = '{B9A3D72F-04C1-4878-993C-D89F83E20569}';
  IID_IGroupCollection: TGUID = '{4C5C99DC-09CF-4A9C-BD94-8D655454A1F4}';
  IID_IGroup: TGUID = '{84513274-9C49-4AAA-B8FC-316EB32CFE95}';
  IID_ISettings: TGUID = '{2BC8C53B-3312-4A77-AC87-31DF18A1EC51}';
  IID_IClient: TGUID = '{838731B0-88E7-4BED-81DC-B35CA8433341}';
  IID_IPluginEvent: TGUID = '{4DF5F83A-0ABA-417D-A6FC-62A68AE06EF7}';
  IID_IPluginMenuItem: TGUID = '{C4BDF667-3FF7-4B44-A9F4-F3937E3E6D86}';
  IID_ISmsMessage: TGUID = '{82D97F2A-0E17-40F3-8E01-24937F936FF4}';
  IID_ISmsTargetCollection: TGUID = '{A2365EF3-4805-4DD3-A9D4-1A8AE3E17D84}';
  IID_ISmsTarget: TGUID = '{A9B9C33A-49A6-41D5-B13F-2AA4F284F406}';
  IID_ISmsChunkCollection: TGUID = '{BBDA7D2D-B1C2-4AF9-AB5B-D762341D8903}';
  IID_ISmsChunk: TGUID = '{A9062508-C926-4415-ABB7-A5A46DB34456}';
  IID_ISmsMessageCollection: TGUID = '{1D5BAB2E-69CC-4136-8E02-FC20767DC9E2}';
  IID_IFileTransferCollection: TGUID = '{70A59A25-E823-4C3F-8F33-775008895EE3}';
  IID_IFileTransfer: TGUID = '{4D36B368-B88C-45FA-B136-3EF77B2EAD39}';
  IID_ICallChannelCollection: TGUID = '{8CB09087-E389-4C6D-A6A2-7D4BCA8545D9}';
  IID_ICallChannelMessage: TGUID = '{0EF6FD5D-607D-4EA3-9C90-683D73449E9D}';
  IID_ISkypeApplication: TGUID = '{55A5200C-B2E8-4795-A6FA-858CA3FE2F26}';
  IID_ISkypeCall: TGUID = '{54590FC3-0405-4A2D-B4A5-BEAC026577F2}';
  IID_ISkypeChat: TGUID = '{3F6758D2-1D3C-4A8F-BD99-6FC6B0E2DC8F}';
  IID_ISkypeChatMessage: TGUID = '{A569B841-CC8A-4C12-B937-CBC17D9E64F0}';
  IID_ISkypeChatMember: TGUID = '{6CF6DBDE-AB7C-4635-96ED-2FF525AB4457}';
  IID_ISkypeConference: TGUID = '{30678F8A-C104-40C0-A6AE-7C150E83300E}';
  IID_ISkypeFileTransfer: TGUID = '{2B929860-6AF4-40DF-9D77-308CA7C23E66}';
  IID_ISkypeSms: TGUID = '{6D0B4547-771D-4C4F-B3E8-98A33FD24D2B}';
  IID_ISkypeUser: TGUID = '{C219279C-F557-4BAD-B3BE-750E91CA9944}';
  IID_ISkypeVoicemail: TGUID = '{4D33E14E-9921-4860-92F4-5DC1586F403C}';
  DIID__ISkypeEvents: TGUID = '{F4F90CDD-C620-4118-945E-CAA1BBEBA435}';
  DIID__ICallChannelManagerEvents: TGUID = '{497ABB45-20AE-49D1-A39D-CBE84A31B20C}';
  IID_ISkypePlugin: TGUID = '{B77A3D1F-6847-402F-BB82-A0564379088E}';
  CLASS_Skype: TGUID = '{830690FC-BF2F-47A6-AC2D-330BCB402664}';
  CLASS_User: TGUID = '{3E01D8E0-A72B-4C9F-99BD-8A6E7B97A48D}';
  CLASS_UserCollection: TGUID = '{7B030003-037D-490D-9169-A4F391B3D831}';
  CLASS_Conversion: TGUID = '{2EEAB6D0-491E-4962-BBA1-FF1CCA6D4DD0}';
  CLASS_Call: TGUID = '{D26B1D42-9C42-4E7B-BB73-86384C4B4345}';
  CLASS_CallCollection: TGUID = '{F3188CF3-EF22-4C5B-92CB-605964761C3B}';
  CLASS_Chat: TGUID = '{7ACDC5B4-76A1-4BDF-918D-6962FCABBAD3}';
  CLASS_ChatCollection: TGUID = '{15B6FEE5-5FB3-4071-AC1F-7AEDC0E2A6BB}';
  CLASS_Participant: TGUID = '{52071016-E648-4D3B-B57E-2B46CC993CE0}';
  CLASS_ParticipantCollection: TGUID = '{E1BC9147-C3E3-4E8A-8304-5E6B5C1C0774}';
  CLASS_Conference: TGUID = '{A983C9EC-D73E-4364-B89B-ACD1E405674F}';
  CLASS_ConferenceCollection: TGUID = '{3506CDB7-8BC6-40C0-B108-CEA0B9480130}';
  CLASS_Voicemail: TGUID = '{DD0E8ED5-1494-4B87-A35C-39F6ED4B1153}';
  CLASS_VoicemailCollection: TGUID = '{A7DF2611-D752-4C9F-A90A-B56F18485EE9}';
  CLASS_Application: TGUID = '{29DCD339-D184-469B-8BFB-199A2CCF014E}';
  CLASS_ApplicationStream: TGUID = '{6FA10A39-4760-4C94-A210-2398848618EC}';
  CLASS_ApplicationStreamCollection: TGUID = '{4B42750B-57A1-47E7-B340-8EAE0E3126A4}';
  CLASS_ChatMessage: TGUID = '{9017071A-2E34-4C3A-9BBB-688CBB5A9FF2}';
  CLASS_ChatMessageCollection: TGUID = '{10DD084E-A5AE-456F-A3BE-DA67EBE6B090}';
  CLASS_Profile: TGUID = '{452CCB69-6A95-4370-9E5A-B3EFB06A7651}';
  CLASS_Group: TGUID = '{222C0F35-3D78-4570-9F6D-BAEE289D0304}';
  CLASS_GroupCollection: TGUID = '{A8109DB9-88E0-42FE-98EA-8A12BE5394C6}';
  CLASS_Settings: TGUID = '{B0FE88F0-C92F-46D6-878F-31599BEA944C}';
  CLASS_Client: TGUID = '{B09AC3FF-0D5D-41C6-A34E-7C3F58A3127C}';
  CLASS_Command: TGUID = '{2DBCDA9F-1248-400B-A382-A56D71BF7B15}';
  CLASS_CallChannel: TGUID = '{89DD2F9D-C325-48BF-A615-96BD039BBC83}';
  CLASS_CallChannelCollection: TGUID = '{42FE718B-A148-41D6-885B-01A0AFAE8723}';
  CLASS_CallChannelManager: TGUID = '{5E541E71-A474-4EAD-8FCB-24D400D023B7}';
  CLASS_CallChannelMessage: TGUID = '{3D3E7C1B-79A7-4CC7-8925-41FA813E9913}';
  CLASS_IEProtocolHandler: TGUID = '{FFC8B962-9B40-4DFF-9458-1830C7DD7F5D}';
  CLASS_SmsMessage: TGUID = '{F278D870-7AF7-4957-96EE-E6AC72D0B109}';
  CLASS_SmsMessageCollection: TGUID = '{1BCA4635-F1FC-44C8-B829-48229AEB32E3}';
  CLASS_SmsChunk: TGUID = '{5792FC7D-5E1D-4F1A-BD4F-A7A50F92BC6E}';
  CLASS_SmsChunkCollection: TGUID = '{CC461FC3-C9BE-41FB-8E47-E0115CBC01CC}';
  CLASS_SmsTarget: TGUID = '{61F8FAF0-82D0-407C-AE97-31441483AE40}';
  CLASS_SmsTargetCollection: TGUID = '{6AC51E9C-7947-4B46-A978-0AD601C4EFC9}';
  CLASS_PluginMenuItem: TGUID = '{3F06DCD2-3A04-463D-A08B-1FFED02C4D4C}';
  CLASS_PluginEvent: TGUID = '{9D073235-D787-497D-8D1F-929559F1C621}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TUserSex
type
  TUserSex = TOleEnum;
const
  usexUnknown = $FFFFFFFF;
  usexMale = $00000000;
  usexFemale = $00000001;

// Constants for enum TBuddyStatus
type
  TBuddyStatus = TOleEnum;
const
  budUnknown = $FFFFFFFF;
  budNeverBeenFriend = $00000000;
  budDeletedFriend = $00000001;
  budPendingAuthorization = $00000002;
  budFriend = $00000003;

// Constants for enum TOnlineStatus
type
  TOnlineStatus = TOleEnum;
const
  olsUnknown = $FFFFFFFF;
  olsOffline = $00000000;
  olsOnline = $00000001;
  olsAway = $00000002;
  olsNotAvailable = $00000003;
  olsDoNotDisturb = $00000004;
  olsSkypeOut = $00000005;
  olsSkypeMe = $00000006;

// Constants for enum TCallType
type
  TCallType = TOleEnum;
const
  cltUnknown = $FFFFFFFF;
  cltIncomingPSTN = $00000000;
  cltOutgoingPSTN = $00000001;
  cltIncomingP2P = $00000002;
  cltOutgoingP2P = $00000003;

// Constants for enum TCallStatus
type
  TCallStatus = TOleEnum;
const
  clsUnknown = $FFFFFFFF;
  clsUnplaced = $00000000;
  clsRouting = $00000001;
  clsEarlyMedia = $00000002;
  clsFailed = $00000003;
  clsRinging = $00000004;
  clsInProgress = $00000005;
  clsOnHold = $00000006;
  clsFinished = $00000007;
  clsMissed = $00000008;
  clsRefused = $00000009;
  clsBusy = $0000000A;
  clsCancelled = $0000000B;
  clsLocalHold = $0000000C;
  clsRemoteHold = $0000000D;
  clsVoicemailBufferingGreeting = $0000000E;
  clsVoicemailPlayingGreeting = $0000000F;
  clsVoicemailRecording = $00000010;
  clsVoicemailUploading = $00000011;
  clsVoicemailSent = $00000012;
  clsVoicemailCancelled = $00000013;
  clsVoicemailFailed = $00000014;
  clsTransferring = $00000015;
  clsTransferred = $00000016;

// Constants for enum TCallFailureReason
type
  TCallFailureReason = TOleEnum;
const
  cfrUnknown = $FFFFFFFF;
  cfrMiscError = $00000000;
  cfrUserDoesNotExist = $00000001;
  cfrUserIsOffline = $00000002;
  cfrNoProxyFound = $00000003;
  cfrSessionTerminated = $00000004;
  cfrNoCommonCodec = $00000005;
  cfrSoundIOError = $00000006;
  cfrRemoteDeviceError = $00000007;
  cfrBlockedByRecipient = $00000008;
  cfrRecipientNotFriend = $00000009;
  cfrNotAuthorizedByRecipient = $0000000A;
  cfrSoundRecordingError = $0000000B;

// Constants for enum TCallVideoStatus
type
  TCallVideoStatus = TOleEnum;
const
  cvsUnknown = $FFFFFFFF;
  cvsNone = $00000000;
  cvsSendEnabled = $00000001;
  cvsReceiveEnabled = $00000002;
  cvsBothEnabled = $00000003;

// Constants for enum TCallVideoSendStatus
type
  TCallVideoSendStatus = TOleEnum;
const
  vssUnknown = $FFFFFFFF;
  vssNotAvailable = $00000000;
  vssAvailable = $00000001;
  vssStarting = $00000002;
  vssRejected = $00000003;
  vssRunning = $00000004;
  vssStopping = $00000005;
  vssPaused = $00000006;

// Constants for enum TCallIoDeviceType
type
  TCallIoDeviceType = TOleEnum;
const
  callIoDeviceTypeUnknown = $FFFFFFFF;
  callIoDeviceTypeSoundcard = $00000000;
  callIoDeviceTypePort = $00000001;
  callIoDeviceTypeFile = $00000002;

// Constants for enum TCallChannelType
type
  TCallChannelType = TOleEnum;
const
  cctUnknown = $FFFFFFFF;
  cctDatagram = $00000000;
  cctReliable = $00000001;

// Constants for enum TUserStatus
type
  TUserStatus = TOleEnum;
const
  cusUnknown = $FFFFFFFF;
  cusOffline = $00000000;
  cusOnline = $00000001;
  cusAway = $00000002;
  cusNotAvailable = $00000003;
  cusDoNotDisturb = $00000004;
  cusInvisible = $00000005;
  cusLoggedOut = $00000006;
  cusSkypeMe = $00000007;

// Constants for enum TConnectionStatus
type
  TConnectionStatus = TOleEnum;
const
  conUnknown = $FFFFFFFF;
  conOffline = $00000000;
  conConnecting = $00000001;
  conPausing = $00000002;
  conOnline = $00000003;

// Constants for enum TAttachmentStatus
type
  TAttachmentStatus = TOleEnum;
const
  apiAttachUnknown = $FFFFFFFF;
  apiAttachSuccess = $00000000;
  apiAttachPendingAuthorization = $00000001;
  apiAttachRefused = $00000002;
  apiAttachNotAvailable = $00000003;
  apiAttachAvailable = $00000004;

// Constants for enum TChatLeaveReason
type
  TChatLeaveReason = TOleEnum;
const
  leaUnknown = $FFFFFFFF;
  leaUserNotFound = $00000000;
  leaUserIncapable = $00000001;
  leaAdderNotFriend = $00000002;
  leaAddedNotAuthorized = $00000003;
  leaAddDeclined = $00000004;
  leaUnsubscribe = $00000005;

// Constants for enum TChatStatus
type
  TChatStatus = TOleEnum;
const
  chsUnknown = $FFFFFFFF;
  chsLegacyDialog = $00000000;
  chsDialog = $00000001;
  chsMultiNeedAccept = $00000002;
  chsMultiSubscribed = $00000003;
  chsUnsubscribed = $00000004;

// Constants for enum TVoicemailType
type
  TVoicemailType = TOleEnum;
const
  vmtUnknown = $FFFFFFFF;
  vmtIncoming = $00000000;
  vmtDefaultGreeting = $00000001;
  vmtCustomGreeting = $00000002;
  vmtOutgoing = $00000003;

// Constants for enum TVoicemailStatus
type
  TVoicemailStatus = TOleEnum;
const
  vmsUnknown = $FFFFFFFF;
  vmsNotDownloaded = $00000000;
  vmsDownloading = $00000001;
  vmsUnplayed = $00000002;
  vmsBuffering = $00000003;
  vmsPlaying = $00000004;
  vmsPlayed = $00000005;
  vmsBlank = $00000006;
  vmsRecording = $00000007;
  vmsRecorded = $00000008;
  vmsUploading = $00000009;
  vmsUploaded = $0000000A;
  vmsDeleting = $0000000B;
  vmsFailed = $0000000C;

// Constants for enum TVoicemailFailureReason
type
  TVoicemailFailureReason = TOleEnum;
const
  vmrUnknown = $FFFFFFFF;
  vmrNoError = $00000000;
  vmrMiscError = $00000001;
  vmrConnectError = $00000002;
  vmrNoPrivilege = $00000003;
  vmrNoVoicemail = $00000004;
  vmrFileReadError = $00000005;
  vmrFileWriteError = $00000006;
  vmrRecordingError = $00000007;
  vmrPlaybackError = $00000008;

// Constants for enum TChatMessageStatus
type
  TChatMessageStatus = TOleEnum;
const
  cmsUnknown = $FFFFFFFF;
  cmsSending = $00000000;
  cmsSent = $00000001;
  cmsReceived = $00000002;
  cmsRead = $00000003;

// Constants for enum TChatMessageType
type
  TChatMessageType = TOleEnum;
const
  cmeUnknown = $FFFFFFFF;
  cmeCreatedChatWith = $00000000;
  cmeSawMembers = $00000001;
  cmeAddedMembers = $00000002;
  cmeSetTopic = $00000003;
  cmeSaid = $00000004;
  cmeLeft = $00000005;
  cmeEmoted = $00000006;
  cmePostedContacts = $00000007;
  cmeGapInChat = $00000008;
  cmeSetRole = $00000009;
  cmeKicked = $0000000A;
  cmeSetOptions = $0000000B;
  cmeKickBanned = $0000000C;
  cmeJoinedAsApplicant = $0000000D;
  cmeSetPicture = $0000000E;
  cmeSetGuidelines = $0000000F;

// Constants for enum TGroupType
type
  TGroupType = TOleEnum;
const
  grpUnknown = $FFFFFFFF;
  grpCustomGroup = $00000000;
  grpAllUsers = $00000001;
  grpAllFriends = $00000002;
  grpSkypeFriends = $00000003;
  grpSkypeOutFriends = $00000004;
  grpOnlineFriends = $00000005;
  grpPendingAuthorizationFriends = $00000006;
  grpRecentlyContactedUsers = $00000007;
  grpUsersWaitingMyAuthorization = $00000008;
  grpUsersAuthorizedByMe = $00000009;
  grpUsersBlockedByMe = $0000000A;
  grpUngroupedFriends = $0000000B;
  grpSharedGroup = $0000000C;
  grpProposedSharedGroup = $0000000D;

// Constants for enum TSmsMessageStatus
type
  TSmsMessageStatus = TOleEnum;
const
  smsMessageStatusUnknown = $FFFFFFFF;
  smsMessageStatusReceived = $00000000;
  smsMessageStatusRead = $00000001;
  smsMessageStatusComposing = $00000002;
  smsMessageStatusSendingToServer = $00000003;
  smsMessageStatusSentToServer = $00000004;
  smsMessageStatusDelivered = $00000005;
  smsMessageStatusSomeTargetsFailed = $00000006;
  smsMessageStatusFailed = $00000007;

// Constants for enum TSmsMessageType
type
  TSmsMessageType = TOleEnum;
const
  smsMessageTypeUnknown = $FFFFFFFF;
  smsMessageTypeIncoming = $00000000;
  smsMessageTypeOutgoing = $00000001;
  smsMessageTypeCCRequest = $00000002;
  smsMessageTypeCCSubmit = $00000003;

// Constants for enum TSmsTargetStatus
type
  TSmsTargetStatus = TOleEnum;
const
  smsTargetStatusUnknown = $FFFFFFFF;
  smsTargetStatusUndefined = $00000000;
  smsTargetStatusAnalyzing = $00000001;
  smsTargetStatusAcceptable = $00000002;
  smsTargetStatusNotRoutable = $00000003;
  smsTargetStatusDeliveryPending = $00000004;
  smsTargetStatusDeliverySuccessful = $00000005;
  smsTargetStatusDeliveryFailed = $00000006;

// Constants for enum TChatMemberRole
type
  TChatMemberRole = TOleEnum;
const
  chatMemberRoleUnknown = $FFFFFFFF;
  chatMemberRoleCreator = $00000000;
  chatMemberRoleMaster = $00000001;
  chatMemberRoleHelper = $00000002;
  chatMemberRoleUser = $00000003;
  chatMemberRoleListener = $00000004;
  chatMemberRoleApplicant = $00000005;

// Constants for enum TChatType
type
  TChatType = TOleEnum;
const
  chatTypeUnknown = $FFFFFFFF;
  chatTypeDialog = $00000000;
  chatTypeLegacyDialog = $00000001;
  chatTypeLegacyUnsubscribed = $00000002;
  chatTypeMultiChat = $00000003;
  chatTypeSharedGroup = $00000004;

// Constants for enum TChatMyStatus
type
  TChatMyStatus = TOleEnum;
const
  chatStatusUnknown = $FFFFFFFF;
  chatStatusConnecting = $00000000;
  chatStatusWaitingRemoteAccept = $00000001;
  chatStatusAcceptRequired = $00000002;
  chatStatusPasswordRequired = $00000003;
  chatStatusSubscribed = $00000004;
  chatStatusUnsubscribed = $00000005;
  chatStatusDisbanded = $00000006;
  chatStatusQueuedBecauseChatIsFull = $00000007;
  chatStatusApplicationDenied = $00000008;
  chatStatusKicked = $00000009;
  chatStatusBanned = $0000000A;
  chatStatusRetryConnecting = $0000000B;

// Constants for enum TCallHistory
type
  TCallHistory = TOleEnum;
const
  chsAllCalls = $00000000;
  chsMissedCalls = $00000001;
  chsIncomingCalls = $00000002;
  chsOutgoingCalls = $00000003;

// Constants for enum TPluginContext
type
  TPluginContext = TOleEnum;
const
  pluginContextUnknown = $FFFFFFFF;
  pluginContextChat = $00000000;
  pluginContextCall = $00000001;
  pluginContextContact = $00000002;
  pluginContextMyself = $00000003;
  pluginContextTools = $00000004;

// Constants for enum TPluginContactType
type
  TPluginContactType = TOleEnum;
const
  pluginContactTypeUnknown = $FFFFFFFF;
  pluginContactTypeAll = $00000000;
  pluginContactTypeSkype = $00000001;
  pluginContactTypeSkypeOut = $00000002;

// Constants for enum TApiSecurityContext
type
  TApiSecurityContext = TOleEnum;
const
  apiContextUnknown = $00000000;
  apiContextVoice = $00000001;
  apiContextMessaging = $00000002;
  apiContextAccount = $00000004;
  apiContextContacts = $00000008;

// Constants for enum TSmsFailureReason
type
  TSmsFailureReason = TOleEnum;
const
  smsFailureReasonUnknown = $FFFFFFFF;
  smsFailureReasonMiscError = $00000000;
  smsFailureReasonServerConnectFailed = $00000001;
  smsFailureReasonNoSmsCapability = $00000002;
  smsFailureReasonInsufficientFunds = $00000003;
  smsFailureReasonInvalidConfirmationCode = $00000004;
  smsFailureReasonUserBlocked = $00000005;
  smsFailureReasonIPBlocked = $00000006;
  smsFailureReasonNodeBlocked = $00000007;

// Constants for enum TFileTransferType
type
  TFileTransferType = TOleEnum;
const
  fileTransferTypeIncoming = $00000000;
  fileTransferTypeOutgoing = $00000001;

// Constants for enum TFileTransferStatus
type
  TFileTransferStatus = TOleEnum;
const
  fileTransferStatusNew = $00000000;
  fileTransferStatusConnecting = $00000001;
  fileTransferStatusWaitingForAccept = $00000002;
  fileTransferStatusTransferring = $00000003;
  fileTransferStatusTransferringOverRelay = $00000004;
  fileTransferStatusPaused = $00000005;
  fileTransferStatusRemotelyPaused = $00000006;
  fileTransferStatusCancelled = $00000007;
  fileTransferStatusCompleted = $00000008;
  fileTransferStatusFailed = $00000009;

// Constants for enum TFileTransferFailureReason
type
  TFileTransferFailureReason = TOleEnum;
const
  fileTransferFailureReasonSenderNotAuthorized = $00000001;
  fileTransferFailureReasonRemotelyCancelled = $00000002;
  fileTransferFailureReasonFailedRead = $00000003;
  fileTransferFailureReasonFailedRemoteRead = $00000004;
  fileTransferFailureReasonFailedWrite = $00000005;
  fileTransferFailureReasonFailedRemoteWrite = $00000006;
  fileTransferFailureReasonRemoteDoesNotSupportFT = $00000007;
  fileTransferFailureReasonRemoteOfflineTooLong = $00000008;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IApplication = interface;
  IApplicationDisp = dispinterface;
  IApplicationStreamCollection = interface;
  IApplicationStreamCollectionDisp = dispinterface;
  IApplicationStream = interface;
  IApplicationStreamDisp = dispinterface;
  IUserCollection = interface;
  IUserCollectionDisp = dispinterface;
  IUser = interface;
  IUserDisp = dispinterface;
  ICall = interface;
  ICallDisp = dispinterface;
  IParticipantCollection = interface;
  IParticipantCollectionDisp = dispinterface;
  IParticipant = interface;
  IParticipantDisp = dispinterface;
  ICallChannel = interface;
  ICallChannelDisp = dispinterface;
  ICallChannelManager = interface;
  ICallChannelManagerDisp = dispinterface;
  ISkype = interface;
  ISkypeDisp = dispinterface;
  IConversion = interface;
  IConversionDisp = dispinterface;
  ICallCollection = interface;
  ICallCollectionDisp = dispinterface;
  IChatMessageCollection = interface;
  IChatMessageCollectionDisp = dispinterface;
  IChatMessage = interface;
  IChatMessageDisp = dispinterface;
  IChat = interface;
  IChatDisp = dispinterface;
  IChatMemberCollection = interface;
  IChatMemberCollectionDisp = dispinterface;
  IChatMember = interface;
  IChatMemberDisp = dispinterface;
  ICommand = interface;
  ICommandDisp = dispinterface;
  IChatCollection = interface;
  IChatCollectionDisp = dispinterface;
  IConference = interface;
  IConferenceDisp = dispinterface;
  IConferenceCollection = interface;
  IConferenceCollectionDisp = dispinterface;
  IVoicemailCollection = interface;
  IVoicemailCollectionDisp = dispinterface;
  IVoicemail = interface;
  IVoicemailDisp = dispinterface;
  IProfile = interface;
  IProfileDisp = dispinterface;
  IGroupCollection = interface;
  IGroupCollectionDisp = dispinterface;
  IGroup = interface;
  IGroupDisp = dispinterface;
  ISettings = interface;
  ISettingsDisp = dispinterface;
  IClient = interface;
  IClientDisp = dispinterface;
  IPluginEvent = interface;
  IPluginEventDisp = dispinterface;
  IPluginMenuItem = interface;
  IPluginMenuItemDisp = dispinterface;
  ISmsMessage = interface;
  ISmsMessageDisp = dispinterface;
  ISmsTargetCollection = interface;
  ISmsTargetCollectionDisp = dispinterface;
  ISmsTarget = interface;
  ISmsTargetDisp = dispinterface;
  ISmsChunkCollection = interface;
  ISmsChunkCollectionDisp = dispinterface;
  ISmsChunk = interface;
  ISmsChunkDisp = dispinterface;
  ISmsMessageCollection = interface;
  ISmsMessageCollectionDisp = dispinterface;
  IFileTransferCollection = interface;
  IFileTransferCollectionDisp = dispinterface;
  IFileTransfer = interface;
  IFileTransferDisp = dispinterface;
  ICallChannelCollection = interface;
  ICallChannelCollectionDisp = dispinterface;
  ICallChannelMessage = interface;
  ICallChannelMessageDisp = dispinterface;
  ISkypeApplication = interface;
  ISkypeApplicationDisp = dispinterface;
  ISkypeCall = interface;
  ISkypeCallDisp = dispinterface;
  ISkypeChat = interface;
  ISkypeChatDisp = dispinterface;
  ISkypeChatMessage = interface;
  ISkypeChatMessageDisp = dispinterface;
  ISkypeChatMember = interface;
  ISkypeChatMemberDisp = dispinterface;
  ISkypeConference = interface;
  ISkypeConferenceDisp = dispinterface;
  ISkypeFileTransfer = interface;
  ISkypeFileTransferDisp = dispinterface;
  ISkypeSms = interface;
  ISkypeSmsDisp = dispinterface;
  ISkypeUser = interface;
  ISkypeUserDisp = dispinterface;
  ISkypeVoicemail = interface;
  ISkypeVoicemailDisp = dispinterface;
  _ISkypeEvents = dispinterface;
  _ICallChannelManagerEvents = dispinterface;
  ISkypePlugin = interface;
  ISkypePluginDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Skype = ISkype;
  User = IUser;
  UserCollection = IUserCollection;
  Conversion = IConversion;
  Call = ICall;
  CallCollection = ICallCollection;
  Chat = IChat;
  ChatCollection = IChatCollection;
  Participant = IParticipant;
  ParticipantCollection = IParticipantCollection;
  Conference = IConference;
  ConferenceCollection = IConferenceCollection;
  Voicemail = IVoicemail;
  VoicemailCollection = IVoicemailCollection;
  Application = IApplication;
  ApplicationStream = IApplicationStream;
  ApplicationStreamCollection = IApplicationStreamCollection;
  ChatMessage = IChatMessage;
  ChatMessageCollection = IChatMessageCollection;
  Profile = IProfile;
  Group = IGroup;
  GroupCollection = IGroupCollection;
  Settings = ISettings;
  Client = IClient;
  Command = ICommand;
  CallChannel = ICallChannel;
  CallChannelCollection = ICallChannelCollection;
  CallChannelManager = ICallChannelManager;
  CallChannelMessage = ICallChannelMessage;
  IEProtocolHandler = IUnknown;
  SmsMessage = ISmsMessage;
  SmsMessageCollection = ISmsMessageCollection;
  SmsChunk = ISmsChunk;
  SmsChunkCollection = ISmsChunkCollection;
  SmsTarget = ISmsTarget;
  SmsTargetCollection = ISmsTargetCollection;
  PluginMenuItem = IPluginMenuItem;
  PluginEvent = IPluginEvent;


// *********************************************************************//
// Interface: IApplication
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F9ADA565-2FAD-424D-878C-7AD12DF9E617}
// *********************************************************************//
  IApplication = interface(IDispatch)
    ['{F9ADA565-2FAD-424D-878C-7AD12DF9E617}']
    function Get_Name: WideString; safecall;
    procedure Create; safecall;
    procedure Delete; safecall;
    function Get_Streams: IApplicationStreamCollection; safecall;
    function Get_ConnectableUsers: IUserCollection; safecall;
    function Get_ConnectingUsers: IUserCollection; safecall;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); safecall;
    function Get_SendingStreams: IApplicationStreamCollection; safecall;
    function Get_ReceivedStreams: IApplicationStreamCollection; safecall;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); safecall;
    property Name: WideString read Get_Name;
    property Streams: IApplicationStreamCollection read Get_Streams;
    property ConnectableUsers: IUserCollection read Get_ConnectableUsers;
    property ConnectingUsers: IUserCollection read Get_ConnectingUsers;
    property SendingStreams: IApplicationStreamCollection read Get_SendingStreams;
    property ReceivedStreams: IApplicationStreamCollection read Get_ReceivedStreams;
  end;

// *********************************************************************//
// DispIntf:  IApplicationDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F9ADA565-2FAD-424D-878C-7AD12DF9E617}
// *********************************************************************//
  IApplicationDisp = dispinterface
    ['{F9ADA565-2FAD-424D-878C-7AD12DF9E617}']
    property Name: WideString readonly dispid 1;
    procedure Create; dispid 2;
    procedure Delete; dispid 3;
    property Streams: IApplicationStreamCollection readonly dispid 4;
    property ConnectableUsers: IUserCollection readonly dispid 5;
    property ConnectingUsers: IUserCollection readonly dispid 6;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); dispid 7;
    property SendingStreams: IApplicationStreamCollection readonly dispid 8;
    property ReceivedStreams: IApplicationStreamCollection readonly dispid 9;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); dispid 10;
  end;

// *********************************************************************//
// Interface: IApplicationStreamCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D8F1BCC4-2A73-47B9-83B3-D4D299CA5BC6}
// *********************************************************************//
  IApplicationStreamCollection = interface(IDispatch)
    ['{D8F1BCC4-2A73-47B9-83B3-D4D299CA5BC6}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IApplicationStream); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IApplicationStream; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IApplicationStream read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IApplicationStreamCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D8F1BCC4-2A73-47B9-83B3-D4D299CA5BC6}
// *********************************************************************//
  IApplicationStreamCollectionDisp = dispinterface
    ['{D8F1BCC4-2A73-47B9-83B3-D4D299CA5BC6}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IApplicationStream); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IApplicationStream readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IApplicationStream
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E072F615-F833-4BCA-B473-CEE44D302030}
// *********************************************************************//
  IApplicationStream = interface(IDispatch)
    ['{E072F615-F833-4BCA-B473-CEE44D302030}']
    function Get_ApplicationName: WideString; safecall;
    function Get_Handle: WideString; safecall;
    function Read: WideString; safecall;
    procedure Write(const Text: WideString); safecall;
    procedure SendDatagram(const Text: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    procedure Disconnect; safecall;
    function Get_PartnerHandle: WideString; safecall;
    property ApplicationName: WideString read Get_ApplicationName;
    property Handle: WideString read Get_Handle;
    property DataLength: Integer read Get_DataLength;
    property PartnerHandle: WideString read Get_PartnerHandle;
  end;

// *********************************************************************//
// DispIntf:  IApplicationStreamDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E072F615-F833-4BCA-B473-CEE44D302030}
// *********************************************************************//
  IApplicationStreamDisp = dispinterface
    ['{E072F615-F833-4BCA-B473-CEE44D302030}']
    property ApplicationName: WideString readonly dispid 1;
    property Handle: WideString readonly dispid 2;
    function Read: WideString; dispid 3;
    procedure Write(const Text: WideString); dispid 4;
    procedure SendDatagram(const Text: WideString); dispid 5;
    property DataLength: Integer readonly dispid 6;
    procedure Disconnect; dispid 7;
    property PartnerHandle: WideString readonly dispid 8;
  end;

// *********************************************************************//
// Interface: IUserCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {EC163200-44EB-483B-907F-A8C1CF56B8EE}
// *********************************************************************//
  IUserCollection = interface(IDispatch)
    ['{EC163200-44EB-483B-907F-A8C1CF56B8EE}']
    function Get_Count: Integer; safecall;
    procedure Add(const pUser: IUser); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IUser; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IUser read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IUserCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {EC163200-44EB-483B-907F-A8C1CF56B8EE}
// *********************************************************************//
  IUserCollectionDisp = dispinterface
    ['{EC163200-44EB-483B-907F-A8C1CF56B8EE}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pUser: IUser); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IUser readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IUser
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D0BC5168-A518-4192-9D20-0B907B23C4D1}
// *********************************************************************//
  IUser = interface(IDispatch)
    ['{D0BC5168-A518-4192-9D20-0B907B23C4D1}']
    function Get_Handle: WideString; safecall;
    procedure Set_Handle(const pVal: WideString); safecall;
    function Get_FullName: WideString; safecall;
    function Get_Birthday: TDateTime; safecall;
    function Get_Sex: TUserSex; safecall;
    function Get_Country: WideString; safecall;
    function Get_Province: WideString; safecall;
    function Get_City: WideString; safecall;
    function Get_PhoneHome: WideString; safecall;
    function Get_PhoneOffice: WideString; safecall;
    function Get_PhoneMobile: WideString; safecall;
    function Get_Homepage: WideString; safecall;
    function Get_About: WideString; safecall;
    function Get_HasCallEquipment: WordBool; safecall;
    function Get_BuddyStatus: TBuddyStatus; safecall;
    procedure Set_BuddyStatus(pVal: TBuddyStatus); safecall;
    function Get_IsAuthorized: WordBool; safecall;
    procedure Set_IsAuthorized(pVal: WordBool); safecall;
    function Get_IsBlocked: WordBool; safecall;
    procedure Set_IsBlocked(pVal: WordBool); safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_OnlineStatus: TOnlineStatus; safecall;
    function Get_LastOnline: TDateTime; safecall;
    function Get_CountryCode: WideString; safecall;
    function Get_ReceivedAuthRequest: WideString; safecall;
    function Get_SpeedDial: WideString; safecall;
    procedure Set_SpeedDial(const pVal: WideString); safecall;
    function Get_CanLeaveVoicemail: WordBool; safecall;
    function Get_MoodText: WideString; safecall;
    function Get_Aliases: WideString; safecall;
    function Get_Timezone: Integer; safecall;
    function Get_IsCallForwardActive: WordBool; safecall;
    function Get_Language: WideString; safecall;
    function Get_LanguageCode: WideString; safecall;
    function Get_IsVideoCapable: WordBool; safecall;
    function Get_IsSkypeOutContact: WordBool; safecall;
    function Get_NumberOfAuthBuddies: Integer; safecall;
    procedure Set_DisplayName(const pVal: WideString); safecall;
    function Get_RichMoodText: WideString; safecall;
    function Get_IsVoicemailCapable: WordBool; safecall;
    property Handle: WideString read Get_Handle write Set_Handle;
    property FullName: WideString read Get_FullName;
    property Birthday: TDateTime read Get_Birthday;
    property Sex: TUserSex read Get_Sex;
    property Country: WideString read Get_Country;
    property Province: WideString read Get_Province;
    property City: WideString read Get_City;
    property PhoneHome: WideString read Get_PhoneHome;
    property PhoneOffice: WideString read Get_PhoneOffice;
    property PhoneMobile: WideString read Get_PhoneMobile;
    property Homepage: WideString read Get_Homepage;
    property About: WideString read Get_About;
    property HasCallEquipment: WordBool read Get_HasCallEquipment;
    property BuddyStatus: TBuddyStatus read Get_BuddyStatus write Set_BuddyStatus;
    property IsAuthorized: WordBool read Get_IsAuthorized write Set_IsAuthorized;
    property IsBlocked: WordBool read Get_IsBlocked write Set_IsBlocked;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property OnlineStatus: TOnlineStatus read Get_OnlineStatus;
    property LastOnline: TDateTime read Get_LastOnline;
    property CountryCode: WideString read Get_CountryCode;
    property ReceivedAuthRequest: WideString read Get_ReceivedAuthRequest;
    property SpeedDial: WideString read Get_SpeedDial write Set_SpeedDial;
    property CanLeaveVoicemail: WordBool read Get_CanLeaveVoicemail;
    property MoodText: WideString read Get_MoodText;
    property Aliases: WideString read Get_Aliases;
    property Timezone: Integer read Get_Timezone;
    property IsCallForwardActive: WordBool read Get_IsCallForwardActive;
    property Language: WideString read Get_Language;
    property LanguageCode: WideString read Get_LanguageCode;
    property IsVideoCapable: WordBool read Get_IsVideoCapable;
    property IsSkypeOutContact: WordBool read Get_IsSkypeOutContact;
    property NumberOfAuthBuddies: Integer read Get_NumberOfAuthBuddies;
    property RichMoodText: WideString read Get_RichMoodText;
    property IsVoicemailCapable: WordBool read Get_IsVoicemailCapable;
  end;

// *********************************************************************//
// DispIntf:  IUserDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D0BC5168-A518-4192-9D20-0B907B23C4D1}
// *********************************************************************//
  IUserDisp = dispinterface
    ['{D0BC5168-A518-4192-9D20-0B907B23C4D1}']
    property Handle: WideString dispid 1;
    property FullName: WideString readonly dispid 2;
    property Birthday: TDateTime readonly dispid 3;
    property Sex: TUserSex readonly dispid 4;
    property Country: WideString readonly dispid 5;
    property Province: WideString readonly dispid 6;
    property City: WideString readonly dispid 7;
    property PhoneHome: WideString readonly dispid 8;
    property PhoneOffice: WideString readonly dispid 9;
    property PhoneMobile: WideString readonly dispid 10;
    property Homepage: WideString readonly dispid 11;
    property About: WideString readonly dispid 12;
    property HasCallEquipment: WordBool readonly dispid 13;
    property BuddyStatus: TBuddyStatus dispid 14;
    property IsAuthorized: WordBool dispid 15;
    property IsBlocked: WordBool dispid 16;
    property DisplayName: WideString dispid 17;
    property OnlineStatus: TOnlineStatus readonly dispid 18;
    property LastOnline: TDateTime readonly dispid 19;
    property CountryCode: WideString readonly dispid 20;
    property ReceivedAuthRequest: WideString readonly dispid 21;
    property SpeedDial: WideString dispid 22;
    property CanLeaveVoicemail: WordBool readonly dispid 23;
    property MoodText: WideString readonly dispid 24;
    property Aliases: WideString readonly dispid 25;
    property Timezone: Integer readonly dispid 26;
    property IsCallForwardActive: WordBool readonly dispid 27;
    property Language: WideString readonly dispid 28;
    property LanguageCode: WideString readonly dispid 29;
    property IsVideoCapable: WordBool readonly dispid 30;
    property IsSkypeOutContact: WordBool readonly dispid 31;
    property NumberOfAuthBuddies: Integer readonly dispid 32;
    property RichMoodText: WideString readonly dispid 35;
    property IsVoicemailCapable: WordBool readonly dispid 36;
  end;

// *********************************************************************//
// Interface: ICall
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76036886-436C-405F-A96E-7835CCFB82F3}
// *********************************************************************//
  ICall = interface(IDispatch)
    ['{76036886-436C-405F-A96E-7835CCFB82F3}']
    function Get_Id: Integer; safecall;
    function Get_Timestamp: TDateTime; safecall;
    function Get_PartnerHandle: WideString; safecall;
    function Get_PartnerDisplayName: WideString; safecall;
    function Get_ConferenceId: Integer; safecall;
    function Get_type_: TCallType; safecall;
    function Get_Status: TCallStatus; safecall;
    procedure Set_Status(pVal: TCallStatus); safecall;
    function Get_FailureReason: TCallFailureReason; safecall;
    function Get_Subject: WideString; safecall;
    function Get_PstnNumber: WideString; safecall;
    function Get_Duration: Integer; safecall;
    function Get_PstnStatus: WideString; safecall;
    procedure Set_Seen(pVal: WordBool); safecall;
    procedure Hold; safecall;
    procedure Resume; safecall;
    procedure Finish; safecall;
    procedure Answer; safecall;
    procedure Set_DTMF(const Param1: WideString); safecall;
    function Get_Participants: IParticipantCollection; safecall;
    procedure Join(Id: Integer); safecall;
    function Get_VmDuration: Integer; safecall;
    function Get_VmAllowedDuration: Integer; safecall;
    function Get_VideoStatus: TCallVideoStatus; safecall;
    function Get_VideoSendStatus: TCallVideoSendStatus; safecall;
    function Get_VideoReceiveStatus: TCallVideoSendStatus; safecall;
    procedure StartVideoSend; safecall;
    procedure StopVideoSend; safecall;
    procedure StartVideoReceive; safecall;
    procedure StopVideoReceive; safecall;
    procedure RedirectToVoicemail; safecall;
    procedure Forward; safecall;
    function Get_Rate: Integer; safecall;
    function Get_RateCurrency: WideString; safecall;
    function Get_RatePrecision: Integer; safecall;
    function Get_InputDevice(DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_InputDevice(DeviceType: TCallIoDeviceType; const pVal: WideString); safecall;
    function Get_OutputDevice(DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_OutputDevice(DeviceType: TCallIoDeviceType; const pVal: WideString); safecall;
    function Get_CaptureMicDevice(DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_CaptureMicDevice(DeviceType: TCallIoDeviceType; const pVal: WideString); safecall;
    function Get_InputStatus: WordBool; safecall;
    function Get_ForwardedBy: WideString; safecall;
    function Get_Seen: WordBool; safecall;
    function Get_CanTransfer(const Target: WideString): WordBool; safecall;
    function Get_TransferStatus: TCallStatus; safecall;
    function Get_TransferActive: WordBool; safecall;
    function Get_TransferredBy: WideString; safecall;
    function Get_TransferredTo: WideString; safecall;
    procedure Transfer(const Target: WideString); safecall;
    function Get_TargetIdentity: WideString; safecall;
    property Id: Integer read Get_Id;
    property Timestamp: TDateTime read Get_Timestamp;
    property PartnerHandle: WideString read Get_PartnerHandle;
    property PartnerDisplayName: WideString read Get_PartnerDisplayName;
    property ConferenceId: Integer read Get_ConferenceId;
    property type_: TCallType read Get_type_;
    property Status: TCallStatus read Get_Status write Set_Status;
    property FailureReason: TCallFailureReason read Get_FailureReason;
    property Subject: WideString read Get_Subject;
    property PstnNumber: WideString read Get_PstnNumber;
    property Duration: Integer read Get_Duration;
    property PstnStatus: WideString read Get_PstnStatus;
    property Seen: WordBool read Get_Seen write Set_Seen;
    property DTMF: WideString write Set_DTMF;
    property Participants: IParticipantCollection read Get_Participants;
    property VmDuration: Integer read Get_VmDuration;
    property VmAllowedDuration: Integer read Get_VmAllowedDuration;
    property VideoStatus: TCallVideoStatus read Get_VideoStatus;
    property VideoSendStatus: TCallVideoSendStatus read Get_VideoSendStatus;
    property VideoReceiveStatus: TCallVideoSendStatus read Get_VideoReceiveStatus;
    property Rate: Integer read Get_Rate;
    property RateCurrency: WideString read Get_RateCurrency;
    property RatePrecision: Integer read Get_RatePrecision;
    property InputDevice[DeviceType: TCallIoDeviceType]: WideString read Get_InputDevice write Set_InputDevice;
    property OutputDevice[DeviceType: TCallIoDeviceType]: WideString read Get_OutputDevice write Set_OutputDevice;
    property CaptureMicDevice[DeviceType: TCallIoDeviceType]: WideString read Get_CaptureMicDevice write Set_CaptureMicDevice;
    property InputStatus: WordBool read Get_InputStatus;
    property ForwardedBy: WideString read Get_ForwardedBy;
    property CanTransfer[const Target: WideString]: WordBool read Get_CanTransfer;
    property TransferStatus: TCallStatus read Get_TransferStatus;
    property TransferActive: WordBool read Get_TransferActive;
    property TransferredBy: WideString read Get_TransferredBy;
    property TransferredTo: WideString read Get_TransferredTo;
    property TargetIdentity: WideString read Get_TargetIdentity;
  end;

// *********************************************************************//
// DispIntf:  ICallDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76036886-436C-405F-A96E-7835CCFB82F3}
// *********************************************************************//
  ICallDisp = dispinterface
    ['{76036886-436C-405F-A96E-7835CCFB82F3}']
    property Id: Integer readonly dispid 1;
    property Timestamp: TDateTime readonly dispid 2;
    property PartnerHandle: WideString readonly dispid 3;
    property PartnerDisplayName: WideString readonly dispid 4;
    property ConferenceId: Integer readonly dispid 5;
    property type_: TCallType readonly dispid 6;
    property Status: TCallStatus dispid 7;
    property FailureReason: TCallFailureReason readonly dispid 8;
    property Subject: WideString readonly dispid 9;
    property PstnNumber: WideString readonly dispid 10;
    property Duration: Integer readonly dispid 11;
    property PstnStatus: WideString readonly dispid 12;
    property Seen: WordBool dispid 13;
    procedure Hold; dispid 14;
    procedure Resume; dispid 15;
    procedure Finish; dispid 16;
    procedure Answer; dispid 17;
    property DTMF: WideString writeonly dispid 18;
    property Participants: IParticipantCollection readonly dispid 19;
    procedure Join(Id: Integer); dispid 20;
    property VmDuration: Integer readonly dispid 21;
    property VmAllowedDuration: Integer readonly dispid 22;
    property VideoStatus: TCallVideoStatus readonly dispid 23;
    property VideoSendStatus: TCallVideoSendStatus readonly dispid 24;
    property VideoReceiveStatus: TCallVideoSendStatus readonly dispid 25;
    procedure StartVideoSend; dispid 26;
    procedure StopVideoSend; dispid 27;
    procedure StartVideoReceive; dispid 28;
    procedure StopVideoReceive; dispid 29;
    procedure RedirectToVoicemail; dispid 30;
    procedure Forward; dispid 31;
    property Rate: Integer readonly dispid 32;
    property RateCurrency: WideString readonly dispid 33;
    property RatePrecision: Integer readonly dispid 34;
    property InputDevice[DeviceType: TCallIoDeviceType]: WideString dispid 35;
    property OutputDevice[DeviceType: TCallIoDeviceType]: WideString dispid 36;
    property CaptureMicDevice[DeviceType: TCallIoDeviceType]: WideString dispid 37;
    property InputStatus: WordBool readonly dispid 38;
    property ForwardedBy: WideString readonly dispid 39;
    property CanTransfer[const Target: WideString]: WordBool readonly dispid 40;
    property TransferStatus: TCallStatus readonly dispid 41;
    property TransferActive: WordBool readonly dispid 42;
    property TransferredBy: WideString readonly dispid 43;
    property TransferredTo: WideString readonly dispid 44;
    procedure Transfer(const Target: WideString); dispid 45;
    property TargetIdentity: WideString readonly dispid 46;
  end;

// *********************************************************************//
// Interface: IParticipantCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F825FE12-9826-4BD2-BF7E-1ED95B8480FE}
// *********************************************************************//
  IParticipantCollection = interface(IDispatch)
    ['{F825FE12-9826-4BD2-BF7E-1ED95B8480FE}']
    function Get_Count: Integer; safecall;
    procedure Add(const pParticipant: IParticipant); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IParticipant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IParticipant read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IParticipantCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F825FE12-9826-4BD2-BF7E-1ED95B8480FE}
// *********************************************************************//
  IParticipantCollectionDisp = dispinterface
    ['{F825FE12-9826-4BD2-BF7E-1ED95B8480FE}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pParticipant: IParticipant); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IParticipant readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IParticipant
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F1B2AD09-1961-4336-A6BF-93010B73DE72}
// *********************************************************************//
  IParticipant = interface(IDispatch)
    ['{F1B2AD09-1961-4336-A6BF-93010B73DE72}']
    function Get_Handle: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_CallType: TCallType; safecall;
    function Get_CallStatus: TCallStatus; safecall;
    property Handle: WideString read Get_Handle;
    property DisplayName: WideString read Get_DisplayName;
    property CallType: TCallType read Get_CallType;
    property CallStatus: TCallStatus read Get_CallStatus;
  end;

// *********************************************************************//
// DispIntf:  IParticipantDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F1B2AD09-1961-4336-A6BF-93010B73DE72}
// *********************************************************************//
  IParticipantDisp = dispinterface
    ['{F1B2AD09-1961-4336-A6BF-93010B73DE72}']
    property Handle: WideString readonly dispid 1;
    property DisplayName: WideString readonly dispid 2;
    property CallType: TCallType readonly dispid 3;
    property CallStatus: TCallStatus readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ICallChannel
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B76C04C9-0EB1-4748-B0C0-D01DEB595906}
// *********************************************************************//
  ICallChannel = interface(IDispatch)
    ['{B76C04C9-0EB1-4748-B0C0-D01DEB595906}']
    function Get_type_: TCallChannelType; safecall;
    procedure Set_type_(pVal: TCallChannelType); safecall;
    procedure SendTextMessage(const Text: WideString); safecall;
    function Get_Stream: IApplicationStream; safecall;
    function Get_Manager: ICallChannelManager; safecall;
    function Get_Call: ICall; safecall;
    property type_: TCallChannelType read Get_type_ write Set_type_;
    property Stream: IApplicationStream read Get_Stream;
    property Manager: ICallChannelManager read Get_Manager;
    property Call: ICall read Get_Call;
  end;

// *********************************************************************//
// DispIntf:  ICallChannelDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B76C04C9-0EB1-4748-B0C0-D01DEB595906}
// *********************************************************************//
  ICallChannelDisp = dispinterface
    ['{B76C04C9-0EB1-4748-B0C0-D01DEB595906}']
    property type_: TCallChannelType dispid 2;
    procedure SendTextMessage(const Text: WideString); dispid 3;
    property Stream: IApplicationStream readonly dispid 4;
    property Manager: ICallChannelManager readonly dispid 5;
    property Call: ICall readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ICallChannelManager
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {30C27C77-525F-4A3F-B9DD-C5A77C67250D}
// *********************************************************************//
  ICallChannelManager = interface(IDispatch)
    ['{30C27C77-525F-4A3F-B9DD-C5A77C67250D}']
    procedure Connect(const pSkype: ISkype); safecall;
    procedure Disconnect; safecall;
    function Get_Channels: ICallChannelCollection; safecall;
    function Get_ChannelType: TCallChannelType; safecall;
    procedure Set_ChannelType(pVal: TCallChannelType); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    procedure CreateApplication(const ApplicationName: WideString); safecall;
    function Get_Created: WordBool; safecall;
    property Channels: ICallChannelCollection read Get_Channels;
    property ChannelType: TCallChannelType read Get_ChannelType write Set_ChannelType;
    property Name: WideString read Get_Name write Set_Name;
    property Created: WordBool read Get_Created;
  end;

// *********************************************************************//
// DispIntf:  ICallChannelManagerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {30C27C77-525F-4A3F-B9DD-C5A77C67250D}
// *********************************************************************//
  ICallChannelManagerDisp = dispinterface
    ['{30C27C77-525F-4A3F-B9DD-C5A77C67250D}']
    procedure Connect(const pSkype: ISkype); dispid 1;
    procedure Disconnect; dispid 2;
    property Channels: ICallChannelCollection readonly dispid 3;
    property ChannelType: TCallChannelType dispid 6;
    property Name: WideString dispid 7;
    procedure CreateApplication(const ApplicationName: WideString); dispid 8;
    property Created: WordBool readonly dispid 9;
  end;

// *********************************************************************//
// Interface: ISkype
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B1878BFE-53D3-402E-8C86-190B19AF70D5}
// *********************************************************************//
  ISkype = interface(IDispatch)
    ['{B1878BFE-53D3-402E-8C86-190B19AF70D5}']
    function Get_Timeout: Integer; safecall;
    procedure Set_Timeout(pVal: Integer); safecall;
    function Get_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                           const PropName: WideString): WideString; safecall;
    procedure Set_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                            const PropName: WideString; const pVal: WideString); safecall;
    function Get_Variable(const Name: WideString): WideString; safecall;
    procedure Set_Variable(const Name: WideString; const pVal: WideString); safecall;
    function Get_CurrentUserHandle: WideString; safecall;
    function Get_CurrentUserStatus: TUserStatus; safecall;
    procedure Set_CurrentUserStatus(pVal: TUserStatus); safecall;
    function Get_ConnectionStatus: TConnectionStatus; safecall;
    function Get_Mute: WordBool; safecall;
    procedure Set_Mute(pVal: WordBool); safecall;
    function Get_Version: WideString; safecall;
    function Get_Privilege(const Name: WideString): WordBool; safecall;
    function Get_CurrentUser: IUser; safecall;
    function Get_Convert: IConversion; safecall;
    function Get_Friends: IUserCollection; safecall;
    function SearchForUsers(const Target: WideString): IUserCollection; safecall;
    function Get_Calls(const Target: WideString): ICallCollection; safecall;
    function Get_ActiveCalls: ICallCollection; safecall;
    function Get_MissedCalls: ICallCollection; safecall;
    function Get_Messages(const Target: WideString): IChatMessageCollection; safecall;
    function Get_MissedMessages: IChatMessageCollection; safecall;
    function Get_AttachmentStatus: TAttachmentStatus; safecall;
    function Get_Protocol: Integer; safecall;
    procedure Set_Protocol(pVal: Integer); safecall;
    procedure Attach(Protocol: Integer; Wait: WordBool); safecall;
    function PlaceCall(const Target: WideString; const Target2: WideString; 
                       const Target3: WideString; const Target4: WideString): ICall; safecall;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; safecall;
    function Get_User(const Username: WideString): IUser; safecall;
    function Get_Message(Id: Integer): IChatMessage; safecall;
    function Get_Call(Id: Integer): ICall; safecall;
    procedure SendCommand(const pCommand: ICommand); safecall;
    function Get_Chats: IChatCollection; safecall;
    function Get_Chat(const Name: WideString): IChat; safecall;
    procedure ChangeUserStatus(newVal: TUserStatus); safecall;
    function Get_Conference(Id: Integer): IConference; safecall;
    function Get_Conferences: IConferenceCollection; safecall;
    function Get_Profile(const Property_: WideString): WideString; safecall;
    procedure Set_Profile(const Property_: WideString; const pVal: WideString); safecall;
    function Get_ActiveChats: IChatCollection; safecall;
    function Get_MissedChats: IChatCollection; safecall;
    function Get_RecentChats: IChatCollection; safecall;
    function Get_BookmarkedChats: IChatCollection; safecall;
    function CreateChatWith(const Username: WideString): IChat; safecall;
    function CreateChatMultiple(const pMembers: IUserCollection): IChat; safecall;
    function Get_Voicemails: IVoicemailCollection; safecall;
    function SendVoicemail(const Username: WideString): IVoicemail; safecall;
    function Get_UsersWaitingAuthorization: IUserCollection; safecall;
    procedure ClearChatHistory; safecall;
    procedure ClearVoicemailHistory; safecall;
    procedure ClearCallHistory(const Username: WideString; Type_: TCallHistory); safecall;
    function Get_CommandId: WordBool; safecall;
    procedure Set_CommandId(pVal: WordBool); safecall;
    function Get_Application(const Name: WideString): IApplication; safecall;
    function Get_Greeting(const Username: WideString): IVoicemail; safecall;
    function Get_Cache: WordBool; safecall;
    procedure Set_Cache(pVal: WordBool); safecall;
    procedure ResetCache; safecall;
    function Get_CurrentUserProfile: IProfile; safecall;
    function Get_Groups: IGroupCollection; safecall;
    function Get_CustomGroups: IGroupCollection; safecall;
    function Get_HardwiredGroups: IGroupCollection; safecall;
    function CreateGroup(const GroupName: WideString): IGroup; safecall;
    procedure DeleteGroup(GroupId: Integer); safecall;
    function Get_Settings: ISettings; safecall;
    function Get_Client: IClient; safecall;
    procedure Set_FriendlyName(const Param1: WideString); safecall;
    function Get_Command(Id: Integer; const Command: WideString; const Reply: WideString; 
                         Block: WordBool; Timeout: Integer): ICommand; safecall;
    function Get_Voicemail(Id: Integer): IVoicemail; safecall;
    function Get_MissedVoicemails: IVoicemailCollection; safecall;
    procedure EnableApiSecurityContext(Context: TApiSecurityContext); safecall;
    function Get_ApiSecurityContextEnabled(Context: TApiSecurityContext): WordBool; safecall;
    function CreateSms(MessageType: TSmsMessageType; const TargetNumbers: WideString): ISmsMessage; safecall;
    function Get_Smss: ISmsMessageCollection; safecall;
    function Get_MissedSmss: ISmsMessageCollection; safecall;
    function SendSms(const TargetNumbers: WideString; const MessageText: WideString; 
                     const ReplyToNumber: WideString): ISmsMessage; safecall;
    function AsyncSearchUsers(const Target: WideString): Integer; safecall;
    function Get_ApiWrapperVersion: WideString; safecall;
    function Get_SilentMode: WordBool; safecall;
    procedure Set_SilentMode(pVal: WordBool); safecall;
    function Get_FileTransfers: IFileTransferCollection; safecall;
    function Get_ActiveFileTransfers: IFileTransferCollection; safecall;
    function Get_FocusedContacts: IUserCollection; safecall;
    function FindChatUsingBlob(const Blob: WideString): IChat; safecall;
    function CreateChatUsingBlob(const Blob: WideString): IChat; safecall;
    function Get_PredictiveDialerCountry: WideString; safecall;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
    property Property_[const ObjectType: WideString; const ObjectId: WideString; 
                       const PropName: WideString]: WideString read Get_Property_ write Set_Property_;
    property Variable[const Name: WideString]: WideString read Get_Variable write Set_Variable;
    property CurrentUserHandle: WideString read Get_CurrentUserHandle;
    property CurrentUserStatus: TUserStatus read Get_CurrentUserStatus write Set_CurrentUserStatus;
    property ConnectionStatus: TConnectionStatus read Get_ConnectionStatus;
    property Mute: WordBool read Get_Mute write Set_Mute;
    property Version: WideString read Get_Version;
    property Privilege[const Name: WideString]: WordBool read Get_Privilege;
    property CurrentUser: IUser read Get_CurrentUser;
    property Convert: IConversion read Get_Convert;
    property Friends: IUserCollection read Get_Friends;
    property Calls[const Target: WideString]: ICallCollection read Get_Calls;
    property ActiveCalls: ICallCollection read Get_ActiveCalls;
    property MissedCalls: ICallCollection read Get_MissedCalls;
    property Messages[const Target: WideString]: IChatMessageCollection read Get_Messages;
    property MissedMessages: IChatMessageCollection read Get_MissedMessages;
    property AttachmentStatus: TAttachmentStatus read Get_AttachmentStatus;
    property Protocol: Integer read Get_Protocol write Set_Protocol;
    property User[const Username: WideString]: IUser read Get_User;
    property Message[Id: Integer]: IChatMessage read Get_Message;
    property Call[Id: Integer]: ICall read Get_Call;
    property Chats: IChatCollection read Get_Chats;
    property Chat[const Name: WideString]: IChat read Get_Chat;
    property Conference[Id: Integer]: IConference read Get_Conference;
    property Conferences: IConferenceCollection read Get_Conferences;
    property Profile[const Property_: WideString]: WideString read Get_Profile write Set_Profile;
    property ActiveChats: IChatCollection read Get_ActiveChats;
    property MissedChats: IChatCollection read Get_MissedChats;
    property RecentChats: IChatCollection read Get_RecentChats;
    property BookmarkedChats: IChatCollection read Get_BookmarkedChats;
    property Voicemails: IVoicemailCollection read Get_Voicemails;
    property UsersWaitingAuthorization: IUserCollection read Get_UsersWaitingAuthorization;
    property CommandId: WordBool read Get_CommandId write Set_CommandId;
    property Application[const Name: WideString]: IApplication read Get_Application;
    property Greeting[const Username: WideString]: IVoicemail read Get_Greeting;
    property Cache: WordBool read Get_Cache write Set_Cache;
    property CurrentUserProfile: IProfile read Get_CurrentUserProfile;
    property Groups: IGroupCollection read Get_Groups;
    property CustomGroups: IGroupCollection read Get_CustomGroups;
    property HardwiredGroups: IGroupCollection read Get_HardwiredGroups;
    property Settings: ISettings read Get_Settings;
    property Client: IClient read Get_Client;
    property FriendlyName: WideString write Set_FriendlyName;
    property Command[Id: Integer; const Command: WideString; const Reply: WideString; 
                     Block: WordBool; Timeout: Integer]: ICommand read Get_Command;
    property Voicemail[Id: Integer]: IVoicemail read Get_Voicemail;
    property MissedVoicemails: IVoicemailCollection read Get_MissedVoicemails;
    property ApiSecurityContextEnabled[Context: TApiSecurityContext]: WordBool read Get_ApiSecurityContextEnabled;
    property Smss: ISmsMessageCollection read Get_Smss;
    property MissedSmss: ISmsMessageCollection read Get_MissedSmss;
    property ApiWrapperVersion: WideString read Get_ApiWrapperVersion;
    property SilentMode: WordBool read Get_SilentMode write Set_SilentMode;
    property FileTransfers: IFileTransferCollection read Get_FileTransfers;
    property ActiveFileTransfers: IFileTransferCollection read Get_ActiveFileTransfers;
    property FocusedContacts: IUserCollection read Get_FocusedContacts;
    property PredictiveDialerCountry: WideString read Get_PredictiveDialerCountry;
  end;

// *********************************************************************//
// DispIntf:  ISkypeDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B1878BFE-53D3-402E-8C86-190B19AF70D5}
// *********************************************************************//
  ISkypeDisp = dispinterface
    ['{B1878BFE-53D3-402E-8C86-190B19AF70D5}']
    property Timeout: Integer dispid 1;
    property Property_[const ObjectType: WideString; const ObjectId: WideString; 
                       const PropName: WideString]: WideString dispid 2;
    property Variable[const Name: WideString]: WideString dispid 3;
    property CurrentUserHandle: WideString readonly dispid 4;
    property CurrentUserStatus: TUserStatus dispid 5;
    property ConnectionStatus: TConnectionStatus readonly dispid 6;
    property Mute: WordBool dispid 7;
    property Version: WideString readonly dispid 8;
    property Privilege[const Name: WideString]: WordBool readonly dispid 9;
    property CurrentUser: IUser readonly dispid 10;
    property Convert: IConversion readonly dispid 11;
    property Friends: IUserCollection readonly dispid 12;
    function SearchForUsers(const Target: WideString): IUserCollection; dispid 13;
    property Calls[const Target: WideString]: ICallCollection readonly dispid 14;
    property ActiveCalls: ICallCollection readonly dispid 15;
    property MissedCalls: ICallCollection readonly dispid 16;
    property Messages[const Target: WideString]: IChatMessageCollection readonly dispid 17;
    property MissedMessages: IChatMessageCollection readonly dispid 18;
    property AttachmentStatus: TAttachmentStatus readonly dispid 19;
    property Protocol: Integer dispid 20;
    procedure Attach(Protocol: Integer; Wait: WordBool); dispid 21;
    function PlaceCall(const Target: WideString; const Target2: WideString; 
                       const Target3: WideString; const Target4: WideString): ICall; dispid 22;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; dispid 23;
    property User[const Username: WideString]: IUser readonly dispid 24;
    property Message[Id: Integer]: IChatMessage readonly dispid 25;
    property Call[Id: Integer]: ICall readonly dispid 26;
    procedure SendCommand(const pCommand: ICommand); dispid 27;
    property Chats: IChatCollection readonly dispid 28;
    property Chat[const Name: WideString]: IChat readonly dispid 29;
    procedure ChangeUserStatus(newVal: TUserStatus); dispid 30;
    property Conference[Id: Integer]: IConference readonly dispid 31;
    property Conferences: IConferenceCollection readonly dispid 32;
    property Profile[const Property_: WideString]: WideString dispid 33;
    property ActiveChats: IChatCollection readonly dispid 34;
    property MissedChats: IChatCollection readonly dispid 35;
    property RecentChats: IChatCollection readonly dispid 36;
    property BookmarkedChats: IChatCollection readonly dispid 37;
    function CreateChatWith(const Username: WideString): IChat; dispid 38;
    function CreateChatMultiple(const pMembers: IUserCollection): IChat; dispid 39;
    property Voicemails: IVoicemailCollection readonly dispid 40;
    function SendVoicemail(const Username: WideString): IVoicemail; dispid 41;
    property UsersWaitingAuthorization: IUserCollection readonly dispid 42;
    procedure ClearChatHistory; dispid 43;
    procedure ClearVoicemailHistory; dispid 44;
    procedure ClearCallHistory(const Username: WideString; Type_: TCallHistory); dispid 45;
    property CommandId: WordBool dispid 46;
    property Application[const Name: WideString]: IApplication readonly dispid 47;
    property Greeting[const Username: WideString]: IVoicemail readonly dispid 48;
    property Cache: WordBool dispid 49;
    procedure ResetCache; dispid 50;
    property CurrentUserProfile: IProfile readonly dispid 51;
    property Groups: IGroupCollection readonly dispid 52;
    property CustomGroups: IGroupCollection readonly dispid 53;
    property HardwiredGroups: IGroupCollection readonly dispid 54;
    function CreateGroup(const GroupName: WideString): IGroup; dispid 55;
    procedure DeleteGroup(GroupId: Integer); dispid 56;
    property Settings: ISettings readonly dispid 57;
    property Client: IClient readonly dispid 58;
    property FriendlyName: WideString writeonly dispid 59;
    property Command[Id: Integer; const Command: WideString; const Reply: WideString; 
                     Block: WordBool; Timeout: Integer]: ICommand readonly dispid 60;
    property Voicemail[Id: Integer]: IVoicemail readonly dispid 61;
    property MissedVoicemails: IVoicemailCollection readonly dispid 62;
    procedure EnableApiSecurityContext(Context: TApiSecurityContext); dispid 63;
    property ApiSecurityContextEnabled[Context: TApiSecurityContext]: WordBool readonly dispid 64;
    function CreateSms(MessageType: TSmsMessageType; const TargetNumbers: WideString): ISmsMessage; dispid 65;
    property Smss: ISmsMessageCollection readonly dispid 66;
    property MissedSmss: ISmsMessageCollection readonly dispid 67;
    function SendSms(const TargetNumbers: WideString; const MessageText: WideString; 
                     const ReplyToNumber: WideString): ISmsMessage; dispid 68;
    function AsyncSearchUsers(const Target: WideString): Integer; dispid 69;
    property ApiWrapperVersion: WideString readonly dispid 70;
    property SilentMode: WordBool dispid 71;
    property FileTransfers: IFileTransferCollection readonly dispid 72;
    property ActiveFileTransfers: IFileTransferCollection readonly dispid 73;
    property FocusedContacts: IUserCollection readonly dispid 74;
    function FindChatUsingBlob(const Blob: WideString): IChat; dispid 75;
    function CreateChatUsingBlob(const Blob: WideString): IChat; dispid 76;
    property PredictiveDialerCountry: WideString readonly dispid 77;
  end;

// *********************************************************************//
// Interface: IConversion
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8D82F88A-B307-4074-8ED5-11C3BD213452}
// *********************************************************************//
  IConversion = interface(IDispatch)
    ['{8D82F88A-B307-4074-8ED5-11C3BD213452}']
    function OnlineStatusToText(Status: TOnlineStatus): WideString; safecall;
    function TextToOnlineStatus(const Text: WideString): TOnlineStatus; safecall;
    function BuddyStatusToText(Status: TBuddyStatus): WideString; safecall;
    function TextToBuddyStatus(const Text: WideString): TBuddyStatus; safecall;
    function CallStatusToText(Status: TCallStatus): WideString; safecall;
    function TextToCallStatus(const Text: WideString): TCallStatus; safecall;
    function CallTypeToText(CallType: TCallType): WideString; safecall;
    function TextToCallType(const Text: WideString): TCallType; safecall;
    function UserSexToText(Sex: TUserSex): WideString; safecall;
    function TextToUserSex(const Text: WideString): TUserSex; safecall;
    function ConnectionStatusToText(Status: TConnectionStatus): WideString; safecall;
    function TextToConnectionStatus(const Text: WideString): TConnectionStatus; safecall;
    function UserStatusToText(Status: TUserStatus): WideString; safecall;
    function TextToUserStatus(const Text: WideString): TUserStatus; safecall;
    function CallFailureReasonToText(reason: TCallFailureReason): WideString; safecall;
    function AttachmentStatusToText(Status: TAttachmentStatus): WideString; safecall;
    function ChatLeaveReasonToText(reason: TChatLeaveReason): WideString; safecall;
    function ChatStatusToText(Status: TChatStatus): WideString; safecall;
    function VoicemailTypeToText(Type_: TVoicemailType): WideString; safecall;
    function VoicemailStatusToText(Status: TVoicemailStatus): WideString; safecall;
    function TextToVoicemailStatus(const Text: WideString): TVoicemailStatus; safecall;
    function VoicemailFailureReasonToText(code: TVoicemailFailureReason): WideString; safecall;
    function ChatMessageStatusToText(Status: TChatMessageStatus): WideString; safecall;
    function TextToChatMessageStatus(const Text: WideString): TChatMessageStatus; safecall;
    function ChatMessageTypeToText(Type_: TChatMessageType): WideString; safecall;
    function TextToChatMessageType(const Text: WideString): TChatMessageType; safecall;
    function TextToAttachmentStatus(const Text: WideString): TAttachmentStatus; safecall;
    function GroupTypeToText(Type_: TGroupType): WideString; safecall;
    function TextToGroupType(const Text: WideString): TGroupType; safecall;
    function CallVideoStatusToText(Status: TCallVideoStatus): WideString; safecall;
    function CallVideoSendStatusToText(Status: TCallVideoSendStatus): WideString; safecall;
    function Get_Language: WideString; safecall;
    procedure Set_Language(const pVal: WideString); safecall;
    function SmsMessageStatusToText(Status: TSmsMessageStatus): WideString; safecall;
    function SmsMessageTypeToText(Type_: TSmsMessageType): WideString; safecall;
    function SmsTargetStatusToText(Status: TSmsTargetStatus): WideString; safecall;
    property Language: WideString read Get_Language write Set_Language;
  end;

// *********************************************************************//
// DispIntf:  IConversionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8D82F88A-B307-4074-8ED5-11C3BD213452}
// *********************************************************************//
  IConversionDisp = dispinterface
    ['{8D82F88A-B307-4074-8ED5-11C3BD213452}']
    function OnlineStatusToText(Status: TOnlineStatus): WideString; dispid 1;
    function TextToOnlineStatus(const Text: WideString): TOnlineStatus; dispid 2;
    function BuddyStatusToText(Status: TBuddyStatus): WideString; dispid 3;
    function TextToBuddyStatus(const Text: WideString): TBuddyStatus; dispid 4;
    function CallStatusToText(Status: TCallStatus): WideString; dispid 5;
    function TextToCallStatus(const Text: WideString): TCallStatus; dispid 6;
    function CallTypeToText(CallType: TCallType): WideString; dispid 7;
    function TextToCallType(const Text: WideString): TCallType; dispid 8;
    function UserSexToText(Sex: TUserSex): WideString; dispid 9;
    function TextToUserSex(const Text: WideString): TUserSex; dispid 10;
    function ConnectionStatusToText(Status: TConnectionStatus): WideString; dispid 11;
    function TextToConnectionStatus(const Text: WideString): TConnectionStatus; dispid 12;
    function UserStatusToText(Status: TUserStatus): WideString; dispid 13;
    function TextToUserStatus(const Text: WideString): TUserStatus; dispid 14;
    function CallFailureReasonToText(reason: TCallFailureReason): WideString; dispid 15;
    function AttachmentStatusToText(Status: TAttachmentStatus): WideString; dispid 16;
    function ChatLeaveReasonToText(reason: TChatLeaveReason): WideString; dispid 17;
    function ChatStatusToText(Status: TChatStatus): WideString; dispid 18;
    function VoicemailTypeToText(Type_: TVoicemailType): WideString; dispid 19;
    function VoicemailStatusToText(Status: TVoicemailStatus): WideString; dispid 20;
    function TextToVoicemailStatus(const Text: WideString): TVoicemailStatus; dispid 21;
    function VoicemailFailureReasonToText(code: TVoicemailFailureReason): WideString; dispid 22;
    function ChatMessageStatusToText(Status: TChatMessageStatus): WideString; dispid 23;
    function TextToChatMessageStatus(const Text: WideString): TChatMessageStatus; dispid 24;
    function ChatMessageTypeToText(Type_: TChatMessageType): WideString; dispid 25;
    function TextToChatMessageType(const Text: WideString): TChatMessageType; dispid 26;
    function TextToAttachmentStatus(const Text: WideString): TAttachmentStatus; dispid 27;
    function GroupTypeToText(Type_: TGroupType): WideString; dispid 28;
    function TextToGroupType(const Text: WideString): TGroupType; dispid 29;
    function CallVideoStatusToText(Status: TCallVideoStatus): WideString; dispid 30;
    function CallVideoSendStatusToText(Status: TCallVideoSendStatus): WideString; dispid 31;
    property Language: WideString dispid 32;
    function SmsMessageStatusToText(Status: TSmsMessageStatus): WideString; dispid 33;
    function SmsMessageTypeToText(Type_: TSmsMessageType): WideString; dispid 34;
    function SmsTargetStatusToText(Status: TSmsTargetStatus): WideString; dispid 35;
  end;

// *********************************************************************//
// Interface: ICallCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {72E1EC13-0DF9-48AE-8A31-E2900D85BA55}
// *********************************************************************//
  ICallCollection = interface(IDispatch)
    ['{72E1EC13-0DF9-48AE-8A31-E2900D85BA55}']
    function Get_Count: Integer; safecall;
    procedure Add(const pCall: ICall); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): ICall; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: ICall read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICallCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {72E1EC13-0DF9-48AE-8A31-E2900D85BA55}
// *********************************************************************//
  ICallCollectionDisp = dispinterface
    ['{72E1EC13-0DF9-48AE-8A31-E2900D85BA55}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pCall: ICall); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: ICall readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IChatMessageCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9511117A-7BCA-4669-BE96-6EA6866975C1}
// *********************************************************************//
  IChatMessageCollection = interface(IDispatch)
    ['{9511117A-7BCA-4669-BE96-6EA6866975C1}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IChatMessage); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IChatMessage; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IChatMessage read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IChatMessageCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9511117A-7BCA-4669-BE96-6EA6866975C1}
// *********************************************************************//
  IChatMessageCollectionDisp = dispinterface
    ['{9511117A-7BCA-4669-BE96-6EA6866975C1}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IChatMessage); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IChatMessage readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IChatMessage
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4CFF5C70-3C95-4566-824A-FA164586D535}
// *********************************************************************//
  IChatMessage = interface(IDispatch)
    ['{4CFF5C70-3C95-4566-824A-FA164586D535}']
    function Get_Id: Integer; safecall;
    function Get_Timestamp: TDateTime; safecall;
    function Get_FromHandle: WideString; safecall;
    function Get_FromDisplayName: WideString; safecall;
    function Get_type_: TChatMessageType; safecall;
    function Get_Status: TChatMessageStatus; safecall;
    function Get_LeaveReason: TChatLeaveReason; safecall;
    function Get_Body: WideString; safecall;
    function Get_ChatName: WideString; safecall;
    function Get_Users: IUserCollection; safecall;
    procedure Set_Seen(Param1: WordBool); safecall;
    function Get_Chat: IChat; safecall;
    function Get_Sender: IUser; safecall;
    function Get_EditedBy: WideString; safecall;
    function Get_EditedTimestamp: TDateTime; safecall;
    procedure Set_Body(const pVal: WideString); safecall;
    function Get_Role: TChatMemberRole; safecall;
    function Get_Options: Integer; safecall;
    function Get_IsEditable: WordBool; safecall;
    property Id: Integer read Get_Id;
    property Timestamp: TDateTime read Get_Timestamp;
    property FromHandle: WideString read Get_FromHandle;
    property FromDisplayName: WideString read Get_FromDisplayName;
    property type_: TChatMessageType read Get_type_;
    property Status: TChatMessageStatus read Get_Status;
    property LeaveReason: TChatLeaveReason read Get_LeaveReason;
    property Body: WideString read Get_Body write Set_Body;
    property ChatName: WideString read Get_ChatName;
    property Users: IUserCollection read Get_Users;
    property Seen: WordBool write Set_Seen;
    property Chat: IChat read Get_Chat;
    property Sender: IUser read Get_Sender;
    property EditedBy: WideString read Get_EditedBy;
    property EditedTimestamp: TDateTime read Get_EditedTimestamp;
    property Role: TChatMemberRole read Get_Role;
    property Options: Integer read Get_Options;
    property IsEditable: WordBool read Get_IsEditable;
  end;

// *********************************************************************//
// DispIntf:  IChatMessageDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4CFF5C70-3C95-4566-824A-FA164586D535}
// *********************************************************************//
  IChatMessageDisp = dispinterface
    ['{4CFF5C70-3C95-4566-824A-FA164586D535}']
    property Id: Integer readonly dispid 1;
    property Timestamp: TDateTime readonly dispid 2;
    property FromHandle: WideString readonly dispid 3;
    property FromDisplayName: WideString readonly dispid 4;
    property type_: TChatMessageType readonly dispid 5;
    property Status: TChatMessageStatus readonly dispid 6;
    property LeaveReason: TChatLeaveReason readonly dispid 7;
    property Body: WideString dispid 8;
    property ChatName: WideString readonly dispid 9;
    property Users: IUserCollection readonly dispid 10;
    property Seen: WordBool writeonly dispid 11;
    property Chat: IChat readonly dispid 12;
    property Sender: IUser readonly dispid 13;
    property EditedBy: WideString readonly dispid 14;
    property EditedTimestamp: TDateTime readonly dispid 15;
    property Role: TChatMemberRole readonly dispid 16;
    property Options: Integer readonly dispid 17;
    property IsEditable: WordBool readonly dispid 18;
  end;

// *********************************************************************//
// Interface: IChat
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8C24106C-3368-4580-93E5-5ED4ACCDE306}
// *********************************************************************//
  IChat = interface(IDispatch)
    ['{8C24106C-3368-4580-93E5-5ED4ACCDE306}']
    function Get_Name: WideString; safecall;
    function Get_Messages: IChatMessageCollection; safecall;
    function Get_Timestamp: TDateTime; safecall;
    function Get_Adder: IUser; safecall;
    function Get_Status: TChatStatus; safecall;
    function Get_Posters: IUserCollection; safecall;
    function Get_Members: IUserCollection; safecall;
    function Get_Topic: WideString; safecall;
    procedure Set_Topic(const pVal: WideString); safecall;
    function Get_ActiveMembers: IUserCollection; safecall;
    function Get_FriendlyName: WideString; safecall;
    procedure OpenWindow; safecall;
    function SendMessage(const MessageText: WideString): IChatMessage; safecall;
    procedure Leave; safecall;
    procedure AddMembers(const pMembers: IUserCollection); safecall;
    function Get_RecentMessages: IChatMessageCollection; safecall;
    function Get_Bookmarked: WordBool; safecall;
    procedure Bookmark; safecall;
    procedure Unbookmark; safecall;
    function Get_TopicXML: WideString; safecall;
    procedure Set_TopicXML(const pVal: WideString); safecall;
    function Get_MemberObjects: IChatMemberCollection; safecall;
    function Get_Blob: WideString; safecall;
    function Get_Options: Integer; safecall;
    procedure Set_Options(pVal: Integer); safecall;
    function Get_PasswordHint: WideString; safecall;
    procedure SetPassword(const Password: WideString; const Hint: WideString); safecall;
    function Get_GuideLines: WideString; safecall;
    procedure Set_GuideLines(const pVal: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pVal: WideString); safecall;
    function Get_DialogPartner: WideString; safecall;
    function Get_ActivityTimestamp: TDateTime; safecall;
    function Get_MyRole: TChatMemberRole; safecall;
    function Get_Applicants: IUserCollection; safecall;
    procedure Join; safecall;
    procedure Kick(const Handle: WideString); safecall;
    procedure KickBan(const Handle: WideString); safecall;
    procedure Disband; safecall;
    procedure EnterPassword(const Password: WideString); safecall;
    procedure ClearRecentMessages; safecall;
    procedure AcceptAdd; safecall;
    procedure Set_AlertString(const Param1: WideString); safecall;
    function Get_type_: TChatType; safecall;
    function Get_MyStatus: TChatMyStatus; safecall;
    property Name: WideString read Get_Name;
    property Messages: IChatMessageCollection read Get_Messages;
    property Timestamp: TDateTime read Get_Timestamp;
    property Adder: IUser read Get_Adder;
    property Status: TChatStatus read Get_Status;
    property Posters: IUserCollection read Get_Posters;
    property Members: IUserCollection read Get_Members;
    property Topic: WideString read Get_Topic write Set_Topic;
    property ActiveMembers: IUserCollection read Get_ActiveMembers;
    property FriendlyName: WideString read Get_FriendlyName;
    property RecentMessages: IChatMessageCollection read Get_RecentMessages;
    property Bookmarked: WordBool read Get_Bookmarked;
    property TopicXML: WideString read Get_TopicXML write Set_TopicXML;
    property MemberObjects: IChatMemberCollection read Get_MemberObjects;
    property Blob: WideString read Get_Blob;
    property Options: Integer read Get_Options write Set_Options;
    property PasswordHint: WideString read Get_PasswordHint;
    property GuideLines: WideString read Get_GuideLines write Set_GuideLines;
    property Description: WideString read Get_Description write Set_Description;
    property DialogPartner: WideString read Get_DialogPartner;
    property ActivityTimestamp: TDateTime read Get_ActivityTimestamp;
    property MyRole: TChatMemberRole read Get_MyRole;
    property Applicants: IUserCollection read Get_Applicants;
    property AlertString: WideString write Set_AlertString;
    property type_: TChatType read Get_type_;
    property MyStatus: TChatMyStatus read Get_MyStatus;
  end;

// *********************************************************************//
// DispIntf:  IChatDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8C24106C-3368-4580-93E5-5ED4ACCDE306}
// *********************************************************************//
  IChatDisp = dispinterface
    ['{8C24106C-3368-4580-93E5-5ED4ACCDE306}']
    property Name: WideString readonly dispid 1;
    property Messages: IChatMessageCollection readonly dispid 2;
    property Timestamp: TDateTime readonly dispid 3;
    property Adder: IUser readonly dispid 4;
    property Status: TChatStatus readonly dispid 5;
    property Posters: IUserCollection readonly dispid 6;
    property Members: IUserCollection readonly dispid 7;
    property Topic: WideString dispid 8;
    property ActiveMembers: IUserCollection readonly dispid 9;
    property FriendlyName: WideString readonly dispid 10;
    procedure OpenWindow; dispid 11;
    function SendMessage(const MessageText: WideString): IChatMessage; dispid 12;
    procedure Leave; dispid 13;
    procedure AddMembers(const pMembers: IUserCollection); dispid 14;
    property RecentMessages: IChatMessageCollection readonly dispid 15;
    property Bookmarked: WordBool readonly dispid 16;
    procedure Bookmark; dispid 17;
    procedure Unbookmark; dispid 18;
    property TopicXML: WideString dispid 19;
    property MemberObjects: IChatMemberCollection readonly dispid 20;
    property Blob: WideString readonly dispid 21;
    property Options: Integer dispid 22;
    property PasswordHint: WideString readonly dispid 23;
    procedure SetPassword(const Password: WideString; const Hint: WideString); dispid 24;
    property GuideLines: WideString dispid 25;
    property Description: WideString dispid 26;
    property DialogPartner: WideString readonly dispid 27;
    property ActivityTimestamp: TDateTime readonly dispid 28;
    property MyRole: TChatMemberRole readonly dispid 29;
    property Applicants: IUserCollection readonly dispid 30;
    procedure Join; dispid 31;
    procedure Kick(const Handle: WideString); dispid 32;
    procedure KickBan(const Handle: WideString); dispid 33;
    procedure Disband; dispid 34;
    procedure EnterPassword(const Password: WideString); dispid 35;
    procedure ClearRecentMessages; dispid 36;
    procedure AcceptAdd; dispid 37;
    property AlertString: WideString writeonly dispid 38;
    property type_: TChatType readonly dispid 39;
    property MyStatus: TChatMyStatus readonly dispid 40;
  end;

// *********************************************************************//
// Interface: IChatMemberCollection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FFC686B-2E5E-4763-8B03-FF4FD3A0C4DA}
// *********************************************************************//
  IChatMemberCollection = interface(IDispatch)
    ['{8FFC686B-2E5E-4763-8B03-FF4FD3A0C4DA}']
    function Get_Count: Integer; safecall;
    function Get_Item(Index: Integer): IChatMember; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IChatMember read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IChatMemberCollectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FFC686B-2E5E-4763-8B03-FF4FD3A0C4DA}
// *********************************************************************//
  IChatMemberCollectionDisp = dispinterface
    ['{8FFC686B-2E5E-4763-8B03-FF4FD3A0C4DA}']
    property Count: Integer readonly dispid 1;
    property Item[Index: Integer]: IChatMember readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IChatMember
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9E9F0473-94C2-4FDD-85D2-43E3478765F9}
// *********************************************************************//
  IChatMember = interface(IDispatch)
    ['{9E9F0473-94C2-4FDD-85D2-43E3478765F9}']
    function Get_Id: WideString; safecall;
    function Get_Handle: WideString; safecall;
    function Get_Role: TChatMemberRole; safecall;
    procedure Set_Role(pVal: TChatMemberRole); safecall;
    function Get_CanSetRoleTo(Role: TChatMemberRole): WordBool; safecall;
    function Get_IsActive: WordBool; safecall;
    function Get_Chat: IChat; safecall;
    property Id: WideString read Get_Id;
    property Handle: WideString read Get_Handle;
    property Role: TChatMemberRole read Get_Role write Set_Role;
    property CanSetRoleTo[Role: TChatMemberRole]: WordBool read Get_CanSetRoleTo;
    property IsActive: WordBool read Get_IsActive;
    property Chat: IChat read Get_Chat;
  end;

// *********************************************************************//
// DispIntf:  IChatMemberDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9E9F0473-94C2-4FDD-85D2-43E3478765F9}
// *********************************************************************//
  IChatMemberDisp = dispinterface
    ['{9E9F0473-94C2-4FDD-85D2-43E3478765F9}']
    property Id: WideString readonly dispid 1;
    property Handle: WideString readonly dispid 2;
    property Role: TChatMemberRole dispid 3;
    property CanSetRoleTo[Role: TChatMemberRole]: WordBool readonly dispid 4;
    property IsActive: WordBool readonly dispid 5;
    property Chat: IChat readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ICommand
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {48E046A8-31D7-4E5F-A611-47BF32B86405}
// *********************************************************************//
  ICommand = interface(IDispatch)
    ['{48E046A8-31D7-4E5F-A611-47BF32B86405}']
    function Get_Id: Integer; safecall;
    procedure Set_Id(pVal: Integer); safecall;
    function Get_Timeout: Integer; safecall;
    procedure Set_Timeout(pVal: Integer); safecall;
    function Get_Blocking: WordBool; safecall;
    procedure Set_Blocking(pVal: WordBool); safecall;
    function Get_Command: WideString; safecall;
    procedure Set_Command(const pVal: WideString); safecall;
    function Get_Reply: WideString; safecall;
    procedure Set_Reply(const pVal: WideString); safecall;
    function Get_Expected: WideString; safecall;
    procedure Set_Expected(const pVal: WideString); safecall;
    property Id: Integer read Get_Id write Set_Id;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
    property Blocking: WordBool read Get_Blocking write Set_Blocking;
    property Command: WideString read Get_Command write Set_Command;
    property Reply: WideString read Get_Reply write Set_Reply;
    property Expected: WideString read Get_Expected write Set_Expected;
  end;

// *********************************************************************//
// DispIntf:  ICommandDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {48E046A8-31D7-4E5F-A611-47BF32B86405}
// *********************************************************************//
  ICommandDisp = dispinterface
    ['{48E046A8-31D7-4E5F-A611-47BF32B86405}']
    property Id: Integer dispid 1;
    property Timeout: Integer dispid 2;
    property Blocking: WordBool dispid 3;
    property Command: WideString dispid 4;
    property Reply: WideString dispid 5;
    property Expected: WideString dispid 6;
  end;

// *********************************************************************//
// Interface: IChatCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {052A4165-010A-457D-A874-E661B6322846}
// *********************************************************************//
  IChatCollection = interface(IDispatch)
    ['{052A4165-010A-457D-A874-E661B6322846}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IChat); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IChat; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IChat read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IChatCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {052A4165-010A-457D-A874-E661B6322846}
// *********************************************************************//
  IChatCollectionDisp = dispinterface
    ['{052A4165-010A-457D-A874-E661B6322846}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IChat); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IChat readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IConference
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {AEB48497-5090-479C-8BE0-BD51597156A1}
// *********************************************************************//
  IConference = interface(IDispatch)
    ['{AEB48497-5090-479C-8BE0-BD51597156A1}']
    function Get_Id: Integer; safecall;
    function Get_Calls: ICallCollection; safecall;
    function Get_ActiveCalls: ICallCollection; safecall;
    procedure Hold; safecall;
    procedure Resume; safecall;
    procedure Finish; safecall;
    property Id: Integer read Get_Id;
    property Calls: ICallCollection read Get_Calls;
    property ActiveCalls: ICallCollection read Get_ActiveCalls;
  end;

// *********************************************************************//
// DispIntf:  IConferenceDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {AEB48497-5090-479C-8BE0-BD51597156A1}
// *********************************************************************//
  IConferenceDisp = dispinterface
    ['{AEB48497-5090-479C-8BE0-BD51597156A1}']
    property Id: Integer readonly dispid 1;
    property Calls: ICallCollection readonly dispid 2;
    property ActiveCalls: ICallCollection readonly dispid 3;
    procedure Hold; dispid 4;
    procedure Resume; dispid 5;
    procedure Finish; dispid 6;
  end;

// *********************************************************************//
// Interface: IConferenceCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F3E0C529-7D4F-4AF5-B501-27D25D4D2825}
// *********************************************************************//
  IConferenceCollection = interface(IDispatch)
    ['{F3E0C529-7D4F-4AF5-B501-27D25D4D2825}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IConference); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IConference; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IConference read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IConferenceCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F3E0C529-7D4F-4AF5-B501-27D25D4D2825}
// *********************************************************************//
  IConferenceCollectionDisp = dispinterface
    ['{F3E0C529-7D4F-4AF5-B501-27D25D4D2825}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IConference); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IConference readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IVoicemailCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {21494848-BBD5-4192-BE32-57D1A19D8E94}
// *********************************************************************//
  IVoicemailCollection = interface(IDispatch)
    ['{21494848-BBD5-4192-BE32-57D1A19D8E94}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IVoicemail); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IVoicemail; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IVoicemail read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IVoicemailCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {21494848-BBD5-4192-BE32-57D1A19D8E94}
// *********************************************************************//
  IVoicemailCollectionDisp = dispinterface
    ['{21494848-BBD5-4192-BE32-57D1A19D8E94}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IVoicemail); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IVoicemail readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IVoicemail
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {120B425E-6313-4924-B5A9-3E9F2E444A00}
// *********************************************************************//
  IVoicemail = interface(IDispatch)
    ['{120B425E-6313-4924-B5A9-3E9F2E444A00}']
    function Get_type_: TVoicemailType; safecall;
    function Get_PartnerHandle: WideString; safecall;
    function Get_PartnerDisplayName: WideString; safecall;
    function Get_Status: TVoicemailStatus; safecall;
    function Get_FailureReason: TVoicemailFailureReason; safecall;
    function Get_Timestamp: TDateTime; safecall;
    function Get_Duration: Integer; safecall;
    function Get_AllowedDuration: Integer; safecall;
    function Get_Id: Integer; safecall;
    procedure Open; safecall;
    procedure StartPlayback; safecall;
    procedure StopPlayback; safecall;
    procedure Upload; safecall;
    procedure Download; safecall;
    procedure StartRecording; safecall;
    procedure StopRecording; safecall;
    procedure Delete; safecall;
    procedure StartPlaybackInCall; safecall;
    procedure SetUnplayed; safecall;
    property type_: TVoicemailType read Get_type_;
    property PartnerHandle: WideString read Get_PartnerHandle;
    property PartnerDisplayName: WideString read Get_PartnerDisplayName;
    property Status: TVoicemailStatus read Get_Status;
    property FailureReason: TVoicemailFailureReason read Get_FailureReason;
    property Timestamp: TDateTime read Get_Timestamp;
    property Duration: Integer read Get_Duration;
    property AllowedDuration: Integer read Get_AllowedDuration;
    property Id: Integer read Get_Id;
  end;

// *********************************************************************//
// DispIntf:  IVoicemailDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {120B425E-6313-4924-B5A9-3E9F2E444A00}
// *********************************************************************//
  IVoicemailDisp = dispinterface
    ['{120B425E-6313-4924-B5A9-3E9F2E444A00}']
    property type_: TVoicemailType readonly dispid 1;
    property PartnerHandle: WideString readonly dispid 2;
    property PartnerDisplayName: WideString readonly dispid 3;
    property Status: TVoicemailStatus readonly dispid 4;
    property FailureReason: TVoicemailFailureReason readonly dispid 5;
    property Timestamp: TDateTime readonly dispid 6;
    property Duration: Integer readonly dispid 7;
    property AllowedDuration: Integer readonly dispid 8;
    property Id: Integer readonly dispid 9;
    procedure Open; dispid 10;
    procedure StartPlayback; dispid 11;
    procedure StopPlayback; dispid 12;
    procedure Upload; dispid 13;
    procedure Download; dispid 14;
    procedure StartRecording; dispid 15;
    procedure StopRecording; dispid 16;
    procedure Delete; dispid 17;
    procedure StartPlaybackInCall; dispid 18;
    procedure SetUnplayed; dispid 19;
  end;

// *********************************************************************//
// Interface: IProfile
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B9A3D72F-04C1-4878-993C-D89F83E20569}
// *********************************************************************//
  IProfile = interface(IDispatch)
    ['{B9A3D72F-04C1-4878-993C-D89F83E20569}']
    function Get_FullName: WideString; safecall;
    procedure Set_FullName(const pVal: WideString); safecall;
    function Get_Birthday: WideString; safecall;
    procedure Set_Birthday(const pVal: WideString); safecall;
    function Get_Sex: TUserSex; safecall;
    procedure Set_Sex(pVal: TUserSex); safecall;
    function Get_Languages: WideString; safecall;
    procedure Set_Languages(const pVal: WideString); safecall;
    function Get_Country: WideString; safecall;
    procedure Set_Country(const pVal: WideString); safecall;
    function Get_Province: WideString; safecall;
    procedure Set_Province(const pVal: WideString); safecall;
    function Get_City: WideString; safecall;
    procedure Set_City(const pVal: WideString); safecall;
    function Get_PhoneHome: WideString; safecall;
    procedure Set_PhoneHome(const pVal: WideString); safecall;
    function Get_PhoneOffice: WideString; safecall;
    procedure Set_PhoneOffice(const pVal: WideString); safecall;
    function Get_PhoneMobile: WideString; safecall;
    procedure Set_PhoneMobile(const pVal: WideString); safecall;
    function Get_Homepage: WideString; safecall;
    procedure Set_Homepage(const pVal: WideString); safecall;
    function Get_About: WideString; safecall;
    procedure Set_About(const pVal: WideString); safecall;
    function Get_MoodText: WideString; safecall;
    procedure Set_MoodText(const pVal: WideString); safecall;
    function Get_Timezone: Integer; safecall;
    procedure Set_Timezone(pVal: Integer); safecall;
    function Get_CallNoAnswerTimeout: Smallint; safecall;
    procedure Set_CallNoAnswerTimeout(pVal: Smallint); safecall;
    function Get_CallApplyCF: WordBool; safecall;
    procedure Set_CallApplyCF(pVal: WordBool); safecall;
    function Get_CallSendToVM: WordBool; safecall;
    procedure Set_CallSendToVM(pVal: WordBool); safecall;
    function Get_CallForwardRules: WideString; safecall;
    procedure Set_CallForwardRules(const pVal: WideString); safecall;
    function Get_Balance: Integer; safecall;
    function Get_BalanceCurrency: WideString; safecall;
    function Get_BalanceToText: WideString; safecall;
    function Get_IPCountry: WideString; safecall;
    function Get_ValidatedSmsNumbers: WideString; safecall;
    function Get_RichMoodText: WideString; safecall;
    procedure Set_RichMoodText(const pVal: WideString); safecall;
    property FullName: WideString read Get_FullName write Set_FullName;
    property Birthday: WideString read Get_Birthday write Set_Birthday;
    property Sex: TUserSex read Get_Sex write Set_Sex;
    property Languages: WideString read Get_Languages write Set_Languages;
    property Country: WideString read Get_Country write Set_Country;
    property Province: WideString read Get_Province write Set_Province;
    property City: WideString read Get_City write Set_City;
    property PhoneHome: WideString read Get_PhoneHome write Set_PhoneHome;
    property PhoneOffice: WideString read Get_PhoneOffice write Set_PhoneOffice;
    property PhoneMobile: WideString read Get_PhoneMobile write Set_PhoneMobile;
    property Homepage: WideString read Get_Homepage write Set_Homepage;
    property About: WideString read Get_About write Set_About;
    property MoodText: WideString read Get_MoodText write Set_MoodText;
    property Timezone: Integer read Get_Timezone write Set_Timezone;
    property CallNoAnswerTimeout: Smallint read Get_CallNoAnswerTimeout write Set_CallNoAnswerTimeout;
    property CallApplyCF: WordBool read Get_CallApplyCF write Set_CallApplyCF;
    property CallSendToVM: WordBool read Get_CallSendToVM write Set_CallSendToVM;
    property CallForwardRules: WideString read Get_CallForwardRules write Set_CallForwardRules;
    property Balance: Integer read Get_Balance;
    property BalanceCurrency: WideString read Get_BalanceCurrency;
    property BalanceToText: WideString read Get_BalanceToText;
    property IPCountry: WideString read Get_IPCountry;
    property ValidatedSmsNumbers: WideString read Get_ValidatedSmsNumbers;
    property RichMoodText: WideString read Get_RichMoodText write Set_RichMoodText;
  end;

// *********************************************************************//
// DispIntf:  IProfileDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B9A3D72F-04C1-4878-993C-D89F83E20569}
// *********************************************************************//
  IProfileDisp = dispinterface
    ['{B9A3D72F-04C1-4878-993C-D89F83E20569}']
    property FullName: WideString dispid 1;
    property Birthday: WideString dispid 2;
    property Sex: TUserSex dispid 3;
    property Languages: WideString dispid 4;
    property Country: WideString dispid 5;
    property Province: WideString dispid 6;
    property City: WideString dispid 7;
    property PhoneHome: WideString dispid 8;
    property PhoneOffice: WideString dispid 9;
    property PhoneMobile: WideString dispid 10;
    property Homepage: WideString dispid 11;
    property About: WideString dispid 12;
    property MoodText: WideString dispid 13;
    property Timezone: Integer dispid 14;
    property CallNoAnswerTimeout: Smallint dispid 15;
    property CallApplyCF: WordBool dispid 16;
    property CallSendToVM: WordBool dispid 17;
    property CallForwardRules: WideString dispid 18;
    property Balance: Integer readonly dispid 19;
    property BalanceCurrency: WideString readonly dispid 20;
    property BalanceToText: WideString readonly dispid 21;
    property IPCountry: WideString readonly dispid 22;
    property ValidatedSmsNumbers: WideString readonly dispid 23;
    property RichMoodText: WideString dispid 24;
  end;

// *********************************************************************//
// Interface: IGroupCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C5C99DC-09CF-4A9C-BD94-8D655454A1F4}
// *********************************************************************//
  IGroupCollection = interface(IDispatch)
    ['{4C5C99DC-09CF-4A9C-BD94-8D655454A1F4}']
    function Get_Count: Integer; safecall;
    procedure Add(const pGroup: IGroup); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IGroup; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IGroup read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IGroupCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C5C99DC-09CF-4A9C-BD94-8D655454A1F4}
// *********************************************************************//
  IGroupCollectionDisp = dispinterface
    ['{4C5C99DC-09CF-4A9C-BD94-8D655454A1F4}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pGroup: IGroup); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IGroup readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IGroup
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {84513274-9C49-4AAA-B8FC-316EB32CFE95}
// *********************************************************************//
  IGroup = interface(IDispatch)
    ['{84513274-9C49-4AAA-B8FC-316EB32CFE95}']
    function Get_Id: Integer; safecall;
    function Get_type_: TGroupType; safecall;
    function Get_CustomGroupId: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    procedure Set_DisplayName(const pVal: WideString); safecall;
    function Get_Users: IUserCollection; safecall;
    function Get_OnlineUsers: IUserCollection; safecall;
    function Get_IsVisible: WordBool; safecall;
    function Get_IsExpanded: WordBool; safecall;
    procedure AddUser(const Username: WideString); safecall;
    procedure RemoveUser(const Username: WideString); safecall;
    procedure Share(const MessageText: WideString); safecall;
    procedure Accept; safecall;
    procedure Decline; safecall;
    property Id: Integer read Get_Id;
    property type_: TGroupType read Get_type_;
    property CustomGroupId: WideString read Get_CustomGroupId;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property Users: IUserCollection read Get_Users;
    property OnlineUsers: IUserCollection read Get_OnlineUsers;
    property IsVisible: WordBool read Get_IsVisible;
    property IsExpanded: WordBool read Get_IsExpanded;
  end;

// *********************************************************************//
// DispIntf:  IGroupDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {84513274-9C49-4AAA-B8FC-316EB32CFE95}
// *********************************************************************//
  IGroupDisp = dispinterface
    ['{84513274-9C49-4AAA-B8FC-316EB32CFE95}']
    property Id: Integer readonly dispid 1;
    property type_: TGroupType readonly dispid 2;
    property CustomGroupId: WideString readonly dispid 3;
    property DisplayName: WideString dispid 4;
    property Users: IUserCollection readonly dispid 5;
    property OnlineUsers: IUserCollection readonly dispid 6;
    property IsVisible: WordBool readonly dispid 7;
    property IsExpanded: WordBool readonly dispid 8;
    procedure AddUser(const Username: WideString); dispid 9;
    procedure RemoveUser(const Username: WideString); dispid 10;
    procedure Share(const MessageText: WideString); dispid 11;
    procedure Accept; dispid 12;
    procedure Decline; dispid 13;
  end;

// *********************************************************************//
// Interface: ISettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2BC8C53B-3312-4A77-AC87-31DF18A1EC51}
// *********************************************************************//
  ISettings = interface(IDispatch)
    ['{2BC8C53B-3312-4A77-AC87-31DF18A1EC51}']
    function Get_AudioIn: WideString; safecall;
    procedure Set_AudioIn(const pVal: WideString); safecall;
    function Get_AudioOut: WideString; safecall;
    procedure Set_AudioOut(const pVal: WideString); safecall;
    function Get_AutoAway: WordBool; safecall;
    procedure Set_AutoAway(pVal: WordBool); safecall;
    function Get_Ringer: WideString; safecall;
    procedure Set_Ringer(const pVal: WideString); safecall;
    procedure Set_Avatar(const Id: WideString; const Param2: WideString); safecall;
    function Get_RingToneStatus(const Id: WideString): WordBool; safecall;
    procedure Set_RingTone(const Id: WideString; const Param2: WideString); safecall;
    function Get_VideoIn: WideString; safecall;
    procedure Set_VideoIn(const pVal: WideString); safecall;
    function Get_PCSpeaker: WordBool; safecall;
    procedure Set_PCSpeaker(pVal: WordBool); safecall;
    function Get_AGC: WordBool; safecall;
    procedure Set_AGC(pVal: WordBool); safecall;
    function Get_AEC: WordBool; safecall;
    procedure Set_AEC(pVal: WordBool); safecall;
    function Get_Language: WideString; safecall;
    procedure Set_Language(const pVal: WideString); safecall;
    procedure Set_RingToneStatus(const Id: WideString; pVal: WordBool); safecall;
    procedure SaveAvatarToFile(const Filename: WideString; const AvatarId: WideString); safecall;
    procedure LoadAvatarFromFile(const Filename: WideString; const AvatarId: WideString); safecall;
    procedure ResetIdleTimer; safecall;
    property AudioIn: WideString read Get_AudioIn write Set_AudioIn;
    property AudioOut: WideString read Get_AudioOut write Set_AudioOut;
    property AutoAway: WordBool read Get_AutoAway write Set_AutoAway;
    property Ringer: WideString read Get_Ringer write Set_Ringer;
    property Avatar[const Id: WideString]: WideString write Set_Avatar;
    property RingToneStatus[const Id: WideString]: WordBool read Get_RingToneStatus write Set_RingToneStatus;
    property RingTone[const Id: WideString]: WideString write Set_RingTone;
    property VideoIn: WideString read Get_VideoIn write Set_VideoIn;
    property PCSpeaker: WordBool read Get_PCSpeaker write Set_PCSpeaker;
    property AGC: WordBool read Get_AGC write Set_AGC;
    property AEC: WordBool read Get_AEC write Set_AEC;
    property Language: WideString read Get_Language write Set_Language;
  end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2BC8C53B-3312-4A77-AC87-31DF18A1EC51}
// *********************************************************************//
  ISettingsDisp = dispinterface
    ['{2BC8C53B-3312-4A77-AC87-31DF18A1EC51}']
    property AudioIn: WideString dispid 1;
    property AudioOut: WideString dispid 2;
    property AutoAway: WordBool dispid 3;
    property Ringer: WideString dispid 4;
    property Avatar[const Id: WideString]: WideString writeonly dispid 5;
    property RingToneStatus[const Id: WideString]: WordBool dispid 6;
    property RingTone[const Id: WideString]: WideString writeonly dispid 7;
    property VideoIn: WideString dispid 8;
    property PCSpeaker: WordBool dispid 9;
    property AGC: WordBool dispid 10;
    property AEC: WordBool dispid 11;
    property Language: WideString dispid 12;
    procedure SaveAvatarToFile(const Filename: WideString; const AvatarId: WideString); dispid 13;
    procedure LoadAvatarFromFile(const Filename: WideString; const AvatarId: WideString); dispid 14;
    procedure ResetIdleTimer; dispid 15;
  end;

// *********************************************************************//
// Interface: IClient
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {838731B0-88E7-4BED-81DC-B35CA8433341}
// *********************************************************************//
  IClient = interface(IDispatch)
    ['{838731B0-88E7-4BED-81DC-B35CA8433341}']
    procedure Start(Minimized: WordBool; Nosplash: WordBool); safecall;
    procedure Minimize; safecall;
    procedure Shutdown; safecall;
    function Get_IsRunning: WordBool; safecall;
    procedure OpenProfileDialog; safecall;
    procedure OpenUserInfoDialog(const Username: WideString); safecall;
    procedure OpenConferenceDialog; safecall;
    procedure OpenSearchDialog; safecall;
    procedure OpenOptionsDialog(const Page: WideString); safecall;
    procedure OpenCallHistoryTab; safecall;
    procedure OpenContactsTab; safecall;
    procedure OpenDialpadTab; safecall;
    procedure OpenSendContactsDialog(const Username: WideString); safecall;
    procedure OpenBlockedUsersDialog; safecall;
    procedure OpenImportContactsWizard; safecall;
    procedure OpenGettingStartedWizard; safecall;
    procedure OpenAuthorizationDialog(const Username: WideString); safecall;
    procedure OpenDialog(const Name: WideString; const Param1: WideString; const Param2: WideString); safecall;
    procedure OpenVideoTestDialog; safecall;
    procedure OpenAddContactDialog(const Username: WideString); safecall;
    procedure OpenMessageDialog(const Username: WideString; const Text: WideString); safecall;
    procedure OpenFileTransferDialog(const User: WideString; const Folder: WideString); safecall;
    procedure Focus; safecall;
    procedure ButtonPressed(const Key: WideString); safecall;
    procedure ButtonReleased(const Key: WideString); safecall;
    procedure OpenSmsDialog(const SmsId: WideString); safecall;
    function CreateEvent(const EventId: WideString; const Caption: WideString; 
                         const Hint: WideString): IPluginEvent; safecall;
    function CreateMenuItem(const MenuItemId: WideString; PluginContext: TPluginContext; 
                            const CaptionText: WideString; const HintText: WideString; 
                            const IconPath: WideString; Enabled: WordBool; 
                            ContactType: TPluginContactType; MultipleContacts: WordBool): IPluginMenuItem; safecall;
    function Get_Wallpaper: WideString; safecall;
    procedure Set_Wallpaper(const pVal: WideString); safecall;
    procedure OpenLiveTab; safecall;
    property IsRunning: WordBool read Get_IsRunning;
    property Wallpaper: WideString read Get_Wallpaper write Set_Wallpaper;
  end;

// *********************************************************************//
// DispIntf:  IClientDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {838731B0-88E7-4BED-81DC-B35CA8433341}
// *********************************************************************//
  IClientDisp = dispinterface
    ['{838731B0-88E7-4BED-81DC-B35CA8433341}']
    procedure Start(Minimized: WordBool; Nosplash: WordBool); dispid 1;
    procedure Minimize; dispid 2;
    procedure Shutdown; dispid 3;
    property IsRunning: WordBool readonly dispid 4;
    procedure OpenProfileDialog; dispid 5;
    procedure OpenUserInfoDialog(const Username: WideString); dispid 6;
    procedure OpenConferenceDialog; dispid 7;
    procedure OpenSearchDialog; dispid 8;
    procedure OpenOptionsDialog(const Page: WideString); dispid 9;
    procedure OpenCallHistoryTab; dispid 10;
    procedure OpenContactsTab; dispid 11;
    procedure OpenDialpadTab; dispid 12;
    procedure OpenSendContactsDialog(const Username: WideString); dispid 13;
    procedure OpenBlockedUsersDialog; dispid 14;
    procedure OpenImportContactsWizard; dispid 15;
    procedure OpenGettingStartedWizard; dispid 16;
    procedure OpenAuthorizationDialog(const Username: WideString); dispid 17;
    procedure OpenDialog(const Name: WideString; const Param1: WideString; const Param2: WideString); dispid 18;
    procedure OpenVideoTestDialog; dispid 19;
    procedure OpenAddContactDialog(const Username: WideString); dispid 20;
    procedure OpenMessageDialog(const Username: WideString; const Text: WideString); dispid 21;
    procedure OpenFileTransferDialog(const User: WideString; const Folder: WideString); dispid 22;
    procedure Focus; dispid 23;
    procedure ButtonPressed(const Key: WideString); dispid 24;
    procedure ButtonReleased(const Key: WideString); dispid 25;
    procedure OpenSmsDialog(const SmsId: WideString); dispid 26;
    function CreateEvent(const EventId: WideString; const Caption: WideString; 
                         const Hint: WideString): IPluginEvent; dispid 27;
    function CreateMenuItem(const MenuItemId: WideString; PluginContext: TPluginContext; 
                            const CaptionText: WideString; const HintText: WideString; 
                            const IconPath: WideString; Enabled: WordBool; 
                            ContactType: TPluginContactType; MultipleContacts: WordBool): IPluginMenuItem; dispid 28;
    property Wallpaper: WideString dispid 29;
    procedure OpenLiveTab; dispid 30;
  end;

// *********************************************************************//
// Interface: IPluginEvent
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4DF5F83A-0ABA-417D-A6FC-62A68AE06EF7}
// *********************************************************************//
  IPluginEvent = interface(IDispatch)
    ['{4DF5F83A-0ABA-417D-A6FC-62A68AE06EF7}']
    function Get_Id: WideString; safecall;
    procedure Delete; safecall;
    property Id: WideString read Get_Id;
  end;

// *********************************************************************//
// DispIntf:  IPluginEventDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4DF5F83A-0ABA-417D-A6FC-62A68AE06EF7}
// *********************************************************************//
  IPluginEventDisp = dispinterface
    ['{4DF5F83A-0ABA-417D-A6FC-62A68AE06EF7}']
    property Id: WideString readonly dispid 1;
    procedure Delete; dispid 2;
  end;

// *********************************************************************//
// Interface: IPluginMenuItem
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C4BDF667-3FF7-4B44-A9F4-F3937E3E6D86}
// *********************************************************************//
  IPluginMenuItem = interface(IDispatch)
    ['{C4BDF667-3FF7-4B44-A9F4-F3937E3E6D86}']
    function Get_Id: WideString; safecall;
    procedure Set_Caption(const Param1: WideString); safecall;
    procedure Set_Hint(const Param1: WideString); safecall;
    procedure Set_Enabled(Param1: WordBool); safecall;
    procedure Delete; safecall;
    property Id: WideString read Get_Id;
    property Caption: WideString write Set_Caption;
    property Hint: WideString write Set_Hint;
    property Enabled: WordBool write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  IPluginMenuItemDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C4BDF667-3FF7-4B44-A9F4-F3937E3E6D86}
// *********************************************************************//
  IPluginMenuItemDisp = dispinterface
    ['{C4BDF667-3FF7-4B44-A9F4-F3937E3E6D86}']
    property Id: WideString readonly dispid 1;
    property Caption: WideString writeonly dispid 2;
    property Hint: WideString writeonly dispid 3;
    property Enabled: WordBool writeonly dispid 4;
    procedure Delete; dispid 5;
  end;

// *********************************************************************//
// Interface: ISmsMessage
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {82D97F2A-0E17-40F3-8E01-24937F936FF4}
// *********************************************************************//
  ISmsMessage = interface(IDispatch)
    ['{82D97F2A-0E17-40F3-8E01-24937F936FF4}']
    function Get_Id: Integer; safecall;
    function Get_type_: TSmsMessageType; safecall;
    function Get_Status: TSmsMessageStatus; safecall;
    function Get_FailureReason: TSmsFailureReason; safecall;
    function Get_IsFailedUnseen: WordBool; safecall;
    procedure Set_Seen(Param1: WordBool); safecall;
    function Get_Price: Integer; safecall;
    function Get_PricePrecision: Integer; safecall;
    function Get_PriceCurrency: WideString; safecall;
    function Get_ReplyToNumber: WideString; safecall;
    procedure Set_ReplyToNumber(const pVal: WideString); safecall;
    function Get_Targets: ISmsTargetCollection; safecall;
    function Get_Body: WideString; safecall;
    procedure Set_Body(const pVal: WideString); safecall;
    function Get_Chunks: ISmsChunkCollection; safecall;
    function Get_Timestamp: TDateTime; safecall;
    procedure Send; safecall;
    procedure Delete; safecall;
    function Get_TargetNumbers: WideString; safecall;
    procedure Set_TargetNumbers(const pVal: WideString); safecall;
    property Id: Integer read Get_Id;
    property type_: TSmsMessageType read Get_type_;
    property Status: TSmsMessageStatus read Get_Status;
    property FailureReason: TSmsFailureReason read Get_FailureReason;
    property IsFailedUnseen: WordBool read Get_IsFailedUnseen;
    property Seen: WordBool write Set_Seen;
    property Price: Integer read Get_Price;
    property PricePrecision: Integer read Get_PricePrecision;
    property PriceCurrency: WideString read Get_PriceCurrency;
    property ReplyToNumber: WideString read Get_ReplyToNumber write Set_ReplyToNumber;
    property Targets: ISmsTargetCollection read Get_Targets;
    property Body: WideString read Get_Body write Set_Body;
    property Chunks: ISmsChunkCollection read Get_Chunks;
    property Timestamp: TDateTime read Get_Timestamp;
    property TargetNumbers: WideString read Get_TargetNumbers write Set_TargetNumbers;
  end;

// *********************************************************************//
// DispIntf:  ISmsMessageDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {82D97F2A-0E17-40F3-8E01-24937F936FF4}
// *********************************************************************//
  ISmsMessageDisp = dispinterface
    ['{82D97F2A-0E17-40F3-8E01-24937F936FF4}']
    property Id: Integer readonly dispid 1;
    property type_: TSmsMessageType readonly dispid 2;
    property Status: TSmsMessageStatus readonly dispid 3;
    property FailureReason: TSmsFailureReason readonly dispid 4;
    property IsFailedUnseen: WordBool readonly dispid 5;
    property Seen: WordBool writeonly dispid 6;
    property Price: Integer readonly dispid 7;
    property PricePrecision: Integer readonly dispid 8;
    property PriceCurrency: WideString readonly dispid 9;
    property ReplyToNumber: WideString dispid 10;
    property Targets: ISmsTargetCollection readonly dispid 11;
    property Body: WideString dispid 12;
    property Chunks: ISmsChunkCollection readonly dispid 13;
    property Timestamp: TDateTime readonly dispid 14;
    procedure Send; dispid 15;
    procedure Delete; dispid 16;
    property TargetNumbers: WideString dispid 17;
  end;

// *********************************************************************//
// Interface: ISmsTargetCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A2365EF3-4805-4DD3-A9D4-1A8AE3E17D84}
// *********************************************************************//
  ISmsTargetCollection = interface(IDispatch)
    ['{A2365EF3-4805-4DD3-A9D4-1A8AE3E17D84}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: ISmsTarget); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): ISmsTarget; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: ISmsTarget read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISmsTargetCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A2365EF3-4805-4DD3-A9D4-1A8AE3E17D84}
// *********************************************************************//
  ISmsTargetCollectionDisp = dispinterface
    ['{A2365EF3-4805-4DD3-A9D4-1A8AE3E17D84}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: ISmsTarget); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: ISmsTarget readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISmsTarget
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A9B9C33A-49A6-41D5-B13F-2AA4F284F406}
// *********************************************************************//
  ISmsTarget = interface(IDispatch)
    ['{A9B9C33A-49A6-41D5-B13F-2AA4F284F406}']
    function Get_Status: TSmsTargetStatus; safecall;
    function Get_Number: WideString; safecall;
    function Get_Message: ISmsMessage; safecall;
    property Status: TSmsTargetStatus read Get_Status;
    property Number: WideString read Get_Number;
    property Message: ISmsMessage read Get_Message;
  end;

// *********************************************************************//
// DispIntf:  ISmsTargetDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A9B9C33A-49A6-41D5-B13F-2AA4F284F406}
// *********************************************************************//
  ISmsTargetDisp = dispinterface
    ['{A9B9C33A-49A6-41D5-B13F-2AA4F284F406}']
    property Status: TSmsTargetStatus readonly dispid 1;
    property Number: WideString readonly dispid 2;
    property Message: ISmsMessage readonly dispid 3;
  end;

// *********************************************************************//
// Interface: ISmsChunkCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BBDA7D2D-B1C2-4AF9-AB5B-D762341D8903}
// *********************************************************************//
  ISmsChunkCollection = interface(IDispatch)
    ['{BBDA7D2D-B1C2-4AF9-AB5B-D762341D8903}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: ISmsChunk); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): ISmsChunk; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: ISmsChunk read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISmsChunkCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BBDA7D2D-B1C2-4AF9-AB5B-D762341D8903}
// *********************************************************************//
  ISmsChunkCollectionDisp = dispinterface
    ['{BBDA7D2D-B1C2-4AF9-AB5B-D762341D8903}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: ISmsChunk); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: ISmsChunk readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISmsChunk
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A9062508-C926-4415-ABB7-A5A46DB34456}
// *********************************************************************//
  ISmsChunk = interface(IDispatch)
    ['{A9062508-C926-4415-ABB7-A5A46DB34456}']
    function Get_Id: Integer; safecall;
    function Get_CharactersLeft: Integer; safecall;
    function Get_Text: WideString; safecall;
    function Get_Message: ISmsMessage; safecall;
    property Id: Integer read Get_Id;
    property CharactersLeft: Integer read Get_CharactersLeft;
    property Text: WideString read Get_Text;
    property Message: ISmsMessage read Get_Message;
  end;

// *********************************************************************//
// DispIntf:  ISmsChunkDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A9062508-C926-4415-ABB7-A5A46DB34456}
// *********************************************************************//
  ISmsChunkDisp = dispinterface
    ['{A9062508-C926-4415-ABB7-A5A46DB34456}']
    property Id: Integer readonly dispid 1;
    property CharactersLeft: Integer readonly dispid 2;
    property Text: WideString readonly dispid 3;
    property Message: ISmsMessage readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISmsMessageCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1D5BAB2E-69CC-4136-8E02-FC20767DC9E2}
// *********************************************************************//
  ISmsMessageCollection = interface(IDispatch)
    ['{1D5BAB2E-69CC-4136-8E02-FC20767DC9E2}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: ISmsMessage); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): ISmsMessage; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: ISmsMessage read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISmsMessageCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1D5BAB2E-69CC-4136-8E02-FC20767DC9E2}
// *********************************************************************//
  ISmsMessageCollectionDisp = dispinterface
    ['{1D5BAB2E-69CC-4136-8E02-FC20767DC9E2}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: ISmsMessage); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: ISmsMessage readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IFileTransferCollection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70A59A25-E823-4C3F-8F33-775008895EE3}
// *********************************************************************//
  IFileTransferCollection = interface(IDispatch)
    ['{70A59A25-E823-4C3F-8F33-775008895EE3}']
    function Get_Count: Integer; safecall;
    procedure Add(const pItem: IFileTransfer); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IFileTransfer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: IFileTransfer read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IFileTransferCollectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70A59A25-E823-4C3F-8F33-775008895EE3}
// *********************************************************************//
  IFileTransferCollectionDisp = dispinterface
    ['{70A59A25-E823-4C3F-8F33-775008895EE3}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pItem: IFileTransfer); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: IFileTransfer readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IFileTransfer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4D36B368-B88C-45FA-B136-3EF77B2EAD39}
// *********************************************************************//
  IFileTransfer = interface(IDispatch)
    ['{4D36B368-B88C-45FA-B136-3EF77B2EAD39}']
    function Get_Id: WideString; safecall;
    function Get_type_: TFileTransferType; safecall;
    function Get_Status: TFileTransferStatus; safecall;
    function Get_FailureReason: TFileTransferFailureReason; safecall;
    function Get_PartnerHandle: WideString; safecall;
    function Get_PartnerDisplayName: WideString; safecall;
    function Get_StartTime: TDateTime; safecall;
    function Get_FinishTime: TDateTime; safecall;
    function Get_FilePath: WideString; safecall;
    function Get_Filename: WideString; safecall;
    function Get_BytesPerSecond: WideString; safecall;
    function Get_BytesTransferred: WideString; safecall;
    property Id: WideString read Get_Id;
    property type_: TFileTransferType read Get_type_;
    property Status: TFileTransferStatus read Get_Status;
    property FailureReason: TFileTransferFailureReason read Get_FailureReason;
    property PartnerHandle: WideString read Get_PartnerHandle;
    property PartnerDisplayName: WideString read Get_PartnerDisplayName;
    property StartTime: TDateTime read Get_StartTime;
    property FinishTime: TDateTime read Get_FinishTime;
    property FilePath: WideString read Get_FilePath;
    property Filename: WideString read Get_Filename;
    property BytesPerSecond: WideString read Get_BytesPerSecond;
    property BytesTransferred: WideString read Get_BytesTransferred;
  end;

// *********************************************************************//
// DispIntf:  IFileTransferDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4D36B368-B88C-45FA-B136-3EF77B2EAD39}
// *********************************************************************//
  IFileTransferDisp = dispinterface
    ['{4D36B368-B88C-45FA-B136-3EF77B2EAD39}']
    property Id: WideString readonly dispid 1;
    property type_: TFileTransferType readonly dispid 2;
    property Status: TFileTransferStatus readonly dispid 3;
    property FailureReason: TFileTransferFailureReason readonly dispid 4;
    property PartnerHandle: WideString readonly dispid 5;
    property PartnerDisplayName: WideString readonly dispid 6;
    property StartTime: TDateTime readonly dispid 7;
    property FinishTime: TDateTime readonly dispid 8;
    property FilePath: WideString readonly dispid 9;
    property Filename: WideString readonly dispid 10;
    property BytesPerSecond: WideString readonly dispid 11;
    property BytesTransferred: WideString readonly dispid 12;
  end;

// *********************************************************************//
// Interface: ICallChannelCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8CB09087-E389-4C6D-A6A2-7D4BCA8545D9}
// *********************************************************************//
  ICallChannelCollection = interface(IDispatch)
    ['{8CB09087-E389-4C6D-A6A2-7D4BCA8545D9}']
    function Get_Count: Integer; safecall;
    procedure Add(const pChannel: ICallChannel); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): ICallChannel; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[Index: Integer]: ICallChannel read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICallChannelCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8CB09087-E389-4C6D-A6A2-7D4BCA8545D9}
// *********************************************************************//
  ICallChannelCollectionDisp = dispinterface
    ['{8CB09087-E389-4C6D-A6A2-7D4BCA8545D9}']
    property Count: Integer readonly dispid 1;
    procedure Add(const pChannel: ICallChannel); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure RemoveAll; dispid 4;
    property Item[Index: Integer]: ICallChannel readonly dispid 0; default;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ICallChannelMessage
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0EF6FD5D-607D-4EA3-9C90-683D73449E9D}
// *********************************************************************//
  ICallChannelMessage = interface(IDispatch)
    ['{0EF6FD5D-607D-4EA3-9C90-683D73449E9D}']
    function Get_Text: WideString; safecall;
    procedure Set_Text(const pVal: WideString); safecall;
    property Text: WideString read Get_Text write Set_Text;
  end;

// *********************************************************************//
// DispIntf:  ICallChannelMessageDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0EF6FD5D-607D-4EA3-9C90-683D73449E9D}
// *********************************************************************//
  ICallChannelMessageDisp = dispinterface
    ['{0EF6FD5D-607D-4EA3-9C90-683D73449E9D}']
    property Text: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: ISkypeApplication
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {55A5200C-B2E8-4795-A6FA-858CA3FE2F26}
// *********************************************************************//
  ISkypeApplication = interface(IDispatch)
    ['{55A5200C-B2E8-4795-A6FA-858CA3FE2F26}']
    procedure CreateApplication(const Name: WideString); safecall;
    procedure DeleteApplication(const Name: WideString); safecall;
    function Get_ApplicationStreams(const AppName: WideString): IApplicationStreamCollection; safecall;
    procedure ConnectApplicationUser(const AppName: WideString; const Username: WideString; 
                                     WaitConnected: WordBool); safecall;
    function ReadApplicationStream(const AppName: WideString; const StreamHandle: WideString): WideString; safecall;
    procedure WriteApplicationStream(const AppName: WideString; const StreamHandle: WideString; 
                                     const Text: WideString); safecall;
    function Get_ApplicationConnectableUsers(const AppName: WideString): IUserCollection; safecall;
    function Get_ApplicationConnectingUsers(const AppName: WideString): IUserCollection; safecall;
    procedure DisconnectApplicationUser(const AppName: WideString; const Username: WideString); safecall;
    procedure SendApplicationDatagram(const AppName: WideString; const StreamHandle: WideString; 
                                      const Text: WideString); safecall;
    function Get_ApplicationSendingStreams(const AppName: WideString): IApplicationStreamCollection; safecall;
    function Get_ApplicationReceivedStreams(const AppName: WideString): IApplicationStreamCollection; safecall;
    property ApplicationStreams[const AppName: WideString]: IApplicationStreamCollection read Get_ApplicationStreams;
    property ApplicationConnectableUsers[const AppName: WideString]: IUserCollection read Get_ApplicationConnectableUsers;
    property ApplicationConnectingUsers[const AppName: WideString]: IUserCollection read Get_ApplicationConnectingUsers;
    property ApplicationSendingStreams[const AppName: WideString]: IApplicationStreamCollection read Get_ApplicationSendingStreams;
    property ApplicationReceivedStreams[const AppName: WideString]: IApplicationStreamCollection read Get_ApplicationReceivedStreams;
  end;

// *********************************************************************//
// DispIntf:  ISkypeApplicationDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {55A5200C-B2E8-4795-A6FA-858CA3FE2F26}
// *********************************************************************//
  ISkypeApplicationDisp = dispinterface
    ['{55A5200C-B2E8-4795-A6FA-858CA3FE2F26}']
    procedure CreateApplication(const Name: WideString); dispid 1;
    procedure DeleteApplication(const Name: WideString); dispid 2;
    property ApplicationStreams[const AppName: WideString]: IApplicationStreamCollection readonly dispid 3;
    procedure ConnectApplicationUser(const AppName: WideString; const Username: WideString; 
                                     WaitConnected: WordBool); dispid 4;
    function ReadApplicationStream(const AppName: WideString; const StreamHandle: WideString): WideString; dispid 5;
    procedure WriteApplicationStream(const AppName: WideString; const StreamHandle: WideString; 
                                     const Text: WideString); dispid 6;
    property ApplicationConnectableUsers[const AppName: WideString]: IUserCollection readonly dispid 7;
    property ApplicationConnectingUsers[const AppName: WideString]: IUserCollection readonly dispid 8;
    procedure DisconnectApplicationUser(const AppName: WideString; const Username: WideString); dispid 9;
    procedure SendApplicationDatagram(const AppName: WideString; const StreamHandle: WideString; 
                                      const Text: WideString); dispid 10;
    property ApplicationSendingStreams[const AppName: WideString]: IApplicationStreamCollection readonly dispid 11;
    property ApplicationReceivedStreams[const AppName: WideString]: IApplicationStreamCollection readonly dispid 12;
  end;

// *********************************************************************//
// Interface: ISkypeCall
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {54590FC3-0405-4A2D-B4A5-BEAC026577F2}
// *********************************************************************//
  ISkypeCall = interface(IDispatch)
    ['{54590FC3-0405-4A2D-B4A5-BEAC026577F2}']
    function Get_CallTimestamp(const CallId: WideString): TDateTime; safecall;
    function Get_CallPartnerHandle(const CallId: WideString): WideString; safecall;
    function Get_CallPartnerDisplayName(const CallId: WideString): WideString; safecall;
    function Get_CallConferenceId(const CallId: WideString): WideString; safecall;
    function Get_CallType(const CallId: WideString): TCallType; safecall;
    function Get_CallStatus(const CallId: WideString): TCallStatus; safecall;
    procedure Set_CallStatus(const CallId: WideString; pVal: TCallStatus); safecall;
    function Get_CallFailureReason(const CallId: WideString): TCallFailureReason; safecall;
    function Get_CallSubject(const CallId: WideString): WideString; safecall;
    function Get_CallPstnNumber(const CallId: WideString): WideString; safecall;
    function Get_CallDuration(const CallId: WideString): Integer; safecall;
    function Get_CallPstnStatus(const CallId: WideString): WideString; safecall;
    procedure Set_CallSeenStatus(const CallId: WideString; pVal: WordBool); safecall;
    procedure CallHold(const CallId: WideString); safecall;
    procedure CallResume(const CallId: WideString); safecall;
    procedure CallFinish(const CallId: WideString); safecall;
    procedure CallAnswer(const CallId: WideString); safecall;
    procedure Set_CallDTMF(const CallId: WideString; const Param2: WideString); safecall;
    function Get_CallParticipants(const CallId: WideString): IParticipantCollection; safecall;
    procedure CallJoin(const CallId1: WideString; const CallId2: WideString); safecall;
    function Get_CallVmDuration(const CallId: WideString): Integer; safecall;
    function Get_CallVmAllowedDuration(const CallId: WideString): Integer; safecall;
    function Get_CallVideoStatus(const CallId: WideString): TCallVideoStatus; safecall;
    function Get_CallVideoSendStatus(const CallId: WideString): TCallVideoSendStatus; safecall;
    function Get_CallVideoReceiveStatus(const CallId: WideString): TCallVideoSendStatus; safecall;
    procedure StartCallVideoSend(const CallId: WideString); safecall;
    procedure StopCallVideoSend(const CallId: WideString); safecall;
    procedure StartCallVideoReceive(const CallId: WideString); safecall;
    procedure StopCallVideoReceive(const CallId: WideString); safecall;
    procedure RedirectCallToVoicemail(const CallId: WideString); safecall;
    procedure ForwardCall(const CallId: WideString); safecall;
    function Get_CallRate(const CallId: WideString): Integer; safecall;
    function Get_CallRateCurrency(const CallId: WideString): WideString; safecall;
    function Get_CallRatePrecision(const CallId: WideString): Integer; safecall;
    function Get_CallInputDevice(const CallId: WideString; DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_CallInputDevice(const CallId: WideString; DeviceType: TCallIoDeviceType; 
                                  const pVal: WideString); safecall;
    function Get_CallOutputDevice(const CallId: WideString; DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_CallOutputDevice(const CallId: WideString; DeviceType: TCallIoDeviceType; 
                                   const pVal: WideString); safecall;
    function Get_CallCaptureMicDevice(const CallId: WideString; DeviceType: TCallIoDeviceType): WideString; safecall;
    procedure Set_CallCaptureMicDevice(const CallId: WideString; DeviceType: TCallIoDeviceType; 
                                       const pVal: WideString); safecall;
    function Get_CallInputStatus(const CallId: WideString): WordBool; safecall;
    function Get_CallForwardedBy(const CallId: WideString): WideString; safecall;
    function Get_CallSeenStatus(const CallId: WideString): WordBool; safecall;
    function Get_CallCanTransfer(const CallId: WideString; const Target: WideString): WordBool; safecall;
    function Get_CallTransferStatus(const CallId: WideString): TCallStatus; safecall;
    function Get_CallTransferActive(const CallId: WideString): WordBool; safecall;
    function Get_CallTransferredBy(const CallId: WideString): WideString; safecall;
    function Get_CallTransferredTo(const CallId: WideString): WideString; safecall;
    procedure TransferCall(const CallId: WideString; const Target: WideString); safecall;
    function Get_CallTargetIdentity(const CallId: WideString): WideString; safecall;
    property CallTimestamp[const CallId: WideString]: TDateTime read Get_CallTimestamp;
    property CallPartnerHandle[const CallId: WideString]: WideString read Get_CallPartnerHandle;
    property CallPartnerDisplayName[const CallId: WideString]: WideString read Get_CallPartnerDisplayName;
    property CallConferenceId[const CallId: WideString]: WideString read Get_CallConferenceId;
    property CallType[const CallId: WideString]: TCallType read Get_CallType;
    property CallStatus[const CallId: WideString]: TCallStatus read Get_CallStatus write Set_CallStatus;
    property CallFailureReason[const CallId: WideString]: TCallFailureReason read Get_CallFailureReason;
    property CallSubject[const CallId: WideString]: WideString read Get_CallSubject;
    property CallPstnNumber[const CallId: WideString]: WideString read Get_CallPstnNumber;
    property CallDuration[const CallId: WideString]: Integer read Get_CallDuration;
    property CallPstnStatus[const CallId: WideString]: WideString read Get_CallPstnStatus;
    property CallSeenStatus[const CallId: WideString]: WordBool read Get_CallSeenStatus write Set_CallSeenStatus;
    property CallDTMF[const CallId: WideString]: WideString write Set_CallDTMF;
    property CallParticipants[const CallId: WideString]: IParticipantCollection read Get_CallParticipants;
    property CallVmDuration[const CallId: WideString]: Integer read Get_CallVmDuration;
    property CallVmAllowedDuration[const CallId: WideString]: Integer read Get_CallVmAllowedDuration;
    property CallVideoStatus[const CallId: WideString]: TCallVideoStatus read Get_CallVideoStatus;
    property CallVideoSendStatus[const CallId: WideString]: TCallVideoSendStatus read Get_CallVideoSendStatus;
    property CallVideoReceiveStatus[const CallId: WideString]: TCallVideoSendStatus read Get_CallVideoReceiveStatus;
    property CallRate[const CallId: WideString]: Integer read Get_CallRate;
    property CallRateCurrency[const CallId: WideString]: WideString read Get_CallRateCurrency;
    property CallRatePrecision[const CallId: WideString]: Integer read Get_CallRatePrecision;
    property CallInputDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString read Get_CallInputDevice write Set_CallInputDevice;
    property CallOutputDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString read Get_CallOutputDevice write Set_CallOutputDevice;
    property CallCaptureMicDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString read Get_CallCaptureMicDevice write Set_CallCaptureMicDevice;
    property CallInputStatus[const CallId: WideString]: WordBool read Get_CallInputStatus;
    property CallForwardedBy[const CallId: WideString]: WideString read Get_CallForwardedBy;
    property CallCanTransfer[const CallId: WideString; const Target: WideString]: WordBool read Get_CallCanTransfer;
    property CallTransferStatus[const CallId: WideString]: TCallStatus read Get_CallTransferStatus;
    property CallTransferActive[const CallId: WideString]: WordBool read Get_CallTransferActive;
    property CallTransferredBy[const CallId: WideString]: WideString read Get_CallTransferredBy;
    property CallTransferredTo[const CallId: WideString]: WideString read Get_CallTransferredTo;
    property CallTargetIdentity[const CallId: WideString]: WideString read Get_CallTargetIdentity;
  end;

// *********************************************************************//
// DispIntf:  ISkypeCallDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {54590FC3-0405-4A2D-B4A5-BEAC026577F2}
// *********************************************************************//
  ISkypeCallDisp = dispinterface
    ['{54590FC3-0405-4A2D-B4A5-BEAC026577F2}']
    property CallTimestamp[const CallId: WideString]: TDateTime readonly dispid 1;
    property CallPartnerHandle[const CallId: WideString]: WideString readonly dispid 2;
    property CallPartnerDisplayName[const CallId: WideString]: WideString readonly dispid 3;
    property CallConferenceId[const CallId: WideString]: WideString readonly dispid 4;
    property CallType[const CallId: WideString]: TCallType readonly dispid 5;
    property CallStatus[const CallId: WideString]: TCallStatus dispid 6;
    property CallFailureReason[const CallId: WideString]: TCallFailureReason readonly dispid 7;
    property CallSubject[const CallId: WideString]: WideString readonly dispid 8;
    property CallPstnNumber[const CallId: WideString]: WideString readonly dispid 9;
    property CallDuration[const CallId: WideString]: Integer readonly dispid 10;
    property CallPstnStatus[const CallId: WideString]: WideString readonly dispid 11;
    property CallSeenStatus[const CallId: WideString]: WordBool dispid 12;
    procedure CallHold(const CallId: WideString); dispid 13;
    procedure CallResume(const CallId: WideString); dispid 14;
    procedure CallFinish(const CallId: WideString); dispid 15;
    procedure CallAnswer(const CallId: WideString); dispid 16;
    property CallDTMF[const CallId: WideString]: WideString writeonly dispid 17;
    property CallParticipants[const CallId: WideString]: IParticipantCollection readonly dispid 18;
    procedure CallJoin(const CallId1: WideString; const CallId2: WideString); dispid 19;
    property CallVmDuration[const CallId: WideString]: Integer readonly dispid 20;
    property CallVmAllowedDuration[const CallId: WideString]: Integer readonly dispid 21;
    property CallVideoStatus[const CallId: WideString]: TCallVideoStatus readonly dispid 22;
    property CallVideoSendStatus[const CallId: WideString]: TCallVideoSendStatus readonly dispid 23;
    property CallVideoReceiveStatus[const CallId: WideString]: TCallVideoSendStatus readonly dispid 24;
    procedure StartCallVideoSend(const CallId: WideString); dispid 25;
    procedure StopCallVideoSend(const CallId: WideString); dispid 26;
    procedure StartCallVideoReceive(const CallId: WideString); dispid 27;
    procedure StopCallVideoReceive(const CallId: WideString); dispid 28;
    procedure RedirectCallToVoicemail(const CallId: WideString); dispid 29;
    procedure ForwardCall(const CallId: WideString); dispid 30;
    property CallRate[const CallId: WideString]: Integer readonly dispid 31;
    property CallRateCurrency[const CallId: WideString]: WideString readonly dispid 32;
    property CallRatePrecision[const CallId: WideString]: Integer readonly dispid 33;
    property CallInputDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString dispid 34;
    property CallOutputDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString dispid 35;
    property CallCaptureMicDevice[const CallId: WideString; DeviceType: TCallIoDeviceType]: WideString dispid 36;
    property CallInputStatus[const CallId: WideString]: WordBool readonly dispid 37;
    property CallForwardedBy[const CallId: WideString]: WideString readonly dispid 38;
    property CallCanTransfer[const CallId: WideString; const Target: WideString]: WordBool readonly dispid 39;
    property CallTransferStatus[const CallId: WideString]: TCallStatus readonly dispid 40;
    property CallTransferActive[const CallId: WideString]: WordBool readonly dispid 41;
    property CallTransferredBy[const CallId: WideString]: WideString readonly dispid 42;
    property CallTransferredTo[const CallId: WideString]: WideString readonly dispid 43;
    procedure TransferCall(const CallId: WideString; const Target: WideString); dispid 44;
    property CallTargetIdentity[const CallId: WideString]: WideString readonly dispid 46;
  end;

// *********************************************************************//
// Interface: ISkypeChat
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3F6758D2-1D3C-4A8F-BD99-6FC6B0E2DC8F}
// *********************************************************************//
  ISkypeChat = interface(IDispatch)
    ['{3F6758D2-1D3C-4A8F-BD99-6FC6B0E2DC8F}']
    function Get_ChatTimestamp(const ChatName: WideString): TDateTime; safecall;
    function Get_ChatAdder(const ChatName: WideString): IUser; safecall;
    function Get_ChatStatus(const ChatName: WideString): TChatStatus; safecall;
    function Get_ChatPosters(const ChatName: WideString): IUserCollection; safecall;
    function Get_ChatMembers(const ChatName: WideString): IUserCollection; safecall;
    function Get_ChatTopic(const ChatName: WideString): WideString; safecall;
    procedure Set_ChatTopic(const ChatName: WideString; const pVal: WideString); safecall;
    function Get_ChatActiveMembers(const ChatName: WideString): IUserCollection; safecall;
    function Get_ChatFriendlyName(const ChatName: WideString): WideString; safecall;
    function Get_ChatMessages(const ChatName: WideString): IChatMessageCollection; safecall;
    procedure OpenChatWindow(const ChatName: WideString); safecall;
    function SendChatMessage(const ChatName: WideString; const MessageText: WideString): IChatMessage; safecall;
    procedure LeaveChat(const ChatName: WideString); safecall;
    procedure AddChatMembers(const ChatName: WideString; const pMembers: IUserCollection); safecall;
    function Get_ChatRecentMessages(const ChatName: WideString): IChatMessageCollection; safecall;
    function Get_ChatBookmarked(const ChatName: WideString): WordBool; safecall;
    procedure BookmarkChat(const ChatName: WideString); safecall;
    procedure UnbookmarkChat(const ChatName: WideString); safecall;
    function Get_ChatTopicXML(const ChatName: WideString): WideString; safecall;
    procedure Set_ChatTopicXML(const ChatName: WideString; const pVal: WideString); safecall;
    function Get_ChatMemberObjects(const ChatName: WideString): IChatMemberCollection; safecall;
    function Get_ChatBlob(const ChatName: WideString): WideString; safecall;
    function Get_ChatOptions(const ChatName: WideString): Integer; safecall;
    procedure Set_ChatOptions(const ChatName: WideString; pVal: Integer); safecall;
    function Get_ChatPasswordHint(const ChatName: WideString): WideString; safecall;
    procedure SetChatPassword(const ChatName: WideString; const Password: WideString; 
                              const Hint: WideString); safecall;
    function Get_ChatGuideLines(const ChatName: WideString): WideString; safecall;
    procedure Set_ChatGuideLines(const ChatName: WideString; const pVal: WideString); safecall;
    function Get_ChatDescription(const ChatName: WideString): WideString; safecall;
    procedure Set_ChatDescription(const ChatName: WideString; const pVal: WideString); safecall;
    function Get_ChatDialogPartner(const ChatName: WideString): WideString; safecall;
    function Get_ChatActivityTimestamp(const ChatName: WideString): TDateTime; safecall;
    function Get_ChatMyRole(const ChatName: WideString): TChatMemberRole; safecall;
    function Get_ChatApplicants(const ChatName: WideString): IUserCollection; safecall;
    procedure JoinChat(const ChatName: WideString); safecall;
    procedure KickChatMember(const ChatName: WideString; const Handle: WideString); safecall;
    procedure KickBanChatMember(const ChatName: WideString; const Handle: WideString); safecall;
    procedure DisbandChat(const ChatName: WideString); safecall;
    procedure EnterChatPassword(const ChatName: WideString; const Password: WideString); safecall;
    procedure ClearChatRecentMessages(const ChatName: WideString); safecall;
    procedure AcceptSharedGroupAdd(const ChatName: WideString); safecall;
    procedure Set_ChatAlertString(const ChatName: WideString; const Param2: WideString); safecall;
    function Get_ChatType(const ChatName: WideString): TChatType; safecall;
    function Get_ChatMyStatus(const ChatName: WideString): TChatMyStatus; safecall;
    property ChatTimestamp[const ChatName: WideString]: TDateTime read Get_ChatTimestamp;
    property ChatAdder[const ChatName: WideString]: IUser read Get_ChatAdder;
    property ChatStatus[const ChatName: WideString]: TChatStatus read Get_ChatStatus;
    property ChatPosters[const ChatName: WideString]: IUserCollection read Get_ChatPosters;
    property ChatMembers[const ChatName: WideString]: IUserCollection read Get_ChatMembers;
    property ChatTopic[const ChatName: WideString]: WideString read Get_ChatTopic write Set_ChatTopic;
    property ChatActiveMembers[const ChatName: WideString]: IUserCollection read Get_ChatActiveMembers;
    property ChatFriendlyName[const ChatName: WideString]: WideString read Get_ChatFriendlyName;
    property ChatMessages[const ChatName: WideString]: IChatMessageCollection read Get_ChatMessages;
    property ChatRecentMessages[const ChatName: WideString]: IChatMessageCollection read Get_ChatRecentMessages;
    property ChatBookmarked[const ChatName: WideString]: WordBool read Get_ChatBookmarked;
    property ChatTopicXML[const ChatName: WideString]: WideString read Get_ChatTopicXML write Set_ChatTopicXML;
    property ChatMemberObjects[const ChatName: WideString]: IChatMemberCollection read Get_ChatMemberObjects;
    property ChatBlob[const ChatName: WideString]: WideString read Get_ChatBlob;
    property ChatOptions[const ChatName: WideString]: Integer read Get_ChatOptions write Set_ChatOptions;
    property ChatPasswordHint[const ChatName: WideString]: WideString read Get_ChatPasswordHint;
    property ChatGuideLines[const ChatName: WideString]: WideString read Get_ChatGuideLines write Set_ChatGuideLines;
    property ChatDescription[const ChatName: WideString]: WideString read Get_ChatDescription write Set_ChatDescription;
    property ChatDialogPartner[const ChatName: WideString]: WideString read Get_ChatDialogPartner;
    property ChatActivityTimestamp[const ChatName: WideString]: TDateTime read Get_ChatActivityTimestamp;
    property ChatMyRole[const ChatName: WideString]: TChatMemberRole read Get_ChatMyRole;
    property ChatApplicants[const ChatName: WideString]: IUserCollection read Get_ChatApplicants;
    property ChatAlertString[const ChatName: WideString]: WideString write Set_ChatAlertString;
    property ChatType[const ChatName: WideString]: TChatType read Get_ChatType;
    property ChatMyStatus[const ChatName: WideString]: TChatMyStatus read Get_ChatMyStatus;
  end;

// *********************************************************************//
// DispIntf:  ISkypeChatDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3F6758D2-1D3C-4A8F-BD99-6FC6B0E2DC8F}
// *********************************************************************//
  ISkypeChatDisp = dispinterface
    ['{3F6758D2-1D3C-4A8F-BD99-6FC6B0E2DC8F}']
    property ChatTimestamp[const ChatName: WideString]: TDateTime readonly dispid 1;
    property ChatAdder[const ChatName: WideString]: IUser readonly dispid 2;
    property ChatStatus[const ChatName: WideString]: TChatStatus readonly dispid 3;
    property ChatPosters[const ChatName: WideString]: IUserCollection readonly dispid 4;
    property ChatMembers[const ChatName: WideString]: IUserCollection readonly dispid 5;
    property ChatTopic[const ChatName: WideString]: WideString dispid 6;
    property ChatActiveMembers[const ChatName: WideString]: IUserCollection readonly dispid 7;
    property ChatFriendlyName[const ChatName: WideString]: WideString readonly dispid 8;
    property ChatMessages[const ChatName: WideString]: IChatMessageCollection readonly dispid 9;
    procedure OpenChatWindow(const ChatName: WideString); dispid 10;
    function SendChatMessage(const ChatName: WideString; const MessageText: WideString): IChatMessage; dispid 11;
    procedure LeaveChat(const ChatName: WideString); dispid 12;
    procedure AddChatMembers(const ChatName: WideString; const pMembers: IUserCollection); dispid 13;
    property ChatRecentMessages[const ChatName: WideString]: IChatMessageCollection readonly dispid 14;
    property ChatBookmarked[const ChatName: WideString]: WordBool readonly dispid 15;
    procedure BookmarkChat(const ChatName: WideString); dispid 16;
    procedure UnbookmarkChat(const ChatName: WideString); dispid 17;
    property ChatTopicXML[const ChatName: WideString]: WideString dispid 18;
    property ChatMemberObjects[const ChatName: WideString]: IChatMemberCollection readonly dispid 19;
    property ChatBlob[const ChatName: WideString]: WideString readonly dispid 20;
    property ChatOptions[const ChatName: WideString]: Integer dispid 21;
    property ChatPasswordHint[const ChatName: WideString]: WideString readonly dispid 22;
    procedure SetChatPassword(const ChatName: WideString; const Password: WideString; 
                              const Hint: WideString); dispid 23;
    property ChatGuideLines[const ChatName: WideString]: WideString dispid 24;
    property ChatDescription[const ChatName: WideString]: WideString dispid 25;
    property ChatDialogPartner[const ChatName: WideString]: WideString readonly dispid 26;
    property ChatActivityTimestamp[const ChatName: WideString]: TDateTime readonly dispid 27;
    property ChatMyRole[const ChatName: WideString]: TChatMemberRole readonly dispid 28;
    property ChatApplicants[const ChatName: WideString]: IUserCollection readonly dispid 29;
    procedure JoinChat(const ChatName: WideString); dispid 30;
    procedure KickChatMember(const ChatName: WideString; const Handle: WideString); dispid 31;
    procedure KickBanChatMember(const ChatName: WideString; const Handle: WideString); dispid 32;
    procedure DisbandChat(const ChatName: WideString); dispid 33;
    procedure EnterChatPassword(const ChatName: WideString; const Password: WideString); dispid 34;
    procedure ClearChatRecentMessages(const ChatName: WideString); dispid 35;
    procedure AcceptSharedGroupAdd(const ChatName: WideString); dispid 36;
    property ChatAlertString[const ChatName: WideString]: WideString writeonly dispid 37;
    property ChatType[const ChatName: WideString]: TChatType readonly dispid 38;
    property ChatMyStatus[const ChatName: WideString]: TChatMyStatus readonly dispid 39;
  end;

// *********************************************************************//
// Interface: ISkypeChatMessage
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A569B841-CC8A-4C12-B937-CBC17D9E64F0}
// *********************************************************************//
  ISkypeChatMessage = interface(IDispatch)
    ['{A569B841-CC8A-4C12-B937-CBC17D9E64F0}']
    function Get_ChatMessageTimestamp(const MessageId: WideString): TDateTime; safecall;
    function Get_ChatMessageFromHandle(const MessageId: WideString): WideString; safecall;
    function Get_ChatMessageFromDisplayName(const MessageId: WideString): WideString; safecall;
    function Get_ChatMessageType(const MessageId: WideString): TChatMessageType; safecall;
    function Get_ChatMessageStatus(const MessageId: WideString): TChatMessageStatus; safecall;
    function Get_ChatMessageLeaveReason(const MessageId: WideString): TChatLeaveReason; safecall;
    function Get_ChatMessageBody(const MessageId: WideString): WideString; safecall;
    function Get_ChatMessageChatname(const MessageId: WideString): WideString; safecall;
    function Get_ChatMessageUsers(const MessageId: WideString): IUserCollection; safecall;
    procedure Set_ChatMessageSeen(const Param1: WideString); safecall;
    function Get_ChatMessageSender(const MessageId: WideString): IUser; safecall;
    function Get_ChatMessageIsEditable(const MessageId: WideString): WordBool; safecall;
    function Get_ChatMessageEditedBy(const MessageId: WideString): WideString; safecall;
    function Get_ChatMessageEditedTimestamp(const MessageId: WideString): TDateTime; safecall;
    procedure Set_ChatMessageBody(const MessageId: WideString; const pVal: WideString); safecall;
    function Get_ChatMessageRole(const MessageId: WideString): TChatMemberRole; safecall;
    function Get_ChatMessageOptons(const MessageId: WideString): Integer; safecall;
    property ChatMessageTimestamp[const MessageId: WideString]: TDateTime read Get_ChatMessageTimestamp;
    property ChatMessageFromHandle[const MessageId: WideString]: WideString read Get_ChatMessageFromHandle;
    property ChatMessageFromDisplayName[const MessageId: WideString]: WideString read Get_ChatMessageFromDisplayName;
    property ChatMessageType[const MessageId: WideString]: TChatMessageType read Get_ChatMessageType;
    property ChatMessageStatus[const MessageId: WideString]: TChatMessageStatus read Get_ChatMessageStatus;
    property ChatMessageLeaveReason[const MessageId: WideString]: TChatLeaveReason read Get_ChatMessageLeaveReason;
    property ChatMessageBody[const MessageId: WideString]: WideString read Get_ChatMessageBody write Set_ChatMessageBody;
    property ChatMessageChatname[const MessageId: WideString]: WideString read Get_ChatMessageChatname;
    property ChatMessageUsers[const MessageId: WideString]: IUserCollection read Get_ChatMessageUsers;
    property ChatMessageSeen: WideString write Set_ChatMessageSeen;
    property ChatMessageSender[const MessageId: WideString]: IUser read Get_ChatMessageSender;
    property ChatMessageIsEditable[const MessageId: WideString]: WordBool read Get_ChatMessageIsEditable;
    property ChatMessageEditedBy[const MessageId: WideString]: WideString read Get_ChatMessageEditedBy;
    property ChatMessageEditedTimestamp[const MessageId: WideString]: TDateTime read Get_ChatMessageEditedTimestamp;
    property ChatMessageRole[const MessageId: WideString]: TChatMemberRole read Get_ChatMessageRole;
    property ChatMessageOptons[const MessageId: WideString]: Integer read Get_ChatMessageOptons;
  end;

// *********************************************************************//
// DispIntf:  ISkypeChatMessageDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {A569B841-CC8A-4C12-B937-CBC17D9E64F0}
// *********************************************************************//
  ISkypeChatMessageDisp = dispinterface
    ['{A569B841-CC8A-4C12-B937-CBC17D9E64F0}']
    property ChatMessageTimestamp[const MessageId: WideString]: TDateTime readonly dispid 1;
    property ChatMessageFromHandle[const MessageId: WideString]: WideString readonly dispid 2;
    property ChatMessageFromDisplayName[const MessageId: WideString]: WideString readonly dispid 3;
    property ChatMessageType[const MessageId: WideString]: TChatMessageType readonly dispid 4;
    property ChatMessageStatus[const MessageId: WideString]: TChatMessageStatus readonly dispid 5;
    property ChatMessageLeaveReason[const MessageId: WideString]: TChatLeaveReason readonly dispid 6;
    property ChatMessageBody[const MessageId: WideString]: WideString dispid 7;
    property ChatMessageChatname[const MessageId: WideString]: WideString readonly dispid 8;
    property ChatMessageUsers[const MessageId: WideString]: IUserCollection readonly dispid 9;
    property ChatMessageSeen: WideString writeonly dispid 10;
    property ChatMessageSender[const MessageId: WideString]: IUser readonly dispid 11;
    property ChatMessageIsEditable[const MessageId: WideString]: WordBool readonly dispid 12;
    property ChatMessageEditedBy[const MessageId: WideString]: WideString readonly dispid 13;
    property ChatMessageEditedTimestamp[const MessageId: WideString]: TDateTime readonly dispid 14;
    property ChatMessageRole[const MessageId: WideString]: TChatMemberRole readonly dispid 15;
    property ChatMessageOptons[const MessageId: WideString]: Integer readonly dispid 16;
  end;

// *********************************************************************//
// Interface: ISkypeChatMember
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6CF6DBDE-AB7C-4635-96ED-2FF525AB4457}
// *********************************************************************//
  ISkypeChatMember = interface(IDispatch)
    ['{6CF6DBDE-AB7C-4635-96ED-2FF525AB4457}']
    function Get_ChatMemberHandle(const MemberId: WideString): WideString; safecall;
    function Get_ChatMemberRole(const MemberId: WideString): TChatMemberRole; safecall;
    procedure Set_ChatMemberRole(const MemberId: WideString; pVal: TChatMemberRole); safecall;
    function Get_CanSetChatMemberRoleTo(const MemberId: WideString; Role: TChatMemberRole): WordBool; safecall;
    function Get_IsChatMemberActive(const MemberId: WideString): WordBool; safecall;
    function Get_ChatMemberChat(const MemberId: WideString): IChat; safecall;
    property ChatMemberHandle[const MemberId: WideString]: WideString read Get_ChatMemberHandle;
    property ChatMemberRole[const MemberId: WideString]: TChatMemberRole read Get_ChatMemberRole write Set_ChatMemberRole;
    property CanSetChatMemberRoleTo[const MemberId: WideString; Role: TChatMemberRole]: WordBool read Get_CanSetChatMemberRoleTo;
    property IsChatMemberActive[const MemberId: WideString]: WordBool read Get_IsChatMemberActive;
    property ChatMemberChat[const MemberId: WideString]: IChat read Get_ChatMemberChat;
  end;

// *********************************************************************//
// DispIntf:  ISkypeChatMemberDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6CF6DBDE-AB7C-4635-96ED-2FF525AB4457}
// *********************************************************************//
  ISkypeChatMemberDisp = dispinterface
    ['{6CF6DBDE-AB7C-4635-96ED-2FF525AB4457}']
    property ChatMemberHandle[const MemberId: WideString]: WideString readonly dispid 1;
    property ChatMemberRole[const MemberId: WideString]: TChatMemberRole dispid 2;
    property CanSetChatMemberRoleTo[const MemberId: WideString; Role: TChatMemberRole]: WordBool readonly dispid 3;
    property IsChatMemberActive[const MemberId: WideString]: WordBool readonly dispid 4;
    property ChatMemberChat[const MemberId: WideString]: IChat readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ISkypeConference
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {30678F8A-C104-40C0-A6AE-7C150E83300E}
// *********************************************************************//
  ISkypeConference = interface(IDispatch)
    ['{30678F8A-C104-40C0-A6AE-7C150E83300E}']
    function Get_ConferenceCalls(const ConfId: WideString): ICallCollection; safecall;
    function Get_ConferenceActiveCalls(const ConfId: WideString): ICallCollection; safecall;
    property ConferenceCalls[const ConfId: WideString]: ICallCollection read Get_ConferenceCalls;
    property ConferenceActiveCalls[const ConfId: WideString]: ICallCollection read Get_ConferenceActiveCalls;
  end;

// *********************************************************************//
// DispIntf:  ISkypeConferenceDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {30678F8A-C104-40C0-A6AE-7C150E83300E}
// *********************************************************************//
  ISkypeConferenceDisp = dispinterface
    ['{30678F8A-C104-40C0-A6AE-7C150E83300E}']
    property ConferenceCalls[const ConfId: WideString]: ICallCollection readonly dispid 1;
    property ConferenceActiveCalls[const ConfId: WideString]: ICallCollection readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ISkypeFileTransfer
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {2B929860-6AF4-40DF-9D77-308CA7C23E66}
// *********************************************************************//
  ISkypeFileTransfer = interface(IDispatch)
    ['{2B929860-6AF4-40DF-9D77-308CA7C23E66}']
    function Get_FileTransferType(const TransferId: WideString): TFileTransferType; safecall;
    function Get_FileTransferStatus(const TransferId: WideString): TFileTransferStatus; safecall;
    function Get_FileTransferFailureReason(const TransferId: WideString): TFileTransferFailureReason; safecall;
    function Get_FileTransferPartnerHandle(const TransferId: WideString): WideString; safecall;
    function Get_FileTransferPartnerDisplayName(const TransferId: WideString): WideString; safecall;
    function Get_FileTransferStartTime(const TransferId: WideString): TDateTime; safecall;
    function Get_FileTransferFinishTime(const TransferId: WideString): TDateTime; safecall;
    function Get_FileTransferFilePath(const TransferId: WideString): WideString; safecall;
    function Get_FileTransferFileName(const TransferId: WideString): WideString; safecall;
    function Get_FileTransferBytesPerSecond(const TransferId: WideString): WideString; safecall;
    function Get_FileTransferBytesTransferred(const TransferId: WideString): WideString; safecall;
    property FileTransferType[const TransferId: WideString]: TFileTransferType read Get_FileTransferType;
    property FileTransferStatus[const TransferId: WideString]: TFileTransferStatus read Get_FileTransferStatus;
    property FileTransferFailureReason[const TransferId: WideString]: TFileTransferFailureReason read Get_FileTransferFailureReason;
    property FileTransferPartnerHandle[const TransferId: WideString]: WideString read Get_FileTransferPartnerHandle;
    property FileTransferPartnerDisplayName[const TransferId: WideString]: WideString read Get_FileTransferPartnerDisplayName;
    property FileTransferStartTime[const TransferId: WideString]: TDateTime read Get_FileTransferStartTime;
    property FileTransferFinishTime[const TransferId: WideString]: TDateTime read Get_FileTransferFinishTime;
    property FileTransferFilePath[const TransferId: WideString]: WideString read Get_FileTransferFilePath;
    property FileTransferFileName[const TransferId: WideString]: WideString read Get_FileTransferFileName;
    property FileTransferBytesPerSecond[const TransferId: WideString]: WideString read Get_FileTransferBytesPerSecond;
    property FileTransferBytesTransferred[const TransferId: WideString]: WideString read Get_FileTransferBytesTransferred;
  end;

// *********************************************************************//
// DispIntf:  ISkypeFileTransferDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {2B929860-6AF4-40DF-9D77-308CA7C23E66}
// *********************************************************************//
  ISkypeFileTransferDisp = dispinterface
    ['{2B929860-6AF4-40DF-9D77-308CA7C23E66}']
    property FileTransferType[const TransferId: WideString]: TFileTransferType readonly dispid 1;
    property FileTransferStatus[const TransferId: WideString]: TFileTransferStatus readonly dispid 2;
    property FileTransferFailureReason[const TransferId: WideString]: TFileTransferFailureReason readonly dispid 3;
    property FileTransferPartnerHandle[const TransferId: WideString]: WideString readonly dispid 4;
    property FileTransferPartnerDisplayName[const TransferId: WideString]: WideString readonly dispid 5;
    property FileTransferStartTime[const TransferId: WideString]: TDateTime readonly dispid 6;
    property FileTransferFinishTime[const TransferId: WideString]: TDateTime readonly dispid 7;
    property FileTransferFilePath[const TransferId: WideString]: WideString readonly dispid 8;
    property FileTransferFileName[const TransferId: WideString]: WideString readonly dispid 9;
    property FileTransferBytesPerSecond[const TransferId: WideString]: WideString readonly dispid 10;
    property FileTransferBytesTransferred[const TransferId: WideString]: WideString readonly dispid 11;
  end;

// *********************************************************************//
// Interface: ISkypeSms
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6D0B4547-771D-4C4F-B3E8-98A33FD24D2B}
// *********************************************************************//
  ISkypeSms = interface(IDispatch)
    ['{6D0B4547-771D-4C4F-B3E8-98A33FD24D2B}']
    function Get_SmsMessageType(const SmsId: WideString): TSmsMessageType; safecall;
    function Get_SmsMessageStatus(const SmsId: WideString): TSmsMessageStatus; safecall;
    function Get_SmsFailureReason(const SmsId: WideString): TSmsFailureReason; safecall;
    function Get_SmsIsFailedUnseen(const SmsId: WideString): WordBool; safecall;
    procedure Set_SmsSeen(const Param1: WideString); safecall;
    function Get_SmsPrice(const SmsId: WideString): Integer; safecall;
    function Get_SmsPriceCurrency(const SmsId: WideString): WideString; safecall;
    function Get_SmsPricePrecision(const SmsId: WideString): Integer; safecall;
    function Get_SmsReplyToNumber(const SmsId: WideString): WideString; safecall;
    procedure Set_SmsReplyToNumber(const SmsId: WideString; const pVal: WideString); safecall;
    function Get_SmsTargets(const SmsId: WideString): ISmsTargetCollection; safecall;
    function Get_SmsBody(const SmsId: WideString): WideString; safecall;
    procedure Set_SmsBody(const SmsId: WideString; const pVal: WideString); safecall;
    function Get_SmsChunks(const SmsId: WideString): ISmsChunkCollection; safecall;
    procedure SendSmsMessage(const SmsId: WideString); safecall;
    procedure DeleteSmsMessage(const SmsId: WideString); safecall;
    function Get_SmsTargetStatus(const SmsId: WideString; const Number: WideString): TSmsTargetStatus; safecall;
    function Get_SmsChunkText(const SmsId: WideString; const ChunkId: WideString): WideString; safecall;
    function Get_SmsChunkCharactersLeft(const SmsId: WideString; const ChunkId: WideString): Integer; safecall;
    function Get_SmsTimestamp(const SmsId: WideString): TDateTime; safecall;
    function Get_SmsTargetNumbers(const SmsId: WideString): WideString; safecall;
    procedure Set_SmsTargetNumbers(const SmsId: WideString; const pVal: WideString); safecall;
    property SmsMessageType[const SmsId: WideString]: TSmsMessageType read Get_SmsMessageType;
    property SmsMessageStatus[const SmsId: WideString]: TSmsMessageStatus read Get_SmsMessageStatus;
    property SmsFailureReason[const SmsId: WideString]: TSmsFailureReason read Get_SmsFailureReason;
    property SmsIsFailedUnseen[const SmsId: WideString]: WordBool read Get_SmsIsFailedUnseen;
    property SmsSeen: WideString write Set_SmsSeen;
    property SmsPrice[const SmsId: WideString]: Integer read Get_SmsPrice;
    property SmsPriceCurrency[const SmsId: WideString]: WideString read Get_SmsPriceCurrency;
    property SmsPricePrecision[const SmsId: WideString]: Integer read Get_SmsPricePrecision;
    property SmsReplyToNumber[const SmsId: WideString]: WideString read Get_SmsReplyToNumber write Set_SmsReplyToNumber;
    property SmsTargets[const SmsId: WideString]: ISmsTargetCollection read Get_SmsTargets;
    property SmsBody[const SmsId: WideString]: WideString read Get_SmsBody write Set_SmsBody;
    property SmsChunks[const SmsId: WideString]: ISmsChunkCollection read Get_SmsChunks;
    property SmsTargetStatus[const SmsId: WideString; const Number: WideString]: TSmsTargetStatus read Get_SmsTargetStatus;
    property SmsChunkText[const SmsId: WideString; const ChunkId: WideString]: WideString read Get_SmsChunkText;
    property SmsChunkCharactersLeft[const SmsId: WideString; const ChunkId: WideString]: Integer read Get_SmsChunkCharactersLeft;
    property SmsTimestamp[const SmsId: WideString]: TDateTime read Get_SmsTimestamp;
    property SmsTargetNumbers[const SmsId: WideString]: WideString read Get_SmsTargetNumbers write Set_SmsTargetNumbers;
  end;

// *********************************************************************//
// DispIntf:  ISkypeSmsDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6D0B4547-771D-4C4F-B3E8-98A33FD24D2B}
// *********************************************************************//
  ISkypeSmsDisp = dispinterface
    ['{6D0B4547-771D-4C4F-B3E8-98A33FD24D2B}']
    property SmsMessageType[const SmsId: WideString]: TSmsMessageType readonly dispid 1;
    property SmsMessageStatus[const SmsId: WideString]: TSmsMessageStatus readonly dispid 2;
    property SmsFailureReason[const SmsId: WideString]: TSmsFailureReason readonly dispid 3;
    property SmsIsFailedUnseen[const SmsId: WideString]: WordBool readonly dispid 4;
    property SmsSeen: WideString writeonly dispid 5;
    property SmsPrice[const SmsId: WideString]: Integer readonly dispid 6;
    property SmsPriceCurrency[const SmsId: WideString]: WideString readonly dispid 7;
    property SmsPricePrecision[const SmsId: WideString]: Integer readonly dispid 8;
    property SmsReplyToNumber[const SmsId: WideString]: WideString dispid 9;
    property SmsTargets[const SmsId: WideString]: ISmsTargetCollection readonly dispid 10;
    property SmsBody[const SmsId: WideString]: WideString dispid 11;
    property SmsChunks[const SmsId: WideString]: ISmsChunkCollection readonly dispid 12;
    procedure SendSmsMessage(const SmsId: WideString); dispid 13;
    procedure DeleteSmsMessage(const SmsId: WideString); dispid 14;
    property SmsTargetStatus[const SmsId: WideString; const Number: WideString]: TSmsTargetStatus readonly dispid 15;
    property SmsChunkText[const SmsId: WideString; const ChunkId: WideString]: WideString readonly dispid 16;
    property SmsChunkCharactersLeft[const SmsId: WideString; const ChunkId: WideString]: Integer readonly dispid 17;
    property SmsTimestamp[const SmsId: WideString]: TDateTime readonly dispid 18;
    property SmsTargetNumbers[const SmsId: WideString]: WideString dispid 19;
  end;

// *********************************************************************//
// Interface: ISkypeUser
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C219279C-F557-4BAD-B3BE-750E91CA9944}
// *********************************************************************//
  ISkypeUser = interface(IDispatch)
    ['{C219279C-F557-4BAD-B3BE-750E91CA9944}']
    function Get_UserFullName(const Username: WideString): WideString; safecall;
    function Get_UserBirthday(const Username: WideString): TDateTime; safecall;
    function Get_UserSex(const Username: WideString): TUserSex; safecall;
    function Get_UserCountry(const Username: WideString): WideString; safecall;
    function Get_UserProvince(const Username: WideString): WideString; safecall;
    function Get_UserCity(const Username: WideString): WideString; safecall;
    function Get_UserPhoneHome(const Username: WideString): WideString; safecall;
    function Get_UserPhoneOffice(const Username: WideString): WideString; safecall;
    function Get_UserPhoneMobile(const Username: WideString): WideString; safecall;
    function Get_UserHomepage(const Username: WideString): WideString; safecall;
    function Get_UserAbout(const Username: WideString): WideString; safecall;
    function Get_UserHasCallEquipment(const Username: WideString): WordBool; safecall;
    function Get_UserBuddyStatus(const Username: WideString): TBuddyStatus; safecall;
    procedure Set_UserBuddyStatus(const Username: WideString; pVal: TBuddyStatus); safecall;
    function Get_UserIsAuthorized(const Username: WideString): WordBool; safecall;
    procedure Set_UserIsAuthorized(const Username: WideString; pVal: WordBool); safecall;
    function Get_UserIsBlocked(const Username: WideString): WordBool; safecall;
    procedure Set_UserIsBlocked(const Username: WideString; pVal: WordBool); safecall;
    function Get_UserDisplayName(const Username: WideString): WideString; safecall;
    function Get_UserOnlineStatus(const Username: WideString): TOnlineStatus; safecall;
    function Get_UserLastOnline(const Username: WideString): TDateTime; safecall;
    function Get_UserCountryCode(const Username: WideString): WideString; safecall;
    function Get_UserReceivedAuthRequest(const Username: WideString): WideString; safecall;
    function Get_UserSpeedDial(const Username: WideString): WideString; safecall;
    procedure Set_UserSpeedDial(const Username: WideString; const pVal: WideString); safecall;
    function Get_UserCanLeaveVoicemail(const Username: WideString): WordBool; safecall;
    function Get_UserMoodText(const Username: WideString): WideString; safecall;
    function Get_UserAliases(const Username: WideString): WideString; safecall;
    function Get_UserTimezone(const Username: WideString): Integer; safecall;
    function Get_IsCallForwardActive(const Username: WideString): WordBool; safecall;
    function Get_UserLanguage(const Username: WideString): WideString; safecall;
    function Get_UserLanguageCode(const Username: WideString): WideString; safecall;
    function Get_UserIsVideoCapable(const Username: WideString): WordBool; safecall;
    function Get_UserNumberOfAuthBuddies(const Username: WideString): Integer; safecall;
    procedure Set_UserDisplayName(const Username: WideString; const pVal: WideString); safecall;
    function Get_UserRichMoodText(const Username: WideString): WideString; safecall;
    function Get_UserIsVoicemailCapable(const Username: WideString): WordBool; safecall;
    property UserFullName[const Username: WideString]: WideString read Get_UserFullName;
    property UserBirthday[const Username: WideString]: TDateTime read Get_UserBirthday;
    property UserSex[const Username: WideString]: TUserSex read Get_UserSex;
    property UserCountry[const Username: WideString]: WideString read Get_UserCountry;
    property UserProvince[const Username: WideString]: WideString read Get_UserProvince;
    property UserCity[const Username: WideString]: WideString read Get_UserCity;
    property UserPhoneHome[const Username: WideString]: WideString read Get_UserPhoneHome;
    property UserPhoneOffice[const Username: WideString]: WideString read Get_UserPhoneOffice;
    property UserPhoneMobile[const Username: WideString]: WideString read Get_UserPhoneMobile;
    property UserHomepage[const Username: WideString]: WideString read Get_UserHomepage;
    property UserAbout[const Username: WideString]: WideString read Get_UserAbout;
    property UserHasCallEquipment[const Username: WideString]: WordBool read Get_UserHasCallEquipment;
    property UserBuddyStatus[const Username: WideString]: TBuddyStatus read Get_UserBuddyStatus write Set_UserBuddyStatus;
    property UserIsAuthorized[const Username: WideString]: WordBool read Get_UserIsAuthorized write Set_UserIsAuthorized;
    property UserIsBlocked[const Username: WideString]: WordBool read Get_UserIsBlocked write Set_UserIsBlocked;
    property UserDisplayName[const Username: WideString]: WideString read Get_UserDisplayName write Set_UserDisplayName;
    property UserOnlineStatus[const Username: WideString]: TOnlineStatus read Get_UserOnlineStatus;
    property UserLastOnline[const Username: WideString]: TDateTime read Get_UserLastOnline;
    property UserCountryCode[const Username: WideString]: WideString read Get_UserCountryCode;
    property UserReceivedAuthRequest[const Username: WideString]: WideString read Get_UserReceivedAuthRequest;
    property UserSpeedDial[const Username: WideString]: WideString read Get_UserSpeedDial write Set_UserSpeedDial;
    property UserCanLeaveVoicemail[const Username: WideString]: WordBool read Get_UserCanLeaveVoicemail;
    property UserMoodText[const Username: WideString]: WideString read Get_UserMoodText;
    property UserAliases[const Username: WideString]: WideString read Get_UserAliases;
    property UserTimezone[const Username: WideString]: Integer read Get_UserTimezone;
    property IsCallForwardActive[const Username: WideString]: WordBool read Get_IsCallForwardActive;
    property UserLanguage[const Username: WideString]: WideString read Get_UserLanguage;
    property UserLanguageCode[const Username: WideString]: WideString read Get_UserLanguageCode;
    property UserIsVideoCapable[const Username: WideString]: WordBool read Get_UserIsVideoCapable;
    property UserNumberOfAuthBuddies[const Username: WideString]: Integer read Get_UserNumberOfAuthBuddies;
    property UserRichMoodText[const Username: WideString]: WideString read Get_UserRichMoodText;
    property UserIsVoicemailCapable[const Username: WideString]: WordBool read Get_UserIsVoicemailCapable;
  end;

// *********************************************************************//
// DispIntf:  ISkypeUserDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C219279C-F557-4BAD-B3BE-750E91CA9944}
// *********************************************************************//
  ISkypeUserDisp = dispinterface
    ['{C219279C-F557-4BAD-B3BE-750E91CA9944}']
    property UserFullName[const Username: WideString]: WideString readonly dispid 1;
    property UserBirthday[const Username: WideString]: TDateTime readonly dispid 2;
    property UserSex[const Username: WideString]: TUserSex readonly dispid 3;
    property UserCountry[const Username: WideString]: WideString readonly dispid 4;
    property UserProvince[const Username: WideString]: WideString readonly dispid 5;
    property UserCity[const Username: WideString]: WideString readonly dispid 6;
    property UserPhoneHome[const Username: WideString]: WideString readonly dispid 7;
    property UserPhoneOffice[const Username: WideString]: WideString readonly dispid 8;
    property UserPhoneMobile[const Username: WideString]: WideString readonly dispid 9;
    property UserHomepage[const Username: WideString]: WideString readonly dispid 10;
    property UserAbout[const Username: WideString]: WideString readonly dispid 11;
    property UserHasCallEquipment[const Username: WideString]: WordBool readonly dispid 12;
    property UserBuddyStatus[const Username: WideString]: TBuddyStatus dispid 13;
    property UserIsAuthorized[const Username: WideString]: WordBool dispid 14;
    property UserIsBlocked[const Username: WideString]: WordBool dispid 15;
    property UserDisplayName[const Username: WideString]: WideString dispid 16;
    property UserOnlineStatus[const Username: WideString]: TOnlineStatus readonly dispid 17;
    property UserLastOnline[const Username: WideString]: TDateTime readonly dispid 18;
    property UserCountryCode[const Username: WideString]: WideString readonly dispid 19;
    property UserReceivedAuthRequest[const Username: WideString]: WideString readonly dispid 20;
    property UserSpeedDial[const Username: WideString]: WideString dispid 21;
    property UserCanLeaveVoicemail[const Username: WideString]: WordBool readonly dispid 22;
    property UserMoodText[const Username: WideString]: WideString readonly dispid 23;
    property UserAliases[const Username: WideString]: WideString readonly dispid 24;
    property UserTimezone[const Username: WideString]: Integer readonly dispid 25;
    property IsCallForwardActive[const Username: WideString]: WordBool readonly dispid 26;
    property UserLanguage[const Username: WideString]: WideString readonly dispid 27;
    property UserLanguageCode[const Username: WideString]: WideString readonly dispid 28;
    property UserIsVideoCapable[const Username: WideString]: WordBool readonly dispid 29;
    property UserNumberOfAuthBuddies[const Username: WideString]: Integer readonly dispid 30;
    property UserRichMoodText[const Username: WideString]: WideString readonly dispid 31;
    property UserIsVoicemailCapable[const Username: WideString]: WordBool readonly dispid 32;
  end;

// *********************************************************************//
// Interface: ISkypeVoicemail
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4D33E14E-9921-4860-92F4-5DC1586F403C}
// *********************************************************************//
  ISkypeVoicemail = interface(IDispatch)
    ['{4D33E14E-9921-4860-92F4-5DC1586F403C}']
    function Get_VoicemailType(const VoicemailId: WideString): TVoicemailType; safecall;
    function Get_VoicemailPartnerHandle(const VoicemailId: WideString): WideString; safecall;
    function Get_VoicemailPartnerDisplayName(const VoicemailId: WideString): WideString; safecall;
    function Get_VoicemailStatus(const VoicemailId: WideString): TVoicemailStatus; safecall;
    function Get_VoicemailFailureReason(const VoicemailId: WideString): TVoicemailFailureReason; safecall;
    function Get_VoicemailTimestamp(const VoicemailId: WideString): TDateTime; safecall;
    function Get_VoicemailDuration(const VoicemailId: WideString): Integer; safecall;
    function Get_VoicemailAllowedDuration(const VoicemailId: WideString): Integer; safecall;
    procedure OpenVoicemail(const VoicemailId: WideString); safecall;
    procedure StartVoicemailPlayback(const VoicemailId: WideString); safecall;
    procedure StopVoicemailPlayback(const VoicemailId: WideString); safecall;
    procedure UploadVoicemail(const VoicemailId: WideString); safecall;
    procedure DownloadVoicemail(const VoicemailId: WideString); safecall;
    procedure StartVoicemailRecording(const VoicemailId: WideString); safecall;
    procedure StopVoicemailRecording(const VoicemailId: WideString); safecall;
    procedure DeleteVoicemail(const VoicemailId: WideString); safecall;
    procedure StartVoicemailPlaybackInCall(const VoicemailId: WideString); safecall;
    procedure SetVoicemailUnplayed(const VoicemailId: WideString); safecall;
    property VoicemailType[const VoicemailId: WideString]: TVoicemailType read Get_VoicemailType;
    property VoicemailPartnerHandle[const VoicemailId: WideString]: WideString read Get_VoicemailPartnerHandle;
    property VoicemailPartnerDisplayName[const VoicemailId: WideString]: WideString read Get_VoicemailPartnerDisplayName;
    property VoicemailStatus[const VoicemailId: WideString]: TVoicemailStatus read Get_VoicemailStatus;
    property VoicemailFailureReason[const VoicemailId: WideString]: TVoicemailFailureReason read Get_VoicemailFailureReason;
    property VoicemailTimestamp[const VoicemailId: WideString]: TDateTime read Get_VoicemailTimestamp;
    property VoicemailDuration[const VoicemailId: WideString]: Integer read Get_VoicemailDuration;
    property VoicemailAllowedDuration[const VoicemailId: WideString]: Integer read Get_VoicemailAllowedDuration;
  end;

// *********************************************************************//
// DispIntf:  ISkypeVoicemailDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4D33E14E-9921-4860-92F4-5DC1586F403C}
// *********************************************************************//
  ISkypeVoicemailDisp = dispinterface
    ['{4D33E14E-9921-4860-92F4-5DC1586F403C}']
    property VoicemailType[const VoicemailId: WideString]: TVoicemailType readonly dispid 1;
    property VoicemailPartnerHandle[const VoicemailId: WideString]: WideString readonly dispid 2;
    property VoicemailPartnerDisplayName[const VoicemailId: WideString]: WideString readonly dispid 3;
    property VoicemailStatus[const VoicemailId: WideString]: TVoicemailStatus readonly dispid 4;
    property VoicemailFailureReason[const VoicemailId: WideString]: TVoicemailFailureReason readonly dispid 5;
    property VoicemailTimestamp[const VoicemailId: WideString]: TDateTime readonly dispid 6;
    property VoicemailDuration[const VoicemailId: WideString]: Integer readonly dispid 7;
    property VoicemailAllowedDuration[const VoicemailId: WideString]: Integer readonly dispid 8;
    procedure OpenVoicemail(const VoicemailId: WideString); dispid 9;
    procedure StartVoicemailPlayback(const VoicemailId: WideString); dispid 10;
    procedure StopVoicemailPlayback(const VoicemailId: WideString); dispid 11;
    procedure UploadVoicemail(const VoicemailId: WideString); dispid 12;
    procedure DownloadVoicemail(const VoicemailId: WideString); dispid 13;
    procedure StartVoicemailRecording(const VoicemailId: WideString); dispid 14;
    procedure StopVoicemailRecording(const VoicemailId: WideString); dispid 15;
    procedure DeleteVoicemail(const VoicemailId: WideString); dispid 16;
    procedure StartVoicemailPlaybackInCall(const VoicemailId: WideString); dispid 17;
    procedure SetVoicemailUnplayed(const VoicemailId: WideString); dispid 18;
  end;

// *********************************************************************//
// DispIntf:  _ISkypeEvents
// Flags:     (4096) Dispatchable
// GUID:      {F4F90CDD-C620-4118-945E-CAA1BBEBA435}
// *********************************************************************//
  _ISkypeEvents = dispinterface
    ['{F4F90CDD-C620-4118-945E-CAA1BBEBA435}']
    procedure Command(const pCommand: ICommand); dispid 1;
    procedure Reply(const pCommand: ICommand); dispid 2;
    procedure Error(const pCommand: ICommand; Number: Integer; const Description: WideString); dispid 3;
    procedure AttachmentStatus(Status: TAttachmentStatus); dispid 4;
    procedure ConnectionStatus(Status: TConnectionStatus); dispid 5;
    procedure UserStatus(Status: TUserStatus); dispid 6;
    procedure OnlineStatus(const pUser: IUser; Status: TOnlineStatus); dispid 7;
    procedure CallStatus(const pCall: ICall; Status: TCallStatus); dispid 8;
    procedure CallHistory; dispid 9;
    procedure Mute(Mute: WordBool); dispid 10;
    procedure MessageStatus(const pMessage: IChatMessage; Status: TChatMessageStatus); dispid 11;
    procedure MessageHistory(const Username: WideString); dispid 12;
    procedure AutoAway(Automatic: WordBool); dispid 13;
    procedure CallDtmfReceived(const pCall: ICall; const code: WideString); dispid 14;
    procedure VoicemailStatus(const pMail: IVoicemail; Status: TVoicemailStatus); dispid 15;
    procedure ApplicationConnecting(const pApp: IApplication; const pUsers: IUserCollection); dispid 16;
    procedure ApplicationStreams(const pApp: IApplication; 
                                 const pStreams: IApplicationStreamCollection); dispid 17;
    procedure ApplicationDatagram(const pApp: IApplication; const pStream: IApplicationStream; 
                                  const Text: WideString); dispid 18;
    procedure ApplicationSending(const pApp: IApplication; 
                                 const pStreams: IApplicationStreamCollection); dispid 19;
    procedure ApplicationReceiving(const pApp: IApplication; 
                                   const pStreams: IApplicationStreamCollection); dispid 20;
    procedure ContactsFocused(const Username: WideString); dispid 21;
    procedure GroupVisible(const pGroup: IGroup; Visible: WordBool); dispid 22;
    procedure GroupExpanded(const pGroup: IGroup; Expanded: WordBool); dispid 23;
    procedure GroupUsers(const pGroup: IGroup; const pUsers: IUserCollection); dispid 24;
    procedure GroupDeleted(GroupId: Integer); dispid 25;
    procedure UserMood(const pUser: IUser; const MoodText: WideString); dispid 26;
    procedure SmsMessageStatusChanged(const pMessage: ISmsMessage; Status: TSmsMessageStatus); dispid 27;
    procedure SmsTargetStatusChanged(const pTarget: ISmsTarget; Status: TSmsTargetStatus); dispid 28;
    procedure CallInputStatusChanged(const pCall: ICall; Status: WordBool); dispid 29;
    procedure AsyncSearchUsersFinished(Cookie: Integer; const pUsers: IUserCollection); dispid 30;
    procedure CallSeenStatusChanged(const pCall: ICall; Status: WordBool); dispid 31;
    procedure PluginEventClicked(const pEvent: IPluginEvent); dispid 32;
    procedure PluginMenuItemClicked(const pMenuItem: IPluginMenuItem; 
                                    const pUsers: IUserCollection; PluginContext: TPluginContext; 
                                    const ContextId: WideString); dispid 33;
    procedure WallpaperChanged(const Path: WideString); dispid 34;
    procedure FileTransferStatusChanged(const pTransfer: IFileTransfer; Status: TFileTransferStatus); dispid 35;
    procedure CallTransferStatusChanged(const pCall: ICall; Status: TCallStatus); dispid 36;
    procedure ChatMembersChanged(const pChat: IChat; const pMembers: IUserCollection); dispid 37;
    procedure ChatMemberRoleChanged(const pMember: IChatMember; Role: TChatMemberRole); dispid 38;
    procedure CallVideoStatusChanged(const pCall: ICall; Status: TCallVideoStatus); dispid 39;
    procedure CallVideoSendStatusChanged(const pCall: ICall; Status: TCallVideoSendStatus); dispid 40;
    procedure CallVideoReceiveStatusChanged(const pCall: ICall; Status: TCallVideoSendStatus); dispid 41;
    procedure SilentModeStatusChanged(Silent: WordBool); dispid 42;
    procedure UILanguageChanged(const code: WideString); dispid 43;
    procedure UserAuthorizationRequestReceived(const pUser: IUser); dispid 44;
  end;

// *********************************************************************//
// DispIntf:  _ICallChannelManagerEvents
// Flags:     (4096) Dispatchable
// GUID:      {497ABB45-20AE-49D1-A39D-CBE84A31B20C}
// *********************************************************************//
  _ICallChannelManagerEvents = dispinterface
    ['{497ABB45-20AE-49D1-A39D-CBE84A31B20C}']
    procedure Channels(const pManager: ICallChannelManager; const pChannels: ICallChannelCollection); dispid 1;
    procedure Message(const pManager: ICallChannelManager; const pChannel: ICallChannel; 
                      const pMessage: ICallChannelMessage); dispid 2;
    procedure Created; dispid 3;
  end;

// *********************************************************************//
// Interface: ISkypePlugin
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B77A3D1F-6847-402F-BB82-A0564379088E}
// *********************************************************************//
  ISkypePlugin = interface(IDispatch)
    ['{B77A3D1F-6847-402F-BB82-A0564379088E}']
    function CreatePluginEvent(const EventId: WideString; const Caption: WideString; 
                               const Hint: WideString): IPluginEvent; safecall;
    procedure DeletePluginEvent(const EventId: WideString); safecall;
    function CreatePluginMenuItem(const MenuItemId: WideString; PluginContext: TPluginContext; 
                                  const CaptionText: WideString; const HintText: WideString; 
                                  const IconPath: WideString; Enabled: WordBool; 
                                  ContactType: TPluginContactType; MultipleContacts: WordBool): IPluginMenuItem; safecall;
    procedure DeletePluginMenuItem(const MenuItemId: WideString); safecall;
    procedure Set_PluginMenuItemCaption(const MenuItemId: WideString; const Param2: WideString); safecall;
    procedure Set_PluginMenuItemHint(const MenuItemId: WideString; const Param2: WideString); safecall;
    procedure EnablePluginMenuItem(const MenuItemId: WideString; Enabled: WordBool); safecall;
    property PluginMenuItemCaption[const MenuItemId: WideString]: WideString write Set_PluginMenuItemCaption;
    property PluginMenuItemHint[const MenuItemId: WideString]: WideString write Set_PluginMenuItemHint;
  end;

// *********************************************************************//
// DispIntf:  ISkypePluginDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B77A3D1F-6847-402F-BB82-A0564379088E}
// *********************************************************************//
  ISkypePluginDisp = dispinterface
    ['{B77A3D1F-6847-402F-BB82-A0564379088E}']
    function CreatePluginEvent(const EventId: WideString; const Caption: WideString; 
                               const Hint: WideString): IPluginEvent; dispid 1;
    procedure DeletePluginEvent(const EventId: WideString); dispid 2;
    function CreatePluginMenuItem(const MenuItemId: WideString; PluginContext: TPluginContext; 
                                  const CaptionText: WideString; const HintText: WideString; 
                                  const IconPath: WideString; Enabled: WordBool; 
                                  ContactType: TPluginContactType; MultipleContacts: WordBool): IPluginMenuItem; dispid 3;
    procedure DeletePluginMenuItem(const MenuItemId: WideString); dispid 4;
    property PluginMenuItemCaption[const MenuItemId: WideString]: WideString writeonly dispid 5;
    property PluginMenuItemHint[const MenuItemId: WideString]: WideString writeonly dispid 6;
    procedure EnablePluginMenuItem(const MenuItemId: WideString; Enabled: WordBool); dispid 7;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TSkype
// Help String      : Skype class.
// Default Interface: ISkype
// Def. Intf. DISP? : No
// Event   Interface: _ISkypeEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TSkypeCommand = procedure(ASender: TObject; const pCommand: ICommand) of object;
  TSkypeReply = procedure(ASender: TObject; const pCommand: ICommand) of object;
  TSkypeError = procedure(ASender: TObject; const pCommand: ICommand; Number: Integer; 
                                            const Description: WideString) of object;
  TSkypeAttachmentStatus = procedure(ASender: TObject; Status: TAttachmentStatus) of object;
  TSkypeConnectionStatus = procedure(ASender: TObject; Status: TConnectionStatus) of object;
  TSkypeUserStatus = procedure(ASender: TObject; Status: TUserStatus) of object;
  TSkypeOnlineStatus = procedure(ASender: TObject; const pUser: IUser; Status: TOnlineStatus) of object;
  TSkypeCallStatus = procedure(ASender: TObject; const pCall: ICall; Status: TCallStatus) of object;
  TSkypeMute = procedure(ASender: TObject; Mute: WordBool) of object;
  TSkypeMessageStatus = procedure(ASender: TObject; const pMessage: IChatMessage; 
                                                    Status: TChatMessageStatus) of object;
  TSkypeMessageHistory = procedure(ASender: TObject; const Username: WideString) of object;
  TSkypeAutoAway = procedure(ASender: TObject; Automatic: WordBool) of object;
  TSkypeCallDtmfReceived = procedure(ASender: TObject; const pCall: ICall; const code: WideString) of object;
  TSkypeVoicemailStatus = procedure(ASender: TObject; const pMail: IVoicemail; 
                                                      Status: TVoicemailStatus) of object;
  TSkypeApplicationConnecting = procedure(ASender: TObject; const pApp: IApplication; 
                                                            const pUsers: IUserCollection) of object;
  TSkypeApplicationStreams = procedure(ASender: TObject; const pApp: IApplication; 
                                                         const pStreams: IApplicationStreamCollection) of object;
  TSkypeApplicationDatagram = procedure(ASender: TObject; const pApp: IApplication; 
                                                          const pStream: IApplicationStream; 
                                                          const Text: WideString) of object;
  TSkypeApplicationSending = procedure(ASender: TObject; const pApp: IApplication; 
                                                         const pStreams: IApplicationStreamCollection) of object;
  TSkypeApplicationReceiving = procedure(ASender: TObject; const pApp: IApplication; 
                                                           const pStreams: IApplicationStreamCollection) of object;
  TSkypeContactsFocused = procedure(ASender: TObject; const Username: WideString) of object;
  TSkypeGroupVisible = procedure(ASender: TObject; const pGroup: IGroup; Visible: WordBool) of object;
  TSkypeGroupExpanded = procedure(ASender: TObject; const pGroup: IGroup; Expanded: WordBool) of object;
  TSkypeGroupUsers = procedure(ASender: TObject; const pGroup: IGroup; const pUsers: IUserCollection) of object;
  TSkypeGroupDeleted = procedure(ASender: TObject; GroupId: Integer) of object;
  TSkypeUserMood = procedure(ASender: TObject; const pUser: IUser; const MoodText: WideString) of object;
  TSkypeSmsMessageStatusChanged = procedure(ASender: TObject; const pMessage: ISmsMessage; 
                                                              Status: TSmsMessageStatus) of object;
  TSkypeSmsTargetStatusChanged = procedure(ASender: TObject; const pTarget: ISmsTarget; 
                                                             Status: TSmsTargetStatus) of object;
  TSkypeCallInputStatusChanged = procedure(ASender: TObject; const pCall: ICall; Status: WordBool) of object;
  TSkypeAsyncSearchUsersFinished = procedure(ASender: TObject; Cookie: Integer; 
                                                               const pUsers: IUserCollection) of object;
  TSkypeCallSeenStatusChanged = procedure(ASender: TObject; const pCall: ICall; Status: WordBool) of object;
  TSkypePluginEventClicked = procedure(ASender: TObject; const pEvent: IPluginEvent) of object;
  TSkypePluginMenuItemClicked = procedure(ASender: TObject; const pMenuItem: IPluginMenuItem; 
                                                            const pUsers: IUserCollection; 
                                                            PluginContext: TPluginContext; 
                                                            const ContextId: WideString) of object;
  TSkypeWallpaperChanged = procedure(ASender: TObject; const Path: WideString) of object;
  TSkypeFileTransferStatusChanged = procedure(ASender: TObject; const pTransfer: IFileTransfer; 
                                                                Status: TFileTransferStatus) of object;
  TSkypeCallTransferStatusChanged = procedure(ASender: TObject; const pCall: ICall; 
                                                                Status: TCallStatus) of object;
  TSkypeChatMembersChanged = procedure(ASender: TObject; const pChat: IChat; 
                                                         const pMembers: IUserCollection) of object;
  TSkypeChatMemberRoleChanged = procedure(ASender: TObject; const pMember: IChatMember; 
                                                            Role: TChatMemberRole) of object;
  TSkypeCallVideoStatusChanged = procedure(ASender: TObject; const pCall: ICall; 
                                                             Status: TCallVideoStatus) of object;
  TSkypeCallVideoSendStatusChanged = procedure(ASender: TObject; const pCall: ICall; 
                                                                 Status: TCallVideoSendStatus) of object;
  TSkypeCallVideoReceiveStatusChanged = procedure(ASender: TObject; const pCall: ICall; 
                                                                    Status: TCallVideoSendStatus) of object;
  TSkypeSilentModeStatusChanged = procedure(ASender: TObject; Silent: WordBool) of object;
  TSkypeUILanguageChanged = procedure(ASender: TObject; const code: WideString) of object;
  TSkypeUserAuthorizationRequestReceived = procedure(ASender: TObject; const pUser: IUser) of object;

  TSkype = class(TOleControl)
  private
    FOnCommand: TSkypeCommand;
    FOnReply: TSkypeReply;
    FOnError: TSkypeError;
    FOnAttachmentStatus: TSkypeAttachmentStatus;
    FOnConnectionStatus: TSkypeConnectionStatus;
    FOnUserStatus: TSkypeUserStatus;
    FOnOnlineStatus: TSkypeOnlineStatus;
    FOnCallStatus: TSkypeCallStatus;
    FOnCallHistory: TNotifyEvent;
    FOnMute: TSkypeMute;
    FOnMessageStatus: TSkypeMessageStatus;
    FOnMessageHistory: TSkypeMessageHistory;
    FOnAutoAway: TSkypeAutoAway;
    FOnCallDtmfReceived: TSkypeCallDtmfReceived;
    FOnVoicemailStatus: TSkypeVoicemailStatus;
    FOnApplicationConnecting: TSkypeApplicationConnecting;
    FOnApplicationStreams: TSkypeApplicationStreams;
    FOnApplicationDatagram: TSkypeApplicationDatagram;
    FOnApplicationSending: TSkypeApplicationSending;
    FOnApplicationReceiving: TSkypeApplicationReceiving;
    FOnContactsFocused: TSkypeContactsFocused;
    FOnGroupVisible: TSkypeGroupVisible;
    FOnGroupExpanded: TSkypeGroupExpanded;
    FOnGroupUsers: TSkypeGroupUsers;
    FOnGroupDeleted: TSkypeGroupDeleted;
    FOnUserMood: TSkypeUserMood;
    FOnSmsMessageStatusChanged: TSkypeSmsMessageStatusChanged;
    FOnSmsTargetStatusChanged: TSkypeSmsTargetStatusChanged;
    FOnCallInputStatusChanged: TSkypeCallInputStatusChanged;
    FOnAsyncSearchUsersFinished: TSkypeAsyncSearchUsersFinished;
    FOnCallSeenStatusChanged: TSkypeCallSeenStatusChanged;
    FOnPluginEventClicked: TSkypePluginEventClicked;
    FOnPluginMenuItemClicked: TSkypePluginMenuItemClicked;
    FOnWallpaperChanged: TSkypeWallpaperChanged;
    FOnFileTransferStatusChanged: TSkypeFileTransferStatusChanged;
    FOnCallTransferStatusChanged: TSkypeCallTransferStatusChanged;
    FOnChatMembersChanged: TSkypeChatMembersChanged;
    FOnChatMemberRoleChanged: TSkypeChatMemberRoleChanged;
    FOnCallVideoStatusChanged: TSkypeCallVideoStatusChanged;
    FOnCallVideoSendStatusChanged: TSkypeCallVideoSendStatusChanged;
    FOnCallVideoReceiveStatusChanged: TSkypeCallVideoReceiveStatusChanged;
    FOnSilentModeStatusChanged: TSkypeSilentModeStatusChanged;
    FOnUILanguageChanged: TSkypeUILanguageChanged;
    FOnUserAuthorizationRequestReceived: TSkypeUserAuthorizationRequestReceived;
    FIntf: ISkype;
    function  GetControlInterface: ISkype;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                           const PropName: WideString): WideString;
    procedure Set_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                            const PropName: WideString; const pVal: WideString);
    function Get_Variable(const Name: WideString): WideString;
    procedure Set_Variable(const Name: WideString; const pVal: WideString);
    function Get_Privilege(const Name: WideString): WordBool;
    function Get_CurrentUser: IUser;
    function Get_Convert: IConversion;
    function Get_Friends: IUserCollection;
    function Get_Calls(const Target: WideString): ICallCollection;
    function Get_ActiveCalls: ICallCollection;
    function Get_MissedCalls: ICallCollection;
    function Get_Messages(const Target: WideString): IChatMessageCollection;
    function Get_MissedMessages: IChatMessageCollection;
    function Get_User(const Username: WideString): IUser;
    function Get_Message(Id: Integer): IChatMessage;
    function Get_Call(Id: Integer): ICall;
    function Get_Chats: IChatCollection;
    function Get_Chat(const Name: WideString): IChat;
    function Get_Conference(Id: Integer): IConference;
    function Get_Conferences: IConferenceCollection;
    function Get_Profile(const Property_: WideString): WideString;
    procedure Set_Profile(const Property_: WideString; const pVal: WideString);
    function Get_ActiveChats: IChatCollection;
    function Get_MissedChats: IChatCollection;
    function Get_RecentChats: IChatCollection;
    function Get_BookmarkedChats: IChatCollection;
    function Get_Voicemails: IVoicemailCollection;
    function Get_UsersWaitingAuthorization: IUserCollection;
    function Get_Application(const Name: WideString): IApplication;
    function Get_Greeting(const Username: WideString): IVoicemail;
    function Get_CurrentUserProfile: IProfile;
    function Get_Groups: IGroupCollection;
    function Get_CustomGroups: IGroupCollection;
    function Get_HardwiredGroups: IGroupCollection;
    function Get_Settings: ISettings;
    function Get_Client: IClient;
    function Get_Command(Id: Integer; const Command: WideString; const Reply: WideString; 
                         Block: WordBool; Timeout: Integer): ICommand;
    function Get_Voicemail(Id: Integer): IVoicemail;
    function Get_MissedVoicemails: IVoicemailCollection;
    function Get_ApiSecurityContextEnabled(Context: TApiSecurityContext): WordBool;
    function Get_Smss: ISmsMessageCollection;
    function Get_MissedSmss: ISmsMessageCollection;
    function Get_FileTransfers: IFileTransferCollection;
    function Get_ActiveFileTransfers: IFileTransferCollection;
    function Get_FocusedContacts: IUserCollection;
  public
    function SearchForUsers(const Target: WideString): IUserCollection;
    procedure Attach(Protocol: Integer; Wait: WordBool);
    function PlaceCall(const Target: WideString; const Target2: WideString; 
                       const Target3: WideString; const Target4: WideString): ICall;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
    procedure SendCommand(const pCommand: ICommand);
    procedure ChangeUserStatus(newVal: TUserStatus);
    function CreateChatWith(const Username: WideString): IChat;
    function CreateChatMultiple(const pMembers: IUserCollection): IChat;
    function SendVoicemail(const Username: WideString): IVoicemail;
    procedure ClearChatHistory;
    procedure ClearVoicemailHistory;
    procedure ClearCallHistory(const Username: WideString; Type_: TCallHistory);
    procedure ResetCache;
    function CreateGroup(const GroupName: WideString): IGroup;
    procedure DeleteGroup(GroupId: Integer);
    procedure EnableApiSecurityContext(Context: TApiSecurityContext);
    function CreateSms(MessageType: TSmsMessageType; const TargetNumbers: WideString): ISmsMessage;
    function SendSms(const TargetNumbers: WideString; const MessageText: WideString; 
                     const ReplyToNumber: WideString): ISmsMessage;
    function AsyncSearchUsers(const Target: WideString): Integer;
    function FindChatUsingBlob(const Blob: WideString): IChat;
    function CreateChatUsingBlob(const Blob: WideString): IChat;
    property  ControlInterface: ISkype read GetControlInterface;
    property  DefaultInterface: ISkype read GetControlInterface;
    property Property_[const ObjectType: WideString; const ObjectId: WideString; 
                       const PropName: WideString]: WideString read Get_Property_ write Set_Property_;
    property Variable[const Name: WideString]: WideString read Get_Variable write Set_Variable;
    property CurrentUserHandle: WideString index 4 read GetWideStringProp;
    property ConnectionStatus: TOleEnum index 6 read GetTOleEnumProp;
    property Version: WideString index 8 read GetWideStringProp;
    property Privilege[const Name: WideString]: WordBool read Get_Privilege;
    property CurrentUser: IUser read Get_CurrentUser;
    property Convert: IConversion read Get_Convert;
    property Friends: IUserCollection read Get_Friends;
    property Calls[const Target: WideString]: ICallCollection read Get_Calls;
    property ActiveCalls: ICallCollection read Get_ActiveCalls;
    property MissedCalls: ICallCollection read Get_MissedCalls;
    property Messages[const Target: WideString]: IChatMessageCollection read Get_Messages;
    property MissedMessages: IChatMessageCollection read Get_MissedMessages;
    property AttachmentStatus: TOleEnum index 19 read GetTOleEnumProp;
    property User[const Username: WideString]: IUser read Get_User;
    property Message[Id: Integer]: IChatMessage read Get_Message;
    property Call[Id: Integer]: ICall read Get_Call;
    property Chats: IChatCollection read Get_Chats;
    property Chat[const Name: WideString]: IChat read Get_Chat;
    property Conference[Id: Integer]: IConference read Get_Conference;
    property Conferences: IConferenceCollection read Get_Conferences;
    property Profile[const Property_: WideString]: WideString read Get_Profile write Set_Profile;
    property ActiveChats: IChatCollection read Get_ActiveChats;
    property MissedChats: IChatCollection read Get_MissedChats;
    property RecentChats: IChatCollection read Get_RecentChats;
    property BookmarkedChats: IChatCollection read Get_BookmarkedChats;
    property Voicemails: IVoicemailCollection read Get_Voicemails;
    property UsersWaitingAuthorization: IUserCollection read Get_UsersWaitingAuthorization;
    property Application[const Name: WideString]: IApplication read Get_Application;
    property Greeting[const Username: WideString]: IVoicemail read Get_Greeting;
    property CurrentUserProfile: IProfile read Get_CurrentUserProfile;
    property Groups: IGroupCollection read Get_Groups;
    property CustomGroups: IGroupCollection read Get_CustomGroups;
    property HardwiredGroups: IGroupCollection read Get_HardwiredGroups;
    property Settings: ISettings read Get_Settings;
    property Client: IClient read Get_Client;
    property FriendlyName: WideString index 59 write SetWideStringProp;
    property Command[Id: Integer; const Command: WideString; const Reply: WideString; 
                     Block: WordBool; Timeout: Integer]: ICommand read Get_Command;
    property Voicemail[Id: Integer]: IVoicemail read Get_Voicemail;
    property MissedVoicemails: IVoicemailCollection read Get_MissedVoicemails;
    property ApiSecurityContextEnabled[Context: TApiSecurityContext]: WordBool read Get_ApiSecurityContextEnabled;
    property Smss: ISmsMessageCollection read Get_Smss;
    property MissedSmss: ISmsMessageCollection read Get_MissedSmss;
    property ApiWrapperVersion: WideString index 70 read GetWideStringProp;
    property FileTransfers: IFileTransferCollection read Get_FileTransfers;
    property ActiveFileTransfers: IFileTransferCollection read Get_ActiveFileTransfers;
    property FocusedContacts: IUserCollection read Get_FocusedContacts;
    property PredictiveDialerCountry: WideString index 77 read GetWideStringProp;
  published
    property Anchors;
    property Timeout: Integer index 1 read GetIntegerProp write SetIntegerProp stored False;
    property CurrentUserStatus: TOleEnum index 5 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Mute: WordBool index 7 read GetWordBoolProp write SetWordBoolProp stored False;
    property Protocol: Integer index 20 read GetIntegerProp write SetIntegerProp stored False;
    property CommandId: WordBool index 46 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cache: WordBool index 49 read GetWordBoolProp write SetWordBoolProp stored False;
    property SilentMode: WordBool index 71 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnCommand: TSkypeCommand read FOnCommand write FOnCommand;
    property OnReply: TSkypeReply read FOnReply write FOnReply;
    property OnError: TSkypeError read FOnError write FOnError;
    property OnAttachmentStatus: TSkypeAttachmentStatus read FOnAttachmentStatus write FOnAttachmentStatus;
    property OnConnectionStatus: TSkypeConnectionStatus read FOnConnectionStatus write FOnConnectionStatus;
    property OnUserStatus: TSkypeUserStatus read FOnUserStatus write FOnUserStatus;
    property OnOnlineStatus: TSkypeOnlineStatus read FOnOnlineStatus write FOnOnlineStatus;
    property OnCallStatus: TSkypeCallStatus read FOnCallStatus write FOnCallStatus;
    property OnCallHistory: TNotifyEvent read FOnCallHistory write FOnCallHistory;
    property OnMute: TSkypeMute read FOnMute write FOnMute;
    property OnMessageStatus: TSkypeMessageStatus read FOnMessageStatus write FOnMessageStatus;
    property OnMessageHistory: TSkypeMessageHistory read FOnMessageHistory write FOnMessageHistory;
    property OnAutoAway: TSkypeAutoAway read FOnAutoAway write FOnAutoAway;
    property OnCallDtmfReceived: TSkypeCallDtmfReceived read FOnCallDtmfReceived write FOnCallDtmfReceived;
    property OnVoicemailStatus: TSkypeVoicemailStatus read FOnVoicemailStatus write FOnVoicemailStatus;
    property OnApplicationConnecting: TSkypeApplicationConnecting read FOnApplicationConnecting write FOnApplicationConnecting;
    property OnApplicationStreams: TSkypeApplicationStreams read FOnApplicationStreams write FOnApplicationStreams;
    property OnApplicationDatagram: TSkypeApplicationDatagram read FOnApplicationDatagram write FOnApplicationDatagram;
    property OnApplicationSending: TSkypeApplicationSending read FOnApplicationSending write FOnApplicationSending;
    property OnApplicationReceiving: TSkypeApplicationReceiving read FOnApplicationReceiving write FOnApplicationReceiving;
    property OnContactsFocused: TSkypeContactsFocused read FOnContactsFocused write FOnContactsFocused;
    property OnGroupVisible: TSkypeGroupVisible read FOnGroupVisible write FOnGroupVisible;
    property OnGroupExpanded: TSkypeGroupExpanded read FOnGroupExpanded write FOnGroupExpanded;
    property OnGroupUsers: TSkypeGroupUsers read FOnGroupUsers write FOnGroupUsers;
    property OnGroupDeleted: TSkypeGroupDeleted read FOnGroupDeleted write FOnGroupDeleted;
    property OnUserMood: TSkypeUserMood read FOnUserMood write FOnUserMood;
    property OnSmsMessageStatusChanged: TSkypeSmsMessageStatusChanged read FOnSmsMessageStatusChanged write FOnSmsMessageStatusChanged;
    property OnSmsTargetStatusChanged: TSkypeSmsTargetStatusChanged read FOnSmsTargetStatusChanged write FOnSmsTargetStatusChanged;
    property OnCallInputStatusChanged: TSkypeCallInputStatusChanged read FOnCallInputStatusChanged write FOnCallInputStatusChanged;
    property OnAsyncSearchUsersFinished: TSkypeAsyncSearchUsersFinished read FOnAsyncSearchUsersFinished write FOnAsyncSearchUsersFinished;
    property OnCallSeenStatusChanged: TSkypeCallSeenStatusChanged read FOnCallSeenStatusChanged write FOnCallSeenStatusChanged;
    property OnPluginEventClicked: TSkypePluginEventClicked read FOnPluginEventClicked write FOnPluginEventClicked;
    property OnPluginMenuItemClicked: TSkypePluginMenuItemClicked read FOnPluginMenuItemClicked write FOnPluginMenuItemClicked;
    property OnWallpaperChanged: TSkypeWallpaperChanged read FOnWallpaperChanged write FOnWallpaperChanged;
    property OnFileTransferStatusChanged: TSkypeFileTransferStatusChanged read FOnFileTransferStatusChanged write FOnFileTransferStatusChanged;
    property OnCallTransferStatusChanged: TSkypeCallTransferStatusChanged read FOnCallTransferStatusChanged write FOnCallTransferStatusChanged;
    property OnChatMembersChanged: TSkypeChatMembersChanged read FOnChatMembersChanged write FOnChatMembersChanged;
    property OnChatMemberRoleChanged: TSkypeChatMemberRoleChanged read FOnChatMemberRoleChanged write FOnChatMemberRoleChanged;
    property OnCallVideoStatusChanged: TSkypeCallVideoStatusChanged read FOnCallVideoStatusChanged write FOnCallVideoStatusChanged;
    property OnCallVideoSendStatusChanged: TSkypeCallVideoSendStatusChanged read FOnCallVideoSendStatusChanged write FOnCallVideoSendStatusChanged;
    property OnCallVideoReceiveStatusChanged: TSkypeCallVideoReceiveStatusChanged read FOnCallVideoReceiveStatusChanged write FOnCallVideoReceiveStatusChanged;
    property OnSilentModeStatusChanged: TSkypeSilentModeStatusChanged read FOnSilentModeStatusChanged write FOnSilentModeStatusChanged;
    property OnUILanguageChanged: TSkypeUILanguageChanged read FOnUILanguageChanged write FOnUILanguageChanged;
    property OnUserAuthorizationRequestReceived: TSkypeUserAuthorizationRequestReceived read FOnUserAuthorizationRequestReceived write FOnUserAuthorizationRequestReceived;
  end;

// *********************************************************************//
// The Class CoUser provides a Create and CreateRemote method to          
// create instances of the default interface IUser exposed by              
// the CoClass User. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUser = class
    class function Create: IUser;
    class function CreateRemote(const MachineName: string): IUser;
  end;

// *********************************************************************//
// The Class CoUserCollection provides a Create and CreateRemote method to          
// create instances of the default interface IUserCollection exposed by              
// the CoClass UserCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUserCollection = class
    class function Create: IUserCollection;
    class function CreateRemote(const MachineName: string): IUserCollection;
  end;

// *********************************************************************//
// The Class CoConversion provides a Create and CreateRemote method to          
// create instances of the default interface IConversion exposed by              
// the CoClass Conversion. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoConversion = class
    class function Create: IConversion;
    class function CreateRemote(const MachineName: string): IConversion;
  end;

// *********************************************************************//
// The Class CoCall provides a Create and CreateRemote method to          
// create instances of the default interface ICall exposed by              
// the CoClass Call. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCall = class
    class function Create: ICall;
    class function CreateRemote(const MachineName: string): ICall;
  end;

// *********************************************************************//
// The Class CoCallCollection provides a Create and CreateRemote method to          
// create instances of the default interface ICallCollection exposed by              
// the CoClass CallCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCallCollection = class
    class function Create: ICallCollection;
    class function CreateRemote(const MachineName: string): ICallCollection;
  end;

// *********************************************************************//
// The Class CoChat provides a Create and CreateRemote method to          
// create instances of the default interface IChat exposed by              
// the CoClass Chat. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoChat = class
    class function Create: IChat;
    class function CreateRemote(const MachineName: string): IChat;
  end;

// *********************************************************************//
// The Class CoChatCollection provides a Create and CreateRemote method to          
// create instances of the default interface IChatCollection exposed by              
// the CoClass ChatCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoChatCollection = class
    class function Create: IChatCollection;
    class function CreateRemote(const MachineName: string): IChatCollection;
  end;

// *********************************************************************//
// The Class CoParticipant provides a Create and CreateRemote method to          
// create instances of the default interface IParticipant exposed by              
// the CoClass Participant. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoParticipant = class
    class function Create: IParticipant;
    class function CreateRemote(const MachineName: string): IParticipant;
  end;

// *********************************************************************//
// The Class CoParticipantCollection provides a Create and CreateRemote method to          
// create instances of the default interface IParticipantCollection exposed by              
// the CoClass ParticipantCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoParticipantCollection = class
    class function Create: IParticipantCollection;
    class function CreateRemote(const MachineName: string): IParticipantCollection;
  end;

// *********************************************************************//
// The Class CoConference provides a Create and CreateRemote method to          
// create instances of the default interface IConference exposed by              
// the CoClass Conference. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoConference = class
    class function Create: IConference;
    class function CreateRemote(const MachineName: string): IConference;
  end;

// *********************************************************************//
// The Class CoConferenceCollection provides a Create and CreateRemote method to          
// create instances of the default interface IConferenceCollection exposed by              
// the CoClass ConferenceCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoConferenceCollection = class
    class function Create: IConferenceCollection;
    class function CreateRemote(const MachineName: string): IConferenceCollection;
  end;

// *********************************************************************//
// The Class CoVoicemail provides a Create and CreateRemote method to          
// create instances of the default interface IVoicemail exposed by              
// the CoClass Voicemail. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoVoicemail = class
    class function Create: IVoicemail;
    class function CreateRemote(const MachineName: string): IVoicemail;
  end;

// *********************************************************************//
// The Class CoVoicemailCollection provides a Create and CreateRemote method to          
// create instances of the default interface IVoicemailCollection exposed by              
// the CoClass VoicemailCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoVoicemailCollection = class
    class function Create: IVoicemailCollection;
    class function CreateRemote(const MachineName: string): IVoicemailCollection;
  end;

// *********************************************************************//
// The Class CoApplication provides a Create and CreateRemote method to          
// create instances of the default interface IApplication exposed by              
// the CoClass Application. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoApplication = class
    class function Create: IApplication;
    class function CreateRemote(const MachineName: string): IApplication;
  end;

// *********************************************************************//
// The Class CoApplicationStream provides a Create and CreateRemote method to          
// create instances of the default interface IApplicationStream exposed by              
// the CoClass ApplicationStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoApplicationStream = class
    class function Create: IApplicationStream;
    class function CreateRemote(const MachineName: string): IApplicationStream;
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

// *********************************************************************//
// The Class CoChatMessage provides a Create and CreateRemote method to          
// create instances of the default interface IChatMessage exposed by              
// the CoClass ChatMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoChatMessage = class
    class function Create: IChatMessage;
    class function CreateRemote(const MachineName: string): IChatMessage;
  end;

// *********************************************************************//
// The Class CoChatMessageCollection provides a Create and CreateRemote method to          
// create instances of the default interface IChatMessageCollection exposed by              
// the CoClass ChatMessageCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoChatMessageCollection = class
    class function Create: IChatMessageCollection;
    class function CreateRemote(const MachineName: string): IChatMessageCollection;
  end;

// *********************************************************************//
// The Class CoProfile provides a Create and CreateRemote method to          
// create instances of the default interface IProfile exposed by              
// the CoClass Profile. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoProfile = class
    class function Create: IProfile;
    class function CreateRemote(const MachineName: string): IProfile;
  end;

// *********************************************************************//
// The Class CoGroup provides a Create and CreateRemote method to          
// create instances of the default interface IGroup exposed by              
// the CoClass Group. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoGroup = class
    class function Create: IGroup;
    class function CreateRemote(const MachineName: string): IGroup;
  end;

// *********************************************************************//
// The Class CoGroupCollection provides a Create and CreateRemote method to          
// create instances of the default interface IGroupCollection exposed by              
// the CoClass GroupCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoGroupCollection = class
    class function Create: IGroupCollection;
    class function CreateRemote(const MachineName: string): IGroupCollection;
  end;

// *********************************************************************//
// The Class CoSettings provides a Create and CreateRemote method to          
// create instances of the default interface ISettings exposed by              
// the CoClass Settings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSettings = class
    class function Create: ISettings;
    class function CreateRemote(const MachineName: string): ISettings;
  end;

// *********************************************************************//
// The Class CoClient provides a Create and CreateRemote method to          
// create instances of the default interface IClient exposed by              
// the CoClass Client. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoClient = class
    class function Create: IClient;
    class function CreateRemote(const MachineName: string): IClient;
  end;

// *********************************************************************//
// The Class CoCommand provides a Create and CreateRemote method to          
// create instances of the default interface ICommand exposed by              
// the CoClass Command. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCommand = class
    class function Create: ICommand;
    class function CreateRemote(const MachineName: string): ICommand;
  end;

// *********************************************************************//
// The Class CoCallChannel provides a Create and CreateRemote method to          
// create instances of the default interface ICallChannel exposed by              
// the CoClass CallChannel. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCallChannel = class
    class function Create: ICallChannel;
    class function CreateRemote(const MachineName: string): ICallChannel;
  end;

// *********************************************************************//
// The Class CoCallChannelCollection provides a Create and CreateRemote method to          
// create instances of the default interface ICallChannelCollection exposed by              
// the CoClass CallChannelCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCallChannelCollection = class
    class function Create: ICallChannelCollection;
    class function CreateRemote(const MachineName: string): ICallChannelCollection;
  end;

// *********************************************************************//
// The Class CoCallChannelManager provides a Create and CreateRemote method to          
// create instances of the default interface ICallChannelManager exposed by              
// the CoClass CallChannelManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCallChannelManager = class
    class function Create: ICallChannelManager;
    class function CreateRemote(const MachineName: string): ICallChannelManager;
  end;

// *********************************************************************//
// The Class CoCallChannelMessage provides a Create and CreateRemote method to          
// create instances of the default interface ICallChannelMessage exposed by              
// the CoClass CallChannelMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCallChannelMessage = class
    class function Create: ICallChannelMessage;
    class function CreateRemote(const MachineName: string): ICallChannelMessage;
  end;

// *********************************************************************//
// The Class CoIEProtocolHandler provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass IEProtocolHandler. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoIEProtocolHandler = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: string): IUnknown;
  end;

// *********************************************************************//
// The Class CoSmsMessage provides a Create and CreateRemote method to          
// create instances of the default interface ISmsMessage exposed by              
// the CoClass SmsMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsMessage = class
    class function Create: ISmsMessage;
    class function CreateRemote(const MachineName: string): ISmsMessage;
  end;

// *********************************************************************//
// The Class CoSmsMessageCollection provides a Create and CreateRemote method to          
// create instances of the default interface ISmsMessageCollection exposed by              
// the CoClass SmsMessageCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsMessageCollection = class
    class function Create: ISmsMessageCollection;
    class function CreateRemote(const MachineName: string): ISmsMessageCollection;
  end;

// *********************************************************************//
// The Class CoSmsChunk provides a Create and CreateRemote method to          
// create instances of the default interface ISmsChunk exposed by              
// the CoClass SmsChunk. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsChunk = class
    class function Create: ISmsChunk;
    class function CreateRemote(const MachineName: string): ISmsChunk;
  end;

// *********************************************************************//
// The Class CoSmsChunkCollection provides a Create and CreateRemote method to          
// create instances of the default interface ISmsChunkCollection exposed by              
// the CoClass SmsChunkCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsChunkCollection = class
    class function Create: ISmsChunkCollection;
    class function CreateRemote(const MachineName: string): ISmsChunkCollection;
  end;

// *********************************************************************//
// The Class CoSmsTarget provides a Create and CreateRemote method to          
// create instances of the default interface ISmsTarget exposed by              
// the CoClass SmsTarget. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsTarget = class
    class function Create: ISmsTarget;
    class function CreateRemote(const MachineName: string): ISmsTarget;
  end;

// *********************************************************************//
// The Class CoSmsTargetCollection provides a Create and CreateRemote method to          
// create instances of the default interface ISmsTargetCollection exposed by              
// the CoClass SmsTargetCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSmsTargetCollection = class
    class function Create: ISmsTargetCollection;
    class function CreateRemote(const MachineName: string): ISmsTargetCollection;
  end;

// *********************************************************************//
// The Class CoPluginMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface IPluginMenuItem exposed by              
// the CoClass PluginMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPluginMenuItem = class
    class function Create: IPluginMenuItem;
    class function CreateRemote(const MachineName: string): IPluginMenuItem;
  end;

// *********************************************************************//
// The Class CoPluginEvent provides a Create and CreateRemote method to          
// create instances of the default interface IPluginEvent exposed by              
// the CoClass PluginEvent. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPluginEvent = class
    class function Create: IPluginEvent;
    class function CreateRemote(const MachineName: string): IPluginEvent;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TSkype.InitControlData;
const
  CEventDispIDs: array [0..43] of DWORD = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $00000007, $00000008, $00000009, $0000000A, $0000000B, $0000000C,
    $0000000D, $0000000E, $0000000F, $00000010, $00000011, $00000012,
    $00000013, $00000014, $00000015, $00000016, $00000017, $00000018,
    $00000019, $0000001A, $0000001B, $0000001C, $0000001D, $0000001E,
    $0000001F, $00000020, $00000021, $00000022, $00000023, $00000024,
    $00000025, $00000026, $00000027, $00000028, $00000029, $0000002A,
    $0000002B, $0000002C);
  CControlData: TControlData2 = (
    ClassID: '{830690FC-BF2F-47A6-AC2D-330BCB402664}';
    EventIID: '{F4F90CDD-C620-4118-945E-CAA1BBEBA435}';
    EventCount: 44;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004002*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnCommand) - Cardinal(Self);
end;

procedure TSkype.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ISkype;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TSkype.GetControlInterface: ISkype;
begin
  CreateControl;
  Result := FIntf;
end;

function TSkype.Get_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                              const PropName: WideString): WideString;
begin
    Result := DefaultInterface.Property_[ObjectType, ObjectId, PropName];
end;

procedure TSkype.Set_Property_(const ObjectType: WideString; const ObjectId: WideString; 
                               const PropName: WideString; const pVal: WideString);
  { Warning: The property Property_ has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Property_ := pVal;
end;

function TSkype.Get_Variable(const Name: WideString): WideString;
begin
    Result := DefaultInterface.Variable[Name];
end;

procedure TSkype.Set_Variable(const Name: WideString; const pVal: WideString);
  { Warning: The property Variable has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Variable := pVal;
end;

function TSkype.Get_Privilege(const Name: WideString): WordBool;
begin
    Result := DefaultInterface.Privilege[Name];
end;

function TSkype.Get_CurrentUser: IUser;
begin
    Result := DefaultInterface.CurrentUser;
end;

function TSkype.Get_Convert: IConversion;
begin
    Result := DefaultInterface.Convert;
end;

function TSkype.Get_Friends: IUserCollection;
begin
    Result := DefaultInterface.Friends;
end;

function TSkype.Get_Calls(const Target: WideString): ICallCollection;
begin
    Result := DefaultInterface.Calls[Target];
end;

function TSkype.Get_ActiveCalls: ICallCollection;
begin
    Result := DefaultInterface.ActiveCalls;
end;

function TSkype.Get_MissedCalls: ICallCollection;
begin
    Result := DefaultInterface.MissedCalls;
end;

function TSkype.Get_Messages(const Target: WideString): IChatMessageCollection;
begin
    Result := DefaultInterface.Messages[Target];
end;

function TSkype.Get_MissedMessages: IChatMessageCollection;
begin
    Result := DefaultInterface.MissedMessages;
end;

function TSkype.Get_User(const Username: WideString): IUser;
begin
    Result := DefaultInterface.User[Username];
end;

function TSkype.Get_Message(Id: Integer): IChatMessage;
begin
    Result := DefaultInterface.Message[Id];
end;

function TSkype.Get_Call(Id: Integer): ICall;
begin
    Result := DefaultInterface.Call[Id];
end;

function TSkype.Get_Chats: IChatCollection;
begin
    Result := DefaultInterface.Chats;
end;

function TSkype.Get_Chat(const Name: WideString): IChat;
begin
    Result := DefaultInterface.Chat[Name];
end;

function TSkype.Get_Conference(Id: Integer): IConference;
begin
    Result := DefaultInterface.Conference[Id];
end;

function TSkype.Get_Conferences: IConferenceCollection;
begin
    Result := DefaultInterface.Conferences;
end;

function TSkype.Get_Profile(const Property_: WideString): WideString;
begin
    Result := DefaultInterface.Profile[Property_];
end;

procedure TSkype.Set_Profile(const Property_: WideString; const pVal: WideString);
  { Warning: The property Profile has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Profile := pVal;
end;

function TSkype.Get_ActiveChats: IChatCollection;
begin
    Result := DefaultInterface.ActiveChats;
end;

function TSkype.Get_MissedChats: IChatCollection;
begin
    Result := DefaultInterface.MissedChats;
end;

function TSkype.Get_RecentChats: IChatCollection;
begin
    Result := DefaultInterface.RecentChats;
end;

function TSkype.Get_BookmarkedChats: IChatCollection;
begin
    Result := DefaultInterface.BookmarkedChats;
end;

function TSkype.Get_Voicemails: IVoicemailCollection;
begin
    Result := DefaultInterface.Voicemails;
end;

function TSkype.Get_UsersWaitingAuthorization: IUserCollection;
begin
    Result := DefaultInterface.UsersWaitingAuthorization;
end;

function TSkype.Get_Application(const Name: WideString): IApplication;
begin
    Result := DefaultInterface.Application[Name];
end;

function TSkype.Get_Greeting(const Username: WideString): IVoicemail;
begin
    Result := DefaultInterface.Greeting[Username];
end;

function TSkype.Get_CurrentUserProfile: IProfile;
begin
    Result := DefaultInterface.CurrentUserProfile;
end;

function TSkype.Get_Groups: IGroupCollection;
begin
    Result := DefaultInterface.Groups;
end;

function TSkype.Get_CustomGroups: IGroupCollection;
begin
    Result := DefaultInterface.CustomGroups;
end;

function TSkype.Get_HardwiredGroups: IGroupCollection;
begin
    Result := DefaultInterface.HardwiredGroups;
end;

function TSkype.Get_Settings: ISettings;
begin
    Result := DefaultInterface.Settings;
end;

function TSkype.Get_Client: IClient;
begin
    Result := DefaultInterface.Client;
end;

function TSkype.Get_Command(Id: Integer; const Command: WideString; const Reply: WideString; 
                            Block: WordBool; Timeout: Integer): ICommand;
begin
    Result := DefaultInterface.Command[Id, Command, Reply, Block, Timeout];
end;

function TSkype.Get_Voicemail(Id: Integer): IVoicemail;
begin
    Result := DefaultInterface.Voicemail[Id];
end;

function TSkype.Get_MissedVoicemails: IVoicemailCollection;
begin
    Result := DefaultInterface.MissedVoicemails;
end;

function TSkype.Get_ApiSecurityContextEnabled(Context: TApiSecurityContext): WordBool;
begin
    Result := DefaultInterface.ApiSecurityContextEnabled[Context];
end;

function TSkype.Get_Smss: ISmsMessageCollection;
begin
    Result := DefaultInterface.Smss;
end;

function TSkype.Get_MissedSmss: ISmsMessageCollection;
begin
    Result := DefaultInterface.MissedSmss;
end;

function TSkype.Get_FileTransfers: IFileTransferCollection;
begin
    Result := DefaultInterface.FileTransfers;
end;

function TSkype.Get_ActiveFileTransfers: IFileTransferCollection;
begin
    Result := DefaultInterface.ActiveFileTransfers;
end;

function TSkype.Get_FocusedContacts: IUserCollection;
begin
    Result := DefaultInterface.FocusedContacts;
end;

function TSkype.SearchForUsers(const Target: WideString): IUserCollection;
begin
  Result := DefaultInterface.SearchForUsers(Target);
end;

procedure TSkype.Attach(Protocol: Integer; Wait: WordBool);
begin
  DefaultInterface.Attach(Protocol, Wait);
end;

function TSkype.PlaceCall(const Target: WideString; const Target2: WideString; 
                          const Target3: WideString; const Target4: WideString): ICall;
begin
  Result := DefaultInterface.PlaceCall(Target, Target2, Target3, Target4);
end;

function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
begin
  Result := DefaultInterface.SendMessage(Username, Text);
end;

procedure TSkype.SendCommand(const pCommand: ICommand);
begin
  DefaultInterface.SendCommand(pCommand);
end;

procedure TSkype.ChangeUserStatus(newVal: TUserStatus);
begin
  DefaultInterface.ChangeUserStatus(newVal);
end;

function TSkype.CreateChatWith(const Username: WideString): IChat;
begin
  Result := DefaultInterface.CreateChatWith(Username);
end;

function TSkype.CreateChatMultiple(const pMembers: IUserCollection): IChat;
begin
  Result := DefaultInterface.CreateChatMultiple(pMembers);
end;

function TSkype.SendVoicemail(const Username: WideString): IVoicemail;
begin
  Result := DefaultInterface.SendVoicemail(Username);
end;

procedure TSkype.ClearChatHistory;
begin
  DefaultInterface.ClearChatHistory;
end;

procedure TSkype.ClearVoicemailHistory;
begin
  DefaultInterface.ClearVoicemailHistory;
end;

procedure TSkype.ClearCallHistory(const Username: WideString; Type_: TCallHistory);
begin
  DefaultInterface.ClearCallHistory(Username, Type_);
end;

procedure TSkype.ResetCache;
begin
  DefaultInterface.ResetCache;
end;

function TSkype.CreateGroup(const GroupName: WideString): IGroup;
begin
  Result := DefaultInterface.CreateGroup(GroupName);
end;

procedure TSkype.DeleteGroup(GroupId: Integer);
begin
  DefaultInterface.DeleteGroup(GroupId);
end;

procedure TSkype.EnableApiSecurityContext(Context: TApiSecurityContext);
begin
  DefaultInterface.EnableApiSecurityContext(Context);
end;

function TSkype.CreateSms(MessageType: TSmsMessageType; const TargetNumbers: WideString): ISmsMessage;
begin
  Result := DefaultInterface.CreateSms(MessageType, TargetNumbers);
end;

function TSkype.SendSms(const TargetNumbers: WideString; const MessageText: WideString; 
                        const ReplyToNumber: WideString): ISmsMessage;
begin
  Result := DefaultInterface.SendSms(TargetNumbers, MessageText, ReplyToNumber);
end;

function TSkype.AsyncSearchUsers(const Target: WideString): Integer;
begin
  Result := DefaultInterface.AsyncSearchUsers(Target);
end;

function TSkype.FindChatUsingBlob(const Blob: WideString): IChat;
begin
  Result := DefaultInterface.FindChatUsingBlob(Blob);
end;

function TSkype.CreateChatUsingBlob(const Blob: WideString): IChat;
begin
  Result := DefaultInterface.CreateChatUsingBlob(Blob);
end;

class function CoUser.Create: IUser;
begin
  Result := CreateComObject(CLASS_User) as IUser;
end;

class function CoUser.CreateRemote(const MachineName: string): IUser;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_User) as IUser;
end;

class function CoUserCollection.Create: IUserCollection;
begin
  Result := CreateComObject(CLASS_UserCollection) as IUserCollection;
end;

class function CoUserCollection.CreateRemote(const MachineName: string): IUserCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_UserCollection) as IUserCollection;
end;

class function CoConversion.Create: IConversion;
begin
  Result := CreateComObject(CLASS_Conversion) as IConversion;
end;

class function CoConversion.CreateRemote(const MachineName: string): IConversion;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Conversion) as IConversion;
end;

class function CoCall.Create: ICall;
begin
  Result := CreateComObject(CLASS_Call) as ICall;
end;

class function CoCall.CreateRemote(const MachineName: string): ICall;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Call) as ICall;
end;

class function CoCallCollection.Create: ICallCollection;
begin
  Result := CreateComObject(CLASS_CallCollection) as ICallCollection;
end;

class function CoCallCollection.CreateRemote(const MachineName: string): ICallCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CallCollection) as ICallCollection;
end;

class function CoChat.Create: IChat;
begin
  Result := CreateComObject(CLASS_Chat) as IChat;
end;

class function CoChat.CreateRemote(const MachineName: string): IChat;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Chat) as IChat;
end;

class function CoChatCollection.Create: IChatCollection;
begin
  Result := CreateComObject(CLASS_ChatCollection) as IChatCollection;
end;

class function CoChatCollection.CreateRemote(const MachineName: string): IChatCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ChatCollection) as IChatCollection;
end;

class function CoParticipant.Create: IParticipant;
begin
  Result := CreateComObject(CLASS_Participant) as IParticipant;
end;

class function CoParticipant.CreateRemote(const MachineName: string): IParticipant;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Participant) as IParticipant;
end;

class function CoParticipantCollection.Create: IParticipantCollection;
begin
  Result := CreateComObject(CLASS_ParticipantCollection) as IParticipantCollection;
end;

class function CoParticipantCollection.CreateRemote(const MachineName: string): IParticipantCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ParticipantCollection) as IParticipantCollection;
end;

class function CoConference.Create: IConference;
begin
  Result := CreateComObject(CLASS_Conference) as IConference;
end;

class function CoConference.CreateRemote(const MachineName: string): IConference;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Conference) as IConference;
end;

class function CoConferenceCollection.Create: IConferenceCollection;
begin
  Result := CreateComObject(CLASS_ConferenceCollection) as IConferenceCollection;
end;

class function CoConferenceCollection.CreateRemote(const MachineName: string): IConferenceCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ConferenceCollection) as IConferenceCollection;
end;

class function CoVoicemail.Create: IVoicemail;
begin
  Result := CreateComObject(CLASS_Voicemail) as IVoicemail;
end;

class function CoVoicemail.CreateRemote(const MachineName: string): IVoicemail;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Voicemail) as IVoicemail;
end;

class function CoVoicemailCollection.Create: IVoicemailCollection;
begin
  Result := CreateComObject(CLASS_VoicemailCollection) as IVoicemailCollection;
end;

class function CoVoicemailCollection.CreateRemote(const MachineName: string): IVoicemailCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_VoicemailCollection) as IVoicemailCollection;
end;

class function CoApplication.Create: IApplication;
begin
  Result := CreateComObject(CLASS_Application) as IApplication;
end;

class function CoApplication.CreateRemote(const MachineName: string): IApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Application) as IApplication;
end;

class function CoApplicationStream.Create: IApplicationStream;
begin
  Result := CreateComObject(CLASS_ApplicationStream) as IApplicationStream;
end;

class function CoApplicationStream.CreateRemote(const MachineName: string): IApplicationStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ApplicationStream) as IApplicationStream;
end;

class function CoApplicationStreamCollection.Create: IApplicationStreamCollection;
begin
  Result := CreateComObject(CLASS_ApplicationStreamCollection) as IApplicationStreamCollection;
end;

class function CoApplicationStreamCollection.CreateRemote(const MachineName: string): IApplicationStreamCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ApplicationStreamCollection) as IApplicationStreamCollection;
end;

class function CoChatMessage.Create: IChatMessage;
begin
  Result := CreateComObject(CLASS_ChatMessage) as IChatMessage;
end;

class function CoChatMessage.CreateRemote(const MachineName: string): IChatMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ChatMessage) as IChatMessage;
end;

class function CoChatMessageCollection.Create: IChatMessageCollection;
begin
  Result := CreateComObject(CLASS_ChatMessageCollection) as IChatMessageCollection;
end;

class function CoChatMessageCollection.CreateRemote(const MachineName: string): IChatMessageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ChatMessageCollection) as IChatMessageCollection;
end;

class function CoProfile.Create: IProfile;
begin
  Result := CreateComObject(CLASS_Profile) as IProfile;
end;

class function CoProfile.CreateRemote(const MachineName: string): IProfile;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Profile) as IProfile;
end;

class function CoGroup.Create: IGroup;
begin
  Result := CreateComObject(CLASS_Group) as IGroup;
end;

class function CoGroup.CreateRemote(const MachineName: string): IGroup;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Group) as IGroup;
end;

class function CoGroupCollection.Create: IGroupCollection;
begin
  Result := CreateComObject(CLASS_GroupCollection) as IGroupCollection;
end;

class function CoGroupCollection.CreateRemote(const MachineName: string): IGroupCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_GroupCollection) as IGroupCollection;
end;

class function CoSettings.Create: ISettings;
begin
  Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: string): ISettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

class function CoClient.Create: IClient;
begin
  Result := CreateComObject(CLASS_Client) as IClient;
end;

class function CoClient.CreateRemote(const MachineName: string): IClient;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Client) as IClient;
end;

class function CoCommand.Create: ICommand;
begin
  Result := CreateComObject(CLASS_Command) as ICommand;
end;

class function CoCommand.CreateRemote(const MachineName: string): ICommand;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Command) as ICommand;
end;

class function CoCallChannel.Create: ICallChannel;
begin
  Result := CreateComObject(CLASS_CallChannel) as ICallChannel;
end;

class function CoCallChannel.CreateRemote(const MachineName: string): ICallChannel;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CallChannel) as ICallChannel;
end;

class function CoCallChannelCollection.Create: ICallChannelCollection;
begin
  Result := CreateComObject(CLASS_CallChannelCollection) as ICallChannelCollection;
end;

class function CoCallChannelCollection.CreateRemote(const MachineName: string): ICallChannelCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CallChannelCollection) as ICallChannelCollection;
end;

class function CoCallChannelManager.Create: ICallChannelManager;
begin
  Result := CreateComObject(CLASS_CallChannelManager) as ICallChannelManager;
end;

class function CoCallChannelManager.CreateRemote(const MachineName: string): ICallChannelManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CallChannelManager) as ICallChannelManager;
end;

class function CoCallChannelMessage.Create: ICallChannelMessage;
begin
  Result := CreateComObject(CLASS_CallChannelMessage) as ICallChannelMessage;
end;

class function CoCallChannelMessage.CreateRemote(const MachineName: string): ICallChannelMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CallChannelMessage) as ICallChannelMessage;
end;

class function CoIEProtocolHandler.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_IEProtocolHandler) as IUnknown;
end;

class function CoIEProtocolHandler.CreateRemote(const MachineName: string): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_IEProtocolHandler) as IUnknown;
end;

class function CoSmsMessage.Create: ISmsMessage;
begin
  Result := CreateComObject(CLASS_SmsMessage) as ISmsMessage;
end;

class function CoSmsMessage.CreateRemote(const MachineName: string): ISmsMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsMessage) as ISmsMessage;
end;

class function CoSmsMessageCollection.Create: ISmsMessageCollection;
begin
  Result := CreateComObject(CLASS_SmsMessageCollection) as ISmsMessageCollection;
end;

class function CoSmsMessageCollection.CreateRemote(const MachineName: string): ISmsMessageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsMessageCollection) as ISmsMessageCollection;
end;

class function CoSmsChunk.Create: ISmsChunk;
begin
  Result := CreateComObject(CLASS_SmsChunk) as ISmsChunk;
end;

class function CoSmsChunk.CreateRemote(const MachineName: string): ISmsChunk;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsChunk) as ISmsChunk;
end;

class function CoSmsChunkCollection.Create: ISmsChunkCollection;
begin
  Result := CreateComObject(CLASS_SmsChunkCollection) as ISmsChunkCollection;
end;

class function CoSmsChunkCollection.CreateRemote(const MachineName: string): ISmsChunkCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsChunkCollection) as ISmsChunkCollection;
end;

class function CoSmsTarget.Create: ISmsTarget;
begin
  Result := CreateComObject(CLASS_SmsTarget) as ISmsTarget;
end;

class function CoSmsTarget.CreateRemote(const MachineName: string): ISmsTarget;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsTarget) as ISmsTarget;
end;

class function CoSmsTargetCollection.Create: ISmsTargetCollection;
begin
  Result := CreateComObject(CLASS_SmsTargetCollection) as ISmsTargetCollection;
end;

class function CoSmsTargetCollection.CreateRemote(const MachineName: string): ISmsTargetCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SmsTargetCollection) as ISmsTargetCollection;
end;

class function CoPluginMenuItem.Create: IPluginMenuItem;
begin
  Result := CreateComObject(CLASS_PluginMenuItem) as IPluginMenuItem;
end;

class function CoPluginMenuItem.CreateRemote(const MachineName: string): IPluginMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PluginMenuItem) as IPluginMenuItem;
end;

class function CoPluginEvent.Create: IPluginEvent;
begin
  Result := CreateComObject(CLASS_PluginEvent) as IPluginEvent;
end;

class function CoPluginEvent.CreateRemote(const MachineName: string): IPluginEvent;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PluginEvent) as IPluginEvent;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TSkype]);
end;

end.
