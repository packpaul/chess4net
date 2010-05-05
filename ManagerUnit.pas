unit ManagerUnit;

{$DEFINE GAME_LOG}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, TntMenus, ActnList, TntActnList, ExtCtrls,
{$IFDEF TRILLIAN}
  plugin,
{$ENDIF}
  // Chess4Net Units
  ChessBoardHeaderUnit, ChessRulesEngine, ChessBoardUnit, PosBaseChessBoardUnit,
  ConnectorUnit, ConnectingUnit, GameOptionsUnit,
  ModalForm, DialogUnit, ContinueUnit, LocalizerUnit;

type
  TManager = class(TForm, ILocalizable)
    ActionList: TTntActionList;
    LookFeelOptionsAction: TTntAction;
    AboutAction: TTntAction;

    ConnectedPopupMenu: TTntPopupMenu;
    LookFeelOptionsConnected: TTntMenuItem;
    StartStandartGameConnected: TTntMenuItem;
    StartPPRandomGameConnected: TTntMenuItem;
    GameOptionsConnected: TTntMenuItem;
    ChangeColorConnected: TTntMenuItem;
    GamePopupMenu: TTntPopupMenu;
    AbortGame: TTntMenuItem;
    DrawGame: TTntMenuItem;
    ResignGame: TTntMenuItem;
    N4: TTntMenuItem;
    LookFeelOptionsGame: TTntMenuItem;
    TakebackGame: TTntMenuItem;
    GamePause: TTntMenuItem;
    N1: TTntMenuItem;
    AboutConnected: TTntMenuItem;
    N2: TTntMenuItem;
    AboutGame: TTntMenuItem;
    StartAdjournedGameConnected: TTntMenuItem;
    AdjournGame: TTntMenuItem;
    N5: TTntMenuItem;
    N6: TTntMenuItem;

    ConnectorTimer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure LookFeelOptionsActionExecute(Sender: TObject);
    procedure AbortGameClick(Sender: TObject);
    procedure DrawGameClick(Sender: TObject);
    procedure ResignGameClick(Sender: TObject);
    procedure ChangeColorConnectedClick(Sender: TObject);
    procedure GameOptionsConnectedClick(Sender: TObject);
    procedure StartStandartGameConnectedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ConnectorTimerTimer(Sender: TObject);
    procedure StartPPRandomGameConnectedClick(Sender: TObject);
    procedure TakebackGameClick(Sender: TObject);
    procedure GamePauseClick(Sender: TObject);
    procedure AboutActionExecute(Sender: TObject);
    procedure StartAdjournedGameConnectedClick(Sender: TObject);
    procedure AdjournGameClick(Sender: TObject);
    procedure GamePopupMenuPopup(Sender: TObject);

  private
    m_strAdjourned: string;
    m_ConnectingForm: TConnectingForm;
    m_ContinueForm: TContinueForm;
    m_Connector: TConnector;
    m_ChessBoard: TPosBaseChessBoard;
    m_Dialogs: TDialogs;    

    m_ExtBaseList: TStringList;
    m_strExtBaseName: string;
{$IFDEF QIP}
    iProtoDllHandle: integer;
    wAccName: WideString;
    QIPConnectionError: boolean;
{$ENDIF}
{$IFDEF TRILLIAN}
    contactlistEntry: TTtkContactListEntry;
{$ENDIF}
{$IFDEF SKYPE}
    SkypeConnectionError: boolean;
    bDontShowCredits: boolean;
{$ENDIF}
    m_lwOpponentClientVersion: LongWord;
    // для ChessBoard
    you_unlimited, opponent_unlimited: boolean;
    you_time, opponent_time,
    you_inc, opponent_inc: word;
    you_takebacks, opponent_takebacks: boolean;
    can_pause_game, can_adjourn_game, move_done: boolean;

    m_strPlayerNick, m_strOpponentNick: string;
    m_strOpponentId: string;
    m_strPlayerNickId: string;

    extra_exit: boolean;
    m_bConnectionOccured: boolean;
{$IFDEF GAME_LOG}
    // для лога игры
    gameLog: string;
    procedure InitGameLog;
    procedure WriteToGameLog(const s: string);
    procedure FlushGameLog;
{$ENDIF}
    procedure ChessBoardHandler(e: TChessBoardEvent;
                                d1: pointer = nil; d2: pointer = nil);
    procedure SetClock; overload;
    procedure SetClock(var sr: string); overload;
    procedure DialogFormHandler(modSender: TModalForm; msgDlgID: TModalFormID);
    procedure CloseConnector;
    procedure FPopulateExtBaseList;
    function SetCommonSettings(setToOpponent: boolean): boolean;
    procedure WriteSettings;
    function ClockToStr: string;
    procedure ChangeColor;
    procedure PauseGame;
    procedure ContinueGame;
    procedure FAdjournGame;
    procedure FExitGameMode;
    procedure FBuildAdjournedStr;
    procedure FStartAdjournedGame;

    function FGetOpponentNickId: string;
{$IFDEF SKYPE}
    procedure FShowCredits;
{$ENDIF}

    property AdjournedStr: string read m_strAdjourned write m_strAdjourned;
    property PlayerNickId: string read m_strPlayerNickId;
    property OpponentNickId: string read FGetOpponentNickId;

  protected
    constructor RCreate;

    procedure ROnCreate; virtual; abstract;
    procedure ROnDestroy; virtual;

    procedure ConnectorHandler(e: TConnectorEvent;
      d1: pointer = nil; d2: pointer = nil); virtual;
    procedure RCreateChessBoardAndDialogs;
    procedure RCreateAndPopulateExtBaseList;
    procedure RSetChessBoardToView;
    procedure RSetPrivateSettings;
    procedure RShowConnectingForm;

    procedure ILocalizable.Localize = RLocalize;
    procedure RLocalize;

    class procedure RSplitStr(s: string; var strLeft: string; var strRight: string);
    procedure RHandleConnectorDataCommand(sl: string);
    procedure RSetOpponentClientVersion(lwVersion: LongWord); virtual;

    procedure RSendData(const cmd: string = ''); virtual; abstract;

    procedure RSetConnectionOccured; virtual;

    property Connector: TConnector read m_Connector write m_Connector;
    property ChessBoard: TPosBaseChessBoard read m_ChessBoard write m_ChessBoard;

    property PlayerNick: string read m_strPlayerNick write m_strPlayerNick;
    property OpponentNick: string read m_strOpponentNick write m_strOpponentNick;
    property OpponentId: string read m_strOpponentId write m_strOpponentId;

  public
{$IFDEF AND_RQ}
    class function Create: TManager; reintroduce;
{$ENDIF}
{$IFDEF QIP}
    class function Create(const accName: WideString; const protoDllHandle: integer): TManager; reintroduce;
{$ENDIF}
{$IFDEF TRILLIAN}
    class function Create(const vContactlistEntry: TTtkContactListEntry): TManager; reintroduce;
{$ENDIF}
{$IFDEF SKYPE}
    class function Create: TManager; reintroduce;
{$ENDIF}
  end;

implementation

{$R *.dfm}
{$J+}

uses
  // Chess4Net
  DateUtils, Math, StrUtils, TntIniFiles, Dialogs,
  LookFeelOptionsUnit, GlobalsLocalUnit, InfoUnit
{$IFDEF AND_RQ}
  , CallExec
{$ENDIF}
{$IFDEF QIP}
  , ControlUnit
{$ENDIF}
{$IFDEF SKYPE}
  , SelectSkypeContactUnit, CreditsFormUnit
{$ENDIF}
  ;

const
  USR_BASE_NAME = 'Chess4Net';
  INI_FILE_NAME = 'Chess4net.ini';

  INITIAL_CLOCK_TIME = '5 0 5 0'; // 5:00 5:00
  NO_CLOCK_TIME ='u u';

  FULL_TIME_FORMAT = 'h:n:s"."z';
  HOUR_TIME_FORMAT = 'h:nn:ss';

  // Command shorthands for Connector
  CMD_ECHO = 'echo';
  CMD_START_GAME = 'strt';
  CMD_GAME_OPTIONS = 'gmopt'; // Doesn't exist from 2007.5
  CMD_CHANGE_COLOR = 'chclr';
  CMD_NICK_ID = 'nkid';
  CMD_RESIGN = 'res';
  CMD_ABORT = 'abrt';
  CMD_ABORT_ACCEPTED = 'abrtacc';
  CMD_ABORT_DECLINED = 'abrtdec';
  CMD_DRAW = 'draw';
  CMD_DRAW_ACCEPTED = 'drawacc';
  CMD_DRAW_DECLINED = 'drawdec';
  CMD_FLAG = 'flg';
  CMD_FLAG_YES = 'flgyes';
  CMD_FLAG_NO = 'flgno';
  CMD_TAKEBACK = 'tkbk';
  CMD_TAKEBACK_YES = 'tkbkyes';
  CMD_TAKEBACK_NO = 'tkbkno';
  CMD_SWITCH_CLOCK = 'swclck';
  CMD_REPEAT_COMMAND = 'rptcmd';
  CMD_POSITION = 'pos';
  CMD_VERSION = 'ver';
  CMD_WELCOME = 'wlcm'; // Accept of connection
  CMD_GOODBYE = 'gdb'; // Refusion of connection
  // существует с 2007.5
  CMD_NO_SETTINGS = 'noset'; // If global settings are absent then request from partner's client
  CMD_ALLOW_TAKEBACKS = 'alwtkb';
  CMD_SET_CLOCK = 'clck'; // Change of timing  
  CMD_SET_TRAINING = 'trnng'; // Setting training mode
  // Существует с 2007.6
  CMD_CAN_PAUSE_GAME = 'canpaus';
  CMD_PAUSE_GAME = 'paus';
  CMD_PAUSE_GAME_YES = 'pausyes';
  CMD_PAUSE_GAME_NO = 'pausno';
  CMD_CONTINUE_GAME = 'cont';
  // Существует с 2008.1
  CMD_CAN_ADJOURN_GAME = 'canadj';
  CMD_SET_ADJOURNED = 'setadj'; // Setting of adj. position and timing
  CMD_ADJOURN_GAME = 'adj';
  CMD_ADJOURN_GAME_YES = 'adjyes';
  CMD_ADJOURN_GAME_NO = 'adjno';
  CMD_START_ADJOURNED_GAME = 'strtadj';

  CMD_DELIMITER = '&&'; // CMD_DELIMITER has to be present in arguments

  // CMD_DELIMITER = 'ext' - IS RESERVED

  // INI-file
  PRIVATE_SECTION_NAME = 'Private';
  COMMON_SECTION_PREFIX = 'Common';
  ANIMATION_KEY_NAME = 'Animation';
  HILIGHT_LAST_MOVE_KEY_NAME = 'HilightLastMove';
  FLASH_ON_MOVE_NAME = 'FlashOnMove';
  SHOW_COORDINATES_KEY_NAME = 'ShowCoordinates';
  STAY_ON_TOP_KEY_NAME = 'StayOnTop';
  EXTRA_EXIT_KEY_NAME = 'ExtraExit';
  CAN_PAUSE_GAME_KEY_NAME = 'CanPauseGame';
  CAN_ADJOURN_GAME_KEY_NAME = 'CanAdjournGame';
  ALLOW_TAKEBACKS_KEY_NAME = 'AllowTakebacks';
  EXTERNAL_BASE_NAME_KEY_NAME = 'ExternalBaseName';
  USE_USER_BASE_KEY_NAME = 'UseUserBase';
  AUTO_FLAG_KEY_NAME = 'AutoFlag';
  TRAINING_MODE_KEY_NAME = 'TrainingMode';
  PLAYER_COLOR_KEY_NAME = 'PlayerColor';
  CLOCK_KEY_NAME = 'Clock';
  ADJOURNED_KEY_NAME = 'Adjourned';
  LANGUAGE_KEY_NAME = 'Language';
{$IFDEF SKYPE}
  DONT_SHOW_CREDITS = 'DontShowCredits';
{$ENDIF}

type
  TManagerDefault = class(TManager) // TODO: TRILLIAN, AND_RQ, QIP, SKYPE -> own classes
  protected
    procedure ROnCreate; override;
    procedure ROnDestroy; override;
    procedure RSendData(const cmd: string = ''); override;
  public
{$IFDEF AND_RQ}
    constructor Create; reintroduce;
{$ENDIF}
{$IFDEF QIP}
    constructor Create(const accName: WideString; const protoDllHandle: integer); reintroduce;
{$ENDIF}
{$IFDEF TRILLIAN}
    constructor Create(const vContactlistEntry: TTtkContactListEntry); reintroduce;
{$ENDIF}
{$IFDEF SKYPE}
    constructor Create; reintroduce;
{$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////
// TManager

procedure TManager.RCreateChessBoardAndDialogs;
begin
//  m_ChessBoard := TPosBaseChessBoard.Create(self, ChessBoardHandler, Chess4NetPath + USR_BASE_NAME);
  m_ChessBoard := TPosBaseChessBoard.Create(nil, ChessBoardHandler, Chess4NetPath + USR_BASE_NAME);
  m_Dialogs := TDialogs.Create(ChessBoard, DialogFormHandler);
end;


procedure TManager.FormCreate(Sender: TObject);
begin
  ROnCreate;
end;


procedure TManager.RShowConnectingForm;
begin
  m_ConnectingForm := (m_Dialogs.CreateDialog(TConnectingForm) as TConnectingForm);
  m_ConnectingForm.Show;
end;


procedure TManager.ChessBoardHandler(e: TChessBoardEvent;
                            d1: pointer = nil; d2: pointer = nil);
var
  s: string;
  wstrMsg1, wstrMsg2: WideString;
begin
  case e of
    cbeKeyPressed:
      if extra_exit and (Word(d1) = VK_ESCAPE) then
        begin
{$IFDEF GAME_LOG}
          if ChessBoard.Mode = mGame then
		        begin
              WriteToGameLog('*');
              FlushGameLog;
		        end;
{$ENDIF}
          Release;
        end;

    cbeExit:
      Close;

    cbeMenu:
      if (not m_Dialogs.Showing) then
      begin
        case ChessBoard.Mode of
          mView:
            if (Connector.connected) then
              ConnectedPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          mGame:
            GamePopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;
      end;

    cbeMoved:
      begin
        RSendData(PString(d1)^);
{$IFDEF GAME_LOG}
        if (ChessBoard.PositionColor = fcBlack) or (not move_done) then
          begin
            WriteToGameLog(' ' + IntToStr(ChessBoard.NMoveDone) + '.');
            if ChessBoard.PositionColor = fcWhite then
              WriteToGameLog(' ...');
          end;
        WriteToGameLog(' ' + PString(d1)^);
{$ENDIF}
        move_done := TRUE;
        TakebackGame.Enabled := TRUE;
      end;

    cbeMate:
      with ChessBoard do
      begin
        FExitGameMode;
{$IFDEF GAME_LOG}
        WriteToGameLog('#');
        if PositionColor = fcWhite then WriteToGameLog(#13#10 + '0 - 1')
          else WriteToGameLog(#13#10 + '1 - 0');
        FlushGameLog;
{$ENDIF}
        with TLocalizer.Instance do
          if (PositionColor = fcWhite) then
          begin
            wstrMsg1 := GetMessage(0); // White is checkmated. You win.
            wstrMsg2 := GetMessage(1); // White is checkmated. You loose.
          end
          else
          begin
            wstrMsg1 := GetMessage(2); // Black is checkmated. You win.
            wstrMsg2 := GetMessage(3); // Black is checkmated. You loose.
          end;
        if ((PlayerColor <> fcWhite) and (PositionColor = fcWhite)) or
           ((PlayerColor <> fcBlack) and (PositionColor = fcBlack)) then
        begin
          m_Dialogs.MessageDlg(wstrMsg1, mtCustom, [mbOK], mfNone);
          ChessBoard.WriteGameToBase(grWin);
        end
        else
        begin
          m_Dialogs.MessageDlg(wstrMsg2, mtCustom, [mbOK], mfNone);
          ChessBoard.WriteGameToBase(grLost);
        end;
      end;

    cbeStaleMate:
    begin
      FExitGameMode;
{$IFDEF GAME_LOG}
      WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
      FlushGameLog;
{$ENDIF}
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(4), mtCustom, [mbOK], mfNone); // It's stalemate. No one wins.
      ChessBoard.WriteGameToBase(grDraw);
    end;

    cbeClockSwitched:
      with ChessBoard do
      begin
        if (move_done and (ClockColor = PositionColor)) then
        begin
          if (ClockColor <> PlayerColor) then
          begin
            Time[PlayerColor] := IncSecond(Time[PlayerColor], you_inc);
            LongTimeFormat:= FULL_TIME_FORMAT;
            s := TimeToStr(Time[PlayerColor]);
            if (not Unlimited[PlayerColor] or (m_lwOpponentClientVersion < 200706)) then
              RSendData(CMD_SWITCH_CLOCK + ' ' + s);
          end
          else
          begin
            if (PlayerColor = fcWhite) then
              Time[fcBlack] := IncSecond(Time[fcBlack], opponent_inc)
            else
              Time[fcWhite] := IncSecond(Time[fcWhite], opponent_inc);
          end;
        end;
      end; { with }

    cbeTimeOut:
    begin
      RSendData(CMD_FLAG);
    end;

    cbeActivate:
    begin
      m_Dialogs.BringToFront;
    end;

    cbeFormMoving:
    begin
      m_Dialogs.MoveForms(integer(d1), integer(d2));
    end;
  end;
end;


class procedure TManager.RSplitStr(s: string; var strLeft: string; var strRight: string);
var
  x: integer;
begin
  x := pos(' ', s);
  strLeft := copy(s, 1, sign(x) * (x - 1) + (1 - sign(x)) * length(s));
  strRight := copy(s, length(strLeft) + 2, length(s));
end;


procedure TManager.SetClock(var sr: string);
var
  sl: string;
begin
  RSplitStr(sr, sl, sr);
  if sl = 'u' then
    opponent_unlimited := TRUE
  else
    begin
      opponent_unlimited:= FALSE;
      opponent_time:= StrToInt(sl);
      RSplitStr(sr, sl, sr);
      opponent_inc := StrToInt(sl);
    end;
    RSplitStr(sr, sl, sr);
    if sl = 'u' then
      you_unlimited:= TRUE
    else
      begin
        you_unlimited := FALSE;
        you_time := StrToInt(sl);
        RSplitStr(sr, sl, sr);
        you_inc := StrToInt(sl);
      end;
  SetClock;
end;


procedure TManager.ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
var
  strCmd: string;
  strLeft: string;
begin
  case e of
    ceConnected:
    begin
{$IFDEF SKYPE}
      PlayerNick := Connector.UserHandle;
      OpponentNick := Connector.ContactHandle;
      OpponentId := OpponentNick;
{$ENDIF}
      if (Assigned(m_ConnectingForm)) then
        m_ConnectingForm.Shut;
      RSendData(CMD_VERSION + ' ' + IntToStr(CHESS4NET_VERSION));
    end;

    ceDisconnected:
    begin
      if (not Connector.connected) then
      begin
{$IFDEF SKYPE}
        Application.Terminate; // KLUDGE
{$ENDIF}
        exit;
      end;
      case ChessBoard.Mode of
        mView:
        begin
          m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(5), mtCustom, [mbOK],
            mfMsgLeave); // 'Your opponent leaves.'
        end;
        mGame:
        begin
{$IFDEF GAME_LOG}
          WriteToGameLog('*');
          FlushGameLog;
{$ENDIF}
          m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(6), mtWarning,
                             [mbOK], mfMsgLeave); // Your opponent leaves. The game is aborted.
        end;
      end;
    end; { ceDisconnected }

    ceError:
    begin
{$IFDEF GAME_LOG}
      if ChessBoard.Mode = mGame then
      begin
        WriteToGameLog('*');
        FlushGameLog;
      end;
{$ENDIF}
        m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(7), mtWarning,
                           [mbOk], mfMsgLeave); // An error during connection occured.
    end;
{$IFDEF QIP}
    ceQIPError:
    begin
      QIPConnectionError := TRUE;
      // TODO: Localize
      m_Dialogs.MessageDlg('Special message channel is not responding.' + sLineBreak +
                         'This can happen due to the following reasons:' + sLineBreak +
                         '  1) Your partner is using an IM other than QIP Infium. OR' + sLineBreak +
                         '  2) Your partner is offline. OR' + sLineBreak +
                         '  3) Protocol doesn''t support multiple channels. OR' + sLineBreak +
                         '  4) Other reasons.' + sLineBreak +
                         'Chess4Net won''t start.', mtWarning, [mbOk], mfMsgLeave);
    end;
{$ENDIF}
{$IFDEF SKYPE}
    ceSkypeError:
    begin
      SkypeConnectionError := TRUE;
      // TODO: Localize
      m_Dialogs.MessageDlg('Chess4Net was unable to attach to your Skype application' + sLineBreak +
                         'This can happen due to the following reasons:' + sLineBreak +
                         '  1) You have an old version of Skype. OR' + sLineBreak +
                         '  2) Your Skype is blocking Chess4Net. OR' + sLineBreak +
                         '  3) Your Skype doesn''t support Skype applications. OR' + sLineBreak +
                         '  4) Other reasons.' + sLineBreak +
                         'Chess4Net won''t start.', mtWarning, [mbOk], mfMsgLeave);
    end;

    ceShowConnectableUsers:
    begin
      if (Assigned(ConnectingForm)) then
        ConnectingForm.ShowSkypeAcceptLogo := FALSE;
      with m_Dialogs.CreateDialog(TSelectSkypeContactForm) as TSelectSkypeContactForm do
      begin
        Init(d1);
        Show;
      end;
    end;
{$ENDIF}

    ceData:
    begin
      strCmd := PString(d1)^;
      repeat
        strLeft := LeftStr(strCmd, pos(CMD_DELIMITER, strCmd) - 1);
        strCmd := RightStr(strCmd, length(strCmd) - length(strLeft) - length(CMD_DELIMITER));

        RHandleConnectorDataCommand(strLeft);
      until (strCmd = '');

    end; {  ceData }

  end; { case ChessBoard.Mode }
end;


procedure TManager.RSetOpponentClientVersion(lwVersion: LongWord);
begin
  m_lwOpponentClientVersion := lwVersion;
end;


procedure TManager.RSetConnectionOccured;
begin
  m_bConnectionOccured := TRUE;
end;


procedure TManager.RHandleConnectorDataCommand(sl: string);
var
  AMode: TMode;
  sr: string;
  ms: string;
begin
  RSplitStr(sl, sl, sr);

  if (Assigned(ChessBoard)) then
    AMode := ChessBoard.Mode
  else
    AMode := mView;

  case AMode of
  mView:
    if (sl = CMD_VERSION) then
    begin
      RSplitStr(sr, sl, sr);
      RSetOpponentClientVersion(StrToInt(sl));
      RSendData(CMD_WELCOME);
      if (m_lwOpponentClientVersion < CHESS4NET_VERSION) then
      begin
        m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(8), mtWarning,
          [mbOK], mfNone); // Your opponent is using an older version of Chess4Net. ...
      end;

        // 2007.4 is the first client with a backward compatibility
        // For incompatible versions:
        // else RSendData(CMD_GOODBYE);
    end
    else if (sl = CMD_WELCOME) then
    begin
      RSendData(CMD_NICK_ID + ' ' + OpponentNickId);
      if (Assigned(ChessBoard)) then
        ChessBoard.InitPosition;
      SetClock;
      RSetConnectionOccured;
    end
    else if (sl = CMD_GOODBYE) then // For the future versions
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(9) , mtWarning, [mbOK], mfIncompatible); // The current version of Chess4Net is incompatible ...
    end
    else if (sl = CMD_START_GAME) then
      with ChessBoard do
      begin
        // Starting from 2007.6 only white can start the game
        if ((m_lwOpponentClientVersion >= 200706) and (PlayerColor = fcWhite)) then
          ChangeColor;
        SetClock;
        ResetMoveList;
        Mode := mGame;
        move_done:= FALSE;
        TakebackGame.Enabled := FALSE;
        SwitchClock(PositionColor);
{$IFDEF GAME_LOG}
        InitGameLog;
{$ENDIF}
      end
    else if (sl = CMD_START_ADJOURNED_GAME) then
    begin
      if ChessBoard.Mode <> mGame then
        FStartAdjournedGame;
    end
    else if (sl = CMD_ALLOW_TAKEBACKS) then
    begin
      RSplitStr(sr, sl, sr);
      opponent_takebacks := (sl = '1');
      TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
    end
    else if (sl = CMD_CAN_PAUSE_GAME) then
    begin
      RSplitStr(sr, sl, sr);
      can_pause_game := (sl = '1');
      GamePause.Visible := can_pause_game;
    end
    else if (sl = CMD_CAN_ADJOURN_GAME) then
    begin
      RSplitStr(sr, sl, sr);
      can_adjourn_game := (sl = '1');
    end
    else if (sl = CMD_SET_CLOCK) then
    begin
      SetClock(sr);
    end
    else if (sl = CMD_SET_TRAINING) then
    begin
      RSplitStr(sr, sl, sr);
      ChessBoard.pTrainingMode := (sl = '1');
      TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
    end
    else if (sl = CMD_GAME_OPTIONS) then // 2007.4
    begin
      SetClock(sr);
      RSplitStr(sr, sl, sr);
      opponent_takebacks := (sl = '1');
      RSplitStr(sr, sl, sr);
      ChessBoard.pTrainingMode := (sl = '1');
      TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
    end
    else if (sl = CMD_SET_ADJOURNED) then // 2008.1
    begin
      if ((AdjournedStr = '') or (CompareStr(PlayerNickId, OpponentNickId) > 0)) then
      begin
        if pos('&w&', sr) > 0 then
          sr := StringReplace(sr, '&w&', '&b&', []) // White -> Black
        else
          sr := StringReplace(sr, '&b&', '&w&', []); // Black -> White
        AdjournedStr := sr;
      end;
    end
    else if (sl = CMD_CHANGE_COLOR) then
    begin
      ChangeColor;
    end
    else if (sl = CMD_NICK_ID) then
    begin
      m_strPlayerNickId := sr;
      if CompareStr(PlayerNickId, OpponentNickId) < 0 then
      begin
        StartStandartGameConnected.Enabled := TRUE;
        StartPPRandomGameConnected.Enabled := TRUE;
        if (Assigned(ChessBoard)) then
        begin
          ChessBoard.PlayerColor := fcWhite;
          ChessBoard.Caption := PlayerNick + ' - ' + OpponentNick;
        end;
        if (not SetCommonSettings(TRUE)) then
          RSendData(CMD_NO_SETTINGS);
      end
      else
      begin
        StartStandartGameConnected.Enabled := FALSE;
        StartPPRandomGameConnected.Enabled := FALSE;
        if (Assigned(ChessBoard)) then
        begin
          ChessBoard.PlayerColor := fcBlack;
          ChessBoard.Caption := OpponentNick + ' - ' + PlayerNick;
        end;
        SetCommonSettings(FALSE);
      end; // if CompareStr
    end
    else if (sl = CMD_POSITION) then
    begin
      if (Assigned(ChessBoard)) then
        ChessBoard.SetPosition(sr);
    end
    else if (sl = CMD_NO_SETTINGS) then
    begin
      SetCommonSettings(TRUE);
    end;

  mGame:
    if (sl = CMD_DRAW) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(10), mtConfirmation,
        [mbYes, mbNo], mfMsgDraw) // Draw?
    end
    else if (sl = CMD_ABORT) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(11), mtConfirmation,
        [mbYes, mbNo], mfMsgAbort); // Can we abort the game?
    end
    else if (sl = CMD_RESIGN) then
    begin
      FExitGameMode;
{$IFDEF GAME_LOG}
      if ChessBoard.PlayerColor = fcWhite then
        WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0')
      else
        WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1');
      FlushGameLog;
{$ENDIF}
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(12),
                         mtCustom, [mbOK], mfNone); // I resign. You win this game. Congratulations!
      ChessBoard.WriteGameToBase(grWin);
    end
    else if (sl = CMD_ABORT_ACCEPTED) then
    begin
      FExitGameMode;
{$IFDEF GAME_LOG}
      WriteToGameLog('*');
      FlushGameLog;
{$ENDIF}
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(13), mtCustom,
        [mbOK], mfNone); // The game is aborted.
    end
    else if (sl = CMD_ABORT_DECLINED) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(14),
        mtCustom, [mbOK], mfNone) // Sorry, but we have to finish this game.
    end
    else if (sl = CMD_DRAW_ACCEPTED) then
    begin
      FExitGameMode;
{$IFDEF GAME_LOG}
      WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
      FlushGameLog;
{$ENDIF}
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(15), mtCustom, [mbOK], mfNone); // The game is drawn.
      ChessBoard.WriteGameToBase(grDraw);
    end
    else if (sl = CMD_DRAW_DECLINED) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(16), mtCustom, [mbOK], mfNone) // No draw, sorry.
    end
    else if (sl = CMD_SWITCH_CLOCK) then
      with ChessBoard do
      begin
        RSplitStr(sr, sl, sr);
        ms := RightStr(sl, length(sl) - LastDelimiter(':.', sl));
        sl := LeftStr(sl, length(sl) - length(ms) - 1);
        if (PlayerColor = fcWhite) then
          Time[fcBlack] := StrToTime(sl) + EncodeTime(0, 0, 0, StrToInt(ms))
        else
          Time[fcWhite] := StrToTime(sl) + EncodeTime(0, 0, 0, StrToInt(ms));
      end // with
    else if (sl = CMD_FLAG) then
      with ChessBoard do
      begin
        if (Time[PlayerColor] = 0.0) then
          begin
            RSendData(CMD_FLAG_YES);
            FExitGameMode;
{$IFDEF GAME_LOG}
            if (ChessBoard.PlayerColor = fcWhite) then
              WriteToGameLog(#13#10 + 'White forfeits on time')
            else
              WriteToGameLog(#13#10 + 'Black forfeits on time');
            FlushGameLog;
{$ENDIF}
            m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(17), mtCustom, [mbOK], mfNone); // You forfeited on time.
            ChessBoard.WriteGameToBase(grLostTime);
          end
        else
          RSendData(CMD_FLAG_NO);
      end // with
    else
    if (sl = CMD_FLAG_YES) then
    begin
      FExitGameMode;
{$IFDEF GAME_LOG}
      if (ChessBoard.PlayerColor = fcWhite) then
        WriteToGameLog(#13#10 + 'Black forfeits on time')
      else
        WriteToGameLog(#13#10 + 'White forfeits on time');
      FlushGameLog;
{$ENDIF}
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(18), mtCustom,
        [mbOK], mfNone); // Your opponent forfeited on time.
      ChessBoard.WriteGameToBase(grWinTime);
    end
    else if (sl = CMD_FLAG_NO) then
      with ChessBoard do
      begin
        case PlayerColor of
          fcWhite:
            if (Time[fcBlack] = 0.0) then
              RSendData(CMD_FLAG);
          fcBlack:
            if (Time[fcWhite] = 0.0) then
              RSendData(CMD_FLAG);
        end // case
      end // with
    else if (sl = CMD_PAUSE_GAME) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(19), mtConfirmation,
        [mbYes, mbNo], mfCanPause); // Can we pause the game?
    end
    else if (sl = CMD_PAUSE_GAME_YES) then
    begin
      PauseGame;
    end
    else if (sl = CMD_PAUSE_GAME_NO) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(20), mtCustom,
        [mbOk], mfNone); // No pause, sorry.
    end
    else if (sl = CMD_CONTINUE_GAME) then
    begin
      if (Assigned(m_ContinueForm)) then
        m_ContinueForm.Shut;
      ContinueGame;
    end
    else if (sl = CMD_TAKEBACK) then
    begin
      if (you_takebacks or ChessBoard.pTrainingMode) then
      begin
        m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(21),
                           mtConfirmation, [mbYes, mbNo], mfMsgTakeBack); // 'May I take back last move?'
      end
      else
        RSendData(CMD_TAKEBACK_NO)
    end
    else if (sl = CMD_ADJOURN_GAME) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(22),
                             mtConfirmation, [mbYes, mbNo], mfMsgAdjourn); // Can we adjourn this game?
    end
    else if (sl = CMD_ADJOURN_GAME_YES) then
    begin
      FAdjournGame;
    end
    else if (sl = CMD_ADJOURN_GAME_NO) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(23), mtCustom, [mbOk],
        mfNone); // No adjourns, sorry.
    end
    else
    if (sl = CMD_TAKEBACK_YES) then
    begin
      ChessBoard.TakeBack;
      FBuildAdjournedStr;
      TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
      WriteToGameLog(' <takeback>');
{$ENDIF}
      ChessBoard.SwitchClock(ChessBoard.PositionColor);
    end
    else if (sl = CMD_TAKEBACK_NO) then
    begin
      m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(24), mtCustom,
        [mbOK], mfNone); // Sorry, no takebacks!
    end
    else if ((sl = CMD_POSITION) and (CompareStr(PlayerNickId, OpponentNickId) > 0)) then
    begin
      ChessBoard.StopClock;
      ChessBoard.Mode := mView;
      ChessBoard.SetPosition(sr);
    end
    else
      with ChessBoard do
      begin
        if (PlayerColor <> PositionColor) and DoMove(sl) then
          begin
{$IFDEF GAME_LOG}
            if ((PositionColor = fcBlack) or (not move_done)) then
            begin
              WriteToGameLog(' ' + IntToStr(NMoveDone) + '.');
              if (PositionColor = fcWhite) then
                WriteToGameLog(' ...');
            end;
            WriteToGameLog(' ' + sl);
{$ENDIF}
            move_done := TRUE;
            TakebackGame.Enabled := TRUE;
            FBuildAdjournedStr; // AdjournedStr помечается только при входящем ходе противника
          end;
      end; // with ChessBoard
  end; // case ChessBoard.Mode
end;


procedure TManager.ROnDestroy;
begin
  TLocalizer.Instance.DeleteSubscriber(self);

  if (m_bConnectionOccured) then
    WriteSettings;

  m_ExtBaseList.Free;

  if (Assigned(ChessBoard)) then
  begin
    ChessBoard.Release;
    m_ChessBoard := nil;
  end;
  m_Dialogs.Free;
end;


procedure TManager.FormDestroy(Sender: TObject);
begin
  ROnDestroy;
end;


procedure TManager.LookFeelOptionsActionExecute(Sender: TObject);
var
  lookFeelOptionsForm: TLookFeelOptionsForm;
begin
  lookFeelOptionsForm := (m_Dialogs.CreateDialog(TLookFeelOptionsForm) as TLookFeelOptionsForm);
  with lookFeelOptionsForm, ChessBoard do
    begin
      AnimationComboBox.ItemIndex := ord(animation);
      HilightLastMoveBox.Checked := LastMoveHilighted;
      FlashIncomingMoveBox.Checked := FlashOnMove;
      CoordinatesBox.Checked := CoordinatesShown;
      StayOnTopBox.Checked := StayOnTop;
      ExtraExitBox.Checked := extra_exit;
    end;
  lookFeelOptionsForm.Show;
end;


procedure TManager.AbortGameClick(Sender: TObject);
begin
  RSendData(CMD_ABORT);
end;

procedure TManager.DrawGameClick(Sender: TObject);
begin
  RSendData(CMD_DRAW);
end;

procedure TManager.ResignGameClick(Sender: TObject);
begin
  m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(25),
                mtConfirmation, [mbYes, mbNo], mfMsgResign); // Do you really want to resign?
end;


procedure TManager.ChangeColorConnectedClick(Sender: TObject);
begin
  if ChessBoard.Mode = mGame then
    exit;
  ChangeColor;
  RSendData(CMD_CHANGE_COLOR);
end;


procedure TManager.GameOptionsConnectedClick(Sender: TObject);
var
  GameOptionsForm: TGameOptionsForm;
  i: integer;
begin
  GameOptionsForm := (m_Dialogs.CreateDialog(TGameOptionsForm) as TGameOptionsForm);
  with GameOptionsForm do
  begin
    EqualTimeCheckBox.Checked := ((you_unlimited = opponent_unlimited) and
      (you_time = opponent_time) and (you_inc = opponent_inc));
    YouUnlimitedCheckBox.Checked:= you_unlimited;
    OpponentUnlimitedCheckBox.Checked:= opponent_unlimited;
    YouMinUpDown.Position := you_time;
    YouIncUpDown.Position := you_inc;
    OpponentMinUpDown.Position := opponent_time;
    OpponentIncUpDown.Position := opponent_inc;
    AutoFlagCheckBox.Checked := ChessBoard.AutoFlag;
    TakeBackCheckBox.Checked := you_takebacks;
    TrainingEnabledCheckBox.Checked := ChessBoard.pTrainingMode;
    for i := 1 to m_ExtBaseList.Count - 1 do
    begin
      ExtBaseComboBox.Items.Append(m_ExtBaseList[i]);
      if (m_strExtBaseName = m_ExtBaseList[i]) then
        ExtBaseComboBox.ItemIndex := i;
    end;
    UsrBaseCheckBox.Checked := ChessBoard.pUseUserBase;
    GamePauseCheckBox.Checked := (can_pause_game and (m_lwOpponentClientVersion >= 200706));
    GameAdjournCheckBox.Checked := (can_adjourn_game and (m_lwOpponentClientVersion >= 200801));
    Show;
  end; // with
end;


procedure TManager.StartStandartGameConnectedClick(Sender: TObject);
begin
  with ChessBoard do
    begin
      SetClock;
      InitPosition;
      ResetMoveList;

      RSendData(CMD_POSITION + ' ' + GetPosition);
      RSendData(CMD_START_GAME);

      move_done:= FALSE;
      TakebackGame.Enabled := FALSE;
      Mode := mGame;
      SwitchClock(ChessBoard.PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;


procedure TManager.SetClock;
begin
  if (not Assigned(ChessBoard)) then
    exit;
     
  with ChessBoard do
  begin
    Unlimited[PlayerColor] := you_unlimited;
    Time[PlayerColor] := EncodeTime(you_time div 60, you_time mod 60, 0,0);
    if PlayerColor = fcWhite then
      begin
        Unlimited[fcBlack] := opponent_unlimited;
        Time[fcBlack] := EncodeTime(opponent_time div 60,
                                 opponent_time mod 60, 0,0);
      end
    else
      begin
        Unlimited[fcWhite] := opponent_unlimited;
        Time[fcWhite] := EncodeTime(opponent_time div 60,
                                 opponent_time mod 60, 0,0);
      end;
  end;
end;

procedure TManager.RSetChessBoardToView;
var
  clockTime: string;
begin
  with ChessBoard do
  begin
    clockTime := NO_CLOCK_TIME;
    SetClock(clockTime);
    Mode := mView;
    Caption := CHESS4NET_TITLE;
    ChessBoard.icon := Chess4NetIcon;
    InitPosition;

    Left:= (Screen.Width - Width) div 2;
    Top:= (Screen.Height - Height) div 2;
    Show;
  end;
end;


procedure TManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Assigned(Connector) and Connector.connected) then
  begin
    m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(26), mtConfirmation, [mbYes, mbNo], mfMsgClose); // Do you want to exit?
    Action:= caNone;
  end
  else
//    Release;    
    Action := caFree;
end;


procedure TManager.CloseConnector;
begin
  ConnectorTimer.Enabled := TRUE;
end;


procedure TManager.ConnectorTimerTimer(Sender: TObject);
begin
  ConnectorTimer.Enabled := FALSE;
  if (Assigned(Connector)) then
    Connector.Close;
  Release;
end;


procedure TManager.StartPPRandomGameConnectedClick(Sender: TObject);
begin
  with ChessBoard do
    begin
      SetClock;
      PPRandom;
      ResetMoveList;

      RSendData(CMD_POSITION + ' ' + GetPosition);
      RSendData(CMD_START_GAME);

      Mode := mGame;
      move_done := FALSE;
      TakebackGame.Enabled := FALSE;
      SwitchClock(ChessBoard.PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;


procedure TManager.TakebackGameClick(Sender: TObject);
begin
  RSendData(CMD_TAKEBACK);
end;


constructor TManager.RCreate;
begin
//  inherited Create(Application);
  inherited Create(nil);
end;

{$IFDEF AND_RQ}
class function TManager.Create: TManager;
begin
  Result := TManagerDefault.Create;
end;
{$ENDIF}

{$IFDEF QIP}
class function TManager.Create(const accName: WideString; const protoDllHandle: integer): TManager;
begin
  Result := TManagerDefault.Create(accName, protoDllHandle);
end;
{$ENDIF}

{$IFDEF TRILLIAN}
class function TManager.Create(const vContactlistEntry: TTtkContactListEntry): TManager;
begin
  Result := TManagerDefault.Create(vContactlistEntry);
end;
{$ENDIF}

{$IFDEF SKYPE}
class function TManager.Create: TManager;
begin
  Result := TManagerDefault.Create;
end;
{$ENDIF}


procedure TManager.DialogFormHandler(modSender: TModalForm; msgDlgID: TModalFormID);
var
  modRes: TModalResult;
  s, prevClock: string;
begin
  modRes := modSender.ModalResult;
  case msgDlgID of
    mfNone: ;

    mfMsgClose:
    begin
      if modRes = mrYes then
      begin
{$IFDEF GAME_LOG}
        if ChessBoard.Mode = mGame then
        begin
          WriteToGameLog('*');
          FlushGameLog;
        end;
{$ENDIF}
{$IFDEF SKYPE}
        FShowCredits;
{$ENDIF}
        Release;
      end;
    end;

    mfMsgLeave, mfIncompatible:
    begin
{$IFDEF SKYPE}
      FShowCredits;
{$ENDIF}
      if (Assigned(Connector) and Connector.connected) then
        CloseConnector
      else
        Close;
    end;

    mfMsgAbort:
    begin
      if ChessBoard.Mode = mGame then
      begin
        if (modRes = mrNo) or (modRes = mrNone) then
          RSendData(CMD_ABORT_DECLINED)
        else
        begin
          RSendData(CMD_ABORT_ACCEPTED);
          FExitGameMode;
{$IFDEF GAME_LOG}
          WriteToGameLog('*');
          FlushGameLog;
{$ENDIF}
          m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(13), mtCustom,
            [mbOK], mfNone); // The game is aborted.
        end;
      end;
    end;

    mfMsgResign:
    begin
	    if ChessBoard.Mode = mGame then
      begin
        if modRes = mrYes then
        begin
          FExitGameMode;
          RSendData(CMD_RESIGN);
          ChessBoard.WriteGameToBase(grLost);
{$IFDEF GAME_LOG}
          if ChessBoard.PlayerColor = fcWhite then
            WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1')
          else
            WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0');
          FlushGameLog;
{$ENDIF}
        end;
      end;
    end;

    mfMsgDraw:
    begin
      if ChessBoard.Mode = mGame then
      begin
        if (modRes = mrNo) or (modRes = mrNone) then
          RSendData(CMD_DRAW_DECLINED)
        else
        begin
          RSendData(CMD_DRAW_ACCEPTED);
          FExitGameMode;
{$IFDEF GAME_LOG}
            WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
            FlushGameLog;
{$ENDIF}
          m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(15), mtCustom, [mbOK], mfNone);
          ChessBoard.WriteGameToBase(grDraw); // The game is drawn.
        end;
      end;
    end;

    mfMsgTakeBack:
    begin
      if ChessBoard.Mode = mGame then
      begin
        if modRes = mrYes then
        begin
          RSendData(CMD_TAKEBACK_YES);
          ChessBoard.TakeBack;
          FBuildAdjournedStr;
          TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
          WriteToGameLog(' <takeback>');
{$ENDIF}
          ChessBoard.SwitchClock(ChessBoard.PositionColor);
        end
        else
          RSendData(CMD_TAKEBACK_NO);
      end;
    end;

    mfMsgAdjourn:
    begin
      if ChessBoard.Mode = mGame then
      begin
        if modRes = mrYes then
        begin
          RSendData(CMD_ADJOURN_GAME_YES);
          FAdjournGame;
        end
        else
          RSendData(CMD_ADJOURN_GAME_NO);
      end;
    end;

    mfConnecting:
    begin
      m_ConnectingForm := nil;
      if modRes = mrAbort then
        Close; // ConnectionAbort;
    end;

    mfGameOptions:
    begin
      if (ChessBoard.Mode <> mGame) and (modRes = mrOK) then
        with (modSender as TGameOptionsForm) do
        begin
          prevClock := ClockToStr;
          you_unlimited := YouUnlimitedCheckBox.Checked;
          opponent_unlimited := OpponentUnlimitedCheckBox.Checked;
          you_time := StrToInt(YouMinEdit.Text);
          you_inc := StrToInt(YouIncEdit.Text);
          opponent_time := StrToInt(OpponentMinEdit.Text);
          opponent_inc := StrToInt(OpponentIncEdit.Text);
          ChessBoard.AutoFlag := AutoFlagCheckBox.Checked;
          // Отображение на доске
          SetClock;
          // синхронизация времени у оппонента
          s := ClockToStr;
          if (m_lwOpponentClientVersion >= 200705) then
          begin
            if prevClock <> s then
              RSendData(CMD_SET_CLOCK + ' ' + s);
            RSendData(CMD_ALLOW_TAKEBACKS + IfThen(TakeBackCheckBox.Checked, ' 1', ' 0'));
          end;
          you_takebacks := TakeBackCheckBox.Checked;
          if (m_lwOpponentClientVersion >= 200706) then
          begin
            if can_pause_game <> GamePauseCheckBox.Checked then
            begin
              can_pause_game := GamePauseCheckBox.Checked;
              RSendData(CMD_CAN_PAUSE_GAME + IfThen(can_pause_game, ' 1', ' 0'))
            end;
          end;
          if (m_lwOpponentClientVersion >= 200801) then
          begin
            if can_adjourn_game <> GameAdjournCheckBox.Checked then
            begin
              can_adjourn_game := GameAdjournCheckBox.Checked;
              RSendData(CMD_CAN_ADJOURN_GAME + IfThen(can_adjourn_game, ' 1', ' 0'))
            end;
          end;
          // Training mode
          if (m_lwOpponentClientVersion >= 200705) and (ChessBoard.pTrainingMode <> TrainingEnabledCheckBox.Checked) then
          begin
            RSendData(CMD_SET_TRAINING + IfThen(TrainingEnabledCheckBox.Checked, ' 1', ' 0'));
          end;
          ChessBoard.pTrainingMode := TrainingEnabledCheckBox.Checked;
          m_strExtBaseName := m_ExtBaseList[ExtBaseComboBox.ItemIndex];
          if (m_strExtBaseName <> '') then
            ChessBoard.SetExternalBase(Chess4NetPath + m_strExtBaseName)
          else
            ChessBoard.UnsetExternalBase;
          ChessBoard.pUseUserBase := UsrBaseCheckBox.Checked;
          GamePause.Visible := can_pause_game;
          TakebackGame.Visible := (ChessBoard.pTrainingMode or opponent_takebacks);

          if (m_lwOpponentClientVersion < 200705) then // 2007.4
          begin
            if ChessBoard.pTrainingMode then
              s := s + ' 1 1'
            else
              s := s + IfThen(you_takebacks, ' 1 0', ' 0 0');
            RSendData(CMD_GAME_OPTIONS + ' ' + s);
          end;
        end;
    end;

    mfLookFeel:
    begin
      with (modSender as TLookFeelOptionsForm), ChessBoard do
      begin
        animation := TAnimation(AnimationComboBox.ItemIndex);
        LastMoveHilighted := HilightLastMoveBox.Checked;
        FlashOnMove := FlashIncomingMoveBox.Checked;
        CoordinatesShown := CoordinatesBox.Checked;
        StayOnTop := StayOnTopBox.Checked;
        extra_exit := ExtraExitBox.Checked;
      end;
    end;

    mfContinue:
    begin
      m_ContinueForm := nil;
      if modRes = mrOk then
      begin
        RSendData(CMD_CONTINUE_GAME);
        ContinueGame;
      end;
    end;

    mfCanPause:
    begin
      if modRes = mrYes then
      begin
        RSendData(CMD_PAUSE_GAME_YES);
        PauseGame;
      end
      else // modRes = mrNo
        RSendData(CMD_PAUSE_GAME_NO);
    end;
{$IFDEF SKYPE}
    mfSelectSkypeContact:
    begin
      if (modRes = mrOk) then
      begin
        with modSender as TSelectSkypeContactForm do
        begin
          Connector.ConnectToContact(SelectedContactIndex);
        end;
      end
      else
      begin
        if (Assigned(ConnectingForm)) then
          ConnectingForm.Close
        else
          Close;
      end;
    end;
{$ENDIF}
  end;
end;

{$IFDEF GAME_LOG}
procedure TManager.InitGameLog;
var
  s: string;
begin
  gameLog := '';

  LongTimeFormat:= HOUR_TIME_FORMAT;
  WriteToGameLog('[' + DateTimeToStr(Now) + ']' + #13#10);

  if ChessBoard.PlayerColor = fcWhite then
    WriteToGameLog(PlayerNick + ' - ' + OpponentNick)
  else
    WriteToGameLog(OpponentNick + ' - ' + PlayerNick);

  if not (you_unlimited and opponent_unlimited) then
  begin
    WriteToGameLog(' (');
    case ChessBoard.PlayerColor of
      fcWhite:
        begin
          if not you_unlimited then
            begin
              WriteToGameLog(IntToStr(you_time));
              if you_inc > 0 then
                WriteToGameLog('.' + IntToStr(you_inc));
            end
          else
            WriteToGameLog('inf');

          WriteToGameLog(':');

          if not opponent_unlimited then
            begin
              WriteToGameLog(IntToStr(opponent_time));
              if opponent_inc > 0 then
                WriteToGameLog('.' + IntToStr(opponent_inc));
            end
          else
            WriteToGameLog('inf');
        end;
      fcBlack:
        begin
          if not opponent_unlimited then
            begin
              WriteToGameLog(IntToStr(opponent_time));
              if opponent_inc > 0 then
                WriteToGameLog('.' + IntToStr(opponent_inc));
            end
          else
            WriteToGameLog('inf');

          WriteToGameLog(':');

          if not you_unlimited then
            begin
              WriteToGameLog(IntToStr(you_time));
              if you_inc > 0 then
                WriteToGameLog('.' + IntToStr(you_inc));
            end
          else
            WriteToGameLog('inf');
        end;
    end;
    WriteToGameLog(')');
  end;
  WriteToGameLog(#13#10);

  s := ChessBoard.GetPosition;
  if s <> INITIAL_CHESS_POSITION then
    WriteToGameLog(s + #13#10);
end;


procedure TManager.WriteToGameLog(const s: string);
begin
  gameLog := gameLog + s;
end;


procedure TManager.FlushGameLog;
var
  gameLogFile: TextFile;
begin
  if not move_done then
    exit;

  AssignFile(gameLogFile, Chess4NetPath + 'Chess4Net_GAMELOG.txt');
{$I-}
  Append(gameLogFile);
{$I+}
  if IOResult <> 0 then
    begin
      Rewrite(gameLogFile);
      if IOResult = 0 then
        writeln(gameLogFile, gameLog);
    end
  else
    writeln(gameLogFile, #13#10 + gameLog);

  CloseFile(gameLogFile);
end;
{$ENDIF}

procedure TManager.FPopulateExtBaseList;
var
  sr: TSearchRec;
  extBaseName: string;
begin
  m_ExtBaseList.Append('');
  if (FindFirst(Chess4NetPath + '*.pos', faAnyFile, sr) = 0) then
  begin
    repeat
      extBaseName := LeftStr(sr.Name, length(sr.Name) - length(ExtractFileExt(sr.Name)));
      if (extBaseName <> USR_BASE_NAME) and FileExists(Chess4NetPath + extBaseName + '.mov') then
        m_ExtBaseList.Append(extBaseName);
    until FindNext(sr) <> 0;
  end; // if
  FindClose(sr);
end;


procedure TManager.RCreateAndPopulateExtBaseList;
begin
  m_ExtBaseList := TStringList.Create;
  FPopulateExtBaseList;
  m_strExtBaseName := '';
end;


procedure TManager.RSetPrivateSettings;
var
  iniFile: TTntIniFile;
  initialClockTime: string;
begin
  // Общие настройки по умолчанию
  initialClockTime := INITIAL_CLOCK_TIME;
  SetClock(initialClockTime);
  ChessBoard.AutoFlag := TRUE;
  you_takebacks := FALSE;
  opponent_takebacks := FALSE;

  // Считывание личных настроек из INI-файла
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    ChessBoard.animation := TAnimation(iniFile.ReadInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(aQuick)));
    ChessBoard.LastMoveHilighted := iniFile.ReadBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, FALSE);
    ChessBoard.FlashOnMove := iniFile.ReadBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME, FALSE);
    ChessBoard.CoordinatesShown := iniFile.ReadBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, TRUE);
    ChessBoard.StayOnTop := iniFile.ReadBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, FALSE);
    extra_exit := iniFile.ReadBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, FALSE);
    TLocalizer.Instance.ActiveLanguage := iniFile.ReadInteger(PRIVATE_SECTION_NAME, LANGUAGE_KEY_NAME, 1) - 1;
{$IFDEF SKYPE}
    bDontShowCredits := iniFile.ReadBool(PRIVATE_SECTION_NAME, DONT_SHOW_CREDITS, FALSE);
{$ENDIF}

  finally
    iniFile.Free;
  end;
end;


function TManager.SetCommonSettings(setToOpponent: boolean): boolean;
var
  iniFile: TTntIniFile;
  commonSectionName: string;
  playerColor: TFigureColor;
  clockStr: string;
  flag: boolean;
begin
  if (m_lwOpponentClientVersion < 200705) then // For 2007.4 common settings are not applied
  begin
    Result := TRUE;
    exit;
  end;

  Result := FALSE;
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + OpponentId;
    if (not iniFile.SectionExists(commonSectionName)) then
      exit;

    if (setToOpponent) then
    begin
      playerColor := TFigureColor(iniFile.ReadInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(fcBlack)));
      if (ChessBoard.PlayerColor = playerColor) then // каждый раз менять сохранённый цвет на противоположный
      begin
        ChangeColor;
        RSendData(CMD_CHANGE_COLOR);
      end;
      clockStr := iniFile.ReadString(commonSectionName, CLOCK_KEY_NAME, INITIAL_CLOCK_TIME);
      if (clockStr <> ClockToStr) then
      begin
        SetClock(clockStr);
        RSendData(CMD_SET_CLOCK + ' ' + ClockToStr);
      end;

      flag := iniFile.ReadBool(commonSectionName, TRAINING_MODE_KEY_NAME, FALSE);
      if (ChessBoard.pTrainingMode <> flag) then
      begin
        ChessBoard.pTrainingMode := flag;
        RSendData(CMD_SET_TRAINING + IfThen(ChessBoard.pTrainingMode, ' 1', ' 0'));
      end;

      if (m_lwOpponentClientVersion >= 200706) then
      begin
        flag := iniFile.ReadBool(commonSectionName, CAN_PAUSE_GAME_KEY_NAME, FALSE);
        if (can_pause_game <> flag) then
        begin
          can_pause_game := flag;
          RSendData(CMD_CAN_PAUSE_GAME + IfThen(can_pause_game, ' 1', ' 0'));
        end;
      end; { if opponentClientVersion >= 200706}

      if (m_lwOpponentClientVersion >= 200801) then
      begin
        flag := iniFile.ReadBool(commonSectionName, CAN_ADJOURN_GAME_KEY_NAME, FALSE);
        if (can_adjourn_game <> flag) then
        begin
          can_adjourn_game := flag;
          RSendData(CMD_CAN_ADJOURN_GAME + IfThen(can_adjourn_game, ' 1', ' 0'));
        end;
      end; { opponentClientVersion >= 200801 }
    end; { if setToOpponent }

    m_strExtBaseName := iniFile.ReadString(commonSectionName, EXTERNAL_BASE_NAME_KEY_NAME, '');
    if (m_strExtBaseName <> '') then
      ChessBoard.SetExternalBase(Chess4NetPath + m_strExtBaseName)
    else
      ChessBoard.UnsetExternalBase;

    ChessBoard.pUseUserBase := iniFile.ReadBool(commonSectionName, USE_USER_BASE_KEY_NAME, FALSE);
    flag := iniFile.ReadBool(commonSectionName, ALLOW_TAKEBACKS_KEY_NAME, FALSE);
    if you_takebacks <> flag then
      begin
        you_takebacks := flag;
        RSendData(CMD_ALLOW_TAKEBACKS + IfThen(you_takebacks, ' 1', ' 0'));
      end;
    ChessBoard.AutoFlag := iniFile.ReadBool(commonSectionName, AUTO_FLAG_KEY_NAME, FALSE);

    TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
    GamePause.Visible := can_pause_game;

    if (m_lwOpponentClientVersion >= 200801) then
    begin
      AdjournedStr := iniFile.ReadString(commonSectionName, ADJOURNED_KEY_NAME, '');
      if (AdjournedStr <> '') then
      begin
        RSendData(CMD_SET_ADJOURNED + ' ' + AdjournedStr);
        iniFile.WriteString(commonSectionName, ADJOURNED_KEY_NAME, '');
      end;
    end;
    
  finally
    iniFile.Free;
  end;

  Result := TRUE;  
end;


procedure TManager.WriteSettings;
var
  iniFile: TTntIniFile;
  commonSectionName: string;
begin
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    // Запись личных настроек
    iniFile.WriteInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(ChessBoard.animation));
    iniFile.WriteBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, ChessBoard.LastMoveHilighted);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME, ChessBoard.FlashOnMove);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, ChessBoard.CoordinatesShown);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, ChessBoard.StayOnTop);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, extra_exit);
    iniFile.WriteInteger(PRIVATE_SECTION_NAME, LANGUAGE_KEY_NAME, TLocalizer.Instance.ActiveLanguage + 1);
{$IFDEF SKYPE}
    if (bDontShowCredits) then
      iniFile.WriteBool(PRIVATE_SECTION_NAME, DONT_SHOW_CREDITS, bDontShowCredits);
{$ENDIF}
    // Запись общих настроек
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + OpponentId;
    iniFile.WriteInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(ChessBoard.PlayerColor));
    iniFile.WriteString(commonSectionName, CLOCK_KEY_NAME, ClockToStr);
    iniFile.WriteBool(commonSectionName, TRAINING_MODE_KEY_NAME, ChessBoard.pTrainingMode);
    iniFile.WriteString(commonSectionName, EXTERNAL_BASE_NAME_KEY_NAME, m_strExtBaseName);
    iniFile.WriteBool(commonSectionName, USE_USER_BASE_KEY_NAME, ChessBoard.pUseUserBase);
    iniFile.WriteBool(commonSectionName, ALLOW_TAKEBACKS_KEY_NAME, you_takebacks);
    iniFile.WriteBool(commonSectionName, CAN_PAUSE_GAME_KEY_NAME, can_pause_game);
    iniFile.WriteBool(commonSectionName, CAN_ADJOURN_GAME_KEY_NAME, can_adjourn_game);
    iniFile.WriteBool(commonSectionName, AUTO_FLAG_KEY_NAME, ChessBoard.AutoFlag);
    iniFile.WriteString(commonSectionName, ADJOURNED_KEY_NAME, AdjournedStr);

  finally
    iniFile.Free;
  end;
end;


function TManager.ClockToStr: string;
var
  s: string;
begin
  if you_unlimited then
    s := 'u'
  else
    s := IntToStr(you_time) + ' ' + IntToStr(you_inc);
  if opponent_unlimited then
    s := s + ' u'
  else
    s := s + ' ' + IntToStr(opponent_time) + ' ' + IntToStr(opponent_inc);

  Result := s;
end;


procedure TManager.ChangeColor;
begin
   with ChessBoard do
     begin
       if PlayerColor = fcWhite then
         begin
           StartStandartGameConnected.Enabled := FALSE;
           StartPPRandomGameConnected.Enabled := FALSE;
           PlayerColor := fcBlack;
           ChessBoard.Caption := OpponentNick + ' - ' + PlayerNick;
           SetClock;
         end
       else
         begin
           StartStandartGameConnected.Enabled := TRUE;
           StartPPRandomGameConnected.Enabled := TRUE;
           PlayerColor := fcWhite;
           ChessBoard.Caption := PlayerNick + ' - ' + OpponentNick;
           SetClock;
         end;
     end
end;


procedure TManager.GamePauseClick(Sender: TObject);
begin
  RSendData(CMD_PAUSE_GAME);
end;


procedure TManager.PauseGame;
begin
  ChessBoard.StopClock;
  m_ContinueForm := (m_Dialogs.CreateDialog(TContinueForm) as TContinueForm);
  m_ContinueForm.Show;
end;


procedure TManager.ContinueGame;
begin
  ChessBoard.SwitchClock(ChessBoard.PositionColor);
end;

procedure TManager.AboutActionExecute(Sender: TObject);
begin
  ShowInfo;
end;

procedure TManager.AdjournGameClick(Sender: TObject);
begin
  RSendData(CMD_ADJOURN_GAME);
end;

procedure TManager.StartAdjournedGameConnectedClick(Sender: TObject);
begin
  if AdjournedStr <> '' then
  begin
    RSendData(CMD_START_ADJOURNED_GAME);
    FStartAdjournedGame;
  end;
end;


procedure TManager.FAdjournGame;
begin
  if (ChessBoard.Mode <> mGame) then
    exit;
  FBuildAdjournedStr;  
  ChessBoard.StopClock;
  ChessBoard.Mode := mView;
{$IFDEF GAME_LOG}
  WriteToGameLog('*');
  FlushGameLog;
{$ENDIF}
  m_Dialogs.MessageDlg(TLocalizer.Instance.GetMessage(27), mtCustom, [mbOK], mfNone); // The game is adjourned.
end;

procedure TManager.FExitGameMode;
begin
  ChessBoard.StopClock;
  ChessBoard.Mode := mView;
  if move_done then
    AdjournedStr := '';
end;


procedure TManager.FBuildAdjournedStr;
var
  str: string;
begin
  // AdjournedStr ::= <position>&<this player's color>&<time control>&<current time>
  with ChessBoard do
    begin
      // <position>
      str := ChessBoard.GetPosition + '&';
      // <this player's color>
      str := str + IfThen((ChessBoard.PlayerColor = fcWhite), 'w', 'b') + '&';
      // <time control>
      str := str + ClockToStr + '&';
      // <current time>
      LongTimeFormat := HOUR_TIME_FORMAT;
      str := str + TimeToStr(Time[fcWhite]) + ' ' + TimeToStr(Time[fcBlack]);
    end;
  AdjournedStr := str;  
end;

procedure TManager.FStartAdjournedGame;
var
  l: integer;
  str: string;
  strPosition, strPlayerColor, strTimeControl, strCurrentTime: string;
begin
  if AdjournedStr = '' then
    exit;
  str := AdjournedStr;

  // AdjournedStr ::= <position>&<this player's color>&<time control>&<current time>

  l := pos('&', str);
  strPosition := LeftStr(str, l - 1);
  str := RightStr(str, length(str) - l);

  l := pos('&', str);
  strPlayerColor := LeftStr(str, l - 1);
  str := RightStr(str, length(str) - l);

  l := pos('&', str);
  strTimeControl := LeftStr(str, l - 1);
  strCurrentTime := RightStr(str, length(str) - l);

  SetClock(strTimeControl);
  if ((ChessBoard.PlayerColor = fcWhite) and (strPlayerColor <> 'w')) or
     ((ChessBoard.PlayerColor = fcBlack) and (strPlayerColor <> 'b')) then
    ChangeColor;

  with ChessBoard do
    begin
      SetPosition(strPosition);

      RSplitStr(strCurrentTime, str, strCurrentTime);
      LongTimeFormat := HOUR_TIME_FORMAT;
      Time[fcWhite] := StrToTime(str);
      Time[fcBlack] := StrToTime(strCurrentTime);

      ResetMoveList;

      move_done:= FALSE;
      TakebackGame.Enabled := FALSE;
      Mode := mGame;
      SwitchClock(PositionColor);
    end;  
end;


procedure TManager.GamePopupMenuPopup(Sender: TObject);
begin
  N6.Visible := (AdjournGame.Visible or GamePause.Visible or TakebackGame.Visible);
end;


procedure TManager.RLocalize;
begin
  with TLocalizer.Instance do
  begin
    StartAdjournedGameConnected.Caption := GetLabel(51);
    StartStandartGameConnected.Caption := GetLabel(52);
    StartPPRandomGameConnected.Caption := GetLabel(53);
    ChangeColorConnected.Caption := GetLabel(54);
    GameOptionsConnected.Caption := GetLabel(55);
    LookFeelOptionsAction.Caption := GetLabel(56);
    AboutAction.Caption := GetLabel(57);

    AbortGame.Caption := GetLabel(58);
    DrawGame.Caption := GetLabel(59);
    ResignGame.Caption := GetLabel(60);
    AdjournGame.Caption := GetLabel(61);
    GamePause.Caption := GetLabel(62);
    TakebackGame.Caption := GetLabel(63);
  end;
end;


function TManager.FGetOpponentNickId: string;
begin
  Result := OpponentNick + OpponentId;
end;


procedure TManager.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  AdjournGame.Visible := can_adjourn_game;
  AdjournGame.Enabled := ((adjournedStr <> '') and move_done);
  StartAdjournedGameConnected.Visible := (adjournedStr <> '');
end;

{$IFDEF SKYPE}
procedure TManager.FShowCredits;

  function NFridayThe13: boolean; // just for fun!
  begin
    Result := ((DayOfTheMonth(Today) = 13) and (DayOfWeek(Today) = 6));
  end;

begin // TManager.FShowCredits
  if (connectionOccured and (not bDontShowCredits) and (not NFridayThe13)) then
  begin
    with TCreditsForm.Create(nil) do
    try
      ShowModal;
      bDontShowCredits := DontShowAgain;
    finally
      Free;
    end;
  end;
end;
{$ENDIF}


{$IFDEF AND_RQ}
constructor TManagerDefault.Create;
begin
  RCreate;
end;
{$ENDIF}

{$IFDEF QIP}
constructor TManagerDefault.Create(const accName: WideString; const protoDllHandle: integer);
begin
  iProtoDllHandle := protoDllHandle;
  wAccName := accName;

  RCreate;
end;
{$ENDIF}

{$IFDEF TRILLIAN}
constructor TManagerDefault.Create(const vContactlistEntry: TTtkContactListEntry);
begin
  contactListEntry := vContactlistEntry;
  RCreate;
end;
{$ENDIF}

{$IFDEF SKYPE}
constructor TManagerDefault.Create;
begin
  RCreate;
end;
{$ENDIF}

procedure TManagerDefault.ROnCreate;
begin
  try
    RCreateChessBoardAndDialogs;

    TLocalizer.Instance.AddSubscriber(self);
    RLocalize;

    RSetChessBoardToView;
    RSetPrivateSettings;
        
{$IFDEF AND_RQ}
    Connector := TConnector.Create(RQ_GetChatUIN, ConnectorHandler);
{$ENDIF}
{$IFDEF QIP}
//    QIPConnectionError := FALSE;
    Connector := TConnector.Create(wAccName, iProtoDllHandle, ConnectorHandler);
{$ENDIF}
{$IFDEF TRILLIAN}
    Connector := TConnector.Create(@contactlistEntry, ConnectorHandler);
{$ENDIF}
{$IFDEF SKYPE}
//    SkypeConnectionError := FALSE;
    Connector := TConnector.Create(ConnectorHandler);
{$ENDIF}

    RCreateAndPopulateExtBaseList;

    // инициализация ников
{$IFDEF AND_RQ}
    PlayerNick := RQ_GetDisplayedName(RQ_GetCurrentUser);
    OpponentNick := RQ_GetDisplayedName(RQ_GetChatUIN);
    OpponentId := IntToStr(RQ_GetChatUIN);
{$ENDIF}
{$IFDEF QIP}
    PlayerNick := GetOwnerNick(wAccName, iProtoDllHandle);
    OpponentNick := GetContactNick(wAccName, iProtoDllHandle);
    OpponentId := wAccName;
{$ENDIF}
//    OpponentNickId := OpponentNick + OpponentId;
//    connectionOccured := FALSE;
{$IFDEF TRILLIAN}
    PlayerNick := trillianOwnerNick;
    OpponentNick := contactlistEntry.name;
    OpponentId := contactlistEntry.real_name;
{$ENDIF}
{$IFDEF QIP}
    if (not QIPConnectionError) then
      begin
{$ENDIF}
{$IFDEF SKYPE}
    if (not SkypeConnectionError) then
      begin
{$ENDIF}
        RShowConnectingForm;
{$IFDEF QIP}
      end;
{$ENDIF}
{$IFDEF SKYPE}
      end;
{$ENDIF}

  except
    Release;
    raise;
  end;
end;


procedure TManagerDefault.ROnDestroy;
begin
  if (Assigned(Connector)) then
  begin
    Connector.Close;
  end;

  inherited ROnDestroy;
end;


procedure TManagerDefault.RSendData(const cmd: string);
const
  last_cmd: string = '';
begin
  if (cmd = '') then
    exit;
  last_cmd := cmd + CMD_DELIMITER;
  Connector.SendData(last_cmd);
end;

end.

