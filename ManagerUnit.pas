unit ManagerUnit;

{$DEFINE GAME_LOG}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, ActnList, ExtCtrls,
{$IFDEF MIRANDA}
  ControlUnit,
{$ENDIF}
{$IFDEF TRILLIAN}
  plugin,
{$ENDIF}
  // Chess4Net Units
  ChessBoardHeaderUnit, ChessBoardUnit, PosBaseChessBoardUnit,
  ConnectorUnit, ConnectingUnit, GameOptionsUnit,
  ModalForm, DialogUnit, ContinueUnit;

type
{$IFDEF MIRANDA}
  TManager = class(TForm, IMirandaPlugin)
{$ELSE} // TRILLIAN, AND_RQ, QIP
  TManager = class(TForm)
{$ENDIF}
    ActionList: TActionList;
    OptionsAction: TAction;
    ConnectedPopupMenu: TPopupMenu;
    LookFeelOptionsConnected: TMenuItem;
    StartStandartGameConnected: TMenuItem;
    StartPPRandomGameConnected: TMenuItem;
    N3: TMenuItem;
    GameOptionsConnected: TMenuItem;
    ChangeColorConnected: TMenuItem;
    GamePopupMenu: TPopupMenu;
    AbortGame: TMenuItem;
    DrawGame: TMenuItem;
    ResignGame: TMenuItem;
    N4: TMenuItem;
    LookFeelOptionsGame: TMenuItem;
    ConnectorTimer: TTimer;
    TakebackGame: TMenuItem;
    GamePause: TMenuItem;
    N1: TMenuItem;
    AboutConnected: TMenuItem;
    N2: TMenuItem;
    AboutGame: TMenuItem;
    AboutAction: TAction;
    StartAdjournedGameConnected: TMenuItem;
    AdjournGame: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
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
    ChessBoard: TPosBaseChessBoard;
    Connector: TConnector;
    ConnectingForm: TConnectingForm;
    ContinueForm: TContinueForm;
    ExtBaseList: TStringList;
    ExtBaseName: string;
{$IFDEF QIP}
    iProtoDllHandle: integer;
    wAccName: WideString;
    QIPConnectionError: boolean;
{$ENDIF}
{$IFDEF TRILLIAN}
    contactlistEntry: TTtkContactListEntry;
{$ENDIF}         
    opponentClientVersion : LongWord;
    // дл€ ChessBoard
    you_unlimited, opponent_unlimited: boolean;
    you_time, opponent_time,
    you_inc, opponent_inc: word;
    you_takebacks, opponent_takebacks: boolean;
    can_pause_game, can_adjourn_game, move_done: boolean;
    player_nick, opponent_nick: string;
    opponent_id: string;
    player_nick_id, opponent_nick_id: string;
    // Extra Exit
    extra_exit: boolean;
    // откладывание
    _adjournedStr: string;
    connectionOccured: boolean;
    dialogs: TDialogs;
{$IFDEF GAME_LOG}
    // дл€ лога игры
    gameLog: string;
    procedure InitGameLog;
    procedure WriteToGameLog(const s: string);
    procedure FlushGameLog;
{$ENDIF}
    procedure ChessBoardHandler(e: TChessBoardEvent;
                                d1: pointer = nil; d2: pointer = nil);
    procedure ConnectorHandler(e: TConnectorEvent;
                                d1: pointer = nil; d2: pointer = nil);
    procedure SendData(const cmd: string = '');
    procedure SetClock; overload;
    procedure SetClock(var sr: string); overload;
    procedure CB2View;
    procedure DialogFormHandler(modSender: TModalForm; msgDlgID: TModalFormID);
    procedure CloseConnector;
    procedure PopulateExtBaseList;
    procedure SetPrivateSettings;
    function SetCommonSettings(setToOpponent: boolean): boolean;
    procedure WriteSettings;
    function ClockToStr: string;
    procedure ChangeColor;
    procedure PauseGame;
    procedure ContinueGame;
    procedure FAdjournGame;
    procedure FExitGameMode;
    procedure FSetAdjournedStr(const str: string);
    procedure FBuildAdjournedStr;
    procedure FStartAdjournedGame;

    property AdjournedStr: string read _adjournedStr write FSetAdjournedStr;

  public
{$IFDEF MIRANDA}
    constructor Create(Connector: TConnector); reintroduce;
    procedure Start;
{$ENDIF}
{$IFDEF AND_RQ}
    constructor Create; reintroduce;
{$ENDIF}
{$IFDEF QIP}
    constructor Create(const accName: WideString; const protoDllHandle: integer); reintroduce;
{$ENDIF}
{$IFDEF TRILLIAN}
    constructor Create(const vContactlistEntry: TTtkContactListEntry); reintroduce;
{$ENDIF}        
  end;

implementation

{$R *.dfm}
{$J+}

uses
  Types,
  // Chess4Net
  DateUtils, Math, StrUtils, IniFiles, Dialogs,
  LookFeelOptionsUnit, GlobalsLocalUnit, InfoUnit
{$IFDEF MIRANDA}
  , m_globaldefs, m_api
{$ENDIF}
{$IFDEF AND_RQ}
  , CallExec
{$ENDIF}
{$IFDEF QIP}
  , ControlUnit
{$ENDIF}
  ;

const
  USR_BASE_NAME = 'Chess4Net';
  INI_FILE_NAME = 'Chess4net.ini';

  INITIAL_CLOCK_TIME = '5 0 5 0'; // 5:00 5:00
  NO_CLOCK_TIME ='u u';

  FULL_TIME_FORMAT = 'h:n:s"."z';
  HOUR_TIME_FORMAT = 'h:nn:ss';
  // сокращение команд дл€ Connector
  CMD_ECHO = 'echo';
  CMD_START_GAME = 'strt';
  CMD_GAME_OPTIONS = 'gmopt'; // не существует с 2007.5
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
  CMD_WELCOME = 'wlcm'; // прин€тие соединени€
  CMD_GOODBYE = 'gdb'; // отказ от соединени€
  // существует с 2007.5
  CMD_NO_SETTINGS = 'noset'; // отсутствуют глобальные установки - запросить с клиента партнЄра
  CMD_ALLOW_TAKEBACKS = 'alwtkb';
  CMD_SET_CLOCK = 'clck'; // изменение времени
  CMD_SET_TRAINING = 'trnng'; // установка тренировочного режима
  // —уществует с 2007.6
  CMD_CAN_PAUSE_GAME = 'canpaus';
  CMD_PAUSE_GAME = 'paus';
  CMD_PAUSE_GAME_YES = 'pausyes';
  CMD_PAUSE_GAME_NO = 'pausno';
  CMD_CONTINUE_GAME = 'cont';
  // —уществует с 2008.1
  CMD_CAN_ADJOURN_GAME = 'canadj';
  CMD_SET_ADJOURNED = 'setadj'; // установка отложенной позиции и времени
  CMD_ADJOURN_GAME = 'adj';
  CMD_ADJOURN_GAME_YES = 'adjyes';
  CMD_ADJOURN_GAME_NO = 'adjno';
  CMD_START_ADJOURNED_GAME = 'strtadj';

// {$IFNDEF MIRANDA}
  CMD_DELIMITER = '&&'; // CMD_DELIMITER не должен присутствовать в аргументах
// {$ENDIF}  
  // CMD_DELIMITER = 'ext' - зарезервировано

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

{$IFDEF MIRANDA}
procedure TManager.FormCreate(Sender: TObject);
begin
  // List of external bases
  ExtBaseList := TStringList.Create;
  PopulateExtBaseList;
  ExtBaseName := '';

  // Nicks initialization
  player_nick := Connector.OwnerNick;
  opponent_nick := Connector.ContactNick;
  opponent_id := IntToStr(Connector.ContactID);
  opponent_nick_id := opponent_nick + opponent_id;
  connectionOccured := FALSE;
end;
{$ENDIF} // {$IFDEF MIRANDA}

{$IFNDEF MIRANDA}
procedure TManager.FormCreate(Sender: TObject);
begin
  try
    ChessBoard := TPosBaseChessBoard.Create(self, ChessBoardHandler, Chess4NetPath + USR_BASE_NAME);
    dialogs := TDialogs.Create(ChessBoard, DialogFormHandler);
    with ChessBoard do
      begin
        CB2View;
        Left:= (Screen.Width - Width) div 2;
        Top:= (Screen.Height - Height) div 2;
        Show;
        SetPrivateSettings;
      end;
{$IFDEF AND_RQ}
    Connector:= TConnector.Create(RQ_GetChatUIN, ConnectorHandler);
{$ENDIF}
{$IFDEF QIP}
    QIPConnectionError := FALSE;
    Connector := TConnector.Create(wAccName, iProtoDllHandle, ConnectorHandler);
{$ENDIF}
{$IFDEF TRILLIAN}
    Connector:= TConnector.Create(@contactlistEntry, ConnectorHandler);
{$ENDIF}
    // список внешних баз
    ExtBaseList := TStringList.Create;
    PopulateExtBaseList;
    ExtBaseName := '';
  // инициализаци€ ников
{$IFDEF AND_RQ}
    player_nick := RQ_GetDisplayedName(RQ_GetCurrentUser);
    opponent_nick := RQ_GetDisplayedName(RQ_GetChatUIN);
    opponent_id := IntToStr(RQ_GetChatUIN);
{$ENDIF}
{$IFDEF QIP}
    player_nick := GetOwnerNick(wAccName, iProtoDllHandle);
    opponent_nick := GetContactNick(wAccName, iProtoDllHandle);
    opponent_id := wAccName;
{$ENDIF}
    opponent_nick_id := opponent_nick + opponent_id;
    connectionOccured := FALSE;
{$IFDEF TRILLIAN}
    player_nick := trillianOwnerNick;
    opponent_nick := contactlistEntry.name;
    opponent_id := contactlistEntry.real_name;
{$ENDIF}
{$IFDEF QIP}
    if not QIPConnectionError then
      begin
{$ENDIF}
        ConnectingForm := (dialogs.CreateDialog(TConnectingForm) as TConnectingForm);
        ConnectingForm.Show;
{$IFDEF QIP}
      end;
{$ENDIF}
  except
    Release;
    raise;
  end;
end;
{$ENDIF} // {$IFNDEF MIRANDA}


procedure TManager.ChessBoardHandler(e: TChessBoardEvent;
                            d1: pointer = nil; d2: pointer = nil);
var
  s: string;
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
      if not dialogs.Showing then
        begin
          case ChessBoard.Mode of
            mView:
              if Connector.connected then
                ConnectedPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
            mGame:
              GamePopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          end;
        end;
    cbeMoved:
      begin
        SendData(PString(d1)^);
{$IFDEF GAME_LOG}
        if (ChessBoard.PositionColor = Black) or (not move_done) then
          begin
            WriteToGameLog(' ' + IntToStr(ChessBoard.NMoveDone) + '.');
            if ChessBoard.PositionColor = White then
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
          if PositionColor = White then WriteToGameLog(#13#10 + '0 - 1')
            else WriteToGameLog(#13#10 + '1 - 0');
          FlushGameLog;
{$ENDIF}
          if PositionColor = White then s := 'White'
            else s := 'Black';
          if ((PlayerColor <> White) and (PositionColor = White)) or
             ((PlayerColor <> Black) and (PositionColor = Black)) then
            begin
              dialogs.MessageDlg(s + ' is checkmated. You win.', mtCustom, [mbOK], mfNone);
              ChessBoard.WriteGameToBase(grWin);
            end
          else
            begin
              dialogs.MessageDlg(s + ' is checkmated. You loose.', mtCustom, [mbOK], mfNone);
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
        dialogs.MessageDlg('It''s stalemate. No one wins.', mtCustom, [mbOK], mfNone);
        ChessBoard.WriteGameToBase(grDraw);
      end;
    cbeClockSwitched:
      with ChessBoard do
        begin
          if move_done and (ClockColor = PositionColor) then
            begin
              if ClockColor <> PlayerColor then
                begin
                  Time[PlayerColor] := IncSecond(Time[PlayerColor], you_inc);
                  LongTimeFormat:= FULL_TIME_FORMAT;
                  s := TimeToStr(Time[PlayerColor]);
                  if not Unlimited[PlayerColor] or (opponentClientVersion < 200706) then
                    SendData(CMD_SWITCH_CLOCK + ' ' + s);
                end
              else
                begin
                  if PlayerColor = White then
                    Time[Black] := IncSecond(Time[Black], opponent_inc)
                  else
                    Time[White] := IncSecond(Time[White], opponent_inc);
                end;
            end;
        end; { with }
    cbeTimeOut:
      begin
        SendData(CMD_FLAG);
      end;
    cbeActivate:
      begin
        dialogs.BringToFront;
      end;
    cbeFormMoving:
      begin
        dialogs.MoveForms(integer(d1), integer(d2));
      end;
  end;
end;


procedure SplitStr(s: string; var strLeft: string; var strRight: string);
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
  SplitStr(sr, sl, sr);
  if sl = 'u' then
    opponent_unlimited := TRUE
  else
    begin
      opponent_unlimited:= FALSE;
      opponent_time:= StrToInt(sl);
      SplitStr(sr, sl, sr);
      opponent_inc := StrToInt(sl);
    end;
    SplitStr(sr, sl, sr);
    if sl = 'u' then
      you_unlimited:= TRUE
    else
      begin
        you_unlimited := FALSE;
        you_time := StrToInt(sl);
        SplitStr(sr, sl, sr);
        you_inc := StrToInt(sl);
      end;
  SetClock;
end;


procedure TManager.ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
var
  cmd_str, sl, sr, ms: string;
{$IFDEF MIRANDA}
  iData: integer;
{$ENDIF}
label
  l;
begin
  case e of
  ceConnected:
    begin
      if Assigned(ConnectingForm) then
        ConnectingForm.Shut;
      SendData(CMD_VERSION + ' ' + IntToStr(CHESS4NET_VERSION));
    end;
  ceDisconnected:
    begin
      case ChessBoard.Mode of
      mView:
        begin
          if (not Connector.connected) then
            exit;
          dialogs.MessageDlg('Your opponent leaves.', mtCustom, [mbOK], mfMsgLeave);
        end;
      mGame:
        begin
          if (not Connector.connected) then
            exit;
          ChessBoard.StopClock;
{$IFDEF GAME_LOG}
          WriteToGameLog('*');
          FlushGameLog;
{$ENDIF}
          dialogs.MessageDlg('Your opponent leaves. The game is aborted.', mtWarning,
                             [mbOK], mfMsgLeave);
        end;
    end;
  end; { ceDisconnected }
  ceError:
    begin
{$IFDEF MIRANDA}
     Connector.Close;
{$ENDIF}
{$IFDEF GAME_LOG}
      if ChessBoard.Mode = mGame then
        begin
          WriteToGameLog('*');
          FlushGameLog;
        end;
{$ENDIF}
        dialogs.MessageDlg('An error during connection occured.', mtWarning,
                           [mbOk], mfMsgLeave);
    end;
{$IFDEF QIP}
  ceQIPError:
    begin
      QIPConnectionError := TRUE;
      dialogs.MessageDlg('Special message channel is not responding.' + #13#10 +
                         'This can happen by one of the following reasons:' + #13#10 +
                         '  1) Your partner is using an IM other than QIP Infium. OR' + #13#10 +
                         '  2) Your partner is offline. OR' + #13#10 +
                         '  3) Protocol doesn''t support multiple channels. OR' + #13#10 +
                         '  4) Other reasons.' + #13#10 +
                         'Chess4Net won''t start.', mtWarning, [mbOk], mfMsgLeave);
    end;
{$ENDIF}
  ceData:
    begin
{$IFDEF MIRANDA}
      Assert(High(TStringDynArray(d1)) >= 0);
      iData := 0;
{$ELSE} // TRILLIAN, AND_RQ, QIP
      cmd_str:= PString(d1)^;
{$ENDIF}
l:
{$IFDEF MIRANDA}
      sl := TStringDynArray(d1)[iData];
      inc(iData);
      cmd_str := IfThen((iData <= High(TStringDynArray(d1))), '*');
{$ELSE} // TRILLIAN, AND_RQ, QIP
      sl := LeftStr(cmd_str, pos(CMD_DELIMITER, cmd_str) - 1);
      cmd_str:= RightStr(cmd_str, length(cmd_str) - length(sl) - length(CMD_DELIMITER));
{$ENDIF}
      SplitStr(sl, sl, sr);

      case ChessBoard.Mode of
      mView:
        if sl = CMD_VERSION then
          begin
            SplitStr(sr, sl, sr);
            opponentClientVersion := StrToInt(sl);
{$IFDEF MIRANDA}
            if (opponentClientVersion >= 200901) then
              Connector.MultiSession := TRUE;
{$ENDIF}
            SendData(CMD_WELCOME);
            if opponentClientVersion < CHESS4NET_VERSION then
              dialogs.MessageDlg('Your opponent is using an older version of Chess4Net.' + #13#10 +
                                 'Most of functionality will be not available.'  + #13#10 +
                                 'Please, ask him/her to update the client.', mtWarning, [mbOK], mfNone);
              // 2007.4 первый клиент с обратной совместимостью
              // ƒл€ несовместимых версий:
              // else SendData(CMD_GOODBYE);
          end
        else
        if sl = CMD_WELCOME then
          begin
            SendData(CMD_NICK_ID + ' ' + opponent_nick_id);
            ChessBoard.InitPosition;
            SetClock;
            connectionOccured := TRUE;
          end
        else
        if sl = CMD_GOODBYE then // дл€ будущих версий
          begin
            dialogs.MessageDlg('The current version of Chess4Net is incompatible with the one of you partner.' + #13#10+
                               'Please check the versions.' , mtWarning, [mbOK], mfIncompatible);
          end
        else
	      if sl = CMD_START_GAME then
    	    with ChessBoard do
            begin
              // — 2007.6 только белые могут начинать игру
              if (opponentClientVersion >= 200706) and (PlayerColor = White) then
                ChangeColor;
              SetClock;
              ResetMoveList;
              Mode := mGame;
              move_done:= FALSE;
              TakebackGame.Enabled := FALSE;
              AdjournGame.Enabled := FALSE;
              SwitchClock(PositionColor);
{$IFDEF GAME_LOG}
              InitGameLog;
{$ENDIF}
            end
        else
        if sl = CMD_START_ADJOURNED_GAME then
          begin
            if ChessBoard.Mode <> mGame then
              FStartAdjournedGame;
          end
        else
        if sl = CMD_ALLOW_TAKEBACKS then
          begin
            SplitStr(sr, sl, sr);
            opponent_takebacks := (sl = '1');
            TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
          end
        else
        if sl = CMD_CAN_PAUSE_GAME then
          begin
            SplitStr(sr, sl, sr);
            can_pause_game := (sl = '1');
            GamePause.Visible := can_pause_game;
          end
        else
        if sl = CMD_CAN_ADJOURN_GAME then
          begin
            SplitStr(sr, sl, sr);
            can_adjourn_game := (sl = '1');
            AdjournGame.Visible := can_adjourn_game;
          end
        else
        if sl = CMD_SET_CLOCK then
          begin
            SetClock(sr);
          end
        else
        if sl = CMD_SET_TRAINING then
          begin
            SplitStr(sr, sl, sr);
            ChessBoard.pTrainingMode := (sl = '1');
            TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
          end
        else
        if sl = CMD_GAME_OPTIONS then // 2007.4
          begin
            SetClock(sr);
            SplitStr(sr, sl, sr);
            opponent_takebacks := (sl = '1');
            SplitStr(sr, sl, sr);
            ChessBoard.pTrainingMode := (sl = '1');
            TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
          end
        else  
        if sl = CMD_SET_ADJOURNED then // 2008.1
          begin
            if (AdjournedStr = '') or (CompareStr(player_nick_id, opponent_nick_id) > 0) then
              begin
                if pos('&w&', sr) > 0 then
                  sr := StringReplace(sr, '&w&', '&b&', []) // White -> Black
                else
                  sr := StringReplace(sr, '&b&', '&w&', []); // Black -> White
                AdjournedStr := sr;
              end;
          end
        else
        if sl = CMD_CHANGE_COLOR then
          begin
            ChangeColor;
          end
        else
        if sl = CMD_NICK_ID then
          begin
            player_nick_id := sr;
            if CompareStr(player_nick_id, opponent_nick_id) < 0 then
              begin
                StartStandartGameConnected.Enabled := TRUE;
                StartPPRandomGameConnected.Enabled := TRUE;
                ChessBoard.PlayerColor := White;
                ChessBoard.Caption := player_nick + ' - ' + opponent_nick;
                if not SetCommonSettings(TRUE) then
                  SendData(CMD_NO_SETTINGS);
              end
            else
              begin
                StartStandartGameConnected.Enabled := FALSE;
                StartPPRandomGameConnected.Enabled := FALSE;
                ChessBoard.PlayerColor := Black;
                ChessBoard.Caption := opponent_nick + ' - ' + player_nick;
                SetCommonSettings(FALSE);
              end;
          end { if CompareStr }
        else
        if sl = CMD_POSITION then
          begin
            ChessBoard.SetPosition(sr);
          end
        else
        if sl = CMD_NO_SETTINGS then
          begin
            SetCommonSettings(TRUE);
          end;
	    mGame:
        if sl = CMD_DRAW then
          begin
            dialogs.MessageDlg('Draw?', mtConfirmation, [mbYes, mbNo], mfMsgDraw)
          end
        else
        if sl = CMD_ABORT then
          begin
            dialogs.MessageDlg('Can we abort the game?', mtConfirmation, [mbYes, mbNo], mfMsgAbort);
          end
        else
        if sl = CMD_RESIGN then
          begin
            FExitGameMode;
{$IFDEF GAME_LOG}
            if ChessBoard.PlayerColor = White then
              WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0')
            else
              WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1');
            FlushGameLog;
{$ENDIF}
            dialogs.MessageDlg('I resign. You win this game. Congratulations!',
                               mtCustom, [mbOK], mfNone);
            ChessBoard.WriteGameToBase(grWin);
          end
        else
        if sl = CMD_ABORT_ACCEPTED then
          begin
            FExitGameMode;
{$IFDEF GAME_LOG}
            WriteToGameLog('*');
            FlushGameLog;
{$ENDIF}
            dialogs.MessageDlg('The game is aborted.', mtCustom, [mbOK], mfNone);
          end
        else
        if sl = CMD_ABORT_DECLINED then
          dialogs.MessageDlg('Sorry, but we have to finish this game.',
                             mtCustom, [mbOK], mfNone)
        else
        if sl = CMD_DRAW_ACCEPTED then
          begin
            FExitGameMode;
{$IFDEF GAME_LOG}
            WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
            FlushGameLog;
{$ENDIF}
            dialogs.MessageDlg('The game is drawn.', mtCustom, [mbOK], mfNone);
            ChessBoard.WriteGameToBase(grDraw);
          end
        else
        if sl = CMD_DRAW_DECLINED then
          dialogs.MessageDlg('No draw, sorry.', mtCustom, [mbOK], mfNone)
        else
        if sl = CMD_SWITCH_CLOCK then
          with ChessBoard do
            begin
              SplitStr(sr, sl, sr);
              ms:= RightStr(sl, length(sl) - LastDelimiter(':.', sl));
              sl:= LeftStr(sl, length(sl) - length(ms) - 1);
              if PlayerColor = White then
                Time[Black] := StrToTime(sl) + EncodeTime(0,0,0, StrToInt(ms))
              else
                Time[White] := StrToTime(sl) + EncodeTime(0,0,0, StrToInt(ms));
            end { with }
        else
        if sl = CMD_FLAG then
          with ChessBoard do
            begin
              if (Time[PlayerColor] = 0.0) then
                begin
                  SendData(CMD_FLAG_YES);
                  FExitGameMode;
{$IFDEF GAME_LOG}
                  if ChessBoard.PlayerColor = White then
                    WriteToGameLog(#13#10 + 'White forfeits on time')
                  else
                    WriteToGameLog(#13#10 + 'Black forfeits on time');
                  FlushGameLog;
{$ENDIF}
                  dialogs.MessageDlg('You forfeit on time.', mtCustom, [mbOK], mfNone);
                  ChessBoard.WriteGameToBase(grLostTime);
                end
              else
                SendData(CMD_FLAG_NO);
            end { with }
        else
        if sl = CMD_FLAG_YES then
          begin
            FExitGameMode;
{$IFDEF GAME_LOG}
            if ChessBoard.PlayerColor = White then
              WriteToGameLog(#13#10 + 'Black forfeits on time')
            else
              WriteToGameLog(#13#10 + 'White forfeits on time');
            FlushGameLog;
{$ENDIF}
            dialogs.MessageDlg('Your opponent forfeits on time.', mtCustom, [mbOK], mfNone);
            ChessBoard.WriteGameToBase(grWinTime);
          end
        else
        if sl = CMD_FLAG_NO then
          with ChessBoard do
            case PlayerColor of
            White:
              if Time[Black] = 0.0 then SendData(CMD_FLAG);
            Black:
              if Time[White] = 0.0 then SendData(CMD_FLAG);
            end { case }
        else
        if sl = CMD_PAUSE_GAME then
          begin
            dialogs.MessageDlg('Can we pause the game?', mtConfirmation, [mbYes, mbNo], mfCanPause);
          end
        else
        if sl = CMD_PAUSE_GAME_YES then
          begin
            PauseGame;
          end
        else
        if sl = CMD_PAUSE_GAME_NO then
          begin
            dialogs.MessageDlg('No pause, sorry.', mtCustom, [mbOk], mfNone);
          end
        else
        if sl = CMD_CONTINUE_GAME then
          begin
            if Assigned(ContinueForm) then
              ContinueForm.Shut;
            ContinueGame;
          end
        else
        if sl = CMD_TAKEBACK then
          begin
            if you_takebacks or ChessBoard.pTrainingMode then
              begin
                dialogs.MessageDlg('May I take back last move?',
                                   mtConfirmation, [mbYes, mbNo], mfMsgTakeBack);
              end
            else
              SendData(CMD_TAKEBACK_NO)
          end
        else
        if sl = CMD_ADJOURN_GAME then
          begin
            dialogs.MessageDlg('Can we adjourn this game?',
                                   mtConfirmation, [mbYes, mbNo], mfMsgAdjourn);
          end
        else
        if sl = CMD_ADJOURN_GAME_YES then
          begin
            FAdjournGame;
          end
        else
        if sl = CMD_ADJOURN_GAME_NO then
          begin
            dialogs.MessageDlg('No adjourns, sorry.', mtCustom, [mbOk], mfNone);
          end
        else
        if sl = CMD_TAKEBACK_YES then
          begin
            ChessBoard.TakeBack;
            FBuildAdjournedStr;
            TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
            WriteToGameLog(' <takeback>');
{$ENDIF}
            ChessBoard.SwitchClock(ChessBoard.PositionColor);
          end
        else
        if sl = CMD_TAKEBACK_NO then
          begin
            dialogs.MessageDlg('Sorry, no takebacks!', mtCustom, [mbOK], mfNone);
          end
        else
        if (sl = CMD_POSITION) and (CompareStr(player_nick_id, opponent_nick_id) > 0) then
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
                  if (PositionColor = Black) or not move_done then
                    begin
                      WriteToGameLog(' ' + IntToStr(NMoveDone) + '.');
                      if PositionColor = White then WriteToGameLog(' ...');
                    end;
                  WriteToGameLog(' ' + sl);
{$ENDIF}
                  move_done := TRUE;
                  TakebackGame.Enabled := TRUE;
                  FBuildAdjournedStr; // AdjournedStr помечаетс€ только при вход€щем ходе противника
                end;
            end; {  with ChessBoard}
      end; { case ChessBoard.Mode }

      if (cmd_str <> '') then
        goto l;
    end; {  ceData }
  end; { case ChessBoard.Mode }
end;


procedure TManager.FormDestroy(Sender: TObject);
begin
  if connectionOccured then
    WriteSettings;
  ExtBaseList.Free;
{$IFNDEF MIRANDA}
  if (Connector.Connected) then
    Connector.Close;
{$ELSE} // TRILLIAN, AND_RQ, QIP
  if Assigned(Connector) then
    Connector.Free;
{$ENDIF MIRANDA}
  ChessBoard.Release;
  ChessBoard := nil;
  dialogs.Free;
end;

procedure TManager.OptionsActionExecute(Sender: TObject);
var
  lookFeelOptionsForm: TLookFeelOptionsForm;
begin
  lookFeelOptionsForm := (dialogs.CreateDialog(TLookFeelOptionsForm) as TLookFeelOptionsForm);
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
  SendData(CMD_ABORT);
end;

procedure TManager.DrawGameClick(Sender: TObject);
begin
  SendData(CMD_DRAW);
end;

procedure TManager.ResignGameClick(Sender: TObject);
begin
  dialogs.MessageDlg('Do you really want to resign?',
                mtConfirmation, [mbYes, mbNo], mfMsgResign);
end;


procedure TManager.ChangeColorConnectedClick(Sender: TObject);
begin
  if ChessBoard.Mode = mGame then
    exit;
  ChangeColor;
  SendData(CMD_CHANGE_COLOR);
end;


procedure TManager.GameOptionsConnectedClick(Sender: TObject);
var
  GameOptionsForm: TGameOptionsForm;
  i: integer;
begin
  GameOptionsForm := (dialogs.CreateDialog(TGameOptionsForm) as TGameOptionsForm);
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
      for i := 1 to ExtBaseList.Count - 1 do
        begin
          ExtBaseComboBox.Items.Append(ExtBaseList[i]);
          if ExtBaseName = ExtBaseList[i] then
            ExtBaseComboBox.ItemIndex := i;
        end;
      UsrBaseCheckBox.Checked := ChessBoard.pUseUserBase;
      GamePauseCheckBox.Checked := (can_pause_game and (opponentClientVersion >= 200706));
      GameAdjournCheckBox.Checked := (can_adjourn_game and (opponentClientVersion >= 200801));
      Show;
    end;
end;


procedure TManager.StartStandartGameConnectedClick(Sender: TObject);
begin
  with ChessBoard do
    begin
      SetClock;
      InitPosition;
      ResetMoveList;

      SendData(CMD_POSITION + ' ' + GetPosition);
      SendData(CMD_START_GAME);

      move_done:= FALSE;
      TakebackGame.Enabled := FALSE;
      AdjournGame.Enabled := FALSE;      
      Mode := mGame;
      SwitchClock(ChessBoard.PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;

{$IFDEF MIRANDA}
procedure TManager.SendData(const cmd: string);
begin
  Connector.SendData(cmd);
end;

{$ELSE} // TRILLIAN, AND_RQ, QIP

procedure TManager.SendData(const cmd: string);
const
  last_cmd: string = '';
begin
  if (cmd = '') then
    exit;
  last_cmd := cmd + CMD_DELIMITER;
  Connector.SendData(last_cmd);
end;
{$ENDIF} // {$IFDEF MIRANDA}


procedure TManager.SetClock;
begin
  with ChessBoard do
    begin
      Unlimited[PlayerColor]:= you_unlimited;
      Time[PlayerColor]:= EncodeTime(you_time div 60, you_time mod 60, 0,0);
      if PlayerColor = White then
        begin
          Unlimited[Black]:= opponent_unlimited;
          Time[Black]:= EncodeTime(opponent_time div 60,
                                   opponent_time mod 60, 0,0);
        end
      else
        begin
          Unlimited[White]:= opponent_unlimited;
          Time[White]:= EncodeTime(opponent_time div 60,
                                   opponent_time mod 60, 0,0);
        end;
    end;
end;

procedure TManager.CB2View;
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
   end;
end;


procedure TManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Connector.connected then
    begin
      dialogs.MessageDlg('Do you want to exit?', mtConfirmation, [mbYes, mbNo], mfMsgClose);
      Action:= caNone;
    end
  else
//    Release;    
    Action := caFree;
end;


procedure TManager.CloseConnector;
begin
  ConnectorTimer.Enabled:= TRUE;
end;


procedure TManager.ConnectorTimerTimer(Sender: TObject);
begin
  ConnectorTimer.Enabled:= FALSE;
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

      SendData(CMD_POSITION + ' ' + GetPosition);
      SendData(CMD_START_GAME);

      Mode := mGame;
      move_done := FALSE;
      TakebackGame.Enabled := FALSE;
      AdjournGame.Enabled := FALSE;      
      SwitchClock(ChessBoard.PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;


procedure TManager.TakebackGameClick(Sender: TObject);
begin
  SendData(CMD_TAKEBACK);
end;

{$IFDEF MIRANDA}
constructor TManager.Create(Connector: TConnector);
begin
  self.Connector := Connector;
  inherited Create(Application);
end;
{$ENDIF}

{$IFDEF AND_RQ}
constructor TManager.Create;
begin
  inherited Create(Application);
end;
{$ENDIF}

{$IFDEF QIP}
constructor TManager.Create(const accName: WideString; const protoDllHandle: integer);
begin
  iProtoDllHandle := protoDllHandle;
  wAccName := accName;

  inherited Create(Application);
end;
{$ENDIF}

{$IFDEF TRILLIAN}
constructor TManager.Create(const vContactlistEntry: TTtkContactListEntry);
begin
  contactListEntry := vContactlistEntry;  
  inherited Create(Application);
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
            Release;
          end;
      end;
    mfMsgLeave:
      begin
        CloseConnector;
      end;
    mfIncompatible:
      begin
        CloseConnector;
      end;
    mfMsgAbort:
      if ChessBoard.Mode = mGame then
        begin
          if (modRes = mrNo) or (modRes = mrNone) then
            SendData(CMD_ABORT_DECLINED)
          else
            begin
              SendData(CMD_ABORT_ACCEPTED);
              FExitGameMode;
{$IFDEF GAME_LOG}
              WriteToGameLog('*');
              FlushGameLog;
{$ENDIF}
              dialogs.MessageDlg('The game is aborted.', mtCustom, [mbOK], mfNone);
            end;
      end;
    mfMsgResign:
	    if ChessBoard.Mode = mGame then
        begin
          if modRes = mrYes then
            begin
              FExitGameMode;
              SendData(CMD_RESIGN);
              ChessBoard.WriteGameToBase(grLost);
{$IFDEF GAME_LOG}
              if ChessBoard.PlayerColor = White then
                WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1')
              else
                WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0');
              FlushGameLog;
{$ENDIF}
            end;
        end;
    mfMsgDraw:
      if ChessBoard.Mode = mGame then
        begin
          if (modRes = mrNo) or (modRes = mrNone) then
            SendData(CMD_DRAW_DECLINED)
          else
            begin
              SendData(CMD_DRAW_ACCEPTED);
              FExitGameMode;
{$IFDEF GAME_LOG}
                WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
                FlushGameLog;
{$ENDIF}
              dialogs.MessageDlg('The game is drawn.', mtCustom, [mbOK], mfNone);
              ChessBoard.WriteGameToBase(grDraw);
            end;
        end;
    mfMsgTakeBack:
      if ChessBoard.Mode = mGame then
        begin
          if modRes = mrYes then
            begin
              SendData(CMD_TAKEBACK_YES);
              ChessBoard.TakeBack;
              FBuildAdjournedStr;
              TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
              WriteToGameLog(' <takeback>');
{$ENDIF}
              ChessBoard.SwitchClock(ChessBoard.PositionColor);
            end
          else
            SendData(CMD_TAKEBACK_NO);
        end;
    mfMsgAdjourn:
      if ChessBoard.Mode = mGame then
        begin
          if modRes = mrYes then
            begin
              SendData(CMD_ADJOURN_GAME_YES);
              FAdjournGame;
            end
          else
            SendData(CMD_ADJOURN_GAME_NO);
        end;
    mfConnecting:
      begin
        ConnectingForm := nil;
        if modRes = mrAbort then
          Close; // ConnectionAbort;
      end;
    mfGameOptions:
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
          // ќтображение на доске
          SetClock;
          // синхронизаци€ времени у оппонента
          s := ClockToStr;
          if (opponentClientVersion >= 200705) then
            begin
              if prevClock <> s then
                SendData(CMD_SET_CLOCK + ' ' + s);
              SendData(CMD_ALLOW_TAKEBACKS + IfThen(TakeBackCheckBox.Checked, ' 1', ' 0'));
            end;
          you_takebacks := TakeBackCheckBox.Checked;
          if (opponentClientVersion >= 200706) then
            begin
              if can_pause_game <> GamePauseCheckBox.Checked then
                begin
                  can_pause_game := GamePauseCheckBox.Checked;
                  SendData(CMD_CAN_PAUSE_GAME + IfThen(can_pause_game, ' 1', ' 0'))
                end;
            end;
          if (opponentClientVersion >= 200801) then
            begin
              if can_adjourn_game <> GameAdjournCheckBox.Checked then
                begin
                  can_adjourn_game := GameAdjournCheckBox.Checked;
                  SendData(CMD_CAN_ADJOURN_GAME + IfThen(can_adjourn_game, ' 1', ' 0'))
                end;
            end;
          // тренировочный режим
          if (opponentClientVersion >= 200705) and (ChessBoard.pTrainingMode <> TrainingEnabledCheckBox.Checked) then
            begin
              SendData(CMD_SET_TRAINING + IfThen(TrainingEnabledCheckBox.Checked, ' 1', ' 0'));
            end;
          ChessBoard.pTrainingMode := TrainingEnabledCheckBox.Checked;
          ExtBaseName := ExtBaseList[ExtBaseComboBox.ItemIndex];
          if ExtBaseName <> '' then
            ChessBoard.SetExternalBase(Chess4NetPath + ExtBaseName)
          else
            ChessBoard.UnsetExternalBase;
          ChessBoard.pUseUserBase := UsrBaseCheckBox.Checked;
          GamePause.Visible := can_pause_game;
          AdjournGame.Visible := can_adjourn_game;
          TakebackGame.Visible := (ChessBoard.pTrainingMode or opponent_takebacks);

          if (opponentClientVersion < 200705) then // 2007.4
            begin
              if ChessBoard.pTrainingMode then
                s := s + ' 1 1'
              else
                s := s + IfThen(you_takebacks, ' 1 0', ' 0 0');
              SendData(CMD_GAME_OPTIONS + ' ' + s);
            end;
        end;
    mfLookFeel:
      with (modSender as TLookFeelOptionsForm), ChessBoard do
        begin
          animation := TAnimation(AnimationComboBox.ItemIndex);
          LastMoveHilighted := HilightLastMoveBox.Checked;
          FlashOnMove := FlashIncomingMoveBox.Checked;
          CoordinatesShown := CoordinatesBox.Checked;
          StayOnTop := StayOnTopBox.Checked;
          extra_exit := ExtraExitBox.Checked;
        end;
    mfContinue:
      begin
        ContinueForm := nil;
        if modRes = mrOk then
          begin
            SendData(CMD_CONTINUE_GAME);
            ContinueGame;
          end;
      end;
    mfCanPause:
      begin
        if modRes = mrYes then
          begin
            SendData(CMD_PAUSE_GAME_YES);
            PauseGame;
          end
        else // mdRes = mrNo
          SendData(CMD_PAUSE_GAME_NO);
      end;
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

  if ChessBoard.PlayerColor = White then
    WriteToGameLog(player_nick + ' - ' + opponent_nick)
  else
    WriteToGameLog(opponent_nick + ' - ' + player_nick);

  if not (you_unlimited and opponent_unlimited) then
  begin
    WriteToGameLog(' (');
    case ChessBoard.PlayerColor of
      White:
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
      Black:
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

procedure TManager.PopulateExtBaseList;
var
  sr: TSearchRec;
  extBaseName: string;
begin
  ExtBaseList.Append('');
  if FindFirst(Chess4NetPath + '*.pos', faAnyFile, sr) = 0 then
    repeat
      extBaseName := LeftStr(sr.Name, length(sr.Name) - length(ExtractFileExt(sr.Name)));
      if (extBaseName <> USR_BASE_NAME) and FileExists(Chess4NetPath + extBaseName + '.mov') then
        ExtBaseList.Append(extBaseName);
    until FindNext(sr) <> 0;
  FindClose(sr);
end;


procedure TManager.SetPrivateSettings;
var
  iniFile: TIniFile;
  initialClockTime: string;
begin
  // ќбщие настройки по умолчанию
  initialClockTime := INITIAL_CLOCK_TIME;
  SetClock(initialClockTime);
  ChessBoard.AutoFlag := TRUE;
  you_takebacks := FALSE;
  opponent_takebacks := FALSE;

  // —читывание личных настроек из INI-файла
  iniFile := TIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    ChessBoard.animation := TAnimation(iniFile.ReadInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(aQuick)));
    ChessBoard.LastMoveHilighted := iniFile.ReadBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, FALSE);
    ChessBoard.FlashOnMove := iniFile.ReadBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME, FALSE);
    ChessBoard.CoordinatesShown := iniFile.ReadBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, TRUE);
    ChessBoard.StayOnTop := iniFile.ReadBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, FALSE);
    extra_exit := iniFile.ReadBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, FALSE);
  finally
    iniFile.Free;
  end;
end;


function TManager.SetCommonSettings(setToOpponent: boolean): boolean;
var
  iniFile: TIniFile;
  commonSectionName: string;
  playerColor: TFigureColor;
  clockStr: string;
  flag: boolean;
begin
  if opponentClientVersion < 200705 then // дл€ 2007.4 общие настройки не примен€ютс€
    begin
      Result := TRUE;
      exit;
    end;

  Result := FALSE;
  iniFile := TIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + opponent_id;
    if not iniFile.SectionExists(commonSectionName) then
      exit;

    if setToOpponent then
      begin
        playerColor := TFigureColor(iniFile.ReadInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(Black)));
        if (ChessBoard.PlayerColor = playerColor) then // каждый раз мен€ть сохранЄнный цвет на противоположный
          begin
            ChangeColor;
            SendData(CMD_CHANGE_COLOR);
          end;
        clockStr := iniFile.ReadString(commonSectionName, CLOCK_KEY_NAME, INITIAL_CLOCK_TIME);
        if clockStr <> ClockToStr then
          begin
            SetClock(clockStr);
            SendData(CMD_SET_CLOCK + ' ' + ClockToStr);
          end;

        flag := iniFile.ReadBool(commonSectionName, TRAINING_MODE_KEY_NAME, FALSE);
        if ChessBoard.pTrainingMode <> flag then
          begin
            ChessBoard.pTrainingMode := flag;
            SendData(CMD_SET_TRAINING + IfThen(ChessBoard.pTrainingMode, ' 1', ' 0'));
          end;

        if opponentClientVersion >= 200706 then
          begin
            flag := iniFile.ReadBool(commonSectionName, CAN_PAUSE_GAME_KEY_NAME, FALSE);
            if can_pause_game <> flag then
              begin
                can_pause_game := flag;
                SendData(CMD_CAN_PAUSE_GAME + IfThen(can_pause_game, ' 1', ' 0'));
              end;
          end; { if opponentClientVersion >= 200706}

        if opponentClientVersion >= 200801 then
          begin
            flag := iniFile.ReadBool(commonSectionName, CAN_ADJOURN_GAME_KEY_NAME, FALSE);
            if can_adjourn_game <> flag then
              begin
                can_adjourn_game := flag;
                SendData(CMD_CAN_ADJOURN_GAME + IfThen(can_adjourn_game, ' 1', ' 0'));
              end;
          end; { opponentClientVersion >= 200801 }
      end; { if setToOpponent }

    ExtBaseName := iniFile.ReadString(commonSectionName, EXTERNAL_BASE_NAME_KEY_NAME, '');
    if ExtBaseName <> '' then
      ChessBoard.SetExternalBase(Chess4NetPath + ExtBaseName)
    else
      ChessBoard.UnsetExternalBase;
    ChessBoard.pUseUserBase := iniFile.ReadBool(commonSectionName, USE_USER_BASE_KEY_NAME, FALSE);
    flag := iniFile.ReadBool(commonSectionName, ALLOW_TAKEBACKS_KEY_NAME, FALSE);
    if you_takebacks <> flag then
      begin
        you_takebacks := flag;
        SendData(CMD_ALLOW_TAKEBACKS + IfThen(you_takebacks, ' 1', ' 0'));
      end;
    ChessBoard.AutoFlag := iniFile.ReadBool(commonSectionName, AUTO_FLAG_KEY_NAME, FALSE);

    TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
    GamePause.Visible := can_pause_game;
    AdjournGame.Visible := can_adjourn_game;

    if opponentClientVersion >= 200801 then
      begin
        AdjournedStr := iniFile.ReadString(commonSectionName, ADJOURNED_KEY_NAME, '');
        if AdjournedStr <> '' then
        begin
          SendData(CMD_SET_ADJOURNED + ' ' + AdjournedStr);
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
  iniFile: TIniFile;
  commonSectionName: string;
begin
  iniFile := TIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    // «апись личных настроек
    iniFile.WriteInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(ChessBoard.animation));
    iniFile.WriteBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, ChessBoard.LastMoveHilighted);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME, ChessBoard.FlashOnMove);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, ChessBoard.CoordinatesShown);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, ChessBoard.StayOnTop);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, extra_exit);
    // «апись общих настроек
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + opponent_id;
    iniFile.WriteInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(ChessBoard.PlayerColor));
    iniFile.WriteString(commonSectionName, CLOCK_KEY_NAME, ClockToStr);
    iniFile.WriteBool(commonSectionName, TRAINING_MODE_KEY_NAME, ChessBoard.pTrainingMode);
    iniFile.WriteString(commonSectionName, EXTERNAL_BASE_NAME_KEY_NAME, ExtBaseName);
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
       if PlayerColor = White then
         begin
           StartStandartGameConnected.Enabled := FALSE;
           StartPPRandomGameConnected.Enabled := FALSE;
           PlayerColor := Black;
           ChessBoard.Caption := opponent_nick + ' - ' + player_nick;
           SetClock;
         end
       else
         begin
           StartStandartGameConnected.Enabled := TRUE;
           StartPPRandomGameConnected.Enabled := TRUE;
           PlayerColor := White;
           ChessBoard.Caption := player_nick + ' - ' + opponent_nick;
           SetClock;
         end;
     end
end;


procedure TManager.GamePauseClick(Sender: TObject);
begin
  SendData(CMD_PAUSE_GAME);
end;


procedure TManager.PauseGame;
begin
  ChessBoard.StopClock;
  ContinueForm := (dialogs.CreateDialog(TContinueForm) as TContinueForm);
  ContinueForm.Show;
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
  SendData(CMD_ADJOURN_GAME);
end;

procedure TManager.StartAdjournedGameConnectedClick(Sender: TObject);
begin
  if AdjournedStr <> '' then
    begin
      SendData(CMD_START_ADJOURNED_GAME);
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
  dialogs.MessageDlg('The game is adjourned.', mtCustom, [mbOK], mfNone);
end;

procedure TManager.FExitGameMode;
begin
  ChessBoard.StopClock;
  ChessBoard.Mode := mView;
  if move_done then
    AdjournedStr := '';
end;

procedure TManager.FSetAdjournedStr(const str: string);
begin
  _adjournedStr := str;
  AdjournGame.Enabled := (_adjournedStr <> '') and move_done;
  StartAdjournedGameConnected.Visible := (_adjournedStr <> '');
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
      str := str + IfThen((ChessBoard.PlayerColor = White), 'w', 'b') + '&';
      // <time control>
      str := str + ClockToStr + '&';
      // <current time>
      LongTimeFormat := HOUR_TIME_FORMAT;
      str := str + TimeToStr(Time[White]) + ' ' + TimeToStr(Time[Black]);
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
  if ((ChessBoard.PlayerColor = White) and (strPlayerColor <> 'w')) or
     ((ChessBoard.PlayerColor = Black) and (strPlayerColor <> 'b')) then
    ChangeColor;

  with ChessBoard do
    begin
      SetPosition(strPosition);

      SplitStr(strCurrentTime, str, strCurrentTime);
      LongTimeFormat := HOUR_TIME_FORMAT;
      Time[White] := StrToTime(str);
      Time[Black] := StrToTime(strCurrentTime);

      ResetMoveList;

      move_done:= FALSE;
      TakebackGame.Enabled := FALSE;
      AdjournGame.Enabled := FALSE;
      Mode := mGame;
      SwitchClock(PositionColor);
    end;  
end;


procedure TManager.GamePopupMenuPopup(Sender: TObject);
begin
  N6.Visible := (AdjournGame.Visible or GamePause.Visible or TakebackGame.Visible);
end;

{$IFDEF MIRANDA}
procedure TManager.Start;
begin
  if (Assigned(ChessBoard)) then
  begin
    Show;
    exit;
  end;

  ChessBoard := TPosBaseChessBoard.Create(self, ChessBoardHandler, Chess4NetPath + USR_BASE_NAME);
  dialogs := TDialogs.Create(ChessBoard, DialogFormHandler);
    
  try
    with ChessBoard do
    begin
      CB2View;
      Left:= (Screen.Width - Width) div 2;
      Top:= (Screen.Height - Height) div 2;
      Show;
      SetPrivateSettings;
    end;

    ConnectingForm := (dialogs.CreateDialog(TConnectingForm) as TConnectingForm);
    ConnectingForm.Show;

    if (not Connector.Open(FALSE)) then
      raise Exception.Create('ERROR: Cannot open connector!');
  except
    ChessBoard.Release;
    ChessBoard := nil;
    raise;
  end;
end;
{$ENDIF}

end.

