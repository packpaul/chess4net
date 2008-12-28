unit ManagerUnit;

{$DEFINE GAME_LOG}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  TntForms, Dialogs, TntDialogs, Menus, ActnList, ExtCtrls,
  // Chess4Net Units
  ChessBoardHeaderUnit, ChessBoardUnit, PosBaseChessBoardUnit,
  ConnectorUnit, ConnectingUnit, ContinueUnit;

type
  TManager = class(TForm)
    MainPopupMenu: TPopupMenu;
    ExitMain: TMenuItem;
    LookFeelOptionsMain: TMenuItem;
    N1: TMenuItem;
    ActionList: TActionList;
    ExitAction: TAction;
    ConnectMain: TMenuItem;
    N2: TMenuItem;
    OptionsAction: TAction;
    CopyAction: TAction;
    PasteAction: TAction;
    ConnectedPopupMenu: TPopupMenu;
    DisconnectConnected: TMenuItem;
    LookFeelOptionsConnected: TMenuItem;
    MenuItem4: TMenuItem;
    ExitConnected: TMenuItem;
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
    N5: TMenuItem;
    ExitGame: TMenuItem;
    ConnectorTimer: TTimer;
    GamePause: TMenuItem;
    TakebackGame: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
    procedure ConnectMainClick(Sender: TObject);
    procedure AbortGameClick(Sender: TObject);
    procedure DrawGameClick(Sender: TObject);
    procedure ResignGameClick(Sender: TObject);
    procedure DisconnectConnectedClick(Sender: TObject);
    procedure ChangeColorConnectedClick(Sender: TObject);
    procedure GameOptionsConnectedClick(Sender: TObject);
    procedure StartStandartGameConnectedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ConnectorTimerTimer(Sender: TObject);
    procedure StartPPRandomGameConnectedClick(Sender: TObject);
    procedure TakebackGameClick(Sender: TObject);
    procedure GamePauseClick(Sender: TObject);
  private
    ChessBoard: TPosBaseChessBoard;
    Connector: TConnector;
    ConnectingForm: TConnectingForm;
    ContinueForm: TContinueForm;
    // базы
    ExtBaseList: TStringList;
    ExtBaseName: string;
    // для ChessBoard
    you_unlimited, opponent_unlimited: boolean;
    you_time, opponent_time,
    you_inc, opponent_inc: word;
    you_takebacks, opponent_takebacks: boolean;
    can_pause_game, move_done: boolean;
    player_nick, opponent_nick: string;
    bConnected: boolean;
    extra_exit: boolean;
    opponentClientVersion: LongWord;
    ipDomainPortServer: string;
{$IFDEF GAME_LOG}
    // для лога игры
    gameLog: string;
    procedure InitGameLog;
    procedure WriteToGameLog(const s: string);
    procedure FlushGameLog;
{$ENDIF}
    procedure ChessBoardHandler(e: TChessBoardEvent; d1: pointer = nil; d2: pointer = nil);
    procedure ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
    procedure ContinueHandler;
    procedure ConnectionAbort;
    procedure SendData(cmd: string = '');
    procedure SetClock; overload;
    procedure SetClock(var sr: string); overload;
    procedure CB2View;
    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons): Word;
    procedure CloseConnector;
    procedure PopulateExtBaseList;
    procedure SetPrivateSettings;
    function SetCommonSettings(setToOpponent: boolean): boolean;
    procedure WritePrivateSettings;
    procedure WriteCommonSettings;
    function ClockToStr: string;
    procedure ChangeColor;
    procedure PauseGame;
    procedure ContinueGame;
  end;

implementation

{$R *.dfm}
{$J+}

uses
  DateUtils, Math, StrUtils, TntIniFiles,
  GameOptionsUnit, ConnectionUnit, LookFeelOptionsUnit, GlobalsLocalUnit,
  GlobalsUnit;

const
  USR_BASE_NAME = 'Chess4Net';
  INI_FILE_NAME = 'Chess4net.ini';

  INITIAL_CLOCK_TIME = '5 0 5 0'; // 5:00 5:00
  NO_CLOCK_TIME ='u u';
  DEFAULT_PLAYER_NICK = 'NN';
  DEFAULT_IPDOMAIN_PORT_SERVER = '127.0.0.1:5555-S';

  FULL_TIME_FORMAT = 'h:n:s"."z';
  HOUR_TIME_FORMAT = 'h:nn:ss';
  // сокращение команд для Connector
  CMD_ECHO = 'echo';
  CMD_START_GAME = 'strt';
  CMD_GAME_OPTIONS = 'gmopt'; // не существует с 2007.5
  CMD_CHANGE_COLOR = 'chclr';
  CMD_NICK = 'nk';
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
  CMD_WELCOME = 'wlcm'; // принятие соединения
  CMD_GOODBYE = 'gdb'; // отказ от соединения
  // существует с 2007.5
  CMD_NO_SETTINGS = 'noset'; // отсутствуют глобальные установки - запросить с клиента партнёра
  CMD_ALLOW_TAKEBACKS = 'alwtkb';
  CMD_SET_CLOCK = 'clck'; // изменение времени
  CMD_SET_TRAINING = 'trnng'; // установка тренировочного режима
  // Существует с 2007.6
  CMD_CAN_PAUSE_GAME = 'canpaus';
  CMD_PAUSE_GAME = 'paus';
  CMD_PAUSE_GAME_YES = 'pausyes';
  CMD_PAUSE_GAME_NO = 'pausno';
  CMD_CONTINUE_GAME = 'cont';

  CMD_DELIMITER = #0;

  // INI-file
  PRIVATE_SECTION_NAME = 'Private';
  COMMON_SECTION_PREFIX = 'Common';
  PLAYER_NICK_KEY_NAME = 'PlayerNick';
  IP_DOMAIN_PORT_SERVER_KEY_NAME = 'ipDomainPortServer';
  ANIMATION_KEY_NAME = 'Animation';
  HILIGHT_LAST_MOVE_KEY_NAME = 'HilightLastMove';
  FLASH_ON_MOVE_NAME = 'FlashOnMove';
  SHOW_COORDINATES_KEY_NAME = 'ShowCoordinates';
  STAY_ON_TOP_KEY_NAME = 'StayOnTop';
  EXTRA_EXIT_KEY_NAME = 'ExtraExit';
  CAN_PAUSE_GAME_KEY_NAME = 'CanPauseGame';
  ALLOW_TAKEBACKS_KEY_NAME = 'AllowTakebacks';
  EXTERNAL_BASE_NAME_KEY_NAME = 'ExternalBaseName';
  USE_USER_BASE_KEY_NAME = 'UseUserBase';
  AUTO_FLAG_KEY_NAME = 'AutoFlag';
  TRAINING_MODE_KEY_NAME = 'TrainingMode';
  PLAYER_COLOR_KEY_NAME = 'PlayerColor';
  CLOCK_KEY_NAME = 'Clock';

  MANAGER_UNIT_INSTANCES: integer = 0; // количество инстанций TManager

var
  Chess4NetPath: string;


procedure TManager.FormCreate(Sender: TObject);
begin
  ChessBoard := TPosBaseChessBoard.Create(self, ChessBoardHandler, USR_BASE_NAME);
  Connector := TConnector.Create(self, ConnectorHandler);

  ExtBaseList := TStringList.Create;
  PopulateExtBaseList;

  // инициализация переменных
  bConnected := FALSE;

  with ChessBoard do
    begin
      CB2View;
      Left:= (Screen.Width - Width) div 2;
      Top:= (Screen.Height - Height) div 2;
      Show;

      SetPrivateSettings;

      MainPopupMenu.Popup(Left + Width div 2, Top + Height div 2);
    end;
end;


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
      begin
        case ChessBoard.Mode of
          mView:
            if (Connector.State = []) then
              MainPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y)
            else
              begin
                if bConnected then
                  ConnectedPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
              end;
          mGame:
            GamePopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;
      end;
    cbeMoved:
      begin
        SendData(PString(d1)^);
{$IFDEF GAME_LOG}
        if (ChessBoard.PositionColor = Black) or not move_done then
          begin
            WriteToGameLog(' ' + IntToStr(ChessBoard.NMoveDone) + '.');
            if ChessBoard.PositionColor = White then
              WriteToGameLog(' ...');
          end;
        WriteToGameLog(' ' + PString(d1)^);
{$ENDIF}
        move_done:= TRUE;
        TakebackGame.Enabled:= TRUE;
      end;
    cbeMate:
      with ChessBoard do
        begin
          StopClock;
          Mode:= mView;
{$IFDEF GAME_LOG}
          WriteToGameLog('#');
          if PositionColor = White then WriteToGameLog(#13#10 + '0 - 1')
            else WriteToGameLog(#13#10 + '1 - 0');
          FlushGameLog;
{$ENDIF}
          if PositionColor = White then
            s := 'White'
          else
            s := 'Black';
          if ((PlayerColor <> White) and (PositionColor = White)) or
             ((PlayerColor <> Black) and (PositionColor = Black)) then
          begin
            MessageDlg(s + ' is checkmated. You win.', mtCustom, [mbOK]);
            ChessBoard.WriteGameToBase(grWin);
          end
          else
          begin
            MessageDlg(s + ' is checkmated. You loose.', mtCustom, [mbOK]);
            ChessBoard.WriteGameToBase(grLost);
          end;
        end;
    cbeStaleMate:
      begin
        ChessBoard.StopClock;
        ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
        WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
        FlushGameLog;
{$ENDIF}
        MessageDlg('It''s stalemate. No one wins.', mtCustom, [mbOK]);
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


procedure TManager.ConnectorHandler(e: TConnectorEvent; d1: pointer = nil; d2: pointer = nil);
var
  cmd_str, sl, sr, ms: string;
label
  l;
begin
  case e of
    ceConnected:
      begin
        if Assigned(ConnectingForm) then
          begin
            ConnectingForm.Shut;
            ConnectingForm := nil; // освобождается caFree
            ChessBoard.Enabled := TRUE;
            SendData(CMD_VERSION + ' ' + IntToStr(CHESS4NET_VERSION));
          end;
      end;
    ceDisconnected:
      begin
        if Connector.State = [] then exit;
        case ChessBoard.Mode of
          mView:
            begin
              MessageDlg('Your opponent leaves.', mtCustom, [mbOK]);
            end;
          mGame:
            begin
              ChessBoard.StopClock;
{$IFDEF GAME_LOG}
                WriteToGameLog('*');
                FlushGameLog;
{$ENDIF}
              MessageDlg('Your opponent leaves. The game is aborted.', mtWarning,
                         [mbOK]);
            end;
        end;
        CloseConnector;
        CB2View;
        bConnected := FALSE;
      end;
    ceError:
      begin
{$IFDEF GAME_LOG}
        if ChessBoard.Mode = mGame then
		  begin
            WriteToGameLog('*');
            FlushGameLog;
		  end;
{$ENDIF}	  
        MessageDlg('An error during connection occured.', mtWarning,
                   [mbOk]);
        Connector.Close;
        CB2View;
      end;
    ceData:
      begin
        cmd_str:= PString(d1)^;
l:
        sl:= LeftStr(cmd_str, pos(CMD_DELIMITER, cmd_str) - 1);
        cmd_str:= RightStr(cmd_str, length(cmd_str) -
                           length(sl) - length(CMD_DELIMITER));
        SplitStr(sl, sl, sr);
        if sl = CMD_ECHO then
          begin
{
            delta_time:= Now - past_time
}
          end
        else
        if sl = CMD_REPEAT_COMMAND then SendData;

        case ChessBoard.Mode of
	        mView:
            if sl = CMD_VERSION then
              begin
                SendData(CMD_WELCOME);
                SplitStr(sr, sl, sr);
                opponentClientVersion := StrToInt(sl);
                if opponentClientVersion < CHESS4NET_VERSION then
                MessageDlg('Your opponent is using an older version of Chess4Net.' + #13#10 +
                           'Most of functionality will be not available.'  + #13#10 +
                           'Please, ask him/her to update the client.', mtWarning, [mbOK]);
                // 2007.4 первый клиент с обратной совместимостью
                // Для несовместимых версий:
                // else SendData(CMD_GOODBYE);
              end
            else
            if sl = CMD_WELCOME then
              begin
                SendData(CMD_NICK + ' ' + player_nick);
                ChessBoard.InitPosition;
                if csServer in Connector.State then
                  ChessBoard.PlayerColor:= White
                else
                  ChessBoard.PlayerColor:= Black; // csClient in Connector.State
                SetClock;
                bConnected := TRUE;
              end
            else
            if sl = CMD_GOODBYE then // для будущих версий
              begin
                CloseConnector;
                MessageDlg('The current version of Chess4Net is incompatible with the one of you partner.' + #13#10+
                           'Please check the versions.' , mtWarning, [mbOK]);
              end
            else
	          if sl = CMD_START_GAME then
    	        with ChessBoard do
                begin
                  SetClock;
                  ResetMoveList;
                  Mode:= mGame;
                  move_done:= FALSE;
                  TakebackGame.Enabled := FALSE;
                  SwitchClock(PositionColor);
{$IFDEF GAME_LOG}
                  InitGameLog;
{$ENDIF}
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
            if sl = CMD_CHANGE_COLOR then  
              begin
                ChangeColor;
              end
            else
            if sl = CMD_NICK then
              begin
                opponent_nick := sr;
                if csServer in Connector.State then
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
              end
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
                if MessageDlg('Draw?', mtConfirmation, [mbYes, mbNo]) = mrNo then
                  begin
                    if ChessBoard.Mode = mGame then SendData(CMD_DRAW_DECLINED);
                  end
                else
                  if ChessBoard.Mode = mGame then
                    begin
                      SendData(CMD_DRAW_ACCEPTED);
                      ChessBoard.StopClock;
                      ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                      WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
                      FlushGameLog;
{$ENDIF}
                      MessageDlg('The game is drawn.', mtCustom, [mbOK]);
                        ChessBoard.WriteGameToBase(grDraw);
                    end;
              end
            else
            if sl = CMD_ABORT then
              begin
                if MessageDlg('Can we abort the game?', mtConfirmation, [mbYes, mbNo]) = mrNo then
                  begin
                    if ChessBoard.Mode = mGame then SendData(CMD_ABORT_DECLINED);
                  end
                else
                  begin
                    if ChessBoard.Mode = mGame then
                      begin
                        SendData(CMD_ABORT_ACCEPTED);
                        ChessBoard.StopClock;
                        ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                        WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
                        FlushGameLog;
{$ENDIF}
                        MessageDlg('The game is aborted.', mtCustom, [mbOK]);
                      end;
                  end;  
              end
            else
            if sl = CMD_RESIGN then
              begin
                ChessBoard.StopClock;
                ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                if ChessBoard.PlayerColor = White then
                  WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0')
                else
                  WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1');
                FlushGameLog;
{$ENDIF}
                MessageDlg('I resign. You win this game. Congratulations!',
                           mtCustom, [mbOK]);
                  ChessBoard.WriteGameToBase(grWin);
              end
            else
            if sl = CMD_ABORT_ACCEPTED then
              begin
                ChessBoard.StopClock;
                ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                WriteToGameLog('*');
                FlushGameLog;
{$ENDIF}
                MessageDlg('The game is aborted.', mtCustom, [mbOK]);
              end
            else
            if sl = CMD_ABORT_DECLINED then
              MessageDlg('Sorry, but we have to finish this game.',
                         mtCustom, [mbOK])
            else
            if sl = CMD_DRAW_ACCEPTED then
              begin
                ChessBoard.StopClock;
                ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                WriteToGameLog('=' + #13#10 + '1/2 - 1/2');
                FlushGameLog;
{$ENDIF}
                MessageDlg('The game is drawn.', mtCustom, [mbOK]);
                ChessBoard.WriteGameToBase(grDraw);
              end
            else
            if sl = CMD_DRAW_DECLINED then
              MessageDlg('No draw, sorry.', mtCustom, [mbOK])
            else
            if sl = CMD_SWITCH_CLOCK then
              with ChessBoard do
                try
                  SplitStr(sr, sl, sr);
                  ms:= RightStr(sl, length(sl) - LastDelimiter(':.', sl));
                  sl:= LeftStr(sl, length(sl) - length(ms) - 1);

                  if PlayerColor = White then
                    Time[Black]:= StrToTime(sl) + EncodeTime(0,0,0, StrToInt(ms))
                  else
                    Time[White]:= StrToTime(sl) + EncodeTime(0,0,0, StrToInt(ms));
                except
                  on Exception do ;
                end
            else
            if sl = CMD_FLAG then
              with ChessBoard do
                begin
                  if (Time[PlayerColor] = 0.0) then
                    begin
                      SendData(CMD_FLAG_YES);
                      ChessBoard.StopClock;
                      ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
                      if ChessBoard.PlayerColor = White then
                        WriteToGameLog(#13#10 + 'White forfeits on time')
                      else
                        WriteToGameLog(#13#10 + 'Black forfeits on time');
                      FlushGameLog;
{$ENDIF}
                      MessageDlg('You forfeit on time.', mtCustom, [mbOK]);
                        ChessBoard.WriteGameToBase(grLostTime);
                    end
                  else
                    SendData(CMD_FLAG_NO);
                end
            else
            if sl = CMD_FLAG_YES then
              begin
                ChessBoard.StopClock;
                ChessBoard.Mode:= mView;
{$IFDEF GAME_LOG}
              if ChessBoard.PlayerColor = White then
                WriteToGameLog(#13#10 + 'Black forfeits on time')
              else
                WriteToGameLog(#13#10 + 'White forfeits on time');
              FlushGameLog;
{$ENDIF}
                MessageDlg('Your opponent forfeits on time.', mtCustom, [mbOK]);
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
                end
            else
            if sl = CMD_PAUSE_GAME then
              begin
                if MessageDlg('Can we pause the game?', mtConfirmation, [mbYes, mbNo]) = mrYes then
                  begin
                    SendData(CMD_PAUSE_GAME_YES);
                    PauseGame;
                  end
                else // mrNo
                  SendData(CMD_PAUSE_GAME_NO);
              end
            else
            if sl = CMD_PAUSE_GAME_YES then
              begin
                PauseGame;
              end
            else
              if sl = CMD_PAUSE_GAME_NO then
            begin
              MessageDlg('No pause, sorry.', mtCustom, [mbOk]);
            end
            else
            if sl = CMD_CONTINUE_GAME then
              begin
                if Assigned(ContinueForm) then
                  begin
                    ContinueForm.Shut;
                    ContinueForm := nil; // освобождается caFree
                    ChessBoard.Enabled := TRUE;
                    ContinueGame;
                  end;
              end
            else
            if sl = CMD_TAKEBACK then
              begin
                if you_takebacks or ChessBoard.pTrainingMode then
                  begin
                    if (MessageDlg('May I take back last move?',
                                  mtConfirmation, [mbYes, mbNo]) = mrYes) and
                       (ChessBoard.Mode = mGame) then
                      begin
                        SendData(CMD_TAKEBACK_YES);
                        ChessBoard.TakeBack;
                        TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
                        WriteToGameLog(' <takeback>');
{$ENDIF}
                        ChessBoard.SwitchClock(ChessBoard.PositionColor);
                      end
                    else
                      SendData(CMD_TAKEBACK_NO);
                  end
                else
                  SendData(CMD_TAKEBACK_NO)
              end
            else
            if sl = CMD_TAKEBACK_YES then
              begin
                ChessBoard.TakeBack;
                TakebackGame.Enabled:= (ChessBoard.NMoveDone > 0);
{$IFDEF GAME_LOG}
                WriteToGameLog(' <takeback>');
{$ENDIF}
                ChessBoard.SwitchClock(ChessBoard.PositionColor);
              end
            else
            if sl = CMD_TAKEBACK_NO then
              begin
                MessageDlg('Sorry, no takebacks!', mtCustom, [mbOK]);
              end
            else
            if (sl = CMD_POSITION) and (csClient in Connector.State) then
              begin
                ChessBoard.StopClock;
                ChessBoard.Mode:= mView;
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
                        if PositionColor = White then
                          WriteToGameLog(' ...');
                      end;
                    WriteToGameLog(' ' + sl);
{$ENDIF}
                    move_done := TRUE;
                    TakebackGame.Enabled := TRUE;
                  end;
              end; {  with ChessBoard}
        end; { case ChessBoard.Mode }
        if cmd_str <> '' then goto l;
    end; {  ceData }
  end; { case ChessBoard.Mode }
end;


procedure TManager.FormDestroy(Sender: TObject);
begin
  WritePrivateSettings;
  ExtBaseList.Free;
  Connector.Free;
//  ChessBoard.Release;
end;


procedure TManager.ExitActionExecute(Sender: TObject);
begin
  Close;
end;


procedure TManager.OptionsActionExecute(Sender: TObject);
var
  lookFeelOptionsForm: TLookFeelOptionsForm;
begin
  lookFeelOptionsForm := TLookFeelOptionsForm.Create(ChessBoard);
  with lookFeelOptionsForm, ChessBoard do
    try
      AnimationComboBox.ItemIndex:= ord(animation);
      HilightLastMoveBox.Checked:= LastMoveHilighted;
      FlashIncomingMoveBox.Checked := FlashOnMove;
      CoordinatesBox.Checked := CoordinatesShown;
      StayOnTopBox.Checked := ChessBoard.StayOnTop;
      ExtraExitBox.Checked := extra_exit;

      if (lookFeelOptionsForm.ShowModal = mrCancel) then
        exit;

      animation := TAnimation(AnimationComboBox.ItemIndex);
      LastMoveHilighted := HilightLastMoveBox.Checked;
      FlashOnMove := FlashIncomingMoveBox.Checked;
      CoordinatesShown:= CoordinatesBox.Checked;
      StayOnTop := StayOnTopBox.Checked;
      extra_exit := ExtraExitBox.Checked;
    finally
      lookFeelOptionsForm.Free;
    end;
end;


procedure TManager.ConnectMainClick(Sender: TObject);
var
  ConnectionForm: TConnectionForm;
  p: integer;
  s: string;
begin
  if Assigned(ConnectingForm) then
    exit;
    
  ConnectionForm:= TConnectionForm.Create(ChessBoard);
  with ConnectionForm, Connector do
    try
      NickEdit.Text := player_nick;
      s := ipDomainPortServer;
      if s[length(s)] = 'S' then
        ConnectionForm.ServerRadioButton.Checked := TRUE
      else
        ConnectionForm.ClientRadioButton.Checked := TRUE;
      s := LeftStr(s, length(s) - 2);
      p := pos(':', s);
      if p >= 1 then
        begin
          // TODO: проверка на синтаксис IP
          IPEdit.Text := LeftStr(s, p - 1);
          PortEdit.Text := RightStr(s, length(s) - p);
        end;
      if ConnectionForm.ShowModal = mrCancel then exit;
      // mrOk
      player_nick := NickEdit.Text;
      ipDomainPortServer := IPEdit.Text + ':' + PortEdit.Text;
      if ConnectionForm.ServerRadioButton.Checked then
        ipDomainPortServer := ipDomainPortServer + '-S'
      else
        ipDomainPortServer := ipDomainPortServer + '-C';
      if ServerRadioButton.Checked then
        OpenServer(GetPort)
      else
        OpenClient(IPEdit.Text, GetPort);
    finally
      ConnectionForm.Free;
    end;
  ConnectingForm := TConnectingForm.Create(ChessBoard, ConnectionAbort);
  ChessBoard.Enabled := FALSE;
  ConnectingForm.Show;
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
  if (MessageDlg('Do you really want to resign?', mtConfirmation, [mbYes, mbNo]) = mrNo) or
     (ChessBoard.Mode <> mGame) then exit;

  ChessBoard.StopClock;
{$IFDEF GAME_LOG}
  if ChessBoard.PlayerColor = White then
    WriteToGameLog(#13#10 + 'White resigns' + #13#10 + '0 - 1')
  else
    WriteToGameLog(#13#10 + 'Black resigns' + #13#10 + '1 - 0');
  FlushGameLog;
{$ENDIF}
  ChessBoard.Mode:= mView;
  SendData(CMD_RESIGN);
    ChessBoard.WriteGameToBase(grLost);
end;

procedure TManager.DisconnectConnectedClick(Sender: TObject);
begin
  CloseConnector;
  CB2View;
  bConnected := FALSE;
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
  s, prevClock: string;
  GameOptionsForm: TGameOptionsForm;
  i: integer;
begin
  GameOptionsForm := TGameOptionsForm.Create(ChessBoard);
  with GameOptionsForm do
    try
      EqualTimeCheckBox.Checked := ((you_unlimited = opponent_unlimited) and
         (you_time = opponent_time) and (you_inc = opponent_inc));
      YouUnlimitedCheckBox.Checked := you_unlimited;
      OpponentUnlimitedCheckBox.Checked := opponent_unlimited;
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
      if opponentClientVersion < 200706 then
        GamePauseCheckBox.Enabled := FALSE
      else
        GamePauseCheckBox.Checked := can_pause_game;

      if (ShowModal = mrCancel) or (ChessBoard.Mode = mGame) then
        exit;
        
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
      if (opponentClientVersion >= 200705) then
        begin
          if prevClock <> s then
            SendData(CMD_SET_CLOCK + ' ' + s);
          if you_takebacks <> TakeBackCheckBox.Checked then
            begin
              if TakeBackCheckBox.Checked then
                SendData(CMD_ALLOW_TAKEBACKS + ' 1')
              else
                SendData(CMD_ALLOW_TAKEBACKS + ' 0');
            end;
        end;
      you_takebacks := TakeBackCheckBox.Checked;
      if (opponentClientVersion >= 200706) then
        begin
          if can_pause_game <> GamePauseCheckBox.Checked then
            begin
              can_pause_game := GamePauseCheckBox.Checked;
              if can_pause_game then
                SendData(CMD_CAN_PAUSE_GAME + ' 1')
              else
                SendData(CMD_CAN_PAUSE_GAME + ' 0')
            end;
        end;
      // тренировочный режим
      if (opponentClientVersion >= 200705) and (ChessBoard.pTrainingMode <> TrainingEnabledCheckBox.Checked) then
        begin
          if TrainingEnabledCheckBox.Checked then
            SendData(CMD_SET_TRAINING + ' 1')
          else
            SendData(CMD_SET_TRAINING + ' 0');
        end;
      ChessBoard.pTrainingMode := TrainingEnabledCheckBox.Checked;
      ExtBaseName := ExtBaseList[ExtBaseComboBox.ItemIndex];
      if ExtBaseName <> '' then
        ChessBoard.SetExternalBase(Chess4NetPath + ExtBaseName)
      else
        ChessBoard.UnsetExternalBase;  
      ChessBoard.pUseUserBase := UsrBaseCheckBox.Checked;

      GamePause.Visible := can_pause_game;
      TakebackGame.Visible := (ChessBoard.pTrainingMode or self.opponent_takebacks);

      if (opponentClientVersion < 200705) then // 2007.4
        begin
          if ChessBoard.pTrainingMode then
            s := s + ' 1 1'
          else
            begin
              if you_takebacks then
                s := s + ' 1 0'
              else
                s := s + ' 0 0';
              end;
          SendData(CMD_GAME_OPTIONS + ' ' + s);
        end;
    finally
      GameOptionsForm.Free;
    end;  
end;

procedure TManager.ConnectionAbort;
begin
  ConnectingForm := nil; // caFree
  ChessBoard.Enabled := TRUE;
  Connector.Close;
end;

procedure TManager.StartStandartGameConnectedClick(Sender: TObject);
begin
  with ChessBoard do
    begin
      SetClock;
      InitPosition;
      ResetMoveList;
      SendData(CMD_POSITION + ' ' + GetPosition + CMD_DELIMITER + CMD_START_GAME);
      Mode:= mGame;
      move_done:= FALSE;
      TakebackGame.Enabled := FALSE;
      SwitchClock(PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;


procedure TManager.SendData(cmd: string);
const
  last_cmd: string = '';
begin
  if cmd = '' then exit;
  last_cmd:= cmd + CMD_DELIMITER;
  Connector.SendData(last_cmd);
end;

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
     ChessBoard.Caption := CHESS4NET_TITLE;
     Mode := mView;
     InitPosition;
   end;
end;


procedure TManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MessageDlg('Do you want to exit?',
                mtConfirmation, [mbYes, mbNo]) = mrYes then
  begin
{$IFDEF GAME_LOG}
    if ChessBoard.Mode = mGame then
      begin
        WriteToGameLog('*');
        FlushGameLog;
      end;
{$ENDIF}
    CloseConnector;
    Release;
  end
  else
    Action:= caNone;
end;


function TManager.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
                             Buttons: TMsgDlgButtons): Word;
var
  DialogForm: TTntForm;
begin
  DialogForm := WideCreateMessageDialog(Msg, DlgType, Buttons);
  DialogForm.Caption := DIALOG_CAPTION;
  DialogForm.FormStyle := ChessBoard.FormStyle;
  with DialogForm do
    begin
      Left := ChessBoard.Left + (ChessBoard.Width - Width) div 2;
      Top := ChessBoard.Top + (ChessBoard.Height - Height) div 2;
      Result := ShowModal;
      DialogForm.Free;
    end;
end;


procedure TManager.CloseConnector;
begin
  if bConnected then
    WriteCommonSettings;
  ConnectorTimer.Enabled:= TRUE;
end;


procedure TManager.ConnectorTimerTimer(Sender: TObject);
begin
  Connector.Close;
  ConnectorTimer.Enabled:= FALSE;
end;


procedure TManager.StartPPRandomGameConnectedClick(
  Sender: TObject);
begin
  with ChessBoard do
    begin
      SetClock;
      PPRandom;
      ResetMoveList;
      SendData(CMD_POSITION + ' ' + GetPosition + CMD_DELIMITER + CMD_START_GAME);
      Mode:= mGame;
      move_done:= FALSE;
      SwitchClock(PositionColor);
    end;
{$IFDEF GAME_LOG}
  InitGameLog;
{$ENDIF}
end;


procedure TManager.TakebackGameClick(Sender: TObject);
begin
  SendData(CMD_TAKEBACK);
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


procedure TManager.SetClock(var sr: string);
var
  sl: string;
begin
  SplitStr(sr, sl, sr);
  if sl = 'u' then
    opponent_unlimited:= TRUE
  else
    begin
      opponent_unlimited:= FALSE;
      opponent_time:= StrToInt(sl);
      SplitStr(sr, sl, sr); opponent_inc:= StrToInt(sl);
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


procedure TManager.SetPrivateSettings;
var
  iniFile: TTntIniFile;
begin
  // Общие настройки по умолчанию
  ChessBoard.AutoFlag := TRUE;
  you_takebacks := FALSE;
  opponent_takebacks := FALSE;

  // Считывание личных настроек из INI-файла
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    player_nick := iniFile.ReadString(PRIVATE_SECTION_NAME, PLAYER_NICK_KEY_NAME, DEFAULT_PLAYER_NICK);
    ipDomainPortServer := iniFile.ReadString(PRIVATE_SECTION_NAME, IP_DOMAIN_PORT_SERVER_KEY_NAME, DEFAULT_IPDOMAIN_PORT_SERVER);
    ChessBoard.animation := TAnimation(iniFile.ReadInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(aQuick)));
    ChessBoard.LastMoveHilighted := iniFile.ReadBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, FALSE);
    ChessBoard.FlashOnMove := iniFile.ReadBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME
    , FALSE);
    ChessBoard.CoordinatesShown := iniFile.ReadBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, TRUE);
    ChessBoard.StayOnTop := iniFile.ReadBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, FALSE);
    extra_exit := iniFile.ReadBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, FALSE);
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
  clockStr := INITIAL_CLOCK_TIME;
  SetClock(clockStr);
  if opponentClientVersion < 200705 then // для 2007.4 общие настройки не применяются
    begin
      Result := TRUE;
      exit;
    end;

  Result := FALSE;
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + opponent_nick;
    if not iniFile.SectionExists(commonSectionName) then
      exit;

    if setToOpponent then
      begin
        playerColor := TFigureColor(iniFile.ReadInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(Black)));
        if (ChessBoard.PlayerColor = playerColor) then // каждый раз менять сохранённый цвет на противоположный
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
            if ChessBoard.pTrainingMode then
              SendData(CMD_SET_TRAINING + ' 1')
            else
              SendData(CMD_SET_TRAINING + ' 0');
          end;

        if opponentClientVersion >= 200706 then
          begin
            flag := iniFile.ReadBool(commonSectionName, CAN_PAUSE_GAME_KEY_NAME, FALSE);
            if can_pause_game <> flag then
              begin
                can_pause_game := flag;
                if can_pause_game then
                  SendData(CMD_CAN_PAUSE_GAME + ' 1')
                else
                  SendData(CMD_CAN_PAUSE_GAME + ' 0');
              end;
          end; { if opponentClientVersion }
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
        if you_takebacks then
          SendData(CMD_ALLOW_TAKEBACKS + ' 1')
        else
          SendData(CMD_ALLOW_TAKEBACKS + ' 0');
      end;
    ChessBoard.AutoFlag := iniFile.ReadBool(commonSectionName, AUTO_FLAG_KEY_NAME, FALSE);

    GamePause.Visible := can_pause_game;
    TakebackGame.Visible := (opponent_takebacks or ChessBoard.pTrainingMode);
  finally
    iniFile.Free;
  end;

  Result := TRUE;  
end;


procedure TManager.WritePrivateSettings;
var
  iniFile: TTntIniFile;
begin
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    // Запись личных настроек
    iniFile.WriteString(PRIVATE_SECTION_NAME, PLAYER_NICK_KEY_NAME, player_nick);
    iniFile.WriteString(PRIVATE_SECTION_NAME, IP_DOMAIN_PORT_SERVER_KEY_NAME, ipDomainPortServer);
    iniFile.WriteInteger(PRIVATE_SECTION_NAME, ANIMATION_KEY_NAME, Ord(ChessBoard.animation));
    iniFile.WriteBool(PRIVATE_SECTION_NAME, HILIGHT_LAST_MOVE_KEY_NAME, ChessBoard.LastMoveHilighted);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, FLASH_ON_MOVE_NAME, ChessBoard.FlashOnMove);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, SHOW_COORDINATES_KEY_NAME, ChessBoard.CoordinatesShown);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, STAY_ON_TOP_KEY_NAME, ChessBoard.StayOnTop);
    iniFile.WriteBool(PRIVATE_SECTION_NAME, EXTRA_EXIT_KEY_NAME, extra_exit);
  finally
    iniFile.Free;
  end;
end;


procedure TManager.WriteCommonSettings;
var
  iniFile: TTntIniFile;
  commonSectionName: string;
begin
  iniFile := TTntIniFile.Create(Chess4NetPath + INI_FILE_NAME);
  try
    // Запись общих настроек
    commonSectionName := COMMON_SECTION_PREFIX + ' ' + opponent_nick;
    iniFile.WriteInteger(commonSectionName, PLAYER_COLOR_KEY_NAME, Ord(ChessBoard.PlayerColor));
    iniFile.WriteString(commonSectionName, CLOCK_KEY_NAME, ClockToStr);
    iniFile.WriteBool(commonSectionName, TRAINING_MODE_KEY_NAME, ChessBoard.pTrainingMode);
    iniFile.WriteString(commonSectionName, EXTERNAL_BASE_NAME_KEY_NAME, ExtBaseName);
    iniFile.WriteBool(commonSectionName, USE_USER_BASE_KEY_NAME, ChessBoard.pUseUserBase);
    iniFile.WriteBool(commonSectionName, ALLOW_TAKEBACKS_KEY_NAME, you_takebacks);
    iniFile.WriteBool(commonSectionName, CAN_PAUSE_GAME_KEY_NAME, can_pause_game);
    iniFile.WriteBool(commonSectionName, AUTO_FLAG_KEY_NAME, ChessBoard.AutoFlag);
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


procedure TManager.ContinueHandler;
begin
  ContinueForm := nil; // caFree
  ChessBoard.Enabled := TRUE;
  SendData(CMD_CONTINUE_GAME);
  ContinueGame;
end;


procedure TManager.PauseGame;
begin
  if Assigned(ContinueForm) then
    exit;
  ChessBoard.StopClock;
  ContinueForm := TContinueForm.Create(ChessBoard, ContinueHandler);
  ChessBoard.Enabled := FALSE;
  ContinueForm.Show;
end;


procedure TManager.ContinueGame;
begin
  ChessBoard.SwitchClock(ChessBoard.PositionColor);
end;


initialization
  Chess4NetPath := ExtractFileDir(Application.ExeName) + '\';

end.
