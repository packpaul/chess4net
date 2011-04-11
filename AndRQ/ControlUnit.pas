////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ControlUnit;

interface

function pluginFun(data: pointer): pointer; stdcall;

implementation

uses
  SysUtils, Forms,
  pluginutil, plugin, CallExec,
  GlobalsLocalUnit, InfoUnit, ManagerUnit, ConnectorUnit;

var
  hStartButton: integer;

procedure ButtonProc(btn: integer);
begin
  case btn of
    0: // лева€ кнопка мыши
      begin
        TManager.Create;
      end;
    1: ; // права€
    2: ; // средн€€
  end;
end;


function pluginFun(data: pointer): pointer; stdcall;
var
  APIver, UIN: integer;
  path, userPath: string;
begin
  result := nil;
  if (data = nil) or (_int_at(data) = 0) then exit;
  case _byte_at(data, 4) of
    PM_EVENT:
      case _byte_at(data, 5) of
        PE_INITIALIZE:
          begin
            RQ__ParseInitString(data, callback, APIver, path, userPath, UIN);
            AndRQVersion := RQ_GetAndrqVersion;
            // возврат имени и версии плагина
            result := str2comm(char(PM_DATA) + _istring(PLUGIN_NAME) + _int(PLUGIN_VERSION));
            // создать кнопку plugin(а) в chat(е)
            hStartButton := RQ_CreateChatButton(@ButtonProc, pluginIcon, PLUGIN_NAME);
          end;
        PE_PREFERENCES:
          begin
            ShowInfo;
          end;
        PE_MSG_GOT:
          begin
            Result := MessageGot(data);
          end;
        PE_MSG_SENT:
          begin
//          Result := MessageSent(data);
            MessageSent(data);
          end;
        PE_FINALIZE:
          begin
            RQ_DeleteChatButton(hStartButton);
          end;
      end;
    PM_ERROR:
      begin
        // TODO: ќбработка ошибок
      end;
  end;
end;

end.
