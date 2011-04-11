////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit InterProc;

interface

// Activates application with the same name if it's already running
// Returns TRUE if application was activated, otherwise FALSE
// Drawbackds: It may activate other application with the same name by mistake
function ActivateApplicationIfRunning: boolean;

implementation

uses
  Forms, Windows, SysUtils;

function ActivateApplicationIfRunning: boolean;
var
  Wnd: HWND;
  strApplicationTitle: string;
  Buf: array[0..127] of char;
begin
  Result := FALSE;

  strApplicationTitle := Application.Title;

  Wnd := GetWindow(Application.Handle, GW_HWNDFIRST);
  while (Wnd <> 0) do
  begin
    if ((Wnd <> Application.Handle) and (GetWindow(Wnd, GW_OWNER) = 0)) THEN
    begin
      GetWindowText(Wnd, Buf, SizeOf(buf));
      if (StrPas(Buf) = strApplicationTitle) then
      begin
        SetForegroundWindow(Wnd);
        ShowWindow(Wnd, SW_SHOWNORMAL);
        Result := TRUE;
        exit;
      end;
    end;
    Wnd := GetWindow(Wnd, GW_HWNDNEXT);
  end;
end;

end.
