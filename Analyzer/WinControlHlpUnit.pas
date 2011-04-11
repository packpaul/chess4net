////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit WinControlHlpUnit;

interface

uses
  Controls, Messages;

type
  TWinControlHlp = class(TWinControl)
  public
    class procedure CNKeyDown(WinControl: TWinControl;
      var Message: TWMKeyDown);
  end;

implementation

uses
  Classes, Windows, Forms;

////////////////////////////////////////////////////////////////////////////////
// TWinControlHlp

class procedure TWinControlHlp.CNKeyDown(WinControl: TWinControl;
  var Message: TWMKeyDown);
var
  WCHlp: TWinControlHlp;
  Mask: Integer;
begin
  WCHlp := TWinControlHlp(WinControl);

  with Message do
  begin
    Result := 1;
    WCHlp.UpdateUIState(Message.CharCode);
//    if IsMenuKey(Message) then Exit;
    if not (csDesigning in WCHlp.ComponentState) then
    begin
      if (WCHlp.Perform(CM_CHILDKEY, CharCode, Integer(Self)) <> 0) then
        Exit;
      Mask := 0;
      case CharCode of
        VK_TAB:
          Mask := DLGC_WANTTAB;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          Mask := DLGC_WANTARROWS;
        VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
          Mask := DLGC_WANTALLKEYS;
      end;
      if (Mask <> 0) and
         (WCHlp.Perform(CM_WANTSPECIALKEY, CharCode, 0) = 0) and
         (WCHlp.Perform(WM_GETDLGCODE, 0, 0) and Mask = 0) and
         (GetParentForm(WCHlp).Perform(CM_DIALOGKEY, CharCode, KeyData) <> 0) then
        Exit;
    end;
    Result := 0;
  end;
  
end;

end.
 