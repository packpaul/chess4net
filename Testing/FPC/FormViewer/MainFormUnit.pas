////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MainFormUnit;

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  //
  ModalForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    m_Dialogs: TDialogs;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  LookFeelOptionsUnit, GameOptionsUnit, CreditsFormUnit, InfoUnit;

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  m_Dialogs.CreateDialog(TLookFeelOptionsForm).ShowModal;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  m_Dialogs.CreateDialog(TGameOptionsForm).ShowModal;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  with TCreditsForm.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  m_Dialogs.CreateDialog(TInfoForm).ShowModal;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  m_Dialogs := TDialogs.Create(self, nil);
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  m_Dialogs.Free;
end;

initialization
  {$I MainFormUnit.lrs}

end.

