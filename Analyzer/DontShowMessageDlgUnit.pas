unit DontShowMessageDlgUnit;

interface

uses
  Forms, StdCtrls;

type
  TDontShowMessageDlg = class
  private
    m_MsgDlgForm: TForm;
    m_DontShowCheckBox: TCheckBox;
    constructor Create(const wstrMsg: WideString);
    function FGetDontShow: boolean;
    procedure FSetDontShow(bValue: boolean);
    procedure FShow;
    property DontShow: boolean read FGetDontShow write FSetDontShow;
  public
    destructor Destroy; override;
    class procedure Show(const wstrMsg: WideString; var bDontShowFlag: boolean);
  end;

implementation

uses
  Dialogs, Controls;

////////////////////////////////////////////////////////////////////////////////
// TDontShowMessageDlg

constructor TDontShowMessageDlg.Create(const wstrMsg: WideString);
begin
  inherited Create;

  m_MsgDlgForm := CreateMessageDialog(wstrMsg, mtInformation, [mbOK]);
  m_MsgDlgForm.Position := poScreenCenter;
  m_MsgDlgForm.Height := m_MsgDlgForm.Height + 10; 

  m_DontShowCheckBox := TCheckBox.Create(m_MsgDlgForm);
  m_DontShowCheckBox.Parent := m_MsgDlgForm;
  m_DontShowCheckBox.Caption := 'Don''t Show';

  m_DontShowCheckBox.Left := 10;
  m_DontShowCheckBox.Top := m_MsgDlgForm.ClientHeight - m_DontShowCheckBox.Height - 5;
end;


destructor TDontShowMessageDlg.Destroy;
begin
  m_MsgDlgForm.Free;
  inherited;
end;


class procedure TDontShowMessageDlg.Show(const wstrMsg: WideString; var bDontShowFlag: boolean);
begin
  with TDontShowMessageDlg.Create(wstrMsg) do
  try
    DontShow := bDontShowFlag;
    FShow;
    bDontShowFlag := DontShow;
  finally
    Free;
  end;
end;


function TDontShowMessageDlg.FGetDontShow: boolean;
begin
  Result := m_DontShowCheckBox.Checked;
end;


procedure TDontShowMessageDlg.FSetDontShow(bValue: boolean);
begin
  m_DontShowCheckBox.Checked := bValue;
end;


procedure TDontShowMessageDlg.FShow;
begin
  m_MsgDlgForm.ShowModal;
end;

end.
