////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit DontShowMessageDlgUnit;

interface

uses
  Forms, Dialogs, StdCtrls, Controls;

type
  TDontShowMessageDlg = class
  private
    m_MsgDlgForm: TForm;
    m_DontShowCheckBox: TCheckBox;
    constructor Create(const wstrMsg: WideString; ADlgType: TMsgDlgType;
      AButtons: TMsgDlgButtons);
    function FGetDontShow: boolean;
    procedure FSetDontShow(bValue: boolean);
    function FShow: TModalResult;
    property DontShow: boolean read FGetDontShow write FSetDontShow;
  public
    destructor Destroy; override;
    class procedure Show(const wstrMsg: WideString; var bDontShowFlag: boolean); overload;
    class function Show(const wstrMsg: WideString; ADlgType: TMsgDlgType;
      AButtons: TMsgDlgButtons; var bDontShowFlag: boolean): TModalResult; overload;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TDontShowMessageDlg

constructor TDontShowMessageDlg.Create(const wstrMsg: WideString;
  ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons);
begin
  inherited Create;

  m_MsgDlgForm := CreateMessageDialog(wstrMsg, ADlgType, AButtons);
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
  Show(wstrMsg, mtInformation, [mbOk], bDontShowFlag); 
end;


class function TDontShowMessageDlg.Show(const wstrMsg: WideString; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; var bDontShowFlag: boolean): TModalResult;
begin
  with TDontShowMessageDlg.Create(wstrMsg, ADlgType, AButtons) do
  try
    DontShow := bDontShowFlag;
    Result := FShow;
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


function TDontShowMessageDlg.FShow: TModalResult;
begin
  Result := m_MsgDlgForm.ShowModal;
end;

end.
