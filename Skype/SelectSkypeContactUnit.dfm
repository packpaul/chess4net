object SelectSkypeContactForm: TSelectSkypeContactForm
  Left = 565
  Top = 197
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Skype contact'
  ClientHeight = 319
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    319
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TTntButton
    Left = 236
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TTntButton
    Left = 236
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ContactsListBox: TTntListBox
    Left = 8
    Top = 8
    Width = 222
    Height = 305
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
end
