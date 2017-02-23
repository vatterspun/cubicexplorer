object CELoginPromptDlg: TCELoginPromptDlg
  Left = 0
  Top = 0
  Caption = 'Proxy Login'
  ClientHeight = 140
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    321
    140)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 12
    Top = 12
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object TntLabel2: TTntLabel
    Left = 163
    Top = 12
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object label_warning: TTntLabel
    Left = 12
    Top = 81
    Width = 297
    Height = 20
    AutoSize = False
    Caption = 'Warning: password will be saved in clear text!'
    Enabled = False
    WordWrap = True
  end
  object edit_username: TTntEdit
    Left = 12
    Top = 31
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object edit_password: TTntEdit
    Left = 163
    Top = 31
    Width = 146
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object check_save: TTntCheckBox
    Left = 12
    Top = 58
    Width = 297
    Height = 17
    Caption = 'Remember'
    TabOrder = 2
  end
  object but_ok: TTntButton
    Left = 153
    Top = 108
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object but_cancel: TTntButton
    Left = 234
    Top = 108
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
