object CEUpdateDlg: TCEUpdateDlg
  Left = 0
  Top = 0
  Caption = 'Update CubicExplorer'
  ClientHeight = 302
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    354
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Files to update'
  end
  object Panel1: TPanel
    Left = 8
    Top = 27
    Width = 338
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    TabOrder = 2
  end
  object but_cancel: TTntButton
    Left = 251
    Top = 265
    Width = 95
    Height = 29
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object but_update: TTntButton
    Left = 119
    Top = 265
    Width = 126
    Height = 29
    Anchors = [akRight, akBottom]
    Caption = 'Update'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = but_updateClick
  end
  object progressbar: TProgressBar
    Left = 8
    Top = 242
    Width = 338
    Height = 17
    TabOrder = 3
  end
  object timer_close_dlg: TTimer
    Enabled = False
    OnTimer = timer_close_dlgTimer
    Left = 12
    Top = 268
  end
end
