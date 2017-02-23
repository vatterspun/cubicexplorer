object CEItemSelectSaveDlg: TCEItemSelectSaveDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 109
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object panel_background: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 282
    Height = 109
    Align = alClient
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    DesignSize = (
      282
      109)
    object label_combotitle: TSpTBXLabel
      Left = 8
      Top = 8
      Width = 6
      Height = 6
    end
    object combo: TSpTBXComboBox
      Left = 8
      Top = 29
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = comboChange
    end
    object but_ok: TSpTBXButton
      Left = 53
      Top = 68
      Width = 85
      Height = 29
      Caption = 'OK'
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = but_okClick
      Default = True
    end
    object but_cancel: TSpTBXButton
      Left = 144
      Top = 68
      Width = 85
      Height = 29
      Caption = 'Cancel'
      Anchors = [akTop, akRight]
      TabOrder = 2
      ModalResult = 2
    end
  end
end
