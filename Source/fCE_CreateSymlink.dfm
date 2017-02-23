object CreateSymlinkDlg: TCreateSymlinkDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create Symbolic Link'
  ClientHeight = 156
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = TntFormCreate
  OnKeyPress = TntFormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXTabControl1: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 329
    Height = 117
    Align = alClient
    ActiveTabIndex = 0
    TabVisible = False
    HiddenItems = <>
    object SpTBXTabItem1: TSpTBXTabItem
      Checked = True
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 10
      Width = 329
      Height = 107
      ImageIndex = -1
      DesignSize = (
        329
        107)
      TabItem = 'SpTBXTabItem1'
      object edit_linkname: TSpTBXEdit
        Left = 8
        Top = 28
        Width = 313
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object edit_targetpath: TSpTBXButtonEdit
        Left = 8
        Top = 76
        Width = 313
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        EditButton.Left = 290
        EditButton.Top = 0
        EditButton.Width = 19
        EditButton.Height = 17
        EditButton.Caption = '...'
        EditButton.Align = alRight
        EditButton.OnClick = edit_targetpathSubEditButton0Click
      end
      object SpTBXLabel1: TSpTBXLabel
        Left = 8
        Top = 7
        Width = 54
        Height = 19
        Caption = 'Link Name'
      end
      object SpTBXLabel2: TSpTBXLabel
        Left = 8
        Top = 55
        Width = 71
        Height = 19
        Caption = 'Target Folder'
      end
    end
  end
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 117
    Width = 329
    Height = 39
    Caption = 'SpTBXPanel1'
    Align = alBottom
    TabOrder = 1
    Borders = False
    TBXStyleBackground = True
    DesignSize = (
      329
      39)
    object but_cancel: TSpTBXButton
      Left = 246
      Top = 6
      Width = 75
      Height = 27
      Caption = 'Cancel'
      Anchors = [akTop, akRight]
      TabOrder = 1
      ModalResult = 2
    end
    object but_create: TSpTBXButton
      Left = 160
      Top = 6
      Width = 80
      Height = 27
      Caption = 'Create'
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = but_createClick
      Default = True
    end
  end
end
