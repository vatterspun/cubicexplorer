object BookmarkPropDlg: TBookmarkPropDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Properties'
  ClientHeight = 261
  ClientWidth = 359
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = TntFormKeyPress
  DesignSize = (
    359
    261)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 4
    Top = 4
    Width = 351
    Height = 222
    ActivePage = GeneralSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object GeneralSheet: TTabSheet
      Caption = 'General'
      DesignSize = (
        343
        194)
      object img_icon: TImage
        Left = 11
        Top = 12
        Width = 18
        Height = 21
        Center = True
        Stretch = True
        Transparent = True
      end
      object edit_name: TTntEdit
        Left = 35
        Top = 12
        Width = 297
        Height = 21
        TabOrder = 0
        OnChange = edit_nameChange
      end
      object TargetPanel: TPanel
        Left = 3
        Top = 47
        Width = 337
        Height = 144
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 32
          Height = 13
          Caption = 'Target'
        end
        object edit_target: TSpTBXButtonEdit
          Left = 8
          Top = 27
          Width = 256
          Height = 21
          TabOrder = 0
          OnChange = edit_nameChange
          EditButton.Left = 234
          EditButton.Top = 0
          EditButton.Width = 18
          EditButton.Height = 17
          EditButton.Align = alRight
          EditButton.DropDownMenu = FormPopupMenu
        end
        object check_relative: TCheckBox
          Left = 8
          Top = 54
          Width = 97
          Height = 17
          Caption = 'Relative'
          TabOrder = 1
          OnClick = check_relativeClick
        end
        object but_browse: TButton
          Left = 270
          Top = 27
          Width = 59
          Height = 21
          Caption = 'Browse...'
          TabOrder = 2
          OnClick = but_browseClick
        end
      end
      object SessionPanel: TPanel
        Left = 3
        Top = 47
        Width = 337
        Height = 144
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 66
          Height = 13
          Caption = 'Session Name'
        end
        object combo_session: TComboBox
          Left = 8
          Top = 27
          Width = 174
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = edit_nameChange
        end
      end
    end
  end
  object but_OK: TButton
    Left = 118
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = but_OKClick
  end
  object but_Cancel: TButton
    Left = 199
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = but_CancelClick
  end
  object but_Apply: TButton
    Left = 280
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 3
    OnClick = but_ApplyClick
  end
  object FormPopupMenu: TSpTBXFormPopupMenu
    OnPopup = FormPopupMenuPopup
    BorderStyle = pbsSizeableRightBottom
    OnClosePopup = FormPopupMenuClosePopup
    Left = 88
  end
  object open1: TOpenDialog
    Left = 60
  end
end
