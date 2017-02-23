object CEVersionMgrForm: TCEVersionMgrForm
  Left = 0
  Top = 0
  Caption = 'Version Manager'
  ClientHeight = 373
  ClientWidth = 639
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = TntFormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = TntFormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 639
    Height = 330
    Align = alClient
    ActiveTabIndex = 0
    TabVisible = False
    HiddenItems = <>
    object tab_versions: TSpTBXTabItem
      Caption = 'Versions'
      Checked = True
    end
    object tab_backups: TSpTBXTabItem
      Caption = 'Backups'
    end
    object sheet_backups: TSpTBXTabSheet
      Left = 0
      Top = 25
      Width = 639
      Height = 305
      Caption = 'Backups'
      ImageIndex = -1
      TabItem = 'tab_backups'
    end
    object sheet_versions: TSpTBXTabSheet
      Left = 0
      Top = 25
      Width = 639
      Height = 305
      Caption = 'Versions'
      ImageIndex = -1
      DesignSize = (
        639
        305)
      TabItem = 'tab_versions'
      object label_type: TSpTBXLabel
        Left = 523
        Top = 33
        Width = 102
        Height = 19
        Anchors = [akTop, akRight]
        AutoSize = False
        Alignment = taRightJustify
      end
      object ItemList: TVirtualStringTree
        Left = 8
        Top = 8
        Width = 293
        Height = 252
        Anchors = [akLeft, akTop, akBottom]
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Images = CE_Images.MiscImages
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnCompareNodes = ItemListCompareNodes
        OnFocusChanged = ItemListFocusChanged
        OnFreeNode = ItemListFreeNode
        OnGetText = ItemListGetText
        OnPaintText = ItemListPaintText
        OnGetImageIndex = ItemListGetImageIndex
        Columns = <
          item
            Position = 0
            Width = 119
            WideText = 'Version'
          end
          item
            Position = 1
            Width = 80
            WideText = 'Type'
          end
          item
            Position = 2
            Width = 90
            WideText = 'Date'
          end>
      end
      object label_version: TSpTBXLabel
        Left = 316
        Top = 8
        Width = 6
        Height = 6
      end
      object memo_notes: TTntMemo
        Left = 316
        Top = 58
        Width = 309
        Height = 165
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object label_datetime: TSpTBXLabel
        Left = 316
        Top = 33
        Width = 6
        Height = 6
      end
      object but_check: TSpTBXButton
        Left = 8
        Top = 266
        Width = 137
        Height = 30
        Caption = 'Check for updates'
        Anchors = [akLeft, akBottom]
        TabOrder = 4
        OnClick = but_checkClick
      end
      object label_lastcheck: TSpTBXLabel
        Left = 151
        Top = 272
        Width = 6
        Height = 6
        Anchors = [akLeft, akBottom]
        Enabled = False
      end
      object but_use: TSpTBXButton
        Left = 486
        Top = 229
        Width = 139
        Height = 30
        Caption = 'Use this version'
        Anchors = [akRight, akBottom]
        Enabled = False
        TabOrder = 3
        OnClick = but_useClick
      end
      object but_download: TSpTBXButton
        Left = 316
        Top = 229
        Width = 109
        Height = 30
        Caption = 'Download'
        Anchors = [akLeft, akBottom]
        Enabled = False
        TabOrder = 2
        OnClick = but_downloadClick
      end
    end
  end
  object panel_bottom: TSpTBXPanel
    Left = 0
    Top = 330
    Width = 639
    Height = 43
    Align = alBottom
    TabOrder = 1
    Borders = False
    TBXStyleBackground = True
    DesignSize = (
      639
      43)
    object but_close: TSpTBXButton
      Left = 531
      Top = 6
      Width = 99
      Height = 30
      Caption = 'Close'
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = but_closeClick
      Default = True
    end
    object label_current_version: TSpTBXLabel
      Left = 8
      Top = 12
      Width = 6
      Height = 6
      Anchors = [akLeft, akBottom]
      Enabled = False
    end
  end
end
