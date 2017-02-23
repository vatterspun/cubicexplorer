object CEToolbarCustomizer: TCEToolbarCustomizer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Customize'
  ClientHeight = 293
  ClientWidth = 390
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
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 251
    Width = 390
    Height = 42
    Caption = 'SpTBXPanel1'
    Align = alBottom
    TabOrder = 1
    Borders = False
    TBXStyleBackground = True
    DesignSize = (
      390
      42)
    object but_close: TSpTBXButton
      Left = 299
      Top = 6
      Width = 83
      Height = 29
      Caption = 'Close'
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = but_closeClick
      Default = True
      ModalResult = 2
    end
    object label_help: TSpTBXLabel
      Left = 3
      Top = 0
      Width = 294
      Height = 42
      Align = alCustom
      AutoSize = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
  end
  object TabControl: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 390
    Height = 251
    Align = alClient
    ActiveTabIndex = 0
    TabBackgroundBorders = True
    OnActiveTabChange = TabControlActiveTabChange
    HiddenItems = <>
    object tab_toolbars: TSpTBXTabItem
      Caption = 'Toolbars'
      Checked = True
    end
    object tab_buttons: TSpTBXTabItem
      Caption = 'Buttons'
    end
    object tab_theme: TSpTBXTabItem
      Caption = 'Theme'
    end
    object SpTBXTabSheet2: TSpTBXTabSheet
      Left = 0
      Top = 25
      Width = 390
      Height = 226
      Caption = 'Buttons'
      ImageIndex = -1
      TabItem = 'tab_buttons'
      object ActionTree: TVirtualStringTree
        Left = 2
        Top = 0
        Width = 384
        Height = 222
        Align = alClient
        BorderStyle = bsNone
        DragType = dtVCL
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MaxHeight = 100
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
        OnCompareNodes = ActionTreeCompareNodes
        OnDragAllowed = ActionTreeDragAllowed
        OnDragOver = ActionTreeDragOver
        OnDragDrop = ActionTreeDragDrop
        OnGetText = ActionTreeGetText
        OnPaintText = ActionTreePaintText
        OnGetImageIndexEx = ActionTreeGetImageIndexEx
        OnStartDrag = ActionTreeStartDrag
        Columns = <
          item
            Position = 0
            Width = 384
            WideText = 'Name'
          end>
      end
    end
    object SpTBXTabSheet3: TSpTBXTabSheet
      Left = 0
      Top = 25
      Width = 390
      Height = 226
      Caption = 'Theme'
      ImageIndex = -1
      DesignSize = (
        390
        226)
      TabItem = 'tab_theme'
      object ThemeList: TSpTBXListBox
        Left = 8
        Top = 8
        Width = 193
        Height = 209
        ItemHeight = 16
        TabOrder = 0
        OnClick = ThemeListClick
      end
      object but_loadTheme: TSpTBXButton
        Left = 207
        Top = 188
        Width = 175
        Height = 29
        Hint = 'Load theme from file'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnClick = but_loadThemeClick
      end
      object label_themeName: TSpTBXLabel
        Left = 207
        Top = 8
        Width = 175
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object label_themeAuthor: TSpTBXLabel
        Left = 207
        Top = 32
        Width = 175
        Height = 29
        AutoSize = False
        Wrapping = twWrap
      end
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 25
      Width = 390
      Height = 226
      Caption = 'Toolbars'
      ImageIndex = -1
      DesignSize = (
        390
        226)
      TabItem = 'tab_toolbars'
      object ToolbarList: TSpTBXCheckListBox
        Left = 8
        Top = 8
        Width = 177
        Height = 209
        ItemHeight = 16
        TabOrder = 0
        OnClick = ToolbarListClick
      end
      object group_displayMode: TSpTBXRadioGroup
        Left = 195
        Top = 8
        Width = 182
        Height = 97
        Caption = 'Display Mode'
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 1
        OnClick = group_displayModeClick
      end
      object check_largeIcons: TSpTBXCheckBox
        Left = 195
        Top = 111
        Width = 80
        Height = 21
        Caption = 'Large Icons'
        Enabled = False
        TabOrder = 2
        OnClick = check_largeIconsClick
      end
      object check_borders: TSpTBXCheckBox
        Left = 195
        Top = 132
        Width = 61
        Height = 21
        Caption = 'Borders'
        Enabled = False
        TabOrder = 3
        OnClick = check_bordersClick
      end
      object check_stretch: TSpTBXCheckBox
        Left = 195
        Top = 152
        Width = 59
        Height = 21
        Caption = 'Stretch'
        Enabled = False
        TabOrder = 4
        OnClick = check_stretchClick
      end
      object check_dragHandle: TSpTBXCheckBox
        Left = 195
        Top = 172
        Width = 83
        Height = 21
        Caption = 'Drag Handle'
        Enabled = False
        TabOrder = 5
        OnClick = check_dragHandleClick
      end
    end
  end
end
