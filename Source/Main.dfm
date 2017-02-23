object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'CubicExplorer'
  ClientHeight = 441
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnClick = FormCreate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = TntFormResize
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object panel_curtain: TPanel
    Left = 0
    Top = 0
    Width = 628
    Height = 441
    BevelOuter = bvNone
    Caption = 'Starting CubicExplorer...'
    Color = clWindow
    ParentBackground = False
    TabOrder = 5
  end
  object BottomToolDock: TSpTBXDock
    Left = 0
    Top = 432
    Width = 628
    Height = 9
    Position = dpBottom
  end
  object LeftToolDock: TSpTBXDock
    Left = 0
    Top = 59
    Width = 9
    Height = 373
    Position = dpLeft
  end
  object MainPanel: TPanel
    Left = 9
    Top = 59
    Width = 610
    Height = 373
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object RightToolDock: TSpTBXDock
    Left = 619
    Top = 59
    Width = 9
    Height = 373
    Position = dpRight
  end
  object TopToolDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 628
    Height = 59
    PopupMenu = ToolbarPopupMenu
    object MainToolbar: TCEToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockPos = 0
      DragHandleStyle = dhNone
      Images = CE_Images.SmallIcons
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 0
      Caption = 'Main Menu'
      Customizable = False
      MenuBar = True
      object fileMenuItem: TSpTBXSubmenuItem
        Caption = 'File'
        DisplayMode = nbdmTextOnly
        object SpTBXItem4: TSpTBXItem
          Action = CEActions.act_tabs_addtab
        end
        object SpTBXItem43: TSpTBXItem
          Action = CEActions.act_navi_texteditor
        end
        object SpTBXItem90: TSpTBXItem
          Action = CEActions.act_navi_quickview
        end
        object SpTBXItem81: TSpTBXItem
          Action = CEActions.act_navi_filesearch
        end
        object SpTBXSeparatorItem13: TSpTBXSeparatorItem
        end
        object SpTBXItem72: TSpTBXItem
          Action = CEActions.act_tabs_duplicatetab
        end
        object SpTBXItem6: TSpTBXItem
          Action = CEActions.act_tabs_closetab
        end
        object SpTBXSeparatorItem19: TSpTBXSeparatorItem
        end
        object SpTBXSubmenuItem1: TSpTBXSubmenuItem
          Action = CEActions.act_tabs_undo
          DropdownCombo = True
          OnPopup = sub_closed_tab_listPopup
        end
        object SpTBXSeparatorItem5: TSpTBXSeparatorItem
        end
        object SpTBXItem9: TSpTBXItem
          Action = CEActions.act_navi_back
        end
        object SpTBXItem8: TSpTBXItem
          Action = CEActions.act_navi_forward
        end
        object SpTBXItem7: TSpTBXItem
          Action = CEActions.act_navi_folderup
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem96: TSpTBXItem
          Action = CEActions.act_gen_new_instance
        end
        object SpTBXItem3: TSpTBXItem
          Action = CEActions.act_gen_exit
        end
      end
      object editMenuItem: TSpTBXSubmenuItem
        Caption = 'Edit'
        object SpTBXItem25: TSpTBXItem
          Action = CEActions.act_edit_copy
        end
        object SpTBXItem24: TSpTBXItem
          Action = CEActions.act_edit_cut
        end
        object SpTBXItem23: TSpTBXItem
          Action = CEActions.act_edit_paste
        end
        object SpTBXItem42: TSpTBXItem
          Action = CEActions.act_edit_duplicate
        end
        object SpTBXItem78: TSpTBXItem
          Action = CEActions.act_edit_copypath
        end
        object SpTBXSeparatorItem7: TSpTBXSeparatorItem
        end
        object SpTBXItem22: TSpTBXItem
          Action = CEActions.act_edit_delete
        end
        object SpTBXItem98: TSpTBXItem
          Action = CEActions.act_edit_undo_delete
        end
        object SpTBXSeparatorItem30: TSpTBXSeparatorItem
        end
        object SpTBXItem21: TSpTBXItem
          Action = CEActions.act_edit_rename
        end
        object SpTBXSeparatorItem8: TSpTBXSeparatorItem
        end
        object SpTBXItem77: TSpTBXItem
          Action = CEActions.act_edit_newfile
        end
        object SpTBXItem76: TSpTBXItem
          Action = CEActions.act_edit_newfolder
        end
        object SpTBXSeparatorItem11: TSpTBXSeparatorItem
        end
        object SpTBXItem45: TSpTBXItem
          Action = CEActions.act_edit_create_symlink
        end
        object SpTBXSeparatorItem6: TSpTBXSeparatorItem
        end
        object SpTBXItem20: TSpTBXItem
          Action = CEActions.act_edit_selall
        end
        object SpTBXItem19: TSpTBXItem
          Action = CEActions.act_edit_invertsel
        end
        object SpTBXSeparatorItem9: TSpTBXSeparatorItem
        end
        object SpTBXItem17: TSpTBXItem
          Action = CEActions.act_edit_properties
        end
      end
      object viewMenuItem: TSpTBXSubmenuItem
        Caption = 'View'
        object SpTBXItem86: TSpTBXItem
          Action = CEActions.act_view_viewstyle
        end
        object SpTBXItem10: TSpTBXItem
          Action = CEActions.act_view_arrangeby
        end
        object SpTBXItem11: TSpTBXItem
          Action = CEActions.act_view_groupby
        end
        object SpTBXItem35: TSpTBXItem
          Action = CEActions.act_view_hiddenfiles
        end
        object SpTBXItem79: TSpTBXItem
          Action = CEActions.act_view_showextensions
        end
        object SpTBXSeparatorItem18: TSpTBXSeparatorItem
        end
        object toolbarsMenuItem: TSpTBXSubmenuItem
          Caption = 'Toolbars'
        end
        object SpTBXSubmenuItem9: TSpTBXSubmenuItem
          Caption = 'Panels'
          object SpTBXItem26: TSpTBXItem
            Action = CEActions.act_view_folders
          end
          object SpTBXItem18: TSpTBXItem
            Action = CEActions.act_view_bookmark
          end
          object SpTBXItem27: TSpTBXItem
            Action = CEActions.act_view_quickview
          end
          object SpTBXItem53: TSpTBXItem
            Action = CEActions.act_view_filters
          end
          object SpTBXItem51: TSpTBXItem
            Action = CEActions.act_view_dropstack
          end
          object SpTBXItem80: TSpTBXItem
            Action = CEActions.act_view_archiver
          end
        end
        object SpTBXItem87: TSpTBXItem
          Action = CEActions.act_view_infobar
        end
        object SpTBXItem56: TSpTBXItem
          Action = CEActions.act_view_statusbar
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object SpTBXSubmenuItem6: TSpTBXSubmenuItem
          Caption = 'Theme'
          ImageIndex = 40
          object SkinGroupItem: TSpTBXSkinGroupItem
          end
          object SpTBXSeparatorItem21: TSpTBXSeparatorItem
          end
          object SpTBXItem82: TSpTBXItem
            Action = CEActions.act_view_loadskin
          end
        end
        object LanguageMenuItem: TSpTBXSubmenuItem
          Caption = 'Language'
          ImageIndex = 52
          OnPopup = LanguageMenuItemPopup
        end
        object SpTBXSeparatorItem16: TSpTBXSeparatorItem
        end
        object AlphaSubmenuItem: TSpTBXSubmenuItem
          Caption = 'Transparency'
          ImageIndex = 65
          Visible = False
          OnPopup = TransparencyPopup
          object SpTBXItem57: TSpTBXItem
            Caption = 'None'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem58: TSpTBXItem
            Tag = 1
            Caption = '10%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem59: TSpTBXItem
            Tag = 2
            Caption = '20%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem60: TSpTBXItem
            Tag = 3
            Caption = '30%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem61: TSpTBXItem
            Tag = 4
            Caption = '40%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem62: TSpTBXItem
            Tag = 5
            Caption = '50%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem63: TSpTBXItem
            Tag = 6
            Caption = '60%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem64: TSpTBXItem
            Tag = 7
            Caption = '70%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem65: TSpTBXItem
            Tag = 8
            Caption = '80%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
          object SpTBXItem66: TSpTBXItem
            Tag = 9
            Caption = '90%'
            AutoCheck = True
            GroupIndex = 1
            OnClick = TransparencyClick
          end
        end
        object SpTBXItem73: TSpTBXItem
          Action = CEActions.act_view_alwaysontop
        end
        object SpTBXItem74: TSpTBXItem
          Action = CEActions.act_view_fullscreen
        end
        object SpTBXSeparatorItem3: TSpTBXSeparatorItem
        end
        object SpTBXItem71: TSpTBXItem
          Action = CEActions.act_view_showhints
        end
        object SpTBXItem50: TSpTBXItem
          Action = CEActions.act_view_showheaderalways
        end
        object SpTBXItem46: TSpTBXItem
          Action = CEActions.act_view_checkbox_selection
        end
      end
      object bookmarkMenuItem: TSpTBXSubmenuItem
        Caption = 'Bookmarks'
      end
      object sessionsMenuItem: TSpTBXSubmenuItem
        Caption = 'Sessions'
        object SpTBXItem95: TSpTBXItem
          Action = CEActions.act_sessions_save
        end
        object SpTBXItem94: TSpTBXItem
          Action = CEActions.act_sessions_manage
        end
        object SpTBXSeparatorItem26: TSpTBXSeparatorItem
        end
        object sessionHistoryMenuItem: TSpTBXSubmenuItem
          Caption = 'History'
          object SpTBXItem85: TSpTBXItem
            Action = CEActions.act_sessions_addhistoryitem
          end
          object SpTBXItem47: TSpTBXItem
            Action = CEActions.act_sessions_clearhistory
          end
          object SpTBXSeparatorItem23: TSpTBXSeparatorItem
          end
          object SpTBXItem88: TSpTBXItem
            Action = CEActions.act_sessions_enablehistory
          end
          object SpTBXSeparatorItem22: TSpTBXSeparatorItem
          end
        end
        object SpTBXSeparatorItem27: TSpTBXSeparatorItem
        end
      end
      object toolsMenuItem: TSpTBXSubmenuItem
        Caption = 'Tools'
        object SpTBXItem54: TSpTBXItem
          Action = CEActions.act_tools_mapdrive
        end
        object SpTBXItem55: TSpTBXItem
          Action = CEActions.act_tools_disconnectdrive
        end
        object SpTBXSeparatorItem28: TSpTBXSeparatorItem
        end
        object SpTBXItem44: TSpTBXItem
          Action = CEActions.act_tools_showcustomizer
        end
        object SpTBXItem75: TSpTBXItem
          Action = CEActions.act_help_versionmgr
        end
        object SpTBXSeparatorItem15: TSpTBXSeparatorItem
        end
        object SpTBXItem84: TSpTBXItem
          Action = CEActions.act_tools_showoptions
        end
      end
      object helpMenuItem: TSpTBXSubmenuItem
        Caption = 'Help'
        object SpTBXItem5: TSpTBXItem
          Action = CEActions.act_help_home
        end
        object SpTBXItem2: TSpTBXItem
          Action = CEActions.act_help_forum
        end
        object SpTBXItem91: TSpTBXItem
          Action = CEActions.act_help_donate
        end
        object SpTBXSeparatorItem12: TSpTBXSeparatorItem
        end
        object SpTBXItem83: TSpTBXItem
          Action = CEActions.act_help_poedit_form
        end
        object SpTBXSeparatorItem31: TSpTBXSeparatorItem
        end
        object but_reset_layout: TSpTBXItem
          Action = CEActions.act_help_restore_layout
        end
        object SpTBXSeparatorItem29: TSpTBXSeparatorItem
        end
        object SpTBXItem97: TSpTBXItem
          Action = CEActions.act_help_checkupdates
        end
        object SpTBXSeparatorItem20: TSpTBXSeparatorItem
        end
        object SpTBXItem1: TSpTBXItem
          Action = CEActions.act_help_about
        end
      end
    end
    object ViewToolbar: TCEToolbar
      Left = 270
      Top = 25
      ChevronMoveItems = False
      DockPos = 266
      DockRow = 1
      Images = CE_Images.MediumIcons
      PopupMenu = ToolbarPopupMenu
      TabOrder = 1
      Caption = 'View'
      object SpTBXItem28: TSpTBXItem
        Action = CEActions.act_view_large
      end
      object SpTBXItem33: TSpTBXItem
        Action = CEActions.act_view_small
      end
      object SpTBXItem32: TSpTBXItem
        Action = CEActions.act_view_list
      end
      object SpTBXItem31: TSpTBXItem
        Action = CEActions.act_view_details
      end
      object SpTBXItem30: TSpTBXItem
        Action = CEActions.act_view_tiles
      end
      object SpTBXItem29: TSpTBXItem
        Action = CEActions.act_view_thumbs
      end
      object SpTBXItem34: TSpTBXItem
        Action = CEActions.act_view_filmstrip
      end
    end
    object NavigationToolbar: TCEToolbar
      Left = 0
      Top = 25
      ChevronMoveItems = False
      DockPos = 0
      DockRow = 1
      Images = CE_Images.MediumIcons
      PopupMenu = ToolbarPopupMenu
      TabOrder = 2
      Caption = 'Navigation'
      object SpTBXItem40: TSpTBXItem
        Action = CEActions.act_navi_back
      end
      object SpTBXItem39: TSpTBXItem
        Action = CEActions.act_navi_forward
      end
      object SpTBXItem38: TSpTBXItem
        Action = CEActions.act_navi_folderup
      end
      object SpTBXItem41: TSpTBXItem
        Action = CEActions.act_navi_refresh
      end
      object SpTBXSeparatorItem10: TSpTBXSeparatorItem
      end
      object SpTBXItem37: TSpTBXItem
        Action = CEActions.act_tabs_addtab
      end
      object SpTBXItem36: TSpTBXItem
        Action = CEActions.act_tabs_closetab
      end
      object SpTBXSeparatorItem14: TSpTBXSeparatorItem
      end
      object SpTBXItem48: TSpTBXItem
        Action = CEActions.act_navi_texteditor
      end
      object SpTBXItem49: TSpTBXItem
        Action = CEActions.act_navi_filesearch
      end
    end
    object EditToolbar: TCEToolbar
      Left = 511
      Top = 25
      DockPos = 511
      DockRow = 1
      Images = CE_Images.SmallIcons
      PopupMenu = ToolbarPopupMenu
      TabOrder = 3
      Caption = 'Edit'
      object SpTBXItem67: TSpTBXItem
        Action = CEActions.act_edit_copy
      end
      object SpTBXItem68: TSpTBXItem
        Action = CEActions.act_edit_cut
      end
      object SpTBXItem69: TSpTBXItem
        Action = CEActions.act_edit_paste
      end
      object SpTBXItem70: TSpTBXItem
        Action = CEActions.act_edit_delete
      end
    end
  end
  object ToolbarPopupMenu: TSpTBXPopupMenu
    Left = 16
    Top = 64
    object SpTBXItem52: TSpTBXItem
      Action = CEActions.act_tools_showcustomizer
    end
    object SpTBXSeparatorItem17: TSpTBXSeparatorItem
    end
  end
  object StartUpTimer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = StartUpTimerTimer
    Left = 48
    Top = 64
  end
  object TabPopupMenu: TSpTBXPopupMenu
    Images = CE_Images.SmallIcons
    OnPopup = TabPopupMenuPopup
    Left = 96
    Top = 64
    object SpTBXItem14: TSpTBXItem
      Action = CEActions.act_tabs_addtab
    end
    object SpTBXItem15: TSpTBXItem
      Action = CEActions.act_tabs_duplicatetab
    end
    object SpTBXSeparatorItem24: TSpTBXSeparatorItem
    end
    object SpTBXItem12: TSpTBXItem
      Action = CEActions.act_tabs_closetab
    end
    object SpTBXItem13: TSpTBXItem
      Action = CEActions.act_tabs_closeothertabs
    end
    object SpTBXItem16: TSpTBXItem
      Action = CEActions.act_tabs_closeonleft
    end
    object SpTBXItem89: TSpTBXItem
      Action = CEActions.act_tabs_closeonright
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object sub_closed_tab_list: TSpTBXSubmenuItem
      Action = CEActions.act_tabs_undo
      DropdownCombo = True
      OnPopup = sub_closed_tab_listPopup
    end
  end
  object MainMenuPopupMenu: TSpTBXPopupMenu
    Images = CE_Images.SmallIcons
    Left = 128
    Top = 64
  end
  object ApplicationEvents: TApplicationEvents
    Left = 160
    Top = 64
  end
  object TrayIcon: TJvTrayIcon
    IconIndex = 0
    Hint = 'CubicExplorer'
    PopupMenu = TrayPopupMenu
    Visibility = [tvVisibleTaskBar, tvVisibleTaskList]
    OnMouseUp = TrayIconMouseUp
    Left = 196
    Top = 64
  end
  object TrayPopupMenu: TSpTBXPopupMenu
    Left = 196
    Top = 96
    object SpTBXItem93: TSpTBXItem
      Action = CEActions.act_gen_showhide
    end
    object SpTBXSeparatorItem25: TSpTBXSeparatorItem
    end
    object SpTBXItem92: TSpTBXItem
      Action = CEActions.act_gen_exit
    end
  end
  object AutoUpdateTimer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = AutoUpdateTimerTimer
    Left = 48
    Top = 96
  end
end
