object CEActions: TCEActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 305
  Width = 385
  object ActionList: TTntActionList
    Images = CE_Images.SmallIcons
    Left = 28
    Top = 12
    object act_tabs_closetab: TTntAction
      Tag = 661
      Category = 'Tabs'
      Caption = 'Close Tab'
      Hint = 'Close Tab'
      ImageIndex = 10
      ShortCut = 16471
      SecondaryShortCuts.Strings = (
        'Ctrl+F4')
      OnExecute = ActionExecute
    end
    object act_gen_exit: TTntAction
      Tag = 100
      Category = 'General'
      Caption = 'Exit'
      ImageIndex = 0
      ShortCut = 32883
      OnExecute = ActionExecute
    end
    object act_view_dropstack: TTntAction
      Tag = 305
      Category = 'View'
      Caption = 'Stack'
      ImageIndex = 43
      ShortCut = 16437
      OnExecute = ActionExecute
    end
    object act_help_donate: TTntAction
      Tag = 505
      Category = 'Help'
      Caption = 'Donate'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_navi_forward: TCEToolbarAction
      Tag = 604
      Category = 'Navigation'
      Caption = 'Forward'
      ImageIndex = 6
      ShortCut = 32807
      OnExecute = ActionExecute
    end
    object act_navi_back: TCEToolbarAction
      Tag = 603
      Category = 'Navigation'
      Caption = 'Back'
      ImageIndex = 5
      ShortCut = 32805
      OnExecute = ActionExecute
    end
    object act_navi_folderup: TTntAction
      Tag = 605
      Category = 'Navigation'
      Caption = 'Folder Up'
      ImageIndex = 7
      OnExecute = ActionExecute
    end
    object act_navi_refresh: TTntAction
      Tag = 606
      Category = 'Navigation'
      Caption = 'Refresh'
      ImageIndex = 8
      ShortCut = 116
      OnExecute = ActionExecute
    end
    object act_view_folders: TTntAction
      Tag = 301
      Category = 'View'
      Caption = 'Folders'
      ImageIndex = 28
      ShortCut = 16433
      OnExecute = ActionExecute
    end
    object act_edit_undo_delete: TCEToolbarAction
      Tag = 215
      Category = 'Edit'
      Caption = 'Undo Delete'
      ImageIndex = 55
      OnExecute = ActionExecute
    end
    object act_view_bookmark: TTntAction
      Tag = 302
      Category = 'View'
      Caption = 'Bookmarks'
      ImageIndex = 18
      ShortCut = 16434
      OnExecute = ActionExecute
    end
    object act_view_large: TTntAction
      Tag = 351
      Category = 'View'
      Caption = 'Large Icons'
      ImageIndex = 11
      OnExecute = ActionExecute
    end
    object act_view_small: TTntAction
      Tag = 352
      Category = 'View'
      Caption = 'Small Icons'
      ImageIndex = 12
      OnExecute = ActionExecute
    end
    object act_view_list: TTntAction
      Tag = 353
      Category = 'View'
      Caption = 'List'
      ImageIndex = 13
      OnExecute = ActionExecute
    end
    object act_view_showheaderalways: TTntAction
      Tag = 333
      Category = 'View'
      Caption = 'Always Show Sort Columns'
      OnExecute = ActionExecute
    end
    object act_view_filters: TTntAction
      Tag = 304
      Category = 'View'
      Caption = 'Filters'
      ImageIndex = 29
      ShortCut = 16436
      OnExecute = ActionExecute
    end
    object act_view_details: TTntAction
      Tag = 354
      Category = 'View'
      Caption = 'Details'
      ImageIndex = 14
      OnExecute = ActionExecute
    end
    object act_view_thumbs: TTntAction
      Tag = 355
      Category = 'View'
      Caption = 'Thumbnails'
      ImageIndex = 16
      OnExecute = ActionExecute
    end
    object act_view_loadskin: TTntAction
      Tag = 371
      Category = 'View'
      Caption = 'Load theme from file...'
      Hint = 'Load theme from file'
      OnExecute = ActionExecute
    end
    object act_view_tiles: TTntAction
      Tag = 356
      Category = 'View'
      Caption = 'Tiles'
      ImageIndex = 15
      OnExecute = ActionExecute
    end
    object act_view_alwaysontop: TTntAction
      Tag = 335
      Category = 'View'
      Caption = 'Always On Top'
      ImageIndex = 60
      OnExecute = ActionExecute
    end
    object act_view_groupby: TCEToolbarAction
      Tag = 374
      Category = 'View'
      Caption = 'Group By'
      ImageIndex = 31
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_viewstyle: TCEToolbarAction
      Tag = 373
      Category = 'View'
      Caption = 'View Style'
      ImageIndex = 16
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_filmstrip: TTntAction
      Tag = 358
      Category = 'View'
      Caption = 'Filmstrip'
      ImageIndex = 17
      OnExecute = ActionExecute
    end
    object act_view_checkbox_selection: TTntAction
      Tag = 337
      Category = 'View'
      Caption = 'Checkbox Selection'
      ImageIndex = 56
      OnExecute = ActionExecute
    end
    object act_edit_copy: TTntAction
      Tag = 201
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 1
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_edit_cut: TTntAction
      Tag = 202
      Category = 'Edit'
      Caption = 'Cut'
      ImageIndex = 2
      ShortCut = 16472
      OnExecute = ActionExecute
    end
    object act_edit_paste: TTntAction
      Tag = 203
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 3
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      OnExecute = ActionExecute
    end
    object act_edit_delete: TTntAction
      Tag = 204
      Category = 'Edit'
      Caption = 'Delete'
      ImageIndex = 4
      ShortCut = 46
      OnExecute = ActionExecute
    end
    object act_edit_selall: TTntAction
      Tag = 205
      Category = 'Edit'
      Caption = 'Select All'
      ImageIndex = 57
      ShortCut = 16449
      OnExecute = ActionExecute
    end
    object act_edit_invertsel: TTntAction
      Tag = 206
      Category = 'Edit'
      Caption = 'Invert Selection'
      ImageIndex = 58
      ShortCut = 24641
      OnExecute = ActionExecute
    end
    object act_edit_properties: TTntAction
      Tag = 207
      Category = 'Edit'
      Caption = 'Properties...'
      ImageIndex = 26
      ShortCut = 32781
      OnExecute = ActionExecute
    end
    object act_edit_rename: TTntAction
      Tag = 208
      Category = 'Edit'
      Caption = 'Rename'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = ActionExecute
    end
    object act_quick_none: TTntAction
      Tag = 701
      Category = 'Quickview'
      Caption = 'None'
      OnExecute = ActionExecute
    end
    object act_quick_auto: TTntAction
      Tag = 702
      Category = 'Quickview'
      Caption = 'Auto'
      OnExecute = ActionExecute
    end
    object act_quick_text: TTntAction
      Tag = 703
      Category = 'Quickview'
      Caption = 'Text'
      OnExecute = ActionExecute
    end
    object act_quick_image: TTntAction
      Tag = 704
      Category = 'Quickview'
      Caption = 'Image'
      OnExecute = ActionExecute
    end
    object act_quick_hex: TTntAction
      Tag = 705
      Category = 'Quickview'
      Caption = 'Hex'
      OnExecute = ActionExecute
    end
    object act_help_home: TTntAction
      Tag = 501
      Category = 'Help'
      Caption = 'CubicExplorer Home Page'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_help_forum: TTntAction
      Tag = 502
      Category = 'Help'
      Caption = 'Support Forum'
      ImageIndex = 47
      OnExecute = ActionExecute
    end
    object act_help_about: TTntAction
      Tag = 503
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 48
      OnExecute = ActionExecute
    end
    object act_view_quickview: TTntAction
      Tag = 303
      Category = 'View'
      Caption = 'Quickview'
      ImageIndex = 20
      ShortCut = 16435
      OnExecute = ActionExecute
    end
    object act_edit_duplicate: TTntAction
      Tag = 209
      Category = 'Edit'
      Caption = 'Duplicate'
      ImageIndex = 37
      ShortCut = 16452
      OnExecute = ActionExecute
    end
    object act_navi_texteditor: TTntAction
      Tag = 650
      Category = 'Navigation'
      Caption = 'Text Editor'
      ImageIndex = 21
      ShortCut = 118
      OnExecute = ActionExecute
    end
    object act_navi_filesearch: TTntAction
      Tag = 651
      Category = 'Navigation'
      Caption = 'File Search'
      ImageIndex = 22
      ShortCut = 16454
      OnExecute = ActionExecute
    end
    object act_tools_mapdrive: TTntAction
      Tag = 451
      Category = 'Tools'
      Caption = 'Map Network Drive...'
      OnExecute = ActionExecute
    end
    object act_tools_disconnectdrive: TTntAction
      Tag = 452
      Category = 'Tools'
      Caption = 'Disconnect Network Drive...'
      OnExecute = ActionExecute
    end
    object act_edit_newfile: TCEToolbarAction
      Tag = 210
      Category = 'Edit'
      Caption = 'New'
      ImageIndex = 36
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_edit_copypath: TCEToolbarAction
      Tag = 211
      Category = 'Edit'
      Caption = 'Copy Path'
      ImageIndex = 50
      ShortCut = 16466
      OnExecute = ActionExecute
    end
    object act_view_showhints: TTntAction
      Tag = 330
      Category = 'View'
      Caption = 'Show Hints'
      OnExecute = ActionExecute
    end
    object act_tools_showcustomizer: TTntAction
      Tag = 401
      Category = 'Tools'
      Caption = 'Customizer...'
      ImageIndex = 40
      OnExecute = ActionExecute
    end
    object act_view_hiddenfiles: TTntAction
      Tag = 332
      Category = 'View'
      Caption = 'Show Hidden Files'
      ImageIndex = 53
      OnExecute = ActionExecute
    end
    object act_edit_newfolder: TTntAction
      Tag = 212
      Category = 'Edit'
      Caption = 'New Folder'
      ImageIndex = 25
      ShortCut = 16462
      OnExecute = ActionExecute
    end
    object act_edit_paste_shortcut: TTntAction
      Tag = 213
      Category = 'Edit'
      Caption = 'Paste Shortcut'
      OnExecute = ActionExecute
    end
    object act_tools_cmd: TTntAction
      Tag = 454
      Category = 'Tools'
      Caption = 'Open Command Prompt'
      ImageIndex = 27
      OnExecute = ActionExecute
    end
    object act_view_statusbar: TTntAction
      Tag = 300
      Category = 'View'
      Caption = 'Status Bar'
      OnExecute = ActionExecute
    end
    object act_view_fullscreen: TTntAction
      Tag = 370
      Category = 'View'
      Caption = 'Fullscreen'
      ImageIndex = 51
      ShortCut = 122
      OnExecute = ActionExecute
    end
    object act_view_showextensions: TTntAction
      Tag = 334
      Category = 'View'
      Caption = 'Show Extensions'
      ImageIndex = 54
      OnExecute = ActionExecute
    end
    object act_help_poedit_form: TTntAction
      Tag = 504
      Category = 'Help'
      Caption = 'Translate CubicExplorer'
      ImageIndex = 52
      OnExecute = ActionExecute
    end
    object act_tools_showoptions: TTntAction
      Tag = 402
      Category = 'Tools'
      Caption = 'Options...'
      ImageIndex = 39
      OnExecute = ActionExecute
    end
    object act_sessions_save: TTntAction
      Tag = 851
      Category = 'Sessions'
      Caption = 'Save...'
      Hint = 'Save active session.'
      OnExecute = ActionExecute
    end
    object act_sessions_manage: TTntAction
      Tag = 852
      Category = 'Sessions'
      Caption = 'Manage...'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_view_arrangeby: TCEToolbarAction
      Tag = 372
      Category = 'View'
      Caption = 'Arrange By'
      ImageIndex = 30
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_navi_scrollleft: TTntAction
      Tag = 608
      Category = 'Navigation'
      Caption = 'Scroll Left'
      OnExecute = ActionExecute
    end
    object act_navi_scrollright: TTntAction
      Tag = 609
      Category = 'Navigation'
      Caption = 'Scroll Right'
      OnExecute = ActionExecute
    end
    object act_tabs_closeothertabs: TTntAction
      Tag = 662
      Category = 'Tabs'
      Caption = 'Close Other Tabs'
      Hint = 'Close Other Tabs'
      ImageIndex = 34
      OnExecute = ActionExecute
    end
    object act_tabs_addtab: TTntAction
      Tag = 663
      Category = 'Tabs'
      Caption = 'Add Tab'
      Hint = 'Add Tab'
      ImageIndex = 9
      ShortCut = 16468
      OnExecute = ActionExecute
    end
    object act_tabs_duplicatetab: TTntAction
      Tag = 664
      Category = 'Tabs'
      Caption = 'Duplicate Tab'
      Hint = 'Duplicate Tab'
      ImageIndex = 32
      OnExecute = ActionExecute
    end
    object act_tabs_closeonleft: TTntAction
      Tag = 665
      Category = 'Tabs'
      Caption = 'Close Tabs on Left'
      Hint = 'Close Tabs on Left'
      ImageIndex = 35
      OnExecute = ActionExecute
    end
    object act_tabs_closeonright: TTntAction
      Tag = 666
      Category = 'Tabs'
      Caption = 'Close Tabs on Right'
      Hint = 'Close Tabs on Right'
      ImageIndex = 33
      OnExecute = ActionExecute
    end
    object act_gen_menu: TCEToolbarAction
      Tag = -1
      Category = 'General'
      Caption = 'Menu'
      ImageIndex = 49
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_navi_quickview: TTntAction
      Tag = 652
      Category = 'Navigation'
      Caption = 'QuickView'
      ImageIndex = 20
      ShortCut = 119
      OnExecute = ActionExecute
    end
    object act_sessions_addhistoryitem: TTntAction
      Tag = 853
      Category = 'Sessions'
      Caption = 'Add History Item'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_sessions_clearhistory: TTntAction
      Tag = 854
      Category = 'Sessions'
      Caption = 'Clear History'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_bookmarks_menu: TCEToolbarAction
      Tag = -1
      Category = 'Bookmarks'
      Caption = 'Bookmarks'
      ImageIndex = 18
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_sessions_menu: TCEToolbarAction
      Tag = -1
      Category = 'Sessions'
      Caption = 'Sessions'
      ImageIndex = 41
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_infobar: TTntAction
      Tag = 336
      Category = 'View'
      Caption = 'Info Bar'
      ImageIndex = 66
      OnExecute = ActionExecute
    end
    object act_edit_create_symlink: TTntAction
      Tag = 214
      Category = 'Edit'
      Caption = 'Create Symbolic Link'
      OnExecute = ActionExecute
    end
    object act_sessions_enablehistory: TTntAction
      Tag = 855
      Category = 'Sessions'
      Caption = 'Auto Save History'
      Hint = 'Show Session Manager'
      OnExecute = ActionExecute
    end
    object act_gen_showhide: TTntAction
      Tag = 101
      Category = 'General'
      Caption = 'Show/Hide'
      OnExecute = ActionExecute
    end
    object act_tabs_undo: TCEToolbarAction
      Tag = 667
      Category = 'Tabs'
      Caption = 'Undo Tab Close'
      Hint = 'Undo Tab Close'
      ImageIndex = 42
      ShortCut = 24660
      OnExecute = ActionExecute
    end
    object act_tabs_next: TTntAction
      Tag = 668
      Category = 'Tabs'
      Caption = 'Switch to Next Tab'
      ShortCut = 16393
      SecondaryShortCuts.Strings = (
        'Ctrl+PgDn')
      OnExecute = ActionExecute
    end
    object act_tabs_prev: TTntAction
      Tag = 669
      Category = 'Tabs'
      Caption = 'Swith to Previous Tab'
      ShortCut = 24585
      SecondaryShortCuts.Strings = (
        'Ctrl+PgUp')
      OnExecute = ActionExecute
    end
    object act_focus_addressbar: TTntAction
      Tag = 951
      Category = 'Focus'
      Caption = 'Set focus to address bar'
      ShortCut = 32836
      SecondaryShortCuts.Strings = (
        'F4')
      OnExecute = ActionExecute
    end
    object act_filters_menu: TCEToolbarAction
      Tag = 375
      Category = 'Filters'
      Caption = 'Filters'
      ImageIndex = 29
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_filters_pattern: TCEToolbarAction
      Category = 'Filters'
      Caption = 'Text Filter'
      ImageIndex = 29
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_filters_clear: TTntAction
      Tag = 901
      Category = 'Filters'
      Caption = 'Clear Filters'
      ImageIndex = 44
      OnExecute = ActionExecute
    end
    object act_help_versionmgr: TTntAction
      Tag = 506
      Category = 'Help'
      Caption = 'Version Manager'
      ImageIndex = 46
      OnExecute = ActionExecute
    end
    object act_view_archiver: TTntAction
      Tag = 306
      Category = 'View'
      Caption = 'Archiver'
      Visible = False
      OnExecute = ActionExecute
    end
    object act_gen_new_instance: TTntAction
      Tag = 103
      Category = 'General'
      Caption = 'New Window'
      ImageIndex = 61
      OnExecute = ActionExecute
    end
    object act_help_checkupdates: TTntAction
      Tag = 507
      Category = 'Help'
      Caption = 'Check For Updates'
      ImageIndex = 45
      OnExecute = ActionExecute
    end
    object act_tools_emptytrash: TTntAction
      Tag = 453
      Category = 'Tools'
      Caption = 'Empty Recycle Bin'
      ImageIndex = 24
      OnExecute = ActionExecute
    end
    object act_tools_systempower: TCEToolbarAction
      Tag = 455
      Category = 'Tools'
      Caption = 'System Power'
      Hint = 'System Power'
      ImageIndex = 59
      OnExecute = ActionExecute
      CanExecute = False
    end
    object act_view_lock_toolbars: TTntAction
      Tag = 390
      Category = 'View'
      Caption = 'Lock Toolbars'
      OnExecute = ActionExecute
    end
    object act_navi_refresh_current: TTntAction
      Tag = 610
      Category = 'Navigation'
      Caption = 'Refresh Fileview'
      ImageIndex = 62
      ShortCut = 8308
      OnExecute = ActionExecute
    end
    object act_filters_strict: TTntAction
      Tag = 902
      Category = 'Filters'
      Caption = 'Strict'
      Hint = 'Use strict filtering (wildcards are ? and *)'
      ImageIndex = 63
      OnExecute = ActionExecute
    end
    object act_filters_exclude: TTntAction
      Tag = 903
      Category = 'Filters'
      Caption = 'Exclude'
      Hint = 'Exclude'
      ImageIndex = 64
      OnExecute = ActionExecute
    end
    object act_help_restore_layout: TTntAction
      Tag = 508
      Category = 'Help'
      Caption = 'Restore Default Layout'
      OnExecute = ActionExecute
    end
    object act_tabs_menu: TCEToolbarAction
      Tag = 670
      Category = 'Tabs'
      Caption = 'Tabs'
      ImageIndex = 67
      OnExecute = ActionExecute
    end
    object act_edit_newemptyfile: TTntAction
      Tag = 217
      Category = 'Edit'
      Caption = 'New File'
      ImageIndex = 36
      ShortCut = 24654
      OnExecute = ActionExecute
    end
    object act_stack_open: TCEToolbarAction
      Tag = 921
      Category = 'Stack'
      Caption = 'Open'
      ImageIndex = 68
      OnExecute = ActionExecute
    end
    object act_stack_save: TCEToolbarAction
      Tag = 922
      Category = 'Stack'
      Caption = 'Save'
      ImageIndex = 69
      OnExecute = ActionExecute
    end
    object act_stack_remove: TTntAction
      Tag = 923
      Category = 'Stack'
      Caption = 'Remove from Stack'
      ImageIndex = 4
      OnExecute = ActionExecute
    end
    object act_stack_clear: TTntAction
      Tag = 924
      Category = 'Stack'
      Caption = 'Clear List'
      ImageIndex = 44
      OnExecute = ActionExecute
    end
    object act_stack_allowmove: TTntAction
      Tag = 925
      Category = 'Stack'
      Caption = 'Allow Move'
      ImageIndex = 70
      OnExecute = ActionExecute
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = UpdateTimerTimer
    Left = 92
    Top = 12
  end
  object ApplicationEvents: TApplicationEvents
    OnActivate = ApplicationEventsActivate
    OnMessage = ApplicationEventsMessage
    Left = 180
    Top = 12
  end
  object BackgroundCMItems_up: TTntPopupMenu
    OnPopup = BackgroundCMItems_upPopup
    Left = 64
    Top = 104
    object View1: TTntMenuItem
      Caption = 'View'
      object LargeIcons1: TTntMenuItem
        Action = act_view_large
        RadioItem = True
      end
      object SmallIcons1: TTntMenuItem
        Action = act_view_small
        RadioItem = True
      end
      object List1: TTntMenuItem
        Action = act_view_list
        RadioItem = True
      end
      object Details1: TTntMenuItem
        Action = act_view_details
        RadioItem = True
      end
      object iles1: TTntMenuItem
        Action = act_view_tiles
        RadioItem = True
      end
      object humbnails1: TTntMenuItem
        Action = act_view_thumbs
        RadioItem = True
      end
      object Filmstrip1: TTntMenuItem
        Action = act_view_filmstrip
        RadioItem = True
      end
    end
    object N1: TTntMenuItem
      Caption = '-'
    end
    object MenuItem_ArragneBy: TTntMenuItem
      Caption = 'Arrange By'
    end
    object MenuItem_GroupBy: TTntMenuItem
      Caption = 'Group By'
    end
    object Refresh1: TTntMenuItem
      Action = act_navi_refresh
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object Paste1: TTntMenuItem
      Action = act_edit_paste
    end
    object PasteShortcut1: TTntMenuItem
      Action = act_edit_paste_shortcut
    end
    object CopyPath1: TTntMenuItem
      Action = act_edit_copypath
    end
    object N4: TTntMenuItem
      Caption = '-'
    end
    object CreateSymbolicLink1: TTntMenuItem
      Action = act_edit_create_symlink
    end
    object N5: TTntMenuItem
      Caption = '-'
    end
  end
  object BackgroundCMItems_down: TTntPopupMenu
    Left = 64
    Top = 168
    object NewFolder1: TTntMenuItem
      Action = act_edit_newfolder
    end
    object N3: TTntMenuItem
      Caption = '-'
    end
    object Properties1: TTntMenuItem
      Action = act_edit_properties
    end
  end
end
