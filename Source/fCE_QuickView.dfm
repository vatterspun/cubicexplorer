object CEQuickView: TCEQuickView
  Left = 0
  Top = 0
  Width = 678
  Height = 461
  TabOrder = 0
  TabStop = True
  object Dock_Bottom: TSpTBXDock
    Left = 0
    Top = 409
    Width = 678
    Height = 52
    Position = dpBottom
    object toolbar_seekbar: TCEToolbar
      Left = 0
      Top = 0
      BorderStyle = bsNone
      ChevronMoveItems = False
      DockPos = 9
      DragHandleStyle = dhNone
      Images = CE_Images.QuickViewImages
      ParentShowHint = False
      PopupMenu = QuickViewPopup
      ShowHint = True
      Stretch = True
      TabOrder = 0
      Caption = 'Seekbar'
    end
    object toolbar_controls: TCEToolbar
      Left = 0
      Top = 26
      BorderStyle = bsNone
      ChevronMoveItems = False
      DockRow = 1
      DragHandleStyle = dhNone
      Images = CE_Images.QuickViewImages
      PopupMenu = QuickViewPopup
      Stretch = True
      TabOrder = 1
      Caption = 'Controls'
      ChevronVertical = False
    end
  end
  object panel_content: TPanel
    Left = 3
    Top = 0
    Width = 458
    Height = 409
    Align = alClient
    BevelOuter = bvNone
    Color = clAppWorkSpace
    TabOrder = 1
  end
  object tabs_playlist: TSpTBXTabControl
    Left = 464
    Top = 0
    Width = 214
    Height = 409
    Align = alRight
    ActiveTabIndex = 0
    TabAutofit = True
    TabPosition = ttpBottom
    OnActiveTabChange = tabs_playlistActiveTabChange
    HiddenItems = <>
    object tab_playlist: TSpTBXTabItem
      Caption = 'Playlist'
      Checked = True
      CustomWidth = 105
    end
    object tab_filelist: TSpTBXTabItem
      Caption = 'Filelist'
      CustomWidth = 105
    end
    object sheet_filelist: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 214
      Height = 384
      Caption = 'Filelist'
      ImageIndex = -1
      TabItem = 'tab_filelist'
    end
    object sheet_playlist: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 214
      Height = 384
      Caption = 'Playlist'
      ImageIndex = -1
      TabItem = 'tab_playlist'
    end
  end
  object splitter_left: TSpTBXSplitter
    Left = 0
    Top = 0
    Width = 3
    Height = 409
    Cursor = crSizeWE
    GripSize = 0
  end
  object splitter_right: TSpTBXSplitter
    Left = 461
    Top = 0
    Width = 3
    Height = 409
    Cursor = crSizeWE
    Align = alRight
    GripSize = 0
  end
  object QuickViewPopup: TSpTBXPopupMenu
    OnPopup = QuickViewPopupPopup
    Left = 160
    Top = 128
    object but_detach: TSpTBXItem
      Tag = 101
      Caption = 'Detach to Window'
      OnClick = HandlePopupItemClick
    end
    object but_ontop: TSpTBXItem
      Tag = 102
      Caption = 'Always On Top'
      OnClick = HandlePopupItemClick
    end
  end
end
