inherited CEBookmarkPanel: TCEBookmarkPanel
  Caption = 'CEBookmarkPanel'
  ClientHeight = 297
  ClientWidth = 429
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 445
  ExplicitHeight = 331
  PixelsPerInch = 96
  TextHeight = 13
  inherited TopDock: TSpTBXDock
    Width = 429
    ExplicitWidth = 429
  end
  inherited BottomDock: TSpTBXDock
    Top = 288
    Width = 429
    ExplicitTop = 288
    ExplicitWidth = 429
  end
  object BookmarkPopupMenu: TSpTBXPopupMenu
    OnPopup = BookmarkPopupMenuPopup
    Left = 376
    Top = 84
    object but_open_new_tab: TSpTBXItem
      Tag = 11
      Caption = 'Open in new tab'
      OnClick = PopupMenuClick
    end
    object but_openAll: TSpTBXSubmenuItem
      Tag = 5
      Caption = 'Open All in Tabs'
      OnClick = PopupMenuClick
      DropdownCombo = True
      object SpTBXItem1: TSpTBXItem
        Tag = 6
        Caption = 'And Launch Files'
        OnClick = PopupMenuClick
      end
      object SpTBXItem2: TSpTBXItem
        Tag = 7
        Caption = 'Launch Files Only'
        OnClick = PopupMenuClick
      end
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object but_addCat: TSpTBXItem
      Tag = 1
      Caption = 'Add Category'
      OnClick = PopupMenuClick
    end
    object but_addBookmark: TSpTBXItem
      Tag = 2
      Caption = 'Add Bookmark'
      OnClick = PopupMenuClick
    end
    object but_addSession: TSpTBXItem
      Tag = 10
      Caption = 'Add Session'
      OnClick = PopupMenuClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object but_properties: TSpTBXItem
      Tag = 8
      Caption = 'Properties...'
      OnClick = PopupMenuClick
    end
    object but_rename: TSpTBXItem
      Tag = 3
      Caption = 'Rename'
      OnClick = PopupMenuClick
    end
    object but_delete: TSpTBXItem
      Tag = 4
      Caption = 'Delete'
      OnClick = PopupMenuClick
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object but_refresh: TSpTBXItem
      Tag = 9
      Caption = 'Refresh'
      OnClick = PopupMenuClick
    end
  end
end
