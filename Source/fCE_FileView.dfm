inherited CEFileViewPage: TCEFileViewPage
  Width = 586
  OnClick = View
  ExplicitWidth = 586
  object QuickViewSplitter: TSpTBXSplitter
    Left = 0
    Top = 0
    Height = 424
    Cursor = crSizeWE
    Visible = False
    ResizeStyle = rsPattern
    OnMoved = QuickViewSplitterMoved
  end
  object QuickViewPopupMenu: TSpTBXPopupMenu
    OnPopup = QuickViewPopupMenuPopup
    Left = 440
    Top = 116
    object SpTBXSubmenuItem1: TSpTBXSubmenuItem
      Caption = 'Thumbs Position'
      object but_thumbpos_left: TSpTBXItem
        Caption = 'Left'
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = ThumbPositionClick
      end
      object but_thumbpos_top: TSpTBXItem
        Tag = 1
        Caption = 'Top'
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = ThumbPositionClick
      end
      object but_thumbpos_right: TSpTBXItem
        Tag = 2
        Caption = 'Right'
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = ThumbPositionClick
      end
      object but_thumbpos_bottom: TSpTBXItem
        Tag = 3
        Caption = 'Bottom'
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = ThumbPositionClick
      end
    end
    object SpTBXSubmenuItem2: TSpTBXSubmenuItem
      Caption = 'Thumbs View Style'
      object but_thumbstyle_icon: TSpTBXItem
        Tag = 1
        Caption = 'Large Icons'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_smallicon: TSpTBXItem
        Tag = 2
        Caption = 'Small Icons'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_list: TSpTBXItem
        Tag = 3
        Caption = 'List'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_details: TSpTBXItem
        Tag = 4
        Caption = 'Details'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_tiles: TSpTBXItem
        Tag = 5
        Caption = 'Tiles'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_thumbnails: TSpTBXItem
        Tag = 6
        Caption = 'Thumbnails'
        AutoCheck = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
      object but_thumbstyle_filmstrip: TSpTBXItem
        Tag = 7
        Caption = 'Filmstrip'
        AutoCheck = True
        Checked = True
        GroupIndex = 1
        OnClick = ThumbViewStyleClick
      end
    end
  end
end
