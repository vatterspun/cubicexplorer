inherited TCEOptionsPage_Hotkeys: TTCEOptionsPage_Hotkeys
  object TntBevel1: TTntBevel
    Left = 241
    Top = 12
    Width = 192
    Height = 25
    Shape = bsBottomLine
  end
  object TntLabel1: TTntLabel
    Left = 249
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Hotkeys'
  end
  object HotkeyList: TVirtualStringTree
    Left = 8
    Top = 12
    Width = 221
    Height = 313
    Align = alCustom
    DragType = dtVCL
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MaxHeight = 100
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowHint]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnFocusChanged = HotkeyListFocusChanged
    OnGetText = HotkeyListGetText
    OnPaintText = HotkeyListPaintText
    OnGetImageIndexEx = HotkeyListGetImageIndexEx
    OnKeyDown = HotkeyListKeyDown
    Columns = <
      item
        Position = 0
        Width = 142
        WideText = 'Name'
      end
      item
        Alignment = taRightJustify
        Position = 1
        Width = 75
        WideText = 'Shortcut'
      end>
  end
  object label_action_name: TSpTBXLabel
    Left = 241
    Top = 12
    Width = 192
    Height = 25
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Wrapping = twEndEllipsis
    Images = CE_Images.SmallIcons
  end
  object list_actionhotkeys: TListBox
    Left = 249
    Top = 63
    Width = 176
    Height = 106
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 2
    OnClick = list_actionhotkeysClick
    OnDragDrop = list_actionhotkeysDragDrop
    OnDragOver = list_actionhotkeysDragOver
    OnMouseDown = list_actionhotkeysMouseDown
  end
  object TntGroupBox1: TTntGroupBox
    Left = 249
    Top = 179
    Width = 176
    Height = 146
    Caption = 'Modify Hotkey'
    TabOrder = 3
    object edit_hotkey: TTntEdit
      Left = 12
      Top = 21
      Width = 153
      Height = 21
      AutoSelect = False
      TabOrder = 0
      OnKeyDown = edit_hotkeyKeyDown
      OnKeyPress = edit_hotkeyKeyPress
    end
    object but_add: TTntButton
      Left = 12
      Top = 48
      Width = 74
      Height = 25
      Caption = 'Add'
      Enabled = False
      TabOrder = 1
      OnClick = but_addClick
    end
    object but_replace: TTntButton
      Left = 92
      Top = 48
      Width = 73
      Height = 25
      Caption = 'Replace'
      Enabled = False
      TabOrder = 2
      OnClick = but_replaceClick
    end
    object but_delete: TTntButton
      Left = 12
      Top = 79
      Width = 153
      Height = 25
      Caption = 'Delete'
      Enabled = False
      TabOrder = 3
      OnClick = but_deleteClick
    end
    object but_reset: TTntButton
      Left = 12
      Top = 110
      Width = 153
      Height = 25
      Caption = 'Reset to default'
      Enabled = False
      TabOrder = 4
      OnClick = but_resetClick
    end
  end
end
