inherited CE_OptionsPage_Display_Bookmarks: TCE_OptionsPage_Display_Bookmarks
  object check_autoexpand: TTntCheckBox
    Left = 16
    Top = 16
    Width = 409
    Height = 17
    Caption = 'Auto expand tree'
    TabOrder = 0
    OnClick = HandleChange
  end
  object check_autocollapse: TTntCheckBox
    Left = 16
    Top = 39
    Width = 409
    Height = 17
    Caption = 'Auto collapse tree'
    TabOrder = 1
    OnClick = HandleChange
  end
  object check_singleclick: TTntCheckBox
    Left = 16
    Top = 62
    Width = 409
    Height = 17
    Caption = 'Single click mode'
    TabOrder = 2
    OnClick = HandleChange
  end
  object check_newtabdefault: TTntCheckBox
    Left = 16
    Top = 85
    Width = 409
    Height = 17
    Caption = 'Open in new tab by default'
    TabOrder = 3
    OnClick = HandleChange
  end
end
