inherited CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView
  object TntLabel1: TTntLabel
    Left = 16
    Top = 185
    Width = 76
    Height = 13
    Caption = 'File size format:'
  end
  object check_fullrowselect: TTntCheckBox
    Left = 16
    Top = 16
    Width = 401
    Height = 17
    Caption = 'Highlight row completely'
    TabOrder = 0
    OnClick = HandleChange
  end
  object check_selectprev: TTntCheckBox
    Left = 16
    Top = 39
    Width = 401
    Height = 17
    Caption = 'Select previous folder'
    TabOrder = 1
    OnClick = HandleChange
  end
  object check_autoselect: TTntCheckBox
    Left = 16
    Top = 62
    Width = 401
    Height = 17
    Caption = 'Select first item automatically'
    TabOrder = 2
    OnClick = HandleChange
  end
  object check_autosize_liststyle: TTntCheckBox
    Left = 16
    Top = 85
    Width = 401
    Height = 17
    Caption = 'Auto size cells in list view style.'
    TabOrder = 3
    OnClick = HandleChange
  end
  object check_sortfoldersfirst: TTntCheckBox
    Left = 16
    Top = 108
    Width = 401
    Height = 17
    Caption = 'Always sort folders first.'
    TabOrder = 4
    OnClick = HandleChange
  end
  object check_infotips: TTntCheckBox
    Left = 16
    Top = 131
    Width = 401
    Height = 17
    Caption = 'Show InfoTips'
    TabOrder = 5
    OnClick = HandleChange
  end
  object check_singleclick: TTntCheckBox
    Left = 16
    Top = 154
    Width = 401
    Height = 17
    Caption = 'Single click browsing'
    TabOrder = 6
    OnClick = HandleChange
  end
  object combo_sizeformat: TTntComboBox
    Left = 16
    Top = 200
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    OnChange = HandleChange
  end
end
