inherited CEOptionsPage_Tabs: TCEOptionsPage_Tabs
  object NewTabGroup: TTntGroupBox
    Left = 16
    Top = 16
    Width = 409
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Open new tab as'
    TabOrder = 0
    DesignSize = (
      409
      121)
    object radio_newtab_1: TTntRadioButton
      Left = 8
      Top = 24
      Width = 398
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Copy of active tab'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = radio_newtab_1Click
    end
    object radio_newtab_2: TTntRadioButton
      Left = 8
      Top = 47
      Width = 398
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Desktop'
      TabOrder = 1
      OnClick = radio_newtab_1Click
    end
    object radio_newtab_3: TTntRadioButton
      Left = 8
      Top = 70
      Width = 398
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Custom...'
      TabOrder = 2
      OnClick = radio_newtab_1Click
    end
    object edit_newtab: TTntEdit
      Left = 8
      Top = 91
      Width = 320
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 3
      OnChange = HandleChange
    end
    object but_newtab: TTntButton
      Left = 334
      Top = 91
      Width = 66
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Browse...'
      Enabled = False
      TabOrder = 4
      OnClick = but_newtabClick
    end
  end
  object check_newtab_switch: TTntCheckBox
    Left = 16
    Top = 152
    Width = 406
    Height = 17
    Caption = 'Switch to a new tab immediately'
    TabOrder = 1
    OnClick = HandleChange
  end
  object check_opentab_switch: TTntCheckBox
    Left = 16
    Top = 175
    Width = 406
    Height = 17
    Caption = 'When I open folder/file in a new tab, switch to it immediately '
    TabOrder = 2
    OnClick = HandleChange
  end
  object check_reusetabs_switch: TTntCheckBox
    Left = 16
    Top = 198
    Width = 409
    Height = 17
    Caption = 'Reuse already open tabs'
    TabOrder = 3
    OnClick = HandleChange
  end
  object check_nexttocur_switch: TTntCheckBox
    Left = 16
    Top = 221
    Width = 409
    Height = 17
    Caption = 'Open new tab next to current'
    TabOrder = 4
    OnClick = HandleChange
  end
  object check_autofit_switch: TTntCheckBox
    Left = 16
    Top = 244
    Width = 409
    Height = 17
    Caption = 'Auto fit tabs'
    TabOrder = 5
    OnClick = HandleChange
  end
  object check_dblclick_switch: TTntCheckBox
    Left = 16
    Top = 267
    Width = 409
    Height = 17
    Caption = 'Close tab on double click'
    TabOrder = 6
    OnClick = HandleChange
  end
  object check_exit: TTntCheckBox
    Left = 16
    Top = 290
    Width = 409
    Height = 17
    Caption = 'Exit program when last tab is closed'
    TabOrder = 7
    OnClick = HandleChange
  end
end
