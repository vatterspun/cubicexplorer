inherited CESearchPage: TCESearchPage
  object ResultView: TVirtualMultiPathExplorerEasyListview
    Left = 0
    Top = 197
    Width = 584
    Height = 205
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    CompressedFile.Color = clBlue
    CompressedFile.Font.Charset = DEFAULT_CHARSET
    CompressedFile.Font.Color = clWindowText
    CompressedFile.Font.Height = -11
    CompressedFile.Font.Name = 'Tahoma'
    CompressedFile.Font.Style = []
    DefaultSortColumn = 0
    EditManager.Enabled = True
    EditManager.Font.Charset = DEFAULT_CHARSET
    EditManager.Font.Color = clWindowText
    EditManager.Font.Height = -11
    EditManager.Font.Name = 'Tahoma'
    EditManager.Font.Style = []
    EncryptedFile.Color = clGreen
    EncryptedFile.Font.Charset = DEFAULT_CHARSET
    EncryptedFile.Font.Color = clWindowText
    EncryptedFile.Font.Height = -11
    EncryptedFile.Font.Name = 'Tahoma'
    EncryptedFile.Font.Style = []
    FileSizeFormat = vfsfDefault
    DragManager.Enabled = True
    Grouped = False
    GroupingColumn = 0
    Header.Visible = True
    Options = [eloExecuteOnDblClick, eloThreadedImages, eloThreadedDetails, eloQueryInfoHints, eloShellContextMenus, eloGhostHiddenFiles]
    PaintInfoGroup.MarginBottom.CaptionIndent = 4
    PaintInfoGroup.MarginTop.Visible = False
    ParentShowHint = False
    ShowHint = True
    ShowThemedBorder = False
    Sort.Algorithm = esaQuickSort
    Sort.AutoSort = True
    Selection.EnableDragSelect = True
    Selection.MouseButton = [cmbLeft, cmbRight]
    Selection.MultiSelect = True
    Selection.RectSelect = True
    TabOrder = 0
    ThumbsManager.StorageFilename = 'Thumbnails.album'
    View = elsReport
    OnColumnCustomView = ResultViewColumnCustomView
    OnContextMenuCmd = ResultViewContextMenuCmd
    OnContextMenuShow = ResultViewContextMenuShow
    OnHintCustomInfo = ResultViewHintCustomInfo
    OnItemContextMenu = ResultViewItemContextMenu
    OnMouseDown = ResultViewMouseDown
    OnMouseUp = ResultViewMouseUp
  end
  object SearchPanel: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 197
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      584
      197)
    object group_searchbuttons: TSpTBXGroupBox
      Left = 449
      Top = 6
      Width = 127
      Height = 134
      Anchors = [akTop, akRight]
      TabOrder = 0
      TBXStyleBackground = True
      object but_search_start: TSpTBXButton
        Left = 15
        Top = 17
        Width = 97
        Height = 28
        Caption = 'Search'
        TabOrder = 0
        OnClick = but_search_startClick
        Default = True
        DrawPushedCaption = False
      end
      object but_search_stop: TSpTBXButton
        Left = 15
        Top = 58
        Width = 97
        Height = 28
        Caption = 'Stop'
        TabOrder = 1
        OnClick = but_search_stopClick
        DrawPushedCaption = False
      end
      object check_clear_before: TSpTBXCheckBox
        Left = 17
        Top = 94
        Width = 87
        Height = 21
        Caption = 'Clear Results'
        TabOrder = 2
        Checked = True
        State = cbChecked
      end
    end
    object CriteriaTabControl: TSpTBXTabControl
      Left = 10
      Top = 6
      Width = 433
      Height = 185
      Anchors = [akLeft, akTop, akRight]
      ActiveTabIndex = 0
      TabAutofit = True
      TabBackgroundColor = clBtnFace
      TabPosition = ttpBottom
      HiddenItems = <>
      object SpTBXTabItem1: TSpTBXTabItem
        Caption = 'Name && Location'
        Checked = True
        CustomWidth = 85
      end
      object SpTBXTabItem2: TSpTBXTabItem
        Caption = 'Date && Time'
        CustomWidth = 85
      end
      object SpTBXTabItem3: TSpTBXTabItem
        Caption = 'Size && Attributes'
        CustomWidth = 85
      end
      object SpTBXTabItem4: TSpTBXTabItem
        Caption = 'Content'
        CustomWidth = 85
      end
      object SpTBXTabItem8: TSpTBXTabItem
        Caption = 'Filters'
        CustomWidth = 85
      end
      object sheet_filters: TSpTBXTabSheet
        Left = 0
        Top = 0
        Width = 433
        Height = 160
        Caption = 'Filters'
        ImageIndex = -1
        DesignSize = (
          433
          160)
        TabItem = 'SpTBXTabItem8'
        object SpTBXLabel1: TSpTBXLabel
          Left = 12
          Top = 9
          Width = 98
          Height = 19
          Caption = 'Exclude File Masks:'
        end
        object memo_filters_exclude: TTntMemo
          Left = 12
          Top = 30
          Width = 199
          Height = 108
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
          WordWrap = False
        end
        object memo_filters_include: TTntMemo
          Left = 217
          Top = 30
          Width = 204
          Height = 108
          Anchors = [akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 2
          WordWrap = False
        end
        object SpTBXLabel3: TSpTBXLabel
          Left = 217
          Top = 9
          Width = 96
          Height = 19
          Caption = 'Include File Masks:'
          Anchors = [akTop, akRight]
        end
        object SpTBXLabel5: TSpTBXLabel
          Left = 12
          Top = 138
          Width = 195
          Height = 19
          Caption = 'Enter each file mask on a separate line.'
        end
      end
      object sheet_size_attributes: TSpTBXTabSheet
        Left = 0
        Top = 0
        Width = 433
        Height = 160
        Caption = 'Size && Attributes'
        ImageIndex = -1
        DesignSize = (
          433
          160)
        TabItem = 'SpTBXTabItem3'
        object group_size: TSpTBXGroupBox
          Left = 12
          Top = 10
          Width = 182
          Height = 143
          Caption = 'Size'
          Anchors = [akLeft, akTop, akBottom]
          TabOrder = 0
          object spin_size_atleast: TSpTBXSpinEdit
            Left = 12
            Top = 45
            Width = 93
            Height = 21
            Enabled = False
            TabOrder = 0
            SpinButton.Left = 75
            SpinButton.Top = 0
            SpinButton.Width = 14
            SpinButton.Height = 17
            SpinButton.Align = alRight
            SpinButton.Enabled = False
            SpinButton.DrawPushedCaption = False
          end
          object combo_size_atleast: TSpTBXComboBox
            Left = 111
            Top = 45
            Width = 58
            Height = 21
            Enabled = False
            ItemHeight = 13
            ItemIndex = 1
            TabOrder = 1
            Text = 'KB'
            Items.Strings = (
              'Bytes'
              'KB'
              'MB'
              'GB')
          end
          object check_size_atleast: TSpTBXCheckBox
            Left = 12
            Top = 21
            Width = 68
            Height = 21
            Caption = 'At Least:'
            TabOrder = 2
            OnClick = check_size_Click
          end
          object spin_size_atmost: TSpTBXSpinEdit
            Left = 12
            Top = 103
            Width = 93
            Height = 21
            Enabled = False
            TabOrder = 3
            SpinButton.Left = 75
            SpinButton.Top = 0
            SpinButton.Width = 14
            SpinButton.Height = 17
            SpinButton.Align = alRight
            SpinButton.Enabled = False
            SpinButton.DrawPushedCaption = False
          end
          object combo_size_atmost: TSpTBXComboBox
            Left = 111
            Top = 103
            Width = 58
            Height = 21
            Enabled = False
            ItemHeight = 13
            ItemIndex = 1
            TabOrder = 4
            Text = 'KB'
            Items.Strings = (
              'Bytes'
              'KB'
              'MB'
              'GB')
          end
          object check_size_atmost: TSpTBXCheckBox
            Left = 12
            Top = 79
            Width = 65
            Height = 21
            Caption = 'At Most:'
            TabOrder = 5
            OnClick = check_size_Click
          end
        end
        object group_attributes: TSpTBXGroupBox
          Left = 206
          Top = 10
          Width = 215
          Height = 143
          Caption = 'Attributes'
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 1
          object check_attr_readonly: TSpTBXCheckBox
            Left = 12
            Top = 20
            Width = 74
            Height = 21
            Caption = 'Read Only'
            TabOrder = 0
            AllowGrayed = True
            State = cbGrayed
          end
          object check_attr_hidden: TSpTBXCheckBox
            Left = 12
            Top = 47
            Width = 57
            Height = 21
            Caption = 'Hidden'
            TabOrder = 1
            AllowGrayed = True
            State = cbGrayed
          end
          object check_attr_compressed: TSpTBXCheckBox
            Left = 12
            Top = 101
            Width = 83
            Height = 21
            Caption = 'Compressed'
            TabOrder = 2
            AllowGrayed = True
            State = cbGrayed
          end
          object check_attr_system: TSpTBXCheckBox
            Left = 12
            Top = 74
            Width = 59
            Height = 21
            Caption = 'System'
            TabOrder = 3
            AllowGrayed = True
            State = cbGrayed
          end
          object check_attr_folder: TSpTBXCheckBox
            Left = 105
            Top = 20
            Width = 54
            Height = 21
            Caption = 'Folder'
            TabOrder = 4
            AllowGrayed = True
            State = cbGrayed
          end
        end
      end
      object sheet_date_time: TSpTBXTabSheet
        Left = 0
        Top = 0
        Width = 433
        Height = 160
        Caption = 'Date && Time'
        ImageIndex = -1
        DesignSize = (
          433
          160)
        TabItem = 'SpTBXTabItem2'
        object TabControl_DateTime: TSpTBXTabControl
          Left = 12
          Top = 11
          Width = 409
          Height = 142
          Anchors = [akLeft, akTop, akRight, akBottom]
          ActiveTabIndex = 0
          TabBackgroundBorders = True
          HiddenItems = <>
          object SpTBXTabItem5: TSpTBXTabItem
            Caption = 'Created'
            Checked = True
          end
          object SpTBXTabItem6: TSpTBXTabItem
            Caption = 'Modified'
          end
          object SpTBXTabItem7: TSpTBXTabItem
            Caption = 'Accessed'
          end
          object sheet_accessed: TSpTBXTabSheet
            Left = 0
            Top = 25
            Width = 409
            Height = 117
            Caption = 'Accessed'
            ImageIndex = -1
            TabItem = 'SpTBXTabItem7'
            object AccessedBeforeTime: TDateTimePicker
              Left = 12
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 0
            end
            object AccessedBeforeDate: TDateTimePicker
              Left = 12
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 1
            end
            object AccessedAfterDate: TDateTimePicker
              Left = 136
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 2
            end
            object AccessedAfterTime: TDateTimePicker
              Left = 136
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 3
            end
            object check_AccessedBeforeDate: TSpTBXCheckBox
              Left = 12
              Top = 7
              Width = 86
              Height = 21
              Caption = 'Before Date:'
              TabOrder = 4
              OnClick = check_dateClick
            end
            object check_AccessedBeforeTime: TSpTBXCheckBox
              Left = 12
              Top = 59
              Width = 85
              Height = 21
              Caption = 'Before Time:'
              TabOrder = 5
              OnClick = check_dateClick
            end
            object check_AccessedAfterDate: TSpTBXCheckBox
              Left = 136
              Top = 7
              Width = 79
              Height = 21
              Caption = 'After Date:'
              TabOrder = 6
              OnClick = check_dateClick
            end
            object check_AccessedAfterTime: TSpTBXCheckBox
              Left = 136
              Top = 59
              Width = 78
              Height = 21
              Caption = 'After Time:'
              TabOrder = 7
              OnClick = check_dateClick
            end
          end
          object sheet_modified: TSpTBXTabSheet
            Left = 0
            Top = 25
            Width = 409
            Height = 117
            Caption = 'Modified'
            ImageIndex = -1
            TabItem = 'SpTBXTabItem6'
            object ModifiedBeforeTime: TDateTimePicker
              Left = 12
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 0
            end
            object ModifiedBeforeDate: TDateTimePicker
              Left = 12
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 1
            end
            object ModifiedAfterDate: TDateTimePicker
              Left = 136
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 2
            end
            object ModifiedAfterTime: TDateTimePicker
              Left = 136
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 3
            end
            object check_ModifiedBeforeDate: TSpTBXCheckBox
              Left = 12
              Top = 7
              Width = 86
              Height = 21
              Caption = 'Before Date:'
              TabOrder = 4
              OnClick = check_dateClick
            end
            object check_ModifiedBeforeTime: TSpTBXCheckBox
              Left = 12
              Top = 59
              Width = 85
              Height = 21
              Caption = 'Before Time:'
              TabOrder = 5
              OnClick = check_dateClick
            end
            object check_ModifiedAfterDate: TSpTBXCheckBox
              Left = 136
              Top = 7
              Width = 79
              Height = 21
              Caption = 'After Date:'
              TabOrder = 6
              OnClick = check_dateClick
            end
            object check_ModifiedAfterTime: TSpTBXCheckBox
              Left = 136
              Top = 59
              Width = 78
              Height = 21
              Caption = 'After Time:'
              TabOrder = 7
              OnClick = check_dateClick
            end
          end
          object sheet_created: TSpTBXTabSheet
            Left = 0
            Top = 25
            Width = 409
            Height = 117
            Caption = 'Created'
            ImageIndex = -1
            TabItem = 'SpTBXTabItem5'
            object CreatedBeforeTime: TDateTimePicker
              Left = 12
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 0
            end
            object CreatedBeforeDate: TDateTimePicker
              Left = 12
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 1
            end
            object CreatedAfterDate: TDateTimePicker
              Left = 136
              Top = 30
              Width = 109
              Height = 22
              Date = 36578.000000000000000000
              Time = 36578.000000000000000000
              Enabled = False
              TabOrder = 2
            end
            object CreatedAfterTime: TDateTimePicker
              Left = 136
              Top = 82
              Width = 109
              Height = 22
              Date = 36579.998521296300000000
              Time = 36579.998521296300000000
              Enabled = False
              Kind = dtkTime
              TabOrder = 3
            end
            object check_CreatedBeforeDate: TSpTBXCheckBox
              Left = 12
              Top = 7
              Width = 86
              Height = 21
              Caption = 'Before Date:'
              TabOrder = 4
              OnClick = check_dateClick
            end
            object check_CreatedBeforeTime: TSpTBXCheckBox
              Left = 12
              Top = 59
              Width = 85
              Height = 21
              Caption = 'Before Time:'
              TabOrder = 5
              OnClick = check_dateClick
            end
            object check_CreatedAfterDate: TSpTBXCheckBox
              Left = 136
              Top = 7
              Width = 79
              Height = 21
              Caption = 'After Date:'
              TabOrder = 6
              OnClick = check_dateClick
            end
            object check_CreatedAfterTime: TSpTBXCheckBox
              Left = 136
              Top = 59
              Width = 78
              Height = 21
              Caption = 'After Time:'
              TabOrder = 7
              OnClick = check_dateClick
            end
          end
        end
      end
      object sheet_content: TSpTBXTabSheet
        Left = 0
        Top = 0
        Width = 433
        Height = 160
        Caption = 'Content'
        ImageIndex = -1
        DesignSize = (
          433
          160)
        TabItem = 'SpTBXTabItem4'
        object SpTBXLabel4: TSpTBXLabel
          Left = 12
          Top = 9
          Width = 71
          Height = 19
          Caption = 'File Contains:'
        end
        object memo_content: TTntMemo
          Left = 12
          Top = 30
          Width = 409
          Height = 82
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssBoth
          TabOrder = 1
        end
        object check_content_wordwrap: TSpTBXCheckBox
          Left = 214
          Top = 7
          Width = 207
          Height = 21
          Caption = 'Word Wrap'
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          TabOrder = 2
          OnClick = check_content_wordwrapClick
          Alignment = taRightJustify
        end
        object check_content_case_sensitive: TSpTBXCheckBox
          Left = 12
          Top = 116
          Width = 94
          Height = 21
          Caption = 'Case Sensitive'
          TabOrder = 3
        end
        object check_content_wholeword: TSpTBXCheckBox
          Left = 12
          Top = 136
          Width = 83
          Height = 21
          Caption = 'Whole Word'
          TabOrder = 4
        end
      end
      object sheet_name_location: TSpTBXTabSheet
        Left = 0
        Top = 0
        Width = 433
        Height = 160
        Caption = 'Name && Location'
        ImageIndex = -1
        DesignSize = (
          433
          160)
        TabItem = 'SpTBXTabItem1'
        object edit_filemask: TSpTBXEdit
          Left = 242
          Top = 31
          Width = 179
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 0
          OnChange = edit_wordphraseChange
        end
        object SpTBXLabel2: TSpTBXLabel
          Left = 12
          Top = 58
          Width = 46
          Height = 19
          Caption = 'Location'
        end
        object check_subfolders: TSpTBXCheckBox
          Left = 12
          Top = 105
          Width = 124
          Height = 52
          Caption = 'Include Subfolders'
          AutoSize = False
          TabOrder = 2
          Wrapping = twWrap
          Checked = True
          State = cbChecked
        end
        object spin_minlevel: TSpTBXSpinEdit
          Left = 142
          Top = 128
          Width = 68
          Height = 21
          Enabled = False
          TabOrder = 3
          SpinButton.Left = 50
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinButton.Enabled = False
          SpinButton.DrawPushedCaption = False
        end
        object check_minlevel: TSpTBXCheckBox
          Left = 142
          Top = 105
          Width = 72
          Height = 21
          Caption = 'Min Level:'
          TabOrder = 4
          OnClick = check_levelClick
        end
        object check_maxlevel: TSpTBXCheckBox
          Left = 242
          Top = 105
          Width = 76
          Height = 21
          Caption = 'Max Level:'
          TabOrder = 5
          OnClick = check_levelClick
        end
        object spin_maxlevel: TSpTBXSpinEdit
          Left = 242
          Top = 128
          Width = 68
          Height = 21
          Enabled = False
          TabOrder = 6
          SpinButton.Left = 50
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinButton.Enabled = False
          SpinButton.DrawPushedCaption = False
        end
        object edit_wordphrase: TSpTBXEdit
          Left = 12
          Top = 31
          Width = 159
          Height = 21
          TabOrder = 7
          OnChange = edit_wordphraseChange
        end
        object radio_name_word: TSpTBXRadioButton
          Left = 10
          Top = 7
          Width = 159
          Height = 21
          Caption = 'Word or Phrase in File Name'
          TabOrder = 8
          TabStop = True
          OnClick = radio_nameClick
          Checked = True
          GroupIndex = 1
        end
        object combo_extension: TSpTBXComboBox
          Left = 175
          Top = 31
          Width = 52
          Height = 21
          ItemHeight = 13
          TabOrder = 9
          Text = 'All'
          OnChange = edit_wordphraseChange
        end
        object radio_name_mask: TSpTBXRadioButton
          Left = 242
          Top = 7
          Width = 97
          Height = 21
          Caption = 'File Name Mask'
          TabOrder = 10
          OnClick = radio_nameClick
          GroupIndex = 1
        end
        object edit_location: TSpTBXButtonEdit
          Left = 12
          Top = 78
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          PopupMenu = FolderTreePopup
          TabOrder = 11
          EditButton.Left = 385
          EditButton.Top = 0
          EditButton.Width = 20
          EditButton.Height = 17
          EditButton.Align = alRight
          EditButton.DrawPushedCaption = False
          EditButton.DropDownMenu = FolderTreePopup
        end
      end
    end
  end
  object panel_status: TSpTBXPanel
    Left = 0
    Top = 402
    Width = 584
    Height = 22
    Caption = 'testtest'
    Align = alBottom
    TabOrder = 2
    Borders = False
    TBXStyleBackground = True
    object label_status: TTntLabel
      Left = 0
      Top = 0
      Width = 584
      Height = 22
      Align = alClient
      AutoSize = False
      Caption = 'Search'
      Transparent = True
      Layout = tlCenter
      ExplicitWidth = 58
      ExplicitHeight = 13
    end
  end
  object FolderTreePopup: TSpTBXFormPopupMenu
    OnPopup = FolderTreePopupPopup
    BorderStyle = pbsSizeable
    PopupFocus = True
    OnClosePopup = FolderTreePopupClosePopup
    Left = 460
    Top = 152
  end
  object timer_status: TTimer
    Enabled = False
    Interval = 100
    OnTimer = timer_statusTimer
    Left = 498
    Top = 152
  end
end
