inherited CETextEditorPage: TCETextEditorPage
  Width = 670
  Height = 528
  ExplicitWidth = 670
  ExplicitHeight = 528
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 670
    Height = 25
    object MainToolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      DragHandleStyle = dhNone
      ProcessShortCuts = True
      Stretch = True
      TabOrder = 0
      Caption = 'MainToolbar'
      object SpTBXSubmenuItem1: TSpTBXSubmenuItem
        Caption = 'File'
        object SpTBXItem1: TSpTBXItem
          Action = text_file_new
        end
        object SpTBXItem2: TSpTBXItem
          Action = text_file_open
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object SpTBXItem3: TSpTBXItem
          Action = text_file_save
        end
        object SpTBXItem4: TSpTBXItem
          Action = text_file_saveas
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem5: TSpTBXItem
          Action = text_file_close
        end
        object SpTBXItem21: TSpTBXItem
          Action = text_file_reload
        end
      end
      object SpTBXSubmenuItem2: TSpTBXSubmenuItem
        Caption = 'Edit'
        object SpTBXItem6: TSpTBXItem
          Action = text_edit_undo
        end
        object SpTBXItem7: TSpTBXItem
          Action = text_edit_redo
        end
        object SpTBXSeparatorItem3: TSpTBXSeparatorItem
        end
        object SpTBXItem8: TSpTBXItem
          Action = text_edit_copy
        end
        object SpTBXItem9: TSpTBXItem
          Action = text_edit_cut
        end
        object SpTBXItem10: TSpTBXItem
          Action = text_edit_paste
        end
        object SpTBXItem11: TSpTBXItem
          Action = text_edit_delete
        end
        object SpTBXSeparatorItem6: TSpTBXSeparatorItem
        end
        object SpTBXItem12: TSpTBXItem
          Action = text_edit_selall
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object SpTBXItem13: TSpTBXItem
          Action = text_edit_search
        end
        object SpTBXItem14: TSpTBXItem
          Action = text_edit_findnext
        end
        object SpTBXItem15: TSpTBXItem
          Action = text_edit_findprev
        end
      end
      object SpTBXSubmenuItem3: TSpTBXSubmenuItem
        Caption = 'Format'
        object SpTBXItem17: TSpTBXItem
          Action = text_format_wrap
        end
        object SpTBXSeparatorItem5: TSpTBXSeparatorItem
        end
        object SpTBXItem19: TSpTBXItem
          Action = text_format_options
        end
      end
      object SpTBXSubmenuItem4: TSpTBXSubmenuItem
        Caption = 'View'
        object SpTBXItem20: TSpTBXItem
          Action = text_view_toolbar
        end
        object SpTBXItem22: TSpTBXItem
          Action = text_view_statusbar
        end
      end
      object highlighterSubmenu: TSpTBXSubmenuItem
        Caption = 'Highlighter'
        object SpTBXItem16: TSpTBXItem
          Tag = -1
          Caption = 'None'
          GroupIndex = 1
          RadioItem = True
        end
        object SpTBXItem18: TSpTBXItem
          Tag = -2
          Caption = 'Automatic'
          Checked = True
          GroupIndex = 1
          RadioItem = True
        end
        object SpTBXSeparatorItem8: TSpTBXSeparatorItem
        end
      end
    end
  end
  object Editor: TSynEdit
    Left = 0
    Top = 25
    Width = 670
    Height = 323
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    BorderStyle = bsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Gutter.Width = 0
    Options = [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    RightEdgeColor = clWindow
    WantTabs = True
    OnReplaceText = EditorReplaceText
    OnStatusChange = EditorStatusChange
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 348
    Width = 670
    Height = 25
    SizeGrip = False
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object label_input: TSpTBXLabelItem
      Caption = 'Insert'
      Alignment = taCenter
    end
    object SpTBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object label_modified: TSpTBXLabelItem
      Alignment = taCenter
    end
    object SpTBXSeparatorItem10: TSpTBXSeparatorItem
    end
    object label_path: TSpTBXLabelItem
      Wrapping = twPathEllipsis
    end
  end
  object FindPanel: TSpTBXPanel
    Left = 0
    Top = 373
    Width = 670
    Height = 155
    Align = alBottom
    TabOrder = 3
    Visible = False
    TBXStyleBackground = True
    DesignSize = (
      670
      155)
    object SpTBXLabel1: TSpTBXLabel
      Left = 8
      Top = 2
      Width = 39
      Height = 19
      Caption = 'Search'
    end
    object SearchMemo: TTntMemo
      Left = 8
      Top = 20
      Width = 405
      Height = 53
      Anchors = [akLeft, akTop, akRight]
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object SpTBXLabel2: TSpTBXLabel
      Left = 8
      Top = 75
      Width = 44
      Height = 19
      Caption = 'Replace'
    end
    object ReplaceMemo: TTntMemo
      Left = 8
      Top = 93
      Width = 405
      Height = 53
      Anchors = [akLeft, akTop, akRight]
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object SpTBXGroupBox1: TSpTBXGroupBox
      Left = 419
      Top = 15
      Width = 134
      Height = 129
      Caption = 'Options'
      Anchors = [akTop, akRight]
      TabOrder = 4
      object opt_check1: TSpTBXCheckBox
        Left = 8
        Top = 20
        Width = 99
        Height = 21
        Caption = 'Case sensitivity'
        TabOrder = 0
      end
      object opt_check2: TSpTBXCheckBox
        Left = 8
        Top = 41
        Width = 109
        Height = 21
        Caption = 'Whole words only'
        TabOrder = 1
      end
      object opt_check3: TSpTBXCheckBox
        Left = 8
        Top = 62
        Width = 115
        Height = 21
        Caption = 'Search from cursor'
        TabOrder = 2
        Checked = True
        State = cbChecked
      end
      object opt_check4: TSpTBXCheckBox
        Left = 8
        Top = 83
        Width = 111
        Height = 21
        Caption = 'Selected text only'
        TabOrder = 3
      end
      object opt_check5: TSpTBXCheckBox
        Left = 8
        Top = 104
        Width = 116
        Height = 21
        Caption = 'Regular expression'
        TabOrder = 4
      end
    end
    object SpTBXButton1: TSpTBXButton
      Left = 559
      Top = 10
      Width = 109
      Height = 24
      Caption = 'Search'
      Anchors = [akTop, akRight]
      TabOrder = 5
      OnClick = SpTBXButton1Click
      Default = True
      DrawPushedCaption = False
    end
    object SpTBXButton2: TSpTBXButton
      Left = 559
      Top = 36
      Width = 109
      Height = 24
      Caption = 'Replace'
      Anchors = [akTop, akRight]
      TabOrder = 6
      OnClick = SpTBXButton2Click
      DrawPushedCaption = False
    end
    object SpTBXButton3: TSpTBXButton
      Left = 559
      Top = 62
      Width = 109
      Height = 24
      Caption = 'Replace All'
      Anchors = [akTop, akRight]
      TabOrder = 7
      OnClick = SpTBXButton3Click
      DrawPushedCaption = False
    end
    object opt_radio: TSpTBXRadioGroup
      Left = 559
      Top = 90
      Width = 109
      Height = 54
      Caption = 'Direction'
      Anchors = [akTop, akRight]
      TabOrder = 8
      ItemIndex = 1
      Items.Strings = (
        'Up'
        'Down')
    end
  end
  object ActionList: TTntActionList
    Left = 12
    Top = 32
    object text_file_new: TTntAction
      Tag = 101
      Category = 'File'
      Caption = 'New'
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_edit_undo: TTntAction
      Tag = 201
      Category = 'Edit'
      Caption = 'Undo'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_file_open: TTntAction
      Tag = 102
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_file_save: TTntAction
      Tag = 103
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_file_saveas: TTntAction
      Tag = 104
      Category = 'File'
      Caption = 'Save As...'
      ShortCut = 49235
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_file_close: TTntAction
      Tag = 105
      Category = 'File'
      Caption = 'Close'
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_edit_redo: TTntAction
      Tag = 202
      Category = 'Edit'
      Caption = 'Redo'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_copy: TTntAction
      Tag = 203
      Category = 'Edit'
      Caption = 'Copy'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_cut: TTntAction
      Tag = 204
      Category = 'Edit'
      Caption = 'Cut'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_paste: TTntAction
      Tag = 205
      Category = 'Edit'
      Caption = 'Paste'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_delete: TTntAction
      Tag = 206
      Category = 'Edit'
      Caption = 'Delete'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_selall: TTntAction
      Tag = 207
      Category = 'Edit'
      Caption = 'Select All'
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_search: TTntAction
      Tag = 208
      Category = 'Edit'
      Caption = 'Search and Replace'
      ShortCut = 16454
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_findnext: TTntAction
      Tag = 209
      Category = 'Edit'
      Caption = 'Find Next'
      ShortCut = 114
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_edit_findprev: TTntAction
      Tag = 210
      Category = 'Edit'
      Caption = 'Find Previous'
      ShortCut = 8306
      OnExecute = text_edit_Execute
      OnUpdate = text_edit_Update
    end
    object text_format_wrap: TTntAction
      Tag = 301
      Category = 'Format'
      Caption = 'Word Wrap'
      OnExecute = text_format_Execute
      OnUpdate = text_format_Update
    end
    object text_format_options: TTntAction
      Tag = 302
      Category = 'Format'
      Caption = 'Editor Options...'
      OnExecute = text_format_Execute
      OnUpdate = text_format_Update
    end
    object text_view_toolbar: TTntAction
      Tag = 401
      Category = 'View'
      Caption = 'Show Toolbar'
      Visible = False
      OnExecute = text_view_Execute
      OnUpdate = text_view_Update
    end
    object text_file_reload: TTntAction
      Tag = 106
      Category = 'File'
      Caption = 'Reload'
      ShortCut = 16466
      OnExecute = text_file_Execute
      OnUpdate = text_file_Update
    end
    object text_view_statusbar: TTntAction
      Tag = 402
      Category = 'View'
      Caption = 'Show Statusbar'
      OnExecute = text_view_Execute
      OnUpdate = text_view_Update
    end
  end
  object SynEditSearch: TSynEditSearch
    Left = 12
    Top = 64
  end
  object SynEditRegexSearch: TSynEditRegexSearch
    Left = 12
    Top = 96
  end
  object SynCppSyn1: TSynCppSyn
    Left = 12
    Top = 236
  end
  object SynCssSyn1: TSynCssSyn
    Left = 44
    Top = 272
  end
  object SynHTMLSyn1: TSynHTMLSyn
    Left = 12
    Top = 272
  end
  object SynPHPSyn1: TSynPHPSyn
    Left = 76
    Top = 272
  end
  object SynJScriptSyn1: TSynJScriptSyn
    Left = 108
    Top = 272
  end
  object SynXMLSyn1: TSynXMLSyn
    WantBracesParsed = False
    Left = 140
    Top = 272
  end
  object SynBatSyn1: TSynBatSyn
    Left = 12
    Top = 308
  end
  object SynIniSyn1: TSynIniSyn
    Left = 44
    Top = 308
  end
  object SynPasSyn1: TSynPasSyn
    Left = 44
    Top = 236
  end
  object SynJavaSyn1: TSynJavaSyn
    Left = 76
    Top = 236
  end
  object SynPerlSyn1: TSynPerlSyn
    Left = 108
    Top = 236
  end
  object SynDfmSyn1: TSynDfmSyn
    Left = 140
    Top = 236
  end
  object SynURISyn1: TSynURISyn
    Left = 76
    Top = 308
  end
  object SynURIOpener1: TSynURIOpener
    Editor = Editor
    URIHighlighter = SynURISyn1
    Left = 12
    Top = 136
  end
end
