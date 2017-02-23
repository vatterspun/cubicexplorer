inherited CEStackPanel: TCEStackPanel
  Caption = 'Stack'
  ClientWidth = 422
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 438
  ExplicitHeight = 330
  PixelsPerInch = 96
  TextHeight = 13
  inherited TopDock: TSpTBXDock
    Width = 422
    Height = 26
    ExplicitWidth = 422
    ExplicitHeight = 26
    object StackToolbar: TCEToolbar
      Left = 0
      Top = 0
      BorderStyle = bsNone
      DragHandleStyle = dhNone
      Images = CE_Images.SmallIcons
      Stretch = True
      TabOrder = 0
      Caption = 'Stacks'
    end
  end
  inherited BottomDock: TSpTBXDock
    Width = 422
    ExplicitWidth = 422
  end
  object DropStackPopup: TSpTBXPopupMenu
    OnPopup = DropStackPopupPopup
    Left = 368
    Top = 48
    object but_clearlist: TSpTBXItem
      Action = CEActions.act_stack_clear
    end
  end
end
