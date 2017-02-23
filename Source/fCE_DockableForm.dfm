object CECustomDockableForm: TCECustomDockableForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'CECustomDockableForm'
  ClientHeight = 296
  ClientWidth = 425
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 425
    Height = 9
    FixAlign = True
  end
  object BottomDock: TSpTBXDock
    Left = 0
    Top = 287
    Width = 425
    Height = 9
    Position = dpBottom
  end
end
