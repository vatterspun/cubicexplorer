//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is CE_BookmarkBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_BookmarkBar;

interface

uses
  // CE Units
  CE_GlobalCtrl, dCE_Actions, fCE_BookmarkPanel, CE_Bookmarks,
  CE_BookmarkTree, CE_LanguageEngine, CE_Toolbar,
  // Toolbar2K
  TB2Item,
  // SpTBXLib
  SpTBXItem, SpTBXSkins,
  // VirtualTree
  VirtualTrees,
  // VSTools
  MPCommonUtilities, MPCommonObjects, MPShellUtilities,
  // Tnt Ctrls
  TntWideStrUtils,
  // System Units
  Classes, Graphics, ImgList, Windows, ShlObj, SysUtils, Controls;

type
  TTBCustomItemHack = class(TTBCustomItem);

  TCEBookmarkItem = class(TSpTBXItem)
  protected
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State:
        TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var
        CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage:
        TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType; const
        PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList; var
        AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean);
        override;
  public
    BookmarkData: PCEBookData;
    procedure Click; override;
  end;

  TCEBookmarkSubItem = class(TSpTBXSubmenuItem)
  protected
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State:
        TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var
        CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage:
        TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType; const
        PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList; var
        AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean);
        override;
  public
    BookmarkData: PCEBookData;
    procedure Click; override;
  end;

  TCEOpenAllBookmarksItem = class(TSpTBXSubmenuItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure OnSubMenuClick(Sender: TObject);
    procedure OpenAll(Mode: Integer = 0);
  end;


  TCEBookmarkToolbar = class(TCEToolbar)
  private
  protected
    procedure SetLargeImages(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Populate;
  end;

procedure PopulateBookmarkItem(RootItem: TTBCustomItem; BookmarkTree:
    TCEBookmarkTree; OpenAllToRoot: Boolean = false; LargeIcons: Boolean =
    false);

var
  UpdatingBookmarkItems: Boolean = false;

implementation

uses
  CE_StdBookmarkComps, CE_Utils;

{*------------------------------------------------------------------------------
  Populate BookmarkItem
-------------------------------------------------------------------------------}
procedure PopulateBookmarkItem(RootItem: TTBCustomItem; BookmarkTree:
    TCEBookmarkTree; OpenAllToRoot: Boolean = false; LargeIcons: Boolean =
    false);

  procedure EnumNode(Node: PVirtualNode; SubItem: TTBCustomItem);
  var
    chItem: TSpTBXItem;
    chNode: PVirtualNode;
    data: PCEBookData;
    sep: TSpTBXSeparatorItem;
    openAll: TCEOpenAllBookmarksItem;
  begin
    // Show "Open All in Tabs" at top of the menu (Opera style)
    if CEBookmarkPanel.Settings.ShowOpenAllItem and CEBookmarkPanel.Settings.ShowOpenAllAtTop then
    begin
      if (SubItem is TCEBookmarkSubItem) then
      begin
        openAll:= TCEOpenAllBookmarksItem.Create(RootItem);
        openAll.CustomHeight:= 24;
        SubItem.Add(openAll);
        sep:= TSpTBXSeparatorItem.Create(RootItem);
        SubItem.Add(sep);
      end
    end;

    chNode:= Node.FirstChild;
    while assigned(chNode) do
    begin
      data:= BookmarkTree.GetNodeData(chNode);
      if chNode.ChildCount > 0 then
      begin
        chItem:= TCEBookmarkSubItem.Create(RootItem);
        TCEBookmarkSubItem(chItem).BookmarkData:= data;
        if data.BookComp.SubMenuOnly then
        chItem.Options:= chItem.Options + [tboDropdownArrow]
        else
        TTBCustomItemHack(chItem).ItemStyle:= TTBCustomItemHack(chItem).ItemStyle + [tbisCombo];
      end
      else
      begin
        chItem:= TCEBookmarkItem.Create(RootItem);
        TCEBookmarkItem(chItem).BookmarkData:= data;
      end;

      chItem.Caption:= data.BookComp.Title;
      chItem.DisplayMode:= nbdmImageAndText;
      if LargeIcons then
      begin
        if data.BookComp.ImageList = SmallSysImages then
        chItem.Images:= LargeSysImages
        else
        chItem.Images:= data.BookComp.ImageList;
      end
      else
      chItem.Images:= data.BookComp.ImageList;
      
      chItem.ImageIndex:= data.BookComp.GetImageIndex;

      if chItem is TSpTBXSubmenuItem then
      EnumNode(chNode, chItem);

      SubItem.Add(chItem);
      chNode:= chNode.NextSibling;
    end;
    
    // Show "Open All in Tabs" at bottom of the menu (Firefox style)
    if CEBookmarkPanel.Settings.ShowOpenAllItem and not CEBookmarkPanel.Settings.ShowOpenAllAtTop then
    begin
      if (SubItem is TCEBookmarkSubItem) then
      begin
        sep:= TSpTBXSeparatorItem.Create(RootItem);
        SubItem.Add(sep);
        openAll:= TCEOpenAllBookmarksItem.Create(RootItem);
        openAll.CustomHeight:= 24;
        SubItem.Add(openAll);
      end
    end;
  end;
var
  sep: TSpTBXSeparatorItem;
  openAll: TCEOpenAllBookmarksItem;
begin
  UpdatingBookmarkItems:= true;
  try
    RootItem.Clear;

    if not assigned(BookmarkTree) then
    Exit;

    // Show "Open All in Tabs" at top of the menu (Opera style)
    if CEBookmarkPanel.Settings.ShowOpenAllItem and OpenAllToRoot and CEBookmarkPanel.Settings.ShowOpenAllAtTop then
    begin
      openAll:= TCEOpenAllBookmarksItem.Create(RootItem);
      openAll.CustomHeight:= 24;
      RootItem.Add(openAll);
      sep:= TSpTBXSeparatorItem.Create(RootItem);
      RootItem.Add(sep);
    end;

    EnumNode(BookmarkTree.RootNode, RootItem);

    // Show "Open All in Tabs" at bottom of the menu (Firefox style)
    if CEBookmarkPanel.Settings.ShowOpenAllItem and OpenAllToRoot and not CEBookmarkPanel.Settings.ShowOpenAllAtTop then
    begin
      sep:= TSpTBXSeparatorItem.Create(RootItem);
      RootItem.Add(sep);
      openAll:= TCEOpenAllBookmarksItem.Create(RootItem);
      openAll.CustomHeight:= 24;
      RootItem.Add(openAll);
    end;
  finally
    UpdatingBookmarkItems:= false;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEBookmarkToolbar
-------------------------------------------------------------------------------}
constructor TCEBookmarkToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Self.Customizable:= false;
end;

{*------------------------------------------------------------------------------
  Populate toolbar
-------------------------------------------------------------------------------}
procedure TCEBookmarkToolbar.Populate;
begin
  self.BeginUpdate;
  PopulateBookmarkItem(Self.Items, CEBookmarkPanel.BookmarkTree, false);
  self.EndUpdate;
end;

{-------------------------------------------------------------------------------
  Set Large Images
-------------------------------------------------------------------------------}
procedure TCEBookmarkToolbar.SetLargeImages(const Value: Boolean);
begin
  Inherited;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle Click
-------------------------------------------------------------------------------}
procedure TCEBookmarkItem.Click;
var
  ss: TShiftState;
begin
  if UpdatingBookmarkItems then
  Exit;
  
  ss:= GetShiftState;
  Include(ss, ssLeft);
  
  if ss = [ssLeft] then
  BookmarkData.BookComp.MouseClick([ssLeft, ssDouble], mbLeft)
  else if (ss = [ssLeft, ssAlt]) or (ss = [ssLeft, ssCtrl]) then
  BookmarkData.BookComp.MouseClick([ssMiddle], mbMiddle)
  else if (ss = [ssLeft, ssAlt, ssShift]) or (ss = [ssLeft, ssCtrl, ssShift]) then
  BookmarkData.BookComp.MouseClick([ssMiddle, ssShift], mbMiddle);
end;

{*------------------------------------------------------------------------------
  Called when on Caption draw
-------------------------------------------------------------------------------}
procedure TCEBookmarkItem.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect:
    TRect; State: TSpTBXSkinStatesType; var ACaption: WideString; var
    CaptionRect: TRect; var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
{ TODO : Fix this }
//  if assigned(BookmarkData.BookComp) then
//  begin
//    if BookmarkData.BookComp.Ghosted then
//    begin
//      FillChar(ii, SizeOf(TTBXItemInfo), 0);
//      ii.ViewType := TVT_NORMALTOOLBAR;
//      ii.ItemOptions := IO_APPACTIVE or IO_TOOLBARSTYLE;
//      ii.Pushed := false;
//      ii.Selected := false;
//      ii.HoverKind := hkMouseHover;
//      ii.Enabled:= false;
//      ACanvas.Font.Color:= CurrentTheme.GetItemTextColor(ii);
//    end
//  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's calles on image draw
-------------------------------------------------------------------------------}
procedure TCEBookmarkItem.DoDrawImage(ACanvas: TCanvas; State:
    TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
    TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
    PaintDefault: Boolean);
begin
  inherited;
  if not UpdatingBookmarkItems and assigned(BookmarkData) then
  begin
    if assigned(BookmarkData.BookComp) then
    begin
      AImageIndex:= BookmarkData.BookComp.GetImageIndex(false);
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle Click
-------------------------------------------------------------------------------}
procedure TCEBookmarkSubItem.Click;
var
  ss: TShiftState;
begin
  if UpdatingBookmarkItems then
  Exit;
  
  ss:= GetShiftState;
  Include(ss, ssLeft);
  
  if ss = [ssLeft] then
  BookmarkData.BookComp.MouseClick([ssLeft, ssDouble], mbLeft)
  else if (ss = [ssLeft, ssAlt]) or (ss = [ssLeft, ssCtrl]) then
  BookmarkData.BookComp.MouseClick([ssMiddle], mbMiddle)
  else if (ss = [ssLeft, ssAlt, ssShift]) or (ss = [ssLeft, ssCtrl, ssShift]) then
  BookmarkData.BookComp.MouseClick([ssMiddle, ssShift], mbMiddle);
end;

{*------------------------------------------------------------------------------
  Called when on Caption draw
-------------------------------------------------------------------------------}
procedure TCEBookmarkSubItem.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect:
    TRect; State: TSpTBXSkinStatesType; var ACaption: WideString; var
    CaptionRect: TRect; var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
{ TODO : Fix this }
//  if assigned(BookmarkData.BookComp) then
//  begin
//    if BookmarkData.BookComp.Ghosted then
//    begin
//      FillChar(ii, SizeOf(TTBXItemInfo), 0);
//      ii.ViewType := TVT_NORMALTOOLBAR;
//      ii.ItemOptions := IO_APPACTIVE or IO_TOOLBARSTYLE;
//      ii.Pushed := false;
//      ii.Selected := false;
//      ii.HoverKind := hkMouseHover;
//      ii.Enabled:= false;
//      ACanvas.Font.Color:= CurrentTheme.GetItemTextColor(ii);
//    end
//  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's calles on image draw
-------------------------------------------------------------------------------}
procedure TCEBookmarkSubItem.DoDrawImage(ACanvas: TCanvas; State:
    TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
    TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
    PaintDefault: Boolean);
begin
  inherited;
{ TODO : Fix this }  
//  if assigned(BookmarkData) then
//  begin
//    if assigned(BookmarkData.BookComp) then
//    begin
//      AImageIndex:= BookmarkData.BookComp.GetImageIndex(ItemInfo.Pushed and (ItemInfo.ComboPart = cpCombo));
//    end;
//  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEOpenAllBookmarksItem
-------------------------------------------------------------------------------}
constructor TCEOpenAllBookmarksItem.Create(AOwner: TComponent);
var
  item: TSpTBXItem;
begin
  inherited;
  Self.DropdownCombo:= true;
  Self.Caption:= _('Open All in Tabs');

  // "And Launch Files" item
  item:= TSpTBXItem.Create(self);
  item.Caption:= _('And Launch Files');
  item.CustomHeight:= 24;
  item.Tag:= 1;
  item.OnClick:= OnSubMenuClick;
  self.Add(item);

  // "Launch Files Only" item
  item:= TSpTBXItem.Create(self);
  item.Caption:= _('Launch Files Only');
  item.CustomHeight:= 24;
  item.Tag:= 2;
  item.OnClick:= OnSubMenuClick;
  self.Add(item);
end;

{*------------------------------------------------------------------------------
  Handle Click
-------------------------------------------------------------------------------}
procedure TCEOpenAllBookmarksItem.Click;
var
  root: TTBCustomItem;
begin
  root:= Self.Parent;
  if not assigned(root) then
  Exit;

  OpenAll;
end;

{*------------------------------------------------------------------------------
  Handle Sub menu item click
-------------------------------------------------------------------------------}
procedure TCEOpenAllBookmarksItem.OnSubMenuClick(Sender: TObject);
begin
  OpenAll(TSpTBXItem(sender).Tag);
end;

{*------------------------------------------------------------------------------
  Open all items
-------------------------------------------------------------------------------}
procedure TCEOpenAllBookmarksItem.OpenAll(Mode: Integer = 0);
var
  comp: TCECustomBookComp;
  i: Integer;
  root: TTBCustomItem;
  item: TTBCustomItem;
begin
  root:= Self.Parent;
  if not assigned(root) then
  Exit;

  for i:= 0 to root.Count - 1 do
  begin
    item:= root.Items[i];
    if item is TCEBookmarkItem then
    comp:= TCEBookmarkItem(item).BookmarkData.BookComp
    else if item is TCEBookmarkSubItem then
    comp:= TCEBookmarkSubItem(item).BookmarkData.BookComp
    else
    comp:= nil;

    if comp is TCENormalItemComp then
    begin
      if TCENormalItemComp(comp).IsFolder then
      begin
        if Mode < 2 then
        comp.MouseClick([ssShift,ssMiddle],mbMiddle)
      end
      else if Mode > 0  then
      begin
        comp.MouseClick([ssDouble, ssLeft], mbLeft);
      end;
    end;
  end;
end;

end.
