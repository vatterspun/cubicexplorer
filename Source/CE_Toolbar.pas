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
//  The Original Code is CE_Toolbar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Toolbar;

interface

uses
  // TB2K, TBX, SpTBX
  SpTBXItem, TB2Item, SpTBXSkins,
  // System Units
  Classes, Windows, SysUtils, Controls, Messages, TntActnList, Graphics, Math;

type
  TTBItemViewerAccess = class(TTBItemViewer);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);
  
  TCEToolbarItem = class(TSpTBXItem)
  end;

  TCEToolbarSubmenuItem = class(TCEToolbarItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo
        default False;
  end;

  TCEToolbar = class(TSpTBXToolbar)
  private
    fLargeImages: Boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    fDynSpacersList: TList;
    fLastFullSize: Integer;
    fStretchedItemsList: TList;
    procedure AnchorItems(UpdateControlItems: Boolean = True); override;
    procedure Resize; override;
    procedure SetLargeImages(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetStretchedSize: Integer; virtual;
    procedure RightAlignItems; override;
    property LargeImages: Boolean read fLargeImages write SetLargeImages;
  end;

  TCEToolbarSeparatorItem = class(TSpTBXSeparatorItem)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCECustomToolbarSpacerItem = class(TSpTBXCustomLabelItem)
  protected
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault:
        Boolean); override;
  public
  published
    property CustomHeight;
    property CustomWidth;
    property Options;
  end;

  TCEToolbarFixedSpacerItem = class(TCECustomToolbarSpacerItem)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEToolbarDynamicSpacerItem = class(TCECustomToolbarSpacerItem)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCEToolbarStretcherItem = class(TCECustomToolbarSpacerItem)
  protected
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault:
        Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CE_ToolbarEditorItems, TB2Dock, TB2Toolbar;

{*------------------------------------------------------------------------------
  Create an instance of TCEToolbarSubmenuItem
-------------------------------------------------------------------------------}
constructor TCEToolbarSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable,tbisCombo];
end;

{*------------------------------------------------------------------------------
  DoPopup
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  GetDropdownCombo
-------------------------------------------------------------------------------}
function TCEToolbarSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

{*------------------------------------------------------------------------------
  SetDropdownCombo
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbar
-------------------------------------------------------------------------------}
constructor TCEToolbar.Create(AOwner: TComponent);
begin
  inherited;
  // create lists for dynamic spacers and stretchers to speed things up.
  // we'll use these in RightAlignItems method.
  fDynSpacersList:= TList.Create;
  fStretchedItemsList:= TList.Create;
  fLastFullSize:= 0;
end;

{-------------------------------------------------------------------------------
  Destroy TCEToolbar
-------------------------------------------------------------------------------}
destructor TCEToolbar.Destroy;
begin
  fDynSpacersList.Free;
  fStretchedItemsList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  AnchorItems
-------------------------------------------------------------------------------}
procedure TCEToolbar.AnchorItems(UpdateControlItems: Boolean);
begin
  // do nothing
end;

{-------------------------------------------------------------------------------
  Get StretchedSize
-------------------------------------------------------------------------------}
function TCEToolbar.GetStretchedSize: Integer;
var
  i: Integer;
  size, rightPos: Integer;
  toolbar: TTBCustomDockableWindow;
begin
  if assigned(CurrentDock) then
  begin
    size:= 0;
    rightPos:= 0;
    for i:= 0 to CurrentDock.ToolbarCount - 1 do
    begin
      toolbar:= CurrentDock.Toolbars[i];
      if (toolbar <> Self) and (toolbar.DockRow = Self.DockRow) then
      begin
        if (toolbar.DockPos > Self.DockPos) then
        begin
          if IsVertical then
          rightPos:= Max(toolbar.Top, rightPos)
          else
          rightPos:= Max(toolbar.Left, rightPos);
        end
        else
        begin
          if IsVertical then
          size:= size + toolbar.Height
          else
          size:= size + toolbar.Width;
        end;
      end;
    end;

    if rightPos = 0 then
    begin
      if IsVertical then
      rightPos:= CurrentDock.Height
      else
      rightPos:= CurrentDock.Width;
    end;

    Result:= rightPos - size;
    if IsVertical then
    Result:= Result - Self.CalcNCSizes.Y
    else
    Result:= Result - Self.CalcNCSizes.X;    
  end
  else
  begin
    if IsVertical then
    Result:= Self.ClientHeight
    else
    Result:= Self.ClientWidth;
  end;
end;

{-------------------------------------------------------------------------------
  Resize
-------------------------------------------------------------------------------}
procedure TCEToolbar.Resize;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  RightAlignItems
-------------------------------------------------------------------------------}
procedure TCEToolbar.RightAlignItems;
var
  i: Integer;
  item, prevItem: TTBCustomItem;
  IV: TTBItemViewer;
  fullSize, totalSize, overSize, size, newWidth: Integer;
  isDynSpacer, isStretcher: Boolean;
  spacer: TCEToolbarDynamicSpacerItem;
begin
  if (csDestroying in ComponentState) // exit if we are destroying
     or (tstRightAligning in FState)  // exit if already right aligning
     or (Items.Count <= 0)            // exit if we have no items
     or IsUpdating                    // exit if update in progress
  then Exit;

  FState:= FState + [tstRightAligning];
  View.BeginUpdate;
  try
    // Floating or Docked but not stretched or fullsized
    if Floating or not (Stretch or Self.FullSize) then
    begin
      for i:= 0 to Items.Count - 1 do
      begin
        item:= Items.Items[i];
        if (item is TCEToolbarDynamicSpacerItem) or (item is TCEToolbarStretcherItem) then
        begin
          if IsCustomizing then
          begin
            TCECustomToolbarSpacerItem(item).CustomWidth:= 12; // set to default size
            TCECustomToolbarSpacerItem(item).Visible:= true;
          end
          else
          TCECustomToolbarSpacerItem(item).Visible:= false;
        end
        else if item is TCEToolbarEditItem then
        TSpTBXCustomItemAccess(item).CustomWidth:= TCEToolbarEditItem(item).DefaultWidth
        else if item is TSpTBXCustomItem then
        TSpTBXCustomItemAccess(item).CustomWidth:= -1; // set to default size
      end;
    end
    // Docked and Stretched or FullSized
    else
    begin
      // initilize
      size:= 0;
      totalSize:= 0;
      fullSize:= GetStretchedSize;
      fLastFullSize:= fullSize;
      fDynSpacersList.Clear;
      fStretchedItemsList.Clear;

      // set default sizes
      for i:= 0 to View.ViewerCount - 1 do
      begin
        IV:= View.Viewers[i];
        item:= IV.Item;
        if (item is TCEToolbarDynamicSpacerItem) or (item is TCEToolbarStretcherItem) then
        begin
          if IsCustomizing then
          TCECustomToolbarSpacerItem(item).CustomWidth:= 12
          else
          TCECustomToolbarSpacerItem(item).CustomWidth:= 0;
        end
        else if (item is TCEToolbarEditItem) then
        begin
          TCEToolbarEditItem(item).CustomWidth:= TCEToolbarEditItem(item).DefaultWidth;
        end
        else if (item is TSpTBXCustomItem) and not (item is TCEToolbarFixedSpacerItem) then
        begin
          if TSpTBXCustomItemAccess(item).CustomWidth <> -1 then
          begin
            TSpTBXCustomItemAccess(item).CustomWidth:= -1; // set to default size
          end;
        end;
      end;

      // loop through viewers to get totalSize
      for i:= 0 to View.ViewerCount - 1 do
      begin
        IV:= View.Viewers[i];
        item:= IV.Item;

        // continue to next if item is Chevron
        if item is TTBChevronItem then
        Continue;

        isDynSpacer:= item is TCEToolbarDynamicSpacerItem;
        isStretcher:= item is TCEToolbarStretcherItem;

        // add dynamic spacers to list
        if isDynSpacer then
        fDynSpacersList.Add(item)
        // add strethed items to lists
        else if isStretcher and (i > 0) then
        begin
          // add to list only if stretched item is not separator/spacer/strether.
          // else set the size variable to zero so we won't subtract it from totalSize.
          prevItem:= View.Viewers[i-1].Item;
          if not (prevItem is TCECustomToolbarSpacerItem) and
             not (prevItem is TSpTBXSeparatorItem) and
             not (IsVertical and (prevItem is TCEToolbarEditItem)) and
             ((prevItem is TSpTBXCustomItem) or (prevItem is TTBControlItem)) then
          fStretchedItemsList.Add(prevItem)
          else
          size:= 0; 
        end;

        // hide stretchers if not  customizing
        if isStretcher then
        item.Visible:= IsCustomizing;

        // calculate size
        if isStretcher then
        begin
          // customizing, add stretcher to totalSize
          if IsCustomizing then
          begin
            if IsVertical then
            totalSize:= totalSize + TCEToolbarStretcherItem(item).CustomHeight
            else
            totalSize:= totalSize + TCEToolbarStretcherItem(item).CustomWidth;
          end;
          // remove strethed item from totalSize
          if not (IsVertical and (item is TCEToolbarEditItem)) then
          totalSize:= totalSize - size;
        end
        else if not isDynSpacer and item.Visible then // normal item
        begin
          if IsVertical then
          size:= IV.BoundsRect.Bottom - IV.BoundsRect.Top
          else
          size:= IV.BoundsRect.Right - IV.BoundsRect.Left;
          totalSize:= totalSize + size;
        end;
      end;

      // calculate overSize
      overSize:= fullSize - totalSize;

      // stretch items (dynamic spacers are not used if strethers were found)
      if fStretchedItemsList.Count > 0 then
      begin
        // hide/show dynamic spacers
        for i:= 0 to fDynSpacersList.Count - 1 do
        begin
          spacer:= TCEToolbarDynamicSpacerItem(fDynSpacersList.Items[i]);
          if IsCustomizing then 
          begin
            spacer.Visible:= true;
            spacer.CustomWidth:= 12; // set default size
            overSize:= overSize - 12;
          end
          else 
          spacer.Visible:= false;
        end;

        // calculate size for strethed items. minimum size is 20
        if (overSize > 0) and (fStretchedItemsList.Count > 0) then
        size:= Max(Floor(overSize / fStretchedItemsList.Count), 20)
        else
        size:= 20;

        // resize strethed items
        for i:= 0 to fStretchedItemsList.Count - 1 do
        begin
          item:= TTBCustomItem(fStretchedItemsList.Items[i]);

          if i < fStretchedItemsList.Count- 1 then
          begin
            newWidth:= size;
            overSize:= overSize - size;
          end
          else
          newWidth:= Max(overSize,20); // use the left over space for last item, that way we'll fill every pixel.

          if item is TTBControlItem then
          begin
            if assigned(TTBControlItem(item).Control) then
            TTBControlItem(item).Control.Width:= newWidth
          end
          else
          TSpTBXCustomItemAccess(item).CustomWidth:= newWidth;
        end;
      end
      // resize dynamic spacers
      else if fDynSpacersList.Count > 0 then
      begin
        // calculate size for dynamic spacers.
        if (overSize > 0) and (fDynSpacersList.Count > 0) then
        size:= Floor(overSize / fDynSpacersList.Count)
        else
        size:= 0;

        if IsCustomizing and (size < 12) then
        size:= 12;

        // resize dynamic spacers
        for i:= 0 to fDynSpacersList.Count - 1 do
        begin
          item:= TTBCustomItem(fDynSpacersList.Items[i]);
          if i < fDynSpacersList.Count- 1 then
          begin
            TSpTBXCustomItemAccess(item).CustomWidth:= size;
            overSize:= overSize - size;
          end
          else // use the left over space for last item, that way we'll fill every pixel.
          TSpTBXCustomItemAccess(item).CustomWidth:= overSize;
        end;
      end;
    end;
  finally
    View.EndUpdate;
    FState:= FState - [tstRightAligning];
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Set Large Images
-------------------------------------------------------------------------------}
procedure TCEToolbar.SetLargeImages(const Value: Boolean);
begin
  fLargeImages:= Value;
end;

{-------------------------------------------------------------------------------
  WMSize
-------------------------------------------------------------------------------}
procedure TCEToolbar.WMSize(var Message: TWMSize);
begin
  Self.DisableAlign;
  inherited;
  Self.EnableAlign;
end;

{-------------------------------------------------------------------------------
  DoDrawButton
-------------------------------------------------------------------------------}
procedure TCECustomToolbarSpacerItem.DoDrawButton(ACanvas: TCanvas; ARect:
    TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean);
begin
  //inherited;
  PaintDefault:= false;
  if Self.GetParentComponent is TSpTBXToolbar then
  begin
    if TSpTBXToolbar(Self.GetParentComponent).IsCustomizing then
    begin
      ACanvas.Brush.Color:= clWhite;
      ACanvas.Brush.Style:= bsSolid;
      ACanvas.FillRect(ARect);
      ACanvas.Brush.Color:= clBlack;
      ACanvas.FrameRect(ARect);
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarFixedSpacerItem
-------------------------------------------------------------------------------}
constructor TCEToolbarFixedSpacerItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.CustomWidth:= 12;
  Self.CustomHeight:= 16;
  Self.ItemStyle:= Self.ItemStyle + [tbisClicksTransparent];
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarDynamicSpacerItem
-------------------------------------------------------------------------------}
constructor TCEToolbarDynamicSpacerItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.CustomWidth:= 12;
  Self.CustomHeight:= 16;
  Self.ItemStyle:= Self.ItemStyle + [tbisClicksTransparent];
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarSeparatorItem
-------------------------------------------------------------------------------}
constructor TCEToolbarSeparatorItem.Create(AOwner: TComponent);
begin
  inherited;
  //Self.ItemStyle:= Self.ItemStyle - [tbisSeparator];
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarStretcherItem
-------------------------------------------------------------------------------}
constructor TCEToolbarStretcherItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.CustomWidth:= 12;
  Self.CustomHeight:= 16;
  Self.ItemStyle:= Self.ItemStyle + [tbisClicksTransparent];
end;

{-------------------------------------------------------------------------------
  DoDrawButton
-------------------------------------------------------------------------------}
procedure TCEToolbarStretcherItem.DoDrawButton(ACanvas: TCanvas; ARect: TRect;
    ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var
    PaintDefault: Boolean);
begin
  //inherited;
  PaintDefault:= false;
  if Self.GetParentComponent is TSpTBXToolbar then
  begin
    if TSpTBXToolbar(Self.GetParentComponent).IsCustomizing then
    begin
      ACanvas.Brush.Color:= clWhite;
      ACanvas.Brush.Style:= bsSolid;
      ACanvas.FillRect(ARect);
      ACanvas.Brush.Color:= clBlack;
      ACanvas.FrameRect(ARect);

      SpDrawArrow(ACanvas, Round(ARect.Right / 2)-2, Round(ARect.Bottom / 2), clBlack, false, false, 4);
    end;
  end;
end;

end.

