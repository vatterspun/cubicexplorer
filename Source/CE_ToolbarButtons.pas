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
//  The Original Code is CE_ToolbarButtons.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ToolbarButtons;

interface

uses
  // CE Units
  CE_Toolbar, CE_GlobalCtrl, CE_LanguageEngine, CE_ToolbarEditorItems,
  // VSTools
  MPShellUtilities, MPCommonObjects, MPCommonUtilities, EasyListview,
  // TB2K, SpTBX
  SpTBXItem, TB2Item, SpTBXSkins, SpTBXEditors,
  // Tnt Controls
  TntActnList, TntClipbrd, TntSysUtils, TntClasses,
  // System Units
  Classes, Windows, SysUtils, Controls, Messages, Graphics, ImgList, ShlObj,
  Variants;

type
  TCEFileViewBackButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    procedure OnSubClick(Sender: TObject);
  end;


  TCEFileViewForwardButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    procedure OnSubClick(Sender: TObject);
  end;

  TCENewFileButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnSubClick(Sender: TObject);
  end;

  TCEFileViewCopyPathButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    procedure OnSubClick(Sender: TObject);
  end;

  TCEArrangeByButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnSubClick(Sender: TObject);
  end;

  TCEViewStyleButton = class(TCEToolbarSubmenuItem)
  protected
    procedure Populate;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEGroupByButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnSubClick(Sender: TObject);
  end;

type
  TCEMainMenuButton = class(TCEToolbarSubmenuItem)
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCEBookmarksButton = class(TCEToolbarSubmenuItem)
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCESessionsButton = class(TCEToolbarSubmenuItem)
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEClosedTabsListButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCEFiltersMenuButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEToolbarComboBoxItemViewerAccess = class(TCEToolbarComboBoxItemViewer);
  
  TCEFilterPatternItem = class(TCEToolbarComboBoxItem)
  protected
    procedure DoChange(const AText: WideString); override;
    procedure DoClearButtonClick(Shift: TShiftState); override;
    function GetComboItems: TTntStrings; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCEUndoDeleteButton = class(TCEToolbarSubmenuItem)
  private
    fRestoreListItem: TSpTBXItem;
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
    procedure RefreshItems;
  public
    destructor Destroy; override;
    procedure ClearItems(OnlyTrashItems: Boolean = false);
    procedure OnSubClick(Sender: TObject);
  end;

  TCEButtonSettings = class(TPersistent)
  private
    fTabs_ShowTabControls: Boolean;
    function GetUndoDelete_ConfirmRestore: Boolean;
    function GetUndoDelete_DayLimit: Integer;
    function GetUndoDelete_ItemCount: Integer;
    procedure SetUndoDelete_ConfirmRestore(const Value: Boolean);
    procedure SetUndoDelete_DayLimit(const Value: Integer);
    procedure SetUndoDelete_ItemCount(const Value: Integer);
  public
    constructor Create;
  published
    property Tabs_ShowTabControls: Boolean read fTabs_ShowTabControls write
        fTabs_ShowTabControls;
    property UndoDelete_ConfirmRestore: Boolean read GetUndoDelete_ConfirmRestore
        write SetUndoDelete_ConfirmRestore;
    property UndoDelete_DayLimit: Integer read GetUndoDelete_DayLimit write
        SetUndoDelete_DayLimit;
    property UndoDelete_ItemCount: Integer read GetUndoDelete_ItemCount write
        SetUndoDelete_ItemCount;
  end;

type
  TCESystemPowerButton = class(TCEToolbarSubmenuItem)
  protected
    procedure HandleItemClick(Sender: TObject);
    procedure Populate;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCETabsButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
    procedure DoSubClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCEStackOpenButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TCEStackSaveButton = class(TCEToolbarSubmenuItem)
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  GlobalButtonSettings: TCEButtonSettings;

implementation

uses
  CE_FileView, fCE_FileView, CE_BaseFileView, dCE_Actions, dCE_Images, Main,
  CE_Sessions, fCE_FiltersPanel, CE_Utils, MPShellTypes, CE_CommonObjects,
  CE_AppSettings, CE_SystemUtils, CE_SpTabBar, SpTBXTabs, fCE_StackPanel;

{##############################################################################}

{*------------------------------------------------------------------------------
  Do Back Button Popup
-------------------------------------------------------------------------------}
procedure TCEFileViewBackButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  fileView: TCEFileView;
  i: Integer;
  item: TSpTBXItem;
begin
  Sender.Clear;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    Sender.SubMenuImages:= fileView.History.SmallSysImages;
    for i:= fileView.History.ItemIndex-1 downto 0 do
    begin
      item:= TSpTBXItem.Create(Sender);
      item.Caption:= fileView.History.Items[i].NameNormal;
      item.ImageIndex:= fileView.History.Items[i].GetIconIndex(false,icSmall);
      item.Tag:= i;
      item.OnClick:= OnSubClick;
      Sender.Add(item);
    end;
  end;
end;
{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEFileViewBackButton.OnSubClick(Sender: TObject);
var
  fileView: TCEFileView;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    if (TSpTBXItem(Sender).Tag > -1) and (TSpTBXItem(Sender).Tag < fileView.History.Count) then
    begin
      fileView.History.ItemIndex:= TSpTBXItem(Sender).Tag;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Do Forward Button Popup
-------------------------------------------------------------------------------}
procedure TCEFileViewForwardButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  fileView: TCEFileView;
  i: Integer;
  item: TSpTBXItem;
begin
  Sender.Clear;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    Sender.SubMenuImages:= fileView.History.SmallSysImages;
    for i:= fileView.History.ItemIndex+1 to fileView.History.Count-1 do
    begin
      item:= TSpTBXItem.Create(Sender);
      item.Caption:= fileView.History.Items[i].NameNormal;
      item.ImageIndex:= fileView.History.Items[i].GetIconIndex(false,icSmall);
      item.Tag:= i;
      item.OnClick:= OnSubClick;
      Sender.Add(item);
    end;
  end;
end;
{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEFileViewForwardButton.OnSubClick(Sender: TObject);
var
  fileView: TCEFileView;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    if (TSpTBXItem(Sender).Tag > -1) and (TSpTBXItem(Sender).Tag < fileView.History.Count) then
    begin
      fileView.History.ItemIndex:= TSpTBXItem(Sender).Tag;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCENewFileButton
-------------------------------------------------------------------------------}
constructor TCENewFileButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do NewFile Button Popup
-------------------------------------------------------------------------------}
procedure TCENewFileButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  fileView: TCEFileView;
  i: Integer;
  item: TSpTBXItem;
begin
  Sender.Clear;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    Sender.SubMenuImages:= SmallSysImages;
    if fileView.ShellNewMenu.Items.Count = 0 then
    fileView.ShellNewMenu.RebuildMenu;
    
    for i:= 0 to fileView.ShellNewMenu.Items.Count-1 do
    begin
      if fileView.ShellNewMenu.Items[I].Caption <> '-' then
      begin
        item:= TSpTBXItem.Create(nil);
        item.Caption:= _(fileView.ShellNewMenu.Items[I].Caption);
        item.Tag:= i;
        item.ImageIndex := fileView.ShellNewMenu.Items[I].ImageIndex;
        item.OnClick := OnSubClick;
        Sender.Add(item);
      end
      else
        Sender.Add(TSpTBXSeparatorItem.Create(nil));
    end;
  end;
end;

{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCENewFileButton.OnSubClick(Sender: TObject);
var
  fileView: TCEFileView;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    if (TSpTBXItem(Sender).Tag > -1) and (TSpTBXItem(Sender).Tag < fileView.ShellNewMenu.Items.Count) then
    begin
      fileView.ShellNewMenu.Items[TSpTBXItem(Sender).Tag].Click;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Do Forward Button Popup
-------------------------------------------------------------------------------}
procedure TCEFileViewCopyPathButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  item: TSpTBXItem;
  sep: TSpTBXSeparatorItem;
begin
  Sender.Clear;

  item:= TSpTBXItem.Create(Sender);
  item.Caption:= _('Folder Path');
  item.Tag:= 1;
  item.OnClick:= OnSubClick;
  Sender.Add(item);

  item:= TSpTBXItem.Create(Sender);
  item.Caption:= _('Name Only');
  item.Tag:= 2;
  item.OnClick:= OnSubClick;
  Sender.Add(item);

  sep:= TSpTBXSeparatorItem.Create(Sender);
  Sender.Add(sep);

  item:= TSpTBXItem.Create(Sender);
  item.Caption:= _('Short Path');
  item.Tag:= 3;
  item.OnClick:= OnSubClick;
  Sender.Add(item);

  item:= TSpTBXItem.Create(Sender);
  item.Caption:= _('Short Folder Path');
  item.Tag:= 4;
  item.OnClick:= OnSubClick;
  Sender.Add(item);

  item:= TSpTBXItem.Create(Sender);
  item.Caption:= _('Short Name Only');
  item.Tag:= 5;
  item.OnClick:= OnSubClick;
  Sender.Add(item);
end;

{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEFileViewCopyPathButton.OnSubClick(Sender: TObject);
var
  fileView: TCEFileView;
  list: TTntStrings;
  i: Integer;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    case TComponent(Sender).Tag of
      1: TntClipboard.AsText:= IncludeTrailingBackslashW(fileView.RootFolderNamespace.NameForParsing);
      2: begin
           if fileview.Selection.Count > 1 then
           TntClipboard.AsText:= fileview.SelectedFiles.Text
           else if fileview.Selection.Count = 1 then
           TntClipboard.AsText:= fileview.SelectedFile
           else
           TntClipboard.AsText:= fileview.RootFolderNamespace.NameInFolder;
         end;
      3: begin
           if fileview.Selection.Count > 1 then
           begin
             list:= TTntStringList.Create;
             try
               list.Assign(fileview.SelectedPaths);
               for i:= 0 to list.Count - 1 do
               list.Strings[i]:= ShortPath(list.Strings[i]);
               TntClipboard.AsText:= list.Text;
             finally
               list.Free;
             end;
           end
           else if fileview.Selection.Count = 1 then
           TntClipboard.AsText:= ShortPath(fileview.SelectedPath)
           else
           TntClipboard.AsText:= ShortPath(IncludeTrailingBackslashW(fileview.RootFolderNamespace.NameForParsing));
         end;
      4: TntClipboard.AsText:= ShortPath(IncludeTrailingBackslashW(fileView.RootFolderNamespace.NameForParsing));
      5: begin
           if fileview.Selection.Count > 1 then
           begin
             list:= TTntStringList.Create;
             try
               list.Assign(fileview.SelectedPaths);
               for i:= 0 to list.Count - 1 do
               list.Strings[i]:= WideExtractFileName(ShortPath(list.Strings[i]));
               TntClipboard.AsText:= list.Text;
             finally
               list.Free;
             end;
           end
           else if fileview.Selection.Count = 1 then
           TntClipboard.AsText:= WideExtractFileName(ShortPath(fileview.SelectedPath))
           else
           TntClipboard.AsText:= WideExtractFileName(ShortPath(IncludeTrailingBackslashW(fileview.RootFolderNamespace.NameForParsing)));
         end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEArrangeByButton
-------------------------------------------------------------------------------}
constructor TCEArrangeByButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do ArrangeBy Button Popup
-------------------------------------------------------------------------------}
procedure TCEArrangeByButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  fileView: TCEFileView;
  item: TSpTBXItem;
  col: TEasyColumn;
begin
  Sender.Clear;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

    col:= fileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= TSpTBXItem.Create(Sender);
      item.Caption:= col.Caption;
      item.OnClick:= OnSubClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      if col.SortDirection <> esdNone then
      item.Checked:= true;
      Sender.Add(item);
      col:= fileView.Header.NextVisibleColumn(col);
    end;
    // Separator
    Sender.Add(TSpTBXSeparatorItem.Create(Sender));
    // More... item
    item:= TSpTBXItem.Create(Sender);
    item.Caption:= _('More...');
    item.Images:= CE_Images.SmallIcons;
    item.Tag:= -1;
    item.OnClick:= OnSubClick;
    Sender.Add(item);    
  end;
end;

{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEArrangeByButton.OnSubClick(Sender: TObject);
var
  item: TSpTBXItem;
  col, tmpCol: TEasyColumn;
  view: TCECustomFileView;
begin
  item:= TSpTBXItem(Sender);
  if item.Tag = -1 then
  begin
    if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    begin
      TCEFileViewPage(GlobalPathCtrl.ActivePage).ShowHeaderSelector;
    end;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    view:= TCECustomFileView(col.OwnerListview);
    view.BeginUpdate;
    try
      tmpCol:= view.Header.FirstColumn;
      while assigned(tmpCol) do
      begin
        if tmpCol <> col then
        tmpCol.SortDirection:= esdNone
        else
        tmpCol.SortDirection:= esdAscending;
        tmpCol:= view.Header.NextColumn(tmpCol);
      end;
    finally
      view.EndUpdate(true);
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEViewStyleButton
-------------------------------------------------------------------------------}
constructor TCEViewStyleButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
  Populate;
end;

{-------------------------------------------------------------------------------
  Popuplate sub menu
-------------------------------------------------------------------------------}
procedure TCEViewStyleButton.Populate;
var
  item: TSpTBXItem;
begin
  Self.Clear;
  // Large Icons
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_large;
  Self.Add(item);
  // Small Icons
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_small;
  Self.Add(item);
  // List
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_list;
  Self.Add(item);
  // Details
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_details;
  Self.Add(item);
  // Tiles
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_tiles;
  Self.Add(item);
  // Thumbnails
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_thumbs;
  Self.Add(item);
  // Filmstrip
  item:= TSpTBXItem.Create(Self);
  item.Action:= CEActions.act_view_filmstrip;
  Self.Add(item);
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEGroupByButton
-------------------------------------------------------------------------------}
constructor TCEGroupByButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do GroupBy Button Popup
-------------------------------------------------------------------------------}
procedure TCEGroupByButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  fileView: TCEFileView;
  item: TSpTBXItem;
  col: TEasyColumn;
begin
  Sender.Clear;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

    // Grouped toggle item
    item:= TSpTBXItem.Create(Sender);
    item.Caption:= _('Show in Groups');
    item.Images:= CE_Images.SmallIcons;
    item.Tag:= -2;
    item.Checked:= fileView.Grouped;
    item.OnClick:= OnSubClick;
    Sender.Add(item);
    // Separator
    Sender.Add(TSpTBXSeparatorItem.Create(Sender));
    // Group By items
    col:= fileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= TSpTBXItem.Create(Sender);
      item.Caption:= col.Caption;
      item.OnClick:= OnSubClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      if fileView.GroupingColumn = col.Index then
      item.Checked:= true;
      Sender.Add(item);
      col:= fileView.Header.NextVisibleColumn(col);
    end;
    // Separator
    Sender.Add(TSpTBXSeparatorItem.Create(Sender));
    // More... item
    item:= TSpTBXItem.Create(Sender);
    item.Caption:= _('More...');
    item.Images:= CE_Images.SmallIcons;
    item.Tag:= -1;
    item.OnClick:= OnSubClick;
    Sender.Add(item);    
  end;
end;

{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEGroupByButton.OnSubClick(Sender: TObject);
var
  item: TSpTBXItem;
  col: TEasyColumn;
  page: TCEFileViewPage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  page:= TCEFileViewPage(GlobalPathCtrl.ActivePage)
  else
  Exit;

  item:= TSpTBXItem(Sender);
  if item.Tag = -2 then
  begin
    page.FileView.Grouped:= not page.FileView.Grouped;
  end
  else if item.Tag = -1 then
  begin
    page.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    if assigned(col) then
    begin
      page.FileView.GroupingColumn:= col.Index;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEMainMenuButton
-------------------------------------------------------------------------------}
constructor TCEMainMenuButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
  Self.LinkSubitems:= MainForm.MainToolbar.Items;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEBookmarksButton
-------------------------------------------------------------------------------}
constructor TCEBookmarksButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
  Self.LinkSubitems:= MainForm.bookmarkMenuItem;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCESessionsButton
-------------------------------------------------------------------------------}
constructor TCESessionsButton.Create(AOwner: TComponent);
var
  item: TSpTBXItem;
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
  // sessions group
  Add(TCESessionsMenuItem.Create(self));
  // separator item
  Add(TSpTBXSeparatorItem.Create(self));
  // save item
  item:= TSpTBXItem.Create(self);
  item.CustomHeight:= 24;
  item.Action:= CEActions.act_sessions_save;
  Add(item);
  // manage item
  item:= TSpTBXItem.Create(self);
  item.CustomHeight:= 24;
  item.Action:= CEActions.act_sessions_manage;
  Add(item);
end;

{##############################################################################}
// TCEClosedTabsListButton

{*------------------------------------------------------------------------------
  Create an instance of TCEClosedTabsListButton
-------------------------------------------------------------------------------}
constructor TCEClosedTabsListButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= true;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do Popup
-------------------------------------------------------------------------------}
procedure TCEClosedTabsListButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  MainForm.TabSet.PopulateUndoList(Self);
end;

{##############################################################################}
// TCEFiltersMenuButton

{*------------------------------------------------------------------------------
  Create an instance of TCEFiltersMenuButton
-------------------------------------------------------------------------------}
constructor TCEFiltersMenuButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do NewFile Button Popup
-------------------------------------------------------------------------------}
procedure TCEFiltersMenuButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  if not CEFiltersPanel.Filters.Active then
  CEFiltersPanel.Filters.Active:= true;
  CEFiltersPanel.PopulateMenuItem(Self);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of PatternNotifyList
-------------------------------------------------------------------------------}
constructor TCEFilterPatternItem.Create(AOwner: TComponent);
begin
  inherited;
  CEFiltersPanel.PatternNotifyList.Add(Self);
  Self.AutoShowClearButton:= true;
end;

{-------------------------------------------------------------------------------
  Destroy PatternNotifyList
-------------------------------------------------------------------------------}
destructor TCEFilterPatternItem.Destroy;
begin
  if assigned(CEFiltersPanel) then
  CEFiltersPanel.PatternNotifyList.Remove(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Change
-------------------------------------------------------------------------------}
procedure TCEFilterPatternItem.DoChange(const AText: WideString);
begin
  inherited;
  CEFiltersPanel.PatternText:= AText;
end;

{-------------------------------------------------------------------------------
  Do ClearButtonClick
-------------------------------------------------------------------------------}
procedure TCEFilterPatternItem.DoClearButtonClick(Shift: TShiftState);
begin
  inherited;
  CEFiltersPanel.PatternText:= '';
end;

{-------------------------------------------------------------------------------
  Get ComboItems
-------------------------------------------------------------------------------}
function TCEFilterPatternItem.GetComboItems: TTntStrings;
begin
  Result:= CEFiltersPanel.PatternHistory;
end;

{-------------------------------------------------------------------------------
  KeyPress
-------------------------------------------------------------------------------}
procedure TCEFilterPatternItem.KeyPress(var Key: Char);
begin
  // Add text to filter history
  if (Key = #13) and (Self.Text <> '') then
  begin
    CEFiltersPanel.PatternHistory.Insert(0, Self.Text);
    if Self.Viewer is TCEToolbarComboBoxItemViewer then
    begin
      if assigned(TCEToolbarComboBoxItemViewerAccess(Self.Viewer).fComboBox) then
      begin
        TCEToolbarComboBoxItemViewerAccess(Self.Viewer).fComboBox.Items.Assign(GetComboItems);
      end;
    end;
  end;
  inherited;
end;

{##############################################################################}
// TCEUndoDeleteButton

{-------------------------------------------------------------------------------
  Destroy TCEUndoDeleteButton
-------------------------------------------------------------------------------}
destructor TCEUndoDeleteButton.Destroy;
begin
  ClearItems;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear Items
-------------------------------------------------------------------------------}
procedure TCEUndoDeleteButton.ClearItems(OnlyTrashItems: Boolean = false);
var
  i: Integer;
  item: TTBCustomItem;
begin
  Self.ViewBeginUpdate;
  try
    if not OnlyTrashItems then
    begin
      fRestoreListItem:= nil;
      Self.Clear;
    end
    else
    begin
      i:= 0;
      while i < Self.Count do
      begin
        item:= Self.Items[i];
        if (item is TSpTBXItem) and (item.Tag > 0) then
        begin
          Self.Delete(i);
        end
        else
        i:= i + 1;
      end;
    end;
  finally
    Self.ViewEndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Do Popup
-------------------------------------------------------------------------------}
procedure TCEUndoDeleteButton.DoPopup(Sender: TTBCustomItem; FromLink: Boolean);
var
  item, subItem: TSpTBXItem;
  i: Integer;
  ws: WideString;
  ns: TNamespace;
begin
  ClearItems;
  CERecycleBinCtrl.RefreshList;
  // Add Recycle Bin Item
  subItem:= TCEToolbarSubmenuItem.Create(Self);
  subItem.Images:= CE_Images.SmallIcons;
  subItem.Caption:= _('Recycle Bin');
  subItem.Tag:= -1;
  subItem.OnClick:= OnSubClick;
  if CERecycleBinCtrl.IsRecycleBinEmpty then
  subItem.ImageIndex:= 23
  else
  subItem.ImageIndex:= 24;
  Sender.Add(subItem);

    // Add Empty Recycle Bin item
    item:= TSpTBXItem.Create(subItem);
    item.Action:= CEActions.act_tools_emptytrash;
    item.Images:= CE_Images.SmallIcons;
    subItem.Add(item);
    // Add Separator
    subItem.Add(TSpTBXSeparatorItem.Create(subItem));
    // Add Restore List
    item:= TSpTBXItem.Create(subItem);
    item.Caption:= _('Restore List') + ' (' + IntToStr(CERecycleBinCtrl.Items.Count) + ')';
    item.Images:= CE_Images.SmallIcons;
    item.Tag:= -2;
    item.OnClick:= OnSubClick;
    subItem.Add(item);
    fRestoreListItem:= item;
    // Add Restore All
    item:= TSpTBXItem.Create(subItem);
    item.Caption:= _('Restore All') + ' (' + IntToStr(CERecycleBinCtrl.TotalItemCount) + ')';
    item.Images:= CE_Images.SmallIcons;
    item.Tag:= -3;
    item.OnClick:= OnSubClick;
    subItem.Add(item);
    
  // Add Separator
  Sender.Add(TSpTBXSeparatorItem.Create(Self));
  // Populate items
  for i:= 0 to CERecycleBinCtrl.Items.Count - 1 do
  begin
    ns:= CERecycleBinCtrl.Items.Items[i];
    item:= TSpTBXItem.Create(Self);
    item.Tag:= Integer(ns);
    ws:= ns.NameInFolder + #9 + ns.DetailsOf(CERecycleBinCtrl.SortColumn);
    item.Caption:= ws;
    item.Images:= SmallSysImages;
    item.ImageIndex:= ns.GetIconIndex(false, icSmall);
    item.OnClick:= OnSubClick;
    Sender.Add(item);
  end;
end;

{*------------------------------------------------------------------------------
  On Submenu item Click
-------------------------------------------------------------------------------}
procedure TCEUndoDeleteButton.OnSubClick(Sender: TObject);
var
  item: TSpTBXItem;
  itemNS: TNamespace;
begin
  item:= TSpTBXItem(Sender);
  // Restore item
  if item.Tag > 0 then
  begin
    itemNS:= TNamespace(item.Tag);
    if CERecycleBinCtrl.Items.IndexOf(itemNS) > -1 then // make sure the item is still in the list.
    begin
      CERecycleBinCtrl.Restore(itemNS);
    end;
  end
  // Open Recycle Bin
  else if item.Tag = -1 then
  begin
    OpenFolderInTab(Self, CERecycleBinCtrl.RecycleBinNS.AbsolutePIDL);
  end
  // Restore List
  else if item.Tag = -2 then
  begin
    CERecycleBinCtrl.RestoreList;
  end
  // Restore All
  else if item.Tag = -3 then
  begin
    CERecycleBinCtrl.RestoreAll;
  end;
  ClearItems;
  CERecycleBinCtrl.Clear;
end;

{-------------------------------------------------------------------------------
  Refresh Items
-------------------------------------------------------------------------------}
procedure TCEUndoDeleteButton.RefreshItems;
var
  item: TSpTBXItem;
  ns: TNamespace;
  i: Integer;
  ws: WideString;
begin
  Self.ViewBeginUpdate;
  try
    ClearItems(true);
    CERecycleBinCtrl.RefreshList;
    // Populate items
    for i:= 0 to CERecycleBinCtrl.Items.Count - 1 do
    begin
      ns:= CERecycleBinCtrl.Items.Items[i];
      item:= TSpTBXItem.Create(Self);
      item.Tag:= Integer(ns);
      ws:= ns.NameInFolder + #9 + ns.DetailsOf(CERecycleBinCtrl.SortColumn);
      item.Caption:= ws;
      item.Images:= SmallSysImages;
      item.ImageIndex:= ns.GetIconIndex(false, icSmall);
      item.OnClick:= OnSubClick;
      Self.Add(item);
    end;
  finally
    if assigned(fRestoreListItem) then
    begin
      fRestoreListItem.Caption:= _('Restore List') + ' (' + IntToStr(CERecycleBinCtrl.Items.Count) + ')';
    end;
    Self.ViewEndUpdate;
  end;
end;


{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEButtonSettings
-------------------------------------------------------------------------------}
constructor TCEButtonSettings.Create;
begin
  inherited Create;
  fTabs_ShowTabControls:= true;
end;

{-------------------------------------------------------------------------------
  Get/Set UndoDelete_ItemCount
-------------------------------------------------------------------------------}
function TCEButtonSettings.GetUndoDelete_ItemCount: Integer;
begin
  Result:= CERecycleBinCtrl.ItemNumberLimit;
end;
procedure TCEButtonSettings.SetUndoDelete_ItemCount(const Value: Integer);
begin
  CERecycleBinCtrl.ItemNumberLimit:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set UndoDelete_ConfirmRestore
-------------------------------------------------------------------------------}
function TCEButtonSettings.GetUndoDelete_ConfirmRestore: Boolean;
begin
  Result:= CERecycleBinCtrl.ConfirmRestore;
end;
procedure TCEButtonSettings.SetUndoDelete_ConfirmRestore(const Value: Boolean);
begin
  CERecycleBinCtrl.ConfirmRestore:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set UndoDelete_DayLimit
-------------------------------------------------------------------------------}
function TCEButtonSettings.GetUndoDelete_DayLimit: Integer;
begin
  Result:= CERecycleBinCtrl.DayLimit;
end;
procedure TCEButtonSettings.SetUndoDelete_DayLimit(const Value: Integer);
begin
  CERecycleBinCtrl.DayLimit:= Value;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCESystemPowerButton
-------------------------------------------------------------------------------}
constructor TCESystemPowerButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
  Populate;
end;

{-------------------------------------------------------------------------------
  Handle Item Click
-------------------------------------------------------------------------------}
procedure TCESystemPowerButton.HandleItemClick(Sender: TObject);
begin
  case TSpTBXItem(Sender).Tag of
    1: LockWorkStation;
    2: WindowsExit(EWX_LOGOFF);
    3: SetSuspendState(false, false, false);
    4: SetSuspendState(true, false, false);
    5: WindowsExit(EWX_REBOOT);
    6: WindowsExit(EWX_POWEROFF);
  end;
end;

{-------------------------------------------------------------------------------
  Popuplate sub menu
-------------------------------------------------------------------------------}
procedure TCESystemPowerButton.Populate;
var
  item: TSpTBXItem;
begin
  Self.Clear;
  // Lock
  item:= TSpTBXItem.Create(Self);
  item.Caption:= _('Lock');
  item.Images:= CE_Images.SmallIcons;
  item.OnClick:= HandleItemClick;
  item.Tag:= 1;
  Self.Add(item);
  // Logoff
  item:= TSpTBXItem.Create(Self);
  item.Caption:= _('Logoff');
  item.Images:= CE_Images.SmallIcons;
  item.OnClick:= HandleItemClick;
  item.Tag:= 2;
  Self.Add(item);
  // ----
  if IsPwrSuspendAllowed or IsHibernateAllowed then
  Self.Add(TSpTBXSeparatorItem.Create(Self));
  // Sleep
  if IsPwrSuspendAllowed then
  begin
    item:= TSpTBXItem.Create(Self);
    item.Caption:= _('Sleep');
    item.Images:= CE_Images.SmallIcons;
    item.OnClick:= HandleItemClick;
    item.Tag:= 3;
    Self.Add(item);
  end;
  // Hibernate
  if IsHibernateAllowed then
  begin
    item:= TSpTBXItem.Create(Self);
    item.Caption:= _('Hibernate');
    item.Images:= CE_Images.SmallIcons;
    item.OnClick:= HandleItemClick;
    item.Tag:= 4;
    Self.Add(item);
  end;
  // ----
  Self.Add(TSpTBXSeparatorItem.Create(Self));
  // Reboot
  item:= TSpTBXItem.Create(Self);
  item.Caption:= _('Reboot');
  item.Images:= CE_Images.SmallIcons;
  item.OnClick:= HandleItemClick;
  item.Tag:= 5;
  Self.Add(item);
  // Shut down
  item:= TSpTBXItem.Create(Self);
  item.Caption:= _('Shut down');
  item.Images:= CE_Images.SmallIcons;
  item.OnClick:= HandleItemClick;
  item.Tag:= 6;
  Self.Add(item);
end;

{##############################################################################}
// TCETabsButton

{*------------------------------------------------------------------------------
  Create an instance of TCETabsButton
-------------------------------------------------------------------------------}
constructor TCETabsButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do Button Popup
-------------------------------------------------------------------------------}
procedure TCETabsButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  tab: TCESpTabItem;
  item: TSpTBXItem;
  sub: TCEToolbarSubmenuItem;
  sep: TSpTBXSeparatorItem;
  i,idx: Integer;
begin
  Self.Clear;

  if GlobalButtonSettings.Tabs_ShowTabControls then
  begin
    // add "Add Tab" item
    sub:= TCEToolbarSubmenuItem.Create(Self);
    sub.Action:= CEActions.act_tabs_addtab;
    sub.Images:= CE_Images.SmallIcons;
    Self.Add(sub);
      // add "Duplicate Tab" item
      item:= TSpTBXItem.Create(Self);
      item.Action:= CEActions.act_tabs_duplicatetab;
      item.Images:= CE_Images.SmallIcons;
      sub.Add(item);
    // add "Close Tab" item
    sub:= TCEToolbarSubmenuItem.Create(Self);
    sub.Action:= CEActions.act_tabs_closetab;
    sub.Images:= CE_Images.SmallIcons;
    Self.Add(sub);
      // add "Undo Tab Close" item
      item:= TCEClosedTabsListButton.Create(Self);
      item.Action:= CEActions.act_tabs_undo;
      item.Images:= CE_Images.SmallIcons;
      sub.Add(item);
      // add Separator
      sep:= TSpTBXSeparatorItem.Create(Self);
      sub.Add(sep);
      // add "Close Other Tabs" item
      item:= TSpTBXItem.Create(Self);
      item.Action:= CEActions.act_tabs_closeothertabs;
      item.Images:= CE_Images.SmallIcons;
      sub.Add(item);
      // add "Close Tabs on Left" item
      item:= TSpTBXItem.Create(Self);
      item.Action:= CEActions.act_tabs_closeonleft;
      item.Images:= CE_Images.SmallIcons;
      sub.Add(item);
      // add "Close Tabs on Right" item
      item:= TSpTBXItem.Create(Self);
      item.Action:= CEActions.act_tabs_closeonright;
      item.Images:= CE_Images.SmallIcons;
      sub.Add(item);

    // add Separator
    sep:= TSpTBXSeparatorItem.Create(Self);
    Self.Add(sep);
  end;

  // add tab items
  idx:= 0;
  for i:= 0 to MainForm.TabSet.Items.Count - 1 do
  begin
    if MainForm.TabSet.Items.Items[i] is TCESpTabItem then                      
      
    begin
      idx:= idx + 1;
      tab:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
      item:= TSpTBXItem.Create(Self);
      item.Caption:=  ' ' + IntToStr(idx) + ': ' + tab.Caption;
      item.Hint:= tab.Hint;
      item.Options:= [tboShowHint, tboToolbarStyle];
      item.Alignment:= taLeftJustify;
      item.Images:= tab.Images;
      item.ImageIndex:= tab.ImageIndex;
      item.Tag:= Integer(tab);
      item.Checked:= tab.Checked;
      if item.Checked then
      item.FontSettings.Style:= [fsBold];
      item.OnClick:= DoSubClick;  
      Self.Add(item);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do SubClick
-------------------------------------------------------------------------------}
procedure TCETabsButton.DoSubClick(Sender: TObject);
begin
  MainForm.TabSet.SelectTab(TSpTBXTabItem(TSpTBXItem(Sender).Tag));
end;

{##############################################################################}
// TCEStackOpenButton

{*------------------------------------------------------------------------------
  Create an instance of TCEStackOpenButton
-------------------------------------------------------------------------------}
constructor TCEStackOpenButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do Button Popup
-------------------------------------------------------------------------------}
procedure TCEStackOpenButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  CEStackPanel.PopulateStackOpenMenuItem(Sender);
end;

{##############################################################################}
// TCEStackSaveButton

{*------------------------------------------------------------------------------
  Create an instance of TCEStackSaveButton
-------------------------------------------------------------------------------}
constructor TCEStackSaveButton.Create(AOwner: TComponent);
begin
  inherited;
  Self.DropdownCombo:= false;
  Self.Options:= [tboDropdownArrow];
end;

{*------------------------------------------------------------------------------
  Do Button Popup
-------------------------------------------------------------------------------}
procedure TCEStackSaveButton.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  CEStackPanel.PopulateStackSaveMenuItem(Sender, true);
end;

{##############################################################################}

initialization
  GlobalButtonSettings:= TCEButtonSettings.Create;
  GlobalAppSettings.AddItem('Buttons', GlobalButtonSettings, true);

finalization
  FreeAndNil(GlobalButtonSettings);


end.
