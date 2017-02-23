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
//  The Original Code is fCE_FiltersPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//
//******************************************************************************

unit fCE_FiltersPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_FilterPanel, CE_VistaFuncs, CE_GlobalCtrl,
  fCE_FileView, dCE_Images, CE_AppSettings, fCE_SearchPage,
  dCE_Actions,
  // TB2k, TBX, SpTBX
  TB2Dock, SpTBXItem,
  // Virtual Trees
  VirtualTrees,
  // VSTools
  VirtualExplorerEasyListview,
  // Graphics32
  GR32_Image, GR32,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, TB2Item, SpTBXEditors, TB2Toolbar, StdCtrls,
  TntStdCtrls, ExtCtrls, SpTBXSkins, Contnrs, CE_Toolbar, TntClasses;

type
  TControlHack = class(TControl);

  TCEFiltersPanelSettings = class;

  TCEFiltersPanel = class(TCECustomDockableForm)
    Images: TBitmap32List;
    FiltersPopupMenu: TSpTBXPopupMenu;
    check_resetfilters: TSpTBXItem;
    PatternFilterTimer: TTimer;
    but_clear_filterhistory: TSpTBXItem;
    FiltersToolbar: TCEToolbar;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure check_resetfiltersClick(Sender: TObject);
    procedure FiltersPopupMenuPopup(Sender: TObject);
    procedure but_clear_filterhistoryClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DoPatternFilter(Sender: TObject);
  private
    fPatternHistory: TTntStrings;
    fPatternNotifyInProgress: Boolean;
    fPatternText: WideString;
    fSettings: TCEFiltersPanelSettings;
  protected
    fDock: TSpTBXDock;
    FilterBackgroundBitmap: TBitmap;
    procedure DoMenuEditChange(Sender: TObject; const AText: WideString); virtual;
    procedure DoMenuItemClick(Sender: TObject); virtual;
    procedure DoPatternChanged; virtual;
    procedure DrawFilterBitmap;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); override;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); override; stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure SetPatternText(const Value: WideString); virtual;
  public
    Filters: TCEFilterList;
    PatternNotifyList: TObjectList;
    procedure ClearFilters;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
    procedure DoStartUp; override;
    procedure PopulateMenuItem(AItem: TSpTBXItem);
    property PatternHistory: TTntStrings read fPatternHistory;
    property PatternText: WideString read fPatternText write SetPatternText;
  published
    property Settings: TCEFiltersPanelSettings read fSettings write fSettings;
  end;

  TCEFiltersPanelSettings = class(TPersistent)
  private
    fAutoResetFilters: Boolean;
    fFontSize: Integer;
    fLineHeight: Integer;
    fSaveFilterHistory: Boolean;
    function GetFilterHistory: WideString;
    function GetShowBkgrd: Boolean;
    function GetStrictFilter: Boolean;
    procedure SetFilterHistory(const Value: WideString);
    procedure SetFontSize(const Value: Integer);
    procedure SetLineHeight(const Value: Integer);
    procedure SetSaveFilterHistory(const Value: Boolean);
    procedure SetShowBkgrd(const Value: Boolean);
    procedure SetStrictFilter(const Value: Boolean);
  public
    FilterPanel: TCEFiltersPanel;
  published
    property AutoResetFilters: Boolean read fAutoResetFilters write
        fAutoResetFilters;
    property FilterHistory: WideString read GetFilterHistory write SetFilterHistory;
    property FontSize: Integer read fFontSize write SetFontSize;
    property LineHeight: Integer read fLineHeight write SetLineHeight;
    property SaveFilterHistory: Boolean read fSaveFilterHistory write
        SetSaveFilterHistory;
    property ShowBkgrd: Boolean read GetShowBkgrd write SetShowBkgrd;
    property StrictFilter: Boolean read GetStrictFilter write SetStrictFilter;
  end;

var
  CEFiltersPanel: TCEFiltersPanel;

implementation

uses
  CE_LanguageEngine, CE_Utils, CE_Layout, CE_ToolbarButtons, Main;
{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called on Create
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FormCreate(Sender: TObject);
begin
  inherited;
  fDock:= TopDock;
  TopDock.Name:= 'FiltersPanel_TopDock';
  BottomDock.Name:= 'FiltersPanel_BottomDock';

  fPatternHistory:= TTntStringList.Create;
  fPatternHistory.Delimiter:= ',';

  fSettings:= TCEFiltersPanelSettings.Create;
  fSettings.FilterPanel:= Self;
  FilterBackgroundBitmap:= TBitmap.Create;
  DrawFilterBitmap;
  Filters:= TCEFilterList.Create(self);
  Filters.Parent:= self;
  Filters.Align:= alClient;
  Filters.FilteringImage:= FilterBackgroundBitmap;
  Filters.PopupMenu:= FiltersPopupMenu;
  SetDesktopIconFonts(Filters.Font);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 29;
  GlobalFocusCtrl.CtrlList.Add(Filters);
  TControlHack(Filters).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalPathCtrl.RegisterNotify(self);

  GlobalAppSettings.AddItem('FilterPanel', fSettings, true);

  // Default settings
  Settings.StrictFilter:= false;
  Settings.SaveFilterHistory:= true;
  Settings.ShowBkgrd:= true;
  Settings.AutoResetFilters:= true;
  Settings.fFontSize:= -1;
  Settings.fLineHeight:= -1;

  PatternNotifyList:= TObjectList.Create(false);
  fPatternNotifyInProgress:= false;

  CELayoutItems.Add(FiltersToolbar);
end;

{*------------------------------------------------------------------------------
  Get's called on Destroy
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FormDestroy(Sender: TObject);
begin
  if Self = CEFiltersPanel then
  CEFiltersPanel:= nil;

  FilterBackgroundBitmap.Free;
  fSettings.Free;
  PatternNotifyList.Free;
  fPatternHistory.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  On Form Resize
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FormResize(Sender: TObject);
begin
  fDock.Width:= ClientWidth;
end;

{*------------------------------------------------------------------------------
  Draw filtering background image
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DrawFilterBitmap;
var
  b: TBitmap32;
begin
  b:= TBitmap32.Create;
  try
    b.SetSizeFrom(Images.Bitmap[0]);
    b.DrawMode:= dmBlend;
    b.Clear(Color32(clWindow));
    b.Draw(0,0, Images.Bitmap[0]);
    FilterBackgroundBitmap.SetSize(b.Width, b.Height);
    b.DrawTo(FilterBackgroundBitmap.Canvas.Handle,0,0);
  finally
    b.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  if assigned(NewPage) then
  begin
    if NewPage is TCEFileViewPage then
    begin
      if Settings.AutoResetFilters then
      begin
        PatternFilterTimer.Enabled:= false;
        fPatternText:= '';
        DoPatternChanged;
        Filters.ClearFilters;
      end;
      Filters.ExplorerEasyListview:= TCEFileViewPage(NewPage).FileView;
    end
    else if NewPage is TCESearchPage then
    begin
      if Settings.AutoResetFilters then
      begin
        PatternFilterTimer.Enabled:= false;
        fPatternText:= '';
        DoPatternChanged;
        Filters.ClearFilters;
      end;
      Filters.ExplorerEasyListview:= TCESearchPage(NewPage).ResultView;
    end
    else
    begin
      Filters.ExplorerEasyListview:= nil;
      Filters.Clear;
    end;
  end
  else
  begin
    Filters.ExplorerEasyListview:= nil;
    Filters.Clear;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalContentChange(Sender: TObject);
begin
  if Filters.Active then
  begin
    Filters.DoFiltering(true);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoFormShow;
begin
  inherited;
  Filters.Active:= true;
  FormResize(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoFormHide;
begin
  inherited;
  // TODO: make filters deactivate when panel is hidden but not when auto hidden.
  //Filters.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if Settings.AutoResetFilters then
  Filters.ClearFilters;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  if Settings.AutoResetFilters then
  begin
    PatternFilterTimer.Enabled:= false;
    fPatternText:= '';
    DoPatternChanged;
    Filters.ClearFilters;
  end;
end;

{-------------------------------------------------------------------------------
  On but_clear_filterhistory Click
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.but_clear_filterhistoryClick(Sender: TObject);
begin
  PatternHistory.Clear;
end;

{-------------------------------------------------------------------------------
  On check_resetfilter click
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.check_resetfiltersClick(Sender: TObject);
begin
  Settings.AutoResetFilters:= not Settings.AutoResetFilters;
end;

{-------------------------------------------------------------------------------
  ClearFilters
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.ClearFilters;
begin
  PatternFilterTimer.Enabled:= false;
  Filters.ClearFilters;
  fPatternText:= '';
  DoPatternChanged;
  GlobalFileViewSettings.ClearFilters;
end;

{-------------------------------------------------------------------------------
  Do MenuEditChange
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoMenuEditChange(Sender: TObject; const AText:
    WideString);
begin
  PatternText:= AText;
end;

{-------------------------------------------------------------------------------
  Do MenuItemClick
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoMenuItemClick(Sender: TObject);
var
  node, node2: PVirtualNode;
begin
  // Clear Filters
  if TSpTBXItem(Sender).Tag = -1 then
  begin
    CEActions.act_filters_clear.Execute;
  end
  // Other items
  else
  begin
    node:= Pointer(TSpTBXItem(Sender).Tag);
    // Just to be safe, make sure that the node is still in the tree
    node2:= Filters.GetFirst;
    while assigned(node2) do
    begin
      if node2 = node then
      begin
        if node2.CheckState = csCheckedNormal then
        Filters.CheckState[node2]:= csUncheckedNormal
        else
        Filters.CheckState[node2]:= csCheckedNormal;
        break;
      end;
      node2:= Filters.GetNext(node2);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do PatternChange
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoPatternChanged;
var
  i: Integer;
begin
  if fPatternNotifyInProgress then
  Exit;

  fPatternNotifyInProgress:= true;
  try
    for i:= 0 to PatternNotifyList.Count - 1 do
    begin
      if PatternNotifyList.Items[i] is TSpTBXEditItem then
      TSpTBXEditItem(PatternNotifyList.Items[i]).Text:= PatternText;
    end;
  finally
    fPatternNotifyInProgress:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Do Pattern Filter
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoPatternFilter(Sender: TObject);
begin
  inherited;
  PatternFilterTimer.Enabled:= false;
  Filters.PatternText:= PatternText;
  DoPatternChanged;
end;

{-------------------------------------------------------------------------------
  Do StartUp
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoStartUp;
var
  item: TSpTBXCustomItem;
begin
  // Create default toolbar items
  // - pattern field
  item:= TCEFilterPatternItem.Create(FiltersToolbar);
  item.Action:= CEActions.act_filters_pattern;
  FiltersToolbar.Items.Add(item);
  // - stretcher
  item:= TCEToolbarStretcherItem.Create(FiltersToolbar);
  FiltersToolbar.Items.Add(item);
  // - exclude
  item:= TSpTBXItem.Create(FiltersToolbar);
  item.Action:= CEActions.act_filters_exclude;
  FiltersToolbar.Items.Add(item);
  // - strict
  item:= TSpTBXItem.Create(FiltersToolbar);
  item.Action:= CEActions.act_filters_strict;
  FiltersToolbar.Items.Add(item);
  // - clear
  item:= TSpTBXItem.Create(FiltersToolbar);
  item.Action:= CEActions.act_filters_clear;
  FiltersToolbar.Items.Add(item);

  FiltersToolbar.PopupMenu:= MainForm.ToolbarPopupMenu;
end;

{-------------------------------------------------------------------------------
  On FiltersPopupMenu popup
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FiltersPopupMenuPopup(Sender: TObject);
begin
  check_resetfilters.Checked:= Settings.AutoResetFilters;
end;

{-------------------------------------------------------------------------------
  PopulateMenuItem
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.PopulateMenuItem(AItem: TSpTBXItem);
var
  item: TSpTBXItem;
  edit: TSpTBXEditItem;
  node: PVirtualNode;
  data: PFilterItem;
  Text: WideString;
begin
  if assigned(AItem) then
  begin
    AItem.Clear;
    // Add Text filter edit
    edit:= TSpTBXEditItem.Create(AItem);
    edit.Text:= PatternText;
    edit.OnChange:= DoMenuEditChange;
    edit.Images:= CE_Images.SmallIcons;
    edit.EditImageIndex:= 29;
    AItem.Add(edit);
    // Add separator
    AItem.Add(TSpTBXSeparatorItem.Create(AItem));
    // Add exclude item
    item:= TSpTBXItem.Create(AItem);
    item.Action:= CEActions.act_filters_exclude;
    AItem.Add(item);
    // Add items from FilterList
    node:= Filters.GetFirst;
    while assigned(node) do
    begin
      data:= Filters.GetNodeData(node);
      item:= TSpTBXItem.Create(AItem);

      if data.ShowAllItem then
      Text:= _('Show All Files') + ' (' + IntToStr(data.count) + ')'
      else if data.ShowFoldersItem then
      Text:= _('Show Folders') + ' (' + IntToStr(data.count) + ')'
      else if data.Extension = 'none' then
      begin
        Text:= _('No Extension') + ' (' + IntToStr(data.count) + ')';
      end
      else
      Text:= data.extension + ' (' + IntToStr(data.count) + ')';

      item.Caption:= Text;
      item.Checked:= Filters.CheckState[node] = csCheckedNormal;
      item.Tag:= Integer(node);
      item.OnClick:= DoMenuItemClick;
      AItem.Add(item);

      // Add separator
      if data.ShowAllItem then
      begin
        AItem.Add(TSpTBXSeparatorItem.Create(AItem));
      end;
      node:= Filters.GetNext(node);
    end;
    // Add separator
    AItem.Add(TSpTBXSeparatorItem.Create(AItem));
    // Add Clear Filters
    item:= TSpTBXItem.Create(AItem);
    item.Action:= CEActions.act_filters_clear;
    AItem.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Set Pattern Text
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.SetPatternText(const Value: WideString);
begin
  if fPatternNotifyInProgress then
  Exit;
  
  if Value <> fPatternText then
  begin
    fPatternText:= Value;
    PatternFilterTimer.Enabled:= true;
  end;
end;


{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set ShowBkgrd
-------------------------------------------------------------------------------}
function TCEFiltersPanelSettings.GetShowBkgrd: Boolean;
begin
  Result:= FilterPanel.Filters.ShowFilteringBackground;
end;
procedure TCEFiltersPanelSettings.SetShowBkgrd(const Value: Boolean);
begin
  FilterPanel.Filters.ShowFilteringBackground:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set FilterHistory
-------------------------------------------------------------------------------}
function TCEFiltersPanelSettings.GetFilterHistory: WideString;
begin
  Result:= FilterPanel.PatternHistory.DelimitedText;
end;
procedure TCEFiltersPanelSettings.SetFilterHistory(const Value: WideString);
begin
  FilterPanel.PatternHistory.DelimitedText:= Value;
end;

{-------------------------------------------------------------------------------
  Set SaveFilterHistory
-------------------------------------------------------------------------------}
procedure TCEFiltersPanelSettings.SetSaveFilterHistory(const Value: Boolean);
begin
  fSaveFilterHistory:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set StrictFilter
-------------------------------------------------------------------------------}
function TCEFiltersPanelSettings.GetStrictFilter: Boolean;
begin
  Result:= FilterPanel.Filters.UseWildcards;
end;
procedure TCEFiltersPanelSettings.SetStrictFilter(const Value: Boolean);
begin
  FilterPanel.Filters.UseWildcards:= Value;
end;

{-------------------------------------------------------------------------------
  Set FontSize
-------------------------------------------------------------------------------}
procedure TCEFiltersPanelSettings.SetFontSize(const Value: Integer);
begin
  fFontSize:= Value;
  if fFontSize > 0 then
  FilterPanel.Filters.Font.Size:= fFontSize
  else
  SetDesktopIconFonts(FilterPanel.Filters.Font);
end;

{-------------------------------------------------------------------------------
  Set Line Height
-------------------------------------------------------------------------------}
procedure TCEFiltersPanelSettings.SetLineHeight(const Value: Integer);
var
  i: Cardinal;
  node: PVirtualNode;
begin
  fLineHeight:= Value;
  i:= FilterPanel.Filters.DefaultNodeHeight;
  if fLineHeight > 0 then
  FilterPanel.Filters.DefaultNodeHeight:= fLineHeight
  else
  FilterPanel.Filters.DefaultNodeHeight:= SmallShellIconSize + 1;

  // resize nodes
  if i <> FilterPanel.Filters.DefaultNodeHeight then
  begin
    FilterPanel.Filters.BeginUpdate;
    try
      node:= FilterPanel.Filters.GetFirstInitialized;
      while assigned(node) do
      begin
        FilterPanel.Filters.NodeHeight[node]:= FilterPanel.Filters.DefaultNodeHeight;
        node:= FilterPanel.Filters.GetNextInitialized(node);
      end;
    finally
      FilterPanel.Filters.EndUpdate;
    end;
  end;
end;

end.
