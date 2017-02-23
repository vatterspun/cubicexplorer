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
//  The Original Code is CE_FilterPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FilterPanel;

interface

uses
  // CE Units
  CE_LanguageEngine, CE_Utils,
  // Tnt Controls
  TntClasses, TntSysUtils,
  // VT
  VirtualTrees,
  // JVCL
  JvSimpleXml, JvAppStorage,    
  // VSTools
  VirtualExplorerEasyListview, EasyListview, MPShellUtilities,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, Forms, SpTBXItem;

type
  TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);

  PFilterItem = ^AFilterItem;
  AFilterItem = record
    Extension: WideString;
    Count: Integer;
    ShowAllItem: Boolean;
    ShowFoldersItem: Boolean;
  end;

  TCEFilterList = class(TVirtualStringTree, IJvAppStorageHandler)
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
  private
    fActive: Boolean;
    fFilteringImage: TBitmap;
    fExplorerEasyListview: TCustomVirtualExplorerEasyListview;
    fExcludeFromResults: Boolean;
    fPatternText: WideString;
    fRefreshItemCount: Boolean;
    fShowAllExtensions: Boolean;
    fShowFolders: Boolean;
    fShowAllNode: PVirtualNode;
    fShowFilteringBackground: Boolean;
    fShowFoldersNode: PVirtualNode;
    fUseWildcards: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetExplorerEasyListview(const Value:
        TCustomVirtualExplorerEasyListview);
    procedure SetExcludeFromResults(const Value: Boolean);
    procedure SetPatternText(const Value: WideString);
    procedure SetShowAllExtensions(const Value: Boolean);
    procedure SetShowFilteringBackground(const Value: Boolean);
    procedure SetShowFolders(const Value: Boolean);
    procedure SetUseWildcards(const Value: Boolean);
  protected
    procedure DoChecked(Node: PVirtualNode); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
        override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);
        override;
    procedure PopulateTree;
  public
    ActiveFilters: TTntStrings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearFilters;
    procedure DeFilter;
    procedure DoFiltering(ReCountExtensions: Boolean = true);
    function FindByExtension(ext: WideString): PVirtualNode;
    property Active: Boolean read fActive write SetActive;
    property FilteringImage: TBitmap read fFilteringImage write fFilteringImage;
    property ExplorerEasyListview: TCustomVirtualExplorerEasyListview read
        fExplorerEasyListview write SetExplorerEasyListview;
    property ExcludeFromResults: Boolean read fExcludeFromResults write
        SetExcludeFromResults;
    property PatternText: WideString read fPatternText write SetPatternText;
    property ShowAllExtensions: Boolean read fShowAllExtensions write
        SetShowAllExtensions;
    property ShowFilteringBackground: Boolean read fShowFilteringBackground write
        SetShowFilteringBackground;
    property ShowFolders: Boolean read fShowFolders write SetShowFolders;
    property UseWildcards: Boolean read fUseWildcards write SetUseWildcards;
  published
    property RefreshItemCount: Boolean read fRefreshItemCount write
        fRefreshItemCount;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEFilterList
-------------------------------------------------------------------------------}
constructor TCEFilterList.Create(AOwner: TComponent);
begin
  inherited;
  Self.NodeDataSize:= SizeOf(AFilterItem);
  ActiveFilters:= TTntStringList.Create;

  Self.BorderStyle:= bsNone;
  Self.CheckImageKind:= ckSystemDefault;
  Self.TreeOptions.PaintOptions:= [toHideFocusRect,toHideSelection,toShowButtons,toShowDropmark,toThemeAware,toAlwaysHideSelection]; //toShowHorzGridLines];
  Self.TreeOptions.MiscOptions:= [toCheckSupport,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick];
  fShowAllExtensions:= true;
  fShowFolders:= true;
  fShowFilteringBackground:= true;
  fActive:= false;
  fPatternText:= '';
  fUseWildcards:= false;
  fRefreshItemCount:= true;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFilterList
-------------------------------------------------------------------------------}
destructor TCEFilterList.Destroy;
begin
  ActiveFilters.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Clear Filters
-------------------------------------------------------------------------------}
procedure TCEFilterList.ClearFilters;
begin
  ActiveFilters.Clear;
  fShowAllExtensions:= true;
  fShowFolders:= true;
  fPatternText:= '';
  DoFiltering(true);
end;

{*------------------------------------------------------------------------------
  Find Node By Extension
-------------------------------------------------------------------------------}
function TCEFilterList.FindByExtension(ext: WideString): PVirtualNode;
var
  data: PFilterItem;
begin
  Result:= Self.GetFirst;
  while assigned(Result) do
  begin
    data:= Self.GetNodeData(Result);
    if (WideCompareText(data.extension, ext) = 0) and (Result <> fShowAllNode) and (Result <> fShowFoldersNode) then
    break
    else
    Result:= Result.NextSibling;
  end;
end;

{*------------------------------------------------------------------------------
  Populate tree
-------------------------------------------------------------------------------}
procedure TCEFilterList.PopulateTree;
var
  i: Integer;
  data, dataAllItem, dataFolderItem: PFilterItem;
  node: PVirtualNode;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;
  
  Self.BeginUpdate;
  Self.Clear;
  try
    // Add Show Folders node
    fShowFoldersNode:= Self.AddChild(nil);
    fShowFoldersNode.CheckType:= ctCheckBox;
    if ShowFolders then
    fShowFoldersNode.CheckState:= csCheckedNormal;
    dataFolderItem:= Self.GetNodeData(fShowFoldersNode);
    dataFolderItem.ShowFoldersItem:= true;
    // Add Show All node
    fShowAllNode:= Self.AddChild(nil);
    fShowAllNode.CheckType:= ctCheckBox;
    if ShowAllExtensions or (ActiveFilters.Count = 0) then
    fShowAllNode.CheckState:= csCheckedNormal;
    dataAllItem:= Self.GetNodeData(fShowAllNode);
    dataAllItem.ShowAllItem:= true;
    // Add Active Filter nodes
    for i:= 0 to ActiveFilters.Count - 1 do
    begin
      node:= Self.AddChild(nil);
      node.CheckType:= ctCheckBox;
      node.CheckState:= csCheckedNormal;
      data:= Self.GetNodeData(node);
      data.extension:= ActiveFilters.Strings[i];
      data.count:= 0;
    end;
  finally
    Self.EndUpdate;
    Self.SortTree(-1,sdAscending);
  end;
end;

{-------------------------------------------------------------------------------
  DoFiltering
-------------------------------------------------------------------------------}
procedure TCEFilterList.DoFiltering(ReCountExtensions: Boolean = true);
var
  pattern: WideString;

  // IncreaseExtCount
  procedure IncreaseExtCount(ns: TNamespace);
  var
    node: PVirtualNode;
    data: PFilterItem;
  begin
    if ns.Folder then 
    begin
      // update folder node
      data:= Self.GetNodeData(fShowFoldersNode);
      data.Count:= data.Count + 1;
    end
    else
    begin
      node:= FindByExtension(ns.Extension);
      if not assigned(node) then
      begin
        // add new extension node
        node:= Self.AddChild(nil);
        node.CheckType:= ctCheckBox;
        data:= Self.GetNodeData(node);
        data.Extension:= ns.Extension;
        data.Count:= 1;
      end
      else 
      begin
        // update existing extension node
        data:= Self.GetNodeData(node);
        data.Count:= data.Count + 1;
      end;
      // update all files node
      data:= Self.GetNodeData(fShowAllNode);
      data.Count:= data.Count + 1;
    end;
  end;

  // PatternFilterMatch
  function PatternFilterMatch(ns: TNamespace): Boolean;
  begin
    Result:= WideStringMatch(NS.NameParseAddressInFolder, pattern);
  end;

var
  i: Integer;
  item: TEasyItem;
  ns: TNamespace;
  NoFiltering: Boolean;
  view: TCustomVirtualExplorerEasyListviewHack;
  doPatternFiltering: Boolean;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;

  // Init values
  if UseWildcards then
  pattern:= PatternText
  else
  pattern:= '*' + PatternText + '*';
  view:= TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview);
  doPatternFiltering:= (PatternText <> '');
  NoFiltering:= true;

  if ReCountExtensions then
  begin
    Self.BeginUpdate;
    PopulateTree;
  end;
  view.BeginUpdate;
  try
    for i:= 0 to view.Items.Count-1 do
    begin
      item:= view.Items.Items[i];
      if view.ValidateNamespace(item, ns) then
      begin
        // Pattern filtering and ReCounting
        if doPatternFiltering then
        begin
          if ExcludeFromResults then
          item.Visible:= not PatternFilterMatch(ns)
          else
          item.Visible:= PatternFilterMatch(ns);

          if item.Visible then
          begin
            if ReCountExtensions then
            IncreaseExtCount(ns);
          end;
        end
        else
        begin
          item.Visible:= true;
          if ReCountExtensions then
          IncreaseExtCount(ns);
        end;

        // Extension filtering
        if item.Visible then
        begin
          // files
          if not ns.Folder then
          begin
            if not ExcludeFromResults then
              item.Visible:= fShowAllExtensions or
                             (ActiveFilters.IndexOf(ns.Extension) > -1)
            else
              item.Visible:= fShowAllExtensions or
                             (ActiveFilters.IndexOf(ns.Extension) = -1);
          end
          // folders
          else
          item.Visible:= fShowFolders;
        end;
      end;

      if NoFiltering then
      NoFiltering:= item.Visible;
    end;
  finally
    if ReCountExtensions then
    begin
      Self.SortTree(-1, sdAscending);
      Self.EndUpdate;
    end;

    if fShowFilteringBackground then
    view.BackGround.Enabled:= not NoFiltering;

    view.EndUpdate(true);
  end;
end;

{*------------------------------------------------------------------------------
  De-filter items (show all)
-------------------------------------------------------------------------------}
procedure TCEFilterList.DeFilter;
var
  i: Integer;
  view: TCustomVirtualExplorerEasyListviewHack;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;

  view:= TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview);
  view.BeginUpdate;
  try
    for i:= 0 to view.ItemCount - 1 do
    begin
      view.Items.Items[i].Visible:= true;
    end;
  finally
    if fShowFilteringBackground then
    view.BackGround.Enabled:= false;
    
    view.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  DoGetText
-------------------------------------------------------------------------------}
procedure TCEFilterList.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: WideString);
var
  data: PFilterItem;
begin
  data:= Self.GetNodeData(Node);
  if data.ShowAllItem then
  Text:= _('Show All Files') + ' (' + IntToStr(data.count) + ')'
  else if data.ShowFoldersItem then
  Text:= _('Show Folders') + ' (' + IntToStr(data.count) + ')'
  else if data.Extension = '' then
  begin
    Text:= _('No Extension') + ' (' + IntToStr(data.count) + ')';
  end
  else
  Text:= data.extension + ' (' + IntToStr(data.count) + ')';
end;

{*------------------------------------------------------------------------------
  DoChecked
-------------------------------------------------------------------------------}
procedure TCEFilterList.DoChecked(Node: PVirtualNode);
var
  n: PVirtualNode;
  data: PFilterItem;
begin
  data:= Self.GetNodeData(Node);
  // Show All Files item
  if data.ShowAllItem then
  begin
    ShowAllExtensions:= (Node.CheckState = csCheckedNormal) or (ActiveFilters.Count = 0);
    Self.Repaint;
  end
  // Show Folders item
  else if data.ShowFoldersItem then
  begin
    ShowFolders:= (Node.CheckState = csCheckedNormal);
    Self.Repaint;
  end
  // Extension item
  else
  begin
    Self.BeginUpdate;
    try
      n:= Self.GetFirst;
      ActiveFilters.Clear;
      while assigned(n) do
      begin
        if n.CheckState = csCheckedNormal then
        begin
          data:= Self.GetNodeData(n);
          if not data.ShowAllItem and not data.ShowFoldersItem then
          ActiveFilters.Add(data.extension);
        end;
        n:= n.NextSibling;
      end;

      // update Show All Files item
      fShowAllExtensions:= ActiveFilters.Count = 0;
      if assigned(fShowAllNode) then
      begin
        if fShowAllExtensions then
        fShowAllNode.CheckState:= csCheckedNormal
        else
        fShowAllNode.CheckState:= csUncheckedNormal;
      end;

      // filter (no recount needed)
      DoFiltering(false);
    finally
      Self.EndUpdate;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Do Compare
-------------------------------------------------------------------------------}
function TCEFilterList.DoCompare(Node1, Node2: PVirtualNode; Column:
    TColumnIndex): Integer;
var
  data1, data2: PFilterItem;
begin
  data1:= GetNodeData(Node1);
  data2:= GetNodeData(Node2);
  if data1.ShowFoldersItem then // "Show Folders" should be first
  Result:= -1
  else if data2.ShowFoldersItem then // "Show Folders" should be first
  Result:= 1
  else if data1.ShowAllItem then // "Show All Files" should be second
  Result:= -1
  else if data2.ShowAllItem then // "Show All Files" should be second
  Result:= 1
  else if data1.Extension = '' then // "No Extension" should be last
  Result:= 1
  else if data2.Extension = '' then // "No Extension" should be last
  Result:= -1
  else
  Result:= CompareText(data1.Extension, data2.Extension);
end;

{*------------------------------------------------------------------------------
  DoPaintText
-------------------------------------------------------------------------------}
procedure TCEFilterList.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
    Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PFilterItem;
begin
  inherited;

  data:= Self.GetNodeData(Node);
  // Show Folders item
  if data.ShowFoldersItem then
  begin
    if Node.CheckState = csCheckedNormal then
    Canvas.Font.Color:= clWindowText
    else
    Canvas.Font.Color:= clBtnShadow;
  end
  // Show All Files item
  else if fShowAllExtensions then
  begin
    if data.ShowAllItem then
    Canvas.Font.Color:= clWindowText
    else
    Canvas.Font.Color:= clBtnShadow;
  end
  // Extension item
  else
  begin
    if data.ShowAllItem then
    begin
      if ActiveFilters.Count = 0 then
      Canvas.Font.Color:= clWindowText
      else
      Canvas.Font.Color:= clBtnShadow;
    end
    else
    Canvas.Font.Color:= clWindowText;
  end;

  // Checked item
  if Node.CheckState = csCheckedNormal then
  begin
    if data.ShowAllItem or data.ShowFoldersItem then
    Canvas.Font.Style:= [fsBold,fsUnderline]
    else if not fShowAllExtensions then
    Canvas.Font.Style:= [fsBold];
  end
  // UnChecked item
  else
  begin
    if data.ShowAllItem or data.ShowFoldersItem then
    Canvas.Font.Style:= [fsUnderline]
    else
    Canvas.Font.Style:= [];
  end;

end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEFilterList.HandleMouseDown(var Message: TWMMouse; var HitInfo:
    THitInfo);
var
  data: PFilterItem;
begin
  inherited;
  if not assigned(HitInfo.HitNode) then
  Exit;
  
  if Message.Msg = WM_LBUTTONDOWN then
  begin
    if hiOnItemLabel in HitInfo.HitPositions then
    begin
      data:= Self.GetNodeData(HitInfo.HitNode);
      if data.ShowAllItem then
      begin
        // clear extension filters
        Self.CheckState[HitInfo.HitNode]:= csCheckedNormal;
        fShowAllExtensions:= true;
        ActiveFilters.Clear;
        DoFiltering(true);
      end
      else
      begin
        if HitInfo.HitNode.CheckState = csCheckedNormal then
        Self.CheckState[HitInfo.HitNode]:= csUncheckedNormal
        else
        Self.CheckState[HitInfo.HitNode]:= csCheckedNormal;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set ExplorerEasyListview
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetExplorerEasyListview(const Value:
    TCustomVirtualExplorerEasyListview);
begin
  if fExplorerEasyListview <> Value then
  begin
    fExplorerEasyListview:= Value;
    if assigned(fExplorerEasyListview) then
    begin
      if TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Image.Empty and fShowFilteringBackground then
      TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Image:= fFilteringImage;
    end;

    if fActive then
    begin
      // filter (recount needed)
      DoFiltering(true);
    end
    else
    DeFilter;
  end;
end;

{*------------------------------------------------------------------------------
  Set ShowAllExtension
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetShowAllExtensions(const Value: Boolean);
begin
  if assigned(fShowAllNode) then
  begin
    if Value then
    fShowAllNode.CheckState:= csCheckedNormal
    else
    fShowAllNode.CheckState:= csUncheckedNormal;
  end;

  if fShowAllExtensions <> Value then
  begin
    fShowAllExtensions:= Value;
    // filter (no need to recount)
    DoFiltering(false);
  end;
end;

{*------------------------------------------------------------------------------
  Set Active
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive:= Value;
    if fActive then
    begin
      // filter (recount needed)
      DoFiltering(true);
    end
    else
    DeFilter;
  end;
end;

{*------------------------------------------------------------------------------
  Set ShowFilteringBackground
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetShowFilteringBackground(const Value: Boolean);
begin
  fShowFilteringBackground:= Value;

  if assigned(fExplorerEasyListview) then
  begin
    if fShowFilteringBackground then
    begin
      TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Enabled:= false;
      TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Image:= fFilteringImage;
    end
    else
    begin
      TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Enabled:= false;
      TCustomVirtualExplorerEasyListviewHack(fExplorerEasyListview).BackGround.Image.FreeImage;
    end;
  end;

  if fActive then
  begin
    // filter to set the background visibility (no recount needed)
    DoFiltering(false);
  end;
end;

{*------------------------------------------------------------------------------
  Set SetShowFolders
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetShowFolders(const Value: Boolean);
begin
  if assigned(fShowFoldersNode) then
  begin
    if Value then
    fShowFoldersNode.CheckState:= csCheckedNormal
    else
    fShowFoldersNode.CheckState:= csUncheckedNormal;
  end;

  if fShowFolders <> Value then
  begin
    fShowFolders:= Value;
    // filter (no recount needed)
    DoFiltering(false);
  end;
end;

{*------------------------------------------------------------------------------
  Read properties from Storage
-------------------------------------------------------------------------------}
procedure TCEFilterList.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  OldPath: String;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FilterPanel']);
    with AppStorage do
    begin
      fShowFilteringBackground:= ReadBoolean('ShowBkgrd',true);
    end;
  finally
    AppStorage.Path:= OldPath;
  end;
end;

{-------------------------------------------------------------------------------
  Set ExcludeFromResults
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetExcludeFromResults(const Value: Boolean);
begin
  if Value <> fExcludeFromResults then
  begin
    fExcludeFromResults:= Value;
    // filter (do recount if pattern is used)
    DoFiltering(fPatternText <> '');
  end;
end;

{-------------------------------------------------------------------------------
  Set Pattern Text
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetPatternText(const Value: WideString);
begin
  if Value <> fPatternText then
  begin
    fPatternText:= Value;
    // filter (do recount)
    DoFiltering(true);
  end;
end;

{-------------------------------------------------------------------------------
  Use Wildcards
-------------------------------------------------------------------------------}
procedure TCEFilterList.SetUseWildcards(const Value: Boolean);
begin
  fUseWildcards:= Value;
  // filter (do recount if pattern is used)
  DoFiltering(fPatternText <> '');
end;

{*------------------------------------------------------------------------------
  Write properties to Storage
-------------------------------------------------------------------------------}
procedure TCEFilterList.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  OldPath: String;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.DeleteSubTree(BasePath + '\FilterPanel');
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FilterPanel']);
    with AppStorage do
    begin
      WriteBoolean('ShowBkgrd',fShowFilteringBackground);
    end;
  finally
    AppStorage.Path:= OldPath;
  end;
end;

end.
