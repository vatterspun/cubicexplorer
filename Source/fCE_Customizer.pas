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
//  The Original Code is fCE_Customizer.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_Customizer;

interface

uses
  // CE Units
  dCE_Actions, dCE_Images,  CE_Utils, CE_Toolbar, CE_TBActions, CE_VistaFuncs,
  CE_LanguageEngine,
  // VirtualTree
  VirtualTrees,
  // Toolbar2000
  TB2Dock, TB2Toolbar, TB2Item,
  // TNT Controls
  TntActnList, TntForms, TntCheckLst, TntStdCtrls,
  // Png Controls
  PngImageList,
  // SpTBXLib
  SpTBXItem, SpTBXControls,  SpTBXDkPanels, SpTBXTabs, SpTBXEditors, SpTBXSkins,
  // JVCL
  JvSimpleXml,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ActiveX, StdCtrls, Math, CheckLst, Menus;

type
  TTBCustomDockableWindowHack = class(TTBCustomDockableWindow);

  TCESeparatorType = (stNormal, stDynamic, stFixed, stStrether);

  PCEActTreeData = ^TCEActTreeData;
  TCEActTreeData = record
    ActionItem: TTntAction;
    IsCategory: Boolean;
    Name: WideString;
    IconIndex: Integer;
    IsSeparator: Boolean;
    SeparatorType: TCESeparatorType;
  end;

  TCEToolbarCustomizer = class(TTntForm)
    ActionTree: TVirtualStringTree;
    TabControl: TSpTBXTabControl;
    tab_toolbars: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tab_buttons: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    SpTBXPanel1: TSpTBXPanel;
    but_close: TSpTBXButton;
    tab_theme: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    ThemeList: TSpTBXListBox;
    but_loadTheme: TSpTBXButton;
    ToolbarList: TSpTBXCheckListBox;
    group_displayMode: TSpTBXRadioGroup;
    check_largeIcons: TSpTBXCheckBox;
    check_borders: TSpTBXCheckBox;
    check_stretch: TSpTBXCheckBox;
    check_dragHandle: TSpTBXCheckBox;
    label_help: TSpTBXLabel;
    label_themeName: TSpTBXLabel;
    label_themeAuthor: TSpTBXLabel;
    procedure ActionTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ActionTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var Allowed: Boolean);
    procedure ActionTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
        DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt:
        TPoint; var Effect: Integer; Mode: TDropMode);
    procedure ActionTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift:
        TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect:
        Integer; var Accept: Boolean);
    procedure ActionTreeGetImageIndexEx(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
        Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure ActionTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure ActionTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure ActionTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure but_closeClick(Sender: TObject);
    procedure but_loadThemeClick(Sender: TObject);
    procedure check_bordersClick(Sender: TObject);
    procedure check_dragHandleClick(Sender: TObject);
    procedure check_largeIconsClick(Sender: TObject);
    procedure check_stretchClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure group_displayModeClick(Sender: TObject);
    procedure TabControlActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure ThemeListClick(Sender: TObject);
    procedure TntFormKeyPress(Sender: TObject; var Key: Char);
    procedure ToolbarListClick(Sender: TObject);
  private
    fselectedToolbar: TObject;
    tmpItem: TSpTBXItem;
    procedure SetselectedToolbar(const Value: TObject);

  protected
    property selectedToolbar: TObject read fselectedToolbar write
        SetselectedToolbar;
  public
    ParentComponent: TComponent;
    function GetCategory(CatName: String; ATree: TVirtualStringTree): PVirtualNode;
    procedure SetupForm;

  end;

procedure ShowCustomizer(ParentComponent: TComponent);

var
  CEToolbarCustomizer: TCEToolbarCustomizer;

implementation

uses
  Main, CE_Layout, CE_DriveBar;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Show toolbar customizer.
-------------------------------------------------------------------------------}
procedure ShowCustomizer(ParentComponent: TComponent);
begin
  if not assigned(CEToolbarCustomizer) then
  begin
    CEToolbarCustomizer:= TCEToolbarCustomizer.Create(ParentComponent);
  end
  else
  begin
    if not CEToolbarCustomizer.Visible then
    CEToolbarCustomizer.Show;
    Exit;
  end;
  CEToolbarCustomizer.ParentComponent:= ParentComponent;
  CEToolbarCustomizer.SetupForm;
  CEToolbarCustomizer.Show;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormCreate(Sender: TObject);
begin
  SetVistaFont(Font);
  CEGlobalTranslator.TranslateComponent(Self);
  ActionTree.NodeDataSize:= SizeOf(TCEActTreeData);
  tmpItem:= TSpTBXItem.Create(self);
end;

{*------------------------------------------------------------------------------
  On Form Destroy
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormDestroy(Sender: TObject);
begin
  tmpItem.Free;
end;

{*------------------------------------------------------------------------------
  On Form Closed
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
  CEToolbarCustomizer:= nil;
  if MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    MainForm.TabSet.Toolbar.EndCustomize;
    EndToolbarCustomize;
  end;
end;

{*------------------------------------------------------------------------------
  Find Category by it's name.
-------------------------------------------------------------------------------}
function TCEToolbarCustomizer.GetCategory(CatName: String; ATree:
    TVirtualStringTree): PVirtualNode;
var
  data: PCEActTreeData;
  Node: PVirtualNode;
begin
  //Result:= nil;
  Node:= ATree.GetFirst;
  while Node <> nil do
  begin
    data:= ATree.GetNodeData(Node);
    if data.IsCategory then
    begin
      if CompareText(CatName, data.Name) = 0 then
      begin
        break;
      end;
    end;
    Node:= Node.NextSibling;
  end;

  if not assigned(Node) then
  begin
    Node:= ATree.AddChild(nil);
    data:= ATree.GetNodeData(Node);
    data.IsCategory:= true;
    data.IsSeparator:= false;
    data.Name:= CatName;
    data.IconIndex:= -1;
  end;

  Result:= Node;
end;

{*------------------------------------------------------------------------------
  Setup Form
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.SetupForm;
var
  i, index: Integer;
  toolbar: TObject;
  act: TTntAction;
  node, chNode: PVirtualNode;
  data: PCEActTreeData;
begin
  // Populate ActionTree
  ActionTree.Images:= CE_Images.SmallIcons;
  ActionTree.Clear;
  for i:= 0 to CEActions.ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(CEActions.ActionList.Actions[i]);
    node:= GetCategory(act.Category, ActionTree);
    chNode:= ActionTree.AddChild(node);
    data:= ActionTree.GetNodeData(chNode);
    data.ActionItem:= act;
    data.Name:= act.Caption;
    data.IconIndex:= act.ImageIndex;
    data.IsCategory:= false;
    data.IsSeparator:= false;
  end;
  // Add Separator category to ActionList
  node:= ActionTree.GetFirst;
  node:= ActionTree.InsertNode(node, amInsertBefore);
  data:= ActionTree.GetNodeData(Node);
  data.Name:= UTF8Encode(_('Separators'));
  data.IconIndex:= -1;
  data.IsCategory:= true;
  data.IsSeparator:= false;
  // Add Separator item to ActionList
  chNode:= ActionTree.AddChild(node);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '[' + _('Separator') + ']';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;
  data.SeparatorType:= stNormal;
  // Add Dynamic Spacer item to ActionList
  chNode:= ActionTree.AddChild(node);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '[' + _('Dynamic Spacer') + ']';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;
  data.SeparatorType:= stDynamic;
  // Add Fixed Spacer item to ActionList
  chNode:= ActionTree.AddChild(node);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '[' + _('Fixed Spacer') + ']';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;
  data.SeparatorType:= stFixed;
  // Add Stretcher item to ActionList
  chNode:= ActionTree.AddChild(node);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '[' + _('Stretcher') + ']';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;
  data.SeparatorType:= stStrether;

  // Sort actions
  node:= ActionTree.GetFirst;
  while node <> nil do
  begin
    data:= ActionTree.GetNodeData(Node);
    if data.IsCategory then
    begin
      ActionTree.Sort(node, 0, sdAscending, false);
    end;
    node:= node.NextSibling;
  end;
  
  ActionTree.FullExpand;

  // Populate Theme list
  SkinManager.SkinsList.GetSkinNames(ThemeList.Items.AnsiStrings);
  ThemeList.Sorted:= true;
  i:= ThemeList.Items.IndexOf('Default');
  if i > -1 then ThemeList.Items.Move(i, 0);
  ThemeList.ItemIndex:= ThemeList.Items.IndexOf(SkinManager.CurrentSkinName);
  label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
  if SkinManager.CurrentSkin.SkinAuthor <> '' then
  label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
  else
  label_themeAuthor.Caption:= '';

  // Populate Toolbar list
  for i:= 0 to CELayoutItems.Count - 1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXToolbar then
    begin
      toolbar:= CELayoutItems.Items[i];
      index:= ToolbarList.Items.AddObject(TSpTBXToolbar(toolbar).Caption, toolbar);
      ToolbarList.Checked[index]:= TSpTBXToolbar(toolbar).Visible;
    end
    else if CELayoutItems.Items[i] is TSpTBXToolWindow then
    begin
      toolbar:= CELayoutItems.Items[i];
      index:= ToolbarList.Items.AddObject(TSpTBXToolWindow(toolbar).Caption, toolbar);
      ToolbarList.Checked[index]:= TSpTBXToolWindow(toolbar).Visible;
    end;
  end;

  // Populate group_displayMode
  group_displayMode.Items.Add(_('Default'));
  group_displayMode.Items.Add(_('Icon only'));
  group_displayMode.Items.Add(_('Icon above text'));
  group_displayMode.Items.Add(_('Text only'));

  // Get translated caption for Load Theme from file
  but_loadTheme.Caption:= CEActions.act_view_loadskin.Caption;
end;

{-------------------------------------------------------------------------------
  On ActionTree.CompareNodes
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeCompareNodes(Sender: TBaseVirtualTree;
    Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PCEActTreeData;
begin
  data1:= ActionTree.GetNodeData(Node1);
  data2:= ActionTree.GetNodeData(Node2);
  if data1.IsSeparator and data1.IsSeparator then
  Result:= Ord(data1.SeparatorType) - Ord(data2.SeparatorType)
  else
  Result:= WideCompareText(data1.Name, data2.Name);
end;

{*------------------------------------------------------------------------------
  Get node Image index.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeGetImageIndexEx(Sender:
    TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column:
    TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList:
    TCustomImageList);
var
  data: PCEActTreeData;
begin
  if Column = 0 then
  begin
    data:= ActionTree.GetNodeData(Node);
    ImageIndex:= data.IconIndex;
    if not data.IsCategory or not data.IsSeparator then
    ImageList:= CE_Images.SmallIcons
    else
    ImageList:= ActionTree.Images;
  end;
end;

{*------------------------------------------------------------------------------
  Get node text.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeGetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var
    CellText: WideString);
var
  data: PCEActTreeData;
begin
  if Column <> 0 then
  Exit;
  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  CellText:= UTF8Decode(data.Name)
  else
  CellText:= data.Name;
end;

{*------------------------------------------------------------------------------
  Get's called when text gets painted.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreePaintText(Sender: TBaseVirtualTree;
    const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType);
var
  data: PCEActTreeData;
begin
  if Column <> 0 then
  Exit;

  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  TargetCanvas.Font.Style:= [fsBold, fsUnderline]
  else
  TargetCanvas.Font.Style:= [];
end;

{*------------------------------------------------------------------------------
  Get's called when Drag is started.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeStartDrag(Sender: TObject; var
    DragObject: TDragObject);
var
  data: PCEActTreeData;
  item: TTBCustomItem;
  itemClass: TTBCustomItemClass;
begin
  if ActionTree.FocusedNode = nil then
  Exit;

  data:= ActionTree.GetNodeData(ActionTree.FocusedNode);
  if data.IsCategory then
  Exit;

  itemClass:= nil;

  if data.IsSeparator then
  begin
    case data.SeparatorType of
      stNormal: itemClass:= TCEToolbarSeparatorItem;
      stDynamic: itemClass:= TCEToolbarDynamicSpacerItem;
      stFixed: itemClass:= TCEToolbarFixedSpacerItem;
      stStrether: itemClass:= TCEToolbarStretcherItem;
    end;
  end
  else if data.ActionItem is TCEToolbarAction then
  begin
    if Assigned(TCEToolbarAction(data.ActionItem).ItemClass) then
    itemClass:= TCEToolbarAction(data.ActionItem).ItemClass
    else
    itemClass:= TCEToolbarItem;
  end
  else       
  begin
    itemClass:= TCEToolbarItem;
  end;

  if assigned(itemClass) then
  begin
    item:= itemClass.Create(nil);
    item.Action:= data.ActionItem;
    tmpItem.Add(item);
    DragObject := TSpTBXItemDragObject.Create(ActionTree, item);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when determing if it's allowed to drag an item.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragAllowed(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  data: PCEActTreeData;
begin
  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  Allowed:= false
  else
  Allowed:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when item is dropped.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragDrop(Sender: TBaseVirtualTree;
    Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  OrigItem: TTBCustomItem;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) and
    (TSpTBXItemDragObject(Source).SourceControl <> Sender) then
  begin
    OrigItem := TSpTBXItemDragObject(Source).SouceItem;
    OrigItem.Parent.Remove(OrigItem);
    OrigItem.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when item is dragged over.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragOver(Sender: TBaseVirtualTree;
    Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode:
    TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := Assigned(Source) and (Source is TSpTBXItemDragObject);
end;

{-------------------------------------------------------------------------------
  On but_close Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.but_closeClick(Sender: TObject);
begin
  Self.Close;
end;

{-------------------------------------------------------------------------------
  On but_loadTheme Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.but_loadThemeClick(Sender: TObject);
var
  i: Integer;
begin
  CEActions.act_view_loadskin.Execute;
  // Populate Theme list
  SkinManager.SkinsList.GetSkinNames(ThemeList.Items.AnsiStrings);
  ThemeList.Sorted:= true;
  i:= ThemeList.Items.IndexOf('Default');
  if i > -1 then ThemeList.Items.Move(i, 0);
  ThemeList.ItemIndex:= ThemeList.Items.IndexOf(SkinManager.CurrentSkinName);
  label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
  if SkinManager.CurrentSkin.SkinAuthor <> '' then
  label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
  else
  label_themeAuthor.Caption:= '';
end;

{-------------------------------------------------------------------------------
  On TabControl.ActiveTabChange
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.TabControlActiveTabChange(Sender: TObject;
    TabIndex: Integer);
begin
  if (TabControl.ActiveTab = tab_buttons) and not MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    BeginToolbarCustomize;
    MainForm.TabSet.Toolbar.BeginCustomize;
  end
  else if MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    MainForm.TabSet.Toolbar.EndCustomize;
    EndToolbarCustomize;
  end;

  if TabControl.ActiveTab = tab_buttons then
  label_help.Caption:= _('Drag buttons to/from toolbars')
  else
  label_help.Caption:= '';
end;

{-------------------------------------------------------------------------------
  On ThemeList Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ThemeListClick(Sender: TObject);
begin
  if ThemeList.ItemIndex > -1 then
  begin
    SkinManager.SetSkin(ThemeList.Items.Strings[ThemeList.ItemIndex]);
    label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
    if SkinManager.CurrentSkin.SkinAuthor <> '' then
    label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
    else
    label_themeAuthor.Caption:= '';
  end
  else
  begin
    label_themeName.Caption:= '';
    label_themeAuthor.Caption:= '';
  end;
end;

{-------------------------------------------------------------------------------
  Set selectedToolbar
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.SetselectedToolbar(const Value: TObject);
begin
  if Value <> fselectedToolbar then
  begin
    fselectedToolbar:= nil;
    if assigned(Value) then
    begin
      if Value is TSpTBXToolbar then
      begin
        // Display Mode
        group_displayMode.ItemIndex:= Ord(TSpTBXToolbar(Value).DisplayMode);
        group_displayMode.Enabled:= true;
        // Large Icons
        check_largeIcons.Checked:= TSpTBXToolbar(Value).Images = CE_Images.MediumIcons;
        check_largeIcons.Enabled:= true;
        // Borders
        check_borders.Checked:= TSpTBXToolbar(Value).BorderStyle = bsSingle;
        check_borders.Enabled:= true;
        // Stretch
        check_stretch.Checked:= TSpTBXToolbar(Value).Stretch;
        check_stretch.Enabled:= true;
        // Drag Handle
        check_dragHandle.Checked:= TSpTBXToolbar(Value).DragHandleStyle <> dhNone;
        check_dragHandle.Enabled:= true;
      end
      else if Value is TSpTBXToolWindow then
      begin
        // Display Mode
        group_displayMode.Enabled:= false;
        // Large Icons
        check_largeIcons.Enabled:= false;
        // Borders
        check_borders.Checked:= TSpTBXToolWindow(Value).BorderStyle = bsSingle;
        check_borders.Enabled:= true;
        // Stretch
        check_stretch.Checked:= TSpTBXToolWindow(Value).Stretch;
        check_stretch.Enabled:= true;
        // Drag Handle
        check_dragHandle.Checked:= TSpTBXToolWindow(Value).DragHandleStyle <> dhNone;
        check_dragHandle.Enabled:= true;
      end;
      fselectedToolbar:= Value;
    end
    else
    begin
      group_displayMode.Enabled:= false;
      check_largeIcons.Enabled:= false;
      check_borders.Enabled:= false;
      check_stretch.Enabled:= false;
      check_dragHandle.Enabled:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On ToolbarList Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ToolbarListClick(Sender: TObject);
begin
  if ToolbarList.ItemIndex > -1 then
  begin
    selectedToolbar:= ToolbarList.Items.Objects[ToolbarList.ItemIndex];
    if selectedToolbar is TTBCustomDockableWindow then
    TTBCustomDockableWindowHack(selectedToolbar).Visible:= ToolbarList.Checked[ToolbarList.ItemIndex];
  end
  else
  selectedToolbar:= nil;
end;

{-------------------------------------------------------------------------------
  On group_displayMode.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.group_displayModeClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if group_displayMode.ItemIndex > -1 then
    begin
      if selectedToolbar is TSpTBXToolbar then
      begin
        TSpTBXToolbar(selectedToolbar).BeginUpdate;
        TSpTBXToolbar(selectedToolbar).DisplayMode:= TSpTBXToolbarDisplayMode(group_displayMode.ItemIndex);
        if TSpTBXToolbar(selectedToolbar).DisplayMode = tbdmImageAboveCaption then
        TSpTBXToolbar(selectedToolbar).Options:= [tboImageAboveCaption];
        TSpTBXToolbar(selectedToolbar).EndUpdate;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On check_largeIcons.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_largeIconsClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if selectedToolbar is TSpTBXToolbar then
    begin
      if check_largeIcons.Checked then
      TSpTBXToolbar(selectedToolbar).Images:= CE_Images.MediumIcons
      else
      TSpTBXToolbar(selectedToolbar).Images:= CE_Images.SmallIcons;

      if selectedToolbar is TCEToolbar then
      TCEToolbar(selectedToolbar).LargeImages:= check_largeIcons.Checked;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On check_borders.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_bordersClick(Sender: TObject);
begin
  if assigned(selectedToolbar) and (selectedToolbar is TTBCustomDockableWindow) then
  begin
    if check_borders.Checked then
    TTBCustomDockableWindowHack(selectedToolbar).BorderStyle:= bsSingle
    else
    TTBCustomDockableWindowHack(selectedToolbar).BorderStyle:= bsNone;
  end;
end;

{-------------------------------------------------------------------------------
  On check_stretch.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_stretchClick(Sender: TObject);
begin
  if assigned(selectedToolbar) and (selectedToolbar is TTBCustomDockableWindow) then
  begin
    TTBCustomDockableWindowHack(selectedToolbar).Stretch:= check_stretch.Checked;
  end;
end;

{-------------------------------------------------------------------------------
  On check_dragHandle.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_dragHandleClick(Sender: TObject);
begin
  if assigned(selectedToolbar) and (selectedToolbar is TTBCustomDockableWindow) then
  begin
    if check_dragHandle.Checked then
    TTBCustomDockableWindowHack(selectedToolbar).DragHandleStyle:= dhSingle
    else
    TTBCustomDockableWindowHack(selectedToolbar).DragHandleStyle:= dhNone;
  end;
end;

{-------------------------------------------------------------------------------
  On Form Key Press
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.TntFormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_close.Click;
end;

end.
