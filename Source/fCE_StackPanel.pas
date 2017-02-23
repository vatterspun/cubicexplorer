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
//  The Original Code is fCE_StackPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_StackPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_Stacks, CE_StackTree, CE_GlobalCtrl, dCE_Images, CE_VistaFuncs,
  CE_AppSettings, CE_Toolbar, dCE_Actions,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Item, TB2Toolbar, SpTBXEditors, SpTBXSkins,
  // VSTools
  MPCommonObjects, EasyListview, MPCommonUtilities,
  // Tnt
  TntClasses,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, VirtualTrees, ImgList;

type
  TControlHack = class(TControl);

  TCEStackPanelSettings = class;

  TCEStackStartupType = (stEmpty, stLastUsed, stUserSelected);

  TCEStackPanel = class(TCECustomDockableForm)
    DropStackPopup: TSpTBXPopupMenu;
    but_clearlist: TSpTBXItem;
    StackToolbar: TCEToolbar;
    procedure FormCreate(Sender: TObject);
    procedure DropStackPopupPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fSettings: TCEStackPanelSettings;
    { Private declarations }
  protected
    fStackPaths: TTntStrings;
    procedure HandleStackLoadClick(Sender: TObject);
    procedure HandleStackSaveClick(Sender: TObject);
  public
    StackTree: TCEStackTree;
    procedure DoFormShow; override;
    function FindStackNames(AList: TTntStrings): Integer;
    procedure ClearList;
    procedure DoStartUp; override;
    procedure LoadStartupStack;
    procedure PopulateStackOpenMenuItem(AItem: TTBCustomItem);
    procedure PopulateStackSaveMenuItem(AItem: TTBCustomItem; ShowAutoSaveItem:
        Boolean = false);
    property Settings: TCEStackPanelSettings read fSettings write fSettings;
  end;

  TCEStackPanelSettings = class(TPersistent)
  private
    fFontSize: Integer;
    fLineHeight: Integer;
    fLoadOnStartup: WideString;
    fStartupType: TCEStackStartupType;
    function GetAutoCollapse: Boolean;
    function GetAutoExpand: Boolean;
    function GetAutoSaveStack: Boolean;
    function GetFullExpandOnLoad: Boolean;
    function GetMaxHintItemCount: Integer;
    function GetSafeOperationsOnly: Boolean;
    function GetShowWarnings: Boolean;
    procedure SetAutoCollapse(const Value: Boolean);
    procedure SetAutoExpand(const Value: Boolean);
    procedure SetAutoSaveStack(const Value: Boolean);
    procedure SetFontSize(const Value: Integer);
    procedure SetFullExpandOnLoad(const Value: Boolean);
    procedure SetLineHeight(const Value: Integer);
    procedure SetMaxHintItemCount(const Value: Integer);
    procedure SetSafeOperationsOnly(const Value: Boolean);
    procedure SetShowWarnings(const Value: Boolean);
  public
    StackPanel: TCEStackPanel;
  published
    property AutoCollapse: Boolean read GetAutoCollapse write SetAutoCollapse;
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand;
    property LoadOnStartup: WideString read fLoadOnStartup write fLoadOnStartup;
    property AutoSaveStack: Boolean read GetAutoSaveStack write SetAutoSaveStack;
    property FontSize: Integer read fFontSize write SetFontSize;
    property FullExpandOnLoad: Boolean read GetFullExpandOnLoad write
        SetFullExpandOnLoad;
    property LineHeight: Integer read fLineHeight write SetLineHeight;
    property StartupType: TCEStackStartupType read fStartupType write fStartupType;
    property MaxHintItemCount: Integer read GetMaxHintItemCount write
        SetMaxHintItemCount;
    property SafeOperationsOnly: Boolean read GetSafeOperationsOnly write
        SetSafeOperationsOnly;
    property ShowWarnings: Boolean read GetShowWarnings write SetShowWarnings;
  end;

var
  CEStackPanel: TCEStackPanel;

implementation

uses
  CE_LanguageEngine, CE_LanguageUtils, fCE_ItemSelectSaveDlg, CE_Utils,
  CE_ToolbarButtons, CE_Layout, Main;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is created
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormCreate(Sender: TObject);
begin
  inherited;
  // Properties
  TopDock.Name:= 'StackPanel_TopDock';
  BottomDock.Name:= 'StackPanel_BottomDock';
  Caption:= 'Stack';
  // StackTree
  StackTree:= TCEStackTree.Create(self);
  StackTree.Parent:= Self;
  StackTree.Align:= alClient;
  StackTree.BackgroundPopupMenu:= DropStackPopup;
  StackTree.Images:= CE_Images.BookmarkImages;
  StackTree.GroupImageIndex:= 6;
  StackTree.GroupOpenImageIndex:= 6;
  StackTree.NotAvailableImageIndex:= 4;
  StackTree.BottomSpace:= 6;
  // Focus control
  GlobalFocusCtrl.CtrlList.Add(StackTree);
  TControlHack(StackTree).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  // Settings
  fSettings:= TCEStackPanelSettings.Create;
  fSettings.StackPanel:= Self;

  // Default Settings
  fSettings.fFontSize:= -1;
  fSettings.fLineHeight:= -1;

  GlobalAppSettings.AddItem('StackPanel', fSettings, true);

  fStackPaths:= TTntStringList.Create;

  CELayoutItems.Add(StackToolbar);
end;

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is destroyed
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormDestroy(Sender: TObject);
begin
  if assigned(StackTree.ActiveStack) then
  begin
    StackTree.ActiveStack.Free;
    StackTree.ActiveStack:= nil;
  end;
  fStackPaths.Free;
  fSettings.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  On DropStackPopup popup
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DropStackPopupPopup(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
  Do Form Show
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DoFormShow;
begin
  inherited;
  StackToolbar.Realign;
end;

{-------------------------------------------------------------------------------
  Find Stack Names
-------------------------------------------------------------------------------}
function TCEStackPanel.FindStackNames(AList: TTntStrings): Integer;
var
  i: Integer;
begin
  if not assigned(AList) then
  begin
    Result:= -1;
    Exit;
  end;
  
  FindStacks(StackDirPath, fStackPaths);
  Result:= fStackPaths.Count;
  AList.Clear;
  for i:= 0 to fStackPaths.Count - 1 do
  begin
    AList.Add(WideExtractFileName(fStackPaths.Strings[i], true));
  end;
end;

{-------------------------------------------------------------------------------
  ClearList
-------------------------------------------------------------------------------}
procedure TCEStackPanel.ClearList;
begin
  if Settings.ShowWarnings then
  begin
    if TaskDialog(Self.Handle,
                  _('Clear Stack'),
                  _('Removing all items from Stack!'),
                  _('Are you sure you want to clear the list?'),
                  TD_ICON_QUESTION,
                  TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES then
    begin
      StackTree.Clear;
    end;
  end
  else
  StackTree.Clear;
end;

{-------------------------------------------------------------------------------
  Do StartUp
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DoStartUp;
var
  item: TSpTBXCustomItem;
  sep: TCEToolbarSeparatorItem;
begin
  // Create default toolbar items
  // - open
  item:= TCEStackOpenButton.Create(StackToolbar);
  item.Action:= CEActions.act_stack_open;
  StackToolbar.Items.Add(item);
  // - save
  item:= TCEStackSaveButton.Create(StackToolbar);
  item.Action:= CEActions.act_stack_save;
  StackToolbar.Items.Add(item);
  // - separator
  sep:= TCEToolbarSeparatorItem.Create(StackToolbar);
  StackToolbar.Items.Add(sep);
  // - remove
  item:= TSpTBXItem.Create(StackToolbar);
  item.Action:= CEActions.act_stack_remove;
  StackToolbar.Items.Add(item);
  // - clear
  item:= TSpTBXItem.Create(StackToolbar);
  item.Action:= CEActions.act_stack_clear;
  StackToolbar.Items.Add(item);
  // - dynamic spacer
  item:= TCEToolbarDynamicSpacerItem.Create(StackToolbar);
  StackToolbar.Items.Add(item);
  // - allow Move
  item:= TSpTBXItem.Create(StackToolbar);
  item.Action:= CEActions.act_stack_allowmove;
  StackToolbar.Items.Add(item);

  // Popup
  StackToolbar.PopupMenu:= MainForm.ToolbarPopupMenu;
end;

{-------------------------------------------------------------------------------
  On Stack Load item Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackLoadClick(Sender: TObject);
var
  i: Integer;
  ws: WideString;
begin
  i:= TSpTBXItem(Sender).Tag;
  if (i > -1) and (i < fStackPaths.Count) then
  begin
    ws:= fStackPaths.Strings[i];
    // Free previous Stack
    if assigned(StackTree.ActiveStack) then
    begin
      StackTree.ActiveStack.Free;
      StackTree.ActiveStack:= nil;
    end;
    // Create new stack
    StackTree.ActiveStack:= TCEStackItem.Create;
    StackTree.ActiveStack.StackName:= WideExtractFileName(ws, true);
    StackTree.ActiveStack.LoadFromFile(ws);
    StackTree.LoadFromStack(StackTree.ActiveStack);
    if Settings.StartupType = stLastUsed then
    Settings.LoadOnStartup:= StackTree.ActiveStack.StackName;
  end;
end;

{-------------------------------------------------------------------------------
  On Stack Save Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackSaveClick(Sender: TObject);
var
  id, i: Integer;
  dlg: TCEItemSelectSaveDlg;
  stack: TCEStackItem;
  ws: WideString;
  p: TPoint;
begin
  if ReadOnlySettings then
  Exit;
  
  id:= TSpTBXItem(Sender).Tag;
  // Save As...
  if id = -1 then
  begin
    dlg:= TCEItemSelectSaveDlg.Create(Self);
    try
      dlg.Caption:= _('Save Stack');
      dlg.label_combotitle.Caption:= _('Stack Name');
      dlg.but_ok.Caption:= _('Save');
      dlg.ShowExistsWarning:= true;
      dlg.ExistsWarningTitle:= _('Confirm');
      dlg.ExistsWarningDescription:= _('Override existing stack?');
      dlg.ExistsWarningContent:= _('Do you want to override existing stack?');
      for i:= 0 to fStackPaths.Count - 1 do
      dlg.combo.Items.Add(WideExtractFileName(fStackPaths.Strings[i], true));
      dlg.combo.ItemIndex:= -1;
      dlg.combo.Text:= '';
      dlg.Position:= poDesigned;
      p:= StackTree.ClientToScreen(Point(0,0));
      dlg.Left:= p.X;
      dlg.Top:= p.Y;
      // Save stack
      if dlg.ShowModal = mrOK then
      begin
        if dlg.combo.ItemIndex > -1 then
        begin
          if assigned(StackTree.ActiveStack) and
            (StackTree.ActiveStack.StackPath = fStackPaths.Strings[dlg.combo.ItemIndex]) then
          stack:= StackTree.ActiveStack
          else
          begin
            if assigned(StackTree.ActiveStack) then
            begin
              StackTree.ActiveStack.Free;
              StackTree.ActiveStack:= nil;
            end;
            stack:= TCEStackItem.Create;
            stack.StackPath:= fStackPaths.Strings[dlg.combo.ItemIndex];
            stack.StackName:= WideExtractFileName(stack.StackPath, true);
          end;
        end
        else
        begin
          if assigned(StackTree.ActiveStack) then
          begin
            StackTree.ActiveStack.Free;
            StackTree.ActiveStack:= nil;
          end;
          stack:= TCEStackItem.Create;
          stack.StackPath:= StackDirPath + dlg.combo.Text + '.stk';
          stack.StackName:= dlg.combo.Text;
        end;

        // Create stack dir if it doesn't exist
        if not WideDirectoryExists(StackDirPath) then
        WideCreateDir(StackDirPath);

        StackTree.SaveToStack(stack);
        stack.SaveToFile(stack.StackPath);
        StackTree.ActiveStack:= stack;
      end;
    finally
      dlg.Free;
    end;
  end
  // Auto Save option
  else if id = -2 then
  begin
    StackTree.AutoSaveActiveStack:= not StackTree.AutoSaveActiveStack;
  end
  // Save to existing Stack
  else if (id > -1) and (id < fStackPaths.Count) then
  begin
    if (TaskDialog(Self.Handle,
                   _('Confirm'),
                   _('Override existing stack?'),
                   _('Do you want to override existing stack?'),
                   TD_ICON_QUESTION,
                   TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES) then
    begin
      ws:= fStackPaths.Strings[id];
      if assigned(StackTree.ActiveStack) then
      begin
        StackTree.ActiveStack.Free;
        StackTree.ActiveStack:= nil;
      end;
      stack:= TCEStackItem.Create;
      stack.StackPath:= ws;
      stack.StackName:= WideExtractFileName(ws, true);
      StackTree.SaveToStack(stack);
      stack.SaveToFile(stack.StackPath);
      StackTree.ActiveStack:= stack;
    end;
  end; 
end;

{-------------------------------------------------------------------------------
  Load Startup Stack
-------------------------------------------------------------------------------}
procedure TCEStackPanel.LoadStartupStack;
var
  i: integer;
  ws: WideString;
begin
  if (Settings.StartupType <> stEmpty) and (Settings.LoadOnStartup <> '') then
  begin
    FindStacks(StackDirPath, fStackPaths);
    for i:= 0 to fStackPaths.Count - 1 do
    begin
      ws:= WideExtractFileName(fStackPaths.Strings[i], true);
      if ws = Settings.LoadOnStartup then
      begin
        StackTree.ActiveStack:= TCEStackItem.Create;
        StackTree.ActiveStack.StackName:= ws;
        StackTree.ActiveStack.LoadFromFile(fStackPaths.Strings[i]);
        StackTree.LoadFromStack(StackTree.ActiveStack);
        break;
      end;     
    end;
  end;       
end;

procedure TCEStackPanel.PopulateStackOpenMenuItem(AItem: TTBCustomItem);
var
  i: Integer;
  item: TSpTBXItem;
  ws: WideString;
begin
  AItem.Clear;

  FindStacks(StackDirPath, fStackPaths);

  for i:= 0 to fStackPaths.Count - 1 do
  begin
    item:= TSpTBXItem.Create(AItem);
    ws:= fStackPaths.Strings[i];
    item.Caption:= WideExtractFileName(ws, true);
    item.Tag:= i;
    item.OnClick:= HandleStackLoadClick;
    item.Checked:= assigned(StackTree.ActiveStack) and (StackTree.ActiveStack.StackPath = ws);
    item.Images:= CE_Images.SmallIcons;
    item.ImageIndex:= 43;
    AItem.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Populate Stack Save MenuItem
-------------------------------------------------------------------------------}
procedure TCEStackPanel.PopulateStackSaveMenuItem(AItem: TTBCustomItem;
    ShowAutoSaveItem: Boolean = false);
var
  i: Integer;
  item: TSpTBXItem;
  ws: WideString;
begin
  AItem.Clear;
  // Save As...
  item:= TSpTBXItem.Create(AItem);
  item.Caption:= _('Save As...');
  item.Tag:= -1;
  item.OnClick:= HandleStackSaveClick;
  item.Images:= CE_Images.MiscImages;
  item.ImageIndex:= 6;
  AItem.Add(item);
  // Separator
  AItem.Add(TSpTBXSeparatorItem.Create(AItem));
  // Stack Items ->
  FindStacks(StackDirPath, fStackPaths);
  for i:= 0 to fStackPaths.Count - 1 do
  begin
    item:= TSpTBXItem.Create(AItem);
    ws:= fStackPaths.Strings[i];
    item.Caption:= WideExtractFileName(ws, true);
    item.Checked:= assigned(StackTree.ActiveStack) and (StackTree.ActiveStack.StackPath = ws);
    item.Tag:= i;
    item.OnClick:= HandleStackSaveClick;
    item.Images:= CE_Images.SmallIcons;
    item.ImageIndex:= 43;
    AItem.Add(item);
  end;
  // Auto Save Item
  if ShowAutoSaveItem then
  begin
    // Separator
    AItem.Add(TSpTBXSeparatorItem.Create(AItem));
    // Auto Save
    item:= TSpTBXItem.Create(AItem);
    item.Caption:= _('Enable Auto Save');
    item.Tag:= -2;
    item.Checked:= StackTree.AutoSaveActiveStack;
    item.OnClick:= HandleStackSaveClick;
    item.Enabled:= not ReadOnlySettings;
    AItem.Add(item);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set AutoCollapse
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoCollapse: Boolean;
begin
  Result:= StackPanel.StackTree.AutoCollapse;
end;
procedure TCEStackPanelSettings.SetAutoCollapse(const Value: Boolean);
begin
  StackPanel.StackTree.AutoCollapse:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoExpand
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoExpand: Boolean;
begin
  Result:= StackPanel.StackTree.AutoExpand;
end;
procedure TCEStackPanelSettings.SetAutoExpand(const Value: Boolean);
begin
  StackPanel.StackTree.AutoExpand:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoSaveStack
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoSaveStack: Boolean;
begin
  Result:= StackPanel.StackTree.AutoSaveActiveStack;
end;
procedure TCEStackPanelSettings.SetAutoSaveStack(const Value: Boolean);
begin
  StackPanel.StackTree.AutoSaveActiveStack:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set MaxHintItemCount
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetMaxHintItemCount: Integer;
begin
  Result:= StackPanel.StackTree.MaxHintItemCount;
end;
procedure TCEStackPanelSettings.SetMaxHintItemCount(const Value: Integer);
begin
  StackPanel.StackTree.MaxHintItemCount:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SafeOperationsOnly
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetSafeOperationsOnly: Boolean;
begin
  Result:= StackPanel.StackTree.SafeOperationsOnly;
end;
procedure TCEStackPanelSettings.SetSafeOperationsOnly(const Value: Boolean);
begin
  StackPanel.StackTree.SafeOperationsOnly:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set ShowWarnings
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetShowWarnings: Boolean;
begin
  Result:= StackPanel.StackTree.ShowWarnings;
end;
procedure TCEStackPanelSettings.SetShowWarnings(const Value: Boolean);
begin
  StackPanel.StackTree.ShowWarnings:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set FullExpandOnLoad
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetFullExpandOnLoad: Boolean;
begin
  Result:= StackPanel.StackTree.FullExpandOnLoad;
end;
procedure TCEStackPanelSettings.SetFullExpandOnLoad(const Value: Boolean);
begin
  StackPanel.StackTree.FullExpandOnLoad:= Value;
end;

{-------------------------------------------------------------------------------
  Set FontSize
-------------------------------------------------------------------------------}
procedure TCEStackPanelSettings.SetFontSize(const Value: Integer);
begin
  fFontSize:= Value;
  if fFontSize > 0 then
  StackPanel.StackTree.Font.Size:= fFontSize
  else
  SetDesktopIconFonts(StackPanel.StackTree.Font);
end;

{-------------------------------------------------------------------------------
  Set Line Height
-------------------------------------------------------------------------------}
procedure TCEStackPanelSettings.SetLineHeight(const Value: Integer);
var
  i: Integer;
  node: PVirtualNode;
begin
  fLineHeight:= Value;
  i:= StackPanel.StackTree.DefaultNodeHeight;
  if fLineHeight > 0 then
  StackPanel.StackTree.DefaultNodeHeight:= fLineHeight
  else
  StackPanel.StackTree.DefaultNodeHeight:= SmallShellIconSize + 1;

  // resize nodes
  if i <> StackPanel.StackTree.DefaultNodeHeight then
  begin
    StackPanel.StackTree.BeginUpdate;
    try
      node:= StackPanel.StackTree.GetFirstInitialized;
      while assigned(node) do
      begin
        StackPanel.StackTree.NodeHeight[node]:= StackPanel.StackTree.DefaultNodeHeight;
        node:= StackPanel.StackTree.GetNextInitialized(node);
      end;
    finally
      StackPanel.StackTree.EndUpdate;
    end;
  end;
end;

end.
