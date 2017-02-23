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
//  The Original Code is CE_FolderTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FolderTree;

interface

uses
  // CE Units
  CE_Utils,
  // VSTools, VT
  VirtualTrees, VirtualExplorerTree, MPShellUtilities, VirtualShellNotifier,
  MPCommonObjects, MPThreadManager,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, Forms, Controls, ExtCtrls;

type
  TCEFolderTreeSelectedChangeEvent = procedure(Node: PVirtualNode) of object;

  TCEFolderTree = class(TVirtualExplorerTreeview)
  private
    fAutoCollapse: Boolean;
    fAutoExpand: Boolean;
    fBrowseZipFolders: Boolean;
    fCenterOnBrowse: Boolean;
    fCenterOnExpand: Boolean;
    fHiddenFiles: Boolean;
    fOnSelectedChange: TCEFolderTreeSelectedChangeEvent;
    fSelectionTimer: TTimer;
    procedure SetBrowseZipFolders(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    function DoContextMenuCmd(Namespace: TNamespace; Verb: WideString; MenuItemID:
        Integer): Boolean; override;
    function DoContextMenuShow(Namespace: TNamespace; Menu: hMenu): Boolean;
        override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
        override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean);
        override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
        override;
    procedure DoSelectedChange(Node: PVirtualNode);
    procedure DoShellNotify(ShellEvent: TVirtualShellEvent); override;
    procedure HandleEnumFolder(Sender: TCustomVirtualExplorerTree; Namespace:
        TNamespace; var AllowAsChild: Boolean);
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);
        override;
    procedure SelectionTimer(Sender: TObject);
    procedure WMKILLFOCUS(var Message: TMessage); message WM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    function PasteShortcutFromClipboard: Boolean;
    function Refresh: Boolean;
    procedure ScrollToView(ANode: PVirtualNode; AVertical: Boolean = true;
        AHorizontal: Boolean = true);
    procedure SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift = evsCurrent);
        override;
    property AutoCollapse: Boolean read fAutoCollapse write fAutoCollapse;
    property AutoExpand: Boolean read fAutoExpand write fAutoExpand;
    property BrowseZipFolders: Boolean read fBrowseZipFolders write
        SetBrowseZipFolders;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
  published
    property CenterOnBrowse: Boolean read fCenterOnBrowse write fCenterOnBrowse;
    property CenterOnExpand: Boolean read fCenterOnExpand write fCenterOnExpand;
    property OnSelectedChange: TCEFolderTreeSelectedChangeEvent read
        fOnSelectedChange write fOnSelectedChange;
  end;

  TCE_VTEdit = class(TVTEdit)
  private
  protected
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  end;

  TCE_StringEditLink = class(TStringEditLink)
  public
    constructor Create; override;
  end;

implementation

uses
  dCE_Actions, MPCommonUtilities;

{*------------------------------------------------------------------------------
  Create an instance of TCEFolderTree
-------------------------------------------------------------------------------}
constructor TCEFolderTree.Create(AOwner: TComponent);
begin
  inherited;
  Self.BorderStyle:= bsNone;
  Self.TreeOptions.AutoOptions:= [toAutoDropExpand,toAutoScrollOnExpand,toAutoTristateTracking,toAutoDeleteMovedNodes];
  Self.TreeOptions.MiscOptions:= [toAcceptOLEDrop,toEditable,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick];
  Self.TreeOptions.PaintOptions:= [toShowButtons,toShowTreeLines,toUseBlendedImages,toGhostedIfUnfocused];
  Self.TreeOptions.VETImageOptions:= [toImages,toMarkCutAndCopy];
  Self.TreeOptions.VETMiscOptions:= [toBrowseExecuteFolder, toRemoveContextMenuShortCut,toBrowseExecuteFolderShortcut,toChangeNotifierThread,toTrackChangesInMappedDrives,toExecuteOnDblClk, toNoRebuildIconListOnAssocChange];
  Self.TreeOptions.VETShellOptions:= [toRightAlignSizeColumn,toContextMenus,toDragDrop];
  //Self.TreeOptions.SelectionOptions:= [toMultiSelect,toRightClickSelect,toSiblingSelectConstraint,toCenterScrollIntoView];
  Self.VETColors.FileTextColor:= clWindowText;
  Self.VETColors.FolderTextColor:= clWindowText;
  Self.VETColors.CompressedTextColor:= clWindowText;
  Self.VETColors.EncryptedTextColor:= clWindowText;
  Self.OnEnumFolder:= HandleEnumFolder;
  HiddenFiles:= false;

  fSelectionTimer:= TTimer.Create(Self);
  fSelectionTimer.Enabled:= false;
  fSelectionTimer.Interval:= 500;
  fSelectionTimer.OnTimer:= SelectionTimer;
  Self.DefaultNodeHeight:= SmallShellIconSize + 1;
end;

{-------------------------------------------------------------------------------
  Do ContextMenuCmd
-------------------------------------------------------------------------------}
function TCEFolderTree.DoContextMenuCmd(Namespace: TNamespace; Verb:
    WideString; MenuItemID: Integer): Boolean;
begin
  Result:= inherited DoContextMenuCmd(Namespace, Verb, MenuItemID);
  if not Result then
  DoGlobalContextMenuCmd(Self, Namespace, Verb, MenuItemID, Result);
end;

{-------------------------------------------------------------------------------
  Do ContextMenuShow
-------------------------------------------------------------------------------}
function TCEFolderTree.DoContextMenuShow(Namespace: TNamespace; Menu: hMenu):
    Boolean;
begin
  Result:= inherited DoContextMenuShow(Namespace, Menu);
  if Result then
  begin
    DoGlobalContextMenuShow(Self, Namespace, Menu, Result);
  end;
end;

{*------------------------------------------------------------------------------
  Create custom editor
-------------------------------------------------------------------------------}
function TCEFolderTree.DoCreateEditor(Node: PVirtualNode; Column:
    TColumnIndex): IVTEditLink;
begin
  Result:= TCE_StringEditLink.Create;
end;

{-------------------------------------------------------------------------------
  Do EnumFolder
-------------------------------------------------------------------------------}
procedure TCEFolderTree.DoEnumFolder(const Namespace: TNamespace; var
    AllowAsChild: Boolean);
begin
  inherited;

  if fBrowseZipFolders then
  AllowAsChild:= Namespace.Folder
  else
  AllowAsChild:= Namespace.Folder and (WideStrIComp(PWideChar(Namespace.Extension), '.zip') <> 0);
end;

{*------------------------------------------------------------------------------
  Handle Key Actions
-------------------------------------------------------------------------------}
function TCEFolderTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState):
    Boolean;
begin
  Result:= true;
  case CharCode of
    VK_F2,
    VK_F5,
    VK_DELETE:
    begin
      if Shift = [] then
      Result:= false;
    end;
    Ord('C'), Ord('c'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('X'), Ord('x'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('V'), Ord('v'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('A'), Ord('a'):
    begin
      if (Shift = [ssShift,ssCtrl]) or (Shift = [ssCtrl]) then
      Result:= false;
    end;
    VK_LEFT, VK_RIGHT:
    begin
      if Shift = [ssAlt] then
      Result:= false
      else
      begin
        fSelectionTimer.Enabled:= false;
        fSelectionTimer.Enabled:= true;
      end;
    end;
    VK_DOWN, VK_UP:
    begin
      fSelectionTimer.Enabled:= false;
      fSelectionTimer.Enabled:= true;
    end;
    VK_INSERT:
    begin
      if (ssShift in Shift) or (ssCtrl in Shift) then
      Result:= false;
    end;
    VK_RETURN, VK_SPACE:
    begin
      SelectionTimer(self);
      Result:= false;
    end;
  end;

  if Result then
  inherited DoKeyAction(CharCode, Shift);
end;

{-------------------------------------------------------------------------------
  Do Selected Change
-------------------------------------------------------------------------------}
procedure TCEFolderTree.DoSelectedChange(Node: PVirtualNode);
begin
  if Assigned(fOnSelectedChange) then fOnSelectedChange(Node);
end;

{-------------------------------------------------------------------------------
  Do Shell Notify
-------------------------------------------------------------------------------}
procedure TCEFolderTree.DoShellNotify(ShellEvent: TVirtualShellEvent);
begin
  inherited;
  if not ShellEvent.Handled then
  begin
    if ShellEvent.ShellNotifyEvent = vsneAssoccChanged then
    begin
//      RebuildTree;
//      Refresh;
      ShellEvent.Handled:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Enum Folder
-------------------------------------------------------------------------------}
procedure TCEFolderTree.HandleEnumFolder(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; var AllowAsChild: Boolean);
begin
  if fBrowseZipFolders then
  AllowAsChild:= Namespace.Folder
  else
  AllowAsChild:= Namespace.Folder and (WideStrIComp(PWideChar(Namespace.Extension), '.zip') <> 0);
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEFolderTree.HandleMouseDown(var Message: TWMMouse; var HitInfo:
    THitInfo);
var
  ShiftState: TShiftState;
begin
  if Self.IsEditing then
  begin
    Self.EndEditNode;
    Exit;
  end;

  if not assigned(HitInfo.HitNode) then
  begin
    inherited;
  end
  else if (hiOnItemButton in HitInfo.HitPositions) then
  begin
    if HitInfo.HitNode <> self.FocusedNode then
    begin
      if Self.Expanded[HitInfo.HitNode] and Self.HasAsParent(Self.FocusedNode, HitInfo.HitNode) then
      begin
        Self.Expanded[HitInfo.HitNode]:= false;
        DoSelectedChange(HitInfo.HitNode);
      end
      else
      begin
        Self.ToggleNode(HitInfo.HitNode);
      end;
    end
    else
    Self.ToggleNode(HitInfo.HitNode);

    if CenterOnExpand and (vsExpanded in HitInfo.HitNode.States) then
    begin
      ScrollToView(HitInfo.HitNode, false, true);
    end;
  end
  else if HitInfo.HitNode = self.FocusedNode then
  begin
    inherited;
  end
  else
  begin
    Self.BeginUpdate;
    try
      if fAutoCollapse then
      Self.FullCollapse;

      Self.ClearSelection;

      Self.FullyVisible[HitInfo.HitNode]:= true;
      if fAutoExpand and not Self.Expanded[HitInfo.HitNode] then
      Self.ToggleNode(HitInfo.HitNode);

      Self.Selected[HitInfo.HitNode]:= true;
      Self.FocusedNode:= HitInfo.HitNode;
      Self.ScrollIntoView(HitInfo.HitNode,false,false);
    finally
      Self.EndUpdate;
    end;
    
    ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt];
    if ShiftState = [] then
    DoSelectedChange(Self.FocusedNode);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Dbl Click
-------------------------------------------------------------------------------}
procedure TCEFolderTree.HandleMouseDblClick(var Message: TWMMouse; const HitInfo:
    THitInfo);
begin
  if not assigned(HitInfo.HitNode) then
  begin
    inherited;
    Exit;
  end;

  if Self.Expanded[HitInfo.HitNode] then
  begin
    Self.ToggleNode(HitInfo.HitNode);
    Exit;
  end;

  Self.BeginUpdate;
  try
    if fAutoCollapse then
    Self.FullCollapse;
    
    Self.ClearSelection;
    Self.FullyVisible[HitInfo.HitNode]:= true;
    Self.ToggleNode(HitInfo.HitNode);
    Self.Selected[HitInfo.HitNode]:= true;
    Self.FocusedNode:= HitInfo.HitNode;
    Self.ScrollIntoView(HitInfo.HitNode,false,true);
  finally
    Self.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Paste Shortcut From Clipboard
-------------------------------------------------------------------------------}
function TCEFolderTree.PasteShortcutFromClipboard: Boolean;
var
  NS: TNamespace;
  NSA: TNamespaceArray;
  Handled: Boolean;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Handled := False;
      DoClipboardPaste(Handled);
      if not Handled then
      begin
        Result := False;
        if SelectedCount = 1 then
        begin
          SetLength(NSA, 1);
          if ValidateNamespace(GetFirstSelected, NS) then
          begin
            NSA[0] := NS;
            NS.Paste(Self, NSA, true);
            Result := True
          end
        end
      end else
        Result := True
    finally
      WaitCursor(False)
    end
  end else
    Result := False
end;


procedure TCEFolderTree.WMKILLFOCUS(var Message: TMessage);
begin
  inherited;
  //if Self.IsEditing then
  //Self.EndEditNode;
end;

{-------------------------------------------------------------------------------
  Refresh
-------------------------------------------------------------------------------}
function TCEFolderTree.Refresh: Boolean;
begin
  Result:= RefreshTree(true);
end;

{-------------------------------------------------------------------------------
  Scroll To View
-------------------------------------------------------------------------------}
procedure TCEFolderTree.ScrollToView(ANode: PVirtualNode; AVertical: Boolean =
    true; AHorizontal: Boolean = true);
var
  indentW, level: Integer;
  r: TRect;
begin
  if not assigned(ANode) then
  Exit;
  // Vertically
  if AVertical then
  ScrollIntoView(ANode, true);
  // Horizontally
  if AHorizontal then
  begin
    level:= GetNodeLevel(ANode)-1;
    indentW:= Integer(Indent) * level;
    r:= GetDisplayRect(ANode, NoColumn, false, true);
    if ((r.Right - r.Left) + indentW) > Self.ClientWidth then
    OffsetX:= -indentW;
  end;
end;

procedure TCEFolderTree.SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift =
    evsCurrent);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Node:= GetFirstSelected;
      if Assigned(Node) then
      begin
        if ValidateNamespace(Node, NS) then
        begin
          if NS.Delete(Self, SelectedToNamespaceArray, ShiftKeyState) then
          begin
            ReReadAndRefreshNode(node, false);
            DoSelectedChange(GetFirstSelected);
          end;
        end;
      end;
    finally
      WaitCursor(False)
    end
  end
end;

{-------------------------------------------------------------------------------
  Handle Selection Timer Event
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SelectionTimer(Sender: TObject);
begin
  fSelectionTimer.Enabled:= false;
  DoSelectedChange(FocusedNode);
end;

{-------------------------------------------------------------------------------
  Set BrowseZipFolders
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SetBrowseZipFolders(const Value: Boolean);
var
  opts: TVETMiscOptions;
  obj: TFileObjects;
begin
  fBrowseZipFolders:= Value;
  opts:= Self.TreeOptions.VETMiscOptions;
  if fBrowseZipFolders then
  Include(opts, toBrowseExecuteZipFolder)
  else
  Exclude(opts, toBrowseExecuteZipFolder);
  Self.TreeOptions.VETMiscOptions:= opts;

  obj:= FileObjects;
  if fBrowseZipFolders then
  Include(obj, foNonFolders)
  else
  Exclude(obj, foNonFolders);
  FileObjects:= obj;

end;

{-------------------------------------------------------------------------------
  Set Hidden Files
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SetHiddenFiles(const Value: Boolean);
var
  obj: TFileObjects;
begin
  fHiddenFiles:= Value;
  obj:= FileObjects;
  if fHiddenFiles then Include(obj, foHidden) else Exclude(obj, foHidden);
  FileObjects:= obj;
end;

{-------------------------------------------------------------------------------
  Handle WM_KeyDown message
-------------------------------------------------------------------------------}
procedure TCEFolderTree.WMKeyDown(var Message: TWMKeyDown);
var
  Shift: TShiftState;
begin
  inherited;
  if CenterOnExpand and assigned(Self.FocusedNode) then
  begin
    if Message.CharCode = VK_RIGHT then
    begin
      Shift:= KeyDataToShiftState(Message.KeyData);
      if Shift = [] then
      begin
        if Self.Expanded[Self.FocusedNode] then
        begin
          ScrollToView(Self.FocusedNode, false, true);
        end;
      end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle WM_KillFocus message
-------------------------------------------------------------------------------}
procedure TCE_VTEdit.WMKillFocus(var Message: TMessage);
begin
  Self.Perform(CM_Exit,0,0);
  inherited;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCE_StringEditLink
-------------------------------------------------------------------------------}
constructor TCE_StringEditLink.Create;
var
  fEdit: TVTEdit;
begin
  //inherited;
  FEdit := TCE_VTEdit.Create(Self);
  fEdit.Visible := False;
  fEdit.BorderStyle := bsSingle;
  fEdit.AutoSize := False;

  Self.Edit:= fEdit;
end;



end.
