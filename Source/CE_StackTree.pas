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
//  The Original Code is CE_StackTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_StackTree;

interface

uses
  // CE
  CE_Stacks, CE_Utils, CE_CommonObjects, CE_FileUtils,
  // VT
  VirtualTrees,
  // Tnt
  TntClasses,
  // VS Tools
  MPDataObject, MPShellUtilities, MPCommonObjects, MPCommonUtilities,
  VirtualExplorerTree, VirtualResources, VirtualShellNotifier,
  // System Units
  SysUtils, Classes, Windows, Forms, Controls, ActiveX, ImgList, ShlObj,
  Graphics, Menus, Messages;

type
  TCEStackDataObject = class(TVETDataObject);

  TCEStackItemType = (sitGroup, sitShell);

  PCEStackItemData = ^ACEStackItemData;
  ACEStackItemData = record
    Caption: WideString;
    IconIndex: Integer;
    OverlayIndex: Integer;
    ItemType: TCEStackItemType;
    CEPath: WideString;
    PathType: TCEPathType;
    Offline: Boolean;
    fLastOfflineCheck: Integer;
    fLastIconRefresh: Integer;
    Data: TObject;
  end;

  TCEStackTree = class(TVirtualStringTree, IVirtualShellNotify)
  private
    fActiveDataObject: IDataObject;
    fActiveStack: TCEStackItem;
    fAutoCollapse: Boolean;
    fAutoExpand: Boolean;
    fAutoSaveActiveStack: Boolean;
    fBackgroundPopupMenu: TPopupMenu;
    fGroupImageIndex: Integer;
    fGroupOpenImageIndex: Integer;
    fCleanList: TList;
    fFullExpandOnLoad: Boolean;
    fMaxHintItemCount: Integer;
    fNotAvailableImageIndex: Integer;
    fOnSafeOperationsOnlyChange: TNotifyEvent;
    fSafeOperationsOnly: Boolean;
    fShowWarnings: Boolean;
    fStacks: TCEStacks;
    function GetOkToShellNotifyDispatch: Boolean;
    function GetStacks: TCEStacks;
    procedure SetSafeOperationsOnly(const Value: Boolean);
  protected
    LastIconRefresh: Integer;
    LastOfflineCheck: Integer;
    function CanShowDragImage: Boolean; override;
    procedure DoAutoSaveActiveStack; virtual;
    function DoCancelEdit: Boolean; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed:
        Boolean); override;
    procedure DoCollapsed(Node: PVirtualNode); override;
    procedure DoContextMenuCmdCallback(Namespace: TNamespace; Verb: WideString;
        MenuItemID: Integer; var Handled: Boolean); virtual;
    procedure DoContextMenuShowCallback(Namespace: TNamespace; Menu: hMenu; var
        Allow: Boolean); virtual;
    function DoCreateDataObject: IDataObject; override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats:
        TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode:
        TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt:
        TPoint; Mode: TDropMode; var Effect: Integer): Boolean; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoEndEdit: Boolean; override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
        TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
        override;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex; var
        LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: UnicodeString); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
        override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text:
        UnicodeString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position:
        TPoint); override;
    procedure DragAndDrop(AllowedEffects: Integer; DataObject: IDataObject;
        DragEffect: Integer); override;
    procedure FreeNamespaceArray(NamespaceArray: TNamespaceArray);
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);
        override;
    procedure Notify(var Msg: TMessage);
    procedure UpdateScrollbarSize;
    procedure WMCreate(var Msg: TMessage); message WM_CREATE;
    procedure WMNCDestroy(var Msg: TMessage); message WM_NCDESTROY;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
    property OkToShellNotifyDispatch: Boolean read GetOkToShellNotifyDispatch;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckOfflineState(ANode: PVirtualNode; AForce: Boolean = false);
    procedure CheckOfflineStates(AForce: Boolean = false);
    procedure CleanItems(ClearList: Boolean = false);
    procedure Clear; override;
    procedure DeleteSelectedNodes; override;
    function FindByPath(APath: WideString; Offset: PVirtualNode = nil):
        PVirtualNode;
    function InsertGroupNode(ToNode: PVirtualNode; Mode: TVTNodeAttachMode;
        ACaption: WideString = ''): PVirtualNode; virtual;
    function InsertShellNode(ToNode: PVirtualNode; Mode: TVTNodeAttachMode; APIDL:
        PItemIDList): PVirtualNode; virtual;
    procedure LoadFromStack(AStack: TCEStackItem); virtual;
    procedure RefreshIcons(AForce: Boolean = false);
    procedure RefreshIcon(ANode: PVirtualNode; AForce: Boolean = false);
    function RemoveEmptyGroups: Boolean;
    function SaveActiveStack: Boolean;
    procedure SaveToStack(AStack: TCEStackItem); virtual;
    function SelectedToNamespaceArray(AddToCleanList: Boolean = false):
        TNamespaceArray;
    function GroupToNamespaceArray(AGroupNode: PVirtualNode; AddToCleanList:
        Boolean = false): TNamespaceArray;
    property ActiveStack: TCEStackItem read fActiveStack write fActiveStack;
    property AutoCollapse: Boolean read fAutoCollapse write fAutoCollapse;
    property AutoExpand: Boolean read fAutoExpand write fAutoExpand;
  published
    property AutoSaveActiveStack: Boolean read fAutoSaveActiveStack write
        fAutoSaveActiveStack;
    property BackgroundPopupMenu: TPopupMenu read fBackgroundPopupMenu write
        fBackgroundPopupMenu;
    property FullExpandOnLoad: Boolean read fFullExpandOnLoad write
        fFullExpandOnLoad;
    property GroupImageIndex: Integer read fGroupImageIndex write fGroupImageIndex;
    property GroupOpenImageIndex: Integer read fGroupOpenImageIndex write
        fGroupOpenImageIndex;
    property MaxHintItemCount: Integer read fMaxHintItemCount write
        fMaxHintItemCount;
    property NotAvailableImageIndex: Integer read fNotAvailableImageIndex write
        fNotAvailableImageIndex;
    property SafeOperationsOnly: Boolean read fSafeOperationsOnly write
        SetSafeOperationsOnly;
    property ShowWarnings: Boolean read fShowWarnings write fShowWarnings;
    property Stacks: TCEStacks read GetStacks write fStacks;
    property OnSafeOperationsOnlyChange: TNotifyEvent read
        fOnSafeOperationsOnlyChange write fOnSafeOperationsOnlyChange;
  end;

type
  // Simpilies dealing with the CFSTR_LOGICALPERFORMEDDROPEFFECT format
  TCELogicalPerformedDropEffect = class(TCommonClipboardFormat)
  private
    fAction: Cardinal;
  public
    function GetFormatEtc: TFormatEtc; override;
    function LoadFromDataObject(DataObject: IDataObject): Boolean; override;
    function SaveToDataObject(DataObject: IDataObject): Boolean; override;

    property Action: Cardinal read fAction write fAction;
  end;

implementation

uses
  CE_LanguageEngine, dCE_Actions, CE_GlobalCtrl;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEStackTree
-------------------------------------------------------------------------------}
constructor TCEStackTree.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize:= SizeOf(ACEStackItemData);
  fStacks:= nil;
  fGroupImageIndex:= -1;
  fGroupOpenImageIndex:= -1;
  fNotAvailableImageIndex:= -1;
  fSafeOperationsOnly:= true;
  fMaxHintItemCount:= 20;
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  DragMode:= dmAutomatic;
  DragOperations:= [doCopy,doMove,doLink];
  Header.Columns.Add.Text:= 'Name';
  Header.Options:= [hoAutoResize,hoColumnResize,hoDrag,hoShowSortGlyphs];
  HintMode:= hmHint;
  ShowHint:= true;
  TreeOptions.PaintOptions:= [toShowButtons,toShowDropmark,toShowRoot,toShowTreeLines, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions:= [toMultiSelect,toRightClickSelect];
  TreeOptions.StringOptions:= [toSaveCaptions,toShowStaticText,toAutoAcceptEditChange];
  TreeOptions.MiscOptions:= [toEditable, toAcceptOLEDrop,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick,toWheelPanning,toEditOnClick];

  LastIconRefresh:= GetTickCount;
  LastOfflineCheck:= LastIconRefresh;

  fCleanList:= TList.Create;
  Self.Caption:= 'test';
end;

{-------------------------------------------------------------------------------
  Destroy TCEStackTree
-------------------------------------------------------------------------------}
destructor TCEStackTree.Destroy;
begin
  fCleanList.Destroy;
  inherited;
end;

{-------------------------------------------------------------------------------
  Can Show DragImage
-------------------------------------------------------------------------------}
function TCEStackTree.CanShowDragImage: Boolean;
begin
  // Vista and up
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
  Result:= False
  else // Older Windows versions
  Result:= inherited CanShowDragImage;
end;

{-------------------------------------------------------------------------------
  Check Item Offline state
-------------------------------------------------------------------------------}
procedure TCEStackTree.CheckOfflineState(ANode: PVirtualNode; AForce: Boolean =
    false);
var
  data: PCEStackItemData;
  ns: TNamespace;
begin
  data:= Self.GetNodeData(ANode);
  if data.ItemType = sitShell then
  begin
    if not AForce and (data.fLastOfflineCheck >= LastOfflineCheck) then
    Exit;

    if data.Offline then
    begin
      if CEPathExists(data.CEPath, data.PathType) then
      begin
        ns:= CEPathToNamespace(data.CEPath, data.PathType);
        data.Caption:= ns.NameNormal;
        data.IconIndex:= ns.GetIconIndex(false, icSmall, true);
        data.OverlayIndex:= ns.OverlayIconIndex;
        data.fLastIconRefresh:= GetTickCount;
        data.Offline:= false;
        ns.Free;
      end;
    end
    else
    begin
      data.Offline:= not CEPathExists(data.CEPath, data.PathType);
    end;
    data.fLastOfflineCheck:= GetTickCount;
  end;
end;

{-------------------------------------------------------------------------------
  Check Offline States
-------------------------------------------------------------------------------}
procedure TCEStackTree.CheckOfflineStates(AForce: Boolean = false);
var
  node: PVirtualNode;
  data: PCEStackItemData;
  ns: TNamespace;
begin
  LastOfflineCheck:= GetTickCount;
  if AForce then
  begin
    node:= Self.GetFirst;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitShell then
      begin
        if data.Offline then
        begin
          if CEPathExists(data.CEPath, data.PathType) then
          begin
            ns:= CEPathToNamespace(data.CEPath, data.PathType);
            data.Caption:= ns.NameNormal;
            data.IconIndex:= ns.GetIconIndex(false, icSmall, true);
            data.OverlayIndex:= ns.OverlayIconIndex;
            data.fLastIconRefresh:= LastOfflineCheck;
            data.Offline:= false;
            ns.Free;
          end;
        end
        else
        begin
          data.Offline:= not CEPathExists(data.CEPath, data.PathType);
        end;
        data.fLastOfflineCheck:= LastOfflineCheck;
      end;
      node:= Self.GetNext(node);
    end;
  end;
  Self.Refresh;
end;

{-------------------------------------------------------------------------------
  Clean Items
-------------------------------------------------------------------------------}
procedure TCEStackTree.CleanItems(ClearList: Boolean = false);
var
  i: Integer;
  data: PCEStackItemData;
begin
  i:= 0;
  while i < fCleanList.Count do
  begin
    data:= Self.GetNodeData(fCleanList.Items[i]);
    if not data.Offline then // Clean only items that were online
    begin
      // Delete item if it doesn't exist anymore
      if not CEPathExists(data.CEPath, data.PathType) then
      begin
        Self.DeleteNode(fCleanList.Items[i]);
        fCleanList.Delete(i);
      end
      else
      i:= i + 1;
    end
    else // remove offline items from list
    begin
      fCleanList.Delete(i);
    end;
  end;
  if ClearList then
  fCleanList.Clear;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEStackTree.Clear;
begin
  inherited;
  DoAutoSaveActiveStack;
end;

{-------------------------------------------------------------------------------
  DeleteSelectedNodes
-------------------------------------------------------------------------------}
procedure TCEStackTree.DeleteSelectedNodes;
begin
  inherited;
  DoAutoSaveActiveStack;
end;

{-------------------------------------------------------------------------------
  Do Auto Save Active Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoAutoSaveActiveStack;
begin
  if not ReadOnlySettings and AutoSaveActiveStack and (UpdateCount = 0) then
  SaveActiveStack;
end;

{-------------------------------------------------------------------------------
  Do CancelEdit
-------------------------------------------------------------------------------}
function TCEStackTree.DoCancelEdit: Boolean;
begin
  Result:= inherited DoCancelEdit;
  Self.InvalidateNode(Self.FocusedNode);
end;

{-------------------------------------------------------------------------------
  Do CanEdit
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var
    Allowed: Boolean);
var
  data: PCEStackItemData;
begin
  data:= Self.GetNodeData(Node);
  Allowed:= (data.ItemType = sitGroup) and (Column = 0);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Collapsed
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoCollapsed(Node: PVirtualNode);
begin
  inherited;
  UpdateScrollbarSize;
end;

{-------------------------------------------------------------------------------
  Do ContextMenuCmdCallback
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoContextMenuCmdCallback(Namespace: TNamespace; Verb:
    WideString; MenuItemID: Integer; var Handled: Boolean);
begin
  if MenuItemID = 555 then
  begin
    self.DeleteSelectedNodes;
    DoAutoSaveActiveStack;
    Handled:= true;
  end;

  if not Handled then
  DoGlobalContextMenuCmd(Self, Namespace, Verb, MenuItemID, Handled);
end;

{-------------------------------------------------------------------------------
  Do ContextMenuShowCallback
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoContextMenuShowCallback(Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);
var
  infoA: TMenuItemInfoA;
  infoW: TMenuItemInfoW;
  ws: WideString;
  s: String;
begin
  DoGlobalContextMenuShow(Self, Namespace, Menu, Allow);
  
  // Add "Remove from Stack" item
  if Self.SelectedCount > 1 then
  ws:= _('Remove from Stack') + ' (' + IntToStr(Self.SelectedCount) + ' ' + _('items') + ')'
  else
  ws:= _('Remove from Stack');
  if IsUnicode then
  begin
    FillChar(infoW, SizeOf(infoW), #0);
    infoW.cbSize:= SizeOf(infoW);
    infoW.fMask:= MIIM_TYPE or MIIM_ID;
    infoW.fType:= MFT_STRING;
    infoW.dwTypeData:= PWideChar(ws);
    infoW.cch:= Length(ws) + 1;
    infoW.wID:= 555;
    InsertMenuItemW(Menu, 0, true, infoW);
  end
  else
  begin
    s:= String(ws);
    FillChar(infoA, SizeOf(infoA), #0);
    infoA.cbSize:= SizeOf(infoA);
    infoA.fMask:= MIIM_TYPE or MIIM_ID;
    infoA.fType:= MFT_STRING;
    infoA.dwTypeData:= PChar(s);
    infoA.cch:= Length(s) + 1;
    infoA.wID:= 555;
    InsertMenuItemA(Menu, 0, true, infoA);
  end;

  // Add Separators
  FillChar(infoA, SizeOf(infoA), #0);
  infoA.cbSize:= SizeOf(infoA);
  infoA.fMask:= MIIM_TYPE or MIIM_ID;
  infoA.fType:= MFT_SEPARATOR;
  InsertMenuItemA(Menu, 1, true, infoA);
  InsertMenuItemA(Menu, 1, true, infoA);

  // Add Warning item
  if ShowWarnings then
  begin
    InsertMenuItemA(Menu, 1, true, infoA);
    if IsUnicode then
    begin
      ws:= _('WARNING: Real file operations below');
      FillChar(infoW, SizeOf(infoW), #0);
      infoW.cbSize:= SizeOf(infoW);
      infoW.fMask:= MIIM_TYPE or MIIM_STATE;
      infoW.fType:= MFT_STRING;
      infoW.dwTypeData:= PWideChar(ws);
      infoW.cch:= Length(ws) + 1;
      infoW.fState:= MFS_DISABLED	or MFS_HILITE;
      InsertMenuItemW(Menu, 2, true, infoW);
    end
    else
    begin
      ws:= _('WARNING: Real file operations below');
      FillChar(infoA, SizeOf(infoA), #0);
      infoA.cbSize:= SizeOf(infoA);
      infoA.fMask:= MIIM_TYPE or MIIM_STATE;
      infoA.fType:= MFT_STRING;
      infoA.dwTypeData:= PChar(s);
      infoA.cch:= Length(s) + 1;
      infoA.fState:= MFS_DISABLED	or MFS_HILITE;
      InsertMenuItemA(Menu, 2, true, infoA);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do CreateDataObject
-------------------------------------------------------------------------------}
function TCEStackTree.DoCreateDataObject: IDataObject;
var
  DataObj: TCEStackDataObject;
  NSA: TNamespaceArray;
  NSList: TList;
begin
  NSA:= nil;
  Result:= nil;
  if not Assigned(Result) then
  begin
    if (Self.SelectedCount > 0) then
    begin
      fCleanList.Clear;
      NSA:= SelectedToNamespaceArray(true);
      try
        // Create IDataObject
        DataObj:= TCEStackDataObject.Create(Self, False);
        Result:= DataObj;
        fActiveDataObject:= Result;
        // Create ShellDataObject
        DataObj.ShellDataObject:= nil;
        NSList:= NamespaceToNamespaceList(NSA);
        try
          CreateFullyQualifiedShellDataObject(NSList, true,  Result);
        finally
          FreeAndNil(NSList)
        end;
      finally
        FreeNamespaceArray(NSA);
      end;
    end
  end
end;

{-------------------------------------------------------------------------------
  Do DragDrop
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoDragDrop(Source: TObject; DataObject: IDataObject;
    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer;
    Mode: TDropMode);
var
  hd: TCommonShellIDList;
  i: Integer;
  Attachmode: TVTNodeAttachMode;
  node, groupNode: PVirtualNode;
  data: PCEStackItemData;
  Nodes: TNodeArray;
begin
  inherited;
  BeginUpdate;
  try
    groupNode:= nil;
    // Get Attach mode
    case Mode of
      dmAbove:
        AttachMode := amInsertBefore;
      dmOnNode:
        AttachMode := amAddChildLast;
      dmBelow:
        AttachMode := amInsertAfter;
    else
      AttachMode := amNowhere;
    end;

    // Move items in the tree
    if Self.Dragging then
    begin
      // Add group if needed
      if Mode = dmNowhere then
      begin
        AttachMode:= amAddChildLast;
        groupNode:= InsertGroupNode(nil, amInsertAfter, '');
        node:= groupNode;
      end
      else
      begin
        node:= Self.DropTargetNode;
      end;

      data:= Self.GetNodeData(node);
      if assigned(data) and (data.ItemType = sitShell) then
      AttachMode:= amInsertBefore;

      Nodes:= self.GetSortedSelection(True);
      for I := 0 to High(Nodes) do
      begin
        if Mode = dmOnNode then
        begin
          if node.Parent = Nodes[i].Parent then
          begin
            if node.Index > Nodes[i].Index then
            AttachMode:= amInsertAfter
            else
            AttachMode:= amInsertBefore;
          end;
        end;

        data:= Self.GetNodeData(Nodes[i]);
        if data.ItemType = sitGroup then
        begin
          data:= Self.GetNodeData(node);
          if data.ItemType = sitShell then
          self.MoveTo(Nodes[I], node.Parent, AttachMode, False)
          else
          self.MoveTo(Nodes[I], node, AttachMode, False);
        end
        else
        self.MoveTo(Nodes[I], node, AttachMode, False);
      end;
    end
    // Drop items from outside
    else if assigned(DataObject) then
    begin
      Effect:= DROPEFFECT_LINK;
      hd:= TCommonShellIDList.Create;
      try
        hd.LoadFromDataObject(DataObject);
        data:= Self.GetNodeData(Self.DropTargetNode);
        // Insert group
        if (Mode = dmNowhere) then
        begin
          AttachMode:= amAddChildLast;
          groupNode:= InsertGroupNode(nil, amInsertAfter, '');
          node:= groupNode;
        end
        else if assigned(data) and (data.ItemType = sitGroup) and (Mode <> dmOnNode) then
        begin
          AttachMode:= amAddChildLast;
          if Mode = dmAbove then
          groupNode:= InsertGroupNode(Self.DropTargetNode, amInsertBefore, '')
          else
          groupNode:= InsertGroupNode(Self.DropTargetNode, amInsertAfter, '');
          node:= groupNode;
        end
        else // Make sure items are added as a child to Group
        begin
          node:= Self.DropTargetNode;
          if Mode = dmOnNode then
          begin
            data:= Self.GetNodeData(node);
            if data.ItemType = sitShell then
            AttachMode := amInsertBefore;
          end;
        end;
        // Add shell items
        for i:= 0 to hd.PIDLCount-1 do
        begin
          InsertShellNode(node, AttachMode, hd.AbsolutePIDL(i));
        end;
      finally
        hd.Free;
      end;
    end;
    RemoveEmptyGroups;
    if assigned(groupNode) then
    Self.Expanded[groupNode]:= true;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  DoDragOver
-------------------------------------------------------------------------------}
function TCEStackTree.DoDragOver(Source: TObject; Shift: TShiftState; State:
    TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer): Boolean;
var
  node: PVirtualNode;
  data: PCEStackItemData;
  itemType: TCEStackItemType;
  isFirst: Boolean;
begin
  Result:= true;
  if Self.Dragging then
  begin
    Effect:= DROPEFFECT_MOVE;

    // Allow item move only if all selected items are same type
    itemType:= sitShell;
    isFirst:= true;
    node:= Self.GetFirstSelected;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if isFirst then
      begin
        itemType:= data.ItemType;
        isFirst:= false;
      end
      else if itemType <> data.ItemType then
      begin
        Result:= false;
        break;
      end;
      node:= Self.GetNextSelected(node);
    end;

    // Allow group move only to other groups
    if Result and (itemType = sitGroup) then
    begin
      if assigned(Self.DropTargetNode) then
      begin
        data:= Self.GetNodeData(Self.DropTargetNode);
        Result:= data.ItemType = sitGroup;
      end
      else
      Result:= false;
    end;
  end
  else
  Effect:= DROPEFFECT_LINK;
end;

{-------------------------------------------------------------------------------
  Do EndDrag
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoEndDrag(Target: TObject; X, Y: Integer);
var
  Effect: TCELogicalPerformedDropEffect;
begin
  inherited;

  if assigned(fActiveDataObject) then
  begin
    Effect:= TCELogicalPerformedDropEffect.Create;
    try
      if fActiveDataObject.QueryGetData(Effect.GetFormatEtc) = S_OK then
      begin
        CleanItems(true);
        //Effect.LoadFromDataObject(fActiveDataObject);
        //if (Effect.Action = DROPEFFECT_MOVE) then
        //CleanItems(true);
      end;
    finally
      Effect.Free;
    end;
    fActiveDataObject:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Do EndEdit
-------------------------------------------------------------------------------}
function TCEStackTree.DoEndEdit: Boolean;
begin
  Result:= inherited DoEndEdit;
  Self.InvalidateNode(Self.FocusedNode);
end;

{-------------------------------------------------------------------------------
  Do Expanded
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoExpanded(Node: PVirtualNode);
begin
  inherited;
  UpdateScrollbarSize;
end;

{-------------------------------------------------------------------------------
  Do FreeNode
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoFreeNode(Node: PVirtualNode);
var
  data: PCEStackItemData;
begin
  data:= Self.GetNodeData(Node);
  if assigned(data.Data) then
  data.Data.Free;  
  inherited;
end;

{-------------------------------------------------------------------------------
  Do GetImageIndex
-------------------------------------------------------------------------------}
function TCEStackTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
    Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer):
    TCustomImageList;
var
  data: PCEStackItemData;
begin
  Result:= nil;
  if (Kind = ikState) then
  Exit;
  
  data:= Self.GetNodeData(Node);
  if data.ItemType = sitShell then
  begin
    if Kind <> ikOverlay then
    CheckOfflineState(Node);

    if not data.Offline then
    begin
      Result:= SmallSysImages;
      if Kind = ikOverlay then
      Index:= data.OverlayIndex
      else
      Index:= data.IconIndex;
    end
    else
    begin
      Result:= Images;
      if Kind = ikOverlay then
      Index:= -1
      else
      Index:= fNotAvailableImageIndex;
      Ghosted:= true;
    end;
  end
  else if (data.ItemType = sitGroup) and (Kind <> ikOverlay) then
  begin
    Result:= Images;
    if Self.Expanded[Node] then
    Index:= fGroupOpenImageIndex
    else
    Index:= fGroupImageIndex;
    Ghosted:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Do GetNodeHint
-------------------------------------------------------------------------------}
function TCEStackTree.DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex;
    var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  data: PCEStackItemData;
  chNode: PVirtualNode;
  c: Integer;
begin
  c:= 0;
  Result:= inherited DoGetNodeHint(Node, Column, LineBreakStyle);
  data:= Self.GetNodeData(Node);
  if data.ItemType = sitShell then
  begin
    LineBreakStyle:= hlbForceSingleLine;
    if data.PathType = ptPath then
    Result:= data.CEPath
    else
    Result:= data.Caption;
  end
  else
  begin
    LineBreakStyle:= hlbForceMultiLine;
    if Self.ChildCount[Node] = 1 then
    Result:= data.Caption + ' (' + IntToStr(Self.ChildCount[Node]) + ' ' + _('item')+ '):'
    else
    Result:= data.Caption + ' (' + IntToStr(Self.ChildCount[Node]) + ' ' + _('items')+ '):';
    chNode:= Self.GetFirstChild(Node);
    while assigned(chNode) do
    begin
      data:= Self.GetNodeData(chNode);
      if data.PathType = ptPath then
      Result:= Result + #13#10 + data.CEPath
      else
      Result:= Result + #13#10 + data.Caption;
      c:= c + 1;
      if c >= fMaxHintItemCount then
      begin
        Result:= Result + #13#10 + '...';
        break;
      end;
      chNode:= Self.GetNextSibling(chNode);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GetText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: UnicodeString);
var
  data: PCEStackItemData;
  c: Integer;
begin
  data:= Self.GetNodeData(Node);
  if Column = 0 then
  begin
    if data.ItemType = sitGroup then
    begin
      if TextType = ttStatic then
      begin
        c:= Self.ChildCount[Node];
        if c = 1 then
        Text:= '(' + IntToStr(Self.ChildCount[Node]) + ' ' + _('item') + ')'
        else
        Text:= '(' + IntToStr(Self.ChildCount[Node]) + ' ' + _('items') + ')'
      end
      else
      begin
        Text:= data.Caption;
      end;
    end
    else
    begin
      if TextType = ttNormal then
      Text:= data.Caption;
    end;
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Do KeyAction
-------------------------------------------------------------------------------}
function TCEStackTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState):
    Boolean;
begin
  Result:= inherited DoKeyAction(CharCode, Shift);
  if CharCode = VK_DELETE then
  begin
    self.DeleteSelectedNodes;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Do NewText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
    Text: UnicodeString);
var
  data: PCEStackItemData;
begin
  inherited;
  if Column = 0 then
  begin
    data:= Self.GetNodeData(Node);
    if data.ItemType = sitGroup then
    data.Caption:= Text;
  end;
  DoAutoSaveActiveStack;
end;

{-------------------------------------------------------------------------------
  Do PaintText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
    Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PCEStackItemData;
begin
  inherited;
  data:= Self.GetNodeData(Node);
  if (TextType = ttStatic) or data.Offline then
  Canvas.Font.Color:= clGrayText
  else
  Canvas.Font.Color:= clWindowText;
end;

{-------------------------------------------------------------------------------
  Do PopupMenu
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex;
    Position: TPoint);
var
  data: PCEStackItemData;
  info: THitInfo;
  p: TPoint;
  nArray: TNamespaceArray;
  ns: TNamespace;
  pidl: PItemIDList;
begin
  Self.GetHitTestInfoAt(Position.X, Position.Y, false, info);
  if assigned(info.HitNode) and ((hiOnNormalIcon in info.HitPositions) or (hiOnItemLabel in info.HitPositions)) then
  begin
    data:= Self.GetNodeData(Node);
    if data.ItemType = sitShell then
    begin
      pidl:= CEPathToPIDL(data.CEPath);
      if assigned(pidl) then
      begin
        ns:= TNamespace.Create(pidl, nil);
        try
          nArray:= SelectedToNamespaceArray;
          ns.ShowContextMenuMulti(Self, DoContextMenuCmdCallback, DoContextMenuShowCallback, nil, nArray, nil, nil, '', ns);
        finally
          ns.Free;
          FreeNamespaceArray(nArray);
        end;
      end;
    end
    else if data.ItemType = sitGroup then
    begin
      nArray:= GroupToNamespaceArray(Node);
      try
        if Length(nArray) > 0 then
        nArray[0].ShowContextMenuMulti(Self, DoContextMenuCmdCallback, DoContextMenuShowCallback, nil, nArray);
      finally
        FreeNamespaceArray(nArray);
      end;
    end;
  end
  else if not (hiOnItemButton in info.HitPositions) then
  begin
    if assigned(BackgroundPopupMenu) then
    begin
      p:= ClientToScreen(Position);
      BackgroundPopupMenu.Popup(p.X, p.Y);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Drag And Drop
-------------------------------------------------------------------------------}
procedure TCEStackTree.DragAndDrop(AllowedEffects: Integer; DataObject:
    IDataObject; DragEffect: Integer);
var
  effect: Integer;
begin
  if SafeOperationsOnly then
  effect:= DROPEFFECT_COPY or DROPEFFECT_LINK
  else
  effect:= DROPEFFECT_COPY or DROPEFFECT_LINK or DROPEFFECT_MOVE;
  // Vista and up
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
  SHDoDragDrop_MP(Handle, DataObject, nil, effect, DragEffect)
  else // Older Windows versions
  inherited DragAndDrop(effect, DataObject, DragEffect);
end;

{-------------------------------------------------------------------------------
  Find By Path
-------------------------------------------------------------------------------}
function TCEStackTree.FindByPath(APath: WideString; Offset: PVirtualNode =
    nil): PVirtualNode;
var
  node: PVirtualNode;
  data: PCEStackItemData;
begin
  Result:= nil;
  if assigned(Offset) then
  node:= Self.GetNext(Offset)
  else
  node:= Self.GetFirst;
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if data.ItemType = sitShell then
    begin
      if data.CEPath = APath then
      begin
        Result:= node;
        Break;
      end;
    end;
    node:= Self.GetNext(node);
  end;
end;

{-------------------------------------------------------------------------------
  Free NamespaceArray
-------------------------------------------------------------------------------}
procedure TCEStackTree.FreeNamespaceArray(NamespaceArray: TNamespaceArray);
var
  i: Integer;
begin
  for i:= 0 to Length(NamespaceArray) - 1 do
  NamespaceArray[i].Free;
end;

{-------------------------------------------------------------------------------
  Get OkToShellNotifyDispatch
-------------------------------------------------------------------------------}
function TCEStackTree.GetOkToShellNotifyDispatch: Boolean;
begin
  Result:= not Self.Dragging;
end;

{-------------------------------------------------------------------------------
  Get Stacks
-------------------------------------------------------------------------------}
function TCEStackTree.GetStacks: TCEStacks;
begin
  Result:= fStacks;
  if not assigned(Result) then
  raise Exception.CreateFmt('Stacks must be assigned before using TCEStackTree!', []);
end;


{-------------------------------------------------------------------------------
  Insert Group Node
-------------------------------------------------------------------------------}
function TCEStackTree.InsertGroupNode(ToNode: PVirtualNode; Mode:
    TVTNodeAttachMode; ACaption: WideString = ''): PVirtualNode;
var
  data, groupData: PCEStackItemData;
  groupNode: PVirtualNode;
  i: Integer;
  found: Boolean;
begin
  BeginUpdate;
  try
    Result:= InsertNode(ToNode, Mode);
    data:= GetNodeData(Result);
    data.ItemType:= sitGroup;
    // Auto create caption
    if ACaption = '' then
    begin
      i:= 0;
      repeat
        found:= false;
        i:= i + 1;
        groupNode:= Self.GetFirstChild(nil);
        while assigned(groupNode) do
        begin
          groupData:= Self.GetNodeData(groupNode);
          if groupData.Caption = (_('Group') + ' ' + IntToStr(i)) then
          begin
            found:= true;
            break;
          end;
          groupNode:= Self.GetNextSibling(groupNode);
        end;
      until not found;

      data.Caption:= _('Group') + ' ' + IntToStr(i);
    end
    else
    data.Caption:= ACaption;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Insert Group Node
-------------------------------------------------------------------------------}
function TCEStackTree.InsertShellNode(ToNode: PVirtualNode; Mode:
    TVTNodeAttachMode; APIDL: PItemIDList): PVirtualNode;
var
  data: PCEStackItemData;
begin
  BeginUpdate;
  try
    Result:= InsertNode(ToNode, Mode);
    data:= GetNodeData(Result);
    data.ItemType:= sitShell;
    data.Offline:= true;
    data.CEPath:= PIDLToCEPath(APIDL);
    data.PathType:= GetCEPathType(data.CEPath);
    data.Caption:= WideExtractFileName(data.CEPath, false);
    data.IconIndex:= -1;
    data.OverlayIndex:= -1;
    data.fLastOfflineCheck:= 0;
    data.fLastIconRefresh:= 0;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Load From Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.LoadFromStack(AStack: TCEStackItem);
var
  i, i2: Integer;
  list, l: TTntStrings;
  groupNode, node: PVirtualNode;
  data: PCEStackItemData;
begin
  fActiveStack:= AStack;
  if not assigned(AStack) then
  Exit;

  l:= TTntStringList.Create;
  BeginUpdate;
  Self.Clear;
  try
    for i:= 0 to AStack.GroupCount - 1 do
    begin
      groupNode:= InsertGroupNode(nil, amAddChildLast, AStack.GroupName[i]);
      list:= AStack.GroupItems[i];
      for i2:= 0 to list.Count - 1 do
      begin
        if list.Strings[i2] <> '' then
        begin
          l.CommaText:= list.Strings[i2];
          if l.Count > 0 then
          begin
            node:= InsertNode(groupNode, amAddChildLast);
            data:= GetNodeData(node);
            data.ItemType:= sitShell;
            data.Offline:= true;
            data.CEPath:= l.Strings[0];
            data.PathType:= GetCEPathType(data.CEPath);
            if l.Count > 1 then
            data.Caption:= l.Strings[1]
            else
            data.Caption:= WideExtractFileName(data.CEPath, false);
            data.IconIndex:= -1;
            data.OverlayIndex:= -1;
            data.fLastOfflineCheck:= 0;
            data.fLastIconRefresh:= 0;
          end;
        end;
      end;
    end;
  finally
    if FullExpandOnLoad then
    Self.FullExpand;

    EndUpdate;
    l.Free;
    CheckOfflineStates;
  end;
end;

{-------------------------------------------------------------------------------
  Notify (on Shell Notify)
-------------------------------------------------------------------------------}
procedure TCEStackTree.Notify(var Msg: TMessage);
begin
  if HandleAllocated then
  WMShellNotify(Msg)
end;

{-------------------------------------------------------------------------------
  Refresh Icons
-------------------------------------------------------------------------------}
procedure TCEStackTree.RefreshIcons(AForce: Boolean = false);
var
  node: PVirtualNode;
  data: PCEStackItemData;
  ns: TNamespace;
begin
  LastIconRefresh:= GetTickCount;
  if AForce then
  begin
    node:= GetFirst;
    while assigned(node) do
    begin
      data:= GetNodeData(node);
      if data.ItemType = sitShell then
      begin
        ns:= TNamespace.Create(CEPathToPIDL(data.CEPath), nil);
        data.IconIndex:= ns.GetIconIndex(false, icSmall, true);
        data.OverlayIndex:= ns.OverlayIconIndex;
        data.fLastIconRefresh:= GetTickCount;
        ns.Free;
      end;
      node:= GetNext(node);
    end;
  end;
  Self.Refresh;
end;

{-------------------------------------------------------------------------------
  Refresh Item Icon
-------------------------------------------------------------------------------}
procedure TCEStackTree.RefreshIcon(ANode: PVirtualNode; AForce: Boolean =
    false);
var
  data: PCEStackItemData;
  ns: TNamespace;
begin
  data:= Self.GetNodeData(ANode);
  if data.ItemType = sitShell then
  begin
    if not AForce and (data.fLastIconRefresh >= LastIconRefresh) then
    Exit;

    ns:= TNamespace.Create(CEPathToPIDL(data.CEPath), nil);
    data.IconIndex:= ns.GetIconIndex(false, icSmall, true);
    data.OverlayIndex:= ns.OverlayIconIndex;
    data.fLastIconRefresh:= GetTickCount;
    ns.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Remove Empty Groups (Result is TRUE if node(s) have been deleted)
-------------------------------------------------------------------------------}
function TCEStackTree.RemoveEmptyGroups: Boolean;
var
  node, tmpNode: PVirtualNode;
  data: PCEStackItemData;
begin
  Result:= false;
  BeginUpdate;
  try
    node:= Self.GetFirst;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitGroup then
      begin
        tmpNode:= node;
        node:= GetNext(node);
        if not Self.HasChildren[tmpNode] then
        begin
          Self.DeleteNode(tmpNode);
          Result:= true;
        end;
      end
      else
      node:= GetNext(node);
    end;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Save Active Stack
-------------------------------------------------------------------------------}
function TCEStackTree.SaveActiveStack: Boolean;
begin
  Result:= assigned(ActiveStack);
  if Result then
  begin
    SaveToStack(ActiveStack);
    if ActiveStack.StackPath <> '' then
    ActiveStack.SaveToFile(ActiveStack.StackPath);
  end;
end;

{-------------------------------------------------------------------------------
  Save to Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.SaveToStack(AStack: TCEStackItem);

  procedure enumGroup(groupNode: PVirtualNode; groupItems: TTntStrings);
  var
    chNode: PVirtualNode;
    chData: PCEStackItemData;
    l: TTntStrings;
  begin
    l:= TTntStringList.Create;
    try
    chNode:= Self.GetFirstChild(groupNode);
      while assigned(chNode) do
      begin
        chData:= Self.GetNodeData(chNode);
        l.Clear;
        l.Add(chData.CEPath);
        l.Add(chData.Caption);
        groupItems.Add(l.CommaText);
        chNode:= Self.GetNextSibling(chNode);
      end;
    finally
      l.Free;
    end;
  end;

var
  node: PVirtualNode;
  data: PCEStackItemData;
  list: TTntStrings;
begin
  AStack.ClearGroups;
  node:= Self.GetFirstChild(nil);
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if data.ItemType = sitGroup then
    begin
      list:= AStack.GroupItems[AStack.AddGroup(data.Caption)];
      enumGroup(node, list);
    end;
    node:= Self.GetNextSibling(node);
  end;
end;

{-------------------------------------------------------------------------------
  Selected To NamespaceArray
-------------------------------------------------------------------------------}
function TCEStackTree.SelectedToNamespaceArray(AddToCleanList: Boolean =
    false): TNamespaceArray;
var
  i: Integer;
  node, chNode: PVirtualNode;
  data, chData: PCEStackItemData;
  list: TList;
  pidl: PItemIDList;
begin
  list:= TList.Create;
  try
    // Create a list of selected items
    node:= Self.GetFirstSelected;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitShell then
      begin
        pidl:= CEPathToPIDL(data.CEPath);
        if assigned(pidl) then
        begin
          list.Add(Pointer(TNamespace.Create(pidl, nil)));
          if AddToCleanList then
          fCleanList.Add(node);
        end;
      end
      else if data.ItemType = sitGroup then
      begin
        chNode:= Self.GetFirstChild(node);
        while assigned(chNode) do
        begin
          chData:= Self.GetNodeData(chNode);
          pidl:= CEPathToPIDL(chData.CEPath);
          if assigned(pidl) then
          begin
            list.Add(Pointer(TNamespace.Create(pidl, nil)));
            if AddToCleanList then
            fCleanList.Add(chNode);
          end;
          chNode:= Self.GetNextSibling(chNode);
        end;
      end;
      node:= Self.GetNextSelected(node);
    end;
    // Create NamespaceArray
    SetLength(Result, list.Count);
    for i:= 0 to list.Count - 1 do
    begin
      Result[i]:= TNamespace(list.Items[i]);
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Selected To NamespaceArray
-------------------------------------------------------------------------------}
function TCEStackTree.GroupToNamespaceArray(AGroupNode: PVirtualNode;
    AddToCleanList: Boolean = false): TNamespaceArray;
var
  i: Integer;
  node: PVirtualNode;
  data: PCEStackItemData;
  list: TList;
  pidl: PItemIDList;
begin
  list:= TList.Create;
  try
    // Create a list of selected items
    node:= Self.GetFirstChild(AGroupNode);
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitShell then
      begin
        pidl:= CEPathToPIDL(data.CEPath);
        if assigned(pidl) then
        begin
          list.Add(Pointer(TNamespace.Create(pidl, nil)));
          if AddToCleanList then
          fCleanList.Add(node);
        end;
      end;
      node:= Self.GetNextSibling(node);
    end;
    // Create NamespaceArray
    SetLength(Result, list.Count);
    for i:= 0 to list.Count - 1 do
    begin
      Result[i]:= TNamespace(list.Items[i]);
    end;
  finally
    list.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Dbl Click
-------------------------------------------------------------------------------}
procedure TCEStackTree.HandleMouseDblClick(var Message: TWMMouse; const
    HitInfo: THitInfo);
var
  data: PCEStackItemData;
  ns: TNamespace;
  pidl: PItemIDList;
begin
  if not assigned(HitInfo.HitNode) then
  Exit;
  
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

  data:= Self.GetNodeData(HitInfo.HitNode);
  if data.ItemType = sitShell then
  begin
    pidl:= CEPathToPIDL(data.CEPath);
    if assigned(pidl) then
    begin
      ns:= TNamespace.Create(pidl, nil);
      try
        if ns.Folder and (WideStrIComp(PWideChar(ns.Extension), '.zip') <> 0) then
        GlobalPathCtrl.ChangeGlobalPathPIDL(Self, ns.AbsolutePIDL)
        else
        ns.ShellExecuteNamespace(GlobalPathCtrl.CurrentPath, '', true, true, MP_ThreadedShellExecute);
      finally
        ns.Free;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down.
-------------------------------------------------------------------------------}
procedure TCEStackTree.HandleMouseDown(var Message: TWMMouse; var HitInfo:
    THitInfo);
begin
  if GetKeyState(VK_MENU) < 0 then
  Exit;

  if (not assigned(HitInfo.HitNode)) or
     (hiOnItemButton in HitInfo.HitPositions) or
     (not fAutoExpand) then
  begin
    inherited;
    Exit;
  end;

  if HitInfo.HitNode = self.FocusedNode then
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
      if not Self.Expanded[HitInfo.HitNode] then
      Self.ToggleNode(HitInfo.HitNode);

      Self.Selected[HitInfo.HitNode]:= true;
      Self.FocusedNode:= HitInfo.HitNode;
      Self.ScrollIntoView(HitInfo.HitNode,false,true);
    finally
      Self.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set SafeOperationsOnly
-------------------------------------------------------------------------------}
procedure TCEStackTree.SetSafeOperationsOnly(const Value: Boolean);
begin
  if Value <> fSafeOperationsOnly then
  begin
    fSafeOperationsOnly:= Value;
    if assigned(fOnSafeOperationsOnlyChange) then
    fOnSafeOperationsOnlyChange(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Update Scrollbar Size
-------------------------------------------------------------------------------}
procedure TCEStackTree.UpdateScrollbarSize;
var
  i: Integer;
  n: PVirtualNode;
begin
  i:= Self.RootNode.NodeHeight + Self.BottomSpace;
  n:= Self.GetFirstVisible;
  while assigned(n) do
  begin
    i:= i + n.NodeHeight;
    n:= Self.GetNextVisible(n);
  end;
  Self.RootNode.TotalHeight:= i;
end;

{-------------------------------------------------------------------------------
  Handle WM_Create message
-------------------------------------------------------------------------------}
procedure TCEStackTree.WMCreate(var Msg: TMessage);
begin
  ShellNotifyManager.RegisterExplorerWnd(Self);
  ChangeNotifier.RegisterShellChangeNotify(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle WM_NCDestroy message
-------------------------------------------------------------------------------}
procedure TCEStackTree.WMNCDestroy(var Msg: TMessage);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  ShellNotifyManager.UnRegisterExplorerWnd(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle WM_ShellNotify message
-------------------------------------------------------------------------------}
procedure TCEStackTree.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i: Integer;
  node: PVirtualNode;
  data: PCEStackItemData;
  ns: TNamespace;
begin
  if not ShellNotifyManager.OkToDispatch then
  begin
    ShellNotifyManager.ReDispatchShellNotify(TVirtualShellEventList(Msg.wParam));
  end
  else
  begin
    ShellEventList:= TVirtualShellEventList(Msg.wParam);
    List:= ShellEventList.LockList;
    try
      List.Sort(ShellEventSort);
      try
        for i:= 0 to List.Count - 1 do
        begin
          ShellEvent:= TVirtualShellEvent(List.Items[i]);
          case ShellEvent.ShellNotifyEvent of
            vsneUpdateDir, vsneUpdateItem: begin
              LastOfflineCheck:= GetTickCount;
            end;
            vsneRenameItem, vsneRenameFolder: begin
              if assigned(ShellEvent.PIDL1) and assigned(ShellEvent.PIDL2) then
              begin
                ns:= TNamespace.Create(ShellEvent.PIDL1, nil);
                ns.FreePIDLOnDestroy:= false;
                try
                  node:= FindByPath(ns.NameForParsing);
                  if assigned(node) then
                  begin
                    data:= Self.GetNodeData(node);
                    data.CEPath:= PIDLToCEPath(ShellEvent.PIDL2);
                    data.Caption:= WideExtractFileName(data.CEPath, false);
                    data.IconIndex:= -1;
                    data.OverlayIndex:= -1;
                    data.fLastOfflineCheck:= 0;
                    data.fLastIconRefresh:= 0;
                    data.Offline:= true;
                  end;
                finally
                  ns.Free;
                end;
              end;
            end;
          end;
        end;
      except
      //finally
      end;
    finally
      ShellEventList.UnlockList;
      ShellEventList.Release;
      Self.Refresh;
    end;
  end;
end;

{##############################################################################}
// TCELogicalPerformedDropEffect

{-------------------------------------------------------------------------------
  GetFormatEtc
-------------------------------------------------------------------------------}
function TCELogicalPerformedDropEffect.GetFormatEtc: TFormatEtc;
begin
  Result.cfFormat:= CF_LOGICALPERFORMEDDROPEFFECT;
  Result.ptd:= nil;
  Result.dwAspect:= DVASPECT_CONTENT;
  Result.lindex:= -1;
  Result.tymed:= TYMED_HGLOBAL
end;

{-------------------------------------------------------------------------------
  LoadFromDataObject
-------------------------------------------------------------------------------}
function TCELogicalPerformedDropEffect.LoadFromDataObject(DataObject: IDataObject): Boolean;
var
  Ptr: PCardinal;
  StgMedium: TStgMedium;
begin
  Result:= False;
  FillChar(StgMedium, SizeOf(StgMedium), #0);

  if Succeeded(DataObject.GetData(GetFormatEtc, StgMedium)) then
  try
    Ptr:= GlobalLock(StgMedium.hGlobal);
    try
      if Assigned(Ptr) then
      begin
        FAction:= Ptr^;
        Result:= True;
      end;
    finally
      GlobalUnLock(StgMedium.hGlobal);
    end
  finally
    ReleaseStgMedium(StgMedium)
  end
end;

{-------------------------------------------------------------------------------
  SaveToDataObject
-------------------------------------------------------------------------------}
function TCELogicalPerformedDropEffect.SaveToDataObject(DataObject: IDataObject): Boolean;
var
  Ptr: PCardinal;
  StgMedium: TStgMedium;
begin
  FillChar(StgMedium, SizeOf(StgMedium), #0);

  StgMedium.hGlobal:= GlobalAlloc(GPTR, SizeOf(FAction));
  Ptr:= GlobalLock(StgMedium.hGlobal);
  try
    Ptr^:= FAction;
    StgMedium.tymed:= TYMED_HGLOBAL;
    Result:= Succeeded(DataObject.SetData(GetFormatEtc, StgMedium, True))
  finally
    GlobalUnLock(StgMedium.hGlobal);
  end
end;

end.
