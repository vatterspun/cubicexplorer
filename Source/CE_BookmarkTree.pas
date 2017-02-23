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
//  The Original Code is CE_BookmarkTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_BookmarkTree;

interface

uses
  // CE Units
  CE_Bookmarks, CE_VistaFuncs, CE_SpTabBar, CE_Utils,
  // JVCL
  JvSimpleXml, JvAppStorage,
  // VSTools
  MPShellUtilities, MPCommonObjects, MPDataObject, MPCommonUtilities,
  // VT
  VirtualTrees,
  // SpTBX
  SpTBXItem,
  // Tnt
  TntClasses, TntSysUtils,
  // System Units
  Classes, SysUtils, Windows, Messages, ImgList, Controls, Contnrs,
  ActiveX, Graphics, Dialogs;

type
  TCEBookmarkTree = class(TVirtualStringTree)
  private
    fDropNode: PVirtualNode;
    fAutoCollapse: Boolean;
    fAutoExpand: Boolean;
    fDragEnterMouseShiftState: TShiftState;
    fDropNodeAcceptsDrop: Boolean;
    fOnBookmarksChange: TNotifyEvent;
    fSingleClickMode: Boolean;
  protected
    procedure ClearDropNode;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats:
        TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode:
        TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt:
        TPoint; Mode: TDropMode; var Effect: Integer): Boolean; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
        TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
        override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: WideString); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
        override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text:
        WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);
        override;
    procedure HandleMouseUp(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure PopulateTree(XmlDoc: TJvSimpleXml); virtual;
    procedure RefreshScrollHeight;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditSelectedNode;
    function GetNodeComp(Node: PVirtualNode): TCECustomBookComp;
    function AddBookItem(ItemName: String; ParentNode: PVirtualNode): PVirtualNode;
    procedure BookmarksChange;
    procedure LoadFromXmlStream(S: TStream);
    procedure LoadFromXmlFile(AFilePath: WideString);
    procedure LoadFromXmlString(S: String);
    procedure PopulateXmlDoc(XmlDoc: TJvSimpleXml); virtual;
    procedure RefreshIcons;
    procedure SafeDeleteSelectedNodes;
    procedure SaveToXmlFile(AFilePath: WideString);
    procedure SaveToXmlStream(S: TStream);
    function SaveToXmlString: string;
    property AutoCollapse: Boolean read fAutoCollapse write fAutoCollapse;
    property AutoExpand: Boolean read fAutoExpand write fAutoExpand;
    property SingleClickMode: Boolean read fSingleClickMode write fSingleClickMode;
  published
    property OnBookmarksChange: TNotifyEvent read fOnBookmarksChange write
        fOnBookmarksChange;
  end;

implementation

uses
  CE_StdBookmarkComps, dCE_Actions, CE_LanguageEngine, Forms;

{*------------------------------------------------------------------------------
  Create an instance of TCEBookmarkTree object
-------------------------------------------------------------------------------}
constructor TCEBookmarkTree.Create(AOwner: TComponent);
begin
  inherited;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.BorderStyle:= bsNone;
  Self.DragMode:= dmAutomatic;
  self.DragType:= dtVCL;
  Self.NodeDataSize:= SizeOf(TCEBookData);
  Self.TreeOptions.SelectionOptions:= [toRightClickSelect, toMultiSelect];
  Self.TreeOptions.AnimationOptions:= [];
  Self.TreeOptions.AutoOptions:= [toAutoDropExpand,toAutoScrollOnExpand,toAutoTristateTracking,toAutoDeleteMovedNodes];
  Self.TreeOptions.MiscOptions:= [toAcceptOLEDrop,toEditable,toFullRepaintOnResize,toInitOnSave];
  Self.Header.Options:= [hoAutoResize,hoColumnResize];
  Self.ButtonFillMode:= fmShaded;
  Self.Images:= SmallSysImages;
  Self.IncrementalSearch:= isAll;
  Self.DefaultNodeHeight:= SmallShellIconSize + 1;
  LastBookmarkIconRefresh:= GetTickCount;
end;

{*------------------------------------------------------------------------------
  Get's called when Node is being freed.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.DoFreeNode(Node: PVirtualNode);
var
  data: PCEBookData;
begin
  data:= GetNodeData(Node);
  try
    if assigned(data.BookComp) then
    data.BookComp.Free;
  finally
    inherited;
  end;
end;

{*------------------------------------------------------------------------------
  Populate tree from Xml document.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.PopulateTree(XmlDoc: TJvSimpleXml);

  procedure EnumNode(XmlNode: TJvSimpleXmlElem; Node: PVirtualNode);
  var
    i: Integer;
    chNode: PVirtualNode;
    chXmlNode: TJvSimpleXmlElem;
    data: PCEBookData;
  begin
    for i:= 0 to XmlNode.Items.Count - 1 do
    begin
      chXmlNode:= XmlNode.Items.Item[i];
      chNode:= Self.AddChild(Node);
      data:= Self.GetNodeData(chNode);
      data.BookComp:= CEBookCompList.CreateNewComp(chXmlNode.Name);
      if not assigned(data.BookComp) then
      data.BookComp:= TCECustomBookComp.Create;
      data.BookComp.LoadFromXmlNode(chXmlNode);
      EnumNode(chXmlNode, chNode);
      Self.Expanded[chNode]:= data.BookComp.Expanded;
      if not data.BookComp.Enabled then
      Include(chNode.States, vsDisabled);
    end;
  end;

begin
  Self.BeginUpdate;
  Self.Clear;
  EnumNode(XmlDoc.Root, RootNode);
  Self.EndUpdate;
end;

{*------------------------------------------------------------------------------
  Populate Xml Document from tree.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.PopulateXmlDoc(XmlDoc: TJvSimpleXml);

  procedure EnumNode(XmlNode: TJvSimpleXmlElem; Node: PVirtualNode);
  var
    chNode: PVirtualNode;
    chXmlNode: TJvSimpleXmlElem;
    data: PCEBookData;
  begin
    chNode:= Node.FirstChild;
    while chNode <> nil do
    begin
      data:= Self.GetNodeData(chNode);
      data.BookComp.Expanded:= Self.Expanded[chNode];
      chXmlNode:= XmlNode.Items.Add(CEBookCompList.GetCompName(data.BookComp));
      data.BookComp.SaveToXmlNode(chXmlNode);
      EnumNode(chXmlNode, chNode);
      chNode:= chNode.NextSibling;
    end;
  end;

var
  rootXmlNode: TJvSimpleXmlElem;
begin
  XmlDoc.Root.Name:= 'bookmarks';
  rootXmlNode:= XmlDoc.Root;
  EnumNode(rootXmlNode, RootNode);
end;

{*------------------------------------------------------------------------------
  Load tree from Xml stream
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.LoadFromXmlStream(S: TStream);
var
  XmlDoc: TJvSimpleXml;
begin
  try
    XmlDoc:= TJvSimpleXml.Create(nil);
    try
      XmlDoc.LoadFromStream(S);
      PopulateTree(XmlDoc);
    finally
      XmlDoc.Free;
      BookmarksChange;
    end;
  except
  end;
end;

{*------------------------------------------------------------------------------
  Load tree from Xml file
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.LoadFromXmlFile(AFilePath: WideString);
var
  S: TTntFileStream;
begin
  if WideFileExists(AFilePath) then
  begin
    s:= TTntFileStream.Create(AFilePath, fmOpenRead  or fmShareDenyNone);
    try
      LoadFromXmlStream(s);
    finally
      s.Free;
      BookmarksChange;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Load tree from Xml string
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.LoadFromXmlString(S: String);
var
  XmlDoc: TJvSimpleXml;
begin
  XmlDoc:= TJvSimpleXml.Create(nil);
  try
    XmlDoc.LoadFromString(S);
    PopulateTree(XmlDoc);
  finally
    XmlDoc.Free;
    BookmarksChange;
  end;
end;

{*------------------------------------------------------------------------------
  Save tree to Xml stream.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.SaveToXmlStream(S: TStream);
var
  XmlDoc: TJvSimpleXml;
begin
  XmlDoc:= TJvSimpleXml.Create(nil);
  try
    PopulateXmlDoc(XmlDoc);
    XmlDoc.SaveToStream(S);
  finally
    XmlDoc.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Save tree to Xml file.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.SaveToXmlFile(AFilePath: WideString);
var
  S: TTntFileStream;
begin
  try
    s:= TTntFileStream.Create(AFilePath, fmCreate);
  except
    Exit;
  end;

  try
    SaveToXmlStream(s);
  finally
    s.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Save tree to Xml string.
-------------------------------------------------------------------------------}
function TCEBookmarkTree.SaveToXmlString: string;
var
  XmlDoc: TJvSimpleXml;
begin
  XmlDoc:= TJvSimpleXml.Create(nil);
  try
    PopulateXmlDoc(XmlDoc);
  finally
    Result:= XmlDoc.SaveToString;
    XmlDoc.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Call Bookmark change event
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.BookmarksChange;
begin
  if self.UpdateCount > 0 then Exit;
  if Assigned(fOnBookmarksChange) then fOnBookmarksChange(Self);
end;

{*------------------------------------------------------------------------------
  Get Node Comp
-------------------------------------------------------------------------------}
function TCEBookmarkTree.GetNodeComp(Node: PVirtualNode): TCECustomBookComp;
var
  data: PCEBookData;
begin
  data:= GetNodeData(Node);
  if assigned(data) then
  Result:= data.BookComp
  else
  Result:= nil;
end;

{*------------------------------------------------------------------------------
  Delete Selected Nodes
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.SafeDeleteSelectedNodes;
begin
  if (self.HasChildren[self.FocusedNode]) or (self.SelectedCount > 1) then
  begin
    if (TaskDialog(Application.MainFormHandle,
                   _('Confirm'),
                   _('Deleting multiple bookmarks!'),
                   _('Are you sure you want to delete selected bookmarks?'),
                   TD_ICON_QUESTION,
                   TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES) then
    begin
      self.DeleteSelectedNodes;
      BookmarksChange;
    end;
  end
  else
  begin
    self.DeleteSelectedNodes;
    BookmarksChange;
  end;
end;

{*------------------------------------------------------------------------------
  Get Node Text
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: WideString);
var
  data: PCEBookData;
begin
  data:= GetNodeData(Node);
  if assigned(data.BookComp) then
  Text:= data.BookComp.Title;
end;

{*------------------------------------------------------------------------------
  Get's called when a new Text is assigned to a Node.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
    Text: WideString);
var
  data: PCEBookData;
begin
  data:= GetNodeData(Node);
  if assigned(data.BookComp) then
  data.BookComp.Title:= Text;
  inherited;
  BookmarksChange;
end;

{*------------------------------------------------------------------------------
  Do PaintText
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.DoPaintText(Node: PVirtualNode; const Canvas:
    TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PCEBookData;
begin
  data:= Self.GetNodeData(Node);
  if assigned(data.BookComp) then
  begin
    if data.BookComp.Ghosted and not Self.Selected[Node] then
    Canvas.Font.Color:= Colors.DisabledColor;
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get Image Index
-------------------------------------------------------------------------------}
function TCEBookmarkTree.DoGetImageIndex(Node: PVirtualNode; Kind:
    TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index:
    Integer): TCustomImageList;
var
  data: PCEBookData;
begin
  Result:= inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
  if (Column > 0) then
  begin
    Index:= -1;
    Exit;
  end;

  data:= GetNodeData(Node);
  if assigned(data.BookComp) then
  begin
    Index:= data.BookComp.GetImageIndex(Expanded[Node], (Kind = ikOverlay));
    Ghosted:= data.BookComp.Ghosted;
    Result:= data.BookComp.ImageList;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Dbl Click
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.HandleMouseDblClick(var Message: TWMMouse; const
    HitInfo: THitInfo);
begin
//  if not assigned(HitInfo.HitNode) then
//  Exit;
//  
//  if Self.Expanded[HitInfo.HitNode] then
//  begin
//    Self.ToggleNode(HitInfo.HitNode);
//    Exit;
//  end;
//
//  Self.BeginUpdate;
//  try
//    if fAutoCollapse then
//    Self.FullCollapse;
//
//    Self.ClearSelection;
//
//    Self.FullyVisible[HitInfo.HitNode]:= true;
//    Self.ToggleNode(HitInfo.HitNode);
//
//    Self.Selected[HitInfo.HitNode]:= true;
//    Self.FocusedNode:= HitInfo.HitNode;
//    Self.ScrollIntoView(HitInfo.HitNode,false,true);
//  finally
//    Self.EndUpdate;
//  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.HandleMouseDown(var Message: TWMMouse; var HitInfo:
    THitInfo);
begin
  if GetKeyState(VK_MENU) < 0 then
  Exit;
  
  inherited;

  Self.BeginUpdate;
  try
    if assigned(HitInfo.HitNode) and not (hiOnItemButton in HitInfo.HitPositions) then
    begin
      if fAutoCollapse then
      Self.FullCollapse;
      if fAutoExpand and not Self.Expanded[HitInfo.HitNode] then
      Self.ToggleNode(HitInfo.HitNode);
      Self.ScrollIntoView(HitInfo.HitNode,false,true);

      // Update scrollbar size (VT calculates wrong height if new items have been added.)
      RefreshScrollHeight;
    end
    else if (hiOnItemButton in HitInfo.HitPositions) then
    RefreshScrollHeight;
  finally
    Self.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Up.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.HandleMouseUp(var Message: TWMMouse; const HitInfo:
    THitInfo);
begin
  inherited;
  self.DoStateChange([],[tsEditPending]);
end;

{*------------------------------------------------------------------------------
  Get's called on Mouse Down.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  data: PCEBookData;
  HitInfo: THitInfo;
begin
  Self.GetHitTestInfoAt(X,Y,true,HitInfo);

  if not assigned(HitInfo.HitNode) then
  Self.FocusedNode:= nil;

  if (hiOnItemButton in HitInfo.HitPositions) then
  begin
    inherited;
    Exit;
  end;

  if not (hiOnItemLabel in HitInfo.HitPositions) and not (hiOnNormalIcon in HitInfo.HitPositions) then
  begin
    Self.FocusedNode:= nil;
    Exit;
  end;

  if assigned(HitInfo.HitNode) then
  begin
    data:= GetNodeData(HitInfo.HitNode);
    if assigned(data.BookComp) then
    data.BookComp.MouseClick(Shift,Button,fSingleClickMode);
  end;

  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called on Mouse Up
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when a object is dropped.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.DoDragDrop(Source: TObject; DataObject: IDataObject;
    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer;
    Mode: TDropMode);
var
 // data: PCEBookData;
  Attachmode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  i: Integer;
  hd: TCommonShellIDList;
  data: PCEBookData;
  node: PVirtualNode;
begin

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

  if Source = self then // Moving nodes inside self.
  begin
    Nodes:= self.GetSortedSelection(True);
    if Effect = DROPEFFECT_COPY then
    begin
      // TODO
    end
    else
    begin
      for I := 0 to High(Nodes) do
      self.MoveTo(Nodes[I], self.DropTargetNode, AttachMode, False);
    end;
  end
  else if assigned(DataObject) then// Dropping from outside self.
  begin
    Effect:= DROPEFFECT_LINK;
    hd:= TCommonShellIDList.Create;

    // Do custom drop action
    if fDropNodeAcceptsDrop and Assigned(fDropNode) and (Mode <> dmNowhere) then
    begin
      data:= Self.GetNodeData(fDropNode);
      if (Mode = dmOnNode) and data.BookComp.SupportsDragDrop then
      begin
        if Assigned(fDropNode) then
        begin
          try
            data:= Self.GetNodeData(fDropNode);
            Effect:= DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK;
            Shift:= Shift + fDragEnterMouseShiftState;
            data.BookComp.DoDragDrop(Self.DragManager.DataObject, Shift, Self.ClientToScreen(Pt), Effect);
            Exit; // ---> EXIT
          finally
            ClearDropNode;
          end;
        end;
      end;
    end;

    try
      Self.BeginUpdate;
      // Add new bookmark
      hd.LoadFromDataObject(DataObject);
      for i:= 0 to hd.PIDLCount-1 do
      begin
        if AttachMode = amNoWhere then
        AttachMode:= amInsertAfter;
        node:= InsertNode(Self.DropTargetNode, AttachMode);
        Self.ValidateNode(node, false);
        data:= GetNodeData(node);
        data.BookComp:= TCENormalItemComp.Create;
        TCENormalItemComp(data.BookComp).LoadFromPIDL(hd.AbsolutePIDL(i));
      end;

      // Update scrollbar size (VT doesn't do that automatically for some reason)
      RefreshScrollHeight;
    finally
      hd.Free;
      Self.EndUpdate;      
    end;
  end;
  inherited;
  Self.Invalidate;
  BookmarksChange;
end;

{*------------------------------------------------------------------------------
  Get's called when a object is dragged over.
-------------------------------------------------------------------------------}
function TCEBookmarkTree.DoDragOver(Source: TObject; Shift: TShiftState; State:
    TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer): Boolean;
var
  Nodes: TNodeArray;
  i: Integer;
  tmpNode: PVirtualNode;
  data: PCEBookData;
begin
  inherited DoDragOver(Source,Shift,State,Pt,Mode,Effect);
  Result:= false;

  if Source = self then
  begin
    if self.DropTargetNode = nil then
    begin
      Effect:= DROPEFFECT_NONE;
    end
    else // Internal move
    begin
      Nodes:= self.GetSortedSelection(True);
      tmpNode:= self.DropTargetNode;
      while assigned(tmpNode) do
      begin
        for i := 0 to High(Nodes) do
        begin
          if tmpNode = Nodes[i] then
          begin
            Effect:= DROPEFFECT_NONE;
            Exit;
          end;
        end;
        tmpNode:= tmpNode.Parent;
      end;

      if Shift = [ssCtrl] then
      Effect:= DROPEFFECT_COPY
      else
      Effect:= DROPEFFECT_MOVE;
      Result:= true;
    end;
  end
  else if (source is TCESpTabToolbar) or (source is TSpTBXItemDragObject) then
  begin
    Effect:= DROPEFFECT_NONE;
    Result:= false;
  end
  else // Shell drop
  begin
    if State = dsDragLeave then
    begin
      ClearDropNode;
    end
    else if State = dsDragMove then
    begin
      if Assigned(Self.DropTargetNode) and (Mode <> dmNowhere) then
      begin
        data:= Self.GetNodeData(Self.DropTargetNode);
        if (Mode = dmOnNode) and data.BookComp.SupportsDragDrop then
        begin
          if fDropNode <> Self.DropTargetNode then
          begin
            ClearDropNode;
            fDropNode:= Self.DropTargetNode;
            if data.BookComp.DoDragEnter(Self.DragManager.DataObject, Shift, Self.ClientToScreen(Pt), Effect) then
            begin
              fDropNodeAcceptsDrop:= true;
              if ssLeft in Shift then
              fDragEnterMouseShiftState:= [ssLeft]
              else if ssRight in Shift then
              fDragEnterMouseShiftState:= [ssRight]
              else
              fDragEnterMouseShiftState:= [ssMiddle];
            end
            else
            begin
              Result:= True;
              Effect:= DROPEFFECT_LINK;
            end;
          end
          else if fDropNodeAcceptsDrop then
          begin
            Effect:= DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK;
            Result:= data.BookComp.DoDragOver(Shift, Self.ClientToScreen(Pt), Effect);
          end
          else
          begin
            Effect:= DROPEFFECT_LINK;
            Result:= true;
          end;
        end
        else
        begin
          ClearDropNode;
          Result:= True;
          Effect:= DROPEFFECT_LINK;
        end;
      end
      else
      begin
        ClearDropNode;
        Result:= True;
        Effect:= DROPEFFECT_LINK;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on Key Action.
-------------------------------------------------------------------------------}
function TCEBookmarkTree.DoKeyAction(var CharCode: Word; var Shift:
    TShiftState): Boolean;
var
  data: PCEBookData;
begin
  Result:= inherited DoKeyAction(CharCode, Shift);
  if CharCode = VK_DELETE then
  begin
    if Shift = [ssShift] then
    self.DeleteSelectedNodes
    else
    SafeDeleteSelectedNodes;
  end
  else
  begin
    data:= GetNodeData(Self.FocusedNode);
    if assigned(data) then
    begin
      data.BookComp.KeyAction(CharCode, Shift);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Edit selected node.
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.EditSelectedNode;
begin
  if IsEditing then Exit;
  if assigned(FocusedNode) then
  EditNode(FocusedNode,-1);
end;

{*------------------------------------------------------------------------------
  Add new bookmark item
-------------------------------------------------------------------------------}
function TCEBookmarkTree.AddBookItem(ItemName: String; ParentNode:
    PVirtualNode): PVirtualNode;
var
  data: PCEBookData;
begin
  Result:= Self.AddChild(ParentNode);
  data:= Self.GetNodeData(Result);
  data.BookComp:= CEBookCompList.CreateNewComp(ItemName);
  if not assigned(data.BookComp) then
  data.BookComp:= TCECustomBookComp.Create;
end;

{-------------------------------------------------------------------------------
  Clear Drop Node
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.ClearDropNode;
var
  data: PCEBookData;
begin
  if fDropNodeAcceptsDrop and Assigned(fDropNode) then
  begin
    data:= Self.GetNodeData(fDropNode);
    if assigned(data.BookComp) then
    data.BookComp.DoDragLeave;
  end;
  fDropNode:= nil;
  fDropNodeAcceptsDrop:= false;
end;

{-------------------------------------------------------------------------------
  RefreshIcons
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.RefreshIcons;
begin
  LastBookmarkIconRefresh:= GetTickCount;
  Self.Refresh;
end;

{-------------------------------------------------------------------------------
  Refresh Scroll Height
-------------------------------------------------------------------------------}
procedure TCEBookmarkTree.RefreshScrollHeight;
var
  h: Integer;
  node: PVirtualNode;
begin
  h:= Self.RootNode.NodeHeight + Self.BottomSpace;
  node:= Self.GetFirstVisible;
  while assigned(node) do
  begin
    h:= h + node.NodeHeight;
    node:= Self.GetNextVisible(node);
  end;
  Self.RootNode.TotalHeight:= h;
end;


end.
