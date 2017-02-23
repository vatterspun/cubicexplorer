unit CV_Playlist;

interface

uses
  // CubicCore
  ccThreadUtils, ccClasses, ccFileUtils,
  // VSTools
  VirtualShellNotifier, VirtualResources,
  // VirtualTrees
  VirtualTrees,
  // SysUtils
  Windows, Messages, Controls, Classes, Graphics, ImgList, ActiveX, SysUtils;

type

{-------------------------------------------------------------------------------
  TCVPlaylist
-------------------------------------------------------------------------------}
  PCVPlaylistData = ^TCVPlaylistData;
  TCVPlaylistData = record
    fLastUpdate: Cardinal;
    Title: WideString;
    Path: WideString;
    ImageIndex: Integer;
  end;

  TCVPlaylistIsSupportedEvent = procedure(Sender: TObject; AExtension: WideString; var AIsSupported: Boolean) of object;

  TCVPlaylist = class(TVirtualStringTree, IVirtualShellNotify)
  private
    function GetOkToShellNotifyDispatch: Boolean;
  protected
    fActiveFilePath: WideString;
    fActiveItem: PVirtualNode;
    fLastIconRefresh: Cardinal;
    fLoopList: Boolean;
    fOnActiveItemChange: TNotifyEvent;
    fOnDeleteActive: TNotifyEvent;
    fOnIsSupported: TCVPlaylistIsSupportedEvent;
    fOnNavigationStateChange: TNotifyEvent;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats:
        TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode:
        TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt:
        TPoint; Mode: TDropMode; var Effect: Integer): Boolean; override;
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
    procedure DoNavigationStateChange; virtual;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure Notify(var Msg: TMessage);
    procedure RefreshScrollHeight;
    procedure SetActiveFilePath(const Value: WideString); virtual;
    procedure SetActiveItem(const Value: PVirtualNode); virtual;
    procedure WMCreate(var Msg: TMessage); message WM_CREATE;
    procedure WMNCDestroy(var Msg: TMessage); message WM_NCDESTROY;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
    property OkToShellNotifyDispatch: Boolean read GetOkToShellNotifyDispatch;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ActivateNext: PVirtualNode; virtual;
    function ActivatePrevious: PVirtualNode; virtual;
    function Add(APath: WideString; AMakeActive: Boolean = false): PVirtualNode;
        virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function FindItem(AFilePath: WideString): PVirtualNode; virtual;
    function GetActivePath: WideString; virtual;
    function GetNextItem(AMakeActive: Boolean = false; ANextFrom: PVirtualNode =
        nil): PVirtualNode; virtual;
    function GetPreviousItem(AMakeActive: Boolean = false; APreviousFrom:
        PVirtualNode = nil): PVirtualNode; virtual;
    function Insert(ATargetNode: PVirtualNode; AAttachMode: TVTNodeAttachMode;
        APath: WideString; AMakeActive: Boolean = false): PVirtualNode; virtual;
    function IsEmpty: Boolean; virtual;
    function IsSupported(AExt: WideString): Boolean; virtual;
    procedure RefreshIcons; virtual;
    property ActiveFilePath: WideString read fActiveFilePath write
        SetActiveFilePath;
    property ActiveItem: PVirtualNode read fActiveItem write SetActiveItem;
    property LoopList: Boolean read fLoopList write fLoopList;
  published
    property OnActiveItemChange: TNotifyEvent read fOnActiveItemChange write
        fOnActiveItemChange;
    property OnDeleteActive: TNotifyEvent read fOnDeleteActive write
        fOnDeleteActive;
    property OnIsSupported: TCVPlaylistIsSupportedEvent read fOnIsSupported write
        fOnIsSupported;
    property OnNavigationStateChange: TNotifyEvent read fOnNavigationStateChange
        write fOnNavigationStateChange;
  end;

{-------------------------------------------------------------------------------
  TCVFilelist
-------------------------------------------------------------------------------}
const
  FilelistTask_FilesFound = 100;

type
  TCVFilelistTaskType = (fttGetIcon, fttSearchFiles);

  TCVFilelistTask = class(TObject)
    TaskType: TCVFilelistTaskType;
    Path: WideString;
    IncludeSubfolders: Boolean;
    Item: PVirtualNode;
    Index: Integer;
    Results: TCCThreadStringList;
  public
    destructor Destroy; override;
  end;

  TCVFilelist = class(TCVPlaylist)
  protected
    fIncludeSubFolders: Boolean;
    fMultiThreadedSearch: Boolean;
    fTaskTag: Cardinal;
    fThreadedIcons: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
        TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
        override;
    procedure HandleTaskDone(Sender: TObject; AObject: TObject; AData: Pointer;
        ATag: Integer); virtual;
    procedure HandleTaskExecute(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer); virtual;
    procedure HandleTaskSyncMsg(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer; AMsg, AParam1, AParam2: Integer; var AResult:
        Integer); virtual;
    procedure SetActiveFilePath(const Value: WideString); override;
    procedure SetIncludeSubFolders(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort; virtual;
    procedure PopulateList(AFolderPath: WideString; AIncludeSubFolders: Boolean =
        false); virtual;
    property IncludeSubFolders: Boolean read fIncludeSubFolders write
        SetIncludeSubFolders;
    property MultiThreadedSearch: Boolean read fMultiThreadedSearch write
        fMultiThreadedSearch;
  published
    property ThreadedIcons: Boolean read fThreadedIcons write fThreadedIcons;
  end;

implementation

uses
  Forms, MPCommonObjects, MPDataObject, MPShellUtilities;

{##############################################################################}
// TCVPlaylist

{-------------------------------------------------------------------------------
  Create an instance of TCVPlaylist
-------------------------------------------------------------------------------}
constructor TCVPlaylist.Create(AOwner: TComponent);
begin
  inherited;
  Self.NodeDataSize:= SizeOf(TCVPlaylistData);
  // Default values
  Self.DragMode:= dmAutomatic;
  self.DragType:= dtVCL;  
  Self.TreeOptions.PaintOptions:= [toHideFocusRect,toShowButtons,toShowDropmark,toShowHorzGridLines,toThemeAware,toUseBlendedImages];
  Self.TreeOptions.SelectionOptions:= [toFullRowSelect,toMultiSelect];
  Self.LineStyle:= lsSolid;
  Self.DrawSelectionMode:= smBlendedRectangle;
  Self.BorderStyle:= bsNone;
  Self.Images:= SmallSysImages;
  fLastIconRefresh:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  Destroy TCVPlaylist
-------------------------------------------------------------------------------}
destructor TCVPlaylist.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  ActivateNext
-------------------------------------------------------------------------------}
function TCVPlaylist.ActivateNext: PVirtualNode;
begin
  Result:= GetNextItem;
  if assigned(Result) then
  ActiveItem:= Result;
end;

{-------------------------------------------------------------------------------
  Activate Previous
-------------------------------------------------------------------------------}
function TCVPlaylist.ActivatePrevious: PVirtualNode;
begin
  Result:= GetPreviousItem;
  if assigned(Result) then
  ActiveItem:= Result;
end;

{-------------------------------------------------------------------------------
  Add (Returns the index of added item)
-------------------------------------------------------------------------------}
function TCVPlaylist.Add(APath: WideString; AMakeActive: Boolean =
    false): PVirtualNode;
begin
  Result:= Insert(nil, amNoWhere, APath, AMakeActive);
end;

{-------------------------------------------------------------------------------
  Assign
-------------------------------------------------------------------------------}
procedure TCVPlaylist.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCVPlaylist then
  TCVPlaylist(Source).AssignTo(Self);
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCVPlaylist.AssignTo(Dest: TPersistent);
var
  list: TCVPlaylist;
  data, dataDest: PCVPlaylistData;
  node, nodeDest: PVirtualNode;
begin
  if Dest is TCVPlaylist then
  begin
    list:= TCVPlaylist(Dest);

    
    list.BeginUpdate;
    try
      // assign items
      list.Clear;
      node:= Self.GetFirst;
      while assigned(node) do
      begin
        data:= Self.GetNodeData(node);
        nodeDest:= list.AddChild(nil);
        dataDest:= list.GetNodeData(nodeDest);
        dataDest.fLastUpdate:= data.fLastUpdate;
        dataDest.Title:= data.Title;
        dataDest.Path:= data.Path;
        dataDest.ImageIndex:= data.ImageIndex;

        // active item
        if fActiveItem = node then
        list.fActiveItem:= nodeDest;
        
        node:= Self.GetNext(node);
      end;

      list.fLastIconRefresh:= fLastIconRefresh;
      list.fActiveFilePath:= fActiveFilePath;
      list.fLoopList:= fLoopList;
    finally
      list.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCVPlaylist.Clear;
begin
  ActiveItem:= nil;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when a object is dropped.
-------------------------------------------------------------------------------}
procedure TCVPlaylist.DoDragDrop(Source: TObject; DataObject:
    IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var
    Effect: Integer; Mode: TDropMode);
var
 // data: PCEBookData;
  Attachmode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  i: Integer;
  hd: TCommonShellIDList;
  data: PCVPlaylistData;
  node: PVirtualNode;
  ns: TNamespace;
begin

  // Get Attach mode
  case Mode of
    dmAbove:
      AttachMode := amInsertBefore;
    dmOnNode: begin
      AttachMode:= amInsertBefore;
    end;
    dmBelow:
      AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;

  if Source = self then // Moving nodes inside self.
  begin
    Nodes:= self.GetSortedSelection(True);
    if (Length(Nodes) > 0) then
    begin
      node:= Nodes[0];
      if assigned(DropTargetNode) and (Mode = dmOnNode) then
      begin
        if DropTargetNode.Index > node.Index then
        AttachMode:= amInsertAfter
        else
        AttachMode:= amInsertBefore;
      end;
      // move first
      Self.MoveTo(node, self.DropTargetNode, AttachMode, False);
      // move rest
      for I:= 1 to High(Nodes) do
      begin
        self.MoveTo(Nodes[I], node, amInsertAfter, False);
        node:= Nodes[I];
      end;
    end;
  end
  else if assigned(DataObject) then// Dropping from outside self.
  begin
    Effect:= DROPEFFECT_LINK;
    hd:= TCommonShellIDList.Create;

    try
      Self.BeginUpdate;
      // Add new playlist item
      hd.LoadFromDataObject(DataObject);
      for i:= 0 to hd.PIDLCount-1 do
      begin
        if AttachMode = amNoWhere then
        AttachMode:= amInsertAfter;

        ns:= TNamespace.Create(hd.AbsolutePIDL(i), nil);
        try
          if not ns.Folder then
          begin
            node:= InsertNode(Self.DropTargetNode, AttachMode);
            Self.ValidateNode(node, false);
            data:= GetNodeData(node);
            data.Title:= ns.NameParseAddressInFolder;
            data.Path:= ns.NameForParsing;
            data.ImageIndex:= ns.GetIconIndex(false, icSmall);
            data.fLastUpdate:= GetTickCount;
          end;
        finally
          ns.Free;
        end;
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
end;

{*------------------------------------------------------------------------------
  Get's called when a object is dragged over.
-------------------------------------------------------------------------------}
function TCVPlaylist.DoDragOver(Source: TObject; Shift: TShiftState;
    State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer):
    Boolean;
var
  Nodes: TNodeArray;
  i: Integer;
  tmpNode: PVirtualNode;
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
//  else if (source is TCESpTabToolbar) or (source is TSpTBXItemDragObject) then
//  begin
//    Effect:= DROPEFFECT_NONE;
//    Result:= false;
//  end
  else // Shell drop
  begin
    if State = dsDragLeave then
    begin

    end
    else if State = dsDragMove then
    begin
      if Assigned(Self.DropTargetNode) and (Mode <> dmNowhere) then
      begin
        Result:= True;
        Effect:= DROPEFFECT_LINK;
      end
      else
      begin
        Result:= True;
        Effect:= DROPEFFECT_LINK;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do FreeNode
-------------------------------------------------------------------------------}
procedure TCVPlaylist.DoFreeNode(Node: PVirtualNode);
var
  data: PCVPlaylistData;
begin
  if fActiveItem = Node then
  ActiveItem:= nil;
  data:= Self.GetNodeData(Node);
  data.Title:=  '';
  data.Path:= '';
  inherited;
end;

{-------------------------------------------------------------------------------
  Do GetImageIndex
-------------------------------------------------------------------------------}
function TCVPlaylist.DoGetImageIndex(Node: PVirtualNode; Kind:
    TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index:
    Integer): TCustomImageList;
var
  data: PCVPlaylistData;
  ns: TNamespace;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    data:= Self.GetNodeData(Node);
    
    // refresh icon if needed
    if data.fLastUpdate < fLastIconRefresh then
    begin
      try
        ns:= TNamespace.CreateFromFileName(data.Path);
        try
          data.ImageIndex:= ns.GetIconIndex(false, icSmall);
          data.fLastUpdate:= GetTickCount;
        finally
          ns.Free;
        end;
      except
        data.ImageIndex:= -1;
      end;
    end;

    Index:= data.ImageIndex;
  end;
  Result:= inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

{-------------------------------------------------------------------------------
  Do GetNodeHint
-------------------------------------------------------------------------------}
function TCVPlaylist.DoGetNodeHint(Node: PVirtualNode; Column:
    TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  data: PCVPlaylistData;
begin
  data:= Self.GetNodeData(Node);
  Result:= data.Path;
  Result:= inherited DoGetNodeHint(Node, Column, LineBreakStyle);
end;

{-------------------------------------------------------------------------------
  Do GetText
-------------------------------------------------------------------------------}
procedure TCVPlaylist.DoGetText(Node: PVirtualNode; Column:
    TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
var
  data: PCVPlaylistData;
begin
  data:= Self.GetNodeData(Node);
  Text:= data.Title;
  inherited;
end;

{-------------------------------------------------------------------------------
  Do KeyAction
-------------------------------------------------------------------------------}
function TCVPlaylist.DoKeyAction(var CharCode: Word; var Shift:
    TShiftState): Boolean;
begin
  Result:= inherited DoKeyAction(CharCode, Shift);
  if (CharCode = VK_DELETE) and (Shift = []) then
  begin
    Self.BeginUpdate;
    try
      Self.DeleteSelectedNodes;
    finally
      Self.EndUpdate;
    end;
    DoNavigationStateChange;
  end;
end;

{-------------------------------------------------------------------------------
  DoNavigationStateChange
-------------------------------------------------------------------------------}
procedure TCVPlaylist.DoNavigationStateChange;
begin
  if (Self.UpdateCount = 0) and assigned(fOnNavigationStateChange) then
  fOnNavigationStateChange(Self);
end;

{-------------------------------------------------------------------------------
  Do PaintText
-------------------------------------------------------------------------------}
procedure TCVPlaylist.DoPaintText(Node: PVirtualNode; const Canvas:
    TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;
  if Node = fActiveItem then
  Canvas.Font.Style:= [fsBold]
  else
  Canvas.Font.Style:= [];
end;

{-------------------------------------------------------------------------------
  Find Item
-------------------------------------------------------------------------------}
function TCVPlaylist.FindItem(AFilePath: WideString): PVirtualNode;
var
  node: PVirtualNode;
  data: PCVPlaylistData;
begin
  Result:= nil;
  node:= Self.GetFirst;
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if WideCompareText(AFilePath, data.Path) = 0 then
    begin
      Result:= node;
      break;
    end;
    node:= Self.GetNext(node);
  end;
end;

{-------------------------------------------------------------------------------
  Get ActivePath
-------------------------------------------------------------------------------}
function TCVPlaylist.GetActivePath: WideString;
var
  data: PCVPlaylistData;
begin
  if assigned(fActiveItem) then
  begin
    data:= Self.GetNodeData(fActiveItem);
    Result:= data.Path;
  end
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Get Next Item
  - if ANextFrom is nil then ActiveItem is used
-------------------------------------------------------------------------------}
function TCVPlaylist.GetNextItem(AMakeActive: Boolean = false;
    ANextFrom: PVirtualNode = nil): PVirtualNode;
begin
  if ANextFrom = nil then
  ANextFrom:= fActiveItem;
  if assigned(ANextFrom) then
  begin
    Result:= Self.GetNextSibling(ANextFrom);
    if not assigned(Result) and fLoopList then
    Result:= Self.GetFirst;
  end
  else
  Result:= Self.GetFirst;
  
  if AMakeActive then
  ActiveItem:= Result;
end;

{-------------------------------------------------------------------------------
  Get OkToShellNotifyDispatch
-------------------------------------------------------------------------------}
function TCVPlaylist.GetOkToShellNotifyDispatch: Boolean;
begin
  Result:= not Self.Dragging;
end;

{-------------------------------------------------------------------------------
  Get Previous Item
  - if ANextFrom is nil then ActiveItem is used
-------------------------------------------------------------------------------}
function TCVPlaylist.GetPreviousItem(AMakeActive: Boolean = false;
    APreviousFrom: PVirtualNode = nil): PVirtualNode;
begin
  if APreviousFrom = nil then
  APreviousFrom:= fActiveItem;
  if assigned(APreviousFrom) then
  begin
    Result:= Self.GetPreviousSibling(APreviousFrom);
    if not assigned(Result) and fLoopList then
    Result:= Self.GetLast;
  end
  else
  Result:= Self.GetLast;

  if AMakeActive then
  ActiveItem:= Result;
end;

{-------------------------------------------------------------------------------
  Handle MouseDblClick
-------------------------------------------------------------------------------}
procedure TCVPlaylist.HandleMouseDblClick(var Message: TWMMouse; const
    HitInfo: THitInfo);
begin
  inherited;
  if assigned(HitInfo.HitNode) and (HitInfo.HitNode <> ActiveItem) then
  begin
    ActiveItem:= HitInfo.HitNode;
  end;
end;

{-------------------------------------------------------------------------------
  Insert
-------------------------------------------------------------------------------}
function TCVPlaylist.Insert(ATargetNode: PVirtualNode; AAttachMode:
    TVTNodeAttachMode; APath: WideString; AMakeActive: Boolean = false):
    PVirtualNode;
var
  data: PCVPlaylistData;
begin
  if assigned(ATargetNode) then
  Result:= Self.InsertNode(ATargetNode, AAttachMode, Pointer(1))
  else
  Result:= Self.AddChild(nil, Pointer(1));
  data:= Self.GetNodeData(Result);
  data.Path:= APath;
  data.Title:= WideExtractFileName(APath);
  data.ImageIndex:= -1;
  data.fLastUpdate:= 0;

  Self.BeginUpdate;
  try
    if AMakeActive then
    ActiveItem:= Result;
  finally
    Self.EndUpdate;
  end;

  DoNavigationStateChange;
end;

{-------------------------------------------------------------------------------
  IsEmpty
-------------------------------------------------------------------------------}
function TCVPlaylist.IsEmpty: Boolean;
begin
  Result:= Self.ChildCount[Self.RootNode] = 0;
end;

{-------------------------------------------------------------------------------
  IsSupported
-------------------------------------------------------------------------------}
function TCVPlaylist.IsSupported(AExt: WideString): Boolean;
begin
  Result:= true;
  if assigned(fOnIsSupported) then
  fOnIsSupported(Self, AExt, Result);
end;

{-------------------------------------------------------------------------------
  Notify (on Shell Notify)
-------------------------------------------------------------------------------}
procedure TCVPlaylist.Notify(var Msg: TMessage);
begin
  if HandleAllocated then
  WMShellNotify(Msg)
end;

{-------------------------------------------------------------------------------
  RefreshIcons
-------------------------------------------------------------------------------}
procedure TCVPlaylist.RefreshIcons;
begin
  if GetTickCount = fLastIconRefresh then
  fLastIconRefresh:= GetTickCount + 1
  else
  fLastIconRefresh:= GetTickCount;
  Self.Repaint;
end;

{-------------------------------------------------------------------------------
  Refresh Scroll Height
-------------------------------------------------------------------------------}
procedure TCVPlaylist.RefreshScrollHeight;
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

{-------------------------------------------------------------------------------
  Set ActiveFilePath
-------------------------------------------------------------------------------}
procedure TCVPlaylist.SetActiveFilePath(const Value: WideString);
begin
  if fActiveFilePath <> Value then
  begin
    fActiveFilePath:= Value;
    ActiveItem:= FindItem(fActiveFilePath);
  end;
end;

{-------------------------------------------------------------------------------
  Set ActiveItem
-------------------------------------------------------------------------------}
procedure TCVPlaylist.SetActiveItem(const Value: PVirtualNode);
begin
  if fActiveItem <> Value then
  begin
    fActiveItem:= Value;
    Self.ScrollIntoView(fActiveItem, true);
    Self.Invalidate;
    if assigned(fActiveItem) then
    fActiveFilePath:= GetActivePath;
    if assigned(fOnActiveItemChange) then
    fOnActiveItemChange(Self);
    DoNavigationStateChange;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_Create message
-------------------------------------------------------------------------------}
procedure TCVPlaylist.WMCreate(var Msg: TMessage);
begin
  ShellNotifyManager.RegisterExplorerWnd(Self);
  ChangeNotifier.RegisterShellChangeNotify(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle WM_NCDestroy message
-------------------------------------------------------------------------------}
procedure TCVPlaylist.WMNCDestroy(var Msg: TMessage);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  ShellNotifyManager.UnRegisterExplorerWnd(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle WM_ShellNotify message
-------------------------------------------------------------------------------}
procedure TCVPlaylist.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i: Integer;
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
            vsneAssoccChanged: begin
              RefreshIcons;
            end;
          end;
        end;
      except
      end;
    finally
      ShellEventList.UnlockList;
      ShellEventList.Release;
      Self.Refresh;
    end;
  end;
end;

{##############################################################################}
// TCVFilelist

{-------------------------------------------------------------------------------
  Create an instance of TCVFilelist
-------------------------------------------------------------------------------}
constructor TCVFilelist.Create(AOwner: TComponent);
begin
  inherited;
  // Initilize values
  fActiveFilePath:= '';
  fIncludeSubFolders:= false;
  fMultiThreadedSearch:= false;
  fTaskTag:= GlobalTaskPool.GetUniqueTagID;
  fThreadedIcons:= false;
end;

{-------------------------------------------------------------------------------
  Destroy TCVFilelist
-------------------------------------------------------------------------------}
destructor TCVFilelist.Destroy;
begin
  Abort;
  inherited;
end;

{-------------------------------------------------------------------------------
  Abort
-------------------------------------------------------------------------------}
procedure TCVFilelist.Abort;
begin
  GlobalTaskPool.AbortTasksWithTag(fTaskTag);
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCVFilelist.AssignTo(Dest: TPersistent);
var
  list: TCVFilelist;
  data, dataDest: PCVPlaylistData;
  node, nodeDest: PVirtualNode;
begin
  inherited;
  if Dest is TCVFilelist then
  begin
    list:= TCVFilelist(Dest);

    list.BeginUpdate;
    try
      list.fIncludeSubFolders:= fIncludeSubFolders;
      list.fMultiThreadedSearch:= fMultiThreadedSearch;
      list.fThreadedIcons:= fThreadedIcons;
    finally
      list.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GetImageIndex
-------------------------------------------------------------------------------}
function TCVFilelist.DoGetImageIndex(Node: PVirtualNode; Kind:
    TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index:
    Integer): TCustomImageList;
var
  data: PCVPlaylistData;
  task: TCVFilelistTask;
  ns: TNamespace;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    data:= Self.GetNodeData(Node);
    
    // refresh icon if needed
    if data.fLastUpdate < fLastIconRefresh then
    begin
      // threaded
      if fThreadedIcons then
      begin
        task:= TCVFilelistTask.Create;
        task.TaskType:= fttGetIcon;
        task.Path:= data.Path;
        task.Item:= Node;
        GlobalTaskPool.AddTask(nil, task, true, false, fTaskTag, HandleTaskExecute, HandleTaskDone);
      end
      // non threaded
      else
      begin
        try
          ns:= TNamespace.CreateFromFileName(data.Path);
          try
            data.ImageIndex:= ns.GetIconIndex(false, icSmall);
            data.fLastUpdate:= GetTickCount;
          finally
            ns.Free;
          end;
        except
        end;
      end;
    end;
    
    Index:= data.ImageIndex;
  end;
  Result:= SmallSysImages;
end;

{-------------------------------------------------------------------------------
  Handle TaskDone
-------------------------------------------------------------------------------}
procedure TCVFilelist.HandleTaskDone(Sender: TObject; AObject: TObject; AData:
    Pointer; ATag: Integer);
var
  task: TCVFilelistTask;
  dummy: Integer;
  data: PCVPlaylistData;
begin
  if AObject is TCVFilelistTask then
  begin
    task:= TCVFilelistTask(AObject);
    if task.TaskType = fttGetIcon then
    begin
      // NOTE: this is not safe!!! Do not to delete items if get icon tasks are running.
      data:= Self.GetNodeData(task.Item);
      data.ImageIndex:= task.Index;
      data.fLastUpdate:= GetTickCount;
      Self.InvalidateNode(task.Item);
    end
    else if task.TaskType = fttSearchFiles then
    begin
      HandleTaskSyncMsg(nil, AObject, AData, ATag, FilelistTask_FilesFound, 0, 0, dummy);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle TaskExecute (Runs in a separate thread!!!!)
-------------------------------------------------------------------------------}
procedure TCVFilelist.HandleTaskExecute(Sender: TCCTaskPoolThread; AObject:
    TObject; AData: Pointer; ATag: Integer);
var
  lastStatus: Integer;

  // Do File Search
  procedure DoFileSearch(task: TCVFilelistTask; dirPath: WideString; includeSubFolders: Boolean);
  var
    sr: TCCSearchRec;
    newTask: TCVFilelistTask;
  begin
    if WideFindFirst(dirPath + '*.*', faAnyFile, sr) = S_OK then
    begin
      repeat
        // add file to list
        if (sr.Attr and faDirectory) <> faDirectory then
        begin
          if IsSupported(WideExtractFileExt(sr.Name)) then
          begin
            task.Results.Lock;
            task.Results.Add(dirPath + sr.Name);
            task.Results.Unlock;
            // send status every 100ms.
            if lastStatus < (GetTickCount - 100) then
            begin
              Sender.SendSyncMessage(FilelistTask_FilesFound, 0, 0);
              lastStatus:= GetTickCount;
            end;
          end;
        end
        // search sub folders
        else if IncludeSubFolders and not Sender.IsAborted then       
        begin
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            if fMultiThreadedSearch then
            begin
              // use separate thread for sub folders
              newTask:= TCVFilelistTask.Create;
              newTask.TaskType:= fttSearchFiles;
              newTask.Path:= WideIncludeTrailingPathDelimiter(dirPath + sr.Name);
              newTask.IncludeSubfolders:= includeSubFolders;

              // increase refrence count (we need it in TCVFilelistTask.Destroy)
              task.Results.Lock;
              task.Results.Tag:= task.Results.Tag + 1;
              task.Results.Unlock;
              newTask.Results:= task.Results;
              GlobalTaskPool.AddTask(nil, newTask, true, true, fTaskTag, HandleTaskExecute, HandleTaskDone, TCCTaskEvent(nil), HandleTaskSyncMsg);
            end
            else
            begin
              DoFileSearch(task, WideIncludeTrailingPathDelimiter(dirPath + sr.Name), includeSubFolders);
            end;
          end;
        end;
      until (WideFindNext(sr) <> S_OK) or Sender.IsAborted;
      WideFindClose(sr);
      
      if (task.Results.Count > 0) and not Sender.IsAborted then
      Sender.SendSyncMessage(FilelistTask_FilesFound, 0, 0);
    end;
  end;    

var
  ns: TNamespace;
  task: TCVFilelistTask;
begin
  if AObject is TCVFilelistTask then
  begin
    task:= TCVFilelistTask(AObject);
    // Get Icon
    if task.TaskType = fttGetIcon then
    begin
      try
        ns:= TNamespace.CreateFromFileName(task.Path);
        try
          task.Index:= ns.GetIconIndex(false, icSmall);
        finally
          ns.Free;
        end;
      except
      end;
    end
    // File Search
    else if task.TaskType = fttSearchFiles then
    begin
      DoFileSearch(task,
                   WideIncludeTrailingPathDelimiter(task.Path),
                   task.IncludeSubfolders);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle TaskSyncMsg
-------------------------------------------------------------------------------}
procedure TCVFilelist.HandleTaskSyncMsg(Sender: TCCTaskPoolThread; AObject:
    TObject; AData: Pointer; ATag: Integer; AMsg, AParam1, AParam2: Integer;
    var AResult: Integer);
var
  i: Integer;
  task: TCVFilelistTask;
  node: PVirtualNode;
begin
  if AObject is TCVFilelistTask then
  begin
    task:= TCVFilelistTask(AObject);
    if (task.TaskType = fttSearchFiles) and (AMsg = FilelistTask_FilesFound) and assigned(task.Results) then
    begin
      Self.BeginUpdate;
      try
        task.Results.Lock;
        for i:= 0 to task.Results.Count - 1 do
        begin
          node:= Self.Add(task.Results.Strings[i]);
          // set active item
          if WideCompareText(fActiveFilePath, task.Results.Strings[i]) = 0 then
          begin
            Self.fActiveItem:= node;
          end;
        end;
        task.Results.Clear;
        task.Results.Unlock;
      finally
        Self.EndUpdate;
        if assigned(fActiveItem) then
        Self.ScrollIntoView(fActiveItem, true);
        DoNavigationStateChange;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  PopulateList
-------------------------------------------------------------------------------}
procedure TCVFilelist.PopulateList(AFolderPath: WideString;
    AIncludeSubFolders: Boolean = false);
var
  task: TCVFilelistTask;
begin
  Abort;
  Self.fActiveItem:= nil;
  Self.Clear;
  
  task:= TCVFilelistTask.Create;
  task.TaskType:= fttSearchFiles;
  task.Path:= AFolderPath;
  task.IncludeSubfolders:= AIncludeSubFolders;
  task.Results:= TCCThreadStringList.Create;
  task.Results.Tag:= 1;
  GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleTaskExecute, HandleTaskDone, TCCTaskEvent(nil), HandleTaskSyncMsg);
end;

{-------------------------------------------------------------------------------
  Set ActiveFilePath
-------------------------------------------------------------------------------}
procedure TCVFilelist.SetActiveFilePath(const Value: WideString);
begin
  if fActiveFilePath <> Value then
  begin
    inherited;
    PopulateList(WideExtractFilePath(fActiveFilePath), fIncludeSubFolders);
  end;
end;

{-------------------------------------------------------------------------------
  SetIncludeSubFolders
-------------------------------------------------------------------------------}
procedure TCVFilelist.SetIncludeSubFolders(const Value: Boolean);
begin
//  if fIncludeSubFolders <> Value then
//  begin
//    PopulateList(WideExtractFilePath(fActiveFilePath), fIncludeSubFolders);
//  end;
end;

{##############################################################################}
// TCVFilelistTask

{-------------------------------------------------------------------------------
  Destroy TCVFilelistTask
-------------------------------------------------------------------------------}
destructor TCVFilelistTask.Destroy;
begin
  if assigned(Results) then
  begin
    // decrease refrence count
    Results.Lock;
    Results.Tag:= Results.Tag - 1;
    Results.Unlock;
    // free results if it's not used anymore
    if Results.Tag <= 0 then
    FreeAndNil(Results);
  end;
  inherited;
end;

end.
