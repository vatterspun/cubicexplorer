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
//  The Original Code is fCE_BookmarkPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_BookmarkPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_GlobalCtrl, CE_Bookmarks, CE_BookmarkTree, dCE_Images,
  CE_AppSettings, CEJvDockVSNetStyleTBX, CE_Layout,
  // JVCL
  JvDockVSNetStyle,
  // VSTools
  VirtualTrees, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualShellNotifier, VirtualResources,
  // PNG Controls
  PngImageList,
  // Toolbar2k, SpTBX
  TB2Dock, TB2Item, SpTBXItem,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Contnrs, Menus;

type
  TCEBookmarkPanelSettings = class;

  TCEBookmarkPanel = class(TCECustomDockableForm, IVirtualShellNotify)
    BookmarkPopupMenu: TSpTBXPopupMenu;
    but_addCat: TSpTBXItem;
    but_addBookmark: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    but_rename: TSpTBXItem;
    but_delete: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    but_openAll: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    but_properties: TSpTBXItem;
    but_refresh: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    but_addSession: TSpTBXItem;
    but_open_new_tab: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BookmarkPopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);
  private
    fSettings: TCEBookmarkPanelSettings;
    function GetOkToShellNotifyDispatch: Boolean;
    { Private declarations }
  protected
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure Notify(var Msg: TMessage);
    procedure RefreshBookmarks(OnlyIfLocal: Boolean = false);
    procedure WMCreate(var Msg: TMessage); message WM_CREATE;
    procedure WMNCDestroy(var Msg: TMessage); message WM_NCDESTROY;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
    property OkToShellNotifyDispatch: Boolean read GetOkToShellNotifyDispatch;
  public
    BookmarkMenuItems: TComponentList;
    BookmarksPath: WideString;
    BookmarkTree: TCEBookmarkTree;
    procedure LoadBookmarks;
    procedure OnBookmarksChange(Sender: TObject);
    procedure OpenAll(Mode: Integer = 0);
    procedure RePopulateBookmarkItems;
    procedure SaveBookmarks;
  published
    property Settings: TCEBookmarkPanelSettings read fSettings write fSettings;
  end;

  TCEBookmarkPanelSettings = class(TPersistent)
  private
    fFontSize: Integer;
    fLineHeight: Integer;
    fShowOpenAllAtTop: Boolean;
    fShowOpenAllItem: Boolean;
    fUseSystemPopupMenu: Boolean;
    function GetAutoCollapse: Boolean;
    function GetAutoExpand: Boolean;
    function GetOpenInNewTab: Boolean;
    function GetSingleClickMode: Boolean;
    procedure SetAutoCollapse(const Value: Boolean);
    procedure SetAutoExpand(const Value: Boolean);
    procedure SetFontSize(const Value: Integer);
    procedure SetLineHeight(const Value: Integer);
    procedure SetOpenInNewTab(const Value: Boolean);
    procedure SetShowOpenAllAtTop(const Value: Boolean);
    procedure SetShowOpenAllItem(const Value: Boolean);
    procedure SetSingleClickMode(const Value: Boolean);
  public
    BookmarkPanel: TCEBookmarkPanel;
  published
    property AutoCollapse: Boolean read GetAutoCollapse write SetAutoCollapse;
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand;
    property FontSize: Integer read fFontSize write SetFontSize;
    property LineHeight: Integer read fLineHeight write SetLineHeight;
    property OpenInNewTab: Boolean read GetOpenInNewTab write SetOpenInNewTab;
    property ShowOpenAllAtTop: Boolean read fShowOpenAllAtTop write
        SetShowOpenAllAtTop;
    property ShowOpenAllItem: Boolean read fShowOpenAllItem write
        SetShowOpenAllItem;
    property SingleClickMode: Boolean read GetSingleClickMode write
        SetSingleClickMode;
    property UseSystemPopupMenu: Boolean read fUseSystemPopupMenu write
        fUseSystemPopupMenu;
  end;

var
  CEBookmarkPanel: TCEBookmarkPanel;

implementation

uses
  dCE_Actions, CE_BookmarkBar, fCE_FileView, CE_VistaFuncs,
  CE_StdBookmarkComps, fCE_BookmarkPropDlg, CE_LanguageEngine,
  fCE_ItemSelectSaveDlg, CE_Sessions, MPCommonObjects, CE_Utils, ShlObj;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEBookmarkPanel is created
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.FormCreate(Sender: TObject);
begin
  inherited;
  fSettings:= TCEBookmarkPanelSettings.Create;
  fSettings.BookmarkPanel:= Self;
  // Default settings
  fSettings.fFontSize:= -1;
  fSettings.fLineHeight:= -1;
  fSettings.fShowOpenAllItem:= true;
  
  BookmarkMenuItems:= TComponentList.Create(false);
  TopDock.Name:= 'BookmarkPanel_TopDock';
  BottomDock.Name:= 'BookmarkPanel_BottomDock';
  Caption:= 'Bookmarks';
  BookmarkTree:= TCEBookmarkTree.Create(self);
  BookmarkTree.Parent:= self;
  SetDesktopIconFonts(BookmarkTree.Font);
  BookmarkTree.Align:= alClient;
  BookmarkTree.Indent:= 24;
  BookmarkTree.SingleClickMode:= true;
  BookmarkTree.TabOrder:= 2;
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 18;
  GlobalFocusCtrl.CtrlList.Add(BookmarkTree);
  BookmarkTree.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  BookmarkTree.OnBookmarksChange:= OnBookmarksChange;
  //BookmarkTree.PopupMenu:= BookmarkPopupMenu;
  BookmarkTree.OnMouseUp:= DoMouseUp;

  GlobalAppSettings.AddItem('BookmarksPanel', fSettings, true);
end;

{*------------------------------------------------------------------------------
  Get's called when TCEBookmarkPanel is Destroyed
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.FormDestroy(Sender: TObject);
begin
  BookmarkMenuItems.Free;
  BookmarkTree.Free;
  fSettings.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when bookmarks have changed
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.OnBookmarksChange(Sender: TObject);
begin
  RePopulateBookmarkItems;
  SaveBookmarks;
end;

{*------------------------------------------------------------------------------
  Get's called before Popup
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.BookmarkPopupMenuPopup(Sender: TObject);
begin
  but_openAll.Visible:= Settings.ShowOpenAllItem;
  if BookmarkTree.SelectedCount > 0 then
  begin
    but_properties.Enabled:= true;
    but_delete.Enabled:= true;
    but_rename.Enabled:=  BookmarkTree.SelectedCount = 1;
    if Assigned(BookmarkTree.FocusedNode) then
    begin
      if BookmarkTree.FocusedNode.ChildCount > 0 then
      but_openAll.Enabled:= true
      else
      but_openAll.Enabled:= false;
    end
    else
    but_openAll.Enabled:= false;
  end
  else
  begin
    but_delete.Enabled:= false;
    but_rename.Enabled:= false;
    but_openAll.Enabled:= false;
    but_properties.Enabled:= false;
  end;

  if Assigned(BookmarkTree.FocusedNode) then
  begin
    if Settings.OpenInNewTab then
    but_open_new_tab.Caption:= _('Open in current tab')
    else
    but_open_new_tab.Caption:= _('Open in new tab');
    but_open_new_tab.Visible:= BookmarkTree.GetNodeComp(BookmarkTree.FocusedNode) is TCENormalItemComp;
  end;
end;

{*------------------------------------------------------------------------------
  Handle popup menu item click.
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.PopupMenuClick(Sender: TObject);
var
  apidl: PItemIDList;
  node: PVirtualNode;
  comp: TCECustomBookComp;
  sessionDlg: TCEItemSelectSaveDlg;
begin
  case TSpTBXItem(Sender).Tag of
    1: begin
         BookmarkTree.AddBookItem('category',BookmarkTree.FocusedNode);
         BookmarkTree.BookmarksChange;
       end;
    2: begin
         if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         begin
           node:= BookmarkTree.AddBookItem('item',BookmarkTree.FocusedNode);
           comp:= BookmarkTree.GetNodeComp(node);
           apidl:= PIDLMgr.CopyPIDL(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL);
           TCENormalItemComp(comp).LoadFromPIDL(apidl);
           BookmarkTree.BookmarksChange;
         end;
       end;
    3: BookmarkTree.EditSelectedNode;
    4: BookmarkTree.SafeDeleteSelectedNodes;
    5: OpenAll;
    6: OpenAll(1);
    7: OpenAll(2);
    8: begin
         if assigned(BookmarkTree.FocusedNode) then
         begin
           comp:= BookmarkTree.GetNodeComp(BookmarkTree.FocusedNode);
           ShowBookmarkPropDlg(comp,BookmarkTree);
         end;
       end;
    9: RefreshBookmarks;
    10: begin // Add Session
      sessionDlg:= TCEItemSelectSaveDlg.Create(nil);
      try
        sessionDlg.Caption:= _('Select Session');
        sessionDlg.label_combotitle.Caption:= _('Session');
        GetSessionNames(sessionDlg.combo.Items);
        if GlobalSessions.ActiveSessionIndex > -1 then
        begin
          sessionDlg.combo.ItemIndex:= GlobalSessions.ActiveSessionIndex;
        end;
        if (sessionDlg.ShowModal = mrOK) and (sessionDlg.combo.ItemIndex > -1) then
        begin
          node:= BookmarkTree.AddBookItem('session',BookmarkTree.FocusedNode);
          comp:= BookmarkTree.GetNodeComp(node);
          TCESessionComp(comp).SessionName:= sessionDlg.combo.Items.Strings[sessionDlg.combo.ItemIndex];
          TCESessionComp(comp).Title:= TCESessionComp(comp).SessionName;
          BookmarkTree.BookmarksChange;
        end;
      finally
        sessionDlg.Free;
      end;
    end;
    // Open in new tab
    11: begin
      if assigned(BookmarkTree.FocusedNode) then
      begin
        comp:= BookmarkTree.GetNodeComp(BookmarkTree.FocusedNode);
        if Settings.OpenInNewTab then
        comp.MouseClick([ssAlt,ssLeft], mbLeft)
        else
        comp.MouseClick([ssMiddle], mbMiddle);
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Open all bookmarks

  0 = Only Folders
  1 = Open Folders and Launch Files
  2 = Launch only files
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.OpenAll(Mode: Integer = 0);
var
  node, chNode: PVirtualNode;
  comp: TCECustomBookComp;
begin
  if BookmarkTree.SelectedCount = 1 then
  begin
    node:= BookmarkTree.GetFirstSelected;
    chNode:= node.FirstChild;
    while assigned(chNode) do
    begin
      comp:= BookmarkTree.GetNodeComp(chNode);
      if TCENormalItemComp(comp).IsFolder then
      begin
        if Mode < 2 then
        comp.MouseClick([ssShift,ssMiddle],mbMiddle)
      end
      else if Mode > 0 then
      begin
        comp.MouseClick([ssDouble, ssLeft], mbLeft);
      end;
      chNode:= chNode.NextSibling;
    end;
  end
  else
  begin
    node:= BookmarkTree.GetFirstSelected;
    while assigned(node) do
    begin
      comp:= BookmarkTree.GetNodeComp(node);
      if comp is TCENormalItemComp then
      begin
        if TCENormalItemComp(comp).IsFolder then
        begin
          if Mode < 2 then
          comp.MouseClick([ssShift,ssMiddle],mbMiddle)
        end
        else if Mode > 0  then
        begin
          comp.MouseClick([ssDouble, ssLeft], mbLeft);
        end;
      end;
      node:= BookmarkTree.GetNextSelected(node);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Load Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.LoadBookmarks;
begin
  if WideFileExists(BookmarksPath) then
  begin
    BookmarkTree.BeginUpdate;
    try
      BookmarkTree.LoadFromXmlFile(BookmarksPath);
    finally
      BookmarkTree.EndUpdate;
      RePopulateBookmarkItems;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.SaveBookmarks;
begin
  if not ReadOnlySettings and (BookmarksPath <> '') then
  begin
    try
      BookmarkTree.SaveToXmlFile(BookmarksPath);
    except
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do MouseUp
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.DoMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  b, defaultPopup: Boolean;
  p: TPoint;
  info: THitInfo;
  data: PCEBookData;
begin
  if Button = mbRight then
  begin
    defaultPopup:= true;
    GetCursorPos(p);

    b:= CEDockStyle.ChannelOption.MouseleaveHide;
    CEDockStyle.ChannelOption.MouseleaveHide:= false;

    if Settings.UseSystemPopupMenu then
    begin
      BookmarkTree.GetHitTestInfoAt(X, Y, true, info);
      if assigned(info.HitNode) and ((hiOnItemLabel in info.HitPositions) or (hiOnNormalIcon in info.HitPositions)) then
      begin
        data:= BookmarkTree.GetNodeData(info.HitNode);
        if assigned(data.BookComp) then
        defaultPopup:= not data.BookComp.DoPopup(p.X, p.Y);
      end;
    end;

    if defaultPopup then
    BookmarkPopupMenu.Popup(p.X, p.Y);

    CEDockStyle.ChannelOption.MouseleaveHide:= b;
  end;
end;

{-------------------------------------------------------------------------------
  Get OkToShellNotifyDispatch
-------------------------------------------------------------------------------}
function TCEBookmarkPanel.GetOkToShellNotifyDispatch: Boolean;
begin
  Result:= not BookmarkTree.Dragging;
end;

{-------------------------------------------------------------------------------
  Notify (on Shell Notify)
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.Notify(var Msg: TMessage);
begin
  if HandleAllocated then
  WMShellNotify(Msg)
end;

{-------------------------------------------------------------------------------
  Refresh Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.RefreshBookmarks(OnlyIfLocal: Boolean = false);
var
  node: PVirtualNode;
  data: PCEBookData;
begin
  BookmarkTree.BeginUpdate;
  try
    node:= BookmarkTree.GetFirst;
    while node <> nil do
    begin
      data:= BookmarkTree.GetNodeData(Node);
      if assigned(data) then
      begin
        if data.BookComp is TCENormalItemComp then
        begin
          TCENormalItemComp(data.BookComp).Refresh(OnlyIfLocal);
        end;
      end;
      node:= BookmarkTree.GetNext(node);
    end;
  finally
    BookmarkTree.EndUpdate;
    BookmarkTree.Refresh;
    RePopulateBookmarkItems;
  end;
end;

procedure TCEBookmarkPanel.RePopulateBookmarkItems;
var
  i: Integer;
  item: TTBCustomItem;
begin
  for i:= 0 to BookmarkMenuItems.Count - 1 do
  begin
    item:= TTBCustomItem(BookmarkMenuItems.Items[i]);
    item.ViewBeginUpdate;
    PopulateBookmarkItem(item, BookmarkTree, item is TSpTBXSubmenuItem);
    item.ViewEndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_Create message
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.WMCreate(var Msg: TMessage);
begin
  ShellNotifyManager.RegisterExplorerWnd(Self);
  ChangeNotifier.RegisterShellChangeNotify(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle WM_NCDestroy message
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.WMNCDestroy(var Msg: TMessage);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  ShellNotifyManager.UnRegisterExplorerWnd(Self);
  inherited;
end;

{*------------------------------------------------------------------------------
  Handle WMShellNotify messages
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i: Integer;
  b1, b2: Boolean;
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
        b1:= false;
        b2:= false;
        for i:= 0 to List.Count - 1 do
        begin
          ShellEvent:= TVirtualShellEvent(List.Items[i]);
          case ShellEvent.ShellNotifyEvent of
            vsneDriveAddGUI,
            vsneDriveRemoved,
            vsneMediaRemoved,
            vsneMediaInserted,
            vsneDriveAdd: if not b1 then
            begin
              RefreshBookmarks(false);
              b1:= true;
            end;
            vsneUpdateDir, vsneUpdateItem, vsneUpdateImage, vsneAssoccChanged: if not b2 then
            begin
              BookmarkTree.RefreshIcons;
              b2:= true;
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

{-------------------------------------------------------------------------------
  Get/Set SingleClickMode
-------------------------------------------------------------------------------}
function TCEBookmarkPanelSettings.GetSingleClickMode: Boolean;
begin
  Result:= BookmarkPanel.BookmarkTree.SingleClickMode;
end;
procedure TCEBookmarkPanelSettings.SetSingleClickMode(const Value: Boolean);
begin
  BookmarkPanel.BookmarkTree.SingleClickMode:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoExpand
-------------------------------------------------------------------------------}
function TCEBookmarkPanelSettings.GetAutoExpand: Boolean;
begin
  Result:= BookmarkPanel.BookmarkTree.AutoExpand;
end;
procedure TCEBookmarkPanelSettings.SetAutoExpand(const Value: Boolean);
begin
  BookmarkPanel.BookmarkTree.AutoExpand:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoCollapse
-------------------------------------------------------------------------------}
function TCEBookmarkPanelSettings.GetAutoCollapse: Boolean;
begin
  Result:= BookmarkPanel.BookmarkTree.AutoCollapse;
end;
procedure TCEBookmarkPanelSettings.SetAutoCollapse(const Value: Boolean);
begin
  BookmarkPanel.BookmarkTree.AutoCollapse:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set OpenInNewTab
-------------------------------------------------------------------------------}
function TCEBookmarkPanelSettings.GetOpenInNewTab: Boolean;
begin
  Result:= OpenBookmarkInNewTabByDefault;
end;
procedure TCEBookmarkPanelSettings.SetOpenInNewTab(const Value: Boolean);
begin
  OpenBookmarkInNewTabByDefault:= Value;
end;

{-------------------------------------------------------------------------------
  Set FontSize
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanelSettings.SetFontSize(const Value: Integer);
begin
  fFontSize:= Value;
  if fFontSize > 0 then
  BookmarkPanel.BookmarkTree.Font.Size:= fFontSize
  else
  SetDesktopIconFonts(BookmarkPanel.BookmarkTree.Font);
end;

{-------------------------------------------------------------------------------
  Set Line Height
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanelSettings.SetLineHeight(const Value: Integer);
var
  i: Cardinal;
  node: PVirtualNode;
begin
  fLineHeight:= Value;
  i:= BookmarkPanel.BookmarkTree.DefaultNodeHeight;
  if fLineHeight > 0 then
  BookmarkPanel.BookmarkTree.DefaultNodeHeight:= fLineHeight
  else
  BookmarkPanel.BookmarkTree.DefaultNodeHeight:= SmallShellIconSize + 1;

  // resize nodes
  if i <> BookmarkPanel.BookmarkTree.DefaultNodeHeight then
  begin
    BookmarkPanel.BookmarkTree.BeginUpdate;
    try
      node:= BookmarkPanel.BookmarkTree.GetFirstInitialized;
      while assigned(node) do
      begin
        BookmarkPanel.BookmarkTree.NodeHeight[node]:= BookmarkPanel.BookmarkTree.DefaultNodeHeight;
        node:= BookmarkPanel.BookmarkTree.GetNextInitialized(node);
      end;
    finally
      BookmarkPanel.BookmarkTree.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set ShowOpenAllAtTop
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanelSettings.SetShowOpenAllAtTop(const Value: Boolean);
begin
  if fShowOpenAllAtTop <> Value then
  begin
    fShowOpenAllAtTop:= Value;
    BookmarkPanel.RePopulateBookmarkItems;
  end;
end;

{-------------------------------------------------------------------------------
  Set ShowOpenAllItem
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanelSettings.SetShowOpenAllItem(const Value: Boolean);
begin
  if fShowOpenAllItem <> Value then
  begin
    fShowOpenAllItem:= Value;
    BookmarkPanel.RePopulateBookmarkItems;
  end;
end;

end.
