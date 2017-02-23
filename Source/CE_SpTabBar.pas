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
//  The Original Code is CE_SpTabBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_SpTabBar;

interface

uses
  // CE Units
  fCE_TabPage, CE_Layout, CE_Utils, CE_AppSettings, CE_Toolbar, CE_FileUtils,
  // SpTBX
  SpTBXItem, SpTBXTabs, TB2Item, TB2Dock, SpTBXSkins,
  // VSTools
  MPShellUtilities,
  // Windows
  Classes, Windows, SysUtils, Dialogs, Messages, Controls, Forms, ActiveX,
  ExtCtrls, Graphics, StrUtils, ShlObj, Menus, Contnrs, ImgList;

type
  TTBItemViewerAccess = class(TTBItemViewer);
  TSpTBXCustomTabSetAccess = class(TSpTBXCustomTabSet);
  TSpTBXTabItemViewerAccess = class(TSpTBXTabItemViewer);
  TCECustomTabPageAccess = class(TCECustomTabPage);
  TCESpTabSet = class;

  TArrowDirection = (adLeft, adRight, adUp, adDown);

  TCEScrollArrowItem = class(TSpTBXItem)
  private
    fArrowDirection: TArrowDirection;
    fArrowSize: Integer;
  protected
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State:
        TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var
        CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage:
        TSpTBXPaintStage; var PaintDefault: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ArrowDirection: TArrowDirection read fArrowDirection write
        fArrowDirection;
    property ArrowSize: Integer read fArrowSize write fArrowSize;
  end;

  // Closed Tab History Item
  TCEClosedTabHistoryItem = class(TObject)
  public
    TabCaption: WideString;
    TabImageIndex: Integer;
    TabImageList: TCustomImageList;
    TabIndex: Integer;
    TabPageClass: TCECustomTabPageClass;
    TabSettings: TStream;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  // Tab Item Viewer
  TCESpTabItemViewer = class(TSpTBXTabItemViewer)
  private
    function IsTabCloseButtonVisible: Boolean;
  end;

  // Tab Item
  TCESpTabItem = class(TSpTBXTabItem)
  private
    function GetPageVisibility: Boolean;
    procedure SetPageVisibility(const Value: Boolean);
  protected
    fPage: TCECustomTabPage;
    procedure DoTabClosing(var Allow, CloseAndFree: Boolean); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    destructor Destroy; override;
    function CloseTab: Boolean;
    function GetTabSet: TCESpTabSet;
    property Page: TCECustomTabPage read fPage;
    property PageVisibility: Boolean read GetPageVisibility write SetPageVisibility;
  end;
  
  // Tab Toolbar
  TCESpTabToolbar = class(TSpTBXTabToolbar)
  private
    centerTab: TSpTBXTabItem;
    fAutofitting: Boolean;
    fCEAutoFit: Boolean;
    firstTab: TSpTBXTabItem;
    fPreventLastTabClosing: Boolean;
    lastTab: TSpTBXTabItem;
    function GetTabCount: Integer;
    procedure SetCEAutoFit(const Value: Boolean);
  protected
    FLastDropMark: TRect;
    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer): Boolean; override;
    procedure DoAutofit; virtual;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean; Action:
        TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState;
        var Accept: Boolean); override;
    function GetNextTab(ANextFrom: TTBCustomItem): TSpTBXTabItem;
    function GetPrevTab(APrevFrom: TTBCustomItem): TSpTBXTabItem;
    procedure HandleArrowClick(Sender: TObject); virtual;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea:
        Boolean; PaintBorders: Boolean = True); override;
    procedure RightAlignItems; override;
    procedure Scroll(ToRight: Boolean = true);
    procedure ScrollToView(AItem: TSpTBXTabItem; ACenter: Boolean = false);
    procedure WMLButtonDblClick(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
  public
    LeftArrow: TCEScrollArrowItem;
    RightArrow: TCEScrollArrowItem;
    constructor Create(AOwner: TComponent); override;
    procedure ArrangeTabs; virtual;
    function CanTabClose: Boolean;
    function GetTab(Index: Integer): TCESpTabItem;
    function GetTabAt(X, Y: Integer): TCESpTabItem;
    property CEAutoFit: Boolean read fCEAutoFit write SetCEAutoFit;
    property TabCount: Integer read GetTabCount;
  published
    property OnMouseWheel;
    property PreventLastTabClosing: Boolean read fPreventLastTabClosing write
        fPreventLastTabClosing;
  end;

  TCETabSettings = class(TPersistent)
  private
    fAllowDropToTab: Boolean;
    fClosedTabHistory: Boolean;
    fDblClickCloseTab: Boolean;
    fNewTabNamespace: TNamespace;
    fNewTabPath: WideString;
    fNewTabSelect: Boolean;
    fNewTabType: Integer;
    fOpenNextToCurrent: Boolean;
    fOpenTabSelect: Boolean;
    fReuseTabs: Boolean;
    fSwitchTabOnDragHover: Boolean;
    fUndoCount: Integer;
    function GetAutoFit: Boolean;
    function GetAutoFitMaxSize: Integer;
    function GetCloseButton: TSpTBXTabCloseButton;
    function GetMaxTabSize: Integer;
    procedure SetNewTabPath(const Value: WideString);
    procedure SetAutoFit(const Value: Boolean);
    procedure SetAutoFitMaxSize(const Value: Integer);
    procedure SetCloseButton(const Value: TSpTBXTabCloseButton);
    procedure SetMaxTabSize(const Value: Integer);
  public
    TabSet: TCESpTabSet;
    constructor Create; virtual;
    destructor Destroy; override;
    property NewTabNamespace: TNamespace read fNewTabNamespace write
        fNewTabNamespace;
  published
    property AllowDropToTab: Boolean read fAllowDropToTab write fAllowDropToTab;
    property NewTabPath: WideString read fNewTabPath write SetNewTabPath;
    property NewTabSelect: Boolean read fNewTabSelect write fNewTabSelect;
    property NewTabType: Integer read fNewTabType write fNewTabType;
    property OpenTabSelect: Boolean read fOpenTabSelect write fOpenTabSelect;
    property ReuseTabs: Boolean read fReuseTabs write fReuseTabs;
    property AutoFit: Boolean read GetAutoFit write SetAutoFit;
    property AutoFitMaxSize: Integer read GetAutoFitMaxSize write SetAutoFitMaxSize;
    property CloseButton: TSpTBXTabCloseButton read GetCloseButton write
        SetCloseButton;
    property ClosedTabHistory: Boolean read fClosedTabHistory write
        fClosedTabHistory default true;
    property DblClickCloseTab: Boolean read fDblClickCloseTab write
        fDblClickCloseTab;
    property MaxTabSize: Integer read GetMaxTabSize write SetMaxTabSize;
    property OpenNextToCurrent: Boolean read fOpenNextToCurrent write
        fOpenNextToCurrent;
    property SwitchTabOnDragHover: Boolean read fSwitchTabOnDragHover write
        fSwitchTabOnDragHover;
    property UndoCount: Integer read fUndoCount write fUndoCount;
  end;

  // Tab Set
  TCESpTabSet = class(TSpTBXTabSet, IDropTarget)
  private
    fActivePopupTab: TCESpTabItem;
    fClosingTab: TCESpTabItem;
    fDataObj: IDataObject;
    fDropTab: TCESpTabItem;
    fDropTimer: TTimer;
    fEnableClosedTabHistory: Boolean;
    fLayoutController: TCELayoutController;
    fSettings: TCETabSettings;
    fTabPageHost: TWinControl;
    fTabPopupMenu: TPopupMenu;
    function GetCEActiveTabIndex: Integer;
    function GetCEAutoFit: Boolean;
    function GetTabCount: Integer;
    procedure SetCEActiveTabIndex(const Value: Integer);
    procedure SetCEAutoFit(const Value: Boolean);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    fActiveTab: TCESpTabItem;
    fActiveTabHistory: TObjectList;
    fClosedTabHistory: TObjectList;
    function AddToClosedTabHistory(ATab: TCESpTabItem): TCEClosedTabHistoryItem;
        virtual;
    // Tabs
    function CanActiveTabChange(const TabIndex, NewTabIndex: Integer): Boolean;
        override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure DoActiveTabChange(const ItemIndex: Integer); override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt:
        TPoint; var dwEffect: Longint): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint):
        HResult; virtual; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var
        dwEffect: Longint): HResult; virtual; stdcall;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    procedure HandleDrawBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
        const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure HandleDropTimer(Sender: TObject); virtual;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer); virtual;
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer); virtual;
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta:
        Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure HandleToolbarResize(Sender: TObject); virtual;
    procedure HandleUndoItemClick(Sender: TObject);
    procedure TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean = True);
        override;
    procedure TabInserted(Item: TSpTBXTabItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTab(TabPageClass: TCECustomTabPageClass; SelectTab: Boolean =
        false; ActivatePage: Boolean = true; ATabIndex: Integer = -1): TCESpTabItem;
    function CanUndoTabClose: Boolean;
    function CloseAllTabs(ExceptThis: TCESpTabItem = nil; Force: Boolean = false):
        Boolean;
    function CloseSelectedTab(Force: Boolean = false): Boolean;
    function CloseTab(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function CloseTabsOnLeft(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function CloseTabsOnRight(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function GetActiveTab: TCESpTabItem;
    function GetFirstTab: TCESpTabItem;
    function GetLastTab: TCESpTabItem;
    function GetNextTab(From: TSpTBXTabItem): TCESpTabItem;
    function GetPrevTab(From: TSpTBXTabItem): TCESpTabItem;
    function GetTabAt(X, Y: Integer): TCESpTabItem;
    function PopulateUndoList(ASubMenu: TTBCustomItem): Integer;
    procedure SelectNextTab(GoForward: Boolean = true);
    procedure SelectPrevSelectedTab;
    procedure SelectTab(ATab: TSpTBXTabItem);
    procedure UndoTabClose(AIndex: Integer = 0);
    property ActivePopupTab: TCESpTabItem read fActivePopupTab write
        fActivePopupTab;
    property CEActiveTabIndex: Integer read GetCEActiveTabIndex write
        SetCEActiveTabIndex;
    property CEAutoFit: Boolean read GetCEAutoFit write SetCEAutoFit;
    property LayoutController: TCELayoutController read fLayoutController write
        fLayoutController;
    property TabCount: Integer read GetTabCount;
    property TabPageHost: TWinControl read fTabPageHost write fTabPageHost;
    property TabPopupMenu: TPopupMenu read fTabPopupMenu write fTabPopupMenu;
  published
    property ActiveTab: TCESpTabItem read fActiveTab;
    property EnableClosedTabHistory: Boolean read fEnableClosedTabHistory write
        fEnableClosedTabHistory;
    property Settings: TCETabSettings read fSettings write fSettings;
  end;


implementation

uses
  Main, dCE_Actions, fCE_FileView, CE_LanguageEngine, dCE_Images, Math;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCESpTabItem.Destroy;
var
  tabSet: TCESpTabSet;
begin
  tabSet:= GetTabSet;
  if assigned(tabSet) then
  tabSet.fActiveTabHistory.Remove(Self);
  
  if assigned(fPage) then
  FreeAndNil(fPage);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Tab Closing
-------------------------------------------------------------------------------}
procedure TCESpTabItem.DoTabClosing(var Allow, CloseAndFree: Boolean);
var
  c: TComponent;
begin
  c:= GetParentComponent;
  if c is TCESpTabToolbar then
  Allow:= TCESpTabToolbar(c).CanTabClose;

  if Allow then
  begin
    if assigned(fPage) then
    begin
      Allow:= fPage.TabClosing;
    end
    else
    Allow:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCESpTabItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result:= TCESpTabItemViewer;
end;

{-------------------------------------------------------------------------------
  Get PageVisibility
-------------------------------------------------------------------------------}
function TCESpTabItem.GetPageVisibility: Boolean;
begin
  if assigned(fPage) then
  Result:= fPage.Visible
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Set PageVisibility
-------------------------------------------------------------------------------}
procedure TCESpTabItem.SetPageVisibility(const Value: Boolean);
begin
  if assigned(fPage) then
  begin
    if Value then
    fPage.Show
    else
    fPage.HidePage;
  end;
end;

{-------------------------------------------------------------------------------
  Close Tab
-------------------------------------------------------------------------------}
function TCESpTabItem.CloseTab: Boolean;
var
  b: Boolean;
  tabSet: TCESpTabSet;
  item: TCEClosedTabHistoryItem;
begin
  if Visible then
  begin
    Result:= True;
    item:= nil;
    
    // Add Closed Tab History Item
    tabSet:= GetTabSet;
    if assigned(tabSet) then
    begin
      item:= tabset.AddToClosedTabHistory(Self);
    end;

    DoTabClosing(Result, b);
    if Result then
    begin
      // Select previously selected tab
      if Self.Checked then
      begin
        if assigned(tabSet) then
        begin
          tabSet.SelectPrevSelectedTab;
        end;
      end;

      Visible:= False;
      DoTabClose;
    end
    else // Closed Tab History Item (tab could not close)
    begin
      if assigned(tabSet) and assigned(item) then
      tabSet.fClosedTabHistory.Remove(item);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get TabSet
-------------------------------------------------------------------------------}
function TCESpTabItem.GetTabSet: TCESpTabSet;
var
  T: TSpTBXTabToolbar;
begin
  Result:= nil;
  if GetTabToolbar(T) then
  begin
    if T is TCESpTabToolbar then
    begin
      if TCESpTabToolbar(T).FOwnerTabControl is TCESpTabSet then
      begin
        Result:= TCESpTabSet(TCESpTabToolbar(T).FOwnerTabControl);
      end;
    end;
  end;  
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCESpTabSet.Create(AOwner: TComponent);
begin
  inherited;
  fSettings:= TCETabSettings.Create;
  fSettings.TabSet:= Self;
  Self.TabCloseButton:= tcbAll;
  Toolbar.OnMouseDown:= HandleMouseDown;
  Toolbar.OnMouseUp:= HandleMouseUp;
  Self.OnDrawBackground:= HandleDrawBackground;
  TCESpTabToolbar(Toolbar).OnMouseWheel:= HandleMouseWheel;
  fDropTimer:= TTimer.Create(self);
  fDropTimer.Interval:= 1000;
  fDropTimer.Enabled:= false;
  fDropTimer.OnTimer:= HandleDropTimer;
  GlobalAppSettings.AddItem('Tabs', Settings, true);
  Toolbar.OnResize:= HandleToolbarResize;
  Toolbar.ShrinkMode:= tbsmNone;
  fActiveTabHistory:= TObjectList.Create(false);
  fClosedTabHistory:= TObjectList.Create(true);
  EnableClosedTabHistory:= true;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCESpTabSet.Destroy;
begin
  EnableClosedTabHistory:= false;
  fClosedTabHistory.Free;
  
  fSettings.Free;
  fActiveTabHistory.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Toolbar Class
-------------------------------------------------------------------------------}
function TCESpTabSet.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result:= TCESpTabToolbar;
end;

{-------------------------------------------------------------------------------
  Add Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.AddTab(TabPageClass: TCECustomTabPageClass; SelectTab:
    Boolean = false; ActivatePage: Boolean = true; ATabIndex: Integer = -1):
    TCESpTabItem;
var
  page: TCECustomTabPage;
  i: Integer;
begin
  Result:= nil;
  if not assigned(TabPageClass) then
  Exit;
  if not assigned(fTabPageHost) then
  Exit;

  Result:= TCESpTabItem.Create(nil);
  page:= TabPageClass.Create(nil);
  TCECustomTabPageAccess(page).fTabItem:= Result;
  Result.fPage:= page;
  page.Visible:= false;
  page.Parent:= fTabPageHost;
  page.Align:= alClient;
  page.BoundsRect:= fTabPageHost.ClientRect;
  if ActivatePage then
  page.UpdateCaption;
  page.Active:= ActivatePage;

  if ATabIndex > -1 then
  begin
    if ATabIndex < Items.Count then
    Items.Insert(ATabIndex, Result)
    else
    Items.Add(Result);
  end
  else if Settings.OpenNextToCurrent then
  begin
    i:= Items.IndexOf(ActiveTab);
    if i <> -1 then
    begin
      if i+1 < Items.Count then
      Items.Insert(i+1, Result)
      else
      Items.Add(Result);
    end
    else
    Items.Add(Result);
  end
  else
  Items.Add(Result);

  if SelectTab then
  begin
    Result.Checked:= true;
    DoActiveTabChange(Items.IndexOf(Result));
  end;
end;

{-------------------------------------------------------------------------------
  Add To ClosedTabHistory
-------------------------------------------------------------------------------}
function TCESpTabSet.AddToClosedTabHistory(ATab: TCESpTabItem):
    TCEClosedTabHistoryItem;
begin
  Result:= nil;

  if not EnableClosedTabHistory or not Settings.ClosedTabHistory then
  Exit;

  if assigned(ATab) and assigned(ATab.Page) then
  begin
    // Add history item
    Result:= TCEClosedTabHistoryItem.Create;
    fClosedTabHistory.Insert(0, Result);
    Result.TabPageClass:= TCECustomTabPageClass(ATab.Page.ClassType);
    Result.TabIndex:= Self.Toolbar.Items.IndexOf(ATab);
    Result.TabCaption:= ATab.Caption;
    Result.TabImageIndex:= ATab.ImageIndex;
    Result.TabImageList:= ATab.Images;
    // Save Page settings
    ATab.Page.SaveToStream(Result.TabSettings);

    // Remove old items (keep 10 items)
    while fClosedTabHistory.Count > Settings.UndoCount do
    fClosedTabHistory.Delete(fClosedTabHistory.Count-1);
  end;
end;

{-------------------------------------------------------------------------------
  Can Active Tab Change
-------------------------------------------------------------------------------}
function TCESpTabSet.CanActiveTabChange(const TabIndex, NewTabIndex: Integer):
    Boolean;
begin
  Result:= inherited CanActiveTabChange(TabIndex, NewTabIndex);
end;

{-------------------------------------------------------------------------------
  Can Undo Tab Close
-------------------------------------------------------------------------------}
function TCESpTabSet.CanUndoTabClose: Boolean;
begin
  Result:= EnableClosedTabHistory and Settings.ClosedTabHistory and (fClosedTabHistory.Count > 0);
end;

{-------------------------------------------------------------------------------
  Close All Tabs
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseAllTabs(ExceptThis: TCESpTabItem = nil; Force:
    Boolean = false): Boolean;
var
  i: Integer;
  item: TCESpTabItem;
begin
  Result:= false;
  i:= 0;
  Self.ActiveTabIndex:= -1;
  Self.fActiveTab:= nil;
  Self.Toolbar.BeginUpdate;
  try
    while i < Items.Count do
    begin
      if Items.Items[i] is TCESpTabItem then
      begin
        item:= TCESpTabItem(Items.Items[i]);
        if item <> ExceptThis then
        begin
          if Force then
          begin
            item.Free;
          end
          else
          begin
            if assigned(item.Page) then
            begin
              if item.Page.TabClosing then
              begin
                item.Free
              end
              else
              Exit;
            end
            else
            item.Free;
          end;
        end
        else
        i:= i + 1;
      end
      else
      i:= i + 1;
    end;
    Result:= true;
  finally
    Self.Toolbar.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Close SelectedTab
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseSelectedTab(Force: Boolean = false): Boolean;
begin
  Result:= CloseTab(GetActiveTab,Force);
end;

{-------------------------------------------------------------------------------
  Close Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTab(ATab: TCESpTabItem; Force: Boolean = false):
    Boolean;
begin
  if assigned(ATab) then
  begin
    Result:= ATab.CloseTab;

    if Result or Force then
    ATab.Free;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Close tabs on left
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTabsOnLeft(ATab: TCESpTabItem; Force: Boolean =
    false): Boolean;
var
  i: Integer;
begin
  if assigned(ATab) then
  begin
    Result:= Items.IndexOf(ATab) > 0;
    if Result then
    begin
      i:= 0;
      while i < Items.Count do
      begin
        if Items.Items[i] is TCESpTabItem then
        begin
          if Items.Items[i] <> ATab then
          begin
            if Force then
            begin
              Items.Items[i].Free;
            end
            else
            begin
              if TCESpTabItem(Items.Items[i]).Page.TabClosing then
              Items.Items[i].Free
              else
              Exit;
            end;
          end
          else
          break;
        end
        else
        i:= i + 1;
      end;
    end;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Close tabs on right
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTabsOnRight(ATab: TCESpTabItem; Force: Boolean =
    false): Boolean;
var
  i: Integer;
begin
  if assigned(ATab) then
  begin
    i:= Items.IndexOf(ATab) + 1;
    Result:= (i > 0) and (i < Items.Count);
    if Result then
    begin
      while i < Items.Count do
      begin
        if Items.Items[i] is TCESpTabItem then
        begin
          if Items.Items[i] <> ATab then
          begin
            if Force then
            begin
              Items.Items[i].Free;
            end
            else
            begin
              if TCESpTabItem(Items.Items[i]).Page.TabClosing then
              Items.Items[i].Free
              else
              Exit;
            end;
          end
          else
          break;
        end
        else
        i:= i + 1;
      end;
    end;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Create Handle
-------------------------------------------------------------------------------}
procedure TCESpTabSet.CreateHandle;
begin
  inherited;
  RegisterDragDrop(WindowHandle, self);
end;

{-------------------------------------------------------------------------------
  Destroy Handle
-------------------------------------------------------------------------------}
procedure TCESpTabSet.DestroyHandle;
begin
  RevokeDragDrop(WindowHandle);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Active Tab Change
-------------------------------------------------------------------------------}
procedure TCESpTabSet.DoActiveTabChange(const ItemIndex: Integer);
var
  i: Integer;
  tab: TCESpTabItem;
begin
  inherited;
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
  Exit;

  if Items.Items[ItemIndex] is TCESpTabItem then
  tab:= TCESpTabItem(Items.Items[ItemIndex])
  else
  tab:= nil;

  if assigned(tab) then
  begin
    tab.PageVisibility:= true;
    if assigned(tab.Page) then
    begin
      if CompareText(tab.Page.Layout, LayoutController.CurrentLayout) <> 0 then
      begin
        // Save old layout
        if assigned(ActiveTab) then
        begin
          if assigned(ActiveTab.Page) then
          begin
            if ActiveTab.Page.Layout <> tab.Page.Layout then
            LayoutController.SaveLayout(ActiveTab.Page.Layout,
                                        ActiveTab.Page.Settings.RememberInnerToolbarLayout,
                                        ActiveTab.Page.Settings.RememberOuterToolbarLayout,
                                        ActiveTab.Page.Settings.RememberPanelLayout);
          end;
        end;
        // Load new layout
        LayoutController.LoadLayout(tab.page.Layout,
                                    tab.Page.Settings.RememberInnerToolbarLayout,
                                    tab.Page.Settings.RememberOuterToolbarLayout,
                                    tab.Page.Settings.RememberPanelLayout);
        LayoutController.CurrentLayout:= tab.page.Layout;
      end;
      tab.Page.SelectPage;
      TCESpTabToolbar(Toolbar).ScrollToView(tab);
    end;

    // Maintain ActiveTabHistory
    i:= fActiveTabHistory.IndexOf(fActiveTab);
    if i > -1 then
    fActiveTabHistory.Move(i, 0)
    else
    fActiveTabHistory.Insert(0, fActiveTab);

    fActiveTab:= tab;
  end;

  // Hide everything else
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      if i <> ItemIndex then
      TCESpTabItem(Items.Items[i]).PageVisibility:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DragEnter
-------------------------------------------------------------------------------}
function TCESpTabSet.DragEnter(const dataObj: IDataObject; grfKeyState:
    Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  p: TPoint;
begin
  Result:= S_OK;
  fDataObj:= dataObj;

  p:= Self.ScreenToClient(pt);
  fDropTab:= GetTabAt(p.X, p.Y);
  dwEffect:= DROPEFFECT_NONE;
  if assigned(fDropTab) then
  begin
    if Settings.AllowDropToTab then
    begin
      // Send DragEnter to new drop tab
      if assigned(fDropTab) and assigned(fDropTab.Page) then
      TCECustomTabPageAccess(fDropTab.Page).DragEnter(dataObj, grfKeyState, pt, dwEffect);
    end;

    if Settings.SwitchTabOnDragHover then
    fDropTimer.Enabled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  DragLeave
-------------------------------------------------------------------------------}
function TCESpTabSet.DragLeave: HResult;
begin
  fDropTimer.Enabled:= false;
  Result:= S_OK;
  if Settings.AllowDropToTab then
  begin
    // Send DragLeave to drop tab
    if assigned(fDropTab) and assigned(fDropTab.Page) then
    TCECustomTabPageAccess(fDropTab.Page).DragLeave;
  end;
  fDropTab:= nil;
  fDataObj:= nil;
end;

{-------------------------------------------------------------------------------
  DragOver
-------------------------------------------------------------------------------}
function TCESpTabSet.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect:
    Longint): HResult;
var
  p: TPoint;
  tab: TCESpTabItem;
begin
  Result:= S_OK;
  p:= Self.ScreenToClient(pt);

  tab:= GetTabAt(p.X, p.Y);
  if tab <> fDropTab then
  begin
    fDropTimer.Enabled:= false;
    if Settings.AllowDropToTab then
    begin
      // Send DragLeave to previous drop tab
      if assigned(fDropTab) and assigned(fDropTab.Page) then
      TCECustomTabPageAccess(fDropTab.Page).DragLeave;
      // Send DragEnter to new drop tab
      if assigned(tab) and assigned(tab.Page) then
      TCECustomTabPageAccess(tab.Page).DragEnter(fDataObj, grfKeyState, pt, dwEffect);
    end;
    fDropTab:= tab;
  end;



  if fDropTab <> nil then
  begin
    if Settings.AllowDropToTab then
    begin
      // Send DragOver to new drop tab
      if assigned(fDropTab) and assigned(fDropTab.Page) then
      Result:= TCECustomTabPageAccess(fDropTab.Page).DragOver(grfKeyState, pt, dwEffect);
    end
    else
    begin
      dwEffect:= DROPEFFECT_NONE;
    end;

    if Settings.SwitchTabOnDragHover then
    fDropTimer.Enabled:= true;
  end
  else
  begin
    dwEffect:= DROPEFFECT_NONE;
    fDropTimer.Enabled:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Drop
-------------------------------------------------------------------------------}
function TCESpTabSet.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt:
    TPoint; var dwEffect: Longint): HResult;
begin
  Result:= S_OK;
  fDropTimer.Enabled:= false;
  if Settings.AllowDropToTab then
  begin
    // Send Drop to drop tab
    if assigned(fDropTab) and assigned(fDropTab.Page) then
    Result:= TCECustomTabPageAccess(fDropTab.Page).Drop(dataObj, grfKeyState, pt, dwEffect);
  end;
  fDropTab:= nil;
end;

{-------------------------------------------------------------------------------
  Get Active Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetActiveTab: TCESpTabItem;
begin
  Result:= nil;
  if (ActiveTabIndex > -1) and (ActiveTabIndex < Items.Count) then
  begin
    if Items.Items[ActiveTabIndex] is TCESpTabItem then
    Result:= TCESpTabItem(Items.Items[ActiveTabIndex]);
  end;
end;

{-------------------------------------------------------------------------------
  Get CEActiveTabIndex
-------------------------------------------------------------------------------}
function TCESpTabSet.GetCEActiveTabIndex: Integer;
var
  i,i2: Integer;
begin
  Result:= -1;
  i2:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      i2:= i2 + 1;
      if i = ActiveTabIndex then
      begin
        Result:= i2;
        Break;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set CEActiveTabIndex
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SetCEActiveTabIndex(const Value: Integer);
var
  i,i2: Integer;
begin
  i2:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      i2:= i2 + 1;
      if i2 = Value then
      begin
        ActiveTabIndex:= i;
        Break;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get First tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetFirstTab: TCESpTabItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= TCESpTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Last tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetLastTab: TCESpTabItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= Items.Count - 1 downto 0 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= TCESpTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get next tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetNextTab(From: TSpTBXTabItem): TCESpTabItem;
var
  i: Integer;
begin
  Result:= nil;
  i:= Items.IndexOf(From) + 1;
  while i < Items.Count do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= TCESpTabItem(Items.Items[i]);
      break;
    end;
    i:= i + 1;
  end;
end;

{-------------------------------------------------------------------------------
  Get previous tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetPrevTab(From: TSpTBXTabItem): TCESpTabItem;
var
  i: Integer;
begin
  Result:= nil;
  i:= Items.IndexOf(From) - 1;
  while i > -1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= TCESpTabItem(Items.Items[i]);
      break;
    end;
    i:= i - 1;
  end;
end;

{-------------------------------------------------------------------------------
  GetTabAt
-------------------------------------------------------------------------------}
function TCESpTabSet.GetTabAt(X, Y: Integer): TCESpTabItem;
begin
  Result:= TCESpTabToolbar(Toolbar).GetTabAt(X, Y);
end;

{-------------------------------------------------------------------------------
  Get TabCount
-------------------------------------------------------------------------------}
function TCESpTabSet.GetTabCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= Result + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle DrawBackground
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleDrawBackground(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault:
    Boolean);
begin
  if PaintStage = pstPrePaint then
  begin
    ARect.Left:= ARect.Left - 2;
    ARect.Right:= ARect.Right + 3;
    SpDrawXPTabControlBackground(FBackground.Canvas, ARect, Color, TabPosition = ttpBottom, SkinType);
    PaintDefault:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Toolbar MouseDown
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssMiddle] then
  fClosingTab:= GetTabAt(X, Y)
  else
  fClosingTab:= nil;
end;

{-------------------------------------------------------------------------------
  Handle Toolbar MouseUp
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  tab: TCESpTabItem;
  p: TPoint;
  IV: TTBItemViewer;
begin
  if assigned(fClosingTab) then
  begin
    if Button = mbMiddle then
    begin
      tab:= GetTabAt(X, Y);
      if tab = fClosingTab then
      begin
        if tab.CloseTab then
        tab.Free;
      end;
    end;
    fClosingTab:= nil;
  end;

  if Shift = [ssLeft,ssDouble] then
  begin
    fActivePopupTab:= GetTabAt(X, Y);
    if assigned(fActivePopupTab) then
    begin
      if Settings.DblClickCloseTab then
      CEActions.act_tabs_closetab.Execute
      else if fActivePopupTab.Page is TCEFileViewPage then
      CEActions.act_tabs_duplicatetab.Execute
      else
      CEActions.act_tabs_addtab.Execute;
    end
    else
    begin
      IV:= Self.Toolbar.View.ViewerFromPoint(Point(X, Y));
      if (assigned(IV) and (IV.Item is TCEToolbarDynamicSpacerItem)) or not assigned(IV) then
      CEActions.act_tabs_addtab.Execute;
    end;
    fActivePopupTab:= nil;
  end
  else if Button = mbRight then
  begin
    fActivePopupTab:= GetTabAt(X, Y);
    if not assigned(fActivePopupTab) then
    fActivePopupTab:= Self.GetActiveTab;

    if assigned(fActivePopupTab) and Assigned(fTabPopupMenu) then
    begin
      p:= Self.ClientToScreen(Point(X, Y));
      fTabPopupMenu.Popup(p.X, p.Y);
      Application.ProcessMessages;
      fActivePopupTab:= nil;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Wheel
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
  begin
    SelectNextTab;
    Handled:= true;
  end
  else if WheelDelta < 0 then
  begin
    SelectNextTab(false);
    Handled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  On DropTimer
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleDropTimer(Sender: TObject);
begin
  fDropTimer.Enabled:= false;
  if assigned(fDropTab) then
  begin
    SelectTab(fDropTab);
    fDropTab:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  On Toolbar Resize
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleToolbarResize(Sender: TObject);
begin
  if SkinManager.GetSkinType = sknSkin then
  Self.Height:= Toolbar.Height + 2
  else
  Self.Height:= Toolbar.Height + 4
end;

{-------------------------------------------------------------------------------
  Handle Undo Item Click
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleUndoItemClick(Sender: TObject);
begin
  UndoTabClose(TSpTBXItem(Sender).Tag);
end;

{-------------------------------------------------------------------------------
  Populate Undo List (Returns number of items added)
-------------------------------------------------------------------------------}
function TCESpTabSet.PopulateUndoList(ASubMenu: TTBCustomItem): Integer;
var
  i: Integer;
  historyItem: TCEClosedTabHistoryItem;
  item: TSpTBXItem;
begin
  Result:= 0;
  if assigned(ASubMenu) then
  begin
    ASubMenu.Clear;
    for i:= 0 to fClosedTabHistory.Count - 1 do
    begin
      historyItem:= TCEClosedTabHistoryItem(fClosedTabHistory.Items[i]);
      item:= TSpTBXItem.Create(ASubMenu);
      item.Caption:= historyItem.TabCaption;
      item.ImageIndex:= historyItem.TabImageIndex;
      item.Images:= historyItem.TabImageList;
      item.Tag:= i;
      item.OnClick:= HandleUndoItemClick;
      ASubMenu.Add(item);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Select Next Tab
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SelectNextTab(GoForward: Boolean = true);
var
  tab: TSpTBXTabItem;
begin
  tab:= GetActiveTab;
  if assigned(tab) then
  begin
    if GoForward then
    tab:= GetNextTab(tab)
    else
    tab:= GetPrevTab(tab);
    
    if not assigned(tab) then
    begin
      if GoForward then
      tab:= GetFirstTab
      else
      tab:= GetLastTab;
    end;

    if assigned(tab) then
    ActiveTabIndex:= Items.IndexOf(tab);
  end;
end;

{-------------------------------------------------------------------------------
  Select Previously Selected Tab
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SelectPrevSelectedTab;
var
  i, index: Integer;
  tab: TCESpTabItem;
begin
  for i:= 0 to fActiveTabHistory.Count - 1 do
  begin
    tab:= TCESpTabItem(fActiveTabHistory.Items[i]);
    if ActiveTab <> tab then
    begin
      index:= Items.IndexOf(tab);
      if index <> -1 then
      ActiveTabIndex:= index;
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Select Tab
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SelectTab(ATab: TSpTBXTabItem);
var
  i: Integer;
begin
  i:= Items.IndexOf(ATab);
  if i <> -1 then
  ActiveTabIndex:= i;
end;

{-------------------------------------------------------------------------------
  Get CEAutoFit
-------------------------------------------------------------------------------}
function TCESpTabSet.GetCEAutoFit: Boolean;
begin
  Result:= TCESpTabToolbar(Toolbar).CEAutoFit;
end;

{-------------------------------------------------------------------------------
  Set CEAutoFit
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SetCEAutoFit(const Value: Boolean);
begin
  TCESpTabToolbar(Toolbar).CEAutoFit:= Value;
end;

{-------------------------------------------------------------------------------
  Tab Deleting
-------------------------------------------------------------------------------}
procedure TCESpTabSet.TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean =
    True);
begin
  // Do Nothing
end;

{-------------------------------------------------------------------------------
  Tab Inserted
-------------------------------------------------------------------------------}
procedure TCESpTabSet.TabInserted(Item: TSpTBXTabItem);
begin
  // Do Nothing
end;

{-------------------------------------------------------------------------------
  Undo Tab Close
-------------------------------------------------------------------------------}
procedure TCESpTabSet.UndoTabClose(AIndex: Integer = 0);
var
  item: TCEClosedTabHistoryItem;
  tab: TCESpTabItem;
begin
  if (fClosedTabHistory.Count > AIndex) and (AIndex > -1) then
  begin
    // Open tab
    item:= TCEClosedTabHistoryItem(fClosedTabHistory.Items[AIndex]);
    tab:= AddTab(item.TabPageClass, true, true, item.TabIndex);
    item.TabSettings.Position:= 0;
    tab.Page.LoadFromStream(item.TabSettings);
    // Remove tab from history
    fClosedTabHistory.Delete(AIndex);
  end;
end;

{-------------------------------------------------------------------------------
  WM_SpSkinChange
-------------------------------------------------------------------------------}
procedure TCESpTabSet.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  HandleToolbarResize(self);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCESpTabToolbar.Create(AOwner: TComponent);
begin
  inherited;
  fPreventLastTabClosing:= false;
  Self.ShrinkMode:= tbsmNone;
  LeftArrow:= TCEScrollArrowItem.Create(Self);
  LeftArrow.Caption:= '<';
  LeftArrow.Hint:= 'Show more tabs';
  LeftArrow.Visible:= false;
  LeftArrow.OnClick:= HandleArrowClick;
  LeftArrow.ArrowDirection:= adLeft;
  Items.Add(LeftArrow);

  RightArrow:= TCEScrollArrowItem.Create(Self);
  RightArrow.Caption:= '>';
  RightArrow.Hint:= 'Show more tabs';
  RightArrow.Visible:= false;
  RightArrow.OnClick:= HandleArrowClick;
  RightArrow.ArrowDirection:= adRight;
  Items.Add(RightArrow);

  fCEAutoFit:= false;
end;

{-------------------------------------------------------------------------------
  Arrange Tabs
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.ArrangeTabs;
var
  i, prevIndex, index: Integer;
  tabs: TObjectList;
  prevItem, item: TTBCustomItem;
begin
  tabs:= TObjectList.Create(false);
  Self.BeginUpdate;
  try
    // Find tabs
    for i:= 0 to Items.Count - 1 do
    begin
      if Items.Items[i] is TSpTBXTabItem then
      tabs.Add(Items.Items[i]);
    end;
    // Arrange tabs
    prevItem:= nil;
    for i:= 0 to tabs.Count - 1 do
    begin
      item:= TTBCustomItem(tabs.Items[i]);
      if assigned(prevItem) then
      begin
        index:= Items.IndexOf(item);
        prevIndex:= Items.IndexOf(prevItem);
        if index > (prevIndex+1) then
        Items.Move(index, prevIndex+1);
      end;
      prevItem:= item;
    end;
  finally
    Self.EndUpdate;
    tabs.Free;
  end;
end;

{-------------------------------------------------------------------------------
  CanDragCustomize
-------------------------------------------------------------------------------}
function TCESpTabToolbar.CanDragCustomize(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
  TabIV: TCESpTabItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and (Button = mbLeft) then
  begin
    IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
    if Assigned(IV) and (IV is TCESpTabItemViewer) and Assigned(IV.Item) and IV.Item.Enabled and IV.Item.Visible then
    begin
      // Close the tab if the close button is pressed
      TabIV := TCESpTabItemViewer(IV);
      if (TabIV.TabCloseButtonState = sknsHotTrack) and TabIV.IsTabCloseButtonVisible then
      begin
        if TCESpTabItem(TabIV.Item).CloseTab then
        TabIV.Item.Free;
      end
      else
      begin
        // Click the item on mouse down
        if not IV.Item.Checked then
        begin
          Result := True; // Bypass the inherited mouse down
          IV.Item.Click;
          if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
        end
        // Drag reorder
        else if TabDragReorder and not IsCustomizing and IV.Item.Checked then
        begin
          Result:= True; // Bypass the inherited mouse down
          FBeginDragIV := IV;
          BeginDrag(False, 2);
        end;
      end;
    end
    else
      Result := inherited CanDragCustomize(Button, Shift, X, Y);
  end;
end;

{-------------------------------------------------------------------------------
  CanTabClose
-------------------------------------------------------------------------------}
function TCESpTabToolbar.CanTabClose: Boolean;
begin
  Result:= not ((GetTabCount = 1) and PreventLastTabClosing);
end;

{-------------------------------------------------------------------------------
  Do Autofit
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.DoAutofit;
var
  i: Integer;
  fullWidth, overWidth, itemsWidth: Integer;
  tabSize, dynamicSize: Integer;
  IV: TTBItemViewer;
  tabs, spacers: TList;
  spacer: TCEToolbarDynamicSpacerItem;
begin
  if not fCEAutofit or IsUpdating or (Items.Count = 0) or fAutofitting then Exit;
  
  tabs:= TList.Create;
  spacers:= TList.Create;
  View.ValidatePositions;
  View.BeginUpdate;
  fAutofitting:= true;
  try
    fullWidth:= CurrentDock.Width + 4;
    itemsWidth:= 8;
    // Calculate width of visible items
    for i:= 0 to View.ViewerCount - 1 do
    begin
      IV:= View.Viewers[i];
      if IV.Item is TCEToolbarDynamicSpacerItem then
      begin
        spacers.Add(Pointer(IV.Item));
      end
      else if IV.Item.Visible then
      begin
        if IV.Item is TSpTBXTabItem then
        tabs.Add(Pointer(IV.Item))
        else
        itemsWidth:= itemsWidth + (IV.BoundsRect.Right - IV.BoundsRect.Left);
      end;
    end;

    // Resize tabs
    overWidth:= fullWidth - itemsWidth;
    if tabs.Count > 0 then
    begin
      tabSize:= Round(overWidth / tabs.Count);
      if tabSize > TabAutofitMaxSize then
      tabSize:= TabAutofitMaxSize;

      if tabSize < 1 then
      tabSize:= 1;

      for i:= 0 to tabs.Count - 1 do
      begin
        TSpTBXTabItem(tabs.Items[i]).CustomWidth:= tabSize;
        overWidth:= overWidth - tabSize;
      end;
    end;

    // Calculate size for spacers
    if (overWidth > 0) and (spacers.Count > 0) then
    dynamicSize:= Floor(overWidth / spacers.Count)
    else
    dynamicSize:= 0;
    // Set spacer sizes
    for i:= 0 to spacers.Count - 1 do
    begin
      spacer:= TCEToolbarDynamicSpacerItem(spacers.Items[i]);
      if i < spacers.Count-1 then
      spacer.CustomWidth:= dynamicSize
      else
      spacer.CustomWidth:= overWidth;
      overWidth:= overWidth - dynamicSize;
    end;
  finally
    View.EndUpdate;
    tabs.Free;
    spacers.Free;
    fAutofitting:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Do ItemNotification
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.DoItemNotification(Ancestor: TTBCustomItem; Relayed:
    Boolean; Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
var
  I: Integer;
begin
  if (csDestroying in ComponentState) or (csReading in ComponentState) then Exit;

  if not (tstResizing in FState) and not IsItemMoving then
  begin
    if Assigned(OnItemNotification) then
    OnItemNotification(Self, Ancestor, Relayed, Action, Index, Item);

    case Action of
      tbicInserted:
        begin
          if Item is TSpTBXTabItem then
          begin
            if Index < Items.IndexOf(LeftArrow) then
            begin
              BeginItemMove;
              Items.Move(Index, Items.IndexOf(LeftArrow));
              EndItemMove;
            end
            else if Index > Items.IndexOf(RightArrow) then
            begin
              BeginItemMove;
              Items.Move(Index, Items.IndexOf(RightArrow));
              EndItemMove;
            end;
          end
          else
          begin
            if (Index > Items.IndexOf(LeftArrow)) and (Index < Items.IndexOf(RightArrow))  then
            begin
              BeginItemMove;
              Items.Move(Index, Items.IndexOf(LeftArrow));
              EndItemMove;
            end;
          end;
          RightAlignItems;
          AnchorItems(True);
        end;
      tbicDeleting:
        begin
          I := FAnchoredControlItems.IndexOf(Item);
          if I > -1 then
            FAnchoredControlItems.Delete(I);
          if Item = lastTab then
          begin
            lastTab:= GetPrevTab(Item);
            if not assigned(lastTab) then
            lastTab:= GetNextTab(Item);
          end;
          if Item = firstTab then
          begin
            firstTab:= GetNextTab(Item);
            if not assigned(firstTab) then
            firstTab:= GetPrevTab(Item);
          end;
          RightAlignItems;
          AnchorItems(True);
          // Exit app or open new tab if last tab has closed.
          if not Self.IsUpdating and not MainForm.CEIsClosing and (GetTabsCount(true) = 0) then
          begin
            if MainForm.Settings.ExitOnLastTabClose then
            PostMessage(MainForm.Handle, WM_Close, 0, 0)
            else
            CEActions.act_tabs_addtab.Execute;
          end;
        end;
      tbicInvalidateAndResize:
        begin
          RightAlignItems;
        end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DragOver
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.DragOver(Source: TObject; X, Y: Integer; State:
    TDragState; var Accept: Boolean);
var
  D: TSpTBXTabItemDragObject;
  DestIV: TTBItemViewer;
  OrigItem: TTBCustomItem;
  OrigPos, DestPos: Integer;
  DropMark: TRect;
begin
  // Draw drop mark
  if IsCustomizing and Customizable then
  begin
    Accept:= True;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), DestIV, DestPos, DropMark);
    if not EqualRect(DropMark, FLastDropMark) then
    begin
      // Clear the last DropMark
      InvalidateRect(Handle, @FLastDropMark, True);
      // Draw the new DropMark
      SpDrawDropMark(Canvas, DropMark);
      FLastDropMark:= DropMark;
    end;
  end;  

  // Handle Tab dragging
  if TabDragReorder and Assigned(Source) and (Source is TSpTBXTabItemDragObject) then
  begin
    D:= TSpTBXTabItemDragObject(Source);
    OrigItem:= D.SouceItem;
    OrigPos:= OrigItem.Parent.IndexOf(OrigItem);

    // Move the dragging item in the toolbar
    if OrigItem.Parent = Items then
    begin
      Accept:= true;
      SpGetDropPosItemViewer(Items, View, Point(X, Y), OrigPos, DestIV, DestPos);

      if (OrigPos <> DestPos) and (DestPos > -1) and (DestPos < Items.Count) and (OrigItem <> DestIV.Item) and
        (DestPos > Items.IndexOf(LeftArrow)) and (DestPos < Items.IndexOf(RightArrow)) then
      begin
        if TSpTBXCustomTabSetAccess(FOwnerTabControl).CanActiveTabReorder(OrigPos, DestPos) then
        begin
          BeginUpdate;
          BeginItemMove;
          try
            // The item is the active tab, we need to update the ActiveTabIndex
            // Just set the internal value because the page didn't change
            FActiveTabIndex := DestPos;
            Items.Move(OrigPos, DestPos);
            TSpTBXCustomTabSetAccess(FOwnerTabControl).DoActiveTabReorder(DestPos);
          finally
            EndItemMove;
            EndUpdate;
            //InvalidateNC;
          end;
        end;
      end;
    end;
  end
  // Handle dragging of other items
  else
  begin
    OrigPos:= -1;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), OrigPos, DestIV, DestPos);
    Accept:= (DestPos <= Items.IndexOf(LeftArrow)) or (DestPos > Items.IndexOf(RightArrow));
  end;
end;

{-------------------------------------------------------------------------------
  GetNextTab
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetNextTab(ANextFrom: TTBCustomItem): TSpTBXTabItem;
var
  i: Integer;
  index: Integer;
begin
  Result:= nil;
  index:= Items.IndexOf(ANextFrom);
  for i:= index+1 to Items.Count - 1 do
  begin
    if Items.Items[i] is TSpTBXTabItem  then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  GetPrevTab
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetPrevTab(APrevFrom: TTBCustomItem): TSpTBXTabItem;
var
  i: Integer;
  index: Integer;
begin
  Result:= nil;
  index:= Items.IndexOf(APrevFrom);
  for i:= index-1 downto 0 do
  begin
    if Items.Items[i] is TSpTBXTabItem  then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Tab
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTab(Index: Integer): TCESpTabItem;
var
  i,c: Integer;
begin
  Result:= nil;
  if (Index > -1) and (Index < Items.Count) then
  Exit;

  c:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      c:= c + 1;
      if c = Index then
      begin
        Result:= TCESpTabItem(Items.Items[i]);
        Break;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  GetTabAt
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTabAt(X, Y: Integer): TCESpTabItem;
var
  viewer: TTBItemViewer;
begin
  Result:= nil;
  viewer:= View.ViewerFromPoint(Point(X,Y));
  if assigned(viewer) then
  begin
    if viewer.Item is TCESpTabItem then
    Result:= TCESpTabItem(viewer.Item);
  end;
end;

{-------------------------------------------------------------------------------
  Get TabCount
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTabCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= Result + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle ArrowClick
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.HandleArrowClick(Sender: TObject);
begin
  Scroll(Sender = RightArrow);
end;

{-------------------------------------------------------------------------------
  Internal Draw Background
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.InternalDrawBackground(ACanvas: TCanvas; ARect:
    TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True);
var
  B: TBitmap;
  R, BitmapR, DestR: TRect;
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
  T: TSpTBXSkinType;
  PrevDelta, NextDelta: Integer;
begin
  inherited;
  
  T := SpTBXSkinType(SkinType);
  if PaintOnNCArea and Assigned(FOwnerTabControl) then
  begin
    B:= TBitmap.Create;
    B.Canvas.Lock;
    try
      R:= ARect;
      R.Left:= R.Left - 2;
      R.Right:= R.Right + 4;
      B.Width:= (R.Right - R.Left);
      B.Height:= R.Bottom - R.Top;

      SpDrawXPToolbar(Self, B.Canvas, R, PaintOnNCArea, TabBackgroundBorders and (T <> sknNone), skncTabToolbar);

      // Draw the bottom border of the active tab
      Tab:= ActiveTab;
      if Assigned(Tab) and Tab.Visible then
      begin
        IV:= SpFindItemViewer(View, Tab);
        if Assigned(IV) then
        begin
          DestR:= IV.BoundsRect;
          OffsetRect(DestR, 2, 2);  // Add the toolbar margins
          TSpTBXTabItemViewerAccess(IV).DrawBottomBorder(B.Canvas, DestR);
        end;
        if T = sknWindows then
        begin
          if Tab.IsFirstVisibleTab or Assigned(Tab.GetNextTab(False, sivtInmediateSkipNonVisible)) then
            PrevDelta:= 1
          else
            PrevDelta:= -1;
          if Assigned(Tab.GetNextTab(True, sivtInmediateSkipNonVisible)) then
            NextDelta:= 1
          else
            NextDelta:= -1;
          if TabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Bottom - 2, DestR.Right + NextDelta, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Top + 2, DestR.Right + NextDelta, R.Top - 4);
        end
        else
          if TabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Bottom - 2, DestR.Right - 1, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Top + 2, DestR.Right -1 , R.Top - 4);
      end;

      // Draw the bottom border of the tabs pane
      BitmapR := Rect(0, 0, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Width, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Height);
      case TabPosition of
        ttpTop:
          begin
            DestR := Rect(R.Left, R.Bottom - 2, R.Right, R.Bottom);
            BitmapR.Bottom := BitmapR.Top + 2;
          end;
        ttpBottom:
          begin
            DestR := Rect(R.Left, R.Top, R.Right, R.Top + 2);
            BitmapR.Top := BitmapR.Bottom - 2;
          end;
      end;

      B.Canvas.CopyRect(DestR, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Canvas, BitmapR);
      ACanvas.Draw(0, 0, B);
    finally
      B.Canvas.UnLock;
      B.Free;
    end;
  end
  else
    SpDrawXPToolbar(Self, ACanvas, ARect, PaintOnNCArea, TabBackgroundBorders and (T <> sknNone), skncTabToolbar);
end;

{-------------------------------------------------------------------------------
  RightAlignItems
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.RightAlignItems;
var
  i, activeIndex: Integer;
  W,H: Integer;
  totalWidth, fullWidth, overWidth, tabsWidth, rightAlignSize: Integer;
  leftIndex, rightIndex: Integer;
  doRight: Boolean;
  IV: TTBItemViewer;
  tabs, spacers: TList;
  found: Boolean;
  someVisible: Boolean;
  dynamicSize: Integer;
  spacer: TCEToolbarDynamicSpacerItem;
begin
  if (csDestroying in ComponentState)
     or not Assigned(CurrentDock)
     or IsUpdating
     or (tstRightAligning in FState)
     or fAutofitting
     or (CurrentDock.Width <= 0)
     or (CurrentDock.Height <= 0)
     or (Items.Count <= 0)
     or not Stretch
     or (ShrinkMode <> tbsmNone)
     then Exit;

  if CEAutofit then
  begin
    DoAutofit;
    Exit;
  end;

  tabs:= TList.Create;
  spacers:= TList.Create;
  FState:= FState + [tstRightAligning];
  View.BeginUpdate;
  View.ValidatePositions;
  try
    fullWidth:= CurrentDock.Width + 4;
    LeftArrow.Enabled:= false;
    RightArrow.Enabled:= false; 
    //** Calculate total width and get tab list
    activeIndex:= 0;
    totalWidth:= 8;
    tabsWidth:= 0;
    someVisible:= false;
    for i:= 0 to View.ViewerCount - 1 do
    begin
      IV:= View.Viewers[i];
      if (IV.Item <> LeftArrow) and (IV.Item <> RightArrow) then
      begin
        if not (IV.Item is TCEToolbarDynamicSpacerItem) then
        begin
          if IV.Item.Visible then
          W:= IV.BoundsRect.Right - IV.BoundsRect.Left
          else
          TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
          totalWidth:= totalWidth + W;
          // Add tabs to list
          if IV.Item is TSpTBXTabItem then
          begin
            if (IV.Item = centerTab) or (not assigned(centerTab) and IV.Item.Checked) then
            activeIndex:= tabs.Add(IV)
            else
            tabs.Add(Pointer(IV));
            tabsWidth:= tabsWidth + W;
          end;
        end
        else
        spacers.Add(Pointer(IV.Item));
      end;
    end;
    //** Re-Calculate total width if arrows are visible
    if (totalWidth > fullWidth) and (tabs.Count > 1) then
    begin
      IV:= View.Find(RightArrow);
      if assigned(IV) then
      begin
        if IV.Item.Visible then
        W:= IV.BoundsRect.Right - IV.BoundsRect.Left
        else
        TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
        totalWidth:= totalWidth + W;
      end;
      IV:= View.Find(LeftArrow);
      if assigned(IV) then
      begin
        if IV.Item.Visible then
        W:= IV.BoundsRect.Right - IV.BoundsRect.Left
        else
        TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
        totalWidth:= totalWidth + W;
      end;
    end;

    //** Hide tabs
    if totalWidth > fullWidth then
    begin
      overWidth:= fullWidth - (totalWidth - tabsWidth);
      rightAlignSize:= 0;
      //// Hide left most tabs
      if assigned(lastTab) then
      begin
        found:= false;
        for i:= tabs.Count-1 downto 0 do
        begin
          IV:= TTBItemViewer(tabs.Items[i]);
          if not found then
          begin
            found:= IV.Item = lastTab;
            if found then
            RightArrow.Enabled:= i < tabs.Count-1;
          end;
          if (overWidth > 0) and found then
          begin
            if IV.Item.Visible then
            W:= IV.BoundsRect.Right - IV.BoundsRect.Left
            else
            TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
            overWidth:= overWidth - W;
            if overWidth >= 0 then
            IV.Item.Visible:= true
            else
            begin
              rightAlignSize:= overWidth + W;
              IV.Item.Visible:= false;
              LeftArrow.Enabled:= true;
            end;
          end
          else
          begin
            IV.Item.Visible:= false;
            if found then
            LeftArrow.Enabled:= true;
          end;
          if not someVisible then
          someVisible:= IV.Item.Visible;
        end;
      end
      //// Hide right most tabs
      else if assigned(firstTab) then
      begin
        found:= false;
        for i:= 0 to tabs.Count-1 do
        begin
          IV:= TTBItemViewer(tabs.Items[i]);
          if not found then
          begin
            found:= IV.Item = firstTab;
            if found then
            LeftArrow.Enabled:= i > 0;
          end;
          if (overWidth > 0) and found then
          begin
            if IV.Item.Visible then
            W:= IV.BoundsRect.Right - IV.BoundsRect.Left
            else
            TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
            overWidth:= overWidth - W;
            if overWidth >= 0 then
            IV.Item.Visible:= true
            else
            begin
              rightAlignSize:= overWidth + W;
              IV.Item.Visible:= false;
              RightArrow.Enabled:= true;
            end;
          end
          else
          begin
            IV.Item.Visible:= false;
            if found then
            RightArrow.Enabled:= true;
          end;
          if not someVisible then
          someVisible:= IV.Item.Visible;
        end;
      end
      //// Hide tabs around active tab
      else
      begin
        leftIndex:= activeIndex-1;
        rightIndex:= activeIndex;
        doRight:= true;
        i:= 0;
        while i < tabs.Count do
        begin
          // Hide right most tabs
          if doRight then
          begin
            if (rightIndex < tabs.Count) then
            begin
              IV:= TTBItemViewer(tabs.Items[rightIndex]);
              if overWidth > 0 then
              begin
                if IV.Item.Visible then
                W:= IV.BoundsRect.Right - IV.BoundsRect.Left
                else
                TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
                overWidth:= overWidth - W;
                if overWidth >= 0 then
                IV.Item.Visible:= true
                else
                begin
                  rightAlignSize:= overWidth + W;
                  IV.Item.Visible:= false;
                  RightArrow.Enabled:= true;
                end;
              end
              else
              begin
                IV.Item.Visible:= false;
                RightArrow.Enabled:= true;
              end;
              rightIndex:= rightIndex + 1;
              i:= i + 1;
              if not someVisible then
              someVisible:= IV.Item.Visible;
            end;
            doRight:= false;
          end
          // Hide left most tabs
          else
          begin
            if (leftIndex > -1) then
            begin
              IV:= TTBItemViewer(tabs.Items[leftIndex]);
              if overWidth > 0 then
              begin
                if IV.Item.Visible then
                W:= IV.BoundsRect.Right - IV.BoundsRect.Left
                else
                TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
                overWidth:= overWidth - W;
                if overWidth >= 0 then
                IV.Item.Visible:= true
                else
                begin
                  rightAlignSize:= overWidth + W;
                  IV.Item.Visible:= false;
                  LeftArrow.Enabled:= true;
                end;
              end
              else
              begin
                IV.Item.Visible:= false;
                LeftArrow.Enabled:= true;
              end;
              leftIndex:= leftIndex - 1;
              i:= i + 1;
              if not someVisible then
              someVisible:= IV.Item.Visible;
            end;
            doRight:= true;
          end;
        end;
      end;
      // Show at least one tab
      if not someVisible then
      begin
        if assigned(lastTab) then
        lastTab.Visible:= true
        else if assigned(firstTab) then
        firstTab.Visible:= true
        else if (activeIndex > 0) and (activeIndex < tabs.Count) then
        TTBItemViewer(tabs.Items[activeIndex]).Item.Visible:= true
        else if tabs.Count > 0 then
        TTBItemViewer(tabs.Items[0]).Item.Visible:= true;
      end;
    end
    //** Show all tabs
    else
    begin
      for i:= 0 to tabs.Count-1 do
      TTBItemViewer(tabs.Items[i]).Item.Visible:= true;
      firstTab:= nil;
      lastTab:= nil;
      LeftArrow.Enabled:= false;
      RightArrow.Enabled:= false;
      rightAlignSize:= fullWidth - totalWidth;
    end;

    // Show/Hide arrows
    LeftArrow.Visible:= LeftArrow.Enabled or RightArrow.Enabled;
    RightArrow.Visible:= LeftArrow.Visible;

    // Calculate size for spacers
    if (rightAlignSize > 0) and (spacers.Count > 0) then
    dynamicSize:= Floor(rightAlignSize / spacers.Count)
    else
    dynamicSize:= 0;

    if rightAlignSize < 0 then
    rightAlignSize:= 0;
    // Set spacer sizes
    for i:= 0 to spacers.Count - 1 do
    begin
      spacer:= TCEToolbarDynamicSpacerItem(spacers.Items[i]);
      if i < spacers.Count-1 then
      spacer.CustomWidth:= dynamicSize
      else
      spacer.CustomWidth:= rightAlignSize;
      rightAlignSize:= rightAlignSize - dynamicSize;
    end;
  finally
    Self.InvalidateNC;
    View.EndUpdate;
    FState:= FState - [tstRightAligning];
    tabs.Free;
    spacers.Free;   
  end;
end;

{-------------------------------------------------------------------------------
  Scroll
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.Scroll(ToRight: Boolean = true);
var
  i, lastI: Integer;
  item: TSpTBXTabItem;
begin
  lastI:= 0;
  firstTab:= nil;
  lastTab:= nil;
  if ToRight then
  begin
    for i:= Items.Count - 1 downto 0 do
    begin
      if Items.Items[i] is TSpTBXTabItem then
      begin
        item:= TSpTBXTabItem(Items.Items[i]);
        if item.Visible then
        begin
          if lastI > i then
          lastTab:= TSpTBXTabItem(Items.Items[lastI])
          else
          lastTab:= TSpTBXTabItem(Items.Items[i]);
          break;
        end
        else
        lastI:= i;
      end;
    end;
  end
  else
  begin
    lastI:= Items.Count-1;
    for i:= 0 to Items.Count - 1 do
    begin
      if Items.Items[i] is TSpTBXTabItem then
      begin
        item:= TSpTBXTabItem(Items.Items[i]);
        if item.Visible then
        begin
          if lastI < i then
          firstTab:= TSpTBXTabItem(Items.Items[lastI])
          else
          firstTab:= TSpTBXTabItem(Items.Items[i]);
          break;
        end
        else
        lastI:= i;
      end;
    end;
  end;
  RightAlignItems;
end;

{-------------------------------------------------------------------------------
  ScrollToView
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.ScrollToView(AItem: TSpTBXTabItem; ACenter: Boolean =
    false);
var
  i: Integer;
  l, r, index: Integer;
  item: TSpTBXTabItem;
begin
  if assigned(AItem) then
  begin
    if AItem.Visible then
    Exit; // Nothing to do

    if ACenter then
    begin
      centerTab:= AItem;
      lastTab:= nil;
      firstTab:= nil;
      RightAlignItems;
    end
    else
    begin
      centerTab:= nil;
      l:= -1;
      r:= -1;
      index:= -1;
      for i:= 0 to Items.Count - 1 do
      begin
        if Items.Items[i] is TSpTBXTabItem then
        begin
          item:= TSpTBXTabItem(Items.Items[i]);
          if (l = -1) and item.Visible then
          l:= i;
          if item.Visible then
          r:= i;
          if Items.Items[i] = AItem then
          index:= i;
        end;
      end;
      if (index-l) > (r-index) then
      begin
        lastTab:= AItem;
        firstTab:= nil;
      end
      else
      begin
        lastTab:= nil;
        firstTab:= AItem;
      end;
      RightAlignItems;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set CEAutoFit
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.SetCEAutoFit(const Value: Boolean);
var
  i: Integer;
begin
  if fCEAutoFit <> Value then
  begin
    fCEAutoFit:= Value;
    BeginUpdate;
    try
      for i:= 0 to Items.Count - 1 do
      begin
        if Items.Items[i] is TSpTBXTabItem then
        begin
          if not fCEAutoFit then
          TSpTBXTabItem(Items.Items[i]).CustomWidth:= -1
          else
          TSpTBXTabItem(Items.Items[i]).Visible:= true;
        end;
      end;
    finally
      if fCEAutoFit then
      begin
        LeftArrow.Visible:= false;
        RightArrow.Visible:= false;
      end;
      EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle left double click
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.WMLButtonDblClick(var Message: TWMMouse);
begin
  inherited;
  if assigned(OnMouseUp) then
  OnMouseUp(Self, mbLeft, [ssLeft, ssDouble], Message.XPos, Message.YPos);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  IsTabCloseButtonVisible
-------------------------------------------------------------------------------}
function TCESpTabItemViewer.IsTabCloseButtonVisible: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  Result := False;
  if IsOnTabToolbar then begin
    T := TSpTBXTabToolbar(View.Window);
    case T.TabCloseButton of
      tcbNone:
        Exit;
      tcbActive:
        if not Item.Checked then Exit;
    end;
    Result := True;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCETabSettings
-------------------------------------------------------------------------------}
constructor TCETabSettings.Create;
begin
  inherited;
  fClosedTabHistory:= true;
  fNewTabSelect:= true;
  fNewTabType:= 0;
  fOpenTabSelect:= true;
  fOpenNextToCurrent:= false;
  fReuseTabs:= false;
  fUndoCount:= 15;
  fDblClickCloseTab:= false;
  fSwitchTabOnDragHover:= true;
  fAllowDropToTab:= true;
end;

{-------------------------------------------------------------------------------
  Destroy TCETabSettings
-------------------------------------------------------------------------------}
destructor TCETabSettings.Destroy;
begin
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);
  inherited;
end;

{-------------------------------------------------------------------------------
  Set NewTabPath
-------------------------------------------------------------------------------}
procedure TCETabSettings.SetNewTabPath(const Value: WideString);
var
  ws: WideString;
  pidl: PItemIDList;
begin
  fNewTabPath:= Value;
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);

  pidl:= nil;
  if Length(fNewTabPath) > 5 then
  begin
    if LeftStr(fNewTabPath, 5) = 'PIDL:' then
    begin
      ws:= Copy(fNewTabPath, 6, Length(fNewTabPath)-5);
      pidl:= LoadPIDLFromMime(ws);
    end;
  end;

  if not assigned(pidl) then
  pidl:= PathToPIDL(fNewTabPath);

  fNewTabNamespace:= TNamespace.Create(pidl, nil);
end;

{-------------------------------------------------------------------------------
  Get/Set AutoFit
-------------------------------------------------------------------------------}
function TCETabSettings.GetAutoFit: Boolean;
begin
  Result:= TabSet.CEAutoFit;
end;
procedure TCETabSettings.SetAutoFit(const Value: Boolean);
begin
  TabSet.CEAutoFit:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoFitMaxSize
-------------------------------------------------------------------------------}
function TCETabSettings.GetAutoFitMaxSize: Integer;
begin
  Result:= TabSet.TabAutofitMaxSize;
end;
procedure TCETabSettings.SetAutoFitMaxSize(const Value: Integer);
begin
  TabSet.TabAutofitMaxSize:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set MaxTabSize
-------------------------------------------------------------------------------}
function TCETabSettings.GetMaxTabSize: Integer;
begin
  Result:= TabSet.TabMaxSize;
end;
procedure TCETabSettings.SetMaxTabSize(const Value: Integer);
begin
  TabSet.TabMaxSize:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set CloseButton
-------------------------------------------------------------------------------}
function TCETabSettings.GetCloseButton: TSpTBXTabCloseButton;
begin
  Result:= TabSet.TabCloseButton;
end;
procedure TCETabSettings.SetCloseButton(const Value: TSpTBXTabCloseButton);
begin
  TabSet.TabCloseButton:= Value;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEClosedTabHistoryItem
-------------------------------------------------------------------------------}
constructor TCEClosedTabHistoryItem.Create;
begin
  inherited;
  TabSettings:= TMemoryStream.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEClosedTabHistoryItem
-------------------------------------------------------------------------------}
destructor TCEClosedTabHistoryItem.Destroy;
begin
  TabSettings.Size:= 0;
  TabSettings.Free;
  inherited;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEScrollArrowItem
-------------------------------------------------------------------------------}
constructor TCEScrollArrowItem.Create(AOwner: TComponent);
begin
  inherited;
  fArrowsize:= 12;
end;

procedure TCEScrollArrowItem.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect:
    TRect; State: TSpTBXSkinStatesType; var ACaption: WideString; var
    CaptionRect: TRect; var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
var
  r: TRect;
  i: Integer;
  c: TColor;
begin
  if PaintStage = pstPrePaint then
  begin
    r:= ClientAreaRect;
    i:= fArrowsize div 3;
    if (ArrowDirection = adLeft) or (ArrowDirection = adRight) then
    begin
      r.Left:= Round(((r.Right - r.Left) - (i)) / 2);
      r.Top:= r.Top + Round((r.Bottom - r.Top) / 2);
    end
    else
    begin
      r.Left:= Round((r.Right - r.Left) / 2);
      r.Top:= Round(((r.Bottom - r.Top) - i) / 2);
    end;
    c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, State);
    case ArrowDirection of
      adLeft: SpDrawArrow(ACanvas, r.Left, r.Top, c, false, true, i);
      adRight: SpDrawArrow(ACanvas, r.Left, r.Top, c, false, false, i);
      adUp: SpDrawArrow(ACanvas, r.Left, r.Top, c, true, true, i);
      adDown: SpDrawArrow(ACanvas, r.Left, r.Top, c, true, false, i);
    end;
  end;
  PaintDefault:= false;
end;

end.
