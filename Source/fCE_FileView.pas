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
//  The Original Code is fCE_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FileView;

interface

uses
  // CE Units
  fCE_TabPage, CE_FileView, CE_GlobalCtrl, CE_Utils,
  dCE_Images, CE_ContextMenu, CE_LanguageEngine, CE_AppSettings,
  CE_InfoBar, CE_FileUtils, fCE_QuickView,
  // EasyListview
  EasyListview, 
  // VSTools
  VirtualExplorerEasyListview, MPCommonObjects, MPShellUtilities,
  VirtualExplorerTree, MPCommonUtilities, ColumnFormSpTBX,
  VirtualShellNotifier,
  // VT
  VirtualTrees,
  // SpTBX
  SpTBXItem, SpTBXControls, SpTBXDkPanels, SpTBXSkins,
  // TB2K
  TB2Item,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, JvAppStorage, Contnrs, StrUtils, ExtCtrls, ActiveX;

type
  TEasyEditManagerHack = class(TEasyEditManager);

  TCEFileViewHack = class(TCEFileView);

  TCEFileViewPage = class(TCECustomTabPage)
    QuickViewPopupMenu: TSpTBXPopupMenu;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    but_thumbpos_left: TSpTBXItem;
    but_thumbpos_top: TSpTBXItem;
    but_thumbpos_right: TSpTBXItem;
    but_thumbpos_bottom: TSpTBXItem;
    QuickViewSplitter: TSpTBXSplitter;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    but_thumbstyle_icon: TSpTBXItem;
    but_thumbstyle_smallicon: TSpTBXItem;
    but_thumbstyle_list: TSpTBXItem;
    but_thumbstyle_details: TSpTBXItem;
    but_thumbstyle_tiles: TSpTBXItem;
    but_thumbstyle_thumbnails: TSpTBXItem;
    but_thumbstyle_filmstrip: TSpTBXItem;
    procedure ThumbPositionClick(Sender: TObject);
    procedure QuickViewPopupMenuPopup(Sender: TObject);
    procedure ThumbViewStyleClick(Sender: TObject);
    procedure QuickViewSplitterMoved(Sender: TObject);
    procedure View(Sender: TObject);
  private
    fDownShiftState: TShiftState;
    fPathChanging: Boolean;
    fShowInfoBar: Boolean;
    fShowItemContextMenu: Boolean;
    fThumbPosition: TAlign;
    fViewStyle: TEasyListStyle;
    fThumbViewStyle: TEasyListStyle;
    procedure SetShowInfoBar(const Value: Boolean);
    procedure SetThumbPosition(const Value: TAlign);
    procedure SetViewStyle(const Value: TEasyListStyle);
    procedure SetThumbViewStyle(const Value: TEasyListStyle);
  protected
    fQuickView: TCEQuickView;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt:
        TPoint; var dwEffect: Longint): HResult; override; stdcall;
    function DragLeave: HResult; override; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint):
        HResult; override; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var
        dwEffect: Longint): HResult; override; stdcall;
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure InfoBarSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure InfoBarSplitterMoving(Sender: TObject; var NewSize: Integer; var
        Accept: Boolean);
    procedure ItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure OnColumnSizeChanged(Sender: TCustomEasyListview; Column: TEasyColumn);
    procedure OnEnumFinished(Sender: TCustomVirtualExplorerEasyListview);
    procedure OnItemContextMenu(Sender: TCustomEasyListview; HitInfo:
        TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var Handled:
        Boolean);
    procedure OnShellNotify(Sender: TCustomVirtualExplorerEasyListview; ShellEvent:
        TVirtualShellEvent);
    procedure SetActive(const Value: Boolean); override;
  public
    FileView: TCEFileView;
    ThumbViewSize: Integer;
    InfoBar: TCEInfoBar;
    InfoBarSplitter: TSpTBXSplitter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure RootChanging(Sender: TCustomVirtualExplorerEasyListview; const
        NewValue: TRootFolder; const CurrentNamespace, Namespace: TNamespace; var
        Allow: Boolean);
    procedure RootRebuild(Sender: TCustomVirtualExplorerEasyListview);
    procedure RootChange(
      Sender: TCustomVirtualExplorerEasyListview);
    procedure SelectPage; override;
    procedure UpdateCaption; override;
    procedure HidePage; override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure OnContextMenu(Sender: TCustomEasyListview; MousePt: TPoint; var
        Handled: Boolean);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure SaveToStream(AStream: TStream); override;
    procedure ShowHeaderSelector;
    property ShowInfoBar: Boolean read fShowInfoBar write SetShowInfoBar;
    property ThumbPosition: TAlign read fThumbPosition write SetThumbPosition;
    property ViewStyle: TEasyListStyle read fViewStyle write SetViewStyle;
    property ThumbViewStyle: TEasyListStyle read fThumbViewStyle write
        SetThumbViewStyle;
  end;

  TCEFileViewPageSettings = class(TCECustomTabPageSettings)
  private
    function GetPath: WideString;
    function GetViewStyle: TEasyListStyle;
    procedure SetPath(const Value: WideString);
    procedure SetViewStyle(const Value: TEasyListStyle);
  protected
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
    function GetRememberPanelLayout: Boolean; override;
  public
    FileViewPage: TCEFileViewPage;
  published
    property Path: WideString read GetPath write SetPath;
    property ViewStyle: TEasyListStyle read GetViewStyle write SetViewStyle;
  end;

  TCEFileViewSettings = class(TPersistent)
  private
    fArrowBrowse: Boolean;
    fFullRowSelect: Boolean;
    fHiddenFiles: Boolean;
    fSelectPreviousFolder: Boolean;
    fAutoSelectFirstItem: Boolean;
    fAutosizeListViewStyle: Boolean;
    fBrowseZipFolders: Boolean;
    fCellSizes: TCECellSizeSettings;
    fCheckBoxSelection: Boolean;
    fColumns: TCEColumnSettings;
    fFileSizeFormat: TVirtualFileSizeFormat;
    fFilmstrip: TCEFilmstripSettings;
    fFolderUpOnDblClick: Boolean;
    fFontSize: Integer;
    fFullRowDblClick: Boolean;
    fGroupBy: TCEGroupBySettings;
    fInfoBarSize: Integer;
    fShowInfoTips: Boolean;
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fSelectPasted: Boolean;
    fShowExtensions: Boolean;
    fShowGridLines: Boolean;
    fShowHeaderAlways: Boolean;
    fShowInfoBar: Boolean;
    fSingleClickBrowse: Boolean;
    fSingleClickExecute: Boolean;
    fSmoothScroll: Boolean;
    fSortAfterPaste: Boolean;
    fSortFolderFirstAlways: Boolean;
    fThreadedDetails: Boolean;
    fThreadedEnumeration: Boolean;
    fThreadedImages: Boolean;
    fUse_JumboIcons_in_InfoBar: Boolean;
    NotifyList: TComponentList;
    procedure SetArrowBrowse(const Value: Boolean);
    procedure SetFullRowSelect(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
    procedure SetSelectPreviousFolder(const Value: Boolean);
    procedure SetAutoSelectFirstItem(const Value: Boolean);
    procedure SetAutosizeListViewStyle(const Value: Boolean);
    procedure SetBrowseZipFolders(const Value: Boolean);
    procedure SetCheckBoxSelection(const Value: Boolean);
    procedure SetFileSizeFormat(const Value: TVirtualFileSizeFormat);
    procedure SetFolderUpOnDblClick(const Value: Boolean);
    procedure SetFontSize(const Value: Integer);
    procedure SetFullRowDblClick(const Value: Boolean);
    procedure SetSelectPasted(const Value: Boolean);
    procedure SetShowInfoTips(const Value: Boolean);
    procedure SetShowExtensions(const Value: Boolean);
    procedure SetShowGridLines(const Value: Boolean);
    procedure SetShowHeaderAlways(const Value: Boolean);
    procedure SetShowInfoBar(const Value: Boolean);
    procedure SetSingleClickBrowse(const Value: Boolean);
    procedure SetSingleClickExecute(const Value: Boolean);
    procedure SetSmoothScroll(const Value: Boolean);
    procedure SetSortAfterPaste(const Value: Boolean);
    procedure SetSortFolderFirstAlways(const Value: Boolean);
    procedure SetThreadedDetails(const Value: Boolean);
    procedure SetThreadedEnumeration(const Value: Boolean);
    procedure SetThreadedImages(const Value: Boolean);
    procedure SetUse_JumboIcons_in_InfoBar(const Value: Boolean);
  protected
    fUpdateCount: Integer;
    fViewStyle: TEasyListStyle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignSettingsTo(FileViewPage: TCEFileViewPage; AssignColumnSettings:
        Boolean = true);
    procedure AssignSettingsFrom(FileViewPage: TCEFileViewPage);
    procedure AssignFromActivePage;
    procedure RegisterNotify(FileViewPage: TComponent);
    procedure AssignColumnSettingsTo(FileView: TVirtualExplorerEasyListview);
    procedure SendChanges;
    procedure AssignColumnSettingsFrom(FileView: TVirtualExplorerEasyListview);
    procedure BeginUpdate;
    procedure ClearFilters;
    procedure EndUpdate(ASendChanges: Boolean = false);
  published
    property ArrowBrowse: Boolean read fArrowBrowse write SetArrowBrowse;
    property SelectPreviousFolder: Boolean read fSelectPreviousFolder write
        SetSelectPreviousFolder;
    property AutoSelectFirstItem: Boolean read fAutoSelectFirstItem write
        SetAutoSelectFirstItem;
    property AutosizeListViewStyle: Boolean read fAutosizeListViewStyle write
        SetAutosizeListViewStyle;
    property BrowseZipFolders: Boolean read fBrowseZipFolders write
        SetBrowseZipFolders;
    property CellSizes: TCECellSizeSettings read fCellSizes write fCellSizes;
    property CheckBoxSelection: Boolean read fCheckBoxSelection write
        SetCheckBoxSelection;
    property Columns: TCEColumnSettings read fColumns write fColumns;
    property FileSizeFormat: TVirtualFileSizeFormat read fFileSizeFormat write
        SetFileSizeFormat;
    property Filmstrip: TCEFilmstripSettings read fFilmstrip write fFilmstrip;
    property FolderUpOnDblClick: Boolean read fFolderUpOnDblClick write
        SetFolderUpOnDblClick;
    property FontSize: Integer read fFontSize write SetFontSize;
    property FullRowDblClick: Boolean read fFullRowDblClick write
        SetFullRowDblClick;
    property FullRowSelect: Boolean read fFullRowSelect write SetFullRowSelect;
    property GroupBy: TCEGroupBySettings read fGroupBy write fGroupBy;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
    property InfoBarSize: Integer read fInfoBarSize write fInfoBarSize;
    property ShowInfoTips: Boolean read fShowInfoTips write SetShowInfoTips;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout
        write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout
        write fRememberOuterToolbarLayout;
    property SelectPasted: Boolean read fSelectPasted write SetSelectPasted;
    property ShowExtensions: Boolean read fShowExtensions write SetShowExtensions;
    property ShowGridLines: Boolean read fShowGridLines write SetShowGridLines;
    property ShowHeaderAlways: Boolean read fShowHeaderAlways write
        SetShowHeaderAlways;
    property ShowInfoBar: Boolean read fShowInfoBar write SetShowInfoBar;
    property SingleClickBrowse: Boolean read fSingleClickBrowse write
        SetSingleClickBrowse;
    property SingleClickExecute: Boolean read fSingleClickExecute write
        SetSingleClickExecute;
    property SmoothScroll: Boolean read fSmoothScroll write SetSmoothScroll;
    property SortAfterPaste: Boolean read fSortAfterPaste write SetSortAfterPaste;
    property SortFolderFirstAlways: Boolean read fSortFolderFirstAlways write
        SetSortFolderFirstAlways;
    property ThreadedDetails: Boolean read fThreadedDetails write
        SetThreadedDetails;
    property ThreadedEnumeration: Boolean read fThreadedEnumeration write
        SetThreadedEnumeration;
    property ThreadedImages: Boolean read fThreadedImages write SetThreadedImages;
    property Use_JumboIcons_in_InfoBar: Boolean read fUse_JumboIcons_in_InfoBar
        write SetUse_JumboIcons_in_InfoBar;
    property ViewStyle: TEasyListStyle read fViewStyle write fViewStyle;
  end;

var
  GlobalFileViewSettings: TCEFileViewSettings;

implementation

{$R *.dfm}

uses
  dCE_Actions, CE_VistaFuncs, fCE_FolderPanel, CE_CommonObjects, CE_SpTabBar,
  GR32_Math, Math, Main;

{*------------------------------------------------------------------------------
  Get's called when TCEFileViewPage is created.
-------------------------------------------------------------------------------}
constructor TCEFileViewPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEFileViewPageSettings(Settings).FileViewPage:= Self;
  ThumbViewSize:= 100;
  fThumbPosition:= alBottom;
  fThumbViewStyle:= elsFilmStrip;
  Layout:= 'FileView';
  Images:= SmallSysImages;
  FileView:= TCEFileView.Create(Self);
  FileView.Parent:= self;
  FileView.Align:= alClient;
  FileView.Themed:= false;
  FileView.BorderStyle:= bsNone;
  FileView.BoundsRect:= Rect(0,0, self.ClientWidth, self.ClientHeight);
  FileView.OnRootRebuild:= RootRebuild;
  FileView.OnRootChanging:= RootChanging;
  FileView.OnRootChange:= RootChange;
  FileView.OnItemSelectionsChanged:= ItemSelectionsChanged;
  FileView.OnColumnSizeChanged:= OnColumnSizeChanged;
  FileView.OnContextMenu:= OnContextMenu;
  FileView.OnItemContextMenu:= OnItemContextMenu;
  FileView.OnMouseDown:= OnMouseDown;
  FileView.OnMouseUp:= OnMouseUp;
  FileView.OnShellNotify:= OnShellNotify;
  FileView.OnEnumFinished:= OnEnumFinished;
  FileView.FileObjects:= [foFolders,
                          foNonFolders];
  FileView.DragManager.MouseButton:= [cmbLeft,cmbRight];
  FileView.Selection.MouseButton:= [cmbLeft,cmbRight];
  FileView.ParentShowHint:= true;
  FileView.HintType:= ehtText;
  FileView.TabOrder:= 1;
  // translate header
  FileView.TranslateHeader:= false;

  FileView.PaintInfoGroup.BandThickness:= 2;
  FileView.PaintInfoGroup.BandColor:= clWindowText;
  FileView.PaintInfoGroup.BandColorFade:= clWindow;
  FileView.CompressedFile.Hilight:= false;
  FileView.EncryptedFile.Hilight:= false;
  FileView.GroupFont.Style:= [fsBold];
  GlobalFocusCtrl.CtrlList.Add(FileView);
  FileView.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalFileViewSettings.RegisterNotify(Self);
  SetDesktopIconFonts(FileView.Font);


  InfoBar:= TCEInfoBar.Create(nil);
  InfoBar.Parent:= Self;
  InfoBar.Align:= alBottom;
  InfoBar.Visible:= false;
  InfoBar.RowHeight:= SpGetControlTextHeight(InfoBar, InfoBar.Font) + 6;
  InfoBar.Height:= Max(GlobalFileViewSettings.InfoBarSize,InfoBar.RowHeight*3 + 6);

  InfoBarSplitter:= TSpTBXSplitter.Create(nil);
  InfoBarSplitter.Parent:= Self;
  InfoBarSplitter.Align:= alBottom;
  InfoBarSplitter.GripSize:= 0;
  InfoBarSplitter.MinSize:= InfoBar.RowHeight;
  InfoBarSplitter.Top:= InfoBar.BoundsRect.Top - InfoBarSplitter.Height;
  InfoBarSplitter.OnMouseUp:= InfoBarSplitterMouseUp;
  InfoBarSplitter.OnMoving:= InfoBarSplitterMoving;
  InfoBarSplitter.Visible:= false;

  ShowInfoBar:= false;

  GlobalFileViewSettings.AssignSettingsTo(Self);
  fShowItemContextMenu:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when TCEFileViewPage is destoyed.
-------------------------------------------------------------------------------}
destructor TCEFileViewPage.Destroy;
begin
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.ActivePage:= nil;
  //FileView.Free;
  InfoBar.Free;
  InfoBarSplitter.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  DragEnter
-------------------------------------------------------------------------------}
function TCEFileViewPage.DragEnter(const dataObj: IDataObject; grfKeyState:
    Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result:= TCEFileViewHack(FileView).DropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect);
end;

{-------------------------------------------------------------------------------
  DragLeave
-------------------------------------------------------------------------------}
function TCEFileViewPage.DragLeave: HResult;
begin
  Result:= TCEFileViewHack(FileView).DropTarget.DragLeave;
end;

{-------------------------------------------------------------------------------
  DragOver
-------------------------------------------------------------------------------}
function TCEFileViewPage.DragOver(grfKeyState: Longint; pt: TPoint; var
    dwEffect: Longint): HResult;
begin
  Result:= TCEFileViewHack(FileView).DropTarget.DragOver(grfKeyState, pt, dwEffect);
end;

{-------------------------------------------------------------------------------
  Drop
-------------------------------------------------------------------------------}
function TCEFileViewPage.Drop(const dataObj: IDataObject; grfKeyState: Longint;
    pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result:= TCEFileViewHack(FileView).DropTarget.Drop(dataObj, grfKeyState, pt, dwEffect);
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  FileView.BrowseTo(NewPath);
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  fPathChanging:= true;
  FileView.Selection.ClearAll;
  FileView.BrowseToByPIDL(NewPIDL);
  if FileView.Selection.First <> nil then
  FileView.Selection.First.MakeVisible(emvMiddle);
end;

{-------------------------------------------------------------------------------
  Get's called on InfoBarSplitter MouseUp
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.InfoBarSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  InfoBar.RefreshThumbnail;
  GlobalFileViewSettings.InfoBarSize:= InfoBar.Height;
  InfoBar.Repaint;
end;

{-------------------------------------------------------------------------------
  Get's called on InfoBarSplitter Moving
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.InfoBarSplitterMoving(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept:= NewSize > InfoBar.RowHeight;
  InfoBar.Paint;
end;

{*------------------------------------------------------------------------------
  Get's called on when item selection has changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ItemSelectionsChanged(Sender: TCustomEasyListview);
var
  NS: TNamespace;
  Item: TEasyItem;
  lastItem, tmpItem: TEasyItem;
begin
  if FileView.UpdateCount > 0 then
  Exit;
  
  if assigned(FileView.Selection.FocusedItem) and (FileView.Selection.Count > 0) then
  Item:= FileView.Selection.FocusedItem
  else
  Item:= FileView.Selection.First;
  
  if Assigned(Item) then
  begin
    FileView.ValidateNamespace(Item, NS);
    if assigned(NS) then
    begin
      GlobalPathCtrl.ChangeFocusedPath(Self, NS.NameForParsing);
    end;
    if assigned(fQuickView) then
    fQuickView.ActiveFilePath:= NS.NameForParsing;

    // Change Info Bar
    if ShowInfoBar then
    begin
      if FileView.Selection.Count = 1 then
      begin
        InfoBar.LoadFromPIDL(NS.AbsolutePIDL, 1);
      end
      else
      begin
        lastItem:= nil;
        tmpItem:= FileView.Selection.First;
        while assigned(tmpItem) do
        begin
          lastItem:= tmpItem;
          tmpItem:= FileView.Selection.Next(tmpItem);
        end;
        FileView.ValidateNamespace(lastItem, NS);
        InfoBar.LoadFromPIDL(NS.AbsolutePIDL, FileView.Selection.Count);
      end;
    end;
  end
  else
  begin
    GlobalPathCtrl.ChangeFocusedPath(Self, '');
    if assigned(fQuickView) then
    fQuickView.Close;
    if ShowInfoBar then
    InfoBar.Clear;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse down.
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  fDownShiftState:= Shift;

  fShowItemContextMenu:= true;

  if Shift = [ssRight, ssAlt] then
  fShowItemContextMenu:= false
  else if FileView.UseMouseRocker then
  begin
    if (Button = mbLeft) and FileView.RightMouseButton_IsDown then
    fShowItemContextMenu:= false
    else if (Button = mbRight) and FileView.LeftMouseButton_IsDown then
    fShowItemContextMenu:= false
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse up.
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  NS, targetNS: TNamespace;
  item: TEasyItem;
  WindowPt: TPoint;
  freeTarget: Boolean;
begin
  if (ssMiddle in fDownShiftState) or ((ssLeft in fDownShiftState) and (ssAlt in Shift)) then
  begin
    WindowPt := FileView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= FileView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not FileView.EditManager.Editing then
    begin
      FileView.EditManager.EndEdit;
      FileView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        if NS.Link and assigned(NS.ShellLink) then
        begin
          targetNS:= TNamespace.Create(PIDLMgr.CopyPIDL(NS.ShellLink.TargetIDList), nil);
          freeTarget:= true;
        end
        else
        begin
          targetNS:= NS;
          freeTarget:= false;
        end;

        try
          if targetNS.FileSystem and not targetNS.Folder then
          begin
            if ssShift in Shift then
            OpenFileInTab(targetNS.NameForParsing, not MainForm.TabSet.Settings.OpenTabSelect)
            else
            OpenFileInTab(targetNS.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect)
          end
          else
          begin
            if ssShift in Shift then
            OpenFolderInTab(Self, targetNS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
            else
            OpenFolderInTab(Self, targetNS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
          end;
        finally
          if freeTarget then
          targetNS.Free;
        end;            
      end;
    end;
  end
  else if (ssRight in fDownShiftState) and (Shift = [ssAlt]) then
  begin
    WindowPt := FileView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= FileView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not FileView.EditManager.Editing then
    begin
      FileView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        NS.ShowPropertySheet(MainForm);
      end;
    end;
    fShowItemContextMenu:= false;
  end;

  fDownShiftState:= [];
end;

{*------------------------------------------------------------------------------
  Get's called when root path is changing
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootChanging(Sender: TCustomVirtualExplorerEasyListview;
    const NewValue: TRootFolder; const CurrentNamespace, Namespace: TNamespace; var Allow: Boolean);
begin
  if Namespace <> nil then
  begin
    if GlobalPathCtrl.ActivePage = Self then
    begin
      if not fPathChanging then
      GlobalPathCtrl.ChangeGlobalPathPIDL(Self, Namespace.AbsolutePIDL);

      if FileView.Active then
      GlobalFileViewSettings.AssignColumnSettingsFrom(FileView);
    end;
  end;
  fPathChanging:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when root path is changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootChange(
  Sender: TCustomVirtualExplorerEasyListview);
begin
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
end;

{*------------------------------------------------------------------------------
  Get's called on root rebuild
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootRebuild(Sender:
    TCustomVirtualExplorerEasyListview);
begin
  UpdateCaption;
  //GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{*------------------------------------------------------------------------------
  Makes Self as a Active Component in GlobalPathCtrl
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SelectPage;
begin
  inherited;
  // Save old tab's column settings
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  GlobalFileViewSettings.AssignColumnSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView);

  // Load column settings
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
  // Set Page as active
  GlobalPathCtrl.ActivePage:= Self;
  GlobalPathCtrl.ChangeGlobalPathPIDL(Self, FileView.RootFolderNamespace.AbsolutePIDL);
  GlobalPathCtrl.GlobalPathCaption:= FileView.RootFolderNamespace.NameParseAddress;
  FileView.SetFocus;
end;

{*------------------------------------------------------------------------------
  Set Active value
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetActive(const Value: Boolean);
begin
  inherited;
  FileView.Active:= Value;
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
end;

{*------------------------------------------------------------------------------
  Update Caption
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.UpdateCaption;
begin
  TabCaption:= FileView.RootFolderNamespace.NameNormal;
  TCESpTabItem(TabItem).Images:= SmallSysImages;
  TCESpTabItem(TabItem).ImageIndex:= FileView.RootFolderNamespace.GetIconIndex(false, icSmall);
  TabItem.Hint:= FileView.RootFolderNamespace.NameParseAddress;
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.GlobalPathCaption:= FileView.RootFolderNamespace.NameParseAddress;
end;

procedure TCEFileViewPage.View(Sender: TObject);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Save Column settings
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnColumnSizeChanged(Sender: TCustomEasyListview;
    Column: TEasyColumn);
begin
  if GlobalPathCtrl.ActivePage = Self then
  GlobalFileViewSettings.AssignColumnSettingsFrom(FileView);
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCEFileViewPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEFileViewPageSettings;
end;

{*------------------------------------------------------------------------------
  Hide page
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.HidePage;
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Set Thumbnail Position
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetThumbPosition(const Value: TAlign);
begin
  if fThumbPosition = Value then Exit;

  case Value of
    alNone, alClient, alCustom: fThumbPosition:= alBottom;
    else
    fThumbPosition:= Value;
  end;

  if fViewStyle = elsFilmStrip then
  begin
    FileView.Align:= fThumbPosition;
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    begin
      FileView.Height:= ThumbViewSize;
    end
    else
    begin
      FileView.Width:= ThumbViewSize;
    end;
    
    QuickViewSplitter.Align:= fThumbPosition;
    case fThumbPosition of
      alLeft: QuickViewSplitter.Left:= FileView.BoundsRect.Right;
      alRight: QuickViewSplitter.Left:= FileView.BoundsRect.Left-QuickViewSplitter.Width;
      alTop: QuickViewSplitter.Top:= FileView.BoundsRect.Bottom;
      alBottom: QuickViewSplitter.Top:= FileView.BoundsRect.Top-QuickViewSplitter.Height;
    end;
  end;

  GlobalFileViewSettings.AssignSettingsFrom(Self);
end;

{*------------------------------------------------------------------------------
  Set View Style
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetViewStyle(const Value: TEasyListStyle);
begin
  if fViewStyle = Value then Exit;

  if FileView.Active and ((fViewStyle = elsReport) or GlobalFileViewSettings.ShowHeaderAlways) then
  GlobalFileViewSettings.AssignColumnSettingsFrom(FileView);

  fViewStyle:= Value;

  if fViewStyle = elsFilmStrip then
  begin
    if not assigned(fQuickView) then
    begin
      fQuickView:= TCEQuickView.Create(self);
      fQuickView.Parent:= self;
      fQuickView.Align:= alClient;
      fQuickview.PopupMenu:= QuickViewPopupMenu;
      fQuickview.Active:= true;
    end;

    FileView.Align:= fThumbPosition;
    FileView.View:= fThumbViewStyle;
    if ThumbViewSize < 20 then
    ThumbViewSize:= 20;
    
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    begin
      FileView.Height:= ThumbViewSize;
    end
    else
    begin
      FileView.Width:= ThumbViewSize;
    end;
    QuickViewSplitter.Align:= fThumbPosition;
    QuickViewSplitter.Visible:= true;

    case fThumbPosition of
      alLeft: QuickViewSplitter.Left:= FileView.BoundsRect.Right;
      alRight: QuickViewSplitter.Left:= FileView.BoundsRect.Left-QuickViewSplitter.Width;
      alTop: QuickViewSplitter.Top:= FileView.BoundsRect.Bottom;
      alBottom: QuickViewSplitter.Top:= FileView.BoundsRect.Top-QuickViewSplitter.Height;
    end;
  end
  else
  begin
    FileView.Align:= alClient;

    if assigned(fQuickView) then
    begin
      FreeAndNil(fQuickView);
    end;
    QuickViewSplitter.Visible:= false;
    FileView.View:= fViewStyle;
    if FileView.Active and ((fViewStyle = elsReport) or GlobalFileViewSettings.ShowHeaderAlways) then
    GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
  end;

  GlobalFileViewSettings.ViewStyle:= fViewStyle;
end;

{*------------------------------------------------------------------------------
  ThumbViewStyle Click
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ThumbViewStyleClick(Sender: TObject);
begin
  case TSpTBXitem(Sender).Tag of
    1: ThumbViewStyle:= elsIcon;
    2: ThumbViewStyle:= elsSmallIcon;
    3: ThumbViewStyle:= elsList;
    4: ThumbViewStyle:= elsReport;
    5: ThumbViewStyle:= elsTile;
    6: ThumbViewStyle:= elsThumbnail;
    7: ThumbViewStyle:= elsFilmStrip;
  end;
end;

{*------------------------------------------------------------------------------
  Change Thumbnails position (menu item click)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ThumbPositionClick(Sender: TObject);
begin
  case TSpTBXitem(Sender).Tag of
    0: ThumbPosition:= alLeft;
    1: ThumbPosition:= alTop;
    2: ThumbPosition:= alRight;
    3: ThumbPosition:= alBottom;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on QuickView PopupMenu Popup
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.QuickViewPopupMenuPopup(Sender: TObject);
begin
  case ThumbPosition of
    alLeft: but_thumbpos_left.Checked:= true;
    alTop: but_thumbpos_top.Checked:= true;
    alRight: but_thumbpos_right.Checked:= true;
    alBottom: but_thumbpos_bottom.Checked:= true;
  end;

  case ThumbViewStyle of
    elsIcon: but_thumbstyle_icon.Checked:= true;
    elsSmallIcon: but_thumbstyle_smallicon.Checked:= true;
    elsList: but_thumbstyle_list.Checked:= true;
    elsReport: but_thumbstyle_details.Checked:= true;
    elsTile: but_thumbstyle_tiles.Checked:= true;
    elsThumbnail: but_thumbstyle_thumbnails.Checked:= true;
    elsFilmstrip: but_thumbstyle_filmstrip.Checked:= true;
  end;
end;

{*------------------------------------------------------------------------------
  QuickView Splitter Moved
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.QuickViewSplitterMoved(Sender: TObject);
begin
  if fViewStyle = elsFilmStrip then
  begin
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    ThumbViewSize:= FileView.Height
    else
    ThumbViewSize:= FileView.Width;

    GlobalFileViewSettings.AssignSettingsFrom(Self);
  end;
end;

{*------------------------------------------------------------------------------
  Set Thumbnail View Style
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetThumbViewStyle(const Value: TEasyListStyle);
begin
  fThumbViewStyle:= Value;
  if fViewStyle = elsFilmStrip then
  begin
    FileView.View:= Value;
  end;
  
  GlobalFileViewSettings.AssignSettingsFrom(Self);
end;

{*------------------------------------------------------------------------------
  Get's called on background context menu
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnContextMenu(Sender: TCustomEasyListview; MousePt:
    TPoint; var Handled: Boolean);
var
  menu: TCEBackContextMenu;
  item: TEasyItem;
begin
  if fShowItemContextMenu then
  begin
    CEActions.UpdateAll;
    menu:= TCEBackContextMenu.Create;
    try
      menu.UpperMenuItems:= CEActions.BackgroundCMItems_up;
      menu.LowerMenuItems:= CEActions.BackgroundCMItems_down;

      if menu.ShowMenu(MousePt, FileView.RootFolderNamespace, FileView) then
      begin
        item:= TCEFileViewHack(FileView).RereadAndRefresh(False);
        if Assigned(item) then
        begin
          FileView.Selection.SelectRange(item,item,false,true);
          FileView.Selection.FocusedItem:= item;
          item.Edit(nil);
        end;
      end;
    finally
      menu.Free;
      Handled:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On Item ContextMenu
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnItemContextMenu(Sender: TCustomEasyListview;
    HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var
    Handled: Boolean);
begin
  if not Handled then
  Handled:= not fShowItemContextMenu;
end;

{-------------------------------------------------------------------------------
  On Shell Notify
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnShellNotify(Sender:
    TCustomVirtualExplorerEasyListview; ShellEvent: TVirtualShellEvent);
begin
  case ShellEvent.ShellNotifyEvent of
    vsneUpdateDir: begin
      if ShowInfoBar and assigned(InfoBar.LatestNS) then
      ItemSelectionsChanged(FileView);
    end;
    vsneUpdateItem: begin
      if Self.Visible and PIDLMgr.EqualPIDL(ShellEvent.PIDL1, FileView.RootFolderNamespace.AbsolutePIDL) then
      GlobalPathCtrl.ChangeGlobalContent(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load from stream
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.LoadFromStream(AStream: TStream);
var
  CustomPIDL: PItemIDList;
begin
  CustomPIDL:= PIDLMgr.LoadFromStream(AStream);
  if Assigned(CustomPIDL) then
  begin
    FileView.RootFolderCustomPIDL:= CustomPIDL;
  end;
  PIDLMgr.FreePIDL(CustomPIDL);
end;

{-------------------------------------------------------------------------------
  On EnumFinished
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnEnumFinished(Sender:
    TCustomVirtualExplorerEasyListview);
begin
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{-------------------------------------------------------------------------------
  Save to stream
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SaveToStream(AStream: TStream);
begin
  PIDLMgr.SaveToStream(AStream, FileView.RootFolderCustomPIDL);
end;

{-------------------------------------------------------------------------------
  Set Show InfoBar
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetShowInfoBar(const Value: Boolean);
var
  NS: TNamespace;
  Item: TEasyItem;
begin
  if fShowInfoBar <> Value then
  begin
    fShowInfoBar:= Value;
    if fShowInfoBar then
    begin
      InfoBar.Visible:= true;
      InfoBar.Top:= Self.BoundsRect.Bottom;
      InfoBarSplitter.Top:= InfoBar.Top - InfoBarSplitter.Height;
      InfoBarSplitter.Visible:= true;
      InfoBar.Height:= GlobalFileViewSettings.InfoBarSize;

      if FileView.Selection.Count > 1 then
      Item:= FileView.Selection.FocusedItem
      else
      Item:= FileView.Selection.First;

      if FileView.ValidateNamespace(Item, NS) then
      InfoBar.LoadFromPIDL(NS.AbsolutePIDL);
    end
    else
    begin
      InfoBar.Visible:= false;
      InfoBar.Clear;
      InfoBarSplitter.Visible:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Show Header Selector
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ShowHeaderSelector;
begin
  FileView.ShowHeaderSelector;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEFileViewSettings
-------------------------------------------------------------------------------}
constructor TCEFileViewSettings.Create;
begin
  inherited;
  fUpdateCount:= 0;
  NotifyList:= TComponentList.Create(false);
  fColumns:= TCEColumnSettings.Create;
  fGroupBy:= TCEGroupBySettings.Create;
  fCellSizes:= TCECellSizeSettings.Create;
  fFilmstrip:= TCEFilmstripSettings.Create;
  Filmstrip.ThumbPos:= alBottom;
  Filmstrip.ThumbStyle:= elsFilmstrip;
  Filmstrip.ThumbSize:= 120;
  fRememberInnerToolbarLayout:= true;
  fInfoBarSize:= 3;
  fShowInfoTips:= true;
  fBrowseZipFolders:= false;
  fSingleClickBrowse:= false;
  fFileSizeFormat:= vfsfDefault;
  fArrowBrowse:= true;
  fFontSize:= -1;
  fFolderUpOnDblClick:= true;
  fFullRowDblClick:= false;
  fSelectPasted:= true;
  fSortAfterPaste:= true;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFileViewSettings
-------------------------------------------------------------------------------}
destructor TCEFileViewSettings.Destroy;
begin
  NotifyList.Free;
  fColumns.Free;
  fGroupBy.Free;
  fCellSizes.Free;
  fFilmstrip.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign options from FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignSettingsFrom(FileViewPage: TCEFileViewPage);
begin
  if fUpdateCount > 0 then
  Exit;
  
  if not assigned(FileViewPage) then
  Exit;

  ViewStyle:= FileViewPage.ViewStyle;
  Filmstrip.ThumbStyle:= FileViewPage.ThumbViewStyle;
  Filmstrip.ThumbPos:= FileViewPage.ThumbPosition;
  Filmstrip.ThumbSize:= FileViewPage.ThumbViewSize;
end;

{*------------------------------------------------------------------------------
  Assign options to FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignSettingsTo(FileViewPage: TCEFileViewPage;
    AssignColumnSettings: Boolean = true);
var
  options: TVirtualEasyListviewOptions;
begin
  if not assigned(FileViewPage) then
  Exit;

  Self.BeginUpdate;
  FileViewPage.FileView.BeginUpdate;
  try
    FileViewPage.ThumbViewSize:= Filmstrip.ThumbSize;
    FileViewPage.ViewStyle:= ViewStyle;
    FileViewPage.ThumbViewStyle:= Filmstrip.ThumbStyle;
    FileViewPage.ThumbPosition:= Filmstrip.ThumbPos;
    // Toggles
    FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
    if fHiddenFiles then
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
    else
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters];
    FileViewPage.InfoBar.CalculateHiddenItems:= fHiddenFiles;
    FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
    if fShowHeaderAlways then
    FileViewPage.FileView.Header.Visible:= true;
    FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
    FileViewPage.FileView.ShowExtension:= fShowExtensions;
    FileViewPage.FileView.SelectPreviousFolder:= fSelectPreviousFolder;
    FileViewPage.FileView.AutoSelectFirstItem:= fAutoSelectFirstItem;
    FileViewPage.FileView.AutosizeListViewStyle:= fAutosizeListViewStyle;
    FileViewPage.FileView.SortFolderFirstAlways:= fSortFolderFirstAlways;
    FileViewPage.ShowInfoBar:= fShowInfoBar;
    FileViewPage.InfoBar.Height:= fInfoBarSize;
    FileViewPage.InfoBar.UseJumboIcons:= fUse_JumboIcons_in_InfoBar;
    FileViewPage.FileView.SingleClickBrowse:= fSingleClickBrowse;
    FileViewPage.FileView.SingleClickExecute:= fSingleClickExecute;
    FileViewPage.FileView.CheckBoxSelection:= fCheckBoxSelection;
    FileViewPage.FileView.PaintInfoItem.GridLines:= fShowGridLines;
    FileViewPage.FileView.FolderUpOnDblClick:= fFolderUpOnDblClick;
    FileViewPage.FileView.FullRowDblClick:= fFullRowDblClick;
    FileViewPage.FileView.SelectPasted:= fSelectPasted;
    FileViewPage.FileView.SortAfterPaste:= fSortAfterPaste;
    // Options
    options:= FileViewPage.FileView.Options;
    if fBrowseZipFolders then Include(options, eloBrowseExecuteZipFolder) else Exclude(options, eloBrowseExecuteZipFolder);
    if fThreadedImages then Include(options, eloThreadedImages) else Exclude(options, eloThreadedImages);
    if fThreadedEnumeration then Include(options, eloThreadedEnumeration) else Exclude(options, eloThreadedEnumeration);
    if fThreadedDetails then Include(options, eloThreadedDetails) else Exclude(options, eloThreadedDetails);
    if fShowInfoTips then Include(options, eloQueryInfoHints) else Exclude(options, eloQueryInfoHints);

    FileViewPage.FileView.Options:= options;

    // Misc
    FileViewPage.FileView.FileSizeFormat:= fFileSizeFormat;
    FileViewPage.FileView.ArrowBrowse:= fArrowBrowse;
    if FontSize > 0 then
    FileViewPage.FileView.Font.Size:= FontSize
    else
    SetDesktopIconFonts(FileViewPage.FileView.Font);

    // Cell Sizes
    if (CellSizes.LargeIcons_Width < 1) or (CellSizes.LargeIcons_Height < 1) then
    FileViewPage.FileView.CellSizes.Icon.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.Icon.SetSize(CellSizes.LargeIcons_Width, CellSizes.LargeIcons_Height);

    if (CellSizes.SmallIcons_Width < 1) or (CellSizes.SmallIcons_Height < 1) then
    FileViewPage.FileView.CellSizes.SmallIcon.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.SmallIcon.SetSize(CellSizes.SmallIcons_Width, CellSizes.SmallIcons_Height);

    if (CellSizes.List_Width < 1) or (CellSizes.List_Height < 1) then
    FileViewPage.FileView.CellSizes.List.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.List.SetSize(CellSizes.List_Width, CellSizes.List_Height);

    if (CellSizes.Details_Width < 1) or (CellSizes.Details_Height < 1) then
    FileViewPage.FileView.CellSizes.Report.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.Report.SetSize(CellSizes.Details_Width, CellSizes.Details_Height);

    if (CellSizes.Tiles_Width < 1) or (CellSizes.Tiles_Height < 1) then
    FileViewPage.FileView.CellSizes.Tile.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.Tile.SetSize(CellSizes.Tiles_Width, CellSizes.Tiles_Height);

    if (CellSizes.Thumbnails_Width < 1) or (CellSizes.Thumbnails_Height < 1) then
    FileViewPage.FileView.CellSizes.Thumbnail.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.Thumbnail.SetSize(CellSizes.Thumbnails_Width, CellSizes.Thumbnails_Height);

    if (CellSizes.Filmstrip_Width < 1) or (CellSizes.Filmstrip_Height < 1) then
    FileViewPage.FileView.CellSizes.FilmStrip.RestoreDefaults
    else
    FileViewPage.FileView.CellSizes.FilmStrip.SetSize(CellSizes.Filmstrip_Width, CellSizes.Filmstrip_Height);
    
    if AssignColumnSettings then
    AssignColumnSettingsTo(FileViewPage.FileView);
  finally
    FileViewPage.FileView.EndUpdate(FileViewPage.Visible);
    Self.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Save Settings From ActivePage
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignFromActivePage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    AssignSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage));
    InfoBarSize:= TCEFileViewPage(GlobalPathCtrl.ActivePage).InfoBar.Height;
  end;
end;

{*------------------------------------------------------------------------------
  Register Notify
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.RegisterNotify(FileViewPage: TComponent);
begin
  NotifyList.Add(FileViewPage);
end;

{*------------------------------------------------------------------------------
  Restore Column Settings
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignColumnSettingsTo(FileView:
    TVirtualExplorerEasyListview);
var
  i: Integer;
  col: TEasyColumn;
  settings: PCEColSettings;
  grouping: PCEGroupBySetting;
begin
  if not assigned(FileView) then
  Exit;
  
  if FileView.RootFolderNamespace.IsMyComputer then
  begin
    settings:= @Columns.MyComputerColSettings;
    grouping:= @GroupBy.MyComputerGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsControlPanel or FileView.RootFolderNamespace.IsControlPanelChildFolder(false) then
  begin
    settings:= @Columns.ControlPanelColSettings;
    grouping:= @GroupBy.ControlPanelGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns.NetworkColSettings;
    grouping:= @GroupBy.NetworkGroupBySettings;
  end
  else
  begin
    settings:= @Columns.DefaultColSettings;
    grouping:= @GroupBy.DefaultGroupBySettings;
  end;

  FileView.GroupingColumn:= grouping.Index;
  FileView.Grouped:= grouping.Enabled;

  if Length(settings^) = 0 then
  Exit;

  FileView.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to FileView.Header.Columns.Count - 1 do
    begin
      if FileView.Header.Columns.Columns[i].Visible then
      FileView.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(settings^) - 1 do
    begin
      if settings^[i].Index < FileView.Header.Columns.Count then
      begin
        col:= FileView.Header.Columns.Columns[settings^[i].Index];
        col.Visible:= true;
        col.Position:= settings^[i].Position;
        col.Width:= settings^[i].Width;
        col.SortDirection:= settings^[i].Sort;
      end;
    end;


  finally
    FileView.Header.Columns.EndUpdate(true);
  end;
end;

{*------------------------------------------------------------------------------
  Send Changes
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SendChanges;
var
  i: Integer;
  FileViewPage: TCEFileViewPage;
  doRebuild: Boolean;
begin
  if fUpdateCount > 0 then
  Exit;
  
  for i:= 0 to NotifyList.Count - 1 do
  begin
    if NotifyList.Items[i] is TCEFileViewPage then
    begin
      FileViewPage:= TCEFileViewPage(NotifyList.Items[i]);
      doRebuild:= FileViewPage.FileView.ShowExtension <> ShowExtensions;
      AssignSettingsTo(FileViewPage, false);
      if CellSizes.IsChanged(FileViewPage.FileView) then
      FileViewPage.FileView.Groups.Rebuild;
      if doRebuild then
      FileViewPage.FileView.Rebuild;
    end
    else if NotifyList.Items[i] is TVirtualExplorerTree then
    begin
      if fHiddenFiles then
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
      else
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders];
    end;
  end;
  CEFolderPanel.FolderTree.HiddenFiles:= fHiddenFiles;
end;

{*------------------------------------------------------------------------------
  Set Full Row select
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetFullRowSelect(const Value: Boolean);
begin
  fFullRowSelect:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Hidden Files
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetHiddenFiles(const Value: Boolean);
begin
  fHiddenFiles:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set ShowExtensions
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowExtensions(const Value: Boolean);
begin
  fShowExtensions:= Value;
  SendChanges;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Refresh;
  end
end;

{*------------------------------------------------------------------------------
  Set Show Header Always
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowHeaderAlways(const Value: Boolean);
begin
  fShowHeaderAlways:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Smooth Scroll
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSmoothScroll(const Value: Boolean);
begin
  fSmoothScroll:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Save Column Settings
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignColumnSettingsFrom(FileView:
    TVirtualExplorerEasyListview);
var
  col: TEasyColumn;
  i,c: Integer;
  settings: PCEColSettings;
  grouping: PCEGroupBySetting;
begin
  if not assigned(FileView) then
  Exit;

  if FileView.RootFolderNamespace.IsMyComputer then
  begin
    settings:= @Columns.MyComputerColSettings;
    grouping:= @GroupBy.MyComputerGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsControlPanel or FileView.RootFolderNamespace.IsControlPanelChildFolder(false) then
  begin
    settings:= @Columns.ControlPanelColSettings;
    grouping:= @GroupBy.ControlPanelGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns.NetworkColSettings;
    grouping:= @GroupBy.NetworkGroupBySettings;
  end
  else
  begin
    settings:= @Columns.DefaultColSettings;
    grouping:= @GroupBy.DefaultGroupBySettings;
  end;
  
  c:= 0;
  for i:= 0 to FileView.Header.Columns.Count - 1 do
  begin
    if FileView.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(settings^) <> c then
  SetLength(settings^, c);
  
  i:= 0;

  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    settings^[i].Index:= col.Index;
    settings^[i].Position:= col.Position;
    settings^[i].Width:= col.Width;
    settings^[i].Sort:= col.SortDirection;
    col:= FileView.Header.NextVisibleColumn(col);
    inc(i);
  end;

  grouping.Index:= FileView.GroupingColumn;
  grouping.Enabled:= FileView.Grouped;
end;

{-------------------------------------------------------------------------------
  Begin Update
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.BeginUpdate;
begin
  fUpdateCount:= fUpdateCount + 1;
end;

{-------------------------------------------------------------------------------
  End Update
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.EndUpdate(ASendChanges: Boolean = false);
begin
  fUpdateCount:= fUpdateCount - 1;
  if fUpdateCount < 0 then
  fUpdateCount:= 0;

  if (fUpdateCount = 0) and ASendChanges then
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Clear filters from all FileViews
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.ClearFilters;
var
  i,i2: Integer;
  FileViewPage: TCEFileViewPage;
begin
  for i:= 0 to NotifyList.Count - 1 do
  begin
    if NotifyList.Items[i] is TCEFileViewPage then
    begin
      FileViewPage:= TCEFileViewPage(NotifyList.Items[i]);
      FileViewPage.FileView.BeginUpdate;
      try
        for i2:= 0 to FileViewPage.FileView.ItemCount - 1 do
        begin
          FileViewPage.FileView.Items.Items[i2].Visible:= true;
        end;
        FileViewPage.FileView.BackGround.Enabled:= false;
      finally
        FileViewPage.FileView.EndUpdate;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set ArrowBrowse
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetArrowBrowse(const Value: Boolean);
begin
  fArrowBrowse:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SelectPreviousFolder
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSelectPreviousFolder(const Value: Boolean);
begin
  fSelectPreviousFolder:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SelectPreviousFolder
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetAutoSelectFirstItem(const Value: Boolean);
begin
  fAutoSelectFirstItem:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set AutosizeListViewStyle
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetAutosizeListViewStyle(const Value: Boolean);
begin
  fAutosizeListViewStyle:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set BrowseZipFolders
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetBrowseZipFolders(const Value: Boolean);
begin
  fBrowseZipFolders:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set CheckBoxSelection
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetCheckBoxSelection(const Value: Boolean);
begin
  fCheckBoxSelection:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set FileSizeFormat
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetFileSizeFormat(const Value:
    TVirtualFileSizeFormat);
begin
  fFileSizeFormat:= Value;
  SendChanges;
end;

procedure TCEFileViewSettings.SetFolderUpOnDblClick(const Value: Boolean);
begin
  fFolderUpOnDblClick:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set FontSize
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetFontSize(const Value: Integer);
begin
  fFontSize:= Value;
end;

{-------------------------------------------------------------------------------
  Set FullRow Dbl Click
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetFullRowDblClick(const Value: Boolean);
begin
  fFullRowDblClick:= Value;
  SendChanges;
end;

procedure TCEFileViewSettings.SetSelectPasted(const Value: Boolean);
begin
  fSelectPasted:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ShowGridLines
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowGridLines(const Value: Boolean);
begin
  fShowGridLines:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ShowInfoBar
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowInfoBar(const Value: Boolean);
begin
  fShowInfoBar:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ShowInfoTips
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowInfoTips(const Value: Boolean);
begin
  fShowInfoTips:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SingleClickBrowse
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSingleClickBrowse(const Value: Boolean);
begin
  fSingleClickBrowse:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SingleClickExecute
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSingleClickExecute(const Value: Boolean);
begin
  fSingleClickExecute:= Value;
  SendChanges;
end;

procedure TCEFileViewSettings.SetSortAfterPaste(const Value: Boolean);
begin
  fSortAfterPaste:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SortFolderFirstAlways
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSortFolderFirstAlways(const Value: Boolean);
begin
  fSortFolderFirstAlways:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedDetails
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedDetails(const Value: Boolean);
begin
  fThreadedDetails:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedEnumeration
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedEnumeration(const Value: Boolean);
begin
  fThreadedEnumeration:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedImages
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedImages(const Value: Boolean);
begin
  fThreadedImages:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set Use_JumboIcons_in_InfoBar
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetUse_JumboIcons_in_InfoBar(const Value:
    Boolean);
begin
  fUse_JumboIcons_in_InfoBar:= Value;
  SendChanges;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetPath: WideString;
var
  i: Integer;
begin
  if FileViewPage.FileView.RootFolderNamespace.IsDesktop then
  i:= 0
  else
  i:= CE_SpecialNamespaces.GetSpecialID(FileViewPage.FileView.RootFolderNamespace.AbsolutePIDL);

  if i > -1 then
  begin
    Result:= 'special://' + IntToStr(i);
  end
  else if WideDirectoryExists(FileViewPage.FileView.RootFolderNamespace.NameForParsing) then
  begin
    Result:= 'file://' + FileViewPage.FileView.RootFolderNamespace.NameForParsing;
  end
  else
  begin
    Result:= 'pidl://' + SavePIDLToMime(FileViewPage.FileView.RootFolderNamespace.AbsolutePIDL);
  end;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberOuterToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Set Path
-------------------------------------------------------------------------------}
procedure TCEFileViewPageSettings.SetPath(const Value: WideString);
var
  format, fPath: WideString;
  i,c: Integer;
  PIDL: PItemIDList;
begin
  FileViewPage.FileView.BeginUpdate;
  try
    i:= Pos('://', Value);
    if i > 0 then
    begin
      c:= Length(Value);
      format:= LeftStr(Value, i-1);
      fPath:= RightStr(Value, c - i-2);

      if format = 'file' then
      begin
        PIDL:= PathToPIDL(fPath);
      end
      else if format = 'pidl' then
      begin
        PIDL:= LoadPIDLFromMime(fPath);
      end
      else if format = 'special' then
      begin
        SHGetspecialFolderLocation(0, StrToIntDef(fPath, 0), PIDL);
      end
      else begin
        PIDL:= PathToPIDL(fPath);
      end;
    end
    else
    begin
      PIDL:= PathToPIDL(Value);
    end;

    if assigned(PIDL) then
    begin
      FileViewPage.FileView.BrowseToByPIDL(PIDL);
      FileViewPage.FileView.ClearHistory;
      FileViewPage.FileView.fChangeHistory:= false;
      FileViewPage.FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(PIDL),nil),true);
      FileViewPage.FileView.fChangeHistory:= true;
    end;
  finally
    FileViewPage.FileView.EndUpdate(false);
    FileViewPage.UpdateCaption;
  end;
end;

{-------------------------------------------------------------------------------
  Get/Set ViewStyle
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetViewStyle: TEasyListStyle;
begin
  Result:= FileViewPage.ViewStyle;
end;
procedure TCEFileViewPageSettings.SetViewStyle(const Value: TEasyListStyle);
begin
  FileViewPage.ViewStyle:= Value;
end;

{##############################################################################}

initialization
  GlobalFileViewSettings:= TCEFileViewSettings.Create;
  GlobalAppSettings.AddItem('FileView', GlobalFileViewSettings, true);

  TabPageClassList.RegisterClass('FileView', TCEFileViewPage, TCEFileViewPageSettings);

finalization
  FreeAndNil(GlobalFileViewSettings);

end.
