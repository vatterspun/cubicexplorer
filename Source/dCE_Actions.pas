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
//  The Original Code is dCE_Actions.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit dCE_Actions;

interface

uses
  // CE Units
  CE_GlobalCtrl,  dCE_Images, CE_TBActions, CE_Utils, CE_LanguageEngine,
  CE_SpTabBar, CE_AppSettings,
  // PngControls
  PngImageList,
  // EasyListview & VSTools
  EasyListview, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualExplorerEasyListview, MPCommonObjects,
  // Tnt
  TntActnList, TntClipbrd, TntSysUtils, TntWindows, TntClasses, TntSystem,
  TntMenus, 
  // JVCL
  JvDockControlForm,
  // SpTBX
  SpTBXSkins,
  // GraphicEx
  GraphicEx,
  // System Units
  SysUtils, Classes, ActnList, ImgList, Controls, Windows, ExtCtrls, Forms,
  ShellAPI, AppEvnts, Messages, ShlObj, Clipbrd, Menus, DOM, Contnrs;

const
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;

  WM_SingleInstance = WM_USER + 1;
  WM_MakeVisible = WM_USER + 100;
  WM_ExecuteAction = WM_USER + 101;
  WM_AdminResult = WM_USER + 102;

type
  TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);
  TDOMElementHack = class(TDOMElement);

  TCEHotkeySettings = class(TCECustomSettingStorage)
  private
    fActions: TActionList;
    fModifiedActions: TObjectList;
  protected
    property Actions: TActionList read fActions write fActions;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    property ModifiedActions: TObjectList read fModifiedActions;
  end;

  TCEGlobalHotkeys = class(TCECustomSettingStorage)
  private
    fActions: TActionList;
    fIsRegistered: Boolean;
    fMsgHandle: HWND;
    function GetCount: Integer;
  protected
    fHotkeyActions: TObjectList;
    fHotkeys: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddHotkey(AAction: TAction; AHotkey: TShortcut): Integer;
    function CanRegister(AAction: TTntAction; AHotkey: TShortcut): Boolean;
    procedure Clear;
    procedure DeleteHotkey(Index: Integer);
    procedure ExecuteHotkey(id: Integer);
    function GetAction(AIndex: Integer): TAction;
    function GetHotkey(AIndex: Integer): TShortcut; overload;
    function GetHotkey(AAction: TAction): TShortcut; overload;
    function GetHotkeys(AAction: TAction; AResults: TStrings): Integer;
    function IndexOf(AAction: TAction): Integer;
    procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    procedure RegisterAll;
    procedure Replace(AIndex: Integer; AAction: TAction; AHotkey: TShortcut);
    procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    procedure UnRegisterAll;
    property Actions: TActionList read fActions write fActions;
    property Count: Integer read GetCount;
    property IsRegistered: Boolean read fIsRegistered;
    property MsgHandle: HWND read fMsgHandle write fMsgHandle;
  end;
    
  TCEActions = class(TDataModule)
    ActionList: TTntActionList;
    act_gen_exit: TTntAction;
    UpdateTimer: TTimer;
    act_navi_folderup: TTntAction;
    act_navi_refresh: TTntAction;
    act_view_folders: TTntAction;
    act_view_bookmark: TTntAction;
    act_view_large: TTntAction;
    act_view_small: TTntAction;
    act_view_list: TTntAction;
    act_view_details: TTntAction;
    act_view_thumbs: TTntAction;
    act_view_tiles: TTntAction;
    act_view_filmstrip: TTntAction;
    act_edit_copy: TTntAction;
    act_edit_cut: TTntAction;
    act_edit_paste: TTntAction;
    act_edit_delete: TTntAction;
    act_edit_selall: TTntAction;
    act_edit_invertsel: TTntAction;
    act_edit_properties: TTntAction;
    act_edit_rename: TTntAction;
    act_quick_none: TTntAction;
    act_quick_auto: TTntAction;
    act_quick_text: TTntAction;
    act_quick_image: TTntAction;
    act_quick_hex: TTntAction;
    act_help_home: TTntAction;
    act_help_forum: TTntAction;
    act_help_about: TTntAction;
    act_view_quickview: TTntAction;
    act_edit_duplicate: TTntAction;
    act_navi_texteditor: TTntAction;
    act_navi_filesearch: TTntAction;
    ApplicationEvents: TApplicationEvents;
    act_tools_mapdrive: TTntAction;
    act_tools_disconnectdrive: TTntAction;
    act_navi_back: TCEToolbarAction;
    act_navi_forward: TCEToolbarAction;
    act_edit_newfile: TCEToolbarAction;
    act_edit_copypath: TCEToolbarAction;
    act_view_showhints: TTntAction;
    act_tools_showcustomizer: TTntAction;
    act_view_hiddenfiles: TTntAction;
    act_edit_newfolder: TTntAction;
    act_view_showheaderalways: TTntAction;
    act_tools_cmd: TTntAction;
    act_view_filters: TTntAction;
    act_view_statusbar: TTntAction;
    act_view_fullscreen: TTntAction;
    BackgroundCMItems_up: TTntPopupMenu;
    BackgroundCMItems_down: TTntPopupMenu;
    View1: TTntMenuItem;
    LargeIcons1: TTntMenuItem;
    SmallIcons1: TTntMenuItem;
    List1: TTntMenuItem;
    Details1: TTntMenuItem;
    iles1: TTntMenuItem;
    humbnails1: TTntMenuItem;
    Filmstrip1: TTntMenuItem;
    N1: TTntMenuItem;
    Refresh1: TTntMenuItem;
    N2: TTntMenuItem;
    Paste1: TTntMenuItem;
    Properties1: TTntMenuItem;
    CopyPath1: TTntMenuItem;
    N3: TTntMenuItem;
    act_view_showextensions: TTntAction;
    N4: TTntMenuItem;
    act_edit_paste_shortcut: TTntAction;
    PasteShortcut1: TTntMenuItem;
    act_help_poedit_form: TTntAction;
    act_view_loadskin: TTntAction;
    act_tools_showoptions: TTntAction;
    act_sessions_save: TTntAction;
    act_sessions_manage: TTntAction;
    act_view_dropstack: TTntAction;
    MenuItem_ArragneBy: TTntMenuItem;
    act_view_arrangeby: TCEToolbarAction;
    act_view_viewstyle: TCEToolbarAction;
    act_view_groupby: TCEToolbarAction;
    MenuItem_GroupBy: TTntMenuItem;
    act_navi_scrollleft: TTntAction;
    act_navi_scrollright: TTntAction;
    act_view_alwaysontop: TTntAction;
    act_tabs_closetab: TTntAction;
    act_tabs_closeothertabs: TTntAction;
    act_tabs_addtab: TTntAction;
    act_tabs_duplicatetab: TTntAction;
    act_tabs_closeonleft: TTntAction;
    act_tabs_closeonright: TTntAction;
    act_gen_menu: TCEToolbarAction;
    act_navi_quickview: TTntAction;
    act_help_donate: TTntAction;
    act_sessions_addhistoryitem: TTntAction;
    act_sessions_clearhistory: TTntAction;
    act_bookmarks_menu: TCEToolbarAction;
    act_sessions_menu: TCEToolbarAction;
    act_view_infobar: TTntAction;
    act_edit_create_symlink: TTntAction;
    CreateSymbolicLink1: TTntMenuItem;
    N5: TTntMenuItem;
    act_sessions_enablehistory: TTntAction;
    act_gen_showhide: TTntAction;
    act_tabs_undo: TCEToolbarAction;
    act_tabs_next: TTntAction;
    act_tabs_prev: TTntAction;
    act_focus_addressbar: TTntAction;
    act_filters_menu: TCEToolbarAction;
    act_filters_pattern: TCEToolbarAction;
    act_filters_clear: TTntAction;
    act_view_checkbox_selection: TTntAction;
    act_help_versionmgr: TTntAction;
    act_view_archiver: TTntAction;
    act_gen_new_instance: TTntAction;
    act_help_checkupdates: TTntAction;
    act_edit_undo_delete: TCEToolbarAction;
    act_tools_emptytrash: TTntAction;
    act_tools_systempower: TCEToolbarAction;
    act_view_lock_toolbars: TTntAction;
    act_navi_refresh_current: TTntAction;
    act_filters_strict: TTntAction;
    act_filters_exclude: TTntAction;
    NewFolder1: TTntMenuItem;
    act_help_restore_layout: TTntAction;
    act_tabs_menu: TCEToolbarAction;
    act_edit_newemptyfile: TTntAction;
    act_stack_open: TCEToolbarAction;
    act_stack_save: TCEToolbarAction;
    act_stack_remove: TTntAction;
    act_stack_clear: TTntAction;
    act_stack_allowmove: TTntAction;
    procedure ActionExecute(Sender: TObject);
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure BackgroundCMItems_upPopup(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fPageActionList: TTntActionList;
    { Private declarations }
  protected
    procedure DoAssigneByClick(Sender: TObject);
    procedure DoGroupByClick(Sender: TObject);
  public
    GlobalHotkeys: TCEGlobalHotkeys;
    HotkeySettings: TCEHotkeySettings;
    procedure AssignCustomToolbarActions;
    procedure DoGlobalContextMenuCmd(Namespace: TNamespace; Verb: WideString;
        MenuItemID: Integer;  var Handled: Boolean);
    procedure DoGlobalContextMenuShow(Namespace: TNamespace; Menu: hMenu; var
        Allow: Boolean);
    procedure UpdateAll;
    property PageActionList: TTntActionList read fPageActionList write
        fPageActionList;
    { Public declarations }
  end;


// Global
procedure ExecuteCEAction(ActionID: Integer);
procedure UpdateCEAction(ActionID: Integer; TargetAction: TTntAction);

procedure ExecuteEditCategory(ActionID: Integer);
procedure ExecuteSessionsCategory(ActionID: Integer);
procedure ExecuteTabsCategory(ActionID: Integer);
procedure ExecuteHelpCategory(ActionID: Integer);
procedure ExecuteNavigationCategory(ActionID: Integer);
procedure ExecuteQuickviewCategory(ActionID: Integer);
procedure ExecuteBookmarksCategory(ActionID: Integer);

// Action Updates
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction:
    TTntAction);
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TTntAction);

procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);

procedure OpenFileInTab(FilePath: WideString; SelectTab: Boolean = true;
    ActivateApp: Boolean = false);

function OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab: Boolean
    = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem; overload;

function OpenFolderInTab(Sender: TObject; FilePath: WideString; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem; overload;

function IsSingleInstance: Boolean;

procedure ExecuteQuickOptionsCategory(ActionID: Integer);

procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TTntAction);

function HandleCmdParams(Str: WideString): Boolean;

procedure ExecuteViewCategory(ActionID: Integer);

procedure UpdateViewCategory(ActionID: Integer; TargetAction: TTntAction);

procedure UpdateAllActions;

procedure HandleInputMessage(var Msg : TMessage; var Handled: Boolean);

procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TTntAction);

procedure ExecuteToolsCategory(ActionID: Integer);

function ExecuteShortcut(Action: TAction): Boolean;

function FindActionByShortcut(AActionList: TActionList; AShortcut: TShortcut;
    AOffset: Integer = -1): TAction;

// Action Executes
procedure ExecuteGeneralCategory(ActionID: Integer);

function FindActionByName(AActionList: TActionList; AName: String): TAction;

procedure ExecuteFocusCategory(ActionID: Integer);

procedure ExecuteMiscCategory(ActionID: Integer);

procedure UpdateMiscCategory(ActionID: Integer; TargetAction: TTntAction);

procedure DoGlobalContextMenuShow(Sender: TObject; Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);

procedure DoGlobalContextMenuCmd(Sender: TObject; Namespace: TNamespace; Verb:
    WideString; MenuItemID: Integer; var Handled: Boolean);

function HandleExeCommands: Boolean;

var
  CEActions: TCEActions;
  fDisableSingleInstanceTemporarily: Integer = 0;

implementation

{$R *.dfm}

uses
  Main, fCE_FolderPanel, fCE_QuickViewPanel, fCE_BookmarkPanel,
  fCE_TextEditor, fCE_FileView, CE_FileView, 
  CE_Bookmarks, CE_BookmarkTree, fCE_AboutBox,
  CE_ToolbarButtons, fCE_Customizer, fCE_TabPage, fCE_FiltersPanel,
  fCE_PoEditor, fCE_OptionsDialog, CE_Sessions, fCE_StackPanel,
  CE_BaseFileView, fCE_QuickViewTab, fCE_SearchPage, fCE_CreateSymlink,
  fCE_VersionMgrForm, fCE_ArchivePanel, CE_CommonObjects, fCE_QuickView;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Module Create
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleCreate(Sender: TObject);
begin
  AssignCustomToolbarActions;
  // Hotkey Settings
  HotkeySettings:= TCEHotkeySettings.Create;
  HotkeySettings.Actions:= ActionList;
  GlobalAppSettings.AddItem('Hotkeys', HotkeySettings);
  // Global Hotkey Settings
  GlobalHotkeys:= TCEGlobalHotkeys.Create;
  GlobalHotkeys.Actions:= ActionList;
  GlobalHotkeys.MsgHandle:= MainForm.Handle;
  GlobalAppSettings.AddItem('GlobalHotkeys', GlobalHotkeys);
end;

{*------------------------------------------------------------------------------
  On Module Destroy
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleDestroy(Sender: TObject);
begin
  HotkeySettings.Free;
  GlobalHotkeys.Free;
end;

{*------------------------------------------------------------------------------
  Assign Custom Toolbar Actions
-------------------------------------------------------------------------------}
procedure TCEActions.AssignCustomToolbarActions;
begin
  act_navi_back.ItemClass:= TCEFileViewBackButton;
  act_navi_forward.ItemClass:= TCEFileViewForwardButton;
  act_edit_newfile.ItemClass:= TCENewFileButton;
  act_edit_copypath.ItemClass:= TCEFileViewCopyPathButton;
  act_view_arrangeby.ItemClass:= TCEArrangeByButton;
  act_view_viewstyle.ItemClass:= TCEViewStyleButton;
  act_view_groupby.ItemClass:= TCEGroupByButton;
  act_gen_menu.ItemClass:= TCEMainMenuButton;
  act_bookmarks_menu.ItemClass:= TCEBookmarksButton;
  act_sessions_menu.ItemClass:= TCESessionsButton;
  act_tabs_undo.ItemClass:= TCEClosedTabsListButton;
  act_filters_menu.ItemClass:= TCEFiltersMenuButton;
  act_filters_pattern.ItemClass:= TCEFilterPatternItem;
  act_edit_undo_delete.ItemClass:= TCEUndoDeleteButton;
  act_tools_systempower.ItemClass:= TCESystemPowerButton;
  act_tabs_menu.ItemClass:= TCETabsButton;
  act_stack_open.ItemClass:= TCEStackOpenButton;
  act_stack_save.ItemClass:= TCEStackSaveButton;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute action (ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.ActionExecute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  ExecuteCEAction(act.Tag);
  UpdateTimerTimer(Sender);
end;

{*------------------------------------------------------------------------------
  Get's called on Action Update timer (UPDATE ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateTimerTimer(Sender: TObject);
begin
  UpdateAll;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute Action.
-------------------------------------------------------------------------------}
procedure ExecuteCEAction(ActionID: Integer);
begin
  if csDestroying in MainForm.ComponentState then
  Exit;
  
  case ActionID of
    100..199: ExecuteGeneralCategory(ActionID);
    200..299: ExecuteEditCategory(ActionID);
    300..399: ExecuteViewCategory(ActionID);
    400..499: ExecuteToolsCategory(ActionID);
    500..599: ExecuteHelpCategory(ActionID);
    600..659: ExecuteNavigationCategory(ActionID);
    660..699: ExecuteTabsCategory(ActionID);
    700..799: ExecuteQuickviewCategory(ActionID);
    800..849: ExecuteBookmarksCategory(ActionID);
    850..899: ExecuteSessionsCategory(ActionID);
    900..949: ExecuteMiscCategory(ActionID);
    950..999: ExecuteFocusCategory(ActionID);
    1000..1100: ExecuteQuickOptionsCategory(ActionID);
  end;
end;

{*------------------------------------------------------------------------------
  Update Action
-------------------------------------------------------------------------------}
procedure UpdateCEAction(ActionID: Integer; TargetAction: TTntAction);
begin
  case ActionID of
    100..199: UpdateGeneralCategory(ActionID, TargetAction);
    200..299: UpdateEditCategory(ActionID, TargetAction);
    300..399: UpdateViewCategory(ActionID, TargetAction);
    400..499: UpdateToolsCategory(ActionID, TargetAction);
    500..599: UpdateHelpCategory(ActionID, TargetAction);
    600..659: UpdateNavigationCategory(ActionID, TargetAction);
    660..699: UpdateTabsCategory(ActionID, TargetAction);
    700..799: UpdateQuickviewCategory(ActionID, TargetAction);
    800..849: UpdateBookmarksCategory(ActionID, TargetAction);
    850..899: UpdateSessionsCategory(ActionID, TargetAction);
    900..949: UpdateMiscCategory(ActionID, TargetAction);
    1000..1100: UpdateQuickOptionsCategory(ActionID, TargetAction);
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  File Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteGeneralCategory(ActionID: Integer);
begin
  case ActionID of
    100: begin
      Application.MainForm.Close; // Exit
    end;
    101: MainForm.ToggleVisibility;
    102: begin
      if Application.MainForm.CloseQuery then
      begin
        MainForm.Shutdown;
        MainForm.SaveSettingsOnClose:= false;
        fDisableSingleInstanceTemporarily:= GetTickCount;
        ExecShellEx(WideParamStr(0), '', '', SW_SHOWNORMAL, false, false, true);
        Sleep(1000);
        Application.MainForm.Close;
      end;
    end;
    103: begin
      fDisableSingleInstanceTemporarily:= GetTickCount;
      WideShellExecute(0, 'open', WideParamStr(0), '', '', SW_SHOWNORMAL);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  File Category Update
-------------------------------------------------------------------------------}
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Edit Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteEditCategory(ActionID: Integer);
var
  fileview: TCEFileView;
  NS: TNamespace;
  s: String;
begin
  fileview:= nil;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201: CEFolderPanel.FolderTree.CopyToClipBoard;
      202: CEFolderPanel.FolderTree.CutToClipBoard;
      203: CEFolderPanel.FolderTree.PasteFromClipboard;
      204: CEFolderPanel.FolderTree.DeleteSelectedNodes;
      208: if CEFolderPanel.FolderTree.FocusedNode <> nil then
           CEFolderPanel.FolderTree.EditNode(CEFolderPanel.FolderTree.FocusedNode, -1);
      213: CEFolderPanel.FolderTree.PasteShortcutFromClipboard;
    end;
  end
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201: fileview.CopyToClipboard;
        202: fileview.CutToClipboard;
        203: fileview.PasteFromClipboard;
        204: fileview.SelectedFilesDelete;
        208: if fileview.Selection.FocusedItem <> nil then
             fileview.Selection.FocusedItem.Edit;
        209: begin
               fileview.CopyToClipboard;
               fileview.PasteFromClipboard;
             end;
        213: fileview.PasteShortcutFromClipboard;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205: fileview.Selection.SelectAll;
      206: fileview.Selection.Invert;
      207: begin
             if fileview.ValidateNamespace(fileview.Selection.First, NS) then
             NS.ShowPropertySheetMulti(MainForm, fileview.SelectedToNamespaceArray, false)
             else
             fileview.RootFolderNamespace.ShowPropertySheet(MainForm);
           end;
      211: begin
             try
               if fileview.Selection.Count > 1 then
               TntClipboard.AsText:= fileview.SelectedPaths.Text
               else if fileview.Selection.Count = 1 then
               TntClipboard.AsText:= fileview.SelectedPath
               else
               TntClipboard.AsText:= IncludeTrailingBackslashW(fileview.RootFolderNamespace.NameForParsing);
             except
               s:= SysErrorMessage(GetLastError);
               MessageBox(0, PChar(s), 'Clipboard error!', MB_ICONERROR or MB_OK);
             end;
           end;
      212: fileview.CreateNewFolder;
      214: ShowCreateSymlinkDialog(fileview.RootFolderNamespace.NameForParsing, '');
      215: CERecycleBinCtrl.RestoreLastDeleted;
      217: fileview.CreateEmptyFile;
    end;
  end;

end;

{*------------------------------------------------------------------------------
  Edit Category Update
-------------------------------------------------------------------------------}
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TTntAction);
var
  fileview: TCEFileView;
  NS: TNamespace;
begin
  TargetAction.Enabled:= false;
  fileview:= nil;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201,202,204: begin
        TargetAction.Enabled:= CEFolderPanel.FolderTree.SelectedCount > 0;
      end;
      203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
      208: begin
             if CEFolderPanel.FolderTree.FocusedNode <> nil then
             begin
               if CEFolderPanel.FolderTree.ValidateNamespace(CEFolderPanel.FolderTree.FocusedNode, NS) then
               TargetAction.Enabled:= NS.CanRename
               else
               TargetAction.Enabled:= false
             end;
           end;
    end;
  end
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201,202,204,209:
          TargetAction.Enabled:=  fileview.Selection.Count > 0;
        203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
        208: begin
               if fileview.Selection.FocusedItem <> nil then
               begin
                 if fileview.ValidateNamespace(fileview.Selection.FocusedItem, NS) then
                 TargetAction.Enabled:= NS.CanRename
                 else
                 TargetAction.Enabled:= false
               end;
             end;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205,206,207,210,211,212,217: TargetAction.Enabled:= true;
      214: TargetAction.Enabled:= (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4);
      215: TargetAction.Enabled:= not CERecycleBinCtrl.IsRecycleBinEmpty;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Focus Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteFocusCategory(ActionID: Integer);
begin
  case ActionID of
    951: if MainForm.AddressBarToolbar.Visible then
         begin
           if MainForm.AddressBarToolbar.AddressBar.Breadcrumb then
           begin
             MainForm.AddressBarToolbar.AddressBar.AutoSwitchToBreadcrumb:= true;
             MainForm.AddressBarToolbar.AddressBar.Breadcrumb:= false;
           end;
           MainForm.AddressBarToolbar.AddressBar.TextEditor.SetFocus;
           MainForm.AddressBarToolbar.AddressBar.TextEditor.SelectAll;
         end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  View Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteViewCategory(ActionID: Integer);
begin
  case ActionID of
    300: MainForm.StatusBar.Visible:= not MainForm.StatusBar.Visible;
    301: if GetFormVisible(CEFolderPanel) then HideDockForm(CEFolderPanel) else ShowDockForm(CEFolderPanel);
    302: if GetFormVisible(CEBookmarkPanel) then HideDockForm(CEBookmarkPanel) else ShowDockForm(CEBookmarkPanel);
    303: if GetFormVisible(CEQuickviewPanel) then HideDockForm(CEQuickviewPanel) else ShowDockForm(CEQuickviewPanel);
    304: if GetFormVisible(CEFiltersPanel) then HideDockForm(CEFiltersPanel) else ShowDockForm(CEFiltersPanel);
    305: if GetFormVisible(CEStackPanel) then HideDockForm(CEStackPanel) else ShowDockForm(CEStackPanel);
//    306: if GetFormVisible(CEArchiverPanel) then HideDockForm(CEArchiverPanel) else ShowDockForm(CEArchiverPanel);
    330: MainForm.ShowHint:= not MainForm.ShowHint;
    332: begin
      GlobalFileViewSettings.HiddenFiles:= not GlobalFileViewSettings.HiddenFiles;
      CEFolderPanel.FolderTree.HiddenFiles:= GlobalFileViewSettings.HiddenFiles;
    end;
    333: GlobalFileViewSettings.ShowHeaderAlways:= not GlobalFileViewSettings.ShowHeaderAlways;
    334: GlobalFileViewSettings.ShowExtensions:= not GlobalFileViewSettings.ShowExtensions;
    335: if MainForm.FormStyle = fsStayOnTop then
         MainForm.FormStyle:= fsNormal
         else
         MainForm.FormStyle:= fsStayOnTop;
    336: GlobalFileViewSettings.ShowInfoBar:= not GlobalFileViewSettings.ShowInfoBar;
    337: GlobalFileViewSettings.CheckBoxSelection:= not GlobalFileViewSettings.CheckBoxSelection;
    351..358: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle:= TEasyListStyle(Ord(ActionID - 351));
              end;
    370: MainForm.Fullscreen:= not MainForm.Fullscreen;
    371: MainForm.OpenSkin;
    390: MainForm.LockToolbars:= not MainForm.LockToolbars;
  end;
end;

{*------------------------------------------------------------------------------
  View Category Update
-------------------------------------------------------------------------------}
procedure UpdateViewCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    300: TargetAction.Checked:= MainForm.StatusBar.Visible;
    301: TargetAction.Checked:= CEFolderPanel.IsVisible;
    302: TargetAction.Checked:= CEBookmarkPanel.IsVisible;
    303: TargetAction.Checked:= CEQuickviewPanel.IsVisible;
    304: TargetAction.Checked:= CEFiltersPanel.IsVisible;
    305: TargetAction.Checked:= CEStackPanel.IsVisible;
//    306: TargetAction.Checked:= CEArchiverPanel.IsVisible;
    330: TargetAction.Checked:= MainForm.ShowHint;
    332: TargetAction.Checked:= GlobalFileViewSettings.HiddenFiles;
    333: TargetAction.Checked:= GlobalFileViewSettings.ShowHeaderAlways;
    334: TargetAction.Checked:= GlobalFileViewSettings.ShowExtensions;
    335: TargetAction.Checked:= MainForm.FormStyle = fsStayOnTop;
    336: TargetAction.Checked:= GlobalFileViewSettings.ShowInfoBar;
    337: TargetAction.Checked:= GlobalFileViewSettings.CheckBoxSelection;
    351..358: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                if ActionID = (Ord(TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle) + 351) then
                begin
                  TargetAction.Checked:= true;
                  CEActions.act_view_viewstyle.ImageIndex:= TargetAction.ImageIndex;
                end
                else
                TargetAction.Checked:= false;
              end
              else
              begin
                TargetAction.Enabled:= false;
                TargetAction.Checked:= false;
              end;
    370: TargetAction.Checked:= MainForm.Fullscreen;
    372..374: TargetAction.Enabled:= GlobalPathCtrl.ActivePage is TCEFileViewPage;
    390: TargetAction.Checked:= MainForm.LockToolbars;
  end;
end;



{##############################################################################}

{*------------------------------------------------------------------------------
  Tools Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteToolsCategory(ActionID: Integer);
var
  ws: WideString;
begin
  case ActionID of
    401: ShowCustomizer(MainForm);
    402: ShowOptionsDialog;
    451: WNetConnectionDialog(MainForm.Handle, RESOURCETYPE_DISK);
    452: WNetDisconnectDialog(MainForm.Handle, RESOURCETYPE_DISK);
    453: EmptyRecycleBin;
    454: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           if not WideDirectoryExists(ws) then
           ws:= '';
           Tnt_ShellExecuteW(0,'open','cmd.exe','',PWideChar(ws), SW_SHOW);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Tools Category Update
-------------------------------------------------------------------------------}
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    453: begin
      TargetAction.Enabled:= not CERecycleBinCtrl.IsRecycleBinEmpty;
      if TargetAction.Enabled then
      TargetAction.ImageIndex:= 24
      else
      TargetAction.ImageIndex:= 23;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Help Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteHelpCategory(ActionID: Integer);
var
  form: TCEPoEditorForm;
  h: HWND;
begin
  case ActionID of
    501: ShellExecute(0,'open','http://www.cubicreality.com','','',SW_NORMAL);
    502: ShellExecute(0,'open','http://www.cubicreality.com/forum','','',SW_NORMAL);
    503: ShowAboutBox;
    504: begin
      form:= TCEPoEditorForm.CreateNew(MainForm,0);
      form.PoEditor.LocaleDir:= exePath + 'Locale\';
      form.PoEditor.OnTranslateUI:= MainForm.TranslateUI;
      if WideFileExists(exePath + 'Locale\default.pot') then
      form.PoEditor.POTFile.LoadFromFile(exePath + 'Locale\default.pot');
      form.PoEditor.TabControl.ActiveTabIndex:= 0;
      form.PoEditor.POT_Rev:= GetFileVersionBuild(WideParamStr(0));
      form.PoEditor.ActiveLanguage:= MainForm.ActiveLanguage;
      form.Show;
    end;
    505: ShellExecute(0,'open','http://www.cubicreality.com/donate/','','',SW_NORMAL);
    506: ShowVersionManager;
    507: MainForm.AutoUpdater.CheckForUpdates(true);
    508: begin
      if assigned(CEOptionsDialog) then
      h:= CEOptionsDialog.Handle
      else
      h:= MainForm.Handle;
      if WideMessageBox(h, _('Restore Default Layout'),
                        _('Are you sure you want to restore default layout?'),
                        MB_ICONQUESTION or MB_YESNO) = idYes then
      begin
        MainForm.Layouts.LoadDefaultLayout;
        if assigned(MainForm.TabSet.ActiveTab) then
        begin
          MainForm.Layouts.LoadLayout(MainForm.TabSet.ActiveTab.Page.Layout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberInnerToolbarLayout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberOuterToolbarLayout,
                                      MainForm.TabSet.ActiveTab.Page.Settings.RememberPanelLayout,
                                      True);
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Help Category Update
-------------------------------------------------------------------------------}
procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  case ActionID of
    507: TargetAction.Enabled:= not MainForm.AutoUpdater.CheckingUpdate;
    else
    TargetAction.Enabled:= true;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Misc Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteMiscCategory(ActionID: Integer);
begin
  case ActionID of
    901: CEFiltersPanel.ClearFilters;
    902: CEFiltersPanel.Filters.UseWildcards:= not 
      CEFiltersPanel.Filters.UseWildcards;
    903: CEFiltersPanel.Filters.ExcludeFromResults:= not
      CEFiltersPanel.Filters.ExcludeFromResults;
    // stack
    923: CEStackPanel.StackTree.DeleteSelectedNodes;
    924: CEStackPanel.ClearList;
    925: CEStackPanel.StackTree.SafeOperationsOnly:= not CEStackPanel.StackTree.SafeOperationsOnly;
  end;
end;


{-------------------------------------------------------------------------------
  Update Misc Category
-------------------------------------------------------------------------------}
procedure UpdateMiscCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    902: TargetAction.Checked:= CEFiltersPanel.Filters.UseWildcards;
    903: TargetAction.Checked:= CEFiltersPanel.Filters.ExcludeFromResults;
    // stack
    923: TargetAction.Enabled:= CEStackPanel.StackTree.SelectedCount > 0;
    924: TargetAction.Enabled:= CEStackPanel.StackTree.RootNode.ChildCount > 0;
    925: TargetAction.Checked:= not CEStackPanel.StackTree.SafeOperationsOnly;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Navigation Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteNavigationCategory(ActionID: Integer);
var
  page: TCECustomTabPage;
  ws,ext: WideString;
  ns: TNamespace;
  item: TEasyItem;
  editor: TCETextEditorPage;
  quickview: TCEQuickViewPage;
begin
  case ActionID of
    601: begin
           ExecuteTabsCategory(663);
         end;
    602: begin
           MainForm.TabSet.CloseSelectedTab;
         end;
    603: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoBackInHistory;
         end;
    604: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoForwardInHistory;
         end;
    605: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoFolderUp;
         end;
    606: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Rebuild;
           CEFolderPanel.FolderTree.Refresh;
           MainForm.DriveToolbar.Populate;
           MainForm.StatusBar.UpdateLabels(true);
         end;
    607: begin
           ExecuteTabsCategory(664);
         end;
    608: MainForm.TabSet.ScrollLeft;
    609: MainForm.TabSet.ScrollRight;
    610: begin
      if GlobalPathCtrl.ActivePage is TCEFileViewPage then
      TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Rebuild;
    end;
    // Open Editor
    650: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if (GlobalPathCtrl.ActivePage is TCEFileViewPage) then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCESearchPage) then
           begin
             item:= TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.Selection.First;
             if TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;

           editor:= TCETextEditorPage(MainForm.TabSet.AddTab(TCETextEditorPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           if (ws <> '') then
           begin
            ext:= WideUpperCase(WideExtractFileExt(ws));
            if (ext <> '.EXE') and (ext <> '.DLL') then
            editor.OpenDocument(ws);
           end;
         end;
    // Open Search
    651: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           page:= TCECustomTabPage(MainForm.TabSet.AddTab(TCESearchPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           if WideDirectoryExists(ws) then
           begin
             TCESearchPage(page).edit_location.Text:= ws;
           end;
         end;
    // Open QuickView
    652: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCESearchPage) then
           begin
             item:= TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.Selection.First;
             if TCESearchPage(GlobalPathCtrl.ActivePage).ResultView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;
           
           quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, MainForm.TabSet.Settings.NewTabSelect).Page);
           if ws <> '' then
           quickview.OpenFile(ws);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Navigation Category  Update
-------------------------------------------------------------------------------}
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TTntAction);
var
  L,R: Boolean;
begin
  case ActionID of
    //602: TargetAction.Enabled:= MainForm.TabSet.TabCount > 1;
    603: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasBackItems
         else
         TargetAction.Enabled:= false;
    604: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasNextItems
         else
         TargetAction.Enabled:= false;
    605: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= (TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.Parent <> nil)
         else
         TargetAction.Enabled:= false;
    606,607: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= True
         else
         TargetAction.Enabled:= false;
    608, 609: begin
      MainForm.TabSet.ScrollState(L, R);
      if L or R then
      begin
        if not CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= true;
          CEActions.act_navi_scrollright.Visible:= true;
        end;
        CEActions.act_navi_scrollleft.Enabled:= L;
        CEActions.act_navi_scrollright.Enabled:= R;
      end
      else
      begin
        if CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= false;
          CEActions.act_navi_scrollright.Visible:= false;
        end;
      end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Tabs Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteTabsCategory(ActionID: Integer);
var
  pidl: PItemIDList;
begin
  if not assigned(MainForm.TabSet.ActivePopupTab) then
  begin
    MainForm.TabSet.ActivePopupTab:= MainForm.TabSet.GetActiveTab;
  end;

  case ActionID of
    661: MainForm.TabSet.CloseTab(MainForm.TabSet.ActivePopupTab);
    662: MainForm.TabSet.CloseAllTabs(MainForm.TabSet.ActivePopupTab);
    663: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if MainForm.TabSet.Settings.NewTabType = 1 then // Clone active tab
           begin
             if GlobalPathCtrl.ActivePage is TCEFileViewPage then
             begin
              if MainForm.TabSet.Toolbar.GetTabsCount(true) = 0 then
              pidl:= DrivesFolder.AbsolutePIDL
              else
              pidl:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL
             end
             else
             pidl:= nil;
             OpenFolderInTab(nil, pidl, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end
           else if MainForm.TabSet.Settings.NewTabType = 3 then // Open custom path
           begin
             OpenFolderInTab(nil, MainForm.TabSet.Settings.NewTabNamespace.AbsolutePIDL, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end
           else // Open desktop
           begin
             OpenFolderInTab(nil, nil, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end;
         end;
    664: begin
           if assigned(MainForm.TabSet.ActivePopupTab) then
           begin
             if MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage then
             OpenFolderInTab(MainForm.TabSet.ActivePopupTab,
                             TCEFileViewPage(MainForm.TabSet.ActivePopupTab.Page).FileView.RootFolderNamespace.AbsolutePIDL, MainForm.TabSet.Settings.NewTabSelect, false, true);
           end;
         end;
    665: MainForm.TabSet.CloseTabsOnLeft(MainForm.TabSet.ActivePopupTab);
    666: MainForm.TabSet.CloseTabsOnRight(MainForm.TabSet.ActivePopupTab);
    667: MainForm.TabSet.UndoTabClose;
    668: MainForm.TabSet.SelectNextTab(true);
    669: MainForm.TabSet.SelectNextTab(false);
  end;

  MainForm.TabSet.ActivePopupTab:= nil; // popup has closed so set this to nil
end;

{*------------------------------------------------------------------------------
  Tabs Category  Update
-------------------------------------------------------------------------------}
procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  case ActionID of
    661..662,664: TargetAction.Enabled:= assigned(MainForm.TabSet.ActivePopupTab) or (MainForm.TabSet.GetActiveTab <> nil);
    667: TargetAction.Enabled:= MainForm.TabSet.CanUndoTabClose;
    670: TargetAction.Enabled:= MainForm.TabSet.TabCount > 1;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quickview Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickviewCategory(ActionID: Integer);
begin

end;

{*------------------------------------------------------------------------------
  Quickview Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TTntAction);
begin

end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Bookmarks Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteBookmarksCategory(ActionID: Integer);
begin
/// TODO: What's going on here?

//
//  case ActionID of
//    801: MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,amAddChildLast,nil, 'Category', true);
//    802: begin
//           if GlobalPathCtrl.ActiveFileView <> nil then
//           begin
//             MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,
//                                                  amAddChildLast,
//                                                  PIDLMgr.CopyPIDL(GlobalPathCtrl.ActiveFileView.RootFolderNamespace.AbsolutePIDL));
//           end;
//         end;
//    803: MainForm.BookmarkTree.EditNode(MainForm.BookmarkTree.FocusedNode, 0);
//    804: begin
//           MainForm.BookmarkTree.SafeDeleteSelectedNodes;
//         end;
//  end;
end;

{*------------------------------------------------------------------------------
  Bookmarks Category Update
-------------------------------------------------------------------------------}
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
end;


{##############################################################################}

{*------------------------------------------------------------------------------
  Session Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteSessionsCategory(ActionID: Integer);
begin
  case ActionID of
    851: GlobalSessions.SaveSessionDlg;
    852: GlobalSessions.ShowSessionManager;
    853: GlobalSessions.AddHistorySession;
    854: GlobalSessions.ClearHistory;
    855: GlobalSessions.AutoSaveHistory:= not GlobalSessions.AutoSaveHistory;
  end;
end;

{*------------------------------------------------------------------------------
  Session Category Update
-------------------------------------------------------------------------------}
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  if ActionID = 855 then
  TargetAction.Checked:= GlobalSessions.AutoSaveHistory;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Mouse Input actions
-------------------------------------------------------------------------------}
procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);
begin

// No need for this anymore, AppCommand is used instead.
//
//  if (Msg.message = 523) or (Msg.message = 525) then // Navigation buttons
//  begin
//    Shift:= Lo(Msg.wParam);
//    if Shift = MK_XBUTTON1 then  // back
//    ExecuteNavigationCategory(603)
//    else if Shift = MK_XBUTTON2 then // forward
//    ExecuteNavigationCategory(604);
//  end
end;

{*------------------------------------------------------------------------------
  Open file in a new tab
-------------------------------------------------------------------------------}
procedure OpenFileInTab(FilePath: WideString; SelectTab: Boolean = true;
    ActivateApp: Boolean = false);
var
  editor: TCETextEditorPage;
  quickview: TCEQuickViewPage;
begin
  if WideFileExists(FilePath) then
  begin
    GlobalFileViewSettings.AssignFromActivePage;

    if GlobalQuickViewSettings.IsSupported(WideExtractFileExt(FilePath), true) then
    begin
      quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, SelectTab).Page);
      quickview.OpenFile(FilePath);
    end
    else
    begin
      editor:= TCETextEditorPage(MainForm.TabSet.AddTab(TCETextEditorPage, SelectTab).Page);
      editor.OpenDocument(FilePath);
    end;

    if ActivateApp then
    MainForm.MakeVisible;
  end;
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
function OpenFolderInTab(Sender: TObject; FilePath: WideString; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem;
var
  PIDL: PItemIDList;
begin
  PIDL:= PathToPIDL(FilePath);

  Result:= OpenFolderInTab(Sender, PIDL, SelectTab, ActivateApp, ForceNewTab, PaneNumber);
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
function OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab: Boolean
    = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false;
    PaneNumber: Integer = 0): TCESpTabItem;
var
  page: TCEFileViewPage;
  eItem: TExplorerItem;
  i: Integer;
  ns: TNamespace;
  isFolder: Boolean;
  browsePIDL: PItemIDList;
begin
  ns:= TNamespace.Create(PIDL, nil);
  ns.FreePIDLOnDestroy:= false;
  isFolder:= ns.FileSystem and ns.Folder;

  if not isFolder then
  begin
    browsePIDL:= FindBrowseableRootPIDL(ns);
  end
  else
  begin
    browsePIDL:= PIDLMgr.CopyPIDL(PIDL);
  end;

  try
    Result:= nil;
    GlobalFileViewSettings.AssignFromActivePage;
    if MainForm.TabSet.Settings.ReuseTabs and not ForceNewTab then
    begin
      for i:= 0 to MainForm.TabSet.Items.Count -1 do
      begin
        if MainForm.TabSet.Items.Items[i] is TCESpTabItem then
        begin
          Result:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
          if Result.Page is TCEFileViewPage then
          begin
            page:= TCEFileViewPage(Result.Page);
            if ILIsEqual(browsePIDL, page.FileView.RootFolderNamespace.AbsolutePIDL) then
            begin
              MainForm.TabSet.SelectTab(Result);
              if ActivateApp then
              MainForm.MakeVisible;
              break;
            end
            else
            Result:= nil;
          end
          else
          Result:= nil;
        end;
      end;
    end;

    if not assigned(Result) then
    begin
      Result:= MainForm.TabSet.AddTab(TCEFileViewPage, false, false);
      if assigned(Result) then
      begin
        page:= TCEFileViewPage(Result.Page);
        page.FileView.fChangeHistory:= false;
        page.FileView.Selection.ClearAll;
        page.FileView.RootFolderCustomPIDL:= browsePIDL;
        page.UpdateCaption;
        if page.FileView.Selection.First <> nil then
        page.FileView.Selection.First.MakeVisible(emvMiddle);
        page.FileView.ClearHistory;
        page.FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(browsePIDL),nil),true);
        page.FileView.fChangeHistory:= true;
        page.Active:= true;
        if SelectTab or (MainForm.TabSet.Toolbar.GetTabsCount(true) = 1) then
        MainForm.TabSet.SelectTab(Result);
        if ActivateApp then
        MainForm.MakeVisible;
      end;
    end;

    if not isFolder then
    begin
      if assigned(Result) then
      begin
        page:= TCEFileViewPage(Result.Page);
        eItem:= page.FileView.FindItemByPIDL(PIDL);
        if assigned(eItem) then
        begin
          page.FileView.Selection.ClearAll;
          eItem.Selected:= true;
          eItem.Focused:= true;
          eItem.MakeVisible(emvMiddle);
        end;
      end;
    end;

  finally
    ns.Free;
    PIDLMgr.FreeAndNilPIDL(browsePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Is Single Instance query
-------------------------------------------------------------------------------}
function IsSingleInstance: Boolean;
begin
  Result:= MainForm.SingleInstance;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quick Options Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickOptionsCategory(ActionID: Integer);
begin
  //
end;

{*------------------------------------------------------------------------------
  Quick Options Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction:
    TTntAction);
begin
  //
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle Command Line input
-------------------------------------------------------------------------------}
function HandleCmdParams(Str: WideString): Boolean;
var
  list: TTntStrings;
  i: Integer;
  pidl: PItemIDList;
  path, path2: WideString;
  TabOpened: Boolean;
  IsShortcut: Boolean;
  IsFile: Boolean;
  IsIcon: Boolean;
  NS: TNamespace;
begin
  Result:= false;
  list:= TTntStringList.Create;
  try
    list.Delimiter:= ',';
    list.StrictDelimiter:= true;
    list.DelimitedText:= Str;
    
    i:= 0;
    TabOpened:= false;
    IsShortcut:= false;
    IsFile:= false;
    IsIcon:= false;
    while i < list.Count do
    begin
      if IsSameText(list.Strings[i], '/idlist') and not TabOpened then
      begin
        i:= i + 1;
        if i < list.Count then
        begin
          pidl:= StringToPIDL(list.Strings[i]);
          if pidl <> nil then
          begin
            OpenFolderInTab(nil, pidl, true, true);
            Result:= true;
            TabOpened:= true;
          end;
        end;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/n') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/link') then
      begin
        TabOpened:= false;
        IsShortcut:= true;
        IsFile:= false;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/f') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= true;
        IsIcon:= false;
      end
      else if IsSameText(list.Strings[i], '/icon') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
        IsFile:= false;
        IsIcon:= true;
      end
      else if list.Strings[i] <> '' then
      begin
        path:= list.Strings[i];
        if IsIcon then
        begin
          if not WideFileExists(path) then
          path:= DecodeRelativePath(path);
          if WideFileExists(path) then
          begin
            try
              Application.Icon.LoadFromFile(path);
            except
            end;
          end;
          IsIcon:= false;
        end
        else
        begin
          // Open File
          if IsFile then
          begin
            if not WideFileExists(path) then
            path:= DecodeRelativePath(path);
            if WideFileExists(path) then
            begin
              OpenFileInTab(path, true, true);
              Result:= true;
              TabOpened:= true;
            end;
            IsFile:= false;
          end;
          // Open Shortcut
          if IsShortcut then
          begin
            if not WideFileExists(path) then
            path:= DecodeRelativePath(path);
            if WideFileExists(path) then
            begin
              NS:= TNamespace.CreateFromFileName(path);
              try
                if NS.Link then
                begin
                  OpenFolderInTab(nil, NS.ShellLink.TargetIDList, true, true);
                  Result:= true;
                  TabOpened:= true;
                end;
              finally
                NS.Free;
              end;
            end;
            IsShortcut:= false;
          end;
          // Open normal folder
          if not TabOpened then
          begin
            ReplaceSystemVariablePath(path);
            
            if path[1] = ':' then
            begin
              // Open Control Panel in explorer.exe. CE won't work properly with Vista+ control panels.
              if IsWindowsVista and (Pos('::{26EE0668-A00A-44D7-9371-BEB064C98683}', path) = 1) then
              begin
                path2:= WideExpandEnviromentString('%windir%\explorer.exe');

                ShellExecuteW(0, 'open', PWideChar(path2), PWideChar(path), '', SW_SHOWNORMAL);
                Result:= false;
              end
              else
              begin
                OpenFolderInTab(nil, path, true, true);
                Result:= true;
                TabOpened:= true;
              end;
            end
            else
            begin
              if not DirExistsVET(path, false) then
              path:= DecodeRelativePath(path);
              if DirExistsVET(path, false) then
              begin
                OpenFolderInTab(nil, path, true, true);
                Result:= true;
                TabOpened:= true;
              end
            end;
          end;
        end;
      end;
      i:= i + 1;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Update All Actions
-------------------------------------------------------------------------------}
procedure UpdateAllActions;
begin
  if assigned(CEActions) then
  CEActions.UpdateAll
end;

{-------------------------------------------------------------------------------
  Handle Input message
-------------------------------------------------------------------------------}
procedure HandleInputMessage(var Msg : TMessage; var Handled: Boolean);
var
  ws: WideString;
  h: HWND;
begin
  case msg.Msg of
    // Single instance question
    WM_SingleInstance: begin
      if ((GetTickCount - fDisableSingleInstanceTemporarily) > 30000) and IsSingleInstance then
      Msg.Result:= 0
      else
      Msg.Result:= -1;
      Handled:= true;
    end;
    // Copy Data for command line parameters
    WM_COPYDATA: begin
      ws:= PWideChar(TWMCopyData(Msg).CopyDataStruct.lpData);
      TWMCopyData(Msg).Result:= 0;
      HandleCmdParams(ws);
      Handled:= true;
    end;
    // Make CE Visible
    WM_MakeVisible: begin
      MainForm.MakeVisible;
    end;
    // Execute Action
    WM_ExecuteAction: begin
      ExecuteCEAction(Msg.WParam);
    end;
    // Admin command result
    WM_AdminResult: begin
      case Msg.WParam of
        // Register as Default File Manager
        100: begin
          if assigned(CEOptionsDialog) then
          h:= CEOptionsDialog.Handle
          else
          h:= MainForm.Handle;

          if Msg.LParam = 0 then
          WideMessageBox(h, _('Default File Manager'), _('Registered successfully!'), MB_ICONINFORMATION or MB_OK)
          else
          WideMessageBox(h, _('Default File Manager'), _('Registration failed!'), MB_ICONERROR or MB_OK);
        end;
        // UnRegister as Default File Manager
        101: begin
          if assigned(CEOptionsDialog) then
          h:= CEOptionsDialog.Handle
          else
          h:= MainForm.Handle;

          if Msg.LParam = 0 then
          WideMessageBox(h, _('Default File Manager'), _('Unregistered successfully!'), MB_ICONINFORMATION or MB_OK)
          else
          WideMessageBox(h, _('Default File Manager'), _('Unregistration failed!'), MB_ICONERROR or MB_OK);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Execute Shortcuts
-------------------------------------------------------------------------------}
function ExecuteShortcut(Action: TAction): Boolean;
begin
  if assigned(Action) then
  Result:= Action.Execute
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Find Action by shortcut
-------------------------------------------------------------------------------}
function FindActionByShortcut(AActionList: TActionList; AShortcut: TShortcut;
    AOffset: Integer = -1): TAction;
var
  i,i2: Integer;
  act: TAction;
begin
  Result:= nil;
  for i:= 0 to AActionList.ActionCount-1 do
  begin
    act:= TAction(AActionList.Actions[i]);
    if act.ShortCut = AShortcut then
    begin
      Result:= act;
      break;
    end
    else
    begin
      for i2:= 0 to act.SecondaryShortCuts.Count - 1 do
      begin
        if TShortCut(act.SecondaryShortCuts.Objects[i2]) = AShortcut then
        begin
          Result:= act;
          break;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  FindActionByName
-------------------------------------------------------------------------------}
function FindActionByName(AActionList: TActionList; AName: String): TAction;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to AActionList.ActionCount-1 do
  begin
    if AActionList.Actions[i].Name = AName then
    begin
      Result:= TAction(AActionList.Actions[i]);
      break;
    end;
  end;
end;

{##############################################################################}
// Global context menu items

{-------------------------------------------------------------------------------
  Do GlobalContextMenuShow
-------------------------------------------------------------------------------}
procedure DoGlobalContextMenuShow(Sender: TObject; Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);

var
  infoA: TMenuItemInfoA;
  infoW: TMenuItemInfoW;
  ws: WideString;
begin
  // Add "Open in new tab" item
  if Namespace.Folder then
  begin
    ws:= _('Open in new tab');
    if IsUnicode then
    begin
      FillChar(infoW, SizeOf(infoW), #0);
      infoW.cbSize:= SizeOf(infoW);
      infoW.fMask:= MIIM_TYPE or MIIM_ID or MIIM_STATE;
      infoW.fType:= MFT_STRING;
      infoW.dwTypeData:= PWideChar(ws);
      infoW.cch:= Length(ws) + 1;
      infoW.wID:= 664;
      InsertMenuItemW(Menu, 0, true, infoW);
    end
    else
    begin
      FillChar(infoA, SizeOf(infoA), #0);
      infoA.cbSize:= SizeOf(infoA);
      infoA.fMask:= MIIM_TYPE or MIIM_ID or MIIM_STATE;
      infoA.fType:= MFT_STRING;
      infoA.dwTypeData:= PChar(String(ws));
      infoA.cch:= Length(ws) + 1;
      infoA.wID:= 664;
      InsertMenuItemA(Menu, 0, true, infoA);
    end;

    // Add Separator
    FillChar(infoA, SizeOf(infoA), #0);
    infoA.cbSize:= SizeOf(infoA);
    infoA.fMask:= MIIM_TYPE or MIIM_ID;
    infoA.fType:= MFT_SEPARATOR;
    InsertMenuItemA(Menu, 1, true, infoA);
  end;
end;

{-------------------------------------------------------------------------------
  Do GlobalContextMenuCmd
-------------------------------------------------------------------------------}
procedure DoGlobalContextMenuCmd(Sender: TObject; Namespace: TNamespace; Verb:
    WideString; MenuItemID: Integer; var Handled: Boolean);
var
  item: TEasyItem;
  ns: TNamespace;
begin
  // Handle "Open in new tab"
  if MenuItemID = 664 then
  begin
    // quick hack to open multiple folders!
    if (Sender is TCECustomFileView) and
       (TCECustomFileView(Sender).Selection.Count > 0) then
    begin
      item:= TCECustomFileView(Sender).Selection.First;
      while assigned(item) do
      begin
        if TCECustomFileView(Sender).ValidateNamespace(item, ns) then
        begin
          if ns.Folder then
          begin
            OpenFolderInTab(Sender, ns.AbsolutePIDL,
              MainForm.TabSet.Settings.OpenTabSelect);
          end;
        end;
        item:= TCECustomFileView(Sender).Selection.Next(item);
      end;
    end
    else
    OpenFolderInTab(Sender, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
  end;
end;

{-------------------------------------------------------------------------------
  Handle Exe Commands (Runs before UI has been created!
    If Result=true, CE will terminate after this function)
-------------------------------------------------------------------------------}
function HandleExeCommands: Boolean;
var
  doShell: Boolean;
  ws, path: WideString;
  index, count: Integer;
begin
  Result:= false;
  if IsWindowsVista then // NOTICE: at the moment there's nothing to do prior to vista.
  begin
    doShell:= false;
    count:= WideParamCount;
    for index:= 1 to count do
    begin
      ws:= WideParamStr(index);
      if not doShell then
      begin
        if ws = '/shell' then
        begin
          doShell:= true;
        end;
      end
      else
      begin
        if IsWindowsVista then
        begin
          // Control Panel
          if (Pos('::{26EE0668-A00A-44D7-9371-BEB064C98683}', ws) = 1) or
             (Pos('::{21EC2020-3AEA-1069-A2DD-08002B30309D}', ws) = 1) then
          begin
            path:= WideExpandEnviromentString('%windir%\explorer.exe');
            ShellExecuteW(0, 'open', PWideChar(path), PWideChar(ws), '', SW_SHOWNORMAL);
            Result:= true;
          end;
        end;
      end;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On Application Activate
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsActivate(Sender: TObject);
begin
  UpdateAll;
end;

{*------------------------------------------------------------------------------
  Application Messages
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  // Do nothing for now
//  case Msg.message of
//    512..525: MouseAction(Msg, Handled);
//  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On BackgroundCMItems_up Popup
-------------------------------------------------------------------------------}
procedure TCEActions.BackgroundCMItems_upPopup(Sender: TObject);
var
  item: TMenuItem;
  page: TCEFileViewPage;
  col: TEasyColumn;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    MenuItem_ArragneBy.Clear;
    page:= TCEFileViewPage(GlobalPathCtrl.ActivePage);
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoAssigneByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      if col.SortDirection <> esdNone then
      item.Checked:= true;
      MenuItem_ArragneBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_ArragneBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('More...');
    item.Tag:= -1;
    item.OnClick:= DoAssigneByClick;
    MenuItem_ArragneBy.Add(item);

    // Group By
      // Sho in Groups
    MenuItem_GroupBy.Clear;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('Show in Groups');
    item.Checked:= page.FileView.Grouped;
    item.Tag:= -2;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);
      // group by items
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoGroupByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      item.Checked:= page.FileView.GroupingColumn = col.Index;
      MenuItem_GroupBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('More...');
    item.Tag:= -1;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Do AssigneBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoAssigneByClick(Sender: TObject);
var
  item: TMenuItem;
  col, tmpCol: TEasyColumn;
  view: TCECustomFileView;
begin
  item:= TMenuItem(Sender);
  if item.Tag = -1 then
  begin
    if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    begin
      TCEFileViewPage(GlobalPathCtrl.ActivePage).ShowHeaderSelector;
    end;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    view:= TCECustomFileView(col.OwnerListview);
    view.BeginUpdate;
    try
      tmpCol:= view.Header.FirstColumn;
      while assigned(tmpCol) do
      begin
        if tmpCol <> col then
        tmpCol.SortDirection:= esdNone
        else
        tmpCol.SortDirection:= esdAscending;
        tmpCol:= view.Header.NextColumn(tmpCol);
      end;
    finally
      view.EndUpdate(true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GlobalContextMenuCmd
-------------------------------------------------------------------------------}
procedure TCEActions.DoGlobalContextMenuCmd(Namespace: TNamespace; Verb:
    WideString; MenuItemID: Integer;  var Handled: Boolean);
begin
  dCE_Actions.DoGlobalContextMenuCmd(MainForm, Namespace, Verb, MenuItemID, Handled);
end;


{-------------------------------------------------------------------------------
  Do GlobalContextMenuShow
-------------------------------------------------------------------------------}
procedure TCEActions.DoGlobalContextMenuShow(Namespace: TNamespace; Menu:
    hMenu; var Allow: Boolean);
begin
  dCE_Actions.DoGlobalContextMenuShow(MainForm, Namespace, Menu, Allow);
end;

{-------------------------------------------------------------------------------
  Do GroupBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoGroupByClick(Sender: TObject);
var
  item: TMenuItem;
  col: TEasyColumn;
  page: TCEFileViewPage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  page:= TCEFileViewPage(GlobalPathCtrl.ActivePage)
  else
  Exit;
  
  item:= TMenuItem(Sender);
  if item.Tag = -2 then
  begin
    page.FileView.Grouped:= not page.FileView.Grouped;
  end
  else if item.Tag = -1 then
  begin
    page.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    if assigned(col) then
    page.FileView.GroupingColumn:= col.Index;
  end;
end;

{-------------------------------------------------------------------------------
  Update All actions
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateAll;
var
  act: TTntAction;
  i: Integer;
begin
  for i:= 0 to ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(ActionList.Actions[i]);
    UpdateCEAction(act.Tag, act);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEHotkeySettings
-------------------------------------------------------------------------------}
constructor TCEHotkeySettings.Create;
begin
  inherited;
  fModifiedActions:= TObjectList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCEHotkeySettings
-------------------------------------------------------------------------------}
destructor TCEHotkeySettings.Destroy;
begin
  fModifiedActions.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCEHotkeySettings.Load(AAppStorage: TCEAppSettings; ANode: TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
  i: Integer;
  s: TShortcut;
  list: TStrings;
begin
  if not assigned(Actions) then
  Exit;
  
  if ANode.HasChildNodes then
  begin
    fModifiedActions.Clear;

    // load shortcuts
    list:= TStringList.Create;
    try
      list.Delimiter:= ',';
      // get action node
      chNode:= ANode.FirstChild;
      while assigned(chNode) do
      begin
        // find action
        act:= FindActionByName(Actions, chNode.NodeName);
        if assigned(act) then
        begin
          // clear default shortcuts
          act.ShortCut:= 0;
          act.SecondaryShortCuts.Clear;
          fModifiedActions.Add(act);
          // add shortcuts
          list.DelimitedText:= chNode.TextContent;
          for i:= 0 to list.Count - 1 do
          begin
            s:= StrToIntDef(list.Strings[i], 0);
            if (i = 0) or (act.ShortCut = 0) then
            act.ShortCut:= s
            else
            begin
              if s <> 0 then
              act.SecondaryShortCuts.AddObject(ShortCutToText(s), TObject(s));
            end;
          end;
        end;
        // get next action node
        chNode:= chNode.NextSibling;
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCEHotkeySettings.Save(AAppStorage: TCEAppSettings; ANode: TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
  i,i2: Integer;
  s: String;
begin
  if assigned(Actions) then
  begin
    // clear all first
    TDOMElementHack(ANode).FreeChildren;
    // loop modified actions
    for i:= 0 to ModifiedActions.Count - 1 do
    begin
      act:= TAction(ModifiedActions.Items[i]);
      // create node with action's name
      chNode:= AAppStorage.XML.CreateElement(act.Name);
      ANode.AppendChild(chNode);
      if act.ShortCut <> 0 then
      begin
        // convert shortcuts to string: '32,144,98...'
        s:= IntToStr(act.ShortCut);
        for i2:= 0 to act.SecondaryShortCuts.Count - 1 do
        s:= s + ',' + IntToStr(act.SecondaryShortCuts.ShortCuts[i2]);
        // add shortcuts to node's text content
        chNode.TextContent:= s;
      end;
    end;
  end;
end;



{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEGlobalHotkeys
-------------------------------------------------------------------------------}
constructor TCEGlobalHotkeys.Create;
begin
  inherited;
  fHotkeyActions:= TObjectList.Create(false);
  fHotkeys:= TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEGlobalHotkeys
-------------------------------------------------------------------------------}
destructor TCEGlobalHotkeys.Destroy;
begin
  Clear;
  fHotkeys.Free;
  fHotkeyActions.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Hotkey (Return index if RegisterHotKey succeeded, else -1)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.AddHotkey(AAction: TAction; AHotkey:
    TShortcut): Integer;
begin
  Result:= -1;
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);

  if assigned(AAction) and (AHotkey <> 0) then
  begin
    Result:= fHotkeyActions.Add(AAction);
    fHotkeys.Add(Pointer(AHotkey));
  end;
end;

{-------------------------------------------------------------------------------
  Can Register (returns true if can register)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.CanRegister(AAction: TTntAction; AHotkey: TShortcut):
    Boolean;
var
  key: Word;
  shift: TShiftState;
  i: Integer;
begin
  Result:= false;
  if assigned(AAction) and (AHotkey <> 0) then
  begin
    // Check if hotkey is already on the list
    for i:= 0 to fHotkeys.Count - 1 do
    begin
      if AHotkey = TShortcut(fHotkeys.Items[i]) then
      Exit;
    end;
    // Register Hotkey
    ShortCutToKey(AHotkey, key, shift);
    if RegisterHotKey(MsgHandle, Integer(AAction), ShiftState2Modifier(shift), key) then
    begin
      Result:= true;
      UnregisterHotKey(MsgHandle, Integer(AAction));
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Clear;
begin
  UnRegisterAll;
  fHotkeyActions.Clear;
  fHotkeys.Clear;
end;

{-------------------------------------------------------------------------------
  Delete Hotkey
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.DeleteHotkey(Index: Integer);
begin
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);
  
  if (Index > -1) and (Index < fHotkeyActions.Count) then
  begin
    fHotkeyActions.Delete(Index);
    fHotkeys.Delete(Index);
  end;
end;

{-------------------------------------------------------------------------------
  Execute Hotkey
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.ExecuteHotkey(id: Integer);
begin
  if (id > -1) and (id < fHotkeyActions.Count) then
  begin
    TAction(fHotkeyActions.Items[id]).Execute;
  end;
end;

{-------------------------------------------------------------------------------
  Get Action
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetAction(AIndex: Integer): TAction;
begin
  Result:= TAction(fHotkeyActions.Items[AIndex]);
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetCount: Integer;
begin
  Result:= fHotkeyActions.Count;
end;

{-------------------------------------------------------------------------------
  Get Hotkey (by index)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkey(AIndex: Integer): TShortcut;
begin
  Result:= Integer(fHotkeys.Items[AIndex]);
end;

{-------------------------------------------------------------------------------
  Get Hotkey (by action)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkey(AAction: TAction): TShortcut;
var
  i: Integer;
begin
  Result:= 0;
  i:= fHotkeyActions.IndexOf(AAction);
  if i > -1 then
  Result:= TShortcut(fHotkeys.Items[i]);
end;

{-------------------------------------------------------------------------------
  Get Hotkeys (Returns number of hotkeys found. Object field in the list will contain index of the hotkey)
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.GetHotkeys(AAction: TAction; AResults: TStrings):
    Integer;
var
  i: Integer;
begin
  Result:= 0;
  if assigned(AResults) then
  begin
    AResults.Clear;
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      if AAction = fHotkeyActions.Items[i] then
      begin
        AResults.AddObject(ShortCutToText(TShortcut(fHotkeys.Items[i])), TObject(i));
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Index Of
-------------------------------------------------------------------------------}
function TCEGlobalHotkeys.IndexOf(AAction: TAction): Integer;
begin
  Result:= fHotkeyActions.IndexOf(AAction);
end;

{-------------------------------------------------------------------------------
  Replace
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Replace(AIndex: Integer; AAction: TAction; AHotkey:
    TShortcut);
begin
  if IsRegistered then
  Raise Exception.CreateFmt('Unregister hotkeys before modifying list!', []);
  
  fHotkeyActions.Items[AIndex]:= AAction;
  fHotkeys.Items[AIndex]:= Pointer(AHotkey);
end;

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Load(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
var
  chNode: TDOMNode;
  act: TAction;
begin
  if not assigned(Actions) then
  Exit;
  
  UnRegisterAll;
  
  if ANode.HasChildNodes then
  begin
    // Clear default shortcuts
    Clear;
    // get action node
    chNode:= ANode.FirstChild;
    while assigned(chNode) do
    begin
      // find action
      act:= FindActionByName(Actions, chNode.NodeName);
      if assigned(act) then
      begin
        // add hotkey
        AddHotkey(act, StrToIntDef(chNode.TextContent, 0));
      end;
      // get next action node
      chNode:= chNode.NextSibling;
    end;
  end;
  RegisterAll;
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.Save(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
var
  chNode: TDOMNode;
  i: Integer;
begin
  // clear all first
  TDOMElementHack(ANode).FreeChildren;
  // loop actions
  for i:= 0 to fHotkeyActions.Count - 1 do
  begin
    // create node with action's name
    chNode:= AAppStorage.XML.CreateElement(TAction(fHotkeyActions.Items[i]).Name);
    ANode.AppendChild(chNode);
    // add hotkey to text content
    chNode.TextContent:= IntToStr(Integer(fHotkeys.Items[i]));
  end;
end;

{-------------------------------------------------------------------------------
  Register All
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.RegisterAll;
var
  i: Integer;
  key: Word;
  shift: TShiftState;
begin
  if not IsRegistered then
  begin
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      ShortCutToKey(TShortcut(fHotkeys.Items[i]), key, shift);
      RegisterHotKey(MsgHandle, i, ShiftState2Modifier(shift), key);
    end;
    fIsRegistered:= true;
  end;
end;

{-------------------------------------------------------------------------------
  UnRegister All
-------------------------------------------------------------------------------}
procedure TCEGlobalHotkeys.UnRegisterAll;
var
  i: Integer;
begin
  if IsRegistered then
  begin
    for i:= 0 to fHotkeyActions.Count - 1 do
    begin
      UnregisterHotKey(MsgHandle, i);
    end;
    fIsRegistered:= false;
  end;
end;

end.
