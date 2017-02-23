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
//  The Original Code is fCE_SearchPage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_SearchPage;

interface

uses
  // CE Units
  CE_VistaFuncs, CE_AppSettings, dCE_Images, CE_GlobalCtrl, CE_BaseFileView,
  CE_FileView, CE_LanguageEngine,
  // CE Frames
  fCE_TabPage, fCE_FileSearchDestDlg,
  // VSTools
  MPCommonObjects, EasyListview, VirtualExplorerEasyListview, VirtualTrees,
  VirtualExplorerTree, MPShellUtilities, MPCommonUtilities,
  // SpTBX
  SpTBXItem, SpTBXTabs, TB2Item, SpTBXSkins, SpTBXEditors, SpTBXControls,
  // FindFile
  FindFileW,
  // Tnt
  TntStdCtrls, TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ShlObj, Menus, SpTBXFormPopupMenu;

type
  TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);

  TCESearchPage = class(TCECustomTabPage)
    ResultView: TVirtualMultiPathExplorerEasyListview; 
    SearchPanel: TPanel;
    group_searchbuttons: TSpTBXGroupBox;
    but_search_start: TSpTBXButton;
    but_search_stop: TSpTBXButton;
    check_clear_before: TSpTBXCheckBox;
    CriteriaTabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabItem2: TSpTBXTabItem;
    SpTBXTabItem3: TSpTBXTabItem;
    SpTBXTabItem4: TSpTBXTabItem;
    SpTBXTabItem8: TSpTBXTabItem;
    sheet_size_attributes: TSpTBXTabSheet;
    group_size: TSpTBXGroupBox;
    spin_size_atleast: TSpTBXSpinEdit;
    combo_size_atleast: TSpTBXComboBox;
    check_size_atleast: TSpTBXCheckBox;
    spin_size_atmost: TSpTBXSpinEdit;
    combo_size_atmost: TSpTBXComboBox;
    check_size_atmost: TSpTBXCheckBox;
    group_attributes: TSpTBXGroupBox;
    check_attr_readonly: TSpTBXCheckBox;
    check_attr_hidden: TSpTBXCheckBox;
    check_attr_compressed: TSpTBXCheckBox;
    check_attr_system: TSpTBXCheckBox;
    sheet_content: TSpTBXTabSheet;
    SpTBXLabel4: TSpTBXLabel;
    memo_content: TTntMemo;
    check_content_wordwrap: TSpTBXCheckBox;
    check_content_case_sensitive: TSpTBXCheckBox;
    check_content_wholeword: TSpTBXCheckBox;
    sheet_filters: TSpTBXTabSheet;
    SpTBXLabel1: TSpTBXLabel;
    memo_filters_exclude: TTntMemo;
    memo_filters_include: TTntMemo;
    SpTBXLabel3: TSpTBXLabel;
    SpTBXLabel5: TSpTBXLabel;
    sheet_date_time: TSpTBXTabSheet;
    TabControl_DateTime: TSpTBXTabControl;
    SpTBXTabItem5: TSpTBXTabItem;
    SpTBXTabItem6: TSpTBXTabItem;
    SpTBXTabItem7: TSpTBXTabItem;
    sheet_modified: TSpTBXTabSheet;
    ModifiedBeforeTime: TDateTimePicker;
    ModifiedBeforeDate: TDateTimePicker;
    ModifiedAfterDate: TDateTimePicker;
    ModifiedAfterTime: TDateTimePicker;
    check_ModifiedBeforeDate: TSpTBXCheckBox;
    check_ModifiedBeforeTime: TSpTBXCheckBox;
    check_ModifiedAfterDate: TSpTBXCheckBox;
    check_ModifiedAfterTime: TSpTBXCheckBox;
    sheet_accessed: TSpTBXTabSheet;
    AccessedBeforeTime: TDateTimePicker;
    AccessedBeforeDate: TDateTimePicker;
    AccessedAfterDate: TDateTimePicker;
    AccessedAfterTime: TDateTimePicker;
    check_AccessedBeforeDate: TSpTBXCheckBox;
    check_AccessedBeforeTime: TSpTBXCheckBox;
    check_AccessedAfterDate: TSpTBXCheckBox;
    check_AccessedAfterTime: TSpTBXCheckBox;
    sheet_created: TSpTBXTabSheet;
    CreatedBeforeTime: TDateTimePicker;
    CreatedBeforeDate: TDateTimePicker;
    CreatedAfterDate: TDateTimePicker;
    CreatedAfterTime: TDateTimePicker;
    check_CreatedBeforeDate: TSpTBXCheckBox;
    check_CreatedBeforeTime: TSpTBXCheckBox;
    check_CreatedAfterDate: TSpTBXCheckBox;
    check_CreatedAfterTime: TSpTBXCheckBox;
    sheet_name_location: TSpTBXTabSheet;
    edit_filemask: TSpTBXEdit;
    SpTBXLabel2: TSpTBXLabel;
    check_subfolders: TSpTBXCheckBox;
    spin_minlevel: TSpTBXSpinEdit;
    check_minlevel: TSpTBXCheckBox;
    check_maxlevel: TSpTBXCheckBox;
    spin_maxlevel: TSpTBXSpinEdit;
    edit_wordphrase: TSpTBXEdit;
    radio_name_word: TSpTBXRadioButton;
    combo_extension: TSpTBXComboBox;
    radio_name_mask: TSpTBXRadioButton;
    panel_status: TSpTBXPanel;
    FolderTreePopup: TSpTBXFormPopupMenu;
    edit_location: TSpTBXButtonEdit;
    label_status: TTntLabel;
    timer_status: TTimer;
    check_attr_folder: TSpTBXCheckBox;
    procedure but_search_startClick(Sender: TObject);
    procedure but_search_stopClick(Sender: TObject);
    procedure check_content_wordwrapClick(Sender: TObject);
    procedure FolderTreePopupClosePopup(Sender: TObject; Selected: Boolean);
    procedure FolderTreePopupPopup(Sender: TObject);
    procedure check_dateClick(Sender: TObject);
    procedure check_size_Click(Sender: TObject);
    procedure radio_nameClick(Sender: TObject);
    procedure check_levelClick(Sender: TObject);
    procedure ResultViewColumnCustomView(Sender: TCustomEasyListview;
      Column: TEasyColumn; var View: TEasyViewColumnClass);
    procedure ResultViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResultViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResultViewItemContextMenu(Sender: TCustomEasyListview;
      HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu;
      var Handled: Boolean);
    procedure ResultViewContextMenuShow(
      Sender: TCustomVirtualExplorerEasyListview; Namespace: TNamespace;
      Menu: HMENU; var Allow: Boolean);
    procedure ResultViewContextMenuCmd(
      Sender: TCustomVirtualExplorerEasyListview; Namespace: TNamespace;
      Verb: WideString; MenuItemID: Integer; var Handled: Boolean);
    procedure timer_statusTimer(Sender: TObject);
    procedure ResultViewHintCustomInfo(Sender: TCustomEasyListview;
      TargetObj: TEasyCollectionItem; Info: TEasyHintInfo);
    procedure edit_wordphraseChange(Sender: TObject);
  private
    fStatus: WideString;
    fOpenFolderID: Integer;
    fDownShiftState: TShiftState;
    fFileCount: Integer;
    fLastButtonAnimChange: Integer;
    fLastStatusChange: Integer;
    fShowItemContextMenu: Boolean;
    fStartTime: Cardinal;
    function GetAttributeStatus(CB: TSpTBXCheckBox): TFileAttributeStatus;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    { Private declarations }
  protected
    procedure ChangeStatus(ANewStatus: WideString);
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure UpdateTheme;
  public
    DestDlg: TCEDestDlg;
    Find: TFindFileW;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleFileMatch(Sender: TObject; const FileInfo: TFileDetails);
    procedure HandleFolderChange(Sender: TObject; const Folder: WideString; var
        IgnoreFolder: TFolderIgnore);
    procedure HandleSearchAbort(Sender: TObject);
    procedure HandleSearchBegin(Sender: TObject);
    procedure HandleSearchFinish(Sender: TObject);
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SelectPage; override;
    procedure StartSearch;
    procedure UpdateCaption; override;
    { Public declarations }
  end;

type
  TCEFileSearchSettings = class(TPersistent)
  private
    fFontSize: Integer;
    fLineHeight: Integer;
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fShowExtensions: Boolean;
    fSubFolders: Boolean;
    function GetColumns: string;
    procedure SetColumns(const Value: string);
  protected
  public
    ColumnSettings: TCEColSettings;
    constructor Create;
    procedure AssignColumnSettingsFrom(FileView:
        TCustomVirtualExplorerEasyListview);
    procedure AssignColumnSettingsTo(FileView: TCustomVirtualExplorerEasyListview);
    procedure AssignSettingsFrom(FileSearch: TCESearchPage);
    procedure AssignSettingsTo(FileSearch: TCESearchPage);
  published
    property Columns: string read GetColumns write SetColumns;
    property FontSize: Integer read fFontSize write fFontSize;
    property LineHeight: Integer read fLineHeight write fLineHeight;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout write
        fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout write
        fRememberOuterToolbarLayout;
    property ShowExtensions: Boolean read fShowExtensions write fShowExtensions;
    property SubFolders: Boolean read fSubFolders write fSubFolders;
  end;

type
  TCEFileSearchPageSettings = class(TCECustomTabPageSettings)
  private
    function GetClearResults: Boolean;
    function GetContent: WideString;
    function GetExt: WideString;
    function GetMask: WideString;
    function GetMaxLevel: Integer;
    function GetMinLevel: Integer;
    function GetPath: WideString;
    function GetSubFolders: Boolean;
    function GetUseMask: Boolean;
    function GetWordPhrase: WideString;
    procedure SetClearResults(const Value: Boolean);
    procedure SetContent(const Value: WideString);
    procedure SetExt(const Value: WideString);
    procedure SetMask(const Value: WideString);
    procedure SetMaxLevel(const Value: Integer);
    procedure SetMinLevel(const Value: Integer);
    procedure SetPath(const Value: WideString);
    procedure SetSubFolders(const Value: Boolean);
    procedure SetUseMask(const Value: Boolean);
    procedure SetWordPhrase(const Value: WideString);
  protected
    function GetRememberPanelLayout: Boolean; override;
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
  public
    FileSearchPage: TCESearchPage;
  published
    property ClearResults: Boolean read GetClearResults write SetClearResults default true;
    property Content: WideString read GetContent write SetContent;
    property Ext: WideString read GetExt write SetExt;
    property Mask: WideString read GetMask write SetMask;
    property MaxLevel: Integer read GetMaxLevel write SetMaxLevel default -1;
    property MinLevel: Integer read GetMinLevel write SetMinLevel default -1;
    property Path: WideString read GetPath write SetPath;
    property SubFolders: Boolean read GetSubFolders write SetSubFolders default true; 
    property UseMask: Boolean read GetUseMask write SetUseMask default false;
    property WordPhrase: WideString read GetWordPhrase write SetWordPhrase;
  end;

var
  CEFileSearchSettings: TCEFileSearchSettings;  

implementation

uses
  Main, dCE_Actions;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCESearchPage
-------------------------------------------------------------------------------}
constructor TCESearchPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEFileSearchPageSettings(Settings).FileSearchPage:= Self;
  Layout:= 'FileSearch';
  Find:= TFindFileW.Create(nil);
  Find.Threaded:= true;
  Find.OnFileMatch:= HandleFileMatch;
  Find.OnFolderChange:= HandleFolderChange;
  Find.OnSearchBegin:= HandleSearchBegin;
  Find.OnSearchFinish:= HandleSearchFinish;
  Find.OnSearchAbort:= HandleSearchAbort;

  DestDlg:= TCEDestDlg.Create(self);
  FolderTreePopup.PopupForm:= DestDlg;
  
  ResultView.Active:= true;

  SkinManager.AddSkinNotification(Self);

  // Initialize
  CreatedBeforeDate.Date:= Date;
  CreatedBeforeTime.Time:= Now;
  CreatedAfterDate.Date:= Date;
  CreatedAfterTime.Time:= Now;

  combo_extension.Items.Delimiter:= ',';
  combo_extension.Items.DelimitedText:= _('All') + ',txt,pdf,ini,zip,rar,jpg,gif,bmp,png,avi,mp3,mov,mp4,mkv,wmv,flv,exe';
  combo_extension.ItemIndex:= 0;

  CriteriaTabControl.ActiveTabIndex:= 0;
    
  CEFileSearchSettings.AssignSettingsTo(Self);
  UpdateTheme;
  CEGlobalTranslator.TranslateComponent(Self);

  // translate size combos
  combo_size_atleast.Items.Strings[0]:= _('Bytes');
  combo_size_atleast.Items.Strings[1]:= _('KB');
  combo_size_atleast.Items.Strings[2]:= _('MB');
  combo_size_atleast.Items.Strings[3]:= _('GB');
  combo_size_atleast.ItemIndex:= 1;
  combo_size_atmost.Items.Assign(combo_size_atleast.Items);
  combo_size_atmost.ItemIndex:= 1;
  fLastStatusChange:= 0;
end;

{-------------------------------------------------------------------------------
  Destroy TCESearchPage
-------------------------------------------------------------------------------}
destructor TCESearchPage.Destroy;
begin
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.ActivePage:= nil; 
  SkinManager.RemoveSkinNotification(Self);
  CEFileSearchSettings.AssignSettingsFrom(Self);
  Find.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  edit_wordphrase Change
-------------------------------------------------------------------------------}
procedure TCESearchPage.edit_wordphraseChange(Sender: TObject);
begin
  UpdateCaption;
end;

{-------------------------------------------------------------------------------
  but_search_start Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.but_search_startClick(Sender: TObject);
begin
  StartSearch;
end;

{-------------------------------------------------------------------------------
  but_search_stop Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.but_search_stopClick(Sender: TObject);
begin
  Find.Abort;
end;

{-------------------------------------------------------------------------------
  Change Status
-------------------------------------------------------------------------------}
procedure TCESearchPage.ChangeStatus(ANewStatus: WideString);
begin
  fStatus:= ANewStatus;
  if not timer_status.Enabled then
  timer_status.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  check_content_wordwrap Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.check_content_wordwrapClick(Sender: TObject);
begin
  memo_content.WordWrap:= check_content_wordwrap.Checked;
  if memo_content.WordWrap then
  memo_content.ScrollBars:= ssVertical
  else
  memo_content.ScrollBars:= ssBoth;
end;

{-------------------------------------------------------------------------------
  check_dateClick
-------------------------------------------------------------------------------}
procedure TCESearchPage.check_dateClick(Sender: TObject);
begin
  if Sender = check_CreatedBeforeDate then
  CreatedBeforeDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_CreatedBeforeTime then
  CreatedBeforeTime.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_CreatedAfterDate then
  CreatedAfterDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_CreatedAfterTime then
  CreatedAfterTime.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_ModifiedBeforeDate then
  ModifiedBeforeDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_ModifiedBeforeTime then
  ModifiedBeforeTime.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_ModifiedAfterDate then
  ModifiedAfterDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_ModifiedAfterTime then
  ModifiedAfterTime.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_AccessedBeforeDate then
  AccessedBeforeDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_AccessedBeforeTime then
  AccessedBeforeTime.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_AccessedAfterDate then
  AccessedAfterDate.Enabled:= TSpTBXCheckBox(Sender).Checked
  else if Sender = check_AccessedAfterTime then
  AccessedAfterTime.Enabled:= TSpTBXCheckBox(Sender).Checked
end;

{-------------------------------------------------------------------------------
  check_levelClick
-------------------------------------------------------------------------------}
procedure TCESearchPage.check_levelClick(Sender: TObject);
begin
  spin_minlevel.Enabled:= check_minlevel.Checked;
  spin_maxlevel.Enabled:= check_maxlevel.Checked;
end;

{-------------------------------------------------------------------------------
  check_size_Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.check_size_Click(Sender: TObject);
begin
  if Sender = check_size_atleast then
  begin
    spin_size_atleast.Enabled:= check_size_atleast.Checked;
    combo_size_atleast.Enabled:= check_size_atleast.Checked;
  end
  else if Sender = check_size_atmost then
  begin
    spin_size_atmost.Enabled:= check_size_atmost.Checked;
    combo_size_atmost.Enabled:= check_size_atmost.Checked;
  end;
end;

{*------------------------------------------------------------------------------
  On Close Popup
-------------------------------------------------------------------------------}
procedure TCESearchPage.FolderTreePopupClosePopup(Sender: TObject; Selected:
    Boolean);
begin
  edit_location.Text:= DestDlg.GetCheckedFolders(check_subfolders.Checked);
end;

{*------------------------------------------------------------------------------
  On Popup
-------------------------------------------------------------------------------}
procedure TCESearchPage.FolderTreePopupPopup(Sender: TObject);
var
  ao: TVTAutoOptions;
begin
  if check_subfolders.Checked <> DestDlg.TriState then
  begin
    ao:= DestDlg.FolderTree.TreeOptions.AutoOptions;
    if check_subfolders.Checked then
    Include(ao, toAutoTristateTracking)
    else
    Exclude(ao, toAutoTristateTracking);
    DestDlg.FolderTree.TreeOptions.AutoOptions:= ao;
    DestDlg.TriState:= check_subfolders.Checked;
    DestDlg.FolderTree.RebuildTree;
  end;
  DestDlg.SetCheckedFolders(edit_location.Text);
end;

{-------------------------------------------------------------------------------
  GetAttributeStatus
-------------------------------------------------------------------------------}
function TCESearchPage.GetAttributeStatus(CB: TSpTBXCheckBox):
    TFileAttributeStatus;
begin
  case CB.State of
    cbUnchecked: Result:= fsUnset;
    cbChecked: Result:= fsSet;
  else
    Result:= fsIgnore;
  end;
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCESearchPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEFileSearchPageSettings;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCESearchPage.GlobalPathChanged(Sender: TObject; NewPath: WideString);
begin
  if WideDirectoryExists(NewPath) then
  edit_location.Text:= NewPath;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCESearchPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
var
  ws: WideString;
begin
  ws:= PIDLtoPath(NewPIDL);
  if WideDirectoryExists(ws) then
  edit_location.Text:= ws;
end;

{-------------------------------------------------------------------------------
  HandleFileMatch
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleFileMatch(Sender: TObject; const FileInfo:
    TFileDetails);
var
  NS: TNamespace;
begin
  ResultView.BeginUpdate;
  try
    NS:= TNamespace.CreateFromFileName(FileInfo.Location + FileInfo.Name);
    ResultView.AddCustomItem(nil, NS, true);
    fFileCount:= fFileCount + 1;
  except
    // catch exceptions, nothing else to do.
  end;
  ResultView.EndUpdate(false);
end;

{-------------------------------------------------------------------------------
  HandleFolderChange
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleFolderChange(Sender: TObject; const Folder:
    WideString; var IgnoreFolder: TFolderIgnore);
begin
  ChangeStatus(_('Searching from') + ': ' + Folder);
end;

{-------------------------------------------------------------------------------
  HandleSearchAbort
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchAbort(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
  HandleSearchBegin
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchBegin(Sender: TObject);
begin
  but_search_start.Enabled:= false;
  but_search_stop.Enabled:= true;
  fFileCount:= 0;
  fStartTime:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  HandleSearchFinish
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchFinish(Sender: TObject);
var
  ws: WideString;
begin
  but_search_start.Enabled:= true;
  but_search_start.Caption:= _('Search');
  but_search_stop.Enabled:= false;

  if Find.Aborted then
  ws:= _('Cancelled')
  else
  ws:= _('Finished');

  try
    ChangeStatus(ws + ' - ' + WideFormat(_('%d folder(s) searched and %d item(s) found in %.3f second(s)'),
                                               [Find.SearchedFolderCount, fFileCount, (GetTickCount - fStartTime) / 1000]));
  except
    ChangeStatus(ws + ' - ' + WideFormat('%d folder(s) searched and %d item(s) found in %.3f second(s)',
                                               [Find.SearchedFolderCount, fFileCount, (GetTickCount - fStartTime) / 1000]));
  end;
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{-------------------------------------------------------------------------------
  radio_nameClick
-------------------------------------------------------------------------------}
procedure TCESearchPage.radio_nameClick(Sender: TObject);
begin
  edit_wordphrase.Enabled:= radio_name_word.Checked;
  combo_extension.Enabled:= radio_name_word.Checked;
  edit_filemask.Enabled:= radio_name_mask.Checked;
  UpdateCaption;
end;

{-------------------------------------------------------------------------------
  On ResultView ColumnCustomView
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewColumnCustomView(Sender: TCustomEasyListview;
  Column: TEasyColumn; var View: TEasyViewColumnClass);
begin
  View:= TCEViewColumn;
end;

{-------------------------------------------------------------------------------
  On ResultView ContextMenuShow
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewContextMenuShow(
  Sender: TCustomVirtualExplorerEasyListview; Namespace: TNamespace;
  Menu: HMENU; var Allow: Boolean);
begin
  fOpenFolderID:= AddContextMenuItem(Menu, '-', 0);
  fOpenFolderID:= AddContextMenuItem(Menu, _('Open Containing Folder'), 0);
end;

procedure TCESearchPage.ResultViewHintCustomInfo(Sender: TCustomEasyListview;
  TargetObj: TEasyCollectionItem; Info: TEasyHintInfo);
begin
  inherited;
  if (TargetObj is TExplorerItem) then
  begin
    Info.Text:= Info.Text + #13#10 + 'Path: ' + WideExtractFilePath(TExplorerItem(TargetObj).Namespace.NameForParsing);
  end;
end;

{-------------------------------------------------------------------------------
  On ResultView ContextMenuCmd
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewContextMenuCmd(
  Sender: TCustomVirtualExplorerEasyListview; Namespace: TNamespace;
  Verb: WideString; MenuItemID: Integer; var Handled: Boolean);
begin
  if MenuItemID = fOpenFolderID then
  begin
    if assigned(Namespace) then
    OpenFolderInTab(Self, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
  end;
end;

{-------------------------------------------------------------------------------
  On ResultView ItemContextMenu
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewItemContextMenu(Sender: TCustomEasyListview;
  HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu;
  var Handled: Boolean);
begin
  if not Handled then
  Handled:= not fShowItemContextMenu;
end;

{-------------------------------------------------------------------------------
  On ResultView MouseDown
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDownShiftState:= Shift;
  fShowItemContextMenu:= not (Shift = [ssRight, ssAlt]);
end;

{-------------------------------------------------------------------------------
  On ResultView MouseUp
-------------------------------------------------------------------------------}
procedure TCESearchPage.ResultViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NS: TNamespace;
  item: TEasyItem;
  WindowPt: TPoint;
begin
  if ([ssMiddle] = fDownShiftState) or ((ssLeft in fDownShiftState) and (ssAlt in Shift)) then
  begin
    WindowPt := ResultView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= ResultView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not ResultView.EditManager.Editing then
    begin
      ResultView.EditManager.EndEdit;
      ResultView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        if NS.FileSystem and not NS.Folder then
        begin
          if ssShift in Shift then
          OpenFileInTab(NS.NameForParsing, not MainForm.TabSet.Settings.OpenTabSelect)
          else
          OpenFileInTab(NS.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect)
        end
        else
        begin
          if ssShift in Shift then
          OpenFolderInTab(Self, NS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
          else
          OpenFolderInTab(Self, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
        end;
      end;
    end;
  end
  else if (fDownShiftState = [ssMiddle, ssCtrl]) or (fDownShiftState = [ssLeft, ssAlt, ssCtrl]) then
  begin
    WindowPt := ResultView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= ResultView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not ResultView.EditManager.Editing then
    begin
      ResultView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        if ssShift in Shift then
        OpenFolderInTab(Self, NS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
        else
        OpenFolderInTab(Self, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
      end;
    end;
  end     
  else if (ssRight in fDownShiftState) and (Shift = [ssAlt]) then
  begin
    WindowPt := ResultView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= ResultView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not ResultView.EditManager.Editing then
    begin
      ResultView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        NS.ShowPropertySheet(MainForm);
      end;
    end;
    fShowItemContextMenu:= false;
  end;

  fDownShiftState:= [];
end;

{-------------------------------------------------------------------------------
  Load from stream
-------------------------------------------------------------------------------}
procedure TCESearchPage.LoadFromStream(AStream: TStream);
var
  ws: WideString;
begin
  // edit_wordphrase
  LoadWideString(AStream, ws);
  edit_wordphrase.Text:= ws;
  // edit_filemask
  LoadWideString(AStream, ws);
  edit_filemask.Text:= ws;
  // edit_location
  LoadWideString(AStream, ws);
  edit_location.Text:= ws;
  // combo_extension
  LoadWideString(AStream, ws);
  combo_extension.Text:= ws;
  combo_extension.ItemIndex:= combo_extension.Items.IndexOf(ws);
end;

{-------------------------------------------------------------------------------
  Save to stream
-------------------------------------------------------------------------------}
procedure TCESearchPage.SaveToStream(AStream: TStream);
begin
  // edit_wordphrase
  SaveWideString(AStream, edit_wordphrase.Text);
  // edit_filemask
  SaveWideString(AStream, edit_filemask.Text);
  // edit_location
  SaveWideString(AStream, edit_location.Text);
  // combo_extension
  SaveWideString(AStream, combo_extension.Text);
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCESearchPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  CEFileSearchSettings.AssignColumnSettingsTo(ResultView);
  try
    edit_wordphrase.SetFocus;
  except
  end;
end;

{-------------------------------------------------------------------------------
  Start Search
-------------------------------------------------------------------------------}
procedure TCESearchPage.StartSearch;
var
  ext: WideString;
  i: Integer;
begin
  fLastButtonAnimChange:= 0;
  but_search_start.Tag:= 0;
  fLastStatusChange:= 0;
  // Location
  Find.Criteria.Files.Location:= WideStringReplace(edit_location.Text, ',', ';', [rfReplaceAll], false);
  Find.Criteria.Files.Subfolders:= check_subfolders.Checked;
  if check_minlevel.Checked then
  Find.Criteria.Files.MinLevel:= StrToIntDef(spin_minlevel.Text, 0)
  else
  Find.Criteria.Files.MinLevel:= 0;
  if check_maxlevel.Checked then
  Find.Criteria.Files.MaxLevel:= StrToIntDef(spin_maxlevel.Text, 0)
  else
  Find.Criteria.Files.MaxLevel:= 0;
  // FileName
  if radio_name_word.Checked then
  begin
    if combo_extension.ItemIndex = 0 then
    ext:= '*'
    else
    begin
      ext:= combo_extension.Text;
      if Length(ext) > 0 then
      begin
        if ext[1] <> '.' then
        ext:= '.' + ext;
      end;
    end;
    if edit_wordphrase.Text <> '' then
    Find.Criteria.Files.FileName:= '*' + edit_wordphrase.Text + '*' + ext
    else
    Find.Criteria.Files.FileName:= '*' + ext;
  end
  else
  Find.Criteria.Files.FileName:= edit_filemask.Text;
  // Date & Time
  Find.Criteria.TimeStamp.Clear;
    // Created on
    if Self.check_CreatedBeforeDate.Checked then
    Find.Criteria.TimeStamp.CreatedBefore:= Self.CreatedBeforeDate.Date;
    if Self.check_CreatedBeforeTime.Checked then
    Find.Criteria.TimeStamp.CreatedBefore:= Find.Criteria.TimeStamp.CreatedBefore + Self.CreatedBeforeTime.Time;
    if Self.check_CreatedAfterDate.Checked then
    Find.Criteria.TimeStamp.CreatedAfter:= Self.CreatedAfterDate.Date;
    if Self.check_CreatedAfterTime.Checked then
    Find.Criteria.TimeStamp.CreatedAfter:= Find.Criteria.TimeStamp.CreatedAfter + Self.CreatedAfterTime.Time;
    // Modified on
    if Self.check_ModifiedBeforeDate.Checked then
    Find.Criteria.TimeStamp.ModifiedBefore:= Self.ModifiedBeforeDate.Date;
    if Self.check_ModifiedBeforeTime.Checked then
    Find.Criteria.TimeStamp.ModifiedBefore:= Find.Criteria.TimeStamp.ModifiedBefore + Self.ModifiedBeforeTime.Time;
    if Self.check_ModifiedAfterDate.Checked then
    Find.Criteria.TimeStamp.ModifiedAfter:= Self.ModifiedAfterDate.Date;
    if Self.check_ModifiedAfterTime.Checked then
    Find.Criteria.TimeStamp.ModifiedAfter:= Find.Criteria.TimeStamp.ModifiedAfter + Self.ModifiedAfterTime.Time;
    // Accessed on
    if Self.check_AccessedBeforeDate.Checked then
    Find.Criteria.TimeStamp.AccessedBefore:= Self.AccessedBeforeDate.Date;
    if Self.check_AccessedBeforeTime.Checked then
    Find.Criteria.TimeStamp.AccessedBefore:= Find.Criteria.TimeStamp.AccessedBefore + Self.AccessedBeforeTime.Time;
    if Self.check_AccessedAfterDate.Checked then
    Find.Criteria.TimeStamp.AccessedAfter:= Self.AccessedAfterDate.Date;
    if Self.check_AccessedAfterTime.Checked then
    Find.Criteria.TimeStamp.AccessedAfter:= Find.Criteria.TimeStamp.AccessedAfter + Self.AccessedAfterTime.Time;
  // Size
  if check_size_atleast.Checked then
  begin
    Find.Criteria.Size.Min:= Self.spin_size_atleast.SpinOptions.ValueAsInteger;
    case Self.combo_size_atleast.ItemIndex of
      1: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024;
      2: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024 * 1024;
      3: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024 * 1024 * 1024;
    end;
  end
  else
  Find.Criteria.Size.Min:= 0;
  if check_size_atmost.Checked then
  begin
    Find.Criteria.Size.Max:= Self.spin_size_atmost.SpinOptions.ValueAsInteger;
    case Self.combo_size_atmost.ItemIndex of
      1: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024;
      2: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024 * 1024;
      3: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024 * 1024 * 1024;
    end;
  end
  else
  Find.Criteria.Size.Max:= 0;
  // Attributes
  Find.Criteria.Attributes.Readonly:= GetAttributeStatus(check_attr_readonly);
  Find.Criteria.Attributes.Hidden:= GetAttributeStatus(check_attr_hidden);
  Find.Criteria.Attributes.System:= GetAttributeStatus(check_attr_system);
  Find.Criteria.Attributes.Compressed:= GetAttributeStatus(check_attr_compressed);
  Find.Criteria.Attributes.Directory:= GetAttributeStatus(check_attr_folder);;
  // Content
  Find.Criteria.Content.Phrase := Self.memo_content.Text;
  Find.Criteria.Content.Options := [];
  if check_content_case_sensitive.Checked then
  Find.Criteria.Content.Options:= [csoCaseSensitive];
  if check_content_wholeword.Checked then
  Find.Criteria.Content.Options:= [csoWholeWord];
  // Filters
  Find.Criteria.Files.Filters.Clear;
  for i:= 0 to memo_filters_exclude.Lines.Count - 1 do
  Find.Criteria.Files.Filters.Add('>' + memo_filters_exclude.Lines.Strings[i]);  
  for i:= 0 to memo_filters_exclude.Lines.Count - 1 do
  Find.Criteria.Files.Filters.Add(memo_filters_exclude.Lines.Strings[i]);
  // Clear previous results
  if check_clear_before.Checked then
  ResultView.Clear;
  // Execute Search
  Find.Execute;
end;

{-------------------------------------------------------------------------------
  On timer_status Timer
-------------------------------------------------------------------------------}
procedure TCESearchPage.timer_statusTimer(Sender: TObject);
begin
  timer_status.Enabled:= false;
  label_status.Caption:= fStatus;
  fLastStatusChange:= GetTickCount;

  if (fLastStatusChange - fLastButtonAnimChange) > 500 then
  begin
    if but_search_start.Tag > 3 then
    but_search_start.Tag:= 0;
    case but_search_start.Tag of
      0: but_search_start.Caption:= _('Search') + '   ';
      1: but_search_start.Caption:= _('Search') + '.  ';
      2: but_search_start.Caption:= _('Search') + '.. ';
      3: but_search_start.Caption:= _('Search') + '...';
    end;
    but_search_start.Tag:= but_search_start.Tag + 1;
    fLastButtonAnimChange:= fLastStatusChange;
  end;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCESearchPage.UpdateCaption;
var
  ws: WideString;
begin
  if assigned(TabItem) then
  begin
    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 22;
  end;

  if radio_name_word.Checked then
  begin
    if edit_wordphrase.Text <> '' then
    ws:= Trim(edit_wordphrase.Text);

    if (combo_extension.ItemIndex <> 0) and (combo_extension.Text <> '') then
    ws:= ws + ' .' + combo_extension.Text;

  end
  else
  ws:= '[' + edit_filemask.Text + ']';

  if (ws <> '') then
  TabCaption:= _('Search') + ': ' + ws
  else
  TabCaption:= _('File Search');
end;

{-------------------------------------------------------------------------------
  UpdateTheme
-------------------------------------------------------------------------------}
procedure TCESearchPage.UpdateTheme;
var
  c: TColor;
begin
  c:= SkinManager.CurrentSkin.Options(skncDock, sknsNormal).Body.Color1;
  if c = clNone then
  c:= clBtnFace;
  SearchPanel.Color:= c;
  CriteriaTabControl.TabBackgroundColor:= c;
end;

{-------------------------------------------------------------------------------
  WMSpSkinChange
-------------------------------------------------------------------------------}
procedure TCESearchPage.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  UpdateTheme;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEFileSearchSettings
-------------------------------------------------------------------------------}
constructor TCEFileSearchSettings.Create;
begin
  inherited;
  fShowExtensions:= true;
  fSubFolders:= true;
  fRememberInnerToolbarLayout:= true;
  fFontSize:= -1;
  fLineHeight:= -1;
end;

{-------------------------------------------------------------------------------
  Assign ColumnSettings From
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignColumnSettingsFrom(FileView:
    TCustomVirtualExplorerEasyListview);
var
  col: TEasyColumn;
  i,c: Integer;
  view: TCustomVirtualExplorerEasyListviewHack;
begin
  if not assigned(FileView) then
  Exit;

  view:= TCustomVirtualExplorerEasyListviewHack(FileView);

  c:= 0;
  for i:= 0 to view.Header.Columns.Count - 1 do
  begin
    if view.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(ColumnSettings) <> c then
  SetLength(ColumnSettings, c);
  
  i:= 0;
  col:= view.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    ColumnSettings[i].Index:= col.Index;
    ColumnSettings[i].Position:= col.Position;
    ColumnSettings[i].Width:= col.Width;
    ColumnSettings[i].Sort:= col.SortDirection;
    col:= view.Header.NextVisibleColumn(col);
    inc(i);
  end;
end;

{-------------------------------------------------------------------------------
  Assign ColumnSettings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignColumnSettingsTo(FileView:
    TCustomVirtualExplorerEasyListview);
var
  i: Integer;
  col: TEasyColumn;
  view: TCustomVirtualExplorerEasyListviewHack;
begin
  if not assigned(FileView) or (Length(ColumnSettings) = 0) then
  Exit;
  view:= TCustomVirtualExplorerEasyListviewHack(FileView);

  view.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to view.Header.Columns.Count - 1 do
    begin
      if view.Header.Columns.Columns[i].Visible then
      view.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(ColumnSettings) - 1 do
    begin
      if ColumnSettings[i].Index < view.Header.Columns.Count then
      begin
        col:= view.Header.Columns.Columns[ColumnSettings[i].Index];
        col.Visible:= true;
        col.Position:= ColumnSettings[i].Position;
        col.Width:= ColumnSettings[i].Width;
        col.SortDirection:= ColumnSettings[i].Sort;
      end;
    end;
  finally
    view.Header.Columns.EndUpdate(true);
  end;
end;

{-------------------------------------------------------------------------------
  Assign Settings From
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsFrom(FileSearch: TCESearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  fSubFolders:= FileSearch.check_subfolders.Checked;
  AssignColumnSettingsFrom(FileSearch.ResultView);
end;

{-------------------------------------------------------------------------------
  Assign Settings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsTo(FileSearch: TCESearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  FileSearch.check_subfolders.Checked:= fSubFolders;
  // FontSize
  if fFontSize > 0 then
  FileSearch.ResultView.Font.Size:= fFontSize
  else
  SetDesktopIconFonts(FileSearch.ResultView.Font);
  // LineHeight
  if fLineHeight > 0 then
  FileSearch.ResultView.CellSizes.Report.Height:= fLineHeight
  else
  FileSearch.ResultView.CellSizes.Report.RestoreDefaults;

  AssignColumnSettingsTo(FileSearch.ResultView);
end;

{-------------------------------------------------------------------------------
  Get/Set Columns
-------------------------------------------------------------------------------}
function TCEFileSearchSettings.GetColumns: string;
begin
  Result:= ColSettingsToString(ColumnSettings);
end;
procedure TCEFileSearchSettings.SetColumns(const Value: string);
begin
  StringToColSettings(Value, ColumnSettings);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetPath: WideString;
begin
  Result:= FileSearchPage.edit_location.Text;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberOuterToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Set Path
-------------------------------------------------------------------------------}
procedure TCEFileSearchPageSettings.SetPath(const Value: WideString);
begin
  FileSearchPage.edit_location.Text:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SubFolder
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetSubFolders: Boolean;
begin
  Result:= FileSearchPage.check_subfolders.Checked;
end;
procedure TCEFileSearchPageSettings.SetSubFolders(const Value: Boolean);
begin
  FileSearchPage.check_subfolders.Checked:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set WordPhrase
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetWordPhrase: WideString;
begin
  Result:= FileSearchPage.edit_wordphrase.Text;
end;
procedure TCEFileSearchPageSettings.SetWordPhrase(const Value: WideString);
begin
  FileSearchPage.edit_wordphrase.Text:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Ext
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetExt: WideString;
begin
  Result:= FileSearchPage.combo_extension.Text;
end;
procedure TCEFileSearchPageSettings.SetExt(const Value: WideString);
begin
  FileSearchPage.combo_extension.Text:= Value;
  FileSearchPage.combo_extension.ItemIndex:= FileSearchPage.combo_extension.Items.IndexOf(Value);
end;

{-------------------------------------------------------------------------------
  Get/Set Mask
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetMask: WideString;
begin
  Result:= FileSearchPage.edit_filemask.Text;
end;

procedure TCEFileSearchPageSettings.SetMask(const Value: WideString);
begin
  FileSearchPage.edit_filemask.Text:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set UseMask
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetUseMask: Boolean;
begin
  Result:= FileSearchPage.radio_name_mask.Checked;
end;
procedure TCEFileSearchPageSettings.SetUseMask(const Value: Boolean);
begin
  FileSearchPage.radio_name_mask.Checked:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set MinLevel
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetMinLevel: Integer;
begin
  if FileSearchPage.check_minlevel.Checked then
  Result:= Round(FileSearchPage.spin_minlevel.Value)
  else
  Result:= -1;
end;
procedure TCEFileSearchPageSettings.SetMinLevel(const Value: Integer);
begin
  FileSearchPage.check_minlevel.Checked:= Value > -1;
  if FileSearchPage.check_minlevel.Checked then
  FileSearchPage.spin_minlevel.Value:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set MaxLevel
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetMaxLevel: Integer;
begin
  if FileSearchPage.check_maxlevel.Checked then
  Result:= Round(FileSearchPage.spin_maxlevel.Value)
  else
  Result:= -1;
end;
procedure TCEFileSearchPageSettings.SetMaxLevel(const Value: Integer);
begin
  FileSearchPage.check_maxlevel.Checked:= Value > -1;
  if FileSearchPage.check_maxlevel.Checked then
  FileSearchPage.spin_maxlevel.Value:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set ClearResults
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetClearResults: Boolean;
begin
  Result:= FileSearchPage.check_clear_before.Checked;
end;
procedure TCEFileSearchPageSettings.SetClearResults(const Value: Boolean);
begin
  FileSearchPage.check_clear_before.Checked:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Content
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetContent: WideString;
begin
  Result:= FileSearchPage.memo_content.Text;
end;
procedure TCEFileSearchPageSettings.SetContent(const Value: WideString);
begin
  FileSearchPage.memo_content.Text:= Value;
end;

{##############################################################################}

initialization
  CEFileSearchSettings:= TCEFileSearchSettings.Create;
  GlobalAppSettings.AddItem('FileSearch', CEFileSearchSettings, true);
  TabPageClassList.RegisterClass('FileSearch', TCESearchPage, TCEFileSearchPageSettings);

finalization
  FreeAndNil(CEFileSearchSettings);

end.
