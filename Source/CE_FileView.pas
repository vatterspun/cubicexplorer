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
//  The Original Code is CE_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FileView;

interface

uses
  // CE Units
  CE_Consts, CE_BaseFileView, CE_Toolbar, CE_VistaFuncs, CE_LanguageEngine,
  // Jvcl
  JvBalloonHint, JvSimpleXML,
  // VSTools
  MPCommonObjects, MPCommonUtilities, MPShellUtilities,
  EasyListview, VirtualExplorerEasyListview, VirtualResources,
  VirtualExplorerTree,  VirtualShellHistory,
  VirtualShellNewMenu, VirtualShellNotifier,
  // SpTBX, TB2K
  SpTBXItem, TB2Item,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Forms,
  Graphics, Menus, ShellAPI, ShlObj, Math, ActiveX;

type

  TCEColSetting = record
    Index: Integer;
    Position: Integer;
    Width: Integer;
    Sort: TEasySortDirection;
  end;

  PCEColSettings = ^TCEColSettings;
  TCEColSettings = array of TCEColSetting;

  PCEGroupBySetting = ^TCEGroupBySetting;
  TCEGroupBySetting = record
    Index: Integer;
    Enabled: Boolean;
  end;

  TCEFileView = class(TCECustomFileView)
  private
    fAutosizeListViewStyle: Boolean;
    fAutoSelectFirstItem: Boolean;
    fOnViewStyleChange: TNotifyEvent;
    fSelectPreviousFolder: Boolean;
    fTranslateHeader: Boolean;
    fUseKernelNotification: Boolean;
    fCellWidth: Integer;
    fColumnIndex: Integer;
    fRightMouseButton_IsDown: Boolean;
    fLeftMouseButton_IsDown: Boolean;
    fLeftMouseButton_RockerClicks: Integer;
    fArrowBrowse: Boolean;
    fFolderUpOnDblClick: Boolean;
    fFullRowDblClick: Boolean;
    fOldHistoryIndex: Integer;
    fPasteFocusSet: Boolean;
    fRightMouseButton_RockerClicks: Integer;
    fSelectPasted: Boolean;
    fSingleClickBrowse: Boolean;
    fSingleClickExecute: Boolean;
    fSortAfterPaste: Boolean;
    fUseMouseRocker: Boolean;
    procedure SetAutosizeListViewStyle(const Value: Boolean);
    procedure SetUseKernelNotification(const Value: Boolean);
  protected
    fDoubleClicking: Boolean;
    fLastPaste: Integer;
    fSingleClickBrowsing: Boolean;
    procedure DoAfterShellNotify(ShellEvent: TVirtualShellEvent); override;
    procedure DoCustomColumnAdd; override;
    procedure DoEnumFinished; override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean);
        override;
    procedure DoItemClick(Item: TEasyItem; KeyStates: TCommonKeyStates; HitInfo:
        TEasyItemHitTestInfoSet); override;
    procedure DoKeyAction(var CharCode: Word; var Shift: TShiftState; var
        DoDefault: Boolean); override;
    procedure DoRootChange; override;
    procedure DoRootChanging(const NewRoot: TRootFolder; Namespace: TNamespace; var
        Allow: Boolean); override;
    procedure DoRootRebuild; override;
    procedure DoShellNotify(ShellEvent: TVirtualShellEvent); override;
    procedure HandleDblClick(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HandleMouseDown(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HistoryChange(Sender: TBaseVirtualShellPersistent; ItemIndex:
        Integer; ChangeType: TVSHChangeType);
    procedure OnAfterFileCreate(Sender: TMenu; const NewMenuItem:
        TVirtualShellNewItem; const FileName: WideString);
    procedure OnCreateNewFile(Sender: TMenu; const NewMenuItem:
        TVirtualShellNewItem; var Path, FileName: WideString; var Allow: Boolean);
    procedure SetNotifyFolder(Namespace: TNamespace);
    procedure SetView(Value: TEasyListStyle); override;
    procedure UpdateBackgroundText;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    property ColumnIndex: Integer read fColumnIndex write fColumnIndex;
  public
    fChangeHistory: Boolean;
    History: TVirtualShellHistory;
    ShellNewMenu: TVirtualShellNewMenu;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddCustomItem(Group: TEasyGroup; NS: TNamespace; LockoutSort:
        Boolean): TExplorerItem; override;
    procedure CalculateFolderSizes;
    procedure ClearHistory;
    procedure CreateEmptyFile;
    procedure CreateNewFolder;
    procedure GoBackInHistory;
    procedure GoFolderUp;
    procedure GoForwardInHistory;
    procedure PasteFromClipboard; override;
    procedure PasteShortcutFromClipboard;
    procedure SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift = evsCurrent);
        override;
    procedure SetFocus; override;
    property AutosizeListViewStyle: Boolean read fAutosizeListViewStyle write
        SetAutosizeListViewStyle;
    property FolderUpOnDblClick: Boolean read fFolderUpOnDblClick write
        fFolderUpOnDblClick;
    property TranslateHeader: Boolean read fTranslateHeader write fTranslateHeader;
    property UseMouseRocker: Boolean read fUseMouseRocker write fUseMouseRocker;
    property LeftMouseButton_IsDown: Boolean read fLeftMouseButton_IsDown;
    property LeftMouseButton_RockerClicks: Integer read
        fLeftMouseButton_RockerClicks;
    property RightMouseButton_IsDown: Boolean read fRightMouseButton_IsDown;
    property RightMouseButton_RockerClicks: Integer read
        fRightMouseButton_RockerClicks;
  published
    property AutoSelectFirstItem: Boolean read fAutoSelectFirstItem write
        fAutoSelectFirstItem;
    property ArrowBrowse: Boolean read fArrowBrowse write fArrowBrowse;
    property FullRowDblClick: Boolean read fFullRowDblClick write fFullRowDblClick;
    property SelectPasted: Boolean read fSelectPasted write fSelectPasted;
    property SelectPreviousFolder: Boolean read fSelectPreviousFolder write
        fSelectPreviousFolder;
    property SingleClickBrowse: Boolean read fSingleClickBrowse write
        fSingleClickBrowse;
    property SingleClickExecute: Boolean read fSingleClickExecute write
        fSingleClickExecute;
    property SortAfterPaste: Boolean read fSortAfterPaste write fSortAfterPaste;
    property UseKernelNotification: Boolean read fUseKernelNotification write
        SetUseKernelNotification;
    property OnViewStyleChange: TNotifyEvent read fOnViewStyleChange write
        fOnViewStyleChange;
  end;

  TCEColumnSettings = class(TPersistent)
  private
    function GetControlPanel: string;
    function GetDefault: string;
    function GetMyComputer: string;
    function GetNetwork: string;
    procedure SetControlPanel(const Value: string);
    procedure SetDefault(const Value: string);
    procedure SetMyComputer(const Value: string);
    procedure SetNetwork(const Value: string);
  public
    DefaultColSettings: TCEColSettings;
    MyComputerColSettings: TCEColSettings;
    NetworkColSettings: TCEColSettings;
    ControlPanelColSettings: TCEColSettings;
  published
    property ControlPanel: string read GetControlPanel write SetControlPanel;
    property Default: string read GetDefault write SetDefault;
    property MyComputer: string read GetMyComputer write SetMyComputer;
    property Network: string read GetNetwork write SetNetwork;
  end;

type
  TCEGroupBySettings = class(TPersistent)
  private
    function GetControlPanel: string;
    function GetDefault: string;
    function GetMyComputer: string;
    function GetNetwork: string;
    procedure SetControlPanel(const Value: string);
    procedure SetDefault(const Value: string);
    procedure SetMyComputer(const Value: string);
    procedure SetNetwork(const Value: string);
  public
    DefaultGroupBySettings: TCEGroupBySetting;
    MyComputerGroupBySettings: TCEGroupBySetting;
    NetworkGroupBySettings: TCEGroupBySetting;
    ControlPanelGroupBySettings: TCEGroupBySetting;
    function GroupBySettingsToString(GroupBySetting: TCEGroupBySetting): string;
    procedure StringToGroupBySettings(AString: String; var GroupBySetting:
        TCEGroupBySetting);
  published
    property ControlPanel: string read GetControlPanel write SetControlPanel;
    property Default: string read GetDefault write SetDefault;
    property MyComputer: string read GetMyComputer write SetMyComputer;
    property Network: string read GetNetwork write SetNetwork;
  end;

  TCEFilmstripSettings = class(TPersistent)
  private
    fThumbPos: TAlign;
    fThumbSize: Integer;
    fThumbStyle: TEasyListStyle;
  published
    property ThumbPos: TAlign read fThumbPos write fThumbPos;
    property ThumbSize: Integer read fThumbSize write fThumbSize;
    property ThumbStyle: TEasyListStyle read fThumbStyle write fThumbStyle;
  end;

  TCECellSizeSettings = class(TPersistent)
  private
    fLargeIcons_Height: Integer;
    fSmallIcons_Height: Integer;
    fList_Height: Integer;
    fDetails_Height: Integer;
    fTiles_Height: Integer;
    fThumbnails_Height: Integer;
    fFilmstrip_Height: Integer;
    fLargeIcons_Width: Integer;
    fSmallIcons_Width: Integer;
    fList_Width: Integer;
    fDetails_Width: Integer;
    fTiles_Width: Integer;
    fThumbnails_Width: Integer;
    fFilmstrip_Width: Integer;
  public
    constructor Create;
  published
    function IsChanged(CompareTo: TCECustomFileView): Boolean;
    property LargeIcons_Height: Integer read fLargeIcons_Height write
        fLargeIcons_Height;
    property SmallIcons_Height: Integer read fSmallIcons_Height write
        fSmallIcons_Height;
    property List_Height: Integer read fList_Height write fList_Height;
    property Details_Height: Integer read fDetails_Height write fDetails_Height;
    property Tiles_Height: Integer read fTiles_Height write fTiles_Height;
    property Thumbnails_Height: Integer read fThumbnails_Height write
        fThumbnails_Height;
    property Filmstrip_Height: Integer read fFilmstrip_Height write
        fFilmstrip_Height;
    property LargeIcons_Width: Integer read fLargeIcons_Width write
        fLargeIcons_Width;
    property SmallIcons_Width: Integer read fSmallIcons_Width write
        fSmallIcons_Width;
    property List_Width: Integer read fList_Width write fList_Width;
    property Details_Width: Integer read fDetails_Width write fDetails_Width;
    property Tiles_Width: Integer read fTiles_Width write fTiles_Width;
    property Thumbnails_Width: Integer read fThumbnails_Width write
        fThumbnails_Width;
    property Filmstrip_Width: Integer read fFilmstrip_Width write fFilmstrip_Width;
  end;

function ColSettingsToString(ColSettings: TCEColSettings): string;

procedure StringToColSettings(AString: String; var ColSettings: TCEColSettings);

implementation

uses
  CE_GlobalCtrl, CE_Utils, dCE_Actions, Main, CE_FileUtils;

{##############################################################################}

{-------------------------------------------------------------------------------
  Column Settings to string
-------------------------------------------------------------------------------}
function ColSettingsToString(ColSettings: TCEColSettings): string;
var
  i: Integer;
  col: TCEColSetting;
begin
  Result:= '';
  for i:= 0 to Length(ColSettings) - 1 do
  begin
    col:= ColSettings[i];
    if i > 0 then
    Result:= Result + '|';
    Result:= Result + IntToStr(col.Index) + ','
             + IntToStr(col.Position) + ','
             + IntToStr(col.Width) + ','
             + IntToStr(Ord(col.Sort));
  end;
end;

{-------------------------------------------------------------------------------
  String to Column Settings
-------------------------------------------------------------------------------}
procedure StringToColSettings(AString: String; var ColSettings: TCEColSettings);
var
  i: Integer;
  colListSet, colList: TStrings;
begin
  colListSet:= TStringList.Create;
  colList:= TStringList.Create;
  try
    colListSet.Delimiter:= '|';
    colList.Delimiter:= ',';

    colListSet.DelimitedText:= AString;
    SetLength(ColSettings, colListSet.Count);

    for i:= 0 to colListSet.Count - 1 do
    begin
      colList.DelimitedText:= colListSet.Strings[i];
      if colList.Count >= 3 then
      begin
        ColSettings[i].Index:= StrToInt(colList.Strings[0]);
        ColSettings[i].Position:= StrToInt(colList.Strings[1]);
        ColSettings[i].Width:= StrToInt(colList.Strings[2]);
        if colList.Count = 4 then
        ColSettings[i].Sort:= TEasySortDirection(StrToInt(colList.Strings[3]));
      end;
    end;

  finally
    colListSet.Free;
    colList.Free;
  end;    
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEFileView
-------------------------------------------------------------------------------}
constructor TCEFileView.Create(AOwner: TComponent);
begin
  inherited;
  fCellWidth:= 0;
  fChangeHistory:= false;
  fTranslateHeader:= false;
  History:= TVirtualShellHistory.Create(self);
  History.Add(TNamespace.Create(nil,nil),true); // Add Desktop to history
  //History.ItemIndex:= -1;
  History.OnChange:= HistoryChange;
  fChangeHistory:= true;
  fOldHistoryIndex:= -1;

  ShellNewMenu:= TVirtualShellNewMenu.Create(self);
  ShellNewMenu.NewFolderItem:= true;
  ShellNewMenu.NewShortcutItem:= true;
  ShellNewMenu.OnCreateNewFile:= OnCreateNewFile;
  ShellNewMenu.OnAfterFileCreate:= OnAfterFileCreate;
  //self.PopupMenu:= ShellNewMenu;
  UsePNGAlpha:= true;
  fUseKernelNotification:= true;
  ChangeNotifier.RegisterKernelChangeNotify(Self,AllKernelNotifiers);
  fSelectPreviousFolder:= true;
  fAutoSelectFirstItem:= true;
  fAutosizeListViewStyle:= true;
  // Default CellSizes
  Self.CellSizes.SmallIcon.Height:= SmallShellIconSize + 1;
  Self.CellSizes.List.Height:= SmallShellIconSize + 1;
  Self.CellSizes.Report.Height:= SmallShellIconSize + 1;
  Self.Selection.FirstItemFocus:= false;

  UseMouseRocker:= true;
  fSingleClickBrowse:= false;
  fSingleClickExecute:= false;
  fArrowBrowse:= true;
  fFolderUpOnDblClick:= true;
  fFullRowDblClick:= false;

  Self.BackGround.CaptionShowOnlyWhenEmpty:= false;
  // Paste selection/sort
  fLastPaste:= 0;
  fSelectPasted:= true;
  fSortAfterPaste:= true;
end;

{-------------------------------------------------------------------------------
  Destroy an instance of TCEFileView.
-------------------------------------------------------------------------------}
destructor TCEFileView.Destroy;
begin
  fChangeHistory:= false;
  if fUseKernelNotification then
  ChangeNotifier.UnRegisterKernelChangeNotify(Self);
  //ClearHistory;
  //History.Free;
  //ShellNewMenu.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  AddCustomItem
-------------------------------------------------------------------------------}
function TCEFileView.AddCustomItem(Group: TEasyGroup; NS: TNamespace;
    LockoutSort: Boolean): TExplorerItem;
begin
  Result:= inherited AddCustomItem(Group, NS, True);
  if SelectPasted and (fLastPaste > 0) then
  begin
    Result.Selected:= true;
    if not fPasteFocusSet then // Set focus to first pasted item
    begin
      Self.Selection.FocusedItem:= Result;
      fPasteFocusSet:= true;
    end;
  end;
  UpdateBackgroundText;
end;

{-------------------------------------------------------------------------------
  Calculate Folder Sizes
-------------------------------------------------------------------------------}
procedure TCEFileView.CalculateFolderSizes;
var
  item: TExplorerItem;
begin
  item:= TExplorerItem(Selection.First);
  while assigned(item) do
  begin
    item.Namespace.FolderSize(true, true);
    item:= TExplorerItem(Selection.Next(item));
  end;
end;

{-------------------------------------------------------------------------------
  Clear History
-------------------------------------------------------------------------------}
procedure TCEFileView.ClearHistory;
begin
  fChangeHistory:= false;
  try
    History.Clear;
  finally
    fChangeHistory:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called on mouse dbl click.
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleDblClick(Button: TCommonMouseButton; Msg: TWMMouse);
var
  WindowPt: TPoint;
  item: TEasyItem;
  HitInfo: TEasyItemHitTestInfoSet;
  GoBack: Boolean;
begin
  fDoubleClicking:= true;
  GoBack:= false;
  if Self.EditManager.Editing or fSingleClickBrowsing then
  Exit;

  if FolderUpOnDblClick then
  begin
    WindowPt:= Self.Scrollbars.MapWindowToView(Msg.Pos);
    if (Button = cmbLeft) and (WindowPt.Y >= 0) then
    begin
      item:= self.Groups.ItemByPoint(WindowPt);
      GoBack:= item = nil;

      if not GoBack then
      begin
        item.HitTestAt(WindowPt, HitInfo);
        case View of
          elsReport: begin
            if Self.Selection.FullRowSelect then
            GoBack:= not FullRowDblClick and (HitInfo = [])
            else
            GoBack:= not (ehtOnClickSelectBounds in HitInfo);
          end;
          elsTile: GoBack:= not (ehtOnIcon in HitInfo) and not (ehtOnText in HitInfo) and not (ehtOnClickSelectBounds in HitInfo);
          else
          GoBack:= not (ehtOnIcon in HitInfo) and not (ehtOnText in HitInfo);
        end;
      end;
    end;

    if GoBack then
    self.GoFolderUp
    else
    inherited;
  end
  else
  inherited;
end;

{-------------------------------------------------------------------------------
  On ShellNewMenu.AfterFileCreate
-------------------------------------------------------------------------------}
procedure TCEFileView.OnAfterFileCreate(Sender: TMenu; const NewMenuItem:
    TVirtualShellNewItem; const FileName: WideString);
var
  NS: TNamespace;
  Item: TExplorerItem;
begin
  if (WideDirectoryExists(FileName) or WideFileExists(FileName)) then
  begin
    Item:= Self.FindItemByPath(FileName) as TExplorerItem;
    if not assigned(Item) then
    begin
      NS:= TNamespace.Create(PathToPIDL(FileName), nil);
      Item:= Self.AddCustomItem(nil, NS, True);
    end;
    if Assigned(Item) then
    begin
      Self.Selection.ClearAll;
      Item.MakeVisible(emvAuto);
      Item.Focused:= True;
      Item.Selected:= True;
      Item.Edit;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called when new file is created from ShellNewMenu.
-------------------------------------------------------------------------------}
procedure TCEFileView.OnCreateNewFile(Sender: TMenu; const NewMenuItem:
    TVirtualShellNewItem; var Path, FileName: WideString; var Allow: Boolean);
begin
  Path:= self.RootFolderNamespace.NameForParsing;
  if not WideDirectoryExists(Path) then
  Path:= '';
end;

{-------------------------------------------------------------------------------
  Set View Mode
-------------------------------------------------------------------------------}
procedure TCEFileView.SetView(Value: TEasyListStyle);
var
  i: Integer;
begin
  inherited;
  if Value = elsList then
  begin
    if AutosizeListViewStyle then
    begin
      fCellWidth:= 0;
      for i:= 0 to Self.ItemCount - 1 do
      begin
        fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Self.Items.Items[i].Caption));
      end;
      if CheckBoxSelection then
      fCellWidth:= fCellWidth + Self.PaintInfoItem.CheckSize;
      if fCellWidth > 0 then
      Self.CellSizes.List.Width:= fCellWidth + 32;
      fCellWidth:= 0;
    end
    else
    begin
      Self.CellSizes.List.RestoreDefaults;
    end;
  end;

  if assigned(fOnViewStyleChange) then
  fOnViewStyleChange(self);
end;

{-------------------------------------------------------------------------------
  Create Empty File
-------------------------------------------------------------------------------}
procedure TCEFileView.CreateEmptyFile;
var
  path: WideString;
  NS: TNamespace;
  item: TEasyItem;
  h: THandle;
begin
  if WideDirectoryExists(RootFolderNamespace.NameForParsing) then
  begin
   path:= UniqueDirName(WideIncludeTrailingBackslash(RootFolderNamespace.NameForParsing) + _('New File'));

   h:= CreateFileW(PWideChar(path),0,0,nil,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,0);
   if h <> INVALID_HANDLE_VALUE then
   begin
     CloseHandle(h);
     if WideFileExists(path) then
     begin
       if not Self.Focused then
       Self.SetFocus;
       NS:= TNamespace.CreateFromFileName(path);
       item:= AddCustomItem(nil,NS,true);
       //Self.Selection.SelectRange(item,item,false,true);
       Self.Selection.ClearAll;
       item.MakeVisible(emvAuto);
       item.Focused:= True;
       item.Selected:= True;
       item.Edit;
     end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
  Create new folder
-------------------------------------------------------------------------------}
procedure TCEFileView.CreateNewFolder;
var
  path: WideString;
  NS: TNamespace;
  item: TEasyItem;
begin
  if WideDirectoryExists(RootFolderNamespace.NameForParsing) then
  begin
   path:= UniqueDirName(WideIncludeTrailingBackslash(RootFolderNamespace.NameForParsing) + _('New Folder'));
   WideCreateDir(path);
   if WideDirectoryExists(path) then
   begin
     if not Self.Focused then
     Self.SetFocus;
     NS:= TNamespace.CreateFromFileName(path);
     item:= AddCustomItem(nil,NS,true);
     //Self.Selection.SelectRange(item,item,false,true);
     Self.Selection.ClearAll;
     item.MakeVisible(emvAuto);
     item.Focused:= True;
     item.Selected:= True;
     item.Edit;
   end;
  end;
end;

{-------------------------------------------------------------------------------
  DoAfterShellNotify
-------------------------------------------------------------------------------}
procedure TCEFileView.DoAfterShellNotify(ShellEvent: TVirtualShellEvent);
begin
  inherited;
  if (fLastPaste > 0) and (ShellEvent.ShellNotifyEvent = vsneFreeSpace) then // get's called after paste (not reliable!)
  begin
    if SortAfterPaste then
    begin
      Self.SortList;
    end;

    if Self.Selection.Count > 0 then
    Self.Selection.First.MakeVisible(emvAuto);

    fLastPaste:= 0;
  end;
end;

{-------------------------------------------------------------------------------
  Do CustomColumnAdd
-------------------------------------------------------------------------------}
procedure TCEFileView.DoCustomColumnAdd;
var
  i: Integer;
  col: TEasyColumn;
begin
  inherited;
  if fTranslateHeader then
  begin
    for i:= 0 to Self.Header.Columns.Count-1 do
    begin
      col:= Self.Header.Columns[i];
      col.Caption:= _(col.Caption);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DoEnumFinished
-------------------------------------------------------------------------------}
procedure TCEFileView.DoEnumFinished;
var
  item: TExplorerItem;
begin
  if (View = elsList) and AutosizeListViewStyle then
  begin
    if fCellWidth > 0 then
    begin
      if CheckBoxSelection then
      fCellWidth:= fCellWidth + Self.PaintInfoItem.CheckSize;
      CellSizes.List.Width:= fCellWidth + 32
    end
    else
    CellSizes.List.RestoreDefaults;
    fCellWidth:= 0;
  end;

  // Show Empty Folder text
  if Self.Groups.ItemCount = 0 then
  begin
    if IsEmptyFolder(Self.RootFolderNamespace) then
    self.BackGround.Caption:= _('Empty folder')
    else
    self.BackGround.Caption:= _('Contains hidden items');
    Self.BackGround.CaptionShow:= true;
  end
  else
  begin
    Self.BackGround.CaptionShow:= false;
  end;

  // Select previous folder
  if SelectPreviousFolder then
  begin
    if (fOldHistoryIndex > -1) and (fOldHistoryIndex < History.Count) then
    begin
      Selection.ClearAll;
      item:= FindItemByPIDL(History.Items[fOldHistoryIndex].AbsolutePIDL);
      if assigned(item) then
      begin
        item.MakeVisible(emvTop);
        item.Focused:= true;
        item.Selected:= true;
      end;
      fOldHistoryIndex:= -1;
    end;
  end;

  // Select first item
  if AutoSelectFirstItem and (Selection.Count = 0) then
  begin
    item:= TExplorerItem(Self.Groups.FirstItemInRect(Self.ClientInViewportCoords));
    if assigned(item) then
    begin
      item.MakeVisible(emvTop);
      item.Focused:= true;
      item.Selected:= true;
    end;
  end;
  
  inherited;
end;

{-------------------------------------------------------------------------------
  DoEnumFolder
-------------------------------------------------------------------------------}
procedure TCEFileView.DoEnumFolder(const Namespace: TNamespace; var
    AllowAsChild: Boolean);
begin
  if (View = elsList) and AutosizeListViewStyle then
  begin
    fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Namespace.NameInFolder));
  end;
  
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Item Click
-------------------------------------------------------------------------------}
procedure TCEFileView.DoItemClick(Item: TEasyItem; KeyStates: TCommonKeyStates;
    HitInfo: TEasyItemHitTestInfoSet);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle Key Action
-------------------------------------------------------------------------------}
procedure TCEFileView.DoKeyAction(var CharCode: Word; var Shift: TShiftState;
    var DoDefault: Boolean);
var
  ns: TNamespace;
  item: TEasyItem;
begin
  if DoDefault then
  begin
    DoDefault:= True;
    case CharCode of
      VK_BACK: begin
        GoFolderUp;
        DoDefault:= false;
      end;
      VK_F2,
      VK_F5,
      VK_DELETE: DoDefault:= Shift <> [];
      Ord('C'), Ord('c'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('X'), Ord('x'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('V'), Ord('v'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('A'), Ord('a'): begin
        if (Shift = [ssShift,ssCtrl]) or (Shift = [ssCtrl]) then
        DoDefault:= false;
      end;
      VK_LEFT:
      begin
        if Shift = [ssAlt] then
        DoDefault:= false
        else if ArrowBrowse and (View = elsReport) then
        begin
          GoFolderUp;
          DoDefault:= false;
        end;
      end;
      VK_RIGHT:
      begin
        if Shift = [ssAlt] then
        DoDefault:= false
        else if ArrowBrowse and (View = elsReport) then
        CharCode:= VK_RETURN;
      end;
      VK_INSERT:
      begin
        if (ssShift in Shift) or (ssCtrl in Shift) then
        DoDefault:= false;
      end;
      VK_RETURN:
      begin
        if Self.Selection.Count > 0 then
        begin
          // execute first selected (file)
          if Self.ValidateNamespace(Self.Selection.First, ns) and not ns.Folder then
          DoShellExecute(Self.Selection.First);

          // browse/execute rest selected
          item:= Self.Selection.Next(Self.Selection.First);
          while assigned(item) do
          begin
            if Self.ValidateNamespace(item, ns) then
            begin
              if ns.Folder then
              OpenFolderInTab(Self, ns.AbsolutePIDL, false)
              else
              DoShellExecute(item);
            end;
            item:= Self.Selection.Next(item);
          end;

          // browse to first selected (folder)
          if Self.ValidateNamespace(Self.Selection.First, ns) and ns.Folder then
          Self.BrowseToByPIDL(ns.AbsolutePIDL);
          
          DoDefault:= false;
        end;
      end;
    end
  end;

  inherited DoKeyAction(CharCode, Shift, DoDefault);
end;

{-------------------------------------------------------------------------------
  Do Root Change
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootChange;
begin
  inherited;
  // TODO: Redesign history feature
  if fChangeHistory then
  begin
    fChangeHistory:= false;
    if History.ItemIndex > -1 then
    begin
      if RootFolderNamespace.ComparePIDL(History.Items[History.ItemIndex].AbsolutePIDL, true) <> 0 then
      History.Add(RootFolderNamespace, false, true);
    end;    
    fChangeHistory:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Root changing
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootChanging(const NewRoot: TRootFolder; Namespace:
    TNamespace; var Allow: Boolean);
begin
  inherited;
  fLastPaste:= 0;
  fPasteFocusSet:= false;
  SetNotifyFolder(Namespace);
  Self.BackGround.Caption:= _('Opening...');
  Self.BackGround.CaptionShow:= true;
end;

{-------------------------------------------------------------------------------
  Root rebuild
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootRebuild;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Do ShellNotify
-------------------------------------------------------------------------------}
procedure TCEFileView.DoShellNotify(ShellEvent: TVirtualShellEvent);
begin
  inherited;
  UpdateBackgroundText;
end;

{-------------------------------------------------------------------------------
  Go Back In History
-------------------------------------------------------------------------------}
procedure TCEFileView.GoBackInHistory;
begin
  if Self.ContextMenuShowing then
  Exit;

  if Self.EditManager.Editing then
  Self.EditManager.EndEdit;

  fOldHistoryIndex:= History.ItemIndex;
  History.Back;
end;

{-------------------------------------------------------------------------------
  Go folder up
-------------------------------------------------------------------------------}
procedure TCEFileView.GoFolderUp;
begin
  if Self.ContextMenuShowing then
  Exit;
  
  if Self.EditManager.Editing then
  Self.EditManager.EndEdit;

  fOldHistoryIndex:= History.ItemIndex;
  BrowseToPrevLevel;
end;

{-------------------------------------------------------------------------------
  Go Forward in history
-------------------------------------------------------------------------------}
procedure TCEFileView.GoForwardInHistory;
begin
  if Self.ContextMenuShowing then
  Exit;
  
  if Self.EditManager.Editing then
  Self.EditManager.EndEdit;
  
  History.Next;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Down events.
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleMouseDown(Button: TCommonMouseButton; Msg:
    TWMMouse);
begin
  if Button = cmbLeft then
  fLeftMouseButton_IsDown:= true;
  if Button = cmbRight then
  fRightMouseButton_IsDown:= true;

  // Mouse Rocker Navigation
  if UseMouseRocker then
  begin
    if (Button = cmbLeft) and fRightMouseButton_IsDown then
    begin
      GoBackInHistory;
      Exit;
    end
    else if (Button = cmbRight) and fLeftMouseButton_IsDown then
    begin
      GoForwardInHistory;
      Exit;
    end;
  end;

  Inherited;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse);
var
  doDefault, doBrowse, doExecute: Boolean;
  viewPt: TPoint;
  item: TEasyItem;
  itemHitInfo: TEasyItemHitTestInfoSet;
  ns: TNamespace;
  PIDL: PItemIDList;
  KeyState: TCommonKeyStates;
begin
  fSingleClickBrowsing:= false;
  doDefault:= true;
  
  if Button = cmbLeft then
  begin
    fLeftMouseButton_IsDown:= false;
  end
  else if Button = cmbRight then
  begin
    fRightMouseButton_IsDown:= false;
  end;
  KeyState := KeyToKeyStates(Msg.Keys);
  // SingleClickBrowse and SingleClickExecute
  if (SingleClickBrowse or SingleClickExecute)
      and (Button = cmbLeft)
      and not ((cksControl in KeyState) or (cksShift in KeyState))
      and not fRightMouseButton_IsDown
      and not fDoubleClicking
      and not Self.DragInitiated then
  begin
    viewPt:= Scrollbars.MapWindowToView(Msg.Pos);
    item:= Groups.ItembyPoint(viewPt);
    if assigned(item) then
    begin
      item.HitTestAt(viewPt, itemHitInfo);
      if ((ehtOnIcon in itemHitInfo) or (ehtOnText in itemHitInfo)) and not (ehtOnCheck in itemHitInfo) then
      begin
        if Self.ValidateNamespace(item, ns) then
        begin
          doBrowse:= false;
          doExecute:= false;
          if ns.Folder then
          begin
            // ZIP folders
            if IsSameText(ns.Extension, '.zip') then
            begin
              if (eloBrowseExecuteZipFolder in Options) then
              begin
                doBrowse:= SingleClickBrowse;
                doExecute:= SingleClickExecute;
              end
              else
              doExecute:= SingleClickExecute;
            end
            // Normal folders
            else
            doBrowse:= SingleClickBrowse;
          end
          // Files
          else
          doExecute:= SingleClickExecute;

          // Do Browse
          if doBrowse then
          begin
            PIDL:= PIDLMgr.CopyPIDL(ns.AbsolutePIDL);
            try
              fSingleClickBrowsing:= true;
              BrowseToByPIDL(PIDL);
              doDefault:= false;
            finally
              PIDLMgr.FreePIDL(PIDL);
            end;
          end
          // Do Execute
          else if doExecute then
          begin
            Self.DoShellExecute(item);
            doDefault:= not item.Selected;
          end;
        end;
      end;
    end;
  end;

  if doDefault then
  Inherited;
  fDoubleClicking:= false;
end;

{-------------------------------------------------------------------------------
  On History change
-------------------------------------------------------------------------------}
procedure TCEFileView.HistoryChange(Sender: TBaseVirtualShellPersistent;
    ItemIndex: Integer; ChangeType: TVSHChangeType);
begin
  if fChangeHistory then
  begin
    if (ChangeType = hctSelected) and (ItemIndex > -1) then
    begin
      fChangeHistory:= false;
      if assigned(History.Items[ItemIndex]) then
      BrowseToByPIDL(History.Items[ItemIndex].AbsolutePIDL, false);
      fChangeHistory:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  PasteFromClipboard
-------------------------------------------------------------------------------}
procedure TCEFileView.PasteFromClipboard;
begin
  fLastPaste:= GetTickCount;
  fPasteFocusSet:= false;
  if SelectPasted then
  Self.Selection.ClearAll;
  inherited;
end;

{-------------------------------------------------------------------------------
  Paste Shortcut From Clipboard
-------------------------------------------------------------------------------}
procedure TCEFileView.PasteShortcutFromClipboard;
var
  NSA: TNamespaceArray;
begin
  fLastPaste:= GetTickCount;
  Cursor := crHourglass;
  try
    if Assigned(RootFolderNamespace) then
    begin
      SetLength(NSA, 1);
      NSA[0] := RootFolderNamespace;
      RootFolderNamespace.Paste(Self, NSA, True);
    end;
  finally
    Cursor:= crDefault;
  end
end;

procedure TCEFileView.SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift =
    evsCurrent);
var
  Item: TExplorerItem;
  old_hints: Boolean;
begin
  Cursor := crHourglass;
  try
    Item := TExplorerItem(Selection.First);
    if Assigned(Item) then
    begin
      old_hints:= MainForm.ShowHint;
      MainForm.ShowHint:= false; // Cheap workaround for a bug. In XP, hint will bring CE on top of the delete confirmation window
      Item.Namespace.Delete(MainForm, SelectedToNamespaceArray, ShiftKeyState);
      MainForm.ShowHint:= old_hints;
    end;
  finally
    Cursor := crDefault
  end
end;

{-------------------------------------------------------------------------------
  Set AutosizeListViewStyle
-------------------------------------------------------------------------------}
procedure TCEFileView.SetAutosizeListViewStyle(const Value: Boolean);
var
  i: Integer;
begin
  if Value <> fAutosizeListViewStyle then
  begin
    fAutosizeListViewStyle:= Value;
    if Self.View = elsList then
    begin
      if not fAutosizeListViewStyle then
      Self.CellSizes.List.RestoreDefaults
      else
      begin
        fCellWidth:= 0;
        for i:= 0 to Self.ItemCount - 1 do
        begin
          fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Self.Items.Items[i].Caption));
          if Self.CheckBoxSelection then
          fCellWidth:= Self.PaintInfoItem.CheckSize + fCellWidth;
        end;
        if fCellWidth > 0 then
        Self.CellSizes.List.Width:= fCellWidth + 32;
        fCellWidth:= 0;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Focus
-------------------------------------------------------------------------------}
procedure TCEFileView.SetFocus;
begin
  if not GetRealVisibility(self) then Exit;
  inherited;
  UpdateAllActions;
end;

{-------------------------------------------------------------------------------
  Set Notify Folder
-------------------------------------------------------------------------------}
procedure TCEFileView.SetNotifyFolder(Namespace: TNamespace);
var
  useKernel: Boolean;
begin
  if assigned(Namespace) then
  begin
    if fUseKernelNotification then
    begin
      useKernel:= not Namespace.IsNetworkNeighborhoodChild;
      if useKernel then
      useKernel:= WideGetDriveType(WideIncludeTrailingBackslash(Namespace.NameForParsing)) = DRIVE_FIXED;

      if useKernel then
      ChangeNotifier.NotifyWatchFolder(Self, Namespace.NameForParsing)
      else
      ChangeNotifier.NotifyWatchFolder(Self, '');
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set UseKernelNotification
-------------------------------------------------------------------------------}
procedure TCEFileView.SetUseKernelNotification(const Value: Boolean);
begin
  if Value <> fUseKernelNotification then
  begin
    fUseKernelNotification:= Value;
    if fUseKernelNotification then
    begin
      ChangeNotifier.RegisterKernelChangeNotify(Self,AllKernelNotifiers);
    end
    else
    begin
      ChangeNotifier.UnRegisterKernelChangeNotify(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  UpdateBackgroundText
-------------------------------------------------------------------------------}
procedure TCEFileView.UpdateBackgroundText;
begin
  // Show Empty Folder text
  if Self.Groups.ItemCount = 0 then
  begin
    if IsEmptyFolder(Self.RootFolderNamespace) then
    self.BackGround.Caption:= _('Empty folder')
    else
    self.BackGround.Caption:= _('Contains hidden items');
    Self.BackGround.CaptionShow:= true;
  end
  else
  begin
    Self.BackGround.CaptionShow:= false;
  end;
end;

{-------------------------------------------------------------------------------
  WM_KillFocus
-------------------------------------------------------------------------------}
procedure TCEFileView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  //Self.ShowInactive:= true;
end;

{-------------------------------------------------------------------------------
  WM_SetFocus
-------------------------------------------------------------------------------}
procedure TCEFileView.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  //Self.ShowInactive:= false;
end;



{-------------------------------------------------------------------------------
  Get/Set Default
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetDefault: string;
begin
  Result:= ColSettingsToString(DefaultColSettings);
end;
procedure TCEColumnSettings.SetDefault(const Value: string);
begin
  StringToColSettings(Value, DefaultColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set MyComputer
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetMyComputer: string;
begin
  Result:= ColSettingsToString(MyComputerColSettings);
end;
procedure TCEColumnSettings.SetMyComputer(const Value: string);
begin
  StringToColSettings(Value, MyComputerColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set Network
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetNetwork: string;
begin
  Result:= ColSettingsToString(NetworkColSettings);
end;
procedure TCEColumnSettings.SetNetwork(const Value: string);
begin
  StringToColSettings(Value, NetworkColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set ControlPanel
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetControlPanel: string;
begin
  Result:= ColSettingsToString(ControlPanelColSettings);
end;
procedure TCEColumnSettings.SetControlPanel(const Value: string);
begin
  StringToColSettings(Value, ControlPanelColSettings);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Column Settings to string
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GroupBySettingsToString(GroupBySetting:
    TCEGroupBySetting): string;
begin
  Result:= IntToStr(GroupBySetting.Index) + ',' + BoolToStr(GroupBySetting.Enabled, true);
end;

{-------------------------------------------------------------------------------
  String to Column Settings
-------------------------------------------------------------------------------}
procedure TCEGroupBySettings.StringToGroupBySettings(AString: String; var
    GroupBySetting: TCEGroupBySetting);
var
  groupList: TStrings;
begin
  groupList:= TStringList.Create;
  try
    groupList.CommaText:= AString;
    if groupList.Count > 1 then
    begin
      GroupBySetting.Index:= StrToIntDef(groupList.Strings[0], 0);
      GroupBySetting.Enabled:= StrToBoolDef(groupList.Strings[1], false);
    end;
  finally
    groupList.Free;
  end;    
end;

{-------------------------------------------------------------------------------
  Get/Set Default
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetDefault: string;
begin
  Result:= GroupBySettingsToString(DefaultGroupBySettings);
end;

procedure TCEGroupBySettings.SetDefault(const Value: string);
begin
  StringToGroupBySettings(Value, DefaultGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set MyComputer
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetMyComputer: string;
begin
  Result:= GroupBySettingsToString(MyComputerGroupBySettings);
end;

procedure TCEGroupBySettings.SetMyComputer(const Value: string);
begin
  StringToGroupBySettings(Value, MyComputerGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set Network
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetNetwork: string;
begin
  Result:= GroupBySettingsToString(NetworkGroupBySettings);
end;

procedure TCEGroupBySettings.SetNetwork(const Value: string);
begin
  StringToGroupBySettings(Value, NetworkGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set ControlPanel
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetControlPanel: string;
begin
  Result:= GroupBySettingsToString(ControlPanelGroupBySettings);
end;

procedure TCEGroupBySettings.SetControlPanel(const Value: string);
begin
  StringToGroupBySettings(Value, ControlPanelGroupBySettings);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCECellSizeSettings
-------------------------------------------------------------------------------}
constructor TCECellSizeSettings.Create;
var
  hdcScreen: hDC;
begin
  inherited;
  hdcScreen := GetDC(GetDesktopWindow);
  try
    // Default values
    fLargeIcons_Width:= Round(DEFAULT_WIDTH_ICON * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fLargeIcons_Height:= Round(DEFAULT_HEIGHT_ICON * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fSmallIcons_Width:= Round(DEFAULT_WIDTH_SMALLICON * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fSmallIcons_Height:= Round(DEFAULT_HEIGHT_SMALLICON * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fList_Width:= Round(DEFAULT_WIDTH_LIST * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fList_Height:= Round(DEFAULT_HEIGHT_LIST * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fDetails_Width:= Round(DEFAULT_WIDTH_REPORT * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fDetails_Height:= Round(DEFAULT_HEIGHT_REPORT * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fTiles_Width:= Round(DEFAULT_WIDTH_TILE * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fTiles_Height:= Round(DEFAULT_HEIGHT_TILE * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fThumbnails_Width:= Round(DEFAULT_WIDTH_THUMBNAIL * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fThumbnails_Height:= Round(DEFAULT_HEIGHT_THUMBNAIL * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
    fFilmstrip_Width:= Round(DEFAULT_WIDTH_THUMBNAIL * GetDeviceCaps(hdcScreen, LOGPIXELSX)/DEFAULT_PIXEL_PER_INCH);
    fFilmstrip_Height:= Round(DEFAULT_HEIGHT_THUMBNAIL * GetDeviceCaps(hdcScreen, LOGPIXELSY)/DEFAULT_PIXEL_PER_INCH);
  finally
    ReleaseDC(GetDesktopWindow, hdcScreen)
  end
end;

function TCECellSizeSettings.IsChanged(CompareTo: TCECustomFileView): Boolean;
begin
  if assigned(CompareTo) then
  begin
    Result:= LargeIcons_Width <> CompareTo.CellSizes.Icon.Width;
    if not Result then Result:= LargeIcons_Height <> CompareTo.CellSizes.Icon.Height;
    if not Result then Result:= SmallIcons_Width <> CompareTo.CellSizes.SmallIcon.Width;
    if not Result then Result:= SmallIcons_Height <> CompareTo.CellSizes.SmallIcon.Height;
    if not Result then Result:= List_Width <> CompareTo.CellSizes.List.Width;
    if not Result then Result:= List_Height <> CompareTo.CellSizes.List.Height;
    if not Result then Result:= Details_Width <> CompareTo.CellSizes.Report.Width;
    if not Result then Result:= Details_Height <> CompareTo.CellSizes.Report.Height;
    if not Result then Result:= Tiles_Width <> CompareTo.CellSizes.Tile.Width;
    if not Result then Result:= Tiles_Height <> CompareTo.CellSizes.Tile.Height;
    if not Result then Result:= Thumbnails_Width <> CompareTo.CellSizes.Thumbnail.Width;
    if not Result then Result:= Thumbnails_Height <> CompareTo.CellSizes.Thumbnail.Height;
    if not Result then Result:= Filmstrip_Width <> CompareTo.CellSizes.FilmStrip.Width;
    if not Result then Result:= Filmstrip_Height <> CompareTo.CellSizes.FilmStrip.Height;
  end
  else
  Result:= false;
end;

end.
