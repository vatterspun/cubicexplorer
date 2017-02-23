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
//  The Original Code is dCE_Settings.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit dCE_Settings;

interface

uses
  // CE Units
  CE_Utils, CE_GlobalCtrl, fCE_FileView, CE_FileView, CE_BaseFileView,
  CE_AppStorage, fCE_TabPage,
  // JVCL
  JvFormPlacement, JvComponentBase, JvAppStorage,
  JvAppXMLStorage, JvJVCLUtils,
  // SpTBX
  SpTBXSkins,
  // EasyListView, VSTools
  EasyListView, MPShellUtilities, VirtualShellNotifier, VirtualExplorerTree,
  // TNT
  TntClasses, TntSysUtils,
  // SynEdit
  SynEditOptionsDialog, SynEdit,
  // System Units
  SysUtils, Classes, Forms, Contnrs, Windows, Controls,
  VirtualExplorerEasyListview;

type
  TToolbarSettings = class(TComponent, IJvAppStorageHandler)
  public
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
  end;

  TMainFormSettings = class(TComponent, IJvAppStorageHandler)
  private
    fRememberTabs: Boolean;
    fSingleInstance: Boolean;
  public
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure RestoreTabs(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure StoreTabs(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    property RememberTabs: Boolean read fRememberTabs write fRememberTabs;
    property SingleInstance: Boolean read fSingleInstance write fSingleInstance;
  end;

  TFileViewOptions = class(TComponent, IJvAppStorageHandler)
  private
    fFullRowSelect: Boolean;
    fHiddenFiles: Boolean;
    fShowExtensions: Boolean;
    fShowHeaderAlways: Boolean;
    NotifyList: TComponentList;
    procedure SetFullRowSelect(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
    procedure SetShowExtensions(const Value: Boolean);
    procedure SetShowHeaderAlways(const Value: Boolean);
    procedure SetSmoothScroll(const Value: Boolean);
  public
    CellSizes: Array [0..6] of TPoint;
    ColumnSettings: TCEColumnSettings;
    fSmoothScroll: Boolean;
    ViewStyle: TEasyListStyle;
    ThumbsPosition: TAlign;
    ThumbViewSize: Integer;
    ThumbViewStyle: TEasyListStyle;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignToFileView(FileViewPage: TCEFileViewPage);
    procedure AssignFromFileView(FileViewPage: TCEFileViewPage);
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure AssignFromActivePage;
    procedure RegisterNotify(FileViewPage: TComponent);
    procedure SendChanges;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    property FullRowSelect: Boolean read fFullRowSelect write SetFullRowSelect;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
    property ShowExtensions: Boolean read fShowExtensions write SetShowExtensions;
    property ShowHeaderAlways: Boolean read fShowHeaderAlways write
        SetShowHeaderAlways;
    property SmoothScroll: Boolean read fSmoothScroll write SetSmoothScroll;
  end;

  TTextEditorSettings = class(TComponent, IJvAppStorageHandler)
  public
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
  end;

  TFileSearchSettings = class(TComponent, IJvAppStorageHandler)
  private
    ColSettings: TCEColSettings;
    fShowExtensions: Boolean;
    fSubFolders: Boolean;
    NotifyList: TComponentList;
    procedure SetShowExtensions(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignFromPage(Page: TCECustomTabPage);
    procedure AssignToPage(Page: TCECustomTabPage);
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure RegisterNotify(FileSearchPage: TComponent);
    procedure RestoreColSettings(FileView: TVirtualExplorerEasyListview);
    procedure SaveColSettings(FileView: TVirtualExplorerEasyListview);
    procedure SendChanges;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    property ShowExtensions: Boolean read fShowExtensions write SetShowExtensions;
    property SubFolders: Boolean read fSubFolders write fSubFolders;
  end;

  TCESettings = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fFilePath: WideString;
    { Private declarations }
  public
    FileStorage: TCEAppStorage;
    ControlList: TComponentList;
    MainFormSettings: TMainFormSettings;
    ToolbarSettings: TToolbarSettings;
    FileViewOptions: TFileViewOptions;
    TextEditorSettings: TTextEditorSettings;
    FileSearchSettings: TFileSearchSettings;
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadSettings(AppStoragePath: String = '');
    procedure SaveSettings(AppStoragePath: String = '');
    procedure SaveToFile(AFilePath: WideString = '');
    property FilePath: WideString read fFilePath write fFilePath;
    { Public declarations }
  end;

var
  CESettings: TCESettings;

implementation

uses
  fCE_FolderPanel, fCE_TextEditor, fCE_FileSearch, Main;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Read MainForm Settings from storage
-------------------------------------------------------------------------------}
procedure TMainFormSettings.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  Placement: TWindowPlacement;
begin
  if not assigned(AppStorage) then Exit;

  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'MainForm']);
    // MainForm position
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(MainForm.Handle, @Placement);

    Placement.rcNormalPosition:= StrToRect(AppStorage.ReadString('Pos', ' '), Application.MainForm.BoundsRect);
    Placement.showCmd:= AppStorage.ReadInteger('ShowCmd', 1);
    if Placement.showCmd = SW_SHOWMINIMIZED then
    Placement.showCmd:= SW_SHOWNORMAL;
    SetWindowPlacement(MainForm.Handle, @Placement);
    // theme
    SkinManager.SetSkin(AppStorage.ReadString('Theme', 'Leopard'));
    // transparency
    MainForm.AlphaBlendValue:= AppStorage.ReadInteger('Alpha',255);
    // Misc settings
    MainForm.ShowHint:= AppStorage.ReadBoolean('Hints', true);
    fSingleInstance:= AppStorage.ReadBoolean('SingleInstance', false);
    fRememberTabs:= AppStorage.ReadBoolean('RememberTabs', false);
    Mainform.StatusBar.Visible:= AppStorage.ReadBoolean('StatusBar',true);
    //RestoreLastTabs(AppStorage, '');
    
    // Language settings
    MainForm.ActiveLanguage:= AppStorage.ReadString('Language', '');
  finally
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Write MainForm Settings to storage
-------------------------------------------------------------------------------}
procedure TMainFormSettings.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  Placement: TWindowPlacement;
begin
  if not assigned(AppStorage) then Exit;

  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'MainForm']);
    // MainForm Position
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(MainForm.Handle, @Placement);
    AppStorage.WriteString('Pos', RectToStr(Placement.rcNormalPosition));
    AppStorage.WriteInteger('ShowCmd', Placement.showCmd);
    // theme
    AppStorage.WriteString('Theme', SkinManager.CurrentSkinName);
    // transparency
    AppStorage.WriteInteger('Alpha',MainForm.AlphaBlendValue);
    // Misc settings
    AppStorage.WriteBoolean('Hints',MainForm.ShowHint);
    AppStorage.WriteBoolean('SingleInstance',fSingleInstance);
    AppStorage.WriteBoolean('RememberTabs',fRememberTabs);
    AppStorage.WriteBoolean('StatusBar', MainForm.StatusBar.Visible);
    // Language Settings
    AppStorage.WriteString('Language', MainForm.ActiveLanguage);

    if not fRememberTabs then
    AppStorage.DeleteSubTree('LastTabs');

  finally
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Restore tabs
-------------------------------------------------------------------------------}
procedure TMainFormSettings.RestoreTabs(AppStorage: TJvCustomAppStorage; const
    BasePath: string);
var
  i: Integer;
  page: TCECustomTabPage;
  pageClass: TCECustomTabPageClass;
  s: String;
begin
  MainForm.TabBar.AllowUnselected:= true;
  MainForm.TabBar.IsUpdating:= true;
  try
    i:= 0;
    s:= BasePath + 'Tab_0';
    while AppStorage.PathExists(s) do
    begin
      if AppStorage.PathExists(s + '\FileView') then
      begin
        pageClass:= TCEFileViewPage;
        s:= s + '\FileView\';
      end
      else if AppStorage.PathExists(s + '\TextEditor') then
      begin
        pageClass:= TCETextEditorPage;
        s:= s + '\TextEditor\';
      end
      else if AppStorage.PathExists(s + '\FileSearch') then
      begin
        pageClass:= TCEFileSearchPage;
        s:= s + '\FileSearch\';
      end
      else
      begin
        pageClass:= nil;
      end;

      if assigned(pageClass) then
      begin
        page:= TCECustomTabPage(MainForm.TabBar.AddTab(pageClass, false).Data);
        page.Hide;
        page.ReadFromAppStorage(AppStorage, s);
      end;

      Inc(i,1);
      s:= BasePath + 'Tab_' + IntToStr(i);
    end;
  finally
    MainForm.TabBar.AllowUnselected:= false;
    MainForm.TabBar.IsUpdating:= false;
  end;
  i:= AppStorage.ReadInteger(BasePath + 'SelectedTab', 0);
  if (MainForm.TabBar.Tabs.Count > i) and (i > -1) then
  begin
    TCECustomTabPage(MainForm.TabBar.Tabs.Items[i].Data).Visible:= true;
    MainForm.TabBar.SelectedTab:= MainForm.TabBar.Tabs.Items[i];
  end;
end;

{*------------------------------------------------------------------------------
  Store tabs
-------------------------------------------------------------------------------}
procedure TMainFormSettings.StoreTabs(AppStorage: TJvCustomAppStorage; const
    BasePath: string);
var
  i: Integer;
  page: TCECustomTabPage;
begin
  AppStorage.DeleteSubTree(BasePath);
  for i:= 0 to MainForm.TabBar.Tabs.Count - 1 do
  begin
    page:= MainForm.TabBar.GetPageSafe(MainForm.TabBar.Tabs.Items[i]);
    if assigned(page) then
    begin
      if page is TCEFileViewPage then
      page.WriteToAppStorage(AppStorage, BasePath + 'Tab_' + IntToStr(i) + '\FileView\')
      else if page is TCETextEditorPage then
      page.WriteToAppStorage(AppStorage, BasePath + 'Tab_' + IntToStr(i) + '\TextEditor\')
      else if page is TCEFileSearchPage then
      page.WriteToAppStorage(AppStorage, BasePath + 'Tab_' + IntToStr(i) + '\FileSearch\');
    end;
  end;

  if assigned(MainForm.TabBar.SelectedTab) then
  AppStorage.WriteInteger(BasePath + 'SelectedTab',MainForm.TabBar.SelectedTab.Index);
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Read toolbar Settings from storage
-------------------------------------------------------------------------------}
procedure TToolbarSettings.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  nodePath: String;
begin
  if not assigned(AppStorage) then Exit;
  nodePath:= BasePath+'\Toolbars';
end;

{*------------------------------------------------------------------------------
  Write toolbar Settings to storage
-------------------------------------------------------------------------------}
procedure TToolbarSettings.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  nodePath: String;
begin
  if not assigned(AppStorage) then Exit;
  nodePath:= BasePath+'\Toolbars';
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Get's called when TCESettings is created
-------------------------------------------------------------------------------}
procedure TCESettings.DataModuleCreate(Sender: TObject);
begin
  FileStorage:= TCEAppStorage.Create(self);
  FileStorage.AutoFlush:= false;
  FileStorage.AutoReload:= false;
  FileStorage.FlushOnDestroy:= false;
  FileStorage.RootNodeName:= 'Configuration';
  
  ControlList:= TComponentList.Create(false);
  MainFormSettings:= TMainFormSettings.Create(self);
  ControlList.Add(MainFormSettings);
  ToolbarSettings:= TToolbarSettings.Create(self);
  ControlList.Add(ToolbarSettings);
  FileViewOptions:= TFileViewOptions.Create(self);
  ControlList.Add(FileViewOptions);
  TextEditorSettings:= TTextEditorSettings.Create(self);
  ControlList.Add(TextEditorSettings);
  FileSearchSettings:= TFileSearchSettings.Create(self);
  ControlList.Add(FileSearchSettings);
end;

{*------------------------------------------------------------------------------
  Get's called when TCESettings is destroyed
-------------------------------------------------------------------------------}
procedure TCESettings.DataModuleDestroy(Sender: TObject);
begin
  MainFormSettings.Free;
  ControlList.Free;
end;

{*------------------------------------------------------------------------------
  Load From File
-------------------------------------------------------------------------------}
procedure TCESettings.LoadFromFile(AFilePath: WideString);
var
  s: TStream;
begin
  fFilePath:= AFilePath;
  FileStorage.BeginUpdate;
  try
    if WideFileExists(AFilePath) then
    begin
      s:= TTntFileStream.Create(AFilePath,fmOpenRead or fmShareDenyNone);
      try
        FileStorage.Xml.LoadFromStream(s);
      finally
        s.Free;
      end;
    end
    else
    begin
      FileStorage.Xml.LoadFromResourceName(hInstance, 'DEFAULT_SETTINGS_DATA');
    end;
  finally
    FileStorage.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Load settings.
-------------------------------------------------------------------------------}
procedure TCESettings.LoadSettings(AppStoragePath: String = '');
var
  i: Integer;
  handler: IJvAppStorageHandler;
begin
  try
    FileStorage.BeginUpdate;
    for i:= 0 to ControlList.Count - 1 do
    begin
      if Supports(ControlList.Items[i], IJvAppStorageHandler, handler) then
      begin
        handler.ReadFromAppStorage(FileStorage, AppStoragePath);
      end;
    end;
  finally
    FileStorage.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
procedure TCESettings.SaveToFile(AFilePath: WideString = '');
var
  s: TStream;
begin
  if AFilePath <> '' then
  fFilePath:= AFilePath;

  try
    s:= TTntFileStream.Create(fFilePath,fmCreate);
  except
    Exit;
  end;

  try
    FileStorage.Xml.SaveToStream(s);
  finally
    s.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Save Settings
-------------------------------------------------------------------------------}
procedure TCESettings.SaveSettings(AppStoragePath: String = '');
var
  i: Integer;
  handler: IJvAppStorageHandler;
begin
  try
    FileStorage.BeginUpdate;
    for i:= 0 to ControlList.Count - 1 do
    begin
      if Supports(ControlList.Items[i], IJvAppStorageHandler, handler) then
      begin
        handler.WriteToAppStorage(FileStorage, AppStoragePath);
      end;
    end;
  finally
    FileStorage.EndUpdate;
  end;
end;



{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TFileViewOptions
-------------------------------------------------------------------------------}
constructor TFileViewOptions.Create(AOwner: TComponent);
begin
  inherited;
  ColumnSettings:= TCEColumnSettings.Create;
  NotifyList:= TComponentList.Create(false);
  ThumbsPosition:= alBottom;
  ThumbViewStyle:= elsFilmstrip;
  ThumbViewSize:= 120;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TFileViewOptions
-------------------------------------------------------------------------------}
destructor TFileViewOptions.Destroy;
begin
  NotifyList.Free;
  ColumnSettings.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign options from FileView.
-------------------------------------------------------------------------------}
procedure TFileViewOptions.AssignFromFileView(FileViewPage: TCEFileViewPage);
begin
  if not assigned(FileViewPage) then
  Exit;

  ViewStyle:= FileViewPage.ViewStyle;
  ThumbViewStyle:= FileViewPage.ThumbViewStyle;
  ThumbsPosition:= FileViewPage.ThumbPosition;
  ThumbViewSize:= FileViewPage.ThumbViewSize;
end;

{*------------------------------------------------------------------------------
  Assign options to FileView.
-------------------------------------------------------------------------------}
procedure TFileViewOptions.AssignToFileView(FileViewPage: TCEFileViewPage);
var
  i: Integer;
begin
  if not assigned(FileViewPage) then
  Exit;
  
  FileViewPage.ThumbViewSize:= ThumbViewSize;
  FileViewPage.ViewStyle:= ViewStyle;
  FileViewPage.ThumbViewStyle:= ThumbViewStyle;
  FileViewPage.ThumbPosition:= ThumbsPosition;
  FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
  if fHiddenFiles then
  FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
  else
  FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters];
  FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
  if fShowHeaderAlways then
  FileViewPage.FileView.Header.Visible:= true;
  FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
  FileViewPage.FileView.ShowExtension:= fShowExtensions;
  for i:= 0 to Length(CellSizes) - 1 do
  begin
    case i of
      0: FileViewPage.FileView.CellSizes.Icon.SetSize(CellSizes[i].X, CellSizes[i].Y);
      1: FileViewPage.FileView.CellSizes.SmallIcon.SetSize(CellSizes[i].X, CellSizes[i].Y);
      2: FileViewPage.FileView.CellSizes.List.SetSize(CellSizes[i].X, CellSizes[i].Y);
      3: FileViewPage.FileView.CellSizes.Report.SetSize(CellSizes[i].X, CellSizes[i].Y);
      4: FileViewPage.FileView.CellSizes.Tile.SetSize(CellSizes[i].X, CellSizes[i].Y);
      5: FileViewPage.FileView.CellSizes.Thumbnail.SetSize(CellSizes[i].X, CellSizes[i].Y);
      6: FileViewPage.FileView.CellSizes.FilmStrip.SetSize(CellSizes[i].X, CellSizes[i].Y);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Settings From ActivePage
-------------------------------------------------------------------------------}
procedure TFileViewOptions.AssignFromActivePage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  AssignFromFileView(TCEFileViewPage(GlobalPathCtrl.ActivePage));
end;

{*------------------------------------------------------------------------------
  Read FileView Settings from storage
-------------------------------------------------------------------------------}
procedure TFileViewOptions.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  i: Integer;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FileView']);
    // Read settings
    ViewStyle:= TEasyListStyle(AppStorage.ReadInteger('ViewStyle', 0));
    ThumbsPosition:= TAlign(AppStorage.ReadInteger('ThumbPos', 2));
    case ThumbsPosition of
      alNone, alClient, alCustom: ThumbsPosition:= alBottom;
    end;
    
    ThumbViewStyle:= TEasyListStyle(AppStorage.ReadInteger('ThumbViewStyle', 6));
    ThumbViewSize:= AppStorage.ReadInteger('ThumbViewSize', 120);
    fSmoothScroll:= AppStorage.ReadBoolean('SmoothScroll', false);
    fHiddenFiles:= AppStorage.ReadBoolean('HiddenFiles', false);
    ColumnSettings.ReadFromAppStorage(AppStorage);
    fShowHeaderAlways:= AppStorage.ReadBoolean('ShowHeaderAlways',false);
    fFullRowSelect:= AppStorage.ReadBoolean('FullRowSelect',true);
    fShowExtensions:= AppStorage.ReadBoolean('ShowExtensions',true);
    // Temporary control for folder tree
    if fHiddenFiles then
    CEFolderPanel.FolderTree.FileObjects:= [foFolders,foHidden]
    else
    CEFolderPanel.FolderTree.FileObjects:= [foFolders];
    // Notify Refresh Rate
    VirtualShellNotifyRefreshRate:= AppStorage.ReadInteger('NotifyRefreshRate', 100);

    for i:= 0 to Length(CellSizes) -1  do
    begin
      case i of
        0: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\Icon','75,75'));
        1: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\SmallIcon','200,17'));
        2: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\List','250,17'));
        3: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\Details','75,17'));
        4: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\Tiles','218,58'));
        5: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\Thumbnails','125,143'));
        6: CellSizes[i]:= CEStrToPoint(AppStorage.ReadString('CellSizes\Filmstrip','125,143'));
      end;

      if (CellSizes[i].X = 0) or (CellSizes[i].Y = 0) then
      begin
        case i of
          0: CellSizes[i]:= Point(75,75);
          1: CellSizes[i]:= Point(200,17);
          2: CellSizes[i]:= Point(250,17);
          3: CellSizes[i]:= Point(75,17);
          4: CellSizes[i]:= Point(218,58);
          5: CellSizes[i]:= Point(125,143);
          6: CellSizes[i]:= Point(125,143);
        end;
      end;
    end;

  finally
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Register Notify
-------------------------------------------------------------------------------}
procedure TFileViewOptions.RegisterNotify(FileViewPage: TComponent);
begin
  NotifyList.Add(FileViewPage);
end;

{*------------------------------------------------------------------------------
  Send Changes
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SendChanges;
var
  i: Integer;
  FileViewPage: TCEFileViewPage;
  doRebuild: Boolean;
begin
  for i:= 0 to NotifyList.Count - 1 do
  begin
    if NotifyList.Items[i] is TCEFileViewPage then
    begin
      FileViewPage:= TCEFileViewPage(NotifyList.Items[i]);
      FileViewPage.FileView.BeginUpdate;
      FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
      if fHiddenFiles then
      FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
      else
      FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters]
      FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
      if fShowHeaderAlways then
      FileViewPage.FileView.Header.Visible:= true;
      FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
      
      doRebuild:=  FileViewPage.FileView.ShowExtension <> fShowExtensions;
      FileViewPage.FileView.ShowExtension:= fShowExtensions;
      
      if doRebuild then FileViewPage.FileView.Rebuild;
      FileViewPage.FileView.EndUpdate(FileViewPage.Visible);
    end
    else if NotifyList.Items[i] is TVirtualExplorerTree then
    begin
      if fHiddenFiles then
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
      else
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders];
    end;     
  end;
end;

{*------------------------------------------------------------------------------
  Set Full Row select
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SetFullRowSelect(const Value: Boolean);
begin
  fFullRowSelect:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Hidden Files
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SetHiddenFiles(const Value: Boolean);
begin
  fHiddenFiles := Value;
  SendChanges;
  // Temporary control for folder tree
  if fHiddenFiles then
  CEFolderPanel.FolderTree.FileObjects:= [foFolders,foHidden]
  else
  CEFolderPanel.FolderTree.FileObjects:= [foFolders];
end;

{*------------------------------------------------------------------------------
  Set ShowExtensions
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SetShowExtensions(const Value: Boolean);
begin
  fShowExtensions:= Value;
  SendChanges;
  
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Refresh;
end;

{*------------------------------------------------------------------------------
  Set Show Header Always
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SetShowHeaderAlways(const Value: Boolean);
begin
  fShowHeaderAlways:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Smooth Scroll
-------------------------------------------------------------------------------}
procedure TFileViewOptions.SetSmoothScroll(const Value: Boolean);
begin
  fSmoothScroll:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Write FileView Settings to storage
-------------------------------------------------------------------------------}
procedure TFileViewOptions.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  i: Integer;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FileView']);
    // Write settings
    AppStorage.WriteInteger('ViewStyle', Ord(ViewStyle));
    AppStorage.WriteInteger('ThumbPos', Ord(ThumbsPosition));
    AppStorage.WriteInteger('ThumbViewStyle', Ord(ThumbViewStyle));
    AppStorage.WriteInteger('ThumbViewSize', ThumbViewSize);
    AppStorage.WriteBoolean('SmoothScroll', fSmoothScroll);
    AppStorage.WriteBoolean('HiddenFiles', fHiddenFiles);
    AppStorage.WriteBoolean('ShowHeaderAlways',fShowHeaderAlways);
    AppStorage.WriteBoolean('FullRowSelect',fFullRowSelect);
    AppStorage.WriteBoolean('ShowExtensions',fShowExtensions);
    // Columns
    ColumnSettings.WriteToAppStorage(AppStorage);
    // Notify Refresh Rate
    AppStorage.WriteInteger('NotifyRefreshRate', VirtualShellNotifyRefreshRate);
    // CellSizes
    for i:= 0 to Length(CellSizes) -1  do
    begin
      case i of
        0: AppStorage.WriteString('CellSizes\Icon', CEPointToStr(CellSizes[i]));
        1: AppStorage.WriteString('CellSizes\SmallIcon', CEPointToStr(CellSizes[i]));
        2: AppStorage.WriteString('CellSizes\List', CEPointToStr(CellSizes[i]));
        3: AppStorage.WriteString('CellSizes\Details', CEPointToStr(CellSizes[i]));
        4: AppStorage.WriteString('CellSizes\Tiles', CEPointToStr(CellSizes[i]));
        5: AppStorage.WriteString('CellSizes\Thumbnails', CEPointToStr(CellSizes[i]));
        6: AppStorage.WriteString('CellSizes\Filmstrip', CEPointToStr(CellSizes[i]));
      end;
    end;
    
  finally
    AppStorage.Path:= oldPath;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Read TextEditor Settings from storage
-------------------------------------------------------------------------------}
procedure TTextEditorSettings.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= BasePath;
    // Read settings
    AppStorage.ReadPersistent('TextEditor', CETextEditorOptions.EditorOptions);
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'TextEditor']);
    CETextEditorOptions.WordWrap:= AppStorage.ReadBoolean('WordWrap',false);
  finally
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Write TextEditor Settings to storage
-------------------------------------------------------------------------------}
procedure TTextEditorSettings.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  list: TStrings;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;

  list:= TStringList.Create;
  try
    AppStorage.Path:= BasePath;
    // Ignore these
    list.Add('Tag');
    list.Add('Name');
    // Write settings
    AppStorage.WritePersistent('TextEditor',CETextEditorOptions.EditorOptions, true, list);
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'TextEditor']);
    AppStorage.WriteBoolean('WordWrap', CETextEditorOptions.WordWrap);
  finally
    AppStorage.Path:= oldPath;
    list.Free;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TFileSearchSettings
-------------------------------------------------------------------------------}
constructor TFileSearchSettings.Create(AOwner: TComponent);
begin
  inherited;
  NotifyList:= TComponentList.Create(false);
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TFileSearchSettings
-------------------------------------------------------------------------------}
destructor TFileSearchSettings.Destroy;
begin
  NotifyList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Read toolbar Settings from storage
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  i: Integer;
  colListSet, colList: TStrings;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;
  
  colListSet:= TStringList.Create;
  colList:= TStringList.Create;
  
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FileSearch']);
    fSubFolders:= AppStorage.ReadBoolean('SubFolders', false);
    fShowExtensions:= AppStorage.ReadBoolean('ShowExtensions', true);

    // Column settings
    colListSet.Delimiter:= '|';
    colList.Delimiter:= ',';
    colListSet.DelimitedText:= AppStorage.ReadString('Columns');
    
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
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Write toolbar Settings to storage
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  oldPath: String;
  i: Integer;
  s: String;
begin
  if not assigned(AppStorage) then Exit;
  oldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FileSearch']);
    AppStorage.WriteBoolean('SubFolders', fSubFolders);
    AppStorage.WriteBoolean('ShowExtensions', fShowExtensions);
    // Column settings
    s:= '';
    for i:= 0 to Length(ColSettings) - 1 do
    begin
      if i > 0 then
      s:= s + '|';

      s:= s + IntToStr(ColSettings[i].Index) + ',' +
              IntToStr(ColSettings[i].Position) + ',' +
              IntToStr(ColSettings[i].Width) + ',' +
              IntToStr(Integer(ColSettings[i].Sort));
    end;
    AppStorage.WriteString('Columns', s);
  finally
    AppStorage.Path:= oldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Assign Settings from page
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.AssignFromPage(Page: TCECustomTabPage);
begin
  if Page is TCEFileSearchPage then
  begin
    fSubFolders:= TCEFileSearchPage(Page).check_subdir.Checked;
    SaveColSettings(TCEFileSearchPage(Page).Results);
  end;
end;

{*------------------------------------------------------------------------------
  Assign settings to page
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.AssignToPage(Page: TCECustomTabPage);
begin
  if Page is TCEFileSearchPage then
  begin
    TCEFileSearchPage(Page).check_subdir.Checked:= fSubFolders;
    TCEFileSearchPage(Page).Results.ShowExtension:= fShowExtensions;
    RestoreColSettings(TCEFileSearchPage(Page).Results);
  end;
end;

{*------------------------------------------------------------------------------
  Register Notify
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.RegisterNotify(FileSearchPage: TComponent);
begin
  NotifyList.Add(FileSearchPage);
end;

{*------------------------------------------------------------------------------
  Restore Column Settings
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.RestoreColSettings(FileView:
    TVirtualExplorerEasyListview);
var
  i: Integer;
  col: TEasyColumn;
begin
  if not assigned(FileView) then
  Exit;

  if Length(ColSettings) = 0 then
  Exit;

  FileView.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to FileView.Header.Columns.Count - 1 do
    begin
      if FileView.Header.Columns.Columns[i].Visible then
      FileView.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(ColSettings) - 1 do
    begin
      if ColSettings[i].Index < FileView.Header.Columns.Count then
      begin
        col:= FileView.Header.Columns.Columns[ColSettings[i].Index];
        col.Visible:= true;
        col.Position:= ColSettings[i].Position;
        col.Width:= ColSettings[i].Width;
        col.SortDirection:= ColSettings[i].Sort;
      end;
    end;
  finally
    FileView.Header.Columns.EndUpdate(true);
  end;
end;

{*------------------------------------------------------------------------------
  Save Column Settings
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.SaveColSettings(FileView:
    TVirtualExplorerEasyListview);
var
  col: TEasyColumn;
  i,c: Integer;
begin
  if not assigned(FileView) then
  Exit;

  c:= 0;
  for i:= 0 to FileView.Header.Columns.Count - 1 do
  begin
    if FileView.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(ColSettings) <> c then
  SetLength(ColSettings, c);
  
  i:= 0;

  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    ColSettings[i].Index:= col.Index;
    ColSettings[i].Position:= col.Position;
    ColSettings[i].Width:= col.Width;
    ColSettings[i].Sort:= col.SortDirection;
    col:= FileView.Header.NextVisibleColumn(col);
    inc(i);
  end;
end;

{*------------------------------------------------------------------------------
  Send Changes
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.SendChanges;
var
  i: Integer;
  FileSearchPage: TCEFileSearchPage;
begin
  for i:= 0 to NotifyList.Count - 1 do
  begin
    if NotifyList.Items[i] is TCEFileSearchPage then
    begin
      FileSearchPage:= TCEFileSearchPage(NotifyList.Items[i]);
      FileSearchPage.Results.ShowExtension:= fShowExtensions;
      // Repaint
      if FileSearchPage.Visible then
      FileSearchPage.Results.Repaint;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set ShowExtensions
-------------------------------------------------------------------------------}
procedure TFileSearchSettings.SetShowExtensions(const Value: Boolean);
begin
  if fShowExtensions <> Value then
  begin
    fShowExtensions:= Value;
    SendChanges;
  end;
end;




end.
