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
//  The Original Code is CubicExplorer.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

program CubicExplorer;

{%File 'Muokkaukset.txt'}


uses
  FastMM4,
  ControlResizeBugFix,
  madExcept,
  madLinkDisAsm,
  Forms,
  Controls,
  Messages,
  Windows,
  TntSystem,
  MPShellUtilities in 'Components\Mustangpeak\CommonLibrary\MPShellUtilities.pas',
  Main in 'Main.pas' {MainForm},
  CE_DockInfo in 'CE_DockInfo.pas',
  CE_Utils in 'CE_Utils.pas',
  CE_Layout in 'CE_Layout.pas',
  fCE_TabPage in 'fCE_TabPage.pas' {CECustomTabPage: TFrame},
  fCE_FileView in 'fCE_FileView.pas' {CEFileViewPage: TFrame},
  fCE_DockHostForm in 'fCE_DockHostForm.pas' {CEDockHostForm},
  fCE_DockableForm in 'fCE_DockableForm.pas' {CECustomDockableForm},
  CE_GlobalCtrl in 'CE_GlobalCtrl.pas',
  CE_Consts in 'CE_Consts.pas',
  CE_BaseFileView in 'CE_BaseFileView.pas',
  CE_FileView in 'CE_FileView.pas',
  fCE_FolderPanel in 'fCE_FolderPanel.pas' {CEFolderPanel},
  fCE_BookmarkPanel in 'fCE_BookmarkPanel.pas' {CEBookmarkPanel},
  CE_GifAnim in 'CE_GifAnim.pas',
  fCE_QuickViewPanel in 'fCE_QuickViewPanel.pas' {CEQuickViewPanel},
  dCE_Actions in 'dCE_Actions.pas' {CEActions: TDataModule},
  Documents in 'Documents.pas',
  dCE_Images in 'dCE_Images.pas' {CE_Images: TDataModule},
  CEJvDockVSNetStyleTBX in 'CEJvDockVSNetStyleTBX.pas',
  fCE_TextEditor in 'fCE_TextEditor.pas' {CETextEditorPage: TFrame},
  fCE_AboutBox in 'fCE_AboutBox.pas' {CEAboutBox},
  CE_DriveBar in 'CE_DriveBar.pas',
  CE_BookmarkBar in 'CE_BookmarkBar.pas',
  fCE_Customizer in 'fCE_Customizer.pas' {CEToolbarCustomizer},
  CE_Toolbar in 'CE_Toolbar.pas',
  CE_ToolbarButtons in 'CE_ToolbarButtons.pas',
  CE_StatusBar in 'CE_StatusBar.pas',
  CE_VistaFuncs in 'CE_VistaFuncs.pas',
  CE_StdBookmarkComps in 'CE_StdBookmarkComps.pas',
  CE_Bookmarks in 'CE_Bookmarks.pas',
  CE_BookmarkTree in 'CE_BookmarkTree.pas',
  dCE_Input in 'dCE_Input.pas' {CEInput: TDataModule},
  CE_Breadcrumb in 'CE_Breadcrumb.pas',
  CE_ScrollToolbar in 'CE_ScrollToolbar.pas',
  fCE_FolderTreeForm in 'fCE_FolderTreeForm.pas' {CE_FolderTreeForm},
  CE_XMLStorage in 'CE_XMLStorage.pas',
  CE_AddressToolbar in 'CE_AddressToolbar.pas',
  fCE_ExtAppPage in 'fCE_ExtAppPage.pas' {CEExtAppTabPage: TFrame},
  fCE_FiltersPanel in 'fCE_FiltersPanel.pas' {CEFiltersPanel},
  CE_FilterPanel in 'CE_FilterPanel.pas',
  fCE_BookmarkPropDlg in 'fCE_BookmarkPropDlg.pas' {BookmarkPropDlg},
  CE_FolderTree in 'CE_FolderTree.pas',
  CE_Classes in 'CE_Classes.pas',
  CE_ContextMenu in 'CE_ContextMenu.pas',
  CE_CommonObjects in 'CE_CommonObjects.pas',
  CE_LanguageCodes in 'CE_LanguageCodes.pas',
  fCE_PoEditor_NewForm in 'fCE_PoEditor_NewForm.pas' {CENewTranslationDlg},
  CE_ProcessUtils in 'CE_ProcessUtils.pas',
  fCE_PoEditor in 'fCE_PoEditor.pas' {CEPoEditor: TFrame},
  CE_LanguageUtils in 'CE_LanguageUtils.pas',
  CE_LanguageEngine in 'CE_LanguageEngine.pas',
  fCE_OptionsDialog in 'fCE_OptionsDialog.pas' {CEOptionsDialog},
  fCE_OptionsCustomPage in 'fCE_OptionsCustomPage.pas' {CEOptionsCustomPage: TFrame},
  fCE_OptionsPage_General in 'fCE_OptionsPage_General.pas' {CEOptionsPage_General: TFrame},
  fCE_OptionsPage_Tabs in 'fCE_OptionsPage_Tabs.pas' {CEOptionsPage_Tabs: TFrame},
  fCE_OptionsPage_Display in 'fCE_OptionsPage_Display.pas' {CEOptionsPage_Display: TFrame},
  fCE_OptionsPage_Display_Bookmarks in 'fCE_OptionsPage_Display_Bookmarks.pas' {CE_OptionsPage_Display_Bookmarks: TFrame},
  fCE_OptionsPage_Display_FolderTree in 'fCE_OptionsPage_Display_FolderTree.pas' {CE_OptionsPage_Display_FolderTree: TFrame},
  fCE_OptionsPage_Display_FileView in 'fCE_OptionsPage_Display_FileView.pas' {CE_OptionsPage_Display_FileView: TFrame},
  fCE_StackPanel in 'fCE_StackPanel.pas' {CEStackPanel},
  CE_SpTabBar in 'CE_SpTabBar.pas',
  JvDockVSNetStyle in 'Components\jvcl\JvDockVSNetStyle.pas',
  JvDockControlForm in 'Components\jvcl\JvDockControlForm.pas',
  JvDockTree in 'Components\jvcl\JvDockTree.pas',
  fCE_QuickViewTab in 'fCE_QuickViewTab.pas' {CEQuickViewPage: TFrame},
  CE_AppSettings in 'CE_AppSettings.pas',
  CE_Sessions in 'CE_Sessions.pas',
  fCE_OptionsPage_Advanced in 'fCE_OptionsPage_Advanced.pas' {CEOptionsPage_Advanced: TFrame},
  fCE_SessionManager in 'fCE_SessionManager.pas' {CESessionManager},
  fCE_SearchPage in 'fCE_SearchPage.pas' {CESearchPage: TFrame},
  CE_InfoBar in 'CE_InfoBar.pas',
  FindFileW in 'Components\FindFile\FindFileW.pas',
  CE_ElevatedActions in 'CE_ElevatedActions.pas',
  CE_FileUtils in 'CE_FileUtils.pas',
  fCE_CreateSymlink in 'fCE_CreateSymlink.pas' {CreateSymlinkDlg},
  SpTBXChromeSkin in 'Components\SpSkins\SpTBXChromeSkin.pas',
  fCE_OptionsPage_Hotkeys in 'fCE_OptionsPage_Hotkeys.pas' {TCEOptionsPage_Hotkeys: TFrame},
  fCE_OptionsPage_GlobalHotkeys in 'fCE_OptionsPage_GlobalHotkeys.pas' {TCEOptionsPage_GlobalHotkeys: TFrame},
  fCE_ColumnFormSpTBX in 'fCE_ColumnFormSpTBX.pas' {CEFormColumnSettings: TTntForm},
  CE_Stacks in 'CE_Stacks.pas',
  CE_StackTree in 'CE_StackTree.pas',
  TB2Item in 'Components\Tb2K\TB2Item.pas',
  CE_ToolbarEditorItems in 'CE_ToolbarEditorItems.pas',
  EasyListview in 'Components\Mustangpeak\EasyListview\EasyListview.pas',
  VirtualExplorerEasyListview in 'Components\Mustangpeak\VSTools\VirtualExplorerEasyListview.pas',
  fCE_OptionsPage_Display_Stack in 'fCE_OptionsPage_Display_Stack.pas' {CE_OptionsPage_Display_Stack: TFrame},
  AppCommand in 'AppCommand.pas',
  fCE_ItemSelectSaveDlg in 'fCE_ItemSelectSaveDlg.pas' {CEItemSelectSaveDlg},
  VirtualExplorerTree in 'Components\Mustangpeak\VSTools\VirtualExplorerTree.pas',
  CE_VersionUpdater in 'CE_VersionUpdater.pas',
  CE_XmlUtils in 'CE_XmlUtils.pas',
  fCE_UpdateDlg in 'fCE_UpdateDlg.pas' {CEUpdateDlg},
  fCE_VersionMgrForm in 'fCE_VersionMgrForm.pas' {CEVersionMgrForm: TTntForm},
  CC_Threads in 'Components\CubicCore\CC_Threads.pas',
  CE_ArchiveTree in 'CE_ArchiveTree.pas',
  JclCompressionWide in 'Components\jcl\JclCompressionWide.pas',
  fCE_ArchivePanel in 'fCE_ArchivePanel.pas' {CEArchiverPanel},
  fCE_OptionsPage_General_Updates in 'fCE_OptionsPage_General_Updates.pas' {CE_OptionsPage_General_Updates: TFrame},
  fCE_LoginPromptDlg in 'fCE_LoginPromptDlg.pas' {CELoginPromptDlg},
  CE_SystemUtils in 'CE_SystemUtils.pas',
  CE_SpTBXItems in 'CE_SpTBXItems.pas',
  fCE_QuickView in 'fCE_QuickView.pas' {CEQuickView: TFrame},
  CE_FilePreview in 'CE_FilePreview.pas',
  CV_ImageView in 'CV_ImageView.pas',
  CV_MediaPlayer in 'CV_MediaPlayer.pas',
  CV_MediaPlayerEngines in 'CV_MediaPlayerEngines.pas',
  CV_Playlist in 'CV_Playlist.pas';

{$R *.res}
{$R 'CE_Resources.res'}

{.$define FullDebugMode}

var
  h: HWND;
  ws: WideString;
  i: Integer;
  copyDataStruct : TCopyDataStruct;
begin
  // Handle elevated commands
  if HandleElevatedCommands then
  Exit;

  // Handle Control Panel launch
  if HandleExeCommands then
  Exit;

  //////////////////////////////////////////////////////
  //***** Check if only Single instance is allowed *****
  h:= FindWindow('CubicExplorer_MsgInput','CE_MsgInput');
  if h <> 0 then
  begin
    if SendMessage(h, WM_USER + 1,0,0) = 0 then
    begin
      for i:= 1 to WideParamCount do
      begin
        ws:= ws + WideParamStr(i);
        if i < WideParamCount then
        ws:= ws + ',';
      end;
      if ws <> '' then
      begin
        copyDataStruct.dwData:= 0;
        copyDataStruct.cbData := (1 + Length(ws))*SizeOf(WideChar);
        copyDataStruct.lpData := PWideChar(ws);
        SendMessage(h, WM_COPYDATA, 0, Integer(@copyDataStruct));
      end
      else
      begin
        PostMessage(h, WM_MakeVisible, 0, 0);
      end;
      Exit;
    end;
  end;
  //****************************************************
  //////////////////////////////////////////////////////
  
  // Enable memory leak reporting if run on Debugger
  ReportMemoryLeaksOnShutdown:= DebugHook <> 0;

  Application.Initialize;
  Application.Title := 'CubicExplorer';
  Application.ShowMainForm:= false;
  // Create Main Form
  Application.CreateForm(TMainForm, MainForm);
  MainForm.InitializeUI;
  MainForm.BeginUIUpdate;

  // Create Folder Panel
  CEFolderPanel:= TCEFolderPanel.Create(MainForm);
  CEFolderPanel.Name:= 'FolderPanel';

  // Create Bookmark Panel
  CEBookmarkPanel:= TCEBookmarkPanel.Create(MainForm);
  CEBookmarkPanel.Name:= 'BookmarkPanel';

  // Create 'QuickView Panel
  CEQuickViewPanel:= TCEQuickViewPanel.Create(MainForm);
  CEQuickViewPanel.Name:= 'QuickViewPanel';

  // Create Filters Panel
  CEFiltersPanel:= TCEFiltersPanel.Create(MainForm);
  CEFiltersPanel.Name:= 'FiltersPanel';

  // Create DropStack Panel
  CEStackPanel:= TCEStackPanel.Create(MainForm);
  CEStackPanel.Name:= 'StackPanel';

  // Create Archiver Panel
//  CEArchiverPanel:= TCEArchiverPanel.Create(MainForm);
//  CEArchiverPanel.Name:= 'ArchiverPanel';

  // Run Start up code.
  MainForm.StartUp;
  MainForm.StartUpTimer.Enabled:= true;

  // Run Application
  Application.Run;

end.
