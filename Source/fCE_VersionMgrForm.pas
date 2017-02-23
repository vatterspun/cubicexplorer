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
//  The Original Code is fCE_VersionMgrForm.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_VersionMgrForm;

interface

uses
  // CE Units
  CE_VersionUpdater, dCE_Images,
  // fcl-xml
  XMLRead, DOM, XMLWrite,
  // Tnt
  TntForms, TntStdCtrls,
  // SpTBX
  SpTBXTabs, TB2Item, SpTBXItem, SpTBXControls, 
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, XSBuiltIns, ExtCtrls;

type
  PItemData = ^AItemData;
  AItemData = record
    Caption: WideString;
    Date: TDateTime;
    BuildType: TCEBuildType;
    BuildTypeStr: String;
    Version: TCEVersionNumber;
    Node: TDOMNode;
    IsCurrentVersion: Boolean;
    IsValid: Boolean;
    IsOffline: Boolean;
    fDownloadThread: Integer;
  end;
  
  TCEVersionMgrForm = class(TTntForm)
    ItemList: TVirtualStringTree;
    TabControl: TSpTBXTabControl;
    tab_versions: TSpTBXTabItem;
    sheet_versions: TSpTBXTabSheet;
    tab_backups: TSpTBXTabItem;
    sheet_backups: TSpTBXTabSheet;
    label_version: TSpTBXLabel;
    label_datetime: TSpTBXLabel;
    label_type: TSpTBXLabel;
    memo_notes: TTntMemo;
    but_check: TSpTBXButton;
    label_lastcheck: TSpTBXLabel;
    but_use: TSpTBXButton;
    but_close: TSpTBXButton;
    but_download: TSpTBXButton;
    panel_bottom: TSpTBXPanel;
    label_current_version: TSpTBXLabel;
    procedure but_checkClick(Sender: TObject);
    procedure but_closeClick(Sender: TObject);
    procedure but_downloadClick(Sender: TObject);
    procedure but_useClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ItemListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ItemListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex);
    procedure ItemListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ItemListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: Integer);
    procedure ItemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure ItemListPaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure TntFormClose(Sender: TObject; var Action: TCloseAction);
    procedure TntFormKeyPress(Sender: TObject; var Key: Char);
  private
    fSelectedItem: PVirtualNode;
    procedure SetSelectedItem(const Value: PVirtualNode);
    { Private declarations }
  protected
  public
    Updater: TCEVersionUpdater;
    procedure CheckOfflineStates;
    procedure HandleDownloadProgress(Sender: TObject; Percent: Integer; Current:
        Integer; FileCount: Integer);
    procedure HandleDownloadUpdateConfDone(Sender: TObject; Data: TCEDownloadData);
    procedure HandleDownloadVersionDone(Sender: TObject; Data: TCEDownloadData);
    procedure PopulateItems;
    property SelectedItem: PVirtualNode read fSelectedItem write SetSelectedItem;
    { Public declarations }
  end;

procedure ShowVersionManager;

var
  CEVersionMgrForm: TCEVersionMgrForm;
  
implementation

uses
  CE_XmlUtils, CE_VistaFuncs, httpsend, TypInfo, Math, TntSysUtils, CE_Utils,
  CE_LanguageEngine, MPCommonUtilities;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Show Version Manager
-------------------------------------------------------------------------------}
procedure ShowVersionManager;
begin
  if assigned(CEVersionMgrForm) then
  CEVersionMgrForm.BringToFront
  else
  begin
    CEVersionMgrForm:= TCEVersionMgrForm.Create(nil);
    CEVersionMgrForm.Show;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On FormCreate
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.FormCreate(Sender: TObject);
begin
  // Setup ItemList
  ItemList.NodeDataSize:= SizeOf(AItemData);
  // Setup Updater
  Updater:= TCEVersionUpdater.Create;
  Updater.UpdateConfURL:= UpdateConfURL;
  Updater.OnDownloadUpdateConfDone:= HandleDownloadUpdateConfDone;
  Updater.OnDownloadProgress:= HandleDownloadProgress;
  Updater.OnDownloadVersionDone:= HandleDownloadVersionDone;
  Updater.CurrentVersionStr:= GetAppVersionStr;
  Updater.VersionFolder:= SettingsDirPath + 'Versions\';
  Updater.OutputFolder:= exePath;
  // Setup fonts
  SetVistaFont(Font);
  label_version.Font.Style:= [fsBold];
  label_version.Font.Size:= label_version.Font.Size + 2;
  label_datetime.Font.Style:= [fsItalic];
  // Last Check
  if CELastVersionCheck = 0 then
  label_lastcheck.Caption:= ''
  else
  label_lastcheck.Caption:= _('Last check:') + ' ' + DateTimeToStr(CELastVersionCheck);

  label_current_version.Caption:= _('Current version:') + ' ' + Updater.CurrentVersionStr;
  // Load Offline UpdateConf
  if Updater.LoadUpdateConfFromFile(Updater.VersionFolder + 'updates.xml') then
  PopulateItems;

  // Translate
  CEGlobalTranslator.TranslateComponent(Self);
  if ItemList.Header.Columns.Count = 3 then
  begin
    ItemList.Header.Columns.Items[0].Text:= _('Version');
    ItemList.Header.Columns.Items[1].Text:= _('Type');
    ItemList.Header.Columns.Items[2].Text:= _('Date');
  end;
end;

{-------------------------------------------------------------------------------
  On FormDestroy
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.FormDestroy(Sender: TObject);
begin
  Updater.Free;
end;

{-------------------------------------------------------------------------------
  On but_check.Click
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.but_checkClick(Sender: TObject);
begin
  but_check.Enabled:= false;
  Updater.DownloadUpdateConf;
end;

{-------------------------------------------------------------------------------
  On but_close.Click
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.but_closeClick(Sender: TObject);
begin
  Self.Close;
end;

{-------------------------------------------------------------------------------
  On but_download.Click
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.but_downloadClick(Sender: TObject);
var
  data: PItemData;
  path: WideString;
begin
  data:= ItemList.GetNodeData(SelectedItem);
  if assigned(data) then
  begin
    if but_download.Tag = 0 then
    begin
      but_download.Enabled:= false;
      data.fDownloadThread:= Updater.DownloadVersion(data.Version);
      if data.fDownloadThread = 0 then
      but_download.Enabled:= true;
    end
    else if but_download.Tag = 1 then
    begin
      path:= WideIncludeTrailingPathDelimiter(Updater.VersionFolder) + VersionNumberToStr(data.Version);
      if WideDirectoryExists(path) then
      begin
        WideDeleteDirEx(path);
        data.IsOffline:= false;
        but_download.Tag:= 0;
        but_download.Caption:= _('Download');
        but_use.Enabled:= false;
        ItemList.Repaint;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On but_use.Click
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.but_useClick(Sender: TObject);
var
  data: PItemData;
begin
  data:= ItemList.GetNodeData(SelectedItem);
  if assigned(data) then
  begin
    if Updater.ValidateVersion(data.Node) then
    Updater.UseVersion(data.Version)
    else if TaskDialog(Self.Handle,
                       _('Invalid update!'),
                       _('One or more files are missing!'),
                       _('Do you really want to update?'),
                       TD_ICON_WARNING,
                       TD_BUTTON_YES or TD_BUTTON_NO) = mrYes then
    Updater.UseVersion(data.Version);
  end;
end;

{-------------------------------------------------------------------------------
  Check Offline States
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.CheckOfflineStates;
var
  node: PVirtualNode;
  data: PItemData;
begin
  node:= ItemList.GetFirst;
  while assigned(node) do
  begin
    data:= ItemList.GetNodeData(node);
    data.IsOffline:= Updater.VersionFolderExists(data.Version);
    node:= ItemList.GetNext(node);
  end;
  ItemList.Repaint;
end;

{-------------------------------------------------------------------------------
  HandleDownloadProgress
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.HandleDownloadProgress(Sender: TObject; Percent:
    Integer; Current: Integer; FileCount: Integer);
var
  data: PItemData;
begin
  data:= ItemList.GetNodeData(SelectedItem);
  if assigned(data) and (data.fDownloadThread = Integer(Sender)) then
  but_download.Caption:= IntToStr(Percent) + '% (' + WideFormat(_('%d of %d'), [Current, FileCount]) + ')'; 
end;

{-------------------------------------------------------------------------------
  Handle UpdateConfDownloaded
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.HandleDownloadUpdateConfDone(Sender: TObject; Data:
    TCEDownloadData);
begin
  if assigned(Data) and assigned(Data.HTTP) then
  begin
    try
      // Success
      if Data.IsValidDocument and (Data.HTTP.ResultCode = 200) then
      begin
        CELastVersionCheck:= Now;
        label_lastcheck.Caption:= _('Last check:') + ' ' + DateTimeToStr(CELastVersionCheck);
        PopulateItems;
      end
      else // Error
      begin
        // Show error message
        if Data.HTTP.ResultCode = 200 then
        begin
          label_lastcheck.Caption:= 'Error: Invalid XML document';
          ItemList.Clear;
        end
        else if Data.HTTP.ResultCode = 500 then
        label_lastcheck.Caption:= 'Error: Could not connect!'
        else if Data.HTTP.ResultString <> '' then
        label_lastcheck.Caption:= 'Error: ' + IntToStr(Data.HTTP.ResultCode) + ' - ' + Data.HTTP.ResultString
        else
        label_lastcheck.Caption:= 'Error: ' + IntToStr(Data.HTTP.ResultCode);
      end;
    finally
      but_check.Enabled:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle UpdateConfDownloaded
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.HandleDownloadVersionDone(Sender: TObject; Data:
    TCEDownloadData);
var
  node: PVirtualNode;
  selData: PItemData;
begin
  if Data.Current = Data.FileCount then
  begin
    selData:= ItemList.GetNodeData(SelectedItem);
    if assigned(selData) and (selData.fDownloadThread = Data.ThreadID) then
    begin
      selData.fDownloadThread:= 0;
      but_download.Caption:= _('Delete');
      but_download.Tag:= 1;
      but_download.Enabled:= true;
      but_use.Enabled:= not selData.IsCurrentVersion;
    end
    else
    begin
      node:= ItemList.GetFirst;
      while assigned(node) do
      begin
        selData:= ItemList.GetNodeData(node);
        if selData.fDownloadThread = Integer(Data.ThreadID) then
        selData.fDownloadThread:= 0;
        node:= ItemList.GetNext(node);
      end;
    end;

    if Data.Failed > 0 then
    begin
      try
        TaskDialog(Self.Handle, _('Download failed!'), WideFormat(_('%d file(s) failed to download!'), [IntToStr(Data.Failed)]), '', TD_ICON_WARNING, TD_BUTTON_OK);
      except
        TaskDialog(Self.Handle, _('Download failed!'), WideFormat('%d file(s) failed to download!', [IntToStr(Data.Failed)]), '', TD_ICON_WARNING, TD_BUTTON_OK);
      end;
    end;
    CheckOfflineStates;
  end;
end;

{-------------------------------------------------------------------------------
  On ItemList.CompareNodes
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListCompareNodes(Sender: TBaseVirtualTree;
    Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PItemData;
  ver1, ver2: TCEVersionNumber;
begin
  data1:= Sender.GetNodeData(Node1);
  data2:= Sender.GetNodeData(Node2);
  // Version
  if Column = 0 then
  begin
    ver1:= StrToVersionNumber(data1.Caption);
    ver2:= StrToVersionNumber(data2.Caption);
    Result:= CompareVersion(ver1, ver2);
  end
  // Type
  else if Column = 1 then
  begin
    Result:= Ord(data1.BuildType) - Ord(data2.BuildType);
  end
  // Date
  else if Column = 2 then
  begin
    if data1.Date > data2.Date then
    Result:= 1
    else if data1.Date < data2.Date then
    Result:= -1
    else
    Result:= 0;
  end;
end;

{-------------------------------------------------------------------------------
  On ItemList.FocusChanged
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListFocusChanged(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex);
begin
  SelectedItem:= Node;
end;

{-------------------------------------------------------------------------------
  On ItemList.FreeNode
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListFreeNode(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  data: PItemData;
begin
  data:= Sender.GetNodeData(Node);
  data.Caption:= '';
  data.BuildTypeStr:= '';
end;

{-------------------------------------------------------------------------------
  On ItemList.GetImageIndex
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListGetImageIndex(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: Integer);
var
  data: PItemData;
begin
  if (Column = 0) and (Kind <> ikOverlay) then
  begin
    data:= Sender.GetNodeData(Node);
    if data.IsOffline then
    ImageIndex:= 4
    else
    ImageIndex:= 5;
  end;
end;

{-------------------------------------------------------------------------------
  On ItemList.GetText
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  data: PItemData;
begin
  data:= Sender.GetNodeData(Node);
  case Column of
    0: CellText:= data.Caption;
    1: CellText:= data.BuildTypeStr;
    2: CellText:= DateToStr(data.Date);  
  end;
end;

{-------------------------------------------------------------------------------
  On ItemList.PaintText
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.ItemListPaintText(Sender: TBaseVirtualTree; const
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
    TVSTTextType);
var
  data: PItemData;
begin
  data:= Sender.GetNodeData(Node);
  if data.IsCurrentVersion then
  TargetCanvas.Font.Style:= [fsBold]
  else if (Column = 1) and (data.BuildType = btOfficial) then
  TargetCanvas.Font.Style:= [fsBold]
  else
  TargetCanvas.Font.Style:= [];
end;

{-------------------------------------------------------------------------------
  Populate Items
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.PopulateItems;
var
  rootNode, buildNode: TDOMNode;
  data: PItemData;
  node: PVirtualNode;
begin
  // Process XML Document
  try
    ItemList.BeginUpdate;
    ItemList.Clear;

    if not assigned(Updater.Xml) then
    Exit; // -->

    if assigned(Updater.Xml.DocumentElement) then
    begin
      rootNode:= FindFirstChildDOMNode(Updater.Xml.DocumentElement, 'Updates');
      if assigned(rootNode) then
      begin
        buildNode:= FindFirstChildDOMNode(rootNode, 'Build');
        while assigned(buildNode) do
        begin
          if buildNode is TDOMElement then
          begin
            node:= ItemList.AddChild(nil);
            try
              data:= ItemList.GetNodeData(node);
              data.Caption:= TDOMElement(buildNode).AttribStrings['version'];
              data.Version:= StrToVersionNumber(data.Caption);
              data.Date:= XMLTimeToDateTime(TDOMElement(buildNode).AttribStrings['date'], false);
              data.BuildType:= GetBuildType(TDOMElement(buildNode).AttribStrings['type']);
              data.BuildTypeStr:= GetBuildTypeDescription(data.BuildType);
              data.IsCurrentVersion:= Updater.CurrentVersionStr = data.Caption;
              data.IsOffline:= Updater.VersionFolderExists(data.Version);
              data.Node:= buildNode;
            except
              ItemList.DeleteNode(node);
            end;
          end;
          buildNode:= FindNextSiblingDOMNode(buildNode, 'Build');
        end;
      end;
    end;
  finally
    ItemList.SortTree(2, sdDescending);
    ItemList.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Set SelectedItem
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.SetSelectedItem(const Value: PVirtualNode);
var
  data: PItemData;
  notesNode: TDOMNode;
begin
  if fSelectedItem <> Value then
  begin
    fSelectedItem:= Value;
    if assigned(fSelectedItem) then
    begin
      data:= ItemList.GetNodeData(fSelectedItem);
      label_version.Caption:= data.Caption;
      label_datetime.Caption:= _('Published:') + ' ' + DateTimeToStr(data.Date);
      label_type.Caption:= data.BuildTypeStr;
      // Get notes
      memo_notes.Lines.Clear;
      notesNode:= FindFirstChildDOMNode(data.Node, 'Notes');
      if assigned(notesNode) then
      begin
        memo_notes.Lines.Text:= notesNode.TextContent;
      end;

      but_download.Enabled:= data.fDownloadThread = 0;
      if not data.IsOffline then
      begin
        but_download.Caption:= _('Download');
        but_download.Tag:= 0;
        but_use.Enabled:= false;
      end
      else
      begin
        but_download.Caption:= _('Delete');
        but_download.Tag:= 1;
        but_use.Enabled:= not data.IsCurrentVersion;
      end;
    end
    else
    begin
      label_version.Caption:= '';
      label_datetime.Caption:= '';
      label_type.Caption:= '';
      memo_notes.Lines.Clear;
      but_use.Enabled:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On Close
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.TntFormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  Action:= caFree;
  CEVersionMgrForm:= nil;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEVersionMgrForm.TntFormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_close.Click;
end;

end.
