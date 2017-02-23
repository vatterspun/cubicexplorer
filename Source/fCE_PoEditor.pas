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
//  The Original Code is fCE_PoEditor.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_PoEditor;

interface

uses
  // CE Units
  CE_LanguageUtils, CE_LanguageCodes, CE_ProcessUtils, fCE_PoEditor_NewForm,
  CE_LanguageEngine, dCE_Images,
  // VSTools
  MPCommonUtilities,
  // VT
  VirtualTrees,
  // TNT
  TntSysUtils, TntComCtrls, TntClasses, TntActnList, TntStdCtrls, TntButtons,
  TntForms,
  // SpTBX, TBX, TB2000
  SpTBXTabs, SpTBXControls, SpTBXDkPanels,
  SpTBXItem, TB2Item, TB2ExtItems, SpTBXEditors, TB2Dock, TB2Toolbar,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, ActnList, fCE_TabPage;

type
  PPOListItem = ^APOListItem;
  APOListItem = record
    PoItem: TCEPOItem;
  end;

  TCEPoEditor = class(TFrame)
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    ActionList: TTntActionList;
    act_save: TTntAction;
    act_new: TTntAction;
    act_apply: TTntAction;
    TabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    WordsSheet: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    SettingsSheet: TSpTBXTabSheet;
    Translation_panel: TPanel;
    horz_splitter: TSpTBXSplitter;
    vert_splitter: TSpTBXSplitter;
    PoItemStrID_panel: TSpTBXPanel;
    PoItemStrMsg_panel: TSpTBXPanel;
    PoList_panel: TSpTBXPanel;
    PoList: TVirtualStringTree;
    PoItemStrID: TTntMemo;
    PoItemStrMsg: TTntMemo;
    SpTBXPanel1: TSpTBXPanel;
    SpTBXButton1: TSpTBXButton;
    SpTBXItem4: TSpTBXItem;
    TntLabel5: TTntLabel;
    TntLabel6: TTntLabel;
    LanguagesCombo: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
    TntLabel4: TTntLabel;
    label1: TTntLabel;
    label2: TTntLabel;
    LanguageList: TComboBox;
    label_pot_rev: TTntLabel;
    label_translation_rev: TTntLabel;
    edit_translators_name: TTntEdit;
    edit_translators_email: TTntEdit;
    edit_translation_version: TTntEdit;
    panel_startup: TSpTBXPanel;
    label_startup: TSpTBXLabel;
    act_saveas: TTntAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    procedure act_applyExecute(Sender: TObject);
    procedure act_newExecute(Sender: TObject);
    procedure act_saveasExecute(Sender: TObject);
    procedure act_saveExecute(Sender: TObject);
    procedure act_Update(Sender: TObject);
    procedure edit_translation_versionChange(Sender: TObject);
    procedure edit_translators_Change(Sender: TObject);
    procedure LanguageListChange(Sender: TObject);
    procedure LanguagesComboItemClick(Sender: TObject);
    procedure PoItemStrMsgChange(Sender: TObject);
    procedure PoItemStrMsgKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure PoItemStrMsg_panelResize(Sender: TObject);
    procedure PoListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PoListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure PoListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex);
    procedure PoListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure PoListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure PoListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var
        Shift: TShiftState; var DoDefault: Boolean);
    procedure PoListMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure PoListMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure PoListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; NewText: WideString);
    procedure SpTBXItem4Click(Sender: TObject);
  private
    fActiveLanguage: WideString;
    fCurrentShift: TShiftState;
    fPOT_Rev: Integer;
    fDomainName: String;
    fIsApplyed: Boolean;
    fLocaleDir: WideString;
    fOnTranslateUI: TNotifyEvent;
    fTranslationRev: Integer;
    poFileList: TTntStrings;
    procedure SetActiveLanguage(const Value: WideString);
    procedure SetDomainName(const Value: String);
    procedure SetIsChanged(const Value: Boolean);
    procedure SetLocaleDir(const Value: WideString);
    procedure SetPOT_Rev(const Value: Integer);
    { Private declarations }
  protected
    fActiveFile: WideString;
    fIsChanged: Boolean;
    HeaderInfo: TTntStrings;
    procedure AssignListFromPoFile;
  public
    POFile: TCEPOFile;
    POTFile: TCEPOFile;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetTranslations;
    procedure LoadFromFile(AFilePath: WideString);
    procedure MergePOT(APOTFile: TCEPOFile; DeleteNonExisting: Boolean = false);
    procedure SaveToFile(AFilePath: WideString);
    property ActiveLanguage: WideString read fActiveLanguage write
        SetActiveLanguage;
    property POT_Rev: Integer read fPOT_Rev write SetPOT_Rev;
    property IsChanged: Boolean read fIsChanged write SetIsChanged;
    property DomainName: String read fDomainName write SetDomainName;
    property IsApplyed: Boolean read fIsApplyed write fIsApplyed;
    property LocaleDir: WideString read fLocaleDir write SetLocaleDir;
    property TranslationRev: Integer read fTranslationRev write fTranslationRev;
    { Public declarations }
  published
    property OnTranslateUI: TNotifyEvent read fOnTranslateUI write fOnTranslateUI;
  end;

  TCEPoEditorForm = class(TTntForm)
  protected
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HandleKeyPress(Sender: TObject; var Key: Char);
  public
    PoEditor: TCEPoEditor;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  end;

implementation

uses
  TntDialogs;

{$R *.dfm}

procedure TrimList(AList: TTntStrings);
var
  i: Integer;
  nameW,valueW: WideString;
begin
  for i:= 0 to AList.Count - 1 do
  begin
    nameW:= AList.Names[i];
    valueW:= TrimLeft(AList.Values[nameW]);
    if (valueW <> '') then
    AList.Values[nameW]:= valueW;
  end;
end;

procedure AddSpaceToList(AList: TTntStrings);
var
  i: Integer;
begin
  for i:= 0 to AList.Count - 1 do
  begin
    AList.ValueFromIndex[i]:= ' ' + AList.ValueFromIndex[i];
  end;
end;

{*------------------------------------------------------------------------------
  Create an instance of TCEPoEditor
-------------------------------------------------------------------------------}
constructor TCEPoEditor.Create(AOwner: TComponent);
begin
  inherited;
  POList.NodeDataSize:= SizeOf(APOListItem);
  POFile:= TCEPOFile.Create;
  POTFile:= TCEPOFile.Create;
  HeaderInfo:= TTntStringList.Create;
  HeaderInfo.NameValueSeparator:= ':';
  // Init stuff
  GetLanguageNames(LanguageList.Items);
  LanguageList.Sorted:= true;
  fDomainName:= 'default';
  fActiveLanguage:= '';
  poFileList:= TTntStringList.Create;
  poFileList.NameValueSeparator:= '=';
  IsChanged:= false;
  IsApplyed:= true;
  TranslationRev:= 0;
  POT_Rev:= 0;
  CEGlobalTranslator.TranslateComponent(Self);
  POList.Header.Columns[0].Text:= _('Original');
  POList.Header.Columns[1].Text:= _('Translation');

  // Testing stuff!!!
  if DebugHook <> 0 then
  begin
    SpTBXItem4.Visible:= true;
  end;
end;

{*------------------------------------------------------------------------------
  Destroy TCEPoEditor
-------------------------------------------------------------------------------}
destructor TCEPoEditor.Destroy;
begin
  PoList.Clear;
  HeaderInfo.Free;
  poFileList.Free;
  POFile.Free;
  POTFile.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Apply translation
-------------------------------------------------------------------------------}
procedure TCEPoEditor.act_applyExecute(Sender: TObject);
begin
  if fActiveFile <> '' then
  begin
    try
      if IsChanged then
      SaveToFile(fActiveFile);
    finally
      if assigned(fOnTranslateUI) then fOnTranslateUI(Self);
      IsApplyed:= true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Create new translation
-------------------------------------------------------------------------------}
procedure TCEPoEditor.act_newExecute(Sender: TObject);
var
  lang: WideString;
  path: WideString;
  list: TStrings;
  s: TStream;
  ws: WideString;
  i: Integer;
begin
  if ShowNewTranslationDlg(fLocaleDir, lang, path) then
  begin
    list:= TStringList.Create;
    try
      if not WideDirectoryExists(path) then
      WideForceDirectories(path);
      // Default data
      list.Add('# Translation for CubicExplorer');
      list.Add('#');
      list.Add('msgid ""');
      list.Add('msgstr ""');
      list.Add('"Project-Id-Version: 1.0\n"');
      list.Add('"MIME-Version: 1.0\n"');
      list.Add('"Content-Type: text/plain; charset=UTF-8\n"');
      list.Add('"Content-Transfer-Encoding: 8bit\n"');
      list.Add('"CE-POT-Version: 0\n"');
      list.Add('"CE-Language: ' + lang + '\n"');
      list.Add('');
      // Save PO
      ws:= WideIncludeTrailingBackslash(path) + DomainName + '.po';
      s:= TTntFileStream.Create(ws, fmCreate);
      try
        s.Position:= 0;
        list.SaveToStream(s);
      finally
        s.Free;
      end;
      // Load to UI
      GetTranslations;
      for i:= 0 to poFileList.Count-1 do
      begin
        if CompareText(poFileList.ValueFromIndex[i], ws) = 0 then
        begin
          LanguagesCombo.ItemIndex:= i;
          TabControl.ActiveTabIndex:= 0;
          LoadFromFile(ws);
          break;
        end;
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On act_saveas.Execute
-------------------------------------------------------------------------------}
procedure TCEPoEditor.act_saveasExecute(Sender: TObject);
var
  dlg: TTntSaveDialog;
begin
  dlg:= TTntSaveDialog.Create(nil);
  try
    dlg.FileName:= fActiveFile;
    if dlg.Execute then
    begin
      SaveToFile(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Save translatio
-------------------------------------------------------------------------------}
procedure TCEPoEditor.act_saveExecute(Sender: TObject);
begin
  if fActiveFile <> '' then
  begin
    SaveToFile(fActiveFile);
  end;
end;

{*------------------------------------------------------------------------------
  On action update
-------------------------------------------------------------------------------}
procedure TCEPoEditor.act_Update(Sender: TObject);
begin
  if Sender = act_save then
  act_save.Enabled:= IsChanged
  else if Sender = act_saveas then
  act_saveas.Enabled:= fActiveLanguage <> ''
  else if Sender = act_apply then
  act_apply.Enabled:= not IsApplyed;
end;

{*------------------------------------------------------------------------------
  Assign List From PoFile
-------------------------------------------------------------------------------}
procedure TCEPoEditor.AssignListFromPoFile;
var
  data: PPOListItem;
  node: PVirtualNode;
  i: Integer;
begin
  PoList.Clear;
  PoList.BeginUpdate;
  try
    for i:= 0 to POFile.Count - 1 do
    begin
      node:= PoList.AddChild(nil);
      data:= PoList.GetNodeData(node);
      data.PoItem:= POFile.Items[i];
    end;
  finally
    PoList.SortTree(PoList.Header.SortColumn, PoList.Header.SortDirection, false);
    PoList.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Version info change
-------------------------------------------------------------------------------}
procedure TCEPoEditor.edit_translation_versionChange(Sender: TObject);
begin
  HeaderInfo.Values['Project-Id-Version']:= edit_translation_version.Text;
  IsChanged:= true;
end;

{*------------------------------------------------------------------------------
  Translator info change
-------------------------------------------------------------------------------}
procedure TCEPoEditor.edit_translators_Change(Sender: TObject);
begin
  if edit_translators_email.Text <> '' then
  HeaderInfo.Values['Last-Translator']:= edit_translators_name.Text + ' <' + edit_translators_email.Text + '>'
  else
  HeaderInfo.Values['Last-Translator']:= edit_translators_name.Text;
  IsChanged:= true;
end;

{*------------------------------------------------------------------------------
  Get Translations
    -Enum Locale directory and add translations to LanguagesCombo.
-------------------------------------------------------------------------------}
procedure TCEPoEditor.GetTranslations;
var
  i: Integer;
begin
  LanguagesCombo.Items.Clear;
  poFileList.Clear;
  PoList.Clear;
  PoItemStrID.Clear;
  PoItemStrMsg.Clear;
  fActiveFile:= '';
  fActiveLanguage:= '';
  IsChanged:= false;
  GetPOLanguageList(fLocaleDir, fDomainName, poFileList);
  for i:= 0 to poFileList.Count - 1 do
  LanguagesCombo.Items.Add(poFileList.Names[i]);
end;

{*------------------------------------------------------------------------------
  On LanguageList Change
-------------------------------------------------------------------------------}
procedure TCEPoEditor.LanguageListChange(Sender: TObject);
begin
  HeaderInfo.Values['CE-Language']:= LanguageList.Text;
  IsChanged:= true;
end;

{*------------------------------------------------------------------------------
  On LanguagesCombo ItemClick
-------------------------------------------------------------------------------}
procedure TCEPoEditor.LanguagesComboItemClick(Sender: TObject);
begin
  if LanguagesCombo.ItemIndex > -1 then
  begin
    LoadFromFile(poFileList.ValueFromIndex[LanguagesCombo.ItemIndex]);
    fActiveLanguage:= poFileList.Names[LanguagesCombo.ItemIndex];
    TabControl.Visible:= true;
    panel_startup.Visible:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Load PO file
-------------------------------------------------------------------------------}
procedure TCEPoEditor.LoadFromFile(AFilePath: WideString);
var
  ws: WideString;
  i,i2: Integer;
begin
  PoList.Clear;
  if not WideFileExists(AFilePath) then
  Exit;
  fActiveFile:= AFilePath;
  POFile.LoadFromFile(AFilePath);
  // Header info
  HeaderInfo.Clear;
  HeaderInfo.Text:= UTF8Decode(UnescapeString(POFile.Header.MsgStr));
  TrimList(HeaderInfo);
  edit_translators_name.OnChange:= nil;
  edit_translators_email.OnChange:= nil;
  edit_translation_version.OnChange:= nil;
  LanguageList.OnChange:= nil;
  try
    // Translator's name and email
    ws:= HeaderInfo.Values['Last-Translator'];
    i:= Pos('<', ws);
    if i = 0 then
    begin
      edit_translators_name.Text:= ws;
      edit_translators_email.Text:= '';
    end
    else
    begin
      if ws[i-1] = ' ' then
      edit_translators_name.Text:= Copy(ws, 1, i-2)
      else
      edit_translators_name.Text:= Copy(ws, 1, i-1);

      i:= i+1;
      i2:= Pos('>',ws);
      if i2 > i then
      edit_translators_email.Text:= Copy(ws, i, i2-i)
      else
      edit_translators_email.Text:= '';
    end;
    // Version
    edit_translation_version.Text:= HeaderInfo.Values['Project-Id-Version'];
    // Language
    LanguageList.ItemIndex:= LanguageList.Items.IndexOf(HeaderInfo.Values['CE-Language']);
    // CE Translation version
    TranslationRev:= StrToIntDef(HeaderInfo.Values['CE-POT-Version'], 0);

    //if TranslationRev < POT_Rev then
    //begin
      MergePOT(POTFile);
      HeaderInfo.Values['CE-POT-Version']:= IntToStr(POT_Rev);
    //end;
    
    label_translation_rev.Caption:= IntToStr(TranslationRev);
  finally
    POFile.Sort;
    POFile.Pack;
    AssignListFromPoFile;
    edit_translators_name.OnChange:= edit_translators_Change;
    edit_translators_email.OnChange:= edit_translators_Change;
    edit_translation_version.OnChange:= edit_translation_versionChange;
    LanguageList.OnChange:= LanguageListChange;
    TabControl.Visible:= true;
    panel_startup.Visible:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Merge POT 
-------------------------------------------------------------------------------}
procedure TCEPoEditor.MergePOT(APOTFile: TCEPOFile; DeleteNonExisting: Boolean
    = false);
var
  i: Integer;
begin
  if assigned(APOTFile) then
  begin
    for i:= 0 to APOTFile.Count - 1 do
    begin
      if POFile.IndexOf(APOTFile[i].MsgId) < 0 then
        if APOTFile[i].MsgId <> '' then
        POFile.Add.Assign(APOTFile[i]);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save PO file
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SaveToFile(AFilePath: WideString);
var
  list: TTntStrings;
begin
  list:= TTntStringList.Create;
  try
    list.Assign(HeaderInfo);
    AddSpaceToList(list);
    POFile.Header.MsgStr:= EscapeString(UTF8Encode(list.Text));
    POFile.SaveToFile(AFilePath, false);
    IsChanged:= false;
  finally
    list.Free;
  end;
end;

{*------------------------------------------------------------------------------
  On PoItemStrMsg Change
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoItemStrMsgChange(Sender: TObject);
var
  data: PPOListItem;
begin
  if PoList.FocusedNode <> nil then
  begin
    data:= PoList.GetNodeData(PoList.FocusedNode);
    data.PoItem.MsgStr:= UTF8Encode(PoItemStrMsg.Lines.Text);
    PoList.RepaintNode(PoList.FocusedNode);
    IsChanged:= true;
  end;
end;

{*------------------------------------------------------------------------------
  On PoItemStrMsg KeyDown
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoItemStrMsgKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    if PoList.FocusedNode <> nil then
    begin
      if assigned(PoList.FocusedNode.NextSibling) then
      begin
        PoList.Selected[PoList.FocusedNode.NextSibling]:= true;
        PoList.FocusedNode:= PoList.FocusedNode.NextSibling;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  PoItemStrMsg_panel Resize
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoItemStrMsg_panelResize(Sender: TObject);
var
  r: TRect;
begin
  r:= TWinControl(Sender).ClientRect;
  r.Left:= r.Left + 5;
  r.Top:= r.Top + 24;
  r.Bottom:= r.Bottom - 5;
  r.Right:= r.Right - 5;
  if Sender = PoItemStrMsg_panel then
  PoItemStrMsg.BoundsRect:= r
  else
  PoItemStrId.BoundsRect:= r;
end;

{*------------------------------------------------------------------------------
  On PoList Change
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  data: PPOListItem;
begin
  PoItemStrMsg.OnChange:= nil;
  try
    if Node = nil then
    begin
      PoItemStrID.Lines.Text:= '';
      PoItemStrMsg.Lines.Text:= '';
    end
    else
    begin
      data:= PoList.GetNodeData(Node);
      PoItemStrID.Lines.Text:= UTF8Decode(data.PoItem.MsgID);
      PoItemStrMsg.Lines.Text:= UTF8Decode(data.PoItem.MsgStr);
      PoItemStrMsg.SelStart:= Length(PoItemStrMsg.Lines.Text);
    end;
  finally
    PoItemStrMsg.OnChange:= PoItemStrMsgChange;
  end;
end;

{*------------------------------------------------------------------------------
  Compare Nodes
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListCompareNodes(Sender: TBaseVirtualTree; Node1,
    Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PPOListItem;
begin
  data1:= PoList.GetNodeData(Node1);
  data2:= PoList.GetNodeData(Node2);
  if Column = 0 then
  Result:= WideCompareText(data1.PoItem.MsgID, data2.PoItem.MsgID)
  else
  Result:= WideCompareText(data1.PoItem.MsgStr, data2.PoItem.MsgStr);
end;

{*------------------------------------------------------------------------------
  On PoList Edited
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListEdited(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex);
begin
  if not PoList.Selected[Node] then
  PoList.FocusedNode:= PoList.GetFirstSelected;
end;

{*------------------------------------------------------------------------------
  On PoList GetText
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  data: PPOListItem;
begin
  data:= PoList.GetNodeData(Node);
  if Column = 0 then
  begin
    CellText:=  UTF8Decode(data.PoItem.MsgID);
  end
  else if Column = 1 then
  begin
    CellText:=  UTF8Decode(data.PoItem.MsgStr);
  end;
end;

{*------------------------------------------------------------------------------
  On PoList HeaderClick
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    if Sender.SortColumn > NoColumn then
    Sender.Columns[Sender.SortColumn].Options:= Sender.Columns[Sender.SortColumn].Options + [coParentColor];

    if (Sender.SortColumn = NoColumn) or (Sender.SortColumn <> HitInfo.Column) then
    begin
      Sender.SortColumn:= HitInfo.Column;
      Sender.SortDirection:= sdAscending;
    end
    else
    begin
      if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
      else
      Sender.SortDirection := sdAscending;
    end;
    Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection, False);
  end;
end;

{*------------------------------------------------------------------------------
  On PoList KeyAction
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListKeyAction(Sender: TBaseVirtualTree; var CharCode:
    Word; var Shift: TShiftState; var DoDefault: Boolean);
var
  data: PPOListItem;
  i: Integer;
  node: PVirtualNode;
begin
  if (CharCode = VK_DELETE) and (Shift = []) and (not Sender.IsEditing) then
  begin
    data:= PoList.GetNodeData(Sender.FocusedNode);
    if assigned(data) then
    begin
      for i:= 0 to POFile.Count - 1 do
      begin
        if POFile.Items[i] = data.PoItem then
        begin
          POFile.Delete(i);
          node:= Sender.FocusedNode.NextSibling;
          Sender.DeleteNode(Sender.FocusedNode);
          Sender.Selected[node]:= true;
          Sender.FocusedNode:= node;
          IsChanged:= true;
          break;
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  On PoList MouseDown
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  fCurrentShift:= Shift;
end;

{*------------------------------------------------------------------------------
  On PoList MouseUp
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  info: THitInfo;
begin
  if fCurrentShift = [ssLeft, ssDouble] then
  begin
    PoList.GetHitTestInfoAt(X,Y,true,info);
    if assigned(info.HitNode) then
    PoList.EditNode(info.HitNode, 1);
  end;
  fCurrentShift:= [];
end;

{*------------------------------------------------------------------------------
  On PoList NewText
-------------------------------------------------------------------------------}
procedure TCEPoEditor.PoListNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  data: PPOListItem;
begin
  data:= PoList.GetNodeData(Node);
  if Column = 1 then
  begin
    data.PoItem.MsgStr:= UTF8Encode(NewText);
    IsChanged:= true;
  end;
  if assigned(Node.NextSibling) then
  begin
    PoList.Selected[Node.NextSibling]:= true;
  end;
end;

{*------------------------------------------------------------------------------
  Set Active Language
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SetActiveLanguage(const Value: WideString);
begin
  fActiveLanguage:= Value;
  LoadFromFile(poFileList.Values[Value]);
  LanguagesCombo.ItemIndex:= LanguagesCombo.Items.IndexOf(Value);
end;

{*------------------------------------------------------------------------------
  SetDomainName
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SetDomainName(const Value: String);
begin
  fDomainName:= Value;
  GetTranslations;
end;

{*------------------------------------------------------------------------------
  Set IsChanged
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SetIsChanged(const Value: Boolean);
begin
  if (fIsChanged <> Value) and (fActiveFile <> '') then
  begin
    fIsChanged:= Value;
    IsApplyed:= false;
  end;
end;

{*------------------------------------------------------------------------------
  SetLocaleDir
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SetLocaleDir(const Value: WideString);
begin
  fLocaleDir:= Value;
  GetTranslations;
end;

{*------------------------------------------------------------------------------
  Set POT_Rev
-------------------------------------------------------------------------------}
procedure TCEPoEditor.SetPOT_Rev(const Value: Integer);
begin
  fPOT_Rev:= Value;
  label_pot_rev.Caption:= IntToStr(POT_Rev);
end;

procedure TCEPoEditor.SpTBXItem4Click(Sender: TObject);
var
  i: Integer;
begin
  MergePOT(POTFile);
  for i:= 0 to PoFile.Count - 1 do
  begin
    PoFile.Items[i].MsgStr:= '!!!Test!!!';
    IsChanged:= true;
  end;
  AssignListFromPoFile;
end;

{##############################################################################}

constructor TCEPoEditorForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  Self.OnClose:= FormClose;
  Self.OnCloseQuery:= FormCloseQuery;
  Self.OnKeyPress:= HandleKeyPress;
  Self.KeyPreview:= true;
  Self.Caption:= _('Language Editor');
  Self.Position:= poMainFormCenter;
  Self.Width:= 420;
  Self.Height:= 500;
  Self.FormStyle:= fsStayOnTop;
  PoEditor:= TCEPoEditor.Create(self);
  PoEditor.Parent:= Self;
  PoEditor.Align:= alClient;
  PoEditor.TabControl.ActiveTabIndex:= 0;
end;

procedure TCEPoEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TCEPoEditorForm.FormCloseQuery(Sender: TObject; var CanClose:
    Boolean);
var
  res: Integer;
  ws,ws2: WideString;
begin
  CanClose:= true;
  if PoEditor.IsChanged then
  begin
    ws2:= _('Save changes before closing?');
    ws:= _('Save changes?');
    res:= WideMessageBox(Self.Handle, ws, ws2, MB_ICONQUESTION or MB_YESNOCANCEL);
    if res = idYes then
    PoEditor.act_save.Execute
    else if res = idCancel then
    CanClose:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEPoEditorForm.HandleKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  Self.Close;
end;

end.
