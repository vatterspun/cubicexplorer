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
//  The Original Code is fCE_TextEditor.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_TextEditor;

interface

uses
  // CE Units
  CE_GlobalCtrl, fCE_TabPage, CE_Utils, CE_VistaFuncs, CE_LanguageEngine,
  CE_AppSettings,
  // Toolbar2000
  TB2Dock, TB2Toolbar, TB2Item,
  // SpTBXLib
  SpTBXControls, SpTBXDkPanels, SpTBXItem, SpTBXSkins,
  // Syn Edit
  SynEdit, SynEditOptionsDialog, SynURIOpener, SynEditRegexSearch,
  SynEditMiscClasses, SynEditSearch, SynEditTypes,
  // Syn Highlighters
  SynHighlighterURI, SynHighlighterDfm, SynHighlighterPerl, SynHighlighterJava,
  SynHighlighterPas, SynHighlighterIni, SynHighlighterBat, SynHighlighterXML,
  SynHighlighterJScript, SynHighlighterPHP, SynHighlighterHtml,
  SynHighlighterCSS, SynEditHighlighter, SynHighlighterCpp,
  // VSTools
  MPCommonObjects, MPCommonUtilities,
  // TNT Controls
  TntActnList, TntStdCtrls, TntSysUtils, TntDialogs, TntClasses,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, ActnList,  StdCtrls, fCE_FileView;

type
  TCEActiveFileChangeEvent = procedure(Sender: TObject; FilePath: WideString) of object;

  TCETextEditorPage = class(TCECustomTabPage)
    TopDock: TSpTBXDock;
    MainToolbar: TSpTBXToolbar;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem15: TSpTBXItem;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    highlighterSubmenu: TSpTBXSubmenuItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    Editor: TSynEdit;
    StatusBar: TSpTBXStatusBar;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    label_input: TSpTBXLabelItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    label_modified: TSpTBXLabelItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    label_path: TSpTBXLabelItem;
    FindPanel: TSpTBXPanel;
    SpTBXLabel1: TSpTBXLabel;
    SearchMemo: TTntMemo;
    SpTBXLabel2: TSpTBXLabel;
    ReplaceMemo: TTntMemo;
    SpTBXGroupBox1: TSpTBXGroupBox;
    opt_check1: TSpTBXCheckBox;
    opt_check2: TSpTBXCheckBox;
    opt_check3: TSpTBXCheckBox;
    opt_check4: TSpTBXCheckBox;
    opt_check5: TSpTBXCheckBox;
    SpTBXButton1: TSpTBXButton;
    SpTBXButton2: TSpTBXButton;
    SpTBXButton3: TSpTBXButton;
    opt_radio: TSpTBXRadioGroup;
    ActionList: TTntActionList;
    text_file_new: TTntAction;
    text_edit_undo: TTntAction;
    text_file_open: TTntAction;
    text_file_save: TTntAction;
    text_file_saveas: TTntAction;
    text_file_close: TTntAction;
    text_edit_redo: TTntAction;
    text_edit_copy: TTntAction;
    text_edit_cut: TTntAction;
    text_edit_paste: TTntAction;
    text_edit_delete: TTntAction;
    text_edit_selall: TTntAction;
    text_edit_search: TTntAction;
    text_edit_findnext: TTntAction;
    text_edit_findprev: TTntAction;
    text_format_wrap: TTntAction;
    text_format_options: TTntAction;
    text_view_toolbar: TTntAction;
    text_file_reload: TTntAction;
    text_view_statusbar: TTntAction;
    SynEditSearch: TSynEditSearch;
    SynEditRegexSearch: TSynEditRegexSearch;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynXMLSyn1: TSynXMLSyn;
    SynBatSyn1: TSynBatSyn;
    SynIniSyn1: TSynIniSyn;
    SynPasSyn1: TSynPasSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynDfmSyn1: TSynDfmSyn;
    SynURISyn1: TSynURISyn;
    SynURIOpener1: TSynURIOpener;
    procedure EditorReplaceText(Sender: TObject; const ASearch, AReplace:
        WideString; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SpTBXButton1Click(Sender: TObject);
    procedure SpTBXButton3Click(Sender: TObject);
    procedure text_edit_Execute(Sender: TObject);
    procedure text_edit_Update(Sender: TObject);
    procedure text_file_Execute(Sender: TObject);
    procedure text_file_Update(Sender: TObject);
    procedure text_format_Execute(Sender: TObject);
    procedure text_format_Update(Sender: TObject);
    procedure text_view_Execute(Sender: TObject);
    procedure text_view_Update(Sender: TObject);
    procedure SpTBXButton2Click(Sender: TObject);
  private
    fActiveFile: WideString;
    fClosing: Boolean;
    fOnActiveFileChange: TCEActiveFileChangeEvent;
    fOnModifiedChange: TNotifyEvent;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure SetActiveFile(const Value: WideString);
    { Private declarations }
  protected
    fReplaceAll: Boolean;
    procedure ActiveFileChange;
    procedure DoHighlighterClick(Sender: TObject);
    function GetPageActionList: TActionList; override;
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure ModifiedChange;
  public
    Highlighters: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CloseDocument: Boolean;
    procedure LoadFromStream(AStream: TStream); override;
    procedure NewDocument;
    procedure OpenDocument(FilePath: WideString = '');
    procedure PopuplateHighlighters;
    procedure ReloadDocument;
    function SaveDocument: Boolean;
    function SaveDocumentAs: Boolean;
    procedure SaveToStream(AStream: TStream); override;
    procedure SelectPage; override;
    procedure SetAutoHighlighter;
    procedure ShowOptions;
    function TabClosing: Boolean; override;
    procedure UpdateCaption; override;
    property ActiveFile: WideString read fActiveFile write SetActiveFile;
    { Public declarations }
  published
    property OnActiveFileChange: TCEActiveFileChangeEvent read fOnActiveFileChange
        write fOnActiveFileChange;
    property OnModifiedChange: TNotifyEvent read fOnModifiedChange write
        fOnModifiedChange;
  end;

  TCETextEditorOptions = class(TPersistent)
  private
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fWordWrap: Boolean;
  public
    EditorOptions: TSynEditorOptionsContainer;
    constructor Create;
    destructor Destroy; override;
    procedure AssignSettingsTo(EditPage: TCETextEditorPage);
  published
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout
        write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout
        write fRememberOuterToolbarLayout;
    property WordWrap: Boolean read fWordWrap write fWordWrap;
  end;


  TCETextEditorPageSettings = class(TCECustomTabPageSettings)
  private
    function GetPath: WideString;
    procedure SetPath(const Value: WideString);
  protected
    function GetRememberPanelLayout: Boolean; override;
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
  public
    TextEditorPage: TCETextEditorPage;
  published
    property Path: WideString read GetPath write SetPath;
  end;


function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;

var
  CETextEditorPage: TCETextEditorPage;
  CETextEditorOptions: TCETextEditorOptions;

implementation

uses
  dCE_Images, dCE_Actions;
  
{$R *.dfm}

{*------------------------------------------------------------------------------
  Get Highlighter From File Extension
-------------------------------------------------------------------------------}
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: integer;
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then begin
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        j := Pos(Extension, Filter);
        if (j > 0) and
           ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
        then begin
          Result := Highlighter;
          exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

{##############################################################################}

{=== File Action IDs ===}
// 101 = New
// 102 = Open
// 103 = Save
// 104 = Save As
// 105 = Close
// 106 = Reload
{=== Edit Action IDs ===}
// 201 = Undo
// 202 = Redo
// 203 = Copy
// 204 = Cut
// 205 = Paste
// 206 = Delete
// 207 = Select All
// 208 = Search and Replace
// 209 = Find Next
// 210 = Find Previous
{=== Format Action IDs ===}
// 301 = Word Wrap
// 302 = Editor Options
{=== View Action IDs ===}
// 401 = Show Toolbar
// 402 = Show statusbar

{*------------------------------------------------------------------------------
  Get's called when TCETextEditorPage is created.
-------------------------------------------------------------------------------}
constructor TCETextEditorPage.Create(AOwner: TComponent);
begin
  inherited;
  TCETextEditorPageSettings(Settings).TextEditorPage:= Self;
  fClosing:= false;
  Layout:= 'TextEditor';
  Highlighters:= TStringList.Create;
  Highlighters.Sorted:= true;

  PopuplateHighlighters;
  GlobalFocusCtrl.CtrlList.Add(Editor);
  Editor.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  self.Images:= CE_Images.SmallIcons;
  self.ImageIndex:= 21;
  CETextEditorOptions.AssignSettingsTo(self);

  CEGlobalTranslator.TranslateComponent(Self);
  opt_radio.Items.Strings[0]:= _('Up');
  opt_radio.Items.Strings[1]:= _('Down');
end;

{*------------------------------------------------------------------------------
  Get's called when TCETextEditorPage is destoyed.
-------------------------------------------------------------------------------}
destructor TCETextEditorPage.Destroy;
begin
  if CEActions.PageActionList = Self.ActionList then
  CEActions.PageActionList:= nil;
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.ActivePage:= nil;
  CloseDocument;
  Highlighters.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Close Document
-------------------------------------------------------------------------------}
function TCETextEditorPage.CloseDocument: Boolean;
var
  s,s2: String;
  r: Integer;
  i: Integer;
begin
  Result:= false;
  if Editor.Modified then
  begin
    if not Visible then
    Show;

    s:= 'The text in the ' + TabCaption + ' file has changed.' ;
    s2:= 'Do you want to save changes before closing?';
    r:= TaskDialog(Application.MainFormHandle,
                   'Save before closing?',
                   s,
                   s2,
                   TD_ICON_WARNING,
                   TD_BUTTON_YES + TD_BUTTON_NO + TD_BUTTON_CANCEL);

    if r = TD_RESULT_YES then
    begin
      if not SaveDocument then
      Exit;
    end
    else if r = TD_RESULT_CANCEL then
    begin
      Exit;
    end
    else
    Editor.Clear;
  end;

  if (csDestroying in self.ComponentState) then
  Exit;

  if fClosing then
  begin
    Result:= true;
    exit;
  end;

  ActiveFile:= '';
  Result:= true;
  for i:= 0 to highlighterSubmenu.Count - 1 do
  begin
    if highlighterSubmenu.Items[i] is TSpTBXItem then
    TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
  end;
end;

{*------------------------------------------------------------------------------
  New Document
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.NewDocument;
begin
  if CloseDocument then
  begin
    ActiveFile:= '';
  end;
end;

{*------------------------------------------------------------------------------
  Open Document.
    -If FilePath is empty then will show a open file dialog.
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.OpenDocument(FilePath: WideString = '');
var
  open1: TTntOpenDialog;
  S: TTntFileStream;
begin
  if FilePath = '' then
  begin
    open1:= TTntOpenDialog.Create(self);
    if open1.Execute then
    begin
      if CloseDocument then
      begin
        try
          s:= TTntFileStream.Create(open1.FileName, fmOpenRead or fmShareDenyNone);
          try
            Editor.Lines.LoadFromStream(s);
            ActiveFile:= open1.FileName;
          finally
            s.Free;
          end;
        except
          on E:EFOpenError do
          WideMessageBox(Application.MainFormHandle, _('Error'), E.Message, MB_ICONERROR or MB_OK);
        end;
      end;
    end;
    open1.Free;
  end
  else if WideFileExists(FilePath) then
  begin
    if CloseDocument then
    begin
      try
        s:= TTntFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
        try
          Editor.Lines.LoadFromStream(s);
          ActiveFile:= FilePath;
        finally
          s.Free;
        end;
      except
        on E:EFOpenError do
        WideMessageBox(Application.MainFormHandle, _('Error'), E.Message, MB_ICONERROR or MB_OK);
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Document
-------------------------------------------------------------------------------}
function TCETextEditorPage.SaveDocument: Boolean;
var
  S: TTntFileStream;
begin
  if WideFileExists(ActiveFile) then
  begin
    s:= TTntFileStream.Create(ActiveFile, fmCreate);
    try
      Editor.Lines.SaveToStream(s);
    finally
      s.Free;
    end;
    Editor.Modified:= false;
    Result:= true;
  end
  else
  begin
    Result:= SaveDocumentAs;
  end;
end;

{*------------------------------------------------------------------------------
  Save Document As
-------------------------------------------------------------------------------}
function TCETextEditorPage.SaveDocumentAs: Boolean;
var
  save1: TTntSaveDialog;
  s: TTntFileStream;
begin
  Result:= false;
  save1:= TTntSaveDialog.Create(self);
  save1.DefaultExt:= 'txt';
  if save1.Execute then
  begin
    s:= TTntFileStream.Create(save1.FileName, fmCreate);
    try
      Editor.Lines.SaveToStream(s);
      ActiveFile:= save1.FileName;
    finally
      s.Free;
    end;
    Result:= true;
    Editor.Modified:= false;
  end;
  save1.Free;
end;

{*------------------------------------------------------------------------------
  Reload Document
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.ReloadDocument;
begin
  OpenDocument(ActiveFile);
end;

{*------------------------------------------------------------------------------
  Get's called when active file is changed.
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.ActiveFileChange;
begin
  if csDestroying in self.ComponentState then
  Exit;

  if not fClosing then
  UpdateCaption;
end;

{*------------------------------------------------------------------------------
  Get's called when document's modified value is changed.
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.ModifiedChange;
begin
  UpdateCaption;
  // TODO: TabSet
  //TabItem.Modified:= Editor.Modified;
end;

{*------------------------------------------------------------------------------
  Do SearchReplace Text
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.DoSearchReplaceText(AReplace: boolean; ABackwards:
    boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if opt_check1.Checked then
  Include(Options, ssoMatchCase);
  if opt_check2.Checked then
  Include(Options, ssoWholeWord);
  if not opt_check3.Checked then
  Include(Options, ssoEntireScope);
  if opt_check4.Checked then
  Include(Options, ssoSelectedOnly);
  if ABackwards then
  Include(Options, ssoBackwards);

  if opt_check5.Checked then
  Editor.SearchEngine:= SynEditRegexSearch
  else
  Editor.SearchEngine:= SynEditSearch;

  if Editor.SearchReplace(SearchMemo.Text, ReplaceMemo.Text, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      Editor.BlockEnd := Editor.BlockBegin
    else
      Editor.BlockBegin := Editor.BlockEnd;
    Editor.CaretXY := Editor.BlockBegin;
  end;
end;

{*------------------------------------------------------------------------------
  Popuplate Highlighters
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.PopuplateHighlighters;
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
  item: TSpTBXItem;
begin
  for i := Self.ComponentCount - 1 downto 0 do
  begin
    if not (Self.Components[i] is TSynCustomHighlighter) then
    Continue;
    Highlighter:= Self.Components[i] as TSynCustomHighlighter;
      // only one highlighter for each language
    if Highlighters.IndexOf(Highlighter.GetLanguageName) = -1 then
    Highlighters.AddObject(Highlighter.GetLanguageName, Highlighter);
  end;
  Highlighters.Sort;
  for i:= 0 to Highlighters.Count - 1 do
  begin
    item:= TSpTBXItem.Create(self);
    item.Caption:= Highlighters.Strings[i];
    item.Tag:= i;
    item.RadioItem:= true;
    item.GroupIndex:= 1;
    item.OnClick:= DoHighlighterClick;
    highlighterSubmenu.Add(item);
  end;
end;

{*------------------------------------------------------------------------------
  Set ActiveFile Value
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SetActiveFile(const Value: WideString);
begin
  fActiveFile:= Value;
  ActiveFileChange;
  if SpTBXItem18.Checked then
  begin
    SetAutoHighlighter;
  end;
end;

{*------------------------------------------------------------------------------
  Set AutoHighlighter
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SetAutoHighlighter;
var
  ExtLen: integer;
  i, j, i2, i3: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
  Extension: String;
  item: TSpTBXItem;
begin
  Extension:= ExtractFileExt(ActiveFile);
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if (ExtLen > 0) then
  begin
    i3:= -1;
    for i := 0 to Highlighters.Count - 1 do
    begin
      if not (Highlighters.Objects[i] is TSynCustomHighlighter) then
      Continue;
      Highlighter := TSynCustomHighlighter(Highlighters.Objects[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        j := Pos(Extension, Filter);
        if (j > 0) and ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';')) then
        begin
          Editor.Highlighter:= Highlighter;
          for i2:= 0 to highlighterSubmenu.Count - 1 do
          begin
            if highlighterSubmenu.Items[i2] is TSpTBXItem then
            begin
              item:= TSpTBXItem(highlighterSubmenu.Items[i2]);
              if item.Tag = i then
              item.CaptionGlow:= gldBottomRight
              else
              item.CaptionGlow:= gldNone;
            end;
          end;            
          Exit;
        end
        else if Pos('*.*', Filter) > 0 then
        i3:= i;     
      end;
    end;

    if i3 > -1 then
    begin
      Highlighter := TSynCustomHighlighter(Highlighters.Objects[i3]);
      Editor.Highlighter:= Highlighter;
      for i2:= 0 to highlighterSubmenu.Count - 1 do
      begin
        if highlighterSubmenu.Items[i2] is TSpTBXItem then
        begin
          item:= TSpTBXItem(highlighterSubmenu.Items[i2]);
          if item.Tag = i3 then
          item.CaptionGlow:= gldBottomRight
          else
          item.CaptionGlow:= gldNone;
        end;
      end;

    end;

  end;
end;

{*------------------------------------------------------------------------------
  Show Editor Options
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.ShowOptions;
var
  cont: TSynEditorOptionsContainer;
  dlg: TSynEditOptionsDialog;
begin
  cont:= TSynEditorOptionsContainer.Create(self);
  dlg:= TSynEditOptionsDialog.Create(self);
  try
    cont.Assign(Editor);
    if dlg.Execute(cont) then
    begin
      cont.AssignTo(Editor);
      CETextEditorOptions.EditorOptions.Assign(Editor);
    end;
  finally
    dlg.Free;
    cont.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Do Highlighter Click
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.DoHighlighterClick(Sender: TObject);
var
  item: TSpTBXItem;
  i: Integer;
begin
  item:= TSpTBXItem(Sender);
  if item.Tag > -1 then
  begin
    Editor.Highlighter:= TSynCustomHighlighter(Highlighters.Objects[item.Tag]);
    for i:= 0 to highlighterSubmenu.Count - 1 do
    begin
      if highlighterSubmenu.Items[i] is TSpTBXItem then
      TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
    end;
  end
  else if item.Tag = -2 then
  begin
    SetAutoHighlighter;
  end
  else
  begin
    Editor.Highlighter:= nil;
    for i:= 0 to highlighterSubmenu.Count - 1 do
    begin
      if highlighterSubmenu.Items[i] is TSpTBXItem then
      TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
    end;
  end;
  item.Checked:= true;
end;

{*------------------------------------------------------------------------------
  EditorReplaceText
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.EditorReplaceText(Sender: TObject; const ASearch,
    AReplace: WideString; Line, Column: Integer; var Action: TSynReplaceAction);
var
  s: String;
  r: Integer;
begin
  if ASearch = AReplace then
    Action := raSkip
  else if fReplaceAll then
  begin
    Action:= raReplaceAll;
  end
  else
  begin
    s:= 'Replace this occurence of "' + ASearch + '"';
    r:= MessageBox(0, PChar(s), 'Replace?', MB_ICONQUESTION or MB_YESNOCANCEL);
    case r of
      idYes: Action:= raReplace;
      idNo: Action:= raSkip;
      idCancel: Action:= raCancel;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on Editor status change
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.EditorStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
begin
  if (scInsertMode in Changes) then
  begin
    if Editor.InsertMode then
    label_input.Caption:= _('Insert')
    else
    label_input.Caption:= _('Override');
  end;
  if (scModified in Changes) then
  begin
    if Editor.Modified then
    label_modified.Caption:= _('Modified')
    else
    label_modified.Caption:= '';
    ModifiedChange;
  end;
end;

{-------------------------------------------------------------------------------
  Get Page Action List
-------------------------------------------------------------------------------}
function TCETextEditorPage.GetPageActionList: TActionList;
begin
  Result:= ActionList;
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCETextEditorPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCETextEditorPageSettings;
end;

{-------------------------------------------------------------------------------
  Load from stream
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.LoadFromStream(AStream: TStream);
var
  ws: WideString;
begin
  LoadWideString(AStream, ws);
  if ws <> '' then
  OpenDocument(ws);
end;

{-------------------------------------------------------------------------------
  Save to stream
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SaveToStream(AStream: TStream);
begin
  SaveWideString(AStream, ActiveFile);
end;

{*------------------------------------------------------------------------------
  Search button click
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SpTBXButton1Click(Sender: TObject);
begin
  DoSearchReplaceText(false, (opt_radio.ItemIndex = 0));
end;

{*------------------------------------------------------------------------------
  Replace button click
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SpTBXButton2Click(Sender: TObject);
begin
  fReplaceAll:= false;
  DoSearchReplaceText(true, (opt_radio.ItemIndex = 0));
end;

{*------------------------------------------------------------------------------
  Replace All button click
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SpTBXButton3Click(Sender: TObject);
begin
  fReplaceAll:= true;
  DoSearchReplaceText(true, (opt_radio.ItemIndex = 0));
end;

{*------------------------------------------------------------------------------
  Edit actions Execute
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_edit_Execute(Sender: TObject);
var
  act: TTntAction;
  r: TRect;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    201: Editor.Undo;
    202: Editor.Redo;
    203: Editor.CopyToClipboard;
    204: Editor.CutToClipboard;
    205: Editor.PasteFromClipboard;
    206: Editor.ClearSelection;
    207: Editor.SelectAll;
    208: begin
           if not FindPanel.Visible then
           begin
            r:= self.ClientRect;
            if StatusBar.Visible then
            r.Bottom:= StatusBar.BoundsRect.Top;
            r.Top:= r.Bottom - FindPanel.Height;
            FindPanel.BoundsRect:= r;
            //FindPanel.Realign;
           end;
           FindPanel.Visible:= not FindPanel.Visible;
           if FindPanel.Visible then
           SearchMemo.SetFocus;
         end;
    209: DoSearchReplaceText(false, false);
    210: DoSearchReplaceText(false, true);
  end;
end;

{*------------------------------------------------------------------------------
  Edit actions Update
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_edit_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    201: act.Enabled:= Editor.CanUndo;
    202: act.Enabled:= Editor.CanRedo;
    205: act.Enabled:= Editor.CanPaste;
    208: act.Checked:= FindPanel.Visible;
    //208..211: act.Enabled:= false;
  end;
end;

{*------------------------------------------------------------------------------
  File Actions Execute
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_file_Execute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    101: NewDocument;
    102: OpenDocument;
    103: SaveDocument;
    104: SaveDocumentAs;
    105: CloseDocument;
    106: ReloadDocument;
  end;
end;

{*------------------------------------------------------------------------------
  File Action Update
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_file_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    103: act.Enabled:= Editor.Modified;
    106: act.Enabled:= WideFileExists(fActiveFile);
  end;
end;

{*------------------------------------------------------------------------------
  Format Action Execute
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_format_Execute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    301: Editor.WordWrap:= not Editor.WordWrap;
    302: ShowOptions;
  end;
end;

{*------------------------------------------------------------------------------
  Format Action Update
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_format_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    301: act.Checked:= Editor.WordWrap;
  end;
end;

{*------------------------------------------------------------------------------
  View Actions Execute
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_view_Execute(Sender: TObject);
var
  act: TTntAction;
  r: TRect;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    402: begin
           if not StatusBar.Visible then
           begin
             if FindPanel.Visible then
             begin
               r:= FindPanel.BoundsRect;
               r.Top:= r.Bottom;
               r.Bottom:= r.Top + StatusBar.Height;
               StatusBar.BoundsRect:= r;
             end;
           end;
           StatusBar.Visible:= not StatusBar.Visible;
         end;
  end;
end;

{*------------------------------------------------------------------------------
  View Actions Update
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.text_view_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    402: act.Checked:= StatusBar.Visible;  
  end;
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  CEActions.PageActionList:= Self.ActionList;
  Editor.SetFocus;
  GlobalPathCtrl.GlobalPathCaption:= label_path.Caption;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCETextEditorPage.UpdateCaption;
begin
  if fActiveFile = '' then
  begin
    TabCaption:= _('New text file');
    label_path.Caption:= _('New text file');  
    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 21;
  end
  else
  begin
    TabCaption:= WideExtractFileName(fActiveFile);
    label_path.Caption:= fActiveFile;
    TabItem.Images:= SmallSysImages;
    TabItem.ImageIndex:= GetIconIndex(fActiveFile);
  end;

  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.GlobalPathCaption:= label_path.Caption;
end;

{*------------------------------------------------------------------------------
  Get's called when tab is closing.
-------------------------------------------------------------------------------}
function TCETextEditorPage.TabClosing: Boolean;
begin
  fClosing:= true;
  Result:= CloseDocument;
  if Result then
  begin
    CETextEditorOptions.WordWrap:= Editor.WordWrap;
  end
  else
  fClosing:= false;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCETextEditorOptions
-------------------------------------------------------------------------------}
constructor TCETextEditorOptions.Create;
begin
  inherited;
  EditorOptions:= TSynEditorOptionsContainer.Create(nil);
  fRememberInnerToolbarLayout:= true;
end;

{*------------------------------------------------------------------------------
  Destroy TCETextEditorOptions
-------------------------------------------------------------------------------}
destructor TCETextEditorOptions.Destroy;
begin
  EditorOptions.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign options to TCETextEditorPage
-------------------------------------------------------------------------------}
procedure TCETextEditorOptions.AssignSettingsTo(EditPage: TCETextEditorPage);
begin
  if not assigned(EditPage) then
  Exit;

  EditorOptions.AssignTo(EditPage.Editor);
  EditPage.Editor.WordWrap:= fWordWrap;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCETextEditorPageSettings.GetPath: WideString;
begin
  Result:= TextEditorPage.ActiveFile;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCETextEditorPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= CETextEditorOptions.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCETextEditorPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= CETextEditorOptions.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCETextEditorPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= CETextEditorOptions.RememberOuterToolbarLayout;
end;

procedure TCETextEditorPageSettings.SetPath(const Value: WideString);
begin
  if Value <> '' then
  TextEditorPage.OpenDocument(Value);
end;

{##############################################################################}

initialization
  CETextEditorOptions:= TCETextEditorOptions.Create;
  GlobalAppSettings.AddItem('TextEditor', CETextEditorOptions, true);
  TabPageClassList.RegisterClass('TextEditor', TCETextEditorPage, TCETextEditorPageSettings);

finalization
  FreeAndNil(CETextEditorOptions);

end.
