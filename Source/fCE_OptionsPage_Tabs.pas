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
//  The Original Code is fCE_OptionsPage_Tabs.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Tabs;

interface

uses
  // CE Units
  fCE_OptionsCustomPage, fCE_OptionsDialog, CE_LanguageEngine, CE_Utils,
  CE_FileUtils, CE_CommonObjects,
  // Tnt
  TntStdCtrls, TntFileCtrl,
  // VSTools
  MPShellUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ShlObj, StrUtils;

type
  TCEOptionsPage_Tabs = class(TCEOptionsCustomPage)
    NewTabGroup: TTntGroupBox;
    radio_newtab_1: TTntRadioButton;
    radio_newtab_2: TTntRadioButton;
    radio_newtab_3: TTntRadioButton;
    edit_newtab: TTntEdit;
    but_newtab: TTntButton;
    check_newtab_switch: TTntCheckBox;
    check_opentab_switch: TTntCheckBox;
    check_reusetabs_switch: TTntCheckBox;
    check_nexttocur_switch: TTntCheckBox;
    check_autofit_switch: TTntCheckBox;
    check_dblclick_switch: TTntCheckBox;
    check_exit: TTntCheckBox;
    procedure radio_newtab_1Click(Sender: TObject);
    procedure but_newtabClick(Sender: TObject);
  private
    fNewTabNamespace: TNamespace;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

implementation

uses
  Main;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_General
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_Tabs.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Tabs');
  PageTitle:= _('Tabs Settings');
  PagePath:= 'Tabs';
  ImageIndex:= 1;
  PageListPosition:= 1;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCEOptionsPage_Tabs.Destroy;
begin
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);
  inherited;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Tabs.ApplySettings;
var
  new_path: WideString;
  pidl: PItemIDList;
begin
  if radio_newtab_2.Checked then
  MainForm.TabSet.Settings.NewTabType:= 2
  else if radio_newtab_3.Checked then
  MainForm.TabSet.Settings.NewTabType:= 3
  else
  MainForm.TabSet.Settings.NewTabType:= 1;

  if assigned(fNewTabNamespace) then
  begin
    if CE_SpecialNamespaces.GetSpecialID(fNewTabNamespace.AbsolutePIDL) > -1 then
    pidl:= nil
    else
    pidl:= PathToPIDL(fNewTabNamespace.NameForParsing);

    if not assigned(pidl) then
    new_path:= 'PIDL:' + SavePIDLToMime(fNewTabNamespace.AbsolutePIDL)
    else
    new_path:= fNewTabNamespace.NameForParsing;
    PIDLMgr.FreePIDL(pidl);
  end
  else
  new_path:= '';
  
  MainForm.TabSet.Settings.NewTabPath:= new_path;

  MainForm.TabSet.Settings.NewTabSelect:= check_newtab_switch.Checked;
  MainForm.TabSet.Settings.OpenTabSelect:= check_opentab_switch.Checked;
  MainForm.TabSet.Settings.ReuseTabs:= check_reusetabs_switch.Checked;
  MainForm.TabSet.Settings.OpenNextToCurrent:= check_nexttocur_switch.Checked;
  MainForm.TabSet.Settings.AutoFit:= check_autofit_switch.Checked;
  MainForm.TabSet.Settings.DblClickCloseTab:= check_dblclick_switch.Checked;
  MainForm.Settings.ExitOnLastTabClose:= check_exit.Checked;
end;

{-------------------------------------------------------------------------------
  radio_newtab button click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Tabs.radio_newtab_1Click(Sender: TObject);
begin
  edit_newtab.Enabled:= radio_newtab_3.Checked;
  but_newtab.Enabled:= radio_newtab_3.Checked;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  buttonh_newtab click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Tabs.but_newtabClick(Sender: TObject);
var
  pidl: PItemIDList;
begin
  pidl:= BrowseForFolderPIDL(_('Select default folder for new tabs.'));
  if assigned(pidl) then
  begin
    if assigned(fNewTabNamespace) then
    FreeAndNil(fNewTabNamespace);
    fNewTabNamespace:= TNamespace.Create(pidl, nil);
    edit_newtab.Text:= fNewTabNamespace.NameParseAddress;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Tabs.RefreshSettings;
var
  ws: WideString;
  pidl: PItemIDList;
  i: Integer;
begin
  // New tab type
  i:= MainForm.TabSet.Settings.NewTabType;
  case i of
    2: radio_newtab_2.Checked:= true;
    3: radio_newtab_3.Checked:= true;
    else
    radio_newtab_1.Checked:= true;
  end;
  // New tab custom path
  ws:= MainForm.TabSet.Settings.NewTabPath;
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);

  pidl:= nil;
  if Length(ws) > 5 then
  begin
    if LeftStr(ws, 5) = 'PIDL:' then
    begin
      ws:= Copy(ws, 6, Length(ws)-5);
      pidl:= LoadPIDLFromMime(ws);
    end;
  end;
  if not assigned(pidl) then
  pidl:= PathToPIDL(ws);
  fNewTabNamespace:= TNamespace.Create(pidl, nil);
  edit_newtab.Text:= fNewTabNamespace.NameAddressbar;

  // Switch to new tab
  check_newtab_switch.Checked:= MainForm.TabSet.Settings.NewTabSelect;
  // Switch to opened tab
  check_opentab_switch.Checked:= MainForm.TabSet.Settings.OpenTabSelect;
  // Switch to reuse tab
  check_reusetabs_switch.Checked:= MainForm.TabSet.Settings.ReuseTabs;
  // Switch to next to current
  check_nexttocur_switch.Checked:= MainForm.TabSet.Settings.OpenNextToCurrent;
  // Switch to autofit
  check_autofit_switch.Checked:= MainForm.TabSet.Settings.AutoFit;
  // Switch to close on double click
  check_dblclick_switch.Checked:= MainForm.TabSet.Settings.DblClickCloseTab;
  // Switch to Exit on last tab close
  check_exit.Checked:= MainForm.Settings.ExitOnLastTabClose;
end;

{##############################################################################}



initialization
  RegisterOptionsPageClass(TCEOptionsPage_Tabs);

finalization

end.
