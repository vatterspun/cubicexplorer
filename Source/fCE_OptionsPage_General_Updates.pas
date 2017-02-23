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
//  The Original Code is fCE_OptionsPage_General_Updates.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_General_Updates;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine,
  // SpTBX
  SpTBXItem, SpTBXControls, SpTBXEditors,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, TntCheckLst, TntStdCtrls;

type
  TCE_OptionsPage_General_Updates = class(TCEOptionsCustomPage)
    check_autoupdates: TTntCheckBox;
    list_update_types: TTntCheckListBox;
    group_proxy: TTntGroupBox;
    edit_proxy_address: TTntEdit;
    label_proxy_address: TTntLabel;
    label_proxy_port: TTntLabel;
    edit_proxy_port: TTntEdit;
    check_proxy_system: TTntCheckBox;
    check_proxy: TTntCheckBox;
    TntLabel1: TTntLabel;
    procedure HandleChange(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_General_Updates: TCE_OptionsPage_General_Updates;

implementation

uses
  Main, CE_VersionUpdater, CE_Consts, CE_Utils;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_General_Updates.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Updates');
  PageTitle:= _('Updates Settings');
  PagePath:= 'General/Updates';
  ImageIndex:= 8;
  // update types
  list_update_types.Items.AddObject(GetBuildTypeDescription(btOfficial), TObject(btOfficial));
  list_update_types.Items.AddObject(GetBuildTypeDescription(btSnapshot), TObject(btSnapshot));
  //list_update_types.Items.AddObject(GetBuildTypeDescription(btDailySnapshot), TObject(btDailySnapshot)); // unused
  list_update_types.Items.AddObject(GetBuildTypeDescription(btTest), TObject(btTest));
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_General_Updates.ApplySettings;
var
  i: Integer;
  bt: TCEBuildType;
  bts: TCEBuildTypes;
begin
  // Auto check
  MainForm.Settings.AutoCheckUpdates:= check_autoupdates.Checked;
  bts:= [];
  for i:= 0 to list_update_types.Items.Count - 1 do
  begin
    if list_update_types.Checked[i] then
    begin
      bt:= TCEBuildType(list_update_types.Items.Objects[i]);
      Include(bts, bt);
    end;
  end;
  MainForm.Settings.CheckForUpdateTypes:= bts;
  // Proxy
  CE_ProxyAddress:= edit_proxy_address.Text + ':' + edit_proxy_port.Text;
  CE_UseProxy:= check_proxy.Checked;
  CE_UseSystemProxy:= check_proxy_system.Checked;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_General_Updates.RefreshSettings;
var
  i: Integer;
  port: Integer;
begin
  // Auto Check
  check_autoupdates.Checked:= MainForm.Settings.AutoCheckUpdates;
  for i:= 0 to list_update_types.Items.Count - 1 do
  begin
    list_update_types.Checked[i]:= TCEBuildType(list_update_types.Items.Objects[i]) in MainForm.Settings.CheckForUpdateTypes;
  end;
  // Proxy
  edit_proxy_address.Text:= ExtractUrlPort(CE_ProxyAddress, port);
  edit_proxy_port.Text:= IntToStr(port);
  check_proxy.Checked:= CE_UseProxy;
  check_proxy_system.Checked:= CE_UseSystemProxy;
  check_proxy_system.Enabled:= check_proxy.Checked;
  edit_proxy_address.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
  edit_proxy_port.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
  label_proxy_address.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
  label_proxy_port.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
end;

{-------------------------------------------------------------------------------
  HandleChange
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_General_Updates.HandleChange(Sender: TObject);
begin
  inherited;

  if (Sender = check_proxy_system) or (Sender = check_proxy) then
  begin
    check_proxy_system.Enabled:= check_proxy.Checked;
    edit_proxy_address.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
    edit_proxy_port.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
    label_proxy_address.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
    label_proxy_port.Enabled:= (not check_proxy_system.Checked) and check_proxy.Checked;
  end;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_General_Updates);

finalization

end.
