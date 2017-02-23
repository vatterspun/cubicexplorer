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
//  The Original Code is fCE_OptionsPage_General.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_General;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine, dCE_Images,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SpTBXControls, SpTBXItem;

type
  TCEOptionsPage_General = class(TCEOptionsCustomPage)
    check_singleinstance: TTntCheckBox;
    radio_default: TTntRadioButton;
    group_startup: TTntGroupBox;
    radio_lasttime: TTntRadioButton;
    radio_session: TTntRadioButton;
    combo_sessions: TTntComboBox;
    check_tray_enable: TTntCheckBox;
    check_tray_minimize: TTntCheckBox;
    check_tray_close: TTntCheckBox;
    check_tray_start: TTntCheckBox;
    TntGroupBox1: TTntGroupBox;
    TntGroupBox2: TTntGroupBox;
    but_register: TSpTBXButton;
    but_unregister: TSpTBXButton;
    procedure HandleChange(Sender: TObject);
    procedure radioClick(Sender: TObject);
    procedure but_registerClick(Sender: TObject);
    procedure but_unregisterClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Main, CE_Sessions, CE_ElevatedActions, MPCommonUtilities;

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_General
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_General.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('General');
  PageTitle:= _('General Settings');
  PagePath:= 'General';
  ImageIndex:= 0;
  PageListPosition:= 0;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.ApplySettings;
begin
  // Single Instance
  MainForm.Settings.SingleInstance:= check_singleinstance.Checked;
  // On Startup
  if radio_lasttime.Checked then
  MainForm.Settings.StartupType:= stLastSession
  else if radio_session.Checked then
  MainForm.Settings.StartupType:= stSession
  else
  MainForm.Settings.StartupType:= stNormal;

  if combo_sessions.ItemIndex > -1 then
  MainForm.Settings.AutoLoadSession:= combo_sessions.Items.Strings[combo_sessions.ItemIndex];
  // Tray Icon
  MainForm.Settings.ShowTray:= check_tray_enable.Checked;
  MainForm.Settings.MinimizeToTray:= check_tray_minimize.Checked;
  MainForm.Settings.CloseToTray:= check_tray_close.Checked;
  MainForm.Settings.StartInTray:= check_tray_start.Checked;
end;

{-------------------------------------------------------------------------------
  On HandleChange
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.HandleChange(Sender: TObject);
begin
  inherited;
  check_tray_minimize.Enabled:= check_tray_enable.Checked;
  check_tray_close.Enabled:= check_tray_enable.Checked;
  check_tray_start.Enabled:= check_tray_enable.Checked;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.RefreshSettings;
var
  i: Integer;
  ws: WideString;
begin
  // On Startup
  combo_sessions.Clear;
  for i:= 0 to GlobalSessions.Sessions.Count - 1 do
  begin
    combo_sessions.Items.Add(GlobalSessions.Sessions.Items[i].Name);
  end;

  case MainForm.Settings.StartupType of
    stNormal: radio_default.Checked:= true;
    stLastSession: radio_lasttime.Checked:= true;
    stSession: radio_session.Checked:= true;
  end;

  ws:= MainForm.Settings.AutoLoadSession;
  if ws <> '' then
  combo_sessions.ItemIndex:= combo_sessions.Items.IndexOf(ws);

  // Single Instance
  check_singleinstance.Checked:= MainForm.Settings.SingleInstance;

  // Tray Icon
  check_tray_enable.Checked:= MainForm.Settings.ShowTray;
  check_tray_minimize.Checked:= MainForm.Settings.MinimizeToTray;
  check_tray_close.Checked:= MainForm.Settings.CloseToTray;
  check_tray_start.Checked:= MainForm.Settings.StartInTray;

  check_tray_minimize.Enabled:= check_tray_enable.Checked;
  check_tray_close.Enabled:= check_tray_enable.Checked;
  check_tray_start.Enabled:= check_tray_enable.Checked;  
end;

{-------------------------------------------------------------------------------
  On Radio Click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.radioClick(Sender: TObject);
begin
  HandleChange(Sender);
  combo_sessions.Enabled:= radio_session.Checked;
end;

{-------------------------------------------------------------------------------
  On but_register.Click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.but_registerClick(Sender: TObject);
begin
  Elevated_RegisterDefaultFileManager(Self.Handle);
end;

{-------------------------------------------------------------------------------
  On but_unregister.Click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.but_unregisterClick(Sender: TObject);
begin
  Elevated_UnRegisterDefaultFileManager(Self.Handle);
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_General);

finalization

end.
