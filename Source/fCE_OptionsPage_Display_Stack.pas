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
//  The Original Code is fCE_OptionsPage_Display_FolderTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Display_Stack;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, fCE_FolderPanel,
  CE_LanguageEngine,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCE_OptionsPage_Display_Stack = class(TCEOptionsCustomPage)
    check_autoexpand: TTntCheckBox;
    check_autocollapse: TTntCheckBox;
    check_fullexpand: TTntCheckBox;
    group_startup: TTntGroupBox;
    radio_empty: TTntRadioButton;
    radio_lastused: TTntRadioButton;
    radio_selected: TTntRadioButton;
    combo_stacks: TTntComboBox;
    procedure radioClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_Display_Stack: TCE_OptionsPage_Display_Stack;

implementation

uses
  fCE_StackPanel;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_Stack.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Stack');
  PageTitle:= _('Stack Settings');
  PagePath:= 'Display/Stack';
  ImageIndex:= 7;
end;

{-------------------------------------------------------------------------------
  On radioClick
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_Stack.radioClick(Sender: TObject);
begin
  combo_stacks.Enabled:= radio_selected.Checked;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_Stack.ApplySettings;
begin
  // Toggles
  CEStackPanel.Settings.AutoExpand:= check_autoexpand.Checked;
  CEStackPanel.Settings.AutoCollapse:= check_autocollapse.Checked;
  CEStackPanel.Settings.FullExpandOnLoad:= check_fullexpand.Checked;
  // Startup
  if radio_selected.Checked then
  CEStackPanel.Settings.StartupType:= stUserSelected
  else if radio_lastused.Checked then
  CEStackPanel.Settings.StartupType:= stLastUsed
  else
  CEStackPanel.Settings.StartupType:= stEmpty;
  CEStackPanel.Settings.LoadOnStartup:= combo_stacks.Text;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_Stack.RefreshSettings;
begin
  // Toggles
  check_autoexpand.Checked:= CEStackPanel.Settings.AutoExpand;
  check_autocollapse.Checked:= CEStackPanel.Settings.AutoCollapse;
  check_fullexpand.Checked:= CEStackPanel.Settings.FullExpandOnLoad;
  // Startup
  case CEStackPanel.Settings.StartupType of
    stEmpty: radio_empty.Checked:= true;
    stLastUsed: radio_lastused.Checked:= true;
    stUserSelected: radio_selected.Checked:= true;
  end;
  CEStackPanel.FindStackNames(combo_stacks.Items);
  combo_stacks.ItemIndex:= combo_stacks.Items.IndexOf(CEStackPanel.Settings.LoadOnStartup);
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_Stack);

finalization

end.
