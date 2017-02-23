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

unit fCE_OptionsPage_Display_FolderTree;

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
  TCE_OptionsPage_Display_FolderTree = class(TCEOptionsCustomPage)
    check_autocollapse: TTntCheckBox;
    check_autoexpand: TTntCheckBox;
    check_newtabdefault: TTntCheckBox;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_Display_FolderTree: TCE_OptionsPage_Display_FolderTree;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_FolderTree.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Folders');
  PageTitle:= _('Folders Panel Settings');
  PagePath:= 'Display/Folders';
  ImageIndex:= 4;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FolderTree.ApplySettings;
begin
  // Toggles
  CEFolderPanel.Settings.AutoExpand:= check_autoexpand.Checked;
  CEFolderPanel.Settings.AutoCollapse:= check_autocollapse.Checked;
  CEFolderPanel.Settings.OpenInNewTab:= check_newtabdefault.Checked;
end;

procedure TCE_OptionsPage_Display_FolderTree.RefreshSettings;
begin
  // Toggles
  check_autoexpand.Checked:= CEFolderPanel.Settings.AutoExpand;
  check_autocollapse.Checked:= CEFolderPanel.Settings.AutoCollapse;
  check_newtabdefault.Checked:= CEFolderPanel.Settings.OpenInNewTab;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_FolderTree);

finalization

end.
