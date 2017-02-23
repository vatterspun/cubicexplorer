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
//  The Original Code is fCE_OptionsPage_Display_Bookmarks.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Display_Bookmarks;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, fCE_BookmarkPanel,
  CE_LanguageEngine, CE_StdBookmarkComps,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCE_OptionsPage_Display_Bookmarks = class(TCEOptionsCustomPage)
    check_autoexpand: TTntCheckBox;
    check_autocollapse: TTntCheckBox;
    check_singleclick: TTntCheckBox;
    check_newtabdefault: TTntCheckBox;
  private
    { Private declarations }
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_Display_Bookmarks: TCE_OptionsPage_Display_Bookmarks;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_Bookmarks.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Bookmarks');
  PageTitle:= _('Bookmarks Panel Settings');
  PagePath:= 'Display/Bookmarks';
  ImageIndex:= 3;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_Bookmarks.ApplySettings;
begin
  // Toggles
  CEBookmarkPanel.Settings.AutoExpand:= check_autoexpand.Checked;
  CEBookmarkPanel.Settings.AutoCollapse:= check_autocollapse.Checked;
  CEBookmarkPanel.Settings.SingleClickMode:= check_singleclick.Checked;
  CEBookmarkPanel.Settings.OpenInNewTab:= check_newtabdefault.Checked;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_Bookmarks.RefreshSettings;
begin
  // Toggles
  check_autoexpand.Checked:= CEBookmarkPanel.Settings.AutoExpand;
  check_autocollapse.Checked:= CEBookmarkPanel.Settings.AutoCollapse;
  check_singleclick.Checked:= CEBookmarkPanel.Settings.SingleClickMode;
  check_newtabdefault.Checked:= CEBookmarkPanel.Settings.OpenInNewTab;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_Bookmarks);

finalization

end.
