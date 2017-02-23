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
//  The Original Code is fCE_OptionsPage_Display.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Display;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine, dCE_Actions,
  // SpTBX
  SpTBXSkins,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCEOptionsPage_Display = class(TCEOptionsCustomPage)
    TntLabel1: TTntLabel;
    combo_theme: TComboBox;
    check_path_in_title: TTntCheckBox;
    TntButton1: TTntButton;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CEOptionsPage_Display: TCEOptionsPage_Display;

implementation

uses
  Main;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_Display.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  PageName:= _('Display');
  PageTitle:= _('Display Settings');
  PagePath:= 'Display';
  ImageIndex:= 2;
  PageListPosition:= 2;
  // Fill themes combo
  SkinManager.SkinsList.GetSkinNames(combo_theme.Items);
  SkinManager.SkinsList.Sort;
  I:= SkinManager.SkinsList.IndexOf('Default');
  if I > -1 then
  SkinManager.SkinsList.Move(I, 0);
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.ApplySettings;
begin
  // theme
  if MainForm.Settings.Skin <> combo_theme.Text then
  MainForm.Settings.Skin:= combo_theme.Text;
  MainForm.Settings.PathInTitle:= check_path_in_title.Checked;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.RefreshSettings;
var
  ws: WideString;
begin
  // Skin
  ws:= MainForm.Settings.Skin;
  combo_theme.ItemIndex:= combo_theme.Items.IndexOf(ws);
  check_path_in_title.Checked:= MainForm.Settings.PathInTitle;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_Display);

finalization

end.
