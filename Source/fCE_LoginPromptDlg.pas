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
//  The Original Code is fCE_LoginPromptDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_LoginPromptDlg;

interface

uses
  // Tnt
  TntStdCtrls, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCELoginPromptDlg = class(TTntForm)
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    edit_username: TTntEdit;
    edit_password: TTntEdit;
    check_save: TTntCheckBox;
    label_warning: TTntLabel;
    but_ok: TTntButton;
    but_cancel: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCELoginPromptDlg.FormCreate(Sender: TObject);
begin
  // Translate
  CEGlobalTranslator.TranslateComponent(Self);
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCELoginPromptDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

end.
