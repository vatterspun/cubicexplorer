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
//  The Original Code is fCE_PoEditor_NewForm.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_PoEditor_NewForm;

interface

uses
  // CE Units
  CE_LanguageCodes, CE_LanguageEngine,
  // Tnt
  TntFileCtrl, TntStdCtrls, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TCENewTranslationDlg = class(TTntForm)
    TntLabel1: TTntLabel;
    edit_path: TTntEdit;
    Path: TTntLabel;
    but_browser: TTntButton;
    but_create: TTntButton;
    but_cancel: TTntButton;
    Bevel1: TBevel;
    LanguageList: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure but_browserClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure LanguageListChange(Sender: TObject);
  private
    { Private declarations }
  public
    LocalePath: WideString;
    { Public declarations }
  end;

function ShowNewTranslationDlg(LocalePath: WideString; var Language:
    WideString; var Path: WideString): Boolean;

implementation

uses
  Main;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Show NewTranslation Dialog
-------------------------------------------------------------------------------}
function ShowNewTranslationDlg(LocalePath: WideString; var Language:
    WideString; var Path: WideString): Boolean;
var
  dlg1: TCENewTranslationDlg;
begin
  dlg1:= TCENewTranslationDlg.Create(nil);
  try
    dlg1.LocalePath:= LocalePath;
    dlg1.PopupParent:= MainForm;
    Result:= dlg1.ShowModal = mrOK;
    if Result then
    begin
      Language:= dlg1.LanguageList.Text;
      Path:= dlg1.edit_path.Text;
    end;
  finally
    dlg1.Free;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Create
-------------------------------------------------------------------------------}
procedure TCENewTranslationDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  CEGlobalTranslator.TranslateComponent(Self);
  GetLanguageNames(LanguageList.Items);
  for i:= 0 to LanguageList.Items.Count - 1 do
  begin
    LanguageList.Items.Objects[i]:= Pointer(i);
  end;
  LanguageList.Sorted:= true;
end;

{*------------------------------------------------------------------------------
  On but_browser Click
-------------------------------------------------------------------------------}
procedure TCENewTranslationDlg.but_browserClick(Sender: TObject);
var
  ws: WideString;
begin
  if WideSelectDirectory('Select folder', LocalePath, ws) then
  begin
    edit_path.Text:= ws;
  end;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCENewTranslationDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

{*------------------------------------------------------------------------------
  On LanguageList Change
-------------------------------------------------------------------------------}
procedure TCENewTranslationDlg.LanguageListChange(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  i:= LanguageList.Items.IndexOf(LanguageList.Text);
  if i > -1 then
  s:= GetLanguageCode(Integer(LanguageList.Items.Objects[i]))
  else
  s:= LanguageList.Text;
  edit_path.Text:= LocalePath + s + '\';
end;

end.
