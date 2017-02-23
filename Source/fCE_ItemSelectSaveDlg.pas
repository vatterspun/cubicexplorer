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
//  The Original Code is fCE_ItemSelectSaveDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_ItemSelectSaveDlg;

interface

uses
  // Tnt
  TntStdCtrls, TntDialogs, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SpTBXItem, SpTBXControls, SpTBXEditors;


type
  TCEItemSelectSaveDlg = class(TTntForm)
    panel_background: TSpTBXPanel;
    label_combotitle: TSpTBXLabel;
    combo: TSpTBXComboBox;
    but_ok: TSpTBXButton;
    but_cancel: TSpTBXButton;
    procedure but_okClick(Sender: TObject);
    procedure comboChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    fAllowEmptyText: Boolean;
    fExistsWarningContent: WideString;
    fExistsWarningDescription: WideString;
    fExistsWarningTitle: WideString;
    fShowExistsWarning: Boolean;
    procedure SetAllowEmptyText(const Value: Boolean);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property AllowEmptyText: Boolean read fAllowEmptyText write SetAllowEmptyText;
    property ExistsWarningContent: WideString read fExistsWarningContent write
        fExistsWarningContent;
    property ExistsWarningDescription: WideString read fExistsWarningDescription
        write fExistsWarningDescription;
    property ExistsWarningTitle: WideString read fExistsWarningTitle write
        fExistsWarningTitle;
    property ShowExistsWarning: Boolean read fShowExistsWarning write
        fShowExistsWarning;
  end;

implementation

uses
  CE_VistaFuncs, CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEItemSelectSaveDlg
-------------------------------------------------------------------------------}
constructor TCEItemSelectSaveDlg.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  CEGlobalTranslator.TranslateComponent(Self);
  AllowEmptyText:= false;
  PopupParent:= Application.MainForm;
end;

{-------------------------------------------------------------------------------
  On but_ok Click
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.but_okClick(Sender: TObject);
begin
  if ShowExistsWarning then
  begin
    if combo.Items.IndexOf(combo.Text) > -1 then
    begin
      if (TaskDialog(Self.Handle,
                     ExistsWarningTitle,
                     ExistsWarningDescription,
                     ExistsWarningContent,
                     TD_ICON_QUESTION,
                     TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES) then
      ModalResult:= mrOK
      else
      combo.SetFocus;
    end
    else
    ModalResult:= mrOK;
  end
  else
  ModalResult:= mrOK;
end;

{-------------------------------------------------------------------------------
  On combo.Change
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.comboChange(Sender: TObject);
begin
  if not fAllowEmptyText then
  but_ok.Enabled:= combo.Text <> '';
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

{-------------------------------------------------------------------------------
  Set Allow Empty Text
-------------------------------------------------------------------------------}
procedure TCEItemSelectSaveDlg.SetAllowEmptyText(const Value: Boolean);
begin
  fAllowEmptyText:= Value;
  if fAllowEmptyText then
  but_ok.Enabled:= true
  else
  but_ok.Enabled:= combo.Text <> '';
end;

end.
