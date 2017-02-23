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
//  The Original Code is fCE_AboutBox.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_AboutBox;

interface

uses
  // CE Units
  CE_VistaFuncs, CE_LanguageEngine,
  // PNG Controls
  pngimage,
  // Syn Edit
  SynEditHighlighter, SynHighlighterURI, SynURIOpener, SynEdit, SynMemo,
  // Tnt
  TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TntStdCtrls, jpeg, SpTBXItem, SpTBXControls;

type
  TCEAboutBox = class(TTntForm)
    Image1: TImage;
    SynMemo1: TSynMemo;
    SynURIOpener1: TSynURIOpener;
    SynURISyn1: TSynURISyn;
    but_close: TTntButton;
    Panel1: TPanel;
    VersionLabel: TSpTBXLabel;
    BuildLabel: TSpTBXLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure but_closeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TntFormKeyPress(Sender: TObject; var Key: Char);
  public

  end;

procedure ShowAboutBox;

implementation

uses
  JclFileUtils, Main;
  
{$R *.dfm}

{*------------------------------------------------------------------------------
  Show about dialog
-------------------------------------------------------------------------------}
procedure ShowAboutBox;
var
  box: TCEAboutBox;
begin
  box:= TCEAboutBox.Create(MainForm);
  box.Show;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  close button execute
-------------------------------------------------------------------------------}
procedure TCEAboutBox.but_closeClick(Sender: TObject);
begin
  PostMessage(self.Handle, WM_CLOSE,0,0);
end;

{*------------------------------------------------------------------------------
  Get's called on Form close
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

{*------------------------------------------------------------------------------
  Get's called when TCEAboutBox is created.
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormCreate(Sender: TObject);
var
  ver: TJclFileVersionInfo;
begin
  SetVistaFont(Font);
  ver:= TJclFileVersionInfo.Create(Application.ExeName);
  BuildLabel.Caption:= _('Version:') + ' ' + ver.FileVersion;
  VersionLabel.Caption:= ver.ProductVersion;
  ver.Free;
  CEGlobalTranslator.TranslateComponent(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when TCEAboutBox is shown.
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormShow(Sender: TObject);
begin
  SynMemo1.SetFocus;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEAboutBox.TntFormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) or (Key = #13) then
  but_close.Click;
end;

end.
