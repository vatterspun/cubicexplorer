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
//  The Original Code is fCE_ExtAppPage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_ExtAppPage;

interface

uses
  // CE Units
  CE_AppPanel, fCE_TabPage,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCEExtAppTabPage = class(TCECustomTabPage)
  private
  public
    ExtApp: TCEAppEmbedPanel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectPage; override;
    procedure UpdateCaption; override;
  end;

implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEExtAppTabPage
-------------------------------------------------------------------------------}
constructor TCEExtAppTabPage.Create(AOwner: TComponent);
begin
  inherited;
  Layout:= 'ExternalApp';
  ExtApp:= TCEAppEmbedPanel.Create(self);
  ExtApp.Parent:= self;
  ExtApp.Align:= alClient;
end;

{*------------------------------------------------------------------------------
  Destroy TCEExtAppTabPage
-------------------------------------------------------------------------------}
destructor TCEExtAppTabPage.Destroy;
begin
  ExtApp.Close;
  inherited;
end;

{*------------------------------------------------------------------------------
  Select page
-------------------------------------------------------------------------------}
procedure TCEExtAppTabPage.SelectPage;
begin
  ExtApp.ResizeApp;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEExtAppTabPage.UpdateCaption;
begin
  TabCaption:= ExtApp.GetAppWndText;
end;

end.
