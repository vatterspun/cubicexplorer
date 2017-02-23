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
//  The Original Code is fCE_ArchivePanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_ArchivePanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_ArchiveTree,
  // SpTbx
  TB2Dock, SpTBXItem,
  // VSTools
  MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCEArchiverPanel = class(TCECustomDockableForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
  public
    Browser: TCEArchiveTree;
  end;

var
  CEArchiverPanel: TCEArchiverPanel;

implementation

uses
  CE_GlobalCtrl;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCEArchiverPanel.FormCreate(Sender: TObject);
begin
  inherited;
//  Browser:= TCEArchiveTree.Create(Self);
//  Browser.Parent:= Self;
//  Browser.Align:= alClient;
//  
//  GlobalPathCtrl.RegisterNotify(self);
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCEArchiverPanel.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
//  if not Self.IsVisible then
//  Exit;
//  
//  try
//    if WideFileExists(NewPath) then
//    Browser.OpenArchive(NewPath);
//  except
//  end;
end;

end.
