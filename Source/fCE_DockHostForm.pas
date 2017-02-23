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
//  The Original Code is fCE_DockHostForm.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_DockHostForm;

interface

uses
  // CE Units
  CE_Layout, CE_AppSettings, dCE_Images,
  // JVCL
  JvDockControlForm,
  // Toolbar2000
  TB2Dock,
  // SpTBXLib
  SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TControlAccess = class(TControl);
  TCEDockHostForm = class(TForm)
    CenterPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    DockServer: TJvDockServer;
    BottomGroupToolDock: TSpTBXDock;
    LeftGroupToolDock: TSpTBXDock;
    RightGroupToolDock: TSpTBXDock;
    TopGroupToolDock: TSpTBXDock;
  end;

implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEDockHostForm is created.
-------------------------------------------------------------------------------}
procedure TCEDockHostForm.FormCreate(Sender: TObject);
begin
  DockServer:= TJvDockServer.Create(self);
  DockServer.DockStyle:= CEDockStyle;
  DockServer.TopDockPanel.Parent:= CenterPanel;
  DockServer.TopSplitter.Parent:= CenterPanel;
  DockServer.BottomDockPanel.Parent:= CenterPanel;
  DockServer.BottomSplitter.Parent:= CenterPanel;

  DockServer.LeftSplitter.Width:= 5;
  DockServer.LeftSplitter.Cursor:= crSizeWE;
  DockServer.RightSplitter.Width:= 5;
  DockServer.RightSplitter.Cursor:= crSizeWE;
  DockServer.TopSplitter.Height:= 5;
  DockServer.TopSplitter.Cursor:= crSizeNS;
  DockServer.BottomSplitter.Height:= 5;
  DockServer.BottomSplitter.Cursor:= crSizeNS;

  TControlAccess(DockServer.LeftDockPanel).ParentColor:= true;
  TControlAccess(DockServer.TopDockPanel).ParentColor:= true;
  TControlAccess(DockServer.RightDockPanel).ParentColor:= true;
  TControlAccess(DockServer.BottomDockPanel).ParentColor:= true;

  // Toolbar docks
  TopGroupToolDock:= TSpTBXDock.Create(Self);
  TopGroupToolDock.Name:= 'TopPageToolDock';
  TopGroupToolDock.Parent:= CenterPanel;
  TopGroupToolDock.Position:= dpTop;
  BottomGroupToolDock:= TSpTBXDock.Create(Self);
  BottomGroupToolDock.Name:= 'BottomPageToolDock';
  BottomGroupToolDock.Parent:= CenterPanel;
  BottomGroupToolDock.Position:= dpBottom;
  LeftGroupToolDock:= TSpTBXDock.Create(Self);
  LeftGroupToolDock.Name:= 'LeftPageToolDock';
  LeftGroupToolDock.Parent:= CenterPanel;
  LeftGroupToolDock.Position:= dpLeft;
  RightGroupToolDock:= TSpTBXDock.Create(Self);
  RightGroupToolDock.Name:= 'RightPageToolDock';
  RightGroupToolDock.Parent:= CenterPanel;
  RightGroupToolDock.Position:= dpRight;  
end;

{*------------------------------------------------------------------------------
  Get's called when TCEDockHostForm is destroyed.
-------------------------------------------------------------------------------}
procedure TCEDockHostForm.FormDestroy(Sender: TObject);
begin
  DockServer.Free;
end;

end.
