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
//  The Original Code is fCE_QuickViewPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_QuickViewPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_GlobalCtrl, dCE_Images, CE_LanguageEngine,
  fCE_QuickView,
  // VSTools
  MPCommonUtilities,
  // PNG Controls
  PngImageList,
  // JVCL
  JvDockControlForm, JvDockVIDStyle,
  // SpTBX, Tb2k
  TB2Dock, SpTBXItem, 
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, ImgList, StdCtrls, CE_Toolbar, TB2Item, ComCtrls,
  SpTBXControls, CE_SpTBXItems, TB2Toolbar, ExtCtrls;

const
  WM_ActivateQuickView = WM_USER + 1;
    
type
  TCEQuickViewPanel = class(TCECustomDockableForm)
    PngImageList: TPngImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure WMActivateQuickView(var Message: TMessage); message
        WM_ActivateQuickView;
  public
    QuickView: TCEQuickView;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
  end;

var
  CEQuickViewPanel: TCEQuickViewPanel;



implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEQuickViewPanel is created.
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.FormCreate(Sender: TObject);
begin
  inherited;
  TopDock.Name:= 'QuickViewPanel_TopDock';
  BottomDock.Name:= 'QuickViewPanel_BottomDock';
  Caption:= _('Quickview');

  QuickView:= TCEQuickView.Create(nil);
  QuickView.Parent:= Self;
  QuickView.Align:= alClient;

  GlobalPathCtrl.RegisterNotify(self);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 20;
end;

{*------------------------------------------------------------------------------
  Get's called when TCEQuickViewPanel is destroyed.
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.FormDestroy(Sender: TObject);
begin
  QuickView.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  QuickView.ActiveFilePath:= NewPath;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.DoFormHide;
begin
  inherited;
 QuickView.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.DoFormShow;
begin
  inherited;
  Application.ProcessMessages;
  PostMessage(Handle, WM_ActivateQuickView, 1,0);
end;

{-------------------------------------------------------------------------------
  Activate QuickView
-------------------------------------------------------------------------------}
procedure TCEQuickViewPanel.WMActivateQuickView(var Message: TMessage);
begin
  inherited;
  QuickView.Active:= Message.WParam = 1;
end;

end.
