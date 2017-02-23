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
//  The Original Code is fCE_DockableForm.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_DockableForm;

interface

uses
  // CE Units
  CE_Layout, CE_GlobalCtrl, CE_VistaFuncs,
  // JVCL
  JvDockControlForm,
  // TB2k, SpTBX
  TB2Dock, SpTBXItem,
  // TNT
  TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj;

type
  TCECustomDockableForm = class(TForm, ICEPathChangeHandler)
    //ColorProvider: TmbTBXColorProvider;
    TopDock: TSpTBXDock;
    BottomDock: TSpTBXDock;
  private
    fImageIndex: Integer;
    fImageList: TImageList;
    fVisibleStatus: Boolean;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    { Private declarations }
  protected
    procedure Activate; override;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); virtual;
        stdcall;
  public
    DockClient: TJvDockClient;
    constructor Create(AOwner: TComponent); override;
    function IsVisible: Boolean;
    procedure DoFormHide; virtual;
    procedure DoFormShow; virtual;
    procedure DoStartUp; virtual;
    property ImageIndex: Integer read fImageIndex write fImageIndex;
    property ImageList: TImageList read fImageList write fImageList;
    { Public declarations }
  end;

implementation

uses
  Main, dCE_Actions;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCECustomDockableForm
-------------------------------------------------------------------------------}
constructor TCECustomDockableForm.Create(AOwner: TComponent);
begin
  inherited;
  fVisibleStatus:= false;
  SetVistaFont(Font);
  CEToolbarDocks.Add(TopDock);
  CEToolbarDocks.Add(BottomDock);
  DockClient:= TJvDockClient.Create(self);
  DockClient.DockStyle:= CEDockStyle;
  DockClient.OnFormHide:= FormHide;
  DockClient.OnFormShow:= FormShow;
  Self.OnHide:= FormHide;
  Self.OnShow:= FormShow;
  MainForm.Panels.Add(Self);
end;

{-------------------------------------------------------------------------------
  Called on Activate
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.Activate;
begin
  inherited;
  UpdateAllActions;
end;

{*------------------------------------------------------------------------------
  Handle Hide event
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.FormHide(Sender: TObject);
begin
  if not JvGlobalDockIsLoading then
  begin
    if fVisibleStatus then
    begin
      fVisibleStatus:= false;
      DoFormHide;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Show event
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.FormShow(Sender: TObject);
begin
  if not JvGlobalDockIsLoading then
  begin
    if not fVisibleStatus then
    begin
      fVisibleStatus:= true;
      DoFormShow;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.GlobalActivePageChange(OldPage, NewPage:
    TComponent);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.GlobalContentChange(Sender: TObject);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  // Override from descendant
end;

function TCECustomDockableForm.IsVisible: Boolean;
begin
  Result:= GetFormVisible(self);
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.DoFormHide;
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.DoFormShow;
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Get's called on Application startup
-------------------------------------------------------------------------------}
procedure TCECustomDockableForm.DoStartUp;
begin
  // Override from descendant
end;

end.
