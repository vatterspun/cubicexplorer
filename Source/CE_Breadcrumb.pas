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
//  The Original Code is CE_Breadcrumb.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Breadcrumb;

interface

uses
  // CE Units
  CE_ScrollToolbar, CE_GlobalCtrl, CE_VistaFuncs, fCE_FolderTreeForm,
  // SpTBX, TB2K
  SpTBXFormPopupMenu, SpTBXSkins, SpTBXItem,
  //TBXUtils, TBXThemes,
  // VSTools
  MPShellUtilities,
  // Graphics32
  GR32,
  // System Units
  Windows, SysUtils, Messages, Classes, Controls, ShlObj, Graphics, Math, Forms;

type
  TCEBreadcrumb = class(TCEScrollToolbar, ICEPathChangeHandler)
  private
    fShowBorder: Boolean;
  protected
    procedure DrawBackground; override;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); stdcall;
    procedure OnClosePopup(Sender: TObject; Selected: Boolean); virtual;
    procedure PopupFolderForm(X, Y: Integer; APIDL: PItemIDList); virtual;
  public
    FolderPopup: TSpTBXFormPopupMenu;
    PopupFormSize: TSize;
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    property ShowBorder: Boolean read fShowBorder write fShowBorder;
  end;

  TCEBreadcrumbItem = class(TCEScrollToolbarItem)
  private
    fOnClick: TNotifyEvent;
    fParent: TCEBreadcrumb;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
  public
    Namespace: TNamespace;
    destructor Destroy; override;
    function GetWidth(Buffer: TBitmap32): Integer; override;
    property Parent: TCEBreadcrumb read fParent write fParent;
  published
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
  end;

  TCEBreadcrumbBar = class(TSpTBXToolWindow)
  protected
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    Breadcrumb: TCEBreadcrumb;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  dCE_Actions;

{*------------------------------------------------------------------------------
  Destroy TCEBreadcrumbItem
-------------------------------------------------------------------------------}
destructor TCEBreadcrumbItem.Destroy;
begin
  if assigned(Namespace) then
  Namespace.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get item width
-------------------------------------------------------------------------------}
function TCEBreadcrumbItem.GetWidth(Buffer: TBitmap32): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  Result:= Buffer.TextWidth(Caption) + 8
  else
  Result:= Buffer.TextWidthW(Caption) + 8;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    if assigned(fOnClick) then
    fOnClick(Self);
    if assigned(Namespace) then
    GlobalPathCtrl.ChangeGlobalPathPIDL(nil, Namespace.AbsolutePIDL);
  end
  else if (Shift = [ssMiddle]) or (Shift = [ssAlt,ssLeft]) then
  begin
    if assigned(Namespace) then
    OpenFolderInTab(Self, Namespace.AbsolutePIDL);
  end
  else if (Shift = [ssShift,ssMiddle]) or (Shift = [ssShift,ssAlt,ssLeft]) then
  begin
    if assigned(Namespace) then
    OpenFolderInTab(Self, Namespace.AbsolutePIDL, false);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbItem.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  p: TPoint;
  r: TRect;
begin
  if Button = mbRight then
  begin
    if assigned(Parent) then
    begin
      r:= Parent.GetItemRect(Parent.Items.IndexOf(Self));
      p.X:= r.Left;
      p.Y:= r.Bottom;
      p:= Parent.ClientToScreen(p);
      Parent.PopupFolderForm(p.X,p.Y,Namespace.AbsolutePIDL);
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCEFolderCombo
-------------------------------------------------------------------------------}
constructor TCEBreadcrumb.Create(AOwner: TComponent);
var
  ncm: TNonClientMetrics;
begin
  inherited;

  ncm.cbSize := SizeOf(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(TNonClientMetrics), @ncm, 0);
  Bitmap.Font.Handle :=  CreateFontIndirect(ncm.lfMenuFont);
  Self.Constraints.MinHeight:= Bitmap.TextHeight('jJ') + 2;

  fShowBorder:= true;
  PopupFormSize.cx:= 200;
  PopupFormSize.cy:= 200;
  FolderPopup:= TSpTBXFormPopupMenu.Create(Self);
  FolderPopup.BorderStyle:= pbsSizeableRightBottom;
  FolderPopup.OnClosePopup:= OnClosePopup;
  FolderPopup.PopupFocus:= true;
  GlobalPathCtrl.RegisterNotify(self);
end;

{*------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.DrawBackground;
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalContentChange(Sender: TObject);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called on global focus change.
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Get's called on global path change (string)
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalPathChanged(Sender: TObject; NewPath: WideString);
var
  apidl: PItemIDList;
begin
  apidl:= PathToPIDL(NewPath);
  try
    GlobalPIDLChanged(Sender, apidl);
  finally
    if assigned(apidl) then
    PIDLMgr.FreeAndNilPIDL(apidl);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on global path change (PIDL)
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
var
  item: TCEBreadcrumbItem;
  NS: TNamespace;
  APIDL: PItemIDList;
  i: Integer;
  recreate: Boolean;
begin
  recreate:= true;

  if assigned(NewPIDL) then
  begin
    for i:= 0 to Items.Count - 1 do
    begin
      item:= TCEBreadcrumbItem(Items.Items[i]);
      if PIDLMgr.EqualPIDL(item.Namespace.AbsolutePIDL, NewPIDL) then
      begin
        item.Checked:= true;
        recreate:= false;
        Self.MakeItemVisible(i);
      end
      else
      begin
        item.Checked:= false;
      end;
    end;
  end;

  if not recreate then
  begin
    DrawBar;
    Exit;
  end;

  Clear;

  item:= TCEBreadcrumbItem(InsertItem(TCEBreadcrumbItem,0));
  item.Parent:= self;
  APIDL:= PIDLMgr.CopyPIDL(NewPIDL);
  item.Namespace:= TNamespace.Create(APIDL,nil);
  item.Caption:= item.Namespace.NameInFolder;
  item.Checked:= true;
  i:= 0;
  NS:= item.Namespace;

  if not NS.IsDesktop then
  begin
    while assigned(NS.Parent) do
    begin
      NS:= NS.Parent;
      item:= TCEBreadcrumbItem(InsertItem(TCEBreadcrumbItem,0));
      item.Parent:= self;
      APIDL:= PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
      item.Namespace:= TNamespace.Create(APIDL,nil);
      item.Caption:= item.Namespace.NameInFolder;
      Inc(i);
      if NS.IsDesktop then
      break;
    end;
  end;
  Self.MakeItemVisible(i);
  DrawBar;
end;

{*------------------------------------------------------------------------------
  Popup Folder Form
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.PopupFolderForm(X, Y: Integer; APIDL: PItemIDList);
var
  form: TCE_FolderTreeForm;
  w: Integer;
begin
  if Assigned(ActiveFormPopupMenu) then
  ActiveFormPopupMenu.ClosePopup(false);

  if assigned(FolderPopup.PopupForm) then
  begin
    FolderPopup.PopupForm.Free;
    FolderPopup.PopupForm:= nil;
  end;
  
  form:= TCE_FolderTreeForm.Create(nil);
  form.ChangeGlobalPathOnChange:= true;
  form.CloseOnChange:= true;
  form.FolderTree.RootFolderCustomPIDL:= APIDL;
  form.Width:= PopupFormSize.cx;
  form.Height:= PopupFormSize.cy;
  form.AutoSizeWidth;

  w:= GetSystemMetrics(SM_CXVSCROLL) + 8;
  form.Width:= form.Width + w;
  FolderPopup.PopupForm:= form;
  FolderPopup.Popup(self.Offset + X,Y);
end;

{*------------------------------------------------------------------------------
  Get's called on form popup close
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.OnClosePopup(Sender: TObject; Selected: Boolean);
begin
  if assigned(FolderPopup.PopupForm) then
  begin
    PopupFormSize.cx:= FolderPopup.PopupForm.Width;
    PopupFormSize.cy:= FolderPopup.PopupForm.Height;
    FolderPopup.PopupForm.Free;
    FolderPopup.PopupForm:= nil;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.Resize;
var
  i: Integer;
  r: TRect;
begin
  Inherited;
  for i:= 0 to Self.Items.Count-1 do
  begin
    if TCEBreadcrumbItem(Self.Items.Items[i]).Checked then
    break;
  end;
  if i > -1 then
  begin
    r:= Self.GetItemRect(Self.Items.Count-1);
    if r.Right < Self.Bitmap.Width then
    Self.MakeItemVisible(0)
    else
    Self.MakeItemVisible(i);
  end;
  DrawBar;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCEBreadcrumbBar
-------------------------------------------------------------------------------}
constructor TCEBreadcrumbBar.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  Parent:= TWinControl(AOwner);

  self.Stretch:= true;
  Breadcrumb:= TCEBreadcrumb.Create(self);
  Breadcrumb.Parent:= self;
  Breadcrumb.Align:= alClient;
  Breadcrumb.SeparatorSize:= 1;
  self.MinClientHeight:= Max(Breadcrumb.Constraints.MinHeight,20);
  self.ClientHeight:= 22;
end;

{-------------------------------------------------------------------------------
  Do ContextPopup
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbBar.DoContextPopup(MousePos: TPoint; var Handled:
    Boolean);
begin
  inherited;
  Handled:= Self.Breadcrumb.IndexByPos(MousePos.X, MousePos.Y) > -1;
end;



end.
