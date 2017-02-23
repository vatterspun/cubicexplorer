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
//  The Original Code is fCE_FolderTreeForm.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FolderTreeForm;

interface

uses
  // CE Units
  CE_GlobalCtrl, CE_VistaFuncs, CE_Utils,
  // VSTools, VT
  VirtualTrees, VirtualExplorerTree, MPShellUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls;

type
  TCE_FolderTreeForm = class(TForm)
    FolderTree: TVirtualExplorerTree;
    procedure FolderTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FolderTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure FolderTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
  private
    fChangeGlobalPathOnChange: Boolean;
    fCloseOnChange: Boolean;
    { Private declarations }
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoSizeWidth;
    property ChangeGlobalPathOnChange: Boolean read fChangeGlobalPathOnChange write
        fChangeGlobalPathOnChange;
    property CloseOnChange: Boolean read fCloseOnChange write fCloseOnChange;
    { Public declarations }
  end;

implementation

uses
  dCE_Actions, SpTBXFormPopupMenu, Main;
  
{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCE_FolderTreeForm
-------------------------------------------------------------------------------}
constructor TCE_FolderTreeForm.Create(AOwner: TComponent);
begin
  inherited;
  SetDesktopIconFonts(Font);
  fCloseOnChange:= true;
  FolderTree.DefaultNodeHeight:= SmallShellIconSize + 1;
  FolderTree.Active:= true;
end;

{*------------------------------------------------------------------------------
  AutoSizeWidth
-------------------------------------------------------------------------------}
procedure TCE_FolderTreeForm.AutoSizeWidth;
begin
  Width:= FolderTree.GetMaxColumnWidth(-1);
end;

{*------------------------------------------------------------------------------
  On Key Action
-------------------------------------------------------------------------------}
procedure TCE_FolderTreeForm.FolderTreeKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
var
  NS: TNamespace;
begin
  if CharCode = VK_RETURN then
  begin
    FolderTree.ValidateNamespace(FolderTree.FocusedNode, NS);
    if assigned(NS) and fChangeGlobalPathOnChange then
    GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL);
    if Assigned(ActiveFormPopupMenu) and fCloseOnChange then
    ActiveFormPopupMenu.ClosePopup(True);
  end;
end;

{*------------------------------------------------------------------------------
  On MouseDown
-------------------------------------------------------------------------------}
procedure TCE_FolderTreeForm.FolderTreeMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  NS: TNamespace;
begin
  if not fChangeGlobalPathOnChange then
  Exit;
  
  if (Button = mbMiddle) or (Shift = [ssLeft, ssAlt]) or (Shift = [ssLeft, ssAlt, ssShift])  then
  begin
    node:= FolderTree.GetNodeAt(X,Y);
    if assigned(node) then
    begin
      FolderTree.ValidateNamespace(node, NS);
      if assigned(NS) then
      begin
        if ssShift in Shift then
        OpenFolderInTab(FolderTree, NS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
        else
        OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  On MouseUp
-------------------------------------------------------------------------------}
procedure TCE_FolderTreeForm.FolderTreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NS: TNamespace;
  hit: THitInfo;
begin
  if Button <> mbLeft then
  Exit;

  FolderTree.GetHitTestInfoAt(X, Y, true, hit);
  if assigned(hit.HitNode) then
  begin
    if (hiOnItemLabel in hit.HitPositions) or (hiOnNormalIcon in hit.HitPositions) then
    begin
      FolderTree.ValidateNamespace(hit.HitNode, NS);
      if assigned(NS) and fChangeGlobalPathOnChange then
      begin
        GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL);
      end;

      if Assigned(ActiveFormPopupMenu) and fCloseOnChange then
      ActiveFormPopupMenu.ClosePopup(True);
    end;
  end;
end;

end.
