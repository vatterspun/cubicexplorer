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
//  The Original Code is fCE_BookmarkPropDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_BookmarkPropDlg;

interface

uses
  // CE Units
  fCE_FolderTreeForm, CE_Bookmarks, CE_StdBookmarkComps, dCE_Images,
  CE_Utils, CE_VistaFuncs,
  // TB2K, SpTBX
  SpTBXEditors, SpTBXFormPopupMenu, SpTBXControls,
  // Tnt Controls
  TntStdCtrls, TntForms,
  // VSTools
  MPCommonObjects, MPShellUtilities,
  // VirtualTrees
  VirtualTrees,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,  Menus, ImgList;

type
  TBookmarkPropDlg = class(TTntForm)
    PageControl: TPageControl;
    GeneralSheet: TTabSheet;
    but_OK: TButton;
    but_Cancel: TButton;
    but_Apply: TButton;
    img_icon: TImage;
    Label2: TLabel;
    check_relative: TCheckBox;
    edit_target: TSpTBXButtonEdit;
    FormPopupMenu: TSpTBXFormPopupMenu;
    edit_name: TTntEdit;
    TargetPanel: TPanel;
    but_browse: TButton;
    open1: TOpenDialog;
    SessionPanel: TPanel;
    Label1: TLabel;
    combo_session: TComboBox;
    procedure but_ApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure but_browseClick(Sender: TObject);
    procedure but_CancelClick(Sender: TObject);
    procedure but_OKClick(Sender: TObject);
    procedure check_relativeClick(Sender: TObject);
    procedure edit_nameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPopupMenuClosePopup(Sender: TObject; Selected: Boolean);
    procedure FormPopupMenuPopup(Sender: TObject);
    procedure TntFormKeyPress(Sender: TObject; var Key: Char);
  private
    tmpBookComp: TCECustomBookComp;
    fBookmarkComp: TCECustomBookComp;
    procedure SetBookmarkComp(const Value: TCECustomBookComp);
    procedure SetModified(const Value: Boolean);
    { Private declarations }
  protected
    fBookmarkTree: TCustomVirtualStringTree;
    FolderTreeForm: TCE_FolderTreeForm;
    fModified: Boolean;
    procedure SaveChanges;
    property Modified: Boolean read fModified write SetModified;
  public
    property BookmarkComp: TCECustomBookComp read fBookmarkComp write
        SetBookmarkComp;
  end;

function ShowBookmarkPropDlg(BookmarkComp: TCECustomBookComp; BookmarkTree:
    TCustomVirtualStringTree = nil): TBookmarkPropDlg;

var
  BookmarkPropDlg: TBookmarkPropDlg;

implementation

uses
  fCE_BookmarkPanel, CE_Sessions;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Show Bookmark Properties Dialog
-------------------------------------------------------------------------------}
function ShowBookmarkPropDlg(BookmarkComp: TCECustomBookComp; BookmarkTree:
    TCustomVirtualStringTree = nil): TBookmarkPropDlg;
begin
  Result:= TBookmarkPropDlg.Create(nil);
  Result.fBookmarkTree:= BookmarkTree;
  Result.Show;
  Result.BookmarkComp:= BookmarkComp;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TBookmarkPropDlg
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.FormCreate(Sender: TObject);
begin
  SetDesktopIconFonts(Font);
  FolderTreeForm:= TCE_FolderTreeForm.Create(self);
  FolderTreeForm.ChangeGlobalPathOnChange:= false;
  FolderTreeForm.CloseOnChange:= true;
  FormPopupMenu.PopupForm:= FolderTreeForm;
end;

{*------------------------------------------------------------------------------
  Destroy TBookmarkPropDlg
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.FormDestroy(Sender: TObject);
begin
  if assigned(tmpBookComp) then
  FreeAndNil(tmpBookComp);
end;

{*------------------------------------------------------------------------------
  Get's called on Folder Tree Form popup
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.FormPopupMenuPopup(Sender: TObject);
begin
  FolderTreeForm.FolderTree.BrowseTo(edit_target.Text);
end;

{*------------------------------------------------------------------------------
  Get's called when Folder Tree Form closes
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.FormPopupMenuClosePopup(Sender: TObject; Selected:
    Boolean);
var
  c: TCENormalItemComp;
  NS: TNamespace;
begin
  if tmpBookComp is TCENormalItemComp then
  begin
    if not FolderTreeForm.FolderTree.ValidateNamespace(FolderTreeForm.FolderTree.GetFirstSelected,NS) then
    Exit;
    
    c:= TCENormalItemComp(tmpBookComp);

    if assigned(c.Namespace) then
    begin
      if ILIsEqual(c.Namespace.AbsolutePIDL, NS.AbsolutePIDL) then
      Exit;
      
      if assigned(c.Namespace) then
      FreeAndNil(c.Namespace);
    end;

    c.Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(NS.AbsolutePIDL),nil);
    if check_relative.Checked then
    edit_target.Text:= EncodeRelativePath(NS.NameParseAddress)
    else
    edit_target.Text:= NS.NameParseAddress;
  end;

  if assigned(tmpBookComp.ImageList) then
  begin
    img_icon.Canvas.Brush.Style:= bsSolid;
    img_icon.Canvas.Brush.Color:= Color;
    img_icon.Canvas.FillRect(img_icon.ClientRect);
    tmpBookComp.ImageList.Draw(img_icon.Canvas,0,0,tmpBookComp.GetImageIndex,dsTransparent,itImage,true);
  end;
end;

{*------------------------------------------------------------------------------
  Set Active Bookmark Comp
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.SetBookmarkComp(const Value: TCECustomBookComp);
var
  c: TCECustomBookCompClass;
  i: Integer;
begin
  if assigned(tmpBookComp) then
  FreeAndNil(tmpBookComp);
  
  if not assigned(Value) then
  begin
    fBookmarkComp:= nil;
    Exit;
  end;
  fBookmarkComp:= Value;
  c:= TCECustomBookCompClass(fBookmarkComp.ClassType);
  tmpBookComp:= c.Create;
  tmpBookComp.Assign(Value);
  edit_name.Text:= tmpBookComp.Title;
  if assigned(fBookmarkComp.ImageList) then
  begin
    img_icon.Canvas.Brush.Style:= bsSolid;
    img_icon.Canvas.Brush.Color:= Color;
    img_icon.Canvas.FillRect(img_icon.ClientRect);
    BookmarkComp.ImageList.Draw(img_icon.Canvas,0,0,fBookmarkComp.GetImageIndex,dsTransparent,itImage,true);
  end;

  if fBookmarkComp is TCECategoryComp then
  begin
    TargetPanel.Visible:= false;
    SessionPanel.Visible:= false;
  end
  else if fBookmarkComp is TCENormalItemComp then
  begin
    check_relative.Checked:= TCENormalItemComp(tmpBookComp).Relative;
    if check_relative.Checked then
    edit_target.Text:= EncodeRelativePath(TCENormalItemComp(tmpBookComp).NameParseAddress)
    else
    edit_target.Text:= TCENormalItemComp(tmpBookComp).NameParseAddress;
    TargetPanel.Visible:= true;
    SessionPanel.Visible:= false;
  end
  else if fBookmarkComp is TCESessionComp then
  begin
    TargetPanel.Visible:= false;
    SessionPanel.Visible:= true;
    combo_session.Items.Clear;
    for i:= 0 to GlobalSessions.Sessions.Count - 1 do
    begin
      combo_session.Items.Add(GlobalSessions.Sessions.Items[i].Name);
    end;
    combo_session.ItemIndex:= GlobalSessions.Sessions.IndexOf(TCESessionComp(fBookmarkComp).SessionName);
  end;

  Modified:= false;
end;

{*------------------------------------------------------------------------------
  Browse for target file
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.but_browseClick(Sender: TObject);
var
  c: TCENormalItemComp;
begin
  if open1.Execute then
  begin
    if check_relative.Checked then
    edit_target.Text:= EncodeRelativePath(open1.FileName)
    else
    edit_target.Text:= open1.FileName;
    
    if tmpBookComp is TCENormalItemComp then
    begin
      c:= TCENormalItemComp(tmpBookComp);

      if assigned(c.Namespace) then
      begin
        if assigned(c.Namespace) then
        FreeAndNil(c.Namespace);
        c.Namespace:= TNamespace.CreateFromFileName(open1.FileName);
      end
      else
      begin
        c.Namespace:= TNamespace.CreateFromFileName(open1.FileName);
      end;
    end;

    if assigned(tmpBookComp.ImageList) then
    begin
      img_icon.Canvas.Brush.Style:= bsSolid;
      img_icon.Canvas.Brush.Color:= Color;
      img_icon.Canvas.FillRect(img_icon.ClientRect);
      tmpBookComp.ImageList.Draw(img_icon.Canvas,0,0,tmpBookComp.GetImageIndex,dsTransparent,itImage,true);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Cancel
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.but_CancelClick(Sender: TObject);
begin
  Close;
end;

{*------------------------------------------------------------------------------
  OK
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.but_OKClick(Sender: TObject);
begin
  SaveChanges;
  Close;
end;

{*------------------------------------------------------------------------------
  Apply
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.but_ApplyClick(Sender: TObject);
begin
  SaveChanges;

  if assigned(tmpBookComp.ImageList) then
  begin
    img_icon.Canvas.Brush.Style:= bsSolid;
    img_icon.Canvas.Brush.Color:= Color;
    img_icon.Canvas.FillRect(img_icon.ClientRect);
    tmpBookComp.ImageList.Draw(img_icon.Canvas,0,0,tmpBookComp.GetImageIndex,dsTransparent,itImage,true);
  end;
end;

{*------------------------------------------------------------------------------
  check_relativeClick
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.check_relativeClick(Sender: TObject);
begin
  if tmpBookComp is TCENormalItemComp then
  begin
    if check_relative.Checked then
    edit_Target.Text:= EncodeRelativePath(edit_Target.Text)
    else
    edit_Target.Text:= DecodeRelativePath(edit_Target.Text);

    Modified:= true;
  end;
end;

{*------------------------------------------------------------------------------
  edit_nameChange
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.edit_nameChange(Sender: TObject);
begin
  Modified:= true;
end;

{*------------------------------------------------------------------------------
  On Form Close (Free instance of TBookmarkPropDlg)
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

{*------------------------------------------------------------------------------
  Save changes to bookmark
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.SaveChanges;
var
  node: PVirtualNode;
  c: TCENormalItemComp;
begin
  tmpBookComp.Title:= edit_name.Text;
  if tmpBookComp is TCENormalItemComp then
  begin
    c:= TCENormalItemComp(tmpBookComp);
    c.LoadFromPath(edit_target.Text, check_relative.Checked);
  end
  else if tmpBookComp is TCESessionComp then
  begin
    if combo_session.ItemIndex > -1 then
    TCESessionComp(tmpBookComp).SessionName:= combo_session.Items.Strings[combo_session.ItemIndex];
  end;
  
  if assigned(fBookmarkComp) then
  begin
    fBookmarkComp.Assign(tmpBookComp);
    CEBookmarkPanel.SaveBookmarks;
  end;

  if assigned(fBookmarkTree) then
  begin
    fBookmarkTree.BeginUpdate;
    try
      node:= fBookmarkTree.GetFirstSelected;
      while assigned(node) do
      begin
        fBookmarkTree.ReinitNode(node,false);
        node:= fBookmarkTree.GetNextSelected(node);
      end;
    finally
      fBookmarkTree.Refresh;
      fBookmarkTree.EndUpdate;
    end;
  end;
  Modified:= false;
end;

{*------------------------------------------------------------------------------
  SetModified
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.SetModified(const Value: Boolean);
begin
  fModified:= Value;
  but_Apply.Enabled:= fModified;
end;

{-------------------------------------------------------------------------------
  On TBookmarkPropDlg.KeyPress
-------------------------------------------------------------------------------}
procedure TBookmarkPropDlg.TntFormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

end.
