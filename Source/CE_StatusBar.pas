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
//  The Original Code is CE_StatusBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_StatusBar;

interface

uses
  // CE Unit
  CE_Utils, CE_GlobalCtrl, CE_FileView, fCE_FileView, CE_LanguageEngine,
  // TB2k, TBX, SpTBX
  SpTBXItem,
  // VSTools
  EasyListView, VirtualExplorerEasyListView, MPShellUtilities,
  // System Units
  Classes, Windows, SysUtils, ShlObj;

type

  TCEStatusBar = class(TSpTBXStatusBar, ICEPathChangeHandler)
  private
    SelectionLabel: TSpTBXLabelItem;
    SelectionSizeLabel: TSpTBXLabelItem;
    CurrentItemLabel: TSpTBXLabelItem;
  protected
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;
    procedure UpdateLabels(InvalidateParent: Boolean = false);
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEStatusBar
-------------------------------------------------------------------------------}
constructor TCEStatusBar.Create(AOwner: TComponent);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize items
-------------------------------------------------------------------------------}
procedure TCEStatusBar.Initialize;
begin
  SelectionLabel:= TSpTBXLabelItem.Create(self);
  SelectionLabel.Caption:= '0/0 ' + _('Selected');
  Self.Items.Add(SelectionLabel);

  Self.Items.Add(TSpTBXSeparatorItem.Create(Self));

  SelectionSizeLabel:= TSpTBXLabelItem.Create(self);
  SelectionSizeLabel.Caption:= '0/0 ' + _('KB');
  Self.Items.Add(SelectionSizeLabel);

  Self.Items.Add(TSpTBXSeparatorItem.Create(Self));
  Self.Items.Add(TSpTBXRightAlignSpacerItem.Create(Self));

  CurrentItemLabel:= TSpTBXLabelItem.Create(Self);
  CurrentItemLabel.Alignment:= taRightJustify;
  Self.Items.Add(CurrentItemLabel);
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalContentChange(Sender: TObject);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
 Get's called when global focus has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalFocusChanged(Sender: TObject; NewPath: WideString);
begin
  UpdateLabels;
end;

{*------------------------------------------------------------------------------
  Get's called when global path has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalPathChanged(Sender: TObject; NewPath: WideString);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  Get's called when global PIDL has changed
-------------------------------------------------------------------------------}
procedure TCEStatusBar.GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  Update Labels
-------------------------------------------------------------------------------}
procedure TCEStatusBar.UpdateLabels(InvalidateParent: Boolean = false);
var
  i,it: Int64;
  Item: TEasyItem;
  fileView: TCEFileView;
  si, st: WideString;
  ws: WideString;
  NS: TNamespace;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    fileView:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;
    ws:= IntToStr(fileView.Selection.Count) + '/' +
         IntToStr(fileView.ItemCount) + ' ' + _('Selected');
    SelectionLabel.Caption:= ws;

    i:= 0;
    Item := fileView.Selection.First;
    while Assigned(Item) do
    begin
      i:= i + TExplorerItem(Item).Namespace.SizeOfFileInt64;
      Item:= fileView.Selection.Next(Item);
    end;

    it:= fileView.RootFolderNamespace.FolderSize(InvalidateParent);

    if (i > 1024) and (i < 1048576) then
    si:= FloatToStrF(i / 1024, ffFixed, 4,0) + ' ' + _('KB')
    else if i >= 1048576 then
    si:= FloatToStrF(i / 1048576, ffFixed, 6,2) + ' ' + _('MB')
    else
    si:= IntToStr(i) + ' ' + _('Bytes');

    if (it > 1024) and (it < 1048576) then
    st:= FloatToStrF(it / 1024, ffFixed , 4,0) + ' ' + _('KB')
    else if it >= 1048576 then
    st:= FloatToStrF(it / 1048576, ffFixed , 6,2) + ' ' + _('MB')
    else
    st:= IntToStr(it) + ' ' + _('Bytes');

    SelectionSizeLabel.Caption:= si + ' / ' + st;

    if fileView.Selection.Count > 1 then
    begin
      ws:= IntToStr(fileView.Selection.Count) + ' ' + _('Selected') + ' | ' + _('Total Size') + ': ' + st;
    end
    else if assigned(fileView.Selection.FocusedItem) then
    begin
      NS:= TExplorerItem(fileView.Selection.FocusedItem).Namespace;
      ws:= NS.FileType + ' | ' + NS.LastWriteTime + ' | ' + si;
    end
    else
    begin
      ws:= '';
    end;

    CurrentItemLabel.Caption:= ws;
  end
  else
  begin
    SelectionLabel.Caption:= '0/0 ' + _('Selected');
    SelectionSizeLabel.Caption:= '0/0 ' + _('KB');
  end;
end;

end.
