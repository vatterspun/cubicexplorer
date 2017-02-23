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
//  The Original Code is fCE_OptionsPage_GlobalHotkeys.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_GlobalHotkeys;

interface

uses
  // CE
  fCE_OptionsCustomPage, CE_LanguageEngine, fCE_OptionsDialog, dCE_Images,
  dCE_Actions, CE_Utils,
  // VirtualTrees
  VirtualTrees,
  // SpTBX
  SpTBXItem, SpTBXControls,
  // TNT Controls
  TntActnList, TntExtCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, StdCtrls, TntStdCtrls, ExtCtrls, ActnList;

type
  PCEActTreeData = ^ACEActTreeData;
  ACEActTreeData = record
    ActionItem: TTntAction;
    IsCategory: Boolean;
    Name: WideString;
    IconIndex: Integer;
    Shortcut: TShortcut;
    Index: Integer;
  end;
  
  TTCEOptionsPage_GlobalHotkeys = class(TCEOptionsCustomPage)
    TntBevel1: TTntBevel;
    HotkeyList: TVirtualStringTree;
    label_action_name: TSpTBXLabel;
    TntLabel1: TTntLabel;
    list_actionhotkeys: TListBox;
    TntGroupBox1: TTntGroupBox;
    edit_hotkey: TTntEdit;
    but_add: TTntButton;
    but_replace: TTntButton;
    but_delete: TTntButton;
    procedure HotkeyListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure HotkeyListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure HotkeyListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure edit_hotkeyKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure edit_hotkeyKeyPress(Sender: TObject; var Key: Char);
    procedure but_addClick(Sender: TObject);
    procedure but_deleteClick(Sender: TObject);
    procedure but_replaceClick(Sender: TObject);
    procedure HotkeyListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure list_actionhotkeysClick(Sender: TObject);
    procedure HotkeyListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fnewHotkey: TShortcut;
    fselectedAction: TTntAction;
    procedure SetnewHotkey(const Value: TShortcut);
    procedure SetselectedAction(const Value: TTntAction);
    { Private declarations }
  protected
    property newHotkey: TShortcut read fnewHotkey write SetnewHotkey;
    property selectedAction: TTntAction read fselectedAction write
        SetselectedAction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplySettings; override;
    procedure HandleHide; override;
    procedure HandleShow; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

function GetCategory(CatName: String; ATree: TVirtualStringTree): PVirtualNode;

var
  TCEOptionsPage_GlobalHotkeys: TTCEOptionsPage_GlobalHotkeys;

implementation

uses
  CE_TBActions, MPCommonUtilities;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Find Category by it's name.
-------------------------------------------------------------------------------}
function GetCategory(CatName: String; ATree: TVirtualStringTree): PVirtualNode;
var
  data: PCEActTreeData;
  Node: PVirtualNode;
begin
  //Result:= nil;
  Node:= ATree.GetFirst;
  while Node <> nil do
  begin
    data:= ATree.GetNodeData(Node);
    if data.IsCategory then
    begin
      if CompareText(CatName, data.Name) = 0 then
      begin
        break;
      end;
    end;
    Node:= Node.NextSibling;
  end;

  if not assigned(Node) then
  begin
    Node:= ATree.AddChild(nil);
    data:= ATree.GetNodeData(Node);
    data.IsCategory:= true;
    data.Name:= CatName;
    data.IconIndex:= -1;
  end;

  Result:= Node;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_General
-------------------------------------------------------------------------------}
constructor TTCEOptionsPage_GlobalHotkeys.Create(AOwner: TComponent);
var
  i: Integer;
  data: PCEActTreeData;
  act: TTntAction;
  node: PVirtualNode;
begin
  inherited;
  PageName:= _('Global');
  PageTitle:= _('Global Hotkey Settings');
  PagePath:= 'Hotkeys/Global';
  ImageIndex:= 6;

  // Populate HotkeyList
  HotkeyList.NodeDataSize:= SizeOf(ACEActTreeData);
  HotkeyList.Images:= CE_Images.SmallIcons;
  for i:= 0 to CEActions.ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(CEActions.ActionList.Actions[i]);
    // move to next item if action can't execute
    if act is TCEToolbarAction then
      if not TCEToolbarAction(act).CanExecute then
      Continue; // -->
    // Add Action to list
    node:= GetCategory(act.Category, HotkeyList);
    data:= HotkeyList.GetNodeData(HotkeyList.AddChild(node));
    data.ActionItem:= act;
    data.Name:= act.Caption;
    data.IconIndex:= act.ImageIndex;
    data.IsCategory:= false;
    data.Shortcut:= CEActions.GlobalHotkeys.GetHotkey(act);
  end;
  HotkeyList.FullExpand;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TTCEOptionsPage_GlobalHotkeys.Destroy;
begin
  CEActions.GlobalHotkeys.RegisterAll;
  inherited;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.GetImageIndex
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HotkeyListGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PCEActTreeData;
begin
  if Column = 0 then
  begin
    data:= Sender.GetNodeData(Node);
    ImageIndex:= data.IconIndex;
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.GetText
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HotkeyListGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  data: PCEActTreeData;
begin
  data:= Sender.GetNodeData(Node);
  if Column = 0 then
  begin
    if data.IsCategory then
    CellText:= UTF8Decode(data.Name)
    else
    CellText:= data.Name;
  end
  else
  begin
    if not data.IsCategory then
    CellText:= ShortCutToText(data.ShortCut)
    else
    CellText:= '';
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.KeyDown
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HotkeyListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then
  begin
    edit_hotkey.SetFocus;
    edit_hotkeyKeyDown(Sender, Key, Shift);
    Key:= 0;
  end
  else
  begin
    case Key of
      33..40: begin
        // do nothing
      end
      else
      begin
        edit_hotkey.SetFocus;
        edit_hotkeyKeyDown(Sender, Key, Shift);
        Key:= 0;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.PaintText
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HotkeyListPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PCEActTreeData;
begin
  if Column <> 0 then
  Exit;

  data:= HotkeyList.GetNodeData(Node);
  if data.IsCategory then
  TargetCanvas.Font.Style:= [fsBold, fsUnderline]
  else
  TargetCanvas.Font.Style:= [];
end;

{-------------------------------------------------------------------------------
  On list_actionhotkeys.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.list_actionhotkeysClick(
  Sender: TObject);
begin
  newHotkey:= 0;
  if list_actionhotkeys.ItemIndex > -1 then
  begin
    edit_hotkey.Text:= list_actionhotkeys.Items.Strings[list_actionhotkeys.ItemIndex];
    but_delete.Enabled:= true;
  end
  else
  begin
    edit_hotkey.Text:= '';
    but_delete.Enabled:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.FocusChanged
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HotkeyListFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  data: PCEActTreeData;
begin
  if assigned(Node) then
  begin
    data:= HotkeyList.GetNodeData(Node);
    selectedAction:= data.ActionItem;
    if list_actionhotkeys.Count > 0 then
    begin
      list_actionhotkeys.ItemIndex:= 0;
      list_actionhotkeysClick(self);
    end;
  end
  else
  selectedAction:= nil;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.ApplySettings;
begin
  //
end;

{-------------------------------------------------------------------------------
  On but_add.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.but_addClick(Sender: TObject);
var
  data: PCEActTreeData;
  ws, ws2: String;
  index,i: Integer;
begin
  if assigned(selectedAction) and (newHotkey <> 0) then
  begin
    if CEActions.GlobalHotkeys.CanRegister(selectedAction, newHotkey) then
    begin
      // Add new hotkey
      index:= CEActions.GlobalHotkeys.AddHotkey(selectedAction, newHotkey);
      i:= list_actionhotkeys.Items.AddObject(ShortCutToText(newHotkey), TObject(index));
      list_actionhotkeys.ItemIndex:= i;
      list_actionhotkeysClick(Self);
      // Repaint action
      data:= HotkeyList.GetNodeData(HotkeyList.FocusedNode);
      if assigned(data) then
      begin
        data.Shortcut:= CEActions.GlobalHotkeys.GetHotkey(selectedAction);
        HotkeyList.RepaintNode(HotkeyList.FocusedNode);
      end;
    end
    else // hotkey already registered, show warning.
    begin
      ws2:= ShortCutToText(newHotkey) + ' ' + _('is already in use!')+#13+#10+
          _('Please choose different hotkey.');
      ws:= _('Duplicate Hotkey');
      WideMessageBox(Handle, ws , ws2, MB_ICONWARNING or MB_OK);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On but_delete.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.but_deleteClick(Sender: TObject);
var
  data: PCEActTreeData;
begin
  if assigned(selectedAction) then
  begin
    if list_actionhotkeys.ItemIndex > -1 then
    begin
      // Delete hotkey
      CEActions.GlobalHotkeys.DeleteHotkey(Integer(list_actionhotkeys.Items.Objects[list_actionhotkeys.ItemIndex]));
      list_actionhotkeys.Items.Delete(list_actionhotkeys.ItemIndex);
      list_actionhotkeysClick(self);
      // Repaint action
      data:= HotkeyList.GetNodeData(HotkeyList.FocusedNode);
      if assigned(data) then
      begin
        data.Shortcut:= CEActions.GlobalHotkeys.GetHotkey(selectedAction);
        HotkeyList.RepaintNode(HotkeyList.FocusedNode);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On but_replace.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.but_replaceClick(Sender: TObject);
var
  data: PCEActTreeData;
  ws,ws2: String;
  index: Integer;
begin
  if assigned(selectedAction) then
  begin
    if list_actionhotkeys.ItemIndex > -1 then
    begin
      if CEActions.GlobalHotkeys.CanRegister(selectedAction, newHotkey) then
      begin
        // Replace hotkey
        index:= Integer(list_actionhotkeys.Items.Objects[list_actionhotkeys.ItemIndex]);
        CEActions.GlobalHotkeys.Replace(index, selectedAction, newHotkey);
        list_actionhotkeys.Items.Strings[list_actionhotkeys.ItemIndex]:= ShortCutToText(newHotkey);
        list_actionhotkeysClick(Self);
        // Repaint action
        data:= HotkeyList.GetNodeData(HotkeyList.FocusedNode);
        if assigned(data) then
        begin
          data.Shortcut:= CEActions.GlobalHotkeys.GetHotkey(selectedAction);
          HotkeyList.RepaintNode(HotkeyList.FocusedNode);
        end;
      end
      else // hotkey already registered, show warning.
      begin
        ws2:= ShortCutToText(newHotkey) + ' ' + _('is already in use!')+#13+#10+
            _('Please choose different hotkey.');
        ws:= _('Duplicate Hotkey');
        WideMessageBox(Handle, ws , ws2, MB_ICONWARNING or MB_OK);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.RefreshSettings;
begin
  //
end;

{-------------------------------------------------------------------------------
  On edit_hotkey.KeyDown
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.edit_hotkeyKeyDown(Sender: TObject; var
    Key: Word; Shift: TShiftState);
begin
  newHotkey:= ShortCut(Key, Shift);
  Key:= 0;
  edit_hotkey.Text:= ShortCutToTextRaw(newHotkey);

  // Alt causes buttons not to draw for some reason in Windows 7.
  // This is a cheap fix.
  if ssAlt in Shift then
  begin
    but_add.Repaint;
    but_replace.Repaint;
    but_delete.Repaint;
    CEOptionsDialog.but_ok.Repaint;
    CEOptionsDialog.but_cancel.Repaint;
    CEOptionsDialog.but_apply.Repaint;
  end;
end;

{-------------------------------------------------------------------------------
  On edit_hotkey.KeyPress
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.edit_hotkeyKeyPress(Sender: TObject;
    var Key: Char);
begin
  Key:= #0;
end;

{-------------------------------------------------------------------------------
  Handle Hide
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HandleHide;
begin
  CEActions.ActionList.State:= asNormal;
  CEActions.GlobalHotkeys.RegisterAll;
end;

{-------------------------------------------------------------------------------
  Handle Show
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.HandleShow;
begin
  CEActions.ActionList.State:= asSuspended;
  CEActions.GlobalHotkeys.UnRegisterAll;
end;

{-------------------------------------------------------------------------------
  Set newHotkey
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.SetnewHotkey(const Value: TShortcut);
begin
  fnewHotkey:= Value;
  case WordRec(fnewHotkey).Lo of
    0, $10..$12: begin
      but_add.Enabled:= false;
      but_replace.Enabled:= false;
    end
    else
    begin
      but_add.Enabled:= assigned(selectedAction);
      but_replace.Enabled:= list_actionhotkeys.ItemIndex > -1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set selectedAction
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_GlobalHotkeys.SetselectedAction(const Value:
    TTntAction);
begin
  fselectedAction:= Value;
  list_actionhotkeys.Clear;
  if assigned(fselectedAction) then
  begin
    but_add.Enabled:= false;
    but_replace.Enabled:= false;
    but_delete.Enabled:= false;
    edit_hotkey.Enabled:= true;
    edit_hotkey.Text:= '';
    list_actionhotkeys.Enabled:= true;
    label_action_name.Caption:= fselectedAction.Caption;
    label_action_name.ImageIndex:= fselectedAction.ImageIndex;
    // Get Hotkeys
    CEActions.GlobalHotkeys.GetHotkeys(fselectedAction, list_actionhotkeys.Items);
  end
  else
  begin
    label_action_name.Caption:= '';
    label_action_name.ImageIndex:= -1;
    list_actionhotkeys.Enabled:= false;
    edit_hotkey.Enabled:= false;
    edit_hotkey.Text:= '';
    but_add.Enabled:= false;
    but_replace.Enabled:= false;
    but_delete.Enabled:= false;
  end;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TTCEOptionsPage_GlobalHotkeys);

finalization

end.
