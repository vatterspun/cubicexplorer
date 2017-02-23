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
//  The Original Code is fCE_OptionsPage_Hotkeys.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Hotkeys;

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
  end;
  
  TTCEOptionsPage_Hotkeys = class(TCEOptionsCustomPage)
    HotkeyList: TVirtualStringTree;
    list_actionhotkeys: TListBox;
    label_action_name: TSpTBXLabel;
    TntLabel1: TTntLabel;
    edit_hotkey: TTntEdit;
    TntBevel1: TTntBevel;
    TntGroupBox1: TTntGroupBox;
    but_add: TTntButton;
    but_replace: TTntButton;
    but_delete: TTntButton;
    but_reset: TTntButton;
    procedure HotkeyListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure HotkeyListGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure HotkeyListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure HotkeyListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure list_actionhotkeysClick(Sender: TObject);
    procedure edit_hotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_hotkeyKeyPress(Sender: TObject; var Key: Char);
    procedure but_addClick(Sender: TObject);
    procedure but_replaceClick(Sender: TObject);
    procedure but_deleteClick(Sender: TObject);
    procedure HotkeyListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure but_resetClick(Sender: TObject);
    procedure list_actionhotkeysMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure list_actionhotkeysDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure list_actionhotkeysDragDrop(Sender, Source: TObject; X,
      Y: Integer);
  private
    flist_mousedown: TPoint;
    fnewHotkey: TShortcut;
    fselectedAction: TTntAction;
    procedure SetnewHotkey(const Value: TShortcut);
    procedure SetselectedAction(const Value: TTntAction);
    { Private declarations }
  protected
    function CheckDuplicateHotkey(AShortcut: TShortcut; AShowWarning: Boolean):
        Boolean;
    procedure UpdateActionShortcuts;
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
  TCEOptionsPage_Hotkeys: TTCEOptionsPage_Hotkeys;

implementation

uses
  Main, CE_TBActions, MPCommonUtilities;

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
constructor TTCEOptionsPage_Hotkeys.Create(AOwner: TComponent);
var
  i: Integer;
  act: TTntAction;
  node, chNode: PVirtualNode;
  data: PCEActTreeData;
begin
  inherited;
  PageName:= _('Hotkeys');
  PageTitle:= _('Hotkey Settings');
  PagePath:= 'Hotkeys';
  ImageIndex:= 4;
  PageListPosition:= 3;

  HotkeyList.NodeDataSize:= SizeOf(ACEActTreeData);
  label_action_name.Images:= CE_Images.SmallIcons;

  // Populate HotkeyList
  HotkeyList.Images:= CE_Images.SmallIcons;
  HotkeyList.Clear;
  for i:= 0 to CEActions.ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(CEActions.ActionList.Actions[i]);
    // move to next item if action can't execute
    if act is TCEToolbarAction then
      if not TCEToolbarAction(act).CanExecute then
      Continue; // -->
    // Add Action to list
    node:= GetCategory(act.Category, HotkeyList);
    chNode:= HotkeyList.AddChild(node);
    data:= HotkeyList.GetNodeData(chNode);
    data.ActionItem:= act;
    data.Name:= act.Caption;
    data.IconIndex:= act.ImageIndex;
    data.IsCategory:= false;
  end;
  HotkeyList.FullExpand;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TTCEOptionsPage_Hotkeys.Destroy;
begin
  CEActions.ActionList.State:= asNormal;
  inherited;
end;

{-------------------------------------------------------------------------------
  On edit_hotkey.KeyDown
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.edit_hotkeyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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
procedure TTCEOptionsPage_Hotkeys.edit_hotkeyKeyPress(Sender: TObject;
  var Key: Char);
begin
  Key:= #0;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.ApplySettings;
begin
  //
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.RefreshSettings;
begin
  //
end;

{-------------------------------------------------------------------------------
  On HotkeyList.GetImageIndexEx
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HotkeyListGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var
  data: PCEActTreeData;
begin
  if Column = 0 then
  begin
    data:= HotkeyList.GetNodeData(Node);
    ImageIndex:= data.IconIndex;
    if not data.IsCategory then
    ImageList:= CE_Images.SmallIcons
    else
    ImageList:= HotkeyList.Images;
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.GetText
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HotkeyListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  data: PCEActTreeData;
begin
  data:= HotkeyList.GetNodeData(Node);
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
    CellText:= ShortCutToText(data.ActionItem.ShortCut)
    else
    CellText:= '';
  end;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.KeyDown
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HotkeyListKeyDown(Sender: TObject;
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
procedure TTCEOptionsPage_Hotkeys.HotkeyListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
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
procedure TTCEOptionsPage_Hotkeys.list_actionhotkeysClick(Sender: TObject);
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
  On list_actionhotkeys.DragDrop
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.list_actionhotkeysDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  DropPoint.X:= X;
  DropPoint.Y:= Y;
  StartPosition:= TListBox(Source).ItemAtPos(flist_mousedown,True);
  DropPosition:= TListBox(Source).ItemAtPos(DropPoint,True);
  if DropPosition = -1 then
  DropPosition:= TListBox(Source).Items.Count - 1;
  TListBox(Source).Items.Move(StartPosition, DropPosition);
  TListBox(Source).ItemIndex:= DropPosition;
  UpdateActionShortcuts;
end;

{-------------------------------------------------------------------------------
  On list_actionhotkeys.DragOver
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.list_actionhotkeysDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:= Source = list_actionhotkeys;
end;

{-------------------------------------------------------------------------------
  On list_actionhotkeys.MouseDown
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.list_actionhotkeysMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  flist_mousedown.X:= X;
  flist_mousedown.Y:= Y;
end;

{-------------------------------------------------------------------------------
  On HotkeyList.FocusChanged
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HotkeyListFocusChanged(
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
  Set newHotkey
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.SetnewHotkey(const Value: TShortcut);
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
procedure TTCEOptionsPage_Hotkeys.SetselectedAction(const Value: TTntAction);
var
  i: Integer;
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
    but_reset.Enabled:= CEActions.HotkeySettings.ModifiedActions.IndexOf(fselectedAction) > -1;

    // Get shortcut list
    if fselectedAction.ShortCut <> 0 then
    list_actionhotkeys.Items.AddObject(ShortCutToText(fselectedAction.ShortCut), TObject(fselectedAction.ShortCut));

    for i:= 0 to fselectedAction.SecondaryShortCuts.Count - 1 do
    begin
      list_actionhotkeys.Items.AddObject(ShortCutToText(fselectedAction.SecondaryShortCuts.ShortCuts[i]),
                                         TObject(fselectedAction.SecondaryShortCuts.ShortCuts[i]));
    end;
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
    but_reset.Enabled:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On but_add.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.but_addClick(Sender: TObject);
begin
  if CheckDuplicateHotkey(newHotkey, true) then
  begin
    list_actionhotkeys.ItemIndex:= list_actionhotkeys.Items.AddObject(ShortCutToText(newHotkey),
                                                                      TObject(newHotkey));
    UpdateActionShortcuts;
    list_actionhotkeysClick(Self);
  end;
end;

{-------------------------------------------------------------------------------
  On but_replace.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.but_replaceClick(Sender: TObject);
begin
  if (list_actionhotkeys.ItemIndex > -1) and CheckDuplicateHotkey(newHotkey, true) then
  begin
    list_actionhotkeys.Items.Strings[list_actionhotkeys.ItemIndex]:= ShortCutToText(newHotkey);
    list_actionhotkeys.Items.Objects[list_actionhotkeys.ItemIndex]:= TObject(newHotkey);
    UpdateActionShortcuts;
    list_actionhotkeysClick(Self);
  end;
end;

{-------------------------------------------------------------------------------
  On but_reset.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.but_resetClick(Sender: TObject);
var
  ws, ws2: WideString;
begin
  if assigned(selectedAction) then
  begin
    ws2:= _('Do you want to reset all hotkeys for this action?')
        +#13+#10+''+#13+#10+
        _('Default hotkeys become available after restart!');

    ws:= _('Reset hotkeys?');
    if WideMessageBox(Self.Handle, ws, ws2, MB_ICONQUESTION or MB_YESNO) = ID_YES then
    begin
      CEActions.HotkeySettings.ModifiedActions.Remove(selectedAction);
      selectedAction.ShortCut:= 0;
      selectedAction.SecondaryShortCuts.Clear;
      list_actionhotkeys.Clear;
      edit_hotkey.Text:= '';
      but_reset.Enabled:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On but_delete.Click
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.but_deleteClick(Sender: TObject);
begin
  if list_actionhotkeys.ItemIndex > -1 then
  begin
    list_actionhotkeys.Items.Delete(list_actionhotkeys.ItemIndex);
    UpdateActionShortcuts;
    list_actionhotkeysClick(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Check Duplicate Hotkey (Return false if found, else true)
-------------------------------------------------------------------------------}
function TTCEOptionsPage_Hotkeys.CheckDuplicateHotkey(AShortcut: TShortcut;
    AShowWarning: Boolean): Boolean;
var
  i: Integer;
  act: TTntAction;
  ws,ws2: String;
begin
  Result:= true;
  for i:= 0 to CEActions.ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(CEActions.ActionList.Actions[i]);
    if (act.ShortCut = AShortcut) or (act.SecondaryShortCuts.IndexOfShortCut(AShortcut) > -1) then
    begin
      if AShowWarning then
      begin
        ws2:= ShortCutToText(AShortcut) + ' ' + _('is already assigned to') + ': "' + act.Caption + '"' +
                           #13+#10+_('Do you want to use it anyway?');
        ws:= _('Duplicate Hotkey');
        if WideMessageBox(Self.Handle, ws, ws2, MB_ICONWARNING or MB_YESNO) = idNo then
        Result:= false;
      end
      else
      Result:= false;
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Hide
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HandleHide;
begin
  CEActions.ActionList.State:= asNormal;
end;

{-------------------------------------------------------------------------------
  Handle Show
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.HandleShow;
begin
  CEActions.ActionList.State:= asSuspended;
end;

{-------------------------------------------------------------------------------
  Update Action Shortcuts
-------------------------------------------------------------------------------}
procedure TTCEOptionsPage_Hotkeys.UpdateActionShortcuts;
var
  i: Integer;
begin
  if assigned(selectedAction) then
  begin
    selectedAction.ShortCut:= 0;
    selectedAction.SecondaryShortCuts.Clear;
    if list_actionhotkeys.Count > 0 then
    selectedAction.ShortCut:= TShortcut(list_actionhotkeys.Items.Objects[0]);

    for i:= 1 to list_actionhotkeys.Count - 1 do
    begin
      selectedAction.SecondaryShortCuts.AddObject(list_actionhotkeys.Items.Strings[i],
                                                  list_actionhotkeys.Items.Objects[i]);
    end;

    if CEActions.HotkeySettings.ModifiedActions.IndexOf(fselectedAction) = -1 then
    begin
      CEActions.HotkeySettings.ModifiedActions.Add(selectedAction);
      but_reset.Enabled:= true;
    end;
  end;
  HotkeyList.Repaint;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TTCEOptionsPage_Hotkeys);

finalization

end.
