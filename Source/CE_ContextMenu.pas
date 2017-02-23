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
//  The Original Code is CE_ContextMenu.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ContextMenu;

interface

uses  
  // CE Units
  CE_Classes,
  // VS Tools
  MPShellUtilities, MPDataObject, MPCommonObjects, 
  MPCommonUtilities,
  // Tnt
  TntClasses, TntMenus,
  // SpTBX
  SpTBXItem,
  // System Units
  Classes, Windows, SysUtils, ActiveX, Registry, Messages, Graphics, Menus,
  ShlObj, MPShellTypes, Contnrs, Dialogs;

type
  TCEContextMenuItem = class(TObject)
  public
    IsCustomItem: Boolean;
    Menu: IContextMenu;
    ID: Integer;
    Offset: Integer;
    SubMenu: hMenu;
    NewMenuItem: Boolean;
    CustomItem: TMenuItem;
  end;

  TCEBackContextMenu = class(TCEWndObject)
  private
    fLowerMenuItems: TPopupMenu;
    fUpperMenuItems: TPopupMenu;
    procedure GetGUIDs;
    function GetMenu(ID: Integer): IContextMenu;
    function GetMenuItem(ID: Integer): TCEContextMenuItem;
    function GetMenuItemBySubMenu(SubMenu: hMenu): TCEContextMenuItem;
  protected
    GUIDList: TTntStrings;
    MenuItems: TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddMenuItems(Menu:  hMenu); virtual;
    function ShowMenu(Pos: TPoint; RootFolder: TNamespace; Browser: IShellBrowser =
        nil): Boolean; virtual;
    procedure WindowProc(var Msg : TMessage); override;
    property LowerMenuItems: TPopupMenu read fLowerMenuItems write fLowerMenuItems;
    property UpperMenuItems: TPopupMenu read fUpperMenuItems write fUpperMenuItems;
  end;

const
  CLSID_NEWMENU: TGUID = '{D969A300-E7FF-11d0-A93B-00A0C90F2719}';  

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEBackContextMenu
-------------------------------------------------------------------------------}
constructor TCEBackContextMenu.Create;
begin
  inherited;
  MenuItems:= TObjectList.Create(true);
  GUIDList:= TTntStringList.Create;
  GetGUIDs;
end;

{*------------------------------------------------------------------------------
  Destroy TCEBackContextMenu
-------------------------------------------------------------------------------}
destructor TCEBackContextMenu.Destroy;
begin
  GUIDList.Free;
  MenuItems.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Add custom items into the menu
-------------------------------------------------------------------------------}
procedure TCEBackContextMenu.AddMenuItems(Menu:  hMenu);

  procedure EnumItem(ToMenu: hMenu; Item: TMenuItem; var ItemID: Integer; Upper:
      Boolean);
  var
    i,pos: Integer;
    infoA: TMenuItemInfoA;
    infoW: TMenuItemInfoW;
    chItem: TMenuItem;
    ws: WideString;
    cmItem: TCEContextMenuItem;
  begin
    for i:= 0 to Item.Count-1 do
    begin
      if upper then
      chItem:= Item.Items[(Item.Count-1) - i]
      else
      chItem:= Item.Items[i];

      if chItem.Visible then
      begin
        // Get Caption
        if chItem is TTntMenuItem then
        ws:= TTntMenuItem(chItem).Caption
        else
        ws:= chItem.Caption;
        // Initialize MenuItemInfo
        if IsUnicode then
        begin
          FillChar(infoW, SizeOf(infoW), #0);
          infoW.cbSize:= SizeOf(infoW);
          infoW.fMask:= MIIM_TYPE or MIIM_ID or MIIM_STATE;
          if chItem.Caption = '-' then
          begin
            infoW.fType:= MFT_SEPARATOR
          end
          else
          begin
            if chItem.RadioItem then
            infoW.fType:= MFT_STRING or MFT_RADIOCHECK
            else
            infoW.fType:= MFT_STRING;
            infoW.dwTypeData:= PWideChar(ws);
            infoW.cch:= Length(chItem.Caption) + 1;
            infoW.wID:= ItemID;
            if not chItem.Enabled then
              infoW.fState:= MFS_DISABLED	or MFS_GRAYED;
            if chItem.Checked then
              infoW.fState:= infoW.fState or MFS_CHECKED;
          end;
          if chItem.Count > 0 then
          begin
            infoW.fMask:= infoW.fMask or MIIM_SUBMENU;
            infoW.hSubMenu:= CreatePopupMenu;
          end;
        end
        else
        begin
          FillChar(infoA, SizeOf(infoA), #0);
          infoA.cbSize:= SizeOf(infoA);
          infoA.fMask:= MIIM_TYPE or MIIM_ID or MIIM_STATE;
          if chItem.Caption = '-' then
          begin
            infoA.fType:= MFT_SEPARATOR
          end
          else
          begin
            if chItem.RadioItem then
            infoA.fType:= MFT_STRING or MFT_RADIOCHECK
            else
            infoA.fType:= MFT_STRING;
            infoA.dwTypeData:= PChar(String(ws));
            infoA.cch:= Length(chItem.Caption) + 1;
            infoA.wID:= ItemID;
            if not chItem.Enabled then
              infoA.fState:= MFS_DISABLED	or MFS_GRAYED;
            if chItem.Checked then
              infoA.fState:= infoA.fState or MFS_CHECKED;
          end;
          if chItem.Count > 0 then
          begin
            infoA.fMask:= infoA.fMask or MIIM_SUBMENU;
            infoA.hSubMenu:= CreatePopupMenu;
          end;
        end;

        // Create ContextMenuItem
        cmItem:= TCEContextMenuItem.Create;
        cmItem.IsCustomItem:= true;
        cmItem.Menu:= nil;
        cmItem.ID:= ItemID;
        cmItem.CustomItem:= chItem;
        MenuItems.Add(cmItem);
        // set insert position
        if Upper then
        pos:= 0
        else
        pos:= GetMenuItemCount(ToMenu);
        // Insert to menu
        if IsUnicode then
        InsertMenuItemW(ToMenu, pos, true, infoW)
        else
        InsertMenuItemA(ToMenu, pos, true, infoA);
        // Change item id for next item
        ItemID:= ItemID + 1;
        // Enum child items
        if chItem.Count > 0 then
        begin
          if IsUnicode then
          EnumItem(infoW.hSubMenu, chItem, ItemID, upper)
          else
          EnumItem(infoA.hSubMenu, chItem, ItemID, upper);
        end;
      end;
    end;
  end;
  
var
  id: Integer;
begin
  id:= $7FFF + 1;
  if assigned(UpperMenuItems.OnPopup) then
  UpperMenuItems.OnPopup(self);
  if assigned(LowerMenuItems.OnPopup) then
  LowerMenuItems.OnPopup(self);
  EnumItem(Menu, UpperMenuItems.Items, id, true);
  EnumItem(Menu, LowerMenuItems.Items, id, false);
end;

{*------------------------------------------------------------------------------
  Get Directory Background context menu handlers
-------------------------------------------------------------------------------}
procedure TCEBackContextMenu.GetGUIDs;
var
  reg: TRegistry;
  i: Integer;
  path: string;
  list: TStrings;
begin
  GUIDList.Clear;
  path:= 'Directory\Background\shellex\ContextMenuHandlers';
  reg:= TRegistry.Create;
  list:= TStringList.Create;
  try
    reg.RootKey:= HKEY_CLASSES_ROOT;
    if Reg.OpenKey(path, False) then
    begin
      Reg.GetKeyNames(list);
      Reg.CloseKey;
    end;

    for i:= 0 to list.Count - 1 do
    begin
      if Reg.OpenKey(path + '\' + list[i], False) then
      begin
        GUIDList.Add(Reg.ReadString(''));
        Reg.CloseKey;
      end
    end

  finally
    list.Free;
    Reg.Free;
  end
end;

{*------------------------------------------------------------------------------
  Get menu item by it's ID
-------------------------------------------------------------------------------}
function TCEBackContextMenu.GetMenu(ID: Integer): IContextMenu;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to MenuItems.Count - 1 do
  begin
    if TCEContextMenuItem(MenuItems.Items[i]).ID = ID then
    begin
      Result:= TCEContextMenuItem(MenuItems.Items[i]).Menu;
      break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get menu item by it's ID
-------------------------------------------------------------------------------}
function TCEBackContextMenu.GetMenuItem(ID: Integer): TCEContextMenuItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to MenuItems.Count - 1 do
  begin
    if TCEContextMenuItem(MenuItems.Items[i]).ID = ID then
    begin
      Result:= TCEContextMenuItem(MenuItems.Items[i]);
      break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get menu item by it's submenu
-------------------------------------------------------------------------------}
function TCEBackContextMenu.GetMenuItemBySubMenu(SubMenu: hMenu):
    TCEContextMenuItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to MenuItems.Count - 1 do
  begin
    if TCEContextMenuItem(MenuItems.Items[i]).SubMenu = SubMenu then
    begin
      Result:= TCEContextMenuItem(MenuItems.Items[i]);
      break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle messages
-------------------------------------------------------------------------------}
procedure TCEBackContextMenu.WindowProc(var Msg: TMessage);
var
  cm: IContextMenu;
  cm2: IContextMenu2;
  subMenu: hMenu;
  count, count2: Integer;
  SubMenuIDList: TList;
  info: TMenuItemInfo;
  i: Integer;
  item, newitem: TCEContextMenuItem;
begin
  case Msg.Msg of
    WM_DRAWITEM: begin
                   if TWMDrawItem(Msg).Ctl = 0 then
                   begin
                     cm:= GetMenu(TWMDrawItem(Msg).DrawItemStruct.itemID);
                     if Assigned(cm) then
                     begin
                       if cm.QueryInterface(IContextMenu2, cm2) = S_OK then
                       cm2.HandleMenuMsg(Msg.Msg, Msg.WParam, Msg.LParam);
                     end;
                   end;
                 end;
    WM_INITMENUPOPUP: begin
                        subMenu:= TWMInitMenuPopup(Msg).MenuPopup;
                        count:= GetMenuItemCount(subMenu);
                        item:= GetMenuItemBySubMenu(subMenu);
                        if assigned(item) then
                        cm:= item.Menu;
                        if Assigned(cm) then
                        begin
                          SubMenuIDList:= TList.Create;
                          for i:= 0 to count - 1 do
                          begin
                            FillChar(info, SizeOf(info), #0);
                            info.cbSize := SizeOf(info);
                            info.fMask := MIIM_ID;
                            if GetMenuItemInfo(subMenu, i, true, info) then
                            SubMenuIDList.Add(Pointer(info.wID));
                          end;

                          try
                            if cm.QueryInterface(IContextMenu2, cm2) = S_OK then
                            cm2.HandleMenuMsg(Msg.Msg, Msg.WParam, Msg.LParam);

                            count2:= GetMenuItemCount(subMenu);

                            if count2 > count then
                            begin
                              for i:= 0 to count2 - 1 do
                              begin
                                FillChar(info, SizeOf(info), #0);
                                info.cbSize := SizeOf(info);
                                info.fMask := MIIM_ID;
                                if GetMenuItemInfo(subMenu, i, true, info) then
                                begin
                                  if SubMenuIDList.IndexOf(Pointer(info.wID)) = -1 then
                                  begin
                                    newitem:= TCEContextMenuItem.Create;
                                    newitem.Menu:= cm;
                                    newitem.ID:= info.wID;
                                    newitem.SubMenu:= info.hSubMenu;
                                    newitem.NewMenuItem:= item.NewMenuItem;
                                    newitem.Offset:= item.Offset;
                                    MenuItems.Add(newitem);
                                  end;
                                end;
                              end;
                            end;
                          finally
                            SubMenuIDList.Free;
                          end;
                        end;
                      end;
    WM_MEASUREITEM: begin
                      cm:= GetMenu(TWMMeasureItem(Msg).MeasureItemStruct.itemID);
                      if Assigned(cm) then
                      begin
                        if cm.QueryInterface(IContextMenu2, cm2) = S_OK then
                        cm2.HandleMenuMsg(Msg.Msg, Msg.WParam, Msg.LParam);
                      end;
                    end;
    else
    inherited;
  end;
end;

{*------------------------------------------------------------------------------
  Show the Context menu.
-------------------------------------------------------------------------------}
function TCEBackContextMenu.ShowMenu(Pos: TPoint; RootFolder: TNamespace;
    Browser: IShellBrowser = nil): Boolean;

  procedure RemoveDuplicateSeparators(AMenu: hMenu);
  var
    infoA: TMenuItemInfoA;
    infoW: TMenuItemInfoW;
    b: Boolean;
    i, i2: Integer;
  begin
    // In Windows XP there is a crash in this procedure for some reason.
    // Trying unicode support to see if it helps.
    if IsUnicode then
    begin
      // Remove dublicate separators
      b:= false;
      i:= 0;
      i2:= GetMenuItemCount(AMenu);
      while i < i2 do
      begin
        FillChar(infoW, SizeOf(infoW), #0);
        infoW.cbSize := SizeOf(infoW);
        infoW.fMask := MIIM_TYPE;
        if GetMenuItemInfoW(AMenu, i, true, infoW) then
        begin
          if (infoW.fType = MFT_SEPARATOR) then
          begin
            if b then
            begin
              DeleteMenu(AMenu, i, MF_BYPOSITION);
              i2:= GetMenuItemCount(AMenu);
            end
            else
            begin
              b:= true;
              i:= i + 1;
            end;
          end
          else
          begin
            b:= false;
            i:= i + 1;
          end;
        end
        else
        begin
          i:= i + 1;
          i2:= GetMenuItemCount(AMenu);
        end;
      end;
    end
    else
    begin
      // Remove dublicate separators
      b:= false;
      i:= 0;
      i2:= GetMenuItemCount(AMenu);
      while i < i2 do
      begin
        FillChar(infoA, SizeOf(infoA), #0);
        infoA.cbSize := SizeOf(infoA);
        infoA.fMask := MIIM_TYPE;
        if GetMenuItemInfoA(AMenu, i, true, infoA) then
        begin
          if (infoA.fType = MFT_SEPARATOR) then
          begin
            if b then
            begin
              DeleteMenu(AMenu, i, MF_BYPOSITION);
              i2:= GetMenuItemCount(AMenu);
            end
            else
            begin
              b:= true;
              i:= i + 1;
            end;
          end
          else
          begin
            b:= false;
            i:= i + 1;
          end;
        end
        else
        begin
          i:= i + 1;
          i2:= GetMenuItemCount(AMenu);
        end;
      end;
    end;
  end;

var
  g: TGUID;
  menu: hMenu;
  ResultID: Integer;
  i,i2, id: Integer;
  pv: IUnknown;
  cm: IContextMenu;
  cm2: IContextMenu2;
  cm3: IContextMenu3;
  ShellExtInit: IShellExtInit;
  ObjectWithSite: IObjectWithSite;
  pidl: PItemIDList;
  Flags, PopupFlags: Cardinal;
  firstID, lastID: Integer;
  item: TCEContextMenuItem;
  info: TMenuItemInfo;
  VerbA: String;
  VerbW: WideString;
  VerbP: Pointer;
  InvokeInfo: TCMInvokeCommandInfoEx;
  DataObj: IDataObject;
  ShellIDList: TCommonShellIDList;
  PIDLList: TCommonPIDLList;
  HDrop: TCommonHDrop;
  FileList: TTntStringList;
begin
  Result:= false;
  MenuItems.Clear;
  pidl:= PidlMgr.CopyPIDL(RootFolder.AbsolutePIDL);
  menu:= CreatePopupMenu;
  try
    firstID:= 1000;
    lastID:= $7FFF;
    for i:= 0 to GUIDList.Count - 1 do
    begin
      if CLSIDFromString(PWideChar(GUIDList.Strings[i]), g) = NOERROR then
      begin
        // Create an instance of context menu
        if coCreateInstance(g, nil, CLSCTX_INPROC_SERVER, IContextMenu, pv) = NOERROR then
        begin
          // Create a valid IDataObject
          DataObj := TCommonDataObject.Create;
          ShellIDList := TCommonShellIDList.Create;
          PIDLList := TCommonPIDLList.Create;
          try
            PIDLList.SharePIDLs := False;
            PIDLList.Add(PIDLMgr.StripLastID(PIDLMgr.CopyPIDL(RootFolder.AbsolutePIDL)));
            PIDLList.CopyAdd(RootFolder.RelativePIDL);
            ShellIDList.AssignPIDLs(PIDLList);
            ShellIDList.SaveToDataObject(DataObj)
          finally
            PIDLList.Clear;
            PIDLList.Free;
            ShellIDList.Free
          end;
          FileList := TTntStringList.Create;
          HDrop := TCommonHDrop.Create;
          try
            FileList.Add(RootFolder.NameForParsing);
            HDrop.AssignFilesW(FileList);
            HDrop.SaveToDataObject(DataObj)
          finally
            FileList.Free;
            HDrop.Free
          end;
          // Initialize ShellExt
          ShellExtInit:= pv as IShellExtInit;
          if ShellExtInit.Initialize(pidl,DataObj,0) = NOERROR then
          begin
            // Get ContextMenu interface
            cm:= pv as IContextMenu;
            if cm.QueryInterface(IContextMenu3, cm3) = S_OK then
            cm:= cm3
            else if cm.QueryInterface(IContextMenu2, cm2) = S_OK then
            cm:= cm2;

            if assigned(Browser) and (cm.QueryInterface(IObjectWithSite, ObjectWithSite) = NOERROR) then
            ObjectWithSite.SetSite(Browser);

            Flags:= CMF_NORMAL or CMF_EXPLORE;
            if GetKeyState(VK_SHIFT) and $8000 <> 0 then
            Flags:= Flags or CMF_EXTENDEDVERBS;

            id:= cm.QueryContextMenu(menu, 0, firstID, lastID, Flags);
            if Succeeded(id) then
            begin
              for i2:= firstID to firstID + id do
              begin
                FillChar(info, SizeOf(info), #0);
                info.cbSize := SizeOf(info);
                info.fMask := MIIM_ID or MIIM_SUBMENU;
                if GetMenuItemInfo(Menu, i2, False, info) then
                begin
                  item:= TCEContextMenuItem.Create;
                  item.Menu:= cm;
                  item.ID:= info.wID;
                  item.SubMenu:= info.hSubMenu;
                  item.IsCustomItem:= false;
                  item.NewMenuItem:= IsEqualCLSID(CLSID_NewMenu, g);
                  item.Offset:= firstID;
                  MenuItems.Add(item);
                end;
              end;
              firstID:= firstID + id;
            end;
          end;            
        end;
      end;
    end;

    // Add custom items
    AddMenuItems(menu);

    // Remove dublicate separators
    RemoveDuplicateSeparators(menu);

    // Show popup menu
    PopupFlags:= TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON;
    ResultID:= Integer(TrackPopupMenuEx(menu, PopupFlags, Pos.X, Pos.Y, fHandle, nil));

    // Handle menu click
    if ResultID > 0 then
    begin
      item:= GetMenuItem(ResultID);
      if assigned(item) then
      begin
        if not item.IsCustomItem then
        begin
          if assigned(item.Menu) then
          begin
            cm:= item.Menu;
            if IsUnicode then
            begin
              VerbW:= '';
              SetLength(VerbW, 128);
              VerbP := @VerbW[1];
              Flags := GCS_VERBW
            end else
            begin
              VerbA:= '';
              SetLength(VerbA, 128);
              VerbP := @VerbA[1];
              Flags := GCS_VERBA
            end;

            ResultID:= ResultID - item.Offset;

            if Succeeded(cm.GetCommandString(ResultID, Flags, nil, VerbP, 128)) then
            begin
              if IsUnicode then
              begin
                SetLength(VerbW, lstrlenW(PWideChar(VerbW)));
              end else
              begin
                SetLength(VerbA, lstrlen(PChar(VerbA)));
                VerbW := VerbA
              end;
            end;

            FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);
            with InvokeInfo do
            begin
              { For some reason the lpVerbW won't work }
              lpVerb:= MakeIntResourceA(ResultID);
              if IsUnicode then
              begin
                fMask := CMIC_MASK_UNICODE;
                lpVerbW := MakeIntResourceW(ResultID)
              end;
              // Win95 get confused if size = TCMInvokeCommandInfoEx
              if IsUnicode then
                cbSize := SizeOf(TCMInvokeCommandInfoEx)
              else
                cbSize := SizeOf(TCMInvokeCommandInfo);

              hWnd := fHandle;
              nShow := SW_SHOWNORMAL;
            end;
            cm.InvokeCommand(InvokeInfo);
          end;
          Result:= item.NewMenuItem;
        end
        else
        begin
          item.CustomItem.Click;
        end;
      end;
    end;
        
  finally
    if Assigned(ObjectWithSite) then
    ObjectWithSite.SetSite(nil);
    DestroyMenu(menu);
    PidlMgr.FreePIDL(pidl);
    MenuItems.Clear;
  end;
end;


end.
