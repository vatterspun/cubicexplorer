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
//  The Original Code is CE_CommonObjects.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_CommonObjects;

interface

uses
  // VSTools
  MPShellUtilities, MPCommonObjects,
  // System Units
  ShlObj, ShellAPI, Windows, Contnrs, Classes, SysUtils, Controls, Messages;

type
  TCESpecialNamespaces = class(TObject)  
  private
    fPIDLList: TList;
  protected
    procedure ClearList;
    procedure PopulateList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSpecialID(APIDL: PItemIDList): Integer;
  end;

  TCERecycleBinCtrl = class(TObject)
  private
    fConfirmRestore: Boolean;
    fDayLimit: Integer;
    fIsEmptyCache: Boolean;
    fItemNumberLimit: Integer;
    fLastIsEmptyCheck: Integer;
    fQuedIsEmptyCheck: Integer;
    fSortColumn: Integer;
    fTotalItemCount: Integer;
    function GetIsRecycleBinEmpty: Boolean;
  protected
    function ConfirmMultipleRestore(ItemNumber: Integer): Boolean;
  public
    fItems: TVirtualNameSpaceList;
    RecycleBinNS: TNamespace;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetLocation(NS: TNamespace): WideString;
    function CheckRecycleBinEmpty: Boolean;
    procedure QueIsEmptyCheck;
    procedure RefreshList;
    procedure Restore(NS: TNamespace);
    procedure RestoreAll;
    procedure RestoreLastDeleted;
    procedure RestoreList;
    property ConfirmRestore: Boolean read fConfirmRestore write fConfirmRestore;
    property DayLimit: Integer read fDayLimit write fDayLimit;
    property IsRecycleBinEmpty: Boolean read GetIsRecycleBinEmpty;
    property ItemNumberLimit: Integer read fItemNumberLimit write fItemNumberLimit;
    property Items: TVirtualNameSpaceList read fItems;
    property SortColumn: Integer read fSortColumn write fSortColumn;
    property TotalItemCount: Integer read fTotalItemCount;
  end;

function CERecycleBinCtrl: TCERecycleBinCtrl;

var
  CE_SpecialNamespaces: TCESpecialNamespaces;

implementation

uses
  MPShellTypes, Variants, CE_Utils, Forms, MPCommonUtilities, TntSysUtils,
  CE_LanguageEngine, DateUtils;

var
  fCERecycleBinCtrl: TCERecycleBinCtrl = nil;

{-------------------------------------------------------------------------------
  CERecycleBinCtrl
-------------------------------------------------------------------------------}
function CERecycleBinCtrl: TCERecycleBinCtrl;
begin
  if not assigned(fCERecycleBinCtrl) then
  fCERecycleBinCtrl:= TCERecycleBinCtrl.Create;
  Result:= fCERecycleBinCtrl;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
   Create an instance of TCESpecialNamespaces
-------------------------------------------------------------------------------}
constructor TCESpecialNamespaces.Create;
begin
  inherited;
  fPIDLList:= TList.Create;
  PopulateList;
end;

{*------------------------------------------------------------------------------
  Destroy TCESpecialNamespaces
-------------------------------------------------------------------------------}
destructor TCESpecialNamespaces.Destroy;
begin
  ClearList;
  fPIDLList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Populate list with Special Folder PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.PopulateList;
var
  PIDL: PItemIDList;
  FolderID: Integer;
begin
  for FolderID:= 0 to 63 do
  begin
    SHGetspecialFolderLocation(0, FolderID, PIDL);
    fPIDLList.Add(PIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Clear list and free PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.ClearList;
var
  i: Integer;
begin
  for i:= 0 to fPIDLList.Count-1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    PidlMgr.FreePIDL(fPIDLList.Items[i]);
  end;
  fPIDLList.Clear;
end;

{*------------------------------------------------------------------------------
  Get Special Folder's ID. If not found, -1 is returned.
-------------------------------------------------------------------------------}
function TCESpecialNamespaces.GetSpecialID(APIDL: PItemIDList): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if not assigned(APIDL) then
  Exit;
  
  for i:= 0 to fPIDLList.Count - 1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    begin
      if ILIsEqual(APIDL, fPIDLList.Items[i]) then
      begin
        Result:= i;
        break;
      end;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCERecycleBinCtrl
-------------------------------------------------------------------------------}
constructor TCERecycleBinCtrl.Create;
begin
  inherited Create;
  fIsEmptyCache:= true;
  fLastIsEmptyCheck:= 0;
  fQuedIsEmptyCheck:= GetTickCount;
  fItemNumberLimit:= 20;
  fConfirmRestore:= true;
  fSortColumn:= 2;
  fTotalItemCount:= 0;
  fDayLimit:= -1;
  RecycleBinNS:= CreateSpecialNamespace(CSIDL_BITBUCKET);
  fItems:= TVirtualNameSpaceList.Create(true);
end;

{-------------------------------------------------------------------------------
  Destroy TCERecycleBinCtrl
-------------------------------------------------------------------------------}
destructor TCERecycleBinCtrl.Destroy;
begin
  Clear;
  fItems.Free;

  if assigned(RecycleBinNS) then
  RecycleBinNS.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.Clear;
begin
  fItems.Clear;
end;

{-------------------------------------------------------------------------------
  Get Location
-------------------------------------------------------------------------------}
function TCERecycleBinCtrl.GetLocation(NS: TNamespace): WideString;
var
  pscid: TSHColumnID;
  v: OleVariant;
begin
  Result:= '';
  if assigned(NS) and assigned(RecycleBinNS.ShellFolder2) then
  begin
    pscid.fmtid:= FMTID_Displaced;
    pscid.pid:= PID_DISPLACED_FROM;
    if RecycleBinNS.ShellFolder2.GetDetailsEx(NS.RelativePIDL, pscid, v) = S_OK then
    begin
      try
        Result:= VarToWideStr(v);
      except
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Is RecycleBin Empty
-------------------------------------------------------------------------------}
function TCERecycleBinCtrl.CheckRecycleBinEmpty: Boolean;
var
  enum: IEnumIDList;
  pidl: PItemIDList;
  fetched: Cardinal;
begin
  Result:= true;
  if assigned(RecycleBinNS) and assigned(RecycleBinNS.ShellFolder) then
  begin
    if RecycleBinNS.ShellFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, enum) = S_OK then
    begin
      try
        if enum.Next(1, pidl, fetched) = S_OK then
        begin
          PIDLMgr.FreePIDL(pidl);
          Result:= false;
        end;
      finally
        enum:= nil;
      end;
    end;
  end;
end;

function TCERecycleBinCtrl.ConfirmMultipleRestore(ItemNumber: Integer): Boolean;
var
  ws: WideString;
begin
  ws:= Format(_('WARNING! You are about to restore %d items.'), [ItemNumber]) + #13#10#13#10;
  ws:= ws + _('Are you sure you want to do that?');
  Result:= WideMessageBox(Application.MainFormHandle, _('Restoring multiple items!'), ws, MB_ICONWARNING or MB_YESNO) = idYes;
end;

{-------------------------------------------------------------------------------
  Get IsRecycleBinEmpty
-------------------------------------------------------------------------------}
function TCERecycleBinCtrl.GetIsRecycleBinEmpty: Boolean;
begin
  if assigned(RecycleBinNS) then
  begin
    if fQuedIsEmptyCheck > fLastIsEmptyCheck then
    begin
      fIsEmptyCache:= CheckRecycleBinEmpty;
      fLastIsEmptyCheck:= GetTickCount;
    end;
    Result:= fIsEmptyCache;
  end
  else
  Result:= false;

//      // TODO, might not work in all systems
//      Result:= CERecycleBinCtrl.RecycleBinNS.GetIconIndex(false, icSmall) = 31;
end;

{-------------------------------------------------------------------------------
  Que IsEmpty Check
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.QueIsEmptyCheck;
begin
  fQuedIsEmptyCheck:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  Refresh List
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.RefreshList;

  function GMTToLocalTime(GMTTime: TDateTime): TDateTime;
  var
    GMTST: Windows.TSystemTime;
    LocalST: Windows.TSystemTime;
  begin
     SysUtils.DateTimeToSystemTime(GMTTime, GMTST);
     SysUtils.Win32Check(Windows.SystemTimeToTzSpecificLocalTime(nil, GMTST, LocalST));
     Result:= SysUtils.SystemTimeToDateTime(LocalST);
  end;

  function FindSortIndex(NS: TNamespace): Integer;
  begin
    for Result:= 0 to fItems.Count - 1 do
    begin
      if PDateTime(NS.Tag)^ > PDateTime(fItems.Items[Result].Tag)^ then
      Exit
      else if PDateTime(NS.Tag)^ = PDateTime(fItems.Items[Result].Tag)^ then
      begin
        if WideCompareText(NS.NameInFolder, fItems.Items[Result].NameInFolder) < 0  then
        Exit;
      end;
    end;
    Result:= -1;
  end;
    
var
  itemNS: TNamespace;
  enum: IEnumIDList;
  pidl: PItemIDList;
  flags, fetched: Cardinal;
  ws: WideString;
  d, limit: TDateTime;
  index: Integer;
  itemDate: PDateTime;
  pscid: TSHColumnID;
  v: OleVariant;
begin
  if not assigned(RecycleBinNS) or (fItemNumberLimit = 0) then
  Exit;

  if fItemNumberLimit < -1 then
  fItemNumberLimit:= -1;

  Clear;

  if assigned(RecycleBinNS.ShellFolder) then
  begin
    flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
    if RecycleBinNS.ShellFolder.EnumObjects(0, flags, enum) = S_OK then
    begin
      try
        fLastIsEmptyCheck:= GetTickCount;
        fIsEmptyCache:= true;
        fTotalItemCount:= 0;
        limit:= Now - DayLimit;
        ////////////////////////////////////////////////////////////////////
        // Enumerate child fItems
        while enum.Next(1, pidl, fetched) = S_OK do
        begin
          fTotalItemCount:= fTotalItemCount + 1;
          fIsEmptyCache:= false;
          // Create item
          itemNS:= TNamespace.Create(pidl, RecycleBinNS);

          // Allocate memory for sort value
          New(itemDate);
          itemNS.Tag:= Integer(itemDate);

          // Get sort value
          d:= 0;
          try
            // 1. Get date value (using GetDetailsEx)
            if assigned(RecycleBinNS.ShellFolder2) then
            begin
              pscid.fmtid:= FMTID_Displaced;
              pscid.pid:= PID_DISPLACED_DATE;
              if RecycleBinNS.ShellFolder2.GetDetailsEx(itemNS.RelativePIDL, pscid, v) = S_OK then
              begin
                try
                  // Returned date seems to be in GMT time. Not sure if that's the case in all systems!
                  d:= GMTToLocalTime(VarToDateTime(v));
                except
                  try
                    ws:= v;
                    d:= GMTToLocalTime(StrToDateTime(CleanDateTimeStr(ws)));
                  except
                  end;
                end;
              end;
            end;
            // 2. Fallback to less accurate date value (using DetailsOf)
            if d = 0 then
            begin
              ws:= itemNS.DetailsOf(fSortColumn);
              d:= StrToDateTime(CleanDateTimeStr(ws));
            end;
          except
            d:= 0;
          end;
          PDateTime(itemNS.Tag)^:= d;

          // Add item to list if it's within day limit.
          if not ((DayLimit > 0) and (d < limit)) then
          begin
            // Find sort index
            if d > 0 then
            index:= FindSortIndex(itemNS)
            else
            index:= -1;

            // Add item to list
            if index > -1 then
            fItems.Insert(index, itemNS)
            else
            fItems.Add(itemNS);

            // Delete too many fItems
            if fItemNumberLimit > -1 then
            begin
              while fItems.Count > fItemNumberLimit do
              begin
                Dispose(PDateTime(fItems.Items[fItems.Count - 1].Tag));
                fItems.Delete(fItems.Count - 1);
              end;
            end;
          end
          else
          begin
            Dispose(PDateTime(itemNS.Tag));
            itemNS.Free;
          end;
        end;
        ////////////////////////////////////////////////////////////////////
      finally
        // Free sort values
        for index:= 0 to fItems.Count - 1 do
        Dispose(PDateTime(fItems.Items[index].Tag));
        // Free enumerator
        enum:= nil;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Restore
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.Restore(NS: TNamespace);
var
  ws: WideString;
begin
  if assigned(NS) then
  begin
    if ConfirmRestore then
    begin
      ws:= CERecycleBinCtrl.GetLocation(NS);
      if ws <> '' then
      ws:= WideIncludeTrailingPathDelimiter(ws) + NS.NameInFolder
      else
      ws:= NS.NameInFolder;
      ws:= _('Are you sure you want to restore?') + #13#10#13#10 + ws;

      if WideMessageBox(Application.MainFormHandle, _('Restore'), ws, MB_ICONQUESTION or MB_YESNO) = idYes then
      NS.ExecuteContextMenuVerb(Application.MainForm, 'undelete', nil);
    end
    else
    begin
      NS.ExecuteContextMenuVerb(Application.MainForm, 'undelete', nil);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Restore All
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.RestoreAll;
var
  pidlArray: TRelativePIDLArray;
  enum: IEnumIDList;
  pidl: PItemIDList;
  i: Integer;
  fetched: Cardinal;
  ns: TNamespace;
  list: TVirtualNameSpaceList;
begin
  if assigned(RecycleBinNS) and assigned(RecycleBinNS.ShellFolder) and ConfirmMultipleRestore(TotalItemCount) then
  begin
    if RecycleBinNS.ShellFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, enum) = S_OK then
    begin
      list:= TVirtualNameSpaceList.Create(true);
      try
        // Get namespace list
        while enum.Next(1, pidl, fetched) = S_OK do
        begin
          ns:= TNamespace.Create(pidl, RecycleBinNS);
          list.Add(ns);
        end;

        // Restore items
        if list.Count > 0 then
        begin
          SetLength(pidlArray, list.Count);
          for i:= 0 to list.Count - 1 do
          begin
            pidlArray[i]:= list.Items[i].RelativePIDL;
          end;
          list.Items[0].ExecuteContextMenuVerb(Application.MainForm, 'undelete', pidlArray);
        end;
      finally
        list.Free;
        Clear;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Restore Last Deleted
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.RestoreLastDeleted;
begin
  RefreshList;
  if fItems.Count > 0 then
  begin
    Restore(fItems.Items[0]);
  end;
  Clear;
end;

{-------------------------------------------------------------------------------
  Restore List
-------------------------------------------------------------------------------}
procedure TCERecycleBinCtrl.RestoreList;
var
  pidlArray: TRelativePIDLArray;
  i: Integer;
begin
  if (Items.Count > 0) and ConfirmMultipleRestore(Items.Count) then
  begin
    SetLength(pidlArray, Items.Count);
    for i:= 0 to Items.Count - 1 do
    begin
      pidlArray[i]:= Items.Items[i].RelativePIDL;
    end;
    try
      Items.Items[0].ExecuteContextMenuVerb(Application.MainForm, 'undelete', pidlArray);
    finally
      Clear;
    end;
  end;
end;

{##############################################################################}

initialization
  CE_SpecialNamespaces:= TCESpecialNamespaces.Create;

finalization
  if assigned(fCERecycleBinCtrl) then
  FreeAndNil(fCERecycleBinCtrl);
  FreeAndNil(CE_SpecialNamespaces);

end.
