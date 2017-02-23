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
//  The Original Code is CE_LanguageEngine.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_LanguageEngine;

interface

uses
  // CE units
  CE_LanguageUtils,
  // Tnt Control
  TntSysUtils, TntClasses,
  // System Units
  Windows, SysUtils, TypInfo, Classes, Contnrs;

type
  TCE_IgnoreHandler = procedure(obj: TObject; var IsIgnored: Boolean) of object;
  TCE_IgnoreItem = class
    Handler: TCE_IgnoreHandler;
    ItemClass: TClass;
  end;

  TCETranslationItem = class
    Propname: String;
    OldValue: WideString;
    Obj: TObject;
  end;

  TCETranslationItemCollection = class(TComponent)  
  private
    fList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCETranslationItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add: TCETranslationItem;
    procedure ClearItems;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCETranslationItem read GetItems;
  end;

  TCETranslator = class(TObject)
  private
    fIgnoredItems: TObjectList;
    fIncludeInheritedAlso: Boolean;
    fTestMode: Boolean;
    fTranslatedCount: Integer;
    FTranslationCollections: TComponentList;
    fUseIncludeList: Boolean;
    function GetIsTranslated: Boolean;
    procedure SetTestMode(const Value: Boolean);
  protected
    procedure ClearIgnoredItems;
    function IsIgnored(AObject: TObject): Boolean; virtual;
    procedure RememberProperty(Obj: TObject; PropName: String; OldValue:
        WideString; ItemCollection: TCETranslationItemCollection);
  public
    IgnoredProperties: TStringList;
    IncludeClasses: TClassList;
    POFile: TCEPOFile;
    constructor Create;
    destructor Destroy; override;
    function AddTranslationCollection(AComponent: TComponent):
        TCETranslationItemCollection;
    procedure ClearCollections;
    procedure LoadPOFromFile(AFilePath: WideString);
    procedure RegisterIgnoredClass(AClass: TClass);
    procedure RegisterIgnoredClassHandler(AClass: TClass; AHandler:
        TCE_IgnoreHandler);
    procedure ResetToOld(ClearItems: Boolean = true);
    procedure ReTranslateAll(APOFile: TCEPOFile = nil);
    procedure TranslateComponent(AComponent: TComponent; APOFile: TCEPOFile = nil;
        OnlyThis: Boolean = false);
    procedure TranslateProperties(AObject: TObject; APOFile: TCEPOFile;
        ItemCollection: TCETranslationItemCollection);
    property IncludeInheritedAlso: Boolean read fIncludeInheritedAlso write
        fIncludeInheritedAlso;
    property IsTranslated: Boolean read GetIsTranslated;
    property TestMode: Boolean read fTestMode write SetTestMode;
    property TranslatedCount: Integer read fTranslatedCount write fTranslatedCount;
    property UseIncludeList: Boolean read fUseIncludeList write fUseIncludeList;
  end;
  

function gettext(const szMsgId: WideString): WideString;
function _(const szMsgId: WideString): WideString; overload;

function _(const szMsgId: String): WideString; overload;

var
  CEGlobalTranslator: TCETranslator;

implementation

{*------------------------------------------------------------------------------
  gettext from GlobalTranslator
-------------------------------------------------------------------------------}
function gettext(const szMsgId: WideString): WideString;
begin
  Result:= CEGlobalTranslator.POFile.GetText(szMsgId);
end;
// gettext (shorter version)
function _(const szMsgId: WideString): WideString;
begin
  Result:= gettext(szMsgId);
end;

// gettext (shorter version)
function _(const szMsgId: String): WideString;
begin
  Result:= gettext(WideString(szMsgId));
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCETranslator
-------------------------------------------------------------------------------}
constructor TCETranslator.Create;
begin
  inherited;
  fTestMode:= false;
  fIgnoredItems:= TObjectList.Create(true);
  IgnoredProperties:= TStringList.Create;
  IgnoredProperties.Sorted:= true;
  IgnoredProperties.CaseSensitive:= false;
  IgnoredProperties.Duplicates:= dupError;
  IncludeClasses:= TClassList.Create;
  fUseIncludeList:= false;
  fIncludeInheritedAlso:= false;
  FTranslationCollections:= TComponentList.Create(false);
  POFile:= TCEPOFile.Create;
end;

{*------------------------------------------------------------------------------
  Destroy TCETranslator
-------------------------------------------------------------------------------}
destructor TCETranslator.Destroy;
begin
  POFile.Free;
  ClearCollections;
  FTranslationCollections.Free;
  IncludeClasses.Free;
  IgnoredProperties.Free;
  fIgnoredItems.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Add Translation Collection
-------------------------------------------------------------------------------}
function TCETranslator.AddTranslationCollection(AComponent: TComponent):
    TCETranslationItemCollection;
begin
  Result:= TCETranslationItemCollection.Create(AComponent);
  Result.Name:= 'TCETransItemList';
  FTranslationCollections.Add(Result);
end;

{*------------------------------------------------------------------------------
  Clear Collection
-------------------------------------------------------------------------------}
procedure TCETranslator.ClearCollections;
begin
  while FTranslationCollections.Count > 0 do
  begin
    FTranslationCollections.Items[0].Free;
  end;
end;

procedure TCETranslator.ClearIgnoredItems;
begin
  fIgnoredItems.Clear;
end;

{*------------------------------------------------------------------------------
  Get IsTranslated
-------------------------------------------------------------------------------}
function TCETranslator.GetIsTranslated: Boolean;
begin
  Result:= FTranslationCollections.Count > 0;
end;

{*------------------------------------------------------------------------------
  Check if object should be ignored
-------------------------------------------------------------------------------}
function TCETranslator.IsIgnored(AObject: TObject): Boolean;
var
  i: Integer;
  item: TCE_IgnoreItem;
begin
  // Is in include list
  if fUseIncludeList then
  begin
    Result:= true;
    if fIncludeInheritedAlso then
    begin
      for i:= 0 to IncludeClasses.Count - 1 do
      begin
        if AObject is IncludeClasses.Items[i] then
        begin
          Result:= false;
          break;
        end;
      end;
    end
    else
    begin
      Result:= IncludeClasses.IndexOf(AObject.ClassType) = -1;
    end;

    if Result then
    Exit;
  end
  else
  Result:= false;

  for i:= 0 to fIgnoredItems.Count - 1 do
  begin
    item:= TCE_IgnoreItem(fIgnoredItems.Items[i]);
    if item.ItemClass = AObject.ClassType then
    begin
      Result:= false;
      if assigned(item.Handler) then
      item.Handler(AObject, Result);
      break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Load PO From File
-------------------------------------------------------------------------------}
procedure TCETranslator.LoadPOFromFile(AFilePath: WideString);
begin
  POFile.LoadFromFile(AFilePath, false);
  POFile.Sort;
end;

{*------------------------------------------------------------------------------
  Register Ignored Class
-------------------------------------------------------------------------------}
procedure TCETranslator.RegisterIgnoredClass(AClass: TClass);
var
  item: TCE_IgnoreItem;
begin
  item:= TCE_IgnoreItem.Create;
  item.ItemClass:= AClass;
  item.Handler:= nil;
  fIgnoredItems.Add(item);
end;

{*------------------------------------------------------------------------------
  Register Ignored Class
-------------------------------------------------------------------------------}
procedure TCETranslator.RegisterIgnoredClassHandler(AClass: TClass; AHandler:
    TCE_IgnoreHandler);
var
  item: TCE_IgnoreItem;
begin
  item:= TCE_IgnoreItem.Create;
  item.ItemClass:= AClass;
  item.Handler:= AHandler;
  fIgnoredItems.Add(item);
end;

{*------------------------------------------------------------------------------
  Remember property
-------------------------------------------------------------------------------}
procedure TCETranslator.RememberProperty(Obj: TObject; PropName: String;
    OldValue: WideString; ItemCollection: TCETranslationItemCollection);
var
  item: TCETranslationItem;
begin
  if assigned(ItemCollection) then
  begin
    item:= ItemCollection.Add;
    item.Propname:= PropName;
    item.OldValue:= OldValue;
    item.Obj:= Obj;
  end;
end;

{*------------------------------------------------------------------------------
  Reset To Old
-------------------------------------------------------------------------------}
procedure TCETranslator.ResetToOld(ClearItems: Boolean = true);
var
  collection: TCETranslationItemCollection;
  item: TCETranslationItem;
  i,j: Integer;
begin
  for i:= 0 to FTranslationCollections.Count - 1 do
  begin
    collection:= TCETranslationItemCollection(FTranslationCollections.Items[i]);
    for j:= 0 to collection.Count - 1 do
    begin
      item:= collection.Items[j];
      if assigned(item.Obj) then
      SetWideStrProp(item.Obj, item.Propname, item.OldValue);
    end;
  end;
  POFile.Clear;
  if ClearItems then
  ClearCollections;
end;

{*------------------------------------------------------------------------------
  Re Translate All
-------------------------------------------------------------------------------}
procedure TCETranslator.ReTranslateAll(APOFile: TCEPOFile = nil);
var
  po: TCEPOFile;
  collection: TCETranslationItemCollection;
  item: TCETranslationItem;
  i,j: Integer;
begin
  if assigned(APOFile) then
  po:= APOFile
  else
  po:= POFile;

  for i:= 0 to FTranslationCollections.Count - 1 do
  begin
    collection:= TCETranslationItemCollection(FTranslationCollections.Items[i]);
    for j:= 0 to collection.Count - 1 do
    begin
      item:= collection.Items[j];
      if assigned(item.Obj) then
      begin
        SetWideStrProp(item.Obj, item.Propname, po.gettext(item.OldValue));
        fTranslatedCount:= fTranslatedCount + 1;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set TestMode
-------------------------------------------------------------------------------}
procedure TCETranslator.SetTestMode(const Value: Boolean);
begin
  fTestMode:= Value;
  PoFile.TestMode:= Value;
end;

{*------------------------------------------------------------------------------
  Translate Component
-------------------------------------------------------------------------------}
procedure TCETranslator.TranslateComponent(AComponent: TComponent; APOFile:
    TCEPOFile = nil; OnlyThis: Boolean = false);
var
  i: Integer;
  collection: TCETranslationItemCollection;
  po: TCEPOFile;
begin

  if not assigned(AComponent) then
  Exit;

  if AComponent.FindComponent('TCETransItemList') <> nil then
  Exit;

  if APOFile = nil then
  po:= POFile
  else
  po:= APOFile;

  if not OnlyThis then
  begin
    for i:= 0 to AComponent.ComponentCount - 1 do
    TranslateComponent(AComponent.Components[i], po);
  end;
  collection:= AddTranslationCollection(AComponent);
  TranslateProperties(AComponent, po, collection);
end;

{*------------------------------------------------------------------------------
  Translate Properties
-------------------------------------------------------------------------------}
procedure TCETranslator.TranslateProperties(AObject: TObject; APOFile:
    TCEPOFile; ItemCollection: TCETranslationItemCollection);
var
  i, Count: integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropName: string;
  old: WideString;
  ws: WideString;
  ppi:PPropInfo;
  obj:TObject;
  remember: Boolean;
  po: TCEPOFile;
  tmp: Integer;
begin
  if not assigned(APOFile) then
  Exit;
  if not assigned(AObject) then
  Exit;

  // Is Ignored class?
  if IsIgnored(AObject) then
  Exit;
  
  if APOFile = nil then
  po:= POFile
  else
  po:= APOFile;

  remember:= assigned(ItemCollection);

  Count:= GetPropList(AObject, PropList);
  try
    for i:= 0 to Count - 1 do
    begin
      PropInfo:= PropList[i];
      if (PropInfo^.PropType^.Kind in [tkString, tkLString, tkWString, tkClass]) then
      begin
        PropName:= PropInfo^.Name;
        if IgnoredProperties.Find(PropName, tmp) then
        continue;

        // Ignore the Name
        if PropName = 'Name' then
        continue;

        // class property
        if PropInfo^.PropType^.Kind = tkClass then 
        begin
          obj:= GetObjectProp(AObject, PropName);
          if obj <> nil then
          begin
            TranslateProperties(obj, po, ItemCollection);
          end;
        end
        // strings
        else 
        begin
          if PropInfo^.PropType^.Kind <> tkWString then
          old:= GetStrProp(AObject, PropName)
          else
          old:= GetWideStrProp(AObject, PropName);

          fTranslatedCount:= fTranslatedCount + 1;

          if (old <> '') and (Assigned(PropInfo) and (PropInfo^.SetProc <> nil)) then
          begin
            ws:= po.GetText(old);
            if ws <> old then
            begin
              ppi:=GetPropInfo(AObject, Propname);
              if ppi <> nil then
              begin
                if remember then
                RememberProperty(AObject, Propname, old, ItemCollection);
                if PropInfo^.PropType^.Kind <> tkWString then
                SetStrProp(AObject, ppi, UTF8Encode(ws))
                else
                SetWideStrProp(AObject, ppi, ws);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    if Count <> 0 then
    FreeMem (PropList);
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCETranslationItemCollection
-------------------------------------------------------------------------------}
constructor TCETranslationItemCollection.Create(AOwner: TComponent);
begin
  inherited;
  fList:= TList.Create;
end;

{*------------------------------------------------------------------------------
  Destroy TCETranslationItemCollection
-------------------------------------------------------------------------------}
destructor TCETranslationItemCollection.Destroy;
begin
  ClearItems;
  fList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Add new TCETranslationItem
-------------------------------------------------------------------------------}
function TCETranslationItemCollection.Add: TCETranslationItem;
begin
  Result:= TCETranslationItem.Create;
  fList.Add(Result);
end;

{*------------------------------------------------------------------------------
  Clear items
-------------------------------------------------------------------------------}
procedure TCETranslationItemCollection.ClearItems;
var
  i: Integer;
begin
  for i:= 0 to fList.Count - 1 do
  begin
    TObject(fList.Items[i]).Free;
  end;
  fList.Clear;
end;

{*------------------------------------------------------------------------------
  Get count
-------------------------------------------------------------------------------}
function TCETranslationItemCollection.GetCount: Integer;
begin
  Result:= fList.Count;
end;

{*------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCETranslationItemCollection.GetItems(Index: Integer):
    TCETranslationItem;
begin
  Result:= TCETranslationItem(fList.Items[Index]);
end;

{##############################################################################}

initialization
  CEGlobalTranslator:= TCETranslator.Create;

finalization
  FreeAndNil(CEGlobalTranslator);

end.
