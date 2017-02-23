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
//  The Original Code is CE_AppSettings.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_AppSettings;

interface

uses
  // fcl-xml
  XMLRead, XMLWrite, DOM, WideSupport,
  // Tnt
  TntSysUtils,
  // System Units
  TypInfo, Contnrs, Classes, SysUtils, Windows;

type
  TCEAppSettingItem = class(TObject)
  private
    fExcludeProperties: TStrings;
    fIncludeProperties: TStrings;
    fNodeName: string;
    fObjectToSave: TObject;
    fRecursive: Boolean;
    fSaveDefaults: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ExcludeProperties: TStrings read fExcludeProperties;
    property IncludeProperties: TStrings read fIncludeProperties write
        fIncludeProperties;
    property NodeName: string read fNodeName write fNodeName;
    property ObjectToSave: TObject read fObjectToSave write fObjectToSave;
    property Recursive: Boolean read fRecursive write fRecursive;
    property SaveDefaults: Boolean read fSaveDefaults write fSaveDefaults;
  end;

  TCEAppSettings = class(TPersistent)
  private
    fSaveDefaultsAlways: Boolean;
    fSaveDefaultsNever: Boolean;
    fStringBooleans: Boolean;
    fXML: TXMLDocument;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCEAppSettingItem;
    procedure SetSaveDefaultsAlways(const Value: Boolean);
    procedure SetSaveDefaultsNever(const Value: Boolean);                                  
  protected
    fItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TCEAppSettingItem;
    function AddItem(AName: String; AObject: TObject; ARecursive: Boolean = false;
        ASaveDefaults: Boolean = false; AExcludeProperties: String = '';
        AIncludeProperties: String = ''): TCEAppSettingItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    function FindItem(AName: String): TCEAppSettingItem;
    function LoadFromFile(AFilePath: WideString): Boolean; virtual;
    function LoadFromResource(AInstance: Cardinal; AResName: String): Boolean;
        virtual;
    procedure LoadObjectProperties(AObject: TObject; ANode: TDOMNode; ARecursive:
        Boolean = true; AExcludeProperties: TStrings = nil; AIncludeProperties:
        TStrings = nil); virtual;
    procedure LoadProperties; virtual;
    procedure SaveObjectProperties(AObject: TObject; AParentNode: TDOMNode;
        ARecursive: Boolean = true; ASaveDefaultValues: Boolean = false;
        AExcludeProperties: TStrings = nil; AIncludeProperties: TStrings = nil);
        virtual;
    procedure SaveProperties; virtual;
    procedure SaveToFile(AFilePath: WideString); virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCEAppSettingItem read GetItems; default;
    property SaveDefaultsAlways: Boolean read fSaveDefaultsAlways write
        SetSaveDefaultsAlways;
    property SaveDefaultsNever: Boolean read fSaveDefaultsNever write
        SetSaveDefaultsNever;
    property StringBooleans: Boolean read fStringBooleans write fStringBooleans;
    property XML: TXMLDocument read fXML write fXML;
  published
  end;

  TCECustomSettingStorageClass = class of TCECustomSettingStorage;
  TCECustomSettingStorage = class(TPersistent)
  protected
  public
    procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); virtual;
    procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); virtual;
  end;

var
  GlobalAppSettings: TCEAppSettings;  

implementation

const
  NoDefault = $80000000;

{-------------------------------------------------------------------------------
  Create an instance of TCEAppSettings
-------------------------------------------------------------------------------}
constructor TCEAppSettings.Create;
begin
  inherited;
  fItems:= TObjectList.Create(true);
  fXML:= TXMLDocument.Create;
  XML.AppendChild(XML.CreateElement('CubicExplorer'));
  fStringBooleans:= true;
end;

{-------------------------------------------------------------------------------
  Destroy TCEAppSettings
-------------------------------------------------------------------------------}
destructor TCEAppSettings.Destroy;
begin
  fXML.Free;
  fItems.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add
-------------------------------------------------------------------------------}
function TCEAppSettings.Add: TCEAppSettingItem;
begin
  Result:= TCEAppSettingItem.Create;
  fItems.Add(Result);
end;

{-------------------------------------------------------------------------------
  Add Item (AIgnoreProperties is comma separated)
-------------------------------------------------------------------------------}
function TCEAppSettings.AddItem(AName: String; AObject: TObject; ARecursive:
    Boolean = false; ASaveDefaults: Boolean = false; AExcludeProperties: String
    = ''; AIncludeProperties: String = ''): TCEAppSettingItem;
begin
  Result:= TCEAppSettingItem.Create;
  Result.NodeName:= AName;
  Result.ObjectToSave:= AObject;
  Result.Recursive:= ARecursive;
  Result.SaveDefaults:= ASaveDefaults;
  if AExcludeProperties <> '' then
  Result.ExcludeProperties.CommaText:= AExcludeProperties;
  if AIncludeProperties <> '' then
  Result.IncludeProperties.CommaText:= AIncludeProperties;
  fItems.Add(Result);
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEAppSettings.Clear;
begin
  fItems.Clear;
end;

{-------------------------------------------------------------------------------
  Delete
-------------------------------------------------------------------------------}
procedure TCEAppSettings.Delete(Index: Integer);
begin
  fItems.Delete(Index);
end;

{-------------------------------------------------------------------------------
  Find Item
-------------------------------------------------------------------------------}
function TCEAppSettings.FindItem(AName: String): TCEAppSettingItem;
var
  i: Integer;
  item: TCEAppSettingItem;
begin
  Result:= nil;
  for i:= 0 to fItems.Count - 1 do
  begin
    item:= TCEAppSettingItem(fItems.Items[i]);
    if CompareText(item.NodeName, AName) = 0 then
    begin
      Result:= item;
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCEAppSettings.GetCount: Integer;
begin
  Result:= fItems.Count;
end;

{-------------------------------------------------------------------------------
  Get Items
-------------------------------------------------------------------------------}
function TCEAppSettings.GetItems(Index: Integer): TCEAppSettingItem;
begin
  Result:= TCEAppSettingItem(fItems.Items[Index]);
end;

{-------------------------------------------------------------------------------
  Load From File
-------------------------------------------------------------------------------}
function TCEAppSettings.LoadFromFile(AFilePath: WideString): Boolean;
begin
  if WideFileExists(AFilePath) then
  begin
    fXML.Free;
    Result:= true;
    try
      ReadXMLFile(fXML, AFilePath);
    except on EXMLReadError do
      begin
        if not assigned(fXML) then
        begin
          fXML:= TXMLDocument.Create;
          XML.AppendChild(XML.CreateElement('CubicExplorer'));
        end
        else if not assigned(XML.DocumentElement) then
        begin
          XML.AppendChild(XML.CreateElement('CubicExplorer'));
        end
        else
        Result:= false;
      end;
    end;

    if Result then
    LoadProperties;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  LoadFromResource
-------------------------------------------------------------------------------}
function TCEAppSettings.LoadFromResource(AInstance: Cardinal; AResName:
    String): Boolean;
var
  stream: TStream;
begin
  Result:= false;
  try
    stream:= TResourceStream.Create(AInstance, AResName, RT_RCDATA);
    try
      fXML.Free;
      try
        ReadXMLFile(fXML, stream);
        Result:= true;
      except on EXMLReadError do
        begin
          if not assigned(fXML) then
          begin
            fXML:= TXMLDocument.Create;
            XML.AppendChild(XML.CreateElement('CubicExplorer'));
          end
          else if not assigned(XML.DocumentElement) then
          begin
            XML.AppendChild(XML.CreateElement('CubicExplorer'));
          end;
        end;
      end;

      if Result then
      LoadProperties;
    finally
      stream.Free;
    end;
  except
    Result:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Load Object Properties
-------------------------------------------------------------------------------}
procedure TCEAppSettings.LoadObjectProperties(AObject: TObject; ANode:
    TDOMNode; ARecursive: Boolean = true; AExcludeProperties: TStrings = nil;
    AIncludeProperties: TStrings = nil);
var
  node: TDOMNode;
  propInfo: PPropInfo;
  valueI: Integer;
  valueI64: Int64;
  valueF: Extended;
  valueB: Boolean;
  subObj: TObject;
begin
  if assigned(ANode) and assigned(AObject) then
  begin
    if AObject is TCECustomSettingStorage then
    begin
      TCECustomSettingStorage(AObject).Load(Self, ANode);
    end
    else
    begin
      // get first child node (property)
      node:= ANode.FirstChild;
      // loop all child nodes
      while assigned(node) do
      begin
        // check if property should be ignored
        if assigned(AExcludeProperties) and (AExcludeProperties.IndexOf(node.NodeName) > -1) then
        continue;
        // check is property is in allowed list
        if assigned(AIncludeProperties) and (AIncludeProperties.Count > 0) and (AIncludeProperties.IndexOf(node.NodeName) = -1) then
        continue;

        // find property
        propInfo:= GetPropInfo(AObject, node.NodeName);
        if propInfo <> nil then
        begin
          if propInfo.PropType^ = TypeInfo(Boolean) then
          begin
            valueI:= GetOrdProp(AObject, propInfo);
            valueB:= StrToBoolDef(node.TextContent, Boolean(valueI));
            SetOrdProp(AObject, propInfo, Integer(valueB));
          end
          else
          begin
            // set property value
            case propInfo.PropType^.Kind of
              // String
              tkString, tkLString: begin
                SetStrProp(AObject, propInfo, node.TextContent);
              end;
              // WideString
              tkWString: begin
                SetWideStrProp(AObject, propInfo, node.TextContent);
              end;
              // Ordinal
              tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: begin
                valueI:= GetOrdProp(AObject, propInfo);
                SetOrdProp(AObject, propInfo, StrToIntDef(node.TextContent, valueI));
              end;
              // Variant
              tkVariant: begin
                SetVariantProp(AObject, propInfo, node.TextContent);
              end;
              // Int64
              tkInt64: begin
                valueI64:= GetInt64Prop(AObject, propInfo);
                SetInt64Prop(AObject, propInfo, StrToInt64Def(node.TextContent, valueI64));
              end;
              // Float
              tkFloat: begin
                valueF:= GetFloatProp(AObject, propInfo);
                SetFloatProp(AObject, propInfo, StrToFloatDef(node.TextContent, valueF));
              end;
              // Class
              tkClass: begin
                if ARecursive then
                begin
                  subObj:= GetObjectProp(AObject, propInfo);
                  LoadObjectProperties(subObj, node, ARecursive, AExcludeProperties);
                end;
              end
            end;
          end;
        end;
        node:= node.NextSibling;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load Properties
-------------------------------------------------------------------------------}
procedure TCEAppSettings.LoadProperties;
var
  item: TCEAppSettingItem;
  node: TDOMNode;
begin
  if assigned(XML.DocumentElement) then
  begin
    node:= XML.DocumentElement.FirstChild;
    while assigned(node) do
    begin
      item:= FindItem(node.NodeName);
      if assigned(item) then
      LoadObjectProperties(item.ObjectToSave, node, item.Recursive, item.ExcludeProperties, item.IncludeProperties);
      node:= node.NextSibling;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save Object Properties
-------------------------------------------------------------------------------}
procedure TCEAppSettings.SaveObjectProperties(AObject: TObject; AParentNode:
    TDOMNode; ARecursive: Boolean = true; ASaveDefaultValues: Boolean = false;
    AExcludeProperties: TStrings = nil; AIncludeProperties: TStrings = nil);
var
  index: Integer;
  Data: PTypeData;
  PropList: PPropList;
  PropInfo: PPropInfo;
  propCount: Integer;
  propValue: string;
  propValueI: Integer;
  propValueI64: Int64;
  propValueF: Extended;
  propValueW: WideString;
  node: TDOMElement;
  addNode: Boolean;
  isWide: Boolean;
  saveDefault: Boolean;
  subObj: TObject;
begin
  if assigned(AObject) then
  begin
    if AObject is TCECustomSettingStorage then
    begin
      TCECustomSettingStorage(AObject).Save(Self, AParentNode);
    end
    else
    begin
      Data:= GetTypeData(AObject.ClassInfo);
      propCount:= Data.PropCount;
      GetMem(propList, Data^.propCount * SizeOf(PPropInfo));
      try
        // Should default values be saved?
        if SaveDefaultsAlways then
        saveDefault:= true
        else if SaveDefaultsNever then
        saveDefault:= false
        else
        saveDefault:= ASaveDefaultValues;
        // get list of properties
        GetPropInfos(AObject.ClassInfo, PropList);
        // Loop property items
        for index:= 0 to propCount - 1 do
        begin
          addNode:= true;
          isWide:= false;
          subObj:= nil;
          // Get property info
          propInfo:= propList^[index];
          // Check if property should be ignored
          if assigned(AExcludeProperties) then
          addNode:= AExcludeProperties.IndexOf(propInfo.Name) = -1;
          // Check if property is allowed
          if assigned(AIncludeProperties) and (AIncludeProperties.Count > 0) then
          addNode:= AIncludeProperties.IndexOf(propInfo.Name) > -1;
          // Get property value
          if addNode then
          begin
            if propInfo.PropType^ = TypeInfo(Boolean) then
            begin
              propValueI:= GetOrdProp(AObject, propInfo);
              addNode:= (propValueI <> propInfo.Default);
              if addNode then
              propValue:= BoolToStr(Boolean(propValueI), StringBooleans);
            end
            else
            begin
              case propInfo.PropType^.Kind of
                // String
                tkString, tkLString: begin
                  propValue:= GetStrProp(AObject, propInfo);
                  addNode:= (saveDefault or (propValue <> ''));
                end;
                // WideString
                tkWString: begin
                  propValueW:= GetWideStrProp(AObject, propInfo);
                  isWide:= true;
                  addNode:= (saveDefault or (propValueW <> ''));
                end;
                // Ordinal
                tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: begin
                  propValueI:= GetOrdProp(AObject, propInfo);
                  addNode:= (saveDefault or ((propInfo.Default <> NoDefault) and (propValueI <> propInfo.Default)));
                  if addNode then
                  propValue:= IntToStr(propValueI);
                end;
                // Variant
                tkVariant: begin
                  propValue:= GetVariantProp(AObject, propInfo);
                end;
                // Int64
                tkInt64: begin
                  propValueI64:= GetInt64Prop(AObject, propInfo);
                  addNode:= (saveDefault or (propValueI64 <> 0));
                  if addNode then
                  propValue:= IntToStr(propValueI64);
                end;
                // Float
                tkFloat: begin
                  propValueF:= GetFloatProp(AObject, propInfo);
                  addNode:= (saveDefault or (propValueF <> 0));
                  if addNode then
                  propValue:= FloatToStr(propValueF);
                end;
                // Class
                tkClass: begin
                  addNode:= ARecursive;
                  if addNode then
                  begin
                    propValue:= '';
                    subObj:= GetObjectProp(AObject, propInfo);
                  end;
                end
                else
                addNode:= false;
              end;
            end;

            // Add XML node
            if addNode then
            begin
              node:= XML.CreateElement(propInfo.Name);
              if isWide then
              node.TextContent:= propValueW
              else
              node.TextContent:= propValue;
              if assigned(AParentNode) then
              AParentNode.AppendChild(node)
              else if assigned(XML.DocumentElement) then
              XML.DocumentElement.AppendChild(node)
              else
              begin
                node.Free;
                Exit;
              end;

              if assigned(subObj) then
              SaveObjectProperties(subObj, node, ARecursive, ASaveDefaultValues);
            end;
          end;
        end;
      finally
        FreeMem(PropList);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save Properties
-------------------------------------------------------------------------------}
procedure TCEAppSettings.SaveProperties;
var
  i: Integer;
  item: TCEAppSettingItem;
  node: TDOMElement;
begin
  if assigned(XML.DocumentElement) then
  XML.DocumentElement.Free;
  XML.AppendChild(XML.CreateElement('CubicExplorer'));
  for i:= 0 to fItems.Count - 1 do
  begin
    item:= TCEAppSettingItem(fItems[i]);
    if assigned(item.ObjectToSave) then
    begin
      if item.NodeName <> '' then
      begin
        node:= XML.CreateElement(item.NodeName);
        XML.DocumentElement.AppendChild(node);
        SaveObjectProperties(item.ObjectToSave, node, item.Recursive, item.SaveDefaults, item.ExcludeProperties, item.IncludeProperties);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
procedure TCEAppSettings.SaveToFile(AFilePath: WideString);
var
  FileStream: TStream;
begin
  try
    FileStream:= TWideFileStream.Create(AFilePath, fmCreate);
  except
    Exit;
  end;

  try
    SaveProperties;
    WriteXML(XML.DocumentElement, FileStream);
  finally
    FileStream.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Set SaveDefaultsAlways
-------------------------------------------------------------------------------}
procedure TCEAppSettings.SetSaveDefaultsAlways(const Value: Boolean);
begin
  fSaveDefaultsAlways:= Value;
  if fSaveDefaultsAlways then
  fSaveDefaultsNever:= false;
end;

{-------------------------------------------------------------------------------
  Set SaveDefaultsNever
-------------------------------------------------------------------------------}
procedure TCEAppSettings.SetSaveDefaultsNever(const Value: Boolean);
begin
  fSaveDefaultsNever:= Value;
  if fSaveDefaultsNever then
  fSaveDefaultsAlways:= false;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEAppSettingItem
-------------------------------------------------------------------------------}
constructor TCEAppSettingItem.Create;
begin
  inherited;
  fExcludeProperties:= TStringList.Create;
  fIncludeProperties:= TStringList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEAppSettingItem
-------------------------------------------------------------------------------}
destructor TCEAppSettingItem.Destroy;
begin
  fExcludeProperties.Free;
  fIncludeProperties.Free;
  inherited;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCECustomSettingStorage.Load(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
begin

end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCECustomSettingStorage.Save(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
begin

end;

{##############################################################################}

initialization
  GlobalAppSettings:= TCEAppSettings.Create;
  GlobalAppSettings.SaveDefaultsAlways:= true;
  
finalization
  FreeAndNil(GlobalAppSettings);

end.
