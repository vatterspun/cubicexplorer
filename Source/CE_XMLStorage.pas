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
//  The Original Code is CE_XMLStorage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_XMLStorage;

interface

uses
  // fcl-xml
  DOM, XMLRead, XMLWrite,
  // Tnt
  TntSysUtils,
  // System Units
  Windows, SysUtils, Messages, Classes, WideStrings;

type
  TDOMElementHack = class(TDOMElement);

  TCEXmlStorage = class(TObject)
  private
    fActiveNode: TDOMNode;
    fActivePath: WideString;
    fDocumentRootName: WideString;
    fRootNode: TDOMNode;
    function GetRootNode: TDOMNode;
    procedure SetActivePath(const Value: WideString);
    procedure SetDocumentRootName(const Value: WideString);
  protected
  public
    XmlDoc: TXMLDocument;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DeletePath(APath: WideString);
    function GetNode(APath: WideString; AutoCreate: Boolean = true): TDOMNode;
    function GetAttribute(Node: TDOMNode; AttrName: WideString; AutoCreate: Boolean
        = true): TDOMNode;
    function GetChildElement(ParentNode: TDOMNode; NodeName: WideString;
        AutoCreate: Boolean = true): TDOMNode;
    function GetValue(APath: WideString; ADefault: Boolean): Boolean; overload;
    function GetValue(APath: WideString; ADefault: WideString): WideString;
        overload;
    function GetValue(APath: WideString; ADefault: Integer): Integer; overload;
    function GetValue(APath: WideString; ADefault: TPoint): TPoint; overload;
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFilePath: WideString);
    procedure SaveToStream(AStream: TStream);
    procedure SetValue(APath: WideString; AValue: WideString); overload;
    procedure SetValue(APath: WideString; AValue: Boolean); overload;
    procedure SetValue(APath: WideString; AValue: Integer); overload;
    procedure SetValue(APath: WideString; AValue: TPoint); overload;
    property ActiveNode: TDOMNode read fActiveNode;
    property ActivePath: WideString read fActivePath write SetActivePath;
    property DocumentRootName: WideString read fDocumentRootName write
        SetDocumentRootName;
    property RootNode: TDOMNode read GetRootNode write fRootNode;
  end;

function GetElement(XmlDoc: TXmlDocument; APath: WideString; AutoCreate:
    Boolean = false): TDOMNode;

function GetChildElement(ParentNode: TDOMNode; NodeName: WideString;
    AutoCreate: Boolean = false): TDOMNode;

function DeleteChildElement(ParentNode: TDOMNode; NodeName: WideString):
    Boolean;


{-------------------------------------------------------------------------------
  Valid paths are:

  /node1/node2      = RootNode + Path
  node1/node2       = ActivePath + Path
  /node1?attr1      = Attribute value
--------------------------------------------------------------------------------}
implementation

{-------------------------------------------------------------------------------
  Get/Find Element
--------------------------------------------------------------------------------}
function GetElement(XmlDoc: TXmlDocument; APath: WideString; AutoCreate:
    Boolean = false): TDOMNode;
var
  i,c: Integer;
  startPos, endPos: Integer;
  nodeName: WideString;
  node: TDOMNode;
begin
  Result:= nil;
  startPos:= 1;
  node:= XmlDoc.DocumentElement;
  c:= Length(APath);
  if APath[c] = '/' then
  c:= c - 1;
  
  for i:= 1 to c do
  begin
    if APath[i] = '/' then
    begin
      endPos:= i;
      if i > 1 then
      begin
        nodeName:= Copy(APath, startPos, endPos - startPos);
        if nodeName <> '' then
        node:= GetChildElement(node, nodeName, AutoCreate);
      end;
      startPos:= i+1;
    end;    
    if node = nil then
    Exit;
  end;

  endPos:= c+1;
  nodeName:= Copy(APath, startPos, endPos - startPos);
  if nodeName <> '' then
  begin
    node:= GetChildElement(node, nodeName, AutoCreate);
  end;
  Result:= node;
end;

{-------------------------------------------------------------------------------
  Get/Find Child Element
-------------------------------------------------------------------------------}
function GetChildElement(ParentNode: TDOMNode; NodeName: WideString;
    AutoCreate: Boolean = false): TDOMNode;
var
  tmpNode: TDOMNode;
begin
  tmpNode:= ParentNode.FirstChild;
  while tmpNode <> nil do
  begin
    if tmpNode is TDOMElement then
      if WideSameText(tmpNode.NodeName, NodeName) then
      Break;
    tmpNode:= tmpNode.NextSibling;
  end;
  if (tmpNode = nil) and AutoCreate then
  begin
    tmpNode:= ParentNode.OwnerDocument.CreateElement(NodeName);
    ParentNode.AppendChild(tmpNode);
  end;
  Result:= tmpNode;
end;

{-------------------------------------------------------------------------------
  Delete child element. Returns TRUE if node was deleted.
-------------------------------------------------------------------------------}
function DeleteChildElement(ParentNode: TDOMNode; NodeName: WideString):
    Boolean;
var
  chNode: TDOMNode;
begin
  Result:= false;
  chNode:= GetChildElement(ParentNode, NodeName);
  if assigned(chNode) then
  begin
    ParentNode.RemoveChild(chNode);
    Result:= true;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEXmlStorage
--------------------------------------------------------------------------------}
constructor TCEXmlStorage.Create;
begin
  inherited Create;
  XmlDoc := TXMLDocument.Create;
  DocumentRootName:= 'Root';
  fRootNode:= nil;
end;

{-------------------------------------------------------------------------------
  Destroy TCEXmlStorage
--------------------------------------------------------------------------------}
destructor TCEXmlStorage.Destroy;
begin
  FreeAndNil(XmlDoc);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Clear all
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.Clear;
begin
  XmlDoc.ReplaceChild(XmlDoc.CreateElement(fDocumentRootName), XmlDoc.DocumentElement);
end;

{-------------------------------------------------------------------------------
  Delete Path
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.DeletePath(APath: WideString);
var
  node: TDOMNode;
  attr: TDOMAttr;
begin
  node:= GetNode(APath, false);
  if assigned(node) and (node <> XmlDoc.DocumentElement) then
  begin
    if node is TDOMAttr then
    begin
      attr:= TDOMAttr(node);
      attr.OwnerElement.RemoveAttributeNode(attr);
      FreeAndNil(attr);
    end
    else if assigned(node.ParentNode) then
    begin
      node.ParentNode.RemoveChild(node);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  FindNode
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetNode(APath: WideString; AutoCreate: Boolean = true):
    TDOMNode;
var
  i: Integer;
  isAttribute: Boolean;
  startPos, endPos: Integer;
  nodeName: WideString;
  node: TDOMNode;
begin
  Result:= nil;
  if fActiveNode = nil then
  node:= RootNode
  else
  node:= fActiveNode;
  startPos:= 1;
  isAttribute:= false;
  for i:= 1 to Length(APath) do
  begin
    if APath[i] = '/' then
    begin
      endPos:= i;
      if i = 1 then
      begin
        node:= RootNode;
      end
      else
      begin
        nodeName:= Copy(APath, startPos, endPos - startPos);
        if nodeName <> '' then
        node:= GetChildElement(node, nodeName, AutoCreate);
      end;
      startPos:= i+1;
    end
    else if APath[i] = '?' then
    begin
      endPos:= i;
      nodeName:= Copy(APath, startPos, endPos - startPos);
      if nodeName <> '' then
      node:= GetChildElement(node, nodeName, AutoCreate);
      startPos:= i+1;
      isAttribute:= true;
    end;
    if node = nil then
    Exit;
  end;

  endPos:= Length(APath)+1;
  nodeName:= Copy(APath, startPos, endPos - startPos);
  if nodeName <> '' then
  begin
    if isAttribute then
    node:= GetAttribute(node, nodeName, AutoCreate)
    else
    node:= GetChildElement(node, nodeName, AutoCreate);
  end;
  Result:= node;
end;

{-------------------------------------------------------------------------------
  Get Attribute
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetAttribute(Node: TDOMNode; AttrName: WideString;
    AutoCreate: Boolean = true): TDOMNode;
var
  tmpNode: TDOMNode;
begin
  tmpNode:= Node.Attributes.GetNamedItem(AttrName);
  if (tmpNode = nil) and AutoCreate then
  begin
    tmpNode:= Node.OwnerDocument.CreateAttribute(AttrName);
    Node.Attributes.SetNamedItem(tmpNode);
  end;
  Result:= tmpNode;
end;

{-------------------------------------------------------------------------------
  Get Child Node
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetChildElement(ParentNode: TDOMNode; NodeName:
    WideString; AutoCreate: Boolean = true): TDOMNode;
var
  tmpNode: TDOMNode;
begin
  tmpNode:= ParentNode.FirstChild;
  while tmpNode <> nil do
  begin
    if WideSameText(tmpNode.NodeName, NodeName) then
    Break;
    tmpNode:= tmpNode.NextSibling;
  end;
  if (tmpNode = nil) and AutoCreate then
  begin
    tmpNode:= ParentNode.OwnerDocument.CreateElement(NodeName);
    ParentNode.AppendChild(tmpNode);
  end;
  Result:= tmpNode;
end;

{-------------------------------------------------------------------------------
  Get Value (String)
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetValue(APath: WideString; ADefault: WideString):
    WideString;
var
  node: TDOMNode;
begin
  node:= GetNode(APath, false);
  if assigned(node) then
  begin
    if node is TDOMAttr then
    Result:= TDOMAttr(node).Value
    else
    Result:= Node.TextContent;
  end
  else
  begin
    Result:= ADefault;
  end;
end;

{-------------------------------------------------------------------------------
  Get Value (Boolean)
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetValue(APath: WideString; ADefault: Boolean): Boolean;
var
  ws: WideString;
begin
  ws:= GetValue(APath, '');
  if WideSameText(ws, 'true') then
  Result:= True
  else if WideSameText(ws, 'false') then
  Result:= False
  else
  Result:= ADefault;
end;

{-------------------------------------------------------------------------------
  Get Value (Integer)
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetValue(APath: WideString; ADefault: Integer): Integer;
begin
  Result:= StrToIntDef(GetValue(APath, ''), ADefault);
end;

{-------------------------------------------------------------------------------
  Get Value (TPoint)
--------------------------------------------------------------------------------}
function TCEXmlStorage.GetValue(APath: WideString; ADefault: TPoint): TPoint;
var
  i,c: Integer;
  s, tmpS: String;
begin
  tmpS:= GetValue(APath, '');
  if tmpS = '' then
  begin
    Result:= ADefault;
  end
  else
  begin
    // X
    i:= 1;
    c:= Pos(',',tmpS);
    s:= copy(tmpS,i,c-1);
    Result.X:= StrToIntDef(s,ADefault.X);
    // Y
    i:= c+1;
    s:= copy(tmpS,i,Length(tmpS));
    Result.Y:= StrToIntDef(s,ADefault.Y);
  end;
end;

{-------------------------------------------------------------------------------
  Load From File
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.LoadFromFile(AFilePath: WideString);
begin
  if WideFileExists(AFilePath) then
  begin
    fRootNode:= nil;
    fActiveNode:= nil;
    fActivePath:= '/';
    XmlDoc.Free;
    ReadXMLFile(XmlDoc, AFilePath);
  end;
end;

{-------------------------------------------------------------------------------
  Load From Stream
-------------------------------------------------------------------------------}
procedure TCEXmlStorage.LoadFromStream(AStream: TStream);
begin
  fRootNode:= nil;
  fActiveNode:= nil;
  fActivePath:= '/';
  XmlDoc.Free;
  ReadXMLFile(XmlDoc, AStream);
end;

{-------------------------------------------------------------------------------
  Save to File
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SaveToFile(AFilePath: WideString);
begin
  WriteXMLFile(XmlDoc, AFilePath);
end;

{-------------------------------------------------------------------------------
  Save to Stream
-------------------------------------------------------------------------------}
procedure TCEXmlStorage.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(XmlDoc, AStream);
end;

{-------------------------------------------------------------------------------
  Set Active Path
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetActivePath(const Value: WideString);
var
  path: WideString;
begin
  if Value = '' then
  begin
    fActiveNode:= nil;
    fActivePath:= '/';
  end
  else
  begin
    path:= Value;
    if path[1] <> '/' then
    path:= '/' + path;
    fActiveNode:= GetNode(path, true);
    if assigned(fActiveNode) then
    begin
      fActivePath:= Value;
    end
    else
    begin
      fActiveNode:= RootNode;
      fActivePath:= '/';
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Root Name
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetDocumentRootName(const Value: WideString);
begin
  if Value = fDocumentRootName then
  Exit;

  if Value <> '' then
  fDocumentRootName:= Value
  else
  fDocumentRootName:= 'Root';

  if not assigned(XmlDoc.DocumentElement) then
  begin
    XmlDoc.AppendChild(XmlDoc.CreateElement(fDocumentRootName));
  end
  else
  begin
    TDOMElementHack(XmlDoc.DocumentElement).FNodeName:= fDocumentRootName;
  end;
end;

{-------------------------------------------------------------------------------
  Get Root Node
-------------------------------------------------------------------------------}
function TCEXmlStorage.GetRootNode: TDOMNode;
begin
  if fRootNode = nil then
  Result:= XmlDoc.DocumentElement
  else
  Result:= fRootNode;
end;

{-------------------------------------------------------------------------------
  Set Value (String)
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetValue(APath: WideString; AValue: WideString);
var
  node: TDOMNode;
begin
  node:= GetNode(APath, true);
  if assigned(node) then
  begin
    if node is TDOMAttr then
    TDOMAttr(node).Value:= AValue
    else
    Node.TextContent:= AValue;
  end;
end;

{-------------------------------------------------------------------------------
  Set Value (Boolean)
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetValue(APath: WideString; AValue: Boolean);
begin
  if AValue then
  SetValue(APath, 'True')
  else
  SetValue(APath, 'False');
end;

{-------------------------------------------------------------------------------
  Set Value (Integer)
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetValue(APath: WideString; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

{-------------------------------------------------------------------------------
  Set Value (TPoint)
--------------------------------------------------------------------------------}
procedure TCEXmlStorage.SetValue(APath: WideString; AValue: TPoint);
begin
  SetValue(APath, IntToStr(AValue.X) + ',' + IntToStr(AValue.Y));
end;

end.


