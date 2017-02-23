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
//  The Original Code is CE_Bookmarks.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Bookmarks;

interface

uses
  // CE Units

  // JVCL
  JvSimpleXml,
  // Tnt
  TntClasses, TntSysUtils,
  // System Units
  Classes, SysUtils, Windows, Messages, ImgList, Controls, Contnrs, ActiveX;

type
  TCECustomBookCompClass = class of TCECustomBookComp;
  TCECustomBookComp = class(TObject)
  private
  protected
    fEnabled: Boolean;
    fExpanded: Boolean;
    fGhosted: Boolean;
    fTitle: WideString;
    fImageList: TImageList;
    fSubMenuOnly: Boolean;
  public
    constructor Create; virtual;
    procedure Assign(From: TCECustomBookComp); virtual;
    procedure AssignTo(ToComp: TCECustomBookComp); virtual;
    function DoDragDrop(DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
        var Effect: Integer): Boolean; virtual;
    function DoDragOver(Shift: TShiftState; Pt: TPoint; var Effect: Integer):
        Boolean; virtual;
    function DoDragEnter(DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
        var Effect: Integer): Boolean; virtual;
    procedure DoDragLeave; virtual;
    function DoPopup(X, Y: Integer): Boolean; virtual;
    function GetImageIndex(Open: Boolean = false; Overlay: Boolean = false):
        Integer; virtual;
    procedure KeyAction(CharCode: Word; Shift: TShiftState); virtual;
    procedure LoadFromXmlNode(XmlNode: TJvSimpleXmlElem); virtual;
    procedure MouseClick(Shift: TShiftState; Button: TMouseButton; SingleClickMode:
        Boolean = false); virtual;
    procedure SaveToXmlNode(XmlNode: TJvSimpleXmlElem); virtual;
    function SupportsDragDrop: Boolean; virtual;
    property Enabled: Boolean read fEnabled write fEnabled;
    property Expanded: Boolean read fExpanded write fExpanded;
    property Ghosted: Boolean read fGhosted write fGhosted;
    property ImageList: TImageList read fImageList write fImageList;
    property SubMenuOnly: Boolean read fSubMenuOnly write fSubMenuOnly;
    property Title: WideString read fTitle write fTitle;
  end;

  PCEBookData = ^TCEBookData;
  TCEBookData = record
    BookComp: TCECustomBookComp;
  end;

  TCEBookCompList = class(TObject)
  public
    CompNames: TStrings;
    CompClasses: TClassList;
    constructor Create;
    destructor Destroy; override;
    function CreateNewComp(CompName: String): TCECustomBookComp;
    function GetCompClass(CompName: String): TCECustomBookCompClass; overload;
    function GetCompClass(CompIndex: Integer): TCECustomBookCompClass; overload;
    function GetCompName(CompClass: TCECustomBookComp): String;
    procedure RegisterBookComp(CompName: String; CompClass: TCECustomBookCompClass);
  end;

var
  CEBookCompList: TCEBookCompList;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCECustomBookComp object
-------------------------------------------------------------------------------}
constructor TCECustomBookComp.Create;
begin
  inherited;
  fGhosted:= false;
  fImageList:= nil;
  fEnabled:= true;
end;

{*------------------------------------------------------------------------------
  Assign values from component
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.Assign(From: TCECustomBookComp);
begin
  if not assigned(From) then
  Exit;
  fEnabled:= From.fEnabled;
  fExpanded:= From.fExpanded;
  fGhosted:= From.fGhosted;
  fTitle:= From.Title;
  fImageList:= From.fImageList;
  fSubMenuOnly:= From.fSubMenuOnly;
end;

{*------------------------------------------------------------------------------
  Assign values to component
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.AssignTo(ToComp: TCECustomBookComp);
begin
  if not assigned(ToComp) then
  Exit;
  ToComp.fEnabled:= fEnabled;
  ToComp.fExpanded:= fExpanded;
  ToComp.fGhosted:= fGhosted;
  ToComp.Title:= fTitle;
  ToComp.fImageList:= fImageList;
  ToComp.fSubMenuOnly:= fSubMenuOnly;
end;

{-------------------------------------------------------------------------------
  Do Drag Drop (Return true if drop is handled)
-------------------------------------------------------------------------------}
function TCECustomBookComp.DoDragDrop(DataObject: IDataObject; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer): Boolean;
begin
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Do Drag Over (Return true if drag is handled)
-------------------------------------------------------------------------------}
function TCECustomBookComp.DoDragOver(Shift: TShiftState; Pt: TPoint; var
    Effect: Integer): Boolean;
begin
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Do Drag Enter
-------------------------------------------------------------------------------}
function TCECustomBookComp.DoDragEnter(DataObject: IDataObject; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer): Boolean;
begin
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Do Drag Leave
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.DoDragLeave;
begin
  // nothing to do
end;

{-------------------------------------------------------------------------------
  Do Popup (Return true if handled)
-------------------------------------------------------------------------------}
function TCECustomBookComp.DoPopup(X, Y: Integer): Boolean;
begin
  Result:= false;
end;

{*------------------------------------------------------------------------------
  Load values from xml node.
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.LoadFromXmlNode(XmlNode: TJvSimpleXmlElem);
begin
  fTitle:= UTF8Decode(XmlNode.Properties.Value('name'));
  fExpanded:= XmlNode.Properties.BoolValue('isopen', false);
end;

{*------------------------------------------------------------------------------
  Save values to xml node.
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.SaveToXmlNode(XmlNode: TJvSimpleXmlElem);
begin
  XmlNode.Properties.Add('name', UTF8Encode(fTitle));
  if fExpanded then
  XmlNode.Properties.Add('isopen', 'true');
end;

{*------------------------------------------------------------------------------
  Handle mouse click
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.MouseClick(Shift: TShiftState; Button:
    TMouseButton; SingleClickMode: Boolean = false);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Handle Key action
-------------------------------------------------------------------------------}
procedure TCECustomBookComp.KeyAction(CharCode: Word; Shift: TShiftState);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get ImageIndex
-------------------------------------------------------------------------------}
function TCECustomBookComp.GetImageIndex(Open: Boolean = false; Overlay:
    Boolean = false): Integer;
begin
  Result:= -1;
end;

function TCECustomBookComp.SupportsDragDrop: Boolean;
begin
  Result:= false;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEBookCompList object
-------------------------------------------------------------------------------}
constructor TCEBookCompList.Create;
begin
  inherited;
  CompNames:= TStringList.Create;
  CompClasses:= TClassList.Create;
end;

{*------------------------------------------------------------------------------
  Destroy TCEBookCompList object
-------------------------------------------------------------------------------}
destructor TCEBookCompList.Destroy;
begin
  CompNames.Free;
  CompClasses.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Register new bookmark component class
-------------------------------------------------------------------------------}
procedure TCEBookCompList.RegisterBookComp(CompName: String; CompClass:
    TCECustomBookCompClass);
begin
  if not assigned(CompClass) then
  Exit;
  if CompNames.IndexOf(CompName) > -1 then
  Exit;

  CompNames.Add(CompName);
  CompClasses.Add(CompClass);
end;

{*------------------------------------------------------------------------------
  Get Comp by index
-------------------------------------------------------------------------------}
function TCEBookCompList.GetCompClass(CompIndex: Integer):
    TCECustomBookCompClass;
begin
  Result:= TCECustomBookCompClass(CompClasses.Items[CompIndex]);
end;

{*------------------------------------------------------------------------------
  Get Comp by name
-------------------------------------------------------------------------------}
function TCEBookCompList.GetCompClass(CompName: String): TCECustomBookCompClass;
var
  i: Integer;
begin
  Result:= nil;
  i:= CompNames.IndexOf(CompName);
  if i > -1 then
  Result:= GetCompClass(i);
end;

{*------------------------------------------------------------------------------
  Create new instance of Comp by name
-------------------------------------------------------------------------------}
function TCEBookCompList.CreateNewComp(CompName: String): TCECustomBookComp;
var
  compC: TCECustomBookCompClass;
begin
  Result:= nil;
  compC:= GetCompClass(CompName);
  if assigned(compC) then
  Result:= compC.Create;
end;

{*------------------------------------------------------------------------------
  Get Comp name. Returns '' if name was not found.
-------------------------------------------------------------------------------}
function TCEBookCompList.GetCompName(CompClass: TCECustomBookComp): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to CompClasses.Count - 1 do
  begin
    if CompClass.ClassType = CompClasses.Items[i] then
    begin
      Result:= CompNames.Strings[i];
      Break;
    end;
  end;
end;

{##############################################################################}

initialization
  CEBookCompList:= TCEBookCompList.Create;

finalization
  CEBookCompList.Free;

end.
