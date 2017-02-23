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
//  The Original Code is CE_StdBookmarkComps.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************

unit CE_StdBookmarkComps;

interface

uses
  // CE Units
  CE_Bookmarks, CE_Utils, CE_CommonObjects, CE_FileUtils,
  // JVCL
  JvSimpleXml,
  // VSTools
  MPShellUtilities, MPCommonObjects, MPCommonUtilities, MPDataObject,
  // Tnt
  TntClasses, TntSysUtils,
  // System Units
  Classes, SysUtils, Windows, Messages, ImgList, Controls, Contnrs, Dialogs,
  ShlObj, ActiveX;

type
  TCECategoryComp = class(TCECustomBookComp)
  public
    constructor Create; override;
    function GetImageIndex(Open: Boolean = false; Overlay: Boolean = false):
        Integer; override;
  end;

  TCENormalItemComp = class(TCECustomBookComp)
  private
    fPath: WideString;
    fRelative: Boolean;
    fSpecialFolderID: Integer;
    function GetNameForParsing: WideString;
    function GetNameParseAddress: WideString;
    procedure SetRelative(const Value: Boolean);
  protected
    fIsSpecial: Boolean;
    fIsPath: Boolean;
    fIsPIDL: Boolean;
    fLastIconRefresh: Integer;
    fPIDL: string;
    fSupportsDragDrop: Boolean;
  public
    Namespace: TNamespace;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(From: TCECustomBookComp); override;
    procedure AssignTo(ToComp: TCECustomBookComp); override;
    function DoDragEnter(DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
        var Effect: Integer): Boolean; override;
    procedure DoDragLeave; override;
    function DoDragOver(Shift: TShiftState; Pt: TPoint; var Effect: Integer):
        Boolean; override;
    function DoDragDrop(DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
        var Effect: Integer): Boolean; override;
    function DoPopup(X, Y: Integer): Boolean; override;
    function GetImageIndex(Open: Boolean = false; Overlay: Boolean = false):
        Integer; override;
    function IsExecutable: Boolean;
    function IsFile: Boolean;
    function IsFolder: Boolean;
    procedure KeyAction(CharCode: Word; Shift: TShiftState); override;
    procedure LoadFromPath(APath: WideString; RelativePath: Boolean = false);
    procedure LoadFromPIDL(APIDL: PItemIDList);
    procedure LoadFromXmlNode(XmlNode: TJvSimpleXmlElem); override;
    procedure MouseClick(Shift: TShiftState; Button: TMouseButton; SingleClickMode:
        Boolean = false); override;
    procedure Refresh(OnlyIfLocal: Boolean = false);
    procedure SaveToXmlNode(XmlNode: TJvSimpleXmlElem); override;
    function SupportsDragDrop: Boolean; override;
    property NameForParsing: WideString read GetNameForParsing;
    property NameParseAddress: WideString read GetNameParseAddress;
    property Path: WideString read fPath write fPath;
    property Relative: Boolean read fRelative write SetRelative;
  end;

  TCESessionComp = class(TCECustomBookComp)
  private
    fSessionName: WideString;
  protected
    procedure OpenSession; virtual;
  public
    constructor Create; override;
    procedure Assign(From: TCECustomBookComp); override;
    procedure AssignTo(ToComp: TCECustomBookComp); override;
    function GetImageIndex(Open: Boolean = false; Overlay: Boolean = false):
        Integer; override;
    procedure LoadFromXmlNode(XmlNode: TJvSimpleXmlElem); override;
    procedure MouseClick(Shift: TShiftState; Button: TMouseButton; SingleClickMode:
        Boolean = false); override;
    procedure SaveToXmlNode(XmlNode: TJvSimpleXmlElem); override;
  published
    property SessionName: WideString read fSessionName write fSessionName;
  end;

var
  OpenBookmarkInNewTabByDefault: Boolean = false;  
  LastBookmarkIconRefresh: Integer = 0;

implementation

uses
  CE_GlobalCtrl, dCE_Images, dCE_Actions, Main, CE_Sessions, CE_LanguageEngine;

{*------------------------------------------------------------------------------
  Create an instance of TCECategoryComp object.
-------------------------------------------------------------------------------}
constructor TCECategoryComp.Create;
begin
  inherited;
  fTitle:= _('New Category');
  fSubMenuOnly:= true;
  fImageList:= CE_Images.BookmarkImages;
end;

{*------------------------------------------------------------------------------
  Get ImageIndex
-------------------------------------------------------------------------------}
function TCECategoryComp.GetImageIndex(Open: Boolean = false; Overlay: Boolean
    = false): Integer;
begin
  if Overlay then
  Result:= -1
  else if Open then
  Result:= 1
  else
  Result:= 0;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCENormalItemComp object.
-------------------------------------------------------------------------------}
constructor TCENormalItemComp.Create;
begin
  inherited;
  fIsPIDL:= false;
  fIsPath:= false;
  fIsSpecial:= false;
  fSpecialFolderID:= -1;
  fLastIconRefresh:= GetTickCount;
  Namespace:= nil;
end;

{*------------------------------------------------------------------------------
  Destroy TCENormalItemComp
-------------------------------------------------------------------------------}
destructor TCENormalItemComp.Destroy;
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign values from component
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.Assign(From: TCECustomBookComp);
var
  c: TCENormalItemComp;
begin
  inherited;
  if not (From is TCENormalItemComp) then
  Exit;
  c:= TCENormalItemComp(From);
  
  fIsPath:= c.fIsPath;
  fIsPIDL:= c.fIsPIDL;
  fPath:= c.fPath;
  fPIDL:= c.fPIDL;
  fRelative:= c.fRelative;
  if not assigned(c.Namespace) then
  begin
    if assigned(Namespace) then
    FreeAndNil(Namespace);
  end
  else if not assigned(Namespace) then
  begin
    Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(c.Namespace.AbsolutePIDL),nil);//c.Namespace.Parent);
  end
  else if not ILIsEqual(Namespace.AbsolutePIDL, c.Namespace.AbsolutePIDL) then
  begin
    if assigned(Namespace) then
    FreeAndNil(Namespace);
    Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(c.Namespace.AbsolutePIDL),nil);//c.Namespace.Parent);
  end;
  fSupportsDragDrop:= IsExecutable;
end;

{*------------------------------------------------------------------------------
  Assign values to component
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.AssignTo(ToComp: TCECustomBookComp);
var
  c: TCENormalItemComp;
begin
  inherited;
  if not (ToComp is TCENormalItemComp) then
  Exit;
  c:= TCENormalItemComp(ToComp);
  
  c.fIsPath:= fIsPath;
  c.fIsPIDL:= fIsPIDL;
  c.fPath:= fPath;
  c.fPIDL:= fPIDL;
  c.fRelative:= fRelative;
  if not assigned(Namespace) then
  begin
    if assigned(c.Namespace) then
    FreeAndNil(c.Namespace);
  end
  else if not assigned(c.Namespace) then
  begin
    c.Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(Namespace.AbsolutePIDL),Namespace.Parent);
  end
  else if not ILIsEqual(c.Namespace.AbsolutePIDL, Namespace.AbsolutePIDL) then
  begin
    if assigned(c.Namespace) then
    FreeAndNil(c.Namespace);
    c.Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(Namespace.AbsolutePIDL),Namespace.Parent);
  end;
end;

{-------------------------------------------------------------------------------
  Do Drag Enter
-------------------------------------------------------------------------------}
function TCENormalItemComp.DoDragEnter(DataObject: IDataObject; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer): Boolean;
var
  hd: TCommonShellIDList;
  i: Integer;
begin
  Result:= false;
  if assigned(Namespace) then
  begin
    // Check if DataObject contains folders. Only Files are allowed
    hd:= TCommonShellIDList.Create;
    try
      hd.LoadFromDataObject(DataObject);
      Result:= true;
      for i:= 0 to hd.PIDLCount - 1 do
      begin
        if PIDLIsFolder(hd.AbsolutePIDL(i)) then
        begin
          Result:= false;
          break;
        end;
      end;
    finally
      hd.Free;
    end;
    if Result then
    Result:= Namespace.DragEnter(DataObject, ShiftStateToKeys(Shift), Pt, Effect) = S_OK;
  end;
end;

{-------------------------------------------------------------------------------
  Do Drag Leave
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.DoDragLeave;
begin
  if assigned(Namespace) then
  Namespace.DragLeave;
end;

{-------------------------------------------------------------------------------
  Do Drag Over (Return true if drag is handled)
-------------------------------------------------------------------------------}
function TCENormalItemComp.DoDragOver(Shift: TShiftState; Pt: TPoint; var
    Effect: Integer): Boolean;
begin
  Result:= false;
  if assigned(Namespace) then
  Result:= Namespace.DragOver(ShiftStateToKeys(Shift), Pt, Effect) = S_OK;
end;

{-------------------------------------------------------------------------------
  Do Drop (Return true if drop is handled)
-------------------------------------------------------------------------------}
function TCENormalItemComp.DoDragDrop(DataObject: IDataObject; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer): Boolean;
var
  keyState: Integer;
begin
  Result:= false;
  if assigned(Namespace) and assigned(DataObject) then
  begin
    keyState:= ShiftStateToKeys(Shift);
    effect:= DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK;
    Namespace.DragOver(keystate, pt, effect);
    effect:= DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK;
    Result:= Namespace.Drop(DataObject, keyState, pt, effect) = S_OK;
  end;
end;

{-------------------------------------------------------------------------------
  Do Popup (Return true if handled)
-------------------------------------------------------------------------------}
function TCENormalItemComp.DoPopup(X, Y: Integer): Boolean;
begin
  Result:= false;
  if assigned(Namespace) then
  begin
    Result:= true;
    Namespace.ShowContextMenu(MainForm, CEActions.DoGlobalContextMenuCmd, CEActions.DoGlobalContextMenuShow, nil);
  end;
end;

{*------------------------------------------------------------------------------
  Get ImageIndex
-------------------------------------------------------------------------------}
function TCENormalItemComp.GetImageIndex(Open: Boolean = false; Overlay: Boolean
    = false): Integer;
begin
  if assigned(Namespace) then
  begin
    if LastBookmarkIconRefresh > fLastIconRefresh then
    begin
      fLastIconRefresh:= GetTickCount;
      Namespace.InvalidateCache;
    end;

    if Overlay then
    Result:= Namespace.OverlayIconIndex
    else
    Result:= Namespace.GetIconIndex(Open,icSmall)
  end
  else
  Result:= 4;
end;

{*------------------------------------------------------------------------------
  Get NameForParsing
-------------------------------------------------------------------------------}
function TCENormalItemComp.GetNameForParsing: WideString;
begin
  if assigned(Namespace) then
  Result:= Namespace.NameForParsing
  else
  Result:= fPath;
end;

{*------------------------------------------------------------------------------
  Get NameParseAddress
-------------------------------------------------------------------------------}
function TCENormalItemComp.GetNameParseAddress: WideString;
begin
  if assigned(Namespace) then
  Result:= Namespace.NameParseAddress
  else
  Result:= fPath;
end;

{-------------------------------------------------------------------------------
  Is this an executable?
-------------------------------------------------------------------------------}
function TCENormalItemComp.IsExecutable: Boolean;
begin
  Result:= false;
  if assigned(Namespace) then
  begin
    if not Namespace.Folder and IsSameText(Namespace.Extension,'.exe') then
    begin
      Result:= true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Is this a file
-------------------------------------------------------------------------------}
function TCENormalItemComp.IsFile: Boolean;
begin
  Result:= false;
  if assigned(Namespace) then
  begin
    if IsSameText(Namespace.Extension,'.zip') then
    begin
      Result:= true;
    end
    else if not Namespace.Folder then
    begin
      if WideFileExists(Namespace.NameForParsing) then
      Result:= true;
    end
  end;
end;

{*------------------------------------------------------------------------------
  Is this a folder
-------------------------------------------------------------------------------}
function TCENormalItemComp.IsFolder: Boolean;
begin
  Result:= true;
  if assigned(Namespace) then
  begin
    if not Namespace.Folder then
    begin
      Result:= false;
    end
    else if IsSameText(Namespace.Extension,'.zip') then
    begin
      Result:= false;
    end
  end;
end;

{*------------------------------------------------------------------------------
  Handle Key action
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.KeyAction(CharCode: Word; Shift: TShiftState);
begin
  if CharCode = VK_RETURN then
  begin
    if Shift = [ssCtrl] then
    MouseClick([ssMiddle],mbMiddle)
    else
    MouseClick([ssDouble,ssLeft],mbLeft);
  end;
end;

{*------------------------------------------------------------------------------
  Load values from Path.
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.LoadFromPath(APath: WideString; RelativePath:
    Boolean = false);
var
  PIDL: PItemIDList;
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);

  fIsPath:= true;
  fIsPIDL:= false;
  fIsSpecial:= false;
  fPath:= APath;
  fRelative:= RelativePath;

  ReplaceSystemVariablePath(APath);
  if fRelative then
  PIDL:= PathToPIDL(DecodeRelativePath(APath))
  else
  PIDL:= PathToPIDL(APath);

  if PIDLMgr.IsDesktopFolder(PIDL) then
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
    ImageList:= SmallSysImages;
  end
  else
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
    if Namespace.IsDesktop then
    begin
      FreeAndNil(Namespace);
      fGhosted:= true;
      ImageList:= CE_Images.BookmarkImages;
    end
    else
    begin
      fGhosted:= false;
      ImageList:= SmallSysImages;
    end;
  end;
  fSupportsDragDrop:= IsExecutable;
  fLastIconRefresh:= GetTickCount; 
end;

{*------------------------------------------------------------------------------
  Load values from PIDL.
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.LoadFromPIDL(APIDL: PItemIDList);
var
  PIDL: PItemIDList;
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);

  Namespace:= TNamespace.Create(APIDL,nil);
  fTitle:= Namespace.NameInFolder;
  fImageList:= SmallSysImages;

  fIsPath:= false;
  fIsPIDL:= false;
  fIsSpecial:= false;
  fSpecialFolderID:= CE_SpecialNamespaces.GetSpecialID(Namespace.AbsolutePIDL);
  if fSpecialFolderID > -1 then
  begin
    fIsSpecial:= true;
  end
  else
  begin
    PIDL:= PathToPIDL(Namespace.NameForParsing);
    if assigned(PIDL) then
    begin
      fIsPath:= true;
      fPath:= Namespace.NameForParsing;
      PIDLMgr.FreePIDL(PIDL);
    end
    else
    begin
      fIsPIDL:= true;
      fPIDL:= SavePIDLToMime(Namespace.AbsolutePIDL);
    end;
  end;
  fSupportsDragDrop:= IsExecutable;
  fLastIconRefresh:= GetTickCount;
end;

{*------------------------------------------------------------------------------
  Load values from xml node.
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.LoadFromXmlNode(XmlNode: TJvSimpleXmlElem);
var
  PIDL: PItemIDList;
begin
  inherited;
  if assigned(Namespace) then
  FreeAndNil(Namespace);
  fIsPIDL:= XmlNode.Properties.ItemNamed['pidl'] <> nil;
  fIsSpecial:= XmlNode.Properties.ItemNamed['special'] <> nil;
  if fIsPIDL then
  begin
    fPIDL:= XmlNode.Properties.Value('pidl');
    PIDL:= LoadPIDLFromMime(fPIDL);
  end
  else if fIsSpecial then
  begin
    fSpecialFolderID:= XmlNode.Properties.IntValue('special', -1);
    SHGetspecialFolderLocation(0, fSpecialFolderID, PIDL);
  end
  else
  begin
    fPath:= UTF8Decode(XmlNode.Properties.Value('path'));
    fRelative:= XmlNode.Properties.BoolValue('relative', false);
    if fRelative then
    PIDL:= PathToPIDL(DecodeRelativePath(fPath))
    else
    PIDL:= PathToPIDL(fPath);
    fIsPath:= true;
  end;

  ImageList:= SmallSysImages;

  if PIDLMgr.IsDesktopFolder(PIDL) then
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
  end
  else
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
    if Namespace.IsDesktop then
    begin
      FreeAndNil(Namespace);
      fGhosted:= true;
      ImageList:= CE_Images.BookmarkImages;
    end
    else
    begin
      fGhosted:= false;
    end;
  end;
  fSupportsDragDrop:= IsExecutable;
  fLastIconRefresh:= GetTickCount;
end;

{*------------------------------------------------------------------------------
  Save values to xml node.
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.SaveToXmlNode(XmlNode: TJvSimpleXmlElem);
var
  PIDL: PItemIDList;
  FolderID: Integer;
begin
  XmlNode.Properties.Add('name', UTF8Encode(fTitle));
  if fExpanded then
  XmlNode.Properties.Add('isopen', 'true');
  if assigned(Namespace) then
  begin
    if Namespace.IsDesktop then
    FolderID:= 0
    else
    FolderID:= CE_SpecialNamespaces.GetSpecialID(Namespace.AbsolutePIDL);

    if FolderID > -1 then
    begin
      XmlNode.Properties.Add('special', FolderID);
    end
    else
    begin
      PIDL:= PathToPIDL(Namespace.NameForParsing);
      if assigned(PIDL) or fIsPath then // Save Path
      begin
        if fRelative then
        begin
          XmlNode.Properties.Add('path',UTF8Encode(EncodeRelativePath(Namespace.NameForParsing)));
          XmlNode.Properties.Add('relative', '1');
        end
        else
        XmlNode.Properties.Add('path',UTF8Encode(Namespace.NameForParsing));

        PIDLMgr.FreePIDL(PIDL);
      end
      else // Save PIDL
      begin
        XmlNode.Properties.Add('pidl', SavePIDLToMime(Namespace.AbsolutePIDL));
      end;
    end;
  end
  else
  begin
    if fIsPIDL then
    XmlNode.Properties.Add('pidl',fPIDL)
    else
    XmlNode.Properties.Add('path',UTF8Encode(fPath));

    if fRelative then
     XmlNode.Properties.Add('relative', '1');
  end;
end;

{*------------------------------------------------------------------------------
  Handle mouse click
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.MouseClick(Shift: TShiftState; Button:
    TMouseButton; SingleClickMode: Boolean = false);
begin
  if not assigned(Namespace) then
  Exit;
  
  if Shift = [ssDouble,ssLeft] then
  begin
    if IsSameText(Namespace.Extension,'.zip') then
    begin
      Namespace.ShellExecuteNamespace('','',true);
    end
    else if not Namespace.Folder then
    begin
      Namespace.ShellExecuteNamespace('','');
    end
    else if not SingleClickMode then
    begin
      if OpenBookmarkInNewTabByDefault then
      OpenFolderInTab(nil, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
      else
      GlobalPathCtrl.ChangeGlobalPathPIDL(Self, Namespace.AbsolutePIDL);
    end;
  end
  else if Shift = [ssLeft] then
  begin
    if SingleClickMode and Namespace.Folder and not IsSameText(Namespace.Extension,'.zip') then
    begin
      if OpenBookmarkInNewTabByDefault then
      OpenFolderInTab(nil, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
      else
      GlobalPathCtrl.ChangeGlobalPathPIDL(Self, Namespace.AbsolutePIDL);
    end
  end
  else if (Shift = [ssAlt,ssLeft]) then
  begin
    if OpenBookmarkInNewTabByDefault then
    GlobalPathCtrl.ChangeGlobalPathPIDL(Self, Namespace.AbsolutePIDL)
    else
    OpenFolderInTab(nil, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
  end
  else if (Shift = [ssMiddle]) then
  begin
    if Namespace.Folder and not IsSameText(Namespace.Extension,'.zip') then
    OpenFolderInTab(nil, Namespace.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
    else
    OpenFileInTab(Namespace.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect);
  end
  else if (Shift = [ssShift,ssMiddle]) or (Shift = [ssShift,ssAlt,ssLeft]) then
  begin
    if Namespace.Folder and not IsSameText(Namespace.Extension,'.zip') then
    OpenFolderInTab(nil, Namespace.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
    else
    OpenFileInTab(Namespace.NameForParsing, not MainForm.TabSet.Settings.OpenTabSelect);
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Item
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.Refresh(OnlyIfLocal: Boolean = false);
var
  PIDL: PItemIDList;
begin
  if assigned(Namespace) then
  begin
    if OnlyIfLocal and Namespace.IsNetworkNeighborhoodChild then
    begin
      Exit;
    end;

    FreeAndNil(Namespace);
  end;

  if fIsPIDL then
  begin
    PIDL:= LoadPIDLFromMime(fPIDL);
  end
  else if fIsSpecial then
  begin
    SHGetspecialFolderLocation(0, fSpecialFolderID, PIDL);
  end
  else
  begin
    if fRelative then
    PIDL:= PathToPIDL(DecodeRelativePath(fPath))
    else
    PIDL:= PathToPIDL(fPath);
  end;

  ImageList:= SmallSysImages;

  if PIDLMgr.IsDesktopFolder(PIDL) then
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
  end
  else
  begin
    Namespace:= TNamespace.Create(PIDL,nil);
    if Namespace.IsDesktop then
    begin
      FreeAndNil(Namespace);
      fGhosted:= true;
      ImageList:= CE_Images.BookmarkImages;
    end
    else
    begin
      fGhosted:= false;
    end;
  end;
  fLastIconRefresh:= GetTickCount;
end;

{*------------------------------------------------------------------------------
  SetRelative
-------------------------------------------------------------------------------}
procedure TCENormalItemComp.SetRelative(const Value: Boolean);
begin
  fRelative:= Value;
end;

{-------------------------------------------------------------------------------
  Supports DragDrop?
-------------------------------------------------------------------------------}
function TCENormalItemComp.SupportsDragDrop: Boolean;
begin
  Result:= fSupportsDragDrop;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCESessionComp object.
-------------------------------------------------------------------------------}
constructor TCESessionComp.Create;
begin
  inherited;
  fTitle:= 'Session';
  fSubMenuOnly:= true;
  fImageList:= CE_Images.SmallIcons;
end;

{*------------------------------------------------------------------------------
  Assign values from component
-------------------------------------------------------------------------------}
procedure TCESessionComp.Assign(From: TCECustomBookComp);
begin
  if not assigned(From) then Exit;
  Inherited;

  if From is TCESessionComp then
  fSessionName:= TCESessionComp(From).SessionName;
end;

{*------------------------------------------------------------------------------
  Assign values to component
-------------------------------------------------------------------------------}
procedure TCESessionComp.AssignTo(ToComp: TCECustomBookComp);
begin
  if not assigned(ToComp) then Exit;
  Inherited;

  if ToComp is TCESessionComp then
  TCESessionComp(ToComp).SessionName:= fSessionName;
end;

{*------------------------------------------------------------------------------
  Get ImageIndex
-------------------------------------------------------------------------------}
function TCESessionComp.GetImageIndex(Open: Boolean = false; Overlay: Boolean
    = false): Integer;
begin
  if Overlay then
  Result:= -1
  else
  Result:= 41;
end;

{*------------------------------------------------------------------------------
  Load values from xml node.
-------------------------------------------------------------------------------}
procedure TCESessionComp.LoadFromXmlNode(XmlNode: TJvSimpleXmlElem);
begin
  inherited;
  fSessionName:= XmlNode.Properties.Value('session');
end;

{*------------------------------------------------------------------------------
  Handle mouse click
-------------------------------------------------------------------------------}
procedure TCESessionComp.MouseClick(Shift: TShiftState; Button: TMouseButton;
    SingleClickMode: Boolean = false);
begin
  if Shift = [ssDouble,ssLeft] then
  begin
    OpenSession;
  end;
end;

{-------------------------------------------------------------------------------
  Open Session
-------------------------------------------------------------------------------}
procedure TCESessionComp.OpenSession;
var
  session: TCESessionItem;
begin
  session:= GlobalSessions.Sessions.FindSession(fSessionName);
  if assigned(session) then
  GlobalSessions.ActiveSession:= session;
end;

{*------------------------------------------------------------------------------
  Save values to xml node.
-------------------------------------------------------------------------------}
procedure TCESessionComp.SaveToXmlNode(XmlNode: TJvSimpleXmlElem);
begin
  inherited;
  XmlNode.Properties.Add('session', fSessionName);
end;

{##############################################################################}

initialization
  CEBookCompList.RegisterBookComp('category', TCECategoryComp);
  CEBookCompList.RegisterBookComp('item', TCENormalItemComp);
  CEBookCompList.RegisterBookComp('session', TCESessionComp);

end.
