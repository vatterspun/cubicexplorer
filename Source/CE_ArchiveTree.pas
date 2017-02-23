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
//  The Original Code is CE_ArchiveTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ArchiveTree;

interface

uses
  // VT
  VirtualTrees,
  // JCL
  JclCompressionWide,
  // TNT
  TntClasses,
  // System Units
  Classes, SysUtils, Windows, Forms, ImgList, Messages, Graphics;

type
  PItemData = ^AItemData;
  AItemData = record
    fLastIconRefresh: Integer;
    Name: WideString;
    Kind: TJclCompressionItemKind;
    ImageIndex: Integer;
    Path: WideString;
    IsFolderUp: Boolean;
    Size: Int64;
    PackedSize: Int64;
    Item: TJclCompressionItem;
  end;

  TCEArchiveTree = class;

  TCEArchivePathChangeEvent = procedure(Sender: TCEArchiveTree; Path: WideString) of object;
  TCEArchiveProgressEvent = procedure(Sender: TObject; const Value, MaxValue: Int64) of Object;

  TCEArchiveTree = class(TVirtualStringTree)
  private
    fArchive: TJclCompressionArchive;
    fCheckBoxSelection: Boolean;
    fCurrentFolderNode: PVirtualNode;
    fLastIconRefresh: Integer;
    fLibraryMissing: Boolean;
    fOnCurrentFolderChange: TCEArchivePathChangeEvent;
    fOnProgress: TCEArchiveProgressEvent;
    fShowFilePathInFlat: Boolean;
    fShowFolderUpNode: Boolean;
    fSingleFolderMode: Boolean;
    function GetCurrentFolder: WideString; virtual;
    procedure SetCheckBoxSelection(const Value: Boolean); virtual;
    procedure SetCurrentFolder(const Value: WideString); virtual;
    procedure SetCurrentFolderNode(const Value: PVirtualNode); virtual;
    procedure SetShowFolderUpNode(const Value: Boolean); virtual;
    procedure SetSingleFolderMode(const Value: Boolean); virtual;
  protected
    fCreateSubDirs: Boolean;
    fDestDir: WideString;
    fFolderIconIndex: Integer;
    procedure AddColumns; virtual;
    procedure DoAfterPaint(Canvas: TCanvas); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
        override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
        TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
        override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: UnicodeString); override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure UpdateScrollBarHeight; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckAll; virtual;
    procedure CloseArchive; virtual;
    procedure ExtractCheckedTo(ADestDir: WideString; ACreateSubDirs: Boolean =
        true);
    procedure ExtractTo(ADestDir: WideString; ACreateSubDirs: Boolean = true);
        virtual;
    function FindNodeByPath(APath: WideString): PVirtualNode; virtual;
    procedure GoFolderUp; virtual;
    function OpenArchive(AFilePath: WideString; AsFlat: Boolean = false): Boolean;
        virtual;
    procedure PopulateAsFlat; virtual;
    procedure PopulateAsTree; virtual;
    procedure RefreshItemSelection;
    function VerifyExtract(ADestinationDir: WideString; AConflictingPaths:
        TTntStrings; AOverridesOnly: Boolean = true; AIgnorePathNotFound: Boolean =
        true; ACheckedOnly: Boolean = false): Integer;
    property Archive: TJclCompressionArchive read fArchive;
    property CurrentFolder: WideString read GetCurrentFolder write SetCurrentFolder;
    property CurrentFolderNode: PVirtualNode read fCurrentFolderNode write
        SetCurrentFolderNode;
    property LibraryMissing: Boolean read fLibraryMissing;
  published
    property CheckBoxSelection: Boolean read fCheckBoxSelection write
        SetCheckBoxSelection;
    property ShowFilePathInFlat: Boolean read fShowFilePathInFlat write
        fShowFilePathInFlat;
    property ShowFolderUpNode: Boolean read fShowFolderUpNode write
        SetShowFolderUpNode;
    property SingleFolderMode: Boolean read fSingleFolderMode write
        SetSingleFolderMode;
    property OnCurrentFolderChange: TCEArchivePathChangeEvent read
        fOnCurrentFolderChange write fOnCurrentFolderChange;
    property OnProgress: TCEArchiveProgressEvent read fOnProgress write fOnProgress;
  end;

  function FileAttributeToStr(AFileAttr: Cardinal): String;
  function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;

implementation

uses
  MPCommonUtilities, ShellAPI, MPCommonObjects, TntSysUtils, CE_Utils;

{-------------------------------------------------------------------------------
  FileTime To DateTime
-------------------------------------------------------------------------------}
function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
var
  st: TSystemTime;
begin
  if FileTimeToLocalFileTime(AFileTime, AFileTime) then
  begin
    if FileTimeToSystemTime(AFileTime, st) then
    Result:= SystemTimeToDateTime(st)
    else
    Result:= 0
  end
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  File Attribute To String
-------------------------------------------------------------------------------}
function FileAttributeToStr(AFileAttr: Cardinal): String;
const
  FILE_ATTRIBUTE_READONLY = 1;
  FILE_ATTRIBUTE_HIDDEN = 2;
  FILE_ATTRIBUTE_SYSTEM = 4;
  FILE_ATTRIBUTE_DIRECTORY = 16;
  FILE_ATTRIBUTE_ARCHIVE = 32;
  FILE_ATTRIBUTE_ENCRYPTED = 64;
  FILE_ATTRIBUTE_NORMAL = 128;
  FILE_ATTRIBUTE_TEMPORARY = 256;
  FILE_ATTRIBUTE_SPARSE_FILE = 512;
  FILE_ATTRIBUTE_REPARSE_POINT = 1024;
  FILE_ATTRIBUTE_COMPRESSED = 2048;
  FILE_ATTRIBUTE_OFFLINE = 4096;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 8192;
begin
  Result:= '';
  if AFileAttr and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then Result:= Result + 'R';
  if AFileAttr and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then Result:= Result + 'H';
  if AFileAttr and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then Result:= Result + 'S';
  if AFileAttr and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then Result:= Result + 'D';
  if AFileAttr and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then Result:= Result + 'A';
  if AFileAttr and FILE_ATTRIBUTE_ENCRYPTED = FILE_ATTRIBUTE_ENCRYPTED then Result:= Result + 'E';
  if AFileAttr and FILE_ATTRIBUTE_NORMAL = FILE_ATTRIBUTE_NORMAL then Result:= Result + 'N';
  if AFileAttr and FILE_ATTRIBUTE_TEMPORARY = FILE_ATTRIBUTE_TEMPORARY then Result:= Result + 'T';
  if AFileAttr and FILE_ATTRIBUTE_SPARSE_FILE = FILE_ATTRIBUTE_SPARSE_FILE then Result:= Result + 'P';
  if AFileAttr and FILE_ATTRIBUTE_REPARSE_POINT = FILE_ATTRIBUTE_REPARSE_POINT then Result:= Result + 'L';
  if AFileAttr and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then Result:= Result + 'C';
  if AFileAttr and FILE_ATTRIBUTE_OFFLINE = FILE_ATTRIBUTE_OFFLINE then Result:= Result + 'O';
  if AFileAttr and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = FILE_ATTRIBUTE_NORMAL then Result:= Result + 'I';
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEArchiveTree
-------------------------------------------------------------------------------}
constructor TCEArchiveTree.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize:= SizeOf(AItemData);
  BorderStyle:= bsNone;
  Self.Images:= SmallSysImages;
  AddColumns;
  Self.Header.Options:= [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  Self.TreeOptions.AutoOptions:= [toAutoDropExpand,toAutoScrollOnExpand,toAutoSort,toAutoTristateTracking,toAutoDeleteMovedNodes];
  //Self.TreeOptions.MiscOptions:= [toAcceptOLEDrop,toCheckSupport,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick,toWheelPanning,toEditOnClick];
  Self.TreeOptions.SelectionOptions:= [toMultiSelect];
  fShowFilePathInFlat:= false;
  fSingleFolderMode:= false;
  fLibraryMissing:= not WideFileExists(ExePath + '7z.dll');
end;

{-------------------------------------------------------------------------------
  Destroy TCEArchiveTree
-------------------------------------------------------------------------------}
destructor TCEArchiveTree.Destroy;
begin
  CloseArchive;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Columns
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.AddColumns;
var
  col: TVirtualTreeColumn;
begin
  Self.Header.Columns.BeginUpdate;
  try
    Self.Header.Columns.Clear;
    // Name
    col:= Self.Header.Columns.Add;
    col.Text:= 'Name';
    col.Width:= 150;
    // Size
    col:= Self.Header.Columns.Add;
    col.Text:= 'Size';
    col.Width:= 75;
    col.Alignment:= taRightJustify;
    // Packed Size
    col:= Self.Header.Columns.Add;
    col.Text:= 'Packed Size';
    col.Width:= 75;
    col.Alignment:= taRightJustify;
    // Modified
    col:= Self.Header.Columns.Add;
    col.Text:= 'Modified';
    col.Width:= 100;
    // Created
    col:= Self.Header.Columns.Add;
    col.Text:= 'Created';
    col.Width:= 100;
    // Accessed
    col:= Self.Header.Columns.Add;
    col.Text:= 'Accessed';
    col.Width:= 100;
    // Attributes
    col:= Self.Header.Columns.Add;
    col.Text:= 'Attributes';
    col.Width:= 75;
    col.Alignment:= taRightJustify;
    // Encrypted
    col:= Self.Header.Columns.Add;
    col.Text:= 'Encrypted';
    col.Width:= 75;
    col.Alignment:= taRightJustify;
    // CRC
    col:= Self.Header.Columns.Add;
    col.Text:= 'CRC';
    col.Width:= 100;
    col.Alignment:= taRightJustify;
    // Method
    col:= Self.Header.Columns.Add;
    col.Text:= 'Method';
    col.Width:= 100;
    // Host OS
    col:= Self.Header.Columns.Add;
    col.Text:= 'Host OS';
    col.Width:= 100;
    // Path
    col:= Self.Header.Columns.Add;
    col.Text:= 'Path';
    col.Width:= 200;
  finally
    Self.Header.SortColumn:= 0;
    Self.Header.SortDirection:= sdAscending;
    Self.Header.Columns.EndUpdate;
  end;
end;

procedure TCEArchiveTree.CheckAll;
var
  node: PVirtualNode;
begin
  node:= Self.GetFirst;
  while assigned(node) do
  begin
    Self.CheckState[node]:= csCheckedNormal;
    node:= Self.GetNext(node);
  end;
end;

{-------------------------------------------------------------------------------
  Do GetText
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: UnicodeString);
var
  data: PItemData;

begin
  data:= Self.GetNodeData(Node);
  Text:= '';

  //  0 = Name
  //  1 = Size
  //  2 = Packed Size
  //  3 = Modified
  //  4 = Created
  //  5 = Accessed
  //  6 = Attributes
  //  7 = Encrypted
  //  8 = CRC
  //  9 = Method
  //  10 = Host OS
  //  11 = Path

  case Column of
    // Name
    -1,0: begin
      if data.IsFolderUp then
      Text:= '..'
      else
      Text:= data.Name;
    end;
    // Size
    1: begin
      if data.Kind = ikDirectory then
      Text:= IntToStr(data.Size)
      else if assigned(data.item) then
      Text:= IntToStr(data.item.FileSize);
    end;
    // Packed Size
    2: begin
      if data.Kind = ikDirectory then
      Text:= IntToStr(data.PackedSize)
      else if assigned(data.item) then
      Text:= IntToStr(data.item.PackedSize);
    end;
    // Modified
    3: if assigned(data.item) then Text:= DateTimeToStr(FileTimeToDateTime(data.item.LastWriteTime));
    // Created
    4: if assigned(data.item) then Text:= DateTimeToStr(FileTimeToDateTime(data.item.CreationTime));
    // Accessed
    5: if assigned(data.item) then Text:= DateTimeToStr(FileTimeToDateTime(data.item.LastAccessTime));
    // Attributes
    6: if assigned(data.item) then Text:= FileAttributeToStr(data.item.Attributes);
    // Encrypted
    7: if assigned(data.item) then
    begin
      if data.item.Encrypted then
      Text:= 'Yes';
    end;
    // CRC
    8: if assigned(data.item) then
    begin
      if data.item.CRC <> 0 then
      Text:= IntToHex(data.item.CRC, 8)
      else
      Text:= '';
    end;
    // Method
    9: if assigned(data.item) then Text:= data.item.Method;
    // Host OS
    10: if assigned(data.item) then Text:= data.item.HostOS;
    // Path
    11: if assigned(data.item) then Text:= data.Path;
  end;
end;

{-------------------------------------------------------------------------------
  CloseArchive
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.CloseArchive;
begin
  fCurrentFolderNode:= nil;
  Self.Clear;
  if assigned(Archive) then
  FreeAndNil(fArchive);
end;

{-------------------------------------------------------------------------------
  Do AfterPaint
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.DoAfterPaint(Canvas: TCanvas);
var
  s: String;
  r: TRect;
begin
  inherited;
  if fLibraryMissing then
  begin
    s:= '7z.dll is missing!';
    r:= Self.ClientRect;
    Canvas.Font.Color:= clGrayText;
    Canvas.TextRect(r, s, [tfCenter, tfSingleLine, tfVerticalCenter]);
  end;
end;

{-------------------------------------------------------------------------------
  Do Compare
-------------------------------------------------------------------------------}
function TCEArchiveTree.DoCompare(Node1, Node2: PVirtualNode; Column:
    TColumnIndex): Integer;
var
  data1, data2: PItemData;
  e: Extended;
begin
  Result:= 0;
  //  0 = Name
  //  1 = Size
  //  2 = Packed Size
  //  3 = Modified
  //  4 = Created
  //  5 = Accessed
  //  6 = Attributes
  //  7 = Encrypted
  //  8 = CRC
  //  9 = Method
  //  10 = Host OS
  //  11 = Path

  data1:= Self.GetNodeData(Node1);
  data2:= Self.GetNodeData(Node2);
  
  // Folder up node. Keep always at top
  if data1.IsFolderUp and not data2.IsFolderUp then
  begin
    if Self.Header.SortDirection = sdAscending then
    Result:= -1
    else
    Result:= 1
  end
  else if not data1.IsFolderUp and data2.IsFolderUp then
  begin
    if Self.Header.SortDirection = sdAscending then
    Result:= 1
    else
    Result:= -1
  end
  // Folder node. Keep folders at top
  else if data1.Kind <> data2.Kind then
  begin
    if Self.Header.SortDirection = sdAscending then
    Result:= Ord(data2.Kind) - Ord(data1.Kind)
    else
    Result:= Ord(data1.Kind) - Ord(data2.Kind)
  end
  // Sort normal nodes
  else if (Column = 0) then // Name
  begin
    Result:= Ord(data2.Kind) - Ord(data1.Kind);
    if Result = 0 then
    Result:= WideCompareText(data1.Name, data2.Name);
  end
  else if (Column = 11) then // Path
  begin
    Result:= WideCompareText(data1.Path, data2.Path);
  end
  else if assigned(data1.item) and assigned(data2.item) then
  begin
    case Column of
      // Size
      1: Result:= data1.Size - data2.Size;
      // Packed Size
      2: Result:= data1.PackedSize - data2.PackedSize;
      3..5: begin
        e:= 0;
        case Column of
          // Modified
          3: e:= FileTimeToDateTime(data1.item.LastWriteTime) - FileTimeToDateTime(data2.item.LastWriteTime);
          // Created
          4: e:= FileTimeToDateTime(data1.item.CreationTime) - FileTimeToDateTime(data2.item.CreationTime);
          // Accessed
          5: e:= FileTimeToDateTime(data1.item.LastAccessTime) - FileTimeToDateTime(data2.item.LastAccessTime);
        end;
        if e < 0 then
        Result:= -1
        else if e > 0 then
        Result:= 1
        else
        Result:= 0;
      end;
      // Attributes
      6: Result:= data1.item.Attributes - data2.item.Attributes;
      // Encrypted
      7: Result:= Integer(data1.item.Encrypted) - Integer(data2.item.Encrypted);
      // CRC
      8: Result:= data1.item.CRC - data2.item.CRC;
      // Method
      9: Result:= WideCompareText(data1.item.Method, data2.item.Method);
      // Host OS
      10: Result:= WideCompareText(data1.item.HostOS, data2.item.HostOS);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do FreeNode
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.DoFreeNode(Node: PVirtualNode);
var
  data: PItemData;
begin
  data:= Self.GetNodeData(Node);
  data.Name:= '';
  data.Path:= '';
  inherited;
end;

{-------------------------------------------------------------------------------
  Do GetImageIndex
-------------------------------------------------------------------------------}
function TCEArchiveTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
    Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer):
    TCustomImageList;
var
  data: PItemData;
  FileInfo : SHFILEINFO;
  ext: String;
begin
  if (Kind <> ikOverlay) and ((Column = 0) or (Column = -1)) then
  begin
    data:= Self.GetNodeData(Node);
    // Get Icon index if needed
    if data.fLastIconRefresh < fLastIconRefresh then
    begin
      if (data.Kind = ikDirectory) or data.IsFolderUp then
      begin
        SHGetFileInfo('*',
                  FILE_ATTRIBUTE_DIRECTORY,
                  FileInfo,
                  SizeOf(FileInfo),
                  SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
        data.ImageIndex:= FileInfo.iIcon;
      end
      else if assigned(data.Item) then
      begin
        ext:= '*' + data.item.PackedExtension;
        SHGetFileInfo(PChar(ext),
                      FILE_ATTRIBUTE_NORMAL,
                      FileInfo,
                      SizeOf(FileInfo),
                      SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
        data.ImageIndex:= FileInfo.iIcon;
      end;
      data.fLastIconRefresh:= GetTickCount;
    end;
    Index:= data.ImageIndex;
    Ghosted:= false;
    Result:= SmallSysImages;
  end
  else
    Result:= nil;
end;

{-------------------------------------------------------------------------------
  DoHeaderClick
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column > -1 then
  begin
    Self.BeginUpdate;
    try
      if HitInfo.Column = Self.Header.SortColumn then
      begin
        if Self.Header.SortDirection = sdAscending then
        Self.Header.SortDirection:= sdDescending
        else
        Self.Header.SortDirection:= sdAscending;
      end;
      Self.Header.SortColumn:= HitInfo.Column;
    finally
      Self.EndUpdate;
    end;
  end;
  inherited;
end;

{-------------------------------------------------------------------------------
  Extract Checked To
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.ExtractCheckedTo(ADestDir: WideString; ACreateSubDirs:
    Boolean = true);
begin
  if assigned(fArchive) then
  begin
    RefreshItemSelection;
    TJclDecompressArchive(fArchive).ExtractSelected(ADestDir, ACreateSubDirs);
  end;
end;

{-------------------------------------------------------------------------------
  Extract To
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.ExtractTo(ADestDir: WideString; ACreateSubDirs:
    Boolean = true);
begin
  if assigned(fArchive) then
  begin
    fDestDir:= WideIncludeTrailingBackslash(ADestDir);
    fCreateSubDirs:= ACreateSubDirs;
    TJclDecompressArchive(fArchive).ExtractAll(ADestDir, ACreateSubDirs);
  end;
end;

{-------------------------------------------------------------------------------
  FindNodeByPath
-------------------------------------------------------------------------------}
function TCEArchiveTree.FindNodeByPath(APath: WideString): PVirtualNode;
var
  data: PItemData;
begin
  Result:= Self.GetFirst;
  while assigned(Result) do
  begin
    data:= Self.GetNodeData(Result);
    if data.Path = APath then
    Exit;
    Result:= self.GetNext(Result);
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get CurrentFolder
-------------------------------------------------------------------------------}
function TCEArchiveTree.GetCurrentFolder: WideString;
var
  data: PItemData;
begin
  Result:= '';
  if assigned(CurrentFolderNode) and (CurrentFolderNode <> Self.RootNode) then
  begin
    data:= Self.GetNodeData(CurrentFolderNode);
    Result:= data.Path;
  end;
end;

{-------------------------------------------------------------------------------
  GoFolderUp
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.GoFolderUp;
begin
  if assigned(CurrentFolderNode) then
  begin
    if CurrentFolderNode <> Self.RootNode then
    CurrentFolderNode:= CurrentFolderNode.Parent
    else
    CurrentFolderNode:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  HandleMouseDblClick
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.HandleMouseDblClick(var Message: TWMMouse; const
    HitInfo: THitInfo);
var
  data: PItemData;
begin
  Inherited;
  if SingleFolderMode and assigned(HitInfo.HitNode) then
  begin
    data:= Self.GetNodeData(HitInfo.HitNode);
    if data.IsFolderUp then
    GoFolderUp
    else if data.Kind = ikDirectory then
    CurrentFolderNode:= HitInfo.HitNode;
  end;
end;

{-------------------------------------------------------------------------------
  OpenArchive
-------------------------------------------------------------------------------}
function TCEArchiveTree.OpenArchive(AFilePath: WideString; AsFlat: Boolean =
    false): Boolean;
var
  archiveClass: TJclDecompressArchiveClass;
begin
  if fLibraryMissing then
  fLibraryMissing:= not WideFileExists('7z.dll');
  
  Result:= false;

  if fLibraryMissing then
  Exit;

  archiveClass:= GetArchiveFormats.FindDecompressFormat(AFilePath);
  if assigned(archiveClass) then
  begin
    CloseArchive;

    fArchive:= archiveClass.Create(AFilePath);
    fArchive.OnProgress:= fOnProgress;

    TJclDecompressArchive(fArchive).ListFiles;
    if AsFlat then
    PopulateAsFlat
    else
    PopulateAsTree;

    Result:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Populate As Flat
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.PopulateAsFlat;
var
  i: Integer;
  item: TJclCompressionItem;
  node: PVirtualNode;
  data: PItemData;
begin
  if not assigned(fArchive) then
  Exit;

  Self.BeginUpdate;
  try
    Self.Clear;
    for i:= 0 to fArchive.ItemCount - 1 do
    begin
      item:= fArchive.Items[i];

      // Add Node
      if item.Kind = ikFile then
      begin
        node:= Self.AddChild(nil);
        Self.CheckType[node]:= ctCheckBox;
        data:= Self.GetNodeData(node);
        if fShowFilePathInFlat then
        data.Name:= item.PackedName
        else
        data.Name:= WideExtractFileName(item.PackedName);
        data.Kind:= item.Kind;
        data.Path:= WideExtractFilePath(item.PackedName);
        data.item:= item;
        data.fLastIconRefresh:= 0;
        Self.ValidateNode(node, false);
      end;
    end;
  finally
    fLastIconRefresh:= GetTickCount;
    Self.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Populate As Tree
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.PopulateAsTree;
var
  folderList: TList;

  function GetFolderNode(APath: WideString; ACreate: Boolean): PVirtualNode;
  var
    data: PItemData;
    i: Integer;
    c: Integer;
  begin
    Result:= nil;

    c:= Length(APath);
    if c > 0 then
    begin
      for i:= 0 to folderList.Count - 1 do
      begin
        Result:= folderList.Items[i];
        data:= Self.GetNodeData(Result);
        if (Length(data.Path) = c) and (APath = data.Path) then
        Exit;
      end;
      Result:= nil;

      if ACreate then
      begin
        Result:= Self.AddChild(nil);
        Self.CheckType[Result]:= ctTriStateCheckBox;
        data:= Self.GetNodeData(Result);
        data.Name:= WideExtractFileName(WideExcludeTrailingBackslash(APath));
        data.Path:= APath;
        data.Kind:= ikDirectory;
        data.fLastIconRefresh:= 0;
        folderList.Add(Result);
      end;
    end;
  end;

var
  i: Integer;
  item: TJclCompressionItem;
  parentNode, node: PVirtualNode;
  data, parentData: PItemData;
begin
  if not assigned(fArchive) then
  Exit;

  folderList:= TList.Create;
  Self.BeginUpdate;
  try
    Self.Clear;
    for i:= 0 to fArchive.ItemCount - 1 do
    begin
      item:= fArchive.Items[i];
      // Find folder node
      if item.Kind = ikDirectory then
      node:= GetFolderNode(item.PackedName + '\', false)
      else
      node:= nil;
      // Add Node
      if not assigned(node) then
      begin
        parentNode:= GetFolderNode(WideExtractFilePath(item.PackedName), true);
        if parentNode = nil then
        parentNode:= Self.RootNode;
        node:= Self.AddChild(parentNode);
        if item.Kind = ikDirectory then
        folderList.Add(node);
      end
      else if node <> Self.RootNode then
      parentNode:= node.Parent
      else
      parentNode:= nil;
      
      if item.Kind = ikDirectory then
      Self.CheckType[node]:= ctTriStateCheckBox
      else
      Self.CheckType[node]:= ctCheckBox;
      
      data:= Self.GetNodeData(node);
      // name
      data.Name:= WideExtractFileName(item.PackedName);
      // kind
      data.Kind:= item.Kind;

      // path
      if data.Kind = ikDirectory then
      data.Path:= WideIncludeTrailingBackslash(item.PackedName)
      else
      data.Path:= item.PackedName;
      data.item:= item;
      // sizes
      if data.Kind = ikFile then
      begin
        data.Size:= item.FileSize;
        data.PackedSize:= item.PackedSize;
        // calc parent folder size
        if (data.Size > 0) or (data.PackedSize > 0) then
        begin
          while assigned(parentNode) and (parentNode <> Self.RootNode) do
          begin
            parentData:= Self.GetNodeData(parentNode);
            parentData.Size:= parentData.Size + data.Size;
            parentData.PackedSize:= parentData.PackedSize + data.PackedSize;
            parentNode:= parentNode.Parent;
          end;
        end;
      end;
      data.fLastIconRefresh:= 0;
      
      Self.ValidateNode(node, false);
    end;
  finally
    fLastIconRefresh:= GetTickCount;
    Self.EndUpdate;
    folderList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Item Selection
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.RefreshItemSelection;
var
  node: PVirtualNode;
  data: PItemData;
begin
  node:= Self.GetFirst;
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if assigned(data.Item) then
    begin
      data.Item.Selected:= Self.CheckState[node] = csCheckedNormal;
    end;
    node:= Self.GetNext(node);
  end;
end;

{-------------------------------------------------------------------------------
  Set CheckBoxSelection
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.SetCheckBoxSelection(const Value: Boolean);
begin
  if fCheckBoxSelection <> Value then
  begin
    fCheckBoxSelection:= Value;
    if fCheckBoxSelection then
    Self.TreeOptions.MiscOptions:= Self.TreeOptions.MiscOptions + [toCheckSupport]
    else
    Self.TreeOptions.MiscOptions:= Self.TreeOptions.MiscOptions - [toCheckSupport];
  end;
end;

{-------------------------------------------------------------------------------
  Set CurrentFolder
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.SetCurrentFolder(const Value: WideString);
var
  parentNode: PVirtualNode;
begin
  if Value = '' then
  parentNode:= Self.RootNode
  else
  parentNode:= FindNodeByPath(Value);

  CurrentFolderNode:= parentNode;
end;

{-------------------------------------------------------------------------------
  Set CurrentFolderNode
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.SetCurrentFolderNode(const Value: PVirtualNode);
var
  node, tmpNode: PVirtualNode;
  data: PItemData;
begin
  if not assigned(Value) then
  fCurrentFolderNode:= Self.RootNode
  else
  fCurrentFolderNode:= Value;

  if assigned(fCurrentFolderNode) and assigned(Self.GetFirst()) then
  begin
    Self.BeginUpdate;
    try
      // Hide all nodes
      node:= Self.GetFirst;
      while assigned(node) do
      begin
        data:= Self.GetNodeData(node);
        if data.IsFolderUp then // delete Folder Up nodes
        begin
          tmpNode:= node;
          node:= Self.GetNext(node);
          Self.DeleteNode(tmpNode);
        end
        else // Hide nodes
        begin
          node.States:= node.States + [vsHidden];
          node:= Self.GetNext(node);
        end;
      end;

      // Add Folder Up Node (not needed for root)
      if fShowFolderUpNode and (fCurrentFolderNode <> Self.RootNode) then
      begin
        node:= Self.InsertNode(fCurrentFolderNode, amAddChildFirst);
        data:= Self.GetNodeData(node);
        data.IsFolderUp:= true;
        data.ImageIndex:= fFolderIconIndex;
        node:= Self.GetNextSibling(node);
      end
      else
      node:= Self.GetFirstChild(fCurrentFolderNode);
      // Show child nodes
      while assigned(node) do
      begin
        node.States:= node.States - [vsHidden];
        node:= Self.GetNextSibling(node);
      end;

      Self.Expanded[fCurrentFolderNode]:= true;
    finally
      UpdateScrollBarHeight;
      Self.EndUpdate;
    end;
  end;

  if (Self.UpdateCount = 0) and assigned(fOnCurrentFolderChange) then
  fOnCurrentFolderChange(Self, CurrentFolder);
end;

{-------------------------------------------------------------------------------
  Set ShowFolderUpNode
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.SetShowFolderUpNode(const Value: Boolean);
begin
  fShowFolderUpNode:= Value;
  if SingleFolderMode then
  CurrentFolderNode:= CurrentFolderNode;
end;

{-------------------------------------------------------------------------------
  Set SingleFolderMode
-------------------------------------------------------------------------------}
procedure TCEArchiveTree.SetSingleFolderMode(const Value: Boolean);
var
  node, tmpNode: PVirtualNode;
  data: PItemData;
begin
  if fSingleFolderMode <> Value then
  begin
    fSingleFolderMode:= Value;
    Self.BeginUpdate;
    try
      if fSingleFolderMode then
      begin
        Self.TreeOptions.PaintOptions:= Self.TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines, toShowButtons];
        Self.Indent:= 0;
        CurrentFolderNode:= Self.RootNode;
      end
      else
      begin
        node:= Self.GetFirst;
        while assigned(node) do
        begin
          data:= Self.GetNodeData(node);
          if data.IsFolderUp then // delete Folder Up nodes
          begin
            tmpNode:= node;
            node:= Self.GetNext(node);
            Self.DeleteNode(tmpNode);
          end
          else // Make nodes visible
          begin
            node.States:= node.States - [vsHidden];
            node:= Self.GetNext(node);
          end;
        end;
        Self.TreeOptions.PaintOptions:= Self.TreeOptions.PaintOptions + [toShowRoot, toShowTreeLines, toShowButtons];
        Self.Indent:= 18;
        Self.FullCollapse;
      end;
    finally
      Self.EndUpdate;
    end;
  end;
end;

procedure TCEArchiveTree.UpdateScrollBarHeight;
var
  h: Integer;
  node: PVirtualNode;
begin
  // Update scrollbar size (VT doesn't do that automatically for some reason)
  h:= Self.RootNode.NodeHeight + Self.BottomSpace;
  node:= Self.GetFirstVisible;
  while assigned(node) do
  begin
    h:= h + node.NodeHeight;
    node:= Self.GetNextVisible(node);
  end;
  Self.RootNode.TotalHeight:= h;
end;

{-------------------------------------------------------------------------------
  VerifyExtract (Returns number of conflicting paths. -1 is returned when destination dir doesn't exist)
-------------------------------------------------------------------------------}
function TCEArchiveTree.VerifyExtract(ADestinationDir: WideString;
    AConflictingPaths: TTntStrings; AOverridesOnly: Boolean = true;
    AIgnorePathNotFound: Boolean = true; ACheckedOnly: Boolean = false):
    Integer;
var
  i: Integer;
  item: TJclCompressionItem;
  path: WideString;
  h: Integer;
  err: Integer;
begin
  Result:= -1;
  
  if not assigned(AConflictingPaths) then
  raise Exception.Create('AConflictingPaths has to be assigned!');

  ADestinationDir:= WideIncludeTrailingBackslash(ADestinationDir);
  if WideDirectoryExists(ADestinationDir) then
  begin
    AConflictingPaths.Clear;
    if assigned(fArchive) then
    begin
      if ACheckedOnly then
      RefreshItemSelection;

      for i:= 0 to fArchive.ItemCount - 1 do
      begin
        item:= fArchive.Items[i];
        if (item.Kind = ikFile) and (not ACheckedOnly or (ACheckedOnly and item.Selected)) then
        begin
          path:= ADestinationDir + item.PackedName;
          if WideFileExists(path) then
          begin
            h:= WideFileOpen(path, fmOpenReadWrite);
            err:= GetLastError;
            if h < 0 then
            AConflictingPaths.AddObject(path, TObject(err))
            else
            FileClose(h);
          end
          else if not AOverridesOnly then
          begin
            h:= WideFileCreate(path);
            err:= GetLastError;
            if h < 0 then
            begin
              if err <> ERROR_PATH_NOT_FOUND then
              AConflictingPaths.AddObject(path, TObject(err))
            end
            else
            FileClose(h);
            if WideFileExists(path) then
            WideDeleteFile(path);
          end;
        end;
      end;
      Result:= AConflictingPaths.Count;
    end;
  end
end;

end.
