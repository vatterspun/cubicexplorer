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

unit CE_FileUtils;

interface

uses
  // CE Units
  CE_CommonObjects,
  // JCL
  JclMime,
  // Tnt
  TntSysUtils, TntWindows,
  // VSTools
  MPShellUtilities, MPCommonObjects, MPCommonUtilities,
  // System Units
  Windows, SysUtils, Classes, ShlObj;

const
  INVALID_FILE_ATTRIBUTES = DWORD(-1);

  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;

  REPARSE_MOUNTPOINT_HEADER_SIZE = 8;

  IO_REPARSE_TAG_RESERVED_ZERO  = $000000000;
  IO_REPARSE_TAG_SYMBOLIC_LINK  = IO_REPARSE_TAG_RESERVED_ZERO;
  IO_REPARSE_TAG_RESERVED_ONE   = $000000001;
  IO_REPARSE_TAG_RESERVED_RANGE = $000000001;
  IO_REPARSE_TAG_VALID_VALUES   = $0E000FFFF;
  IO_REPARSE_TAG_HSM            = $0C0000004;
  IO_REPARSE_TAG_NSS            = $080000005;
  IO_REPARSE_TAG_NSSRECOVER     = $080000006;
  IO_REPARSE_TAG_SIS            = $080000007;
  IO_REPARSE_TAG_DFS            = $080000008;
  IO_REPARSE_TAG_MOUNT_POINT    = $0A0000003;

  FILE_ANY_ACCESS = 0;
  FILE_READ_DATA  = 1;
  FILE_WRITE_DATA = 2;

  FILE_DEVICE_FILE_SYSTEM = $0009;

  METHOD_BUFFERED   = 0;
  METHOD_IN_DIRECT  = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER    = 3;    

  FSCTL_SET_REPARSE_POINT    = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (41 shl 2) or (METHOD_BUFFERED);
  FSCTL_GET_REPARSE_POINT    = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (42 shl 2) or (METHOD_BUFFERED);
  FSCTL_DELETE_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (43 shl 2) or (METHOD_BUFFERED);

type
  TCreateSymbolicLink = function(Link, Target: PWideChar; Flags: DWORD): BOOL; stdcall;

  REPARSE_MOUNTPOINT_DATA_BUFFER = packed record
    ReparseTag: DWORD;
    ReparseDataLength: DWORD;
    Reserved: Word;
    ReparseTargetLength: Word;
    ReparseTargetMaximumLength: Word;
    Reserved1: Word;
    ReparseTarget: array[0..0] of WideChar;
  end;
  TReparseMountPointDataBuffer = REPARSE_MOUNTPOINT_DATA_BUFFER;
  PReparseMountPointDataBuffer = ^TReparseMountPointDataBuffer;

  TCEPathType = (ptPath, ptPIDL, ptSpecial, ptUnknown);

  function PIDLToCEPath(APIDL: PItemIDList): WideString;
  function CEPathToPIDL(APath: WideString): PItemIDList;
  function PIDLExists(APIDL: PItemIDList): Boolean;
  function SavePIDLToMime(APIDL: PItemIDList): String;
  function LoadPIDLFromMime(MimeStr: String): PItemIDList;
  function NamespaceToCEPath(ANamespace: TNamespace; out PathType: TCEPathType):
    WideString;
  function GetCEPathType(APath: WideString): TCEPathType;
  function CEPathExists(APath: WideString; APathType: TCEPathType): Boolean;
  function PathExists(Path: WideString): Boolean;
  function CEPathToNamespace(APath: WideString; APathType: TCEPathType):
    TNamespace;
  function CreateJunction(ALink: WideString; ATarget: WideString; ShowErrors:
    Boolean = true): Boolean;
  function FileOrFolderExists(APath: WideString): Boolean;

function IsEmptyFolder(ANamespace: TNamespace): Boolean;

function GetRedirectedPath(APath: WideString): WideString;

function RegisterDefaultFileManager(AName, ADescription: String; AFilePath:
    WideString): Boolean;

function UnRegisterDefaultFileManager(AName: String): Boolean;

function IsDefaultFileManager(AName: String): Boolean;

implementation

uses
  TntRegistry, CE_Utils;

{-------------------------------------------------------------------------------
  PIDL to CEPath
-------------------------------------------------------------------------------}
function PIDLToCEPath(APIDL: PItemIDList): WideString;
var
  folderID: Integer;
  pidl: PItemIDList;
  ns: TNamespace;
begin
  Result:= '';
  if not assigned(APIDL) then Exit;
  // Get Special folder ID
  if PIDLMgr.IsDesktopFolder(APIDL) then
  folderID:= 0
  else
  folderID:= CE_SpecialNamespaces.GetSpecialID(APIDL);

  // Special Folder ID
  if folderID > -1 then
  Result:= 'special://' + IntToStr(folderID)
  else
  begin
    ns:= TNamespace.Create(APIDL, nil);
    ns.FreePIDLOnDestroy:= false;
    try
      pidl:= PathToPIDL(ns.NameForParsing);
      // Normal Path
      if assigned(pidl) then
      begin
        Result:= ns.NameForParsing;
        PIDLMgr.FreeAndNilPIDL(pidl);
      end
      // PIDL
      else
      begin
        Result:= 'pidl://' + SavePIDLToMime(APIDL);
      end;
    finally
      ns.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CE Path to PIDL
-------------------------------------------------------------------------------}
function CEPathToPIDL(APath: WideString): PItemIDList;
var
  ws: WideString;
  c, folderID: Integer;
begin
  Result:= nil;
  c:= Length(APath);
  if c = 0 then Exit;
  // PIDL
  if Pos('pidl://', APath) = 1 then
  begin
    if c > 7 then
    begin
      ws:= Copy(APath, 8, c - 7);
      Result:= LoadPIDLFromMime(ws);
    end;
  end
  // Special folder id
  else if Pos('special://', APath) = 1 then
  begin
    if c > 10 then
    begin
      ws:= Copy(APath, 11, c - 10);
      folderID:= StrToIntDef(ws, -1);
      if folderID > -1 then
      SHGetspecialFolderLocation(0, folderID, Result);
    end;
  end
  // Normal Path
  else
  begin
    Result:= PathToPIDL(APath);
  end;
end;

{-------------------------------------------------------------------------------
  PIDLExists
-------------------------------------------------------------------------------}
function PIDLExists(APIDL: PItemIDList): Boolean;
var
  desktop, parent: IShellFolder;
  relativePIDL, pidl: PItemIDList;
  enum: IEnumIDList;
  c: Cardinal;
begin
  Result:= false;
  if not assigned(APIDL) then
  Exit;
  
  // Get parent IShellFolder
  SHGetDesktopFolder(desktop);
  if PIDLMgr.IDCount(APIDL) > 1 then
  begin
    pidl:= PIDLMgr.StripLastID(PIDLMgr.CopyPIDL(APIDL));
    desktop.BindToObject(pidl, nil, IID_IShellFolder, Pointer(parent));
    PIDLMgr.FreePIDL(pidl);
  end
  else
  parent:= desktop;

  // Enum folder objects
  if assigned(parent) then
  begin
    relativePIDL:= PIDLMgr.GetPointerToLastID(APIDL);
    if parent.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, enum) = S_OK then
    begin
      while enum.Next(1, pidl, c) = NOERROR do
      begin
        if PIDLMgr.EqualPIDL(relativePIDL, pidl) then
        begin
          Result:= true;
          break;
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save PIDL to Mime encoded string
-------------------------------------------------------------------------------}
function SavePIDLToMime(APIDL: PItemIDList): String;
var
  stream: TStream;
  buf: AnsiString;
begin
  Result:= '';
  stream:= TMemoryStream.Create;
  try
    PIDLMgr.SaveToStream(stream, APIDL);
    SetLength(buf,stream.Size);
    stream.Position:= 0;
    stream.Read(buf[1],stream.Size);
    Result:= MimeEncodeStringNoCRLF(buf);
  finally
    stream.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Load PIDL from Mime encoded string
-------------------------------------------------------------------------------}
function LoadPIDLFromMime(MimeStr: String): PItemIDList;
var
  stream: TStream;
  buf: AnsiString;
begin
  stream:= TMemoryStream.Create;
  try
    buf:= MimeDecodeString(MimeStr);
    stream.Write(buf[1],Length(buf));
    stream.Position:= 0;
    Result:= PIDLMgr.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Namespace to CEPath
-------------------------------------------------------------------------------}
function NamespaceToCEPath(ANamespace: TNamespace; out PathType: TCEPathType):
    WideString;
var
  folderID: Integer;
  pidl: PItemIDList;
begin
  Result:= '';
  if not assigned(ANamespace) then Exit;
  // Get Special folder ID
  if PIDLMgr.IsDesktopFolder(ANamespace.AbsolutePIDL) then
  folderID:= 0
  else
  folderID:= CE_SpecialNamespaces.GetSpecialID(ANamespace.AbsolutePIDL);

  // Special Folder ID
  if folderID > -1 then
  begin
    Result:= 'special://' + IntToStr(folderID);
    PathType:= ptSpecial;
  end
  else
  begin
    pidl:= PathToPIDL(ANamespace.NameForParsing);
    // Normal Path
    if assigned(pidl) then
    begin
      Result:= ANamespace.NameForParsing;
      PIDLMgr.FreeAndNilPIDL(pidl);
      PathType:= ptPath;
    end
    // PIDL
    else
    begin
      Result:= 'pidl://' + SavePIDLToMime(ANamespace.AbsolutePIDL);
      PathType:= ptPIDL;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get CEPath Type
-------------------------------------------------------------------------------}
function GetCEPathType(APath: WideString): TCEPathType;
begin
  if Pos('pidl://', APath) = 1 then
  Result:= ptPIDL
  else if Pos('special://', APath) = 1 then
  Result:= ptSpecial
  else
  Result:= ptPath;       
end;

{-------------------------------------------------------------------------------
  CEPathExists
-------------------------------------------------------------------------------}
function CEPathExists(APath: WideString; APathType: TCEPathType): Boolean;
var
  c, folderID: Integer;
  ws: WideString;
  pidl: PItemIDList;
begin
  Result:= false;
  if APathType = ptUnknown then
  APathType:= GetCEPathType(APath);
  // Path
  if APathType = ptPath then
  begin
    pidl:= PathToPIDL(APath);
    if assigned(pidl) then
    begin
      Result:= true;
      PIDLMgr.FreePIDL(pidl);
    end;
  end
  // PIDL
  else if APathType = ptPIDL then
  begin
    Result:= PIDLExists(CEPathToPIDL(APath));
  end
  // Special
  else if APathType = ptSpecial then
  begin
    if Pos('special://', APath) = 1 then //
    begin
      c:= Length(APath);
      if c > 10 then
      begin
        ws:= Copy(APath, 11, c - 10);
        folderID:= StrToIntDef(ws, -1);
        if folderID > -1 then
        begin
          Result:= SHGetspecialFolderLocation(0, folderID, pidl) = S_OK;
          PIDLMgr.FreePIDL(pidl);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Check if Path Exists
-------------------------------------------------------------------------------}
function PathExists(Path: WideString): Boolean;
begin
  if IsUnicode then
  Result:= GetFileAttributesW(PWideChar(Path)) <> INVALID_FILE_ATTRIBUTES
  else
  Result:= GetFileAttributesA(PChar(String(Path))) <> INVALID_FILE_ATTRIBUTES;
end;

{-------------------------------------------------------------------------------
  CEPath to Namespace
-------------------------------------------------------------------------------}
function CEPathToNamespace(APath: WideString; APathType: TCEPathType):
    TNamespace;
begin
  if APathType = ptPath then
  Result:= TNamespace.CreateFromFileName(APath)
  else
  Result:= TNamespace.Create(CEPathToPIDL(APath), nil);
end;

{-------------------------------------------------------------------------------
  Create Junction
-------------------------------------------------------------------------------}
function CreateJunction(ALink: WideString; ATarget: WideString; ShowErrors:
    Boolean = true): Boolean;
var
  h: Cardinal;
  proc: TCreateSymbolicLink;
  flag: Cardinal;
  s: String;
  sr: TSearchRecW;
  Buffer: PReparseMountPointDataBuffer;
  BufSize: integer;
  TargetName: WideString;
  BytesRead: DWORD;
begin
  Result:= false;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    ATarget:= WideExcludeTrailingPathDelimiter(ATarget);
    ALink:= WideExcludeTrailingPathDelimiter(ALink);
    // Pre check: Make sure Link doesn't exist.
    if WideFindFirst(ALink, faAnyFile or faDirectory, sr) = 0 then
    begin
      if ShowErrors then
      MessageBox(0, 'File/Folder already exists.'#13#10'Please choose another link name.', 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
      WideFindClose(sr);
      Exit;
    end;

    // Pre checks: Make sure target exists. Is target folder of a file?
    if WideFindFirst(ATarget, faAnyFile or faDirectory, sr) = 0 then
    begin
      if (sr.Attr and faDirectory) = faDirectory then
      flag:= 1
      else
      flag:= 0;
      WideFindClose(sr);
    end
    else
    begin
      if ShowErrors then
      MessageBox(0, 'Target does not exist.', 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
      Exit;
    end;

    // Create Symbolic Link in Vista and Win7
    if not false and (Win32MajorVersion >= 6) then
    begin
      h:= GetModuleHandle('kernel32.dll');
      if h <> 0 then
      begin
        @proc:= GetProcAddress(h, 'CreateSymbolicLinkW');
        if assigned(Proc) then
        begin
          Result:= proc(PWideChar(ALink), PWideChar(ATarget), flag);
          if not Result and ShowErrors then // Show error dialog if needed
          begin
            s:= SysErrorMessage(GetLastError);
            MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
          end;
        end;
      end;
    end
    // Create Symbolic Link in 2000 and XP
    else if (Win32MajorVersion > 4) then
    begin
      // Create Link folder
      if not CreateDirectoryW(PWideChar(ALink), nil) then
      begin
        if ShowErrors then
        begin
          s:= SysErrorMessage(GetLastError);
          MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
        end;
        Exit;
      end;

      // Open Link folder
      h:= CreateFileW(PWideChar(ALink),
                      GENERIC_WRITE,
                      0,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,
                      0);
      if h = INVALID_HANDLE_VALUE then
      begin
        if ShowErrors then
        begin
          s:= SysErrorMessage(GetLastError);
          MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
        end;
        RemoveDirectoryW(PWideChar(ALink));
        Exit;
      end
      else
      begin
        // Create mount point for Link folder
        TargetName:= '\??\' + ATarget;
        BufSize:= (Length(ATarget)+5) * 2 + REPARSE_MOUNTPOINT_HEADER_SIZE+12;
        GetMem(Buffer, BufSize);
        FillChar(Buffer^, BufSize, 0);

        Move(TargetName[1], Buffer^.ReparseTarget, (Length(TargetName)+1) * 2);
        Buffer^.ReparseTag:= IO_REPARSE_TAG_MOUNT_POINT;
        Buffer^.ReparseTargetLength:= Length(TargetName)*2;
        Buffer^.ReparseTargetMaximumLength:= Buffer^.ReparseTargetLength+2;
        Buffer^.ReparseDataLength:= Buffer^.ReparseTargetLength+12;

        BytesRead:= 0;
        Result:= DeviceIoControl(h,
                                 FSCTL_SET_REPARSE_POINT,
                                 Buffer,
                                 Buffer^.ReparseDataLength+REPARSE_MOUNTPOINT_HEADER_SIZE,
                                 nil,
                                 0,
                                 BytesRead,
                                 nil);
        FreeMem(Buffer);
        CloseHandle(h);

        if not Result then
        begin
          RemoveDirectoryW(PWideChar(ALink));
          if ShowErrors then
          begin
            s:= SysErrorMessage(GetLastError);
            MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
          end;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Check if File Or Folder Exists
-------------------------------------------------------------------------------}
function FileOrFolderExists(APath: WideString): Boolean;
var
  sr: TSearchRecW;
begin
  APath:= WideExcludeTrailingPathDelimiter(APath);
  Result:= WideFindFirst(APath, faAnyFile or faDirectory, sr) = 0;
  if Result then
  WideFindClose(sr);
end;

{-------------------------------------------------------------------------------
  IsEmptyFolder
-------------------------------------------------------------------------------}
function IsEmptyFolder(ANamespace: TNamespace): Boolean;
const
  SHCONTF_INCLUDESUPERHIDDEN = $10000;
  SHCONTF_CHECKING_FOR_CHILDREN = $10;
begin
  if assigned(ANamespace) then
  Result:= not ANamespace.SubItemsEx(SHCONTF_INCLUDEHIDDEN or
                                     SHCONTF_NONFOLDERS or
                                     SHCONTF_FOLDERS or
                                     SHCONTF_INCLUDESUPERHIDDEN or
                                     SHCONTF_CHECKING_FOR_CHILDREN)
  else
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Get RedirectedPath
-------------------------------------------------------------------------------}
function GetRedirectedPath(APath: WideString): WideString;
var
  prog, vstore: WideString;
  subPath: WideString;
  c, c2: Integer;
begin
  Result:= APath;

  prog:= WideIncludeTrailingBackslash(WideExpandEnviromentString('%ProgramFiles%'));
  if Pos(prog, APath) = 1 then
  begin
    c:= Length(prog);
    c2:= Length(APath);
    subPath:= Copy(APath, c+1, c2 - c);
    if subPath <> '' then
    begin
      vstore:= WideExpandEnviromentString('%LocalAppData%' + '\VirtualStore\Program Files\' + subPath);
      if WideFileExists(vstore) then
      Result:= vstore;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Register Default File Manager (Vista+ needs elevated privileges)
-------------------------------------------------------------------------------}
function RegisterDefaultFileManager(AName, ADescription: String; AFilePath:
    WideString): Boolean;
var
  reg: TTntRegistry;
  cmd: WideString;
  oldValue: String;
begin
  Result:= false;
  reg:= TTntRegistry.Create;
  try
    reg.RootKey:= HKEY_CLASSES_ROOT;

    // HKEY_CLASSES_ROOT\Folder\shell
    if reg.OpenKey('\Folder\shell', false) then 
    begin
      oldValue:= reg.ReadString('');
      if oldValue = AName then
      oldValue:= '';

      // HKEY_CLASSES_ROOT\Folder\shell\[AName]
      if reg.OpenKey(AName, true) then 
      begin
        reg.WriteString('', ADescription);
        reg.WriteString('OldDefaultValue', oldValue);

        // HKEY_CLASSES_ROOT\Folder\shell\[AName]\command
        if reg.OpenKey('command', true) then 
        begin
          if Win32MajorVersion = 5 then
          cmd:= '"' + AFilePath + '" /idlist,%I,%L' // 2000, XP, 2003
          else
          cmd:= '"' + AFilePath + '" /shell "%1"';  // Vista and Win7
          
          reg.WriteExpandString('', cmd);
          
          // HKEY_CLASSES_ROOT\Folder\shell
          if reg.OpenKey('\Folder\shell', false) then 
          begin
            reg.WriteString('', AName);
            Result:= true;
          end;
        end;
      end;
    end;
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  UnRegister Default File Manager (Vista+ needs elevated privileges)
-------------------------------------------------------------------------------}
function UnRegisterDefaultFileManager(AName: String): Boolean;
var
  reg: TTntRegistry;
  oldValue: string;
begin
  Result:= false;
  reg:= TTntRegistry.Create;
  try
    reg.RootKey:= HKEY_CLASSES_ROOT;

    if not reg.KeyExists('\Folder\shell\' + AName) then
    begin
      Result:= true;
      Exit; // -->
    end;

    // HKEY_CLASSES_ROOT\Folder\shell\[AName]
    if reg.OpenKey('\Folder\shell\' + AName, false) then
    begin
      oldValue:= reg.ReadString('OldDefaultValue');
      if oldValue = AName then
      oldValue:= '';

      // HKEY_CLASSES_ROOT\Folder\shell
      if reg.OpenKey('\Folder\shell', false) then
      begin
        reg.WriteString('', oldValue);
        Result:= reg.DeleteKey('\Folder\shell\' + AName);
      end;
    end;
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Is Default File Manager? (Vista+ needs elevated privileges)
-------------------------------------------------------------------------------}
function IsDefaultFileManager(AName: String): Boolean;
var
  reg: TTntRegistry;
begin
  Result:= false;
  reg:= TTntRegistry.Create;
  try
    // HKEY_CLASSES_ROOT\Folder\shell
    if reg.OpenKey('\Folder\shell', false) then
    begin
      Result:= reg.ReadString('') = AName;
    end;
  finally
    reg.Free;
  end;
end;

end.
