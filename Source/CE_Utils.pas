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
//  The Original Code is CE_Utils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Utils;

interface

uses
  // JCL
  JclFileUtils, JclMime,
  // TNT Controls
  TntActnList, TntSysUtils, TntSystem, TntWindows, TntClasses,
  // VSTools
  MPCommonUtilities, MPCommonObjects, MPShellUtilities,
  // System Units
  SysUtils, Classes, Windows, StrUtils, ShlObj, ShellAPI, Forms, Controls,
  Registry, WideStrUtils, Consts, Menus;

type
  TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP, wvWin2003, wvWinVista);

  function DecodeRelativePath(Path: WideString): WideString;
  function EncodeRelativePath(Path: WideString): WideString;
  function FindAction(ActionList: TTntActionList; ActionName: String): TTntAction;
  function IsInsideRect(ARect: TRect; X, Y: Integer): Boolean;
  procedure ReplaceChar(var S: string; Orig: Char; Replacement: Char);
  function CEStrToRect(S: String; Sep: String = ','): TRect;
  function CERectToStr(ARect: TRect; Sep: String = ','): String;
  function CEPointToStr(P: TPoint; Sep: String = ','): String;
  function CEStrToPoint(S: String; Sep: String = ','): TPoint;
  function GetIconIndex(FilePath: WideString): Integer;
  function GetWinVersion: TWinVersion;
  procedure EmptyRecycleBin(NoConfirmation: Boolean = false; NoProgressGUI:
      Boolean = false; NoSound: Boolean = false);
  function IsSameText(Str1: WideString; Str2: WideString; CaseSensitive: Boolean
      = false): Boolean;
  function StringToPIDL(aStr: String): PItemIDList;
  procedure ReplaceSystemVariablePath(var Path: WideString);
  function GetFileVersionBuild(Path: WideString): Integer;
  function GetRealVisibility(Comp: TWinControl): Boolean;
  function UseConnection(RemotePath: String; Handle: HWND): Integer;
  function IsUNC(Path: WideString): Boolean;
  function WideGetDriveType(lpRootPathName: WideString): Integer;
  function BrowseForFolderPIDL(aTitle: WideString): PItemIDList;
  function GetIsWindows64: Boolean;

function GetLargeShellIconSize: Integer;

function GetSmallShellIconSize: Integer;

function WideStringMatch(ASource: WideString; APattern: WideString;
    ACaseSensitive: Boolean = false): Boolean;

function IsWindowsVista: Boolean;

function IsWindows64: Boolean;

function ShortCutToTextRaw(ShortCut: TShortCut): string;

function GetSpecialName(ShortCut: TShortCut): string;

procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
  external user32 Name 'SwitchToThisWindow';

var
  ExePath: WideString;
  SettingsDirPath: WideString;
  ReadOnlySettings: Boolean;
  LargeShellIconSize, SmallShellIconSize: Integer;
  CE_SHLockShared: function(Handle: THandle; DWord: DWord): Pointer; stdcall;
  CE_SHUnlockShared: function (Pnt: Pointer): BOOL; stdcall;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

function ShiftState2Modifier(const Shift: TShiftState):Word;

function GetShortCutKey(ShortCut: TShortCut):Word;

function GetShortCutModifier(ShortCut: TShortCut):Word;

function GetSettingsFolderPath(var IsReadOnly: Boolean; ACreate: Boolean):
    WideString;

function GetAppVersionStr: string;

function GetShiftState: TShiftState;

function GetSystemProxyServer(Protocol: String = 'http'): String;

function ExtractUrlPort(Address: String; var Port: Integer): String;

  function CleanDateTimeStr(AStr: WideString): String;

function GetWinMajorVersion: Integer;

function IsWindowsAdmin: Boolean;

function ForceForegroundWindow(hwnd: THandle): Boolean;

procedure ForceForegroundWindow2(hwnd: THandle);

function FindDialogWindow(AProcessID: DWORD = 0): HWND;

var
  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);


implementation

var
  fIsWindowsVista: Boolean;
  fIsWindows64: Boolean;
  
{*------------------------------------------------------------------------------
  Decode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function DecodeRelativePath(Path: WideString): WideString;
var
  ws: WideString;
begin
  ws:= WideGetCurrentDir;
  WideSetCurrentDir(ExePath);
  Result:= WideExpandFileName(Path);
  WideSetCurrentDir(ws);
end;

{*------------------------------------------------------------------------------
  Encode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function EncodeRelativePath(Path: WideString): WideString;
var
  ws: WideString;
begin
  ws:= WideGetCurrentDir;
  WideSetCurrentDir(ExePath);
  Result:= WideExtractRelativePath(WideParamStr(0), Path);
  WideSetCurrentDir(ws);
end;

{*------------------------------------------------------------------------------
  Find action from ActionList
-------------------------------------------------------------------------------}
function FindAction(ActionList: TTntActionList; ActionName: String): TTntAction;
var
  i: Integer;
begin
  Result:= nil;
  if ActionName = '' then
  Exit;
  
  for i:= 0 to ActionList.ActionCount - 1 do
  begin
    if SameText(ActionList.Actions[i].Name, ActionName) then
    begin
      Result:= TTntAction(ActionList.Actions[i]);
      Break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Check if coordinates are inside rect
-------------------------------------------------------------------------------}
function IsInsideRect(ARect: TRect; X, Y: Integer): Boolean;
begin
  Result:= false;
  if (X > ARect.Left) and (X < ARect.Right) then
    if (Y > ARect.Top) and (Y < ARect.Bottom) then
    Result:= true;
end;

{*------------------------------------------------------------------------------
  Replace char
-------------------------------------------------------------------------------}
procedure ReplaceChar(var S: string; Orig: Char; Replacement: Char);
var
  i,c: Integer;
begin
  c:= Length(S);
  for i:= 1 to c do
  begin
    if S[i] = Orig then
    S[i]:= Replacement;
  end;
end;

{*------------------------------------------------------------------------------
  String to Rect (in format: 'left,top,right,bottom')
-------------------------------------------------------------------------------}
function CEStrToRect(S: String; Sep: String = ','): TRect;
var
  i,c: Integer;
  tmpS: String;
begin
  // Left
  i:= 1;
  c:= Pos(Sep,S);
  tmpS:= copy(S,i,c-1);
  Result.Left:= StrToIntDef(tmpS,0);
  // Top
  i:= c+1;
  c:= PosEx(Sep,S,i);
  tmpS:= copy(S,i,c-i);
  Result.Top:= StrToIntDef(tmpS,0);
  // Right
  i:= c+1;
  c:= PosEx(Sep,S,i);
  tmpS:= copy(S,i,c-i);
  Result.Right:= StrToIntDef(tmpS,0);
  // Bottom
  i:= c+1;
  tmpS:= copy(S,i,Length(S));
  Result.Bottom:= StrToIntDef(tmpS,0);
end;

{*------------------------------------------------------------------------------
  Rect to String (in format: 'left,top,right,bottom')
-------------------------------------------------------------------------------}
function CERectToStr(ARect: TRect; Sep: String = ','): String;
begin
  Result:= IntToStr(ARect.Left) + Sep +
           IntToStr(ARect.Top) + Sep +
           IntToStr(ARect.Right) + Sep +
           IntToStr(ARect.Bottom);
end;

{*------------------------------------------------------------------------------
  Point to String (in format: 'X,Y')
-------------------------------------------------------------------------------}
function CEPointToStr(P: TPoint; Sep: String = ','): String;
begin
  Result:= IntToStr(P.X) + Sep + IntToStr(P.Y);
end;

{*------------------------------------------------------------------------------
 String to Point (in format: 'X,Y')
-------------------------------------------------------------------------------}
function CEStrToPoint(S: String; Sep: String = ','): TPoint;
var
  i,c: Integer;
  tmpS: String;
begin
  // X
  i:= 1;
  c:= Pos(Sep,S);
  tmpS:= copy(S,i,c-1);
  Result.X:= StrToIntDef(tmpS,0);
  // Y
  i:= c+1;
  tmpS:= copy(S,i,Length(S));
  Result.Y:= StrToIntDef(tmpS,0);
end;

{*------------------------------------------------------------------------------
  Get file's icon index
-------------------------------------------------------------------------------}
function GetIconIndex(FilePath: WideString): Integer;
var
  Flags: integer;
  InfoA: TSHFileInfoA;
  InfoW: TSHFileInfoW;
begin
  Flags := SHGFI_SYSICONINDEX or SHGFI_SHELLICONSIZE or SHGFI_SMALLICON;

  if IsUnicode then
  begin
    FillChar(InfoW, SizeOf(InfoW), #0);
    if SHGetFileInfoW_MP(PWideChar(FilePath), 0, InfoW, SizeOf(InfoW), Flags) <> 0 then
      Result:= InfoW.iIcon
    else
      Result:= 0
  end
  else
  begin
    FillChar(InfoA, SizeOf(InfoA), #0);
    if SHGetFileInfoA(PChar(String(FilePath)), 0, InfoA, SizeOf(InfoA), Flags) <> 0 then
      Result:= InfoA.iIcon
    else
      Result:= 0
  end
end;

{*------------------------------------------------------------------------------
  Get Windows Version
-------------------------------------------------------------------------------}
function GetWinVersion: TWinVersion;
var
   osVerInfo: TOSVersionInfo;
   majorVersion, minorVersion: Integer;
begin
   Result := wvUnknown;
   osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
   if GetVersionEx(osVerInfo) then
   begin
     minorVersion := osVerInfo.dwMinorVersion;
     majorVersion := osVerInfo.dwMajorVersion;
     case osVerInfo.dwPlatformId of
       VER_PLATFORM_WIN32_NT:
       begin
         if majorVersion <= 4 then
         Result:= wvWinNT
         else if (majorVersion = 5) and (minorVersion = 0) then
         Result:= wvWin2000
         else if (majorVersion = 5) and (minorVersion = 1) then
         Result:= wvWinXP
         else if (majorVersion = 5) and (minorVersion = 2) then
         Result:= wvWin2003
         else if (majorVersion = 6) then
         Result:= wvWinVista;
       end;
       VER_PLATFORM_WIN32_WINDOWS:
       begin
         if (majorVersion = 4) and (minorVersion = 0) then
         Result:= wvWin95
         else if (majorVersion = 4) and (minorVersion = 10) then
         begin
           if osVerInfo.szCSDVersion[1] = 'A' then
           Result:= wvWin98SE
           else
           Result:= wvWin98;
         end
         else if (majorVersion = 4) and (minorVersion = 90) then
         Result := wvWinME
         else
         Result := wvUnknown;
       end;
     end;
   end;
end;

{-------------------------------------------------------------------------------
  Get WinMajorVersion
-------------------------------------------------------------------------------}
function GetWinMajorVersion: Integer;
var
   osVerInfo: TOSVersionInfo;
begin
  osVerInfo.dwOSVersionInfoSize:= SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  Result:= osVerInfo.dwMajorVersion
  else
  Result:= -1;
end;

{*------------------------------------------------------------------------------
  Empty Recycle Bin
-------------------------------------------------------------------------------}
procedure EmptyRecycleBin(NoConfirmation: Boolean = false; NoProgressGUI:
    Boolean = false; NoSound: Boolean = false);
const
  SHERB_NOCONFIRMATION = $00000001;
  SHERB_NOPROGRESSUI = $00000002;
  SHERB_NOSOUND = $00000004;
type
  TSHEmptyRecycleBin = function(Wnd: HWND;
                                pszRootPath: PChar;
                                dwFlags: DWORD): HRESULT;  stdcall;
var
  SHEmptyRecycleBin: TSHEmptyRecycleBin;
  LibHandle: THandle;
  flags: DWORD;
begin
  LibHandle := LoadLibrary(PChar('Shell32.dll'));
  if LibHandle <> 0 then
  begin
    @SHEmptyRecycleBin:= GetProcAddress(LibHandle, 'SHEmptyRecycleBinA')
  end
  else
  begin
    Exit;
  end;

  if @SHEmptyRecycleBin <> nil then
  begin
    flags:= 0;
    if NoConfirmation then
    flags:= flags or SHERB_NOCONFIRMATION;
    if NoProgressGUI then
    flags:= flags or SHERB_NOPROGRESSUI;
    if NoSound then
    flags:= flags or SHERB_NOSOUND;
    SHEmptyRecycleBin(Application.MainFormHandle,
                      nil,
                      flags);
  end;
  FreeLibrary(LibHandle);
  @SHEmptyRecycleBin := nil;
end;

{*------------------------------------------------------------------------------
  Is Same Text?
-------------------------------------------------------------------------------}
function IsSameText(Str1: WideString; Str2: WideString; CaseSensitive: Boolean
    = false): Boolean;
begin
  if CaseSensitive then
  Result:= WideCompareStr(Str1, str2) = 0
  else
  Result:= WideCompareText(Str1, str2) = 0;
end;

{*------------------------------------------------------------------------------
  Get ItemIDList from text ('PID of explorer:PIDL')
-------------------------------------------------------------------------------}
function StringToPIDL(aStr: String): PItemIDList;
var
  map, pid: Cardinal;
  p: Pointer;
  list: TStrings;
begin
  Result:= nil;
  if assigned(CE_SHLockShared) and assigned(CE_SHUnlockShared) then
  begin
    list:= TStringList.Create;
    try
      list.Delimiter:= ':';
      list.DelimitedText:= aStr;
      if list.Count >= 3 then
      begin
        map:= StrToIntDef(list.Strings[1], 0);
        pid:= StrToIntDef(list.Strings[2], 0);
        p:= CE_SHLockShared(map, pid);
        if p <> nil then
        begin
          try
            Result:= PIDLMgr.CopyPIDL(PItemIDList(p));
          finally
            CE_SHUnlockShared(p);
          end;
        end;
      end;
    finally
      list.Free;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Replace System Variable inside Path
-------------------------------------------------------------------------------}
procedure ReplaceSystemVariablePath(var Path: WideString);

  function ReplacePath(Path, Variable, VarPath: WideString): WideString;
  begin
    Result := Tnt_WideStringReplace(Path, Variable, VarPath, [rfReplaceAll, rfIgnoreCase])
  end;

begin
  /// TODO: Optimize this method. 

  // Psudo Variables
  Path := ReplacePath(Path, '%sysdir%', WideLowerCase(WideStripTrailingBackslash(SystemDirectory)));
  //Path := ReplacePath(Path, '%temp%', WideLowerCase(WideStripTrailingBackslash(WideGetTempDir)));
  //Path := ReplacePath(Path, '%appdata%', WideLowerCase(WideStripTrailingBackslash(UserDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%favorites%', WideLowerCase(WideStripTrailingBackslash(FavoritesFolder.NameForParsing)));
  if assigned(MyDocumentsFolder) then
  Path := ReplacePath(Path, '%personal%', WideLowerCase(WideStripTrailingBackslash(MyDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%templates%', WideLowerCase(WideStripTrailingBackslash(TemplatesFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%history%', WideLowerCase(WideStripTrailingBackslash(HistoryFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%desktopfolder%', WideLowerCase(WideStripTrailingBackslash(PhysicalDesktopFolder.NameForParsing)));
  Path := ReplacePath(Path, '%cedrive%', WideLowerCase(WideStripTrailingBackslash(WideExtractFileDrive(ExePath))));

  // Environment variables
  Path:= WideExpandEnviromentStringForUser(Path);
  Path:= WideExpandEnviromentString(Path);

//  // Environment variables
//  Path := ReplacePath(Path, '%userprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%USERPROFILE%')));
//  Path := ReplacePath(Path, '%allusersprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%ALLUSERSPROFILE%')));
//  Path := ReplacePath(Path, '%programfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramFiles%')));
//  Path := ReplacePath(Path, '%systemroot%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemRoot%')));
//  Path := ReplacePath(Path, '%systemdrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemDrive%')));
//  Path := ReplacePath(Path, '%windir%', WideStripTrailingBackslash(WideExpandEnviromentString('%windir%')));
//  Path := ReplacePath(Path, '%tmp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TMP%')));
//  Path := ReplacePath(Path, '%temp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TEMP%')));
//  Path := ReplacePath(Path, '%public%', WideStripTrailingBackslash(WideExpandEnviromentString('%PUBLIC%')));
//  Path := ReplacePath(Path, '%programdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramData%')));
//  Path := ReplacePath(Path, '%homedrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEDRIVE%')));
//  Path := ReplacePath(Path, '%homepath%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEPATH%')));
//  Path := ReplacePath(Path, '%commonprogramfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%CommonProgramFiles%')));
//  Path := ReplacePath(Path, '%appdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%APPDATA%')));
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Load DLL procs
-------------------------------------------------------------------------------}
procedure CELoadShellProcs;
var
  hDLL: HMODULE;
begin
  hDLL:= GetModuleHandle('shlwapi.dll');
  if hDLL <> 0 then
  begin
    CE_SHLockShared:= GetProcAddress(hDLL, PChar(8));
    CE_SHUnlockShared:= GetProcAddress(hDLL, PChar(9));
  end;
end;

{-------------------------------------------------------------------------------
  Get File Version Build
-------------------------------------------------------------------------------}
function GetFileVersionBuild(Path: WideString): Integer;
var
  Size, FixInfoLen: DWORD;
  Handle: THandle;
  Buffer: WideString;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result:= 0;
  Size:= Tnt_GetFileVersionInfoSizeW(PWideChar(Path), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    if Tnt_GetFileVersionInfoW(PWideChar(Path), Handle, Size, Pointer(Buffer)) and
      Tnt_VerQueryValueW(Pointer(Buffer), DirDelimiter, Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result:= LoWord(FixInfoBuf^.dwProductVersionLS)
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Real Visibility
-------------------------------------------------------------------------------}
function GetRealVisibility(Comp: TWinControl): Boolean;
var
  p: TWinControl;
begin
  Result:= false;
  p:= Comp;
  while assigned(p) do
  begin
    Result:= p.Visible;
    if not Result then
    break
    else
    begin
      p:= p.Parent;
      if not assigned(p) then
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Use UNC connection
-------------------------------------------------------------------------------}
function UseConnection(RemotePath: String; Handle: HWND): Integer;
var
  nRes: TNetResource;
  flags: Cardinal;
  size, res: Cardinal;
begin
  FillChar(nRes, SizeOf(nRes), #0);
  nRes.lpRemoteName:= PChar(RemotePath);
  nRes.dwType:= RESOURCETYPE_ANY;
  nRes.lpLocalName:= nil;
  nRes.lpProvider:= nil;
  size:= 0;
  flags:= CONNECT_INTERACTIVE;
  Result:= WNetUseConnection(Handle,
                              nRes,
                              nil,
                              nil,
                              flags,
                              nil,
                              size,
                              res);
end;

{-------------------------------------------------------------------------------
  is UNC path
-------------------------------------------------------------------------------}
function IsUNC(Path: WideString): Boolean;
begin
  if Length(Path) > 1 then
  Result:= ((Path[1] = '\') and (Path[2] = '\'))
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  GetDriveType (unicode)
-------------------------------------------------------------------------------}
function WideGetDriveType(lpRootPathName: WideString): Integer;
begin
  if Win32PlatformIsUnicode then
  Result:= GetDriveTypeW(PWideChar(lpRootPathName))
  else
  Result:= GetDriveTypeA(PAnsiChar(AnsiString(lpRootPathName)));
end;

{-------------------------------------------------------------------------------
  Show BrowseForFolder dialog (returns PIDL)
-------------------------------------------------------------------------------}
function BrowseForFolderPIDL(aTitle: WideString): PItemIDList;
var
  info: TBrowseInfoW;
begin
  FillChar(info, SizeOf(info), 0);
  info.hwndOwner:= Application.ActiveFormHandle;
  info.ulFlags:= BIF_USENEWUI;
  info.lpszTitle:= PWideChar(aTitle);
  Result:= Tnt_SHBrowseForFolderW(info);
end;

{-------------------------------------------------------------------------------
  Get Small Shell Icon Size
-------------------------------------------------------------------------------}
function GetSmallShellIconSize: Integer;
var
  reg: TRegistry;
begin
  Result:= 16;
  reg:= TRegistry.Create(KEY_READ);
  try
    reg.RootKey:= HKEY_CURRENT_USER;
    reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False);
    if reg.ValueExists('Shell Small Icon Size') then
    begin
      try
      case reg.GetDataType('Shell Small Icon Size') of
        rdString, rdExpandString: Result:= StrToIntDef(reg.ReadString('Shell Small Icon Size'), GetSystemMetrics(SM_CXSMICON));
        rdInteger, rdUnknown, rdBinary: Result:= reg.ReadInteger('Shell Small Icon Size');
      end;
      except
        Result:= GetSystemMetrics(SM_CXSMICON);
      end;
    end
    else
    Result:= GetSystemMetrics(SM_CXSMICON);
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Get Large Shell Icon Size
-------------------------------------------------------------------------------}
function GetLargeShellIconSize: Integer;
var
  reg: TRegistry;
begin
  Result:= 32;
  reg:= TRegistry.Create(KEY_READ);
  try
    reg.RootKey:= HKEY_CURRENT_USER;
    reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False);
    if reg.ValueExists('Shell Icon Size') then
    begin
      try
      case reg.GetDataType('Shell Icon Size') of
        rdString, rdExpandString: Result:= StrToIntDef(reg.ReadString('Shell Icon Size'), GetSystemMetrics(SM_CXICON));
        rdInteger, rdUnknown, rdBinary: Result:= reg.ReadInteger('Shell Icon Size');
      end;
      except
        Result:= GetSystemMetrics(SM_CXICON);
      end;
    end
    else
    Result:= GetSystemMetrics(SM_CXICON);
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Wide String Match
-------------------------------------------------------------------------------}
function WideStringMatch(ASource: WideString; APattern: WideString;
    ACaseSensitive: Boolean = false): Boolean;

  function DoMatch(source, pattern: PWideChar): Boolean;
  begin
    if 0 = WStrComp(pattern,'*') then
      Result:= True
    else if (source^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result:= False
    else if source^ = Chr(0) then
      Result:= True
    else begin
      case pattern^ of
      '*': if DoMatch(source,@pattern[1]) then
             Result:= True
           else
             Result:= DoMatch(@source[1],pattern);
      '?': Result:= DoMatch(@source[1],@pattern[1]);
      else
        if ACaseSensitive then
        begin
          if source^ = pattern^ then
          Result:= DoMatch(@source[1],@pattern[1])
          else
          Result:= False;
        end
        else
        begin
          if CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, source, 1, pattern, 1) = 2 then
          Result:= DoMatch(@source[1],@pattern[1])
          else
          Result:= False;
        end;
      end;
    end;
  end;

begin
  Result:= DoMatch(PWideChar(ASource), PWideChar(APattern));
end;

{-------------------------------------------------------------------------------
  Is Windows Vista And Up
-------------------------------------------------------------------------------}
function IsWindowsVista: Boolean;
begin
  Result:= fIsWindowsVista;
end;

{-------------------------------------------------------------------------------
  Get Is Windows 64
-------------------------------------------------------------------------------}
function GetIsWindows64: Boolean;
type
  TIsWow64Process = function(AHandle:THandle; var AIsWow64: BOOL): BOOL; stdcall;
var
  fKernel32Handle: DWORD;
  fIsWow64Process: TIsWow64Process;
  fIsWow64       : BOOL;
begin
  Result:= False;

  fKernel32Handle:= LoadLibrary('kernel32.dll');
  if (fKernel32Handle = 0) then Exit;

  try
    @fIsWow64Process:= GetProcAddress(fKernel32Handle, 'IsWow64Process');
    if not Assigned(fIsWow64Process) then Exit;

    fIsWow64:= False;
    if (fIsWow64Process(GetCurrentProcess, fIsWow64)) then
    Result:= fIsWow64;
  finally
    FreeLibrary(fKernel32Handle);
  end;
end;

{-------------------------------------------------------------------------------
  Is Windows 64
-------------------------------------------------------------------------------}
function IsWindows64: Boolean;
begin
  Result:= fIsWindows64;
end;

{-------------------------------------------------------------------------------
  ShortCutToText (returns Ctrl,Shift,Alt also)
-------------------------------------------------------------------------------}
function ShortCutToTextRaw(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name:= MenuKeyCaps[mkcEnter];
    $10..$12: begin
      Name:= '';
    end;
    $1B: Name:= MenuKeyCaps[mkcEsc];
    $20..$28: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name:= Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name:= Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name:= Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name:= 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name:= GetSpecialName(ShortCut);
  end;

  Result := '';
  if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
  if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
  if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
  Result:= Result + Name;
end;

{-------------------------------------------------------------------------------
  Get Special Name of Shortcut
-------------------------------------------------------------------------------}
function GetSpecialName(ShortCut: TShortCut): string;
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
begin
  Result:= '';
  ScanCode:= MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    Result:= KeyName;
  end;
end;

{-------------------------------------------------------------------------------
  ShiftState2Modifier
-------------------------------------------------------------------------------}
function ShiftState2Modifier(const Shift: TShiftState):Word;
begin
  Result := 0;
  if ssShift in Shift then
    Result := Result or MOD_SHIFT;
  if ssAlt in Shift then
    Result := Result or MOD_ALT;
  if ssCtrl in Shift then
    Result := Result or MOD_CONTROL;
end;

{-------------------------------------------------------------------------------
  GetShortCutKey
-------------------------------------------------------------------------------}
function GetShortCutKey(ShortCut: TShortCut):Word;
var
  shift: TShiftState;
begin
  ShortCutToKey(ShortCut,Result,shift);
end;

{-------------------------------------------------------------------------------
  GetShortCutModifier
-------------------------------------------------------------------------------}
function GetShortCutModifier(ShortCut: TShortCut):Word;
var
  key: Word;
  shift: TShiftState;
begin
  ShortCutToKey(ShortCut,key,shift);
  Result := ShiftState2Modifier(shift);
end; 

{-------------------------------------------------------------------------------
  Get SettingsFolderPath
-------------------------------------------------------------------------------}
function GetSettingsFolderPath(var IsReadOnly: Boolean; ACreate: Boolean):
    WideString;
var
  list: TTntStrings;
  i, p: Integer;
  ws: WideString;
begin
  Result:= '';
  IsReadOnly:= false;
  if WideFileExists(exePath + 'settings.path') then
  begin
    list:= TTntStringList.Create;
    try
      list.LoadFromFile(exePath + 'settings.path');
      for i:= 0 to list.Count - 1 do
      begin
        ws:= Trim(list.Strings[i]);
        if Length(ws) > 0 then
        begin
          if ws[1] <> ';' then
          begin
            p:= Pos('[READONLY]', ws);
            IsReadOnly:= p = 1;
            if not IsReadOnly then
            begin
              ReplaceSystemVariablePath(ws);
              Result:= DecodeRelativePath(ws);
              if ACreate then
              begin
                if not WideDirectoryExists(Result) then
                begin
                  if not WideCreateDir(Result) then
                  Result:= '';
                end;
              end;
            end
            else
            begin
              if Length(ws) > 10 then
              begin
                ws:= Copy(ws, 11, Length(ws) - 11);
                ReplaceSystemVariablePath(ws);
                Result:= DecodeRelativePath(ws);
              end
              else
              Result:= '';
            end;
            Break;
          end;
        end;
      end;
    finally
      list.Free;
    end;
  end;

  if Result = '' then
  Result:= ExePath;

  Result:= WideIncludeTrailingBackslash(Result);
end;

{-------------------------------------------------------------------------------
  Get AppVersionStr
-------------------------------------------------------------------------------}
function GetAppVersionStr: string;
var
  ver: TJclFileVersionInfo;
begin
  ver:= TJclFileVersionInfo.Create(Application.ExeName);
  try
    Result:= ver.FileVersion;
  finally
    ver.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Get ShiftState
-------------------------------------------------------------------------------}
function GetShiftState: TShiftState;
begin
  Result := [];
  if GetAsyncKeyState(VK_SHIFT) < 0 then
  Include(Result, ssShift);
  if GetAsyncKeyState(VK_CONTROL) < 0 then
  Include(Result, ssCtrl);
  if GetAsyncKeyState(VK_MENU) < 0 then
  Include(Result, ssAlt);
end;

{-------------------------------------------------------------------------------
  Get SystemProxyServer
-------------------------------------------------------------------------------}
function GetSystemProxyServer(Protocol: String = 'http'): String;
var
  i, j: Integer;
  Handle: HKey;
  Buffer: array[0..256] of Char;
  BufSize: Integer;
  DataType: Integer;
  ProxyServer: String;
begin
  ProxyServer:= '';

  // Get ProxyServer
  if RegOpenKeyEx(HKEY_CURRENT_USER,
                  'SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings',
                  0, KEY_READ, Handle) = ERROR_SUCCESS then
  begin
    BufSize := SizeOf(Buffer);
    DataType := reg_sz;
    if RegQueryValueEx(Handle, 'ProxyServer', nil, @DataType, @Buffer, @BufSize) = ERROR_SUCCESS then
    ProxyServer:= Buffer;

    RegCloseKey(Handle);
  end;

  // Extract address by protocol
  if ProxyServer <> '' then
  begin
    i:= Pos(Protocol + '=', ProxyServer);
    if (i > 0) then
    begin
      Delete(ProxyServer, 1, i+Length(Protocol));
      j:= Pos(';', ProxyServer);
      if (j > 0) then
      ProxyServer:= Copy(ProxyServer, 1, j-1);
    end;
  end;
  Result:= ProxyServer;
end;

{-------------------------------------------------------------------------------
  ExtractUrlPort
-------------------------------------------------------------------------------}
function ExtractUrlPort(Address: String; var Port: Integer): String;
var
  i: Integer;
begin
  i:= Pos('://', Address);
  if i > 0 then
  i:= i + 3
  else
  i:= 1;
  
  i:= PosEx(':', Address, i);
  if (i > 0) then
  begin
    Port:= StrToIntDef(Copy(Address, i+1, Length(Address)-i), 0);
    Result:= Copy(Address, 1, i-1);
  end
  else
  Result:= Address;
end;

{-------------------------------------------------------------------------------
  Clean DateTime String
-------------------------------------------------------------------------------}
function CleanDateTimeStr(AStr: WideString): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 1 to Length(AStr) do
  begin
    if (Ord(AStr[i]) > 31) and (Ord(AStr[i]) < 127) then
    Result:= Result + AStr[i];
  end;
end;

{-------------------------------------------------------------------------------
  IsWindowsAdmin
-------------------------------------------------------------------------------}
function IsWindowsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5)) ;
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  g: Integer;
  bSuccess: BOOL;
begin
  Result:= False;

  bSuccess:= OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
    bSuccess:= OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  end;

  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess:= GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
      for g := 0 to ptgGroups.GroupCount - 1 do
      if EqualSid(psidAdministrators, ptgGroups.Groups[g].Sid) then
      begin
        Result:= True;
        Break;
      end;
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

{-------------------------------------------------------------------------------
  ForceForegroundWindow
-------------------------------------------------------------------------------}
function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
      // Converted to Delphi by Ray Lischner
      // Published in The Delphi Magazine 55, page 16

      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;
end;

{-------------------------------------------------------------------------------
  ForceForegroundWindow2
-------------------------------------------------------------------------------}
procedure ForceForegroundWindow2(hwnd: THandle);
var
  hlp: TForm;
begin
  hlp:= TForm.Create(nil);
  try
    hlp.BorderStyle := bsNone;
    hlp.SetBounds(0, 0, 1, 1);
    hlp.FormStyle := fsStayOnTop;
    hlp.Show;
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    SetForegroundWindow(hwnd);
  finally
    hlp.Free;
  end;
end;

{-------------------------------------------------------------------------------
  FindDialogWindow (if AProcessID = 0, MainThread is used)
-------------------------------------------------------------------------------}
var
  fDlgWindow: HWND;
  
  function EnumWindowsProc(hHwnd: HWND; lParam : integer): boolean; stdcall;
  var
    pid : DWORD;
    ClassName : string;
  begin
    if (hHwnd=0) then
    begin
      Result:= false;
    end
    else
    begin
      GetWindowThreadProcessId(hHwnd, pid);

      if pid <> DWORD(lParam) then
      begin
        Result:= true;
        Exit;
      end;

      SetLength(ClassName, 255);
      SetLength(ClassName, GetClassName(hHwnd, PChar(ClassName), 255));

      if ClassName = '#32770' then
      begin
        if IsWindowVisible(hHwnd) then
        fDlgWindow:= hHwnd;
      end;
      
      Result:= fDlgWindow = 0;
    end;
  end;

function FindDialogWindow(AProcessID: DWORD = 0): HWND;
var
  pid: DWORD;
begin
  fDlgWindow:= 0;
  if AProcessID <> 0 then
  pid:= AProcessID
  else
  GetWindowThreadProcessId(Application.MainFormHandle, pid);
  EnumWindows(@EnumWindowsProc, pid);
  Result:= fDlgWindow;
end;

{##############################################################################}

initialization
  ExePath:= WideExtractFilePath(WideParamStr(0));
  SettingsDirPath:= GetSettingsFolderPath(ReadOnlySettings, true);
  LargeShellIconSize:= GetLargeShellIconSize;
  SmallShellIconSize:= GetSmallShellIconSize;
  CELoadShellProcs;
  fIsWindowsVista:= GetWinVersion = wvWinVista;
  fIsWindows64:= GetIsWindows64;

finalization

end.
