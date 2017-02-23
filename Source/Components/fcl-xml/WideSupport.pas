unit WideSupport;

interface

uses
  Classes, SysUtils, Windows;

type
  TWideFileStream = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word);
    destructor Destroy; override;
  end;

function WideCreateFile(lpFileName: PWideChar; dwDesiredAccess, dwShareMode:
    DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition,
    dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;

function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;

implementation

function WideCreateFile(lpFileName: PWideChar; dwDesiredAccess, dwShareMode:
    DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition,
    dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CreateFileW(lpFileName, dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else
    Result := CreateFileA(PAnsiChar(AnsiString(lpFileName)), dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
end;

function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(WideCreateFile(PWideChar(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

constructor TWideFileStream.Create(const FileName: WideString; Mode: Word);
var
  CreateHandle: Integer;
begin
  if Mode = fmCreate then
  begin
    CreateHandle := Integer(WideCreateFile(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
    if CreateHandle < 0 then
    begin
      raise EFCreateError.CreateFmt('Cannot create file %s', [FileName]);
    end;
  end else
  begin
    CreateHandle := WideFileOpen(FileName, Mode);
    if CreateHandle < 0 then
    begin
      raise EFOpenError.CreateFmt('Cannot open file %s', [FileName]);
    end;
  end;
  inherited Create(CreateHandle);
end;

destructor TWideFileStream.Destroy;
begin
  if Handle >= 0 then
  FileClose(Handle);
end;

end.
