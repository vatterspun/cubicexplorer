{------------------------------------------------------------------------------}
{                                                                              }
{  TFindFileW v4.02 - Unicode Version                                          }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{
{  - Modified by Marko Savolainen (made FileMatch and FolderChange synchronized)                                                                            }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

{$IFDEF COMPILER6_UP}
  {$WARN SYMBOL_PLATFORM OFF} // This is Win32, no warning for FindData record
{$ENDIF}

{$DEFINE UNICODE}

unit FindFileW;

interface

uses
  Windows, Messages, Classes, SysUtils, TntClasses, TntSysUtils;

const
  FILE_ATTRIBUTE_READONLY            = $00000001;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_READONLY}
  {$ENDIF}
  FILE_ATTRIBUTE_HIDDEN              = $00000002;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_HIDDEN}
  {$ENDIF}
  FILE_ATTRIBUTE_SYSTEM              = $00000004;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_SYSTEM}
  {$ENDIF}
  FILE_ATTRIBUTE_DIRECTORY           = $00000010;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_DIRECTORY}
  {$ENDIF}
  FILE_ATTRIBUTE_ARCHIVE             = $00000020;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_ARCHIVE}
  {$ENDIF}
  FILE_ATTRIBUTE_DEVICE              = $00000040;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_DEVICE}
  {$ENDIF}
  FILE_ATTRIBUTE_NORMAL              = $00000080;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_NORMAL}
  {$ENDIF}
  FILE_ATTRIBUTE_TEMPORARY           = $00000100;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_TEMPORARY}
  {$ENDIF}
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}
  {$ENDIF}
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}
  {$ENDIF}
  FILE_ATTRIBUTE_COMPRESSED          = $00000800;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_COMPRESSED}
  {$ENDIF}
  FILE_ATTRIBUTE_OFFLINE             = $00001000;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_OFFLINE}
  {$ENDIF}
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}
  {$ENDIF}
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}
  {$ENDIF}
  FILE_ATTRIBUTE_VIRTUAL             = $00010000;
  {$IFDEF COMPILER4_UP}
  {$EXTERNALSYM FILE_ATTRIBUTE_VIRTUAL}
  {$ENDIF}

type

  TTargetPath = class(TObject)
  private
    fFolder: WideString;
    fFileMasks: TTntStringList;
    fRecursive: Boolean;
    fMinLevel: Word;
    fMaxLevel: Word;
  public
    constructor Create;
    destructor Destroy; override;
    property Folder: WideString read fFolder write fFolder;
    property FileMasks: TTntStringList read fFileMasks;
    property Recursive: Boolean read fRecursive write fRecursive;
    property MinLevel: Word read fMinLevel write fMinLevel;
    property MaxLevel: Word read fMaxLevel write fMaxLevel;
  end;

  TTargetPaths = class(TList)
  private
    function GetItems(Index: Integer): TTargetPath;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function ExtractMeta(const Info: WideString; var Recursive: Boolean;
      var MinLevel, MaxLevel: Integer): WideString;
  public
    function Find(const Folder: WideString): TTargetPath;
    function AddPath(const PathInfo: WideString; Recursive: Boolean;
      MinLevel, MaxLevel: Integer): TTargetPath;
    function AddFolderAndMasks(const FolderInfo: WideString;
      FileMasks: TTntStringList; Recursive: Boolean;
      MinLevel, MaxLevel: Integer): TTargetPath;
    property Items[Index: Integer]: TTargetPath read GetItems; default;
  end;

  TCustomCriteria = class(TPersistent)
  public
    procedure Clear; virtual; abstract;
  end;

  TFileCriteria = class(TCustomCriteria)
  private
    fFileName: WideString;
    fLocation: WideString;
    fPaths: TTntStringList;
    fSubfolders: Boolean;
    fMinLevel: Word;
    fMaxLevel: Word;
    fFilters: TTntStringList;
    fTargetPaths: TTargetPaths;
    function GetTargetPaths: TTargetPaths;
    procedure SetFileName(const Value: WideString);
    procedure SetLocation(const Value: WideString);
    procedure SetPaths(Value: TTntStringList);
    procedure SetMinLevel(Value: Word);
    procedure SetMaxLevel(Value: Word);
    procedure SetSubfolders(Value: Boolean);
    procedure SetFilters(Value: TTntStringList);
    procedure TargetPathsChanged(Sender: TObject);
  protected
    property TargetPaths: TTargetPaths read GetTargetPaths;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Matches(const Folder, FileName: WideString): Boolean;
  published
    property FileName: WideString read fFileName write SetFileName;
    property Location: WideString read fLocation write SetLocation;
    property Paths: TTntStringList read fPaths write SetPaths;
    property Subfolders: Boolean read fSubfolders write SetSubfolders default True;
    property MinLevel: Word read fMinLevel write SetMinLevel default 0;
    property MaxLevel: Word read fMaxLevel write SetMaxLevel default 0;
    property Filters: TTntStringList read fFilters write SetFilters;
  end;

  TFileAttributeStatus = (fsIgnore, fsSet, fsUnset, fsAnySet, fsAnyUnset);

  TAttributesCriteria = class(TCustomCriteria)
  private
    fOnAttributes: DWORD;
    fOffAttributes: DWORD;
    fAnyOnAttributes: DWORD;
    fAnyOffAttributes: DWORD;
    function GetAttribute(Index: Integer): TFileAttributeStatus;
    procedure SetAttribute(Index: Integer; Value: TFileAttributeStatus);
  protected
    property OnAttributes: DWORD read fOnAttributes write fOnAttributes;
    property OffAttributes: DWORD read fOffAttributes write fOffAttributes;
    property AnyOnAttributes: DWORD read fAnyOnAttributes write fAnyOnAttributes;
    property AnyOffAttributes: DWORD read fAnyOffAttributes write fAnyOffAttributes;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Matches(Attr: DWORD): Boolean;
  published
    property Readonly: TFileAttributeStatus index 1 read GetAttribute write SetAttribute default fsIgnore;
    property Hidden: TFileAttributeStatus index 2 read GetAttribute write SetAttribute default fsUnset;
    property System: TFileAttributeStatus index 3 read GetAttribute write SetAttribute default fsUnset;
    property Directory: TFileAttributeStatus index 5 read GetAttribute write SetAttribute default fsUnset;
    property Archive: TFileAttributeStatus index 6 read GetAttribute write SetAttribute default fsIgnore;
    property Device: TFileAttributeStatus index 7 read GetAttribute write SetAttribute default fsIgnore;
    property Normal: TFileAttributeStatus index 8 read GetAttribute write SetAttribute default fsIgnore;
    property Temporary: TFileAttributeStatus index 9 read GetAttribute write SetAttribute default fsIgnore;
    property SparseFile: TFileAttributeStatus index 10 read GetAttribute write SetAttribute default fsIgnore;
    property ReparsePoint: TFileAttributeStatus index 11 read GetAttribute write SetAttribute default fsIgnore;
    property Compressed: TFileAttributeStatus index 12 read GetAttribute write SetAttribute default fsIgnore;
    property Offline: TFileAttributeStatus index 13 read GetAttribute write SetAttribute default fsIgnore;
    property NotContentIndexed: TFileAttributeStatus index 14 read GetAttribute write SetAttribute default fsIgnore;
    property Encrypted: TFileAttributeStatus index 15 read GetAttribute write SetAttribute default fsIgnore;
    property Virtual: TFileAttributeStatus index 17 read GetAttribute write SetAttribute default fsIgnore;
  end;

  TDateTimeCriteria = class(TCustomCriteria)
  private
     fCreatedBefore: TDateTime;
     fCreatedAfter: TDateTime;
     fModifiedBefore: TDateTime;
     fModifiedAfter: TDateTime;
     fAccessedBefore: TDateTime;
     fAccessedAfter: TDateTime;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Matches(const Created, Modified, Accessed: TFileTime): Boolean;
  published
    property CreatedBefore: TDateTime read fCreatedBefore write fCreatedBefore;
    property CreatedAfter: TDateTime read fCreatedAfter write fCreatedAfter;
    property ModifiedBefore: TDateTime read fModifiedBefore write fModifiedBefore;
    property ModifiedAfter: TDateTime read fModifiedAfter write fModifiedAfter;
    property AccessedBefore: TDateTime read fAccessedBefore write fAccessedBefore;
    property AccessedAfter: TDateTime read fAccessedAfter write fAccessedAfter;
  end;

  TFileSize = {$IFDEF COMPILER4_UP} Int64 {$ELSE} DWORD {$ENDIF};

  TSizeCriteria = class(TCustomCriteria)
  private
    fMin: TFileSize;
    fMax: TFileSize;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Matches(const Size: TFileSize): Boolean;
  published
    property Min: TFileSize read fMin write fMin default 0;
    property Max: TFileSize read fMax write fMax default 0;
  end;

  TContentSearchOption = (csoCaseSensitive, csoWholeWord);
  TContentSearchOptions = set of TContentSearchOption;

  PStringVariants = ^TStringVariants;
  TStringVariants = record
    Ansi: AnsiString;
    Unicode: WideString;
    Utf8: AnsiString;
  end;

  TContentCriteria = class(TCustomCriteria)
  private
    fPhrase: WideString;
    fPhraseVariants: TStringVariants;
    fOptions: TContentSearchOptions;
    procedure SetPhrase(const Value: WideString);
  protected
    property PhraseVariants: TStringVariants read fPhraseVariants;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Matches(const FileName: WideString): Boolean;
  published
    property Phrase: WideString read fPhrase write SetPhrase;
    property Options: TContentSearchOptions read fOptions write fOptions default [];
  end;

  TSearchCriteria = class(TCustomCriteria)
  private
    fFiles: TFileCriteria;
    fAttributes: TAttributesCriteria;
    fTimeStamp: TDateTimeCriteria;
    fSize: TSizeCriteria;
    fContent: TContentCriteria;
    procedure SetFiles(Value: TFileCriteria);
    procedure SetAttributes(Value: TAttributesCriteria);
    procedure SetTimeStamp(Value: TDateTimeCriteria);
    procedure SetSize(Value: TSizeCriteria);
    procedure SetContent(Value: TContentCriteria);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Matches(const Folder: WideString; const FindData: TWin32FindDataW): Boolean;
  published
    property Files: TFileCriteria read fFiles write SetFiles;
    property Attributes: TAttributesCriteria read fAttributes write SetAttributes;
    property TimeStamp: TDateTimeCriteria read fTimeStamp write SetTimeStamp;
    property Size: TSizeCriteria read fSize write SetSize;
    property Content: TContentCriteria read fContent write SetContent;
  end;

  TFolderIgnore = (fiNone, fiJustThis, fiJustSubfolders, fiThisAndSubfolders);

  TFolderChangeEvent = procedure (Sender: TObject; const Folder: WideString;
    var IgnoreFolder: TFolderIgnore) of object;

  PFileDetails = ^TFileDetails;
  TFileDetails = record
    Location: WideString;
    Name: WideString;
    Attributes: DWORD;
    Size: TFileSize;
    CreatedTime: TDateTime;
    ModifiedTime: TDateTime;
    AccessedTime: TDateTime;
  end;

  TFileMatchEvent = procedure (Sender: TObject; const FileInfo: TFileDetails) of object;

  TFindFileW = class(TComponent)
  private
    fCriteria: TSearchCriteria;
    fThreaded: Boolean;
    fThreadPriority: TThreadPriority;
    fAborted: Boolean;
    fBusy: Boolean;
    fCurrentLevel: Word;
    fOnFileMatch: TFileMatchEvent;
    fOnFolderChange: TFolderChangeEvent;
    fOnSearchBegin: TNotifyEvent;
    fOnSearchFinish: TNotifyEvent;
    fOnSearchAbort: TNotifyEvent;
    fThreadWnd: HWND;
    ActiveCriteria: TSearchCriteria;
    ActiveTarget: TTargetPath;
    SubfolderOffAttrs: DWORD;
    SearchThread: TThread;
    CS: TRTLCriticalSection;
    fCurrentFindData: TWin32FindDataW;
    fCurrentFolder: WideString;
    fIgnoreCurrentFolder: TFolderIgnore;
    fSearchedFolderCount: Integer;
    procedure SetCriteria(Value: TSearchCriteria);
    procedure InitializeSearch;
    procedure FinalizeSearch;
    procedure SearchForFiles;
    procedure SearchIn(const Path: WideString);
    procedure ThreadWndCallback(var Msg: TMessage);
  protected
    procedure DoSearchBegin; virtual;
    procedure DoSearchFinish; virtual;
    procedure DoSearchAbort; virtual;
    function DoFolderChange(const Folder: WideString): TFolderIgnore; virtual;
    procedure DoFileMatch(const Folder: WideString; const FindData: TWin32FindDataW); virtual;
    function IsAcceptable(const Folder: WideString; const FindData: TWin32FindDataW): Boolean; virtual;
    procedure SyncFileMatch;
    procedure SyncFolderChange;
    property ThreadWnd: HWND read fThreadWnd;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Abort;
    property Busy: Boolean read fBusy;
    property Aborted: Boolean read fAborted;
    property CurrentLevel: Word read fCurrentLevel;
    property SearchedFolderCount: Integer read fSearchedFolderCount;
  published
    property Criteria: TSearchCriteria read fCriteria write SetCriteria;
    property Threaded: Boolean read fThreaded write fThreaded default False;
    property ThreadPriority: TThreadPriority read fThreadPriority write fThreadPriority default tpNormal;
    property OnFileMatch: TFileMatchEvent read fOnFileMatch write fOnFileMatch;
    property OnFolderChange: TFolderChangeEvent read fOnFolderChange write fOnFolderChange;
    property OnSearchBegin: TNotifyEvent read fOnSearchBegin write fOnSearchBegin;
    property OnSearchFinish: TNotifyEvent read fOnSearchFinish write fOnSearchFinish;
    property OnSearchAbort: TNotifyEvent read fOnSearchAbort write fOnSearchAbort;
  end;

procedure Register;

function FormatFileSize(const Size: TFileSize): WideString;
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function WildcardMatches(S, M: PWideChar): Boolean;
function FileContains(const FileName: WideString; const Phrase: WideString;
  Options: TContentSearchOptions): Boolean;

function WideExpandUNCFileName(const FileName: WideString): WideString;

implementation

{$IFNDEF COMPILER6_UP}
uses Forms;
{$ENDIF}

const
  Delimiter   = ';';
  IncludeSign = '>';
  ExcludeSign = '<';

const
  FF_THREADTERMINATED = WM_USER + 1;

const
  SubfolderOffAttrsMask =
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM or
    FILE_ATTRIBUTE_DEVICE or FILE_ATTRIBUTE_TEMPORARY or
    FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_ENCRYPTED or
    FILE_ATTRIBUTE_VIRTUAL;

{ Character Map for faster case-insensitive search }

type
  PAnsiCharMap = ^TAnsiCharMap;
  TAnsiCharMap = array[AnsiChar] of AnsiChar;
  PWideCharMap = ^TWideCharMap;
  TWideCharMap = array[WideChar] of WideChar;

var
  AnsiCharMap: TAnsiCharMap;
  AnsiLowerCharMap: TAnsiCharMap;
  AnsiIsDelimiter: array[AnsiChar] of Boolean;
  WideCharMap: TWideCharMap;
  WideLowerCharMap: TWideCharMap;
  WideIsDelimiter: array[WideChar] of Boolean;

procedure InitFastContentSearch;
var
  AC: AnsiChar;
  WC: WideChar;
begin
  for AC := Low(TAnsiCharMap) to High(TAnsiCharMap) do
  begin
    AnsiCharMap[AC] := AC;
    AnsiLowerCharMap[AC] := AC;
    AnsiIsDelimiter[AC] := not IsCharAlphaNumericA(AC);
  end;
  AnsiLowerBuff(PAnsiChar(@AnsiLowerCharMap), SizeOf(AnsiLowerCharMap));
  for WC := Low(TWideCharMap) to High(TWideCharMap) do
  begin
    WideCharMap[WC] := WC;
    WideLowerCharMap[WC] := WC;
    WideIsDelimiter[WC] := not IsCharAlphaNumericW(WC);
  end;
  AnsiLowerBuff(PAnsiChar(@WideLowerCharMap), SizeOf(WideLowerCharMap));
end;

{ Helper Functions }

function WideGetUniversalName(const FileName: WideString): WideString;
type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..MaxInt div SizeOf(TNetResourceW) - 1] of TNetResourceW;
var
  I, BufSize, NetResult: Integer;
  Count, Size: LongWord;
  Drive: WideChar;
  NetHandle: THandle;
  NetResources: PNetResourceArray;
  RemoteNameInfo: array[0..2047] of Byte;
begin
  Result := FileName;
  if (Win32Platform <> VER_PLATFORM_WIN32_WINDOWS) or (Win32MajorVersion > 4) then
  begin
    Size := SizeOf(RemoteNameInfo);
    if WNetGetUniversalNameW(PWideChar(FileName), UNIVERSAL_NAME_INFO_LEVEL,
      @RemoteNameInfo, Size) <> NO_ERROR then Exit;
    Result := PRemoteNameInfoW(@RemoteNameInfo).lpUniversalName;
  end else
  begin
  { The following works around a bug in WNetGetUniversalName under Windows 95 }
    Drive := WideLowerCharMap[FileName[1]];
    if (Drive < 'a') or (Drive > 'z') or (Length(FileName) < 3) or
      (FileName[2] <> ':') or (FileName[3] <> '\') then
      Exit;
    if WNetOpenEnumW(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil,
      NetHandle) <> NO_ERROR then Exit;
    try
      BufSize := 50 * SizeOf(TNetResourceW);
      GetMem(NetResources, BufSize);
      try
        while True do
        begin
          Count := $FFFFFFFF;
          Size := BufSize;
          NetResult := WNetEnumResourceW(NetHandle, Count, NetResources, Size);
          if NetResult = ERROR_MORE_DATA then
          begin
            BufSize := Size;
            ReallocMem(NetResources, BufSize);
            Continue;
          end;
          if NetResult <> NO_ERROR then Exit;
          for I := 0 to Count - 1 do
            with NetResources^[I] do
              if (lpLocalName <> nil) and (Drive = WideLowerCharMap[lpLocalName[0]]) then
              begin
                Result := lpRemoteName + Copy(FileName, 3, Length(FileName) - 2);
                Exit;
              end;
        end;
      finally
        FreeMem(NetResources, BufSize);
      end;
    finally
      WNetCloseEnum(NetHandle);
    end;
  end;
end;

function WideExpandUNCFileName(const FileName: WideString): WideString;
begin
  { First get the local resource version of the file name }
  Result := WideExpandFileName(FileName);
  if (Length(Result) >= 3) and (Result[2] = ':') and
     (WideLowerCharMap[Result[1]] >= 'a') and
     (WideLowerCharMap[Result[1]] <= 'z')
  then
    Result := WideGetUniversalName(Result);
end;

function DecodeUTF8Char(Buffer: PAnsiChar; Count: Integer): WideChar;
  {$IFDEF COMPILER2005_UP} inline; {$ENDIF}
begin
  Result := #0;
  MultiByteToWideChar(CP_UTF8, 0, Buffer, Count, @Result, 1);
end;

{$IFNDEF UNICODE}
function AnsiStringToWideString(const S: AnsiString): WideString;
var
  Len: Integer;
begin
  if S <> '' then
  begin
    Len := MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(S), -1, nil, 0);
    SetString(Result, nil, Len - 1);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(S), -1, PWideChar(Result), Len);
  end
  else
    Result := '';
end;
{$ENDIF}

function StringVariantsOf(const S: WideString): TStringVariants;
begin
  with Result do
  begin
    {$IFDEF UNICODE}
    Ansi := AnsiString(S);
    Unicode := WideString(S);
    {$ELSE}
    Ansi := S;
    Unicode := AnsiStringToWideString(S);
    {$ENDIF}
    Utf8 := UTF8Encode(Unicode);
  end;
end;

function StreamContainsPhraseAnsi(Stream: TStream; const Phrase: PAnsiChar;
  PhraseLen: Integer; Options: TContentSearchOptions): Boolean;
const
  MaxBufferSize = $F000;
var
  CharMap: PAnsiCharMap;
  PrvChar, NxtChar: AnsiChar;
  Buffer: array[0..MaxBufferSize-1] of AnsiChar;
  BufferSize: Integer;
  BufferEnd: PAnsiChar;
  PhraseEnd: PAnsiChar;
  bp, pp: PAnsiChar;
begin
  if csoCaseSensitive in Options then
    CharMap := @AnsiCharMap
  else
    CharMap := @AnsiLowerCharMap;
  PrvChar := #0;
  PhraseEnd := Phrase;
  Inc(PhraseEnd, PhraseLen);
  pp := Phrase;
  BufferSize := Stream.Read(Buffer, MaxBufferSize);
  while BufferSize > 0 do
  begin
    bp := @Buffer;
    BufferEnd := @Buffer[BufferSize];
    repeat
      if (CharMap^[bp^] = CharMap^[pp^]) and ((pp <> Phrase) or
         not (csoWholeWord in Options) or AnsiIsDelimiter[PrvChar]) then
      begin
        Inc(pp);
        if pp = PhraseEnd then
        begin
          if csoWholeWord in Options then
          begin
            Inc(bp);
            if bp = BufferEnd then
            begin
              if Stream.Read(NxtChar, SizeOf(NxtChar)) = 0 then
                NxtChar := #0;
            end
            else
              NxtChar := bp^;
            if AnsiIsDelimiter[NxtChar] then
            begin
              Result := True;
              Exit;
            end;
            if bp = BufferEnd then
              Stream.Seek(-SizeOf(NxtChar), soFromCurrent);
            Dec(bp);
            pp := Phrase;
          end
          else
          begin
            Result := True;
            Exit;
          end;
        end;
      end
      else
        pp := Phrase;
      PrvChar := bp^;
      Inc(bp);
    until bp = BufferEnd;
    BufferSize := Stream.Read(Buffer, MaxBufferSize);
  end;
  Result := False;
end;

function StreamContainsPhraseWide(Stream: TStream; const Phrase: PWideChar;
  PhraseLen: Integer; Options: TContentSearchOptions; Swapped: Boolean): Boolean;
const
  MaxBufferSize = $F000;
var
  CharMap: PWideCharMap;
  PrvChar, NxtChar: WideChar;
  Buffer: array[0..MaxBufferSize-1] of WideChar;
  BufferSize: Integer;
  BufferEnd: PWideChar;
  PhraseEnd: PWideChar;
  bp, pp: PWideChar;
begin
  if csoCaseSensitive in Options then
    CharMap := @WideCharMap
  else
    CharMap := @WideLowerCharMap;
  PrvChar := #0;
  PhraseEnd := Phrase;
  Inc(PhraseEnd, PhraseLen);
  pp := Phrase;
  BufferSize := Stream.Read(Buffer, MaxBufferSize) shr 1;
  while BufferSize > 0 do
  begin
    bp := @Buffer;
    BufferEnd := @Buffer[BufferSize];
    repeat
      if Swapped then
        PWORD(bp)^ := MakeWord(HiByte(PWORD(bp)^), LoByte(PWORD(bp)^));
      if (CharMap^[bp^] = CharMap^[pp^]) and ((pp <> Phrase) or
         not (csoWholeWord in Options) or WideIsDelimiter[PrvChar]) then
      begin
        Inc(pp);
        if pp = PhraseEnd then
        begin
          if csoWholeWord in Options then
          begin
            Inc(bp);
            if bp = BufferEnd then
            begin
              if Stream.Read(NxtChar, SizeOf(NxtChar)) = 0 then
                NxtChar := #0;
            end
            else
              NxtChar := bp^;
            if Swapped then
              NxtChar := WideChar(MakeWord(HiByte(WORD(NxtChar)), LoByte(WORD(NxtChar))));
            if WideIsDelimiter[NxtChar] then
            begin
              Result := True;
              Exit;
            end;
            if bp = BufferEnd then
              Stream.Seek(-SizeOf(NxtChar), soFromCurrent);
            Dec(bp);
            pp := Phrase;
          end
          else
          begin
            Result := True;
            Exit;
          end;
        end;
      end
      else
        pp := Phrase;
      PrvChar := bp^;
      Inc(bp);
    until bp = BufferEnd;
    BufferSize := Stream.Read(Buffer, MaxBufferSize) shr 1;
  end;
  Result := False;
end;

function StreamContainsPhraseUtf8(Stream: TStream; const Phrase: PAnsiChar;
  PhraseLen: Integer; Options: TContentSearchOptions): Boolean;
const
  MaxBufferSize = $F000;
var
  CharMap: PAnsiCharMap;
  PrvChars, NxtChars: array[0..7] of AnsiChar;
  PrvLen, NxtLen: Integer;
  PrvReady: Boolean;
  Rollback: Integer;
  Buffer: array[0..MaxBufferSize-1] of AnsiChar;
  BufferSize: Integer;
  BufferEnd: PAnsiChar;
  PhraseEnd: PAnsiChar;
  bp, pp, np: PAnsiChar;
begin
  if csoCaseSensitive in Options then
    CharMap := @AnsiCharMap
  else
    CharMap := @AnsiLowerCharMap;
  PrvReady := True;
  PrvLen := 0;
  PhraseEnd := Phrase;
  Inc(PhraseEnd, PhraseLen);
  pp := Phrase;
  BufferSize := Stream.Read(Buffer, MaxBufferSize);
  while BufferSize > 0 do
  begin
    bp := @Buffer;
    BufferEnd := @Buffer[BufferSize];
    repeat
      if (PrvReady or (pp <> Phrase)) and
         (CharMap^[bp^] = CharMap^[pp^]) and
         ((pp <> Phrase) or not (csoWholeWord in Options) or
         WideIsDelimiter[DecodeUTF8Char(PrvChars, PrvLen)]) then
      begin
        Inc(pp);
        if pp = PhraseEnd then
        begin
          if csoWholeWord in Options then
          begin
            np := bp;
            Inc(np);
            NxtLen := 0;
            Rollback := 0;
            repeat
              if np = BufferEnd then
              begin
                if Stream.Read(NxtChars[NxtLen], 1) = 1 then
                  Inc(Rollback)
                else
                  Break;
              end
              else
              begin
                NxtChars[NxtLen] := np^;
                Inc(np);
              end;
              if (NxtLen > 0) and ((Ord(NxtChars[NxtLen]) and $C0) = $C0) then
                Break;
              Inc(NxtLen);
            until ((Ord(NxtChars[NxtLen-1]) and $80) = 0) or (NxtLen > High(NxtChars));
            if WideIsDelimiter[DecodeUTF8Char(NxtChars, NxtLen)] then
            begin
              Result := True;
              Exit;
            end;
            if Rollback <> 0 then
              Stream.Seek(-Rollback, soFromCurrent);
            pp := Phrase;
          end
          else
          begin
            Result := True;
            Exit;
          end;
        end;
      end
      else
        pp := Phrase;
      if PrvReady then
        PrvLen := 0;
      PrvChars[PrvLen] := bp^;
      Inc(PrvLen);
      PrvReady := ((Ord(bp^) and $80) = 0) or (PrvLen > High(PrvChars));
      Inc(bp);
      PrvReady := PrvReady or ((Ord(bp^) and $C0) = $C0);
    until bp = BufferEnd;
    BufferSize := Stream.Read(Buffer, MaxBufferSize);
  end;
  Result := False;
end;

function FileContainsPhrase(const FileName: WideString;
  const Phrase: TStringVariants; Options: TContentSearchOptions): Boolean;
const
  UNICODE_BOM: WideChar = #$FEFF;
  UNICODE_BOM_SWAPPED: WideChar = #$FFFE;
  UTF8_BOM: array[1..3] of AnsiChar = (#$EF, #$BB, #$BF);
var
  Stream: TFileStream;
  BOMBytes: array[1..3] of AnsiChar;
  BOM: WideChar absolute BOMBytes;
  BOMSize: Integer;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    BOMSize := Stream.Read(BOM, SizeOf(BOM));
    if BOMSize = SizeOf(BOM) then
    begin
      if (BOM = UNICODE_BOM) or (BOM = UNICODE_BOM_SWAPPED) then
      begin
        Result := StreamContainsPhraseWide(Stream, PWideChar(Phrase.Unicode),
          Length(Phrase.Unicode), Options, BOM = UNICODE_BOM_SWAPPED);
        Exit;
      end
      else if (BOMBytes[1] = UTF8_BOM[1]) and (BOMBytes[2] = UTF8_BOM[2]) then
      begin
        Inc(BOMSize, Stream.Read(BOMBytes[3], SizeOf(BOMBytes[3])));
        if (BOMSize = SizeOf(UTF8_BOM)) and (BOMBytes[3] = UTF8_BOM[3]) then
        begin
          Result := StreamContainsPhraseUtf8(Stream, PAnsiChar(Phrase.Utf8),
            Length(Phrase.Utf8), Options);
          Exit;
        end;
      end;
    end;
    Stream.Seek(-BOMSize, soFromCurrent);
    Result := StreamContainsPhraseAnsi(Stream, PAnsiChar(Phrase.Ansi),
      Length(Phrase.Ansi), Options);
  finally
    Stream.Free;
  end;
end;

function FileContains(const FileName: WideString;
  const Phrase: WideString; Options: TContentSearchOptions): Boolean;
begin
  if Length(Phrase) > 0 then
    Result := FileContainsPhrase(FileName, StringVariantsOf(Phrase), Options)
  else
    Result := True;
end;

function FormatFileSize(const Size: TFileSize): WideString;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

function IsDateBetween(const aDate, Before, After: TDateTime): Boolean;
begin
  Result := True;
  if Before <> 0 then
    if Frac(Before) = 0 then      { Checks date only }
      Result := Result and (Int(aDate) <= Before)
    else if Int(Before) = 0 then  { Checks time only }
      Result := Result and (Frac(aDate) <= Before)
    else                          { Checks date and time }
      Result := Result and (aDate <= Before);
  if After <> 0 then
    if Frac(After) = 0 then       { Checks date only }
      Result := Result and (Int(aDate) >= After)
    else if Int(After) = 0 then   { Checks time only }
      Result := Result and (Frac(aDate) >= After)
    else                          { Checks date and time }
      Result := Result and (aDate >= After);
end;

function WildcardMatches(S, M: PWideChar): Boolean;
const
  {$IFDEF UNICODE}
  CharMap: PWideCharMap = @WideLowerCharMap;
  {$ELSE}
  CharMap: PAnsiCharMap = @AnsiLowerCharMap;
  {$ENDIF}
var
  Stop: WideChar;
begin
  Result := False;
  while (S^ <> #0) and (M^ <> #0) and (M^ <> '*') do
  begin
    if (M^ <> '?') and (CharMap^[M^] <> CharMap^[S^]) then
      Exit;
    Inc(S);
    Inc(M);
  end;
  if (S^ = #0) or (M^ = '*') then
  begin
    while (M^ = '*') or (M^ = '?') do
      Inc(M);
    if (S^ = #0) or (M^ = #0) then
      Result := (M^ = #0)
    else
    begin
      Stop := CharMap^[M^];
      Inc(M);
      while (S^ <> #0) and not Result do
      begin
        while (CharMap^[S^] <> Stop) and (S^ <> #0) do
          Inc(S);
        if S^ <> #0 then
        begin
          Inc(S);
          Result := WildcardMatches(S, M);
        end;
      end;
    end;
  end;
end;

function StringListFromString(const Str: WideString;
  Delimiter: WideChar): TTntStringList;
var
  Item: WideString;
  StartIndex: Integer;
  DelimiterPos: Integer;
  StrLen: Integer;
begin
  Result := TTntStringList.Create;
  StrLen := Length(Str);
  StartIndex := 1;
  repeat
    DelimiterPos := StartIndex;
    while (DelimiterPos <= StrLen) and (Str[DelimiterPos] <> Delimiter) do
      Inc(DelimiterPos);
    if StartIndex <> DelimiterPos then
    begin
      Item := Trim(Copy(Str, StartIndex, DelimiterPos - StartIndex));
      if (Item <> '') and (Result.IndexOf(Item) < 0) then
        Result.Add(Item);
    end;
    StartIndex := DelimiterPos + 1;
  until StartIndex > StrLen;
end;

{ TTargetPath }

constructor TTargetPath.Create;
begin
  inherited Create;
  fFileMasks := TTntStringList.Create;
  fFileMasks.Duplicates := dupIgnore;
  fFileMasks.CaseSensitive := False;
end;

destructor TTargetPath.Destroy;
begin
  fFileMasks.Free;
  inherited Destroy;
end;

{ TTargetPaths }

procedure TTargetPaths.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    TTargetPath(Ptr).Free;
end;

function TTargetPaths.Find(const Folder: WideString): TTargetPath;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if WideCompareText(Result.Folder, Folder) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TTargetPaths.AddPath(const PathInfo: WideString; Recursive: Boolean;
  MinLevel, MaxLevel: Integer): TTargetPath;
var
  FileMask: WideString;
  Folder: WideString;
begin
  FileMask := ExtractFileName(PathInfo);
  Folder := ExtractMeta(ExtractFilePath(PathInfo), Recursive, MinLevel, MaxLevel);
  Result := Find(Folder);
  if not Assigned(Result) then
  begin
    Result := TTargetPath.Create;
    Add(Result);
  end;
  Result.Folder := Folder;
  Result.Recursive := Recursive;
  Result.MinLevel := MinLevel;
  Result.MaxLevel := MaxLevel;
  if FileMask <> '' then
    Result.FileMasks.Add(FileMask)
  else
    Result.FileMasks.Add('*.*');
end;

function TTargetPaths.AddFolderAndMasks(const FolderInfo: WideString;
  FileMasks: TTntStringList; Recursive: Boolean;
  MinLevel, MaxLevel: Integer): TTargetPath;
var
  Folder: WideString;
begin
  Folder := ExtractMeta(FolderInfo, Recursive, MinLevel, MaxLevel);
  Result := Find(Folder);
  if not Assigned(Result) then
  begin
    Result := TTargetPath.Create;
    Add(Result);
  end;
  Result.Folder := Folder;
  Result.Recursive := Recursive;
  Result.MinLevel := MinLevel;
  Result.MaxLevel := MaxLevel;
  if Assigned(FileMasks) and (FileMasks.Count > 0) then
    Result.FileMasks.AddStrings(FileMasks)
  else
    Result.FileMasks.Add('*.*');
end;

function TTargetPaths.ExtractMeta(const Info: WideString; var Recursive: Boolean;
  var MinLevel, MaxLevel: Integer): WideString;
var
  P, L: Integer;
  Level: WideString;
begin
  Result := Info;
  P := WideTextPos(IncludeSign, Info);
  if P <> 0 then
  begin
    System.Delete(Result, 1, P);
    Recursive := True;
    if P > 1 then
    begin
      Level := Trim(Copy(Info, 1, P - 1));
      P := Pos('-', Level);
      if P = 0 then
      begin
        L := StrToIntDef(Level, MaxLevel);
        MinLevel := 0;
        MaxLevel := L;
      end
      else
      begin
        MinLevel := StrToIntDef(Trim(Copy(Level, 1, P - 1)), MinLevel);
        MaxLevel := StrToIntDef(Trim(Copy(Level, P + 1, Length(Level) - P)), MaxLevel);
      end;
    end;
  end
  else
  begin
    P := WideTextPos(ExcludeSign, Info);
    if P <> 0 then
    begin
      System.Delete(Result, 1, P);
      Recursive := False;
    end;
  end;
  Result := WideExpandUNCFileName(Trim(Result));
  {$IFDEF COMPILER7_UP}
  Result := WideIncludeTrailingPathDelimiter(Result);
  {$ELSE}
  Result := WideIncludeTrailingBackslash(Result);
  {$ENDIF}
end;

function TTargetPaths.GetItems(Index: Integer): TTargetPath;
begin
  Result := TTargetPath(Get(Index));
end;

{ TFileCriteria }

constructor TFileCriteria.Create;
begin
  inherited Create;
  fPaths := TTntStringList.Create;
  fPaths.Duplicates := dupIgnore;
  fPaths.CaseSensitive := False;
  fPaths.OnChange := TargetPathsChanged;
  fSubfolders := True;
  fMinLevel := 0;
  fMaxLevel := 0;
  fFilters := TTntStringList.Create;
  fFilters.Duplicates := dupIgnore;
  fFilters.CaseSensitive := False;
end;

destructor TFileCriteria.Destroy;
begin
  fPaths.Free;
  fFilters.Free;
  if Assigned(fTargetPaths) then
    fTargetPaths.Free;
  inherited Destroy;
end;

procedure TFileCriteria.Assign(Source: TPersistent);
begin
  if Source is TFileCriteria then
  begin
    FileName := TFileCriteria(Source).FileName;
    Location := TFileCriteria(Source).Location;
    Paths := TFileCriteria(Source).Paths;
    Filters := TFileCriteria(Source).Filters;
    Subfolders := TFileCriteria(Source).Subfolders;
    MinLevel := TFileCriteria(Source).MinLevel;
    MaxLevel := TFileCriteria(Source).MaxLevel;
  end
  else
    inherited Assign(Source);
end;

procedure TFileCriteria.Clear;
begin
  FileName := '';
  Location := '';
  Paths.Clear;
  Subfolders := True;
  MinLevel := 0;
  MaxLevel := 0;
  Filters.Clear;
end;

function TFileCriteria.Matches(const Folder, FileName: WideString): Boolean;
var
  I: Integer;
  Path: WideString;
  Mask: PWideChar;
begin
  Result := True;
  if Filters.Count <> 0 then
  begin
    Path := Folder + FileName;
    if ExtractFileExt(FileName) = '' then
      Path := Path + '.';
    for I := 0 to Filters.Count - 1 do
    begin
      Mask := PWideChar(Filters[I]);
      if Mask^ = IncludeSign then
      begin
        Inc(Mask);
        if WildcardMatches(PWideChar(Path), Mask) then
           Exit;
      end
      else
      begin
        if Mask^ = ExcludeSign then
          Inc(Mask);
        if WildcardMatches(PWideChar(Path), Mask) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;
end;

function TFileCriteria.GetTargetPaths: TTargetPaths;
var
  I: Integer;
  Files: TTntStringList;
  Folders: TTntStringList;
  Path: WideString;
begin
  if not Assigned(fTargetPaths) then
  begin
    fTargetPaths := TTargetPaths.Create;
    // Add FileName and Location properties
    Files := StringListFromString(FileName, Delimiter);
    try
      if Files.Count = 0 then
        Files.Add('*.*');
      Folders := StringListFromString(Location, Delimiter);
      try
        for I := 0 to Folders.Count - 1 do
          fTargetPaths.AddFolderAndMasks(Folders[I], Files,
            Subfolders, MinLevel, MaxLevel);
      finally
        Folders.Free;
      end;
    finally
      Files.Free;
    end;
    // Add Paths property
    for I := 0 to Paths.Count - 1 do
    begin
      Path := Trim(Paths[I]);
      if Path <> '' then
        fTargetPaths.AddPath(Path, Subfolders, MinLevel, MaxLevel)
    end;
  end;
  Result := fTargetPaths;
end;

procedure TFileCriteria.SetFileName(const Value: WideString);
begin
  if fFileName <> Value then
  begin
    fFileName := Value;
    TargetPathsChanged(nil);
  end;
end;

procedure TFileCriteria.SetLocation(const Value: WideString);
begin
  if fLocation <> Value then
  begin
    fLocation := Value;
    TargetPathsChanged(nil);
  end;
end;

procedure TFileCriteria.SetPaths(Value: TTntStringList);
begin
  fPaths.Assign(Value);
end;

procedure TFileCriteria.SetFilters(Value: TTntStringList);
begin
  fFilters.Assign(Value);
end;

procedure TFileCriteria.SetSubfolders(Value: Boolean);
begin
  if fSubfolders <> Value then
  begin
    fSubfolders := Value;
    TargetPathsChanged(nil);
  end;
end;

procedure TFileCriteria.SetMinLevel(Value: Word);
begin
  if fMinLevel <> Value then
  begin
    fMinLevel := Value;
    TargetPathsChanged(nil);
  end;
end;

procedure TFileCriteria.SetMaxLevel(Value: Word);
begin
  if fMaxLevel <> Value then
  begin
    fMaxLevel := Value;
    TargetPathsChanged(nil);
  end;
end;

procedure TFileCriteria.TargetPathsChanged(Sender: TObject);
begin
  if Assigned(fTargetPaths) then
  begin
    fTargetPaths.Free;
    fTargetPaths := nil;
  end;
end;

{ TAttributesCriteria }

constructor TAttributesCriteria.Create;
begin
  inherited Create;
  fOnAttributes := 0;
  fOffAttributes := FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_DIRECTORY;
  fAnyOnAttributes := 0;
  fAnyOffAttributes := 0;
end;

procedure TAttributesCriteria.Assign(Source: TPersistent);
begin
  if Source is TAttributesCriteria then
  begin
    OnAttributes := TAttributesCriteria(Source).OnAttributes;
    OffAttributes := TAttributesCriteria(Source).OffAttributes;
    AnyOnAttributes := TAttributesCriteria(Source).AnyOnAttributes;
    AnyOffAttributes := TAttributesCriteria(Source).AnyOffAttributes;
  end
  else
    inherited Assign(Source);
end;

procedure TAttributesCriteria.Clear;
begin
  OnAttributes := 0;
  OffAttributes := 0;
  AnyOnAttributes := 0;
  AnyOffAttributes := 0;
end;

function TAttributesCriteria.Matches(Attr: DWORD): Boolean;
begin
  Result := False;
  if ((Attr and OnAttributes) = OnAttributes) and
     ((not Attr and OffAttributes) = OffAttributes) then
  begin
    if (AnyOnAttributes <> 0) and not LongBool(Attr and AnyOnAttributes) then
      Exit;
    if (AnyOffAttributes <> 0) and not LongBool(not Attr and AnyOffAttributes) then
      Exit;
    Result := True;
  end;
end;

function TAttributesCriteria.GetAttribute(Index: Integer): TFileAttributeStatus;
var
  Attr: DWORD;
begin
  Attr := $00000001 shl (Index - 1);
  if LongBool(Attr and fOnAttributes) then
    Result := fsSet
  else if LongBool(Attr and fOffAttributes) then
    Result := fsUnset
  else if LongBool(Attr and fAnyOnAttributes) then
    Result := fsAnySet
  else if LongBool(Attr and fAnyOffAttributes) then
    Result := fsAnyUnset
  else
    Result := fsIgnore;
end;

procedure TAttributesCriteria.SetAttribute(Index: Integer; Value: TFileAttributeStatus);
var
  Attr: DWORD;
begin
  Attr := $00000001 shl (Index - 1);
  fOnAttributes := fOnAttributes and not Attr;
  fOffAttributes := fOffAttributes and not Attr;
  fAnyOnAttributes := fAnyOnAttributes and not Attr;
  fAnyOffAttributes := fAnyOffAttributes and not Attr;
  case Value of
    fsSet: fOnAttributes := fOnAttributes or Attr;
    fsUnset: fOffAttributes := fOffAttributes or Attr;
    fsAnySet: fAnyOnAttributes := fAnyOnAttributes or Attr;
    fsAnyUnset: fAnyOffAttributes := fAnyOffAttributes or Attr;
  end;
end;

{ TDateTimeCriteria }

procedure TDateTimeCriteria.Assign(Source: TPersistent);
begin
  if Source is TDateTimeCriteria then
  begin
    CreatedBefore := TDateTimeCriteria(Source).CreatedBefore;
    CreatedAfter := TDateTimeCriteria(Source).CreatedAfter;
    ModifiedBefore := TDateTimeCriteria(Source).ModifiedBefore;
    ModifiedAfter := TDateTimeCriteria(Source).ModifiedAfter;
    AccessedBefore := TDateTimeCriteria(Source).AccessedBefore;
    AccessedAfter := TDateTimeCriteria(Source).AccessedAfter;
  end
  else
    inherited Assign(Source);
end;

procedure TDateTimeCriteria.Clear;
begin
  CreatedBefore := 0;
  CreatedAfter := 0;
  ModifiedBefore := 0;
  ModifiedAfter := 0;
  AccessedBefore := 0;
  AccessedAfter := 0;
end;

function TDateTimeCriteria.Matches(const Created, Modified, Accessed: TFileTime): Boolean;
var
  DateTime: TDateTime;
begin
  Result := False;
  if (CreatedBefore <> 0) or (CreatedAfter <> 0) then
  begin
    DateTime := FileTimeToDateTime(Created);
    if not IsDateBetween(DateTime, CreatedBefore, CreatedAfter) then Exit;
  end;
  if (ModifiedBefore <> 0) or (ModifiedAfter <> 0) then
  begin
    DateTime := FileTimeToDateTime(Modified);
    if not IsDateBetween(DateTime, ModifiedBefore, ModifiedAfter) then Exit;
  end;
  if (AccessedBefore <> 0) or (AccessedAfter <> 0) then
  begin
    DateTime := FileTimeToDateTime(Accessed);
    if not IsDateBetween(DateTime, AccessedBefore, AccessedAfter) then Exit;
  end;
  Result := True;
end;

{ TSizeCriteria }

procedure TSizeCriteria.Assign(Source: TPersistent);
begin
  if Source is TSizeCriteria then
  begin
    Min := TSizeCriteria(Source).Min;
    Max := TSizeCriteria(Source).Max;
  end
  else
    inherited Assign(Source);
end;

procedure TSizeCriteria.Clear;
begin
  fMin := 0;
  fMax := 0;
end;

function TSizeCriteria.Matches(const Size: TFileSize): Boolean;
begin
  Result := ((Min = 0) or (Size >= Min)) and ((Max = 0) or (Size <= Max));
end;

{ TContentCriteria }

procedure TContentCriteria.Assign(Source: TPersistent);
begin
  if Source is TContentCriteria then
  begin
    Phrase := TContentCriteria(Source).Phrase;
    Options := TContentCriteria(Source).Options;
  end
  else
    inherited Assign(Source);
end;

procedure TContentCriteria.Clear;
begin
  Phrase := '';
  Options := [];
end;

function TContentCriteria.Matches(const FileName: WideString): Boolean;
begin
  if Length(Phrase) > 0 then
    try
      Result := FileContainsPhrase(FileName, PhraseVariants, Options);
    except
      Result := False;
    end
  else
    Result := True;
end;

procedure TContentCriteria.SetPhrase(const Value: WideString);
begin
  if fPhrase <> Value then
  begin
    fPhrase := Value;
    fPhraseVariants := StringVariantsOf(fPhrase);
  end;
end;

{ TSearchCriteria }

constructor TSearchCriteria.Create;
begin
  inherited Create;
  fFiles := TFileCriteria.Create;
  fAttributes := TAttributesCriteria.Create;
  fTimeStamp := TDateTimeCriteria.Create;
  fSize := TSizeCriteria.Create;
  fContent := TContentCriteria.Create;
end;

destructor TSearchCriteria.Destroy;
begin
  fFiles.Free;
  fAttributes.Free;
  fTimeStamp.Free;
  fSize.Free;
  fContent.Free;
  inherited Destroy;
end;

procedure TSearchCriteria.Assign(Source: TPersistent);
begin
  if Source is TSearchCriteria then
  begin
    Files := TSearchCriteria(Source).Files;
    Attributes := TSearchCriteria(Source).Attributes;
    TimeStamp := TSearchCriteria(Source).TimeStamp;
    Size := TSearchCriteria(Source).Size;
    Content := TSearchCriteria(Source).Content;
  end
  else
    inherited Assign(Source);
end;

procedure TSearchCriteria.Clear;
begin
  Files.Clear;
  Attributes.Clear;
  TimeStamp.Clear;
  Size.Clear;
  Content.Clear;
end;

function TSearchCriteria.Matches(const Folder: WideString;
  const FindData: TWin32FindDataW): Boolean;
begin
  with FindData do
  begin
    Result :=
      Attributes.Matches(dwFileAttributes) and
      Size.Matches(nFileSizeLow {$IFDEF COMPILER4_UP} or (Int64(nFileSizeHigh) shl 32) {$ENDIF}) and
      TimeStamp.Matches(ftCreationTime, ftLastWriteTime, ftLastAccessTime) and
      Files.Matches(Folder, WideString(FindData.cFileName)) and
      Content.Matches(Folder + WideString(FindData.cFileName));
  end;
end;

procedure TSearchCriteria.SetFiles(Value: TFileCriteria);
begin
  Files.Assign(Value);
end;

procedure TSearchCriteria.SetAttributes(Value: TAttributesCriteria);
begin
  Attributes.Assign(Value);
end;

procedure TSearchCriteria.SetTimeStamp(Value: TDateTimeCriteria);
begin
  TimeStamp.Assign(Value);
end;

procedure TSearchCriteria.SetSize(Value: TSizeCriteria);
begin
  Size.Assign(Value);
end;

procedure TSearchCriteria.SetContent(Value: TContentCriteria);
begin
  Content.Assign(Value);
end;

{ TSearchThread }

type
  TSearchThread = class(TThread)
  private
    Owner: TFindFileW;
  protected
    constructor Create(AOwner: TFindFileW);
    procedure Execute; override;
  end;

constructor TSearchThread.Create(AOwner: TFindFileW);
begin
  inherited Create(True);
  Owner := AOwner;
  Priority := Owner.ThreadPriority;
  Resume;
end;

procedure TSearchThread.Execute;
begin
  try
    try
      Owner.SearchForFiles;
    except
      ShowException(ExceptObject, ExceptAddr);
    end;
  finally
    PostMessage(Owner.ThreadWnd, FF_THREADTERMINATED, 0, 0);
  end;
end;

{ TFindFileW }

constructor TFindFileW.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(CS);
  fThreadWnd := AllocateHWnd(ThreadWndCallback);
  fCriteria := TSearchCriteria.Create;
  fThreaded := False;
  fThreadPriority := tpNormal;
  fAborted := False;
  fBusy := False;
end;

destructor TFindFileW.Destroy;
begin
  if Busy then Abort;
  fCriteria.Free;
  DeallocateHWnd(fThreadWnd);
  DeleteCriticalSection(CS);
  inherited Destroy;
end;

procedure TFindFileW.Abort;
var
  Msg: TMSG;
begin
  if Busy and not Aborted then
  begin
    fAborted := True;
    while not TryEnterCriticalSection(CS) do
    begin
      if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
    DoSearchAbort;
    LeaveCriticalSection(CS);
  end;
end;

procedure TFindFileW.DoFileMatch(const Folder: WideString;
  const FindData: TWin32FindDataW);
var
  FileInfo: TFileDetails;
begin
  if not Aborted and Assigned(fOnFileMatch) then
  begin
    FileInfo.Location := Folder;
    FileInfo.Name := WideString(FindData.cFileName);
    FileInfo.Attributes := FindData.dwFileAttributes;
    FileInfo.Size := FindData.nFileSizeLow {$IFDEF COMPILER4_UP} or Int64(FindData.nFileSizeHigh) shl 32 {$ENDIF};
    FileInfo.CreatedTime := FileTimeToDateTime(FindData.ftCreationTime);
    FileInfo.ModifiedTime := FileTimeToDateTime(FindData.ftLastWriteTime);
    FileInfo.AccessedTime := FileTimeToDateTime(FindData.ftLastAccessTime);
    if not Threaded then
    begin
      EnterCriticalSection(CS);
      try
        fOnFileMatch(Self, FileInfo);
      finally
        LeaveCriticalSection(CS);
      end;
    end
    else
    fOnFileMatch(Self, FileInfo);
  end;
end;

function TFindFileW.DoFolderChange(const Folder: WideString): TFolderIgnore;
begin
  Result:= fiNone;
  if not Aborted and Assigned(fOnFolderChange) then
  begin
    if not Threaded then
    begin
      EnterCriticalSection(CS);
      try
        fOnFolderChange(Self, Folder, Result);
      finally
        LeaveCriticalSection(CS);
      end;
    end
    else
    fOnFolderChange(Self, Folder, Result);
  end;
end;

procedure TFindFileW.DoSearchBegin;
begin
  if Assigned(fOnSearchBegin) and not (csDestroying in ComponentState) then
    fOnSearchBegin(Self);
end;

procedure TFindFileW.DoSearchFinish;
begin
  if Assigned(fOnSearchFinish) and not (csDestroying in ComponentState) then
    fOnSearchFinish(Self);
end;

procedure TFindFileW.DoSearchAbort;
begin
  if Assigned(fOnSearchAbort) and not (csDestroying in ComponentState) then
    fOnSearchAbort(Self);
end;

function TFindFileW.IsAcceptable(const Folder: WideString;
  const FindData: TWin32FindDataW): Boolean;
begin
  Result := not Aborted and ActiveCriteria.Matches(Folder, FindData);
end;

procedure TFindFileW.InitializeSearch;
begin
  fBusy := True;
  fAborted := False;
  fSearchedFolderCount:= 0;
  ActiveCriteria := TSearchCriteria.Create;
  ActiveCriteria.Assign(fCriteria);
  SubfolderOffAttrs := ActiveCriteria.Attributes.OffAttributes and SubfolderOffAttrsMask;
  DoSearchBegin;
end;

procedure TFindFileW.FinalizeSearch;
begin
  ActiveCriteria.Free;
  ActiveCriteria := nil;
  fBusy := False;
  DoSearchFinish;
end;

procedure TFindFileW.SearchForFiles;
var
  I: Integer;
begin
  with ActiveCriteria.Files.TargetPaths do
    for I := 0 to Count - 1 do
    begin
      if Aborted then Exit;
      fCurrentLevel := 0;
      ActiveTarget := Items[I];
      SearchIn(ActiveTarget.Folder);
    end;
end;

procedure TFindFileW.SearchIn(const Path: WideString);
var
  I: Integer;
  FindData: TWin32FindDataW;
  IgnoreFolder: TFolderIgnore;
  Handle: THandle;
begin
  if Aborted then Exit;
  Inc(fCurrentLevel);
  try
    if assigned(SearchThread) then
    begin
      fCurrentFolder:= Path;
      TSearchThread(SearchThread).Synchronize(SyncFolderChange);
      IgnoreFolder:= fIgnoreCurrentFolder;
    end
    else
    IgnoreFolder:= DoFolderChange(Path);

    with ActiveTarget do
    begin
      // Searches in the current folder for all file masks
      if (IgnoreFolder in [fiNone, fiJustSubfolders]) and (CurrentLevel >= MinLevel) then
      begin
        fSearchedFolderCount:= fSearchedFolderCount + 1;
        for I := 0 to FileMasks.Count - 1 do
        begin
          Handle := Windows.FindFirstFileW(PWideChar(Path + FileMasks[I]), FindData);
          if Handle <> INVALID_HANDLE_VALUE then
           try
             repeat
               if Aborted then Exit;
               if (not LongBool(FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) or
                  ((FindData.cFileName[0] <> '.') or ((FindData.cFileName[1] <> #0) and
                  ((FindData.cFileName[1] <> '.') or (FindData.cFileName[2] <> #0))))) and
                  IsAcceptable(Path, FindData)
               then
               begin
                 if assigned(SearchThread) then
                 begin
                   fCurrentFindData:= FindData; 
                   TSearchThread(SearchThread).Synchronize(SyncFileMatch);
                 end
                 else
                 DoFileMatch(Path, FindData);
               end;

             until not Windows.FindNextFileW(Handle, FindData);
           finally
             Windows.FindClose(Handle);
           end;
        end;
      end;
      // Searches in subfolders
      if Recursive and (IgnoreFolder in [fiNone, fiJustThis]) and
        ((MaxLevel = 0) or (CurrentLevel < MaxLevel)) then
      begin
        Handle := Windows.FindFirstFileW(PWideChar(Path + '*.*'), FindData);
        if Handle <> INVALID_HANDLE_VALUE then
          try
            repeat
              if Aborted then Exit;
              if LongBool(FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) and
                 not LongBool(FindData.dwFileAttributes and SubfolderOffAttrs) and
                ((FindData.cFileName[0] <> '.') or ((FindData.cFileName[1] <> #0) and
                ((FindData.cFileName[1] <> '.') or (FindData.cFileName[2] <> #0))))
              then
                SearchIn(Path + WideString(FindData.cFileName) + '\');
            until not Windows.FindNextFileW(Handle, FindData);
          finally
            Windows.FindClose(Handle);
          end;
      end;
    end;
  finally
    Dec(fCurrentLevel);
  end;
end;

procedure TFindFileW.Execute;
begin
  if not Busy then
  begin
    if not Threaded then
    begin
      InitializeSearch;
      try
        SearchForFiles;
      finally
        FinalizeSearch;
      end;
    end
    else
    begin
      InitializeSearch;
      try
       SearchThread := TSearchThread.Create(Self);
      except
        FinalizeSearch;
        raise;
      end;
    end;
  end;
end;

procedure TFindFileW.SetCriteria(Value: TSearchCriteria);
begin
  Criteria.Assign(Value);
end;

procedure TFindFileW.SyncFileMatch;
begin
  DoFileMatch(fCurrentFolder, fCurrentFindData);
end;

procedure TFindFileW.SyncFolderChange;
begin
  fIgnoreCurrentFolder:= DoFolderChange(fCurrentFolder);
end;

procedure TFindFileW.ThreadWndCallback(var Msg: TMessage);
begin
  case Msg.Msg of
    FF_THREADTERMINATED:
    begin
      SearchThread.Free;
      SearchThread:= nil;
      FinalizeSearch;
    end;
  else
    with Msg do Result := DefWindowProc(ThreadWnd, Msg, WParam, LParam);
  end;
end;

procedure Register;
begin
  RegisterComponents('Delphi Area', [TFindFileW]);
end;

initialization
  InitFastContentSearch;
end.
