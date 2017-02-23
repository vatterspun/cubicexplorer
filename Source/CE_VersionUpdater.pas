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
//  The Original Code is CE_VersionUpdater.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_VersionUpdater;

interface

uses
  // CC,CE Units
  CC_Threads,
  // Synapse
  HTTPSend, blcksock, 
  // fcl-xml
  XMLRead, XMLWrite, DOM,
  // System Units
  SysUtils, Classes, Windows, ExtCtrls, Contnrs;

type
  TCEBuildType = (btOfficial, btSnapshot, btWeeklySnapshot, btDailySnapshot, btTest);
  TCEBuildTypes = set of TCEBuildType;
const
  CEBuildTypeStr: array[TCEBuildType] of String = ('official', 'snapshot', 'weekly', 'daily', 'test');

type  
  TCEVersionNumber = record
    Major: Integer;
    Minor: Integer;
    Release: Integer;
    Build: Integer;
  end;

  TCEDownloadType = (dtUpdateConf, dtPayload);
  TCEDownloadData = class
  protected
    fLastProgress: Integer;
  public
    URL: WideString;
    DownloadType: TCEDownloadType;
    HTTP: THTTPSend;
    IsValidDocument: Boolean;
    FileCount: Integer;
    Current: Integer;
    DestDir: WideString;
    Failed: Integer;
    ThreadID: Integer;
    Version: TCEVersionNumber;
    destructor Destroy; override;
  end;

  TCEDownloadProgressMsg = class
  public
    Percent: Integer;
    FileCount: Integer;
    Current: Integer;
  end;

  TCEProxyLoginPromptMsg = class
  public
    Username: String;
    Password: String;
    Remember: Boolean;
    Result: Integer;
  end;

  TCEDownloadDoneEvent = procedure(Sender: TObject; Data: TCEDownloadData) of object;
  TCEDownloadProgressEvent = procedure(Sender: TObject; Percent: Integer; Current: Integer; FileCount: Integer) of object;

  TCEVersionUpdater = class(TObject)
  private
    fUpdateConfURL: WideString;
    fBackupFolder: WideString;
    fCurrentVersion: TCEVersionNumber;
    fCurrentVersionStr: string;
    fDestroying: Boolean;
    fOnDownloadProgress: TCEDownloadProgressEvent;
    fOnDownloadUpdateConfDone: TCEDownloadDoneEvent;
    fOnDownloadVersionDone: TCEDownloadDoneEvent;
    fOutputFolder: WideString;
    fVersionFolder: WideString;
    fXML: TXMLDocument;
    procedure SetCurrentVersionStr(const Value: string);
  protected
    fThreadList: TObjectList;
    procedure ExecuteDownload(Sender: TCCBaseThread); virtual;
    procedure HandleSockStatus(Sender: TObject; Reason: THookSocketReason; const
        Value: String); virtual;
    procedure HandleSyncedMessage(Sender: TCCBaseThread; Msg: TObject); virtual;
    procedure HandleThreadTerminate(Sender: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckForNewerVersion: Boolean; virtual;
    procedure DownloadUpdateConf; virtual;
    function DownloadVersion(Version: TCEVersionNumber): Integer; virtual;
    function FindBuildNode(Version: String): TDOMNode; overload; virtual;
    function FindBuildNode(Version: TCEVersionNumber): TDOMNode; overload; virtual;
    function FindNewestVersion(OnlyNewerThanCurrent: Boolean = true; BuildTypes:
        TCEBuildTypes = []): TDOMNode; virtual;
    function LoadUpdateConfFromFile(AFilePath: WideString): Boolean; virtual;
    procedure MergeUpdateConf(XmlToMerge: TXMLDocument); virtual;
    function VersionFolderExists(Version: TCEVersionNumber): Boolean;
    procedure UseVersion(Version: TCEVersionNumber); virtual;
    function ValidateVersion(Node: TDOMNode): Boolean;
    property UpdateConfURL: WideString read fUpdateConfURL write fUpdateConfURL;
    property BackupFolder: WideString read fBackupFolder write fBackupFolder;
    property CurrentVersion: TCEVersionNumber read fCurrentVersion;
    property CurrentVersionStr: string read fCurrentVersionStr write
        SetCurrentVersionStr;
    property VersionFolder: WideString read fVersionFolder write fVersionFolder;
    property XML: TXMLDocument read fXML;
  published
    property OutputFolder: WideString read fOutputFolder write fOutputFolder;
    property OnDownloadProgress: TCEDownloadProgressEvent read fOnDownloadProgress
        write fOnDownloadProgress;
    property OnDownloadUpdateConfDone: TCEDownloadDoneEvent read
        fOnDownloadUpdateConfDone write fOnDownloadUpdateConfDone;
    property OnDownloadVersionDone: TCEDownloadDoneEvent read
        fOnDownloadVersionDone write fOnDownloadVersionDone;
  end;

  TCEUpdateFoundEvent = procedure(Sender: TObject; BuildType: TCEBuildType; Version: TCEVersionNumber; Notes: WideString; var DoUpdate: Boolean) of object;

  TCEAutoUpdater = class(TObject)
  private
    fBuildTypes: TCEBuildTypes;
    fCheckingUpdate: Boolean;
    fCloseUpdaterTimer: TTimer;
    fCurrentVersion: TCEVersionNumber;
    fCurrentVersionStr: string;
    fOnUpdateFound: TCEUpdateFoundEvent;
    fOutputFolder: WideString;
    fShowNoNewUpdatesMsg: Boolean;
    fVersionFolder: WideString;
    procedure SetCurrentVersionStr(const Value: string);
  protected
    fUpdater: TCEVersionUpdater;
    procedure HandleCloseUpdaterTimer(Sender: TObject); virtual;
    procedure HandleDownloadUpdateConfDone(Sender: TObject; Data: TCEDownloadData);
        virtual;
    procedure HandleDownloadVersionDone(Sender: TObject; Data: TCEDownloadData);
        virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckForUpdates(ShowNoUpdatesMsg: Boolean = false);
    property BuildTypes: TCEBuildTypes read fBuildTypes write fBuildTypes;
    property CurrentVersion: TCEVersionNumber read fCurrentVersion;
    property CurrentVersionStr: string read fCurrentVersionStr write
        SetCurrentVersionStr;
    property VersionFolder: WideString read fVersionFolder write fVersionFolder;
  published
    property CheckingUpdate: Boolean read fCheckingUpdate;
    property OutputFolder: WideString read fOutputFolder write fOutputFolder;
    property OnUpdateFound: TCEUpdateFoundEvent read fOnUpdateFound write fOnUpdateFound;
  end;

  function CompareVersion(Ver1, Ver2: TCEVersionNumber): Integer;
  function StrToVersionNumber(Str: String): TCEversionNumber;
  function VersionNumberToStr(Version: TCEVersionNumber): String;
  function ExtractUrlFileName(const AUrl: string): string;
  procedure UpdateFiles(ADestFolder: WideString; ASrcFolder: WideString );
  procedure ClearFolder(AFolder: WideString; ARecursive: Boolean = true);
  function NeedElevation(AFolder: WideString = ''): Boolean;
  function UpdateCEFromZip(AZipPath: WideString; ADestFolderPath: WideString;
      OldApplicationHandle: Integer = 0; CheckElevationNeed: Boolean = false):
      Boolean;

  function GetBuildType(AFrom: String): TCEBuildType;

function GetBuildTypeDescription(ABuildType: TCEBuildType): WideString;

var
  CELastVersionCheck: TDateTime;
  UpdateConfURL: String = 'http://cubicreality.pp.fi/ce/updates/updates.xml';


implementation

uses
  CE_XmlUtils, Math, MPCommonUtilities, TntClasses, TntSysUtils, TntSystem,
  TntWindows, fCE_UpdateDlg, Controls, CE_VistaFuncs, Messages, dCE_Input,
  Forms, CE_FileUtils, CE_LanguageEngine, CE_Consts, CE_Utils,
  fCE_LoginPromptDlg;

{-------------------------------------------------------------------------------
  Compare Version (Result < 0 if Ver1 is smaller,
                   Result > 0 if Ver1 is bigger,
                   Result = 0 if equal)
-------------------------------------------------------------------------------}
function CompareVersion(Ver1, Ver2: TCEVersionNumber): Integer;
begin
  Result:= Ver1.Major - Ver2.Major;
  if Result = 0 then
  Result:= Ver1.Minor - Ver2.Minor;
  if Result = 0 then
  Result:= Ver1.Release - Ver2.Release;
  if Result = 0 then
  Result:= Ver1.Build - Ver2.Build;
end;

{-------------------------------------------------------------------------------
  Str To Version Number (Str must be in format: '0.90.1.1234')
-------------------------------------------------------------------------------}
function StrToVersionNumber(Str: String): TCEversionNumber;
const
  VERSION_SEPARATOR = '.';
var
  i, p: Integer;
begin
  // Major
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Delete(Str,1,p);
  Result.Major:= i;
  // Minor
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Delete(Str,1,p);
  Result.Minor:= i;
  // Release
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Result.Release:= i;
  // Build
  i:= StrToInt(Copy(Str,p+1,Length(Str)-p));
  Result.Build:= i;
end;

{-------------------------------------------------------------------------------
  Version Number To Str
-------------------------------------------------------------------------------}
function VersionNumberToStr(Version: TCEVersionNumber): String;
begin
  Result:= IntToStr(Version.Major) + '.' +
           IntToStr(Version.Minor) + '.' +
           IntToStr(Version.Release) + '.' +
           IntToStr(Version.Build);
end;

{-------------------------------------------------------------------------------
  ExtractUrlFileName
-------------------------------------------------------------------------------}
function ExtractUrlFileName(const AUrl: string): string;
var 
  i: Integer;
begin 
  i:= LastDelimiter('/', AUrl);
  Result:= Copy(AUrl, i + 1, Length(AUrl) - (i));
end;

{-------------------------------------------------------------------------------
  Update Files
-------------------------------------------------------------------------------}
procedure UpdateFiles(ADestFolder: WideString; ASrcFolder: WideString );

  procedure EnumFolder(AFolder: WideString; SubFolder: WideString);
  var
    sr: TSearchRecW;
    dir: WideString;
  begin
    if WideFindFirst(AFolder + '*', faDirectory or faAnyFile, sr) = S_OK then
    begin
      try
        repeat
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            // sub folder
            if (sr.Attr and faDirectory) = faDirectory then
            begin
              // create dir if it doesn't exist
              dir:= ADestFolder + SubFolder + sr.Name;
              if not WideDirectoryExists(dir) then
              WideCreateDir(dir);
              // enum sub folder
              EnumFolder(AFolder + sr.Name + '\', SubFolder + sr.Name + '\');
            end
            // file
            else
            begin
              dir:= ADestFolder + SubFolder;
              WideCopyFile(AFolder + sr.Name, dir + sr.Name, false);
            end;
          end;
        until WideFindNext(sr) <> S_OK;
      finally
        WideFindClose(sr);
      end;
    end;
  end;

begin
  if WideDirectoryExists(ADestFolder) and WideDirectoryExists(ASrcFolder) then
  begin
    ADestFolder:= WideIncludeTrailingPathDelimiter(ADestFolder);
    ASrcFolder:= WideIncludeTrailingPathDelimiter(ASrcFolder);
    EnumFolder(ASrcFolder, '');
  end;
end;

{-------------------------------------------------------------------------------
  Clear Folder
-------------------------------------------------------------------------------}
procedure ClearFolder(AFolder: WideString; ARecursive: Boolean = true);
var
  sr: TSearchRecW;
begin
  AFolder:= WideIncludeTrailingPathDelimiter(AFolder);
  if WideFindFirst(AFolder + '*', faDirectory or faAnyFile or faHidden, sr) = S_OK then
  begin
    try
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          if (sr.Attr and faDirectory) = faDirectory then
          begin
            if ARecursive then
            begin
              ClearFolder(AFolder + sr.Name, true);
              WideRemoveDir(AFolder + sr.Name);
            end;
          end
          else
          WideDeleteFile(AFolder + sr.Name);
        end;
      until WideFindNext(sr) <> S_OK;
    finally
      WideFindClose(sr);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Need Elevation test
-------------------------------------------------------------------------------}
function NeedElevation(AFolder: WideString = ''): Boolean;
var
  dir, tmpFile: WideString;
  h: Integer;
begin
  Result:= false;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
  begin
    if AFolder <> '' then
    dir:= WideIncludeTrailingPathDelimiter(AFolder)
    else
    dir:= WideExtractFilePath(WideParamStr(0));
    // Generate random file name
    Randomize;
    tmpFile:= 'tmp' + IntToStr(Random($FFFFFF)) + '.exe';
    while WideFileExists(dir + tmpFile) do
    begin
      tmpFile:= 'tmp' + IntToStr(Random($FFFFFF)) + '.exe';
    end;
    tmpFile:= dir + tmpFile;
    // Try to create a file
    h:= WideFileCreate(tmpFile);
    if GetLastError = ERROR_ACCESS_DENIED then
    Result:= true;
    if h > -1 then
    begin
      FileClose(h);
      // Delete created file
      if WideFileExists(tmpFile) then
      WideDeleteFile(tmpFile);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Extract Zip Tp
-------------------------------------------------------------------------------}
function UpdateCEFromZip(AZipPath: WideString; ADestFolderPath: WideString;
    OldApplicationHandle: Integer = 0; CheckElevationNeed: Boolean = false):
    Boolean;
var
  dlg: TCEUpdateDlg;
begin
  Result:= false;
  if CheckElevationNeed and NeedElevation then
  begin
    AZipPath:= GetRedirectedPath(AZipPath);
    WideShellExecute(0,
                     'runas',
                     WideParamStr(0),
                     '/update ' + '"' + AZipPath + '" "' + ADestFolderPath + '" ' + IntToStr(OldApplicationHandle),
                     ADestFolderPath);
    Exit;
  end;

  dlg:= TCEUpdateDlg.Create(nil);
  try
    try
      dlg.OpenArchive(AZipPath);
      dlg.DestinationDir:= ADestFolderPath;
    except
    end;
    Result:= dlg.ShowModal = mrOk;
  finally
    dlg.Free;

    if Result then
    begin
      if TaskDialog(GetActiveWindow,
                   _('Restart needed!'),
                   _('Restart CubicExplorer Now?'),
                   '',
                   TD_ICON_QUESTION,
                   TD_BUTTON_YES+TD_BUTTON_NO) = mrYes then
      begin
        if OldApplicationHandle <> 0 then
        PostMessage(OldApplicationHandle, WM_USER + 101, 102, 0); // Post restart message
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get BuildType from string
-------------------------------------------------------------------------------}
function GetBuildType(AFrom: String): TCEBuildType;
begin
  for Result:= Low(Result) to High(Result) do
  begin
    if AFrom = CEBuildTypeStr[Result] then
    Exit; // ->
  end;
  // Raise exception if invalid string was used
  raise Exception.Create('Invalid BuildType string used!');
end;

{-------------------------------------------------------------------------------
  Get BuildType Description
-------------------------------------------------------------------------------}
function GetBuildTypeDescription(ABuildType: TCEBuildType): WideString;
begin
  Result:= '';
  case ABuildType of
    btOfficial: Result:= _('Official');
    btSnapshot: Result:= _('Snapshot');
    btWeeklySnapshot: Result:= _('Snapshot'); // Weekly is depricated
    btDailySnapshot: Result:= _('Daily');
    btTest: Result:= _('For testing');
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEVersionUpdater
-------------------------------------------------------------------------------}
constructor TCEVersionUpdater.Create;
begin
  inherited;
  fXML:= TXMLDocument.Create;
  fOnDownloadProgress:= nil;
  fOnDownloadUpdateConfDone:= nil;
  fOnDownloadVersionDone:= nil;
  fThreadList:= TObjectList.Create(false);
  fDestroying:= false;
end;

{-------------------------------------------------------------------------------
  Destroy TCEVersionUpdater
-------------------------------------------------------------------------------}
destructor TCEVersionUpdater.Destroy;
var
  i: Integer;
  thread: TThread;
begin
  fDestroying:= true;
  // Terminate threads
  for i:= 0 to fThreadList.Count - 1 do
  begin
    thread:= TThread(fThreadList.Items[i]);
    thread.OnTerminate:= nil;
    thread.Terminate;
  end;

  FreeAndNil(fThreadList);

  if assigned(fXML) then
  FreeAndNil(fXML);
  inherited;
end;

{-------------------------------------------------------------------------------
  Check For Newer Version
-------------------------------------------------------------------------------}
function TCEVersionUpdater.CheckForNewerVersion: Boolean;
var
  rootNode, buildNode: TDOMNode;
  ver: TCEVersionNumber;
begin
  Result:= false;
  if assigned(fXML.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(fXML.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      buildNode:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(buildNode) and (buildNode is TDOMElement) do
      begin
        try
          ver:= StrToVersionNumber(TDOMElement(buildNode).AttribStrings['version']);
          if CompareVersion(CurrentVersion, ver) < 0  then
          begin
            Result:= true;
            Exit;
          end;
        except
        end;
        buildNode:= FindNextSiblingDOMNode(buildNode, 'Build');
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Download UpdateConf
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.DownloadUpdateConf;
var
  thread: TCCBaseThread;
  data: TCEDownloadData;
  proxy: String;
  port: Integer;
begin
  thread:= TCCBaseThread.Create(true);
  fThreadList.Add(thread);
  thread.FreeOnTerminate:= true;
  thread.OnExecute:= ExecuteDownload;
  thread.OnSyncedMessage:= HandleSyncedMessage;
  thread.Name:= 'Downloader_UpdateConf';
  thread.OnTerminate:= HandleThreadTerminate;
  // Create data
  data:= TCEDownloadData.Create;
  data.URL:= UpdateConfURL;
  data.DownloadType:= dtUpdateConf;
  data.HTTP:= THTTPSend.Create;
  if CE_UseProxy then
  begin
    if CE_UseSystemProxy then
    begin
      proxy:= GetSystemProxyServer('http');
      data.HTTP.ProxyHost:= ExtractUrlPort(proxy, port);
      data.HTTP.ProxyPort:= IntToStr(port);
    end
    else
    begin
      data.HTTP.ProxyHost:= ExtractUrlPort(CE_ProxyAddress, port);
      data.HTTP.ProxyPort:= IntToStr(port);
    end;
    data.HTTP.ProxyUser:= CE_ProxyUsername;
    data.HTTP.ProxyPass:= CE_ProxyPassword;
  end;
  data.ThreadID:= Integer(thread);
  thread.Data:= data;
  thread.FreeDataOnDestroy:= true;
  // Start
  thread.Resume;
end;

{-------------------------------------------------------------------------------
  Download Version (Returns pointer to the thread)
-------------------------------------------------------------------------------}
function TCEVersionUpdater.DownloadVersion(Version: TCEVersionNumber): Integer;
var
  thread: TCCBaseThread;
  data: TCEDownloadData;
  buildNode, fileNode: TDOMNode;
  list: TStrings;
  dir: WideString;
  rootURL: String;
  proxy: String;
  port: Integer;
begin
  Result:= 0;
  list:= TStringList.Create;
  list.Delimiter:= ';';
  try
    buildNode:= FindBuildNode(Version);
    if assigned(buildNode) then
    begin
      // Get root URL
      rootURL:= TDOMElement(buildNode).AttribStrings['root'];
      if Length(rootURL) > 0 then
      begin
        if rootURL[Length(rootURL)] <> '/' then
        rootURL:= rootURL + '/';
      end;

      fileNode:= FindFirstChildDOMNode(buildNode, 'File');
      while assigned(fileNode) do
      begin
        list.Add(rootURL + TDOMElement(fileNode).AttribStrings['src']);
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;

      // Check/Create main Versions folder
      dir:= WideIncludeTrailingBackslash(VersionFolder);
      if not WideDirectoryExists(dir) then
        if not WideCreateDir(dir) then
        begin
          MessageBox(0, PChar(String(dir)), 'Could not create folder!', MB_ICONERROR or MB_OK);
        end;

      // Check/Create version folder
      dir:= dir + VersionNumberToStr(Version) + '\';
      if not WideDirectoryExists(dir) then
      begin
        if not WideCreateDir(dir) then
        begin
          MessageBox(0, PChar(String(dir)), 'Could not create folder!', MB_ICONERROR or MB_OK);
        end;
      end
      else
      ClearFolder(dir);

      if WideDirectoryExists(dir) then
      begin
        thread:= TCCBaseThread.Create(true);
        fThreadList.Add(thread);
        Result:= Integer(thread);
        thread.FreeOnTerminate:= true;
        thread.OnExecute:= ExecuteDownload;
        thread.OnSyncedMessage:= HandleSyncedMessage;
        thread.OnTerminate:= HandleThreadTerminate;
        thread.Name:= 'Downloader_Payload';
        // Create data
        data:= TCEDownloadData.Create;
        data.URL:= list.DelimitedText;
        data.DestDir:= dir;
        data.DownloadType:= dtPayload;
        data.HTTP:= THTTPSend.Create;
        if CE_UseProxy then
        begin
          if CE_UseSystemProxy then
          begin
            proxy:= GetSystemProxyServer('http');
            data.HTTP.ProxyHost:= ExtractUrlPort(proxy, port);
            data.HTTP.ProxyPort:= IntToStr(port);
          end
          else
          begin
            data.HTTP.ProxyHost:= ExtractUrlPort(CE_ProxyAddress, port);
            data.HTTP.ProxyPort:= IntToStr(port);
          end;
        end;
        data.HTTP.Sock.OnStatus:= HandleSockStatus;
        data.FileCount:= list.Count;
        data.Current:= 0;
        data.fLastProgress:= 0;
        data.Failed:= 0;
        data.ThreadID:= Result;
        data.Version:= Version;
        thread.Data:= data;
        thread.FreeDataOnDestroy:= true;
        // Start
        thread.Resume;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Execute Download (!!! Runs in separate thread !!!)
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.ExecuteDownload(Sender: TCCBaseThread);
var
  data: TCEDownloadData;
  list: TStrings;
  i: Integer;
  fs: TTntFileStream;
  fileName: String;
  loginMsg: TCEProxyLoginPromptMsg;
begin
  data:= TCEDownloadData(Sender.Data);
  try
    if data.DownloadType = dtUpdateConf then
    begin
      data.HTTP.HTTPMethod('GET', data.URL);
      if data.HTTP.ResultCode = 407 then
      begin
        loginMsg:= TCEProxyLoginPromptMsg.Create;
        try
          Sender.SendSyncedMessage(loginMsg);
          if loginMsg.Result = mrOK then
          begin
            data.HTTP.ProxyUser:= loginMsg.Username;
            data.HTTP.ProxyPass:= loginMsg.Password;
            data.HTTP.HTTPMethod('GET', data.URL);
            if not loginMsg.Remember then
            begin
              data.HTTP.ProxyUser:= '';
              data.HTTP.ProxyPass:= '';
            end;
          end;
        finally
          loginMsg.Free;
        end;
      end;
    end
    else
    begin
      list:= TStringList.Create;
      list.Delimiter:= ';';
      list.DelimitedText:= data.URL;
      data.HTTP.Sock.Tag:= Integer(Sender);
      try
        for i:= 0 to list.Count - 1 do
        begin
          data.Current:= i + 1;
          data.HTTP.HTTPMethod('GET', list.Strings[i]);
          // Create file
          fileName:= ExtractUrlFileName(list.Strings[i]);
          if (data.HTTP.ResultCode = 200) and (fileName <> '') then
          begin
            try
              fs:= TTntFileStream.Create(data.DestDir + fileName, fmCreate);
              try
                fs.Seek(0, soFromBeginning);
                fs.CopyFrom(data.HTTP.Document, 0);
              finally
                fs.Free;
              end;
            except
              data.Failed:= data.Failed + 1;
            end;
          end
          else
          data.Failed:= data.Failed + 1;
        end;
      finally
        list.Free;
      end;
    end;
  finally
    // Send Synced "Done" message
    if not Sender.Terminated then
    Sender.SendSyncedMessage(Sender.Data);
  end;
end;

{-------------------------------------------------------------------------------
  Handle Sock Status  (!!! Runs in separate thread !!!)
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.HandleSockStatus(Sender: TObject; Reason:
    THookSocketReason; const Value: String);
var
  thread: TCCBaseThread;
  msg: TCEDownloadProgressMsg;
  http: THTTPSend;
  data: TCEDownloadData;
begin
  if Reason = HR_ReadCount then
  begin
    thread:= TCCBaseThread(TBlockSocket(Sender).Tag);
    http:= THTTPSend(TBlockSocket(Sender).Owner);
    if assigned(thread) then
    begin
      data:= TCEDownloadData(thread.Data);
      if (GetTickCount - data.fLastProgress) > 50 then // Send Progress event at maximum every 50ms.
      begin
        data.fLastProgress:= GetTickCount;
        msg:= TCEDownloadProgressMsg.Create;
        try
          if http.Document.Size > 0 then
          msg.Percent:= Min(Ceil((100 / http.DownloadSize) * http.Document.Size), 100)
          else
          msg.Percent:= 0;

          msg.FileCount:= data.FileCount;
          msg.Current:= data.Current;
          if thread.Terminated then
          http.Abort
          else
          thread.SendSyncedMessage(msg);
        finally
          msg.Free;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle SyncedMessage
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.HandleSyncedMessage(Sender: TCCBaseThread; Msg:
    TObject);
var
  stream: TStream;
  tmpXML: TXMLDocument;
  dlg: TCELoginPromptDlg;
begin
  if Sender.Terminated then
  Exit;
  
  if Msg is TCEDownloadData then
  begin
    // UpdateConf download done. Load XML from receive stream.
    if TCEDownloadData(Msg).DownloadType = dtUpdateConf then
    begin
      try
        if TCEDownloadData(Msg).HTTP.ResultCode = 200 then
        begin
          stream:= TCEDownloadData(Msg).HTTP.Document;
          try
            stream.Position:= 0;

            ReadXMLFile(tmpXML, stream);
            try
              // Check if valid xml file
              if assigned(FindFirstChildDOMNode(tmpXML.DocumentElement, 'Updates')) then
              begin
                TCEDownloadData(Msg).IsValidDocument:= true;
                MergeUpdateConf(tmpXml);
              end;
            finally
              tmpXML.Free;
            end;
          except on EXMLReadError do

          end;
        end;
        if CE_UseProxy then
        begin
          CE_ProxyUsername:= TCEDownloadData(Msg).HTTP.ProxyUser;
          CE_ProxyPassword:= TCEDownloadData(Msg).HTTP.ProxyPass;
        end;    
      finally
        if assigned(fOnDownloadUpdateConfDone) then
        fOnDownloadUpdateConfDone(Self, TCEDownloadData(Msg));
      end;
    end
    // Version download is done.
    else
    begin
      if assigned(fOnDownloadVersionDone) then
        fOnDownloadVersionDone(Self, TCEDownloadData(Msg));
    end;
  end
  else if Msg is TCEDownloadProgressMsg then
  begin
    if assigned(fOnDownloadProgress) then
    fOnDownloadProgress(Sender,
                        TCEDownloadProgressMsg(Msg).Percent,
                        TCEDownloadProgressMsg(Msg).Current,
                        TCEDownloadProgressMsg(Msg).FileCount);
  end
  else if Msg is TCEProxyLoginPromptMsg then
  begin
    dlg:= TCELoginPromptDlg.Create(nil);
    try
      TCEProxyLoginPromptMsg(Msg).Result:= dlg.ShowModal;
      TCEProxyLoginPromptMsg(Msg).Username:= dlg.edit_username.Text;
      TCEProxyLoginPromptMsg(Msg).Password:= dlg.edit_password.Text;
      TCEProxyLoginPromptMsg(Msg).Remember:= dlg.check_save.Checked;
    finally
      dlg.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Build Node
-------------------------------------------------------------------------------}
function TCEVersionUpdater.FindBuildNode(Version: TCEVersionNumber): TDOMNode;
var
  rootNode: TDOMNode;
  s: String;
begin
  if assigned(Xml.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(Xml.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      s:= VersionNumberToStr(Version);
      Result:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(Result) do
      begin
        if TDOMElement(Result).AttribStrings['version'] = s then
        Exit; // -->
        Result:= FindNextSiblingDOMNode(Result, 'Build');
      end;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Find Build Node
-------------------------------------------------------------------------------}
function TCEVersionUpdater.FindBuildNode(Version: String): TDOMNode;
var
  rootNode: TDOMNode;
begin
  if assigned(Xml.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(Xml.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      Result:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(Result) do
      begin
        if TDOMElement(Result).AttribStrings['version'] = Version then
        Exit; // -->
        Result:= FindNextSiblingDOMNode(Result, 'Build');
      end;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Find Newest version
-------------------------------------------------------------------------------}
function TCEVersionUpdater.FindNewestVersion(OnlyNewerThanCurrent: Boolean =
    true; BuildTypes: TCEBuildTypes = []): TDOMNode;
var
  rootNode, buildNode: TDOMNode;
  ver, highVer: TCEVersionNumber;
  buildType: TCEBuildType;
begin
  Result:= nil;
  if BuildTypes = [] then
  Exit;

  if OnlyNewerThanCurrent then
  highVer:= CurrentVersion
  else
  begin
    highVer.Major:= 0;
    highVer.Minor:= 0;
    highVer.Release:= 0;
    highVer.Build:= 0;
  end;
  if assigned(fXML.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(fXML.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      buildNode:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(buildNode) and (buildNode is TDOMElement) do
      begin
        try
          buildType:= GetBuildType(TDOMElement(buildNode).AttribStrings['type']);

          if (buildType in BuildTypes) or ((buildType = btSnapshot) and (btWeeklySnapshot in BuildTypes)) then
          begin
            ver:= StrToVersionNumber(TDOMElement(buildNode).AttribStrings['version']);
            if CompareVersion(highVer, ver) < 0  then
            begin
              Result:= buildNode;
              highVer:= ver;
            end;
          end;
        except
        end;
        buildNode:= FindNextSiblingDOMNode(buildNode, 'Build');
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle ThreadTerminate
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.HandleThreadTerminate(Sender: TObject);
begin
  if not fDestroying then
  fThreadList.Remove(Sender);
end;

{-------------------------------------------------------------------------------
  Load UpdateConf From File
-------------------------------------------------------------------------------}
function TCEVersionUpdater.LoadUpdateConfFromFile(AFilePath: WideString):
    Boolean;
var
  fs: TStream;
begin
  Result:= false;
  if WideFileExists(AFilePath) then
  begin
    fs:= TTntFileStream.Create(AFilePath, fmOpenRead);
    try
      fs.Seek(0, soFromBeginning);
      try
        if assigned(fXML) then
        FreeAndNil(fXML);
        
        ReadXMLFile(fXML, fs);
        Result:= true;
      except on EXMLReadError do
        if not assigned(fXML) then
        begin
          fXML:= TXMLDocument.Create;
          fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
        end
        else if not assigned(fXML.DocumentElement) then
        begin
          fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Merge Update Conf
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.MergeUpdateConf(XmlToMerge: TXMLDocument);
var
  fs: TTntFileStream;
  updNode, buildNode, tmpNode: TDOMNode;
  newUpdNode, newBuildNode: TDOMNode;
begin
  if not assigned(XMLToMerge) and not assigned(XmlToMerge.DocumentElement) then
  Exit;
  
  // Create Base XML
  if not assigned(fXML) then
  begin
    fXML:= TXMLDocument.Create;
    fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
  end
  else if not assigned(fXML.DocumentElement) then
  begin
    fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
  end;
  // Get/Create Updates node
  updNode:= FindFirstChildDOMNode(fXML.DocumentElement, 'Updates');
  if not assigned(updNode) then
  begin
    updNode:= fXML.CreateElement('Updates');
    fXML.DocumentElement.AppendChild(updNode);
  end;

  // Remove non offline build nodes
  buildNode:= FindFirstChildDOMNode(updNode, 'Build');
  while assigned(buildNode) do
  begin
    tmpNode:= buildNode;
    buildNode:= FindNextSiblingDOMNode(buildNode, 'Build');
    if not WideDirectoryExists(WideIncludeTrailingPathDelimiter(VersionFolder) +
                               TDOMElement(tmpNode).AttribStrings['version']) then
    begin
      updNode.RemoveChild(tmpNode);
    end;
  end;

  // Merge build nodes
  newUpdNode:= FindFirstChildDOMNode(XmlToMerge.DocumentElement, 'Updates');
  if assigned(newUpdNode) then
  begin
    newBuildNode:= FindFirstChildDOMNode(newUpdNode, 'Build');
    while assigned(newBuildNode) do
    begin
      if not assigned(FindBuildNode(TDOMElement(newBuildNode).AttribStrings['version'])) then
      begin
        buildNode:= newBuildNode.CloneNode(true, fXML);
        updNode.AppendChild(buildNode);
      end;
      newBuildNode:= FindNextSiblingDOMNode(newBuildNode, 'Build');
    end;
  end;

  // save UpdateConf to disk for offline use
  if not WideDirectoryExists(VersionFolder) then
  WideCreateDir(VersionFolder);
  if WideDirectoryExists(VersionFolder) then
  begin
    try
      fs:= TTntFileStream.Create(VersionFolder + 'updates.xml', fmCreate);
      try
        fs.Seek(0, soFromBeginning);
        WriteXMLFile(fXML, fs);
      finally
        fs.Free;
      end;
    except
    end;
  end;
end;

{-------------------------------------------------------------------------------
  VersionFolderExists
-------------------------------------------------------------------------------}
function TCEVersionUpdater.VersionFolderExists(Version: TCEVersionNumber):
    Boolean;
begin
  Result:= WideDirectoryExists(WideIncludeTrailingPathDelimiter(VersionFolder) + VersionNumberToStr(Version));
end;

{-------------------------------------------------------------------------------
  Set CurrentVersionStr
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.SetCurrentVersionStr(const Value: string);
begin
  fCurrentVersionStr:= Value;
  fCurrentVersion:= StrToVersionNumber(fCurrentVersionStr);
end;

{-------------------------------------------------------------------------------
  UseVersion
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.UseVersion(Version: TCEVersionNumber);
var
  buildNode, fileNode: TDOMNode;
  dir: WideString;
  path: WideString;
  op: WideString;
begin
  buildNode:= FindBuildNode(Version);
  if assigned(buildNode) then
  begin
    dir:= WideIncludeTrailingBackslash(VersionFolder) + TDOMElement(buildNode).AttribStrings['version'];
    if WideDirectoryExists(dir) then
    begin
      fileNode:= FindFirstChildDOMNode(buildNode, 'File');
      while assigned(fileNode) do
      begin
        path:= TDOMElement(fileNode).AttribStrings['src'];
        path:= WideStringReplace(path, '/', '\', [rfReplaceAll]);
        path:= WideIncludeTrailingPathDelimiter(dir) + path;
        if WideFileExists(path) then
        begin
          op:= TDOMElement(fileNode).AttribStrings['operation'];
          if op = 'extract' then
          begin
            UpdateCEFromZip(path, OutputFolder, CEInput.MsgInput.Handle, true);
          end;
        end;
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;
    end;  
  end;
end;

{-------------------------------------------------------------------------------
  Validate Version
-------------------------------------------------------------------------------}
function TCEVersionUpdater.ValidateVersion(Node: TDOMNode): Boolean;
var
  fileNode: TDOMNode;
  dir: WideString;
  path: WideString;
begin
  Result:= false;
  if assigned(Node) then
  begin
    dir:= WideIncludeTrailingBackslash(VersionFolder) + TDOMElement(Node).AttribStrings['version'];
    if WideDirectoryExists(dir) then
    begin
      fileNode:= FindFirstChildDOMNode(Node, 'File');
      while assigned(fileNode) do
      begin
        path:= TDOMElement(fileNode).AttribStrings['src'];
        path:= WideStringReplace(path, '/', '\', [rfReplaceAll]);
        path:= WideIncludeTrailingPathDelimiter(dir) + path;
        if not WideFileExists(path) then
        Exit;
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;
      Result:= true;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy TCEDownloadData
-------------------------------------------------------------------------------}
destructor TCEDownloadData.Destroy;
begin
  if assigned(HTTP) then HTTP.Free;
  inherited;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create TCEAutoUpdater
-------------------------------------------------------------------------------}
constructor TCEAutoUpdater.Create;
begin
  inherited;
  fBuildTypes:= [btOfficial, btWeeklySnapshot];
end;

{-------------------------------------------------------------------------------
  Destroy TCEAutoUpdater
-------------------------------------------------------------------------------}
destructor TCEAutoUpdater.Destroy;
begin
  if assigned(fCloseUpdaterTimer) then
  FreeAndNil(fCloseUpdaterTimer);

  if assigned(fUpdater) then
  FreeAndNil(fUpdater);

  inherited;
end;

{-------------------------------------------------------------------------------
  Check For Updates
-------------------------------------------------------------------------------}
procedure TCEAutoUpdater.CheckForUpdates(ShowNoUpdatesMsg: Boolean = false);
begin
  if fCheckingUpdate then
  Exit;

  if BuildTypes = [] then
  begin
    if ShowNoUpdatesMsg then
    begin
      WideMessageBox(Application.MainFormHandle,
                     _('Check For Updates'),
                     _('You have no update type notifications selected.')+#13#10+_('Use Options->Updates to change them.'),
                     MB_ICONINFORMATION or MB_OK);
    end;
    Exit;
  end;

  fShowNoNewUpdatesMsg:= ShowNoUpdatesMsg;
  fCheckingUpdate:= true;
  try
    if assigned(fCloseUpdaterTimer) then
    fCloseUpdaterTimer.Enabled:= false;

    if not assigned(fUpdater) then
    begin
      fUpdater:= TCEVersionUpdater.Create;
      fUpdater.CurrentVersionStr:= CurrentVersionStr;
      fUpdater.UpdateConfURL:= UpdateConfURL;
      fUpdater.VersionFolder:= VersionFolder;
      fUpdater.OutputFolder:= OutputFolder;
      fUpdater.OnDownloadUpdateConfDone:= HandleDownloadUpdateConfDone;
      fUpdater.OnDownloadVersionDone:= HandleDownloadVersionDone;
      fUpdater.LoadUpdateConfFromFile(VersionFolder + 'updates.xml');
    end;
    fUpdater.DownloadUpdateConf;
  except
    fCheckingUpdate:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Handle CloseUpdaterTimer
-------------------------------------------------------------------------------}
procedure TCEAutoUpdater.HandleCloseUpdaterTimer(Sender: TObject);
begin
  fCloseUpdaterTimer.Enabled:= false;
  FreeAndNil(fUpdater);
  fCheckingUpdate:= false;
end;

{-------------------------------------------------------------------------------
  HandleDownloadUpdateConfDone
-------------------------------------------------------------------------------}
procedure TCEAutoUpdater.HandleDownloadUpdateConfDone(Sender: TObject; Data:
    TCEDownloadData);
var
  newBuild: TDOMNode;
  notesNode: TDOMNode;
  ver: TCEVersionNumber;
  buildType: TCEBuildType;
  doUpdate: Boolean;
  notes: WideString;
  msg: WideString;
begin
  newBuild:= fUpdater.FindNewestVersion(true, BuildTypes);
  if assigned(newBuild) then
  begin
    if assigned(fOnUpdateFound) then
    begin
      ver:= StrToVersionNumber(TDOMElement(newBuild).AttribStrings['version']);
      buildType:= GetBuildType(TDOMElement(newBuild).AttribStrings['type']);
      doUpdate:= false;
      notesNode:= FindFirstChildDOMNode(newBuild, 'Notes');
      if assigned(notesNode) then
      notes:= notesNode.TextContent;

      fOnUpdateFound(Self, buildType, ver, Notes, doUpdate);
      if doUpdate then
      begin
        fUpdater.DownloadVersion(ver);
      end;
    end;
  end
  else if fShowNoNewUpdatesMsg then
  begin
    // Show error message
    if Data.HTTP.ResultCode = 200 then
    msg:= _('No new updates')
    else if Data.HTTP.ResultCode = 500 then
    msg:= _('Error: Could not connect!')
    else if Data.HTTP.ResultString <> '' then
    msg:= _('Error') + ': ' + IntToStr(Data.HTTP.ResultCode) + ' - ' + Data.HTTP.ResultString
    else
    msg:= _('Error') + ': ' + IntToStr(Data.HTTP.ResultCode);

    TaskDialog(Application.MainFormHandle,
               _('Check For Updates'),
               msg,
               '',
               TD_ICON_INFORMATION,
               TD_BUTTON_OK);
  end;

  // Create timer to free fUpdater after 5 secounds
  if not doUpdate then
  begin
    if not assigned(fCloseUpdaterTimer) then
    begin
      fCloseUpdaterTimer:= TTimer.Create(nil);
      fCloseUpdaterTimer.Enabled:= false;
      fCloseUpdaterTimer.Interval:= 1000;
      fCloseUpdaterTimer.OnTimer:= HandleCloseUpdaterTimer;
    end;
    fCloseUpdaterTimer.Enabled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Handle DownloadVersionDone
-------------------------------------------------------------------------------}
procedure TCEAutoUpdater.HandleDownloadVersionDone(Sender: TObject; Data:
    TCEDownloadData);
begin
  if Data.Current = Data.FileCount then
  begin
    fUpdater.UseVersion(Data.Version);
    if not assigned(fCloseUpdaterTimer) then
    begin
      fCloseUpdaterTimer:= TTimer.Create(nil);
      fCloseUpdaterTimer.Enabled:= false;
      fCloseUpdaterTimer.Interval:= 1000;
      fCloseUpdaterTimer.OnTimer:= HandleCloseUpdaterTimer;
    end;
    fCloseUpdaterTimer.Enabled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Set CurrentVersionStr
-------------------------------------------------------------------------------}
procedure TCEAutoUpdater.SetCurrentVersionStr(const Value: string);
begin
  fCurrentVersionStr:= Value;
  fCurrentVersion:= StrToVersionNumber(fCurrentVersionStr);
end;

end.
