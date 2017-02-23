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
//  The Original Code is CE_AppPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_AppPanel;

interface

uses
  // Tnt
  TntWindows, TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Classes, ExtCtrls, Forms, Dialogs, Controls;

type
  TCEAppEmbedPanel = class(TPanel)
  private
    fAppWndClass: string;
    fAppWndStyle: Cardinal;
    fAppWndText: WideString;
    fCommandLine: WideString;
  protected
    procedure InitApp; virtual;
    function RunApp: Boolean; virtual;
  public
    fAppWndHandle: HWND;
    OffsetRect: TRect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Close: Boolean;
    function GetAppWndText: WideString;
    function IsAppRunning: Boolean;
    procedure Resize; override;
    procedure ResizeApp; virtual;
    function Run: Boolean;
    property AppWndClass: string read fAppWndClass write fAppWndClass;
    property AppWndHandle: HWND read fAppWndHandle;
    property AppWndStyle: Cardinal read fAppWndStyle write fAppWndStyle;
    property AppWndText: WideString read fAppWndText write fAppWndText;
    property CommandLine: WideString read fCommandLine write fCommandLine;
  end;

implementation

var
  tmpHandle: HWND;
  tmpClassName: String;
  tmpWndText: String;
  tmpClassNameW: WideString;
  tmpWndTextW: WideString;

{*------------------------------------------------------------------------------
  Enumerate Windows to find MMC's main window.
-------------------------------------------------------------------------------}
function GetWindowProc(Handle: HWND; LParam: longint): bool; stdcall;
var
  Buffer: array [0..255] of char;
  BufferW: array [0..255] of widechar;
  tid: Integer;
begin
  Result := true;
  tid:= GetWindowThreadProcessID(Handle,nil);
  if tid = LParam then
  begin
    if Win32PlatformIsUnicode then
    begin
      GetClassNameW(Handle,BufferW,255);
      if BufferW = tmpClassNameW then
      begin
        if tmpWndTextW <> '' then
        begin
          GetWindowTextW(Handle,BufferW,255);
          if BufferW = tmpWndTextW then
          begin
            tmpHandle:= Handle;
            Result:= false;
          end;
        end
        else
        begin
          tmpHandle:= Handle;
          Result:= false;
        end;
      end;
    end
    else
    begin
      GetClassName(Handle,Buffer,255);
      if Buffer = tmpClassName then
      begin
        if tmpWndText <> '' then
        begin
          GetWindowText(Handle,Buffer,255);
          if Buffer = tmpWndText then
          begin
            tmpHandle:= Handle;
            Result:= false;
          end;
        end
        else
        begin
          tmpHandle:= Handle;
          Result:= false;
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Kill Process
-------------------------------------------------------------------------------}
procedure KillProcess(hWindowHandle: HWND);
var
  hprocessID: INTEGER;
  processHandle: THandle;
  DWResult: DWORD;
begin
  SendMessageTimeout(hWindowHandle, WM_CLOSE, 0, 0,
    SMTO_ABORTIFHUNG or SMTO_NORMAL, 5000, DWResult);

  if isWindow(hWindowHandle) then
  begin
    // PostMessage(hWindowHandle, WM_QUIT, 0, 0);

    { Get the process identifier for the window}
    GetWindowThreadProcessID(hWindowHandle, @hprocessID);
    if hprocessID <> 0 then
    begin
      { Get the process handle }
      processHandle := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION,
        False, hprocessID);
      if processHandle <> 0 then
      begin
        { Terminate the process }
        TerminateProcess(processHandle, 0);
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEAppEmbedPanel
-------------------------------------------------------------------------------}
constructor TCEAppEmbedPanel.Create(AOwner: TComponent);
begin
  inherited;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  BorderStyle:= bsNone;
  fAppWndClass:= '';
  fAppWndText:= '';
  fCommandLine:= '';
  fAppWndHandle:= 0;
  fAppWndStyle:= WS_CHILD;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEAppEmbedPanel
-------------------------------------------------------------------------------}
destructor TCEAppEmbedPanel.Destroy;
begin
  Close;
  inherited;
end;

{*------------------------------------------------------------------------------
  Create app
-------------------------------------------------------------------------------}
function TCEAppEmbedPanel.RunApp: Boolean;
var
  StartInfoW  : TStartupInfoW;
  ProcInfo   : TProcessInformation;
  CreateOK   : Boolean;
  i: Integer;
begin
  FillChar(StartInfoW,SizeOf(TStartupInfoW),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfoW.cb := SizeOf(TStartupInfoW);
  StartInfoW.wShowWindow:= SW_HIDE;
  StartInfoW.dwFlags:= STARTF_USESHOWWINDOW;
  try
    CreateOK:= Tnt_CreateProcessW(nil,
                                  PWideChar(fCommandLine),
                                  nil,
                                  nil,
                                  false,
                                  NORMAL_PRIORITY_CLASS,
                                  nil,
                                  nil,
                                  StartInfoW,
                                  ProcInfo);
  except
    CreateOK:= false;
  end;

  Result:= CreateOK;
  if CreateOK then
  begin
    // init tmp values
    tmpHandle:= 0;
    tmpClassName:= fAppWndClass;
    tmpClassNameW:= fAppWndClass;
    tmpWndText:= fAppWndText;
    tmpWndTextW:= fAppWndText;
    // Get Main Form
    for i:= 0 to 30 do
    begin
      EnumWindows(@GetWindowProc, ProcInfo.dwThreadId);
      if tmpHandle = 0 then
      sleep(100)
      else
      break;
    end;
    fAppWndHandle:= tmpHandle;
    InitApp;
    ResizeApp;
    Application.ProcessMessages;
    ResizeApp;
  end;
end;

{*------------------------------------------------------------------------------
  Initialize the app
-------------------------------------------------------------------------------}
procedure TCEAppEmbedPanel.InitApp;
var
  FTID:    DWord;  // Foreground Thread ID
  TTID:    DWord;  // This Thread ID
begin
  if fAppWndHandle <> 0 then
  begin
    Windows.SetParent(fAppWndHandle, Handle);

    SetWindowLong(fAppWndHandle, GWL_STYLE, fAppWndStyle);
    SetWindowPos(fAppWndHandle,
                 0, OffsetRect.Left, OffsetRect.Top, ClientWidth + OffsetRect.Right, ClientHeight + OffsetRect.Bottom,
                 SWP_NOZORDER or SWP_ASYNCWINDOWPOS);

    ShowWindow(fAppWndHandle, SW_SHOWNORMAL);
    FTID:= GetWindowThreadProcessID(fAppWndHandle,nil);
    TTID:= GetWindowThreadProcessID(Handle, nil);
    AttachThreadInput(FTID,TTID,true);
  end;
end;

{*------------------------------------------------------------------------------
  Resize App
-------------------------------------------------------------------------------}
procedure TCEAppEmbedPanel.ResizeApp;
begin
  if fAppWndHandle <> 0 then
  begin
    SetWindowPos(fAppWndHandle,
                 0, OffsetRect.Left, OffsetRect.Top, ClientWidth + OffsetRect.Right, ClientHeight + OffsetRect.Bottom,
                 SWP_NOZORDER or SWP_ASYNCWINDOWPOS);
  end;
end;

{*------------------------------------------------------------------------------
  Close (terminate) app
-------------------------------------------------------------------------------}
function TCEAppEmbedPanel.Close: Boolean;
begin
  Result:= false;
  if IsAppRunning then
  begin
    KillProcess(fAppWndHandle);
    fAppWndHandle:= 0;
    Result:= true;
  end
  else
  fAppWndHandle:= 0;
end;

{*------------------------------------------------------------------------------
  Check if app is running
-------------------------------------------------------------------------------}
function TCEAppEmbedPanel.IsAppRunning: Boolean;
begin
  if fAppWndHandle <> 0 then
  Result:= IsWindow(fAppWndHandle)
  else
  Result:= false;
end;

{*------------------------------------------------------------------------------
  Run
-------------------------------------------------------------------------------}
function TCEAppEmbedPanel.Run: Boolean;
begin
  Result:= false;
  if not IsAppRunning then
  RunApp;
end;

{*------------------------------------------------------------------------------
  Get App window text
-------------------------------------------------------------------------------}
function TCEAppEmbedPanel.GetAppWndText: WideString;
var
  Buffer: array [0..255] of char;
  BufferW: array [0..255] of widechar;
begin
  if not IsAppRunning then
  begin
    Result:= '';
    Exit;
  end;
  
  if Win32PlatformIsUnicode then
  begin
    GetWindowText(fAppWndHandle, Buffer, 255);
    Result:= Buffer;
  end
  else
  begin
    GetWindowTextW(fAppWndHandle, BufferW, 255);
    Result:= BufferW;
  end;
end;

{*------------------------------------------------------------------------------
   Get's  called on Resize
-------------------------------------------------------------------------------}
procedure TCEAppEmbedPanel.Resize;
begin
  inherited;
  ResizeApp;
end;

end.
