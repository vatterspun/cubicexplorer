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
//  The Original Code is CE_ProcessUtils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ProcessUtils;

interface

uses
  // TNT Controls
  TntWindows, TntSysUtils,
  // System Units
  Windows;

procedure RunProcess(Path: WideString; Params: WideString = ''; Hidden: Boolean
    = false; WaitUntilDone: Boolean = true);

implementation

procedure RunProcess(Path: WideString; Params: WideString = ''; Hidden: Boolean
    = false; WaitUntilDone: Boolean = true);
var
  cmd: WideString;
  currDir: WideString;
  flags: DWORD;
  startupInfo: TStartupInfoW;
  procInfo   : TProcessInformation;
  createOK   : Boolean;
begin
  cmd:= Path;
  if Params <> '' then
  cmd:= cmd + ' ' + Params;
  currDir:= WideExtractFilePath(Path);
  // Flags
  flags:= NORMAL_PRIORITY_CLASS;
  // Startup info
  FillChar(startupInfo, SizeOf(TStartupInfo), #0);
  startupInfo.cb:= SizeOf(TStartupInfo);
  if Hidden then
  begin
    startupInfo.wShowWindow:= SW_HIDE;
    Inc(startupInfo.dwFlags, STARTF_USESHOWWINDOW);
  end;
  FillChar(procInfo,SizeOf(TProcessInformation),#0);
  // Create process
  createOK:= Tnt_CreateProcessW(nil,                 // Application
                                PWideChar(cmd),      // Command line
                                nil,                 // lpProcessAttributes
                                nil,                 // lpThreadAttributes
                                false,               // bInheritHandles
                                flags,               // dwCreationFlags
                                nil,                 // lpEnvironment
                                PWideChar(currDir),  // lpCurrentDirectory
                                startupInfo,         // lpStartupInfo
                                procInfo);           // lpProcessInformation 

  if CreateOK and WaitUntilDone then
  WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

end.
