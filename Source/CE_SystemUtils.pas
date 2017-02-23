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
//  The Original Code is CE_SystemUtils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_SystemUtils;

interface

uses
  Windows, SysUtils;

function WindowsExit(RebootParam: Longword): Boolean;
function SetSuspendState(hibernate, forcecritical, disablewakeevent: boolean): boolean; stdcall; external 'powrprof.dll' name 'SetSuspendState';
function IsHibernateAllowed: boolean; stdcall; external 'powrprof.dll' name 'IsPwrHibernateAllowed';
function IsPwrSuspendAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrSuspendAllowed';
function IsPwrShutdownAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrShutdownAllowed';
function LockWorkStation: boolean; stdcall; external 'user32.dll' name 'LockWorkStation';

implementation

{-------------------------------------------------------------------------------
  WindowsExit
-------------------------------------------------------------------------------}
function WindowsExit(RebootParam: Longword): Boolean;
var
  TTokenHd: THandle;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
const
   SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
   if Win32Platform = VER_PLATFORM_WIN32_NT then
   begin
     tpResult:= OpenProcessToken(GetCurrentProcess(),
                                 TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                                 TTokenHd);
     if tpResult then
     begin
       tpResult:= LookupPrivilegeValue(nil,
                                       SE_SHUTDOWN_NAME,
                                       TTokenPvg.Privileges[0].Luid);
       TTokenPvg.PrivilegeCount:= 1;
       TTokenPvg.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED;
       cbtpPrevious:= SizeOf(rTTokenPvg);
       pcbtpPreviousRequired:= 0;
       if tpResult then
       begin
         Windows.AdjustTokenPrivileges(TTokenHd,
                                       False,
                                       TTokenPvg,
                                       cbtpPrevious,
                                       rTTokenPvg,
                                       pcbtpPreviousRequired);
       end;
     end;
   end;
   Result:= ExitWindowsEx(RebootParam, 0);
end;

end.
