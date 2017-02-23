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
//  The Original Code is CE_ElevatedActions.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ElevatedActions;

interface

uses
  // CE Units
  CE_FileUtils,
  // Tnt
  TntSysUtils, TntWindows, TntSystem,
  // System Units
  Windows, SysUtils, ShellAPI, Forms;

function HandleElevatedCommands: Boolean;

function Elevated_CreateJunction(ALink: WideString; ATarget: WideString;
    ParentHWND: HWND): Boolean;

function RunElevatedCommand(AParentHWND: HWND; const AParams: WideString):
    Boolean;

function Elevated_RegisterDefaultFileManager(AParentHWND: HWND): Boolean;

function Elevated_UnRegisterDefaultFileManager(AParentHWND: HWND): Boolean;

implementation

uses
  CE_VersionUpdater, dCE_Input, dCE_Actions, Main, CE_Utils;

{-------------------------------------------------------------------------------
  Handle Elevated Commands
-------------------------------------------------------------------------------}
function HandleElevatedCommands: Boolean;
var
  index, count: Integer;
  doAdmin: Boolean;
  doUpdate: Boolean;
  action: String;
  param1, param2: WideString;
  paramI: Integer;
  b: Boolean;
begin
  Result:= false;
  doAdmin:= false;
  doUpdate:= false;
  index:= 1;
  count:= ParamCount;
  // Loop through cmd params.
  while index <= count do
  begin
    // Do Admin actions
    if doAdmin then
    begin
      action:= ParamStr(index);

      // Create Junction
      if action = 'create_symlink' then
      begin
        if index + 2 <= count then // make sure we have all needed params present
        begin
          param1:= WideParamStr(index + 1);
          param2:= WideParamStr(index + 2);
          index:= index + 2;

          CreateJunction(param1, param2);
        end;
      end
      // Register
      else if action = 'register' then
      begin
        b:= RegisterDefaultFileManager('cubicexplorer', 'Open in CubicExplorer', WideParamStr(0));
        if index + 1 <= count then
        begin
          param1:= WideParamStr(index + 1);
          paramI:= StrToIntDef(param1, 0);
          if paramI > 0 then
          begin
            if b then
            PostMessage(paramI, WM_AdminResult, 100, 0)
            else
            PostMessage(paramI, WM_AdminResult, 100, -1);
            index:= index + 1;
          end;
        end;
      end
      else if action = 'unregister' then
      begin
        b:= UnRegisterDefaultFileManager('cubicexplorer');
        if index + 1 <= count then
        begin
          param1:= WideParamStr(index + 1);
          paramI:= StrToIntDef(param1, 0);
          if paramI > 0 then
          begin
            if b then
            PostMessage(paramI, WM_AdminResult, 101, 0)
            else
            PostMessage(paramI, WM_AdminResult, 101, -1);
            index:= index + 1;
          end;
        end;
      end;

      index:= index + 1;

      doAdmin:= false;
    end
    // Do Update
    else if doUpdate then
    begin
      if index + 1 < count then
      begin
        param1:= WideParamStr(index); // zip path
        param2:= WideParamStr(index + 1); // dest folder
        index:= index + 2;
        if index <= count then
        begin
          paramI:= StrToIntDef(WideParamStr(index), 0); // Handle to old app
          index:= index + 1;
        end
        else
        paramI:= 0;

        UpdateCEFromZip(param1, param2, paramI, false);
        doUpdate:= false;
      end
      else
      index:= index + 1;
    end
    else
    begin
      doAdmin:= (ParamStr(index) = '/admin') and (index < ParamCount); // Check if /admin switch is present
      if doAdmin then
      Result:= true
      else
      begin
        doUpdate:= (ParamStr(index) = '/update') and (index < count);
        if doUpdate then
        Result:= true;
      end;
      index:= index + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Run Elevated Command
-------------------------------------------------------------------------------}
function RunElevatedCommand(AParentHWND: HWND; const AParams: WideString): Boolean;
var
  op: WideString;
begin
  Result:= true; // TODO: add proper result value

  if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6)) or not IsWindowsAdmin then
  op:= 'runas'
  else
  op:= 'open';

  Tnt_ShellExecuteW(AParentHWND,
                    PWideChar(op),
                    PWideChar(WideParamStr(0)),
                    PWideChar(AParams),
                    '',
                    SW_HIDE);
end;

{-------------------------------------------------------------------------------
  CreateJunction (Elevated)
-------------------------------------------------------------------------------}
function Elevated_CreateJunction(ALink: WideString; ATarget: WideString;
    ParentHWND: HWND): Boolean;
var
  ws: WideString;
begin
  ws:= '/admin create_symlink "' + ALink + '" "' + ATarget + '"';
  Result:= RunElevatedCommand(ParentHWND, ws);
end;

{-------------------------------------------------------------------------------
  Register Default File Manager (Elevated)
-------------------------------------------------------------------------------}
function Elevated_RegisterDefaultFileManager(AParentHWND: HWND): Boolean;
begin
  if (Win32MajorVersion > 5) or not IsWindowsAdmin then
  Result:= RunElevatedCommand(AParentHWND, '/admin register ' + IntToStr(CEInput.MsgInput.Handle))
  else
  begin
    Result:= RegisterDefaultFileManager('cubicexplorer', 'Open in CubicExplorer', WideParamStr(0));
    if Result then
    PostMessage(CEInput.MsgInput.Handle, WM_AdminResult, 100, 0)
    else
    PostMessage(CEInput.MsgInput.Handle, WM_AdminResult, 100, -1);
  end;
end;

{-------------------------------------------------------------------------------
  UnRegister Default File Manager (Elevated)
-------------------------------------------------------------------------------}
function Elevated_UnRegisterDefaultFileManager(AParentHWND: HWND): Boolean;
begin
  if (Win32MajorVersion > 5) or not IsWindowsAdmin then
  Result:= RunElevatedCommand(AParentHWND, '/admin unregister ' + IntToStr(CEInput.MsgInput.Handle))
  else
  begin
    Result:= UnRegisterDefaultFileManager('cubicexplorer');
    if Result then
    PostMessage(CEInput.MsgInput.Handle, WM_AdminResult, 101, 0)
    else
    PostMessage(CEInput.MsgInput.Handle, WM_AdminResult, 101, -1);
  end;
end;

end.
