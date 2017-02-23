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
//  The Original Code is CE_VistaFuncs.pas.
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_VistaFuncs;

interface

uses Forms, Windows, Graphics;

const
  VistaFont = 'Segoe UI'; 
  VistaContentFont = 'Calibri';
  XPContentFont = 'Verdana';
  XPFont = 'Tahoma';     

  TD_ICON_BLANK = 0;
  TD_ICON_WARNING = 84;
  TD_ICON_QUESTION = 99;
  TD_ICON_ERROR = 98;
  TD_ICON_INFORMATION = 81;
  TD_ICON_SHIELD_QUESTION = 104;
  TD_ICON_SHIELD_ERROR = 105;
  TD_ICON_SHIELD_OK = 106;        
  TD_ICON_SHIELD_WARNING = 107;                     

  TD_BUTTON_OK = 1;
  TD_BUTTON_YES = 2;
  TD_BUTTON_NO = 4;
  TD_BUTTON_CANCEL = 8;
  TD_BUTTON_RETRY = 16;
  TD_BUTTON_CLOSE = 32;

  TD_RESULT_OK = 1;
  TD_RESULT_CANCEL = 2;
  TD_RESULT_RETRY = 4;
  TD_RESULT_YES = 6;
  TD_RESULT_NO = 7;
  TD_RESULT_CLOSE = 8;

  TaskDialogSig = 'TaskDialog';

  function IsWindowsVista: Boolean;
  procedure SetVistaFonts(const AForm: TCustomForm);
  procedure SetVistaContentFonts(const AFont: TFont);
  procedure SetDesktopIconFonts(const AFont: TFont);
  function TaskDialog(const AHandle: THandle; const ATitle, ADescription,
      AContent: WideString; const Icon, Buttons: integer): Integer;
  procedure SetVistaTreeView(const AHandle: THandle);
  procedure SetVistaFont(const AFont: TFont);

var
  CheckOSVerForFonts: Boolean = True;

implementation

uses
  SysUtils, Dialogs, Controls, UxTheme, MPCommonUtilities;

{*------------------------------------------------------------------------------
  SetVistaTreeView
-------------------------------------------------------------------------------}
procedure SetVistaTreeView(const AHandle: THandle);
begin
  if IsWindowsVista then
    SetWindowTheme(AHandle, 'explorer', nil);
end;

{*------------------------------------------------------------------------------
  SetVistaFont
-------------------------------------------------------------------------------}
procedure SetVistaFont(const AFont: TFont);
begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AFont.Name, VistaFont)
    and (Screen.Fonts.IndexOf(VistaFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 1;
    AFont.Name := VistaFont;
  end;
end;

{*------------------------------------------------------------------------------
  SetVistaFonts
-------------------------------------------------------------------------------}
procedure SetVistaFonts(const AForm: TCustomForm);
begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AForm.Font.Name, VistaFont)
    and (Screen.Fonts.IndexOf(VistaFont) >= 0) then
  begin
    AForm.Font.Size := AForm.Font.Size + 1;
    AForm.Font.Name := VistaFont;
  end;
end;

{*------------------------------------------------------------------------------
  SetVistaContentFonts
-------------------------------------------------------------------------------}
procedure SetVistaContentFonts(const AFont: TFont);
begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 2;
    AFont.Name := VistaContentFont;
  end;
end;

{*------------------------------------------------------------------------------
  SetDefaultFonts
-------------------------------------------------------------------------------}
procedure SetDefaultFonts(const AFont: TFont);
begin
  AFont.Handle:= GetStockObject(DEFAULT_GUI_FONT);
end;

{*------------------------------------------------------------------------------
  SetDesktopIconFonts
-------------------------------------------------------------------------------}
procedure SetDesktopIconFonts(const AFont: TFont);
var
  LogFont: TLogFont;
begin
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
    AFont.Handle:= CreateFontIndirect(LogFont)
  else
    SetDefaultFonts(AFont);
end;

{*------------------------------------------------------------------------------
  IsWindowsVista
-------------------------------------------------------------------------------}
function IsWindowsVista: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);        
  Result := VerInfo.dwMajorVersion >= 6;
end;  

{*------------------------------------------------------------------------------
  TaskDialog
-------------------------------------------------------------------------------}
function TaskDialog(const AHandle: THandle; const ATitle, ADescription,
    AContent: WideString; const Icon, Buttons: integer): Integer;
var
  DLLHandle: THandle;
  res: integer;
  Btns: Integer;
  DlgIcon: Integer;
  TaskDialogProc: function(HWND: THandle; hInstance: THandle; cTitle,
    cDescription, cContent: pwidechar; Buttons: Integer; Icon: integer;
    ResButton: pinteger): integer; cdecl stdcall;
begin                          
  Result:= 0;
  if IsWindowsVista then
  begin
    DLLHandle:= LoadLibrary(comctl32);
    if DLLHandle >= 32 then
    begin
      @TaskDialogProc:= GetProcAddress(DLLHandle, TaskDialogSig);

      if Assigned(TaskDialogProc) then
      begin
        TaskDialogProc(AHandle, 0, PWideChar(ATitle), PWideChar(ADescription), PWideChar(AContent), Buttons,
          Icon, @res);

        Result:= mrOK;

        case res of
          TD_RESULT_CANCEL : Result := mrCancel;
          TD_RESULT_RETRY : Result := mrRetry;
          TD_RESULT_YES : Result := mrYes;
          TD_RESULT_NO : Result := mrNo;
          TD_RESULT_CLOSE : Result := mrAbort;
        end;
      end;
      FreeLibrary(DLLHandle);
    end;
  end
  else
  begin
    if Buttons = TD_BUTTON_OK then
    Btns:= MB_OK
    else if Buttons = TD_BUTTON_OK+TD_BUTTON_CANCEL then
    Btns:= MB_OKCANCEL
    else if Buttons = TD_BUTTON_RETRY+TD_BUTTON_CLOSE then
    Btns:= MB_ABORTRETRYIGNORE
    else if Buttons = TD_BUTTON_YES+TD_BUTTON_NO+TD_BUTTON_CANCEL then
    Btns:= MB_YESNOCANCEL
    else if Buttons = TD_BUTTON_YES+TD_BUTTON_NO then
    Btns:= MB_YESNO
    else if Buttons = TD_BUTTON_RETRY+TD_BUTTON_CANCEL then
    Btns:= MB_RETRYCANCEL
    else
    Exit;

    DlgIcon:= 0;
    case Icon of
      TD_ICON_WARNING: DlgIcon:= MB_ICONWARNING;
      TD_ICON_QUESTION: DlgIcon:= MB_ICONQUESTION;
      TD_ICON_ERROR: DlgIcon:= MB_ICONERROR;
      TD_ICON_INFORMATION: DlgIcon:= MB_ICONINFORMATION;
    end;

    if AContent = '' then
    Result:= WideMessageBox(AHandle, PWideChar(ATitle), PWideChar(ADescription), DlgIcon or Btns)
    else
    Result:= WideMessageBox(AHandle, PWideChar(ATitle), PWideChar(AContent), DlgIcon or Btns);
  end;
end;

end.
