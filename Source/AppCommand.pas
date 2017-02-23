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
//  The Original Code is AppCommand.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************
unit AppCommand;

{$RANGECHECKS OFF}

interface

uses
  Windows, Math, SysUtils, Classes;

type
  TWMAppCommand = packed record
    Msg: Cardinal;
    Wnd: HWND;
    Flags: Word;
    Cmd  : Byte;
    Device: Byte;
    Result: Longint;
  end;

const
  WM_APPCOMMAND = $0319;

const
  // Windows 2000, ME, and above
  APPCOMMAND_BROWSER_BACKWARD                   = 1;
  APPCOMMAND_BROWSER_FORWARD                    = 2;
  APPCOMMAND_BROWSER_REFRESH                    = 3;
  APPCOMMAND_BROWSER_STOP                       = 4;
  APPCOMMAND_BROWSER_SEARCH                     = 5;
  APPCOMMAND_BROWSER_FAVORITES                  = 6;
  APPCOMMAND_BROWSER_HOME                       = 7;
  APPCOMMAND_VOLUME_MUTE                        = 8;
  APPCOMMAND_VOLUME_DOWN                        = 9;
  APPCOMMAND_VOLUME_UP                          = 10;
  APPCOMMAND_MEDIA_NEXTTRACK                    = 11;
  APPCOMMAND_MEDIA_PREVIOUSTRACK                = 12;
  APPCOMMAND_MEDIA_STOP                         = 13;
  APPCOMMAND_MEDIA_PLAY_PAUSE                   = 14;
  APPCOMMAND_LAUNCH_MAIL                        = 15;
  APPCOMMAND_LAUNCH_MEDIA_SELECT                = 16;
  APPCOMMAND_LAUNCH_APP1                        = 17;
  APPCOMMAND_LAUNCH_APP2                        = 18;
  APPCOMMAND_BASS_DOWN                          = 19;
  APPCOMMAND_BASS_BOOST                         = 20;
  APPCOMMAND_BASS_UP                            = 21;
  APPCOMMAND_TREBLE_DOWN                        = 22;
  APPCOMMAND_TREBLE_UP                          = 23;
  // Windows XP and above
  APPCOMMAND_MICROPHONE_VOLUME_MUTE             = 24;
  APPCOMMAND_MICROPHONE_VOLUME_DOWN             = 25;
  APPCOMMAND_MICROPHONE_VOLUME_UP               = 26;
  APPCOMMAND_HELP                               = 27;
  APPCOMMAND_FIND                               = 28;
  APPCOMMAND_NEW                                = 29;
  APPCOMMAND_OPEN                               = 30;
  APPCOMMAND_CLOSE                              = 31;
  APPCOMMAND_SAVE                               = 32;
  APPCOMMAND_PRINT                              = 33;
  APPCOMMAND_UNDO                               = 34;
  APPCOMMAND_REDO                               = 35;
  APPCOMMAND_COPY                               = 36;
  APPCOMMAND_CUT                                = 37;
  APPCOMMAND_PASTE                              = 38;
  APPCOMMAND_REPLY_TO_MAIL                      = 39;
  APPCOMMAND_FORWARD_MAIL                       = 40;
  APPCOMMAND_SEND_MAIL                          = 41;
  APPCOMMAND_SPELL_CHECK                        = 42;
  APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE  = 43;
  APPCOMMAND_MIC_ON_OFF_TOGGLE                  = 44;
  APPCOMMAND_CORRECTION_LIST                    = 45;
  // Windows XP SP1 and above
  APPCOMMAND_MEDIA_PLAY                         = 46;
  APPCOMMAND_MEDIA_PAUSE                        = 47;
  APPCOMMAND_MEDIA_RECORD                       = 48;
  APPCOMMAND_MEDIA_FAST_FORWARD                 = 49;
  APPCOMMAND_MEDIA_REWIND                       = 50;
  APPCOMMAND_MEDIA_CHANNEL_UP                   = 51;
  APPCOMMAND_MEDIA_CHANNEL_DOWN                 = 52;

  AppCommandStrings: array [APPCOMMAND_BROWSER_BACKWARD ..
    APPCOMMAND_MEDIA_CHANNEL_DOWN] of string = (
    'APPCOMMAND_BROWSER_BACKWARD',
    'APPCOMMAND_BROWSER_FORWARD',
    'APPCOMMAND_BROWSER_REFRESH',
    'APPCOMMAND_BROWSER_STOP',
    'APPCOMMAND_BROWSER_SEARCH',
    'APPCOMMAND_BROWSER_FAVORITES',
    'APPCOMMAND_BROWSER_HOME',
    'APPCOMMAND_VOLUME_MUTE',
    'APPCOMMAND_VOLUME_DOWN',
    'APPCOMMAND_VOLUME_UP',
    'APPCOMMAND_MEDIA_NEXTTRACK',
    'APPCOMMAND_MEDIA_PREVIOUSTRACK',
    'APPCOMMAND_MEDIA_STOP',
    'APPCOMMAND_MEDIA_PLAY_PAUSE',
    'APPCOMMAND_LAUNCH_MAIL',
    'APPCOMMAND_LAUNCH_MEDIA_SELECT',
    'APPCOMMAND_LAUNCH_APP1',
    'APPCOMMAND_LAUNCH_APP2',
    'APPCOMMAND_BASS_DOWN',
    'APPCOMMAND_BASS_BOOST',
    'APPCOMMAND_BASS_UP',
    'APPCOMMAND_TREBLE_DOWN',
    'APPCOMMAND_TREBLE_UP',
    'APPCOMMAND_MICROPHONE_VOLUME_MUTE',
    'APPCOMMAND_MICROPHONE_VOLUME_DOWN',
    'APPCOMMAND_MICROPHONE_VOLUME_UP',
    'APPCOMMAND_HELP',
    'APPCOMMAND_FIND',
    'APPCOMMAND_NEW',
    'APPCOMMAND_OPEN',
    'APPCOMMAND_CLOSE',
    'APPCOMMAND_SAVE',
    'APPCOMMAND_PRINT',
    'APPCOMMAND_UNDO',
    'APPCOMMAND_REDO',
    'APPCOMMAND_COPY',
    'APPCOMMAND_CUT',
    'APPCOMMAND_PASTE',
    'APPCOMMAND_REPLY_TO_MAIL',
    'APPCOMMAND_FORWARD_MAIL',
    'APPCOMMAND_SEND_MAIL',
    'APPCOMMAND_SPELL_CHECK',
    'APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE',
    'APPCOMMAND_MIC_ON_OFF_TOGGLE',
    'APPCOMMAND_CORRECTION_LIST',
    'APPCOMMAND_MEDIA_PLAY',
    'APPCOMMAND_MEDIA_PAUSE',
    'APPCOMMAND_MEDIA_RECORD',
    'APPCOMMAND_MEDIA_FAST_FORWARD',
    'APPCOMMAND_MEDIA_REWIND',
    'APPCOMMAND_MEDIA_CHANNEL_UP',
    'APPCOMMAND_MEDIA_CHANNEL_DOWN'
  );

  FAPPCOMMAND_MOUSE = $8000;
  FAPPCOMMAND_KEY   = 0;
  FAPPCOMMAND_OEM   = $1000;
  FAPPCOMMAND_MASK  = $F000;

  function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Short;
  function GET_DEVICE_LPARAM(lParam: LPARAM): Word;
  function GET_KEYSTATE_LPARAM(lParam: LPARAM): Word;
  function DeviceIsMouse(Dev: Byte): Boolean;
  function DeviceIsKey(Dev: Byte): Boolean;
  function AppCommandToString( Cmd: Word ): String;
  function FlagsToString( Flags: Word ): string;


implementation

{-------------------------------------------------------------------------------
  GET_APPCOMMAND_LPARAM
-------------------------------------------------------------------------------}
function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Short;
begin
  Result:= HiWord(lParam) and not FAPPCOMMAND_MASK;
end;

{-------------------------------------------------------------------------------
  GET_DEVICE_LPARAM
-------------------------------------------------------------------------------}
function GET_DEVICE_LPARAM(lParam: LPARAM): Word;
begin
  Result:= HiWord(lParam) and FAPPCOMMAND_MASK;
end;

{-------------------------------------------------------------------------------
  GET_KEYSTATE_LPARAM
-------------------------------------------------------------------------------}
function GET_KEYSTATE_LPARAM(lParam: LPARAM): Word;
begin
  Result:= LoWord(lParam);
end;

{-------------------------------------------------------------------------------
  DeviceIsMouse
-------------------------------------------------------------------------------}
function DeviceIsMouse(Dev: Byte): Boolean;
begin
  Result:= (Dev and $80) <> 0;
end;

{-------------------------------------------------------------------------------
  DeviceIsKey
-------------------------------------------------------------------------------}
function DeviceIsKey(Dev: Byte): Boolean;
begin
  Result:= (Dev and $80) = 0;
end;

{-------------------------------------------------------------------------------
  AppCommandToString
-------------------------------------------------------------------------------}
function AppCommandToString( Cmd: Word ): String;
begin
  if Math.InRange(Cmd, Low(AppCommandStrings), High(AppCommandStrings)) then
  Result:= AppCommandStrings[Cmd]
  else
  Result:= Format('Unknown appcommand: %u',[Cmd]);
end;

{-------------------------------------------------------------------------------
  FlagsToString
-------------------------------------------------------------------------------}
function FlagsToString( Flags: Word ): string;
type
  TFlag = record
            Value: Word;
            Name: string;
          end;
const
  MK_XBUTTON1 = $0020;
  MK_XBUTTON2 = $0040;
  AFlags : array [0..6] of TFlag = (
    ( value: MK_CONTROL; Name: 'CONTROL'),
    ( value: MK_LBUTTON; Name: 'LBUTTON'),
    ( value: MK_MBUTTON; Name: 'MBUTTON'),
    ( value: MK_RBUTTON; Name: 'RBUTTON'),
    ( value: MK_SHIFT; Name: 'SHIFT'),
    ( value: MK_XBUTTON1; Name: 'XBUTTON1'),
    ( value: MK_XBUTTON2; Name: 'XBUTTON2'));
var
  Sl: TStringList;
  I: Integer;
begin
  Sl:= TStringList.Create();
  try
    for I:= Low(AFlags) to High(AFLags) do
    begin
      if (Flags and AFLags[I].Value) = AFlags[I].Value then
      Sl.Add(AFlags[I].Name);
    end;
    Result:= Format('[%s]', [sl.Commatext]);
  finally
    Sl.Free;
  end;
end;

end.