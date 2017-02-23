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
//  The Original Code is CE_SettingsStorage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_SettingsStorage;

interface

uses
  CE_XmlStorage, CE_SettingsIntf,
  // System Units
  Windows, SysUtils, Messages, Classes;

type
  TCESettingsStorage = class(TInterfacedObject, ICESettingsStorage)
  private
    fAppStorage: TCEXmlStorage;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClosePath; stdcall;
    procedure DeletePath(Path: WideString); stdcall;
    procedure LoadFromFile(AFilePath: WideString);
    procedure OpenPath(Path: WideString); stdcall;
    function ReadBoolean(Path: WideString; Default: Boolean): Boolean; stdcall;
    function ReadInteger(Path: WideString; Default: Integer): Integer; stdcall;
    function ReadString(Path: WideString; Default: WideString): WideString; stdcall;
    function ReadPoint(Path: WideString; Default: TPoint): TPoint; stdcall;
    procedure SaveToFile(AFilePath: WideString);
    procedure WriteBoolean(Path: WideString; Value: Boolean); stdcall;
    procedure WriteInteger(Path: WideString; Value: Integer); stdcall;
    procedure WriteString(Path: WideString; Value: WideString); stdcall;
    procedure WritePoint(Path: WideString; Value: TPoint); stdcall;
    property AppStorage: TCEXmlStorage read fAppStorage;
  end;

  //TCESettingList = class

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCESettingsStorage
-------------------------------------------------------------------------------}
constructor TCESettingsStorage.Create;
begin
  inherited;
  fAppStorage:= TCEXmlStorage.Create;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCESettingsStorage
-------------------------------------------------------------------------------}
destructor TCESettingsStorage.Destroy;
begin
  fAppStorage.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Close Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.ClosePath;
begin
  fAppStorage.ActivePath:= '/';
end;

{-------------------------------------------------------------------------------
  Delete Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.DeletePath(Path: WideString);
begin
  fAppStorage.DeletePath(Path);
end;

{-------------------------------------------------------------------------------
  Load From File
--------------------------------------------------------------------------------}
procedure TCESettingsStorage.LoadFromFile(AFilePath: WideString);
begin
  fAppStorage.LoadFromFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Open Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.OpenPath(Path: WideString);
begin
  fAppStorage.ActivePath:= Path;
end;

{*------------------------------------------------------------------------------
  ReadBoolean
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadBoolean(Path: WideString; Default: Boolean):
    Boolean;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadInteger
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadInteger(Path: WideString; Default: Integer):
    Integer;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadString
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadString(Path: WideString; Default: WideString):
    WideString;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadPoint
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadPoint(Path: WideString; Default: TPoint):
    TPoint;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{-------------------------------------------------------------------------------
  Save to File
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.SaveToFile(AFilePath: WideString);
begin
  fAppStorage.SaveToFile(AFilePath);
end;

{*------------------------------------------------------------------------------
  WriteBoolean
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteBoolean(Path: WideString; Value: Boolean);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WriteInteger
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteInteger(Path: WideString; Value: Integer);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WriteString
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteString(Path: WideString; Value: WideString);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WritePoint
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WritePoint(Path: WideString; Value: TPoint);
begin
  fAppStorage.SetValue(Path, Value);
end;

{##############################################################################}


end.
