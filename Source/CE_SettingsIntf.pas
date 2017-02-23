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
//  The Original Code is CE_SettingsIntf.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_SettingsIntf;

interface

uses
  Windows;

type
  ICESettingsStorage = interface(IInterface)
  ['{310E498B-2643-451C-8DB1-364F8D53405C}']
    procedure ClosePath; stdcall;
    procedure DeletePath(Path: WideString); stdcall;
    procedure OpenPath(Path: WideString); stdcall;
    function ReadBoolean(Path: WideString; Default: Boolean): Boolean; stdcall;
    function ReadInteger(Path: WideString; Default: Integer): Integer; stdcall;
    function ReadPoint(Path: WideString; Default: TPoint): TPoint; stdcall;
    function ReadString(Path: WideString; Default: WideString): WideString; stdcall;
    procedure WriteBoolean(Path: WideString; Value: Boolean); stdcall;
    procedure WriteInteger(Path: WideString; Value: Integer); stdcall;
    procedure WritePoint(Path: WideString; Value: TPoint); stdcall;
    procedure WriteString(Path: WideString; Value: WideString); stdcall;
  end;
  
  ICESettingsHandler = interface(IInterface)
  ['{4EC9F1B1-3E44-40A9-8810-76500D48DE83}']
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  end;

implementation

end.
