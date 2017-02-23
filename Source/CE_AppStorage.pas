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
//  The Original Code is CE_AppStorage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_AppStorage;

interface

uses
  // JVCL
  JvAppXMLStorage, JvAppStorage,
  // System Units
  Windows, SysUtils, Messages, Classes;

type
  TCEAppStorage = class(TJvAppXMLFileStorage)
  protected
    function GetPhysicalReadOnly: Boolean; override;
  end;

implementation

{*------------------------------------------------------------------------------
  Fix bug in Windows 98.
-------------------------------------------------------------------------------}
function TCEAppStorage.GetPhysicalReadOnly: Boolean;
begin
  if Self.FileName = '' then
  Result:= false
  else
  Result:= FPhysicalReadOnly;
end;

end.
