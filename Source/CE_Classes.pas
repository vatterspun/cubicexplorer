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
//  The Original Code is CE_Classes.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Classes;

interface

uses
  Windows, Messages, Classes;

type
  TCEWndObject = class(TObject)
  protected
    fHandle: HWND;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AllocHWnd(Method: TWndMethod): HWND; virtual;
    procedure DeallocHWnd(Wnd: HWND); virtual;
    procedure WindowProc(var Msg : TMessage); virtual;
    property Handle: HWND read fHandle;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEWndObject
-------------------------------------------------------------------------------}
constructor TCEWndObject.Create;
begin
  inherited;
  fHandle:= AllocHWnd(WindowProc);
end;

{*------------------------------------------------------------------------------
  Destroy TCEWndObject
-------------------------------------------------------------------------------}
destructor TCEWndObject.Destroy;
begin
  DeallocHWnd(fHandle);
  inherited;
end;

{*------------------------------------------------------------------------------
  Allocate Window Handle
-------------------------------------------------------------------------------}
function TCEWndObject.AllocHWnd(Method: TWndMethod): HWND;
begin
  Result:= AllocateHWnd(WindowProc);
end;

{*------------------------------------------------------------------------------
  Deallocate Window Handle
-------------------------------------------------------------------------------}
procedure TCEWndObject.DeallocHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  if Instance <> @DefWindowProc then
  begin
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;
  DestroyWindow(Wnd);
end;

{*------------------------------------------------------------------------------
  Handle messages
-------------------------------------------------------------------------------}
procedure TCEWndObject.WindowProc(var Msg : TMessage);
begin
  Msg.Result:= DefWindowProc(fHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
