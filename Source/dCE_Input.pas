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
//  The Original Code is dCE_Input.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit dCE_Input;

interface

uses
  // CE Units

  // System Units
  Windows, SysUtils, Messages, Classes;

type
  TCEMsgInput = class(TObject)
  public
    fHandle: HWND;
    constructor Create;
    destructor Destroy; override;
    function AllocHWnd(Method: TWndMethod): HWND;
    procedure DeallocHWnd(Wnd: HWND);
    procedure MsgProc(var Msg : TMessage);
    property Handle: HWND read fHandle;
  end;

var
  CEMsgInputWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'CubicExplorer_MsgInput');

type
  TCEInput = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    MsgInput: TCEMsgInput;
  end;

var
  CEInput: TCEInput;

implementation

uses
  dCE_Actions;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEMsgInput
-------------------------------------------------------------------------------}
constructor TCEMsgInput.Create;
begin
  inherited;
  fHandle:= AllocHWnd(MsgProc);
end;

{*------------------------------------------------------------------------------
  Destroy TCEMsgInput
-------------------------------------------------------------------------------}
destructor TCEMsgInput.Destroy;
begin
  DeallocHWnd(fHandle);
  inherited;
end;

{*------------------------------------------------------------------------------
  Allocate HWnd
-------------------------------------------------------------------------------}
function TCEMsgInput.AllocHWnd(Method: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin

  ClassRegistered := GetClassInfo(HInstance, CEMsgInputWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(CEMsgInputWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(CEMsgInputWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, CEMsgInputWindowClass.lpszClassName,
    'CE_MsgInput', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(Method) then
    SetWindowLong(Result, GWL_WNDPROC, Longint(MakeObjectInstance(Method)));
end;

{*------------------------------------------------------------------------------
  Deallocate HWnd
-------------------------------------------------------------------------------}
procedure TCEMsgInput.DeallocHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  if Instance <> @DefWindowProc then
  begin
    { make sure we restore the default 
      windows procedure before freeing memory } 
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;
  DestroyWindow(Wnd);
end;

{*------------------------------------------------------------------------------
  Handle messages
-------------------------------------------------------------------------------}
procedure TCEMsgInput.MsgProc(var Msg : TMessage);
var
  handled: Boolean;
begin
  handled:= false;
  HandleInputMessage(Msg, handled);
  if not handled then
  Msg.Result:= DefWindowProc(fHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  DataModuleCreate
-------------------------------------------------------------------------------}
procedure TCEInput.DataModuleCreate(Sender: TObject);
begin
  MsgInput:= TCEMsgInput.Create;
end;

{*------------------------------------------------------------------------------
  DataModuleDestroy
-------------------------------------------------------------------------------}
procedure TCEInput.DataModuleDestroy(Sender: TObject);
begin
  MsgInput.Free;
end;

end.
