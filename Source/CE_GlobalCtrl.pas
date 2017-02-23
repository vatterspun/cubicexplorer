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
//  The Original Code is CE_GlobalCtrl.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_GlobalCtrl;

interface

uses
  // CE Units
  CE_FileView,
  // VSTools
  VirtualExplorerTree, MPCommonUtilities, VirtualShellNewMenu,
  // SpTBXLib
  SpTBXItem, SpTBXControls,  SpTBXDkPanels,
  // Tnt
  TntForms,
  // System Units
  SysUtils, ShlObj, Classes, Windows, Contnrs, Controls, Messages, Forms;

type


  ICEPathChangeHandler = interface(IInterface)
  ['{47F442E5-A7FA-466C-8FEB-F5BCB10DFF03}']
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); stdcall;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); stdcall;
    procedure GlobalContentChange(Sender: TObject); stdcall;
  end;

  TCEPathCtrl = class(TObject)
  private
    fActivePage: TComponent;
    fChangeCurrentDirVar: Boolean;
    fCurrentPath: WideString;
    fFocusChanging: Boolean;
    fGlobalPathCaption: WideString;
    fPathChanging: Boolean;
    procedure SetActivePage(const Value: TComponent);
    procedure SetGlobalPathCaption(const Value: WideString);
  protected
    fGlobalPathCtrls: TComponentList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ChangeFocusedPath(Sender: TObject; NewPath: WideString);
    procedure ChangeGlobalPath(Sender: TObject; NewPath: WideString);
    procedure ChangeGlobalPathPIDL(Sender: TObject; APIDL: PItemIDList);
    procedure RegisterNotify(ReceiverCtrl: TComponent);
    procedure ChangeGlobalContent(Sender: TObject);
    property ActivePage: TComponent read fActivePage write SetActivePage;
    property ChangeCurrentDirVar: Boolean read fChangeCurrentDirVar write
        fChangeCurrentDirVar;
    property CurrentPath: WideString read fCurrentPath;
    property GlobalPathCaption: WideString read fGlobalPathCaption write
        SetGlobalPathCaption;
  published
  end;

  TCEMouseCtrl = class(TObject)
  private
    fTextEditorBugFix: Boolean;
  public
    CtrlList: TComponentList;
    constructor Create;
    destructor Destroy; override;
    procedure DoMouseWheel(Sender: TObject; Shift: TShiftState;
              WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  end;

var
  GlobalPathCtrl: TCEPathCtrl;
  GlobalFocusCtrl: TCEMouseCtrl;

implementation

uses
  Main, dCE_Actions, fCE_FileView, fCE_TextEditor, CE_FileUtils, TntSysUtils;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEPathCtrl
-------------------------------------------------------------------------------}
constructor TCEPathCtrl.Create;
begin
  inherited;
  fGlobalPathCtrls:= TComponentList.Create(false);
  fChangeCurrentDirVar:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEPathCtrl
-------------------------------------------------------------------------------}
destructor TCEPathCtrl.Destroy;
begin
  fGlobalPathCtrls.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Changes global path.

  @Param Sender A Component that calls this function. It's path will not be changed here.
  @Param NewPath A new directory path.
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.ChangeGlobalPath(Sender: TObject; NewPath: WideString);
var
  i: Integer;
  PathHandler: ICEPathChangeHandler;
begin
  if fPathChanging then
  Exit;

  fPathChanging:= true;

  // GlobalPath Controls
  for i:= 0 to fGlobalPathCtrls.Count - 1 do
  begin
    if Supports(fGlobalPathCtrls.Items[i], ICEPathChangeHandler, PathHandler) then
    PathHandler.GlobalPathChanged(Sender, NewPath);
  end; 
  // Active Component
  if Assigned(fActivePage) and (Sender <> fActivePage) then
  begin
    if Supports(fActivePage, ICEPathChangeHandler, PathHandler) then
    PathHandler.GlobalPathChanged(Sender, NewPath);
  end;

  fCurrentPath:= NewPath;
  WideSetCurrentDir(fCurrentPath);
  fPathChanging:= false;
end;

{*------------------------------------------------------------------------------
  Changes global path.

  @Param Sender A Component that calls this function. It's path will not be changed here.
  @Param APIDL A PIDL of the new directory.
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.ChangeGlobalPathPIDL(Sender: TObject; APIDL: PItemIDList);
var
  i: Integer;
  PathHandler: ICEPathChangeHandler;
begin
  if fPathChanging then
  Exit;

  if APIDL = nil then
  Exit;

  fPathChanging:= true;

  // GlobalPath Controls
  for i:= 0 to fGlobalPathCtrls.Count - 1 do
  begin
    if fGlobalPathCtrls.Items[i] <> Sender then
    begin
      if Supports(fGlobalPathCtrls.Items[i], ICEPathChangeHandler, PathHandler) then
      PathHandler.GlobalPIDLChanged(Sender, APIDL);
    end;
  end;
  // Active Component
  if Assigned(fActivePage) and (Sender <> fActivePage) then
  begin
   if Supports(fActivePage, ICEPathChangeHandler, PathHandler) then
    PathHandler.GlobalPIDLChanged(Sender, APIDL);
  end;

  fCurrentPath:= PIDLToCEPath(APIDL);
  if ChangeCurrentDirVar then
  WideSetCurrentDir(fCurrentPath);
  fPathChanging:= false;
end;

{*------------------------------------------------------------------------------
  Change focused path
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.ChangeFocusedPath(Sender: TObject; NewPath: WideString);
var
  PathHandler: ICEPathChangeHandler;
  i: Integer;
begin
  if fFocusChanging then
  Exit;

  fFocusChanging:= true;

  // GlobalPath Controls
  for i:= 0 to fGlobalPathCtrls.Count - 1 do
  begin
    if Supports(fGlobalPathCtrls.Items[i], ICEPathChangeHandler, PathHandler) then
    PathHandler.GlobalFocusChanged(Sender, NewPath);
  end; 
  // Active Component
  if Assigned(fActivePage) and (Sender <> fActivePage) then
  begin
   if Supports(fActivePage, ICEPathChangeHandler, PathHandler) then
    PathHandler.GlobalFocusChanged(Sender, NewPath);;
  end;

  fFocusChanging:= false;
end;

{*------------------------------------------------------------------------------
  Register Component to be notified on Global path change
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.RegisterNotify(ReceiverCtrl: TComponent);
begin
  fGlobalPathCtrls.Add(ReceiverCtrl);
end;

{*------------------------------------------------------------------------------
  Send Global Content changed event.
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.ChangeGlobalContent(Sender: TObject);
var
  i: Integer;
  PathHandler: ICEPathChangeHandler;
begin
  for i:= 0 to fGlobalPathCtrls.Count - 1 do
  begin
    if Supports(fGlobalPathCtrls.Items[i], ICEPathChangeHandler, PathHandler) then
    begin
      PathHandler.GlobalContentChange(Sender);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set Active component
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.SetActivePage(const Value: TComponent);
var
  i: Integer;
  PathHandler: ICEPathChangeHandler;
begin
  try
    for i:= 0 to fGlobalPathCtrls.Count - 1 do
    begin
      if Supports(fGlobalPathCtrls.Items[i], ICEPathChangeHandler, PathHandler) then
      PathHandler.GlobalActivePageChange(fActivePage, Value);
    end;
  finally
    fActivePage:= Value;
  end;
end;

{-------------------------------------------------------------------------------
  Set GlobalPathCaption
-------------------------------------------------------------------------------}
procedure TCEPathCtrl.SetGlobalPathCaption(const Value: WideString);
begin
  fGlobalPathCaption:= Value;

  if MainForm.PathInTitle then
  begin
    MainForm.Caption:= fGlobalPathCaption;
    TntApplication.Title:= fGlobalPathCaption;
  end;

  MainForm.TrayIcon.Hint:= 'CubicExplorer: ' +  fGlobalPathCaption;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEMouseCtrl
-------------------------------------------------------------------------------}
constructor TCEMouseCtrl.Create;
begin
  inherited;
  CtrlList:= TComponentList.Create(false);
  fTextEditorBugFix:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEMouseCtrl
-------------------------------------------------------------------------------}
destructor TCEMouseCtrl.Destroy;
begin
  CtrlList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  MouseWheel focus change
-------------------------------------------------------------------------------}
procedure TCEMouseCtrl.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  p: TPoint;
  h: HWND;
  w: TWinControl;
  i: Integer;
  wParam, lParam, key: Integer;
  pos: TPoint;
begin
  if (Shift <> [ssCtrl]) then
  fTextEditorBugFix:= false;

  // Go Forward/Back in history
  if (Shift = [ssShift]) and (WheelDelta <> 0) then
  begin
    if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    begin
      if WheelDelta > 0 then
      CEActions.act_navi_forward.Execute
      else
      CEActions.act_navi_back.Execute;

      Handled:= true;
      Exit;
    end;
  end
  // Select Next/Previous tab
  else if (Shift = [ssCtrl]) and (WheelDelta <> 0) then
  begin
    if not (GlobalPathCtrl.ActivePage is TCETextEditorPage) or not fTextEditorBugFix then
    begin
      if WheelDelta > 0 then
      MainForm.TabSet.SelectNextTab(true)
      else if WheelDelta < 0 then
      MainForm.TabSet.SelectNextTab(false);
    end;

    Handled:= true;
    Exit;
  end
  else if (Shift = [ssLeft]) and (WheelDelta <> 0) then
  begin
    if GlobalPathCtrl.ActivePage is TCETextEditorPage then
    fTextEditorBugFix:= true;
  end;

  Handled:= false;
  
  if GetCursorPos(p) then
  begin
    h:= WindowFromPoint(p);
    if h = TWinControl(Sender).Handle then
    Exit;

    for i:= 0 to CtrlList.Count-1 do
    begin
      w:= TWinControl(CtrlList.Items[i]);
      if w.Handle = h then
      begin
        if not w.Focused then
        begin
          GetCursorPos(pos);
          key:= 0;
          if ssShift in Shift then
          key:= key + MK_SHIFT;
          if ssCtrl in Shift then
          key:= key + MK_CONTROL;
          if ssLeft in Shift then
          key:= key + MK_LBUTTON;
          if ssRight in Shift then
          key:= key + MK_RBUTTON;
          if ssMiddle in Shift then
          key:= key + MK_MBUTTON;

          wParam:= MakeWParam(Key, WheelDelta);
          lParam:= MakeLParam(pos.X, pos.Y);

          w.SetFocus;
          PostMessage(w.Handle, WM_MOUSEWHEEL, wParam, lParam);
          Handled:= true;
        end;
        break;
      end;
    end;

  end;
end;

{##############################################################################}

initialization
  GlobalPathCtrl:= TCEPathCtrl.Create;
  GlobalFocusCtrl:= TCEMouseCtrl.Create;

finalization
  FreeAndNil(GlobalPathCtrl);
  FreeAndNil(GlobalFocusCtrl);

end.
