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
//  The Original Code is CE_ToolbarEditorItems.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ToolbarEditorItems;

interface

uses
  // SpTBX
  SpTBXItem, SpTBXEditors, TB2Item, SpTBXSkins, TB2Toolbar, TB2Common,
  // Tnt
  TntStdCtrls,
  // System units
  Graphics, Windows, Classes, SysUtils, StdCtrls, Controls, Messages,
  Forms, TntClasses;

type
  TCustomEditAccess = class(TCustomEdit);
  TSpTBXComboBoxAccess = class(TSpTBXComboBox)
  end;

  TTBCustomItemAccess = class(TTBCustomItem);
  TAccessWinControl = class(TWinControl);
  TSpTBXEditItemAccess = class(TSpTBXEditItem);

  TCEClearButtonClickEvent = procedure(Sender: TObject; Shift: TShiftState; var ClearText: Boolean) of object;

  TCEToolbarEditItemViewer = class;

  TCEToolbarEditItem = class(TSpTBXEditItem)
  private
    fAutoShowClearButton: Boolean;
    fDefaultWidth: Integer;
    fOnClearButtonClick: TCEClearButtonClickEvent;
    fShowClearButton: Boolean;
    procedure SetShowClearButton(const Value: Boolean);
  protected
    fResizingEditControl: Boolean;
    fViewer: TCEToolbarEditItemViewer;
    procedure DoChange(const AText: WideString); override;
    procedure DoClearButtonClick(Shift: TShiftState); virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure KeyPress(var Key: Char); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property DefaultWidth: Integer read fDefaultWidth write fDefaultWidth;
    property Viewer: TCEToolbarEditItemViewer read fViewer;
  published
    property AutoShowClearButton: Boolean read fAutoShowClearButton write
        fAutoShowClearButton;
    property ShowClearButton: Boolean read fShowClearButton write
        SetShowClearButton;
    property OnClearButtonClick: TCEClearButtonClickEvent read fOnClearButtonClick write
        fOnClearButtonClick;
  end;

  TCEToolbarEditItemViewer = class(TSpTBXEditItemViewer)
  private
    fMouseDownOnClearButton: Boolean;
    fMouseDownShiftState: TShiftState;
  protected
    fDragSizing: Boolean;
    fInvertedSizing: Boolean;
    fMouseDownOffset: TPoint;
    ResizeAreaSize: Integer;
    function DoExecute: Boolean; override;
    procedure GetClearButtonRect(var R: TRect); virtual;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    function GetEditControlText: WideString; virtual;
    procedure GetEditRect(var R: TRect); override;
    function GetIndentAfter: Integer; override;
    function GetIndentBefore: Integer; override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo); override;
    procedure InternalEditControlExit; override;
    procedure DoKeyPress(Sender: TObject; var Key: Char); virtual;
    function EditLoop(const CapHandle: HWND): Boolean; reintroduce; virtual;
    procedure EditWndProc(var Message: TMessage); reintroduce; virtual;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu:
        Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
        override;
  end;

  TCEToolbarComboBoxItem = class(TCEToolbarEditItem)
  private
    fComboItemIndex: Integer;
    procedure SetComboItemIndex(const Value: Integer);
  protected
    function GetComboItems: TTntStrings; virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ComboItemIndex: Integer read fComboItemIndex write SetComboItemIndex;
    property ComboItems: TTntStrings read GetComboItems;
  end;

  TCEToolbarComboBoxItemViewer = class(TCEToolbarEditItemViewer)
  private
    fMouseOverDropButton: Boolean;
    procedure UpdateDropDownButton;
  protected
    fComboBox: TSpTBXComboBox;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    function GetIndentAfter: Integer; override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo); override;
    procedure DoChange(NeedResize: Boolean);
    function EditLoop(const CapHandle: HWND): Boolean; override;
    function GetEditControlText: WideString; override;
    procedure GetEditRect(var R: TRect); override;
    procedure InternalEditControlExit; override;
    procedure InternalEditControlChange(Sender: TObject); override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu:
        Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure SetMouseOverDropButton(Value: Boolean);
  public
    destructor Destroy; override;
    function GetDropDownButtonRect: TRect;
    function GetMouseInDropDownButton: Boolean;
  published
  end;

implementation

uses
  Themes;

{##############################################################################}
// TCEToolbarEditItem

{-------------------------------------------------------------------------------
  Create and instance of TCEToolbarEditItem
-------------------------------------------------------------------------------}
constructor TCEToolbarEditItem.Create(AOwner: TComponent);
begin
  inherited;
  fDefaultWidth:= 64;
end;

{-------------------------------------------------------------------------------
  DoChange
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.DoChange(const AText: WideString);
begin
  inherited;
  if fAutoShowClearButton then
  ShowClearButton:= AText <> '';
end;

{-------------------------------------------------------------------------------
  Do ClearButtonClick
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.DoClearButtonClick(Shift: TShiftState);
var
  ClearText: Boolean;
begin
  ClearText:= true;
  if Assigned(fOnClearButtonClick) then fOnClearButtonClick(Self, Shift, ClearText);
  if ClearText then
  Text:= '';
end;

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCEToolbarEditItem.GetItemViewerClass(AView: TTBView):
    TTBItemViewerClass;
begin
  Result:= TCEToolbarEditItemViewer;
end;

{-------------------------------------------------------------------------------
  Key Press
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.KeyPress(var Key: Char);
begin

end;

{-------------------------------------------------------------------------------
  Show Clear Button
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.SetShowClearButton(const Value: Boolean);
begin
  if Value <> fShowClearButton then
  begin
    fShowClearButton:= Value;
    fResizingEditControl:= true;    
    Change(true);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarEditItemViewer
-------------------------------------------------------------------------------}
constructor TCEToolbarEditItemViewer.Create(AView: TTBView; AItem:
    TTBCustomItem; AGroupLevel: Integer);
begin
  inherited;
  ResizeAreaSize:= 3;
  if AItem is TCEToolbarEditItem then
  TCEToolbarEditItem(AItem).fViewer:= Self;
end;

{-------------------------------------------------------------------------------
  DoExecute
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.DoExecute: Boolean;
begin
  // Close any delay-close popup menus before entering the edit loop
  View.CancelChildPopups;
  Result:= False;
  if EditLoop(View.GetCaptureWnd) then
  begin
    View.EndModal;
    if ecsAccept in FEditControlStatus then
      Result := True;
  end;
end;

{-------------------------------------------------------------------------------
  Get ClearButtonRect
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.GetClearButtonRect(var R: TRect);
begin
  R.Left:= (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
  R.Right:= R.Left + 16;
  R.Top:= 0;
  R.Bottom:= BoundsRect.Bottom - BoundsRect.Top;
end;

{-------------------------------------------------------------------------------
  Get Cursor
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.GetCursor(const Pt: TPoint; var ACursor:
    HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  //InflateRect(R, -2, 0);
  if PtInRect(R, Pt) then
    ACursor:= LoadCursor(0, IDC_IBEAM)
  else if IsToolbarStyle and (//((Pt.X <= ResizeAreaSize) or
          (Pt.X >= (BoundsRect.Right-BoundsRect.Left-ResizeAreaSize))) then
    ACursor:= LoadCursor(0, IDC_SIZEWE)
  else
    ACursor:= LoadCursor(0, IDC_ARROW);
end;

{-------------------------------------------------------------------------------
  Get EditControlText
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetEditControlText: WideString;
begin
  Result := '';
  if Assigned(FEditControl) then begin
    if FEditControl is TSpTBXUnicodeEdit then
      Result := TSpTBXUnicodeEdit(FEditControl).Text
    else
      Result := TCustomEditAccess(FEditControl).Text;
  end;
end;

procedure TCEToolbarEditItemViewer.GetEditRect(var R: TRect);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Indent After
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetIndentAfter: Integer;
begin
  Result:= ResizeAreaSize;
  if TCEToolbarEditItem(Item).ShowClearButton then
  Result:= Result + 16;
end;

{-------------------------------------------------------------------------------
  Get Indent Before
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetIndentBefore: Integer;
begin
  Result:= ResizeAreaSize;
end;

{-------------------------------------------------------------------------------
  Internal Draw Frame
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.InternalDrawFrame(ACanvas: TCanvas; ARect:
    TRect; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
begin
  inherited;

  if TCEToolbarEditItem(Item).fResizingEditControl then
  begin
    if assigned(FEditControl) then
    begin
      GetEditRect(R);
      InflateRect(R, -3, -3);
      FEditControl.BoundsRect:= R;
    end;
    TCEToolbarEditItem(Item).fResizingEditControl:= false;
  end;

  // Draw Clear button
  if TCEToolbarEditItem(Item).ShowClearButton then
  begin
    GetClearButtonRect(R);
//    if ItemInfo.HotTrack then
//    SpDrawGlyphPattern(ACanvas, R, 0, clWindowText)
//    else

    SpDrawGlyphPattern(ACanvas, R, 0, clGrayText);
  end;
end;

{-------------------------------------------------------------------------------
  InternalEditControlExit
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.InternalEditControlExit;
begin
  Item.Text:= GetEditControlText;
end;

{-------------------------------------------------------------------------------
  DoKeyPress
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.DoKeyPress(Sender: TObject; var Key: Char);
begin
  TCEToolbarEditItem(Item).KeyPress(Key);
end;

{-------------------------------------------------------------------------------
  EditLoop
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.EditLoop(const CapHandle: HWND): Boolean;

  procedure ControlMessageLoop;

    function PointInWindow(const Wnd: HWND; const P: TPoint): Boolean;
    var
      W: HWND;
    begin
      Result := False;
      W := WindowFromPoint(P);
      if W = 0 then Exit;
      if W = Wnd then
        Result := True
      else
        if IsChild(Wnd, W) then
          Result := True;
    end;

    function ContinueLoop: Boolean;
    begin
      Result := (ecsContinueLoop in FEditControlStatus) and
        not View.IsModalEnding and FEditControl.Focused and Item.Enabled;
      { Note: View.IsModalEnding is checked since TTBView.CancelMode doesn't
        destroy popup windows; it merely hides them and calls EndModal. So if
        IsModalEnding returns True we can infer that CancelMode was likely
        called. }
    end;

  var
    Msg: TMsg;
    IsKeypadDigit: Boolean;
    ScanCode: Byte;
    V: Integer;
  begin
    try
      while ContinueLoop do begin
        { Examine the next message before popping it out of the queue }
        if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
          WaitMessage;
          Continue;
        end;
        case Msg.message of
          WM_SYSKEYDOWN: begin
              { Exit immediately if Alt+[key] or F10 are pressed, but not
                Alt+Shift, Alt+`, or Alt+[keypad digit] }
              if (Msg.wParam <> VK_MENU) and (Msg.wParam <> VK_SHIFT) and
                 (Msg.wParam <> VK_HANJA) then begin
                IsKeypadDigit := False;
                { This detect digits regardless of whether Num Lock is on: }
                ScanCode := Byte(Msg.lParam shr 16);
                if ScanCode <> 0 then
                  for V := VK_NUMPAD0 to VK_NUMPAD9 do
                    if MapVirtualKey(V, 0) = ScanCode then begin
                      IsKeypadDigit := True;
                      Break;
                    end;
                if not IsKeypadDigit then begin
                  FEditControlStatus := [ecsClose];
                  Exit;
                end;
              end;
            end;
          WM_SYSKEYUP: begin
              { Exit when Alt is released by itself }
              if Msg.wParam = VK_MENU then begin
                FEditControlStatus := [ecsClose];
                Exit;
              end;
            end;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
          WM_MBUTTONDOWN, WM_MBUTTONDBLCLK,
          WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK,
          WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK,
          WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK: begin
              { If a mouse click outside the edit control is in the queue,
                exit and let the upstream message loop deal with it }
              if Msg.hwnd <> FEditControl.Handle then
                Exit;
            end;
          WM_MOUSEMOVE, WM_NCMOUSEMOVE: begin
              if GetCapture = CapHandle then begin
                if PointInWindow(FEditControl.Handle, Msg.pt) then
                  ReleaseCapture;
              end
              else if GetCapture = 0 then begin
                if not PointInWindow(FEditControl.Handle, Msg.pt) then
                  SetCapture(CapHandle);
              end;
              if GetCapture = CapHandle then
                SetCursor(LoadCursor(0, IDC_ARROW));
            end;
        end;
        { Now pop the message out of the queue }
        if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
          Continue;
        if ((Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST)) and
           (Msg.hwnd = CapHandle) then
          { discard, so that the selection doesn't get changed }
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Make sure there are no outstanding WM_*CHAR messages }
      RemoveMessages(WM_CHAR, WM_DEADCHAR);
      RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    end;
  end;

var
  R: TRect;
  ActiveWnd, FocusWnd: HWND;
  S: WideString;
begin
  GetEditRect(R);
  if IsRectEmpty(R) then begin
    Result := False;
    Exit;
  end;

  ActiveWnd := GetActiveWindow;
  FocusWnd := GetFocus;

  { Create the edit control }
  InflateRect(R, -3, -3);
  FEditControl := GetEditControlClass.Create(nil);
  try
    FEditControl.Name := Format('%s_edit_control_%p', [ClassName, Pointer(FEditControl)]);
    FEditControl.Visible := False;
    TCustomEditAccess(FEditControl).ReadOnly := Item.ReadOnly;
    TCustomEditAccess(FEditControl).BorderStyle := bsNone;
    TCustomEditAccess(FEditControl).AutoSize := False;
    TCustomEditAccess(FEditControl).Font.Assign(View.GetFont);
    Item.EditorFontSettings.Apply(TCustomEditAccess(FEditControl).Font);
    if FEditControl is TSpTBXUnicodeEdit then begin
      TSpTBXUnicodeEdit(FEditControl).Alignment := Item.Alignment;
      TSpTBXUnicodeEdit(FEditControl).PasswordChar := Item.PasswordChar;
      TSpTBXUnicodeEdit(FEditControl).Text := Item.Text
    end
    else
      TCustomEditAccess(FEditControl).Text := Item.Text;
    TCustomEditAccess(FEditControl).CharCase := Item.CharCase;
    TCustomEditAccess(FEditControl).MaxLength := Item.MaxLength;
    FEditControl.BoundsRect := R;
    FEditControl.WindowProc := EditWndProc;
    FEditControl.ParentWindow := View.Window.Handle;
    TCustomEditAccess(FEditControl).OnChange := InternalEditControlChange;
    TCustomEditAccess(FEditControl).OnKeyPress:= DoKeyPress;
    FEditControl.SelectAll;
    DoBeginEdit;
    FEditControl.Visible := True;
    FEditControl.SetFocus;
    if GetActiveWindow <> ActiveWnd then
      SendMessage(ActiveWnd, WM_NCACTIVATE, 1, 0) // Don't gray out title bar of old active window
    else
      ActiveWnd := 0;

    FEditControlStatus := [ecsContinueLoop];
    ControlMessageLoop;
  finally
    if FEditControlStatus = [ecsContinueLoop] then
      InternalEditControlExit;
    S := GetEditControlText;
    FreeAndNil(FEditControl);
  end;

  if (FEditControlStatus = [ecsContinueLoop]) and Item.ExtendedAccept then
  begin
    if TSpTBXEditItemAccess(Item).DoAcceptText(S) then
    TSpTBXEditItemAccess(Item).SetTextEx(S, tcrEditControl);
  end;

  { ensure the area underneath the edit control is repainted immediately }
  View.Window.Update;
  { If app is still active, set focus to previous control and restore capture
    to CapHandle if another control hasn't taken it }
  if GetActiveWindow <> 0 then begin
    SetFocus(FocusWnd);
    if GetCapture = 0 then
      SetCapture(CapHandle);
  end;
  if ActiveWnd <> 0 then
    SendMessage(ActiveWnd, WM_NCACTIVATE, Ord(GetActiveWindow = ActiveWnd), 0);
  { The SetFocus call above can change the Z order of windows. If the parent
    window is a popup window, reassert its topmostness. }
  if View.Window is TTBPopupWindow then
    SetWindowPos(View.Window.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  { Send an MSAA "focus" event now that we're returning to the regular modal loop }
  View.NotifyFocusEvent;

  Result := ecsClose in FEditControlStatus;
  if not Result and (GetCapture = CapHandle) then begin
    if ecsAccept in FEditControlStatus then
      { if we are accepting but not closing, Tab must have been pressed }
      View.Selected := View.NextSelectable(View.Selected,
        GetKeyState(VK_SHIFT) >= 0);
  end;
end;

{-------------------------------------------------------------------------------
  EditWndProc
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.EditWndProc(var Message: TMessage);

  procedure AcceptText;
  var
    S: WideString;
  begin
    S := GetEditControlText;
    if TSpTBXEditItemAccess(Item).DoAcceptText(S) then
    TSpTBXEditItemAccess(Item).SetTextEx(S, tcrEditControl);
  end;

begin
  if FEditControl = nil then
    Exit;

  if not HandleEditMessage(Message) then begin
    if Message.Msg = WM_CHAR then
      case TWMChar(Message).CharCode of
        VK_TAB: begin
            FEditControlStatus := [ecsAccept];
            AcceptText;
            Exit;
          end;
        VK_RETURN: begin
            FEditControlStatus := [ecsAccept, ecsClose];
            AcceptText;
            Exit;
          end;
        VK_ESCAPE: begin
            FEditControlStatus := [];
            Exit;
          end;
      end;
    TCustomEditAccess(FEditControl).WndProc(Message);
  end;
  
  if Message.Msg = WM_KILLFOCUS then begin
    View.CancelMode;
    FEditControlStatus := [ecsClose];
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
    var MouseDownOnMenu: Boolean);
var
  w,h: Integer;
  R: TRect;
begin
  w:= BoundsRect.Right-BoundsRect.Left;
  if (X <= ResizeAreaSize) then
  begin
    fDragSizing:= false;
    fInvertedSizing:= true;
  end
  else if (X >= (w-ResizeAreaSize)) then
  begin
    fDragSizing:= true;
    fInvertedSizing:= false;
  end
  else
  fDragSizing:= false;

  if IsToolbarStyle and fDragSizing and (Shift = []) then
  begin
    h:= BoundsRect.Bottom-BoundsRect.Top;
    fMouseDownOffset:= Point(w-X, h-Y);
    Self.View.Selected:= Self;
    Self.View.SetCapture;
  end
  else
  begin
    GetClearButtonRect(R);
    fMouseDownOnClearButton:= PtInRect(R, Point(X, Y));
    fMouseDownShiftState:= Shift;
    if not fMouseDownOnClearButton then
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Move
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseMove(X, Y: Integer);
var
  size: Integer;
begin
  if fDragSizing then
  begin
    if Self.Item.Visible then
    begin
      if fInvertedSizing then
      size:= fMouseDownOffset.X - X
      else
      size:= X + fMouseDownOffset.X;
      TCEToolbarEditItem(Item).DefaultWidth:= size;
      Item.CustomWidth:= size;
    end
    else
    begin

    end;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Up
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu:
    Boolean);
var
  R: TRect;
begin
  if fDragSizing then
  begin
    fDragSizing:= false;
    Self.View.CancelCapture;
  end
  else
  begin
    if fMouseDownOnClearButton and TCEToolbarEditItem(Item).ShowClearButton then
    begin
      GetClearButtonRect(R);
      if PtInRect(R, Point(X,Y)) then
      begin
        TCEToolbarEditItem(Item).DoClearButtonClick(fMouseDownShiftState);
      end;
    end
    else
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarComboBoxItem
-------------------------------------------------------------------------------}
constructor TCEToolbarComboBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  fComboItemIndex:= -1;
end;

{-------------------------------------------------------------------------------
  Destroy TCEToolbarComboBoxItem
-------------------------------------------------------------------------------}
destructor TCEToolbarComboBoxItem.Destroy;
begin
  inherited;
end;

function TCEToolbarComboBoxItem.GetComboItems: TTntStrings;
begin
  Result:= nil;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItem.GetItemViewerClass(AView: TTBView):
    TTBItemViewerClass;
begin
  Result:= TCEToolbarComboBoxItemViewer;
end;

{-------------------------------------------------------------------------------
  Key Press
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItem.KeyPress(var Key: Char);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Set ComboItemIndex
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItem.SetComboItemIndex(const Value: Integer);
begin
  if Value < -1 then
  fComboItemIndex:= -1
  else
  fComboItemIndex:= Value;

  if assigned(ComboItems) and  (fComboItemIndex > -1) then
  Self.Text:= ComboItems.Strings[fComboItemIndex];
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy TCEToolbarComboBoxItemViewer
-------------------------------------------------------------------------------}
destructor TCEToolbarComboBoxItemViewer.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Cursor
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.GetCursor(const Pt: TPoint; var ACursor:
    HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  //InflateRect(R, -2, 0);
  if PtInRect(R, Pt) then
  begin
    ACursor:= LoadCursor(0, IDC_IBEAM);
    SetMouseOverDropButton(false);
  end
  else if IsToolbarStyle and ((Pt.X <= ResizeAreaSize) or (Pt.X >= (BoundsRect.Right-BoundsRect.Left-ResizeAreaSize))) then
  begin
    ACursor:= LoadCursor(0, IDC_SIZEWE);
    SetMouseOverDropButton(false);
  end
  else
  begin
    ACursor:= LoadCursor(0, IDC_ARROW);
    SetMouseOverDropButton(GetMouseInDropDownButton);
  end;
end;

{-------------------------------------------------------------------------------
  Get DropDown Button Rect
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetDropDownButtonRect: TRect;
var
  ButtonWidth: Integer;
  T: TSpTBXSkinType;
begin
  ButtonWidth:= GetSystemMetrics(SM_CXHSCROLL);
  Result.Left:= (Self.BoundsRect.Right - Self.BoundsRect.Left) - ButtonWidth;
  Result.Top:= 0;
  Result.Right:= Result.Left + ButtonWidth;
  Result.Bottom:= Self.BoundsRect.Bottom - Self.BoundsRect.Top;


  T:= SkinManager.GetSkinType;
  case T of
    sknNone:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknWindows:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknSkin:
      begin
        InflateRect(Result, 0, -2);
        OffsetRect(Result, -2, 0);
      end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Indent After
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetIndentAfter: Integer;
begin
  Result:= inherited GetIndentAfter;
  Result:= Result + GetSystemMetrics(SM_CXHSCROLL) - ResizeAreaSize;
end;

{-------------------------------------------------------------------------------
  Get MouseInDropDownButton
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetMouseInDropDownButton: Boolean;
var
  P: TPoint;
  ButtonR: TRect;
  ButtonWidth: Integer;
begin
  Result := False;

  if not (csDesigning in Item.ComponentState) and GetCursorPos(P) then
  begin
    P:= ScreenToClient(P);
    ButtonWidth:= GetSystemMetrics(SM_CXHSCROLL);
    ButtonR.Left:= (BoundsRect.Right - BoundsRect.Left) - ButtonWidth;
    ButtonR.Top:= 0;
    ButtonR.Right:= ButtonR.Left + ButtonWidth;
    ButtonR.Bottom:= BoundsRect.Bottom - BoundsRect.Top;
    Result:= PtInRect(ButtonR, P);
  end;
end;

{-------------------------------------------------------------------------------
  Internal Draw Frame
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.InternalDrawFrame(ACanvas: TCanvas;
    ARect: TRect; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
  T: TSpTBXSkinType;
begin
  inherited;
  T:= SkinManager.GetSkinType;

  R:= GetDropDownButtonRect;
  fMouseOverDropButton:= GetMouseInDropDownButton;
  SpDrawXPComboButton(ACanvas, R, ItemInfo.Enabled,  ItemInfo.HotTrack, fMouseOverDropButton, false, True, T);
end;

{-------------------------------------------------------------------------------
  DoChange
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.DoChange(NeedResize: Boolean);
begin
  TTBCustomItemAccess(Item).Change(NeedResize);
end;

{-------------------------------------------------------------------------------
  EditLoop
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.EditLoop(const CapHandle: HWND): Boolean;
var
  fRedrawIsLocked: Boolean;

  procedure ControlMessageLoop;

    function PointInWindow(const Wnd: HWND; const P: TPoint): Boolean;
    var
      W: HWND;
    begin
      Result := False;
      W := WindowFromPoint(P);
      if W = 0 then Exit;
      if W = Wnd then
        Result := True
      else
        if IsChild(Wnd, W) then
          Result := True;
    end;

    function ContinueLoop: Boolean;
    begin
      Result := (ecsContinueLoop in FEditControlStatus) and
        // TODO: Why is View.IsModalEnding true at the beginning?
        //not View.IsModalEnding and
        fComboBox.Focused and Item.Enabled;
      { Note: View.IsModalEnding is checked since TTBView.CancelMode doesn't
        destroy popup windows; it merely hides them and calls EndModal. So if
        IsModalEnding returns True we can infer that CancelMode was likely
        called. }
    end;

  var
    Msg: TMsg;
    IsKeypadDigit: Boolean;
    V: Integer;
  begin
    try
      while ContinueLoop do begin
        { Examine the next message before popping it out of the queue }
        if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
          WaitMessage;
          Continue;
        end;
        case Msg.message of
          WM_SYSKEYDOWN: begin
              { Exit immediately if Alt+[key] or F10 are pressed, but not
                Alt+Shift, Alt+`, or Alt+[keypad digit] }
              if (Msg.wParam <> VK_MENU) and (Msg.wParam <> VK_SHIFT) and
                 (Msg.wParam <> VK_HANJA) then begin
                IsKeypadDigit := False;
                { This detect digits regardless of whether Num Lock is on: }
                if Lo(LongRec(Msg.lParam).Hi) <> 0 then
                  for V := VK_NUMPAD0 to VK_NUMPAD9 do
                    if MapVirtualKey(V, 0) = Lo(LongRec(Msg.lParam).Hi) then begin
                      IsKeypadDigit := True;
                      Break;
                    end;
                if not IsKeypadDigit then begin
                  FEditControlStatus := [ecsClose];
                  Exit;
                end;
              end;
            end;
          WM_SYSKEYUP: begin
              { Exit when Alt is released by itself }
              if Msg.wParam = VK_MENU then begin
                FEditControlStatus := [ecsClose];
                Exit;
              end;
            end;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
          WM_MBUTTONDOWN, WM_MBUTTONDBLCLK,
          WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK,
          WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK,
          WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK: begin
              { If a mouse click outside the edit control is in the queue,
                exit and let the upstream message loop deal with it }

                if not PointInWindow(fComboBox.Handle, Msg.pt) and not fComboBox.DroppedDown then
                Exit;
//              if Msg.hwnd <> fComboBox.Handle then
//                Exit;
                UpdateDropDownButton;
            end;
          WM_MOUSEMOVE, WM_NCMOUSEMOVE: begin
              if GetCapture = CapHandle then
              begin
                if PointInWindow(fComboBox.Handle, Msg.pt) then
                ReleaseCapture;
              end
              else if GetCapture = 0 then
              begin
                if not PointInWindow(fComboBox.Handle, Msg.pt) then
                SetCapture(CapHandle);
              end;
              if GetCapture = CapHandle then
              SetCursor(LoadCursor(0, IDC_ARROW));
              UpdateDropDownButton;
            end;
          WM_PAINT: begin
            if fRedrawIsLocked then
            begin
              SendMessage(View.Window.Handle, WM_SETREDRAW, 1,0); // prevent flicker
              fRedrawIsLocked:= false;
            end;
          end;
          WM_CHAR: begin
            if Msg.wParam = Ord(#9) then
            begin
              exit;
            end;
          end;
        end;
        { Now pop the message out of the queue }
        if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
          Continue;
        if ((Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST)) and
           (Msg.hwnd = CapHandle) then
          { discard, so that the selection doesn't get changed }
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Make sure there are no outstanding WM_*CHAR messages }
      RemoveMessages(WM_CHAR, WM_DEADCHAR);
      RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    end;
  end;

var
  R: TRect;
  ActiveWnd, FocusWnd: HWND;
begin
  GetEditRect(R);
  if IsRectEmpty(R) then
  begin
    Result:= False;
    Exit;
  end;

  ActiveWnd:= GetActiveWindow;
  FocusWnd:= GetFocus;

  { Create the edit control }
  //InflateRect(R, -3, -3);
  //Self.View.BeginUpdate;
  fComboBox:= TSpTBXComboBox.Create(nil);
  try
    fComboBox.Name:= Format('%s_combo_control_%p', [ClassName, Pointer(fComboBox)]);
    fComboBox.Visible:= False;
    fComboBox.BoundsRect:= Self.BoundsRect;
    fComboBox.TabStop:= false;
    //fComboBox.WindowProc:= EditWndProc;

    fComboBox.ParentWindow:= View.Window.Handle;
    fComboBox.SkinType:= SkinManager.GetSkinType;

    fComboBox.Font.Assign(View.GetFont);
    Item.EditorFontSettings.Apply(fComboBox.Font);

    if assigned(TCEToolbarComboBoxItem(Item).ComboItems) then
    fComboBox.Items.Assign(TCEToolbarComboBoxItem(Item).ComboItems);
    fComboBox.ItemIndex:= fComboBox.Items.IndexOf(Item.Text);
    fComboBox.Text:= Item.Text;

    fComboBox.CharCase := Item.CharCase;
    fComboBox.MaxLength := Item.MaxLength;

    fComboBox.OnChange := InternalEditControlChange;
    fComboBox.OnKeyPress:= DoKeyPress;
    fComboBox.SelectAll;
    DoBeginEdit;
    
    fComboBox.Visible := True;
    fComboBox.SetFocus;

    if GetActiveWindow <> ActiveWnd then
      SendMessage(ActiveWnd, WM_NCACTIVATE, 1, 0) // Don't gray out title bar of old active window
    else
      ActiveWnd := 0;

    fComboBox.DroppedDown:= fMouseOverDropButton;

    FEditControlStatus := [ecsContinueLoop];

    SendMessage(View.Window.Handle, WM_SETREDRAW, 0,0); // prevent flicker
    fRedrawIsLocked:= true;

    ControlMessageLoop;
  finally
    if FEditControlStatus = [ecsContinueLoop] then
      InternalEditControlExit;

    TCEToolbarComboBoxItem(Item).ComboItemIndex:= fComboBox.ItemIndex;
    if fComboBox.ItemIndex = -1 then
    Item.Text:= fComboBox.Text;

    fComboBox.DroppedDown:= false;

    if not fRedrawIsLocked then
    SendMessage(View.Window.Handle, WM_SETREDRAW, 0,0); // prevent flicker

    FreeAndNil(fComboBox);
    
    SendMessage(View.Window.Handle, WM_SETREDRAW, 1,0); // prevent flicker
  end;

  View.Window.Update;

  { If app is still active, set focus to previous control and restore capture
    to CapHandle if another control hasn't taken it }
  if GetActiveWindow <> 0 then
  begin
    SetFocus(FocusWnd);
    if GetCapture = 0 then
    SetCapture(CapHandle);
  end;
  if ActiveWnd <> 0 then
    SendMessage(ActiveWnd, WM_NCACTIVATE, Ord(GetActiveWindow = ActiveWnd), 0);
  { The SetFocus call above can change the Z order of windows. If the parent
    window is a popup window, reassert its topmostness. }
  if View.Window is TTBPopupWindow then
    SetWindowPos(View.Window.Handle, HWND_TOPMOST, 0, 0, 0, 0,
                 SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  { Send an MSAA "focus" event now that we're returning to the regular modal loop }
  View.NotifyFocusEvent;

  Result:= ecsClose in FEditControlStatus;
  if not Result and (GetCapture = CapHandle) then
  begin
    if ecsAccept in FEditControlStatus then
      { if we are accepting but not closing, Tab must have been pressed }
    View.Selected:= View.NextSelectable(View.Selected, GetKeyState(VK_SHIFT) >= 0);
  end;
end;

{-------------------------------------------------------------------------------
  Get EditControlText
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetEditControlText: WideString;
begin
  Result := '';
  if Assigned(fComboBox) then
  begin
    Result:= TTntComboBox(fComboBox).Text;
  end;
end;

{-------------------------------------------------------------------------------
  Get EditRect
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.GetEditRect(var R: TRect);
begin
  R:= BoundsRect;
  R.Left:= R.Left + GetIndentBefore;
  R.Right:= R.Right - GetIndentAfter;

  case SkinManager.GetSkinType of
    sknNone:
      begin
        InflateRect(R, 0, -1);
        OffsetRect(R, -1, 0);
      end;
    sknWindows:
      begin
        InflateRect(R, 0, -1);
        OffsetRect(R, -1, 0);
      end;
    sknSkin:
      begin
        InflateRect(R, 0, -2);
        OffsetRect(R, -2, 0);
      end;
  end;
end;

{-------------------------------------------------------------------------------
  InternalEditControlExit
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.InternalEditControlExit;
begin
  Item.Text:= fComboBox.Text;
end;

{-------------------------------------------------------------------------------
  InternalEditControlChange
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.InternalEditControlChange(Sender:
    TObject);
begin
  // Used by descendants
  TCEToolbarEditItem(Item).SetTextEx(GetEditControlText, tcrEditControl);
end;

{-------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.MouseDown(Shift: TShiftState; X, Y:
    Integer; var MouseDownOnMenu: Boolean);
var
  r: TRect;
begin
  r:= GetDropDownButtonRect;
  fMouseOverDropButton:= PtInRect(r, Point(X, Y));

  inherited;
end;

{-------------------------------------------------------------------------------
  Mouse Up
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.MouseUp(X, Y: Integer;
    MouseWasDownOnMenu: Boolean);
begin
//  if IsPtInButtonPart(X, Y) then
//    MouseBeginEdit
//  else
    inherited;
end;

{-------------------------------------------------------------------------------
  Set MouseOverButton
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.SetMouseOverDropButton(Value: Boolean);
begin
  if Value <> fMouseOverDropButton then
  begin
    fMouseOverDropButton:= Value;
    DoChange(false);
  end;
end;

{-------------------------------------------------------------------------------
  UpdateDropDownButton
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.UpdateDropDownButton;
var
  ButtonState: Boolean;
begin
  if not fComboBox.DroppedDown then
  begin
    ButtonState:= GetMouseInDropDownButton;
    if ButtonState <> fMouseOverDropButton then
      if fComboBox.HandleAllocated then
      fComboBox.Invalidate;
    fMouseOverDropButton:= ButtonState;
  end;
end;

end.
