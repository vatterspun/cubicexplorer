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
//  The Original Code is CE_ScrollToolbar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ScrollToolbar;

interface

uses
  //Graphics32
  GR32_Image, GR32,
  // TBX
  SpTBXItem, SpTBXSkins,
  // System Units
  Classes, SysUtils, Messages, Windows, Contnrs, Controls, Graphics, ExtCtrls,
  UxTheme, Themes;

type
  TItemStyle = (itNormal, itSelected, itPushed, itChecked);

  TCEScrollToolbarItemClass = class of TCEScrollToolbarItem;
  TCEScrollToolbarItem = class(TObject)
  private
    fCaption: string;
    fChecked: Boolean;
  protected
    procedure DrawItem(Buffer: TBitmap32; ARect: TRect; ItemStyle: TItemStyle =
        itNormal); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetChecked(const Value: Boolean); virtual;
  public
    function GetWidth(Buffer: TBitmap32): Integer; virtual;
    property Caption: string read fCaption write SetCaption;
    property Checked: Boolean read fChecked write SetChecked;
  end;

  TCEScrollToolbar = class(TImage32)  
  private
    fAnimTimer: TTimer;
    fArrowSize: Integer;
    fDestOffset: Integer;
    fItems: TObjectList;
    fLeftArrowVisible: Boolean;
    fOffset: Integer;
    fMouseDown: Boolean;
    fOnBackgroundClick: TNotifyEvent;
    fRightArrowVisible: Boolean;
    fSelectedIndex: Integer;
    fSeparatorSize: Integer;
    procedure SetOffset(const Value: Integer);
    procedure SpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure ClickLeftArrow; virtual;
    procedure ClickRightArrow; virtual;
    procedure DoBackgroundClick; virtual;
    procedure DrawBackground; virtual;
    procedure DrawLeftArrow(ItemStyle: TItemStyle = itNormal); virtual;
    procedure DrawRightArrow(ItemStyle: TItemStyle = itNormal); virtual;
    procedure DrawSeparator(ARect: TRect); virtual;
    function GetItemRect(Index: Integer): TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        overload; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure OnAnimTimer(Sender: TObject);
    procedure Paint; override;
    procedure SetSelectedIndex(const Value: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem(ItemClass: TCEScrollToolbarItemClass): TCEScrollToolbarItem;
        virtual;
    procedure MakeItemVisible(Index: Integer);
    procedure Clear;
    procedure DrawBar; virtual;
    function GetItem(Index: Integer): TCEScrollToolbarItem; virtual;
    function IndexByPos(X, Y: Integer): Integer; virtual;
    function InsertItem(ItemClass: TCEScrollToolbarItemClass; Index: Integer):
        TCEScrollToolbarItem; virtual;
    procedure Resize; override;
    property ArrowSize: Integer read fArrowSize write fArrowSize;
    property Items: TObjectList read fItems;
    property LeftArrowVisible: Boolean read fLeftArrowVisible;
    property Offset: Integer read fOffset write SetOffset;
    property RightArrowVisible: Boolean read fRightArrowVisible;
    property SelectedIndex: Integer read fSelectedIndex write SetSelectedIndex;
    property SeparatorSize: Integer read fSeparatorSize write fSeparatorSize;
  published
    property OnBackgroundClick: TNotifyEvent read fOnBackgroundClick write
        fOnBackgroundClick;
  end;

procedure DrawMenuItem(Buffer: TBitmap32; ARect: TRect; Checked: Boolean;
    ItemStyle: TItemStyle = itNormal; DrawBackground: Boolean = false);

implementation

{##############################################################################}

{*------------------------------------------------------------------------------
  Draw Menu Item
-------------------------------------------------------------------------------}
procedure DrawMenuItem(Buffer: TBitmap32; ARect: TRect; Checked: Boolean;
    ItemStyle: TItemStyle = itNormal; DrawBackground: Boolean = false);
var
  state: TSpTBXSkinStatesType;
  Flags: Integer;
begin
  Flags:= 0;
  
  if DrawBackground then
  begin
    case SkinManager.GetSkinType of
      sknNone:
        begin
          Buffer.Canvas.Brush.Color := clBtnFace;
          Buffer.Canvas.FillRect(ARect);
        end;
      sknWindows: begin
        DrawThemeBackground(ThemeServices.Theme[teRebar], Buffer.Canvas.Handle, 0, 0, ARect, nil);
      end;
      sknSkin: begin
        CurrentSkin.PaintBackground(Buffer.Canvas, ARect, skncDock, sknsNormal, True, false);
        CurrentSkin.PaintBackground(Buffer.Canvas, ARect, skncToolbar, sknsNormal, True, False);
      end;
    end;
  end;

  state:= sknsNormal;
  case ItemStyle of
    itNormal: state:= sknsNormal;
    itSelected: state:= sknsHotTrack;
    itPushed: state:= sknsPushed;
    itChecked: state:= sknsChecked;
  end;
  if Checked then
  state:= sknsChecked;

  case SkinManager.GetSkinType of
    sknNone: begin
      Buffer.Canvas.Brush.Color := clBtnFace;
      Buffer.Canvas.FillRect(ARect);
      if Checked then
      begin
        if ItemStyle <> itSelected  then
        begin
          Buffer.Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);;
          Buffer.Canvas.Brush.Bitmap.HandleType := bmDDB;  { needed for Win95, or else brush is solid white }
          Buffer.Canvas.FillRect(ARect);
          Buffer.Canvas.Brush.Style := bsClear;
        end;
        Windows.DrawEdge(Buffer.Canvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
      end
      else
      begin
        case state of
          sknsHotTrack:
            Windows.DrawEdge(Buffer.Canvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
          sknsPushed:
            Windows.DrawEdge(Buffer.Canvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
        end;
      end;
    end;
    sknWindows: begin
      case state of
        sknsNormal:   Flags := TS_NORMAL;
        sknsDisabled: Flags := TS_DISABLED;
        sknsHotTrack: Flags := TS_HOT;
        sknsPushed:   Flags := TS_PRESSED;
        sknsChecked:  Flags := TS_CHECKED;
        sknsCheckedAndHotTrack: Flags := TS_HOTCHECKED;
      end;
      DrawThemeBackground(ThemeServices.Theme[teToolBar], Buffer.Canvas.Handle, TP_BUTTON, Flags, ARect, nil);
    end;
    sknSkin: begin
      CurrentSkin.PaintBackground(Buffer.Canvas, ARect, skncToolbarItem, state, True, True, False, []);
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEScrollToolbar
-------------------------------------------------------------------------------}
constructor TCEScrollToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Height:= 24;
  fItems:= TObjectList.Create(true);
  fSelectedIndex:= -1;
  fOffset:= 0;
  fArrowSize:= 12;
  fSeparatorSize:= 0;
  fAnimTimer:= TTimer.Create(Self);
  fAnimTimer.Interval:= 10;
  fAnimTimer.Enabled:= false;
  fAnimTimer.OnTimer:= OnAnimTimer;
  SkinManager.AddSkinNotification(Self);
end;

{*------------------------------------------------------------------------------
  Destroy instance of TCEScrollToolbar
-------------------------------------------------------------------------------}
destructor TCEScrollToolbar.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  fItems.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called on TBX theme change
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.SpSkinChange(var Message: TMessage);
begin
  DrawBar;
end;

{*------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.Resize;
begin
  inherited;
  DrawBar;
end;

{*------------------------------------------------------------------------------
  Clear all items
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.Clear;
begin
  fItems.Clear;
  fOffset:= 0;
end;

{*------------------------------------------------------------------------------
  Add Item
-------------------------------------------------------------------------------}
function TCEScrollToolbar.AddItem(ItemClass: TCEScrollToolbarItemClass): TCEScrollToolbarItem;
begin
  Result:= ItemClass.Create;
  Items.Add(Result);
end;

{*------------------------------------------------------------------------------
  Insert Item
-------------------------------------------------------------------------------}
function TCEScrollToolbar.InsertItem(ItemClass: TCEScrollToolbarItemClass;
    Index: Integer): TCEScrollToolbarItem;
begin
  Result:= ItemClass.Create;
  Items.Insert(Index, Result);
end;

{*------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCEScrollToolbar.GetItem(Index: Integer): TCEScrollToolbarItem;
begin
  Result:= TCEScrollToolbarItem(fItems.Items[Index]);
end;

{*------------------------------------------------------------------------------
  Get item index from position
-------------------------------------------------------------------------------}
function TCEScrollToolbar.IndexByPos(X, Y: Integer): Integer;
var
  i,tmpX,tmpW: Integer;
begin
  Result:= -1;
  tmpX:= 0;
  for i:= 0 to fItems.Count - 1 do
  begin
    Inc(tmpX, fSeparatorSize);
    tmpW:= GetItem(i).GetWidth(Bitmap);
    if (X >= tmpX) and (X <= (tmpX+tmpW)) then
    begin
      Result:= i;
      Break;
    end;
    Inc(tmpX, tmpW);
  end;
end;

{*------------------------------------------------------------------------------
  Get item position rect
-------------------------------------------------------------------------------}
function TCEScrollToolbar.GetItemRect(Index: Integer): TRect;
var
  i: Integer;
begin
  Result.Left:= 0;
  for i:= 0 to fItems.Count - 1 do
  begin
    Inc(Result.Left,fSeparatorSize);
    Result.Right:= Result.Left + TCEScrollToolbarItem(fItems.Items[i]).GetWidth(Bitmap);
    if i = Index then
    begin
      Break;
    end
    else
    Result.Left:= Result.Right;
  end;
  Result.Top:= 0;
  Result.Bottom:= Bitmap.Height
end;

{*------------------------------------------------------------------------------
  Handle Mouse Leave event
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.MouseLeave;
begin
  inherited;
  SelectedIndex:= -1;
end;

{*------------------------------------------------------------------------------
  Mouse Move
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if fLeftArrowVisible and (x <= fArrowSize) then
  begin
    SelectedIndex:= -2;
  end
  else if fRightArrowVisible and (x >= (Bitmap.Width - fArrowSize)) then
  begin
    SelectedIndex:= -3;
  end
  else
  SelectedIndex:= IndexByPos((0 - fOffset) + X,Y);
end;

{*------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  item: TCEScrollToolbarItem;
  i: Integer;
  r: TRect;
begin
  inherited;
  fMouseDown:= true;
  if fLeftArrowVisible and (x <= fArrowSize) then
  begin
    if Button = mbLeft then
    begin
      DrawLeftArrow(itPushed);
      ClickLeftArrow;
    end;
  end
  else if fRightArrowVisible and (x >= (Bitmap.Width - fArrowSize)) then
  begin
    fMouseDown:= true;
    if Button = mbLeft then
    begin
      ClickRightArrow;
    end;
  end
  else
  begin
    i:= IndexByPos((0 - fOffset) + X,Y);
    if i > -1 then
    begin
      item:= TCEScrollToolbarItem(fItems.Items[i]);
      r:= GetItemRect(i);
      item.MouseDown(Button, Shift, (0 - fOffset) + X - r.Left, Y - r.Top);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Left arrow click
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.ClickLeftArrow;
var
  i: Integer;
  r: TRect;
begin
  fAnimTimer.Enabled:= false;
  i:= IndexByPos((0 - fOffset) + fArrowSize,0);
  if i > 0 then
  begin
    Dec(i,1);
    r:= GetItemRect(i);
    fDestOffset:= 0 - r.Left + fSeparatorSize;
    fAnimTimer.Enabled:= true;
  end
  else
  Offset:= 0;
end;

{*------------------------------------------------------------------------------
  Handle Right arrow click
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.ClickRightArrow;
var
  i: Integer;
  r: TRect;
begin
  i:= IndexByPos((0 - fOffset) + (Bitmap.Width - fArrowSize),0);
  if i > 0 then
  begin
    Inc(i,1);
    r:= GetItemRect(i);
    fDestOffset:= (Bitmap.Width - r.Right) - fSeparatorSize;
    fAnimTimer.Enabled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Do BackgroundClick
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DoBackgroundClick;
begin
  if Assigned(fOnBackgroundClick) then fOnBackgroundClick(Self);
end;

{*------------------------------------------------------------------------------
  Set Selected Index
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.SetSelectedIndex(const Value: Integer);
begin
  if fSelectedIndex <> Value then
  begin
    fSelectedIndex:= Value;
    DrawBar;
  end;
end;

{*------------------------------------------------------------------------------
  Set Button Offset
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.SetOffset(const Value: Integer);
begin
  if Value = fOffset then
  Exit;

  if Value > 0 then
  fOffset:= 0
  else
  fOffset:= Value;
  DrawBar;
end;

{*------------------------------------------------------------------------------
  Make Item Visible
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.MakeItemVisible(Index: Integer);
var
  r: TRect;
  i: Integer;
begin
  r:= GetItemRect(Index);
  if r.Right <= Bitmap.Width then
  begin
    if r.Left < -fOffset then
    fOffset:= -(r.Left - fSeparatorSize);
    Exit;
  end
  else
  begin
    i:= Bitmap.Width - r.Right;
    fOffset:= i;
  end;  
end;

{*------------------------------------------------------------------------------
  Handle scroll animation
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.OnAnimTimer(Sender: TObject);
begin
  fAnimTimer.Enabled:= false;

  if fDestOffset < fOffset then
  begin
    Dec(fOffset,10);
    if fDestOffset < fOffset then
    begin
      DrawBar;
      fAnimTimer.Enabled:= true;
    end
    else
    begin
      fOffset:= fDestOffset;
      DrawBar;
    end;
  end
  else if fDestOffset > fOffset then
  begin
    Inc(fOffset,10);
    if fDestOffset > fOffset then
    begin
      DrawBar;
      fAnimTimer.Enabled:= true;
    end
    else
    begin
      fOffset:= fDestOffset;
      DrawBar;
    end;
  end
end;

{*------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DrawBackground;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        Bitmap.Canvas.Brush.Color := clBtnFace;
        Bitmap.Canvas.FillRect(Bitmap.BoundsRect);
      end;
    sknWindows: begin
      //if Position in [dpLeft, dpRight] then Inc(ARect.Bottom, 1);  // Fix WindowsXP bug
      DrawThemeBackground(ThemeServices.Theme[teRebar], Bitmap.Canvas.Handle, 0, 0, Bitmap.BoundsRect, nil);
    end;
    sknSkin: begin
      CurrentSkin.PaintBackground(Bitmap.Canvas, Bitmap.BoundsRect, skncDock, sknsNormal, True, false);
      CurrentSkin.PaintBackground(Bitmap.Canvas, Bitmap.BoundsRect, skncToolbar, sknsNormal, True, False);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Draw Left Arrow
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DrawLeftArrow(ItemStyle: TItemStyle = itNormal);
var
  r: TRect;
  i: Integer;
  c: TColor;
  state: TSpTBXSkinStatesType;
begin
  // Background
  r.Left:= 0;
  r.Top:= 1;
  r.Right:= fArrowSize;
  r.Bottom:= Bitmap.Height-1;
  DrawMenuItem(Bitmap, r, false, ItemStyle, true);
  // Arrow
  state:= sknsNormal;
  case ItemStyle of
    itNormal: state:= sknsNormal;
    itSelected: state:= sknsHotTrack;
    itPushed: state:= sknsPushed;
    itChecked: state:= sknsChecked;
  end;
  i:= fArrowsize div 3;
  r.Left:= Round((fArrowSize - i) / 2);
  r.Top:= r.Top + Round((r.Bottom - r.Top) / 2);
  c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, state);
  SpDrawArrow(Bitmap.Canvas,r.Left,r.Top, c, false, true, i);
end;

{*------------------------------------------------------------------------------
  Draw Right Arrow
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DrawRightArrow(ItemStyle: TItemStyle = itNormal);
var
  r: TRect;
  i: Integer;
  c: TColor;
  state: TSpTBXSkinStatesType;
begin
  // Background
  r.Right:= Bitmap.Width;
  r.Left:= r.Right - fArrowSize;
  r.Top:= 1;
  r.Bottom:= Bitmap.Height-1;
  DrawMenuItem(Bitmap, r, false, ItemStyle, true);
  // Arrow
  state:= sknsNormal;
  case ItemStyle of
    itNormal: state:= sknsNormal;
    itSelected: state:= sknsHotTrack;
    itPushed: state:= sknsPushed;
    itChecked: state:= sknsChecked;
  end; 
  i:= fArrowsize div 3;
  r.Left:= r.Left + Round((fArrowSize - i) / 2);
  r.Top:= r.Top + Round((r.Bottom - r.Top) / 2);
  c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, state);
  SpDrawArrow(Bitmap.Canvas,r.Left,r.Top, c, false, false, i);
end;

{*------------------------------------------------------------------------------
  Draw Bar
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DrawBar;
var
  i: Integer;
  item: TCEScrollToolbarItem;
  r: TRect;
  it: TItemStyle;
begin
  if (Self.Width < 1) or (Self.Height < 1) then  // Nothing is visible so exit
  Exit;
  
  Bitmap.BeginUpdate;
  try
    // Init Buffer
    Bitmap.SetSize(Self.Width, Self.Height);
    r.Left:= fOffset;
    r.Top:= fSeparatorSize;
    r.Right:= fOffset;
    r.Bottom:= Bitmap.Height-fSeparatorSize;
    // Draw Background
    DrawBackground;
    // Loop items
    for i:= 0 to fItems.Count - 1 do
    begin
      item:= GetItem(i);
      // Separator
      r.Left:= r.Right;
      Inc(r.Right,fSeparatorSize);
      if (i > 0) and not item.Checked then
      begin
        if (not GetItem(i-1).Checked) and (fSelectedIndex <> i) and (fSelectedIndex <> i-1) then
        DrawSeparator(r);
      end;
      r.Left:= r.Right;

      // Draw item
      r.Right:= r.Left + item.GetWidth(Bitmap);
      if fSelectedIndex = i then
      it:= itSelected
      else
      it:= itNormal;
      item.DrawItem(Bitmap,r,it);
      r.Left:= r.Right;
    end;
    // Draw Left Arrow
    fLeftArrowVisible:= fOffset < 0;
    if fLeftArrowVisible then
    begin
      it:= itNormal;
      if fSelectedIndex = -2 then
      begin
        if fMouseDown then
        it:= itPushed
        else
        it:= itSelected;
      end;
      DrawLeftArrow(it);
    end;
    // Draw Right Arrow
    fRightArrowVisible:= r.Left > Bitmap.Width;
    if fRightArrowVisible then
    begin
      it:= itNormal;
      if fSelectedIndex = -3 then
      begin
        if fMouseDown then
        it:= itPushed
        else
        it:= itSelected;
      end;
      DrawRightArrow(it);
    end;
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

{*------------------------------------------------------------------------------
  Draw Separator
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.DrawSeparator(ARect: TRect);
begin
  if SkinManager.GetSkinType = sknNone then
  DrawEdge(Bitmap.Handle, ARect, EDGE_ETCHED, BF_LEFT)
  else
  SpDrawXPMenuSeparator(Bitmap.Canvas, ARect, true, true);
end;

{*------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
var
  item: TCEScrollToolbarItem;
  i: Integer;
  r: TRect;
begin
  inherited;
  fMouseDown:= false;

  if fLeftArrowVisible and (x <= fArrowSize) then
  begin
    if Button = mbRight then
    ClickLeftArrow;
  end
  else if fRightArrowVisible and (x >= (Bitmap.Width - fArrowSize)) then
  begin
    if Button = mbRight then
    ClickRightArrow;
  end
  else
  begin
    i:= IndexByPos((0 - fOffset) + X,Y);
    if i > -1 then
    begin
      item:= TCEScrollToolbarItem(fItems.Items[i]);
      r:= GetItemRect(i);
      item.MouseUp(Button, Shift, (0 - fOffset) + X - r.Left, Y - r.Top);
    end
    else if Button = mbLeft then
    DoBackgroundClick;
  end;
end;

{-------------------------------------------------------------------------------
  Paint
-------------------------------------------------------------------------------}
procedure TCEScrollToolbar.Paint;
begin
  if (Self.Width > 0) and (Self.Height > 0) then
  begin
    inherited;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Draw Item
-------------------------------------------------------------------------------}
procedure TCEScrollToolbarItem.DrawItem(Buffer: TBitmap32; ARect: TRect;
    ItemStyle: TItemStyle = itNormal);
var
  state: TSpTBXSkinStatesType;
begin
  state:= sknsNormal;
  case ItemStyle of
    itNormal: state:= sknsNormal;
    itSelected: state:= sknsHotTrack;
    itPushed: state:= sknsPushed;
    itChecked: state:= sknsChecked;
  end;
  if Checked then
  state:= sknsChecked;

  DrawMenuItem(Buffer, ARect, Checked, ItemStyle);

  Buffer.Font.Color:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, state);
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  Buffer.Textout(ARect,DT_CENTER+DT_VCENTER+DT_SINGLELINE,fCaption)
  else
  Buffer.TextoutW(ARect,DT_CENTER+DT_VCENTER+DT_SINGLELINE,fCaption);
end;

{*------------------------------------------------------------------------------
  Get item width
-------------------------------------------------------------------------------}
function TCEScrollToolbarItem.GetWidth(Buffer: TBitmap32): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  Result:= Buffer.TextWidth(fCaption) + 20
  else
  Result:= Buffer.TextWidthW(fCaption) + 20;
end;

{*------------------------------------------------------------------------------
  Set Caption
-------------------------------------------------------------------------------}
procedure TCEScrollToolbarItem.SetCaption(const Value: string);
begin
  fCaption:= Value;
end;

{*------------------------------------------------------------------------------
  Set Checked
-------------------------------------------------------------------------------}
procedure TCEScrollToolbarItem.SetChecked(const Value: Boolean);
begin
  fChecked := Value;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEScrollToolbarItem.MouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEScrollToolbarItem.MouseUp(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  // Do nothing
end;




end.
