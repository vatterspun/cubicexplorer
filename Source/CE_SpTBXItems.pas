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
//  The Original Code is CE_SpTBXItems.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_SpTBXItems;

interface

uses
  // SpTBX
  SpTBXControls, SpTBXSkins, SpTBXItem,
  // System Units
  Classes, Controls, Messages, Windows, CommCtrl, Graphics, ComCtrls, SysUtils;

{==============================================================================}
type
{-------------------------------------------------------------------------------
  TCETrackBar
-------------------------------------------------------------------------------}
  TCEPositionHintOrientation = (phoAbove, phoBelow);

  TCEShowHintQueryEvent = procedure(Sender: TObject; var AHint: String; var AShowHint: Boolean) of object;

  TCETrackBar = class(TSpTBXTrackBar)
  private
    FCanDrawChannelSelection: Boolean;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    fChanging: Boolean;
    fChannelSize: Integer;
    fPositionHintWindow: THintWindow;
    fOnAfterChange: TNotifyEvent;
    fOnShowHintQueryEvent: TCEShowHintQueryEvent;
    fPositionHintOrientation: TCEPositionHintOrientation;
    fShowFocusRect: Boolean;
    fShowPositionHint: Boolean;
    procedure Changed; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShowHint; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure SetChannelSize(const Value: Integer); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetShowFocusRect(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function ChannelRect: TRect; reintroduce; virtual;
    function MouseInThumb: Boolean; reintroduce; virtual;
    property Changing: Boolean read fChanging;
    property PositionHintWindow: THintWindow read fPositionHintWindow;
  published
    property ChannelSize: Integer read fChannelSize write SetChannelSize default -1;
    property PositionHintOrientation: TCEPositionHintOrientation read
        fPositionHintOrientation write fPositionHintOrientation;
    property ShowFocusRect: Boolean read fShowFocusRect write SetShowFocusRect
        default true;
    property ShowPositionHint: Boolean read fShowPositionHint write
        fShowPositionHint;
    property OnAfterChange: TNotifyEvent read fOnAfterChange write fOnAfterChange;
    property OnShowHintQueryEvent: TCEShowHintQueryEvent read fOnShowHintQueryEvent
        write fOnShowHintQueryEvent;
  end;

{==============================================================================}
implementation

uses
  Math;

{##############################################################################}
// TCETrackBar

{-------------------------------------------------------------------------------
  Create an instance of TCETrackBar
-------------------------------------------------------------------------------}
constructor TCETrackBar.Create(AOwner: TComponent);
begin
  inherited;
  fShowFocusRect:= true;
  fChannelSize:= -1;
  fPositionHintWindow:= THintWindow.Create(Self);
  fPositionHintWindow.Color:= clWindow;
  fPositionHintWindow.Parent:= Self;
  fPositionHintOrientation:= phoAbove;
  fShowPositionHint:= false;
end;

{-------------------------------------------------------------------------------
  Changed
-------------------------------------------------------------------------------}
procedure TCETrackBar.Changed;
begin
  if fShowPositionHint and fChanging then
  DoShowHint;
  inherited;
end;

{-------------------------------------------------------------------------------
  ChannelRect
-------------------------------------------------------------------------------}
function TCETrackBar.ChannelRect: TRect;
var
  R: TRect;
begin
  // TBM_GETCHANNELRECT allways returns the horizontal channel rect, even
  // when the Orientation is vertical.
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, LPARAM(@Result));
  if Orientation = trVertical then
  begin
    R:= Result;
    Result:= Rect(R.Top, R.Left, R.Bottom, R.Right);
    if ChannelSize > -1 then
    begin
      Result.Left:= Round((Result.Left + ((Result.Right - Result.Left) / 2)) - (ChannelSize / 2));
      Result.Right:= Result.Left + ChannelSize;
    end;
  end
  else
  begin
    if ChannelSize > -1 then
    begin
      Result.Top:= Round((Result.Top + ((Result.Bottom - Result.Top) / 2)) - (ChannelSize / 2));
      Result.Bottom:= Result.Top + ChannelSize;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle CNNotify message (paint here)
-------------------------------------------------------------------------------}
procedure TCETrackBar.CNNotify(var Message: TWMNotify);
var
  Info: PNMCustomDraw;
  ACanvas: TCanvas;
  R: TRect;
  Rgn: HRGN;
  Offset: Integer;
begin
  if Message.NMHdr.code = NM_CUSTOMDRAW then
  begin
    Message.Result:= CDRF_DODEFAULT;
    Info := Pointer(Message.NMHdr);
    case Info.dwDrawStage of
      CDDS_PREPAINT:
        Message.Result := CDRF_NOTIFYITEMDRAW;
      CDDS_ITEMPREPAINT:
        begin
          ACanvas:= TCanvas.Create;
          ACanvas.Lock;
          try
            ACanvas.Handle := Info.hdc;
            case Info.dwItemSpec of
              TBCD_TICS:
                begin
                  R:= ClientRect;
                  SpDrawParentBackground(Self, ACanvas.Handle, R);
                  
                  if Focused and fShowFocusRect then
                  SpDrawFocusRect(ACanvas, R);

                  if TickMarks <> tmxCenter then
                  DrawTicks(ACanvas);

                  Message.Result:= CDRF_SKIPDEFAULT;
                end;
              TBCD_THUMB:
                begin
                  if SliderVisible then
                  begin
                    SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
                    if DoDrawThumb(ACanvas, R, pstPrePaint) then
                    SpDrawXPTrackBar(ACanvas, R, TBCD_THUMB, Orientation = trVertical, MouseInThumb, False, TickMarks, Min, Max, SelStart, SelEnd, SkinType);

                    DoDrawThumb(ACanvas, R, pstPostPaint);
                    Message.Result:= CDRF_SKIPDEFAULT;
                  end;
                end;
              TBCD_CHANNEL:
                begin
                  SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
                  Offset:= 0;
                  if Focused then
                  Inc(Offset);
                  
                  if Orientation = trHorizontal then
                  begin
                    R.Left:= ClientRect.Left + Offset;
                    R.Right:= ClientRect.Right - Offset;
                  end
                  else
                  begin
                    R.Top:= ClientRect.Top + Offset;
                    R.Bottom:= ClientRect.Bottom - Offset;
                  end;

                  Rgn:= CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
                  SelectClipRgn(ACanvas.Handle, Rgn);
                  try
                    SpDrawParentBackground(Self, ACanvas.Handle, ClientRect);
                    R:= ChannelRect;

                    if DoDrawChannel(ACanvas, R, pstPrePaint) then
                      SpDrawXPTrackBar(ACanvas, R, TBCD_CHANNEL, Orientation = trVertical, False, FCanDrawChannelSelection, TickMarks, Min, Max, SelStart, SelEnd, SkinType);
                    DoDrawChannel(ACanvas, R, pstPostPaint);

                    // Draw channel tics
                    if TickMarks = tmxCenter then
                      DrawTicks(ACanvas);
                  finally
                    DeleteObject(Rgn);
                    SelectClipRgn(ACanvas.Handle, 0);
                  end;
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
            end;
          finally
            ACanvas.Unlock;
            ACanvas.Handle := 0;
            ACanvas.Free;
          end;
        end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCETrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FCanDrawChannelSelection:= (Params.Style and TBS_ENABLESELRANGE) <> 0;
end;

{-------------------------------------------------------------------------------
  Do ShowHint
-------------------------------------------------------------------------------}
procedure TCETrackBar.DoShowHint;
var
  r,r2: TRect;
  s: String;
  b: Boolean;
begin
  s:= IntToStr(Self.Position);
  b:= fShowPositionHint;
  
  if assigned(fOnShowHintQueryEvent) then
  fOnShowHintQueryEvent(Self, s, b);
  
  if b then
  begin
    // get hint rect
    r:= fPositionHintWindow.CalcHintRect(200, s, nil);
    // get thumb rect
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@r2));
    r2.TopLeft:= Self.ClientToScreen(r2.TopLeft);
    r2.BottomRight:= Self.ClientToScreen(r2.BottomRight);
    // calculate hint rect
    r.Left:= r2.Left + Round( ((r2.Right-r2.Left) / 2) - (r.Right / 2) );
    if fPositionHintOrientation = phoAbove then
    r.Top:= r2.Top - r.Bottom - 5
    else
    r.Top:= r2.Bottom + 1;

    // make sure the hint is positioned between the trackbar channel
    SendMessage(Handle, TBM_GETCHANNELRECT, 0, LPARAM(@r2));
    r2.TopLeft:= Self.ClientToScreen(r2.TopLeft);
    r2.BottomRight:= Self.ClientToScreen(r2.BottomRight);
    r.Left:= Math.Min(r2.Right - r.Right,
                      Math.Max(r.Left, r2.Left));

    r.Right:= r.Left + r.Right;
    r.Bottom:= r.Top + r.Bottom;
    // show hint
    fPositionHintWindow.ActivateHint(r, s);
  end
  else
  fPositionHintWindow.ReleaseHandle;
end;

{-------------------------------------------------------------------------------
  MouseDown
-------------------------------------------------------------------------------}
procedure TCETrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  fChanging:= Shift = [ssLeft];
  if fShowPositionHint and fChanging then
  DoShowHint;
  inherited;
end;

{-------------------------------------------------------------------------------
  MouseInThumb
-------------------------------------------------------------------------------}
function TCETrackBar.MouseInThumb: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then
  Result:= False
  else
  begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
    GetCursorPos(P);
    P:= ScreenToClient(P);
    Result:= PtInRect(R, P)
  end;
end;

{-------------------------------------------------------------------------------
  MouseUp
-------------------------------------------------------------------------------}
procedure TCETrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  inherited;
  if fChanging then
  begin
    fChanging:= false;

    // hide hint
    fPositionHintWindow.ReleaseHandle;
    fPositionHintWindow.Hide;

    if assigned(fOnAfterChange) then
    fOnAfterChange(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Set ChannelSize
-------------------------------------------------------------------------------}
procedure TCETrackBar.SetChannelSize(const Value: Integer);
begin
  if fChannelSize <> Value then
  begin
    fChannelSize:= Value;
    Self.InvalidateBackground;
  end;
end;

{-------------------------------------------------------------------------------
  Set Parent
-------------------------------------------------------------------------------}
procedure TCETrackBar.SetParent(AParent: TWinControl);
begin
  inherited;
  SetShowFocusRect(fShowFocusRect);
end;

{-------------------------------------------------------------------------------
  Set ShowFocusRect
-------------------------------------------------------------------------------}
procedure TCETrackBar.SetShowFocusRect(const Value: Boolean);
begin
  fShowFocusRect:= Value;
  if assigned(Self.Parent) then
  begin
    if fShowFocusRect then
    SendMessage(Self.Handle, WM_CHANGEUISTATE, MakeWParam(UIS_CLEAR, UISF_HIDEFOCUS), 0)
    else
    SendMessage(Self.Handle, WM_CHANGEUISTATE, MakeWParam(UIS_SET, UISF_HIDEFOCUS), 0);
  end;
end;

end.
