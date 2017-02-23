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
//  The Original Code is CEJvDockVSNetStyleTBX.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

{-----------------------------------------------------------------------------
 Unit Name: TJvDockVSNETTBX
 Author:    Kiriakos Vlahos
 Date:      01-Jul-2005
 Purpose:   TBX themed TJvDockVSNETTree docking style for JvDocking
            Donated to JvTBXLib (mxs.bergsoft.net)
 History:
-----------------------------------------------------------------------------}

unit CEJvDockVSNetStyleTBX;

{$I jvcl.inc}

interface

uses
  // SpTBX
  SpTBXItem, SpTBXSkins, SpTBXDkPanels, SpTBXTabs,
  // System Units
  Forms, StrUtils, Messages, Windows, SysUtils, Controls, Classes, Graphics,
  ComCtrls, CommCtrl, ExtCtrls, UxTheme, Themes,
  // JVCL
  JvDockTree, JvDockControlForm, JvDockSupportControl, JvDockVSNetStyle;

Type
  TJvDockVSNETTreeTBX = class(TJvDockVSNETTree)
  protected
    procedure CustomLoadZone(Stream: TStream; var Zone: TJvDockZone); override;
    procedure CustomSaveZone(Stream: TStream; Zone: TJvDockZone); override;
    procedure TBMThemeChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure DrawDockGrabber(Control: TWinControl; const ARect: TRect); override;
    procedure DrawAutoHideButton(Zone: TJvDockZone;
      Left, Top: Integer); override;
    procedure DrawCloseButton(Canvas: TCanvas;
     Zone: TJvDockZone; Left, Top: Integer); override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    procedure HideControl(Control: TControl); override;
    //procedure GetCaptionRect(var Rect: TRect); override;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass;
      ADockStyle: TJvDockObservableStyle); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabPanelTBX = class(TJvDockVSNETTabPanel)
  private
    //ii: TTBXItemInfo;
    // unfortunately the parent class does not expose this
    // so I had to replicate the methods CMMouseLeave and
    // MouseMove
    FSelectHotIndex: Integer;
    FPaintBackground : Boolean;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure TBMThemeChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property SelectedHotIndex : integer read FSelectHotIndex;
    property PaintBackground : Boolean read FPaintBackground write FPaintBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabPageControlTBX = class(TJvDockVSNETTabPageControl)
  protected
    procedure CreatePanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnSheetShow(Sender: TObject);
    procedure OnSheetHide(Sender: TObject);
  end;

  TJvDockVSNETPanelTBX = class(TJvDockVSNETPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSChannelTBX = class(TJvDockVSChannel)
  private
    fAnimTimer: TTimer;
    FDelayPane: TJvDockVSPane;
    FPaintBackground : Boolean;
    procedure DoAnimationDelay(Sender: TObject);
    function PaneAtPos(MousePos: TPoint): TJvDockVSPane;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // KV move GetBlockRect to protected
    procedure MyGetBlockRect(Block: TJvDockVSBlock; Index: Integer; var ARect:
        TRect);
    procedure TBMThemeChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure Paint; override;
    procedure PopupPaneChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PaintBackground : Boolean read FPaintBackground write FPaintBackground;
  end;

  TMouseLoc = (mlNone, mlOver, mlDown);

  TJvDockVSNETSplitterTBX = class(TJvDockVSNETSplitter)
  private
   FMouseLoc: TMouseLoc;
   FMouseOver: boolean;
   FMouseDown: boolean;
  protected
    procedure TBMThemeChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  end;

  TJvDockVSNetStyleTBX = class(TJvDockVSNetStyle)
  protected
    procedure BeginPopup(AChannel: TJvDockVSChannel); override;
    procedure EndPopup(AChannel: TJvDockVSChannel); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEJvDockVSNETTabSheet = class(TJvDockVSNETTabSheet)
  
  protected
    procedure SetPageControl(APageControl: TJvDockPageControl); override;
  end;


implementation

Uses                                        // TODO: Fix this
  JvDockVIDStyle;//, fCE_DockableForm;


{fduenas: begin}
//Trucate a text and add ellipsis depending on the rendered Canvas's font
function TruncateText( _str: string; _canvas: TCanvas;
 _maxwidth: integer; _ellipsis: boolean=false): string;
var _suffix: String;
begin
 result := _str;
 if (not (_maxwidth>0)) or (_str='') then
    exit;

 if _ellipsis then
    _suffix := '...'
 else
    _suffix := '';

 while _canvas.TextExtent(result).cx > _maxwidth do
 begin
  _str := LeftStr(_str, length(_str)-1);
  result := _str+_suffix;
 end;

end;
{fduenas: end}

//procedure SetItemState(State: string; var AState: TSpTBXSkinStatesType);
//begin
//  if State = 'normal' then
//  begin
//    AState:= sknsNormal;
//  end;
//  if State = 'hot' then
//  begin
//    AState:= sknsHotTrack;
//  end;
//  if State = 'down' then
//  begin
//    AState:= sknsPushed;
//  end;
//  if State = 'checked' then
//  begin
//    AState:= sknsChecked;
//  end;
//  if State = 'checked-hot' then
//  begin
//    AState:= sknsCheckedAndHotTrack;
//  end;
//  if State = 'checked-down' then
//  begin
//    AState:= sknsCheckedAndHotTrack;
//  end;
//end;

procedure PaintItem(Canvas: TCanvas; R: TRect; state: string; Orientation: TTabPosition);
var
 Bmp: TBitmap;
 SourceR, DestR: TRect;
 AState: TSpTBXSkinStatesType;
 Checked, HotTracked, Pushed: Boolean;
 Flags: Cardinal;
 SkinType: TSpTBXSkinType;
begin
  Bmp:= TBitmap.Create;
  try
    Bmp.PixelFormat:= pf32Bit;
    Bmp.Width:= R.Right - R.Left;
    Bmp.Height:= R.Bottom - R.Top + 2;
    Bmp.Transparent:= true;
    SourceR:= Rect(0, 0, Bmp.Width, Bmp.Height);

    SkinType:= SkinManager.GetSkinType;

    DestR:= R;
    
    Checked:= (State = 'checked') or (State = 'checked-hot');
    HotTracked:= (State = 'hot') or (State = 'checked-hot');
    Pushed:= (State = 'down') or (State = 'checked-down');

    case SkinType of
      sknNone:
        if Checked then
        begin
          //Orientation:= tpTop;  // Don't need to flip
          Bmp.Canvas.Brush.Color:= clBtnFace;
          Bmp.Canvas.FillRect(SourceR);
          ExtCtrls.Frame3D(Bmp.Canvas, SourceR, clWindow, clBtnShadow, 1);
          //ExtCtrls.Frame3D(Bmp.Canvas, SourceR, Bmp.Canvas.Brush.Color, clBtnShadow, 1);
          SourceR:= Rect(0, 0, Bmp.Width, Bmp.Height);  // Frame3D changed R
        end;
      sknWindows:
        begin
          Flags := TIS_NORMAL;
          if Checked then
          Flags:= TIS_SELECTED
          else
          if HotTracked then
          Flags:= TIS_HOT;
          DrawThemeBackground(ThemeServices.Theme[teTab], Bmp.Canvas.Handle, TABP_TABITEM, Flags, SourceR, nil);
        end;
      sknSkin:
        begin
            // Flip top to bottom
          if Orientation = tpBottom then
          begin
            DestR.Top:= R.Bottom-1;
            DestR.Bottom:= R.Top-1;
          end;
          AState:= CurrentSkin.GetState(True, Pushed, HotTracked, Checked);
          Bmp.Canvas.CopyRect(SourceR, Canvas, DestR);
          CurrentSkin.PaintBackground(Bmp.Canvas, SourceR, skncTab, AState, True, True);
        end;
    end;

    DestR:= R;
    if (SkinType = sknNone) and (Orientation = tpBottom) then
    begin
      if Checked then
      begin
        SourceR.Top:= SourceR.Top + 2;
      end
      else
      begin
        SourceR.Top:= SourceR.Top + 3;
        DestR.Bottom:= DestR.Bottom - 1;
      end;
    end
    else
    begin
      if Checked then
      begin
        SourceR.Bottom:= SourceR.Bottom - 2;
      end
      else
      begin
        SourceR.Bottom:= SourceR.Bottom - 3;
        DestR.Bottom:= DestR.Bottom - 1;
      end;
      // Flip top to bottom
      if Orientation = tpBottom then
      begin
        SourceR.Top:= SourceR.Bottom-1;
        SourceR.Bottom:= -1;
      end;
    end;




    Canvas.CopyRect(DestR, Bmp.Canvas, SourceR);

  finally
    Bmp.Free;
  end;
end;

procedure SetBtnItemInfo(var IInfo: TSpTBXMenuItemInfo; BtnState: TJvDockBtnState);
begin
  FillChar(IInfo, SizeOf(TSpTBXMenuItemInfo), 0);
  IInfo.ToolbarStyle:= true;
  IInfo.IsVertical := False;
  IInfo.Enabled := true;
  IInfo.SkinType:= SkinManager.GetSkinType;

   case BtnState of
     bsNormal :
      begin
       IInfo.State:= sknsNormal;
      end;
     bsUp :
      begin
        IInfo.State:= sknsHotTrack;
      end;
     bsDown :
      begin
        IInfo.State:= sknsPushed;
      end;
   end;
end;

{ TJvDockVSNETTreeTBX }

constructor TJvDockVSNETTreeTBX.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, DockZoneClass, ADockStyle);;
  GrabberSize := 22;
  ButtonHeight := 15;
  ButtonWidth := 15; //15
  LeftOffset := 0; //0
  RightOffset := 3;
  TopOffset := 2;
  BottomOffset := 2;
  ButtonSplitter := 0;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;
  GrabberBottomEdgeColor := clNone;
  SkinManager.AddSkinNotification(Self);
end;

Type
  TCrackJvDockVSNETZone = class(TJvDockVSNETZone)
  end;

  TCrackJvDockVSBlock = class(TJvDockVSBlock)
  end;

destructor TJvDockVSNETTreeTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETTreeTBX.CustomLoadZone(Stream: TStream; var Zone:
    TJvDockZone);
begin
  inherited;
end;

procedure TJvDockVSNETTreeTBX.CustomSaveZone(Stream: TStream; Zone:
    TJvDockZone);
begin
  inherited;
end;

procedure TJvDockVSNETTreeTBX.DrawAutoHideButton(Zone: TJvDockZone; Left,
  Top: Integer);
var
  AZone: TCrackJvDockVSNETZone;
  ADockClient: TJvDockClient;
  R : TRect;
  ii: TSpTBXMenuItemInfo;
  c: TColor;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Left := Left + ButtonWidth; // move the auto hide button to the Close Button's location

    AZone := TCrackJvDockVSNETZone(Zone);
    R := Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight);

    SetBtnItemInfo(ii, AZone.AutoHideBtnState);
    SpDrawXPMenuItem(Canvas, R, ii);

    c:= SkinManager.CurrentSkin.GetTextColor(skncDockablePanelTitleBar, ii.State);
    if c = clNone then
    c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, ii.State);
    Canvas.Pen.Color:= c;
    
    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
    begin
      Dec(Left, 2);
      Inc(Top, 1);
      Canvas.MoveTo(Left + 9, Top + 10);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.MoveTo(Left + 6, Top + 7);
      Canvas.LineTo(Left + 13, Top + 7);
      Canvas.MoveTo(Left + 7, Top + 6);
      Canvas.LineTo(Left + 7, Top + 2);
      Canvas.LineTo(Left + 10, Top + 2);
      Canvas.LineTo(Left + 10, Top + 6);
      Canvas.LineTo(Left + 11, Top + 6);
      Canvas.LineTo(Left + 11, Top + 1);
    end
    else
    if DockSite.Align in [alNone] then
    begin
      Dec(Left, 2);
      Inc(Top, 1);
      Canvas.MoveTo(Left + 5, Top + 6);
      Canvas.LineTo(Left + 8, Top + 6);
      Canvas.MoveTo(Left + 8, Top + 3);
      Canvas.LineTo(Left + 8, Top + 10);
      Canvas.MoveTo(Left + 9, Top + 4);
      Canvas.LineTo(Left + 12, Top + 4);
      Canvas.LineTo(Left + 12, Top + 7);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.LineTo(Left + 9, Top + 8);
      Canvas.LineTo(Left + 13, Top + 8);
    end;
  end;
end;

procedure TJvDockVSNETTreeTBX.DrawCloseButton(Canvas: TCanvas;
  Zone: TJvDockZone; Left, Top: Integer);
var
  DrawRect: TRect;
  AZone: TCrackJvDockVSNETZone;
  ADockClient: TJvDockClient;
  AForm: TForm;
  R : TRect;
  ii: TSpTBXMenuItemInfo;
  c: TColor;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Exit;
    if Zone.ChildControl is TJvDockTabHostForm then
    begin
      AForm := TJvDockTabHostForm(Zone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
          Exit;
      end;
    end;
    AZone := TCrackJvDockVSNETZone(Zone);

    R := Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight);

    SetBtnItemInfo(ii, AZone.CloseBtnState);
    SpDrawXPMenuItem(Canvas, R, ii);

    c:= SkinManager.CurrentSkin.GetTextColor(skncDockablePanelTitleBar, ii.State);
    if c = clNone then
    c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, ii.State);
    Canvas.Pen.Color:= c;

    DrawRect.Left := Left + 4;
    DrawRect.Right := DrawRect.Left + 7;
    DrawRect.Top := Top + 4;
    DrawRect.Bottom := DrawRect.Top + 7;

    Canvas.MoveTo(DrawRect.Left, DrawRect.Top);
    Canvas.LineTo(DrawRect.Right, DrawRect.Bottom);

    Canvas.MoveTo(DrawRect.Left+1, DrawRect.Top);
    Canvas.LineTo(DrawRect.Right+1, DrawRect.Bottom);

    Canvas.MoveTo(DrawRect.Right - 1, DrawRect.Top);
    Canvas.LineTo(DrawRect.Left - 1, DrawRect.Bottom);
    Canvas.MoveTo(DrawRect.Right, DrawRect.Top);
    Canvas.LineTo(DrawRect.Left, DrawRect.Bottom);
  end;
end;

procedure TJvDockVSNETTreeTBX.DrawDockGrabber(Control: TWinControl; const
    ARect: TRect);
var
  Option: TJvDockVIDConjoinServerOption;
  DrawRect: TRect;
  CRect: TRect;
  ShowCloseButtonOnGrabber: Boolean;
  {$IFDEF JVCL_DOCKING_NOTIFYLISTENERS}
  TabServerOption:TJvDockVIDTabServerOption;
  {$ENDIF JVCL_DOCKING_NOTIFYLISTENERS}
  //dpi: TTBXDockPanelInfo;
  c: TColor;
begin
  Assert(Assigned(Control));
  ShowCloseButtonOnGrabber := True;

  {$IFDEF JVCL_DOCKING_NOTIFYLISTENERS}
  if Assigned(DockStyle) then
   // Still not extremely easy, but a lot less evil than the above.
   if Control is TJvDockTabHostForm then
   begin
     TabServerOption := TJvDockVIDTabServerOption(TJvDockBasicStyle(DockStyle).TabServerOption);
     ShowCloseButtonOnGrabber := TabServerOption.ShowCloseButtonOnGrabber;
   end;
  {$ELSE}
  { The old way }
  if Assigned(DockSite) and (DockSite is TJvDockPanel) then
  begin
    if Control is TJvDockTabHostForm then
    begin
      ShowCloseButtonOnGrabber := ((DockSite as TJvDockPanel).DockServer  {I'm begging for access violations! }
        .DockStyle.TabServerOption as TJvDockVIDTabServerOption).ShowCloseButtonOnGrabber;
    end;
  end;
  {$ENDIF JVCL_DOCKING_NOTIFYLISTENERS}

  // Defaults should work if DockStyle is not set!
  // -> Assert(DontAssume(Assigned(DockStyle)));

  with ARect do
  begin
    if GrabbersPosition = gpTop then
    begin
      if DockSite is TJvDockPanel then
        Option := TJvDockVIDConjoinServerOption(TJvDockPanel(DockSite).DockServer.DockStyle.ConjoinServerOption)
      else
      if DockSite is TJvDockConjoinPanel then
        Option :=
          TJvDockVIDConjoinServerOption(TJvDockConjoinHostForm(TJvDockConjoinPanel(DockSite).ParentForm).DockClient.DockStyle.ConjoinServerOption)
      else
        Option := nil;


      DrawRect:= ARect;
      Dec(DrawRect.Left,2);
      Dec(DrawRect.Top, 2);
      Inc(DrawRect.Right,4);
      DrawRect.Bottom := DrawRect.Top + GrabberSize+2;

      if (DrawRect.Bottom <= DrawRect.Top) or (DrawRect.Right <= DrawRect.Left) then
      Exit;
      {fduenas: begin}


      Canvas.Font.Assign(SmCaptionFont);

      //GetCaptionRect( CRect );

      CRect:= DrawRect;
      Inc(CRect.Top,1);
      Dec(CRect.Right, 40);
      Inc(CRect.Left, 4);
//      PanelCaption := TruncateText(TForm(Control).Caption, Canvas,
//                      (CRect.Right-CRect.Left), true);

      {fduenas: end}

      if Option <> nil then
      begin
        SpDrawXPDockablePanelTitleBar(Canvas,DrawRect,false,false);

        c:= SkinManager.CurrentSkin.GetTextColor(skncDockablePanelTitleBar, sknsNormal);
        if c = clNone then
        c:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
        Canvas.Font.Color:= c;
        SpDrawXPText(Canvas,
                     UTF8Decode(TForm(Control).Caption),
                     CRect,
                     DT_SINGLELINE+DT_VCENTER+DT_END_ELLIPSIS);

        Canvas.Brush.Style:= bsClear;

        if ShowCloseButtonOnGrabber then
        DrawCloseButton(Canvas, FindControlZone(Control), Right - RightOffset - ButtonWidth - 1, Top + TopOffset);
      end
      else
      begin
        {$IFDEF JVDOCK_DEBUG}
        OutputDebugString('Not supported GrabbersPosition');
        {$ENDIF JVDOCK_DEBUG}
      end;
    end;
  end;
  

  if DockSite.Align <> alClient then
  begin
    DrawAutoHideButton(FindControlZone(Control),
      ARect.Right - RightOffset - 2 * ButtonWidth - ButtonSplitter,
      ARect.Top + TopOffset)
  end;
end;

procedure TJvDockVSNETTreeTBX.DrawSplitterRect(const ARect: TRect);
var
  IsVertical: Boolean;
  state: TSpTBXSkinStatesType;
begin
  IsVertical:= (ARect.Right - ARect.Left) < (ARect.Bottom - ARect.Top);
  state:= sknsNormal;
  CurrentSkin.PaintBackground(Canvas, ARect, skncSplitter, state, True, False, IsVertical);
end;

procedure TJvDockVSNETTreeTBX.HideControl(Control: TControl);
var
  DC: TJvDockClient;
  form: TForm;
begin
  inherited HideControl(Control);
  if assigned(Control) then
  begin
    if Control is TJvDockTabHostForm then
    begin
      form:= TJvDockTabHostForm(Control).GetActiveDockForm;
      if assigned(form) then
      begin
        DC:= FindDockClient(form);
        if assigned(DC) then
        DC.MakeHideEvent;
      end;
    end
    else
    begin
      DC:= FindDockClient(Control);
      if assigned(DC) then
      DC.MakeHideEvent;
    end;
  end;
end;

procedure TJvDockVSNETTreeTBX.TBMThemeChange(var Message: TMessage);
begin
//  if Message.WParam = TSC_VIEWCHANGE then
//  begin
//
//  end;
    UpdateAll;
    DockSite.Invalidate;
end;

{ TJvDockVSNetStyleTBX }

constructor TJvDockVSNetStyleTBX.Create(AOwner: TComponent);
begin
  inherited;
  ConjoinPanelTreeClass := TJvDockVSNETTreeTBX;
  DockPanelTreeClass := TJvDockVSNETTreeTBX;
  TabDockClass := TJvDockVSNETTabPageControlTBX;
  DockPanelClass := TJvDockVSNETPanelTBX;
  DockSplitterClass := TJvDockVSNETSplitterTBX;
  SetAnimationInterval(10);
end;

procedure TJvDockVSNetStyleTBX.BeginPopup(AChannel: TJvDockVSChannel);
var
  DC: TJvDockClient;
begin
  ChannelOption.MouseleaveHide:= false;
  inherited BeginPopup(AChannel);
  if assigned(AChannel.PopupPane) then
  begin
    DC:= FindDockClient(AChannel.PopupPane.DockForm);
    if assigned(DC) then
    DC.MakeShowEvent;
  end;
end;

procedure TJvDockVSNetStyleTBX.EndPopup(AChannel: TJvDockVSChannel);
var
  DC: TJvDockClient;
begin
  inherited EndPopup(AChannel);
  if assigned(AChannel.PopupPane) then
  begin
    DC:= FindDockClient(AChannel.PopupPane.DockForm);
    if assigned(DC) then
    DC.MakeHideEvent;
  end;
end;


{ TJvDockVSNETTabPageControlTBX }

constructor TJvDockVSNETTabPageControlTBX.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  TabPanelClass:= TJvDockVSNETTabPanelTBX;
  TabSheetClass:= TCEJvDockVSNETTabSheet;
  Self.TabStop:= false;
end;

procedure TJvDockVSNETTabPageControlTBX.CreatePanel;
begin
  inherited;
  TJvDockVSNETTabPanelTBX(Panel).TabBottomOffset := 1; // changed from 3 to match Tabbar
  TJvDockVSNETTabPanelTBX(Panel).TabHeight := 24;
  TJvDockVSNETTabPanelTBX(Panel).PaintBackground := True;
end;

procedure TJvDockVSNETTabPageControlTBX.OnSheetShow(Sender: TObject);
var
  i: integer;
  child: TControl;
  page: TJvDockTabSheet;
begin
  if Sender is TJvDockTabSheet then
  page:= TJvDockTabSheet(Sender)
  else
  Exit;

  for i:= 0 to Page.ControlCount-1 do
  begin
    child:= Page.Controls[i];
    if child is TForm then
    begin
      if assigned(TForm(child).OnShow) then
      TForm(child).OnShow(Sender);
    end;
  end;
end;

procedure TJvDockVSNETTabPageControlTBX.OnSheetHide(Sender: TObject);
var
  i: integer;
  child: TControl;
  page: TJvDockTabSheet;
begin
  if Sender is TJvDockTabSheet then
  page:= TJvDockTabSheet(Sender)
  else
  Exit;

  for i:= 0 to Page.ControlCount-1 do
  begin
    child:= Page.Controls[i];
    if child is TForm then
    begin
      if assigned(TForm(child).OnHide) then
      TForm(child).OnHide(Sender);
    end;
  end;
end;

{ TJvDockVSNETTabPanelTBX }

Type
  TCrackJvDockVIDTabSheet = class(TJvDockVIDTabSheet)
  end;

procedure TJvDockVSNETTabPanelTBX.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FSelectHotIndex <> -1 then
  begin
    FSelectHotIndex := -1;
    Invalidate;
  end;
end;

constructor TJvDockVSNETTabPanelTBX.Create(AOwner: TComponent);
begin
  inherited;
  FSelectHotIndex := -1;
  CaptionTopOffset := 3;
  SkinManager.AddSkinNotification(Self);
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;  // To avoid flicker
end;

destructor TJvDockVSNETTabPanelTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETTabPanelTBX.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  Index : integer;
begin
  inherited;
  Index := GetPageIndexFromMousePos(X, Y);
  if Page.HotTrack and (Index <> FSelectHotIndex) then
  begin
    FSelectHotIndex := Index;
    Invalidate;
  end;
end;

procedure TJvDockVSNETTabPanelTBX.Paint;

  procedure PaintSeparator(Canvas: TCanvas; R: TRect);
  begin
    SpDrawXPMenuSeparator(Canvas, R, false, true);
  end;

var
  ARect: TRect;
  CurrTabWidth: Integer;
  I, CompleteWidth: Integer;
  ImageWidth: Integer;
  CaptionString: WideString;
begin
  //inherited Paint;
  if Page = nil then
    Exit;

  Page.ShowTabImages:= false;  

  if (Page.Images <> nil) and (Page.ShowTabImages) then
    ImageWidth := Page.Images.Width
  else
    ImageWidth := 0;

  // Draw background
  ARect := Rect(0, 0, PanelWidth, PanelHeight);

  case SkinManager.GetSkinType of
    sknNone: begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(ARect);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(ARect.Left,ARect.Top+1);
      Canvas.LineTo(ARect.Right,ARect.Top+1);
    end;
    sknWindows: begin
      DrawThemeBackground(ThemeServices.Theme[teRebar], Canvas.Handle, 0, 0, ARect, nil);
      DrawThemeBackground(ThemeServices.Theme[teTab], Canvas.Handle, TABP_PANE, 0, ARect, nil);
    end;
    sknSkin: begin
      CurrentSkin.PaintBackground(Canvas, ARect, skncDock, sknsNormal, True, False);
      CurrentSkin.PaintBackground(Canvas, ARect, skncToolbar, sknsNormal, True, False);
      ARect.Bottom:= ARect.Top+2;
      SpDrawXPTabControlBackground(Canvas, ARect, clBtnFace, true, sknSkin);
    end;
  end;

  CompleteWidth := 0;

  // Paint all the tabs
  for I := 0 to Page.Count - 1 do
  begin
    if not Page.Pages[I].TabVisible then
      Continue;

    CurrTabWidth := TCrackJvDockVIDTabSheet(Page.Pages[I]).ShowTabWidth;

    //  Calculate Button area
    case Page.TabPosition of
      tpLeft:
        ARect := Rect(TabTopOffset, CompleteWidth + TabLeftOffset,
          PanelHeight, CompleteWidth + TabLeftOffset + CurrTabWidth);
      tpRight:
        ARect := Rect(TabBottomOffset, CompleteWidth + TabLeftOffset,
          PanelHeight - TabTopOffset, CompleteWidth + TabLeftOffset + CurrTabWidth);
      tpTop:
        ARect := Rect(CompleteWidth + TabLeftOffset, TabTopOffset,
          CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight);
      tpBottom:
        ARect := Rect(CompleteWidth + TabLeftOffset, TabBottomOffset,
            CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight - TabTopOffset);
    end;

    if Page.ActivePageIndex = I then
    begin
      // paint the background of selected tabs
      if Page.HotTrack and (FSelectHotIndex = I) then
        PaintItem(Canvas, ARect, 'checked-hot', Page.TabPosition)
      else
        PaintItem(Canvas, ARect, 'checked', Page.TabPosition);

     Canvas.Font.Assign(Page.ActiveFont);
     Canvas.Font.Color:= SkinManager.CurrentSkin.GetTextColor(skncTab, sknsChecked);
    end
    else
    begin
      if (I < Page.ActivePageIndex - 1) or (I > Page.ActivePageIndex) then
      begin
        // paint the separator between inactive items
        Canvas.Pen.Color := Page.InactiveFont.Color;
        case Page.TabPosition of
          tpLeft, tpRight:
            begin
              PaintSeparator(Canvas, Rect(TabTopOffset + 2,
                CompleteWidth + TabLeftOffset + CurrTabWidth - 4,
                PanelHeight - TabBottomOffset - 3,
                CompleteWidth + TabLeftOffset + CurrTabWidth+ 4));
            end;
          tpTop, tpBottom:
            begin
              PaintSeparator(Canvas, Rect(CompleteWidth + TabLeftOffset + CurrTabWidth -4,
              TabTopOffset + 2,
              CompleteWidth + TabLeftOffset + CurrTabWidth + 4,
              PanelHeight - TabBottomOffset - 3));
            end;
        end;
      end;

      // Paint tab background
//      if Page.HotTrack and (FSelectHotIndex = I) then
//        PaintItem(Canvas, ARect, 'hot', Page.TabPosition)
//      else
//        PaintItem(Canvas, ARect, 'normal', Page.TabPosition);
        
      Canvas.Font.Assign(Page.InactiveFont);
      Canvas.Font.Color:= SkinManager.CurrentSkin.GetTextColor(skncTab, sknsNormal);
    end;

    // now paint the caption
    case Page.TabPosition of
      tpLeft:
        ARect := Rect(TabTopOffset + CaptionTopOffset+1,
          CompleteWidth + TabLeftOffset + CaptionLeftOffset,
          PanelHeight,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset);

      tpRight:
        ARect := Rect(TabBottomOffset + CaptionTopOffset+1,
          CompleteWidth + TabLeftOffset + CaptionLeftOffset,
          PanelHeight,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset);

      tpTop:
        ARect := Rect(CompleteWidth + TabLeftOffset + CaptionLeftOffset +
          Integer(ShowTabImages) * (ImageWidth + CaptionLeftOffset),
          TabTopOffset + CaptionTopOffset+1,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset,
          PanelHeight);

      tpBottom:
        ARect := Rect(CompleteWidth + TabLeftOffset + CaptionLeftOffset +
          Integer(ShowTabImages) * (ImageWidth + CaptionLeftOffset),
          TabBottomOffset + CaptionTopOffset+1,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset,
          PanelHeight);
    end;

    Canvas.Brush.Style:= bsClear;
    CaptionString:= UTF8Decode(Page.Pages[I].Caption);

    SpDrawXPText(Canvas, CaptionString, ARect, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS);
    //DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
    //  ARect, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS);

    // finally paint the image
    if ShowTabImages and (Page.Images <> nil) and (CurrTabWidth > ImageWidth + 2 * CaptionLeftOffset) then
    begin
       SpDrawImageList(Canvas,
                       Rect(CompleteWidth + TabLeftOffset + CaptionLeftOffset,
                            TabBottomOffset + CaptionTopOffset,
                            CompleteWidth + TabLeftOffset + CaptionLeftOffset + Page.Images.Width -1,
                            TabBottomOffset + CaptionTopOffset + Page.Images.Height-1),
                       Page.Images,
                       Page.Pages[I].ImageIndex,
                       true,
                       false);
    end;

    Inc(CompleteWidth, CurrTabWidth + TabSplitterWidth);
  end;

end;

procedure TJvDockVSNETTabPanelTBX.TBMThemeChange(var Message: TMessage);
begin
  //if Message.WParam = TSC_VIEWCHANGE then
  Invalidate;
end;


{ TJvDockVSNETPanelTBX }

constructor TJvDockVSNETPanelTBX.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  VSChannelClass := TJvDockVSChannelTBX;
end;

{ TJvDockVSChannelTBX }

constructor TJvDockVSChannelTBX.Create(AOwner: TComponent);
begin
  inherited;
  FPaintBackground := True;
  ControlStyle := ControlStyle + [csOpaque];
  SkinManager.AddSkinNotification(Self);
end;

destructor TJvDockVSChannelTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSChannelTBX.DoAnimationDelay(Sender: TObject);
var
  P: TPoint;
begin
  try
    // Show the form only if the cursor is still above the same pane
    GetCursorPos(P);
    if PaneAtPos(ScreenToClient(P)) = FDelayPane then
      PopupDockForm(FDelayPane);
  finally
    // dangerous to free in handler?
    fAnimTimer.Free;
    fAnimTimer:= nil;
  end;
end;

procedure TJvDockVSChannelTBX.MouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  Pane: TJvDockVSPane;
begin
  //inherited MouseDown(Button, Shift, X, Y);
  Pane := PaneAtPos(Point(X, Y));
  if Assigned(Pane) then
  begin
    if PopupPane = Pane then
    begin
      if Pane.DockForm.CanFocus then
        Pane.DockForm.SetFocus;
    end
    else
    begin
      PopupDockForm(Pane);
    end;
  end;
end;

procedure TJvDockVSChannelTBX.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewDelayPane: TJvDockVSPane;
begin
  //inherited MouseMove(Shift, X, Y);

  NewDelayPane := PaneAtPos(Point(X, Y));
  if Assigned(NewDelayPane) and (NewDelayPane <> PopupPane) then
  begin
    // Create the timer object if not existing
    if fAnimTimer = nil then
    begin
      fAnimTimer := TTimer.Create(nil);
      fAnimTimer.OnTimer := DoAnimationDelay;
      fAnimTimer.Interval := 500;
      fAnimTimer.Enabled := True;
    end
    // Restart the timer only, if mouse is above another pane now
    else
    if NewDelayPane <> FDelayPane then
    begin
      fAnimTimer.Enabled := False;
      fAnimTimer.Enabled := True;
    end;
  end
  else
    FreeAndNil(fAnimTimer);

  FDelayPane := NewDelayPane;
end;

procedure TJvDockVSChannelTBX.MyGetBlockRect(Block: TJvDockVSBlock; Index:
    Integer; var ARect: TRect);
var
  BlockWidth: Integer;
  BlockHack: TCrackJvDockVSBlock;
begin
  BlockHack:= TCrackJvDockVSBlock(Block);

//  if Block.VSPane[Index] <> BlockHack.ActivePane then
//    BlockWidth := BlockHack.InactiveBlockWidth
//  else

  BlockWidth:= BlockHack.ActiveBlockWidth;
  // Add Separator size
  BlockWidth:= BlockWidth + 4;

  case Align of
    alLeft:
      begin
        ARect.Left := -1;
        ARect.Top := CurrentPos;
        ARect.Right := Width - BlockUpOffset;
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alRight:
      begin
        ARect.Left := BlockUpOffset;
        ARect.Top := CurrentPos;
        ARect.Right := Width + 1;
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alTop:
      begin
        ARect.Left := CurrentPos;
        ARect.Top := -1;
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height - BlockUpOffset;
      end;
    alBottom:
      begin
        ARect.Left := CurrentPos;
        ARect.Top := BlockUpOffset;
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height + 1;
      end;
  end;

  CurrentPos:= CurrentPos + BlockWidth - 1;
end;

procedure TJvDockVSChannelTBX.Paint;
var
  I: Integer;
  IsVertical: Boolean;

  procedure DrawSingleBlock(Block: TJvDockVSBlock);
  var
    DrawRect: TRect;
    BlockRect: TRect;
    SepRect: TRect;
    I: Integer;
    OldGraphicsMode: Integer;
    VisiblePaneCount: Integer;
    ii: TSpTBXMenuItemInfo;
    // TODO: Fix this
    //tmpForm: TCECustomDockableForm;

    procedure AdjustImagePos;
    begin
      if Align = alLeft then
      begin
        Inc(DrawRect.Left, 3);
        Inc(DrawRect.Top, 4);
      end
      else
      if Align = alTop then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 2);
      end
      else
      if Align = alRight then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 4);
      end
      else
      if Align = alBottom then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 3);
      end;
    end;

  begin
    VisiblePaneCount:= 0;
    for I := 0 to Block.VSPaneCount - 1 do
    begin
      if not Block.VSPane[I].Visible then
      Continue;

      MyGetBlockRect(Block, I, DrawRect);

      BlockRect:= DrawRect;
      SepRect:= DrawRect;
      if IsVertical then
      begin
        BlockRect.Bottom:= BlockRect.Bottom - 4;
        SepRect.Top:= BlockRect.Bottom;
      end
      else
      begin
        BlockRect.Right:= BlockRect.Right - 4;
        SepRect.Left:= BlockRect.Right;
      end;

      if TCrackJvDockVSBlock(Block).ActiveDockControl = Block.VSPane[I].DockForm then
      begin
        if Block.VSPane[I].Active then
        begin
          SetBtnItemInfo(ii, bsDown);
          Canvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsPushed);
        end
        else
        begin
          SetBtnItemInfo(ii, bsNormal);
          Canvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
        end;
        SpDrawXPMenuItem(Canvas, BlockRect, ii);
      end
      else
      begin
        SetBtnItemInfo(ii, bsNormal);
        SpDrawXPMenuItem(Canvas, BlockRect, ii);
        Canvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
      end;

      SpDrawXPMenuSeparator(Canvas, SepRect,false,not IsVertical);

      AdjustImagePos;
      
//TODO: Fix this
//      if Block.VSPane[i].DockForm is TCECustomDockableForm then
//      begin
//        tmpForm:= TCECustomDockableForm(Block.VSPane[i].DockForm);
//        if assigned(tmpForm.ImageList) then
//        begin
//          CurrentTheme.PaintImage(Canvas, Rect(DrawRect.Left, DrawRect.Top,
//            DrawRect.Left + tmpForm.ImageList.Width, DrawRect.Top + tmpForm.ImageList.Height),
//            ii, tmpForm.ImageList, tmpForm.ImageIndex);
//        end;
//      end
//      else
      begin
        // TODO: fix this
//        CurrentTheme.PaintImage(Canvas, Rect(DrawRect.Left, DrawRect.Top,
//          DrawRect.Left + Block.ImageList.Width, DrawRect.Top + Block.ImageList.Height),
//          ii, Block.ImageList, I);
      end;

      //if TCrackJvDockVSBlock(Block).ActiveDockControl = Block.VSPane[I].DockForm then
      //begin
      
        // draw the Caption
        if Align in [alTop, alBottom] then
          Inc(DrawRect.Left, TCrackJvDockVSBlock(Block).InactiveBlockWidth)
        else
        if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, TCrackJvDockVSBlock(Block).InactiveBlockWidth);
          if Align = alLeft then
            DrawRect.Left := 15
          else
            DrawRect.Left := 20;
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;
        //Canvas.Brush.Color := (DockServer.DockStyle as TJvDockVSNetStyleMod).ChannelOption.TabColor;
        //Canvas.Pen.Color := clBlack;

        Dec(DrawRect.Right, 3);

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        Canvas.Brush.Style := bsClear;

        SpDrawXPText(Canvas, UTF8Decode(Block.VSPane[I].DockForm.Caption), DrawRect, DT_END_ELLIPSIS or DT_NOCLIP);
//        else
//        DrawText(Canvas.Handle, PChar(Block.VSPane[I].DockForm.Caption), -1, DrawRect, DT_END_ELLIPSIS or DT_NOCLIP);
        SetGraphicsMode(Canvas.Handle, OldGraphicsMode);
        
      //end;
      Inc(VisiblePaneCount);
    end;
    if VisiblePaneCount > 0 then
      CurrentPos := CurrentPos + BlockInterval;
  end;

begin
  IsVertical:= (Align = alLeft) or (Align = alRight);
  // Paint background
  case SkinManager.GetSkinType of
    sknNone: begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(ClientRect);
    end;
    sknWindows: begin
      DrawThemeBackground(ThemeServices.Theme[teRebar], Canvas.Handle, 0, 0, ClientRect, nil);
    end;
    sknSkin: begin
      CurrentSkin.PaintBackground(Canvas, ClientRect, skncDock, sknsNormal, True, False, IsVertical);
      CurrentSkin.PaintBackground(Canvas, ClientRect, skncToolbar, sknsNormal, True, False, IsVertical);
    end;
  end;

  // Paint blocks
  CurrentPos := BlockStartOffset;
  for I := 0 to BlockCount - 1 do
  DrawSingleBlock(Block[I]);
end;

function TJvDockVSChannelTBX.PaneAtPos(MousePos: TPoint): TJvDockVSPane;
var
  I, J: Integer;
  ARect: TRect;
begin
  Result := nil;
  CurrentPos:= BlockStartOffset;
  for I := 0 to BlockCount - 1 do
  begin
    for J := 0 to Block[I].VSPaneCount - 1 do
    begin
      if not Block[I].VSPane[J].Visible then
        Continue;
      MyGetBlockRect(Block[I], J, ARect);
      if PtInRect(ARect, MousePos) then
      begin
        Result := Block[I].VSPane[J];
        Exit;
      end;
    end;
    CurrentPos := CurrentPos + BlockInterval;
  end;
end;

procedure TJvDockVSChannelTBX.PopupPaneChanged;
begin
  inherited;
  Invalidate;
end;

procedure TJvDockVSChannelTBX.TBMThemeChange(var Message: TMessage);
begin
  //if Message.WParam = TSC_VIEWCHANGE then
    Invalidate;
end;

{ TJvDockVSNETSplitterTBX }

constructor TJvDockVSNETSplitterTBX.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLoc := mlNone;
  FMouseOver := false;
  FMouseDown := false;
  ControlStyle := ControlStyle + [csOpaque];
  SkinManager.AddSkinNotification(Self);
end;

procedure TJvDockVSNETSplitterTBX.CMMouseLeave(var Message: TMessage);
begin
  FMouseOver := false;
  if not FMouseDown then
  begin
    FMouseLoc := mlNone;
    Invalidate;
  end;
end;

procedure TJvDockVSNETSplitterTBX.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;
  if FMouseOver then
    FMouseLoc := mlOver
  else
    FMouseLoc := mlNone;
  inherited;
  Invalidate;
end;

procedure TJvDockVSNETSplitterTBX.CMMouseEnter(var Message: TMessage);
begin
  if not FMouseDown then
  begin
    FMouseOver := true;
    FMouseLoc := mlOver;
    Invalidate;
  end;
end;

destructor TJvDockVSNETSplitterTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETSplitterTBX.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := true;
  FMouseLoc := mlDown;
  inherited;
  Invalidate;
end;

procedure TJvDockVSNETSplitterTBX.Paint;
var
 IsVertical: Boolean;
 r: TRect;
 state: TSpTBXSkinStatesType;
begin
  IsVertical:= (Align = alLeft) or (Align = alRight);
  r:= Self.ClientRect;
  if IsVertical then
  begin
    r.Left:= r.Left - 3;
    r.Right:= r.Right + 3;
  end
  else
  begin
    r.Top:= r.Top - 3;
    r.Bottom:= r.Bottom + 3;
  end;
  case FMouseLoc of
    mlNone: state:= sknsNormal;
    mlOver: state:= sknsHotTrack;
    else state:= sknsPushed;
  end;
  CurrentSkin.PaintBackground(Canvas, r, skncSplitter, state, True, False, IsVertical);
  if Assigned(OnPaint) then
   OnPaint(Self);
end;

procedure TJvDockVSNETSplitterTBX.TBMThemeChange(var Message: TMessage);
begin
  //if Message.WParam = TSC_VIEWCHANGE then
  Invalidate;
end;

procedure TCEJvDockVSNETTabSheet.SetPageControl(APageControl:
    TJvDockPageControl);
begin
  inherited SetPageControl(APageControl);
  
  if APageControl is TJvDockVSNEtTabPageControlTBX then
  begin
    Self.OnShow:= TJvDockVSNEtTabPageControlTBX(APageControl).OnSheetShow;
    Self.OnHide:= TJvDockVSNEtTabPageControlTBX(APageControl).OnSheetHide;
  end;
end;

end.
