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
//  The Original Code is CV_ImageView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CV_ImageView;

interface

uses
  // CubicCore
  ccThreadUtils, ccClasses, ccFileUtils,
  // GraphicEx
  GraphicEx,
  // Graphics32
  GR32, GR32_Image, GR32_Layers, GR32_Resamplers,
  // System Units
  SysUtils, Classes, Controls, StdCtrls, Forms, Messages, Windows, ExtCtrls,
  Graphics, jpeg;

type

{-------------------------------------------------------------------------------
  TCVImageView
-------------------------------------------------------------------------------}
  TCVImageViewTask = class(TObject)
  protected
    fLoadingSize: Extended;
  public
    Bitmap: TBitmap;
    FilePath: WideString;
    BackgroundColor: TColor;
    OptimalSize: Boolean;
    ClientSize: TPoint;
    FullSize: TPoint;
    DoResizeToFit: Boolean;
    Thumbnail: Boolean;
    StartTick: Cardinal;
    destructor Destroy; override;
  end;

  TCVImageResampler = (irNearest, irLinear, irDraft, irBox, irCosine, irCubic,
                       irHermite, irMitchell, irSinsh, irSpline, irAlbrecht,
                       irBlackman, irGaussian, irHamming, irHann, irLanczos);

  TCVImageViewStatus = (ivsEmpty, ivsLoading, ivsLoaded, ivsAborted, ivsError);

  TCVImageView = class(TImage32)
  private
    fAutoResizeToFit: Boolean;
    fAutoResizeToOptimalFit: Boolean;
    fLoadingSize: Extended;
    procedure SetAutoResizeToFit(const Value: Boolean); virtual;
    procedure SetAutoResizeToOptimalFit(const Value: Boolean); virtual;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    fActiveFilePath: WideString;
    fAdaptiveZoom: Boolean;
    fAnchor: TFloatPoint;
    fAnchorOffset: TFloatPoint;
    fCurrentResampler: TCVImageResampler;
    fDragging: Boolean;
    fDragPoint: TPoint;
    fFullSize: TPoint;
    fLastTaskTick: Cardinal;
    fLoadErrorMsg: WideString;
    fScaleUpResampler: TCVImageResampler;
    fOffset: TFloatPoint;
    fOnStatusChanged: TNotifyEvent;
    fOnZoomChange: TNotifyEvent;
    fScaleDownResampler: TCVImageResampler;
    fScale: Extended;
    fStatus: TCVImageViewStatus;
    fSupportsOptimizedLoad: Boolean;
    fTaskTag: Integer;
    fLoadOptimalSize: Boolean;
    fOnLoadError: TNotifyEvent;
    fUseThumbnailPreview: Boolean;
    fZoom: Extended;
    procedure BitmapResized; override;
    procedure ChangeResampler; virtual;
    procedure ChangeStatus(AStatus: TCVImageViewStatus); virtual;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoLoadOptimized; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos:
        TPoint): Boolean; override;
    procedure DoPaintGDIOverlay; override;
    function GetViewport: TFloatRect; virtual;
    function GetViewRange: TFloatRect; virtual;
    function GetViewRangeSize: TFloatPoint; virtual;
    procedure HandleTaskDone(Sender: TObject; AObject: TObject; AData: Pointer;
        ATag: Integer); virtual;
    procedure HandleTaskExecute(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
        Layer: TCustomLayer); reintroduce; overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
        reintroduce; overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
        Layer: TCustomLayer); reintroduce; overload; override;
    procedure SetAdaptiveZoom(const Value: Boolean); virtual;
    procedure SetScaleUpResampler(const Value: TCVImageResampler); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetScaleDownResampler(const Value: TCVImageResampler); virtual;
    procedure SetLoadOptimalSize(const Value: Boolean);
    procedure SetZoom(const Value: Extended); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort; virtual;
    procedure Clear; virtual;
    procedure DoPanAndZoom; virtual;
    function GetResizeToFitScale: Extended; virtual;
    procedure OpenFile(AFilePath: WideString; Async: Boolean = true; ResizeToFit:
        Boolean = true); virtual;
    procedure Resize; override;
    procedure ResizeToActualSize; virtual;
    procedure ResizeToFit; virtual;
    procedure ResizeToOptimalFit; virtual;
    procedure UpdateScrollbars; virtual;
    property ActiveFilePath: WideString read fActiveFilePath;
    property AutoResizeToFit: Boolean read fAutoResizeToFit write
        SetAutoResizeToFit;
    property AutoResizeToOptimalFit: Boolean read fAutoResizeToOptimalFit write
        SetAutoResizeToOptimalFit;
    property FullSize: TPoint read fFullSize;
    property ScaleUpResampler: TCVImageResampler read fScaleUpResampler write
        SetScaleUpResampler;
    property ScaleDownResampler: TCVImageResampler read fScaleDownResampler write
        SetScaleDownResampler;
    property Status: TCVImageViewStatus read fStatus;
    property LoadOptimalSize: Boolean read fLoadOptimalSize write
        SetLoadOptimalSize;
    property UseThumbnailPreview: Boolean read fUseThumbnailPreview write
        fUseThumbnailPreview;
    property Zoom: Extended read fZoom write SetZoom;
  published
    property AdaptiveZoom: Boolean read fAdaptiveZoom write SetAdaptiveZoom;
    property Align;
    property Anchors;
    property Font;
    property LoadErrorMsg: WideString read fLoadErrorMsg write fLoadErrorMsg;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnLoadError: TNotifyEvent read fOnLoadError write fOnLoadError;
    property OnStatusChanged: TNotifyEvent read fOnStatusChanged write
        fOnStatusChanged;
    property OnZoomChange: TNotifyEvent read fOnZoomChange write fOnZoomChange;
  end;

{-------------------------------------------------------------------------------
  TCVImageViewPanel
-------------------------------------------------------------------------------}
  TCVImageViewPanel = class(TPanel)
  protected
    fView: TCVImageView;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCVImageView read fView;
  end;

  TJPEGImageAccess = class(TJPEGImage);

function LoadGraphic(AFilePath: WideString; var FullWidth, FullHeight: Integer;
    AOptimalWidth: Integer = -1; AOptimalHeight: Integer = -1): TGraphic;
function LoadThumbnail(AFilePath: WideString; ThumbWidth, ThumbHeight: Integer;
    BackgroundColor: TColor; UseFallback: Boolean = false): TBitmap;
procedure StretchDrawGraphic(AGraphic: TGraphic; ADestCanvas: TCanvas;
    ADestRect: TRect; ASubsampling: Boolean = true);
procedure DrawGraphic(AGraphic: TGraphic; ADestCanvas: TCanvas; X, Y: Integer);



implementation

uses
  Math, GR32_LowLevel, MPShellUtilities, ActiveX,
  MPShellTypes, MPCommonUtilities;

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  Load Graphic
-------------------------------------------------------------------------------}
function LoadGraphic(AFilePath: WideString; var FullWidth, FullHeight: Integer;
    AOptimalWidth: Integer = -1; AOptimalHeight: Integer = -1): TGraphic;
var
  GraphicClass: TGraphicClass;
  Ext: WideString;
  fs: TStream;
  scale: Extended;
  J: TJPEGImage;
begin
  Result:= nil;
  Ext:= WideLowerCase(WideExtractFileExt(AFilePath));
  GraphicClass:= FileFormatList.GraphicFromExtension(Ext);
  if GraphicClass <> nil then
  begin
    fs:= TCCFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
    try
      fs.Position:= 0;
      Result:= GraphicClass.Create;
      try
        Result.LoadFromStream(fs);

        FullWidth:= Result.Width;
        FullHeight:= Result.Height;
        // set optimal scale
        if ((AOptimalWidth > -1) and (AOptimalHeight > -1)) then
        begin
          // jpg
          if Result is TJPEGImage then
          begin
            J:= TJPEGImage(Result);
            J.Performance:= jpBestQuality;
            J.Scale:= jsFullSize;
            while ((J.Width > AOptimalWidth) or (J.Height > AOptimalHeight)) and (J.Scale < jsEighth) do
            J.Scale:= Succ(J.Scale);

            if (J.Scale <> jsFullSize) and ((J.Width < AOptimalWidth) and (J.Height < AOptimalHeight)) then
            J.Scale:= Pred(J.Scale);

            J.DibNeeded;
          end
          // metafile
          else if Result is TMetafile then
          begin
            scale:= Min(1, Min(AOptimalWidth / Result.Width, AOptimalHeight / Result.Height));
            Result.Width:= Round(Result.Width * scale);
            Result.Height:= Round(Result.Height * scale);
          end;
        end;
      except
        FreeAndNil(Result);
      end;
    finally
      fs.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  LoadThumbnail
-------------------------------------------------------------------------------}
function LoadThumbnail(AFilePath: WideString; ThumbWidth, ThumbHeight: Integer;
    BackgroundColor: TColor; UseFallback: Boolean = false): TBitmap;
var
  Graphic: TGraphic;
  Ext: WideString;
  w,h: Integer;
  ns: TNamespace;
  ms: TMemoryStream;
  bit: TBitmap;
  ar: Extended;
begin
  Result:= nil;
  Ext:= WideLowerCase(WideExtractFileExt(AFilePath));
  // load jpeg and metafile
  if (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.jif') or (Ext = '.jpe') or (Ext = '.wmf') or (Ext = '.emf') then
  begin
    Graphic:= LoadGraphic(AFilePath,w,h,ThumbWidth,ThumbHeight);
    if assigned(Graphic) then
    begin
      try
        Result:= TBitmap.Create;
        Result.Assign(Graphic);
      finally
        Graphic.Free;
      end;
    end;
  end;

  // extract thumbnail from shell if needed
  if not assigned(Result) then
  begin
    ns:= TNamespace.CreateFromFileName(AFilePath);
    try
      if Assigned(NS.ExtractImage) then
      begin
        NS.ExtractImage.Flags:= NS.ExtractImage.Flags or IEIFLAG_OFFLINE or IEIFLAG_ORIGSIZE;

        NS.ExtractImage.Width:= ThumbWidth;
        NS.ExtractImage.Height:= ThumbHeight;

        NS.ExtractImage.ImagePath;
        Result:= NS.ExtractImage.Image;
        if assigned(Result) then
        begin
          ConvertBitmapEx(Result, Result, BackgroundColor);
          ms:= TMemoryStream.Create;
          try
            // flip image
            // TODO: find out why this is needed.
            Result.SaveToStream(ms);
            ms.Position:= 0;
            Result.LoadFromStream(ms);
          finally
            ms.Free;
          end;
        end;
      end;
    finally
      ns.Free;
    end;
  end;

  // use fallback method if needed
  if not assigned(Result) and UseFallback then
  begin
    Graphic:= LoadGraphic(AFilePath,w,h,ThumbWidth,ThumbHeight);
    if assigned(Graphic) then
    begin
      try
        Result:= TBitmap.Create;
        try
          // icons
          if Graphic is TIcon then
          begin
            Result.SetSize(Graphic.Width, Graphic.Height);
            Result.Canvas.Lock;
            try
              Result.Canvas.Brush.Color:= BackgroundColor;
              Result.Canvas.FillRect(Rect(0,0,Result.Width,Result.Height));
              Result.Canvas.Draw(0,0,Graphic);
            finally
              Result.Canvas.Unlock;
            end;
          end
          // everything else
          else
          Result.Assign(Graphic);
        except
        end;
      finally
        Graphic.Free;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  StretchDrawGraphic
  - use this for thread-safe drawing
-------------------------------------------------------------------------------}
procedure StretchDrawGraphic(AGraphic: TGraphic; ADestCanvas: TCanvas;
    ADestRect: TRect; ASubsampling: Boolean = true);
var
  bit: TBitmap;
  freeBit: Boolean;
begin
  if not assigned(ADestCanvas) or not assigned(AGraphic) then
  Exit;

  // Draw icon or metafiles
  if (AGraphic is TIcon) or (AGraphic is TMetafile) then
  begin
    ADestCanvas.Lock;
    try
      // Draw should be thread-safe in TIcon and TMetafile
      ADestCanvas.StretchDraw(ADestRect, AGraphic);
    finally
      ADestCanvas.Unlock;
    end;
  end
  else
  begin
    bit:= nil;
    freeBit:= false;
    // jpeg
    if AGraphic is TJPEGImage then
    bit:= TJPEGImageAccess(AGraphic).Bitmap
    // bitmap (and GraphicEx formats)
    else if AGraphic is TBitmap then
    bit:= TBitmap(AGraphic)
    // icon
    else
    begin
      freeBit:= true;
      bit:= TBitmap.Create;
      try
        bit.Assign(AGraphic);
      finally
      end;
    end;

    // draw to canvas
    if assigned(bit) then
    begin
      ADestCanvas.Lock;
      bit.Canvas.Lock;
      try
        if ASubsampling then
        begin
          SetStretchBltMode(ADestCanvas.Handle, STRETCH_HALFTONE);
          SetBrushOrgEx(ADestCanvas.Handle, 0, 0, nil);
        end
        else
        begin
          SetStretchBltMode(ADestCanvas.Handle, STRETCH_DELETESCANS);
        end;

        StretchBlt(ADestCanvas.Handle,
          ADestRect.Left, ADestRect.Top, ADestRect.Right-ADestRect.Left, ADestRect.Bottom-ADestRect.Top,
          bit.Canvas.Handle, 0, 0, bit.Width, bit.Height, SRCCopy);
      finally
        bit.Canvas.Unlock;
        ADestCanvas.Unlock;
      end;

      // free bitmap is needed
      if freeBit then
      bit.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DrawGraphic
-------------------------------------------------------------------------------}
procedure DrawGraphic(AGraphic: TGraphic; ADestCanvas: TCanvas; X, Y: Integer);
var
  bit: TBitmap;
  freeBit: Boolean;
begin
  if not assigned(ADestCanvas) or not assigned(AGraphic) then
  Exit;

  // Draw icon or metafile
  if (AGraphic is TIcon) or (AGraphic is TMetafile) then
  begin
    ADestCanvas.Lock;
    try
      // Draw should be thread-safe in TIcon and TMetafile
      ADestCanvas.Draw(X,Y, AGraphic);
    finally
      ADestCanvas.Unlock;
    end;
  end
  // Draw other formats
  else
  begin
    bit:= nil;
    freeBit:= false;
    // jpeg
    if AGraphic is TJPEGImage then
    bit:= TJPEGImageAccess(AGraphic).Bitmap
    // bitmap (and GraphicEx formats)
    else if AGraphic is TBitmap then
    bit:= TBitmap(AGraphic)
    // other
    else
    begin
      freeBit:= true;
      bit:= TBitmap.Create;
      try
        bit.Assign(AGraphic);
      finally
      end;
    end;

    // draw to canvas
    if assigned(bit) then
    begin
      ADestCanvas.Lock;
      bit.Canvas.Lock;
      try
        BitBlt(ADestCanvas.Handle,
          X, Y, bit.Width, bit.Height,
          bit.Canvas.Handle, 0, 0,
          SRCCopy);
      finally
        bit.Canvas.Unlock;
        ADestCanvas.Unlock;
      end;

      if freeBit then
      bit.Free;
    end;
  end;
end;

{##############################################################################}
// TCVImageView

{-------------------------------------------------------------------------------
  Create an instance of TCVImageView
-------------------------------------------------------------------------------}
constructor TCVImageView.Create(AOwner: TComponent);
begin
  inherited;
  Self.BitmapAlign:= baCustom;
  Self.TabStop:= true;
  Self.ScaleMode:= smScale;
  Self.Options:= [pboAutoFocus];

  // Init values
  fScale:= 1;
  fZoom:= 100;
  fAnchor:= FloatPoint(0,0);
  fAnchorOffset:= FloatPoint(0,0);
  fDragging:= false;
  fOffset:= FloatPoint(0,0);
  fAutoResizeToFit:= false;
  fAutoResizeToOptimalFit:= false;
  fAdaptiveZoom:= true;
  fScaleDownResampler:= irNearest;
  fScaleUpResampler:= irNearest;
  fCurrentResampler:= irNearest;
  fTaskTag:= GlobalTaskPool.GetUniqueTagID;
  fStatus:= ivsEmpty;
  fLastTaskTick:= 0;
  fSupportsOptimizedLoad:= false;
  fUseThumbnailPreview:= true;
  fLoadOptimalSize:= true;
  fLoadErrorMsg:= 'Error';
  Color:= clWindow;
end;

{-------------------------------------------------------------------------------
  Destroy TCVImageView
-------------------------------------------------------------------------------}
destructor TCVImageView.Destroy;
begin
  GlobalTaskPool.AbortTasksWithTag(fTaskTag);
  inherited;
end;

{-------------------------------------------------------------------------------
  Abort
-------------------------------------------------------------------------------}
procedure TCVImageView.Abort;
begin
  GlobalTaskPool.AbortTasksWithTag(fTaskTag);
  fLoadingSize:= 0;
  if fStatus = ivsLoading then ChangeStatus(ivsAborted);
end;

{-------------------------------------------------------------------------------
  On BitmapResized
-------------------------------------------------------------------------------}
procedure TCVImageView.BitmapResized;
begin
  inherited;
  if (UpdateCount = 0) then
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  Change Resampler
-------------------------------------------------------------------------------}
procedure TCVImageView.ChangeResampler;
begin
  if (fScale < 1) and (fScaleDownResampler <> fCurrentResampler) then
  begin
    case fScaleDownResampler of
      irNearest: TNearestResampler.Create(Bitmap);
      irLinear: TLinearResampler.Create(Bitmap);
      irDraft: TDraftResampler.Create(Bitmap);
      irBox: TKernelResampler.Create(Bitmap).Kernel:= TBoxKernel.Create;
      irCosine:TKernelResampler.Create(Bitmap).Kernel:= TCosineKernel.Create;
      irCubic: TKernelResampler.Create(Bitmap).Kernel:= TCubicKernel.Create;
      irHermite: TKernelResampler.Create(Bitmap).Kernel:= THermiteKernel.Create;
      irMitchell: TKernelResampler.Create(Bitmap).Kernel:= TMitchellKernel.Create;
      irSinsh: TKernelResampler.Create(Bitmap).Kernel:= TSinshKernel.Create;
      irSpline: TKernelResampler.Create(Bitmap).Kernel:= TSplineKernel.Create;
      irAlbrecht: TKernelResampler.Create(Bitmap).Kernel:= TAlbrechtKernel.Create;
      irBlackman: TKernelResampler.Create(Bitmap).Kernel:= TBlackmanKernel.Create;
      irGaussian: TKernelResampler.Create(Bitmap).Kernel:= TGaussianKernel.Create;
      irHamming: TKernelResampler.Create(Bitmap).Kernel:= THammingKernel.Create;
      irHann: TKernelResampler.Create(Bitmap).Kernel:= THannKernel.Create;
      irLanczos: TKernelResampler.Create(Bitmap).Kernel:= TLanczosKernel.Create;
    end;
    fCurrentResampler:= fScaleDownResampler;
  end
  else if (fScale > 1) and (fScaleUpResampler <> fCurrentResampler) then
  begin
    case fScaleUpResampler of
      irNearest: TNearestResampler.Create(Bitmap);
      irLinear: TLinearResampler.Create(Bitmap);
      irDraft: TDraftResampler.Create(Bitmap);
      irBox: TKernelResampler.Create(Bitmap).Kernel:= TBoxKernel.Create;
      irCosine:TKernelResampler.Create(Bitmap).Kernel:= TCosineKernel.Create;
      irCubic: TKernelResampler.Create(Bitmap).Kernel:= TCubicKernel.Create;
      irHermite: TKernelResampler.Create(Bitmap).Kernel:= THermiteKernel.Create;
      irMitchell: TKernelResampler.Create(Bitmap).Kernel:= TMitchellKernel.Create;
      irSinsh: TKernelResampler.Create(Bitmap).Kernel:= TSinshKernel.Create;
      irSpline: TKernelResampler.Create(Bitmap).Kernel:= TSplineKernel.Create;
      irAlbrecht: TKernelResampler.Create(Bitmap).Kernel:= TAlbrechtKernel.Create;
      irBlackman: TKernelResampler.Create(Bitmap).Kernel:= TBlackmanKernel.Create;
      irGaussian: TKernelResampler.Create(Bitmap).Kernel:= TGaussianKernel.Create;
      irHamming: TKernelResampler.Create(Bitmap).Kernel:= THammingKernel.Create;
      irHann: TKernelResampler.Create(Bitmap).Kernel:= THannKernel.Create;
      irLanczos: TKernelResampler.Create(Bitmap).Kernel:= TLanczosKernel.Create;
    end;
    fCurrentResampler:= fScaleUpResampler;
  end;
end;

{-------------------------------------------------------------------------------
  ChangeStatus
-------------------------------------------------------------------------------}
procedure TCVImageView.ChangeStatus(AStatus: TCVImageViewStatus);
begin
  fStatus:= AStatus;
  if assigned(fOnStatusChanged) then
  fOnStatusChanged(Self);
  if fStatus = ivsError then
  Paint;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCVImageView.Clear;
begin
  Abort;
  fActiveFilePath:= '';
  Self.Bitmap.SetSize(0,0);
  fScale:= 1;
  fZoom:= 100;
  ChangeStatus(ivsEmpty);
end;

{-------------------------------------------------------------------------------
  Create Handle
-------------------------------------------------------------------------------}
procedure TCVImageView.CreateHandle;
begin
  inherited;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  Create Params
-------------------------------------------------------------------------------}
procedure TCVImageView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:= Params.Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_HSCROLL or WS_VSCROLL;
end;

{-------------------------------------------------------------------------------
  Do LoadOptimized
-------------------------------------------------------------------------------}
procedure TCVImageView.DoLoadOptimized;
var
  task: TCVImageViewTask;
  e: Extended;
  doLoad: Boolean;
  t: Cardinal;
begin
  if fSupportsOptimizedLoad and (fScale <> 1) then
  begin
    if fScale > 1 then
    begin
      doLoad:= Bitmap.Width < fFullSize.X;
      if doLoad then
      e:= Ceil(fFullSize.X / Max(Round((fFullSize.X / Bitmap.Width)) * 0.5, 1))
      else
      e:= 0;
    end
    else
    begin
      e:= Ceil(fFullSize.X / Min(Round((fFullSize.X / Bitmap.Width)) * 2, 8));
      doLoad:= (e > (Bitmap.Width*fScale)) and (e < Bitmap.Width);
    end;

    if doLoad and (fLoadingSize <> e) then
    begin
      t:= GetTickCount;
      if t <= fLastTaskTick then
      fLastTaskTick:= fLastTaskTick + 1
      else
      fLastTaskTick:= t;

      // abort previous tasks
      Abort;

      // create new task
      task:= TCVImageViewTask.Create;
      task.FilePath:= fActiveFilePath;
      task.OptimalSize:= true;
      task.ClientSize:= Point(Ceil(Bitmap.Width*fScale), Ceil(Bitmap.Height*fScale));
      task.DoResizeToFit:= false;
      task.Thumbnail:= false;
      task.StartTick:= fLastTaskTick;

      fLoadingSize:= e;
      ChangeStatus(ivsLoading);
      GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleTaskExecute, HandleTaskDone);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DoMouseWheel
-------------------------------------------------------------------------------}
function TCVImageView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
    MousePos: TPoint): Boolean;
var
  rangeSize: TFloatPoint;
begin
  inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  Result:= true;
  // Wheel Down
  if WheelDelta < 0 then
  begin
    fScale:= fScale * 0.9;
  end
  // Wheel Up
  else
  begin
    fScale:= fScale * 1.1;
  end;

  if fAdaptiveZoom then
  begin
    fAutoResizeToFit:= false;
    fAutoResizeToOptimalFit:= false;
  end;

  rangeSize:= GetViewRangeSize;
  MousePos:= Self.ScreenToClient(MousePos);
  fAnchor.X:= MousePos.X;
  fAnchor.Y:= MousePos.Y;
  fAnchorOffset.X:= (fOffset.X + fAnchor.X) / rangeSize.X;
  fAnchorOffset.Y:= (fOffset.Y + fAnchor.Y) / rangeSize.Y;

  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  Do PaintGDIOverlay
-------------------------------------------------------------------------------}
procedure TCVImageView.DoPaintGDIOverlay;

  function GetFontColor(BackgroundColor: TColor): TColor;
  var
    R, G, B: Integer;
  begin
    R := GetRValue(BackgroundColor) * 2;
    G := GetGValue(BackgroundColor) * 5;
    B := GetBValue(BackgroundColor);
    if R + G + B < 1024 then
    begin
      // dark background, use white
      Result:= clWhite;
    end
    else
    begin
      // bright background, use black
      Result:= clBlack;
    end;
  end;

var
  r: TRect;
begin
  inherited;
  // show error message
  if (fStatus = ivsError) and (fLoadErrorMsg <> '') then
  begin
    Self.Canvas.Lock;
    try
      Self.Canvas.Brush.Style:= bsClear;
      Self.Canvas.Font.Assign(Self.Font);
      Self.Canvas.Font.Color:= GetFontColor(Color);
      r:= Self.ClientRect;

      DrawTextW(Self.Canvas.Handle,
                PWideChar(fLoadErrorMsg), Length(fLoadErrorMsg),
                r, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
    finally
      Self.Canvas.Unlock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do PanAndZoom
-------------------------------------------------------------------------------}
procedure TCVImageView.DoPanAndZoom;
var
  rangeNew: TFloatPoint;
  newZoom: Extended;
begin
  if (Self.Bitmap.Width = 0) or (Self.Bitmap.Height = 0) or (Self.UpdateCount <> 0) then
  Exit;

  // Validate scale
  if fAutoResizeToFit then 
  begin
    // AutoResizeToFit
    fScale:= GetResizeToFitScale;
  end
  else if fAutoResizeToOptimalFit then 
  begin
    // AutoResizeToOptimalFit
    fScale:= GetResizeToFitScale;
    if fScale > 1 then
    fScale:= 1;
  end
  else // Normal
  begin
    //   make sure at least one pixel is visible
    if ((Self.Bitmap.Width * fScale) < 1) then
    fScale:= 1 / Self.Bitmap.Width;
    if ((Self.Bitmap.Height * fScale) < 1) then
    fScale:= 1 / Self.Bitmap.Height;
  end;

  // Calculate range
  rangeNew.X:= Self.Bitmap.Width * fScale;
  rangeNew.Y:= Self.Bitmap.Height * fScale;

  // Validate fAnchorOffset
  if fAnchorOffset.X > 1 then
  fAnchorOffset.X:= 1
  else if fAnchorOffset.X < 0 then
  fAnchorOffset.X:= 0;

  if fAnchorOffset.Y > 1 then
  fAnchorOffset.Y:= 1
  else if fAnchorOffset.Y < 0 then
  fAnchorOffset.Y:= 0;

  // Calculate offset
  fOffset.X:= (rangeNew.X * fAnchorOffset.X) - fAnchor.X;
  fOffset.Y:= (rangeNew.Y * fAnchorOffset.Y) - fAnchor.Y;

  // Validate horizontal offset
  if fOffset.X < 0 then
  fOffset.X:= 0;
  if rangeNew.X <= ClientWidth then
  begin
    // center if the range is smaller than the viewport
    fOffset.X:= 0 - (ClientWidth - rangeNew.X) / 2;
  end
  else if (fOffset.X + ClientWidth) > rangeNew.X then
  begin
    fOffset.X:= rangeNew.X - ClientWidth;
  end;

  // Validate vertical offset
  if fOffset.Y < 0 then
  fOffset.Y:= 0;
  if rangeNew.Y <= ClientHeight then
  begin
    // center if the range is smaller than the viewport
    fOffset.Y:= 0 - (ClientHeight - rangeNew.Y) / 2;
  end
  else if (fOffset.Y + ClientHeight) > rangeNew.Y then
  begin
    fOffset.Y:= rangeNew.Y - ClientHeight;
  end;  

  // Change values for fImage
  if (fScale <> Self.Scale) or
     (Self.OffsetHorz <> -fOffset.X) or
     (Self.OffsetVert <> -fOffset.Y) then
  begin
    Self.BeginUpdate;
    try
      ChangeResampler;
      Self.Scale:= fScale;
      Self.OffsetHorz:= -fOffset.X;
      Self.OffsetVert:= -fOffset.Y;
    finally
      Self.EndUpdate;
      Self.Invalidate;
    end;
  end;

  // Calculate Zoom
  if fFullSize.X <> 0 then
  begin
    newZoom:= ((Bitmap.Width*fScale) / fFullSize.X) * 100;
    if newZoom <> fZoom then
    begin
      DoLoadOptimized;

      fZoom:= newZoom;
      if assigned(fOnZoomChange) then
      fOnZoomChange(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get ResizeToFitScale
-------------------------------------------------------------------------------}
function TCVImageView.GetResizeToFitScale: Extended;
begin
  if (Bitmap.Width / Bitmap.Height) > (Width / Height) then
  Result:= Width / (Bitmap.Width)
  else
  Result:= Height / (Bitmap.Height);
end;

{-------------------------------------------------------------------------------
  Resize
-------------------------------------------------------------------------------}
procedure TCVImageView.Resize;
begin
  inherited;
  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  Set Parent
-------------------------------------------------------------------------------}
procedure TCVImageView.SetParent(AParent: TWinControl);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Viewport
-------------------------------------------------------------------------------}
function TCVImageView.GetViewport: TFloatRect;
begin
  Result:= FloatRect(fOffset.X,
                     fOffset.Y,
                     fOffset.X + ClientWidth,
                     fOffset.Y + ClientHeight);
end;

{-------------------------------------------------------------------------------
  Get ViewRange
-------------------------------------------------------------------------------}
function TCVImageView.GetViewRange: TFloatRect;
begin
  Result:= FloatRect(0,
                     0,
                     Self.Bitmap.Width * Self.Scale,
                     Self.Bitmap.Height * Self.Scale);
end;

{-------------------------------------------------------------------------------
  Get ViewRange Size
-------------------------------------------------------------------------------}
function TCVImageView.GetViewRangeSize: TFloatPoint;
begin
  Result.X:= Self.Bitmap.Width * Self.Scale;
  Result.Y:= Self.Bitmap.Height * Self.Scale;
end;

{-------------------------------------------------------------------------------
  Handle Task Done
-------------------------------------------------------------------------------}
procedure TCVImageView.HandleTaskDone(Sender: TObject; AObject: TObject; AData:
    Pointer; ATag: Integer);
var
  task: TCVImageViewTask;
  newScale: Extended;
begin
  if (ATag = fTaskTag) and (AObject is TCVImageViewTask) then
  begin
    task:= TCVImageViewTask(AObject);

    if task.fLoadingSize = fLoadingSize then
    fLoadingSize:= 0;

    if task.StartTick < fLastTaskTick then // discard old tasks
    Exit
    else
    fLastTaskTick:= task.StartTick;

    if assigned(task.Bitmap) then
    begin
      Self.BeginUpdate;
      try
        // calculate new scale
        if (Self.Bitmap.Width > 0) and (Self.Bitmap.Height > 0) then
        begin
          newScale:= (Self.Bitmap.Width*fScale) / task.Bitmap.Width;
          fScale:= newScale;
        end;
        // copy bitmap
        Self.Bitmap.SetSize(task.Bitmap.Width, task.Bitmap.Height);               
        Self.Bitmap.Canvas.Brush.Color:= task.BackgroundColor;
        Self.Bitmap.Canvas.FillRect(Rect(0,0,Self.Bitmap.Width, Self.Bitmap.Height));
        DrawGraphic(task.Bitmap, Self.Bitmap.Canvas, 0, 0);
        FreeAndNil(task.Bitmap);
      finally
        fFullSize:= task.FullSize;
        Self.EndUpdate;
        ChangeStatus(ivsLoaded);
        if task.DoResizeToFit then
        ResizeToFit
        else
        DoPanAndZoom;
      end;
    end
    else
    begin
      ChangeStatus(ivsError);
      if assigned(fOnLoadError) then
      fOnLoadError(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Task Execute (runs in a separate thread!!!)
-------------------------------------------------------------------------------}
procedure TCVImageView.HandleTaskExecute(Sender: TCCTaskPoolThread; AObject: TObject;
    AData: Pointer; ATag: Integer);
var
  Ext: WideString;
  NewGraphic: TGraphic;
  NewGraphicClass: TGraphicClass;
  task: TCVImageViewTask;
  pic: TPicture;
begin
  if AObject is TCVImageViewTask then
  begin
    task:= TCVImageViewTask(AObject);
    if WideFileExists(task.FilePath) then
    begin
      Ext:= WideExtractFileExt(task.FilePath);
      // Thumbnail
      if task.Thumbnail then
      begin
        CoInitialize(nil);
        try
          task.Bitmap:= LoadThumbnail(task.FilePath, task.ClientSize.X, task.ClientSize.Y, task.BackgroundColor, false);
          if assigned(task.Bitmap) then
          begin
            task.FullSize.X:= task.Bitmap.Width;
            task.FullSize.Y:= task.Bitmap.Height;
          end;
        finally
          CoUninitialize;
        end;
      end
      // Normal
      else
      begin
        if task.OptimalSize then
        NewGraphic:= LoadGraphic(task.FilePath, task.FullSize.X, task.FullSize.Y, task.ClientSize.X, task.ClientSize.Y)
        else
        NewGraphic:= LoadGraphic(task.FilePath, task.FullSize.X, task.FullSize.Y);

        if assigned(NewGraphic) then
        begin
          task.Bitmap:= TBitmap.Create;
          try
            task.Bitmap.SetSize(NewGraphic.Width, NewGraphic.Height);
            task.Bitmap.Canvas.Lock;
            try
              task.Bitmap.Canvas.Brush.Color:= Color;
              task.Bitmap.Canvas.FillRect(Rect(0,0,NewGraphic.Width,NewGraphic.Height));

              // Use DrawGraphic to draw thread-safely.
              // Generally TGraphic.Draw is not thread safe and will cause memory leaks.
              //DrawGraphic(NewGraphic, task.Bitmap.Canvas, 0,0);
              StretchDrawGraphic(NewGraphic, task.Bitmap.Canvas, Rect(0,0,NewGraphic.Width,NewGraphic.Height));
            finally
              task.Bitmap.Canvas.Unlock;
            end;
          finally
            FreeAndNil(NewGraphic);
          end;
        end;

      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Key Down
-------------------------------------------------------------------------------}
procedure TCVImageView.KeyDown(var Key: Word; Shift: TShiftState);
var
  msg: TWMHScroll;
begin
  inherited;

  case Key of
    // Page up
    VK_PRIOR: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGELEFT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_PAGEUP;
        WMVScroll(msg);
      end;
    end;
    // Page down
    VK_NEXT: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGERIGHT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_PAGEDOWN;
        WMVScroll(msg);
      end;
    end;
    // End
    VK_END: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_RIGHT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_BOTTOM;
        WMVScroll(msg);
      end;
    end;
    // Home
    VK_HOME: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_LEFT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_TOP;
        WMVScroll(msg);
      end;
    end;
    // Left
    VK_LEFT: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGELEFT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_LINELEFT;
        WMHScroll(msg);
      end;
    end;
    // Up
    VK_UP: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGEUP;
        WMVScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_LINEUP;
        WMVScroll(msg);
      end;
    end;
    // Right
    VK_RIGHT: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGERIGHT;
        WMHScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_LINERIGHT;
        WMHScroll(msg);
      end;
    end;
    // Down
    VK_DOWN: begin
      if Shift = [ssCtrl] then
      begin
        msg.ScrollCode:= SB_PAGEDOWN;
        WMVScroll(msg);
      end
      else
      begin
        msg.ScrollCode:= SB_LINEDOWN;
        WMVScroll(msg);
      end;
    end;
    // Add
    VK_ADD: begin
      Zoom:= Zoom * 1.1;
    end;
    // Subtract
    VK_SUBTRACT: begin
      Zoom:= Zoom / 1.1;
    end;
    // zero
    VK_NUMPAD0, Ord('0'): begin
      if Shift = [ssCtrl] then
      ResizeToActualSize
      else if Shift = [ssShift] then
      ResizeToOptimalFit
      else
      ResizeToFit;
    end;    
  end;

end;

{-------------------------------------------------------------------------------
  MouseDown
-------------------------------------------------------------------------------}
procedure TCVImageView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer; Layer: TCustomLayer);
begin
  inherited;
  if (pboAutoFocus in Options) then
  Windows.SetFocus(Handle);

  if Shift = [ssLeft] then
  begin
    fDragging:= true;
    fDragPoint.X:= X;
    fDragPoint.Y:= Y;
  end
  else
  fDragging:= false;

  if Shift = [ssLeft, ssDouble] then
  ResizeToFit;
end;

{-------------------------------------------------------------------------------
  MouseMove
-------------------------------------------------------------------------------}
procedure TCVImageView.MouseMove(Shift: TShiftState; X, Y: Integer; Layer:
    TCustomLayer);
var
  rangeSize: TFloatPoint;
begin
  inherited;
  if fDragging then
  begin
    rangeSize:= GetViewRangeSize;

    fAnchorOffset.X:= fOffset.X / rangeSize.X;
    fAnchorOffset.Y:= fOffset.Y / rangeSize.Y;
    fAnchorOffset.X:=  fAnchorOffset.X - ((X - fDragPoint.X) / rangeSize.X);
    fAnchorOffset.Y:=  fAnchorOffset.Y - ((Y - fDragPoint.Y) / rangeSize.Y);
    fAnchor.X:= 0;
    fAnchor.Y:= 0;

    DoPanAndZoom;
    UpdateScrollbars;
    fDragPoint.X:= X;
    fDragPoint.Y:= Y;
  end;
end;

{-------------------------------------------------------------------------------
  MouseUp
-------------------------------------------------------------------------------}
procedure TCVImageView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer; Layer: TCustomLayer);
begin
  inherited;
  fDragging:= false;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
procedure TCVImageView.OpenFile(AFilePath: WideString; Async: Boolean = true;
    ResizeToFit: Boolean = true);
var
  task: TCVImageViewTask;
  t: Cardinal;
  Ext: WideString;
begin
  t:= GetTickCount;
  if t <= fLastTaskTick then
  fLastTaskTick:= fLastTaskTick + 1
  else
  fLastTaskTick:= t;

  // abort previous tasks
  Abort;

  if WideFileExists(AFilePath) then
  begin
    fActiveFilePath:= AFilePath;
    if Async then
    begin
      Ext:= WideLowerCase(WideExtractFileExt(fActiveFilePath));
      fSupportsOptimizedLoad:= fLoadOptimalSize and ((Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.jif') or (Ext = '.jpe') or (Ext = '.wmf') or (Ext = '.emf'));
      // Thumbnail
      if fUseThumbnailPreview and not fSupportsOptimizedLoad then
      begin
        task:= TCVImageViewTask.Create;
        task.FilePath:= fActiveFilePath;
        task.BackgroundColor:= Color;
        task.OptimalSize:= false;
        task.ClientSize:= Self.ClientRect.BottomRight;
        task.DoResizeToFit:= true;
        task.Thumbnail:= true;
        task.StartTick:= fLastTaskTick;
        ChangeStatus(ivsLoading);
        GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleTaskExecute, HandleTaskDone);
      end;

      // Optimized/Full
      task:= TCVImageViewTask.Create;
      task.FilePath:= AFilePath;
      task.BackgroundColor:= Color;
      task.OptimalSize:= fSupportsOptimizedLoad;
      task.ClientSize:= Self.ClientRect.BottomRight;
      task.DoResizeToFit:= ResizeToFit;
      task.Thumbnail:= false;
      task.StartTick:= fLastTaskTick + 1;
      ChangeStatus(ivsLoading);
      GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleTaskExecute, HandleTaskDone);
    end
    else
    begin
      ChangeStatus(ivsLoading);
      try
        Bitmap.LoadFromFile(AFilePath);
      finally
        ChangeStatus(ivsLoaded);
      end;
      fFullSize:= Bitmap.BoundsRect.BottomRight;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ResizeToActualSize
-------------------------------------------------------------------------------}
procedure TCVImageView.ResizeToActualSize;
begin
  if fAdaptiveZoom then
  begin
    fAutoResizeToFit:= false;
    fAutoResizeToOptimalFit:= false;
  end;
  if fSupportsOptimizedLoad then
  fScale:= fFullSize.X / Bitmap.Width
  else
  fScale:= 1;
  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  ResizeToFit
-------------------------------------------------------------------------------}
procedure TCVImageView.ResizeToFit;
begin
  if fAdaptiveZoom then
  begin
    fAutoResizeToFit:= true;
    fAutoResizeToOptimalFit:= false;
  end
  else
  fScale:= GetResizeToFitScale;
  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  ResizeToOptimalFit
-------------------------------------------------------------------------------}
procedure TCVImageView.ResizeToOptimalFit;
begin
  if fAdaptiveZoom then
  begin
    fAutoResizeToFit:= false;
    fAutoResizeToOptimalFit:= true;
  end
  else
  begin
    fScale:= GetResizeToFitScale;
    if fScale > 1 then
    fScale:= 1;
  end;

  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  Set AdaptiveZoom
-------------------------------------------------------------------------------}
procedure TCVImageView.SetAdaptiveZoom(const Value: Boolean);
begin
  if fAdaptiveZoom <> Value then
  begin
    fAdaptiveZoom:= Value;
    if not fAdaptiveZoom then
    begin
      fAutoResizeToFit:= false;
      fAutoResizeToOptimalFit:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set AutoResizeToFit
-------------------------------------------------------------------------------}
procedure TCVImageView.SetAutoResizeToFit(const Value: Boolean);
begin
  if fAutoResizeToFit <> Value then
  begin
    fAutoResizeToFit:= Value;
    if fAutoResizeToFit then
    ResizeToFit;
  end;
end;

{-------------------------------------------------------------------------------
  Set AutoResizeToOptimalFit
-------------------------------------------------------------------------------}
procedure TCVImageView.SetAutoResizeToOptimalFit(const Value: Boolean);
begin
  if fAutoResizeToOptimalFit <> Value then
  begin
    fAutoResizeToOptimalFit:= Value;
    if fAutoResizeToOptimalFit then
    ResizeToOptimalFit;
  end;
end;

{-------------------------------------------------------------------------------
  Set ScaleUpResampler
-------------------------------------------------------------------------------}
procedure TCVImageView.SetScaleUpResampler(const Value: TCVImageResampler);
begin
  if fScaleUpResampler <> Value then
  begin
    fScaleUpResampler:= Value;
    ChangeResampler;
  end;
end;

{-------------------------------------------------------------------------------
  Set ScaleDownResampler
-------------------------------------------------------------------------------}
procedure TCVImageView.SetScaleDownResampler(const Value: TCVImageResampler);
begin
  if fScaleDownResampler <> Value then
  begin
    fScaleDownResampler:= Value;
    ChangeResampler;
  end;
end;

{-------------------------------------------------------------------------------
  Set LoadOptimalSize
-------------------------------------------------------------------------------}
procedure TCVImageView.SetLoadOptimalSize(const Value: Boolean);
begin
  if Value <> fLoadOptimalSize then
  begin
    fLoadOptimalSize:= Value;
    if fLoadOptimalSize then
    DoLoadOptimized;
  end;
end;

{-------------------------------------------------------------------------------
  Set Zoom
-------------------------------------------------------------------------------}
procedure TCVImageView.SetZoom(const Value: Extended);
var
  rangeSize: TFloatPoint;
  e: Extended;
begin
  e:= Value * 0.01;
  if e <> fScale then
  begin
    if fAdaptiveZoom then
    begin
      fAutoResizeToFit:= false;
      fAutoResizeToOptimalFit:= false;
    end;
    fScale:= e;

    // Zoom to center
    rangeSize:= GetViewRangeSize;
    fAnchor.X:= ClientWidth / 2;
    fAnchor.Y:= ClientHeight / 2;
    fAnchorOffset.X:= (fOffset.X + fAnchor.X) / rangeSize.X;
    fAnchorOffset.Y:= (fOffset.Y + fAnchor.Y) / rangeSize.Y;

    DoPanAndZoom;
    UpdateScrollbars;
  end;
end;

{-------------------------------------------------------------------------------
  Update Scrollbars
-------------------------------------------------------------------------------}
procedure TCVImageView.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
  vport: TFloatRect;
  rangeSize: TFloatPoint;
begin
  if (ClientWidth = 0) or (ClientHeight = 0) then
  Exit;
  
  ScrollInfo.cbSize:= SizeOf(ScrollInfo);
  ScrollInfo.fMask:= SIF_ALL;

  vport:= GetViewport;
  rangeSize:= GetViewRangeSize;

  // Horizontal bar
  ScrollInfo.nMin:= 0;
  ScrollInfo.nMax:= ClientWidth;
  if (rangeSize.X > Width) and (rangeSize.X > 0) then
  begin
    ScrollInfo.nPage:= Round(ClientWidth * (ClientWidth / rangeSize.X)) + 1;
    ScrollInfo.nPos:= Round((ClientWidth / rangeSize.X) * vport.Left);
  end
  else
  ScrollInfo.nPage:= ClientWidth+1; // hides scrollbar

  SetScrollInfo(Self.Handle, SB_HORZ, ScrollInfo, true);

  // Vertical bar
  ScrollInfo.nMin:= 0;
  ScrollInfo.nMax:= ClientHeight;
  if (rangeSize.Y > Height) and (rangeSize.Y > 0) then
  begin
    ScrollInfo.nPage:= Round(ClientHeight * (ClientHeight / rangeSize.Y)) + 1;
    ScrollInfo.nPos:= Round((ClientHeight / rangeSize.Y) * vport.Top);
  end
  else
  ScrollInfo.nPage:= ClientHeight+1; // hides scrollbar
  SetScrollInfo(Self.Handle, SB_VERT, ScrollInfo, true); 
end;

{-------------------------------------------------------------------------------
  WMHScroll
-------------------------------------------------------------------------------}
procedure TCVImageView.WMHScroll(var Message: TWMHScroll);
var
  w: Extended;
begin
  Message.Result:= 0;

  // reset anchors
  fAnchor.X:= 0;
  w:= Self.Bitmap.Width * Self.Scale;
  fAnchorOffset.X:= fOffset.X / w;
  w:= ClientWidth / w;

  case Message.ScrollCode of
    SB_LEFT: fAnchorOffset.X:= 0;
    SB_LINELEFT: fAnchorOffset.X:= fAnchorOffset.X - 0.01;
    SB_LINERIGHT: fAnchorOffset.X:= fAnchorOffset.X + 0.01;
    SB_PAGELEFT: fAnchorOffset.X:= fAnchorOffset.X - w;
    SB_PAGERIGHT: fAnchorOffset.X:= fAnchorOffset.X + w;
    SB_RIGHT: fAnchorOffset.X:= 1;
    SB_THUMBTRACK:
      begin
        fAnchorOffset.X:= Message.Pos / ClientWidth;
      end;
    else
    Exit;
  end;
  
  DoPanAndZoom;
  UpdateScrollbars;
end;

{-------------------------------------------------------------------------------
  WMVScroll
-------------------------------------------------------------------------------}
procedure TCVImageView.WMVScroll(var Message: TWMVScroll);
var
  h: Extended;
begin
  Message.Result := 0;

  // reset anchors
  fAnchor.Y:= 0;
  h:= Self.Bitmap.Height * Self.Scale;
  fAnchorOffset.Y:= fOffset.Y / h;
  h:= ClientHeight / h;

  case Message.ScrollCode of
    SB_TOP: fAnchorOffset.Y:= 0;
    SB_LINEUP: fAnchorOffset.Y:= fAnchorOffset.Y - 0.01;
    SB_LINEDOWN: fAnchorOffset.Y:= fAnchorOffset.Y + 0.01;
    SB_PAGEUP: fAnchorOffset.Y:= fAnchorOffset.Y - h;
    SB_PAGEDOWN: fAnchorOffset.Y:= fAnchorOffset.Y + h;
    SB_BOTTOM: fAnchorOffset.Y:= 1;
    SB_THUMBTRACK:
      begin
        fAnchorOffset.Y:= Message.Pos / ClientHeight;
      end;
    else
    Exit;
  end;
  DoPanAndZoom;
  UpdateScrollbars;
end;

{##############################################################################}
// TCVImageViewPanel

{-------------------------------------------------------------------------------
  Create an instance of TCVImageViewPanel
-------------------------------------------------------------------------------}
constructor TCVImageViewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  fView:= TCVImageView.Create(Self);
  fView.Parent:= Self;
  fView.Align:= alClient;
end;

{-------------------------------------------------------------------------------
  Destroy TCVImageViewPanel
-------------------------------------------------------------------------------}
destructor TCVImageViewPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCVImageViewPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  MessageBox(0, 'test', '', MB_ICONWARNING or MB_OK);
end;

{##############################################################################}
// TCVImageViewTask

{-------------------------------------------------------------------------------
  Destroy TCVImageViewTask
-------------------------------------------------------------------------------}
destructor TCVImageViewTask.Destroy;
begin
  FilePath:= '';
  if assigned(Bitmap) then
  FreeAndNil(Bitmap);
  inherited;
end;

end.                                                                                 
