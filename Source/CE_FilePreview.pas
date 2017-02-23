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
//  The Original Code is CE_FilePreview.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FilePreview;

interface

uses
  // CubicCore
  ccThreadUtils,
  // GraphicEx
  GraphicEx,
  // Delphi units
  Windows, Controls, Graphics, SysUtils, Classes, ExtCtrls, Messages;

type

{-------------------------------------------------------------------------------
  TCEFilePreviewTask
-------------------------------------------------------------------------------}
  TCEFilePreviewTaskType = (fpttThumbnail, fpttInfo, fpttIconIndex);

  TCEFilePreviewTask = class(TObject)
  protected
    fStartTick: Cardinal;
  public
    BackgroundColor: TColor;
    FilePath: WideString;
    Thumbnail: TBitmap;
    ThumbSize: TPoint;
    Info: WideString;
    IconIndex: Integer;
    TaskType: TCEFilePreviewTaskType;
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------
  TCEFilePreview
-------------------------------------------------------------------------------}
  TCEInfoAlign = (iaTop, iaBottom, iaAutoLeft, iaAutoRight);

  TCEFilePreview = class(TCustomControl)
  protected
    fBorderSize: Integer;
    fFileIconIndex: Integer;
    fFileInfo: WideString;
    fFilePath: WideString;
    fInfoAlign: TCEInfoAlign;
    fInfoBuffer: TBitmap;
    fLastAbort: Cardinal;
    fResizeTimer: TTimer;
    fShowIcon: Boolean;
    fShowInformation: Boolean;
    fShowThumbnail: Boolean;
    fTaskTag: Integer;
    fThumbBuffer: TBitmap;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure HandleExecuteTask(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer); virtual;
    procedure DoFetchInformation; virtual;
    procedure DoFetchThumbnail; virtual;
    procedure DoUpdateInfoBuffer; virtual;
    procedure DoFetchIconIndex; virtual;
    function GetInfoArea: TRect; virtual;
    function GetThumbnailArea: TRect; virtual;
    procedure HandleResizeTimer(Sender: TObject); virtual;
    procedure HandleTaskDone(Sender: TObject; AObject: TObject; AData: Pointer;
        ATag: Integer); virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetInfoAlign(const Value: TCEInfoAlign);
    procedure SetShowIcon(const Value: Boolean);
    procedure SetShowInformation(const Value: Boolean);
    procedure SetShowThumbnail(const Value: Boolean);
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure OpenFile(AFilePath: WideString); virtual;
    property InfoAlign: TCEInfoAlign read fInfoAlign write SetInfoAlign;
  published
    property BorderSize: Integer read fBorderSize write fBorderSize;
    property ShowInformation: Boolean read fShowInformation write
        SetShowInformation;
    property ShowThumbnail: Boolean read fShowThumbnail write SetShowThumbnail;
    property TaskTag: Integer read fTaskTag write fTaskTag;
    property Color;
    property Font;
    property ShowIcon: Boolean read fShowIcon write SetShowIcon;
    property OnClick;
    property PopupMenu;
  end;

implementation

uses
  MPShellUtilities, MPShellTypes, ActiveX, MPCommonUtilities, SpTBXSkins,
  ccClasses, VirtualThumbnails, Math, SpTBXControls, MPCommonObjects,
  CV_ImageView;

{##############################################################################}
// TCEFilePreview

constructor TCEFilePreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // initialize values
  fTaskTag:= GlobalTaskPool.GetUniqueTagID;
  fShowInformation:= true;
  fShowThumbnail:= true;
  fShowIcon:= true;
  Self.ParentColor:= false;
  Self.Color:= clWindow;
  fBorderSize:= 6;
  fInfoAlign:= iaBottom;
  fFileIconIndex:= -1;
  fLastAbort:= 0;
  // create instances
  fResizeTimer:= TTimer.Create(Self);
  fResizeTimer.Interval:= 250;
  fResizeTimer.OnTimer:= HandleResizeTimer;
end;

{-------------------------------------------------------------------------------
  Destroy TCEFilePreview
-------------------------------------------------------------------------------}
destructor TCEFilePreview.Destroy;
begin
  // abort tasks
  GlobalTaskPool.AbortTasksWithTag(fTaskTag);
  // free instances
  if assigned(fInfoBuffer) then
  FreeAndNil(fInfoBuffer);
  if assigned(fThumbBuffer) then
  FreeAndNil(fThumbBuffer);
  inherited;
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCEFilePreview.AssignTo(Dest: TPersistent);
var
  preview: TCEFilePreview;
begin
  if Dest is TCEFilePreview then
  begin
    preview:= TCEFilePreview(Dest);
    preview.fBorderSize:= fBorderSize;
    preview.fInfoAlign:= fInfoAlign;
    preview.Color:= Color;
    preview.Font.Assign(Font);
    preview.fShowIcon:= fShowIcon;
    preview.fShowInformation:= fShowInformation;
    preview.fShowThumbnail:= fShowThumbnail;
    preview.fFileIconIndex:= fFileIconIndex;
    preview.fFileInfo:= fFileInfo;
    preview.fFilePath:= fFilePath;
    // create buffers
    if assigned(fInfoBuffer) then
    begin
      preview.fInfoBuffer:= TBitmap.Create;
      preview.fInfoBuffer.Assign(fInfoBuffer);
    end;
    if assigned(fThumbBuffer) then
    begin
      preview.fThumbBuffer:= TBitmap.Create;
      preview.fThumbBuffer.Assign(fThumbBuffer);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEFilePreview.Clear;
begin
  if assigned(fInfoBuffer) then
  FreeAndNil(fInfoBuffer);
  if assigned(fThumbBuffer) then
  FreeAndNil(fThumbBuffer);
  fFileIconIndex:= -1;
  Paint;
end;

{-------------------------------------------------------------------------------
  Handle CreateParams
-------------------------------------------------------------------------------}
procedure TCEFilePreview.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
//    with Params.WindowClass do
//      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{-------------------------------------------------------------------------------
  Do Execute Task (runs in a separate thread!!!)
-------------------------------------------------------------------------------}
procedure TCEFilePreview.HandleExecuteTask(Sender: TCCTaskPoolThread; AObject:
    TObject; AData: Pointer; ATag: Integer);
var
  ns: TNamespace;
  task: TCEFilePreviewTask;
  w,h: Integer;
  s: TStream;
begin
  if (ATag = fTaskTag) and (AObject is TCEFilePreviewTask) then
  begin
    task:= TCEFilePreviewTask(AObject);
    try
      CoInitialize(nil);
      ns:= TNamespace.CreateFromFileName(task.FilePath);
      try
        // thumbnail
        if task.TaskType = fpttThumbnail then
        begin
          task.Thumbnail:= LoadThumbnail(task.FilePath,
                                         task.ThumbSize.X,task.ThumbSize.Y,
                                         task.BackgroundColor,true);
        end
        // info
        else if task.TaskType = fpttInfo then
        begin
          task.Info:= ns.NameParseAddressInFolder + #13#10 + ns.InfoTip;
        end
        // icon index
        else if task.TaskType = fpttIconIndex then
        begin
          task.IconIndex:= ns.GetIconIndex(true, icLarge);
        end;
      finally
        ns.Free;
        CoUninitialize;
      end;
    except
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do Task Done
  - Bitmap buffers are created here.
-------------------------------------------------------------------------------}
procedure TCEFilePreview.HandleTaskDone(Sender: TObject; AObject: TObject;
    AData: Pointer; ATag: Integer);
var
  task: TCEFilePreviewTask;
begin
  if (ATag = fTaskTag) and (AObject is TCEFilePreviewTask) then
  begin
    task:= TCEFilePreviewTask(AObject);

    // disregard old tasks
    if task.fStartTick < fLastAbort then
    Exit;
    
    // Info
    if task.TaskType = fpttInfo then
    begin
      fFileInfo:= task.Info;
      if fShowInformation then
      begin
        if not assigned(fInfoBuffer) then
        fInfoBuffer:= TBitmap.Create;
        DoUpdateInfoBuffer;
      end;
    end
    // Thumbnail
    else if (task.TaskType = fpttThumbnail) and assigned(task.Thumbnail) then
    begin
      // free previous thumbnail
      if assigned(fThumbBuffer) then
      FreeAndNil(fThumbBuffer);
      // assign new thumbnail
      fThumbBuffer:= task.Thumbnail;
      task.Thumbnail:= nil; // set to nil, otherwise the thumbnail will be freed on task destroy
    end
    else if (task.TaskType = fpttIconIndex) then
    begin
      fFileIconIndex:= task.IconIndex;
    end;
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Do FetchInformation
-------------------------------------------------------------------------------}
procedure TCEFilePreview.DoFetchInformation;
var
  task: TCEFilePreviewTask;
begin
  if fFilePath <> '' then
  begin
    task:= TCEFilePreviewTask.Create;
    task.FilePath:= fFilePath;
    task.TaskType:= fpttInfo;
    task.fStartTick:= Max(GetTickCount, fLastAbort);
    GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleExecuteTask, HandleTaskDone);
  end;
end;

{-------------------------------------------------------------------------------
  Do FetchThumbnail
-------------------------------------------------------------------------------}
procedure TCEFilePreview.DoFetchThumbnail;
var
  task: TCEFilePreviewTask;
begin
  if fFilePath <> '' then
  begin
    task:= TCEFilePreviewTask.Create;
    task.FilePath:= fFilePath;
    task.ThumbSize:= Point(ClientWidth, ClientHeight);
    task.BackgroundColor:= Color;
    task.TaskType:= fpttThumbnail;
    task.fStartTick:= Max(GetTickCount, fLastAbort);
    GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleExecuteTask, HandleTaskDone);
  end;
end;

{-------------------------------------------------------------------------------
  Do FetchIconIndex
-------------------------------------------------------------------------------}
procedure TCEFilePreview.DoFetchIconIndex;
var
  task: TCEFilePreviewTask;
begin
  if fFilePath <> '' then
  begin
    task:= TCEFilePreviewTask.Create;
    task.FilePath:= fFilePath;
    task.TaskType:= fpttIconIndex;
    task.fStartTick:= Max(GetTickCount, fLastAbort);
    GlobalTaskPool.AddTask(nil, task, true, true, fTaskTag, HandleExecuteTask, HandleTaskDone);
  end;
end;

{-------------------------------------------------------------------------------
  Do UpdateInfoBuffer
-------------------------------------------------------------------------------}
procedure TCEFilePreview.DoUpdateInfoBuffer;
var
  r: TRect;
  list: TCCStrings;
  i,h,w,c: Integer;
  ws: WideString;
  posArray: array of TRect;
  s: TSize;
  spaceW: Integer;
  areaWidth, areaHeight: Integer;
begin
  if not assigned(fInfoBuffer) then
  Exit;

  list:= TCCStringList.Create;
  try
    if (fInfoAlign = iaTop) or (fInfoAlign = iaBottom) then
    begin
      areaWidth:= ClientWidth - (fBorderSize*2);
      areaHeight:= ClientHeight - (fBorderSize*2);
    end
    else
    begin
      r:= GetInfoArea;
      areaWidth:= r.Right-r.Left;
      areaHeight:= r.Bottom-r.Top;
    end;

    if (areaWidth < 1) or (areaHeight < 1) then
    Exit;
    
    list.NameValueSeparator:= ':';
    list.Text:= fFileInfo;

    // calculate buffer size
    w:= 0;
    h:= 0;
    c:= 0;
    spaceW:= Self.Canvas.TextWidth('  ');

    SetLength(posArray, list.Count);

    for i:= 0 to list.Count - 1 do
    begin
      list.Strings[i]:= Trim(list.Strings[i]);

      r:= ClientRect;
      if i = 0 then
      begin
        Self.Canvas.Font.Style:= [fsBold];
      end
      else
      begin
        Self.Canvas.Font.Style:= [];
      end;

      Windows.DrawTextW(Self.Canvas.Handle, PWideChar(list.Strings[i]), -1, r, DT_CALCRECT or DT_SINGLELINE);
      s.cx:= r.Right;
      s.cy:= r.Bottom;

      // file name
      if i = 0 then
      begin
        posArray[i]:= Rect(w, h, Min(areaWidth, w+s.cx), h+s.cy);
        h:= h + s.cy;
      end
      // information
      else
      begin
        if c > 0 then
        w:= w + spaceW;

        if ((w + s.cx) > (areaWidth)) and (c > 0) then
        begin
          // new line
          w:= 0;
          c:= 0;
          h:= h + s.cy;
        end;

        c:= c + 1;

        posArray[i]:= Rect(w,h,Min(areaWidth, w + s.cx),h + s.cy);

        w:= w + s.cx;
      end;
    end;

    if c > 0 then
    h:= h + s.cy;

    //if (fInfoAlign = iaTop) or (fInfoAlign = iaBottom) then
    fInfoBuffer.SetSize(areaWidth, h);

    // paint background
    fInfoBuffer.Canvas.Brush.Color:= Color;
    fInfoBuffer.Canvas.FillRect(Rect(0,0,fInfoBuffer.Width, fInfoBuffer.Height));

    // paint info text
    fInfoBuffer.Canvas.Font.Assign(Self.Canvas.Font);
    for i:= 0 to list.Count - 1 do
    begin
      if i = 0 then
      begin
        fInfoBuffer.Canvas.Font.Style:= [fsBold];
        Windows.DrawTextW(fInfoBuffer.Canvas.Handle, PWideChar(list.Strings[i]), -1, posArray[i], DT_END_ELLIPSIS or DT_SINGLELINE);
      end
      else
      begin
        fInfoBuffer.Canvas.Font.Style:= [];
        r:= posArray[i];
        if list.ValueFromIndex[i] <> '' then
        begin
          // draw name
          ws:= list.Names[i] + ':';
          Windows.DrawTextW(Self.Canvas.Handle, PWideChar(ws), -1, r, DT_CALCRECT);
          w:= r.Right;
          fInfoBuffer.Canvas.Font.Color:= clGrayText;
          Windows.DrawTextW(fInfoBuffer.Canvas.Handle, PWideChar(ws), -1, posArray[i], DT_END_ELLIPSIS);
          // draw value
          ws:= list.ValueFromIndex[i];
          r:= posArray[i];
          r.Left:= w;
          fInfoBuffer.Canvas.Font.Color:= Self.Font.Color;
          Windows.DrawTextW(fInfoBuffer.Canvas.Handle, PWideChar(ws), -1, r, DT_END_ELLIPSIS);
        end
        else
        begin
          // draw name
          ws:= list.Names[i];
          Windows.DrawTextW(Self.Canvas.Handle, PWideChar(ws), -1, r, DT_CALCRECT);
          w:= r.Right;
          fInfoBuffer.Canvas.Font.Color:= Self.Font.Color;
          Windows.DrawTextW(fInfoBuffer.Canvas.Handle, PWideChar(ws), -1, posArray[i], DT_END_ELLIPSIS);        
        end;
      end;
    end;

  finally
    list.Free;
  end;
end;

function TCEFilePreview.GetInfoArea: TRect;
begin
  if fInfoAlign = iaTop then
  Result:= Rect(fBorderSize, fBorderSize, fInfoBuffer.Width+fBorderSize, fInfoBuffer.Height+fBorderSize)
  else if fInfoAlign = iaBottom then
  Result:= Rect(fBorderSize, ClientHeight-fInfoBuffer.Height-fBorderSize, fInfoBuffer.Width+fBorderSize, ClientHeight-fBorderSize)
  else if fInfoAlign = iaAutoLeft then
  Result:= Rect(fBorderSize, fBorderSize, ClientWidth-ClientHeight-(fBorderSize*2), ClientHeight-fBorderSize)
  else if fInfoAlign = iaAutoRight then
  Result:= Rect(fBorderSize+ClientHeight+fBorderSize, fBorderSize, ClientWidth-fBorderSize, ClientHeight-fBorderSize);
end;

{-------------------------------------------------------------------------------
  Get ThumbnailArea
-------------------------------------------------------------------------------}
function TCEFilePreview.GetThumbnailArea: TRect;
var
  r: TRect;
begin
  if assigned(fInfoBuffer) then
  begin
    if fInfoAlign = iaTop then
    Result:= Rect(fBorderSize, fBorderSize+fInfoBuffer.Height+fBorderSize, ClientWidth-fBorderSize, ClientHeight-fBorderSize)
    else if fInfoAlign = iaBottom then
    Result:= Rect(fBorderSize, fBorderSize, ClientWidth-fBorderSize, ClientHeight-fBorderSize-fInfoBuffer.Height-fBorderSize)
    else if fInfoAlign = iaAutoLeft then
    Result:= Rect(ClientWidth-ClientHeight-fBorderSize, fBorderSize, ClientWidth-fBorderSize, ClientHeight-fBorderSize)
    else if fInfoAlign = iaAutoRight then
    Result:= Rect(fBorderSize, fBorderSize, ClientHeight + fBorderSize, ClientHeight-fBorderSize);
  end
  else
  begin
    Result:= ClientRect;
    InflateRect(Result, -fBorderSize, -fBorderSize);
  end;
end;

{-------------------------------------------------------------------------------
  Handle ResizeTimer
-------------------------------------------------------------------------------}
procedure TCEFilePreview.HandleResizeTimer(Sender: TObject);
begin
  fResizeTimer.Enabled:= false;
  if assigned(fThumbBuffer) then
  DoFetchThumbnail;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
procedure TCEFilePreview.OpenFile(AFilePath: WideString);
var
  task: TCEFilePreviewTask;
  info: TThumbInfo;
  ns: TNamespace;
  B: TBitmap;
  t: Cardinal;
begin
  // abort previous tasks
  GlobalTaskPool.AbortTasksWithTag(fTaskTag);
  t:= GetTickCount;
  if t = fLastAbort then
  fLastAbort:= t + 1
  else
  fLastAbort:= t;

  // free previous buffer
  if assigned(fThumbBuffer) then
  FreeAndNil(fThumbBuffer);

  // init values
  fFilePath:= AFilePath;
  fFileInfo:= '';
  fFileIconIndex:= -1;

  // start thumbnail fetching task
  if fShowThumbnail then
  DoFetchThumbnail;
  // start icon index fetching task
  if fShowIcon then
  DoFetchIconIndex;
  // start information fetching task
  if fShowInformation then
  DoFetchInformation;
end;

{-------------------------------------------------------------------------------
  Paint
-------------------------------------------------------------------------------}
procedure TCEFilePreview.Paint;
var
  buf: TBitmap;
  ar, size: Real;
  r: TRect;
  i, w, h: Integer;
begin
  buf:= TBitmap.Create;
  try
    buf.PixelFormat:= pf32bit;
    buf.SetSize(ClientWidth, ClientHeight);
    // paint background
    buf.Canvas.Brush.Color:= Color;
    buf.Canvas.FillRect(ClientRect);

    // draw thumbnail
    if assigned(fThumbBuffer) and not ((fThumbBuffer.Width = 0) or (fThumbBuffer.Height = 0)) then
    begin
      r:= GetThumbnailArea;
      w:= r.Right - r.Left;
      h:= r.Bottom - r.Top;
      if (w > 0) and (h > 0) then
      begin
        ar:= fThumbBuffer.Width / fThumbBuffer.Height;
        if ar > (w / h) then
        begin
          size:= w / ar;
          r.Top:= r.Top + Round((h - size) / 2);
          r.Bottom:= Round(r.Top + size);
        end
        else
        begin
          size:= h * ar;
          r.Left:= r.Left + Round((w - size) / 2);
          r.Right:= Round(r.Left + size);
        end;
        // blt to buf
        SetStretchBltMode(buf.Canvas.Handle, STRETCH_HALFTONE);
        SetBrushOrgEx(buf.Canvas.Handle, 0, 0, nil);
        StretchBlt(buf.Canvas.Handle, r.Left, r.Top, r.Right-r.Left, r.Bottom-r.Top,
                   fThumbBuffer.Canvas.Handle, 0,0,fThumbBuffer.Width, fThumbBuffer.Height,
                   SRCCopy);
      end;
    end
    // draw icon
    else if fFileIconIndex > -1 then
    begin
      r:= GetThumbnailArea;
      r.Left:= r.Left + Round( ((r.Right-r.Left) - ExtraLargeSysImages.Width) / 2);
      r.Top:= r.Top + Round( ((r.Bottom-r.Top) - ExtraLargeSysImages.Width) / 2);
      ExtraLargeSysImages.Draw(buf.Canvas, r.Left, r.Top, fFileIconIndex);
    end;

    // draw info
    if assigned(fInfoBuffer) then
    begin
      r:= GetInfoArea;
      buf.Canvas.Draw(r.Left,r.Top,fInfoBuffer);
      //buf.Canvas.StretchDraw(GetInfoArea, fInfoBuffer);
    end;

    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, buf.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    buf.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Resize
-------------------------------------------------------------------------------}
procedure TCEFilePreview.Resize;
begin
  inherited;
  if assigned(fThumbBuffer) then
  begin
    fResizeTimer.Enabled:= false;
    fResizeTimer.Enabled:= true;
  end;
  DoUpdateInfoBuffer;
end;

{-------------------------------------------------------------------------------
  Set InfoAlign
-------------------------------------------------------------------------------}
procedure TCEFilePreview.SetInfoAlign(const Value: TCEInfoAlign);
begin
  if fInfoAlign <> Value then
  begin
    fInfoAlign:= Value;
    Resize;
  end;
end;

procedure TCEFilePreview.SetShowIcon(const Value: Boolean);
begin
  if fShowIcon <> Value then
  begin
    fShowIcon:= Value;
    if fShowIcon then
    DoFetchIconIndex;
  end;
end;

{-------------------------------------------------------------------------------
  Set ShowInformation
-------------------------------------------------------------------------------}
procedure TCEFilePreview.SetShowInformation(const Value: Boolean);
begin
  if fShowInformation <> Value then
  begin
    fShowInformation:= Value;
    if fShowInformation then
    begin
      DoFetchInformation;
    end
    else
    begin

    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set ShowThumbnail
-------------------------------------------------------------------------------}
procedure TCEFilePreview.SetShowThumbnail(const Value: Boolean);
begin
  if fShowThumbnail <> Value then
  begin
    fShowThumbnail:= Value;
    if fShowThumbnail then
    DoFetchThumbnail;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_EraseBkgnd message (Don't erase background)
-------------------------------------------------------------------------------}
procedure TCEFilePreview.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;

{##############################################################################}
// TCEFilePreviewTask

{-------------------------------------------------------------------------------
  Destroy TCEFilePreviewTask
-------------------------------------------------------------------------------}
destructor TCEFilePreviewTask.Destroy;
begin
  if assigned(Thumbnail) then
  FreeAndNil(Thumbnail);
  inherited Destroy;
end;

end.
