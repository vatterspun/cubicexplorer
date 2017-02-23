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
//  The Original Code is CE_InfoBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_InfoBar;

interface

uses
  // CE Units
  CE_LanguageEngine, CE_Utils,
  // SpTBX
  SpTBXSkins, SpTBXItem, SpTBXControls,
  // TNT
  TntGraphics, TntClasses,
  // VSTools
  MPShellUtilities, MPShellTypes, MPCommonObjects,
  // System Units
  ExtCtrls, Classes, Controls, Windows, Graphics, Messages, Contnrs,
  Forms, Math, SysUtils, ShlObj, ActiveX, UxTheme, Themes, GraphicEx;

type
  TCEIconSize = (itSmallIcon, itLargeIcon, itExtraLargeIcon, itJumboIcon);

  TCEInfoBar = class;

  // Thumbnail Thread
  TCEThumbLoadThread = class(TThread)
  protected
    InfoBar: TCEInfoBar;
    procedure SyncFinished; virtual;
    procedure Execute; override;
  public
    PIDL: PItemIDList;
    Thumbnail: TBitmap;
    ThumbnailFound: Boolean;
    ThumbnailSize: TSize;
  end;

  // InfoQuery Thread
  TCEInfoLoadThread = class(TThread)
  protected
    InfoBar: TCEInfoBar;
    procedure SyncFinished; virtual;
    procedure Execute; override;
  public
    PIDL: PItemIDList;
    Info: WideString;
    InfoFound: Boolean;
    CalculateHiddenItems: Boolean;
    ShowFolderItemCount: Boolean;
    ShowInfoTip: Boolean;
  end;  

  // InfoBar
  TCEInfoBar = class(TCustomControl)
  private
    fAutoRefreshThumbnail: Boolean;
    fCalculateHiddenItems: Boolean;
    fRowHeight: Integer;
    fShowFolderItemCount: Boolean;
    fUseJumboIcons: Boolean;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    fBuffer: TBitmap;
    fIconBorderSize: Integer;
    fIconHeight: Integer;
    fIconRect: TRect;
    fIconSize: TCEIconSize;
    fIconWidth: Integer;
    fInfoFound: Boolean;
    fInfoList: TObjectList;
    fInfoRect: TRect;
    fLatestNS: TNamespace;
    fLatestThumbThread: TCEThumbLoadThread;
    fLatestInfoThread: TCEInfoLoadThread;
    fMyComputerPIDL: PItemIDList;
    fSelectionCount: Integer;
    fThumbHeight: Integer;
    fThumbnailBuffer: TBitmap;
    fThumbnailFound: Boolean;
    fThumbWidth: Integer;
    procedure BuildInfoList(AInfo: WideString = ''); virtual;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBuffer; virtual;
    procedure DrawIcon(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawThumbnail(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawInfoList(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure RunThumbnailThread; virtual;
    procedure RunInfoThread; virtual;
    procedure ThumbThreadFinished(AThread: TCEThumbLoadThread); virtual;
    procedure InfoThreadFinished(AThread: TCEInfoLoadThread); virtual;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    fResizedThumbnail: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddInfoItem(AText: WideString; APrefix: WideString): Integer; virtual;
    procedure Clear;
    procedure LoadFromPIDL(APIDL: PItemIDList; ASelectionCount: Integer = 1);
        virtual;
    procedure Paint; override;
    procedure RefreshThumbnail;
    procedure Reload;
    procedure Resize; override;
    property AutoRefreshThumbnail: Boolean read fAutoRefreshThumbnail write
        fAutoRefreshThumbnail;
    property CalculateHiddenItems: Boolean read fCalculateHiddenItems write
        fCalculateHiddenItems;
    property Font;
    property LatestNS: TNamespace read fLatestNS;
    property RowHeight: Integer read fRowHeight write fRowHeight;
    property ShowFolderItemCount: Boolean read fShowFolderItemCount write
        fShowFolderItemCount;
    property UseJumboIcons: Boolean read fUseJumboIcons write fUseJumboIcons;
  end;

  TCustomControlHack = class(TCustomControl);

  TCEInfoItem = class(TObject)
  private    
    fPrefix: WideString;
  public
    fText: WideString;
    ItemRect: TRect;
    constructor Create;
    destructor Destroy; override;
    property Prefix: WideString read fPrefix write fPrefix;
    property Text: WideString read fText write fText;
  end;

implementation

uses
  CE_VistaFuncs;

{-------------------------------------------------------------------------------
  Create an instance of TCEInfoBar
-------------------------------------------------------------------------------}
constructor TCEInfoBar.Create(AOwner: TComponent);
begin
  inherited;
  SetDesktopIconFonts(Canvas.Font);
  fRowHeight:= 18;
  fIconBorderSize:= 6;
  fThumbnailBuffer:= TBitmap.Create;
  fBuffer:= TBitmap.Create;
  fInfoList:= TObjectList.Create(true);
  fAutoRefreshThumbnail:= false;
  fShowFolderItemCount:= true;
  fCalculateHiddenItems:= false;
  fUseJumboIcons:= false;
  Resize;
  SkinManager.AddSkinNotification(Self);
  SHGetSpecialFolderLocation(Application.MainFormHandle, CSIDL_DRIVES, fMyComputerPIDL); 
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoBar
-------------------------------------------------------------------------------}
destructor TCEInfoBar.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  if assigned(fLatestNS) then
  fLatestNS.Free;
  fThumbnailBuffer.Free;
  fBuffer.Free;
  fInfoList.Free;
  PIDLMgr.FreePIDL(fMyComputerPIDL);
  inherited;
end;

{-------------------------------------------------------------------------------
  Add new Info Item
-------------------------------------------------------------------------------}
function TCEInfoBar.AddInfoItem(AText: WideString; APrefix: WideString):
    Integer;
var
  item: TCEInfoItem;
begin
  item:= TCEInfoItem.Create;
  item.Text:= AText;
  item.Prefix:= APrefix;
  Result:= fInfoList.Add(item);
end;

{-------------------------------------------------------------------------------
  Build InfoList
-------------------------------------------------------------------------------}
procedure TCEInfoBar.BuildInfoList(AInfo: WideString = '');
var
  list: TTntStrings;
  i,c: Integer;
begin
  if not assigned(fLatestNS) then
  Exit;

  list:= TTntStringList.Create;
  list.NameValueSeparator:= ':';
  try
    fInfoList.Clear;
    // Add Name
    if fSelectionCount > 1 then
    AddInfoItem(fLatestNS.NameNormal, '(' + IntToStr(fSelectionCount) + ' ' + _('Selected') + ')')
    else
    AddInfoItem(fLatestNS.NameNormal, '');

    // Add Info items
    if AInfo <> '' then
    begin
      list.Text:= AInfo;
      if (list.Count > 0) and fShowFolderItemCount and fLatestNS.Folder then
      begin
        AddInfoItem(list.ValueFromIndex[list.Count-1] + ' ' + _('Item(s)'), '');
        c:= list.Count - 2;
      end
      else
      c:= list.Count - 1;

      for i:= c downto 0 do
      begin
        if list.Names[i] = '' then
        AddInfoItem('', list.Strings[i])
        else
        AddInfoItem(Trim(list.ValueFromIndex[i]), list.Names[i]);
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Handle CanResize
-------------------------------------------------------------------------------}
function TCEInfoBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result:= NewHeight > fRowHeight;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Clear;
begin
  if assigned(fLatestThumbThread) then
  begin
    fLatestThumbThread.Terminate;
  end;
  fThumbnailBuffer.SetSize(0,0);
  if assigned(fLatestNS) then
  FreeAndNil(fLatestNS);
  fThumbnailFound:= false;
  fSelectionCount:= 0;
  fInfoList.Clear;
  DrawBuffer;
  Paint;
end;

{-------------------------------------------------------------------------------
  Handle CreateParams
-------------------------------------------------------------------------------}
procedure TCEInfoBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{-------------------------------------------------------------------------------
  Draw Buffer
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawBuffer;
var
  r: TRect;
begin
  if csDestroying in Self.ComponentState then
  Exit;

  fBuffer.Canvas.Lock;
  try
    // Set buffer size
    fBuffer.SetSize(Width,Height);

    // Paint Background
    r:= Rect(0,0, Width, Height);
    if CurrentSkin.SkinName = 'Eos' then
    fBuffer.Canvas.Brush.Color:= SkinManager.CurrentSkin.Options(skncDock).Body.Color1
    else
    fBuffer.Canvas.Brush.Color:= SkinManager.CurrentSkin.ColorBtnFace;
    fBuffer.Canvas.FillRect(r);

    //SpDrawXPMenuSeparator(fBuffer.Canvas, R, false, true);

    // Paint Thumbnail and InfoList
    if fInfoList.Count > 0 then
    begin
      // Paint Thumbnail
      if fThumbnailFound and (Height > 16) then
      begin
        DrawThumbnail(fBuffer.Canvas, fIconRect);
      end
      // Paint Icon
      else
      begin
        DrawIcon(fBuffer.Canvas, fIconRect);
      end;
    end;
    
    // Paint InfoList
    DrawInfoList(fBuffer.Canvas, fInfoRect);
  finally
    fBuffer.Canvas.Unlock;
  end;             
end;

{-------------------------------------------------------------------------------
  Draw Icon
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawIcon(ACanvas: TCanvas; ARect: TRect);
var
  bit, bit2: TBitmap;
  l,t,w,h: Integer;
  imgList: TImageList;
begin
  if assigned(fLatestNS) then
  begin
    bit:= TBitmap.Create;
    bit2:= TBitmap.Create;
    try
      // get system icon list
      case fIconSize of
        itSmallIcon: imgList:= SmallSysImages;
        itLargeIcon: imgList:= LargeSysImages;
        itExtraLargeIcon: imgList:= ExtraLargeSysImages;
        else
        imgList:= JumboSysImages;
      end;

      // Paint background
      bit.SetSize(imgList.Width, imgList.Height);
      bit.Canvas.Brush.Color:= ACanvas.Brush.Color;
      bit.Canvas.FillRect(Rect(0,0,bit.Width,bit.Height));
      imgList.Draw(bit.Canvas, 0,0,fLatestNS.GetIconIndex(false, icLarge));

      // Draw icon to buffer
      l:= ARect.Left;
      t:= ARect.Top;
      w:= ARect.Right - ARect.Left;
      h:= ARect.Bottom - ARect.Top;
      Stretch(w,h, sfLanczos3, 0, bit, bit2);
      BitBlt(ACanvas.Handle, l, t, w, h, bit2.Canvas.Handle, 0,0, SRCCOPY);
    finally
      bit.Free;
      bit2.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Draw Thumbnail
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawThumbnail(ACanvas: TCanvas; ARect: TRect);
var
  ar: Single;
  l,t,w,h: Integer;
  bit: TBitmap;
begin
  if (fThumbnailBuffer.Width = 0) or (fThumbnailBuffer.Height = 0) then
  Exit;
  // Resize thumbnail
  fThumbWidth:= ARect.Right - ARect.Left;
  fThumbHeight:= ARect.Bottom - ARect.Top;
  if fThumbnailBuffer.Width > fThumbnailBuffer.Height then
  begin
    ar:= fThumbnailBuffer.Height / fThumbnailBuffer.Width;
    fThumbHeight:= Round(fThumbWidth * ar);
  end
  else
  begin
    ar:= fThumbnailBuffer.Width / fThumbnailBuffer.Height;
    fThumbWidth:= Round(fThumbHeight * ar);
  end;
  fResizedThumbnail:= (fThumbWidth <> fThumbnailBuffer.Width) or (fThumbHeight <> fThumbnailBuffer.Height);

  // Calculate thumbnail position and size.
  w:= fThumbWidth;
  h:= fThumbHeight;
  l:= Round((ARect.Right - ARect.Left - fThumbWidth) / 2) + ARect.Left;
  t:= Round((ARect.Bottom - ARect.Top - fThumbHeight) / 2) + ARect.Top;

  // Draw thumbnail to buffer using resample filter.
  // If thumbnail is 1 pixel height, use StretchBlt instead, GraphicEx.DoStretch has a bug.
  if (fThumbnailBuffer.Height > 1) and (h > 1) then
  begin
    bit:= TBitmap.Create;
    try
      Stretch(w,h, sfLanczos3, 0, fThumbnailBuffer, bit);
      BitBlt(ACanvas.Handle, l, t, w, h, bit.Canvas.Handle, 0,0, SRCCOPY);
    finally
      bit.Free;
    end;
  end
  else
  begin
    StretchBlt(ACanvas.Handle, l, t, w, h,
               fThumbnailBuffer.Canvas.Handle, 0, 0, fThumbnailBuffer.Width, fThumbnailBuffer.Height,
               SRCCOPY);
  end;
end;

{-------------------------------------------------------------------------------
  Draw InfoList
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawInfoList(ACanvas: TCanvas; ARect: TRect);
var
  r: TRect;
  item: TCEInfoItem;
  ws: WideString;
  i: Integer;
  row, row_num: Integer;
  col, col_num: Integer;
  prefix_size, size: TSize;
  original_font_size, name_size: Integer;
begin
  // Draw InfoList
  r:= ARect;
  original_font_size:= ACanvas.Font.Size;
  name_size:= original_font_size + 1;
  if fInfoList.Count > 0 then
  begin
    // Single row
    if Height < (RowHeight * 2) then 
    begin
      // Draw Name
      ACanvas.Font.Style:= [];
      ACanvas.Font.Size:= name_size;
      ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
      item:= TCEInfoItem(fInfoList.Items[0]);
      size:= WideCanvasTextExtent(ACanvas, item.Text);
      SpDrawXPText(ACanvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

      // Draw Other infos
      ACanvas.Font.Size:= original_font_size;
      ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsDisabled);
      // selection count
      if fSelectionCount > 1 then
      begin
        r.Left:= r.Left + size.cx + 3;
        size:= WideCanvasTextExtent(ACanvas, item.Prefix);
        SpDrawXPText(ACanvas, item.Prefix, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
      end;
      // infos
      r.Left:= r.Left + size.cx + 3;
      ws:= '';
      for i:= 1 to fInfoList.Count - 1 do
      begin
        item:= TCEInfoItem(fInfoList.Items[i]);
        if item.Text <> '' then
        begin
          ws:= ws + ' | ' + item.Text;
        end
        else if item.Prefix <> '' then
        begin
          ws:= ws + ' | ' + item.Prefix;
        end;
      end;
      SpDrawXPText(ACanvas, ws, r, DT_END_ELLIPSIS  or DT_SINGLELINE or DT_VCENTER);
    end
    // Multiple rows
    else
    begin
      // Draw Name
      ACanvas.Font.Style:= [fsBold];
      ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
      ACanvas.Font.Size:= name_size;
      r.Bottom:= r.Top + RowHeight-2;
      item:= TCEInfoItem(fInfoList.Items[0]);
      SpDrawXPText(ACanvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

      if item.Prefix <> '' then
      begin
        size:= WideCanvasTextExtent(ACanvas, item.Text);
        ACanvas.Font.Style:= [];
        ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsDisabled);
        r.Left:= r.Left + size.cx + 4;
        SpDrawXPText(ACanvas, item.Prefix, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
      end;

      // Draw Other Infos
      ACanvas.Font.Style:= [];
      ACanvas.Font.Size:= original_font_size;
      r.Left:= fInfoRect.Left;
      r.Top:= r.Bottom;
      r.Bottom:= r.Top + RowHeight - 3;
      row:= 2;
      row_num:= Max(1,Height div RowHeight);
      col:= 1;
      col_num:= Ceil((fInfoList.Count - 1) / row_num);
      if col_num < 1 then
      col_num:= 1;

      for i:= 1 to fInfoList.Count - 1 do
      begin
        item:= TCEInfoItem(fInfoList.Items[i]);
        if (item.Text <> '') or (item.Prefix <> '') then
        begin
          // get text sizes
          size:= WideCanvasTextExtent(ACanvas, item.Text);
          if item.Prefix <> '' then
          begin
            prefix_size:= WideCanvasTextExtent(ACanvas, item.Prefix + ':');
            prefix_size.cx:= prefix_size.cx + 2;
          end
          else
          prefix_size.cx:= 0;

          // go to row
          if (((r.Right - r.Left) < (size.cx + prefix_size.cx)) and (row < row_num) and (col_num > 1))
             or ((col > col_num) and (row <= row_num)) then
          begin
            if ((fInfoRect.Bottom - fInfoRect.Top) + Max(prefix_size.cy, size.cy)) >= (((row+1) * RowHeight)) then
            begin
              row:= row + 1;
              col:= 1;
              r.Left:= fInfoRect.Left;
              r.Top:= r.Bottom;
              r.Bottom:= r.Top + RowHeight - 3;
            end;
          end;

          // draw prefix
          ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsDisabled);
          if prefix_size.cx > 0 then
          begin
            if size.cx > 0 then
            SpDrawXPText(ACanvas, item.Prefix + ':', r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER)
            else
            SpDrawXPText(ACanvas, item.Prefix, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
            r.Left:= r.Left + prefix_size.cx;
          end;

          // draw text
          ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
          if size.cx > 0 then
          begin
            SpDrawXPText(ACanvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
          end;
          r.Left:= r.Left + size.cx + 10;
          col:= col + 1;
        end;
      end;
    end;
  end
  else
  begin
    // Draw "No Selection" notify.
    ACanvas.Font.Size:= name_size;
    ACanvas.Font.Color:= CurrentSkin.GetTextColor(skncToolbarItem, sknsDisabled);
    if Height < (RowHeight + 10) then
    begin
      r.Left:= 10;
      r.Top:= 3;
    end
    else
    begin
      r.Left:= 10;
      r.Top:= 10;
    end;
    SpDrawXPText(ACanvas, _('No Selection'), r, DT_END_ELLIPSIS);
  end;
  
  ACanvas.Font.Size:= original_font_size;
end;

{-------------------------------------------------------------------------------
  Load From PIDL
-------------------------------------------------------------------------------}
procedure TCEInfoBar.LoadFromPIDL(APIDL: PItemIDList; ASelectionCount: Integer
    = 1);
begin
  if csDestroying in Self.ComponentState then
  exit;

  fSelectionCount:= ASelectionCount;

  // Terminate previous threads if present
  if assigned(fLatestThumbThread) then
  fLatestThumbThread.Terminate;
  if assigned(fLatestInfoThread) then
  fLatestInfoThread.Terminate;
  
  if assigned(APIDL) then
  begin
    // Create namespace
    if assigned(fLatestNS) then
    fLatestNS.Free;
    fLatestNS:= TNamespace.Create(PIDLMgr.CopyPIDL(APIDL), nil);
    fLatestNS.FreePIDLOnDestroy:= true;

    // Build InfoList
    fInfoFound:= false;
    BuildInfoList;
    RunInfoThread;
    
    // Draw normal Icon first
    fThumbnailFound:= false;
    Resize;
    //DrawBuffer;
    Self.Repaint;

    RunThumbnailThread;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Paint
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Paint;
begin
  if csDestroying in Self.ComponentState then
  Exit;

  BitBlt(Canvas.Handle, 0, 0, Width, Height, fBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

{-------------------------------------------------------------------------------
  Refresh Thumbnail
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RefreshThumbnail;
begin
  if (Height > 16) and ((fThumbHeight > fThumbnailBuffer.Height) or (fThumbWidth > fThumbnailBuffer.Width)) then
  RunThumbnailThread;
end;

{-------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Resize;
var
  h: Integer;
begin
  inherited;
  if csDestroying in Self.ComponentState then
  Exit;

  h:= fIconBorderSize * 2;
  // Get Icon Size
  if (Height <= (h + 16)) then // Small Icon (16px).
  fIconSize:= itSmallIcon
  else if (Height > (h + 16)) and (Height <= (h + 32)) then // Large Icon (32px).
  fIconSize:= itLargeIcon
  else if (Height > (h + 32)) and (Height <= (h + 64)) then // Extra Large Icon (48px).
  fIconSize:= itExtraLargeIcon
  else // Jumbo size (256px). (Vista/win7 only)
  begin
    if UseJumboIcons and IsWindowsVista then
    fIconSize:= itJumboIcon
    else
    fIconSize:= itExtraLargeIcon;
  end;

  if fIconSize = itSmallIcon then
  begin
    fIconHeight:= 16;
    fIconWidth:= 16;
    fIconRect.Left:= fIconBorderSize;
    fIconRect.Top:= Round((Height - 16) / 2);
    fIconRect.Right:= fIconRect.Left + 16;
    fIconRect.Bottom:= fIconRect.Top + 16;
  end
  else
  begin
    fIconHeight:= Height - (fIconBorderSize * 2);
    fIconWidth:= fIconHeight;
    fIconRect:= Rect(fIconBorderSize,fIconBorderSize, fIconBorderSize + fIconWidth, fIconBorderSize + fIconHeight);
  end;
  // Calculate info rect
  fInfoRect:= fIconRect;
  if fIconSize = itSmallIcon then
  finfoRect.Left:= fIconRect.Right + 4
  else
  fInfoRect.Left:= fIconRect.Right + (fIconBorderSize*2);
  fInfoRect.Right:= Width - 3;

  DrawBuffer;
end;

{-------------------------------------------------------------------------------
  Run Thumbnail Thread
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RunThumbnailThread;
begin
  if assigned(fLatestNS) then
  begin
    if CompareText(fLatestNS.Extension, '.ico') = 0 then
    Exit;
    
    fLatestThumbThread:= TCEThumbLoadThread.Create(true);
    fLatestThumbThread.FreeOnTerminate:= true;
    fLatestThumbThread.InfoBar:= Self;
    fLatestThumbThread.PIDL:= PIDLMgr.CopyPIDL(fLatestNS.AbsolutePIDL);
    fLatestThumbThread.ThumbnailSize.cx:= fIconRect.Right - fIconRect.Left;
    fLatestThumbThread.ThumbnailSize.cy:= fIconRect.Bottom - fIconRect.Top;
    fLatestThumbThread.Resume;
  end;
end;

{-------------------------------------------------------------------------------
  Run Info Thread
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RunInfoThread;
begin
  if assigned(fLatestNS) then
  begin
    fLatestInfoThread:= TCEInfoLoadThread.Create(true);
    fLatestInfoThread.FreeOnTerminate:= true;
    fLatestInfoThread.InfoBar:= Self;
    fLatestInfoThread.PIDL:= PIDLMgr.CopyPIDL(fLatestNS.AbsolutePIDL);
    fLatestInfoThread.ShowFolderItemCount:= fShowFolderItemCount and fLatestNS.Folder;
    fLatestInfoThread.CalculateHiddenItems:= CalculateHiddenItems;

    fLatestInfoThread.ShowInfoTip:= (not (fLatestNS.Folder and fLatestNS.FileSystem)) or
                                    fLatestNS.IsParentByNamespace(DesktopFolder, true) or
                                    fLatestNS.IsParentByNamespace(DrivesFolder, true) or
                                    fLatestNS.IsParentByNamespace(ControlPanelFolder, false);
    fLatestInfoThread.Resume;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called when thread has finished
-------------------------------------------------------------------------------}
procedure TCEInfoBar.ThumbThreadFinished(AThread: TCEThumbLoadThread);
begin
  if AThread = fLatestThumbThread then
  begin
    fThumbnailFound:= not AThread.Terminated and AThread.ThumbnailFound;
    if fThumbnailFound then
    begin
      fThumbnailBuffer.SetSize(AThread.Thumbnail.Width, AThread.Thumbnail.Height);
      if CurrentSkin.SkinName = 'Eos' then
      fThumbnailBuffer.Canvas.Brush.Color:= SkinManager.CurrentSkin.Options(skncDock).Body.Color1
      else
      fThumbnailBuffer.Canvas.Brush.Color:= SkinManager.CurrentSkin.ColorBtnFace;
      fThumbnailBuffer.Canvas.FillRect(Rect(0,0,fThumbnailBuffer.Width,fThumbnailBuffer.Height));
      fThumbnailBuffer.Canvas.Draw(0,0,AThread.Thumbnail);
      DrawBuffer;
      Paint;
    end;
    fLatestThumbThread:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called when thread has finished
-------------------------------------------------------------------------------}
procedure TCEInfoBar.InfoThreadFinished(AThread: TCEInfoLoadThread);
begin
  if AThread = fLatestInfoThread then
  begin
    fInfoFound:= not AThread.Terminated and AThread.InfoFound;
    if fInfoFound then
    begin
      BuildInfoList(AThread.Info);
      DrawBuffer;
      Paint;
    end;
    fLatestInfoThread:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Reload Info
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Reload;
var
  pidl: PItemIDList;
begin
  if assigned(fLatestNS) then
  begin
    pidl:= PIDLMgr.CopyPIDL(fLatestNS.AbsolutePIDL);
    try
      FreeAndNil(fLatestNS);
      LoadFromPIDL(pidl, fSelectionCount);
    finally
      PIDLMgr.FreePIDL(pidl);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_EraseBkgnd message (Don't erase background)
-------------------------------------------------------------------------------}
procedure TCEInfoBar.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;

{-------------------------------------------------------------------------------
  Handle WM_SpSkinChange
-------------------------------------------------------------------------------}
procedure TCEInfoBar.WMSpSkinChange(var Message: TMessage);
begin
  DrawBuffer;
  Paint;
end;

{##############################################################################}
// TCEThumbLoadThread

{-------------------------------------------------------------------------------
  Execute ThumbLoadThread
-------------------------------------------------------------------------------}
procedure TCEThumbLoadThread.Execute;
var
  Bits: HBITMAP;
  ns: TNamespace;
begin
  CoInitialize(nil); // <- needed for video thumbnails
  Thumbnail:= TBitmap.Create;
  Thumbnail.PixelFormat:= pf32bit;
  Thumbnail.Transparent:= true;
  ns:= TNamespace.Create(PIDL, nil);
  ns.FreePIDLOnDestroy:= true;
  ThumbnailFound:= false;
  try
    // Extract Image
    if Assigned(ns.ExtractImage.ExtractImageInterface) then
    begin
      ns.ExtractImage.Width:= ThumbnailSize.cx;
      ns.ExtractImage.Height:= ThumbnailSize.cy;
      ns.ExtractImage.ColorDepth:= 32;
      ns.ExtractImage.Flags:= IEIFLAG_SCREEN or IEIFLAG_QUALITY;
      try
        ns.ExtractImage.ImagePath;
        if Succeeded(ns.ExtractImage.ExtractImageInterface.Extract(Bits)) then
        begin
          Thumbnail.Handle:= Bits;
          ThumbnailFound:= true;
      end;
      except
      end;
    end;
    // Synchronize end results
    Synchronize(SyncFinished);
  finally
    Thumbnail.Free;
    ns.Free;
    CoUninitialize;
  end;
end;

{-------------------------------------------------------------------------------
  SyncFinished
-------------------------------------------------------------------------------}
procedure TCEThumbLoadThread.SyncFinished;
begin
  InfoBar.ThumbThreadFinished(Self);
end;

{##############################################################################}
// TCEInfoItem

{-------------------------------------------------------------------------------
  Create an instance of TCEInfoItem
-------------------------------------------------------------------------------}
constructor TCEInfoItem.Create;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoItem
-------------------------------------------------------------------------------}
destructor TCEInfoItem.Destroy;
begin
  inherited;
end;

{##############################################################################}
// TCEInfoLoadThread

{-------------------------------------------------------------------------------
  Execute ThumbLoadThread
-------------------------------------------------------------------------------}
procedure TCEInfoLoadThread.Execute;
var
  ns: TNamespace;
  i: Integer;
  flags: Cardinal;
  EnumList: IEnumIDList;
  NewItem: PItemIDList;
  Dummy: Cardinal;  
begin
  CoInitialize(nil);
  ns:= TNamespace.Create(PIDL, nil);
  ns.FreePIDLOnDestroy:= true;
  try
    // Extract Info
    if ShowInfoTip then
    Info:= ns.InfoTip;
    
    // Add Folder Item Count
    if ShowFolderItemCount then
    begin
      i:= 0;
      if CalculateHiddenItems then
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN
      else
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS;


      if ns.ShellFolder.EnumObjects(0, Flags, EnumList) = S_OK then
      begin
        while (EnumList.Next(1, NewItem, Dummy) = S_OK) and (not Self.Terminated) do
        begin
          i:= i + 1;
          PIDLMgr.FreePIDL(NewItem);
        end;
      end;
      if Info <> '' then
      Info:= Info + #13#10 + _('Item(s)') + ':' + IntToStr(i)
      else
      Info:= _('Item(s)') + ':' + IntToStr(i);
    end;

    InfoFound:= Info <> '';

    // Synchronize end results
    Synchronize(SyncFinished);
  finally
    ns.Free;
    CoUninitialize;
  end;
end;

{-------------------------------------------------------------------------------
  SyncFinished
-------------------------------------------------------------------------------}
procedure TCEInfoLoadThread.SyncFinished;
begin
  InfoBar.InfoThreadFinished(Self);
end;

end.
