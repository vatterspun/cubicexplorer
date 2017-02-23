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
//  The Original Code is CE_GifAnim.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_GifAnim;

interface

uses
  // CE Units

  // JVCL
  JvGif,
  // Graphic32
  GR32, GR32_Image,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, ExtCtrls;

type
  TCEGifImage = class(TImage32)
  private
    fAnimate: Boolean;
    procedure SetAnimate(const Value: Boolean);
  protected
    procedure PaintFrame;
  public
    fTimer: TTimer;
    GifImage: TJvGIFImage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(FilePath: String): Boolean;
    procedure OnNextFrameTimer(Sender: TObject);
    property Animate: Boolean read fAnimate write SetAnimate;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEGifImage
-------------------------------------------------------------------------------}
constructor TCEGifImage.Create(AOwner: TComponent);
begin
  inherited;
  fTimer:= TTimer.Create(self);
  fTimer.Enabled:= false;
  fTimer.Interval:= 100;
  fTimer.OnTimer:= OnNextFrameTimer;
  fAnimate:= false;
  GifImage:= TJvGIFImage.Create;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEGifImage
-------------------------------------------------------------------------------}
destructor TCEGifImage.Destroy;
begin
  GifImage.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Load image from file
-------------------------------------------------------------------------------}
function TCEGifImage.LoadFromFile(FilePath: String): Boolean;
begin
  Result:= false;
  try
    fTimer.Enabled:= false;
    fAnimate:= false;
    Bitmap.BeginUpdate;
    GifImage.LoadFromFile(FilePath);
    
    if (GifImage.Width > 0) and (GifImage.Height > 0) then
    begin
      Bitmap.SetSize(GifImage.Width, GifImage.Height);
      Result:= true;
    end
    else
    begin
      Bitmap.SetSize(150,40);
      Bitmap.Clear(clWhite32);
      Bitmap.Textout(Bitmap.BoundsRect, DT_CENTER+DT_VCENTER+DT_SINGLELINE, 'Invalid dimensions!');
    end;

    if Result then
    PaintFrame;
  finally
    Bitmap.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Paint GIF frame
-------------------------------------------------------------------------------}
procedure TCEGifImage.PaintFrame;
var
  i: Integer;
  tmpF: TJvGIFFrame;
  tmpFrame: TJvGIFFrame;
  r: TRect;
  transparent: Boolean;
begin
  if (GifImage.FrameIndex < 0) or (GifImage.FrameIndex >= GifImage.Count) then
  begin
    Bitmap.Clear(clWhite32);
    Bitmap.Textout(Bitmap.BoundsRect,DT_SINGLELINE+DT_VCENTER+DT_CENTER,'Invalid Frame');
  end;

  try
    Bitmap.BeginUpdate;
    tmpFrame:= GifImage.Frames[GifImage.FrameIndex];
    if tmpFrame.TransparentColor <> clNone then
    begin
      transparent:= true;
      if tmpFrame.DisposalMethod = dmUndefined then
      begin
        transparent:= false;
      end
      else if tmpFrame.DisposalMethod = dmLeave then
      begin
        transparent:= true;
      end
      else if tmpFrame.DisposalMethod = dmRestoreBackground then
      begin
        Bitmap.Clear(Color32(Self.Color));
        transparent:= true;
      end
      else if (tmpFrame.DisposalMethod = dmRestorePrevious) or (tmpFrame.DisposalMethod = dmReserved4) then
      begin
        for i:= GifImage.FrameIndex-1 downto 0 do
        begin
          tmpF:= GifImage.Frames[i];
          if tmpF.DisposalMethod = dmUndefined then
          begin
            tmpF.Draw(Bitmap.Canvas,bitmap.BoundsRect,false);
            Break;
          end;
        end;
        transparent:= true;
      end;
    end
    else
    begin
      transparent:= false;
    end;
    r.TopLeft:= tmpFrame.Origin;
    r.Right:= r.Left + tmpFrame.Width;
    r.Bottom:= r.Top + tmpFrame.Height;
    tmpFrame.Draw(Bitmap.Canvas, r, transparent);
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on next frame timer.
-------------------------------------------------------------------------------}
procedure TCEGifImage.OnNextFrameTimer(Sender: TObject);
var
  i: Integer;
begin
  fTimer.Enabled:= false;
  if not fAnimate then
  Exit;
  
  i:= GifImage.FrameIndex;
  inc(i);
  if i >= GifImage.Count then
  begin
    i:= 0;
  end;
  fTimer.Interval:= GifImage.Frames[GifImage.FrameIndex].AnimateInterval;
  GifImage.FrameIndex:= i;
  PaintFrame;
  fTimer.Enabled:= true;
end;

{*------------------------------------------------------------------------------
  Set Animate value
-------------------------------------------------------------------------------}
procedure TCEGifImage.SetAnimate(const Value: Boolean);
begin
  if fAnimate = Value then
  Exit;

  fAnimate:= Value;
  if fAnimate then
  fTimer.Enabled:= true
  else
  fTimer.Enabled:= false;
end;

end.
