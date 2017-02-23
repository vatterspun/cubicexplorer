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
//  The Original Code is CV_MediaPlayer.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CV_MediaPlayer;

{==============================================================================}
interface

uses
  // CubicCore
  ccFileUtils, ccInterfaces,
  // System Units
  Windows, SysUtils, Classes, Controls, Messages, Types, ExtCtrls, Contnrs,
  cUtils;

{==============================================================================}
const
  IID_ICVMediaPlayer: TGUID        = '{A5DE5CBA-62B5-4B07-81D6-4652FD29D40B}';
  IID_ICVMediaPlayerEngine: TGUID  = '{6D4CB6FC-B945-4EB5-A193-25613A800347}';
  IID_ICVMediaEngineControl: TGUID = '{881F0B3C-3BB0-4EAC-991F-FFD56409B1E3}';
  IID_ICVMediaEngineSeeking: TGUID = '{1E391419-4376-4BA2-A5EA-D513B6B6B977}';
  IID_ICVMediaEngineVideo: TGUID   = '{0597AE0E-C7EA-4621-925D-8DC1979D3227}';
  IID_ICVMediaEngineAudio: TGUID   = '{74205AAD-17EB-4E7C-82C4-933412EBCC5B}';
  IID_ICVMediaEngineStill: TGUID   = '{F0A1381F-55F6-4509-8A93-8D87A807BE12}';

type
{-------------------------------------------------------------------------------
  ICVMediaPlayer
-------------------------------------------------------------------------------}
  TCVMediaPlayerStatus = (mpsClosed,     // nothing is loaded in memory
                          mpsOpening,    // opening file
                          mpsBuffering,  // buffering
                          mpsPlaying,    // playing
                          mpsPaused,     // paused
                          mpsStopped,    // stopped, position should be 0
                          mpsDone,       // file has reached the end.
                          mpsError);     // error happened. 

  ICVMediaPlayer = interface(IInterface)
  ['{A5DE5CBA-62B5-4B07-81D6-4652FD29D40B}']
    // Close
    // - Frees file from memory
    procedure Close; stdcall;

    // GetDuration
    // - Returns duration in milliseconds.
    function GetDuration: Int64; stdcall;

    // GetPosition
    // - Returns current position in milliseconds.
    function GetPosition: Int64; stdcall;

    // GetStatus
    function GetStatus: TCVMediaPlayerStatus; stdcall;

    // GetStatusText
    function GetStatusText: WideString; stdcall;

    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; stdcall;

    // GetVolume
    // - Range is from 0 to 100
    function GetVolume: Integer; stdcall;

    // HasAudio
    // - Return true if currently loaded media has audio
    function HasAudio: Boolean; stdcall;
    
    // HasControl
    // - Return TRUE if currently loaded file supports play/pause/stop.
    // - Return FALSE with still media like images.
    function HasControl: Boolean; stdcall;

    // HasSeeking
    // - Return true if currently loaded media supports seeking
    function HasSeeking: Boolean; stdcall;

    // HasVideo
    // - Return true if currently loaded media has video
    function HasVideo: Boolean; stdcall;

    // IsStill
    // - Return true if currently loaded media is still (image or similar)
    function IsStill: Boolean; stdcall;

    // OpenFile
    function OpenFile(AFilePath: WideString): Boolean; stdcall;

    // Pause
    procedure Pause; stdcall;

    // Play
    procedure Play; stdcall;

    // Set Position
    // - Set position in milliseconds
    procedure SetPosition(APosition: Int64); stdcall;

    // Set Slideshow Interval
    // - Set interval for stills in milliseconds.
    procedure SetSlideshowInterval(AInterval: Integer); stdcall;

    // Set Volume
    // - Range is from 0 to 100
    procedure SetVolume(AVolume: Integer); stdcall;

    // Stop
    procedure Stop; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICVMediaEngine
-------------------------------------------------------------------------------}
  ICVMediaEngine = interface;

  TCVEngineNotifyEvent = procedure(ASender: ICVMediaEngine) of object;

  ICVMediaEngine = interface(IInterface)
  ['{6D4CB6FC-B945-4EB5-A193-25613A800347}']
    // Close
    // - Frees file from memory
    procedure Close; stdcall;

    // GetID
    // - Return unique TGuid
    function GetID: TGUID; stdcall;

    // GetStatus
    function GetStatus: TCVMediaPlayerStatus; stdcall;

    // GetStatusText
    function GetStatusText: WideString; stdcall;

    // GetSupportedExtensions
    // - Return comma separated list of file extensions that are supported.
    // - Example: jpg,gif,avi
    // - Use * to support all extensions.
    function GetSupportedExtensions: WideString; stdcall;

    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; stdcall;

    // OpenFile
    function OpenFile(AFilePath: WideString): Boolean; stdcall;

    // SetStatusChangedEvent
    // - AHandler is called when status changes
    procedure SetStatusChangedEvent(AHandler: TCVEngineNotifyEvent); stdcall;

  end;

{-------------------------------------------------------------------------------
  ICVMediaEngineControl
-------------------------------------------------------------------------------}
  ICVMediaEngineControl = interface(IInterface)
  ['{881F0B3C-3BB0-4EAC-991F-FFD56409B1E3}']
    // HasControl
    // - Return TRUE if currently loaded file supports play/pause/stop.
    // - Return FALSE with still media like images.
    function HasControl: Boolean; stdcall;
    // Pause
    procedure Pause; stdcall;
    // Play
    procedure Play; stdcall;
    // Stop
    procedure Stop; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICVMediaEngineSeeking
-------------------------------------------------------------------------------}
  ICVMediaEngineSeeking = interface(IInterface)
  ['{1E391419-4376-4BA2-A5EA-D513B6B6B977}']
    // GetDuration
    // - Returns duration in milliseconds.
    function GetDuration: Int64; stdcall;
    // GetPosition
    // - Returns current position in milliseconds.
    function GetPosition: Int64; stdcall;
    // HasSeeking
    // - Return true if currently loaded file supports seeking
    function HasSeeking: Boolean; stdcall;
    // SetPosition
    // - Set position in milliseconds
    procedure SetPosition(APosition: Int64); stdcall;
  end;

{-------------------------------------------------------------------------------
  ICVMediaEngineVideo
-------------------------------------------------------------------------------}
  ICVMediaEngineVideo = interface(IInterface)
  ['{0597AE0E-C7EA-4621-925D-8DC1979D3227}']
    // GetVideoSize
    // - Should return the video size (actual not resized).
    function GetVideoSize: TPoint; stdcall;
    // HasVideo
    // - Return true if currently loaded file has video
    function HasVideo: Boolean; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICVMediaEngineAudio
-------------------------------------------------------------------------------}
  ICVMediaEngineAudio = interface(IInterface)
  ['{74205AAD-17EB-4E7C-82C4-933412EBCC5B}']
    // GetVolume
    // - Range should be from 0 to 100 (linear).
    function GetVolume: Integer; stdcall;
    // HasAudio
    // - Return true if currently loaded file has audio
    function HasAudio: Boolean; stdcall;
    // SetVolume
    // - Range should be from 0 to 100 (linear).
    procedure SetVolume(AVolume: Integer); stdcall;
  end;

{-------------------------------------------------------------------------------
  ICVMediaEngineStill
-------------------------------------------------------------------------------}
  ICVMediaEngineStill = interface(IInterface)
  ['{F0A1381F-55F6-4509-8A93-8D87A807BE12}']
    // IsStill
    // - Return true if currentrly loaded media is still (image or similar).
    function IsStill: Boolean; stdcall;

    // SetSlideshowInterval
    // - AInterval is millisecounds.
    // - Engines have to implement their own timer to "play" stills.
    // - Engines should use the interval set in here as the "media length".
    // - Engines should start the timer when Play is called (pause and stop
    //   should be supported also).
    // - When the timer finishes, Status should be changed to mpsDone.
    procedure SetSlideshowInterval(AInterval: Integer); stdcall;
  end;

{-------------------------------------------------------------------------------
  TCVCustomMediaEngine
-------------------------------------------------------------------------------}
  TCVCustomMediaEngine = class(TInterfacedObject, ICVMediaEngine, ICCWindowCtrl)
  protected
    fStatus: TCVMediaPlayerStatus;
    fStatusChangedEvent: TCVEngineNotifyEvent;
    procedure ChangeStatus(AStatus: TCVMediaPlayerStatus); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Close; virtual; stdcall;
    // GetID
    // - Return unique TGuid
    function GetID: TGUID; virtual; stdcall;
    function GetStatus: TCVMediaPlayerStatus; virtual; stdcall;
    function GetStatusText: WideString; virtual; stdcall;
    // GetSupportedExtensions
    // - Return comma separated list of file extensions that are supported.
    // - Example: jpg,gif,avi
    // - Use * to support all extensions.
    function GetSupportedExtensions: WideString; virtual; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; virtual; stdcall;
    procedure SetBounds(ARect: TRect); virtual; stdcall;
    // SetFocus
    procedure SetFocus; virtual; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); virtual; stdcall;
    procedure SetStatusChangedEvent(AHandler: TCVEngineNotifyEvent); virtual;
        stdcall;
  end;



{-------------------------------------------------------------------------------
  TCVMediaPlayer
-------------------------------------------------------------------------------}
  TCVMediaPlayerPositionEvent = procedure(Sender: TObject; APosition: Int64; ADuration: Int64) of object;

  TCVMediaPlayer = class(TCustomControl, ICVMediaPlayer)
  private
    fDurationCache: Int64;
    fOnPositionChange: TCVMediaPlayerPositionEvent;
    fOnStatusChanged: TNotifyEvent;
    fPositionCache: Int64;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    fEngine: ICVMediaEngine;
    fEngineWindow: ICCWindowCtrl;
    fMute: Boolean;
    fPositionInterval: Integer;
    fProgressTimer: TTimer;
    fSlideshowInterval: Integer;
    fVolume: Integer;
    function CheckEngine: Boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    // GetVolume
    // - Range is from 0 to 100
    function GetVolume: Integer; virtual; stdcall;
    procedure HandleProgressTimer(Sender: TObject); virtual;
    procedure HandleStatusChange(ASender: ICVMediaEngine); virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetEngine(const Value: ICVMediaEngine); virtual;
    procedure SetMute(const Value: Boolean); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetPositionInterval(const Value: Integer); virtual;
    // Set Volume
    // - Range is from 0 to 100
    procedure SetVolume(AVolume: Integer); virtual; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close; virtual; stdcall;
    function GetDuration: Int64; virtual; stdcall;
    function GetPosition: Int64; virtual; stdcall;
    function GetStatus: TCVMediaPlayerStatus; virtual; stdcall;
    function GetStatusText: WideString; virtual; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; virtual; stdcall;
    function HasAudio: Boolean; virtual; stdcall;
    function HasVideo: Boolean; virtual; stdcall;
    function HasSeeking: Boolean; virtual; stdcall;
    function HasControl: Boolean; virtual; stdcall;
    // IsStill
    // - Return true if currently loaded media is still (image or similar)
    function IsStill: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; virtual; stdcall;
    procedure Pause; virtual; stdcall;
    procedure Play; virtual; stdcall;
    procedure SetFocus; override;
    // SetPosition
    // - Set position in milliseconds
    procedure SetPosition(APosition: Int64); virtual; stdcall;
    // Set Slideshow Interval
    // - Set interval for stills in milliseconds.
    procedure SetSlideshowInterval(AInterval: Integer); virtual; stdcall;
    procedure Stop; virtual; stdcall;
    property Engine: ICVMediaEngine read fEngine write SetEngine;
    property Mute: Boolean read fMute write SetMute;
    property PositionInterval: Integer read fPositionInterval write
        SetPositionInterval;
    property SlideshowInterval: Integer read fSlideshowInterval write
        SetSlideshowInterval;
    property Volume: Integer read fVolume write SetVolume;
  published
    property OnPositionChange: TCVMediaPlayerPositionEvent read fOnPositionChange
        write fOnPositionChange;
    property OnStatusChanged: TNotifyEvent read fOnStatusChanged write
        fOnStatusChanged;
    property PopupMenu;
    property Color;
  end;

{-------------------------------------------------------------------------------
  TCVMediaEngineList
-------------------------------------------------------------------------------}
  TCVCustomMediaEngineClass = class of TCVCustomMediaEngine;

  PCVMediaEngineListEntry  = ^TCVMediaEngineListEntry;
  TCVMediaEngineListEntry = record
    Extensions: WideString;
    EngineClass: TCVCustomMediaEngineClass;
  end;
  
  TCVMediaEngineList = class(TObject)
  protected
    fList: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer); virtual;
    function Find(AExtension: WideString): TCVCustomMediaEngineClass; virtual;
    function IndexOf(AMediaEngineClass: TCVCustomMediaEngineClass): Integer;
        virtual;
    function RegisterEngine(AMediaEngineClass: TCVCustomMediaEngineClass;
        AExtensions: WideString): Integer; virtual;
    procedure UnRegisterEngine(AMediaEngineClass: TCVCustomMediaEngineClass);
        virtual;
  end;

  function SecToTime(Sec: Integer): string;

{==============================================================================}
implementation

uses
  Graphics, ccClasses;

{##############################################################################}
// Public methods

{*------------------------------------------------------------------------------
  Second To Time
-------------------------------------------------------------------------------}
function SecToTime(Sec: Integer): string;
var
  H, M, S: string;
  ZH, ZM, ZS: Integer;
begin
  ZH:= Sec div 3600;
  ZM:= Sec div 60 - ZH * 60;
  ZS:= Sec - (ZH * 3600 + ZM * 60) ;
  H:= IntToStr(ZH);
  M:= IntToStr(ZM);
  S:= IntToStr(ZS);
  if ZS < 10 then
  S:= '0'+S;

  if h = '0' then
  begin
    Result := M + ':' + S
  end
  else
  begin
    if ZM < 10 then
    M:= '0'+M;
    Result := H + ':' + M + ':' + S;
  end;
end;

{##############################################################################}
// TCVMediaPlayer

{-------------------------------------------------------------------------------
  Create an instance of TCVMediaPlayer
-------------------------------------------------------------------------------}
constructor TCVMediaPlayer.Create(AOwner: TComponent);
begin
  inherited;
  fPositionInterval:= 250;
  fProgressTimer:= TTimer.Create(nil);
  fProgressTimer.Enabled:= false;
  fProgressTimer.Interval:= fPositionInterval;
  fProgressTimer.OnTimer:= HandleProgressTimer;
  fDurationCache:= 0;
  fPositionCache:= 0;
  fVolume:= 100;
  fMute:= false;
end;

{-------------------------------------------------------------------------------
  Destroy TCVMediaPlayer
-------------------------------------------------------------------------------}
destructor TCVMediaPlayer.Destroy;
begin
  // Free ProgressTimer
  fProgressTimer.Free;

  // Close file
  if CheckEngine then
  Engine.Close;

  // Free Engine
  if assigned(fEngineWindow) then
  fEngineWindow:= nil;
  if assigned(fEngine) then
  fEngine:= nil;
  inherited;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style:= Params.Style + WS_CLIPCHILDREN;
end;

{-------------------------------------------------------------------------------
  Check Engine (returns true if Engine is present)
-------------------------------------------------------------------------------}
function TCVMediaPlayer.CheckEngine: Boolean;
begin
  Result:= assigned(fEngine);
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Close;
begin
  if CheckEngine then
  begin
    Engine.Close;
    fProgressTimer.Enabled:= false;
    HandleProgressTimer(nil);
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Get Duration
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetDuration: Int64;
var
  seeking: ICVMediaEngineSeeking;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineSeeking, seeking) then
  Result:= seeking.GetDuration
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  Get Position
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetPosition: Int64;
var
  seeking: ICVMediaEngineSeeking;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineSeeking, seeking) then
  Result:= seeking.GetPosition
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  Get Status
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetStatus: TCVMediaPlayerStatus;
begin
  if CheckEngine then
  Result:= Engine.GetStatus
  else
  Result:= mpsClosed;
end;

{-------------------------------------------------------------------------------
  Get StatusText
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetStatusText: WideString;
begin
  if CheckEngine then
  Result:= Engine.GetStatusText
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetTitle: WideString;
begin
  if CheckEngine then
  Result:= Engine.GetTitle
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  GetVolume
-------------------------------------------------------------------------------}
function TCVMediaPlayer.GetVolume: Integer;
begin
  Result:= fVolume;
end;

{-------------------------------------------------------------------------------
  Handle ProgressTimer
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.HandleProgressTimer(Sender: TObject);
var
  p,d: Int64;
begin
  p:= GetPosition;
  d:= GetDuration;
  if (p <> fPositionCache) or (d <> fDurationCache) then
  begin
    fPositionCache:= p;
    fDurationCache:= d;
    if assigned(fOnPositionChange) then
    fOnPositionChange(Self, fPositionCache, fDurationCache);
  end;
end;

{-------------------------------------------------------------------------------
  Handle StatusChange
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.HandleStatusChange(ASender: ICVMediaEngine);
begin
  if assigned(fOnStatusChanged) then
  fOnStatusChanged(Self);
end;

{-------------------------------------------------------------------------------
  HasAudio
-------------------------------------------------------------------------------}
function TCVMediaPlayer.HasAudio: Boolean;
var
  audio: ICVMediaEngineAudio;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineAudio, audio) then
  Result:= audio.HasAudio
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  HasVideo
-------------------------------------------------------------------------------}
function TCVMediaPlayer.HasVideo: Boolean;
var
  video: ICVMediaEngineVideo;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineVideo, video) then
  Result:= video.HasVideo
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  HasSeeking
-------------------------------------------------------------------------------}
function TCVMediaPlayer.HasSeeking: Boolean;
var
  seek: ICVMediaEngineSeeking;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineSeeking, seek) then
  Result:= seek.HasSeeking
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  HasControl
-------------------------------------------------------------------------------}
function TCVMediaPlayer.HasControl: Boolean;
var
  ctrl: ICVMediaEngineControl;
begin
  if CheckEngine and supports(Engine, ICVMediaEngineControl, ctrl) then
  Result:= ctrl.HasControl
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  IsStill
-------------------------------------------------------------------------------}
function TCVMediaPlayer.IsStill: Boolean;
var
  still: ICVMediaEngineStill;
begin
  if CheckEngine and supports(Engine, ICVMediaEngineStill, still) then
  Result:= still.IsStill
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVMediaPlayer.OpenFile(AFilePath: WideString): Boolean;
begin
  if CheckEngine then
  begin
    // Set Window bounds
    if assigned(fEngineWindow) then
    fEngineWindow.SetBounds(ClientRect);

    // Set Volume
    SetMute(fMute);

    // Set SlideshowInterval
    SetSlideshowInterval(fSlideshowInterval);

    // Open file
    Result:= Engine.OpenFile(AFilePath);
    if Result then
    begin
      fProgressTimer.Enabled:= true;
      HandleProgressTimer(nil);
    end;
  end
  else
  Result:= false;
  Paint;
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Pause;
var
  ctrl: ICVMediaEngineControl;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineControl, ctrl) then
  begin
    ctrl.Pause;
    fProgressTimer.Enabled:= false;
    HandleProgressTimer(nil);
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Play;
var
  ctrl: ICVMediaEngineControl;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineControl, ctrl) then
  begin
    ctrl.Play;
    fProgressTimer.Enabled:= true;
    HandleProgressTimer(nil);
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Stop;
var
  ctrl: ICVMediaEngineControl;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineControl, ctrl) then
  begin
    ctrl.Stop;
    fProgressTimer.Enabled:= false;
    HandleProgressTimer(nil);
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Set Engine
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetEngine(const Value: ICVMediaEngine);
begin
  // Disable position timer
  fProgressTimer.Enabled:= false;

  // Free previous instance
  if assigned(fEngineWindow) then
  fEngineWindow:= nil;
  if assigned(fEngine) then
  begin
    fEngine.Close;
    fEngine:= nil;
  end;

  // Assign new instance
  if assigned(Value) then
  begin
    fEngine:= Value;
    // assign events
    fEngine.SetStatusChangedEvent(HandleStatusChange);

    // setup window
    if Supports(value, IID_ICCWindowCtrl, fEngineWindow) then
    begin
      fEngineWindow.SetParentWindow(Self.Handle);
      fEngineWindow.SetBounds(Self.ClientRect);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Resize
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Resize;
begin
  inherited;
  if assigned(fEngineWindow) then
  fEngineWindow.SetBounds(Self.ClientRect);
end;

{-------------------------------------------------------------------------------
  Set Parent
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetParent(AParent: TWinControl);
begin
  inherited;
  if assigned(fEngineWindow) then
  begin
    fEngineWindow.SetParentWindow(Self.Handle);
    fEngineWindow.SetBounds(Self.ClientRect);
  end;
end;

{-------------------------------------------------------------------------------
  WMEraseBkgnd
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:= 1;
end;

{-------------------------------------------------------------------------------
  Paint
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.Paint;
begin
  // Paint background
  if assigned(Engine) then
  begin
    if Engine.GetStatus = mpsClosed then
    begin
      Canvas.Brush.Color:= clGray;
      Canvas.FillRect(ClientRect);
    end
    else
    begin
      Canvas.Brush.Color:= clBlack;
      Canvas.FillRect(ClientRect);
    end;
  end
  else
  begin
    Canvas.Brush.Color:= clGray;
    Canvas.FillRect(ClientRect);
  end;
end;

{-------------------------------------------------------------------------------
  SetFocus
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetFocus;
begin
  inherited;
  if assigned(fEngineWindow) then
  begin
    fEngineWindow.SetFocus;
  end;
end;

{-------------------------------------------------------------------------------
  Set Position
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetPosition(APosition: Int64);
var
  seeking: ICVMediaEngineSeeking;
begin
  if CheckEngine and supports(Engine, IID_ICVMediaEngineSeeking, seeking) then
  begin
    seeking.SetPosition(APosition);
    HandleProgressTimer(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Set PositionInterval
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetPositionInterval(const Value: Integer);
begin
  fPositionInterval:= Value;
  fProgressTimer.Interval:= fPositionInterval;
end;

{-------------------------------------------------------------------------------
  SetMute
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetMute(const Value: Boolean);
var
  audio: ICVMediaEngineAudio;
begin
  fMute:= Value;
  // set engine  volume
  if CheckEngine and supports(Engine, IID_ICVMediaEngineAudio, audio) then
  begin
    if fMute then
    audio.SetVolume(0)
    else
    audio.SetVolume(fVolume);
  end;
end;

{-------------------------------------------------------------------------------
  SetSlideshowInterval
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetSlideshowInterval(AInterval: Integer);
var
  still: ICVMediaEngineStill;
begin
  fSlideshowInterval:= AInterval;
  if CheckEngine and supports(Engine, ICVMediaEngineStill, still) then
  still.SetSlideshowInterval(fSlideshowInterval);
end;

{-------------------------------------------------------------------------------
  SetVolume
-------------------------------------------------------------------------------}
procedure TCVMediaPlayer.SetVolume(AVolume: Integer);
var
  audio: ICVMediaEngineAudio;
begin
  // validate volume
  if AVolume < 0 then
  AVolume:= 0;
  if AVolume > 100 then
  AVolume:= 100;
  fVolume:= AVolume;
  // set engine  volume
  if CheckEngine and supports(Engine, IID_ICVMediaEngineAudio, audio) then
  audio.SetVolume(fVolume);
  // set mute
  fMute:= fVolume = 0;
end;

{##############################################################################}
// TCVCustomMediaEngine

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCVCustomMediaEngine.Create;
begin
  inherited;
  // Initilize values
  fStatus:= mpsClosed;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCVCustomMediaEngine.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.Close;
begin

end;

{-------------------------------------------------------------------------------
  Do StatusChangedEvent
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.ChangeStatus(AStatus:
    TCVMediaPlayerStatus);
begin
  fStatus:= AStatus;  
  if assigned(fStatusChangedEvent) then
  fStatusChangedEvent(Self);
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.GetID: TGUID;
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Get Status
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.GetStatus: TCVMediaPlayerStatus;
begin
  Result:= fStatus;
end;

{-------------------------------------------------------------------------------
  Get StatusText
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.GetStatusText: WideString;
begin
  case fStatus of
    mpsClosed: Result:= 'Closed';
    mpsOpening: Result:= 'Opening';
    mpsBuffering: Result:= 'Buffering';
    mpsPlaying: Result:= 'Playing';
    mpsPaused: Result:= 'Paused';
    mpsStopped: Result:= 'Stopped';
    mpsDone: Result:= 'Done';
    mpsError: Result:= 'Error';
  end;
end;

{-------------------------------------------------------------------------------
  Get SupportedExtensions
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.GetSupportedExtensions: WideString;
begin
  Result:= '*';
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.GetTitle: WideString;
begin
  Result:= '';
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVCustomMediaEngine.OpenFile(AFilePath: WideString): Boolean;
begin
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.SetBounds(ARect: TRect);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  SetFocus
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.SetFocus;
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.SetParentWindow(AParentWindow: HWND);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Set StatusChangedEvent
-------------------------------------------------------------------------------}
procedure TCVCustomMediaEngine.SetStatusChangedEvent(AHandler:
    TCVEngineNotifyEvent);
begin
  fStatusChangedEvent:= AHandler;
end;

{##############################################################################}
// TCVMediaEngineList

{-------------------------------------------------------------------------------
  Create TCVMediaEngineList
-------------------------------------------------------------------------------}
constructor TCVMediaEngineList.Create;
begin
  inherited Create;
  fList:= TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCVMediaEngineList
-------------------------------------------------------------------------------}
destructor TCVMediaEngineList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCVMediaEngineList.Clear;
var
  i: Integer;
  entry: PCVMediaEngineListEntry;
begin
  for i:= 0 to fList.Count - 1 do
  begin
    entry:= fList.Items[i];
    entry.Extensions:= '';
    FreeMem(entry);
  end;
end;

{-------------------------------------------------------------------------------
  Delete
-------------------------------------------------------------------------------}
procedure TCVMediaEngineList.Delete(AIndex: Integer);
var
  entry: PCVMediaEngineListEntry;
begin
  if (AIndex >= 0) and (AIndex < fList.Count) then
  begin
    entry:= fList.Items[AIndex];
    entry.Extensions:= '';
    FreeMem(entry);
    fList.Delete(AIndex);
  end;
end;

{-------------------------------------------------------------------------------
  Find
-------------------------------------------------------------------------------}
function TCVMediaEngineList.Find(AExtension: WideString):
    TCVCustomMediaEngineClass;
var
  i, i2: Integer;
  entry: PCVMediaEngineListEntry;
  list: TCCStringList;
begin
  Result:= nil;
  AExtension:= WideLowerCase(AExtension);
  list:= TCCStringList.Create;
  try
    list.Delimiter:= ',';
    for i:= 0 to fList.Count - 1 do
    begin
      entry:= fList.Items[i];
      list.DelimitedText:= entry.Extensions;
      for i2:= 0 to list.Count - 1 do
      begin
        if (list.Strings[i2] = AExtension) or (list.Strings[i2] = '*') then
        begin
          Result:= entry.EngineClass;
          Exit;
        end;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  IndexOf
-------------------------------------------------------------------------------}
function TCVMediaEngineList.IndexOf(AMediaEngineClass:
    TCVCustomMediaEngineClass): Integer;
var
  i: Integer;
  entry: PCVMediaEngineListEntry;
begin
  Result:= -1;
  for i:= 0 to fList.Count - 1 do
  begin
    entry:= fList.Items[i];
    if entry.EngineClass = AMediaEngineClass then
    begin
      Result:= i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  RegisterEngine
  - Returns -1 if class is already registered, else entry index is returned.
  - AExtensions should be comma separated list (for example: avi,jpg,txt).
    Use * to support all extensions.
-------------------------------------------------------------------------------}
function TCVMediaEngineList.RegisterEngine(AMediaEngineClass:
    TCVCustomMediaEngineClass; AExtensions: WideString): Integer;
var
  i: Integer;
  entry: PCVMediaEngineListEntry;
begin
  Result:= -1;
  // check if already registered
  for i:= 0 to fList.Count - 1 do
  begin
    entry:= fList.Items[i];
    if entry.EngineClass = AMediaEngineClass then
    Exit;
  end;
  // add to list
  entry:= AllocMem(SizeOf(TCVMediaEngineListEntry));
  entry.Extensions:= WideLowerCase(AExtensions);
  entry.EngineClass:= AMediaEngineClass;
  fList.Add(entry);
end;

{-------------------------------------------------------------------------------
  UnRegisterEngine
-------------------------------------------------------------------------------}
procedure TCVMediaEngineList.UnRegisterEngine(AMediaEngineClass:
    TCVCustomMediaEngineClass);
begin
  Delete(IndexOf(AMediaEngineClass));
end;

end.
