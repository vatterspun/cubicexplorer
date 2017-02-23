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
//  The Original Code is fCE_QuickView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_QuickView;

interface

uses
  // CE
  CE_Toolbar, CE_SpTBXItems, CE_FilePreview, dCE_Images, CE_AppSettings,
  // CV
  CV_MediaPlayer, CV_MediaPlayerEngines, CV_Playlist,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Toolbar, SpTBXControls, TB2Item, SpTBXDkPanels,
  SpTBXTabs,
  // fcl-xml
  DOM,
  // Tnt
  TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, ImgList, ComCtrls, ExtCtrls, Menus;

type
  TCEQuickViewForm = class;

{-------------------------------------------------------------------------------
  TCEQuickView
-------------------------------------------------------------------------------}
  TCEQuickViewLoopMode = (lmNoLooping, lmSingleFile, lmPlaylist, lmFilelist);

  TCEQuickView = class(TFrame)
    Dock_Bottom: TSpTBXDock;
    toolbar_seekbar: TCEToolbar;
    toolbar_controls: TCEToolbar;
    panel_content: TPanel;
    tabs_playlist: TSpTBXTabControl;
    tab_playlist: TSpTBXTabItem;
    sheet_playlist: TSpTBXTabSheet;
    tab_filelist: TSpTBXTabItem;
    sheet_filelist: TSpTBXTabSheet;
    splitter_left: TSpTBXSplitter;
    splitter_right: TSpTBXSplitter;
    QuickViewPopup: TSpTBXPopupMenu;
    but_detach: TSpTBXItem;
    but_ontop: TSpTBXItem;
    procedure HandlePopupItemClick(Sender: TObject); virtual;
    procedure QuickViewPopupPopup(Sender: TObject);
    procedure tabs_playlistActiveTabChange(Sender: TObject; TabIndex: Integer);
  private
    { Private declarations }
  protected
    fActive: Boolean;
    fActiveFilePath: WideString;
    fControlItems: TComponentList;
    fCurrentFilePath: WideString;
    fIsDetached: Boolean;
    fIsGlobalSettingsRead: Boolean;
    fIsSlideshowPlaying: Boolean;
    fLastMute: Boolean;
    fLastPlaylistVisible: Boolean;
    fLastStatus: TCVMediaPlayerStatus;
    fLastVolume: Integer;
    fLoopMode: TCEQuickViewLoopMode;
    fMediaPlayer: TCVMediaPlayer;
    fOnCurrentFileChange: TNotifyEvent;
    fOnDetach: TNotifyEvent;
    fShowPreview: Boolean;
    Preview: TCEFilePreview;
    procedure AssignTo(Dest: TPersistent); override;
    function CreateDetachedQuickView: TCEQuickViewForm; virtual;
    procedure CreateEmbededMediaPlayer; virtual;
    procedure HandleControlClick(Sender: TObject); virtual;
    procedure HandleActiveChange(Sender: TObject); virtual;
    procedure HandleNavigationStateChange(Sender: TObject); virtual;
    procedure HandleIsSupported(Sender: TObject; AExtension: WideString; var
        AIsSupported: Boolean); virtual;
    procedure HandlePreviewClick(Sender: TObject); virtual;
    procedure HandleSeekbarAfterChange(Sender: TObject); virtual;
    procedure HandleShowHintQuery(Sender: TObject; var AHint: String; var
        AShowHint: Boolean); virtual;
    procedure HandleVolumeAfterChange(Sender: TObject); virtual;
    procedure HandleVolumeChange(Sender: TObject); virtual;
    procedure ReadGlobalSettings; virtual;
    procedure SetActive(const Value: Boolean);
    procedure SetActiveFilePath(const Value: WideString);
    procedure SetCurrentFilePath(const Value: WideString); virtual;
    procedure SetLoopMode(const Value: TCEQuickViewLoopMode); virtual;
    procedure UpdateControlStates(Sender: TObject); virtual;
    procedure WriteGlobalSettings; virtual;
    procedure UpdateSeekbarState(Sender: TObject; APosition: Int64; ADuration:
        Int64); virtual;
  public
    Filelist: TCVFilelist;
    Playlist: TCVPlaylist;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachMediaPlayer(AMediaPlayer: TCVMediaPlayer); virtual;
    procedure Close; virtual;
    procedure Detach; virtual;
    procedure OpenFile(AFilePath: WideString); virtual;
    procedure OpenFileInMediaPlayer(AFilePath: WideString); virtual;
    procedure PlayNextFile; virtual;
    procedure PopulateControlsToolbar(AToolbar: TCEToolbar); virtual;
    procedure PopulateSeekToolbar(AToolbar: TCEToolbar); virtual;
    property Active: Boolean read fActive write SetActive;
    property ActiveFilePath: WideString read fActiveFilePath write
        SetActiveFilePath;
    property IsDetached: Boolean read fIsDetached;
    { Public declarations }
  published
    property CurrentFilePath: WideString read fCurrentFilePath write
        SetCurrentFilePath;
    property LoopMode: TCEQuickViewLoopMode read fLoopMode write SetLoopMode;
    property ShowPreview: Boolean read fShowPreview write fShowPreview;
    property OnCurrentFileChange: TNotifyEvent read fOnCurrentFileChange write
        fOnCurrentFileChange;
    property OnDetach: TNotifyEvent read fOnDetach write fOnDetach;
  end;

{-------------------------------------------------------------------------------
  TCEQuickViewForm
    - This is the detached QuickView window
-------------------------------------------------------------------------------}
  TCEQuickViewForm = class(TTntForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure HandleCurrentFileChange(Sender: TObject); virtual;
  public
    QuickView: TCEQuickView;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;

{-------------------------------------------------------------------------------
  TCEQuickViewSettings
-------------------------------------------------------------------------------}
  TCEMediaPlayerType = (mptAuto, mptWMP, mptDirectShow);
  TCEPlaylistVisibility = (pvHidden, pvPlaylist, pvFilelist);

  TCEMediaEngineSettings = class(TCECustomSettingStorage)
  public
    procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    procedure Save(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
  end;

  TCEQuickViewSettings = class(TPersistent)
  private
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fRememberPanelLayout: Boolean;
  protected
    fDirectShowExtensions: WideString;
    fImageExtensions: WideString;
    fLoopMode: TCEQuickViewLoopMode;
    fMediaPlayer: TCEMediaPlayerType;
    fTextExtensions: WideString;
    fMediaExtensions: WideString;
    fPlaylistVisibility: TCEPlaylistVisibility;
    fSlideshowInterval: Integer;
    fWMPExtensions: WideString;
    procedure SetDirectShowExtensions(const Value: WideString); virtual;
    procedure SetImageExtensions(const Value: WideString); virtual;
    procedure SetMediaExtensions(const Value: WideString); virtual;
    procedure SetTextExtensions(const Value: WideString); virtual;
    procedure SetWMPExtensions(const Value: WideString); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateMediaEngine(AExtension: WideString): ICVMediaEngine; virtual;
    function IsSupported(AExtension: WideString; AExcludeText: Boolean = false):
        Boolean; virtual;
  published
    property DirectShowExtensions: WideString read fDirectShowExtensions write
        SetDirectShowExtensions;
    property ImageExtensions: WideString read fImageExtensions write
        SetImageExtensions;
    property LoopMode: TCEQuickViewLoopMode read fLoopMode write fLoopMode;
    property MediaPlayer: TCEMediaPlayerType read fMediaPlayer write fMediaPlayer;
    property TextExtensions: WideString read fTextExtensions write
        SetTextExtensions;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout
        write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout
        write fRememberOuterToolbarLayout;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property MediaExtensions: WideString read fMediaExtensions write
        SetMediaExtensions;
    property PlaylistVisibility: TCEPlaylistVisibility read fPlaylistVisibility
        write fPlaylistVisibility;
    property SlideshowInterval: Integer read fSlideshowInterval write
        fSlideshowInterval;
    property WMPExtensions: WideString read fWMPExtensions write SetWMPExtensions;
  end;

var
  GlobalQuickViewSettings: TCEQuickViewSettings;

implementation

uses
  Math, ccFileUtils, ccClasses, CE_LanguageEngine,
  MPShellUtilities;

{$R *.dfm}

{##############################################################################}
// TCEQuickView

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickView
-------------------------------------------------------------------------------}
constructor TCEQuickView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // initilize values
  fLastMute:= false;
  fLastVolume:= 100;
  fLastPlaylistVisible:= false;
  fIsGlobalSettingsRead:= false;
  fShowPreview:= true;
  fMediaPlayer:= nil;
  fActive:= false;
  fLoopMode:= lmNoLooping;
  fLastStatus:= mpsClosed;
  fIsDetached:= false;

  // create Preview
  Preview:= TCEFilePreview.Create(Self);
  Preview.Parent:= panel_content;
  Preview.Align:= alClient;
  Preview.OnClick:= HandlePreviewClick;
  Preview.PopupMenu:= QuickViewPopup;

  // create Playlist
  Playlist:= TCVPlaylist.Create(Self);
  Playlist.Parent:= sheet_playlist;
  Playlist.Align:= alClient;
  Playlist.LoopList:= false;
  Playlist.OnIsSupported:= HandleIsSupported;
  Playlist.OnActiveItemChange:= HandleActiveChange;
  Playlist.OnNavigationStateChange:= HandleNavigationStateChange;

  // create Filelist
  Filelist:= TCVFilelist.Create(Self);
  Filelist.Parent:= sheet_filelist;
  Filelist.Align:= alClient;
  Filelist.LoopList:= true;
  Filelist.OnIsSupported:= HandleIsSupported;
  Filelist.OnActiveItemChange:= HandleActiveChange;
  Filelist.OnNavigationStateChange:= HandleNavigationStateChange;

  // create control items
  fControlItems:= TComponentList.Create(false);
  PopulateControlsToolbar(toolbar_controls);
  PopulateSeekToolbar(toolbar_seekbar);
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickView
-------------------------------------------------------------------------------}
destructor TCEQuickView.Destroy;
begin
  if assigned(fMediaPlayer) then
  begin
    fMediaPlayer.Free;
    fMediaPlayer:= nil;
  end;
  fControlItems.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCEQuickView.AssignTo(Dest: TPersistent);
var
  quick: TCEQuickView;
begin
  if Dest is TCEQuickView then
  begin
    quick:= TCEQuickView(Dest);
    quick.fLastVolume:= fLastVolume;
    quick.fLastMute:= fLastMute;    
    quick.fLastPlaylistVisible:= fLastPlaylistVisible;
    quick.fActiveFilePath:= fActiveFilePath;
    quick.fActive:= fActive;
    quick.fLoopMode:= fLoopMode;
    quick.fShowPreview:= fShowPreview;    
    quick.Preview.Assign(Preview);

    quick.Playlist.Assign(Playlist);
    quick.Filelist.Assign(Filelist);

    quick.tab_playlist.Checked:= tab_playlist.Checked;
    quick.tab_filelist.Checked:= tab_filelist.Checked;
    
    if assigned(quick.fMediaPlayer) then
    begin
      quick.fMediaPlayer.Volume:= fLastVolume;
      quick.fMediaPlayer.Mute:= fLastMute;
    end;
    quick.UpdateControlStates(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Attach MediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.AttachMediaPlayer(AMediaPlayer: TCVMediaPlayer);
begin
  if assigned(AMediaPlayer) then
  begin
    // free previous instance of MediaPlayerWnd
    Close;
  
    // attach
    fMediaPlayer:= AMediaPlayer;
    fMediaPlayer.Parent:= panel_content;
    fLastVolume:= fMediaPlayer.Volume;
    fLastMute:= fMediaPlayer.Mute;
    fMediaPlayer.OnPositionChange:= UpdateSeekbarState;
    fMediaPlayer.OnStatusChanged:= UpdateControlStates;
  end;
end;

{-------------------------------------------------------------------------------
  Handle PopupItemClick
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandlePopupItemClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    // Detach
    101: Detach;
    // Always on top
    102: begin
      if Parent is TCEQuickViewForm then
      begin
        if TCEQuickViewForm(Parent).FormStyle = fsNormal then
        TCEQuickViewForm(Parent).FormStyle:= fsStayOnTop
        else
        TCEQuickViewForm(Parent).FormStyle:= fsNormal;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCEQuickView.Close;
begin
  if assigned(fMediaPlayer) then
  begin
    fMediaPlayer.Free;
    fMediaPlayer:= nil;
  end;
  Preview.Clear;
end;

{-------------------------------------------------------------------------------
  Create Detached QuickView
-------------------------------------------------------------------------------}
function TCEQuickView.CreateDetachedQuickView: TCEQuickViewForm;
var
  p: TPoint;
begin
  // create form
  Result:= TCEQuickViewForm.CreateNew(Application.MainForm);
  // initilize values
  Result.FormStyle:= fsStayOnTop;
  Result.ScreenSnap:= true;
  // size
  p:= Self.ClientToScreen(Point(0,0));
  Result.ClientHeight:= Self.ClientHeight;
  Result.ClientWidth:= Self.ClientWidth;
  Windows.SetWindowPos(Result.Handle, HWND_TOP,
                       p.X+5, p.Y+5,
                       0, 0,
                       SWP_NOSIZE);
  // show
  Result.Show;
  Result.Resize;
end;

{-------------------------------------------------------------------------------
  Create EmbededMediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.CreateEmbededMediaPlayer;
begin
  if not assigned(fMediaPlayer) then
  begin
    fMediaPlayer:= TCVMediaPlayer.Create(nil);
    fMediaPlayer.Parent:= panel_content;
    fMediaPlayer.Color:= clBlack;
    fMediaPlayer.Align:= alClient;
    fMediaPlayer.Volume:= fLastVolume;
    fMediaPlayer.Mute:= fLastMute;
    fMediaPlayer.BringToFront;
    fMediaPlayer.OnPositionChange:= UpdateSeekbarState;
    fMediaPlayer.OnStatusChanged:= UpdateControlStates;
    fMediaPlayer.PopupMenu:= QuickViewPopup;
    fMediaPlayer.SetSlideshowInterval(GlobalQuickViewSettings.SlideshowInterval);
  end;
end;

{-------------------------------------------------------------------------------
  Detach
-------------------------------------------------------------------------------}
procedure TCEQuickView.Detach;
var
  form: TCEQuickViewForm;
begin
  form:= CreateDetachedQuickView;
  form.QuickView.AttachMediaPlayer(fMediaPlayer);
  form.QuickView.Assign(Self);
  form.Caption:= form.QuickView.ActiveFilePath;
  fMediaPlayer:= nil;
  
  // open preview
  SetActiveFilePath(fActiveFilePath);

  UpdateControlStates(Self);

  if assigned(fOnDetach) then
  fOnDetach(Self);
end;

{-------------------------------------------------------------------------------
  Handle ControlClick
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleControlClick(Sender: TObject);
begin
  if assigned(fMediaPlayer) then
  begin
    if Sender is TSpTBXItem then
    begin
      case TSpTBXItem(Sender).Tag of
        // play
        1: begin
          if fMediaPlayer.GetStatus = mpsPlaying then
          fMediaPlayer.Pause
          else
          fMediaPlayer.Play;
        end;
        // stop
        2: begin
          if TSpTBXItem(Sender).ImageIndex = 2 then
          fMediaPlayer.Stop
          else
          begin
            Close;
            // open preview
            SetActiveFilePath(fActiveFilePath);
          end;
        end;
        // previous
        3: Playlist.ActivatePrevious;
        // next
        4: Playlist.ActivateNext;
        // previous file
        5: begin
          if Filelist.IsEmpty then
          Filelist.ActiveFilePath:= ActiveFilePath;
          Filelist.ActivatePrevious;
        end;
        // next file
        6: begin
          if Filelist.IsEmpty then
          Filelist.ActiveFilePath:= ActiveFilePath;
          Filelist.ActivateNext;
        end;
        // loop modes
        71: LoopMode:= lmNoLooping;
        72: LoopMode:= lmSingleFile;
        73: LoopMode:= lmPlaylist;
        74: LoopMode:= lmFilelist;
        // playlist
        8: begin
          fLastPlaylistVisible:= not fLastPlaylistVisible;
        end;
        // mute
        9: begin
          fLastMute:= not fLastMute;
          fMediaPlayer.Mute:= fLastMute;
        end;
      end;
      UpdateControlStates(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle ActiveChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleActiveChange(Sender: TObject);
begin
  if Sender = Filelist then
  OpenFileInMediaPlayer(Filelist.GetActivePath)
  else if Sender = Playlist then
  OpenFileInMediaPlayer(Playlist.GetActivePath);
end;

{-------------------------------------------------------------------------------
  Handle Navigation State Change
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleNavigationStateChange(Sender: TObject);
var
  L,R: Boolean;
  i: Integer;
begin
  // Playlist
  if (Sender = Playlist) then
  begin
    L:= Playlist.GetPreviousItem <> nil;
    R:= Playlist.GetNextItem <> nil;
    for i:= 0 to fControlItems.Count - 1 do
    begin
      case fControlItems.Items[i].Tag of
        3: begin
          TSpTBXItem(fControlItems.Items[i]).Visible:= Playlist.RootNodeCount > 1;
          TSpTBXItem(fControlItems.Items[i]).Enabled:= L;
        end;
        4: begin
          TSpTBXItem(fControlItems.Items[i]).Visible:= Playlist.RootNodeCount > 1;
          TSpTBXItem(fControlItems.Items[i]).Enabled:= R;
        end;
      end;
    end;
  end
  // Filelist
  else if (Sender = Filelist) then
  begin
    L:= Filelist.GetPreviousItem <> nil;
    R:= Filelist.GetNextItem <> nil;  
    for i:= 0 to fControlItems.Count - 1 do
    begin
      case fControlItems.Items[i].Tag of
        5: TSpTBXItem(fControlItems.Items[i]).Enabled:= L;
        6: TSpTBXItem(fControlItems.Items[i]).Enabled:= R;
      end;
    end;    
  end;
end;

{-------------------------------------------------------------------------------
  Handle IsSupported
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleIsSupported(Sender: TObject; AExtension:
    WideString; var AIsSupported: Boolean);
begin
  AIsSupported:= GlobalQuickViewSettings.IsSupported(AExtension);
end;

{-------------------------------------------------------------------------------
  Handle SeekbarAfterChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleSeekbarAfterChange(Sender: TObject);
begin
  if assigned(fMediaPlayer) then
  fMediaPlayer.SetPosition(Round((fMediaPlayer.GetDuration / MAXWORD) * TCETrackBar(Sender).Position));
end;

{-------------------------------------------------------------------------------
  Handle ShowHintQuery
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleShowHintQuery(Sender: TObject; var AHint: String; var AShowHint: Boolean);
var
  track: TCETrackBar;
  pos: Int64;
begin
  if Sender is TCETrackBar then
  begin
    track:= TCETrackBar(Sender);
    // Seekbar
    if track.Tag = 20 then
    begin
      if assigned(fMediaPlayer) then
      begin
        pos:= Round((fMediaPlayer.GetDuration / track.Max) * track.Position);

        // millisecond to second
        if pos > 0 then
        pos:= pos div 1000;

        AHint:= SecToTime(pos);
        AShowHint:= true;
      end
      else
      AShowHint:= false;
    end
    // Volume
    else if track.Tag = 10 then
    begin
      AHint:= AHint + '%';
      AShowHint:= true;
    end
  end;
end;

{-------------------------------------------------------------------------------
 Handle VolumeAfterChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleVolumeAfterChange(Sender: TObject);
begin
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Handle VolumeChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleVolumeChange(Sender: TObject);
var
  i: Integer;
begin
  if TCETrackBar(Sender).Changing then
  begin
    fLastVolume:= TCETrackBar(Sender).Position;
    fLastMute:= fLastVolume = 0;    
    TCETrackBar(Sender).SelEnd:= fLastVolume;  
    if assigned(fMediaPlayer) then
    begin
      fMediaPlayer.Volume:= fLastVolume;
      fMediaPlayer.Mute:= fLastMute;
    end;

    // update mute buttons  
    for i:= 0 to fControlItems.Count - 1 do
    begin
      if fControlItems.Items[i].Tag = 9 then
      begin
        if TSpTBXItem(fControlItems.Items[i]).Checked <> fLastMute then
        begin
          TSpTBXItem(fControlItems.Items[i]).Checked:= fLastMute;
          if fLastMute then
          TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 9
          else
          TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 8;

          // repaint the toolbar manually because the mute button doesn't get
          //   repainted properly always when using the volume trackbar.
          if TSpTBXItem(fControlItems.Items[i]).Owner is TSpTBXToolbar then
          TSpTBXToolbar(TSpTBXItem(fControlItems.Items[i]).Owner).Repaint;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
procedure TCEQuickView.OpenFile(AFilePath: WideString);
var
  status: TCVMediaPlayerStatus;
begin
  if AFilePath = '' then
  Exit;

  // show preview
  if fShowPreview then
  begin
    // close media player if it's not running.
    if assigned(fMediaPlayer) then
    begin
      status:= fMediaPlayer.GetStatus;
      if (status = mpsClosed) or (status = mpsDone) or (status = mpsError) or (status = mpsStopped)  then
      Close;
    end;
    // open file
    if not assigned(fMediaPlayer) then
    begin
      Preview.OpenFile(AFilePath);
      CurrentFilePath:= AFilePath;
    end;  
  end  
  // open directly in media player
  else
  begin
    ReadGlobalSettings;

    Playlist.Clear;
    Playlist.Add(fActiveFilePath, true); // <-- opens the media player
    Filelist.ActiveFilePath:= fActiveFilePath;
  end;
  
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Handle Preview Click
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandlePreviewClick(Sender: TObject);
begin
  ReadGlobalSettings;

  if GlobalQuickViewSettings.IsSupported(WideExtractFileExt(fActiveFilePath)) then
  begin
    Playlist.Clear;
    Playlist.Add(fActiveFilePath, true); // <-- opens the media player
    Filelist.ActiveFilePath:= fActiveFilePath;

    if assigned(fMediaPlayer) then
    fMediaPlayer.SetFocus;
  end;
end;

{-------------------------------------------------------------------------------
  Open File In MediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.OpenFileInMediaPlayer(AFilePath: WideString);
var
  Ext: WideString;
begin
  CurrentFilePath:= AFilePath;
  
  if AFilePath = '' then
  Exit;

  // create media player
  CreateEmbededMediaPlayer;
  // create media engine
  Ext:= WideLowerCase(WideExtractFileExt(AFilePath));
  fMediaPlayer.Engine:= GlobalQuickViewSettings.CreateMediaEngine(Ext);
  // open file
  fMediaPlayer.OpenFile(AFilePath);
  // clear preview (no need to keep it in memory since it's hidden)
  Preview.Clear;
  
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Play Next File
-------------------------------------------------------------------------------}
procedure TCEQuickView.PlayNextFile;
var
  doPlay: Boolean;
begin
  // next playlist item
  if fLoopMode = lmPlaylist then
  doPlay:= Playlist.ActivateNext <> nil
  // next filelist item
  else if fLoopMode = lmFilelist then
  doPlay:= Filelist.ActivateNext <> nil
  // loop current
  else
  doPlay:= fLoopMode = lmSingleFile;

  // play
  if assigned(fMediaPlayer) and doPlay then
  begin
    fMediaPlayer.SetPosition(0);
    fMediaPlayer.Play;
  end;
end;

{-------------------------------------------------------------------------------
  Populate Controls Toolbar
-------------------------------------------------------------------------------}
procedure TCEQuickView.PopulateControlsToolbar(AToolbar: TCEToolbar);
var
  item: TSpTBXItem;
  sub: TSpTBXSubmenuItem;
  status: TSpTBXLabelItem;
  track: TCETrackBar;
begin
  if assigned(AToolbar) then
  AToolbar.Items.Clear
  else
  Exit;

  // play (1)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 1;
  item.Caption:= 'Play';
  item.ImageIndex:= 0;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // stop (2)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 2;
  item.Caption:= 'Stop';
  item.ImageIndex:= 2;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // previous (3)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 3;
  item.Caption:= 'Previous';
  item.ImageIndex:= 3;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // next (4)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 4;
  item.Caption:= 'Next';
  item.ImageIndex:= 4;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);

  // previous file (5)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 5;
  item.Caption:= 'Previous File';
  item.ImageIndex:= 5;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);

  // next file (6)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 6;
  item.Caption:= 'Next File';
  item.ImageIndex:= 6;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);

  // dynamic spacer
  AToolbar.Items.Add(TCEToolbarDynamicSpacerItem.Create(AToolbar));

  // Loop mode (7)
  sub:= TSpTBXSubmenuItem.Create(AToolbar);
  sub.Tag:= 7;
  sub.ImageIndex:= -1;
  sub.Options:= [tboDropdownArrow];
  AToolbar.Items.Add(sub);
  fControlItems.Add(sub);
    // no looping (71)
    item:= TSpTBXItem.Create(sub);
    item.Tag:= 71;
    item.Caption:= _('No Looping');
    item.ImageIndex:= 10;
    item.OnClick:= HandleControlClick;
    sub.Add(item);
    fControlItems.Add(item);
    // single file (72)
    item:= TSpTBXItem.Create(sub);
    item.Tag:= 72;
    item.Caption:= _('Loop File');
    item.ImageIndex:= 11;
    item.OnClick:= HandleControlClick;
    sub.Add(item);
    fControlItems.Add(item);
    // playlist (73)
    item:= TSpTBXItem.Create(sub);
    item.Tag:= 73;
    item.Caption:= _('Loop Playlist');
    item.ImageIndex:= 12;
    item.OnClick:= HandleControlClick;
    sub.Add(item);
    fControlItems.Add(item);
    // filelist (74)
    item:= TSpTBXItem.Create(sub);
    item.Tag:= 74;
    item.Caption:= _('Loop Filelist');
    item.ImageIndex:= 13;
    item.OnClick:= HandleControlClick;
    sub.Add(item);
    fControlItems.Add(item);

  // playlist (8)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 8;
  item.Caption:= 'Playlist';
  item.ImageIndex:= 7;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // mute (9)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 9;
  item.Caption:= 'Mute';
  item.ImageIndex:= 8;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // volume (10)
  track:= TCETrackBar.Create(AToolbar);
  track.Tag:= 10;
  track.Parent:= AToolbar;
  track.ShowFocusRect:= false;
  track.ShowPositionHint:= true;
  track.Height:= 21;
  track.Width:= 64;
  track.ChannelSize:= 8;
  track.ThumbLength:= 18;
  track.Max:= 100;
  track.PageSize:= 10;
  track.TickStyle:= tsNone;
  track.TickMarks:= tmxCenter;
  track.OnChange:= HandleVolumeChange;
  track.OnAfterChange:= HandleVolumeAfterChange;
  track.OnShowHintQueryEvent:= HandleShowHintQuery;
  fControlItems.Add(track);
end;

{-------------------------------------------------------------------------------
  Populate Seek Toolbar
-------------------------------------------------------------------------------}
procedure TCEQuickView.PopulateSeekToolbar(AToolbar: TCEToolbar);
var
  track: TCETrackBar;
  spacer: TCECustomToolbarSpacerItem;
  time: TSpTBXLabelItem;
  item: TSpTBXItem;
begin
  if assigned(AToolbar) then
  AToolbar.Items.Clear
  else
  Exit;

  // seekbar (20)
  track:= TCETrackBar.Create(AToolbar);
  track.Tag:= 20;
  track.Parent:= AToolbar;
  track.ShowFocusRect:= false;
  track.ShowPositionHint:= true;
  track.Height:= 21;
  track.ChannelSize:= 8;
  track.ThumbLength:= 18;
  track.Max:= MAXWORD;
  track.PageSize:= MAXWORD div 20;
  track.TickStyle:= tsNone;
  track.TickMarks:= tmxCenter;
  track.OnAfterChange:= HandleSeekbarAfterChange;
  track.OnShowHintQueryEvent:= HandleShowHintQuery;
  fControlItems.Add(track);
  // stretcher
  AToolbar.Items.Add(TCEToolbarStretcherItem.Create(AToolbar));

  // time label (21)
  time:= TSpTBXLabelItem.Create(AToolbar);
  time.Tag:= 21;
  time.Caption:= '00:00 / 00:00';
  fControlItems.Add(time);
  AToolbar.Items.Add(time);
end;

{-------------------------------------------------------------------------------
  On QuickViewPopup.Popup
-------------------------------------------------------------------------------}
procedure TCEQuickView.QuickViewPopupPopup(Sender: TObject);
begin
  // detach
  but_detach.Visible:= not fIsDetached;
  but_detach.Enabled:= fActiveFilePath <> '';
  // always on top
  but_ontop.Visible:= fIsDetached;
  if fIsDetached then
  but_ontop.Checked:= (Parent is TCEQuickViewForm) and (TCEQuickViewForm(Parent).FormStyle = fsStayOnTop);
end;

{-------------------------------------------------------------------------------
  Set Active
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive:= Value;
    if fActive then
    OpenFile(fActiveFilePath)
    else
    Close;
  end;
end;

{-------------------------------------------------------------------------------
  Set ActiveFilePath
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetActiveFilePath(const Value: WideString);
var
  ns: TNamespace;
begin
  // get file location from LNK file.
  if WideLowerCase(WideExtractFileExt(Value)) = '.lnk' then
  begin
    ns:= TNamespace.CreateFromFileName(Value);
    try
      if ns.Link then
      fActiveFilePath:= ns.ShellLink.TargetPath
      else
      fActiveFilePath:= Value;
    finally
      ns.Free;
    end;
  end
  else  
  fActiveFilePath:= Value;
  
  if fActive then
  begin
    //if WideFileExists(fActiveFilePath) then
    if fActiveFilePath <> '' then
    OpenFile(fActiveFilePath)
    else if not assigned(fMediaPlayer) then
    Close;
  end;
end;

{-------------------------------------------------------------------------------
  Set LoopMode
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetLoopMode(const Value: TCEQuickViewLoopMode);
begin
  if Value <> fLoopMode then
  begin
    fLoopMode:= Value;
    GlobalQuickViewSettings.LoopMode:= fLoopMode;
    UpdateControlStates(Self);
  end;
end;

{-------------------------------------------------------------------------------
  On tabs_playlist.ActiveTabChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.tabs_playlistActiveTabChange(Sender: TObject; TabIndex:
    Integer);
begin
  WriteGlobalSettings;
end;

{-------------------------------------------------------------------------------
  Update Control States
  - Here we hide/show and enable/disable buttons and toolbars according to
  current media player state.
-------------------------------------------------------------------------------}
procedure TCEQuickView.UpdateControlStates(Sender: TObject);
var
  i: Integer;
  item: TSpTBXItem;
  status: TCVMediaPlayerStatus;
  loaded: Boolean;
begin
  // loaded, show toolbar(s)
  if assigned(fMediaPlayer) then
  begin
    status:= fMediaPlayer.GetStatus;

    // play next file
    if Sender = fMediaPlayer then
    begin
      if (status = mpsDone) or ((status = mpsError) and fIsSlideshowPlaying) then
      begin
        PlayNextFile;
        status:= fMediaPlayer.GetStatus;
      end;
      fIsSlideshowPlaying:= (status <> mpsClosed) and (status <> mpsStopped);
    end;

    // update control states
    loaded:= (status = mpsPlaying) or (status = mpsPaused) or (status = mpsStopped) or (status = mpsDone);
    for i:= 0 to fControlItems.Count - 1 do
    begin
      case fControlItems.Items[i].Tag of
        // play/pause
        1: begin
          item:= TSpTBXItem(fControlItems.Items[i]);
          item.Enabled:= loaded or
                         ((fLoopMode = lmPlaylist) and not Playlist.IsEmpty) or
                         ((fLoopMode = lmFilelist) and not Filelist.IsEmpty);

          if (status = mpsPlaying) then
          item.ImageIndex:= 1
          else
          item.ImageIndex:= 0;
        end;
        // stop/close
        2: begin
          item:= TSpTBXItem(fControlItems.Items[i]);
          // always show stop button
          if not ShowPreview then
          begin
            if item.ImageIndex <> 2 then
            begin
              item.ImageIndex:= 2;
              item.Caption:= _('Stop');              
            end;  
           item.Enabled:= (status = mpsPlaying) or (status = mpsPaused);         
          end
          // show stop button
          else if (status = mpsPlaying) or (status = mpsPaused) then
          begin
            if item.ImageIndex <> 2 then
            begin
              item.ImageIndex:= 2;
              item.Caption:= _('Stop');
              item.Enabled:= true;
            end;
          end
          // show close button
          else
          begin
            if item.ImageIndex <> 14 then
            begin
              item.ImageIndex:= 14;
              item.Caption:= _('Close');
              item.Enabled:= true;
            end;          
          end;
        end;
        // previous
        3: begin
          TSpTBXItem(fControlItems.Items[i]).Visible:= Playlist.RootNodeCount > 1;
          if TSpTBXItem(fControlItems.Items[i]).Visible then
          TSpTBXItem(fControlItems.Items[i]).Enabled:= Playlist.GetPreviousItem <> nil;
        end;
        // next
        4: begin
          TSpTBXItem(fControlItems.Items[i]).Visible:= Playlist.RootNodeCount > 1;
          if TSpTBXItem(fControlItems.Items[i]).Visible then
          TSpTBXItem(fControlItems.Items[i]).Enabled:= Playlist.GetNextItem <> nil;
        end;
        // previous file
        5: begin
          TSpTBXItem(fControlItems.Items[i]).Enabled:= Filelist.GetPreviousItem <> nil;
        end;
        // next file
        6: begin
          TSpTBXItem(fControlItems.Items[i]).Enabled:= Filelist.GetNextItem <> nil;
        end;
        // loop mode
        7: begin
          item:= TSpTBXItem(fControlItems.Items[i]);
          case fLoopMode of
            lmNoLooping: begin
              item.Caption:= _('No Looping');
              item.ImageIndex:= 10;
            end;
            lmSingleFile: begin
              item.Caption:= _('Loop File');
              item.ImageIndex:= 11;
            end;
            lmPlaylist: begin
              item.Caption:= _('Loop Playlist');
              item.ImageIndex:= 12;
            end;
            lmFilelist: begin
              item.Caption:= _('Loop Filelist');
              item.ImageIndex:= 13;
            end;
          end;
        end;        
        71: TSpTBXItem(fControlItems.Items[i]).Checked:= fLoopMode = lmNoLooping;   // no looping
        72: TSpTBXItem(fControlItems.Items[i]).Checked:= fLoopMode = lmSingleFile;  // loop file
        73: TSpTBXItem(fControlItems.Items[i]).Checked:= fLoopMode = lmPlaylist;    // loop playlist
        74: TSpTBXItem(fControlItems.Items[i]).Checked:= fLoopMode = lmFilelist;    // loop filelist
        
        // playlist
        8: begin
          TSpTBXItem(fControlItems.Items[i]).Checked:= fLastPlaylistVisible;
        end;
        // mute
        9: begin
          if fMediaPlayer.HasAudio then
          begin
            TSpTBXItem(fControlItems.Items[i]).Visible:= true;
            TSpTBXItem(fControlItems.Items[i]).Checked:= fLastMute;
            if fLastMute then
            TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 9
            else
            TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 8;
          end
          else
          TSpTBXItem(fControlItems.Items[i]).Visible:= false;
        end;
        // volume
        10: begin
          if fMediaPlayer.HasAudio then
          begin
            TCETrackBar(fControlItems.Items[i]).Visible:= true;
            TCETrackBar(fControlItems.Items[i]).Position:= fLastVolume;
            if not fLastMute then
            TCETrackBar(fControlItems.Items[i]).SelEnd:= fLastVolume
            else
            TCETrackBar(fControlItems.Items[i]).SelEnd:= 0;
          end
          else
          TCETrackBar(fControlItems.Items[i]).Visible:= false;
        end;
        // time label
        21: begin
          if status = mpsError then
          begin
            TSpTBXLabelItem(fControlItems.Items[i]).Caption:= 'Error';
            TSpTBXLabelItem(fControlItems.Items[i]).Hint:= fMediaPlayer.GetStatusText;
          end;
        end;
      end;
    end;
    // Show toolbars
    if not toolbar_controls.Visible then
    begin
      toolbar_controls.Visible:= true;
      toolbar_controls.Invalidate;
    end
    else
    toolbar_controls.RightAlignItems;

    if not toolbar_seekbar.Visible then
    begin
      toolbar_seekbar.Visible:= fMediaPlayer.HasSeeking;
      toolbar_seekbar.Invalidate;
    end
    else
    toolbar_seekbar.Visible:= fMediaPlayer.HasSeeking;

    // Show Playlist
    if tabs_playlist.Visible <> fLastPlaylistVisible then
    begin
      tabs_playlist.Visible:= fLastPlaylistVisible;
      if fLastPlaylistVisible then
      begin
        if tabs_playlist.Align = alRight then
        begin
          splitter_left.Visible:= false;
          splitter_right.Visible:= true;
          splitter_right.Left:= tabs_playlist.Left - splitter_right.Width;
        end
        else if tabs_playlist.Align = alLeft then
        begin
          splitter_left.Visible:= true;
          splitter_right.Visible:= false;
          splitter_left.Left:= tabs_playlist.Width;
        end;
      end
      else
      begin
        splitter_left.Visible:= false;
        splitter_right.Visible:= false;
      end;
      WriteGlobalSettings;
    end;  
  end
  // not loaded, hide everything
  else
  begin
    toolbar_controls.Visible:= false;
    toolbar_seekbar.Visible:= false;
    tabs_playlist.Visible:= false;
    splitter_left.Visible:= false;
    splitter_right.Visible:= false;
  end;  
end;

{-------------------------------------------------------------------------------
  Read Global Settings
-------------------------------------------------------------------------------}
procedure TCEQuickView.ReadGlobalSettings;
begin
  // PlaylistVisibility
  fLastPlaylistVisible:= GlobalQuickViewSettings.PlaylistVisibility <> pvHidden;
  if GlobalQuickViewSettings.PlaylistVisibility = pvPlaylist then
  tab_playlist.Checked:= true
  else
  tab_filelist.Checked:= true;
  // LoopMode
  fLoopMode:= GlobalQuickViewSettings.LoopMode;
  fIsGlobalSettingsRead:= true;
end;

{-------------------------------------------------------------------------------
  Set CurrentFilePath
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetCurrentFilePath(const Value: WideString);
begin
  fCurrentFilePath:= Value;
  if assigned(fOnCurrentFileChange) then
  fOnCurrentFileChange(Self);
end;

{-------------------------------------------------------------------------------
  Write Global Settings
-------------------------------------------------------------------------------}
procedure TCEQuickView.WriteGlobalSettings;
begin
  if not fIsGlobalSettingsRead then
  Exit;
  
  // PlaylistVisibility
  if not tabs_playlist.Visible then
  GlobalQuickViewSettings.PlaylistVisibility:= pvHidden
  else if tab_playlist.Checked then
  GlobalQuickViewSettings.PlaylistVisibility:= pvPlaylist
  else
  GlobalQuickViewSettings.PlaylistVisibility:= pvFilelist;
  // LoopMode
  GlobalQuickViewSettings.LoopMode:= fLoopMode;
end;

{-------------------------------------------------------------------------------
  Update Seekbar State
-------------------------------------------------------------------------------}
procedure TCEQuickView.UpdateSeekbarState(Sender: TObject; APosition: Int64;
    ADuration: Int64);
var
  status: TCVMediaPlayerStatus;
  i, posI: Integer;
  track: TCETrackBar;
  s: String;
begin
  if not assigned(fMediaPlayer) then
  Exit;

  if (ADuration > 0) then
  posI:= Min(Round((MAXWORD / ADuration) * APosition), MAXWORD)
  else
  posI:= 0;

  // milliseconds to seconds
  if APosition > 0 then
  APosition:= APosition div 1000;
  if ADuration > 0 then
  ADuration:= ADuration div 1000;
  s:= SecToTime(APosition) + ' / ' + SecToTime(ADuration);
  status:= fMediaPlayer.GetStatus;

  for i:= 0 to fControlItems.Count - 1 do
  begin
    // Change seekbar positions
    if (fControlItems.Items[i] is TCETrackBar) and (fControlItems.Items[i].Tag = 20) then
    begin
      track:= TCETrackBar(fControlItems.Items[i]);
      track.SelEnd:= posI;
      if (not track.MouseInThumb) and (not track.Changing) then
      track.Position:= posI;
    end
    else if (fControlItems.Items[i] is TSpTBXLabelItem) and (fControlItems.Items[i].Tag = 21) then
    begin
      TSpTBXLabelItem(fControlItems.Items[i]).Hint:= fMediaPlayer.GetStatusText;
      if (status = mpsPlaying) or (status = mpsPaused) or (status = mpsStopped) or (status = mpsDone) then
      TSpTBXLabelItem(fControlItems.Items[i]).Caption:= s
      else if status <> mpsError then
      TSpTBXLabelItem(fControlItems.Items[i]).Caption:= fMediaPlayer.GetStatusText
      else
      begin
        TSpTBXLabelItem(fControlItems.Items[i]).Caption:= 'Error';
      end;
    end;     
  end;
end;

{##############################################################################}
// TCEQuickViewForm

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickViewForm
-------------------------------------------------------------------------------}
constructor TCEQuickViewForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  QuickView:= TCEQuickView.Create(Self);
  QuickView.Parent:= Self;
  QuickView.Align:= alClient;
  QuickView.OnCurrentFileChange:= HandleCurrentFileChange;
  QuickView.fIsDetached:= true;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCEQuickViewForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent:= GetDesktopWindow;
end;                                                           

{-------------------------------------------------------------------------------
  Do Close
-------------------------------------------------------------------------------}
procedure TCEQuickViewForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action:= caFree;
end;

{-------------------------------------------------------------------------------
  Handle ActiveFile Change
-------------------------------------------------------------------------------}
procedure TCEQuickViewForm.HandleCurrentFileChange(Sender: TObject);
begin
  if QuickView.ActiveFilePath = '' then
  Caption:= _('QuickView')
  else
  Caption:= QuickView.CurrentFilePath;
end;

{##############################################################################}
// TCEQuickViewSettings

{-------------------------------------------------------------------------------
  Create instance of TCEQuickViewSettings
-------------------------------------------------------------------------------}
constructor TCEQuickViewSettings.Create;
begin
  inherited;
  // TODO: make the extension settings dynamic for plugin support.
  fImageExtensions:= 'bmp,ico,wmf,emf,jfif,jpg,jpe,jpeg,rle,dib,win,vst,vda,tga,icb,tiff,tif,fax,eps,pcx,pcc,scr,rpf,rla,sgi,rgba,rgb,bw,psd,pdd,ppm,pgm,pbm,cel,pic,pcd,cut,psp,png,gif';
  fTextExtensions:= 'txt,ini,bat,html,htm,pas,css,xml,log,for,php,py,csv';
  fMediaExtensions:= 'avi,wmv,mp4,mpg,mpeg,ogg,ogm,mkv,dvr-ms,mp3,vob,wav,flv';
  fWMPExtensions:= fMediaExtensions;
  fDirectShowExtensions:= fMediaExtensions;
  fMediaPlayer:= mptWMP;
  fLoopMode:= lmNoLooping;
  fSlideshowInterval:= 5000;
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickViewSettings
-------------------------------------------------------------------------------}
destructor TCEQuickViewSettings.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  CreateMediaEngine
-------------------------------------------------------------------------------}
function TCEQuickViewSettings.CreateMediaEngine(AExtension: WideString):
    ICVMediaEngine;
var
  list: TCCStringList;
begin
  Result:= nil;

  if AExtension = '' then
  Exit;

  if (AExtension[1] = '.') then
  Delete(AExtension, 1, 1);

  list:= TCCStringList.Create;
  try
    list.Delimiter:= ',';
    // images
    list.DelimitedText:= fImageExtensions;
    if list.IndexOf(AExtension) > -1 then
    Result:= TCVImageEngine.Create;
    // text
    list.DelimitedText:= fTextExtensions;
    if list.IndexOf(AExtension) > -1 then
    Result:= TCVMemoEngine.Create;
    // media
    if fMediaPlayer <> mptAuto then
    begin
      list.DelimitedText:= fMediaExtensions;
      if list.IndexOf(AExtension) > -1 then
      begin
        if fMediaPlayer = mptWMP then
        Result:= TCVWMPEngine.Create
        else if fMediaPlayer = mptDirectShow then
        Result:= TCVDSEngine.Create;
      end;
    end
    else
    begin
      // wmp
      list.DelimitedText:= fWMPExtensions;
      if list.IndexOf(AExtension) > -1 then
      Result:= TCVWMPEngine.Create;

      // directshow
      if not assigned(Result) then
      begin
        list.DelimitedText:= fDirectShowExtensions;
        if list.IndexOf(AExtension) > -1 then
        Result:= TCVDSEngine.Create;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  IsSupported
-------------------------------------------------------------------------------}
function TCEQuickViewSettings.IsSupported(AExtension: WideString; AExcludeText:
    Boolean = false): Boolean;
var
  list: TCCStringList;
begin
  // TODO: optimize and make thread-safe
  Result:= false;
  
  if AExtension = '' then
  Exit;

  if (AExtension[1] = '.') then
  Delete(AExtension, 1, 1);

  list:= TCCStringList.Create;
  try
    list.Delimiter:= ',';

    if not AExcludeText then
    begin
      if fMediaPlayer = mptAuto then
      list.DelimitedText:= fImageExtensions +','+ fTextExtensions +','+ fWMPExtensions +','+ fDirectShowExtensions
      else
      list.DelimitedText:= fImageExtensions +','+ fTextExtensions +','+ fMediaExtensions;
    end
    else
    begin
      if fMediaPlayer = mptAuto then
      list.DelimitedText:= fImageExtensions +','+ fWMPExtensions +','+ fDirectShowExtensions
      else
      list.DelimitedText:= fImageExtensions +','+ fMediaExtensions;
    end;
    
    Result:= list.IndexOf(AExtension) > -1;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Set DirectShow Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetDirectShowExtensions(const Value: WideString);
begin
  fDirectShowExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Image Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetImageExtensions(const Value: WideString);
begin
  fImageExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Media Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetMediaExtensions(const Value: WideString);
begin
  fMediaExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Text Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetTextExtensions(const Value: WideString);
begin
  fTextExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set WMP Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetWMPExtensions(const Value: WideString);
begin
  fWMPExtensions:= WideLowerCase(Value);
end;

{##############################################################################}
// TCEMediaEngineSettings

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCEMediaEngineSettings.Load(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
begin
 
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
procedure TCEMediaEngineSettings.Save(AAppStorage: TCEAppSettings; ANode:
    TDOMNode);
begin
  //ANode.appendChild(AAppStorage.XML.CreateElement('test'));
 
end;

{##############################################################################}

initialization
  GlobalQuickViewSettings:= TCEQuickViewSettings.Create;
  GlobalAppSettings.AddItem('QuickView', GlobalQuickViewSettings, true);

finalization
  FreeAndNil(GlobalQuickViewSettings);

end.
