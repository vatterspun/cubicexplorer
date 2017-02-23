// Created by Marko Savolainen

unit SpTBXChromeSkin;

interface

uses
  SpTBXSkins,
  Graphics, Windows, Controls;

type
  TSpTBXChromeSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component:
        TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders:
        Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []);
        override;
  end;

implementation

{-------------------------------------------------------------------------------
  Fill Options
-------------------------------------------------------------------------------}
procedure TSpTBXChromeSkin.FillOptions;
begin
  SkinName:= 'Chrome';
  SkinAuthor:= 'Marko Savolainen';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(3, $fdf7f2, $f8eadf, $f8eadf, $f8eadf);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $f8eadf, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $b2b2b2, $b2b2b2, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(1, $fdf7f2, $f8eadf, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, $f8eadf, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $b2b2b2, $b2b2b2, clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, $fdf7f2, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $b2b2b2, $b2b2b2, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(1, $fdf7f2, $f8eadf, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(0, $f8eadf, $f8eadf, clNone, clNone);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $fdf7f2, $fdf7f2, $b2b2b2, $b2b2b2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, $e7bc98, $d5ad8c, clNone, clNone);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $b2b2b2, $f8eadf, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $b2b2b2, $f8eadf, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $b2b2b2, $f8eadf, clNone, clNone);

  //---- Buttons ----//

  //ToolbarItem
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(1, $ffffff, $fbf3ec, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(2, $b0a8a8, $f8eadf, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(1, $e9bea0, $e4b095, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  Options(skncToolbarItem, sknsDisabled).TextColor := $a0a0a0;
  // MenuBarItem
  Options(skncMenuBarItem, sknsHotTrack).Body.Fill(0, $fbf6f2, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsHotTrack).Borders.Fill(2, $ded6cf, $ded6cf, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).Body.Fill(0, $fffaf6, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).Borders.Fill(2, $b0a8a8, $f8eadf, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3a994, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $a8a6a6, $f8eadf, clNone, clNone);
  // MenuItem
  Options(skncMenuItem, sknsHotTrack).Body.Fill(0, $f9c9a9, $e7bc98, clNone, clNone);
  Options(skncMenuItem, sknsHotTrack).Borders.Fill(2, $fdf7f2, $fdf7f2, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Borders.Fill(2, $b0a8a8, $f8eadf, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3a994, clNone, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Fill(2, $a8a6a6, $f8eadf, clNone, clNone);
  // Button
  Options(skncButton, sknsNormal).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(2, $eddace, $eddace, clNone, clNone);
  Options(skncButton, sknsDisabled).TextColor := $a0a0a0;
  Options(skncButton, sknsHotTrack).Body.Fill(1, $ffffff, $fbf3ec, clNone, clNone);
  Options(skncButton, sknsHotTrack).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncButton, sknsPushed).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  Options(skncButton, sknsChecked).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(1, $e9bea0, $e4b095, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(2, $b2b2b2, $f8eadf, clNone, clNone);
  // List Item
  Options(skncListItem, sknsChecked).Body.Fill(0, $f9c9a9, $e7bc98, clNone, clNone);
  Options(skncListItem, sknsHotTrack).Body.Fill(0, $f9c9a9, $e7bc98, clNone, clNone);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3a994, clNone, clNone, clNone);
  // Check Box
  Options(skncCheckBox, sknsNormal).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncCheckBox, sknsNormal).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(2, $eddace, $eddace, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).TextColor := $a0a0a0;
  Options(skncCheckBox, sknsHotTrack).Body.Fill(1, $ffffff, $fbf3ec, clNone, clNone);
  Options(skncCheckBox, sknsHotTrack).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  // Radio Button
  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  
  // Edit Frame
  Options(skncEditFrame, sknsNormal).Borders.Fill(2, $e2cdc5, $e2cdc5, clNone, clNone);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(2, $eddace, $eddace, clNone, clNone);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(2, $d5ad8c, $d5ad8c, clNone, clNone);
  // Edit Button
  Options(skncEditButton, sknsHotTrack).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncEditButton, sknsHotTrack).Borders.Fill(0, $eddad3, $eddad3, clNone, clNone);
  Options(skncEditButton, sknsPushed).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncEditButton, sknsPushed).Borders.Fill(0, $eddad3, $eddad3, clNone, clNone);
  Options(skncEditButton, sknsChecked).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncEditButton, sknsChecked).Borders.Fill(0, $eddad3, $eddad3, clNone, clNone);
  Options(skncEditButton, sknsCheckedAndHotTrack).Body.Fill(1, $e9bea0, $e4b095, clNone, clNone);
  Options(skncEditButton, sknsCheckedAndHotTrack).Borders.Fill(0, $eddad3, $eddad3, clNone, clNone);

  //---- Tabs ----//
  Options(skncTab, sknsNormal).Body.Fill(0, $E5C4A8, $EACBB2, clNone, clNone);
  Options(skncTab, sknsNormal).Borders.Fill(2, $cbad95, $cbad95, clNone, clNone);
  Options(skncTab, sknsDisabled).Body.Fill(0, $e6c4a9, $e6c4a9, clNone, clNone);
  Options(skncTab, sknsDisabled).Borders.Fill(2, $eddad3, $eddad3, clNone, clNone);
  Options(skncTab, sknsHotTrack).Body.Fill(0, $EAD6C6, $EAD6C6, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(2, $ccae96, $ccae96, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(0, $f8eadf, $f8eadf, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(2, $988270, $988270, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(0, $f8eadf, $f8eadf, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $ccae96, $ccae96, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $f8eadf, $f8eadf, clNone, clNone);
  Options(skncTabToolbar, sknsNormal).Body.Fill(1, $e7bc98, $d5ad8c, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(1, $fefaf7, $f7ede5, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $00E3D1CB, $00E3D1CB, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(1, $fcccac, $e7bc98, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $00E3D1CB, $00E3D1CB, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(1, $fefaf7, $f4e1e2, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(2, $00DCC2B9, $00DCC2B9, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(1, $f4e1e2, $fcccac, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(2, $00DCC2B9, $00DCC2B9, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(0, $FDF7F2, clNone, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $e0e0e0, $e0e0e0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(1, $ffffff, $fbf3ec, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $eddad3, $eddad3, clNone, clNone);
  Options(skncHeader, sknsPushed).Body.Fill(1, $fdf7f3, $f8ece3, clNone, clNone);
  Options(skncHeader, sknsPushed).Borders.Fill(1, $b2b2b2, $f8eadf, clNone, clNone);
end;

{-------------------------------------------------------------------------------
  Paint Background
-------------------------------------------------------------------------------}
procedure TSpTBXChromeSkin.PaintBackground(ACanvas: TCanvas; ARect: TRect;
    Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType;
    Background, Borders, Vertical: Boolean; ForceRectBorders: TAnchors);
begin
  if Component = skncTabBackground then
  begin
    SpFillRect(ACanvas, ARect, $f8eadf);
    ACanvas.Pen.Style:= psSolid;
    ACanvas.Pen.Color:= $988270;
    if Borders then
    begin
      ACanvas.MoveTo(ARect.Left, ARect.Top);
      ACanvas.LineTo(ARect.Right, ARect.Top);
    end;
  end
  else if Component = skncTabToolbar then
  begin
    SpGradientFill(ACanvas, ARect, $e7bc98, $d5ad8c, not Vertical);
    ACanvas.Pen.Style:= psSolid;
    ACanvas.Pen.Color:= $988270;
    ACanvas.MoveTo(ARect.Left, ARect.Top+0);
    ACanvas.LineTo(ARect.Right, ARect.Top+0);
    ACanvas.Pen.Color:= $ccae96;
    ACanvas.MoveTo(ARect.Left, ARect.Top+1);
    ACanvas.LineTo(ARect.Right, ARect.Top+1);
  end
  else
  inherited;
end;

{##############################################################################}

initialization
  SkinManager.SkinsList.AddSkin('Chrome', TSpTBXChromeSkin);

end.
