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
//  The Original Code is fCE_OptionsPage_General.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Advanced;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine, CE_AppSettings,
  // Tnt
  TntStdCtrls,
  // JVCL
  JvExControls, JvInspector, JvComponentBase, 
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SpTBXItem, SpTBXControls;

type
  TJvCustomInspectorHack = class(TJvCustomInspector);
  TJvCustomInspectorItemHack = class(TJvCustomInspectorItem);
  
  TCEInspector_Painter = class(TJvInspectorBorlandPainter)
  protected
    procedure DoPaint; override;
  end;

  TCEOptionsPage_Advanced = class(TCEOptionsCustomPage)
    Inspector: TJvInspector;
    SpTBXLabel1: TSpTBXLabel;
    procedure InspectorItemValueChanged(Sender: TObject;
      Item: TJvCustomInspectorItem);
  private
    Painter: TCEInspector_Painter;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddComponents;
    procedure ApplySettings; override;
    { Public declarations }
  end;

implementation

uses
  Main, fCE_FileView;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_General
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_Advanced.Create(AOwner: TComponent);
begin
  inherited;
  Painter:= TCEInspector_Painter.Create(self);
  Inspector.Painter:= Painter;
  Inspector.Style:= isItemPainter;
  PageName:= _('Advanced');
  PageTitle:= _('Advanced Settings');
  PagePath:= 'Advanced';
  ImageIndex:= 3;
  PageListPosition:= 4;
  AddComponents;
end;

procedure TCEOptionsPage_Advanced.InspectorItemValueChanged(Sender: TObject;
  Item: TJvCustomInspectorItem);
begin
  inherited;
  Self.HandleChange(Self);
end;

{-------------------------------------------------------------------------------
  Add Components
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Advanced.AddComponents;
var
  i: Integer;
  item: TCEAppSettingItem;              
begin
  Inspector.Clear;
  for i:= 0 to GlobalAppSettings.Count - 1 do
  begin
    item:= GlobalAppSettings.Items[i];
    Inspector.AddComponent(item.ObjectToSave, item.NodeName);
  end;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Advanced.ApplySettings;
begin
  GlobalFileViewSettings.SendChanges;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Do Paint
-------------------------------------------------------------------------------}
procedure TCEInspector_Painter.DoPaint;
var
  TmpRect: TRect;
begin
  Canvas.Brush.Style:= bsSolid;
  TmpRect := Rects[iprItem];
  if (Item = TJvCustomInspectorHack(Inspector).Selected) and (not TJvCustomInspectorItemHack(Item).IsCategory) then
  begin
    Canvas.Brush.Color:= clHighlight;
    Canvas.FillRect(TmpRect);
    TmpRect:= Rects[iprValueArea];
    Canvas.Brush.Color:= clWindow;
    Canvas.FillRect(TmpRect);

    InflateRect(TmpRect, 0, 1);
    TmpRect.Top:= TmpRect.Top - 1;
    TmpRect.Left:= TmpRect.Left - 2;
    TmpRect.Right:= TmpRect.Right + 2;
    Frame3D(Canvas, TmpRect, clGray, clWhite, 1);
    Frame3D(Canvas, TmpRect, clGray, cl3DLight, 1);
    PaintDivider(TmpRect.Left + TJvCustomInspectorHack(Inspector).DividerAbs, Pred(TmpRect.Top), TmpRect.Bottom);
    Canvas.Font.Color:= clHighlightText;
    Canvas.Font.Style:= [];
  end
  else if not TJvCustomInspectorItemHack(Item).IsCategory then
  begin
    Canvas.Brush.Color:= clWindow;
    Canvas.FillRect(TmpRect);
    PaintDivider(TmpRect.Left + TJvCustomInspectorHack(Inspector).DividerAbs, Pred(TmpRect.Top), TmpRect.Bottom);
    Canvas.Font.Color:= clWindowText;
    Canvas.Font.Style:= [];
  end
  else
  begin
    Canvas.Brush.Color:= clBtnFace;
    Canvas.FillRect(TmpRect);
    Canvas.Font.Color:= clBtnText;
    Canvas.Font.Style:= [fsBold];
  end;

  Canvas.Brush.Style:= bsClear;
  Item.DrawName(Canvas);
  ApplyValueFont;
  Item.DrawValue(Canvas);

  if ButtonImage <> nil then
  Canvas.CopyRect(Rects[iprBtnDstRect], ButtonImage.Canvas, Rects[iprBtnSrcRect]);
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_Advanced);

finalization

end.
