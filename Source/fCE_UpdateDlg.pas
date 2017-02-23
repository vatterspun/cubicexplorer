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
//  The Original Code is fCE_UpdateDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_UpdateDlg;

interface

uses
  // CE Units
  CE_ArchiveTree,
  // VT
  VirtualTrees,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TntStdCtrls, ComCtrls;

type

  TCEUpdateDlg = class(TForm)
    but_cancel: TTntButton;
    but_update: TTntButton;
    Panel1: TPanel;
    TntLabel1: TTntLabel;
    progressbar: TProgressBar;
    timer_close_dlg: TTimer;
    procedure but_updateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure timer_close_dlgTimer(Sender: TObject);
  private
    fDestinationDir: WideString;
    fFailed: Boolean;
  public
    ArchiveTree: TCEArchiveTree;
    procedure HandleProgress(Sender: TObject; const Value, MaxValue: Int64);
    procedure OpenArchive(AFilePath: WideString);
    procedure RenameOpenFiles(DeleteOldFiles: Boolean);
    property DestinationDir: WideString read fDestinationDir write fDestinationDir;
  published
    property Failed: Boolean read fFailed;
  end;

implementation

uses
  TntClasses, TntSysUtils;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On FormCreate
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ArchiveTree:= TCEArchiveTree.Create(Self);
  ArchiveTree.Parent:= Panel1;
  ArchiveTree.Align:= alClient;
  ArchiveTree.CheckBoxSelection:= true;
  ArchiveTree.OnProgress:= HandleProgress;
  for i:= 1 to ArchiveTree.Header.Columns.Count - 1 do
  begin
    ArchiveTree.Header.Columns.Items[i].Options:= ArchiveTree.Header.Columns.Items[i].Options - [coVisible];
  end;
  ArchiveTree.Header.AutoSizeIndex:= 0;
  ArchiveTree.Header.Options:= ArchiveTree.Header.Options + [hoAutoResize] - [hoVisible];
  //ArchiveTree.TreeOptions.PaintOptions:= ArchiveTree.TreeOptions.PaintOptions - [toShowRoot];
end;

{-------------------------------------------------------------------------------
  Open Archive
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.OpenArchive(AFilePath: WideString);
begin
  but_update.Enabled:= ArchiveTree.OpenArchive(AFilePath);
  ArchiveTree.CheckAll;
end;

{-------------------------------------------------------------------------------
  On but_update.Click
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.but_updateClick(Sender: TObject);
begin
  but_update.Enabled:= false;
  RenameOpenFiles(true);
  try
    ArchiveTree.ExtractCheckedTo(DestinationDir, true);
  except
    MessageBox(0, 'There was an error during update! Try to re-download the update.', 'Error', MB_ICONERROR or MB_OK);
  end;
  timer_close_dlg.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

{-------------------------------------------------------------------------------
  Handle Progress
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.HandleProgress(Sender: TObject; const Value, MaxValue: Int64);
begin
  if (Value > 0) and (MaxValue > 0) then
  progressbar.Position:= Round(100 * (Value / MaxValue))
  else
  progressbar.Position:= 0;
end;

{-------------------------------------------------------------------------------
  Rename Open Files
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.RenameOpenFiles(DeleteOldFiles: Boolean);
var
  list: TTntStrings;
  i,i2: Integer;
  path, oldPath: WideString;
begin
  list:= TTntStringList.Create;
  try
    if ArchiveTree.VerifyExtract(DestinationDir, list, true, true, true) > 0 then
    begin
      for i:= 0 to list.Count - 1 do
      begin
        if Integer(list.Objects[i]) = ERROR_SHARING_VIOLATION then
        begin
          path:= list.Strings[i];
          // Delete old file
          if DeleteOldFiles then
          begin
            oldPath:= path + '.old';
            if WideFileExists(oldPath) then
            WideDeleteFile(oldPath);
          end
          else // Find available path for old file
          begin
            oldPath:= path + '.old';
            i2:= 1;
            while WideFileExists(oldPath) do
            begin
              i2:= i2 + 1;
              oldPath:= path + '.' + IntToStr(i2) + '.old';
            end;
          end;

          // Rename file
          WideRenameFile(path, oldPath);
        end;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  On timer_close_dlg.Timer
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.timer_close_dlgTimer(Sender: TObject);
begin
  Self.ModalResult:= mrOK;
end;


end.
