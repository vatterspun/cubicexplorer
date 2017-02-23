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
//  The Original Code is CE_Layout.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Layout;

interface

uses
  // CE Units
  CE_Utils, CE_DockInfo, CE_Toolbar,
  // Toolbar200
  TB2Toolbar, TB2Dock, TB2ToolWindow,
  // SpTBX
  SpTBXItem, SpTBXTabs,
  // TNT
  TntSysUtils, TntClasses, TntActnList,
  // JVCL
  JvAppStorage, JvDockVSNetStyle, JvDockInfo, JvDockControlForm,
  JvDockSupportProc, JvDockGlobals, JvAppXMLStorage, JvSimpleXML,
  // mbTBX
  CEJvDockVSNetStyleTBX,
  // System Units
  Classes, Contnrs, Windows, SysUtils, Forms, TB2Item, Messages, Controls;

type
  TJvCustomAppXMLStorageHack = class(TJvCustomAppXMLStorage);
  TTBCustomDockableWindowHack = class(TTBCustomDockableWindow);
  TSpTBXToolbarAccess = class(TSpTBXToolbar);

  TCEToolbarDockType = (tdtBoth, tdtInner, tdtOuter);

  TCELayoutItems = class(TComponentList)
  public
    procedure PopulateMenuItem(RootItem: TTBCustomItem);
  end;

  TCEToolbarDocks = class(TObject)
  protected
  public
    OuterDocks: TComponentList;
    InnerDocks: TComponentList;
    constructor Create; overload;
    destructor Destroy; override;
    function Add(ADock: TTBDock; InnerDock: Boolean = false): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindDockNamed(AName: String): TTBDock; overload;
    function FindDockNamed(AName: String; out IsInner: Boolean): TTBDock; overload;
    function FindInnerDockNamed(AName: String): TTBDock;
    function FindOuterDockNamed(AName: String): TTBDock;
    function IsInnerDock(ADock: TTBDock): Boolean;
    function IsOuterDock(ADock: TTBDock): Boolean;
  end;

  TCELayoutController = class(TComponent)
  private
    fAutoSave: Boolean;
    fCurrentLayout: String;
    fFilePath: WideString;
    procedure InitSelf;
  protected
    CurrentFormLayout: string;
    CurrentInnerToolbarLayout: string;
    CurrentOuterToolbarLayout: string;
  public
    Layouts: TStrings;
    AppStorage: TJvAppXMLFileStorage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadDefaultLayout;
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadLayout(LayoutName: String; LoadInnerToolbarLayout,
        LoadOuterToolbarLayout, LoadFormLayout: Boolean; ForceReload: Boolean =
        false);
    procedure LoadSettingsForToolbars;
    procedure SaveCurrentLayout;
    procedure SaveLayout(LayoutName: String; SaveInnerToolbarLayout,
        SaveOuterToolbarLayout, SaveFormsLayout: Boolean);
    procedure SaveToFile(AFilePath: WideString = '');
    procedure SaveSettingsForToolbars;
    property AutoSave: Boolean read fAutoSave write fAutoSave;
    property CurrentLayout: String read fCurrentLayout write fCurrentLayout;
    property FilePath: WideString read fFilePath write fFilePath;
  end;


  procedure LoadPositionsForToolbars(AppStorage: TJvCustomAppStorage;
      AppStoragePath: String = ''; DockType: TCEToolbarDockType = tdtBoth);
  procedure SavePositionsForToolbars(AppStorage: TJvCustomAppStorage;
      AppStoragePath: String = ''; DockType: TCEToolbarDockType = tdtBoth);
  procedure LoadDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = ''; HandleMainForm: Boolean = false);
  procedure SaveDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = '');
  procedure CE_DoFloatForm(DockForm: TControl);
  procedure CE_DoFloatAllForm;

procedure BeginToolbarCustomize;

procedure EndToolbarCustomize;

procedure LoadSettingsForToolbars(AppStorage: TJvAppXMLFileStorage;
    AppStoragePath: String = '');

procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

procedure LoadToolbarProperties(Toolbar: TTBCustomDockableWindow; ToolbarNode:
    TJvSimpleXMLElem);

procedure SaveToolbarProperties(Toolbar: TTBCustomDockableWindow; ToolbarNode:
    TJvSimpleXMLElem);

  procedure SaveSettingsForToolbars(AppStorage: TJvAppXMLFileStorage;
      AppStoragePath: String = '');

var
  DefaultLayoutName: String = 'Default';
  CELayoutItems: TCELayoutItems;
  CEToolbarDocks: TCEToolbarDocks;
  CEDockStyle: TJvDockVSNetStyleTBX;

implementation

uses
  fCE_DockHostForm, fCE_Customizer, Main, fCE_DockableForm, CE_SpTabBar,
  CE_StatusBar, dCE_Actions, CE_TBActions, dCE_Images, CE_ToolbarEditorItems,
  SpTBXEditors;

{*------------------------------------------------------------------------------
  Create an instance of TCELayoutController
-------------------------------------------------------------------------------}
constructor TCELayoutController.Create(AOwner: TComponent);
begin
  inherited;
  Layouts:= TStringList.Create;
  AppStorage:= TJvAppXMLFileStorage.Create(nil);
  AppStorage.AutoFlush:= false;
  AppStorage.AutoReload:= false;
  AppStorage.FlushOnDestroy:= false;
  AppStorage.Path:= 'Layouts';
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCELayoutController
-------------------------------------------------------------------------------}
destructor TCELayoutController.Destroy;
begin
  AppStorage.Free;
  Layouts.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize layouts.
-------------------------------------------------------------------------------}
procedure TCELayoutController.InitSelf;

  procedure CloneNode(FromNode, ToNode: TJvSimpleXmlElem);
  var
    i, i2: Integer;
    chFromNode, chToNode: TJvSimpleXmlElem;
  begin
    for i:= 0 to FromNode.Items.Count - 1 do
    begin
      chFromNode:= FromNode.Items.Item[i];
      chToNode:= ToNode.Items.Add(chFromNode.Name);
      chToNode.Value:= chFromNode.Value;
      for i2:= 0 to chFromNode.Properties.Count - 1 do
      chToNode.Properties.Add(chFromNode.Properties[I].Name, chFromNode.Properties[I].Value);
      CloneNode(chFromNode, chToNode);
    end;
  end;

var
  node, chNode, chNode2: TJvSimpleXmlElem;
  i: Integer;
begin
  Layouts.BeginUpdate;
  Layouts.Clear;
  try
    node:= AppStorage.Xml.Root.Items.ItemNamed['Layouts'];
    if node <> nil then
    begin
      for i:= 0 to node.Items.Count-1 do
      begin
        Layouts.Add(node.Items.Item[i].Name);
      end;

      // Make sure older layout files work
      if Layouts.IndexOf(DefaultLayoutName) = -1 then
      begin
        chNode:= node.Items.ItemNamed['FileView'];
        if assigned(chNode) then
        begin
          chNode2:= node.Items.Add(DefaultLayoutName);
          chNode2.Name:= DefaultLayoutName;
          CloneNode(chNode, chNode2);
        end;
      end;
    end;

  finally
    Layouts.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Load Default Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadDefaultLayout;
begin
  AppStorage.BeginUpdate;
  try
    AppStorage.Xml.LoadFromResourceName(hInstance, 'DEFAULT_LAYOUT_DATA');
  finally
    AppStorage.EndUpdate;
    InitSelf;
  end;
end;

{*------------------------------------------------------------------------------
  Load layout configuration from file
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadFromFile(AFilePath: WideString);
var
  s: TStream;
begin
  fFilePath:= AFilePath;
  AppStorage.BeginUpdate;
  try
    if WideFileExists(fFilePath) then
    begin
      s:= TTntFileStream.Create(fFilePath, fmOpenRead or fmShareDenyNone);
      try
        s.Position:= 0;
        try
          AppStorage.Xml.LoadFromStream(s);
        except
          AppStorage.Xml.LoadFromResourceName(hInstance, 'DEFAULT_LAYOUT_DATA');
        end;
      finally
        s.Free;
      end;
    end
    else
    begin
      AppStorage.Xml.LoadFromResourceName(hInstance, 'DEFAULT_LAYOUT_DATA');
    end;
  finally
    AppStorage.EndUpdate;
    InitSelf;
  end;
end;

{*------------------------------------------------------------------------------
  Save layout configuration to file
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveToFile(AFilePath: WideString = '');
var
  s: TStream;
begin
  try
    if AFilePath <> '' then
    fFilePath:= AFilePath;
    s:= TTntFileStream.Create(fFilePath, fmCreate);
    try
      s.Position:= 0;
      AppStorage.Xml.SaveToStream(s);
    finally
      s.Free;
    end;
  except
  end;
end;

{*------------------------------------------------------------------------------
  Load layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadLayout(LayoutName: String;
    LoadInnerToolbarLayout, LoadOuterToolbarLayout, LoadFormLayout: Boolean;
    ForceReload: Boolean = false);
var
  s: String;
begin
  if Layouts.IndexOf(LayoutName) <> -1 then
  begin
    fCurrentLayout:= LayoutName;
    MainForm.BeginUIUpdate;
    try
      // Load Form Layout
      if LoadFormLayout then
      s:= LayoutName
      else
      s:= DefaultLayoutName;
      if (s <> CurrentFormLayout) or ForceReload then
      begin
        LoadDockedForms(AppStorage, s);
        CurrentFormLayout:= s;
      end;

      // Load Toolbar Layout
      if LoadInnerToolbarLayout and LoadOuterToolbarLayout then
      begin
        LoadPositionsForToolbars(AppStorage, LayoutName, tdtBoth);
        CurrentInnerToolbarLayout:= LayoutName;
        CurrentOuterToolbarLayout:= LayoutName;
      end
      else if (not LoadInnerToolbarLayout) and (not LoadOuterToolbarLayout) then
      begin
        LoadPositionsForToolbars(AppStorage, DefaultLayoutName, tdtBoth);
        CurrentInnerToolbarLayout:= DefaultLayoutName;
        CurrentOuterToolbarLayout:= DefaultLayoutName;
      end
      else
      begin
        // Outer Toolbar Layout
        if LoadOuterToolbarLayout then
        begin
          s:= LayoutName;
          if (s <> CurrentOuterToolbarLayout) or ForceReload then
          begin
            LoadPositionsForToolbars(AppStorage, s, tdtOuter);
            CurrentOuterToolbarLayout:= s;
          end;
        end
        else
        begin
          s:= DefaultLayoutName;
          LoadPositionsForToolbars(AppStorage, s, tdtOuter);
          CurrentOuterToolbarLayout:= s;
        end;

        // Inner Toolbar Layout
        if LoadInnerToolbarLayout then
        s:= LayoutName
        else
        s:= DefaultLayoutName;
        if (s <> CurrentInnerToolbarLayout) or ForceReload then
        begin
          LoadPositionsForToolbars(AppStorage, s, tdtInner);
          CurrentInnerToolbarLayout:= s;
        end;
      end;
    finally
      MainForm.EndUIUpdate;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveLayout(LayoutName: String;
    SaveInnerToolbarLayout, SaveOuterToolbarLayout, SaveFormsLayout: Boolean);
var
  s: String;
begin
  if SaveFormsLayout then
  s:= LayoutName
  else
  s:= DefaultLayoutName;
  SaveDockedForms(AppStorage, s);

  if SaveInnerToolbarLayout and SaveOuterToolbarLayout then // Save both to Layout
  begin
    SavePositionsForToolbars(AppStorage, LayoutName, tdtBoth);
  end
  else if (not SaveInnerToolbarLayout) and (not SaveOuterToolbarLayout) then // Save both to Default layout
  begin
    SavePositionsForToolbars(AppStorage, DefaultLayoutName, tdtBoth);
  end
  else if SaveInnerToolbarLayout then // Save both to Layout and Outer to Default layout
  begin
    // Both need's to be saved in case inner toolbar has moved to outer dock.
    // Because saving is done by looping toolbars, changes would not be saved
    // if the toolbar is on Outer dock.
    // TODO: It would be better to loop Docks instead.
    SavePositionsForToolbars(AppStorage, LayoutName, tdtBoth);
    SavePositionsForToolbars(AppStorage, DefaultLayoutName, tdtOuter);
  end
  else // Save Outer to Layout and Inner to Default layout
  begin
    SavePositionsForToolbars(AppStorage, LayoutName, tdtOuter);
    SavePositionsForToolbars(AppStorage, DefaultLayoutName, tdtInner);
  end;

  InitSelf;
  if fAutoSave then
  SaveToFile;
end;

{*------------------------------------------------------------------------------
  Load Toolbars Items
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadSettingsForToolbars;
begin
  CE_Layout.LoadSettingsForToolbars(AppStorage);
end;

{-------------------------------------------------------------------------------
  Save Current Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveCurrentLayout;
begin
  SaveDockedForms(AppStorage, CurrentFormLayout);
  if CurrentInnerToolbarLayout <> CurrentOuterToolbarLayout then
  begin
    // Both need's to be saved in case inner toolbar has moved to outer dock.
    // Because saving is done by looping toolbars, changes would not be saved
    // if the toolbar is on Outer dock.
    // TODO: It would be better to loop Docks instead.
    SavePositionsForToolbars(AppStorage, CurrentInnerToolbarLayout, tdtBoth);

    SavePositionsForToolbars(AppStorage, CurrentOuterToolbarLayout, tdtOuter);
  end
  else
  begin
    SavePositionsForToolbars(AppStorage, CurrentInnerToolbarLayout, tdtBoth);
  end;
end;

{*------------------------------------------------------------------------------
  Saves Toolbars items
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveSettingsForToolbars;
begin
  CE_Layout.SaveSettingsForToolbars(AppStorage);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarDocks
-------------------------------------------------------------------------------}
constructor TCEToolbarDocks.Create;
begin
  inherited Create;
  InnerDocks:= TComponentList.Create(false);
  OuterDocks:= TComponentList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCEToolbarDocks
-------------------------------------------------------------------------------}
destructor TCEToolbarDocks.Destroy;
begin
  InnerDocks.Free;
  OuterDocks.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add
-------------------------------------------------------------------------------}
function TCEToolbarDocks.Add(ADock: TTBDock; InnerDock: Boolean = false):
    Integer;
begin
  if InnerDock then
  Result:= InnerDocks.Add(ADock)
  else
  Result:= OuterDocks.Add(ADock);
end;

{*------------------------------------------------------------------------------
  Begin Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.BeginUpdate;
var
  i: Integer;
begin
  for i:= 0 to InnerDocks.Count-1 do
  begin
    TTBDock(InnerDocks.Items[i]).BeginUpdate;
  end;
  for i:= 0 to OuterDocks.Count-1 do
  begin
    TTBDock(OuterDocks.Items[i]).BeginUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  End Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.EndUpdate;
var
  i: Integer;
begin
  for i:= 0 to InnerDocks.Count-1 do
  begin
    TTBDock(InnerDocks.Items[i]).EndUpdate;
  end;
  for i:= 0 to OuterDocks.Count-1 do
  begin
    TTBDock(OuterDocks.Items[i]).EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Find Dock by it's name
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  // Outer Docks
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
  // Inner Docks
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      Break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Find Dock by it's name
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindDockNamed(AName: String; out IsInner: Boolean):
    TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  IsInner:= false;
  // Outer Docks
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
  // Inner Docks
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      IsInner:= true;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Inner Dock Named
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindInnerDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Outer Dock Named
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindOuterDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Is Inner Dock
-------------------------------------------------------------------------------}
function TCEToolbarDocks.IsInnerDock(ADock: TTBDock): Boolean;
begin
  Result:= InnerDocks.IndexOf(ADock) > -1;
end;

{-------------------------------------------------------------------------------
  Is Outer Dock
-------------------------------------------------------------------------------}
function TCEToolbarDocks.IsOuterDock(ADock: TTBDock): Boolean;
begin
  Result:= OuterDocks.IndexOf(ADock) > -1;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Load toolbar positions from AppStorage.
-------------------------------------------------------------------------------}
procedure LoadPositionsForToolbars(AppStorage: TJvCustomAppStorage;
    AppStoragePath: String = ''; DockType: TCEToolbarDockType = tdtBoth);
var
  OldPath: String;
  RootPath: String;
  i, i2: Integer;
  b, loadPos: Boolean;
  s: String;
  dockablewindow: TTBCustomDockableWindow;
  dock: TTBDock;
  tabset: TCESpTabSet;
  statusbar: TCEStatusBar;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, AppStoragePath, 'Toolbars']);
    RootPath:= AppStorage.Path;
    CEToolbarDocks.BeginUpdate;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      // Toolbars
      if CELayoutItems.Items[i] is TTBCustomDockableWindow then
      begin
        dockablewindow:= TTBCustomDockableWindow(CELayoutItems.Items[i]);
        dockablewindow.BeginUpdate;

        // continue if toolbar can't float or change docks
        if (TTBCustomDockableWindowHack(dockablewindow).DockMode = dmCannotFloatOrChangeDocks) then
        Continue;

        // continue if setting is not found (keeps toolbar in default position).
        if not AppStorage.PathExists(dockablewindow.Name) then
        Continue;

        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, dockablewindow.Name]);
        try
          loadPos:= false;
          if assigned(dockablewindow.CurrentDock) then
          s:= AppStorage.ReadString('Dock', dockablewindow.CurrentDock.Name)
          else
          s:= AppStorage.ReadString('Dock', 'TopToolDock');
          dock:= CEToolbarDocks.FindDockNamed(s);
          // Docked
          if assigned(dock) then 
          begin
            if DockType = tdtInner then
            begin
              if CEToolbarDocks.IsInnerDock(dock) then
              begin
                if dock <> dockablewindow.CurrentDock then
                dockablewindow.CurrentDock:= dock;
                loadPos:= true;
              end;
            end
            else if DockType = tdtOuter then
            begin
              if CEToolbarDocks.IsOuterDock(dock) then
              begin
                if dock <> dockablewindow.CurrentDock then
                dockablewindow.CurrentDock:= dock;
                loadPos:= true;
              end;           
            end
            else
            begin
              if dock <> dockablewindow.CurrentDock then
              dockablewindow.CurrentDock:= dock;
              loadPos:= true;
            end;

            if loadPos then
            begin
              i2:= AppStorage.ReadInteger('DockPos', dockablewindow.DockPos);
              if i2 <> dockablewindow.DockPos then
              dockablewindow.DockPos:= i2;

              i2:= AppStorage.ReadInteger('DockRow', dockablewindow.DockRow);
              if i2 <> dockablewindow.DockRow then
              dockablewindow.DockRow:= i2;

              b:= AppStorage.ReadBoolean('Visible', dockablewindow.Visible);
              if b <> dockablewindow.Visible then
              dockablewindow.Visible:= b;
            end;
          end
          // Floating
          else if (TTBCustomDockableWindowHack(dockablewindow).DockMode = dmCanFloat) then 
          begin
            if not dockablewindow.Floating then
            dockablewindow.Floating:= true;

            dockablewindow.FloatingPosition:= Point(AppStorage.ReadInteger('DockPos', dockablewindow.FloatingPosition.X),
                                                    AppStorage.ReadInteger('DockRow', dockablewindow.FloatingPosition.Y));

            b:= AppStorage.ReadBoolean('Visible', dockablewindow.Visible);
            if b <> dockablewindow.Visible then
            dockablewindow.Visible:= b;
          end;
        finally
          dockablewindow.EndUpdate;
          AppStorage.Path:= RootPath;
        end;
      end
      // TabSet
      else if (CELayoutItems.Items[i] is TCESpTabSet) and (DockType <> tdtInner) then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          tabset.Visible:= AppStorage.ReadBoolean('Visible', tabset.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end
      // StatusBar
      else if (CELayoutItems.Items[i] is TCEStatusBar) and (DockType <> tdtInner) then
      begin
        statusbar:= TCEStatusBar(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, statusbar.Name]);
        try
          statusbar.Visible:= AppStorage.ReadBoolean('Visible', statusbar.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end;
    end;
  finally
    CEToolbarDocks.EndUpdate;
    AppStorage.Path:= OldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Save toolbar positions from CELayoutItems to AppStorage.
-------------------------------------------------------------------------------}
procedure SavePositionsForToolbars(AppStorage: TJvCustomAppStorage;
    AppStoragePath: String = ''; DockType: TCEToolbarDockType = tdtBoth);
var
  OldPath: String;
  RootPath: String;
  i: Integer;
  dockablewindow: TTBCustomDockableWindow;
  tabset: TCESpTabSet;
  statusbar: TCEStatusBar;
begin
  if not assigned(AppStorage) then
  Exit;

  AppStorage.BeginUpdate;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, AppStoragePath, 'Toolbars']);
    RootPath:= AppStorage.Path;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      if CELayoutItems.Items[i] is TTBCustomDockableWindow then
      begin
        dockablewindow:= TTBCustomDockableWindow(CELayoutItems.Items[i]);

        // continue if toolbar can't float or change docks
        if (TTBCustomDockableWindowHack(dockablewindow).DockMode = dmCannotFloatOrChangeDocks) then
        Continue;

        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, dockablewindow.Name]);

        if dockablewindow.Floating then
        begin
          AppStorage.WriteInteger('DockPos', dockablewindow.FloatingPosition.X);
          AppStorage.WriteInteger('DockRow', dockablewindow.FloatingPosition.Y);
          AppStorage.WriteString('Dock', '');
          AppStorage.WriteBoolean('Visible', dockablewindow.Visible);
        end
        else if (DockType = tdtBoth) or
           ((DockType = tdtOuter) and CEToolbarDocks.IsOuterDock(dockablewindow.CurrentDock)) or
           ((DockType = tdtInner) and CEToolbarDocks.IsInnerDock(dockablewindow.CurrentDock)) then
        begin
          AppStorage.WriteInteger('DockPos', dockablewindow.DockPos);
          AppStorage.WriteInteger('DockRow', dockablewindow.DockRow);
          AppStorage.WriteString('Dock', dockablewindow.CurrentDock.Name);
          AppStorage.WriteBoolean('Visible', dockablewindow.Visible);
        end;
        AppStorage.Path:= RootPath;
      end
      else if (CELayoutItems.Items[i] is TCESpTabSet) and (DockType <> tdtInner) then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          AppStorage.WriteBoolean('Visible', tabset.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end
      else if (CELayoutItems.Items[i] is TCEStatusBar) and (DockType <> tdtInner) then
      begin
        statusbar:= TCEStatusBar(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, statusbar.Name]);
        try
          AppStorage.WriteBoolean('Visible', statusbar.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end;
    end;
  finally
    AppStorage.Path:= OldPath;
    AppStorage.EndUpdate;
  end;
end;



{*------------------------------------------------------------------------------
  Load Dockable Forms
-------------------------------------------------------------------------------}
procedure LoadDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
    String = ''; HandleMainForm: Boolean = false);
var
  DockInfoTree: TCEDockInfoTree;
  form: TCustomForm;
  i: Integer;
  r: TRect;
begin
  if not assigned(AppStorage) then
  Exit;
  
  BeginDockLoading;
  try
    CE_DoFloatAllForm;
    HideAllPopupPanel(nil);

    DockInfoTree:= TCEDockInfoTree.Create(TJvDockInfoZone);
    DockInfoTree.HandleMainForm:= HandleMainForm;

    try
      DockInfoTree.AppStorage:= AppStorage;
      DockInfoTree.AppStoragePath:= AppStoragePath;
      try
        DockInfoTree.ReadInfoFromAppStorage;
      finally
      end;
    finally
      DockInfoTree.Free;
    end;

    for I := 0 to Screen.CustomFormCount - 1 do
    begin
      form:= Screen.CustomForms[I];
      if form is TCECustomDockableForm then
      begin
        if form.Parent = MainForm.DockHostForm.CenterPanel then
        begin
          r:= form.BoundsRect;
          form.ManualFloat(r);
        end;
      end;

      if (form is TCECustomDockableForm) or (form is TJvDockTabHostForm) or (form is TCEDockHostForm) then
      begin
        if form.Visible then
        Windows.ShowWindow(form.Handle, SW_SHOW)
        else
        Windows.ShowWindow(form.Handle, SW_HIDE);
      end;
    end;
  finally
    EndDockLoading;
  end;
end;

{*------------------------------------------------------------------------------
  Save Dockable Forms
-------------------------------------------------------------------------------}
procedure SaveDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
var
  DockInfoTree: TCEDockInfoTree;
  i: Integer;
  form: TCustomForm;
begin
  if not assigned(AppStorage) then
  Exit;
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil);
    DockInfoTree := TCEDockInfoTree.Create(TJvDockInfoZone);
    try
      for I := 0 to Screen.CustomFormCount - 1 do
      begin
        form:= Screen.CustomForms[I];
        if ((form.Parent = nil) or (form is TCEDockHostForm)) and ((FindDockClient(form) <> nil) or (FindDockServer(form) <> nil)) then
        begin
          DockInfoTree.CreateZoneAndAddInfoFromApp(form);
        end;
      end;
      DockInfoTree.AppStorage:= AppStorage;
      DockInfoTree.AppStoragePath:= AppStoragePath;
      DockInfoTree.WriteInfoToAppStorage;
    finally
      DockInfoTree.Free;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Do Float Form
-------------------------------------------------------------------------------}
procedure CE_DoFloatForm(DockForm: TControl);
var
  I: TAlign;
  J: Integer;
  ADockServer: TJvDockServer;
  //  ARect: TRect;
  Channel: TJvDockVSChannel;
begin
  if DockForm is TJvDockableForm then
  begin
    with TJvDockableForm(DockForm).DockableControl do
    begin
      for J := DockClientCount - 1 downto 0 do
        CE_DoFloatForm(DockClients[J]);

      DockForm.ManualDock(MainForm.DockHostForm.CenterPanel);
    end;
  end
  else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      for I := alTop to alRight do
      begin
        if Assigned(ADockServer.DockPanelWithAlign[I]) then
        begin
          for J := ADockServer.DockPanelWithAlign[I].DockClientCount - 1 downto 0 do
          begin
            CE_DoFloatForm(ADockServer.DockPanelWithAlign[I].DockClients[J]);
          end;

          if ADockServer.DockPanelWithAlign[I] is TJvDockVSNETPanel then
          begin
            with TJvDockVSNETPanel(ADockServer.DockPanelWithAlign[I]).VSChannel do
            begin
              RemoveAllBlock;
              HidePopupPanel(ActiveDockForm);
            end;
          end;
        end;
      end;
    end
    else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        if (DockForm.HostDockSite.Parent is TJvDockableForm) and (DockForm.HostDockSite.DockClientCount <= 2) then
        PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end;

      Channel := RetrieveChannel(DockForm.HostDockSite);
      if Assigned(Channel) then
      begin
        Channel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(MainForm.DockHostForm.CenterPanel, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end
      else
      begin
        //if DockForm.Parent <> nil then
        DockForm.ManualDock(MainForm.DockHostForm.CenterPanel);
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Do Float AllForm
-------------------------------------------------------------------------------}
procedure CE_DoFloatAllForm;
var
  I: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for I := 0 to Screen.CustomFormCount - 1 do
      if not (Screen.CustomForms[I] is TJvDockableForm) and
        (Assigned(FinddockClient(Screen.CustomForms[I])) or
         Assigned(FinddockServer(Screen.CustomForms[I]))) then
        TempList.Add(Screen.CustomForms[I]);

    for I := 0 to TempList.Count - 1 do
      CE_DoFloatForm(TempList[I]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

{-------------------------------------------------------------------------------
  Begin Toolbar Customize
-------------------------------------------------------------------------------}
procedure BeginToolbarCustomize;
var
  i: Integer;
  toolbar: TSpTBXToolbar;
begin
  for i:= 0 to CELayoutItems.Count-1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXTabSet then
    toolbar:= TSpTBXTabSet(CELayoutItems.Items[i]).Toolbar
    else if CELayoutItems.Items[i] is TSpTBXToolbar then
    toolbar:= TSpTBXToolbar(CELayoutItems.Items[i])
    else
    toolbar:= nil;

    if assigned(toolbar) then
    begin
      toolbar.BeginCustomize;
      TSpTBXToolbarAccess(toolbar).RightAlignItems;
      toolbar.Repaint;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  End Toolbar Customize
-------------------------------------------------------------------------------}
procedure EndToolbarCustomize;
var
  i: Integer;
  toolbar: TSpTBXToolbar;
begin
  for i:= 0 to CELayoutItems.Count-1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXTabSet then
    toolbar:= TSpTBXTabSet(CELayoutItems.Items[i]).Toolbar
    else if CELayoutItems.Items[i] is TSpTBXToolbar then
    toolbar:= TSpTBXToolbar(CELayoutItems.Items[i])
    else
    toolbar:= nil;

    if assigned(toolbar) then
    begin
      toolbar.EndCustomize;
      TSpTBXToolbarAccess(toolbar).RightAlignItems;
      toolbar.Repaint;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load Settings For Toolbars
-------------------------------------------------------------------------------}
procedure LoadSettingsForToolbars(AppStorage: TJvAppXMLFileStorage;
    AppStoragePath: String = '');
var
  i: Integer;
  toolbar: TTBCustomDockableWindow;
  elem,rootElem: TJvSimpleXMLElem;
  path: String;
begin
  CEToolbarDocks.BeginUpdate;
  try
    path:= AppStorage.ConcatPaths([AppStoragePath, 'Toolbars']);
    rootElem:= TJvCustomAppXMLStorageHack(AppStorage).GetNodeFromPath(path);
    if not assigned(rootElem) then // old version uses "ToolbarItems" name
    rootElem:= AppStorage.Xml.Root.Items.ItemNamed['ToolbarItems'];


    if assigned(rootElem) then
    begin
      for i:= 0 to CELayoutItems.Count - 1 do
      begin
        // Normal Toolbars
        if CELayoutItems.Items[i] is TTBCustomDockableWindow then
        begin
          toolbar:= TTBCustomDockableWindow(CELayoutItems.Items[i]);
          elem:= rootElem.Items.ItemNamed[toolbar.Name];
        end
        // TabSet toolbar
        else if CELayoutItems.Items[i] is TCESpTabSet then
        begin
          toolbar:= TCESpTabSet(CELayoutItems.Items[i]).Toolbar;
          elem:= rootElem.Items.ItemNamed[TCESpTabSet(CELayoutItems.Items[i]).Name];
        end
        // Something else (unsupported)
        else
        continue; // continue to next item ->

        if assigned(elem) then
        begin
          LoadToolbarProperties(toolbar, elem);
          if toolbar is TSpTBXToolbar then
          begin
            if TSpTBXToolbar(toolbar).Customizable then
            LoadToolbarItems(TSpTBXToolbar(toolbar),elem);
          end;
        end;
      end;
    end;
  finally
    CEToolbarDocks.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Save toolbar items to XML node.
-------------------------------------------------------------------------------}
procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

  procedure SaveNormalToolbar(ToNode: TJvSimpleXMLElem);
  var
    i: Integer;
    item: TTBCustomItem;
    chNode: TJvSimpleXMLElem;
  begin
    // Loop through toolbar items
    for i:= 0 to Toolbar.Items.Count - 1 do
    begin
      item:= Toolbar.Items.Items[i];

      // Normal Item
      if item is TSpTBXItem then
      begin
        chNode:= ToNode.Items.Add('item');
        if assigned(item.Action) then
        chNode.Properties.Add('action', item.Action.Name)
        else
        chNode.Properties.Add('name', item.Name);
      end
      // Separator
      else if item is TCEToolbarSeparatorItem then
      begin
        ToNode.Items.Add('separator');
      end
      // Dynamic Spacer
      else if item is TCEToolbarDynamicSpacerItem then
      begin
        ToNode.Items.Add('dynamic_spacer');
      end
      // Fixed Spacer
      else if item is TCEToolbarFixedSpacerItem then
      begin
        ToNode.Items.Add('fixed_spacer');
      end
      // Stretcher
      else if item is TCEToolbarStretcherItem then
      begin
        ToNode.Items.Add('stretcher');
      end      
      // Editor Item
      else if item is TSpTBXEditItem then
      begin
        chNode:= ToNode.Items.Add('item');
        if assigned(item.Action) then
        chNode.Properties.Add('action', item.Action.Name)
        else
        chNode.Properties.Add('name', item.Name);
        if item is TCEToolbarEditItem then
        chNode.Properties.Add('size', TCEToolbarEditItem(item).DefaultWidth)
        else
        chNode.Properties.Add('size', TSpTBXEditItem(item).CustomWidth);
      end
      // Submenu
      else if item.ClassType = TSpTBXSubmenuItem then
      begin
        chNode:= ToNode.Items.Add('submenu');
        chNode.Properties.Add('name', item.Name);
      end;
    end;
  end;

  procedure SaveTabToolbar(ToNode: TJvSimpleXMLElem);
  var
    i: Integer;
    item: TTBCustomItem;
    chNode: TJvSimpleXMLElem;
  begin
    // Loop through toolbar items
    for i:= 0 to Toolbar.Items.Count - 1 do
    begin
      item:= Toolbar.Items.Items[i];
      // Tabs
      if item is TCEScrollArrowItem then
      begin
        if item = TCESpTabToolbar(Toolbar).LeftArrow then
        ToNode.Items.Add('tabs');
      end
      // Normal Item
      else if (item is TSpTBXItem) then
      begin
        chNode:= ToNode.Items.Add('item');
        if assigned(item.Action) then
        chNode.Properties.Add('action', item.Action.Name)
        else
        chNode.Properties.Add('name', item.Name);
      end
      // Separator
      else if item is TCEToolbarSeparatorItem then
      begin
        ToNode.Items.Add('separator');
      end
      // Dynamic Spacer
      else if item is TCEToolbarDynamicSpacerItem then
      begin
        ToNode.Items.Add('dynamic_spacer');
      end
      // Fixed Spacer
      else if item is TCEToolbarFixedSpacerItem then
      begin
        ToNode.Items.Add('fixed_spacer');
      end
      // Stretcher
      else if item is TCEToolbarStretcherItem then
      begin
        ToNode.Items.Add('stretcher');
      end
      // Editor Item
      else if item is TSpTBXEditItem then
      begin
        chNode:= ToNode.Items.Add('item');
        if assigned(item.Action) then
        chNode.Properties.Add('action', item.Action.Name)
        else
        chNode.Properties.Add('name', item.Name);
        if item is TCEToolbarEditItem then
        chNode.Properties.Add('size', TCEToolbarEditItem(item).DefaultWidth)
        else
        chNode.Properties.Add('size', TSpTBXEditItem(item).CustomWidth);
      end
      // Submenu
      else if item.ClassType = TSpTBXSubmenuItem then
      begin
        chNode:= ToNode.Items.Add('submenu');
        chNode.Properties.Add('name', item.Name);
      end
    end;
  end;

var
  rootNode: TJvSimpleXMLElem;
begin
  if not assigned(Toolbar) then
  Exit;
  if not assigned(ToolbarNode) then
  Exit;

  rootNode:= ToolbarNode.Items.ItemNamed['Items'];
  if not assigned(rootNode) then
  rootNode:= ToolbarNode.Items.Add('Items')
  else
  rootNode.Clear;

  if Toolbar is TCESpTabToolbar then
  SaveTabToolbar(rootNode)
  else
  SaveNormalToolbar(rootNode);
end;

{*------------------------------------------------------------------------------
  Load toolbar items from XML node.
-------------------------------------------------------------------------------}
procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

  procedure LoadNormalToolbar(FromNode: TJvSimpleXMLElem);
  var
    i: Integer;
    chNode: TJvSimpleXMLElem;
    act: TTntAction;
    item: TTBCustomItem;
    itemClass: TTBCustomItemClass;
  begin
    Toolbar.Items.Clear;
    for i:= 0 to FromNode.Items.Count - 1 do
    begin
      chNode:= FromNode.Items.Item[i];
      // Normal Item
      if SameText(chNode.Name, 'item') then
      begin
        act:= FindAction(CEActions.ActionList, chNode.Properties.Value('action'));
        if act is TCEToolbarAction then
        itemClass:= TCEToolbarAction(act).ItemClass
        else
        itemClass:= TSpTBXItem;

        if assigned(itemClass) then
        begin
          item:= itemClass.Create(Toolbar);
          item.Action:= act;
          // Editor item size
          if item is TCEToolbarEditItem then
          TCEToolbarEditItem(item).DefaultWidth:= chNode.Properties.IntValue('size', TCEToolbarEditItem(item).DefaultWidth)
          else if item is TSpTBXEditItem then
          TSpTBXEditItem(item).CustomWidth:= chNode.Properties.IntValue('size', TSpTBXEditItem(item).CustomWidth);
          Toolbar.Items.Add(item);
        end;
      end
      // Separator
      else if SameText(chNode.Name, 'separator') then
      begin
        item:= TCEToolbarSeparatorItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Dynamic Spacer
      else if SameText(chNode.Name, 'dynamic_spacer') then
      begin
        item:= TCEToolbarDynamicSpacerItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Fixed Spacer
      else if SameText(chNode.Name, 'fixed_spacer') then
      begin
        item:= TCEToolbarFixedSpacerItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Stretcher
      else if SameText(chNode.Name, 'stretcher') then
      begin
        item:= TCEToolbarStretcherItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end      
      // Submenu item
      else if SameText(chNode.Name, 'submenu') then
      begin
        item:= TSpTBXSubmenuItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Right Align Spacer
      // TODO: Depricated, remove this!
      else if SameText(chNode.Name, 'right_align') then
      begin
        item:= TCEToolbarDynamicSpacerItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end;
    end;
  end;

  procedure LoadTabToolbar(FromNode: TJvSimpleXMLElem);
  var
    i, index: Integer;
    chNode: TJvSimpleXMLElem;
    act: TTntAction;
    item: TTBCustomItem;
    itemClass: TTBCustomItemClass;
  begin
    // Clear old items
    i:= 0;
    while i < Toolbar.Items.Count do
    begin
      if not (Toolbar.Items.Items[i] is TCEScrollArrowItem) and not (Toolbar.Items.Items[i] is TSpTBXTabItem) then
      Toolbar.Items.Delete(i)
      else
      i:= i + 1;
    end;
    // Load Items
    index:= 0;
    for i:= 0 to FromNode.Items.Count - 1 do
    begin
      chNode:= FromNode.Items.Item[i];
      item:= nil;
      // Normal Item
      if SameText(chNode.Name, 'item') then
      begin
        act:= FindAction(CEActions.ActionList, chNode.Properties.Value('action'));
        if act is TCEToolbarAction then
        itemClass:= TCEToolbarAction(act).ItemClass
        else
        itemClass:= TSpTBXItem;

        if assigned(itemClass) then
        begin
          item:= itemClass.Create(Toolbar);
          item.Action:= act;
          if item is TCEToolbarEditItem then
          TCEToolbarEditItem(item).DefaultWidth:= chNode.Properties.IntValue('size', TSpTBXEditItem(item).CustomWidth)
          else if item is TSpTBXEditItem then
          TSpTBXEditItem(item).CustomWidth:= chNode.Properties.IntValue('size', TSpTBXEditItem(item).CustomWidth);
        end;
      end
      // Separator
      else if SameText(chNode.Name, 'separator') then
      begin
        item:= TCEToolbarSeparatorItem.Create(Toolbar);
      end
      // Submenu item
      else if SameText(chNode.Name, 'submenu') then
      begin
        item:= TSpTBXSubmenuItem.Create(Toolbar);
      end
      // Tabs
      else if SameText(chNode.Name, 'tabs') then
      begin
        index:= Toolbar.Items.IndexOf(TCESpTabToolbar(Toolbar).RightArrow)+1;
      end
      // Dynamic Spacer
      else if SameText(chNode.Name, 'dynamic_spacer') then
      begin
        item:= TCEToolbarDynamicSpacerItem.Create(Toolbar);
      end
      // Fixed Spacer
      else if SameText(chNode.Name, 'fixed_spacer') then
      begin
        item:= TCEToolbarFixedSpacerItem.Create(Toolbar);
      end
      // Stretcher
      else if SameText(chNode.Name, 'stretcher') then
      begin
        item:= TCEToolbarStretcherItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Right Align Spacer
      // TODO: Depricated, remove this!
      else if SameText(chNode.Name, 'right_align') then
      begin
        item:= TCEToolbarDynamicSpacerItem.Create(Toolbar);
      end;

      if assigned(item) then
      begin
        Toolbar.Items.Insert(index, item);
        index:= index + 1;
      end;
    end;
  end;

var
  rootNode: TJvSimpleXMLElem;
begin
  if not assigned(Toolbar) then
  Exit;
  if not assigned(ToolbarNode) then
  Exit;
  if not assigned(CEActions.ActionList) then
  Exit;

  if Toolbar.MenuBar or (toolbar.Tag = 1) then
  Exit;

  Toolbar.BeginUpdate;
  try
    rootNode:= ToolbarNode.Items.ItemNamed['Items'];
    if not assigned(rootNode) then // old version doesn't use "Items" node.
    rootNode:= ToolbarNode;

    if Toolbar is TCESpTabToolbar then
    LoadTabToolbar(rootNode)
    else
    LoadNormalToolbar(rootNode);
  finally
    Toolbar.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Load toolbar properties from XML node.
-------------------------------------------------------------------------------}
procedure LoadToolbarProperties(Toolbar: TTBCustomDockableWindow; ToolbarNode:
    TJvSimpleXMLElem);
var
  i: Integer;
begin
  if not assigned(Toolbar) or not assigned(ToolbarNode) then
  Exit;

  toolbar.BeginUpdate;
  try
    if Toolbar is TSpTBXToolbar then
    begin
      // Display Mode
      i:= ToolbarNode.Properties.IntValue('DisplayMode', -1);
      if (i > -1) and (i < 4) then
      TSpTBXToolbar(Toolbar).DisplayMode:= TSpTBXToolbarDisplayMode(i);
      if TSpTBXToolbar(Toolbar).DisplayMode = tbdmImageAboveCaption then
      TSpTBXToolbar(Toolbar).Options:= [tboImageAboveCaption];

      // Large Icons
      if ToolbarNode.Properties.BoolValue('LargeIcons', TSpTBXToolbar(Toolbar).Images = CE_Images.MediumIcons) then
      TSpTBXToolbar(Toolbar).Images:= CE_Images.MediumIcons
      else
      TSpTBXToolbar(Toolbar).Images:= CE_Images.SmallIcons;

      if Toolbar is TCEToolbar then
      TCEToolbar(Toolbar).LargeImages:= TSpTBXToolbar(Toolbar).Images = CE_Images.MediumIcons;
    end;

    // Borders
    if ToolbarNode.Properties.BoolValue('Borders', TTBCustomDockableWindowHack(Toolbar).BorderStyle <> bsNone) then
    TTBCustomDockableWindowHack(Toolbar).BorderStyle:= bsSingle
    else
    TTBCustomDockableWindowHack(Toolbar).BorderStyle:= bsNone;

    // Stretch
    TTBCustomDockableWindowHack(Toolbar).Stretch:= ToolbarNode.Properties.BoolValue('Stretch', TTBCustomDockableWindowHack(Toolbar).Stretch);

    // Drag Handle
    if ToolbarNode.Properties.BoolValue('DragHandle', TTBCustomDockableWindowHack(Toolbar).DragHandleStyle <> dhNone) then
    TTBCustomDockableWindowHack(Toolbar).DragHandleStyle:= dhSingle
    else
    TTBCustomDockableWindowHack(Toolbar).DragHandleStyle:= dhNone;
  finally
    toolbar.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Save toolbar properties to XML node.
-------------------------------------------------------------------------------}
procedure SaveToolbarProperties(Toolbar: TTBCustomDockableWindow; ToolbarNode:
    TJvSimpleXMLElem);
begin
  if not assigned(Toolbar) or not assigned(ToolbarNode) then
  Exit;

  ToolbarNode.Properties.Clear;
  if Toolbar is TSpTBXToolbar then
  begin
    ToolbarNode.Properties.Add('DisplayMode', Ord(TSpTBXToolbar(toolbar).DisplayMode));
    ToolbarNode.Properties.Add('LargeIcons', TSpTBXToolbar(toolbar).Images = CE_Images.MediumIcons);
  end;
  ToolbarNode.Properties.Add('Borders', TTBCustomDockableWindowHack(Toolbar).BorderStyle <> bsNone);
  ToolbarNode.Properties.Add('Stretch', TTBCustomDockableWindowHack(Toolbar).Stretch);
  ToolbarNode.Properties.Add('DragHandle', TTBCustomDockableWindowHack(Toolbar).DragHandleStyle <> dhNone);
end;

{*------------------------------------------------------------------------------
  Save Settings For Toolbars 
-------------------------------------------------------------------------------}
procedure SaveSettingsForToolbars(AppStorage: TJvAppXMLFileStorage;
    AppStoragePath: String = '');
var
  i: Integer;
  toolbar: TTBCustomDockableWindow;
  elem,rootElem: TJvSimpleXMLElem;
  path: String;
begin
  AppStorage.BeginUpdate;
  try
    path:= AppStorage.ConcatPaths([AppStoragePath, 'Toolbars']);

    rootElem:= TJvCustomAppXMLStorageHack(AppStorage).CreateAndSetNode(path);
    
    rootElem.Items.Clear;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      // Normal toolbar
      if CELayoutItems.Items[i] is TTBCustomDockableWindow then
      begin
        toolbar:= TTBCustomDockableWindow(CELayoutItems.Items[i]);
        elem:= rootElem.Items.Add(toolbar.Name);
      end
      // TabSet
      else if CELayoutItems.Items[i] is TCESpTabSet then
      begin
        toolbar:= TCESpTabSet(CELayoutItems.Items[i]).Toolbar;
        elem:= rootElem.Items.Add(TCESpTabSet(CELayoutItems.Items[i]).Name);
      end
      // Something else (unsupported)
      else 
      continue; // continue to next item->
      
      SaveToolbarProperties(toolbar, elem);
      if (toolbar is TSpTBXToolbar) then
      begin
        if TSpTBXToolbar(toolbar).Customizable then
        SaveToolbarItems(TSpTBXToolbar(toolbar),elem);
      end;
    end;
  finally
    // Remove deprecated node
    rootElem:= AppStorage.Xml.Root.Items.ItemNamed['ToolbarItems'];
    if assigned(rootElem) then
    begin
      AppStorage.Xml.Root.Items.Remove(rootElem);
      rootElem.Free;
    end;
    
    AppStorage.EndUpdate;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Add toolbar toggles to menuitem
-------------------------------------------------------------------------------}
procedure TCELayoutItems.PopulateMenuItem(RootItem: TTBCustomItem);
var
  i: Integer;
  comp: TControl;
  item: TSpTBXItem;
begin
  // Add Toolbar items
  for i:= 0 to Count - 1 do
  begin
    if (CELayoutItems.Items[i] is TTBCustomDockableWindow) or (CELayoutItems.Items[i] is TCESpTabSet) then
    begin
      comp:= TControl(Items[i]);
      item:= TSpTBXItem.Create(RootItem);
      RootItem.Add(item);
      item.Control:= comp;
      if comp is TSpTBXToolbar then
      item.Caption:= TSpTBXToolbar(comp).Caption
      else if comp is TSpTBXToolWindow then
      item.Caption:= TSpTBXToolWindow(comp).Caption
      else if comp is TCESpTabSet then
      item.Caption:= TCESpTabSet(comp).Toolbar.Caption;
    end;
  end;
  // Add Separator
  RootItem.Add(TSpTBXSeparatorItem.Create(RootItem));
  // Add Lock Toolbars item
  item:= TSpTBXItem.Create(RootItem);
  item.Action:= CEActions.act_view_lock_toolbars;
  RootItem.Add(item);
end;

{##############################################################################}

initialization
  CELayoutItems:= TCELayoutItems.Create(false);
  CEToolbarDocks:= TCEToolbarDocks.Create;
  CEDockStyle:= TJvDockVSNetStyleTBX.Create(nil);

finalization
  FreeAndNil(CEToolbarDocks);
  FreeAndNil(CELayoutItems);
  FreeAndNil(CEDockStyle);
  
end.
