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
//  The Original Code is CE_PaneHost.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_PaneHost;

interface

uses
  // CE Units
  fCE_TabPage,
  // SpTBX
  SpTBXItem, TB2Dock,
  // System Units
  Windows, Classes, Messages, SysUtils, Controls, ExtCtrls, Contnrs;

type
  TCEPageAddedEvent = procedure(Sender: TObject; Page: TCECustomTabPage) of object;
  TCEPageDeletingEvent = procedure(Sender: TObject; Page: TCECustomTabPage) of object;
  TCEPagePaneChangedEvent = procedure(Sender: TObject; Page: TCECustomTabPage) of object;
  TCEPageSelectedEvent = procedure(Sender: TObject; Page: TCECustomTabPage) of object;

  TCECustomPane = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCECustomPaneGroup = class(TCustomPanel)
  private
    fPanes: TObjectList;
  protected
    function GetDefaultPane: TCECustomPane; virtual;
    function GetGroupName: string; virtual;
  public
    TopGroupToolDock: TSpTBXDock;
    BottomGroupToolDock: TSpTBXDock;
    LeftGroupToolDock: TSpTBXDock;
    RightGroupToolDock: TSpTBXDock;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPane(PaneNumber: Integer = -1): TCECustomPane;
    property DefaultPane: TCECustomPane read GetDefaultPane;
    property GroupName: string read GetGroupName;
    property Panes: TObjectList read fPanes;
  end;

  TCECustomPaneGroupClass = class of TCECustomPaneGroup;

  TCEDefaultPaneGroup = class(TCECustomPaneGroup)
  private
    fDefaultPane: TCECustomPane;
  protected
    function GetDefaultPane: TCECustomPane; override;
    function GetGroupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
  TCEPaneGroupHost = class(TObject)
  private
    fActivePaneGroup: TCECustomPaneGroup;
    fAddingPage: Boolean;
    fOnPageAdded: TCEPageAddedEvent;
    fOnPageDeleting: TCEPageDeletingEvent;
    fOnPagePaneChanged: TCEPagePaneChangedEvent;
    fOnPageSelected: TCEPageSelectedEvent;
    fPageList: TComponentList;
    fParent: TWinControl;
    fSelectedPage: TCECustomTabPage;
    function GetActivePaneGroup: TCECustomPaneGroup;
    procedure SetSelectedPage(const Value: TCECustomTabPage);
  protected
    procedure DoPageAdded(Page: TCECustomTabPage);
    procedure DoPageDeleting(Page: TCECustomTabPage);
    procedure DoPagePaneChanged(Page: TCECustomTabPage);
    procedure DoPageSelected(Page: TCECustomTabPage);
  public
    constructor Create;
    destructor Destroy; override;
    function AddPage(TabPageClass: TCECustomTabPageClass; PaneNumber: Integer =
        -1): TCECustomTabPage;
    function ChangePane(Page: TCECustomTabPage; PaneNumber: Integer = -1): Integer;
    procedure ChangePaneGroup(NewPaneGroupClass: TCECustomPaneGroupClass);
    function ClosePage(Page: TCECustomTabPage; Force: Boolean = false): Boolean;
    function GetPage(Index: Integer): TCECustomTabPage;
    function GetPane(PaneNumber: Integer = -1): TCECustomPane;
    property ActivePaneGroup: TCECustomPaneGroup read GetActivePaneGroup;
    property PageList: TComponentList read fPageList;
    property Parent: TWinControl read fParent write fParent;
    property SelectedPage: TCECustomTabPage read fSelectedPage write
        SetSelectedPage;
  published
    property OnPageAdded: TCEPageAddedEvent read fOnPageAdded write fOnPageAdded;
    property OnPageDeleting: TCEPageDeletingEvent read fOnPageDeleting write
        fOnPageDeleting;
    property OnPagePaneChanged: TCEPagePaneChangedEvent read fOnPagePaneChanged
        write fOnPagePaneChanged;
    property OnPageSelected: TCEPageSelectedEvent read fOnPageSelected write
        fOnPageSelected;
  end;

implementation

{-------------------------------------------------------------------------------
  Create an instance of TCEPaneGroupHost
-------------------------------------------------------------------------------}
constructor TCEPaneGroupHost.Create;
begin
  inherited;
  fPageList:= TComponentList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCEPaneGroupHost
-------------------------------------------------------------------------------}
destructor TCEPaneGroupHost.Destroy;
begin
  fPageList.Free;
  if assigned(fActivePaneGroup) then
  FreeAndNil(fActivePaneGroup);
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Page
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.AddPage(TabPageClass: TCECustomTabPageClass;
    PaneNumber: Integer = -1): TCECustomTabPage;
begin
  fAddingPage:= true;
  Result:= TabPageClass.Create(nil);
  ChangePane(Result, PaneNumber);
  DoPageAdded(Result);
  fAddingPage:= false;
end;

{-------------------------------------------------------------------------------
  Change Pane
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.ChangePane(Page: TCECustomTabPage; PaneNumber:
    Integer = -1): Integer;
begin
  Page.Parent:= GetPane(PaneNumber);
  Page.PaneNumber:= ActivePaneGroup.Panes.IndexOf(Page.Parent);
  Result:= Page.PaneNumber;
  if not fAddingPage then
  DoPagePaneChanged(Page);
end;

{-------------------------------------------------------------------------------
  ChangePaneGroup
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.ChangePaneGroup(NewPaneGroupClass:
    TCECustomPaneGroupClass);
var
  group: TCECustomPaneGroup;
  page: TCECustomTabPage;
  i: Integer;
begin
  if not assigned(NewPaneGroupClass) then
  NewPaneGroupClass:= TCEDefaultPaneGroup;

  if assigned(fActivePaneGroup) then
  begin
    if fActivePaneGroup.ClassType = NewPaneGroupClass then
    Exit;
  end;

  group:= NewPaneGroupClass.Create(nil);
  group.Parent:= fParent;
  group.Align:= alClient;
  for i:= 0 to PageList.Count - 1 do
  begin
    page:= TCECustomTabPage(PageList.Items[i]);
    ChangePane(page, page.PaneNumber);
  end;

  if assigned(fActivePaneGroup) then
  FreeAndNil(fActivePaneGroup);

  fActivePaneGroup:= group;
end;

{-------------------------------------------------------------------------------
  Close Page (returns TRUE if successful)
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.ClosePage(Page: TCECustomTabPage; Force: Boolean =
    false): Boolean;
begin
  if Force then
  Result:= true
  else
  Result:= Page.TabClosing;
  if Result then
  begin
    DoPageDeleting(Page);
    Page.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Get Active Pane Group
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.GetActivePaneGroup: TCECustomPaneGroup;
begin
  if not assigned(fActivePaneGroup) then
  ChangePaneGroup(TCEDefaultPaneGroup);
  Result:= fActivePaneGroup;
end;

{-------------------------------------------------------------------------------
  Get Page
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.GetPage(Index: Integer): TCECustomTabPage;
begin
  Result:= TCECustomTabPage(PageList.Items[Index]);
end;

{-------------------------------------------------------------------------------
  Get Pane
-------------------------------------------------------------------------------}
function TCEPaneGroupHost.GetPane(PaneNumber: Integer = -1): TCECustomPane;
begin
  Result:= ActivePaneGroup.GetPane(PaneNumber);
end;

{-------------------------------------------------------------------------------
  Do Page Added
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.DoPageAdded(Page: TCECustomTabPage);
begin
  if Assigned(fOnPageAdded) then fOnPageAdded(Self, Page);
end;

{-------------------------------------------------------------------------------
  Do Page Deleting
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.DoPageDeleting(Page: TCECustomTabPage);
begin
  if Assigned(fOnPageDeleting) then fOnPageDeleting(Self, Page);
end;

{-------------------------------------------------------------------------------
  Do Page Pane Changed
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.DoPagePaneChanged(Page: TCECustomTabPage);
begin
  if Assigned(fOnPagePaneChanged) then fOnPagePaneChanged(Self, Page);
end;

{-------------------------------------------------------------------------------
  Do Page Selected
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.DoPageSelected(Page: TCECustomTabPage);
begin
  if Assigned(fOnPageSelected) then fOnPageSelected(Self, Page);
end;

{-------------------------------------------------------------------------------
  Set Selected Page
-------------------------------------------------------------------------------}
procedure TCEPaneGroupHost.SetSelectedPage(const Value: TCECustomTabPage);
begin
  if Value <> fSelectedPage then
  begin
    fSelectedPage:= Value;
    DoPageSelected(fSelectedPage);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCECustomPaneGroup
-------------------------------------------------------------------------------}
constructor TCECustomPaneGroup.Create(AOwner: TComponent);
begin
  inherited;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  fPanes:= TObjectList.Create(false);

  // Toolbar docks
  TopGroupToolDock:= TSpTBXDock.Create(nil);
  TopGroupToolDock.Name:= 'TopPageToolDock';
  TopGroupToolDock.Parent:= Self;
  TopGroupToolDock.Position:= dpTop;
  BottomGroupToolDock:= TSpTBXDock.Create(nil);
  BottomGroupToolDock.Name:= 'BottomPageToolDock';
  BottomGroupToolDock.Parent:= Self;
  BottomGroupToolDock.Position:= dpBottom;
  LeftGroupToolDock:= TSpTBXDock.Create(nil);
  LeftGroupToolDock.Name:= 'LeftPageToolDock';
  LeftGroupToolDock.Parent:= Self;
  LeftGroupToolDock.Position:= dpLeft;
  RightGroupToolDock:= TSpTBXDock.Create(nil);
  RightGroupToolDock.Name:= 'RightPageToolDock';
  RightGroupToolDock.Parent:= Self;
  RightGroupToolDock.Position:= dpRight;
end;

{-------------------------------------------------------------------------------
  Destroy TCECustomPaneGroup
-------------------------------------------------------------------------------}
destructor TCECustomPaneGroup.Destroy;
begin
  fPanes.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Get DefaultPane
-------------------------------------------------------------------------------}
function TCECustomPaneGroup.GetDefaultPane: TCECustomPane;
begin
  // Override from descendant
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get Group Name
-------------------------------------------------------------------------------}
function TCECustomPaneGroup.GetGroupName: string;
begin
  // Override from descendant
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Get Pane (-1 get's the default pane)
-------------------------------------------------------------------------------}
function TCECustomPaneGroup.GetPane(PaneNumber: Integer = -1): TCECustomPane;
begin
  if (PaneNumber < 0) or (PaneNumber >= Panes.Count) then
  Result:= DefaultPane
  else
  Result:= TCECustomPane(Panes.Items[PaneNumber]);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEDefaultPaneGroup
-------------------------------------------------------------------------------}
constructor TCEDefaultPaneGroup.Create(AOwner: TComponent);
begin
  inherited;
  fDefaultPane:= TCECustomPane.Create(nil);
  fDefaultPane.Parent:= Self;
  fDefaultPane.Align:= alClient;
  Panes.Add(fDefaultPane);
end;

{-------------------------------------------------------------------------------
  Destroy TCEDefaultPaneGroup
-------------------------------------------------------------------------------}
destructor TCEDefaultPaneGroup.Destroy;
begin
  fDefaultPane.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Get DefaultPane
-------------------------------------------------------------------------------}
function TCEDefaultPaneGroup.GetDefaultPane: TCECustomPane;
begin
  Result:= fDefaultPane;
end;

{-------------------------------------------------------------------------------
  Get Group Name
-------------------------------------------------------------------------------}
function TCEDefaultPaneGroup.GetGroupName: string;
begin
  Result:= 'Default';
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCECustomPane
-------------------------------------------------------------------------------}
constructor TCECustomPane.Create(AOwner: TComponent);
begin
  inherited;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
end;

end.
