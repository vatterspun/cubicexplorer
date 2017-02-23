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
//  The Original Code is fCE_OptionsDialog.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsDialog;

interface

uses
  // CE Units
  fCE_OptionsCustomPage, CE_LanguageEngine,
  // VirtualTree
  VirtualTrees,
  // Tnt Controls
  TntStdCtrls, TntExtCtrls, TntForms,
  // SpTBXLib
  SpTBXSkins,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Contnrs, ImgList, PngImageList;

type
  PPageData = ^APageData;
  APageData = record
    PageName: WideString;
    PagePath: WideString;
    Page: TCEOptionsCustomPage;
    ImageIndex: Integer;
  end;

  TCEOptionsDialog = class(TTntForm)
    MainPanel: TPanel;
    BottomPanel: TPanel;
    PagePanel: TPanel;
    but_ok: TTntButton;
    but_cancel: TTntButton;
    but_apply: TTntButton;
    PageContainer: TPanel;
    PageTitlePanel: TTntPanel;
    SmallImageList: TPngImageList;
    PageTree: TVirtualDrawTree;
    LargeImageList: TPngImageList;
    procedure but_applyClick(Sender: TObject);
    procedure but_cancelClick(Sender: TObject);
    procedure but_okClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure PageTreeAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas:
        TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure PageTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PageTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure PageTreeGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
        Node: PVirtualNode; Column: TColumnIndex; var NodeWidth: Integer);
    procedure PageTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
        Node: PVirtualNode; var NodeHeight: Integer);
  private
    fActivePage: TCEOptionsCustomPage;
    fModified: Boolean;
    PageList: TObjectList;
    function GetNodeLevel(ANode: PVirtualNode): Cardinal;
    procedure SetActivePage(const Value: TCEOptionsCustomPage);
    procedure SetModified(const Value: Boolean);
    { Private declarations }
  protected
    function CreatePageNode(PagePath: String): PVirtualNode;
    procedure CreatePages;
    function FindPageNode(PagePath: String): PVirtualNode;
    procedure SelectActivePageFromTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyAll;
    procedure RefreshAll;
    property ActivePage: TCEOptionsCustomPage read fActivePage write SetActivePage;
    property Modified: Boolean read fModified write SetModified;
  end;

function GetOptionsPageClassList: TClassList;
procedure RegisterOptionsPageClass(PageClass: TCEOptionsCustomPageClass);
procedure UnRegisterOptionsPageClass(PageClass: TCEOptionsCustomPageClass);

procedure ShowOptionsDialog;

var
  CEOptionsDialog: TCEOptionsDialog = nil;

implementation

uses
  fCE_OptionsPage_Advanced, MPCommonUtilities;

{$R *.dfm}

var
  fOptionsPageClassList: TClassList;

{*------------------------------------------------------------------------------
  Get Options PageClass List
-------------------------------------------------------------------------------}
function GetOptionsPageClassList: TClassList;
begin
  if not assigned(fOptionsPageClassList) then
  fOptionsPageClassList:= TClassList.Create;
  Result:= fOptionsPageClassList;
end;

{*------------------------------------------------------------------------------
  Register Options PageClass
-------------------------------------------------------------------------------}
procedure RegisterOptionsPageClass(PageClass: TCEOptionsCustomPageClass);
begin
  GetOptionsPageClassList.Add(PageClass);
end;

{*------------------------------------------------------------------------------
  UnRegister Options PageClass
-------------------------------------------------------------------------------}
procedure UnRegisterOptionsPageClass(PageClass: TCEOptionsCustomPageClass);
begin
  GetOptionsPageClassList.Remove(PageClass);
end;

{-------------------------------------------------------------------------------
  Show OptionsDialog
-------------------------------------------------------------------------------}
procedure ShowOptionsDialog;
begin
  if not assigned(CEOptionsDialog) then
  CEOptionsDialog:= TCEOptionsDialog.Create(nil);

  CEOptionsDialog.Show;
end;

{##############################################################################}


{*------------------------------------------------------------------------------
  Create an instance of TCEOptionsDialog
-------------------------------------------------------------------------------}
constructor TCEOptionsDialog.Create(AOwner: TComponent);
var
  node: PVirtualNode;
begin
  inherited Create(AOwner);
  PageTree.NodeDataSize:= SizeOf(APageData);
  PageList := TObjectList.Create(true);
  CreatePages;
  RefreshAll;
  Modified:= false;
  node:= FindPageNode('General');
  if assigned(node) then
  begin
    PageTree.FocusedNode:= node;
    PageTree.Selected[node]:= true;
  end;
  
  CEGlobalTranslator.TranslateComponent(Self);
end;

{*------------------------------------------------------------------------------
  Destroy TCEOptionsDialog
-------------------------------------------------------------------------------}
destructor TCEOptionsDialog.Destroy;
begin
  FreeAndNil(PageList);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  On Apply Click
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.but_applyClick(Sender: TObject);
begin
  if not (ActivePage is TCEOptionsPage_Advanced) then
  ApplyAll
  else
  ActivePage.ApplySettings;
  Modified:= false;
end;

{-------------------------------------------------------------------------------
  On Cancel Click
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.but_cancelClick(Sender: TObject);
begin
  Self.Close;
end;

{-------------------------------------------------------------------------------
  On OK click
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.but_okClick(Sender: TObject);
begin
  if not (ActivePage is TCEOptionsPage_Advanced) then
  ApplyAll;
  Modified:= false;
  Self.Close;
end;

{*------------------------------------------------------------------------------
  Create Pages
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.CreatePages;
var
  i: Integer;
  page: TCEOptionsCustomPage;
  data: PPageData;
  node: PVirtualNode;
begin
  for i:= 0 to GetOptionsPageClassList.Count - 1 do
  begin
    page:= TCEOptionsCustomPageClass(GetOptionsPageClassList.Items[i]).Create(nil);
    CEGlobalTranslator.TranslateComponent(page);
    page.OptionsDialog:= Self;
    page.Parent:= PageContainer;
    page.Align:= alClient;
    node:= CreatePageNode(page.PagePath);
    if node <> nil then
    begin
      data:= PageTree.GetNodeData(node);
      data.PageName:= page.PageName;
      data.PagePath:= page.PagePath;
      data.ImageIndex:= page.ImageIndex;
      data.Page:= page;
      PageList.Add(page);
    end
    else
    page.Free;
  end;
  PageTree.Sort(nil,0,sdAscending, false);
end;

{*------------------------------------------------------------------------------
  Create Page Node
-------------------------------------------------------------------------------}
function TCEOptionsDialog.CreatePageNode(PagePath: String): PVirtualNode;

  function FindChildNode(ParentNode: PVirtualNode; Path: String): PVirtualNode;
  var
    data: PPageData;
    node: PVirtualNode;
  begin
    Result:= nil;
    if assigned(ParentNode) then
    node:= ParentNode.FirstChild
    else
    node:= PageTree.GetFirst;
    while node <> nil do
    begin
      data:= PageTree.GetNodeData(node);
      if CompareText(data.PagePath, Path) = 0 then
      begin
        Result:= node;
        break;
      end;
      node:= node.NextSibling;
    end;
  end;

var
  i: Integer;
  s: String;
  list: TStrings;
  parentNode, prevParent: PVirtualNode;
  data: PPageData;
begin
  list:= TStringList.Create;
  try
    list.Delimiter:= '/';
    list.DelimitedText:= PagePath;
    parentNode:= nil;
    if list.Count > 1 then
    begin
      for i:= 0 to list.Count - 2 do
      begin
        if i > 0 then
        s:= s + '/' + list.Strings[i]
        else
        s:= s + list.Strings[i];
        prevParent:= parentNode;
        parentNode:= FindChildNode(prevParent, s);
        if parentNode = nil then
        begin
          parentNode:= PageTree.AddChild(prevParent);
          data:= PageTree.GetNodeData(parentNode);
          data.PagePath:= s;
          data.PageName:= list.Strings[i];
          data.ImageIndex:= -1;
        end;
      end;
    end;
    Result:= PageTree.AddChild(parentNode);
  finally
    list.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Find Page Node by Path
-------------------------------------------------------------------------------}
function TCEOptionsDialog.FindPageNode(PagePath: String): PVirtualNode;
var
  data: PPageData;
  node: PVirtualNode;
begin
  node:= PageTree.GetFirst;
  while node <> nil do
  begin
    data:= PageTree.GetNodeData(node);
    if CompareText(PagePath,data.PagePath) = 0 then
    break
    else
    node:= PageTree.GetNext(node);
  end;
  Result:= node;
end;

{-------------------------------------------------------------------------------
  On Form Close (free form on close)
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
  CEOptionsDialog:= nil;
end;

{-------------------------------------------------------------------------------
  Get Node Level
-------------------------------------------------------------------------------}
function TCEOptionsDialog.GetNodeLevel(ANode: PVirtualNode): Cardinal;
var
  n: PVirtualNode;
begin
  Result:= 0;
  if ANode = nil then
  Exit;

  n:= ANode;
  while n.Parent <> PageTree.RootNode do
  begin
    Result:= Result + 1;
    n:= n.Parent;
  end;
end;

{-------------------------------------------------------------------------------
  Set ActivePage
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.SetActivePage(const Value: TCEOptionsCustomPage);
var
  i, hr: Integer;
  ws,ws2: WideString;
begin
  if Value <> fActivePage then
  begin
    if fActivePage is TCEOptionsPage_Advanced then
    begin
      Self.RefreshAll;
      Modified:= false;
    end
    else if Value is TCEOptionsPage_Advanced then
    begin
      if Modified then
      begin
        ws2:= _('Apply changes now?')+#13#10+
            #13+#10+
            _('If not, all changes will be lost.');
        ws:= _('Apply Changes');
        hr:= WideMessageBox(Self.Handle, ws, ws2 , MB_ICONQUESTION or MB_YESNOCANCEL);
        if hr = IDCANCEL then
        begin
          SelectActivePageFromTree;
          Exit;
        end
        else if hr = IDYES then
        begin
          ApplyAll;
          Modified:= false;
        end;
      end;
    end;
    
    fActivePage:= Value;
    for i:= 0 to PageList.Count - 1 do
    begin
      TCEOptionsCustomPage(PageList.Items[i]).Visible:= false;
      TCEOptionsCustomPage(PageList.Items[i]).HandleHide;
    end;
    if assigned(fActivePage) then
    begin
      fActivePage.BoundsRect:= PageContainer.ClientRect;
      fActivePage.Visible:= true;
      fActivePage.HandleShow;
      PageTitlePanel.Caption:= fActivePage.PageTitle;
    end
    else
    begin
      PageTitlePanel.Caption:= 'No page';
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On PageTree Change
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  data: PPageData;
begin
  data:= Sender.GetNodeData(Node);
  if assigned(data) then
  ActivePage:= data.Page;

  //if  then
  but_ok.Default:= not (ActivePage is TCEOptionsPage_Advanced);
end;

{-------------------------------------------------------------------------------
  On After PageTree Cell Paint
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeAfterCellPaint(Sender: TBaseVirtualTree;
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect:
    TRect);
var
  c1,c2: TColor;
  baseColor: TColor;
  r: TRect;
  NodeLevel,NextNodeLevel: Integer;
  prevNode: PVirtualNode;
begin
  if Sender.Selected[Node] then
  baseColor:= clHighlight
  else
  baseColor:= clWindow;
  
  r:= CellRect;
  NodeLevel:= GetNodeLevel(Node);
  if NodeLevel = 0 then
  begin
    TargetCanvas.Brush.Color:= clBtnFace;
    TargetCanvas.FrameRect(r);
    r.Bottom:= r.Bottom - 1;
    prevNode:= PageTree.GetPreviousSibling(Node);
    if assigned(prevNode) then
    begin
      if PageTree.Expanded[prevNode] then
      r.Top:= r.Top + 1;
    end;
    c1:= SpLighten(baseColor,20);
    c2:= SpLighten(baseColor,-20);
    SpGradientFill(TargetCanvas, r, c1, c2, true);
  end
  else
  begin
    r.Left:= r.Left + Integer(PageTree.Indent);
    NextNodeLevel:= GetNodeLevel(PageTree.GetNextVisible(Node));
    if NextNodeLevel = 0 then
    r.Bottom:= r.Bottom - 4;
    TargetCanvas.Brush.Color:= clBtnFace;
    TargetCanvas.FrameRect(r);
    r.Left:= r.Left + 1;
    if NextNodeLevel = 0 then
    r.Bottom:= r.Bottom - 1;


    c1:= SpLighten(baseColor,10);
    c2:= SpLighten(baseColor,-10);
    SpGradientFill(TargetCanvas, r, c1, c2, false);
  end;
end;

{-------------------------------------------------------------------------------
  On After PageTree Item Paint
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeAfterItemPaint(Sender: TBaseVirtualTree;
    TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  data: PPageData;
  r: TRect;
  textColor: TColor;
  NodeLevel,NextNodeLevel: Integer;
  ws: WideString;
begin
  data:= Sender.GetNodeData(Node);
  if not assigned(data) then
  exit;

  if PageTree.Selected[Node] then
  textColor:= clHighlightText
  else
  textColor:= clWindowText;

  r:= ItemRect;
  NodeLevel:= GetNodeLevel(Node);
  NextNodeLevel:= GetNodeLevel(PageTree.GetNextVisible(Node));
  if NodeLevel = 0 then
  begin
    r.Left:= r.Left + 2;
    r.Top:= r.Top + 2;
    LargeImageList.Draw(TargetCanvas, r.Left,r.Top, data.ImageIndex);
    TargetCanvas.Brush.Style:= bsClear;
    TargetCanvas.Font.Style:= [fsBold];
    TargetCanvas.Font.Size:= 10;
    TargetCanvas.Font.Color:= textColor;
    if data.ImageIndex > -1 then
    r.Left:= r.Left + 34;
    ws:= data.PageName;
    SpDrawXPText(TargetCanvas, ws, r, DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end
  else
  begin
    r.Left:= r.Left + (NodeLevel * Integer(PageTree.Indent)) + 2;
    r.Top:= r.Top + 2;
    r.Bottom:= r.Bottom - 2;
    if NextNodeLevel = 0 then
    r.Bottom:= r.Bottom - 4;
    
    SmallImageList.Draw(TargetCanvas, r.Left,r.Top, data.ImageIndex);
    TargetCanvas.Brush.Style:= bsClear;
    TargetCanvas.Font.Style:= [];
    TargetCanvas.Font.Size:= 8;
    TargetCanvas.Font.Color:= textColor;
    if data.ImageIndex > -1 then
    r.Left:= r.Left + 18
    else
    r.Left:= r.Left + 2;
    
    ws:= data.PageName;
    SpDrawXPText(TargetCanvas, ws, r, DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end;  
end;

{-------------------------------------------------------------------------------
  On Get PageTree Node Width
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeGetNodeWidth(Sender: TBaseVirtualTree;
    HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; var
    NodeWidth: Integer);
begin
  NodeWidth:= Sender.ClientWidth-4;
  if GetNodeLevel(Node) > 0 then
  NodeWidth:= NodeWidth - Integer(GetNodeLevel(Node) * PageTree.Indent);
end;

{-------------------------------------------------------------------------------
  On PageTree Measure Item
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeMeasureItem(Sender: TBaseVirtualTree;
    TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  if GetNodeLevel(Node) = 0 then
  NodeHeight:= 36
  else
  begin
    if GetNodeLevel(PageTree.GetNextVisible(Node)) < 1 then
    NodeHeight:= 24
    else
    nodeHeight:= 20;
  end;
end;

{-------------------------------------------------------------------------------
  Apply all settings
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.ApplyAll;
var
  i: Integer;
  page: TCEOptionsCustomPage;
begin
  for i:= 0 to PageList.Count - 1 do
  begin
    page:= TCEOptionsCustomPage(Pagelist.Items[i]);
    page.ApplySettings;
  end;
end;

{-------------------------------------------------------------------------------
  On Form Key Down
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if Key = VK_ESCAPE then
  but_cancelClick(self);
end;

{-------------------------------------------------------------------------------
  On Compare Node
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.PageTreeCompareNodes(Sender: TBaseVirtualTree;
    Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PPageData;
begin
  data1:= Sender.GetNodeData(Node1);
  data2:= Sender.GetNodeData(Node2);
  Result:= data1.Page.PageListPosition - data2.Page.PageListPosition;
end;

{-------------------------------------------------------------------------------
  Load All settings
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.RefreshAll;
var
  i: Integer;
  page: TCEOptionsCustomPage;
begin
  for i:= 0 to PageList.Count - 1 do
  begin
    page:= TCEOptionsCustomPage(Pagelist.Items[i]);
    page.RefreshSettings;
  end;
end;

{-------------------------------------------------------------------------------
  Select ActivePage From Tree
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.SelectActivePageFromTree;
var
  data: PPageData;
  node: PVirtualNode;
begin
  node:= PageTree.GetFirst;
  while assigned(node) do
  begin
    data:= PageTree.GetNodeData(node);
    if data.Page = fActivePage then
    begin
      PageTree.FocusedNode:= node;
      PageTree.Selected[node]:= true;
      break;
    end;
    node:= PageTree.GetNext(node);
  end;
end;

{-------------------------------------------------------------------------------
  Set Modified
-------------------------------------------------------------------------------}
procedure TCEOptionsDialog.SetModified(const Value: Boolean);
begin
  fModified:= Value;
  but_Apply.Enabled:= fModified;
end;

{##############################################################################}

initialization

finalization
  if assigned(fOptionsPageClassList) then
  FreeAndNil(fOptionsPageClassList)

end.
