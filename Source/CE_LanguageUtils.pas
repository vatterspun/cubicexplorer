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
//  The Original Code is CE_LanguageUtils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_LanguageUtils;

interface

uses
  // CE units
  CE_LanguageCodes,
  // Tnt Control
  TntSysUtils, TntClasses,
  // System Units
  Windows, SysUtils, TypInfo, Classes;

type
  EPOFileError = class(Exception);

  TCEPOItem = class(TPersistent)
  private
    fComments: TStrings;
    FMsgID: string;
    FMsgStr: string;
    function GetComments: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Comments: TStrings read GetComments;
    property MsgID: string read FMsgID write FMsgID;
    property MsgStr: string read FMsgStr write FMsgStr;
  end;

  TCEPOFile = class
  private
    FItems: TList;
    FHeader: TCEPOItem;
    FSorted: boolean;
    fTestMode: Boolean;
    fTestValue: string;
    function GetCount: integer;
    function GetEntry(Index: integer): TCEPOItem;
  protected
    function DoBinarySearch(AList: TList; const AMsgID: string; var AIndex:
        integer): boolean; virtual;
  public
    constructor Create;
    procedure LoadFromFile(const Filename: WideString; LoadComments: Boolean =
        true);
    procedure SaveToFile(const Filename: WideString; SaveComments: Boolean = true);
    destructor Destroy; override;
    function IndexOf(const AMsgID: string): integer;
    function Add: TCEPOItem;
    procedure Delete(Index: integer);
    procedure Clear;
    function GetText(const MsgID: String): string; overload;
    function GetText(const MsgID: WideString): WideString; overload;
    procedure LoadFromStream(Stream: TStream; LoadComments: Boolean = true);
    // removes duplicates. NOTE: always keeps the *last* duplicate found!
    procedure Pack;
    procedure Sort;
    // adds the items in POFile that are not already available
    procedure Merge(POFile: TCEPOFile);
    procedure SaveToStream(Stream: TStream; SaveComments: Boolean = true);
    property Header: TCEPOItem read FHeader;
    property Items[Index: integer]: TCEPOItem read GetEntry; default;
    property Count: integer read GetCount;
    property Sorted: boolean read FSorted;
    property TestMode: Boolean read fTestMode write fTestMode;
    property TestValue: string read fTestValue write fTestValue;
  end;

  
function BinaryStrCompare(const S1,S2:string): integer;
function trimAllBelow(const S: string; Ch: Char = #33): string;
// replaces #13#10 with \n , #9 with \t etc
function EscapeString(const S: string): string;
// replaces \n with #13#10, \t with #9 etc
function UnescapeString(const S: string): string;
function StripQuotes(const S: string; Ch: Char): string;

procedure GetPOLanguageList(LocaleDir: WideString; DomainName: WideString;
    Results: TTntStrings);

implementation

const
  cMOSignature = $950412DE;

{*------------------------------------------------------------------------------
  Do PO Item Sort
-------------------------------------------------------------------------------}
function DoPOItemSort(Item1, Item2: Pointer): integer;
begin
  Result:= BinaryStrCompare(TCEPOItem(Item1).MsgId, TCEPOItem(Item2).MsgId);
end;

{*------------------------------------------------------------------------------
  Binary StringCompare
-------------------------------------------------------------------------------}
function BinaryStrCompare(const S1,S2:string): integer;
begin
  if S1 = S2 then
    Result:= 0
  else if S1 < S2 then
    Result:= -1
  else
    Result:= +1;
end;

{*------------------------------------------------------------------------------
  trim All Below
-------------------------------------------------------------------------------}
function trimAllBelow(const S: string; Ch: Char = #33): string;
var
  right, left: Integer;
begin
  right:= Length(S);
  left:= 1;
  while (left <= right) and (S[left] < Ch) do
  left:= left + 1;
  if left > right then
  Result:= ''
  else
  begin
    while S[right] < Ch do
    right:= right - 1;
    Result:= Copy(S, left, right - left + 1);
  end;
end;

{*------------------------------------------------------------------------------
  Escape String (replaces #13#10 with \n , #9 with \t etc)
-------------------------------------------------------------------------------}
function EscapeString(const S: string): string;
var
  i, j: integer;
begin
  SetLength(Result, Length(S) * 2); // worst case - all needs escaping
  j:= 0;
  i:= 1;
  while i <= Length(S) do
  begin
    case S[i] of
      #9:
        begin
          j:= j + 1;
          Result[j]:= '\';
          j:= j + 1;
          Result[j]:= 't';
        end;
      #10:
        begin
          j:= j + 1;
          Result[j]:= '\';
          j:= j + 1;
          Result[j]:= 'n';
        end;
      #13: ; // do nothing
      '"':
        begin
          j:= j + 1;
          Result[j]:= '\';
          j:= j + 1;
          Result[j]:= '"';
        end;
    else
      begin
        j:= j + 1;
        Result[j]:= S[i];
      end;
    end; // case
    i:= i + 1;
  end;
  SetLength(Result, j);
end;

{*------------------------------------------------------------------------------
  Unescape String (replaces \n with #13#10, \t with #9 etc)
-------------------------------------------------------------------------------}
function UnescapeString(const S: string): string;
var
  i, j: integer;
begin
  SetLength(Result, Length(S));
  j:= 0;
  i:= 1;
  while i <= Length(S) do
  begin
    case S[i] of
      '\':
        if ((i < Length(S)) and (S[i + 1] <> '\')) then
          case S[i + 1] of
            'n':
              begin
                j:= j + 1;
                Result[j]:= #13;
                j:= j + 1;
                Result[j]:= #10;
                i:= i + 1;
              end;
            'r': ; // do nothing
            't':
              begin
                j:= j + 1;
                Result[j]:= #9;
                i:= i + 1;
              end;
            '"':
              begin
                j:= j + 1;
                Result[j]:= '"';
                i:= i + 1;
              end;
          else
            begin
              j:= j + 1;
              Result[j]:= S[i];
            end;
          end; // case
      #1..#31: ; // do nothing
    else
      begin
        j:= j + 1;
        Result[j]:= S[i];
      end;
    end; // case
    i:= i + 1;
  end;
  SetLength(Result, j);
end;

{*------------------------------------------------------------------------------
  Strip Quotes
-------------------------------------------------------------------------------}
function StripQuotes(const S: string; Ch: Char): string;
begin
  if (Length(S) > 0) and (S[1] = Ch) and (S[Length(S)] = Ch) then
  Result:= Copy(S, 2, Length(S) - 2)
  else
  Result:= S;
end;

{*------------------------------------------------------------------------------
  Get list of PO files (Returns NameValue pair:  LanguageName=FilePath
-------------------------------------------------------------------------------}
procedure GetPOLanguageList(LocaleDir: WideString; DomainName: WideString;
    Results: TTntStrings);
var
  sr: TSearchRecW;
  more:boolean;
  filename, langname: WideString;
begin
  more:= WideFindFirst(LocaleDir + '*', faAnyFile, sr) = 0;
  try
    while more do
    begin
      if (sr.Attr and faDirectory <> 0) and (sr.name <> '.') and (sr.name <> '..') then
      begin
        filename:= LocaleDir + sr.Name + PathDelim + DomainName + '.po';
        if WideFileExists(filename) then
        begin
          langname:= GetLanguageName(sr.Name);
          if langname <> '' then
          Results.Add(langname + '=' + filename)
          else
          Results.Add(sr.name + '=' + filename);
        end;
      end;
      more:= WideFindNext(sr) = 0;
    end;
  finally
    WideFindClose(sr);
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEPOItem
-------------------------------------------------------------------------------}
constructor TCEPOItem.Create;
begin
  inherited Create;
  //FComments:= TStringlist.Create;
end;

{*------------------------------------------------------------------------------
  Destroy TCEPOItem
-------------------------------------------------------------------------------}
destructor TCEPOItem.Destroy;
begin
  if assigned(fComments) then
  fComments.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign TCEPOItem
-------------------------------------------------------------------------------}
procedure TCEPOItem.Assign(Source: TPersistent);
begin
  if (Source is TCEPOItem) then
  begin
    Comments.Assign(TCEPOItem(Source).Comments);
    MsgID:= TCEPOItem(Source).FMsgID;
    MsgStr:= TCEPOItem(Source).FMsgStr;
    Exit;
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get comments
-------------------------------------------------------------------------------}
function TCEPOItem.GetComments: TStrings;
begin
  if not assigned(fComments) then
  fComments:= TStringlist.Create;
  Result:= fComments;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEPOFile
-------------------------------------------------------------------------------}
constructor TCEPOFile.Create;
begin
  inherited Create;
  FItems:= TList.Create;
  FItems.Capacity:= 128;
  fTestMode:= false;
  fTestValue:= 'Test';
end;

{*------------------------------------------------------------------------------
  Destroy TCEPOFile
-------------------------------------------------------------------------------}
destructor TCEPOFile.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Add new item
-------------------------------------------------------------------------------}
function TCEPOFile.Add: TCEPOItem;
begin
  Result:= TCEPOItem.Create;
  FItems.Add(Result);
  FSorted:= false;
end;

{*------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEPOFile.Clear;
var
  i: integer;
begin
  for i:= 0 to FItems.Count - 1 do
  TObject(FItems[i]).Free;
  FItems.Count:= 0;
  FItems.Capacity:= 128;
  FreeAndNil(FHeader);
end;

{*------------------------------------------------------------------------------
  Delete
-------------------------------------------------------------------------------}
procedure TCEPOFile.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

{*------------------------------------------------------------------------------
  Get count
-------------------------------------------------------------------------------}
function TCEPOFile.GetCount: integer;
begin
  Result:= FItems.Count;
end;

{*------------------------------------------------------------------------------
  Get Entry
-------------------------------------------------------------------------------}
function TCEPOFile.GetEntry(Index: integer): TCEPOItem;
begin
  Result:= TCEPOItem(FItems[Index]);
end;

{*------------------------------------------------------------------------------
  Index of
-------------------------------------------------------------------------------}
function TCEPOFile.IndexOf(const AMsgID: string): integer;
begin
  if Sorted then
  begin
    if DoBinarySearch(FItems, AMsgID, Result) then
    Exit;
  end
  else
  begin
    for Result:= 0 to Count - 1 do
    begin
      if AnsiSameStr(Items[Result].MsgId, AMsgID) then
      Exit;
    end;
  end;
  Result:= -1;
end;

{*------------------------------------------------------------------------------
  Load From file
-------------------------------------------------------------------------------}
procedure TCEPOFile.LoadFromFile(const Filename: WideString; LoadComments:
    Boolean = true);
var
  S: TStream;
begin
  if not WideFileExists(Filename) then
  Exit;
  
  S:= TTntFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S, LoadComments);
  finally
    S.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Merge
-------------------------------------------------------------------------------}
procedure TCEPOFile.Merge(POFile: TCEPOFile);
var
  i: integer;
begin
  if POFile = nil then
  raise EPOFileError.Create('POFile cannot be nil');

  for i:= 0 to POFile.Count - 1 do
  begin
    if IndexOf(POFile[i].MsgId) < 0 then
      if POFile[i].MsgId <> '' then
      Add.Assign(POFile[i]);
  end;
end;

{*------------------------------------------------------------------------------
  Pack
-------------------------------------------------------------------------------}
procedure TCEPOFile.Pack;
var
  i, j: integer;
  tmp: boolean;
begin
  tmp:= FSorted;
  try
    // this will not work if sorted, so fake unsorted
    FSorted:= false;
    j:= Count - 1;
    while j >= 0 do
    begin
      i:= IndexOf(Items[j].MsgID);
      if (i >= 0) and (i <> j) then
      begin
        // retain comments
        Items[j].Comments.AddStrings(Items[i].Comments);
        // keep translation if we don't have one already
        if Items[j].MsgStr = '' then
        Items[j].MsgStr:= Items[i].MsgStr;
        // remove duplicate
        Delete(i);
      end;
      j:= j - 1;
    end;
  finally
    FSorted:= tmp;
  end;
end;

{*------------------------------------------------------------------------------
  Do Binary Search
-------------------------------------------------------------------------------}
function TCEPOFile.DoBinarySearch(AList: TList; const AMsgID: string; var
    AIndex: integer): boolean;
var
  L, R, M: integer;
  CompareResult: integer;
begin
  //TODO: Speedup with hash table.
  L:= 0;
  R:= AList.Count - 1;
  while L <= R do
  begin
    M:= (L + R) div 2;
    CompareResult:= BinaryStrCompare(TCEPOItem(AList[M]).MsgID, AMsgID);
    if (CompareResult < 0) then
    L:= M + 1
    else if (CompareResult > 0) then
    R:= M - 1
    else
    begin
      AIndex:= M;
      Result:= true;
      Exit;
    end;
  end;
  // not found, should be located here:
  Result:= false;
  AIndex:= L;
end;

{*------------------------------------------------------------------------------
  Get Text (UTF8 input/output)
-------------------------------------------------------------------------------}
function TCEPOFile.GetText(const MsgID: String): string;
var
  i: Integer;
begin
  if fTestMode then
  begin
    Result:= fTestValue;
  end
  else
  begin
    i:= IndexOf(MsgID);
    if i > -1 then
    begin
      Result:= Items[i].MsgStr;
      if Result = '' then
      Result:= MsgID;
    end
    else
    Result:= MsgID;
  end;
end;

{*------------------------------------------------------------------------------
  Get Text (WideString input/output)
-------------------------------------------------------------------------------}
function TCEPOFile.GetText(const MsgID: WideString): WideString;
begin
  Result:= UTF8Decode(GetText(UTF8Encode(MsgID)));
end;

{*------------------------------------------------------------------------------
  Load From Stream
-------------------------------------------------------------------------------}
procedure TCEPOFile.LoadFromStream(Stream: TStream; LoadComments: Boolean =
    true);
type
  TEntryState = (esNone, esComment, esId, esStr);
var
  i: integer;
  S: TStringlist;
  AEntry: TCEPOItem;
  AState: TEntryState;
begin
  Clear;
  if not assigned(Stream) then
  Exit;
  
  S:= TStringlist.Create;
  try
    AEntry:= nil;
    AState:= esNone;
    S.LoadFromStream(Stream);
    for i:= 0 to S.Count - 1 do
    begin
      if AEntry = nil then
      AEntry:= Add;
      if AnsiSameText(Copy(S[i], 1, 1), '#') then
      begin
        if LoadComments then
        begin
          AEntry.Comments.Add(Copy(S[i], 2, MaxInt));
          AState:= esComment;
        end;
      end
      else if AnsiSameText(Copy(S[i], 1, 6), 'msgid ') then
      begin
        AEntry.MsgId:= StripQuotes(Copy(S[i], 7, MaxInt), '"');
        AState:= esID;
      end
      else if AnsiSameText(Copy(S[i], 1, 7), 'msgstr ') then
      begin
        AEntry.MsgStr:= StripQuotes(Copy(S[i], 8, MaxInt), '"');
        AState:= esStr;
      end
      else if S[i] <> '' then
        case AState of
          esID: AEntry.MsgId:= AEntry.MsgId + StripQuotes(S[i], '"');
          esStr: AEntry.MsgStr:= AEntry.MsgStr + StripQuotes(S[i], '"');
          else
          begin
            AEntry:= nil;
            aState:= esNone;
          end
        end
      else
      begin
        if (AEntry <> nil) and (AEntry.MsgID = '') then
        begin
          if FHeader = nil then
          FHeader:= AEntry;
          // remove the entry even if header is assigned: there can be only one (empty)!
          FItems.Remove(AEntry);
        end;
        AEntry:= nil;
        aState:= esNone;
      end;
    end;
  finally
    S.Free;
  end;
  FSorted:= false;
end;

{*------------------------------------------------------------------------------
  Save to file
-------------------------------------------------------------------------------}
procedure TCEPOFile.SaveToFile(const Filename: WideString; SaveComments:
    Boolean = true);
var
  S: TStream;
begin
  S:= TTntFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S, SaveComments);
  finally
    S.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Save to Stream
-------------------------------------------------------------------------------}
procedure TCEPOFile.SaveToStream(Stream: TStream; SaveComments: Boolean = true);
var
  i, j: integer;
  S, T: TStringlist;
begin
  if not assigned(Stream) then
  Exit;
  
  S:= TStringlist.Create;
  T:= TStringlist.Create;
  try
    // Header
    if FHeader <> nil then
    begin
      if SaveComments then
      begin
        for j:= 0 to FHeader.Comments.Count - 1 do
        begin
          S.Add(Format('#%s', [FHeader.Comments[j]]));
        end;
      end;

      S.Add('msgid ""');
      T.Text:= StringReplace(FHeader.MsgStr, '\n', '\n'#13#10, [rfReplaceAll]);

      if T.Count = 0 then
      begin
        S.Add('msgstr ""')
      end
      else if T.Count > 1 then
      begin
        S.Add('msgstr ""');
        for j:= 0 to T.Count - 1 do
        begin
          S.Add(Format('"%s"', [T[j]]));
        end;
      end
      else
      begin
        S.Add(Format('msgstr "%s"', [T[0]]));
      end;

      S.Add('');
    end;
    // Items
    for i:= 0 to Count - 1 do
    begin
      if Items[i].MsgId = '' then Continue;
      // comments
      if SaveComments then
      begin
        for j:= 0 to Items[i].Comments.Count - 1 do
        begin
          S.Add(Format('#%s', [Items[i].Comments[j]]));
        end;
      end;
      // MsgId
      T.Text:= StringReplace(Items[i].MsgId, '\n', '\n'#13#10, [rfReplaceAll]);
      if T.Count = 0 then
      begin
        S.Add('msgid ""')
      end
      else if T.Count > 1 then
      begin
        S.Add('msgid ""');
        for j:= 0 to T.Count - 1 do
        begin
          S.Add(Format('"%s"', [T[j]]));
        end;
      end
      else
      begin
        S.Add(Format('msgid "%s"', [T[0]]));
      end;
      // MsgStr
      T.Text:= StringReplace(Items[i].MsgStr, '\n', '\n'#13#10, [rfReplaceAll]);
      if T.Count = 0 then
      begin
        S.Add('msgstr ""')
      end
      else if T.Count > 1 then
      begin
        S.Add('msgstr ""');
        for j := 0 to T.Count - 1 do
        begin
          S.Add(Format('"%s"', [T[j]]));
        end;
      end
      else
      begin
        S.Add(Format('msgstr "%s"', [T[0]]));
      end;
      // empty line between entries
      S.Add('');
    end;
    S.SaveToStream(Stream);
  finally
    S.Free;
    T.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Sort
-------------------------------------------------------------------------------}
procedure TCEPOFile.Sort;
begin
  if not FSorted and (FItems.Count > 1) then
  FItems.Sort(DoPOItemSort);
  FSorted:= true;
end;







end.
