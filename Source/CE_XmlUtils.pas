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
//  The Original Code is CE_XmlUtils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_XmlUtils;

interface

uses
  // fcl-xml
  DOM,
  // System Units
  SysUtils, StrUtils;

function FindFirstChildDOMNode(AFrom: TDOMNode; AChildName: WideString):
    TDOMNode;

function FindNextSiblingDOMNode(AFrom: TDOMNode; ASiblingName: WideString):
    TDOMNode;

function ISODateTime2DateTime(const ISODT: String): TDateTime; // (from OmniXML)
function Str2Time(s: string): TDateTime; // (from OmniXML)
function XMLStrToDateTime(nodeValue: WideString; var value: TDateTime): boolean; overload; // (from OmniXML)
function XMLStrToDateTime(nodeValue: WideString): TDateTime; overload; // (from OmniXML)

const // (from OmniXML)
  DEFAULT_DECIMALSEPARATOR  = '.'; // don't change!
  DEFAULT_TRUE              = '1'; // don't change!
  DEFAULT_FALSE             = '0'; // don't change!
  DEFAULT_DATETIMESEPARATOR = 'T'; // don't change!
  DEFAULT_DATESEPARATOR     = '-'; // don't change!
  DEFAULT_TIMESEPARATOR     = ':'; // don't change!
  DEFAULT_MSSEPARATOR       = '.'; // don't change!    

implementation

{-------------------------------------------------------------------------------
  Find First Child DOMNode
-------------------------------------------------------------------------------}
function FindFirstChildDOMNode(AFrom: TDOMNode; AChildName: WideString):
    TDOMNode;
begin
  if assigned(AFrom) then
  begin
    Result:= AFrom.FirstChild;
    while assigned(Result) do
    begin
      if Result.NodeName = AChildName then
      Exit // -->
      else
      Result:= Result.NextSibling;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Find Next Sibling DOMNode
-------------------------------------------------------------------------------}
function FindNextSiblingDOMNode(AFrom: TDOMNode; ASiblingName: WideString):
    TDOMNode;
begin
  if assigned(AFrom) then
  begin
    Result:= AFrom.NextSibling;
    while assigned(Result) do
    begin
      if Result.NodeName = ASiblingName then
      Exit // -->
      else
      Result:= Result.NextSibling;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Str2Time (from OmniXML)
-------------------------------------------------------------------------------}
function Str2Time(s: string): TDateTime;
var
  hour  : word;
  minute: word;
  msec  : word;
  p     : integer;
  second: word;
begin
  s := Trim(s);
  if s = '' then
    Result := 0
  else begin
    p := Pos(DEFAULT_TIMESEPARATOR,s);
    hour := StrToInt(Copy(s,1,p-1));
    Delete(s,1,p);
    p := Pos(DEFAULT_TIMESEPARATOR,s);
    minute := StrToInt(Copy(s,1,p-1));
    Delete(s,1,p);
    p := Pos(DEFAULT_MSSEPARATOR,s);
    if p > 0 then begin
      msec := StrToInt(Copy(s,p+1,Length(s)-p));
      Delete(s,p,Length(s)-p+1);
    end
    else
      msec := 0;
    second := StrToInt(s);
    Result := EncodeTime(hour,minute,second,msec);
  end;
end;

{-------------------------------------------------------------------------------
  ISODateTime2DateTime (from OmniXML)
-------------------------------------------------------------------------------}
function ISODateTime2DateTime (const ISODT: String): TDateTime;
var
  day   : word;
  month : word;
  p     : integer;
  sDate : string;
  sTime : string;
  year  : word;
begin
  p := Pos (DEFAULT_DATETIMESEPARATOR,ISODT);
  // detect all known date/time formats
  if (p = 0) and (Pos(DEFAULT_DATESEPARATOR, ISODT) > 0) then
    p := Length(ISODT) + 1;
  sDate := Trim(Copy(ISODT,1,p-1));
  sTime := Trim(Copy(ISODT,p+1,Length(ISODT)-p));
  Result := 0;
  if sDate <> '' then begin
    p := Pos (DEFAULT_DATESEPARATOR,sDate);
    year :=  StrToInt(Copy(sDate,1,p-1));
    Delete(sDate,1,p);
    p := Pos (DEFAULT_DATESEPARATOR,sDate);
    month :=  StrToInt(Copy(sDate,1,p-1));
    day := StrToInt(Copy(sDate,p+1,Length(sDate)-p));
    Result := EncodeDate(year,month,day);
  end;
  Result := Result + Frac(Str2Time(sTime));
end;

{-------------------------------------------------------------------------------
  XMLStrToDateTime (from OmniXML)
-------------------------------------------------------------------------------}
function XMLStrToDateTime(nodeValue: WideString; var value: TDateTime): boolean;
begin
  try
    value := ISODateTime2DateTime(nodeValue);
    Result := true;
  except
    Result := false;
  end;
end;

{-------------------------------------------------------------------------------
  XMLStrToDateTime (from OmniXML)
-------------------------------------------------------------------------------}
function XMLStrToDateTime(nodeValue: WideString): TDateTime;
begin
  if not XMLStrToDateTime(nodeValue, Result) then
    raise Exception.CreateFmt('%s is not an ISO datetime value', [nodeValue]);
end;

end.
