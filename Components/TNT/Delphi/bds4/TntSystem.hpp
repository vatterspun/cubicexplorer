// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntsystem.pas' rev: 10.00

#ifndef TntsystemHPP
#define TntsystemHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntsystem
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTntSystemUpdate { tsWideResourceStrings };
#pragma option pop

typedef Set<TTntSystemUpdate, tsWideResourceStrings, tsWideResourceStrings>  TTntSystemUpdateSet;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool __fastcall (*WideCustomLoadResString)(System::PResStringRec ResStringRec, WideString &Value);
static const WideChar UNICODE_BOM = WideChar(0xfeff);
static const WideChar UNICODE_BOM_SWAPPED = WideChar(0xfffe);
#define UTF8_BOM "﻿"
#define AllTntSystemUpdates (Set<TTntSystemUpdate, tsWideResourceStrings, tsWideResourceStrings> () << TTntSystemUpdate(0) )
extern PACKAGE unsigned __fastcall DefaultSystemCodePage(void);
extern PACKAGE WideString __fastcall WideLoadResString(System::PResStringRec ResStringRec);
extern PACKAGE int __fastcall WideParamCount(void);
extern PACKAGE WideString __fastcall WideParamStr(int Index);
extern PACKAGE AnsiString __fastcall WideStringToUTF8(const WideString S);
extern PACKAGE WideString __fastcall UTF8ToWideString(const AnsiString S);
extern PACKAGE AnsiString __fastcall WideStringToUTF7(const WideString W);
extern PACKAGE WideString __fastcall UTF7ToWideString(const AnsiString S);
extern PACKAGE WideString __fastcall StringToWideStringEx(const AnsiString S, unsigned CodePage);
extern PACKAGE AnsiString __fastcall WideStringToStringEx(const WideString WS, unsigned CodePage);
extern PACKAGE WideString __fastcall UCS2ToWideString(const AnsiString Value);
extern PACKAGE AnsiString __fastcall WideStringToUCS2(const WideString Value);
extern PACKAGE unsigned __fastcall CharSetToCodePage(unsigned ciCharset);
extern PACKAGE unsigned __fastcall LCIDToCodePage(unsigned ALcid);
extern PACKAGE unsigned __fastcall KeyboardCodePage(void);
extern PACKAGE WideChar __fastcall KeyUnicode(Word CharCode);
extern PACKAGE void __fastcall StrSwapByteOrder(WideChar * Str);
extern PACKAGE void __fastcall InstallTntSystemUpdates(TTntSystemUpdateSet Updates = (Set<TTntSystemUpdate, tsWideResourceStrings, tsWideResourceStrings> () << TTntSystemUpdate(0) ));

}	/* namespace Tntsystem */
using namespace Tntsystem;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntsystem
