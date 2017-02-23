// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntwidestrutils.pas' rev: 10.00

#ifndef TntwidestrutilsHPP
#define TntwidestrutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntwidestrutils
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall Tnt_WStrComp(WideChar * Str1, WideChar * Str2);
extern PACKAGE WideChar * __fastcall Tnt_WStrPos(WideChar * Str, WideChar * SubStr);
extern PACKAGE WideChar * __fastcall WStrECopy(WideChar * Dest, WideChar * Source);
extern PACKAGE int __fastcall WStrLComp(WideChar * Str1, WideChar * Str2, unsigned MaxLen);
extern PACKAGE int __fastcall WStrLIComp(WideChar * Str1, WideChar * Str2, unsigned MaxLen);
extern PACKAGE int __fastcall WStrIComp(WideChar * Str1, WideChar * Str2);
extern PACKAGE WideChar * __fastcall WStrLower(WideChar * Str);
extern PACKAGE WideChar * __fastcall WStrUpper(WideChar * Str);
extern PACKAGE WideChar * __fastcall WStrRScan(const WideChar * Str, WideChar Chr);
extern PACKAGE WideChar * __fastcall WStrLCat(WideChar * Dest, const WideChar * Source, unsigned MaxLen);
extern PACKAGE WideString __fastcall WStrPas(const WideChar * Str);

}	/* namespace Tntwidestrutils */
using namespace Tntwidestrutils;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntwidestrutils
