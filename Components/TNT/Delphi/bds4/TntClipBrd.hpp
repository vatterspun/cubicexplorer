// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntclipbrd.pas' rev: 10.00

#ifndef TntclipbrdHPP
#define TntclipbrdHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Clipbrd.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntclipbrd
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntClipboard;
class PASCALIMPLEMENTATION TTntClipboard : public Clipbrd::TClipboard 
{
	typedef Clipbrd::TClipboard inherited;
	
private:
	WideString __fastcall GetAsWideText();
	void __fastcall SetAsWideText(const WideString Value);
	
public:
	__property WideString AsWideText = {read=GetAsWideText, write=SetAsWideText};
	__property WideString AsText = {read=GetAsWideText, write=SetAsWideText};
public:
	#pragma option push -w-inl
	/* TClipboard.Destroy */ inline __fastcall virtual ~TTntClipboard(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTntClipboard(void) : Clipbrd::TClipboard() { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TTntClipboard* __fastcall TntClipboard(void);

}	/* namespace Tntclipbrd */
using namespace Tntclipbrd;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntclipbrd
