// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntbandactn.pas' rev: 10.00

#ifndef TntbandactnHPP
#define TntbandactnHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Bandactn.hpp>	// Pascal unit
#include <Tntactnlist.hpp>	// Pascal unit
#include <Actnlist.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntbandactn
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntCustomizeActionBars;
class PASCALIMPLEMENTATION TTntCustomizeActionBars : public Bandactn::TCustomizeActionBars 
{
	typedef Bandactn::TCustomizeActionBars inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TCustomizeActionBars.Create */ inline __fastcall virtual TTntCustomizeActionBars(Classes::TComponent* AOwner) : Bandactn::TCustomizeActionBars(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntCustomizeActionBars(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntbandactn */
using namespace Tntbandactn;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntbandactn
