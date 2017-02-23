// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntmenus_design.pas' rev: 10.00

#ifndef Tntmenus_designHPP
#define Tntmenus_designHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Designeditors.hpp>	// Pascal unit
#include <Designintf.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntmenus_design
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntMenuEditor;
class PASCALIMPLEMENTATION TTntMenuEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTntMenuEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntMenuEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tntmenus_design */
using namespace Tntmenus_design;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntmenus_design
