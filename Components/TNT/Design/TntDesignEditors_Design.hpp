// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntdesigneditors_design.pas' rev: 10.00

#ifndef Tntdesigneditors_designHPP
#define Tntdesigneditors_designHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Typinfo.hpp>	// Pascal unit
#include <Designintf.hpp>	// Pascal unit
#include <Designeditors.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntdesigneditors_design
{
//-- type declarations -------------------------------------------------------
typedef IDesigner ITntDesigner;
;

class DELPHICLASS TTntDesignerSelections;
class PASCALIMPLEMENTATION TTntDesignerSelections : public System::TInterfacedObject 
{
	typedef System::TInterfacedObject inherited;
	
public:
	Classes::TPersistent* operator[](int Index) { return Items[Index]; }
	
private:
	Classes::TList* FList;
	Designintf::_di_IDesignObject __fastcall GetDesignObject(int Index);
	
protected:
	int __fastcall Add(const Classes::TPersistent* Item);
	bool __fastcall Equals(const Designintf::_di_IDesignerSelections List);
	Classes::TPersistent* __fastcall Get(int Index);
	int __fastcall GetCount(void);
	__property int Count = {read=GetCount, nodefault};
	__property Classes::TPersistent* Items[int Index] = {read=Get/*, default*/};
	
public:
	__fastcall virtual TTntDesignerSelections(void);
	__fastcall virtual ~TTntDesignerSelections(void);
	void __fastcall ReplaceSelection(const Classes::TPersistent* OldInst, const Classes::TPersistent* NewInst);
private:
	void *__IDesignerSelections;	/* Designintf::IDesignerSelections */
	
public:
	operator IDesignerSelections*(void) { return (IDesignerSelections*)&__IDesignerSelections; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Forms::TCustomForm* __fastcall GetObjectInspectorForm(void);
extern PACKAGE void __fastcall EditPropertyWithDialog(Classes::TPersistent* Component, const AnsiString PropName, const Designintf::_di_IDesigner Designer);

}	/* namespace Tntdesigneditors_design */
using namespace Tntdesigneditors_design;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntdesigneditors_design
