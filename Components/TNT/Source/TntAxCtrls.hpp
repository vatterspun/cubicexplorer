// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntaxctrls.pas' rev: 10.00

#ifndef TntaxctrlsHPP
#define TntaxctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Comobj.hpp>	// Pascal unit
#include <Stdvcl.hpp>	// Pascal unit
#include <Widestrings.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntaxctrls
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TWideStringsAdapter;
class PASCALIMPLEMENTATION TWideStringsAdapter : public Comobj::TAutoIntfObject 
{
	typedef Comobj::TAutoIntfObject inherited;
	
private:
	Widestrings::TWideStrings* FStrings;
	
protected:
	void __fastcall ReferenceStrings(Widestrings::TWideStrings* S);
	void __fastcall ReleaseStrings(void);
	HRESULT __safecall Get_ControlDefault(int Index, OleVariant &Get_ControlDefault_result);
	HRESULT __safecall Set_ControlDefault(int Index, const OleVariant Value);
	HRESULT __safecall Count(int &Count_result);
	HRESULT __safecall Get_Item(int Index, OleVariant &Get_Item_result);
	HRESULT __safecall Set_Item(int Index, const OleVariant Value);
	HRESULT __safecall Remove(int Index);
	HRESULT __safecall Clear(void);
	HRESULT __safecall Add(const OleVariant Item, int &Add_result);
	HRESULT __safecall _NewEnum(System::_di_IInterface &_NewEnum_result);
	
public:
	__fastcall TWideStringsAdapter(Tntclasses::TTntStrings* Strings);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TWideStringsAdapter(void) { }
	#pragma option pop
	
private:
	void *__IStrings;	/* Stdvcl::IStrings */
	void *__IWideStringsAdapter;	/* Widestrings::IWideStringsAdapter */
	
public:
	operator IWideStringsAdapter*(void) { return (IWideStringsAdapter*)&__IWideStringsAdapter; }
	operator IStrings*(void) { return (IStrings*)&__IStrings; }
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntaxctrls */
using namespace Tntaxctrls;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntaxctrls
