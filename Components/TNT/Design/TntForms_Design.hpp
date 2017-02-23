// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntforms_design.pas' rev: 10.00

#ifndef Tntforms_designHPP
#define Tntforms_designHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Designintf.hpp>	// Pascal unit
#include <Toolsapi.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntforms_design
{
//-- type declarations -------------------------------------------------------
typedef unsigned HICON;

class DELPHICLASS TTntNewFormWizard;
class PASCALIMPLEMENTATION TTntNewFormWizard : public Toolsapi::TNotifierObject 
{
	typedef Toolsapi::TNotifierObject inherited;
	
protected:
	WideString __fastcall ThisFormName();
	virtual TMetaClass* __fastcall ThisFormClass(void) = 0 ;
	WideString __fastcall ThisFormUnit();
	
public:
	AnsiString __fastcall GetIDString();
	virtual AnsiString __fastcall GetName();
	Toolsapi::TWizardState __fastcall GetState(void);
	void __fastcall Execute(void);
	AnsiString __fastcall GetAuthor();
	virtual AnsiString __fastcall GetComment(void) = 0 ;
	AnsiString __fastcall GetPage();
	unsigned __fastcall GetGlyph(void);
	AnsiString __fastcall GetDesigner();
	Toolsapi::_di_IOTAGalleryCategory __fastcall GetGalleryCategory();
	AnsiString __fastcall GetPersonality();
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTntNewFormWizard(void) : Toolsapi::TNotifierObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntNewFormWizard(void) { }
	#pragma option pop
	
private:
	void *__IOTAFormWizard;	/* Toolsapi::IOTAFormWizard */
	void *__IOTARepositoryWizard80;	/* Toolsapi::IOTARepositoryWizard80 */
	
public:
	operator IOTARepositoryWizard80*(void) { return (IOTARepositoryWizard80*)&__IOTARepositoryWizard80; }
	operator IOTARepositoryWizard60*(void) { return (IOTARepositoryWizard60*)&__IOTARepositoryWizard80; }
	operator IOTAFormWizard*(void) { return (IOTAFormWizard*)&__IOTAFormWizard; }
	operator IOTARepositoryWizard*(void) { return (IOTARepositoryWizard*)&__IOTARepositoryWizard80; }
	operator IOTAWizard*(void) { return (IOTAWizard*)&__IOTARepositoryWizard80; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tntforms_design */
using namespace Tntforms_design;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntforms_design
