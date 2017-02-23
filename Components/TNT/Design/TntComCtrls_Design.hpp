// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntcomctrls_design.pas' rev: 10.00

#ifndef Tntcomctrls_designHPP
#define Tntcomctrls_designHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Designintf.hpp>	// Pascal unit
#include <Designmenus.hpp>	// Pascal unit
#include <Designeditors.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Comctrls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntcomctrls_design
{
//-- type declarations -------------------------------------------------------
typedef IMenuItem IPrepareMenuItem;
;

class DELPHICLASS TTntListViewEditor;
class PASCALIMPLEMENTATION TTntListViewEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTntListViewEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntListViewEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPageControlEditor;
class PASCALIMPLEMENTATION TTntPageControlEditor : public Designeditors::TDefaultEditor 
{
	typedef Designeditors::TDefaultEditor inherited;
	
private:
	Comctrls::TPageControl* __fastcall PageControl(void);
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
	virtual void __fastcall PrepareItem(int Index, const Designmenus::_di_IMenuItem AItem);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTntPageControlEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TDefaultEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntPageControlEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStatusBarEditor;
class PASCALIMPLEMENTATION TTntStatusBarEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTntStatusBarEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntStatusBarEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntToolBarEditor;
class PASCALIMPLEMENTATION TTntToolBarEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTntToolBarEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntToolBarEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tntcomctrls_design */
using namespace Tntcomctrls_design;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntcomctrls_design
