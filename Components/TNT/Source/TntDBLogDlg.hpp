// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntdblogdlg.pas' rev: 10.00

#ifndef TntdblogdlgHPP
#define TntdblogdlgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Tntforms.hpp>	// Pascal unit
#include <Tntstdctrls.hpp>	// Pascal unit
#include <Tntextctrls.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Extctrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntdblogdlg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntLoginDialog;
class PASCALIMPLEMENTATION TTntLoginDialog : public Tntforms::TTntForm 
{
	typedef Tntforms::TTntForm inherited;
	
__published:
	Tntextctrls::TTntPanel* Panel;
	Tntextctrls::TTntBevel* Bevel;
	Tntstdctrls::TTntLabel* DatabaseName;
	Tntstdctrls::TTntButton* OKButton;
	Tntstdctrls::TTntButton* CancelButton;
	Tntextctrls::TTntPanel* Panel1;
	Tntstdctrls::TTntLabel* Label1;
	Tntstdctrls::TTntLabel* Label2;
	Tntstdctrls::TTntLabel* Label3;
	Tntstdctrls::TTntEdit* Password;
	Tntstdctrls::TTntEdit* UserName;
	void __fastcall FormShow(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TTntForm.Create */ inline __fastcall virtual TTntLoginDialog(Classes::TComponent* AOwner) : Tntforms::TTntForm(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTntLoginDialog(Classes::TComponent* AOwner, int Dummy) : Tntforms::TTntForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTntLoginDialog(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntLoginDialog(HWND ParentWindow) : Tntforms::TTntForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool __fastcall TntLoginDialog(const WideString ADatabaseName, WideString &AUserName, WideString &APassword);
extern PACKAGE bool __fastcall TntLoginDialogEx(const WideString ADatabaseName, WideString &AUserName, WideString &APassword, bool NameReadOnly);
extern PACKAGE bool __fastcall TntRemoteLoginDialog(WideString &AUserName, WideString &APassword);

}	/* namespace Tntdblogdlg */
using namespace Tntdblogdlg;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntdblogdlg
