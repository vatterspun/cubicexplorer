// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntextdlgs.pas' rev: 10.00

#ifndef TntextdlgsHPP
#define TntextdlgsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Tntdialogs.hpp>	// Pascal unit
#include <Tntextctrls.hpp>	// Pascal unit
#include <Tntstdctrls.hpp>	// Pascal unit
#include <Tntbuttons.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntextdlgs
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntOpenPictureDialog;
class PASCALIMPLEMENTATION TTntOpenPictureDialog : public Tntdialogs::TTntOpenDialog 
{
	typedef Tntdialogs::TTntOpenDialog inherited;
	
private:
	Tntextctrls::TTntPanel* FPicturePanel;
	Tntstdctrls::TTntLabel* FPictureLabel;
	Tntbuttons::TTntSpeedButton* FPreviewButton;
	Tntextctrls::TTntPanel* FPaintPanel;
	Tntextctrls::TTntImage* FImageCtrl;
	WideString FSavedFilename;
	bool __fastcall IsFilterStored(void);
	void __fastcall PreviewKeyPress(System::TObject* Sender, char &Key);
	
protected:
	virtual void __fastcall PreviewClick(System::TObject* Sender);
	DYNAMIC void __fastcall DoClose(void);
	DYNAMIC void __fastcall DoSelectionChange(void);
	DYNAMIC void __fastcall DoShow(void);
	__property Tntextctrls::TTntImage* ImageCtrl = {read=FImageCtrl};
	__property Tntstdctrls::TTntLabel* PictureLabel = {read=FPictureLabel};
	
__published:
	__property Filter  = {stored=IsFilterStored};
	
public:
	__fastcall virtual TTntOpenPictureDialog(Classes::TComponent* AOwner);
	virtual bool __fastcall Execute(void)/* overload */;
	virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
public:
	#pragma option push -w-inl
	/* TTntOpenDialog.Destroy */ inline __fastcall virtual ~TTntOpenPictureDialog(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntSavePictureDialog;
class PASCALIMPLEMENTATION TTntSavePictureDialog : public TTntOpenPictureDialog 
{
	typedef TTntOpenPictureDialog inherited;
	
public:
	virtual bool __fastcall Execute(void)/* overload */;
	virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
public:
	#pragma option push -w-inl
	/* TTntOpenPictureDialog.Create */ inline __fastcall virtual TTntSavePictureDialog(Classes::TComponent* AOwner) : TTntOpenPictureDialog(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTntOpenDialog.Destroy */ inline __fastcall virtual ~TTntSavePictureDialog(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntextdlgs */
using namespace Tntextdlgs;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntextdlgs
