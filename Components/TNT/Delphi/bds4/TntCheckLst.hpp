// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntchecklst.pas' rev: 10.00

#ifndef TntchecklstHPP
#define TntchecklstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Checklst.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Tntcontrols.hpp>	// Pascal unit
#include <Tntstdctrls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntchecklst
{
//-- type declarations -------------------------------------------------------
typedef DynamicArray<bool >  TntCheckLst__2;

typedef DynamicArray<Stdctrls::TCheckBoxState >  TntCheckLst__3;

typedef DynamicArray<bool >  TntCheckLst__4;

class DELPHICLASS TTntCheckListBox;
class PASCALIMPLEMENTATION TTntCheckListBox : public Checklst::TCheckListBox 
{
	typedef Checklst::TCheckListBox inherited;
	
private:
	Tntclasses::TTntStrings* FItems;
	Tntclasses::TTntStrings* FSaveItems;
	int FSaveTopIndex;
	int FSaveItemIndex;
	DynamicArray<bool >  FSaved_ItemEnabled;
	DynamicArray<Stdctrls::TCheckBoxState >  FSaved_State;
	DynamicArray<bool >  FSaved_Header;
	Tntstdctrls::TLBGetWideDataEvent FOnData;
	HIDESBASE void __fastcall SetItems(const Tntclasses::TTntStrings* Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall LBGetText(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall LBGetTextLen(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	virtual void __fastcall DrawItem(int Index, const Types::TRect &Rect, Windows::TOwnerDrawState State);
	
public:
	__fastcall virtual TTntCheckListBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCheckListBox(void);
	virtual void __fastcall CopySelection(Controls::TCustomListControl* Destination);
	HIDESBASE virtual void __fastcall AddItem(const WideString Item, System::TObject* AObject);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
	__property Tntclasses::TTntStrings* Items = {read=FItems, write=SetItems};
	__property Tntstdctrls::TLBGetWideDataEvent OnData = {read=FOnData, write=FOnData};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCheckListBox(HWND ParentWindow) : Checklst::TCheckListBox(ParentWindow) { }
	#pragma option pop
	
private:
	void *__IWideCustomListControl;	/* Tntcontrols::IWideCustomListControl */
	
public:
	operator IWideCustomListControl*(void) { return (IWideCustomListControl*)&__IWideCustomListControl; }
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntchecklst */
using namespace Tntchecklst;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntchecklst
