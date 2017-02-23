// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntmenus.pas' rev: 10.00

#ifndef TntmenusHPP
#define TntmenusHPP

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
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntmenus
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntMenuItem;
class PASCALIMPLEMENTATION TTntMenuItem : public Menus::TMenuItem 
{
	typedef Menus::TMenuItem inherited;
	
private:
	bool FIgnoreMenuChanged;
	WideString FCaption;
	WideString FHint;
	HKL FKeyboardLayout;
	WideString __fastcall GetCaption();
	void __fastcall SetInheritedCaption(const AnsiString Value);
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	void __fastcall UpdateMenuString(Menus::TMenu* ParentMenu);
	Word __fastcall GetAlignmentDrawStyle(void);
	int __fastcall MeasureItemTextWidth(Graphics::TCanvas* ACanvas, const WideString Text);
	WideString __fastcall GetHint();
	void __fastcall SetInheritedHint(const AnsiString Value);
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall MenuChanged(bool Rebuild);
	virtual void __fastcall AdvancedDrawItem(Graphics::TCanvas* ACanvas, const Types::TRect &ARect, Windows::TOwnerDrawState State, bool TopLevel);
	HIDESBASE void __fastcall DoDrawText(Graphics::TCanvas* ACanvas, const WideString ACaption, Types::TRect &Rect, bool Selected, int Flags);
	virtual void __fastcall MeasureItem(Graphics::TCanvas* ACanvas, int &Width, int &Height);
	
public:
	virtual void __fastcall InitiateAction(void);
	virtual void __fastcall Loaded(void);
	HIDESBASE Menus::TMenuItem* __fastcall Find(WideString ACaption);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TMenuItem.Create */ inline __fastcall virtual TTntMenuItem(Classes::TComponent* AOwner) : Menus::TMenuItem(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TMenuItem.Destroy */ inline __fastcall virtual ~TTntMenuItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntMainMenu;
class PASCALIMPLEMENTATION TTntMainMenu : public Menus::TMainMenu 
{
	typedef Menus::TMainMenu inherited;
	
protected:
	virtual void __fastcall DoChange(Menus::TMenuItem* Source, bool Rebuild);
	
public:
	DYNAMIC Menus::TMenuItem* __fastcall CreateMenuItem(void);
public:
	#pragma option push -w-inl
	/* TMenu.Create */ inline __fastcall virtual TTntMainMenu(Classes::TComponent* AOwner) : Menus::TMainMenu(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TMenu.Destroy */ inline __fastcall virtual ~TTntMainMenu(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPopupMenu;
class PASCALIMPLEMENTATION TTntPopupMenu : public Menus::TPopupMenu 
{
	typedef Menus::TPopupMenu inherited;
	
protected:
	virtual void __fastcall DoChange(Menus::TMenuItem* Source, bool Rebuild);
	
public:
	__fastcall virtual TTntPopupMenu(Classes::TComponent* AOwner);
	DYNAMIC Menus::TMenuItem* __fastcall CreateMenuItem(void);
	__fastcall virtual ~TTntPopupMenu(void);
	virtual void __fastcall Popup(int X, int Y);
};


class DELPHICLASS TTntPopupList;
class PASCALIMPLEMENTATION TTntPopupList : public Menus::TPopupList 
{
	typedef Menus::TPopupList inherited;
	
private:
	Menus::TPopupList* SavedPopupList;
	
protected:
	virtual void __fastcall WndProc(Messages::TMessage &Message);
public:
	#pragma option push -w-inl
	/* TList.Destroy */ inline __fastcall virtual ~TTntPopupList(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTntPopupList(void) : Menus::TPopupList() { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TTntPopupList* TntPopupList;
extern PACKAGE TTntMenuItem* __fastcall WideNewSubMenu(const WideString ACaption, Classes::THelpContext hCtx, const AnsiString AName, TTntMenuItem* const * Items, const int Items_Size, bool AEnabled);
extern PACKAGE TTntMenuItem* __fastcall WideNewItem(const WideString ACaption, Classes::TShortCut AShortCut, bool AChecked, bool AEnabled, Classes::TNotifyEvent AOnClick, Classes::THelpContext hCtx, const AnsiString AName);
extern PACKAGE Classes::TShortCut __fastcall MessageToShortCut(const Messages::TWMKey &Msg);
extern PACKAGE WideString __fastcall WideShortCutToText(Word WordShortCut);
extern PACKAGE Classes::TShortCut __fastcall WideTextToShortCut(WideString Text);
extern PACKAGE WideString __fastcall WideGetHotkey(const WideString Text);
extern PACKAGE WideString __fastcall WideStripHotkey(const WideString Text);
extern PACKAGE bool __fastcall WideSameCaption(const WideString Text1, const WideString Text2);
extern PACKAGE WideString __fastcall WideGetMenuItemCaption(Menus::TMenuItem* MenuItem);
extern PACKAGE WideString __fastcall WideGetMenuItemHint(Menus::TMenuItem* MenuItem);
extern PACKAGE void __fastcall NoOwnerDrawTopLevelItems(Menus::TMainMenu* Menu);
extern PACKAGE void __fastcall FixMenuBiDiProblem(Menus::TMenu* Menu);
extern PACKAGE bool __fastcall MenuItemHasBitmap(Menus::TMenuItem* MenuItem);

}	/* namespace Tntmenus */
using namespace Tntmenus;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntmenus
