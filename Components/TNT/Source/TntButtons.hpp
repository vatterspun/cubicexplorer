// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntbuttons.pas' rev: 10.00

#ifndef TntbuttonsHPP
#define TntbuttonsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Extctrls.hpp>	// Pascal unit
#include <Commctrl.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Tntcontrols.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntbuttons
{
//-- type declarations -------------------------------------------------------
__interface ITntGlyphButton;
typedef System::DelphiInterface<ITntGlyphButton> _di_ITntGlyphButton;
__interface  INTERFACE_UUID("{15D7E501-1E33-4293-8B45-716FB3B14504}") ITntGlyphButton  : public IInterface 
{
	
public:
	virtual void * __fastcall GetButtonGlyph(void) = 0 ;
	virtual void __fastcall UpdateInternalGlyphList(void) = 0 ;
};

class DELPHICLASS TTntSpeedButton;
class PASCALIMPLEMENTATION TTntSpeedButton : public Buttons::TSpeedButton 
{
	typedef Buttons::TSpeedButton inherited;
	
private:
	bool FPaintInherited;
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	void * __fastcall GetButtonGlyph(void);
	DYNAMIC void __fastcall UpdateInternalGlyphList(void);
	DYNAMIC void __fastcall PaintButton(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TSpeedButton.Create */ inline __fastcall virtual TTntSpeedButton(Classes::TComponent* AOwner) : Buttons::TSpeedButton(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TSpeedButton.Destroy */ inline __fastcall virtual ~TTntSpeedButton(void) { }
	#pragma option pop
	
private:
	void *__ITntGlyphButton;	/* Tntbuttons::ITntGlyphButton */
	
public:
	operator ITntGlyphButton*(void) { return (ITntGlyphButton*)&__ITntGlyphButton; }
	
};


class DELPHICLASS TTntBitBtn;
class PASCALIMPLEMENTATION TTntBitBtn : public Buttons::TBitBtn 
{
	typedef Buttons::TBitBtn inherited;
	
private:
	bool FPaintInherited;
	bool FMouseInControl;
	HIDESBASE bool __fastcall IsCaptionStored(void);
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CNDrawItem(Messages::TWMDrawItem &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	
protected:
	void * __fastcall GetButtonGlyph(void);
	DYNAMIC void __fastcall UpdateInternalGlyphList(void);
	HIDESBASEDYNAMIC void __fastcall DrawItem(const tagDRAWITEMSTRUCT &DrawItemStruct);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TBitBtn.Create */ inline __fastcall virtual TTntBitBtn(Classes::TComponent* AOwner) : Buttons::TBitBtn(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TBitBtn.Destroy */ inline __fastcall virtual ~TTntBitBtn(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntBitBtn(HWND ParentWindow) : Buttons::TBitBtn(ParentWindow) { }
	#pragma option pop
	
private:
	void *__ITntGlyphButton;	/* Tntbuttons::ITntGlyphButton */
	
public:
	operator ITntGlyphButton*(void) { return (ITntGlyphButton*)&__ITntGlyphButton; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TButtonGlyph_CalcButtonLayout(Controls::TControl* Control, HDC DC, const Types::TRect &Client, const Types::TPoint &Offset, const WideString Caption, Buttons::TButtonLayout Layout, int Margin, int Spacing, Types::TPoint &GlyphPos, Types::TRect &TextBounds, int BiDiFlags, bool WordWrap);
extern PACKAGE Types::TRect __fastcall TButtonGlyph_Draw(Controls::TControl* Control, Graphics::TCanvas* Canvas, const Types::TRect &Client, const Types::TPoint &Offset, const WideString Caption, Buttons::TButtonLayout Layout, int Margin, int Spacing, Buttons::TButtonState State, bool Transparent, int BiDiFlags, bool WordWrap);

}	/* namespace Tntbuttons */
using namespace Tntbuttons;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntbuttons
