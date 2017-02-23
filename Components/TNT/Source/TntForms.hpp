// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntforms.pas' rev: 10.00

#ifndef TntformsHPP
#define TntformsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Tntcontrols.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntforms
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntScrollBox;
class PASCALIMPLEMENTATION TTntScrollBox : public Forms::TScrollBox 
{
	typedef Forms::TScrollBox inherited;
	
private:
	int FWMSizeCallCount;
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TScrollBox.Create */ inline __fastcall virtual TTntScrollBox(Classes::TComponent* AOwner) : Forms::TScrollBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TTntScrollBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntScrollBox(HWND ParentWindow) : Forms::TScrollBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomFrame;
class PASCALIMPLEMENTATION TTntCustomFrame : public Forms::TCustomFrame 
{
	typedef Forms::TCustomFrame inherited;
	
private:
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomFrame.Create */ inline __fastcall virtual TTntCustomFrame(Classes::TComponent* AOwner) : Forms::TCustomFrame(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TTntCustomFrame(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomFrame(HWND ParentWindow) : Forms::TCustomFrame(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntFrame;
class PASCALIMPLEMENTATION TTntFrame : public TTntCustomFrame 
{
	typedef TTntCustomFrame inherited;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoScroll  = {default=0};
	__property AutoSize  = {default=0};
	__property BiDiMode ;
	__property Constraints ;
	__property DockSite  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Color ;
	__property Ctl3D ;
	__property Font ;
	__property Padding ;
	__property ParentBackground  = {default=1};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property Visible  = {default=1};
	__property OnAlignInsertBefore ;
	__property OnAlignPosition ;
	__property OnCanResize ;
	__property OnClick ;
	__property OnConstrainedResize ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDockDrop ;
	__property OnDockOver ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetSiteInfo ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnMouseWheel ;
	__property OnMouseWheelDown ;
	__property OnMouseWheelUp ;
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnUnDock ;
public:
	#pragma option push -w-inl
	/* TCustomFrame.Create */ inline __fastcall virtual TTntFrame(Classes::TComponent* AOwner) : TTntCustomFrame(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TTntFrame(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntFrame(HWND ParentWindow) : TTntCustomFrame(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntForm;
class PASCALIMPLEMENTATION TTntForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall WMMenuSelect(Messages::TWMMenuSelect &Message);
	HIDESBASE MESSAGE void __fastcall CMBiDiModeChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanging(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall UpdateActions(void);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DestroyWindowHandle(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC Controls::_di_IDockManager __fastcall CreateDockManager();
	
public:
	__fastcall virtual TTntForm(Classes::TComponent* AOwner);
	virtual void __fastcall DefaultHandler(void *Message);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTntForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTntForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntApplication;
class PASCALIMPLEMENTATION TTntApplication : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FMainFormChecked;
	WideString FHint;
	Controls::TControl* FTntAppIdleEventControl;
	unsigned FSettingChangeTime;
	WideString FTitle;
	WideString __fastcall GetHint();
	void __fastcall SetAnsiAppHint(const AnsiString Value);
	void __fastcall SetHint(const WideString Value);
	WideString __fastcall GetExeName();
	bool __fastcall IsDlgMsg(tagMSG &Msg);
	void __fastcall DoIdle(void);
	WideString __fastcall GetTitle();
	void __fastcall SetTitle(const WideString Value);
	void __fastcall SetAnsiApplicationTitle(const AnsiString Value);
	WideString __fastcall ApplicationMouseControlHint();
	
protected:
	bool __fastcall WndProc(Messages::TMessage &Message);
	bool __fastcall ProcessMessage(tagMSG &Msg);
	
public:
	__fastcall virtual TTntApplication(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntApplication(void);
	__property WideString Hint = {read=GetHint, write=SetHint};
	__property WideString ExeName = {read=GetExeName};
	__property unsigned SettingChangeTime = {read=FSettingChangeTime, nodefault};
	__property WideString Title = {read=GetTitle, write=SetTitle};
};


typedef void __fastcall (*TFormProc)(Forms::TForm* Form);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TTntApplication* TntApplication;
extern PACKAGE bool __fastcall IsWideCharAccel(Word CharCode, const WideString Caption);
extern PACKAGE void __fastcall EnableManualPeekMessageWithRemove(void);
extern PACKAGE void __fastcall DisableManualPeekMessageWithRemove(void);
extern PACKAGE void __fastcall InitTntEnvironment(void);

}	/* namespace Tntforms */
using namespace Tntforms;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntforms
