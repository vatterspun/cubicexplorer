// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntstdctrls.pas' rev: 10.00

#ifndef TntstdctrlsHPP
#define TntstdctrlsHPP

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
#include <Tntcontrols.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Tntsysutils.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Widestrings.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntstdctrls
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntCustomEdit;
class PASCALIMPLEMENTATION TTntCustomEdit : public Stdctrls::TCustomEdit 
{
	typedef Stdctrls::TCustomEdit inherited;
	
private:
	WideChar FPasswordChar;
	HIDESBASE void __fastcall SetSelText(const WideString Value);
	HIDESBASE WideString __fastcall GetText();
	HIDESBASE void __fastcall SetText(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	WideChar __fastcall GetPasswordChar(void);
	HIDESBASE void __fastcall SetPasswordChar(const WideChar Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	HIDESBASE virtual int __fastcall GetSelStart(void);
	HIDESBASE virtual void __fastcall SetSelStart(const int Value);
	HIDESBASE virtual int __fastcall GetSelLength(void);
	HIDESBASE virtual void __fastcall SetSelLength(const int Value);
	HIDESBASE virtual WideString __fastcall GetSelText();
	__property WideChar PasswordChar = {read=GetPasswordChar, write=SetPasswordChar, default=0};
	
public:
	__property WideString SelText = {read=GetSelText, write=SetSelText};
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelLength = {read=GetSelLength, write=SetSelLength, nodefault};
	__property WideString Text = {read=GetText, write=SetText};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomEdit.Create */ inline __fastcall virtual TTntCustomEdit(Classes::TComponent* AOwner) : Stdctrls::TCustomEdit(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomEdit(HWND ParentWindow) : Stdctrls::TCustomEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntCustomEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntEdit;
class PASCALIMPLEMENTATION TTntEdit : public TTntCustomEdit 
{
	typedef TTntCustomEdit inherited;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoSelect  = {default=1};
	__property AutoSize  = {default=1};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {index=1, default=1};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property CharCase  = {default=0};
	__property Color  = {default=-16777211};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property HideSelection  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property MaxLength  = {default=0};
	__property OEMConvert  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PasswordChar  = {default=0};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomEdit.Create */ inline __fastcall virtual TTntEdit(Classes::TComponent* AOwner) : TTntCustomEdit(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntEdit(HWND ParentWindow) : TTntCustomEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntMemoStrings;
class PASCALIMPLEMENTATION TTntMemoStrings : public Tntclasses::TTntStrings 
{
	typedef Tntclasses::TTntStrings inherited;
	
protected:
	Stdctrls::TCustomMemo* FMemo;
	Classes::TStrings* FMemoLines;
	bool FRichEditMode;
	Tntsysutils::TTntTextLineBreakStyle FLineBreakStyle;
	virtual WideString __fastcall Get(int Index);
	virtual int __fastcall GetCount(void);
	virtual WideString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const WideString S);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	__fastcall TTntMemoStrings(void);
	virtual void __fastcall SetTextStr(const WideString Value);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Insert(int Index, const WideString S);
public:
	#pragma option push -w-inl
	/* TTntStrings.Destroy */ inline __fastcall virtual ~TTntMemoStrings(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomMemo;
class PASCALIMPLEMENTATION TTntCustomMemo : public Stdctrls::TCustomMemo 
{
	typedef Stdctrls::TCustomMemo inherited;
	
private:
	Tntclasses::TTntStrings* FLines;
	HIDESBASE void __fastcall SetSelText(const WideString Value);
	HIDESBASE WideString __fastcall GetText();
	HIDESBASE void __fastcall SetText(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	HIDESBASE virtual void __fastcall SetLines(const Tntclasses::TTntStrings* Value);
	HIDESBASE virtual int __fastcall GetSelStart(void);
	HIDESBASE virtual void __fastcall SetSelStart(const int Value);
	HIDESBASE virtual int __fastcall GetSelLength(void);
	HIDESBASE virtual void __fastcall SetSelLength(const int Value);
	HIDESBASE WideString __fastcall GetSelText();
	
public:
	__fastcall virtual TTntCustomMemo(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomMemo(void);
	__property WideString SelText = {read=GetSelText, write=SetSelText};
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelLength = {read=GetSelLength, write=SetSelLength, nodefault};
	__property WideString Text = {read=GetText, write=SetText};
	__property Tntclasses::TTntStrings* Lines = {read=FLines, write=SetLines};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomMemo(HWND ParentWindow) : Stdctrls::TCustomMemo(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntMemo;
class PASCALIMPLEMENTATION TTntMemo : public TTntCustomMemo 
{
	typedef TTntCustomMemo inherited;
	
__published:
	__property Align  = {default=0};
	__property Alignment  = {default=0};
	__property Anchors  = {default=3};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {index=1, default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property Color  = {default=-16777211};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property HideSelection  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property Lines ;
	__property MaxLength  = {default=0};
	__property OEMConvert  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ScrollBars  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property WantReturns  = {default=1};
	__property WantTabs  = {default=0};
	__property WordWrap  = {default=1};
	__property OnChange ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomMemo.Create */ inline __fastcall virtual TTntMemo(Classes::TComponent* AOwner) : TTntCustomMemo(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomMemo.Destroy */ inline __fastcall virtual ~TTntMemo(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntMemo(HWND ParentWindow) : TTntCustomMemo(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntComboBoxStrings;
class PASCALIMPLEMENTATION TTntComboBoxStrings : public Tntclasses::TTntStrings 
{
	typedef Tntclasses::TTntStrings inherited;
	
protected:
	virtual WideString __fastcall Get(int Index);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	Stdctrls::TCustomComboBox* ComboBox;
	virtual int __fastcall Add(const WideString S);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual int __fastcall IndexOf(const WideString S);
	virtual void __fastcall Insert(int Index, const WideString S);
public:
	#pragma option push -w-inl
	/* TTntStrings.Create */ inline __fastcall TTntComboBoxStrings(void) : Tntclasses::TTntStrings() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntStrings.Destroy */ inline __fastcall virtual ~TTntComboBoxStrings(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TWMCharMsgHandler)(Messages::TWMKey &Message);

__interface ITntComboFindString;
typedef System::DelphiInterface<ITntComboFindString> _di_ITntComboFindString;
__interface  INTERFACE_UUID("{63BEBEF4-B1A2-495A-B558-7487B66F6827}") ITntComboFindString  : public IInterface 
{
	
public:
	virtual int __fastcall FindString(const WideString Value, int StartPos) = 0 ;
};

class DELPHICLASS TTntCustomComboBox;
class PASCALIMPLEMENTATION TTntCustomComboBox : public Stdctrls::TCustomComboBox 
{
	typedef Stdctrls::TCustomComboBox inherited;
	
private:
	Tntclasses::TTntStrings* FItems;
	Tntclasses::TTntStrings* FSaveItems;
	int FSaveItemIndex;
	WideString FFilter;
	unsigned FLastTime;
	Tntclasses::TTntStrings* __fastcall GetItems(void);
	HIDESBASE int __fastcall GetSelStart(void);
	HIDESBASE void __fastcall SetSelStart(const int Value);
	HIDESBASE int __fastcall GetSelLength(void);
	HIDESBASE void __fastcall SetSelLength(const int Value);
	HIDESBASE WideString __fastcall GetSelText();
	HIDESBASE void __fastcall SetSelText(const WideString Value);
	HIDESBASE WideString __fastcall GetText();
	HIDESBASE void __fastcall SetText(const WideString Value);
	HIDESBASE MESSAGE void __fastcall CNCommand(Messages::TWMCommand &Message);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall DestroyWnd(void);
	DYNAMIC bool __fastcall GetAutoComplete_UniqueMatchOnly(void);
	DYNAMIC bool __fastcall GetAutoComplete_PreserveDataEntryCase(void);
	virtual void __fastcall DoEditCharMsg(Messages::TWMKey &Message);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall ComboWndProc(Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
	virtual void __fastcall DrawItem(int Index, const Types::TRect &Rect, Windows::TOwnerDrawState State);
	DYNAMIC void __fastcall KeyPress(char &Key);
	HIDESBASE virtual void __fastcall SetItems(const Tntclasses::TTntStrings* Value);
	
public:
	__fastcall virtual TTntCustomComboBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomComboBox(void);
	virtual void __fastcall CopySelection(Controls::TCustomListControl* Destination);
	HIDESBASE virtual void __fastcall AddItem(const WideString Item, System::TObject* AObject);
	__property WideString SelText = {read=GetSelText, write=SetSelText};
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelLength = {read=GetSelLength, write=SetSelLength, nodefault};
	__property WideString Text = {read=GetText, write=SetText};
	__property Tntclasses::TTntStrings* Items = {read=GetItems, write=SetItems};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomComboBox(HWND ParentWindow) : Stdctrls::TCustomComboBox(ParentWindow) { }
	#pragma option pop
	
private:
	void *__IWideCustomListControl;	/* Tntcontrols::IWideCustomListControl */
	
public:
	operator IWideCustomListControl*(void) { return (IWideCustomListControl*)&__IWideCustomListControl; }
	
};


class DELPHICLASS TTntComboBox;
class PASCALIMPLEMENTATION TTntComboBox : public TTntCustomComboBox 
{
	typedef TTntCustomComboBox inherited;
	
__published:
	__property Align  = {default=0};
	__property AutoComplete  = {default=1};
	__property AutoCompleteDelay  = {default=500};
	__property AutoDropDown  = {default=0};
	__property AutoCloseUp  = {default=0};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {index=1, default=1};
	__property Style  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property CharCase  = {default=0};
	__property Color  = {default=-16777211};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property DropDownCount  = {default=8};
	__property Enabled  = {default=1};
	__property Font ;
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ItemHeight ;
	__property ItemIndex  = {default=-1};
	__property MaxLength  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property Sorted  = {default=0};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnClick ;
	__property OnCloseUp ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawItem ;
	__property OnDropDown ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMeasureItem ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnSelect ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property Items ;
public:
	#pragma option push -w-inl
	/* TTntCustomComboBox.Create */ inline __fastcall virtual TTntComboBox(Classes::TComponent* AOwner) : TTntCustomComboBox(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomComboBox.Destroy */ inline __fastcall virtual ~TTntComboBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntComboBox(HWND ParentWindow) : TTntCustomComboBox(ParentWindow) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TLBGetWideDataEvent)(Controls::TWinControl* Control, int Index, WideString &Data);

class DELPHICLASS TAccessCustomListBox;
class PASCALIMPLEMENTATION TAccessCustomListBox : public Stdctrls::TCustomListBox 
{
	typedef Stdctrls::TCustomListBox inherited;
	
public:
	#pragma option push -w-inl
	/* TCustomListBox.Create */ inline __fastcall virtual TAccessCustomListBox(Classes::TComponent* AOwner) : Stdctrls::TCustomListBox(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomListBox.Destroy */ inline __fastcall virtual ~TAccessCustomListBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TAccessCustomListBox(HWND ParentWindow) : Stdctrls::TCustomListBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntListBoxStrings;
class PASCALIMPLEMENTATION TTntListBoxStrings : public Tntclasses::TTntStrings 
{
	typedef Tntclasses::TTntStrings inherited;
	
private:
	TAccessCustomListBox* FListBox;
	Stdctrls::TCustomListBox* __fastcall GetListBox(void);
	void __fastcall SetListBox(const Stdctrls::TCustomListBox* Value);
	
protected:
	virtual void __fastcall Put(int Index, const WideString S);
	virtual WideString __fastcall Get(int Index);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	virtual int __fastcall Add(const WideString S);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual int __fastcall IndexOf(const WideString S);
	virtual void __fastcall Insert(int Index, const WideString S);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	__property Stdctrls::TCustomListBox* ListBox = {read=GetListBox, write=SetListBox};
public:
	#pragma option push -w-inl
	/* TTntStrings.Create */ inline __fastcall TTntListBoxStrings(void) : Tntclasses::TTntStrings() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntStrings.Destroy */ inline __fastcall virtual ~TTntListBoxStrings(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomListBox;
class PASCALIMPLEMENTATION TTntCustomListBox : public Stdctrls::TCustomListBox 
{
	typedef Stdctrls::TCustomListBox inherited;
	
private:
	Tntclasses::TTntStrings* FItems;
	Tntclasses::TTntStrings* FSaveItems;
	int FSaveTopIndex;
	int FSaveItemIndex;
	TLBGetWideDataEvent FOnData;
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
	__property TLBGetWideDataEvent OnData = {read=FOnData, write=FOnData};
	
public:
	__fastcall virtual TTntCustomListBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomListBox(void);
	virtual void __fastcall CopySelection(Controls::TCustomListControl* Destination);
	HIDESBASE virtual void __fastcall AddItem(const WideString Item, System::TObject* AObject);
	__property Tntclasses::TTntStrings* Items = {read=FItems, write=SetItems};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomListBox(HWND ParentWindow) : Stdctrls::TCustomListBox(ParentWindow) { }
	#pragma option pop
	
private:
	void *__IWideCustomListControl;	/* Tntcontrols::IWideCustomListControl */
	
public:
	operator IWideCustomListControl*(void) { return (IWideCustomListControl*)&__IWideCustomListControl; }
	
};


class DELPHICLASS TTntListBox;
class PASCALIMPLEMENTATION TTntListBox : public TTntCustomListBox 
{
	typedef TTntCustomListBox inherited;
	
__published:
	__property Style  = {default=0};
	__property AutoComplete  = {default=1};
	__property AutoCompleteDelay  = {default=500};
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {index=1, default=1};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property Color  = {default=-16777211};
	__property Columns  = {default=0};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property ExtendedSelect  = {default=1};
	__property Font ;
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property IntegralHeight  = {default=0};
	__property ItemHeight ;
	__property Items ;
	__property MultiSelect  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ScrollWidth  = {default=0};
	__property ShowHint ;
	__property Sorted  = {default=0};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property TabWidth  = {default=0};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnData ;
	__property OnDataFind ;
	__property OnDataObject ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawItem ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMeasureItem ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomListBox.Create */ inline __fastcall virtual TTntListBox(Classes::TComponent* AOwner) : TTntCustomListBox(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomListBox.Destroy */ inline __fastcall virtual ~TTntListBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntListBox(HWND ParentWindow) : TTntCustomListBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomLabel;
class PASCALIMPLEMENTATION TTntCustomLabel : public Stdctrls::TCustomLabel 
{
	typedef Stdctrls::TCustomLabel inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	HIDESBASE virtual WideString __fastcall GetLabelText();
	DYNAMIC void __fastcall DoDrawText(Types::TRect &Rect, int Flags);
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomLabel.Create */ inline __fastcall virtual TTntCustomLabel(Classes::TComponent* AOwner) : Stdctrls::TCustomLabel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TTntCustomLabel(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntLabel;
class PASCALIMPLEMENTATION TTntLabel : public TTntCustomLabel 
{
	typedef TTntCustomLabel inherited;
	
__published:
	__property Align  = {default=0};
	__property Alignment  = {default=0};
	__property Anchors  = {default=3};
	__property AutoSize  = {default=1};
	__property BiDiMode ;
	__property Caption ;
	__property Color ;
	__property Constraints ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property EllipsisPosition  = {default=0};
	__property Enabled  = {default=1};
	__property FocusControl ;
	__property Font ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowAccelChar  = {default=1};
	__property ShowHint ;
	__property Transparent ;
	__property Layout  = {default=0};
	__property Visible  = {default=1};
	__property WordWrap  = {default=0};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomLabel.Create */ inline __fastcall virtual TTntLabel(Classes::TComponent* AOwner) : TTntCustomLabel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TTntLabel(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntButton;
class PASCALIMPLEMENTATION TTntButton : public Stdctrls::TButton 
{
	typedef Stdctrls::TButton inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TButton.Create */ inline __fastcall virtual TTntButton(Classes::TComponent* AOwner) : Stdctrls::TButton(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntButton(HWND ParentWindow) : Stdctrls::TButton(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntButton(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomCheckBox;
class PASCALIMPLEMENTATION TTntCustomCheckBox : public Stdctrls::TCustomCheckBox 
{
	typedef Stdctrls::TCustomCheckBox inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
public:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomCheckBox.Create */ inline __fastcall virtual TTntCustomCheckBox(Classes::TComponent* AOwner) : Stdctrls::TCustomCheckBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomCheckBox(HWND ParentWindow) : Stdctrls::TCustomCheckBox(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntCustomCheckBox(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCheckBox;
class PASCALIMPLEMENTATION TTntCheckBox : public TTntCustomCheckBox 
{
	typedef TTntCustomCheckBox inherited;
	
__published:
	__property Action ;
	__property Align  = {default=0};
	__property Alignment  = {default=1};
	__property AllowGrayed  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Caption ;
	__property Checked  = {default=0};
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property State  = {default=0};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property WordWrap  = {default=0};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomCheckBox.Create */ inline __fastcall virtual TTntCheckBox(Classes::TComponent* AOwner) : TTntCustomCheckBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCheckBox(HWND ParentWindow) : TTntCustomCheckBox(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntCheckBox(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntRadioButton;
class PASCALIMPLEMENTATION TTntRadioButton : public Stdctrls::TRadioButton 
{
	typedef Stdctrls::TRadioButton inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TRadioButton.Create */ inline __fastcall virtual TTntRadioButton(Classes::TComponent* AOwner) : Stdctrls::TRadioButton(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntRadioButton(HWND ParentWindow) : Stdctrls::TRadioButton(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntRadioButton(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntScrollBar;
class PASCALIMPLEMENTATION TTntScrollBar : public Stdctrls::TScrollBar 
{
	typedef Stdctrls::TScrollBar inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TScrollBar.Create */ inline __fastcall virtual TTntScrollBar(Classes::TComponent* AOwner) : Stdctrls::TScrollBar(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntScrollBar(HWND ParentWindow) : Stdctrls::TScrollBar(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntScrollBar(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomGroupBox;
class PASCALIMPLEMENTATION TTntCustomGroupBox : public Stdctrls::TCustomGroupBox 
{
	typedef Stdctrls::TCustomGroupBox inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall Paint(void);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomGroupBox.Create */ inline __fastcall virtual TTntCustomGroupBox(Classes::TComponent* AOwner) : Stdctrls::TCustomGroupBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTntCustomGroupBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomGroupBox(HWND ParentWindow) : Stdctrls::TCustomGroupBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntGroupBox;
class PASCALIMPLEMENTATION TTntGroupBox : public TTntCustomGroupBox 
{
	typedef TTntCustomGroupBox inherited;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Caption ;
	__property Color  = {default=-16777211};
	__property Constraints ;
	__property Ctl3D ;
	__property DockSite  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
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
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDockDrop ;
	__property OnDockOver ;
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
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnUnDock ;
public:
	#pragma option push -w-inl
	/* TCustomGroupBox.Create */ inline __fastcall virtual TTntGroupBox(Classes::TComponent* AOwner) : TTntCustomGroupBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTntGroupBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntGroupBox(HWND ParentWindow) : TTntCustomGroupBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomStaticText;
class PASCALIMPLEMENTATION TTntCustomStaticText : public Stdctrls::TCustomStaticText 
{
	typedef Stdctrls::TCustomStaticText inherited;
	
private:
	HIDESBASE void __fastcall AdjustBounds(void);
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	
protected:
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Messages::TMessage &Message);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall SetAutoSize(bool AValue);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	
public:
	__fastcall virtual TTntCustomStaticText(Classes::TComponent* AOwner);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomStaticText(HWND ParentWindow) : Stdctrls::TCustomStaticText(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntCustomStaticText(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStaticText;
class PASCALIMPLEMENTATION TTntStaticText : public TTntCustomStaticText 
{
	typedef TTntCustomStaticText inherited;
	
__published:
	__property Align  = {default=0};
	__property Alignment  = {default=0};
	__property Anchors  = {default=3};
	__property AutoSize  = {default=1};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {index=1, default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=0};
	__property Caption ;
	__property Color ;
	__property Constraints ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property FocusControl ;
	__property Font ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowAccelChar  = {default=1};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property Transparent  = {default=1};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomStaticText.Create */ inline __fastcall virtual TTntStaticText(Classes::TComponent* AOwner) : TTntCustomStaticText(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntStaticText(HWND ParentWindow) : TTntCustomStaticText(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntStaticText(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TntCustomEdit_CreateWindowHandle(Stdctrls::TCustomEdit* Edit, const Controls::TCreateParams &Params);
extern PACKAGE void __fastcall TntCustomEdit_AfterInherited_CreateWnd(Stdctrls::TCustomEdit* Edit, WideChar &FPasswordChar);
extern PACKAGE int __fastcall TntCustomEdit_GetSelStart(Stdctrls::TCustomEdit* Edit);
extern PACKAGE void __fastcall TntCustomEdit_SetSelStart(Stdctrls::TCustomEdit* Edit, const int Value);
extern PACKAGE int __fastcall TntCustomEdit_GetSelLength(Stdctrls::TCustomEdit* Edit);
extern PACKAGE void __fastcall TntCustomEdit_SetSelLength(Stdctrls::TCustomEdit* Edit, const int Value);
extern PACKAGE WideString __fastcall TntCustomEdit_GetSelText(Stdctrls::TCustomEdit* Edit);
extern PACKAGE void __fastcall TntCustomEdit_SetSelText(Stdctrls::TCustomEdit* Edit, const WideString Value);
extern PACKAGE WideChar __fastcall TntCustomEdit_GetPasswordChar(Stdctrls::TCustomEdit* Edit, WideChar &FPasswordChar);
extern PACKAGE void __fastcall TntCustomEdit_SetPasswordChar(Stdctrls::TCustomEdit* Edit, WideChar &FPasswordChar, const WideChar Value);
extern PACKAGE int __fastcall TntMemo_LineStart(unsigned Handle, int Index);
extern PACKAGE int __fastcall TntMemo_LineLength(unsigned Handle, int Index, int StartPos = 0xffffffff);
extern PACKAGE void __fastcall TntCombo_AfterInherited_CreateWnd(Stdctrls::TCustomComboBox* Combo, Tntclasses::TTntStrings* Items, Tntclasses::TTntStrings* &FSaveItems, int FSaveItemIndex, AnsiString PreInheritedAnsiText);
extern PACKAGE void __fastcall TntCombo_BeforeInherited_DestroyWnd(Stdctrls::TCustomComboBox* Combo, Tntclasses::TTntStrings* Items, Tntclasses::TTntStrings* &FSaveItems, int ItemIndex, int &FSaveItemIndex, WideString &SavedText);
extern PACKAGE bool __fastcall TntCombo_ComboWndProc(Stdctrls::TCustomComboBox* Combo, Messages::TMessage &Message, HWND ComboWnd, void * ComboProc, TWMCharMsgHandler DoEditCharMsg);
extern PACKAGE bool __fastcall TntCombo_CNCommand(Stdctrls::TCustomComboBox* Combo, Tntclasses::TTntStrings* Items, Messages::TWMCommand &Message);
extern PACKAGE int __fastcall TntCombo_GetSelStart(Stdctrls::TCustomComboBox* Combo);
extern PACKAGE void __fastcall TntCombo_SetSelStart(Stdctrls::TCustomComboBox* Combo, const int Value);
extern PACKAGE int __fastcall TntCombo_GetSelLength(Stdctrls::TCustomComboBox* Combo);
extern PACKAGE void __fastcall TntCombo_SetSelLength(Stdctrls::TCustomComboBox* Combo, const int Value);
extern PACKAGE WideString __fastcall TntCombo_GetSelText(Stdctrls::TCustomComboBox* Combo);
extern PACKAGE void __fastcall TntCombo_SetSelText(Stdctrls::TCustomComboBox* Combo, const WideString Value);
extern PACKAGE void __fastcall TntCombo_BeforeKeyPress(Stdctrls::TCustomComboBox* Combo, bool &SaveAutoComplete);
extern PACKAGE void __fastcall TntCombo_AfterKeyPress(Stdctrls::TCustomComboBox* Combo, bool &SaveAutoComplete);
extern PACKAGE void __fastcall TntCombo_DropDown_PreserveSelection(Stdctrls::TCustomComboBox* Combo);
extern PACKAGE void __fastcall TntComboBox_AddItem(Tntclasses::TTntStrings* Items, const WideString Item, System::TObject* AObject);
extern PACKAGE void __fastcall TntComboBox_CopySelection(Tntclasses::TTntStrings* Items, int ItemIndex, Controls::TCustomListControl* Destination);
extern PACKAGE void __fastcall TntCombo_AutoSearchKeyPress(Stdctrls::TCustomComboBox* Combo, Tntclasses::TTntStrings* Items, Messages::TWMKey &Message, WideString &FFilter, unsigned &FLastTime);
extern PACKAGE void __fastcall TntCombo_AutoCompleteKeyPress(Stdctrls::TCustomComboBox* Combo, Tntclasses::TTntStrings* Items, Messages::TWMKey &Message, bool AutoComplete_UniqueMatchOnly, bool AutoComplete_PreserveDataEntryCase);
extern PACKAGE void __fastcall TntCombo_DefaultDrawItem(Graphics::TCanvas* Canvas, int Index, const Types::TRect &Rect, Windows::TOwnerDrawState State, Tntclasses::TTntStrings* Items);
extern PACKAGE void __fastcall TntListBox_AfterInherited_CreateWnd(Stdctrls::TCustomListBox* ListBox, Tntclasses::TTntStrings* &FSaveItems, Tntclasses::TTntStrings* FItems, int FSaveTopIndex, int FSaveItemIndex);
extern PACKAGE void __fastcall TntListBox_BeforeInherited_DestroyWnd(Stdctrls::TCustomListBox* ListBox, Tntclasses::TTntStrings* &FSaveItems, const Tntclasses::TTntStrings* FItems, int &FSaveTopIndex, int &FSaveItemIndex);
extern PACKAGE void __fastcall TntListBox_DrawItem_Text(Stdctrls::TCustomListBox* ListBox, Tntclasses::TTntStrings* Items, int Index, const Types::TRect &Rect);
extern PACKAGE void __fastcall TntListBox_AddItem(Tntclasses::TTntStrings* Items, const WideString Item, System::TObject* AObject);
extern PACKAGE void __fastcall TntListBox_CopySelection(Stdctrls::TCustomListBox* ListBox, Tntclasses::TTntStrings* Items, Controls::TCustomListControl* Destination);
extern PACKAGE bool __fastcall TntCustomListBox_LBGetText(Stdctrls::TCustomListBox* ListBox, TLBGetWideDataEvent OnData, Messages::TMessage &Message);
extern PACKAGE bool __fastcall TntCustomListBox_LBGetTextLen(Stdctrls::TCustomListBox* ListBox, TLBGetWideDataEvent OnData, Messages::TMessage &Message);
extern PACKAGE bool __fastcall TntLabel_DoDrawText(Stdctrls::TCustomLabel* Control, Types::TRect &Rect, int Flags, const WideString GetLabelText);
extern PACKAGE void __fastcall TntLabel_CMDialogChar(Stdctrls::TCustomLabel* Control, Messages::TWMKey &Message, const WideString Caption);
extern PACKAGE void __fastcall TntButton_CMDialogChar(Stdctrls::TButton* Button, Messages::TWMKey &Message);

}	/* namespace Tntstdctrls */
using namespace Tntstdctrls;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntstdctrls
