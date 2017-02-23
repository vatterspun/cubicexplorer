// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntcomctrls.pas' rev: 10.00

#ifndef TntcomctrlsHPP
#define TntcomctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Listactns.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Comctrls.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Commctrl.hpp>	// Pascal unit
#include <Contnrs.hpp>	// Pascal unit
#include <Tntcontrols.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Tntsysutils.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Imglist.hpp>	// Pascal unit
#include <Toolwin.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntcomctrls
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntListColumn;
class PASCALIMPLEMENTATION TTntListColumn : public Comctrls::TListColumn 
{
	typedef Comctrls::TListColumn inherited;
	
private:
	WideString FCaption;
	void __fastcall SetInheritedCaption(const AnsiString Value);
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
public:
	#pragma option push -w-inl
	/* TListColumn.Create */ inline __fastcall virtual TTntListColumn(Classes::TCollection* Collection) : Comctrls::TListColumn(Collection) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TListColumn.Destroy */ inline __fastcall virtual ~TTntListColumn(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntListColumns;
class DELPHICLASS TTntCustomListView;
class PASCALIMPLEMENTATION TTntListColumns : public Comctrls::TListColumns 
{
	typedef Comctrls::TListColumns inherited;
	
public:
	TTntListColumn* operator[](int Index) { return Items[Index]; }
	
private:
	HIDESBASE TTntListColumn* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TTntListColumn* Value);
	
public:
	__fastcall TTntListColumns(TTntCustomListView* AOwner);
	HIDESBASE TTntListColumn* __fastcall Add(void);
	HIDESBASE TTntCustomListView* __fastcall Owner(void);
	__property TTntListColumn* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TTntListColumns(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntListItem;
class DELPHICLASS TTntListItems;
class PASCALIMPLEMENTATION TTntListItem : public Comctrls::TListItem 
{
	typedef Comctrls::TListItem inherited;
	
private:
	WideString FCaption;
	Tntclasses::TTntStrings* FSubItems;
	void __fastcall SetInheritedCaption(const AnsiString Value);
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	HIDESBASE void __fastcall SetSubItems(const Tntclasses::TTntStrings* Value);
	HIDESBASE TTntCustomListView* __fastcall GetListView(void);
	TTntListItems* __fastcall GetTntOwner(void);
	
public:
	__fastcall virtual TTntListItem(Comctrls::TListItems* AOwner);
	__fastcall virtual ~TTntListItem(void);
	__property TTntListItems* Owner = {read=GetTntOwner};
	__property TTntCustomListView* ListView = {read=GetListView};
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property Tntclasses::TTntStrings* SubItems = {read=FSubItems, write=SetSubItems};
};


class DELPHICLASS TTntListItemsEnumerator;
class PASCALIMPLEMENTATION TTntListItemsEnumerator : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FIndex;
	TTntListItems* FListItems;
	
public:
	__fastcall TTntListItemsEnumerator(TTntListItems* AListItems);
	TTntListItem* __fastcall GetCurrent(void);
	bool __fastcall MoveNext(void);
	__property TTntListItem* Current = {read=GetCurrent};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntListItemsEnumerator(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntListItems : public Comctrls::TListItems 
{
	typedef Comctrls::TListItems inherited;
	
public:
	TTntListItem* operator[](int Index) { return Item[Index]; }
	
private:
	HIDESBASE TTntListItem* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, const TTntListItem* Value);
	
public:
	HIDESBASE TTntCustomListView* __fastcall Owner(void);
	__property TTntListItem* Item[int Index] = {read=GetItem, write=SetItem/*, default*/};
	HIDESBASE TTntListItem* __fastcall Add(void);
	HIDESBASE TTntListItem* __fastcall AddItem(TTntListItem* Item, int Index = 0xffffffff);
	HIDESBASE TTntListItemsEnumerator* __fastcall GetEnumerator(void);
	HIDESBASE TTntListItem* __fastcall Insert(int Index);
public:
	#pragma option push -w-inl
	/* TListItems.Create */ inline __fastcall TTntListItems(Comctrls::TCustomListView* AOwner) : Comctrls::TListItems(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TListItems.Destroy */ inline __fastcall virtual ~TTntListItems(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TTntLVEditedEvent)(System::TObject* Sender, TTntListItem* Item, WideString &S);

typedef void __fastcall (__closure *TTntLVOwnerDataFindEvent)(System::TObject* Sender, Comctrls::TItemFind Find, const WideString FindString, const Types::TPoint &FindPosition, void * FindData, int StartIndex, Comctrls::TSearchDirection Direction, bool Wrap, int &Index);

class DELPHICLASS _TntInternalCustomListView;
class PASCALIMPLEMENTATION _TntInternalCustomListView : public Comctrls::TCustomListView 
{
	typedef Comctrls::TCustomListView inherited;
	
private:
	WideChar *PWideFindString;
	tagLVDISPINFOW *CurrentDispInfo;
	unsigned OriginalDispInfoMask;
	virtual int __fastcall OwnerDataFindW(Comctrls::TItemFind Find, const WideString FindString, const Types::TPoint &FindPosition, void * FindData, int StartIndex, Comctrls::TSearchDirection Direction, bool Wrap) = 0 ;
	virtual bool __fastcall OwnerDataFetchW(Comctrls::TListItem* Item, Comctrls::TItemRequest Request) = 0 ;
	
protected:
	virtual int __fastcall OwnerDataFind(Comctrls::TItemFind Find, const AnsiString FindString, const Types::TPoint &FindPosition, void * FindData, int StartIndex, Comctrls::TSearchDirection Direction, bool Wrap);
	virtual bool __fastcall OwnerDataFetch(Comctrls::TListItem* Item, Comctrls::TItemRequest Request);
public:
	#pragma option push -w-inl
	/* TCustomListView.Create */ inline __fastcall virtual _TntInternalCustomListView(Classes::TComponent* AOwner) : Comctrls::TCustomListView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomListView.Destroy */ inline __fastcall virtual ~_TntInternalCustomListView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall _TntInternalCustomListView(HWND ParentWindow) : Comctrls::TCustomListView(ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntCustomListView : public _TntInternalCustomListView 
{
	typedef _TntInternalCustomListView inherited;
	
private:
	unsigned FEditHandle;
	void *FEditInstance;
	void *FDefEditProc;
	TTntLVEditedEvent FOnEdited;
	TTntLVOwnerDataFindEvent FOnDataFind;
	void __fastcall EditWndProcW(Messages::TMessage &Message);
	void __fastcall BeginChangingWideItem(void);
	void __fastcall EndChangingWideItem(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	TTntListColumns* __fastcall GetListColumns(void);
	HIDESBASE void __fastcall SetListColumns(const TTntListColumns* Value);
	TTntListColumn* __fastcall ColumnFromIndex(int Index);
	HIDESBASE TTntListColumn* __fastcall GetColumnFromTag(int Tag);
	virtual int __fastcall OwnerDataFindW(Comctrls::TItemFind Find, const WideString FindString, const Types::TPoint &FindPosition, void * FindData, int StartIndex, Comctrls::TSearchDirection Direction, bool Wrap);
	virtual bool __fastcall OwnerDataFetchW(Comctrls::TListItem* Item, Comctrls::TItemRequest Request);
	HIDESBASE TTntListItem* __fastcall GetDropTarget(void);
	HIDESBASE void __fastcall SetDropTarget(const TTntListItem* Value);
	TTntListItem* __fastcall GetItemFocused(void);
	void __fastcall SetItemFocused(const TTntListItem* Value);
	HIDESBASE TTntListItem* __fastcall GetSelected(void);
	HIDESBASE void __fastcall SetSelected(const TTntListItem* Value);
	HIDESBASE TTntListItem* __fastcall GetTopItem(void);
	Contnrs::TObjectList* FSavedItems;
	bool FTestingForSortProc;
	int FChangingWideItemCount;
	TTntListItem* FTempItem;
	HIDESBASE bool __fastcall AreItemsStored(void);
	TTntListItems* __fastcall GetItems(void);
	HIDESBASE void __fastcall SetItems(TTntListItems* Value);
	HIDESBASE MESSAGE void __fastcall CNNotify(Messages::TWMNotify &Message);
	TTntListItem* __fastcall GetItemW(const tagLVITEMW &Value);
	HIDESBASE MESSAGE void __fastcall WMNotify(Messages::TWMNotify &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	HIDESBASE virtual bool __fastcall OwnerDataFetch(Comctrls::TListItem* Item, Comctrls::TItemRequest Request);
	virtual Comctrls::TListItem* __fastcall CreateListItem(void);
	virtual Comctrls::TListItems* __fastcall CreateListItems(void);
	__property TTntListItems* Items = {read=GetItems, write=SetItems, stored=AreItemsStored};
	DYNAMIC void __fastcall Edit(const tagLVITEMA &Item);
	HIDESBASE virtual int __fastcall OwnerDataFind(Comctrls::TItemFind Find, const WideString FindString, const Types::TPoint &FindPosition, void * FindData, int StartIndex, Comctrls::TSearchDirection Direction, bool Wrap);
	__property TTntListColumns* Columns = {read=GetListColumns, write=SetListColumns};
	virtual void __fastcall DrawItem(Comctrls::TListItem* Item, const Types::TRect &Rect, Windows::TOwnerDrawState State);
	__property TTntLVEditedEvent OnEdited = {read=FOnEdited, write=FOnEdited};
	__property TTntLVOwnerDataFindEvent OnDataFind = {read=FOnDataFind, write=FOnDataFind};
	
public:
	__fastcall virtual TTntCustomListView(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomListView(void);
	__property TTntListColumn* Column[int Index] = {read=ColumnFromIndex};
	virtual void __fastcall CopySelection(Controls::TCustomListControl* Destination);
	HIDESBASE virtual void __fastcall AddItem(const WideString Item, System::TObject* AObject);
	HIDESBASE TTntListItem* __fastcall FindCaption(int StartIndex, WideString Value, bool Partial, bool Inclusive, bool Wrap);
	HIDESBASE WideString __fastcall GetSearchString();
	HIDESBASE int __fastcall StringWidth(WideString S);
	__property TTntListItem* DropTarget = {read=GetDropTarget, write=SetDropTarget};
	__property TTntListItem* ItemFocused = {read=GetItemFocused, write=SetItemFocused};
	__property TTntListItem* Selected = {read=GetSelected, write=SetSelected};
	__property TTntListItem* TopItem = {read=GetTopItem};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomListView(HWND ParentWindow) : _TntInternalCustomListView(ParentWindow) { }
	#pragma option pop
	
private:
	void *__IWideCustomListControl;	/* Tntcontrols::IWideCustomListControl */
	
public:
	operator IWideCustomListControl*(void) { return (IWideCustomListControl*)&__IWideCustomListControl; }
	
};


class DELPHICLASS TTntListView;
class PASCALIMPLEMENTATION TTntListView : public TTntCustomListView 
{
	typedef TTntCustomListView inherited;
	
__published:
	__property Action ;
	__property Align  = {default=0};
	__property AllocBy  = {default=0};
	__property Anchors  = {default=3};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelOuter  = {index=1, default=1};
	__property BevelKind  = {default=0};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property Checkboxes  = {default=0};
	__property Color  = {default=-16777211};
	__property Columns ;
	__property ColumnClick  = {default=1};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property FlatScrollBars  = {default=0};
	__property FullDrag  = {default=0};
	__property GridLines  = {default=0};
	__property HideSelection  = {default=1};
	__property HotTrack  = {default=0};
	__property HotTrackStyles  = {default=0};
	__property HoverTime  = {default=-1};
	__property IconOptions ;
	__property Items ;
	__property LargeImages ;
	__property MultiSelect  = {default=0};
	__property OwnerData  = {default=0};
	__property OwnerDraw  = {default=0};
	__property ReadOnly  = {default=0};
	__property RowSelect  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowColumnHeaders  = {default=1};
	__property ShowWorkAreas  = {default=0};
	__property ShowHint ;
	__property SmallImages ;
	__property SortType  = {default=0};
	__property StateImages ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property ViewStyle  = {default=0};
	__property Visible  = {default=1};
	__property OnAdvancedCustomDraw ;
	__property OnAdvancedCustomDrawItem ;
	__property OnAdvancedCustomDrawSubItem ;
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnColumnClick ;
	__property OnColumnDragged ;
	__property OnColumnRightClick ;
	__property OnCompare ;
	__property OnContextPopup ;
	__property OnCustomDraw ;
	__property OnCustomDrawItem ;
	__property OnCustomDrawSubItem ;
	__property OnData ;
	__property OnDataFind ;
	__property OnDataHint ;
	__property OnDataStateChange ;
	__property OnDblClick ;
	__property OnDeletion ;
	__property OnDrawItem ;
	__property OnEdited ;
	__property OnEditing ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnGetSubItemImage ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnInfoTip ;
	__property OnInsert ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnResize ;
	__property OnSelectItem ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomListView.Create */ inline __fastcall virtual TTntListView(Classes::TComponent* AOwner) : TTntCustomListView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomListView.Destroy */ inline __fastcall virtual ~TTntListView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntListView(HWND ParentWindow) : TTntCustomListView(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntToolButton;
class PASCALIMPLEMENTATION TTntToolButton : public Comctrls::TToolButton 
{
	typedef Comctrls::TToolButton inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Messages::TMessage &Message);
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	Menus::TMenuItem* __fastcall GetMenuItem(void);
	HIDESBASE void __fastcall SetMenuItem(const Menus::TMenuItem* Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
	__property Menus::TMenuItem* MenuItem = {read=GetMenuItem, write=SetMenuItem};
public:
	#pragma option push -w-inl
	/* TToolButton.Create */ inline __fastcall virtual TTntToolButton(Classes::TComponent* AOwner) : Comctrls::TToolButton(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TTntToolButton(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntToolBar;
class PASCALIMPLEMENTATION TTntToolBar : public Comctrls::TToolBar 
{
	typedef Comctrls::TToolBar inherited;
	
private:
	WideString FCaption;
	MESSAGE void __fastcall TBInsertButtonA(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMGetText(Messages::TWMGetText &Message);
	HIDESBASE MESSAGE void __fastcall WMGetTextLength(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMSetText(Messages::TWMSetText &Message);
	Menus::TMainMenu* __fastcall GetMenu(void);
	HIDESBASE void __fastcall SetMenu(const Menus::TMainMenu* Value);
	WideString __fastcall GetCaption();
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetCaption(const WideString Value);
	void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
	__property Menus::TMainMenu* Menu = {read=GetMenu, write=SetMenu};
public:
	#pragma option push -w-inl
	/* TToolBar.Create */ inline __fastcall virtual TTntToolBar(Classes::TComponent* AOwner) : Comctrls::TToolBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TToolBar.Destroy */ inline __fastcall virtual ~TTntToolBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntToolBar(HWND ParentWindow) : Comctrls::TToolBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomRichEdit;
class PASCALIMPLEMENTATION TTntCustomRichEdit : public Comctrls::TCustomRichEdit 
{
	typedef Comctrls::TCustomRichEdit inherited;
	
private:
	Tntclasses::TTntStrings* FRichEditStrings;
	int FPrintingTextLength;
	MESSAGE void __fastcall WMGetTextLength(Messages::TWMNoParams &Message);
	HIDESBASE void __fastcall SetRichEditStrings(const Tntclasses::TTntStrings* Value);
	WideString __fastcall GetWideSelText();
	HIDESBASE WideString __fastcall GetText();
	void __fastcall SetWideSelText(const WideString Value);
	HIDESBASE void __fastcall SetText(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetHint(const WideString Value);
	void __fastcall SetRTFText(unsigned Flags, const AnsiString Value);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual AnsiString __fastcall GetSelText();
	int __fastcall CharPosToGet(int RawWin32CharPos);
	int __fastcall CharPosToSet(int EmulatedCharPos);
	HIDESBASE virtual int __fastcall GetSelStart(void);
	HIDESBASE virtual void __fastcall SetSelStart(const int Value);
	HIDESBASE virtual int __fastcall GetSelLength(void);
	HIDESBASE virtual void __fastcall SetSelLength(const int Value);
	Tntsysutils::TTntTextLineBreakStyle __fastcall LineBreakStyle(void);
	__property Tntclasses::TTntStrings* Lines = {read=FRichEditStrings, write=SetRichEditStrings};
	
public:
	__fastcall virtual TTntCustomRichEdit(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomRichEdit(void);
	int __fastcall EmulatedCharPos(int RawWin32CharPos);
	int __fastcall RawWin32CharPos(int EmulatedCharPos);
	virtual void __fastcall Print(const AnsiString Caption);
	__property WideString SelText = {read=GetWideSelText, write=SetWideSelText};
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelLength = {read=GetSelLength, write=SetSelLength, nodefault};
	__property WideString Text = {read=GetText, write=SetText};
	HIDESBASE int __fastcall FindText(const WideString SearchStr, int StartPos, int Length, Comctrls::TSearchTypes Options);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomRichEdit(HWND ParentWindow) : Comctrls::TCustomRichEdit(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntRichEdit;
class PASCALIMPLEMENTATION TTntRichEdit : public TTntCustomRichEdit 
{
	typedef TTntCustomRichEdit inherited;
	
__published:
	__property Align  = {default=0};
	__property Alignment  = {default=0};
	__property Anchors  = {default=3};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelOuter  = {index=1, default=1};
	__property BevelKind  = {default=0};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property Color  = {default=-16777211};
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property HideSelection  = {default=1};
	__property HideScrollBars  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property Constraints ;
	__property Lines ;
	__property MaxLength  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PlainText  = {default=0};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ScrollBars  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property WantTabs  = {default=0};
	__property WantReturns  = {default=1};
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
	__property OnMouseWheel ;
	__property OnMouseWheelDown ;
	__property OnMouseWheelUp ;
	__property OnProtectChange ;
	__property OnResizeRequest ;
	__property OnSaveClipboard ;
	__property OnSelectionChange ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomRichEdit.Create */ inline __fastcall virtual TTntRichEdit(Classes::TComponent* AOwner) : TTntCustomRichEdit(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomRichEdit.Destroy */ inline __fastcall virtual ~TTntRichEdit(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntRichEdit(HWND ParentWindow) : TTntCustomRichEdit(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomTabControl;
class PASCALIMPLEMENTATION TTntCustomTabControl : public Comctrls::TCustomTabControl 
{
	typedef Comctrls::TCustomTabControl inherited;
	
private:
	Tntclasses::TTntStrings* FTabs;
	int FSaveTabIndex;
	Tntclasses::TTntStrings* FSaveTabs;
	HIDESBASE Tntclasses::TTntStrings* __fastcall GetTabs(void);
	HIDESBASE void __fastcall SetTabs(const Tntclasses::TTntStrings* Value);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	__property Tntclasses::TTntStrings* Tabs = {read=GetTabs, write=SetTabs};
	
public:
	__fastcall virtual TTntCustomTabControl(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomTabControl(void);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomTabControl(HWND ParentWindow) : Comctrls::TCustomTabControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntTabControl;
class PASCALIMPLEMENTATION TTntTabControl : public TTntCustomTabControl 
{
	typedef TTntCustomTabControl inherited;
	
public:
	__property DisplayRect ;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property DockSite  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property HotTrack  = {default=0};
	__property Images ;
	__property MultiLine  = {default=0};
	__property MultiSelect  = {default=0};
	__property OwnerDraw  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property RaggedRight  = {default=0};
	__property ScrollOpposite  = {default=0};
	__property ShowHint ;
	__property Style  = {default=0};
	__property TabHeight  = {default=0};
	__property TabOrder  = {default=-1};
	__property TabPosition  = {default=0};
	__property Tabs ;
	__property TabIndex  = {default=-1};
	__property TabStop  = {default=1};
	__property TabWidth  = {default=0};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnChanging ;
	__property OnContextPopup ;
	__property OnDockDrop ;
	__property OnDockOver ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawTab ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnGetSiteInfo ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnUnDock ;
public:
	#pragma option push -w-inl
	/* TTntCustomTabControl.Create */ inline __fastcall virtual TTntTabControl(Classes::TComponent* AOwner) : TTntCustomTabControl(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomTabControl.Destroy */ inline __fastcall virtual ~TTntTabControl(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntTabControl(HWND ParentWindow) : TTntCustomTabControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntTabSheet;
class PASCALIMPLEMENTATION TTntTabSheet : public Comctrls::TTabSheet 
{
	typedef Comctrls::TTabSheet inherited;
	
private:
	bool Force_Inherited_WMSETTEXT;
	HIDESBASE bool __fastcall IsCaptionStored(void);
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	MESSAGE void __fastcall WMSetText(Messages::TWMSetText &Message);
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetHint(const WideString Value);
	
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
	/* TTabSheet.Create */ inline __fastcall virtual TTntTabSheet(Classes::TComponent* AOwner) : Comctrls::TTabSheet(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTabSheet.Destroy */ inline __fastcall virtual ~TTntTabSheet(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntTabSheet(HWND ParentWindow) : Comctrls::TTabSheet(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPageControl;
class PASCALIMPLEMENTATION TTntPageControl : public Comctrls::TPageControl 
{
	typedef Comctrls::TPageControl inherited;
	
private:
	TTntTabSheet* FNewDockSheet;
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMDockNotification(Controls::TCMDockNotification &Message);
	HIDESBASE MESSAGE void __fastcall CMDockClient(Controls::TCMDockClient &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	DYNAMIC void __fastcall DoAddDockClient(Controls::TControl* Client, const Types::TRect &ARect);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TPageControl.Create */ inline __fastcall virtual TTntPageControl(Classes::TComponent* AOwner) : Comctrls::TPageControl(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPageControl.Destroy */ inline __fastcall virtual ~TTntPageControl(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntPageControl(HWND ParentWindow) : Comctrls::TPageControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntTrackBar;
class PASCALIMPLEMENTATION TTntTrackBar : public Comctrls::TTrackBar 
{
	typedef Comctrls::TTrackBar inherited;
	
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
	/* TTrackBar.Create */ inline __fastcall virtual TTntTrackBar(Classes::TComponent* AOwner) : Comctrls::TTrackBar(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntTrackBar(HWND ParentWindow) : Comctrls::TTrackBar(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntTrackBar(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntProgressBar;
class PASCALIMPLEMENTATION TTntProgressBar : public Comctrls::TProgressBar 
{
	typedef Comctrls::TProgressBar inherited;
	
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
	/* TProgressBar.Create */ inline __fastcall virtual TTntProgressBar(Classes::TComponent* AOwner) : Comctrls::TProgressBar(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntProgressBar(HWND ParentWindow) : Comctrls::TProgressBar(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntProgressBar(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomUpDown;
class PASCALIMPLEMENTATION TTntCustomUpDown : public Comctrls::TCustomUpDown 
{
	typedef Comctrls::TCustomUpDown inherited;
	
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
	/* TCustomUpDown.Create */ inline __fastcall virtual TTntCustomUpDown(Classes::TComponent* AOwner) : Comctrls::TCustomUpDown(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomUpDown(HWND ParentWindow) : Comctrls::TCustomUpDown(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntCustomUpDown(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntUpDown;
class PASCALIMPLEMENTATION TTntUpDown : public TTntCustomUpDown 
{
	typedef TTntCustomUpDown inherited;
	
__published:
	__property AlignButton  = {default=1};
	__property Anchors  = {default=3};
	__property Associate ;
	__property ArrowKeys  = {default=1};
	__property Enabled  = {default=1};
	__property Hint ;
	__property Min  = {default=0};
	__property Max  = {default=100};
	__property Increment  = {default=1};
	__property Constraints ;
	__property Orientation  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property Position  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property Thousands  = {default=1};
	__property Visible  = {default=1};
	__property Wrap  = {default=0};
	__property OnChanging ;
	__property OnChangingEx ;
	__property OnContextPopup ;
	__property OnClick ;
	__property OnEnter ;
	__property OnExit ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
public:
	#pragma option push -w-inl
	/* TCustomUpDown.Create */ inline __fastcall virtual TTntUpDown(Classes::TComponent* AOwner) : TTntCustomUpDown(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntUpDown(HWND ParentWindow) : TTntCustomUpDown(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntUpDown(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntDateTimePicker;
class PASCALIMPLEMENTATION TTntDateTimePicker : public Comctrls::TDateTimePicker 
{
	typedef Comctrls::TDateTimePicker inherited;
	
private:
	bool FHadFirstMouseClick;
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &Message);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TDateTimePicker.Create */ inline __fastcall virtual TTntDateTimePicker(Classes::TComponent* AOwner) : Comctrls::TDateTimePicker(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCommonCalendar.Destroy */ inline __fastcall virtual ~TTntDateTimePicker(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntDateTimePicker(HWND ParentWindow) : Comctrls::TDateTimePicker(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntMonthCalendar;
class PASCALIMPLEMENTATION TTntMonthCalendar : public Comctrls::TMonthCalendar 
{
	typedef Comctrls::TMonthCalendar inherited;
	
private:
	HIDESBASE bool __fastcall IsHintStored(void);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE TDate __fastcall GetDate(void);
	HIDESBASE void __fastcall SetDate(const TDate Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
public:
	void __fastcall ForceGetMonthInfo(void);
	
__published:
	__property TDate Date = {read=GetDate, write=SetDate};
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TMonthCalendar.Create */ inline __fastcall virtual TTntMonthCalendar(Classes::TComponent* AOwner) : Comctrls::TMonthCalendar(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCommonCalendar.Destroy */ inline __fastcall virtual ~TTntMonthCalendar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntMonthCalendar(HWND ParentWindow) : Comctrls::TMonthCalendar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPageScroller;
class PASCALIMPLEMENTATION TTntPageScroller : public Comctrls::TPageScroller 
{
	typedef Comctrls::TPageScroller inherited;
	
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
	/* TPageScroller.Create */ inline __fastcall virtual TTntPageScroller(Classes::TComponent* AOwner) : Comctrls::TPageScroller(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntPageScroller(HWND ParentWindow) : Comctrls::TPageScroller(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntPageScroller(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStatusPanel;
class PASCALIMPLEMENTATION TTntStatusPanel : public Comctrls::TStatusPanel 
{
	typedef Comctrls::TStatusPanel inherited;
	
private:
	WideString FText;
	WideString __fastcall GetText();
	HIDESBASE void __fastcall SetText(const WideString Value);
	void __fastcall SetInheritedText(const AnsiString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Text = {read=GetText, write=SetText};
public:
	#pragma option push -w-inl
	/* TStatusPanel.Create */ inline __fastcall virtual TTntStatusPanel(Classes::TCollection* Collection) : Comctrls::TStatusPanel(Collection) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TTntStatusPanel(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStatusPanels;
class PASCALIMPLEMENTATION TTntStatusPanels : public Comctrls::TStatusPanels 
{
	typedef Comctrls::TStatusPanels inherited;
	
public:
	TTntStatusPanel* operator[](int Index) { return Items[Index]; }
	
private:
	HIDESBASE TTntStatusPanel* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TTntStatusPanel* Value);
	
public:
	HIDESBASE TTntStatusPanel* __fastcall Add(void);
	HIDESBASE TTntStatusPanel* __fastcall AddItem(TTntStatusPanel* Item, int Index);
	HIDESBASE TTntStatusPanel* __fastcall Insert(int Index);
	__property TTntStatusPanel* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	#pragma option push -w-inl
	/* TStatusPanels.Create */ inline __fastcall TTntStatusPanels(Comctrls::TCustomStatusBar* StatusBar) : Comctrls::TStatusPanels(StatusBar) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TTntStatusPanels(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomStatusBar;
class PASCALIMPLEMENTATION TTntCustomStatusBar : public Comctrls::TCustomStatusBar 
{
	typedef Comctrls::TCustomStatusBar inherited;
	
private:
	WideString FSimpleText;
	WideString __fastcall GetSimpleText();
	HIDESBASE void __fastcall SetSimpleText(const WideString Value);
	void __fastcall SetInheritedSimpleText(const AnsiString Value);
	WideString __fastcall SyncLeadingTabs(const WideString WideVal, const AnsiString AnsiVal);
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetHint(const WideString Value);
	HIDESBASE MESSAGE void __fastcall WMGetTextLength(Messages::TWMNoParams &Message);
	TTntStatusPanels* __fastcall GetPanels(void);
	HIDESBASE void __fastcall SetPanels(const TTntStatusPanels* Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual Comctrls::TStatusPanels* __fastcall CreatePanels(void);
	virtual TMetaClass* __fastcall GetPanelClass(void);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall WndProc(Messages::TMessage &Msg);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
public:
	DYNAMIC bool __fastcall ExecuteAction(Classes::TBasicAction* Action);
	__property TTntStatusPanels* Panels = {read=GetPanels, write=SetPanels};
	__property WideString SimpleText = {read=GetSimpleText, write=SetSimpleText};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomStatusBar.Create */ inline __fastcall virtual TTntCustomStatusBar(Classes::TComponent* AOwner) : Comctrls::TCustomStatusBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomStatusBar.Destroy */ inline __fastcall virtual ~TTntCustomStatusBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomStatusBar(HWND ParentWindow) : Comctrls::TCustomStatusBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStatusBar;
class PASCALIMPLEMENTATION TTntStatusBar : public TTntCustomStatusBar 
{
	typedef TTntCustomStatusBar inherited;
	
private:
	Comctrls::TDrawPanelEvent __fastcall GetOnDrawPanel();
	void __fastcall SetOnDrawPanel(const Comctrls::TDrawPanelEvent Value);
	
__published:
	__property Action ;
	__property AutoHint  = {default=0};
	__property Align  = {default=2};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property BorderWidth  = {default=0};
	__property Color  = {default=-16777201};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font  = {stored=IsFontStored};
	__property Constraints ;
	__property Panels ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentFont  = {default=0};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property SimplePanel  = {default=0};
	__property SimpleText ;
	__property SizeGrip  = {default=1};
	__property UseSystemFont  = {default=1};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnCreatePanelClass ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnHint ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property Comctrls::TDrawPanelEvent OnDrawPanel = {read=GetOnDrawPanel, write=SetOnDrawPanel};
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomStatusBar.Create */ inline __fastcall virtual TTntStatusBar(Classes::TComponent* AOwner) : TTntCustomStatusBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomStatusBar.Destroy */ inline __fastcall virtual ~TTntStatusBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntStatusBar(HWND ParentWindow) : TTntCustomStatusBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntTreeNode;
class DELPHICLASS TTntTreeNodes;
class DELPHICLASS TTntCustomTreeView;
class PASCALIMPLEMENTATION TTntTreeNode : public Comctrls::TTreeNode 
{
	typedef Comctrls::TTreeNode inherited;
	
public:
	TTntTreeNode* operator[](int Index) { return Item[Index]; }
	
private:
	WideString FText;
	HIDESBASE void __fastcall SetText(const WideString Value);
	void __fastcall SetInheritedText(const AnsiString Value);
	WideString __fastcall GetText();
	HIDESBASE TTntTreeNode* __fastcall GetItem(int Index);
	TTntTreeNodes* __fastcall GetNodeOwner(void);
	HIDESBASE TTntTreeNode* __fastcall GetParent(void);
	HIDESBASE TTntCustomTreeView* __fastcall GetTreeView(void);
	HIDESBASE void __fastcall SetItem(int Index, const TTntTreeNode* Value);
	HIDESBASE bool __fastcall IsEqual(TTntTreeNode* Node);
	HIDESBASE void __fastcall ReadData(Classes::TStream* Stream, Comctrls::PNodeInfo Info);
	void __fastcall WriteData(Classes::TStream* Stream, Comctrls::PNodeInfo Info);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	HIDESBASE TTntTreeNode* __fastcall getFirstChild(void);
	HIDESBASE TTntTreeNode* __fastcall GetLastChild(void);
	HIDESBASE TTntTreeNode* __fastcall GetNext(void);
	HIDESBASE TTntTreeNode* __fastcall GetNextChild(TTntTreeNode* Value);
	HIDESBASE TTntTreeNode* __fastcall getNextSibling(void);
	HIDESBASE TTntTreeNode* __fastcall GetNextVisible(void);
	HIDESBASE TTntTreeNode* __fastcall GetPrev(void);
	HIDESBASE TTntTreeNode* __fastcall GetPrevChild(TTntTreeNode* Value);
	HIDESBASE TTntTreeNode* __fastcall getPrevSibling(void);
	HIDESBASE TTntTreeNode* __fastcall GetPrevVisible(void);
	__property TTntTreeNode* Item[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TTntTreeNodes* Owner = {read=GetNodeOwner};
	__property TTntTreeNode* Parent = {read=GetParent};
	__property WideString Text = {read=GetText, write=SetText};
	__property TTntCustomTreeView* TreeView = {read=GetTreeView};
public:
	#pragma option push -w-inl
	/* TTreeNode.Create */ inline __fastcall TTntTreeNode(Comctrls::TTreeNodes* AOwner) : Comctrls::TTreeNode(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTreeNode.Destroy */ inline __fastcall virtual ~TTntTreeNode(void) { }
	#pragma option pop
	
};


typedef TMetaClass* TTntTreeNodeClass;

class DELPHICLASS TTntTreeNodesEnumerator;
class PASCALIMPLEMENTATION TTntTreeNodesEnumerator : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FIndex;
	TTntTreeNodes* FTreeNodes;
	
public:
	__fastcall TTntTreeNodesEnumerator(TTntTreeNodes* ATreeNodes);
	TTntTreeNode* __fastcall GetCurrent(void);
	bool __fastcall MoveNext(void);
	__property TTntTreeNode* Current = {read=GetCurrent};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTntTreeNodesEnumerator(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntTreeNodes : public Comctrls::TTreeNodes 
{
	typedef Comctrls::TTreeNodes inherited;
	
public:
	TTntTreeNode* operator[](int Index) { return Item[Index]; }
	
private:
	HIDESBASE TTntTreeNode* __fastcall GetNodeFromIndex(int Index);
	TTntCustomTreeView* __fastcall GetNodesOwner(void);
	HIDESBASE void __fastcall ClearCache(void);
	HIDESBASE void __fastcall ReadData(Classes::TStream* Stream);
	void __fastcall WriteData(Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	HIDESBASE TTntTreeNode* __fastcall Add(TTntTreeNode* Sibling, const WideString S);
	HIDESBASE TTntTreeNode* __fastcall AddChild(TTntTreeNode* Parent, const WideString S);
	HIDESBASE TTntTreeNode* __fastcall AddChildFirst(TTntTreeNode* Parent, const WideString S);
	HIDESBASE TTntTreeNode* __fastcall AddChildObject(TTntTreeNode* Parent, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall AddChildObjectFirst(TTntTreeNode* Parent, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall AddFirst(TTntTreeNode* Sibling, const WideString S);
	HIDESBASE TTntTreeNode* __fastcall AddObject(TTntTreeNode* Sibling, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall AddObjectFirst(TTntTreeNode* Sibling, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall Insert(TTntTreeNode* Sibling, const WideString S);
	HIDESBASE TTntTreeNode* __fastcall InsertObject(TTntTreeNode* Sibling, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall InsertNode(TTntTreeNode* Node, TTntTreeNode* Sibling, const WideString S, void * Ptr);
	HIDESBASE TTntTreeNode* __fastcall AddNode(TTntTreeNode* Node, TTntTreeNode* Relative, const WideString S, void * Ptr, Comctrls::TNodeAttachMode Method);
	HIDESBASE TTntTreeNode* __fastcall GetFirstNode(void);
	HIDESBASE TTntTreeNodesEnumerator* __fastcall GetEnumerator(void);
	HIDESBASE TTntTreeNode* __fastcall GetNode(HTREEITEM ItemId);
	__property TTntTreeNode* Item[int Index] = {read=GetNodeFromIndex/*, default*/};
	__property TTntCustomTreeView* Owner = {read=GetNodesOwner};
public:
	#pragma option push -w-inl
	/* TTreeNodes.Create */ inline __fastcall TTntTreeNodes(Comctrls::TCustomTreeView* AOwner) : Comctrls::TTreeNodes(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTreeNodes.Destroy */ inline __fastcall virtual ~TTntTreeNodes(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TTntTVEditedEvent)(System::TObject* Sender, TTntTreeNode* Node, WideString &S);

class DELPHICLASS _TntInternalCustomTreeView;
class PASCALIMPLEMENTATION _TntInternalCustomTreeView : public Comctrls::TCustomTreeView 
{
	typedef Comctrls::TCustomTreeView inherited;
	
private:
	virtual TTntTreeNode* __fastcall Wide_FindNextToSelect(void) = 0 ;
	Comctrls::TTreeNode* __fastcall Inherited_FindNextToSelect(void);
	
public:
	virtual Comctrls::TTreeNode* __fastcall FindNextToSelect(void);
public:
	#pragma option push -w-inl
	/* TCustomTreeView.Create */ inline __fastcall virtual _TntInternalCustomTreeView(Classes::TComponent* AOwner) : Comctrls::TCustomTreeView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomTreeView.Destroy */ inline __fastcall virtual ~_TntInternalCustomTreeView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall _TntInternalCustomTreeView(HWND ParentWindow) : Comctrls::TCustomTreeView(ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntCustomTreeView : public _TntInternalCustomTreeView 
{
	typedef _TntInternalCustomTreeView inherited;
	
private:
	Tntclasses::TTntStrings* FSavedNodeText;
	Comctrls::TSortType FSavedSortType;
	TTntTVEditedEvent FOnEdited;
	bool FTestingForSortProc;
	unsigned FEditHandle;
	void *FEditInstance;
	void *FDefEditProc;
	TTntTreeNodes* __fastcall GetTreeNodes(void);
	HIDESBASE void __fastcall SetTreeNodes(const TTntTreeNodes* Value);
	HIDESBASE MESSAGE void __fastcall CNNotify(Messages::TWMNotify &Message);
	HIDESBASE MESSAGE void __fastcall WMNotify(Messages::TWMNotify &Message);
	HIDESBASE TTntTreeNode* __fastcall GetNodeFromItem(const tagTVITEMA &Item);
	void __fastcall EditWndProcW(Messages::TMessage &Message);
	virtual TTntTreeNode* __fastcall Wide_FindNextToSelect(void);
	HIDESBASE TTntTreeNode* __fastcall GetDropTarget(void);
	HIDESBASE TTntTreeNode* __fastcall GetSelected(void);
	HIDESBASE TTntTreeNode* __fastcall GetSelection(int Index);
	HIDESBASE TTntTreeNode* __fastcall GetTopItem(void);
	HIDESBASE void __fastcall SetDropTarget(const TTntTreeNode* Value);
	HIDESBASE void __fastcall SetSelected(const TTntTreeNode* Value);
	HIDESBASE void __fastcall SetTopItem(const TTntTreeNode* Value);
	WideString __fastcall GetHint();
	HIDESBASE bool __fastcall IsHintStored(void);
	void __fastcall SetHint(const WideString Value);
	
protected:
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	DYNAMIC void __fastcall Edit(const tagTVITEMA &Item);
	virtual Comctrls::TTreeNode* __fastcall CreateNode(void);
	virtual Comctrls::TTreeNodes* __fastcall CreateNodes(void);
	__property TTntTreeNodes* Items = {read=GetTreeNodes, write=SetTreeNodes};
	__property TTntTVEditedEvent OnEdited = {read=FOnEdited, write=FOnEdited};
	
public:
	__fastcall virtual TTntCustomTreeView(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomTreeView(void);
	HIDESBASE void __fastcall LoadFromFile(const WideString FileName);
	HIDESBASE void __fastcall LoadFromStream(Classes::TStream* Stream);
	HIDESBASE void __fastcall SaveToFile(const WideString FileName);
	HIDESBASE void __fastcall SaveToStream(Classes::TStream* Stream);
	HIDESBASE TTntTreeNode* __fastcall GetNodeAt(int X, int Y);
	__property TTntTreeNode* DropTarget = {read=GetDropTarget, write=SetDropTarget};
	__property TTntTreeNode* Selected = {read=GetSelected, write=SetSelected};
	__property TTntTreeNode* TopItem = {read=GetTopItem, write=SetTopItem};
	__property TTntTreeNode* Selections[int Index] = {read=GetSelection};
	HIDESBASE TTntTreeNode* __fastcall GetSelections(Classes::TList* AList);
	HIDESBASE virtual TTntTreeNode* __fastcall FindNextToSelect(void);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomTreeView(HWND ParentWindow) : _TntInternalCustomTreeView(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntTreeView;
class PASCALIMPLEMENTATION TTntTreeView : public TTntCustomTreeView 
{
	typedef TTntCustomTreeView inherited;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoExpand  = {default=0};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelOuter  = {index=1, default=1};
	__property BevelKind  = {default=0};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property ChangeDelay  = {default=0};
	__property Color  = {default=-16777211};
	__property Ctl3D ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property HideSelection  = {default=1};
	__property HotTrack  = {default=0};
	__property Images ;
	__property Indent ;
	__property MultiSelect  = {default=0};
	__property MultiSelectStyle  = {default=1};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property RightClickSelect  = {default=0};
	__property RowSelect  = {default=0};
	__property ShowButtons  = {default=1};
	__property ShowHint ;
	__property ShowLines  = {default=1};
	__property ShowRoot  = {default=1};
	__property SortType  = {default=0};
	__property StateImages ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property ToolTips  = {default=1};
	__property Visible  = {default=1};
	__property OnAddition ;
	__property OnAdvancedCustomDraw ;
	__property OnAdvancedCustomDrawItem ;
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnCollapsed ;
	__property OnCollapsing ;
	__property OnCompare ;
	__property OnContextPopup ;
	__property OnCreateNodeClass ;
	__property OnCustomDraw ;
	__property OnCustomDrawItem ;
	__property OnDblClick ;
	__property OnDeletion ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEdited ;
	__property OnEditing ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnExpanding ;
	__property OnExpanded ;
	__property OnGetImageIndex ;
	__property OnGetSelectedIndex ;
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
	__property Items ;
public:
	#pragma option push -w-inl
	/* TTntCustomTreeView.Create */ inline __fastcall virtual TTntTreeView(Classes::TComponent* AOwner) : TTntCustomTreeView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomTreeView.Destroy */ inline __fastcall virtual ~TTntTreeView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntTreeView(HWND ParentWindow) : TTntCustomTreeView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntcomctrls */
using namespace Tntcomctrls;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntcomctrls
