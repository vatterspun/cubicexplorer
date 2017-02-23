// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntextctrls.pas' rev: 10.00

#ifndef TntextctrlsHPP
#define TntextctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Extctrls.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Tntcontrols.hpp>	// Pascal unit
#include <Tntstdctrls.hpp>	// Pascal unit
#include <Tntgraphics.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Graphutil.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntextctrls
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntShape;
class PASCALIMPLEMENTATION TTntShape : public Extctrls::TShape 
{
	typedef Extctrls::TShape inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TShape.Create */ inline __fastcall virtual TTntShape(Classes::TComponent* AOwner) : Extctrls::TShape(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TShape.Destroy */ inline __fastcall virtual ~TTntShape(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPaintBox;
class PASCALIMPLEMENTATION TTntPaintBox : public Extctrls::TPaintBox 
{
	typedef Extctrls::TPaintBox inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TPaintBox.Create */ inline __fastcall virtual TTntPaintBox(Classes::TComponent* AOwner) : Extctrls::TPaintBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TTntPaintBox(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntImage;
class PASCALIMPLEMENTATION TTntImage : public Extctrls::TImage 
{
	typedef Extctrls::TImage inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	Tntgraphics::TTntPicture* __fastcall GetPicture(void);
	HIDESBASE void __fastcall SetPicture(const Tntgraphics::TTntPicture* Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
public:
	__fastcall virtual TTntImage(Classes::TComponent* AOwner);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
	__property Tntgraphics::TTntPicture* Picture = {read=GetPicture, write=SetPicture};
public:
	#pragma option push -w-inl
	/* TImage.Destroy */ inline __fastcall virtual ~TTntImage(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntBevel;
class PASCALIMPLEMENTATION TTntBevel : public Extctrls::TBevel 
{
	typedef Extctrls::TBevel inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TBevel.Create */ inline __fastcall virtual TTntBevel(Classes::TComponent* AOwner) : Extctrls::TBevel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TTntBevel(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomPanel;
class PASCALIMPLEMENTATION TTntCustomPanel : public Extctrls::TCustomPanel 
{
	typedef Extctrls::TCustomPanel inherited;
	
private:
	WideString __fastcall GetCaption();
	void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	HIDESBASE bool __fastcall IsHintStored(void);
	
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
	/* TCustomPanel.Create */ inline __fastcall virtual TTntCustomPanel(Classes::TComponent* AOwner) : Extctrls::TCustomPanel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTntCustomPanel(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomPanel(HWND ParentWindow) : Extctrls::TCustomPanel(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntPanel;
class PASCALIMPLEMENTATION TTntPanel : public TTntCustomPanel 
{
	typedef TTntCustomPanel inherited;
	
public:
	__property DockManager ;
	
__published:
	__property Align  = {default=0};
	__property Alignment  = {default=2};
	__property Anchors  = {default=3};
	__property AutoSize  = {default=0};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {default=0};
	__property BevelKind  = {default=0};
	__property BevelOuter  = {default=2};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderWidth  = {default=0};
	__property BorderStyle  = {default=0};
	__property Caption ;
	__property Color  = {default=-16777201};
	__property Constraints ;
	__property Ctl3D ;
	__property UseDockManager  = {default=1};
	__property DockSite  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property FullRepaint  = {default=1};
	__property Font ;
	__property Locked  = {default=0};
	__property Padding ;
	__property ParentBiDiMode  = {default=1};
	__property ParentBackground  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property VerticalAlignment  = {default=2};
	__property Visible  = {default=1};
	__property OnAlignInsertBefore ;
	__property OnAlignPosition ;
	__property OnCanResize ;
	__property OnClick ;
	__property OnConstrainedResize ;
	__property OnContextPopup ;
	__property OnDockDrop ;
	__property OnDockOver ;
	__property OnDblClick ;
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
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnUnDock ;
public:
	#pragma option push -w-inl
	/* TCustomPanel.Create */ inline __fastcall virtual TTntPanel(Classes::TComponent* AOwner) : TTntCustomPanel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTntPanel(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntPanel(HWND ParentWindow) : TTntCustomPanel(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomControlBar;
class PASCALIMPLEMENTATION TTntCustomControlBar : public Extctrls::TCustomControlBar 
{
	typedef Extctrls::TCustomControlBar inherited;
	
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
	/* TCustomControlBar.Create */ inline __fastcall virtual TTntCustomControlBar(Classes::TComponent* AOwner) : Extctrls::TCustomControlBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomControlBar.Destroy */ inline __fastcall virtual ~TTntCustomControlBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomControlBar(HWND ParentWindow) : Extctrls::TCustomControlBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntControlBar;
class PASCALIMPLEMENTATION TTntControlBar : public TTntCustomControlBar 
{
	typedef TTntCustomControlBar inherited;
	
public:
	__property Canvas ;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoDock  = {default=1};
	__property AutoDrag  = {default=1};
	__property AutoSize  = {default=0};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelOuter  = {index=1, default=1};
	__property BevelKind  = {default=1};
	__property BevelWidth  = {default=1};
	__property BorderWidth  = {default=0};
	__property Color ;
	__property Constraints ;
	__property CornerEdge  = {default=2};
	__property DockSite  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property DrawingStyle  = {default=0};
	__property Enabled  = {default=1};
	__property GradientDirection  = {default=1};
	__property GradientEndColor ;
	__property GradientStartColor  = {default=-16777211};
	__property ParentBackground  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property Picture ;
	__property PopupMenu ;
	__property RowSize  = {default=26};
	__property RowSnap  = {default=1};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property Visible  = {default=1};
	__property OnAlignInsertBefore ;
	__property OnAlignPosition ;
	__property OnBandDrag ;
	__property OnBandInfo ;
	__property OnBandMove ;
	__property OnBandPaint ;
	__property OnBeginBandMove ;
	__property OnEndBandMove ;
	__property OnCanResize ;
	__property OnClick ;
	__property OnConstrainedResize ;
	__property OnContextPopup ;
	__property OnDockDrop ;
	__property OnDockOver ;
	__property OnDblClick ;
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
	__property OnPaint ;
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnUnDock ;
public:
	#pragma option push -w-inl
	/* TCustomControlBar.Create */ inline __fastcall virtual TTntControlBar(Classes::TComponent* AOwner) : TTntCustomControlBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomControlBar.Destroy */ inline __fastcall virtual ~TTntControlBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntControlBar(HWND ParentWindow) : TTntCustomControlBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomRadioGroup;
class PASCALIMPLEMENTATION TTntCustomRadioGroup : public Tntstdctrls::TTntCustomGroupBox 
{
	typedef Tntstdctrls::TTntCustomGroupBox inherited;
	
private:
	Classes::TList* FButtons;
	Tntclasses::TTntStrings* FItems;
	int FItemIndex;
	int FColumns;
	bool FReading;
	bool FUpdating;
	Tntstdctrls::TTntRadioButton* __fastcall GetButtons(int Index);
	void __fastcall ArrangeButtons(void);
	void __fastcall ButtonClick(System::TObject* Sender);
	void __fastcall ItemsChange(System::TObject* Sender);
	void __fastcall SetButtonCount(int Value);
	void __fastcall SetColumns(int Value);
	void __fastcall SetItemIndex(int Value);
	void __fastcall SetItems(Tntclasses::TTntStrings* Value);
	void __fastcall UpdateButtons(void);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	virtual void __fastcall Loaded(void);
	virtual void __fastcall ReadState(Classes::TReader* Reader);
	virtual bool __fastcall CanModify(void);
	DYNAMIC void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	__property int Columns = {read=FColumns, write=SetColumns, default=1};
	__property int ItemIndex = {read=FItemIndex, write=SetItemIndex, default=-1};
	__property Tntclasses::TTntStrings* Items = {read=FItems, write=SetItems};
	
public:
	__fastcall virtual TTntCustomRadioGroup(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntCustomRadioGroup(void);
	DYNAMIC void __fastcall FlipChildren(bool AllLevels);
	__property Tntstdctrls::TTntRadioButton* Buttons[int Index] = {read=GetButtons};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomRadioGroup(HWND ParentWindow) : Tntstdctrls::TTntCustomGroupBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntRadioGroup;
class PASCALIMPLEMENTATION TTntRadioGroup : public TTntCustomRadioGroup 
{
	typedef TTntCustomRadioGroup inherited;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Caption ;
	__property Color  = {default=-16777211};
	__property Columns  = {default=1};
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property ItemIndex  = {default=-1};
	__property Items ;
	__property Constraints ;
	__property ParentBiDiMode  = {default=1};
	__property ParentBackground  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTntCustomRadioGroup.Create */ inline __fastcall virtual TTntRadioGroup(Classes::TComponent* AOwner) : TTntCustomRadioGroup(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntCustomRadioGroup.Destroy */ inline __fastcall virtual ~TTntRadioGroup(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntRadioGroup(HWND ParentWindow) : TTntCustomRadioGroup(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntSplitter;
class PASCALIMPLEMENTATION TTntSplitter : public Extctrls::TSplitter 
{
	typedef Extctrls::TSplitter inherited;
	
private:
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TSplitter.Create */ inline __fastcall virtual TTntSplitter(Classes::TComponent* AOwner) : Extctrls::TSplitter(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TSplitter.Destroy */ inline __fastcall virtual ~TTntSplitter(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntextctrls */
using namespace Tntextctrls;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntextctrls
