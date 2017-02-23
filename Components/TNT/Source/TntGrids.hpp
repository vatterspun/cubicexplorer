// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntgrids.pas' rev: 10.00

#ifndef TntgridsHPP
#define TntgridsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Widestrings.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntgrids
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntInplaceEdit;
class PASCALIMPLEMENTATION TTntInplaceEdit : public Grids::TInplaceEdit 
{
	typedef Grids::TInplaceEdit inherited;
	
private:
	HIDESBASE WideString __fastcall GetText();
	HIDESBASE void __fastcall SetText(const WideString Value);
	
protected:
	virtual void __fastcall UpdateContents(void);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	
public:
	__property WideString Text = {read=GetText, write=SetText};
public:
	#pragma option push -w-inl
	/* TInplaceEdit.Create */ inline __fastcall virtual TTntInplaceEdit(Classes::TComponent* AOwner) : Grids::TInplaceEdit(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntInplaceEdit(HWND ParentWindow) : Grids::TInplaceEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntInplaceEdit(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TTntGetEditEvent)(System::TObject* Sender, int ACol, int ARow, WideString &Value);

typedef void __fastcall (__closure *TTntSetEditEvent)(System::TObject* Sender, int ACol, int ARow, const WideString Value);

class DELPHICLASS _TTntInternalCustomDrawGrid;
class PASCALIMPLEMENTATION _TTntInternalCustomDrawGrid : public Grids::TCustomDrawGrid 
{
	typedef Grids::TCustomDrawGrid inherited;
	
private:
	bool FSettingEditText;
	DYNAMIC void __fastcall InternalSetEditText(int ACol, int ARow, const AnsiString Value) = 0 ;
	
protected:
	DYNAMIC void __fastcall SetEditText(int ACol, int ARow, const AnsiString Value);
public:
	#pragma option push -w-inl
	/* TCustomGrid.Create */ inline __fastcall virtual _TTntInternalCustomDrawGrid(Classes::TComponent* AOwner) : Grids::TCustomDrawGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomGrid.Destroy */ inline __fastcall virtual ~_TTntInternalCustomDrawGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall _TTntInternalCustomDrawGrid(HWND ParentWindow) : Grids::TCustomDrawGrid(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomDrawGrid;
class PASCALIMPLEMENTATION TTntCustomDrawGrid : public _TTntInternalCustomDrawGrid 
{
	typedef _TTntInternalCustomDrawGrid inherited;
	
private:
	TTntGetEditEvent FOnGetEditText;
	TTntSetEditEvent FOnSetEditText;
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Msg);
	
protected:
	virtual Grids::TInplaceEdit* __fastcall CreateEditor(void);
	DYNAMIC void __fastcall InternalSetEditText(int ACol, int ARow, const AnsiString Value);
	HIDESBASE virtual WideString __fastcall GetEditText(int ACol, int ARow);
	HIDESBASE virtual void __fastcall SetEditText(int ACol, int ARow, const WideString Value);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	HIDESBASEDYNAMIC void __fastcall ShowEditorChar(WideChar Ch);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	__property TTntGetEditEvent OnGetEditText = {read=FOnGetEditText, write=FOnGetEditText};
	__property TTntSetEditEvent OnSetEditText = {read=FOnSetEditText, write=FOnSetEditText};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomGrid.Create */ inline __fastcall virtual TTntCustomDrawGrid(Classes::TComponent* AOwner) : _TTntInternalCustomDrawGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomGrid.Destroy */ inline __fastcall virtual ~TTntCustomDrawGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomDrawGrid(HWND ParentWindow) : _TTntInternalCustomDrawGrid(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntDrawGrid;
class PASCALIMPLEMENTATION TTntDrawGrid : public TTntCustomDrawGrid 
{
	typedef TTntCustomDrawGrid inherited;
	
__published:
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
	__property ColCount  = {default=5};
	__property Constraints ;
	__property Ctl3D ;
	__property DefaultColWidth  = {default=64};
	__property DefaultRowHeight  = {default=24};
	__property DefaultDrawing  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property FixedColor  = {default=-16777201};
	__property FixedCols  = {default=1};
	__property RowCount  = {default=5};
	__property FixedRows  = {default=1};
	__property Font ;
	__property GridLineWidth  = {default=1};
	__property Options  = {default=31};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ScrollBars  = {default=3};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property Visible  = {default=1};
	__property VisibleColCount ;
	__property VisibleRowCount ;
	__property OnClick ;
	__property OnColumnMoved ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawCell ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetEditMask ;
	__property OnGetEditText ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseActivate ;
	__property OnMouseDown ;
	__property OnMouseEnter ;
	__property OnMouseLeave ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnMouseWheelDown ;
	__property OnMouseWheelUp ;
	__property OnRowMoved ;
	__property OnSelectCell ;
	__property OnSetEditText ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnTopLeftChanged ;
public:
	#pragma option push -w-inl
	/* TCustomGrid.Create */ inline __fastcall virtual TTntDrawGrid(Classes::TComponent* AOwner) : TTntCustomDrawGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomGrid.Destroy */ inline __fastcall virtual ~TTntDrawGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntDrawGrid(HWND ParentWindow) : TTntCustomDrawGrid(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStringGridStrings;
class DELPHICLASS TTntStringGrid;
class DELPHICLASS _TTntInternalStringGrid;
class PASCALIMPLEMENTATION _TTntInternalStringGrid : public Grids::TStringGrid 
{
	typedef Grids::TStringGrid inherited;
	
private:
	bool FSettingEditText;
	DYNAMIC void __fastcall InternalSetEditText(int ACol, int ARow, const AnsiString Value) = 0 ;
	
protected:
	DYNAMIC void __fastcall SetEditText(int ACol, int ARow, const AnsiString Value);
public:
	#pragma option push -w-inl
	/* TStringGrid.Create */ inline __fastcall virtual _TTntInternalStringGrid(Classes::TComponent* AOwner) : Grids::TStringGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TStringGrid.Destroy */ inline __fastcall virtual ~_TTntInternalStringGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall _TTntInternalStringGrid(HWND ParentWindow) : Grids::TStringGrid(ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntStringGrid : public _TTntInternalStringGrid 
{
	typedef _TTntInternalStringGrid inherited;
	
private:
	Classes::TStringList* FCreatedRowStrings;
	Classes::TStringList* FCreatedColStrings;
	TTntGetEditEvent FOnGetEditText;
	TTntSetEditEvent FOnSetEditText;
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Msg);
	HIDESBASE WideString __fastcall GetCells(int ACol, int ARow);
	HIDESBASE void __fastcall SetCells(int ACol, int ARow, const WideString Value);
	Tntclasses::TTntStrings* __fastcall FindGridStrings(const bool IsCol, const int ListIndex);
	HIDESBASE Tntclasses::TTntStrings* __fastcall GetCols(int Index);
	HIDESBASE Tntclasses::TTntStrings* __fastcall GetRows(int Index);
	HIDESBASE void __fastcall SetCols(int Index, const Tntclasses::TTntStrings* Value);
	HIDESBASE void __fastcall SetRows(int Index, const Tntclasses::TTntStrings* Value);
	
protected:
	virtual Grids::TInplaceEdit* __fastcall CreateEditor(void);
	virtual void __fastcall DrawCell(int ACol, int ARow, const Types::TRect &ARect, Grids::TGridDrawState AState);
	DYNAMIC void __fastcall InternalSetEditText(int ACol, int ARow, const AnsiString Value);
	HIDESBASE virtual WideString __fastcall GetEditText(int ACol, int ARow);
	HIDESBASE virtual void __fastcall SetEditText(int ACol, int ARow, const WideString Value);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	HIDESBASEDYNAMIC void __fastcall ShowEditorChar(WideChar Ch);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	
public:
	__fastcall virtual TTntStringGrid(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntStringGrid(void);
	__property WideString Cells[int ACol][int ARow] = {read=GetCells, write=SetCells};
	__property Tntclasses::TTntStrings* Cols[int Index] = {read=GetCols, write=SetCols};
	__property Tntclasses::TTntStrings* Rows[int Index] = {read=GetRows, write=SetRows};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
	__property TTntGetEditEvent OnGetEditText = {read=FOnGetEditText, write=FOnGetEditText};
	__property TTntSetEditEvent OnSetEditText = {read=FOnSetEditText, write=FOnSetEditText};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntStringGrid(HWND ParentWindow) : _TTntInternalStringGrid(ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTntStringGridStrings : public Tntclasses::TTntStrings 
{
	typedef Tntclasses::TTntStrings inherited;
	
private:
	bool FIsCol;
	int FColRowIndex;
	TTntStringGrid* FGrid;
	Classes::TStrings* __fastcall GridAnsiStrings(void);
	
protected:
	virtual WideString __fastcall Get(int Index);
	virtual void __fastcall Put(int Index, const WideString S);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	__fastcall TTntStringGridStrings(TTntStringGrid* AGrid, int AIndex);
	virtual int __fastcall Add(const WideString S);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Insert(int Index, const WideString S);
public:
	#pragma option push -w-inl
	/* TTntStrings.Destroy */ inline __fastcall virtual ~TTntStringGridStrings(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntgrids */
using namespace Tntgrids;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntgrids
