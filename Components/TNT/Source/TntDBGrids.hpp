// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntdbgrids.pas' rev: 10.00

#ifndef TntdbgridsHPP
#define TntdbgridsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <Dbgrids.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Dbctrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Tntstdctrls.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntdbgrids
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntColumnTitle;
class PASCALIMPLEMENTATION TTntColumnTitle : public Dbgrids::TColumnTitle 
{
	typedef Dbgrids::TColumnTitle inherited;
	
private:
	WideString FCaption;
	void __fastcall SetInheritedCaption(const AnsiString Value);
	HIDESBASE WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	HIDESBASE bool __fastcall IsCaptionStored(void);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall RestoreDefaults(void);
	HIDESBASE WideString __fastcall DefaultCaption();
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
public:
	#pragma option push -w-inl
	/* TColumnTitle.Create */ inline __fastcall TTntColumnTitle(Dbgrids::TColumn* Column) : Dbgrids::TColumnTitle(Column) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TColumnTitle.Destroy */ inline __fastcall virtual ~TTntColumnTitle(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntColumn;
class PASCALIMPLEMENTATION TTntColumn : public Dbgrids::TColumn 
{
	typedef Dbgrids::TColumn inherited;
	
private:
	Tntclasses::TTntStrings* FWidePickList;
	Tntclasses::TTntStrings* __fastcall GetWidePickList(void);
	void __fastcall SetWidePickList(const Tntclasses::TTntStrings* Value);
	void __fastcall HandlePickListChange(System::TObject* Sender);
	TTntColumnTitle* __fastcall GetTitle(void);
	HIDESBASE void __fastcall SetTitle(const TTntColumnTitle* Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual Dbgrids::TColumnTitle* __fastcall CreateTitle(void);
	
public:
	__fastcall virtual ~TTntColumn(void);
	__property Tntclasses::TTntStrings* WidePickList = {read=GetWidePickList, write=SetWidePickList};
	
__published:
	__property Tntclasses::TTntStrings* PickList = {read=GetWidePickList, write=SetWidePickList};
	__property TTntColumnTitle* Title = {read=GetTitle, write=SetTitle};
public:
	#pragma option push -w-inl
	/* TColumn.Create */ inline __fastcall virtual TTntColumn(Classes::TCollection* Collection) : Dbgrids::TColumn(Collection) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridInplaceEdit;
class PASCALIMPLEMENTATION TDBGridInplaceEdit : public Grids::TInplaceEditList 
{
	typedef Grids::TInplaceEditList inherited;
	
private:
	Dbctrls::TDBLookupListBox* FDataList;
	bool FUseDataList;
	Db::TDataSource* FLookupSource;
	Tntstdctrls::TTntCustomListBox* FWidePickListBox;
	Tntstdctrls::TTntCustomListBox* __fastcall GetWidePickListBox(void);
	
protected:
	DYNAMIC void __fastcall CloseUp(bool Accept);
	virtual void __fastcall DoEditButtonClick(void);
	DYNAMIC void __fastcall DropDown(void);
	virtual void __fastcall UpdateContents(void);
	__property bool UseDataList = {read=FUseDataList, nodefault};
	
public:
	__fastcall virtual TDBGridInplaceEdit(Classes::TComponent* Owner);
	__property Dbctrls::TDBLookupListBox* DataList = {read=FDataList};
	__property Tntstdctrls::TTntCustomListBox* WidePickListBox = {read=GetWidePickListBox};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBGridInplaceEdit(HWND ParentWindow) : Grids::TInplaceEditList(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TDBGridInplaceEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntDBGridInplaceEdit;
class PASCALIMPLEMENTATION TTntDBGridInplaceEdit : public TDBGridInplaceEdit 
{
	typedef TDBGridInplaceEdit inherited;
	
private:
	bool FInDblClick;
	bool FBlockSetText;
	MESSAGE void __fastcall WMSetText(Messages::TWMSetText &Message);
	
protected:
	HIDESBASE virtual WideString __fastcall GetText();
	HIDESBASE virtual void __fastcall SetText(const WideString Value);
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	virtual void __fastcall UpdateContents(void);
	DYNAMIC void __fastcall DblClick(void);
	
public:
	__property WideString Text = {read=GetText, write=SetText};
public:
	#pragma option push -w-inl
	/* TDBGridInplaceEdit.Create */ inline __fastcall virtual TTntDBGridInplaceEdit(Classes::TComponent* Owner) : TDBGridInplaceEdit(Owner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntDBGridInplaceEdit(HWND ParentWindow) : TDBGridInplaceEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TTntDBGridInplaceEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntDBGridColumns;
class PASCALIMPLEMENTATION TTntDBGridColumns : public Dbgrids::TDBGridColumns 
{
	typedef Dbgrids::TDBGridColumns inherited;
	
public:
	TTntColumn* operator[](int Index) { return Items[Index]; }
	
private:
	HIDESBASE TTntColumn* __fastcall GetColumn(int Index);
	HIDESBASE void __fastcall SetColumn(int Index, const TTntColumn* Value);
	
public:
	HIDESBASE TTntColumn* __fastcall Add(void);
	__property TTntColumn* Items[int Index] = {read=GetColumn, write=SetColumn/*, default*/};
public:
	#pragma option push -w-inl
	/* TDBGridColumns.Create */ inline __fastcall TTntDBGridColumns(Dbgrids::TCustomDBGrid* Grid, TMetaClass* ColumnClass) : Dbgrids::TDBGridColumns(Grid, ColumnClass) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TTntDBGridColumns(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntGridDataLink;
class PASCALIMPLEMENTATION TTntGridDataLink : public Dbgrids::TGridDataLink 
{
	typedef Dbgrids::TGridDataLink inherited;
	
private:
	Db::TFieldSetTextEvent OriginalSetText;
	void __fastcall GridUpdateFieldText(Db::TField* Sender, const AnsiString Text);
	
protected:
	virtual void __fastcall UpdateData(void);
	virtual void __fastcall RecordChanged(Db::TField* Field);
public:
	#pragma option push -w-inl
	/* TGridDataLink.Create */ inline __fastcall TTntGridDataLink(Dbgrids::TCustomDBGrid* AGrid) : Dbgrids::TGridDataLink(AGrid) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TGridDataLink.Destroy */ inline __fastcall virtual ~TTntGridDataLink(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntCustomDBGrid;
class PASCALIMPLEMENTATION TTntCustomDBGrid : public Dbgrids::TCustomDBGrid 
{
	typedef Dbgrids::TCustomDBGrid inherited;
	
private:
	WideString FEditText;
	WideString __fastcall GetHint();
	void __fastcall SetHint(const WideString Value);
	HIDESBASE bool __fastcall IsHintStored(void);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Msg);
	TTntDBGridColumns* __fastcall GetColumns(void);
	HIDESBASE void __fastcall SetColumns(const TTntDBGridColumns* Value);
	
protected:
	virtual void __fastcall CreateWindowHandle(const Controls::TCreateParams &Params);
	HIDESBASEDYNAMIC void __fastcall ShowEditorChar(WideChar Ch);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC Dbgrids::TDBGridColumns* __fastcall CreateColumns(void);
	__property TTntDBGridColumns* Columns = {read=GetColumns, write=SetColumns};
	virtual Grids::TInplaceEdit* __fastcall CreateEditor(void);
	DYNAMIC Dbgrids::TGridDataLink* __fastcall CreateDataLink(void);
	HIDESBASE WideString __fastcall GetEditText(int ACol, int ARow);
	virtual void __fastcall DrawCell(int ACol, int ARow, const Types::TRect &ARect, Grids::TGridDrawState AState);
	DYNAMIC void __fastcall SetEditText(int ACol, int ARow, const AnsiString Value);
	
public:
	HIDESBASEDYNAMIC void __fastcall DefaultDrawColumnCell(const Types::TRect &Rect, int DataCol, TTntColumn* Column, Grids::TGridDrawState State);
	HIDESBASE void __fastcall DefaultDrawDataCell(const Types::TRect &Rect, Db::TField* Field, Grids::TGridDrawState State);
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint, stored=IsHintStored};
public:
	#pragma option push -w-inl
	/* TCustomDBGrid.Create */ inline __fastcall virtual TTntCustomDBGrid(Classes::TComponent* AOwner) : Dbgrids::TCustomDBGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBGrid.Destroy */ inline __fastcall virtual ~TTntCustomDBGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntCustomDBGrid(HWND ParentWindow) : Dbgrids::TCustomDBGrid(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTntDBGrid;
class PASCALIMPLEMENTATION TTntDBGrid : public TTntCustomDBGrid 
{
	typedef TTntCustomDBGrid inherited;
	
public:
	__property Canvas ;
	__property SelectedRows ;
	
__published:
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property Color  = {default=-16777211};
	__property Columns  = {stored=false};
	__property Constraints ;
	__property Ctl3D ;
	__property DataSource ;
	__property DefaultDrawing  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property FixedColor  = {default=-16777201};
	__property Font ;
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property Options  = {default=3325};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property TitleFont ;
	__property Visible  = {default=1};
	__property OnCellClick ;
	__property OnColEnter ;
	__property OnColExit ;
	__property OnColumnMoved ;
	__property OnDrawDataCell ;
	__property OnDrawColumnCell ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEditButtonClick ;
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
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnTitleClick ;
public:
	#pragma option push -w-inl
	/* TCustomDBGrid.Create */ inline __fastcall virtual TTntDBGrid(Classes::TComponent* AOwner) : TTntCustomDBGrid(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBGrid.Destroy */ inline __fastcall virtual ~TTntDBGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTntDBGrid(HWND ParentWindow) : TTntCustomDBGrid(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntdbgrids */
using namespace Tntdbgrids;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntdbgrids
