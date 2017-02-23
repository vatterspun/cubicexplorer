// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntdbactns.pas' rev: 10.00

#ifndef TntdbactnsHPP
#define TntdbactnsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Actnlist.hpp>	// Pascal unit
#include <Dbactns.hpp>	// Pascal unit
#include <Tntactnlist.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntdbactns
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntDataSetAction;
class PASCALIMPLEMENTATION TTntDataSetAction : public Dbactns::TDataSetAction 
{
	typedef Dbactns::TDataSetAction inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetAction(Classes::TComponent* AOwner) : Dbactns::TDataSetAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetFirst;
class PASCALIMPLEMENTATION TTntDataSetFirst : public Dbactns::TDataSetFirst 
{
	typedef Dbactns::TDataSetFirst inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetFirst(Classes::TComponent* AOwner) : Dbactns::TDataSetFirst(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetFirst(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetPrior;
class PASCALIMPLEMENTATION TTntDataSetPrior : public Dbactns::TDataSetPrior 
{
	typedef Dbactns::TDataSetPrior inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetPrior(Classes::TComponent* AOwner) : Dbactns::TDataSetPrior(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetPrior(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetNext;
class PASCALIMPLEMENTATION TTntDataSetNext : public Dbactns::TDataSetNext 
{
	typedef Dbactns::TDataSetNext inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetNext(Classes::TComponent* AOwner) : Dbactns::TDataSetNext(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetNext(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetLast;
class PASCALIMPLEMENTATION TTntDataSetLast : public Dbactns::TDataSetLast 
{
	typedef Dbactns::TDataSetLast inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetLast(Classes::TComponent* AOwner) : Dbactns::TDataSetLast(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetLast(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetInsert;
class PASCALIMPLEMENTATION TTntDataSetInsert : public Dbactns::TDataSetInsert 
{
	typedef Dbactns::TDataSetInsert inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetInsert(Classes::TComponent* AOwner) : Dbactns::TDataSetInsert(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetInsert(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetDelete;
class PASCALIMPLEMENTATION TTntDataSetDelete : public Dbactns::TDataSetDelete 
{
	typedef Dbactns::TDataSetDelete inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetDelete(Classes::TComponent* AOwner) : Dbactns::TDataSetDelete(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetDelete(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetEdit;
class PASCALIMPLEMENTATION TTntDataSetEdit : public Dbactns::TDataSetEdit 
{
	typedef Dbactns::TDataSetEdit inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetEdit(Classes::TComponent* AOwner) : Dbactns::TDataSetEdit(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetEdit(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetPost;
class PASCALIMPLEMENTATION TTntDataSetPost : public Dbactns::TDataSetPost 
{
	typedef Dbactns::TDataSetPost inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetPost(Classes::TComponent* AOwner) : Dbactns::TDataSetPost(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetPost(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetCancel;
class PASCALIMPLEMENTATION TTntDataSetCancel : public Dbactns::TDataSetCancel 
{
	typedef Dbactns::TDataSetCancel inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetCancel(Classes::TComponent* AOwner) : Dbactns::TDataSetCancel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetCancel(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDataSetRefresh;
class PASCALIMPLEMENTATION TTntDataSetRefresh : public Dbactns::TDataSetRefresh 
{
	typedef Dbactns::TDataSetRefresh inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntDataSetRefresh(Classes::TComponent* AOwner) : Dbactns::TDataSetRefresh(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDataSetRefresh(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TntDBActn_AfterInherited_Assign(Actnlist::TCustomAction* Action, Classes::TPersistent* Source);

}	/* namespace Tntdbactns */
using namespace Tntdbactns;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntdbactns
