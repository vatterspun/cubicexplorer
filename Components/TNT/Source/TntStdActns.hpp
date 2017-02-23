// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntstdactns.pas' rev: 10.00

#ifndef TntstdactnsHPP
#define TntstdactnsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Actnlist.hpp>	// Pascal unit
#include <Tntactnlist.hpp>	// Pascal unit
#include <Stdactns.hpp>	// Pascal unit
#include <Tntdialogs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntstdactns
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntHintAction;
class PASCALIMPLEMENTATION TTntHintAction : public Stdactns::THintAction 
{
	typedef Stdactns::THintAction inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	
__published:
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* THintAction.Create */ inline __fastcall virtual TTntHintAction(Classes::TComponent* AOwner) : Stdactns::THintAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHintAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditAction;
class PASCALIMPLEMENTATION TTntEditAction : public Stdactns::TEditAction 
{
	typedef Stdactns::TEditAction inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditAction(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditAction(Classes::TComponent* AOwner) : Stdactns::TEditAction(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditCut;
class PASCALIMPLEMENTATION TTntEditCut : public Stdactns::TEditCut 
{
	typedef Stdactns::TEditCut inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditCut(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditCut(Classes::TComponent* AOwner) : Stdactns::TEditCut(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditCopy;
class PASCALIMPLEMENTATION TTntEditCopy : public Stdactns::TEditCopy 
{
	typedef Stdactns::TEditCopy inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditCopy(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditCopy(Classes::TComponent* AOwner) : Stdactns::TEditCopy(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditPaste;
class PASCALIMPLEMENTATION TTntEditPaste : public Stdactns::TEditPaste 
{
	typedef Stdactns::TEditPaste inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditPaste(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditPaste(Classes::TComponent* AOwner) : Stdactns::TEditPaste(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditSelectAll;
class PASCALIMPLEMENTATION TTntEditSelectAll : public Stdactns::TEditSelectAll 
{
	typedef Stdactns::TEditSelectAll inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditSelectAll(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditSelectAll(Classes::TComponent* AOwner) : Stdactns::TEditSelectAll(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditUndo;
class PASCALIMPLEMENTATION TTntEditUndo : public Stdactns::TEditUndo 
{
	typedef Stdactns::TEditUndo inherited;
	
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
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditUndo(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditUndo(Classes::TComponent* AOwner) : Stdactns::TEditUndo(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntEditDelete;
class PASCALIMPLEMENTATION TTntEditDelete : public Stdactns::TEditDelete 
{
	typedef Stdactns::TEditDelete inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall UpdateTarget(System::TObject* Target);
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntEditDelete(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TAction.Create */ inline __fastcall virtual TTntEditDelete(Classes::TComponent* AOwner) : Stdactns::TEditDelete(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowAction;
class PASCALIMPLEMENTATION TTntWindowAction : public Stdactns::TWindowAction 
{
	typedef Stdactns::TWindowAction inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowAction(Classes::TComponent* AOwner) : Stdactns::TWindowAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowClose;
class PASCALIMPLEMENTATION TTntWindowClose : public Stdactns::TWindowClose 
{
	typedef Stdactns::TWindowClose inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowClose(Classes::TComponent* AOwner) : Stdactns::TWindowClose(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowClose(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowCascade;
class PASCALIMPLEMENTATION TTntWindowCascade : public Stdactns::TWindowCascade 
{
	typedef Stdactns::TWindowCascade inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowCascade(Classes::TComponent* AOwner) : Stdactns::TWindowCascade(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowCascade(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowTileHorizontal;
class PASCALIMPLEMENTATION TTntWindowTileHorizontal : public Stdactns::TWindowTileHorizontal 
{
	typedef Stdactns::TWindowTileHorizontal inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowTileHorizontal(Classes::TComponent* AOwner) : Stdactns::TWindowTileHorizontal(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowTileHorizontal(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowTileVertical;
class PASCALIMPLEMENTATION TTntWindowTileVertical : public Stdactns::TWindowTileVertical 
{
	typedef Stdactns::TWindowTileVertical inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowTileVertical(Classes::TComponent* AOwner) : Stdactns::TWindowTileVertical(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowTileVertical(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowMinimizeAll;
class PASCALIMPLEMENTATION TTntWindowMinimizeAll : public Stdactns::TWindowMinimizeAll 
{
	typedef Stdactns::TWindowMinimizeAll inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowMinimizeAll(Classes::TComponent* AOwner) : Stdactns::TWindowMinimizeAll(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowMinimizeAll(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntWindowArrange;
class PASCALIMPLEMENTATION TTntWindowArrange : public Stdactns::TWindowArrange 
{
	typedef Stdactns::TWindowArrange inherited;
	
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
	/* TWindowAction.Create */ inline __fastcall virtual TTntWindowArrange(Classes::TComponent* AOwner) : Stdactns::TWindowArrange(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntWindowArrange(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntHelpAction;
class PASCALIMPLEMENTATION TTntHelpAction : public Stdactns::THelpAction 
{
	typedef Stdactns::THelpAction inherited;
	
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
	/* THelpAction.Create */ inline __fastcall virtual TTntHelpAction(Classes::TComponent* AOwner) : Stdactns::THelpAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHelpAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntHelpContents;
class PASCALIMPLEMENTATION TTntHelpContents : public Stdactns::THelpContents 
{
	typedef Stdactns::THelpContents inherited;
	
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
	/* THelpAction.Create */ inline __fastcall virtual TTntHelpContents(Classes::TComponent* AOwner) : Stdactns::THelpContents(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHelpContents(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntHelpTopicSearch;
class PASCALIMPLEMENTATION TTntHelpTopicSearch : public Stdactns::THelpTopicSearch 
{
	typedef Stdactns::THelpTopicSearch inherited;
	
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
	/* THelpAction.Create */ inline __fastcall virtual TTntHelpTopicSearch(Classes::TComponent* AOwner) : Stdactns::THelpTopicSearch(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHelpTopicSearch(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntHelpOnHelp;
class PASCALIMPLEMENTATION TTntHelpOnHelp : public Stdactns::THelpOnHelp 
{
	typedef Stdactns::THelpOnHelp inherited;
	
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
	/* THelpAction.Create */ inline __fastcall virtual TTntHelpOnHelp(Classes::TComponent* AOwner) : Stdactns::THelpOnHelp(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHelpOnHelp(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntHelpContextAction;
class PASCALIMPLEMENTATION TTntHelpContextAction : public Stdactns::THelpContextAction 
{
	typedef Stdactns::THelpContextAction inherited;
	
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
	/* THelpAction.Create */ inline __fastcall virtual TTntHelpContextAction(Classes::TComponent* AOwner) : Stdactns::THelpContextAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntHelpContextAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntCommonDialogAction;
class PASCALIMPLEMENTATION TTntCommonDialogAction : public Stdactns::TCommonDialogAction 
{
	typedef Stdactns::TCommonDialogAction inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntCommonDialogAction(Classes::TComponent* AOwner) : Stdactns::TCommonDialogAction(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntCommonDialogAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileAction;
class PASCALIMPLEMENTATION TTntFileAction : public Stdactns::TFileAction 
{
	typedef Stdactns::TFileAction inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntFileAction(Classes::TComponent* AOwner) : Stdactns::TFileAction(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFileAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileOpen;
class PASCALIMPLEMENTATION TTntFileOpen : public Stdactns::TFileOpen 
{
	typedef Stdactns::TFileOpen inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	HIDESBASE Tntdialogs::TTntOpenDialog* __fastcall GetDialog(void);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual TMetaClass* __fastcall GetDialogClass(void);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property Tntdialogs::TTntOpenDialog* Dialog = {read=GetDialog};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TFileOpen.Create */ inline __fastcall virtual TTntFileOpen(Classes::TComponent* AOwner) : Stdactns::TFileOpen(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFileOpen(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileOpenWith;
class PASCALIMPLEMENTATION TTntFileOpenWith : public Stdactns::TFileOpenWith 
{
	typedef Stdactns::TFileOpenWith inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	HIDESBASE Tntdialogs::TTntOpenDialog* __fastcall GetDialog(void);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual TMetaClass* __fastcall GetDialogClass(void);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property Tntdialogs::TTntOpenDialog* Dialog = {read=GetDialog};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TFileOpen.Create */ inline __fastcall virtual TTntFileOpenWith(Classes::TComponent* AOwner) : Stdactns::TFileOpenWith(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFileOpenWith(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileSaveAs;
class PASCALIMPLEMENTATION TTntFileSaveAs : public Stdactns::TFileSaveAs 
{
	typedef Stdactns::TFileSaveAs inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	HIDESBASE Tntdialogs::TTntSaveDialog* __fastcall GetDialog(void);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual TMetaClass* __fastcall GetDialogClass(void);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property Tntdialogs::TTntSaveDialog* Dialog = {read=GetDialog};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TFileSaveAs.Create */ inline __fastcall virtual TTntFileSaveAs(Classes::TComponent* AOwner) : Stdactns::TFileSaveAs(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFileSaveAs(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFilePrintSetup;
class PASCALIMPLEMENTATION TTntFilePrintSetup : public Stdactns::TFilePrintSetup 
{
	typedef Stdactns::TFilePrintSetup inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntFilePrintSetup(Classes::TComponent* AOwner) : Stdactns::TFilePrintSetup(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFilePrintSetup(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFilePageSetup;
class PASCALIMPLEMENTATION TTntFilePageSetup : public Stdactns::TFilePageSetup 
{
	typedef Stdactns::TFilePageSetup inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntFilePageSetup(Classes::TComponent* AOwner) : Stdactns::TFilePageSetup(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFilePageSetup(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileExit;
class PASCALIMPLEMENTATION TTntFileExit : public Stdactns::TFileExit 
{
	typedef Stdactns::TFileExit inherited;
	
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
	/* TCustomAction.Create */ inline __fastcall virtual TTntFileExit(Classes::TComponent* AOwner) : Stdactns::TFileExit(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntFileExit(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSearchAction;
class PASCALIMPLEMENTATION TTntSearchAction : public Stdactns::TSearchAction 
{
	typedef Stdactns::TSearchAction inherited;
	
private:
	WideString __fastcall GetCaption();
	HIDESBASE void __fastcall SetCaption(const WideString Value);
	WideString __fastcall GetHint();
	HIDESBASE void __fastcall SetHint(const WideString Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property WideString Caption = {read=GetCaption, write=SetCaption};
	__property WideString Hint = {read=GetHint, write=SetHint};
public:
	#pragma option push -w-inl
	/* TSearchAction.Create */ inline __fastcall virtual TTntSearchAction(Classes::TComponent* AOwner) : Stdactns::TSearchAction(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TSearchAction.Destroy */ inline __fastcall virtual ~TTntSearchAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSearchFind;
class PASCALIMPLEMENTATION TTntSearchFind : public Stdactns::TSearchFind 
{
	typedef Stdactns::TSearchFind inherited;
	
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
	/* TSearchAction.Create */ inline __fastcall virtual TTntSearchFind(Classes::TComponent* AOwner) : Stdactns::TSearchFind(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TSearchAction.Destroy */ inline __fastcall virtual ~TTntSearchFind(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSearchReplace;
class PASCALIMPLEMENTATION TTntSearchReplace : public Stdactns::TSearchReplace 
{
	typedef Stdactns::TSearchReplace inherited;
	
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
	/* TSearchAction.Create */ inline __fastcall virtual TTntSearchReplace(Classes::TComponent* AOwner) : Stdactns::TSearchReplace(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TSearchAction.Destroy */ inline __fastcall virtual ~TTntSearchReplace(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSearchFindFirst;
class PASCALIMPLEMENTATION TTntSearchFindFirst : public Stdactns::TSearchFindFirst 
{
	typedef Stdactns::TSearchFindFirst inherited;
	
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
	/* TSearchFindFirst.Create */ inline __fastcall virtual TTntSearchFindFirst(Classes::TComponent* AOwner) : Stdactns::TSearchFindFirst(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TSearchAction.Destroy */ inline __fastcall virtual ~TTntSearchFindFirst(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSearchFindNext;
class PASCALIMPLEMENTATION TTntSearchFindNext : public Stdactns::TSearchFindNext 
{
	typedef Stdactns::TSearchFindNext inherited;
	
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
	/* TSearchFindNext.Create */ inline __fastcall virtual TTntSearchFindNext(Classes::TComponent* AOwner) : Stdactns::TSearchFindNext(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntSearchFindNext(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFontEdit;
class PASCALIMPLEMENTATION TTntFontEdit : public Stdactns::TFontEdit 
{
	typedef Stdactns::TFontEdit inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntFontEdit(Classes::TComponent* AOwner) : Stdactns::TFontEdit(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntFontEdit(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntColorSelect;
class PASCALIMPLEMENTATION TTntColorSelect : public Stdactns::TColorSelect 
{
	typedef Stdactns::TColorSelect inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntColorSelect(Classes::TComponent* AOwner) : Stdactns::TColorSelect(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntColorSelect(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntPrintDlg;
class PASCALIMPLEMENTATION TTntPrintDlg : public Stdactns::TPrintDlg 
{
	typedef Stdactns::TPrintDlg inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntPrintDlg(Classes::TComponent* AOwner) : Stdactns::TPrintDlg(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntPrintDlg(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TntStdActn_AfterInherited_Assign(Actnlist::TCustomAction* Action, Classes::TPersistent* Source);

}	/* namespace Tntstdactns */
using namespace Tntstdactns;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntstdactns
