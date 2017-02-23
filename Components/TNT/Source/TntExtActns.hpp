// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntextactns.pas' rev: 10.00

#ifndef TntextactnsHPP
#define TntextactnsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Tntactnlist.hpp>	// Pascal unit
#include <Extactns.hpp>	// Pascal unit
#include <Actnlist.hpp>	// Pascal unit
#include <Stdactns.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntextactns
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntCustomFileRun;
class PASCALIMPLEMENTATION TTntCustomFileRun : public Extactns::TCustomFileRun 
{
	typedef Extactns::TCustomFileRun inherited;
	
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
	/* TCustomFileRun.Create */ inline __fastcall virtual TTntCustomFileRun(Classes::TComponent* AOwner) : Extactns::TCustomFileRun(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntCustomFileRun(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntFileRun;
class PASCALIMPLEMENTATION TTntFileRun : public Extactns::TFileRun 
{
	typedef Extactns::TFileRun inherited;
	
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
	/* TCustomFileRun.Create */ inline __fastcall virtual TTntFileRun(Classes::TComponent* AOwner) : Extactns::TFileRun(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntFileRun(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditAction;
class PASCALIMPLEMENTATION TTntRichEditAction : public Extactns::TRichEditAction 
{
	typedef Extactns::TRichEditAction inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditAction(Classes::TComponent* AOwner) : Extactns::TRichEditAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditBold;
class PASCALIMPLEMENTATION TTntRichEditBold : public Extactns::TRichEditBold 
{
	typedef Extactns::TRichEditBold inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditBold(Classes::TComponent* AOwner) : Extactns::TRichEditBold(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditBold(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditItalic;
class PASCALIMPLEMENTATION TTntRichEditItalic : public Extactns::TRichEditItalic 
{
	typedef Extactns::TRichEditItalic inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditItalic(Classes::TComponent* AOwner) : Extactns::TRichEditItalic(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditItalic(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditUnderline;
class PASCALIMPLEMENTATION TTntRichEditUnderline : public Extactns::TRichEditUnderline 
{
	typedef Extactns::TRichEditUnderline inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditUnderline(Classes::TComponent* AOwner) : Extactns::TRichEditUnderline(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditUnderline(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditStrikeOut;
class PASCALIMPLEMENTATION TTntRichEditStrikeOut : public Extactns::TRichEditStrikeOut 
{
	typedef Extactns::TRichEditStrikeOut inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditStrikeOut(Classes::TComponent* AOwner) : Extactns::TRichEditStrikeOut(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditStrikeOut(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditBullets;
class PASCALIMPLEMENTATION TTntRichEditBullets : public Extactns::TRichEditBullets 
{
	typedef Extactns::TRichEditBullets inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditBullets(Classes::TComponent* AOwner) : Extactns::TRichEditBullets(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditBullets(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditAlignLeft;
class PASCALIMPLEMENTATION TTntRichEditAlignLeft : public Extactns::TRichEditAlignLeft 
{
	typedef Extactns::TRichEditAlignLeft inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditAlignLeft(Classes::TComponent* AOwner) : Extactns::TRichEditAlignLeft(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditAlignLeft(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditAlignRight;
class PASCALIMPLEMENTATION TTntRichEditAlignRight : public Extactns::TRichEditAlignRight 
{
	typedef Extactns::TRichEditAlignRight inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditAlignRight(Classes::TComponent* AOwner) : Extactns::TRichEditAlignRight(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditAlignRight(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntRichEditAlignCenter;
class PASCALIMPLEMENTATION TTntRichEditAlignCenter : public Extactns::TRichEditAlignCenter 
{
	typedef Extactns::TRichEditAlignCenter inherited;
	
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
	/* TRichEditAction.Create */ inline __fastcall virtual TTntRichEditAlignCenter(Classes::TComponent* AOwner) : Extactns::TRichEditAlignCenter(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TEditAction.Destroy */ inline __fastcall virtual ~TTntRichEditAlignCenter(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntTabAction;
class PASCALIMPLEMENTATION TTntTabAction : public Extactns::TTabAction 
{
	typedef Extactns::TTabAction inherited;
	
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
	/* TTabAction.Create */ inline __fastcall virtual TTntTabAction(Classes::TComponent* AOwner) : Extactns::TTabAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntTabAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntPreviousTab;
class PASCALIMPLEMENTATION TTntPreviousTab : public Extactns::TPreviousTab 
{
	typedef Extactns::TPreviousTab inherited;
	
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
	/* TTabAction.Create */ inline __fastcall virtual TTntPreviousTab(Classes::TComponent* AOwner) : Extactns::TPreviousTab(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntPreviousTab(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntNextTab;
class PASCALIMPLEMENTATION TTntNextTab : public Extactns::TNextTab 
{
	typedef Extactns::TNextTab inherited;
	
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
	/* TTabAction.Create */ inline __fastcall virtual TTntNextTab(Classes::TComponent* AOwner) : Extactns::TNextTab(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntNextTab(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntOpenPicture;
class PASCALIMPLEMENTATION TTntOpenPicture : public Extactns::TOpenPicture 
{
	typedef Extactns::TOpenPicture inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntOpenPicture(Classes::TComponent* AOwner) : Extactns::TOpenPicture(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntOpenPicture(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSavePicture;
class PASCALIMPLEMENTATION TTntSavePicture : public Extactns::TSavePicture 
{
	typedef Extactns::TSavePicture inherited;
	
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
	/* TCommonDialogAction.Create */ inline __fastcall virtual TTntSavePicture(Classes::TComponent* AOwner) : Extactns::TSavePicture(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCommonDialogAction.Destroy */ inline __fastcall virtual ~TTntSavePicture(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntURLAction;
class PASCALIMPLEMENTATION TTntURLAction : public Extactns::TURLAction 
{
	typedef Extactns::TURLAction inherited;
	
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
	/* TCustomAction.Create */ inline __fastcall virtual TTntURLAction(Classes::TComponent* AOwner) : Extactns::TURLAction(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntURLAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntBrowseURL;
class PASCALIMPLEMENTATION TTntBrowseURL : public Extactns::TBrowseURL 
{
	typedef Extactns::TBrowseURL inherited;
	
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
	/* TCustomAction.Create */ inline __fastcall virtual TTntBrowseURL(Classes::TComponent* AOwner) : Extactns::TBrowseURL(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntBrowseURL(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntDownLoadURL;
class PASCALIMPLEMENTATION TTntDownLoadURL : public Extactns::TDownLoadURL 
{
	typedef Extactns::TDownLoadURL inherited;
	
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
	/* TCustomAction.Create */ inline __fastcall virtual TTntDownLoadURL(Classes::TComponent* AOwner) : Extactns::TDownLoadURL(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntDownLoadURL(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntSendMail;
class PASCALIMPLEMENTATION TTntSendMail : public Extactns::TSendMail 
{
	typedef Extactns::TSendMail inherited;
	
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
	/* TSendMail.Destroy */ inline __fastcall virtual ~TTntSendMail(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Create */ inline __fastcall virtual TTntSendMail(Classes::TComponent* AOwner) : Extactns::TSendMail(AOwner) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlAction;
class PASCALIMPLEMENTATION TTntListControlAction : public Extactns::TListControlAction 
{
	typedef Extactns::TListControlAction inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlAction(Classes::TComponent* AOwner) : Extactns::TListControlAction(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlAction(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlCopySelection;
class PASCALIMPLEMENTATION TTntListControlCopySelection : public Extactns::TListControlCopySelection 
{
	typedef Extactns::TListControlCopySelection inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlCopySelection(Classes::TComponent* AOwner) : Extactns::TListControlCopySelection(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlCopySelection(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlDeleteSelection;
class PASCALIMPLEMENTATION TTntListControlDeleteSelection : public Extactns::TListControlDeleteSelection 
{
	typedef Extactns::TListControlDeleteSelection inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlDeleteSelection(Classes::TComponent* AOwner) : Extactns::TListControlDeleteSelection(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlDeleteSelection(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlSelectAll;
class PASCALIMPLEMENTATION TTntListControlSelectAll : public Extactns::TListControlSelectAll 
{
	typedef Extactns::TListControlSelectAll inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlSelectAll(Classes::TComponent* AOwner) : Extactns::TListControlSelectAll(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlSelectAll(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlClearSelection;
class PASCALIMPLEMENTATION TTntListControlClearSelection : public Extactns::TListControlClearSelection 
{
	typedef Extactns::TListControlClearSelection inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlClearSelection(Classes::TComponent* AOwner) : Extactns::TListControlClearSelection(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlClearSelection(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


class DELPHICLASS TTntListControlMoveSelection;
class PASCALIMPLEMENTATION TTntListControlMoveSelection : public Extactns::TListControlMoveSelection 
{
	typedef Extactns::TListControlMoveSelection inherited;
	
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
	/* TListControlAction.Create */ inline __fastcall virtual TTntListControlMoveSelection(Classes::TComponent* AOwner) : Extactns::TListControlMoveSelection(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTntListControlMoveSelection(void) { }
	#pragma option pop
	
private:
	void *__ITntAction;	/* Tntactnlist::ITntAction */
	
public:
	operator ITntAction*(void) { return (ITntAction*)&__ITntAction; }
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tntextactns */
using namespace Tntextactns;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntextactns
