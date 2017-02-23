// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntdialogs.pas' rev: 10.00

#ifndef TntdialogsHPP
#define TntdialogsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Commdlg.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Tntclasses.hpp>	// Pascal unit
#include <Tntforms.hpp>	// Pascal unit
#include <Tntsysutils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntdialogs
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TIncludeItemEventW)(const _OFNOTIFYEXW &OFN, bool &Include);

class DELPHICLASS TTntOpenDialog;
class PASCALIMPLEMENTATION TTntOpenDialog : public Dialogs::TOpenDialog 
{
	typedef Dialogs::TOpenDialog inherited;
	
private:
	WideString FDefaultExt;
	WideString FFileName;
	WideString FFilter;
	WideString FInitialDir;
	WideString FTitle;
	Tntclasses::TTntStrings* FFiles;
	TIncludeItemEventW FOnIncludeItem;
	WideString __fastcall GetDefaultExt();
	void __fastcall SetInheritedDefaultExt(const AnsiString Value);
	void __fastcall SetDefaultExt(const WideString Value);
	HIDESBASE WideString __fastcall GetFileName();
	void __fastcall SetFileName(const WideString Value);
	WideString __fastcall GetFilter();
	void __fastcall SetInheritedFilter(const AnsiString Value);
	void __fastcall SetFilter(const WideString Value);
	WideString __fastcall GetInitialDir();
	void __fastcall SetInheritedInitialDir(const AnsiString Value);
	HIDESBASE void __fastcall SetInitialDir(const WideString Value);
	WideString __fastcall GetTitle();
	void __fastcall SetInheritedTitle(const AnsiString Value);
	void __fastcall SetTitle(const WideString Value);
	Tntclasses::TTntStrings* __fastcall GetFiles(void);
	#pragma pack(push,1)
	tagOFNA FProxiedOpenFilenameA;
	#pragma pack(pop)
	
protected:
	bool FAllowDoCanClose;
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	bool __fastcall CanCloseW(tagOFNW &OpenFileName);
	DYNAMIC bool __fastcall DoCanClose(void);
	void __fastcall GetFileNamesW(tagOFNW &OpenFileName);
	DYNAMIC void __fastcall DoIncludeItem(const Dialogs::TOFNotifyEx &OFN, bool &Include);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	BOOL __fastcall DoExecuteW(void * Func, HWND ParentWnd)/* overload */;
	BOOL __fastcall DoExecuteW(void * Func)/* overload */;
	
public:
	__fastcall virtual TTntOpenDialog(Classes::TComponent* AOwner);
	__fastcall virtual ~TTntOpenDialog(void);
	virtual bool __fastcall Execute(void)/* overload */;
	virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
	__property Tntclasses::TTntStrings* Files = {read=GetFiles};
	
__published:
	__property WideString DefaultExt = {read=GetDefaultExt, write=SetDefaultExt};
	__property WideString FileName = {read=GetFileName, write=SetFileName};
	__property WideString Filter = {read=GetFilter, write=SetFilter};
	__property WideString InitialDir = {read=GetInitialDir, write=SetInitialDir};
	__property WideString Title = {read=GetTitle, write=SetTitle};
	__property TIncludeItemEventW OnIncludeItem = {read=FOnIncludeItem, write=FOnIncludeItem};
};


class DELPHICLASS TTntSaveDialog;
class PASCALIMPLEMENTATION TTntSaveDialog : public TTntOpenDialog 
{
	typedef TTntOpenDialog inherited;
	
public:
	virtual bool __fastcall Execute(void)/* overload */;
	virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
public:
	#pragma option push -w-inl
	/* TTntOpenDialog.Create */ inline __fastcall virtual TTntSaveDialog(Classes::TComponent* AOwner) : TTntOpenDialog(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTntOpenDialog.Destroy */ inline __fastcall virtual ~TTntSaveDialog(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE HWND __fastcall GetModalParentWnd(void);
extern PACKAGE Tntforms::TTntForm* __fastcall WideCreateMessageDialog(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, Dialogs::TMsgDlgBtn DefaultButton)/* overload */;
extern PACKAGE Tntforms::TTntForm* __fastcall WideCreateMessageDialog(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons)/* overload */;
extern PACKAGE int __fastcall WideMessageDlg(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx, Dialogs::TMsgDlgBtn DefaultButton)/* overload */;
extern PACKAGE int __fastcall WideMessageDlg(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx)/* overload */;
extern PACKAGE int __fastcall WideMessageDlgPos(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx, int X, int Y, Dialogs::TMsgDlgBtn DefaultButton)/* overload */;
extern PACKAGE int __fastcall WideMessageDlgPos(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx, int X, int Y)/* overload */;
extern PACKAGE int __fastcall WideMessageDlgPosHelp(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx, int X, int Y, const WideString HelpFileName, Dialogs::TMsgDlgBtn DefaultButton)/* overload */;
extern PACKAGE int __fastcall WideMessageDlgPosHelp(const WideString Msg, Dialogs::TMsgDlgType DlgType, Dialogs::TMsgDlgButtons Buttons, int HelpCtx, int X, int Y, const WideString HelpFileName)/* overload */;
extern PACKAGE void __fastcall WideShowMessage(const WideString Msg);
extern PACKAGE void __fastcall WideShowMessageFmt(const WideString Msg, System::TVarRec * Params, const int Params_Size);
extern PACKAGE void __fastcall WideShowMessagePos(const WideString Msg, int X, int Y);
extern PACKAGE bool __fastcall WideInputQuery(const WideString ACaption, const WideString APrompt, WideString &Value);
extern PACKAGE WideString __fastcall WideInputBox(const WideString ACaption, const WideString APrompt, const WideString ADefault);
extern PACKAGE bool __fastcall WidePromptForFileName(WideString &AFileName, const WideString AFilter = L"", const WideString ADefaultExt = L"", const WideString ATitle = L"", const WideString AInitialDir = L"", bool SaveDialog = false);

}	/* namespace Tntdialogs */
using namespace Tntdialogs;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntdialogs
