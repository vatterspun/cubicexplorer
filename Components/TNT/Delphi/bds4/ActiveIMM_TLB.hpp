// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Activeimm_tlb.pas' rev: 10.00

#ifndef Activeimm_tlbHPP
#define Activeimm_tlbHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Activex.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Oleserver.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Activeimm_tlb
{
//-- type declarations -------------------------------------------------------
__interface IActiveIMMApp;
typedef System::DelphiInterface<IActiveIMMApp> _di_IActiveIMMApp;
typedef IActiveIMMApp CActiveIMM;
;

struct _userHBITMAP;
typedef _userHBITMAP *wireHBITMAP;

struct _RemotableHandle;
typedef _RemotableHandle *wireHWND;

typedef GUID *PUserType1;

struct tagMSG;
typedef tagMSG *PUserType2;

struct __MIDL___MIDL_itf_dimm_0000_0001;
typedef __MIDL___MIDL_itf_dimm_0000_0001 *PUserType3;

struct __MIDL___MIDL_itf_dimm_0000_0002;
typedef __MIDL___MIDL_itf_dimm_0000_0002 *PUserType4;

struct __MIDL___MIDL_itf_dimm_0000_0005;
typedef __MIDL___MIDL_itf_dimm_0000_0005 *PUserType5;

struct __MIDL___MIDL_itf_dimm_0000_0003;
typedef __MIDL___MIDL_itf_dimm_0000_0003 *PUserType6;

struct __MIDL___MIDL_itf_dimm_0000_0004;
typedef __MIDL___MIDL_itf_dimm_0000_0004 *PUserType7;

struct __MIDL___MIDL_itf_dimm_0000_0006;
typedef __MIDL___MIDL_itf_dimm_0000_0006 *PUserType8;

struct tagPOINT;
typedef tagPOINT *PUserType9;

typedef Word *PWord1;

struct __MIDL___MIDL_itf_dimm_0000_0010;
typedef __MIDL___MIDL_itf_dimm_0000_0010 *PUserType10;

struct __MIDL___MIDL_itf_dimm_0000_0011;
typedef __MIDL___MIDL_itf_dimm_0000_0011 *PUserType11;

struct __MIDL___MIDL_itf_dimm_0000_0012;
typedef __MIDL___MIDL_itf_dimm_0000_0012 *PUserType12;

typedef Byte *PByte1;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0001
{
	
public:
	char *lpReading;
	char *lpWord;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0001  REGISTERWORDA;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0002
{
	
public:
	WideChar *lpReading;
	WideChar *lpWord;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0002  REGISTERWORDW;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0003
{
	
public:
	int lfHeight;
	int lfWidth;
	int lfEscapement;
	int lfOrientation;
	int lfWeight;
	Byte lfItalic;
	Byte lfUnderline;
	Byte lfStrikeOut;
	Byte lfCharSet;
	Byte lfOutPrecision;
	Byte lfClipPrecision;
	Byte lfQuality;
	Byte lfPitchAndFamily;
	Shortint lfFaceName[32];
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0003  LOGFONTA;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0004
{
	
public:
	int lfHeight;
	int lfWidth;
	int lfEscapement;
	int lfOrientation;
	int lfWeight;
	Byte lfItalic;
	Byte lfUnderline;
	Byte lfStrikeOut;
	Byte lfCharSet;
	Byte lfOutPrecision;
	Byte lfClipPrecision;
	Byte lfQuality;
	Byte lfPitchAndFamily;
	Word lfFaceName[32];
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0004  LOGFONTW;

#pragma pack(push,1)
struct tagPOINT
{
	
public:
	int x;
	int y;
} ;
#pragma pack(pop)

#pragma pack(push,1)
struct tagRECT
{
	
public:
	int left;
	int top;
	int right;
	int bottom;
} ;
#pragma pack(pop)

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0005
{
	
public:
	unsigned dwIndex;
	unsigned dwStyle;
	#pragma pack(push,1)
	tagPOINT ptCurrentPos;
	#pragma pack(pop)
	#pragma pack(push,1)
	tagRECT rcArea;
	#pragma pack(pop)
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0005  CANDIDATEFORM;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0006
{
	
public:
	unsigned dwStyle;
	#pragma pack(push,1)
	tagPOINT ptCurrentPos;
	#pragma pack(pop)
	#pragma pack(push,1)
	tagRECT rcArea;
	#pragma pack(pop)
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0006  COMPOSITIONFORM;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0007
{
	
public:
	unsigned dwSize;
	unsigned dwStyle;
	unsigned dwCount;
	unsigned dwSelection;
	unsigned dwPageStart;
	unsigned dwPageSize;
	unsigned dwOffset[1];
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0007  CANDIDATELIST;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0008
{
	
public:
	unsigned dwStyle;
	Shortint szDescription[32];
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0008  STYLEBUFA;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0009
{
	
public:
	unsigned dwStyle;
	Word szDescription[32];
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0009  STYLEBUFW;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0010
{
	
public:
	unsigned cbSize;
	unsigned fType;
	unsigned fState;
	unsigned wID;
	_userHBITMAP *hbmpChecked;
	_userHBITMAP *hbmpUnchecked;
	unsigned dwItemData;
	Shortint szString[80];
	_userHBITMAP *hbmpItem;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0010  IMEMENUITEMINFOA;

#pragma pack(push,1)
struct _userBITMAP
{
	
public:
	int bmType;
	int bmWidth;
	int bmHeight;
	int bmWidthBytes;
	Word bmPlanes;
	Word bmBitsPixel;
	unsigned cbSize;
	Byte *pBuffer;
} ;
#pragma pack(pop)

struct __MIDL_IWinTypes_0007
{
	
	union
	{
		struct 
		{
			_userBITMAP *hRemote;
			
		};
		struct 
		{
			int hInproc;
			
		};
		
	};
} ;

#pragma pack(push,1)
struct _userHBITMAP
{
	
public:
	int fContext;
	__MIDL_IWinTypes_0007 u;
} ;
#pragma pack(pop)

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0011
{
	
public:
	unsigned cbSize;
	unsigned fType;
	unsigned fState;
	unsigned wID;
	_userHBITMAP *hbmpChecked;
	_userHBITMAP *hbmpUnchecked;
	unsigned dwItemData;
	Word szString[80];
	_userHBITMAP *hbmpItem;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0011  IMEMENUITEMINFOW;

struct __MIDL___MIDL_itf_dimm_0000_0013
{
	
	union
	{
		struct 
		{
			__MIDL___MIDL_itf_dimm_0000_0004 W;
			
		};
		struct 
		{
			__MIDL___MIDL_itf_dimm_0000_0003 A;
			
		};
		
	};
} ;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0012
{
	
public:
	_RemotableHandle *hWnd;
	int fOpen;
	#pragma pack(push,1)
	tagPOINT ptStatusWndPos;
	#pragma pack(pop)
	#pragma pack(push,1)
	tagPOINT ptSoftKbdPos;
	#pragma pack(pop)
	unsigned fdwConversion;
	unsigned fdwSentence;
	__MIDL___MIDL_itf_dimm_0000_0013 lfFont;
	#pragma pack(push,1)
	__MIDL___MIDL_itf_dimm_0000_0006 cfCompForm;
	#pragma pack(pop)
	__MIDL___MIDL_itf_dimm_0000_0005 cfCandForm[4];
	unsigned hCompStr;
	unsigned hCandInfo;
	unsigned hGuideLine;
	unsigned hPrivate;
	unsigned dwNumMsgBuf;
	unsigned hMsgBuf;
	unsigned fdwInit;
	unsigned dwReserve[3];
} ;
#pragma pack(pop)

struct __MIDL_IWinTypes_0009
{
	
	union
	{
		struct 
		{
			int hRemote;
			
		};
		struct 
		{
			int hInproc;
			
		};
		
	};
} ;

#pragma pack(push,1)
struct _RemotableHandle
{
	
public:
	int fContext;
	__MIDL_IWinTypes_0009 u;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0012  INPUTCONTEXT;

#pragma pack(push,1)
struct __MIDL___MIDL_itf_dimm_0000_0014
{
	
public:
	unsigned dwPrivateDataSize;
	unsigned fdwProperty;
	unsigned fdwConversionCaps;
	unsigned fdwSentenceCaps;
	unsigned fdwUICaps;
	unsigned fdwSCSCaps;
	unsigned fdwSelectCaps;
} ;
#pragma pack(pop)

typedef __MIDL___MIDL_itf_dimm_0000_0014  IMEINFO;

typedef unsigned UINT_PTR;

typedef int LONG_PTR;

#pragma pack(push,1)
struct tagMSG
{
	
public:
	_RemotableHandle *hWnd;
	unsigned message;
	unsigned wParam;
	int lParam;
	unsigned time;
	#pragma pack(push,1)
	tagPOINT pt;
	#pragma pack(pop)
} ;
#pragma pack(pop)

__interface IEnumRegisterWordA;
typedef System::DelphiInterface<IEnumRegisterWordA> _di_IEnumRegisterWordA;
__interface  INTERFACE_UUID("{08C03412-F96B-11D0-A475-00AA006BCC59}") IEnumRegisterWordA  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Clone(/* out */ _di_IEnumRegisterWordA &ppEnum) = 0 ;
	virtual HRESULT __stdcall Next(unsigned ulCount, /* out */ __MIDL___MIDL_itf_dimm_0000_0001 &rgRegisterWord, /* out */ unsigned &pcFetched) = 0 ;
	virtual HRESULT __stdcall Reset(void) = 0 ;
	virtual HRESULT __stdcall Skip(unsigned ulCount) = 0 ;
};

__interface IEnumRegisterWordW;
typedef System::DelphiInterface<IEnumRegisterWordW> _di_IEnumRegisterWordW;
__interface  INTERFACE_UUID("{4955DD31-B159-11D0-8FCF-00AA006BCC59}") IEnumRegisterWordW  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Clone(/* out */ _di_IEnumRegisterWordW &ppEnum) = 0 ;
	virtual HRESULT __stdcall Next(unsigned ulCount, /* out */ __MIDL___MIDL_itf_dimm_0000_0002 &rgRegisterWord, /* out */ unsigned &pcFetched) = 0 ;
	virtual HRESULT __stdcall Reset(void) = 0 ;
	virtual HRESULT __stdcall Skip(unsigned ulCount) = 0 ;
};

__interface IEnumInputContext;
typedef System::DelphiInterface<IEnumInputContext> _di_IEnumInputContext;
__interface  INTERFACE_UUID("{09B5EAB0-F997-11D1-93D4-0060B067B86E}") IEnumInputContext  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Clone(/* out */ _di_IEnumInputContext &ppEnum) = 0 ;
	virtual HRESULT __stdcall Next(unsigned ulCount, /* out */ unsigned &rgInputContext, /* out */ unsigned &pcFetched) = 0 ;
	virtual HRESULT __stdcall Reset(void) = 0 ;
	virtual HRESULT __stdcall Skip(unsigned ulCount) = 0 ;
};

__interface IActiveIMMRegistrar;
typedef System::DelphiInterface<IActiveIMMRegistrar> _di_IActiveIMMRegistrar;
__interface  INTERFACE_UUID("{B3458082-BD00-11D1-939B-0060B067B86E}") IActiveIMMRegistrar  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall RegisterIME(GUID &rclsid, Word lgid, WideChar * pszIconFile, WideChar * pszDesc) = 0 ;
	virtual HRESULT __stdcall UnregisterIME(GUID &rclsid) = 0 ;
};

__interface IActiveIMMMessagePumpOwner;
typedef System::DelphiInterface<IActiveIMMMessagePumpOwner> _di_IActiveIMMMessagePumpOwner;
__interface  INTERFACE_UUID("{B5CF2CFA-8AEB-11D1-9364-0060B067B86E}") IActiveIMMMessagePumpOwner  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Start(void) = 0 ;
	virtual HRESULT __stdcall End_(void) = 0 ;
	virtual HRESULT __stdcall OnTranslateMessage(tagMSG &pMsg) = 0 ;
	virtual HRESULT __stdcall Pause(/* out */ unsigned &pdwCookie) = 0 ;
	virtual HRESULT __stdcall Resume(unsigned dwCookie) = 0 ;
};

__interface  INTERFACE_UUID("{08C0E040-62D1-11D1-9326-0060B067B86E}") IActiveIMMApp  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall AssociateContext(_RemotableHandle &hWnd, unsigned hIME, /* out */ unsigned &phPrev) = 0 ;
	virtual HRESULT __stdcall ConfigureIMEA(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0001 &pData) = 0 ;
	virtual HRESULT __stdcall ConfigureIMEW(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0002 &pData) = 0 ;
	virtual HRESULT __stdcall CreateContext(/* out */ unsigned &phIMC) = 0 ;
	virtual HRESULT __stdcall DestroyContext(unsigned hIME) = 0 ;
	virtual HRESULT __stdcall EnumRegisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordA &pEnum) = 0 ;
	virtual HRESULT __stdcall EnumRegisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordW &pEnum) = 0 ;
	virtual HRESULT __stdcall EscapeA(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall EscapeW(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall GetCandidateListA(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetCandidateListW(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetCandidateListCountA(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen) = 0 ;
	virtual HRESULT __stdcall GetCandidateListCountW(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen) = 0 ;
	virtual HRESULT __stdcall GetCandidateWindow(unsigned hIMC, unsigned dwIndex, /* out */ __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate) = 0 ;
	virtual HRESULT __stdcall GetCompositionFontA(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0003 &plf) = 0 ;
	virtual HRESULT __stdcall GetCompositionFontW(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0004 &plf) = 0 ;
	virtual HRESULT __stdcall GetCompositionStringA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf) = 0 ;
	virtual HRESULT __stdcall GetCompositionStringW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf) = 0 ;
	virtual HRESULT __stdcall GetCompositionWindow(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm) = 0 ;
	virtual HRESULT __stdcall GetContext(_RemotableHandle &hWnd, /* out */ unsigned &phIMC) = 0 ;
	virtual HRESULT __stdcall GetConversionListA(void * &hKL, unsigned hIMC, char * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetConversionListW(void * &hKL, unsigned hIMC, WideChar * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetConversionStatus(unsigned hIMC, /* out */ unsigned &pfdwConversion, /* out */ unsigned &pfdwSentence) = 0 ;
	virtual HRESULT __stdcall GetDefaultIMEWnd(_RemotableHandle &hWnd, /* out */ wireHWND &phDefWnd) = 0 ;
	virtual HRESULT __stdcall GetDescriptionA(void * &hKL, unsigned uBufLen, char * szDescription, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetDescriptionW(void * &hKL, unsigned uBufLen, WideChar * szDescription, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetGuideLineA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, char * pBuf, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetGuideLineW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, WideChar * pBuf, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetIMEFileNameA(void * &hKL, unsigned uBufLen, char * szFileName, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetIMEFileNameW(void * &hKL, unsigned uBufLen, WideChar * szFileName, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetOpenStatus(unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall GetProperty(void * &hKL, unsigned fdwIndex, /* out */ unsigned &pdwProperty) = 0 ;
	virtual HRESULT __stdcall GetRegisterWordStyleA(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0008 &pStyleBuf, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetRegisterWordStyleW(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0009 &pStyleBuf, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetStatusWindowPos(unsigned hIMC, /* out */ tagPOINT &pptPos) = 0 ;
	virtual HRESULT __stdcall GetVirtualKey(_RemotableHandle &hWnd, /* out */ unsigned &puVirtualKey) = 0 ;
	virtual HRESULT __stdcall InstallIMEA(char * szIMEFileName, char * szLayoutText, /* out */ void * &phKL) = 0 ;
	virtual HRESULT __stdcall InstallIMEW(WideChar * szIMEFileName, WideChar * szLayoutText, /* out */ void * &phKL) = 0 ;
	virtual HRESULT __stdcall IsIME(void * &hKL) = 0 ;
	virtual HRESULT __stdcall IsUIMessageA(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam) = 0 ;
	virtual HRESULT __stdcall IsUIMessageW(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam) = 0 ;
	virtual HRESULT __stdcall NotifyIME(unsigned hIMC, unsigned dwAction, unsigned dwIndex, unsigned dwValue) = 0 ;
	virtual HRESULT __stdcall REGISTERWORDA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister) = 0 ;
	virtual HRESULT __stdcall REGISTERWORDW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister) = 0 ;
	virtual HRESULT __stdcall ReleaseContext(_RemotableHandle &hWnd, unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall SetCandidateWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate) = 0 ;
	virtual HRESULT __stdcall SetCompositionFontA(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0003 &plf) = 0 ;
	virtual HRESULT __stdcall SetCompositionFontW(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0004 &plf) = 0 ;
	virtual HRESULT __stdcall SetCompositionStringA(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen) = 0 ;
	virtual HRESULT __stdcall SetCompositionStringW(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen) = 0 ;
	virtual HRESULT __stdcall SetCompositionWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm) = 0 ;
	virtual HRESULT __stdcall SetConversionStatus(unsigned hIMC, unsigned fdwConversion, unsigned fdwSentence) = 0 ;
	virtual HRESULT __stdcall SetOpenStatus(unsigned hIMC, int fOpen) = 0 ;
	virtual HRESULT __stdcall SetStatusWindowPos(unsigned hIMC, tagPOINT &pptPos) = 0 ;
	virtual HRESULT __stdcall SimulateHotKey(_RemotableHandle &hWnd, unsigned dwHotKeyID) = 0 ;
	virtual HRESULT __stdcall UnregisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szUnregister) = 0 ;
	virtual HRESULT __stdcall UnregisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szUnregister) = 0 ;
	virtual HRESULT __stdcall Activate(int fRestoreLayout) = 0 ;
	virtual HRESULT __stdcall Deactivate(void) = 0 ;
	virtual HRESULT __stdcall OnDefWindowProc(_RemotableHandle &hWnd, unsigned msg, unsigned wParam, int lParam, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall FilterClientWindows(Word &aaClassList, unsigned uSize) = 0 ;
	virtual HRESULT __stdcall GetCodePageA(void * &hKL, /* out */ unsigned &uCodePage) = 0 ;
	virtual HRESULT __stdcall GetLangId(void * &hKL, /* out */ Word &plid) = 0 ;
	virtual HRESULT __stdcall AssociateContextEx(_RemotableHandle &hWnd, unsigned hIMC, unsigned dwFlags) = 0 ;
	virtual HRESULT __stdcall DisableIME(unsigned idThread) = 0 ;
	virtual HRESULT __stdcall GetImeMenuItemsA(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0010 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0010 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetImeMenuItemsW(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0011 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0011 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall EnumInputContext(unsigned idThread, /* out */ _di_IEnumInputContext &ppEnum) = 0 ;
};

__interface IActiveIMMIME;
typedef System::DelphiInterface<IActiveIMMIME> _di_IActiveIMMIME;
__interface  INTERFACE_UUID("{08C03411-F96B-11D0-A475-00AA006BCC59}") IActiveIMMIME  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall AssociateContext(_RemotableHandle &hWnd, unsigned hIME, /* out */ unsigned &phPrev) = 0 ;
	virtual HRESULT __stdcall ConfigureIMEA(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0001 &pData) = 0 ;
	virtual HRESULT __stdcall ConfigureIMEW(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0002 &pData) = 0 ;
	virtual HRESULT __stdcall CreateContext(/* out */ unsigned &phIMC) = 0 ;
	virtual HRESULT __stdcall DestroyContext(unsigned hIME) = 0 ;
	virtual HRESULT __stdcall EnumRegisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordA &pEnum) = 0 ;
	virtual HRESULT __stdcall EnumRegisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordW &pEnum) = 0 ;
	virtual HRESULT __stdcall EscapeA(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall EscapeW(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall GetCandidateListA(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetCandidateListW(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetCandidateListCountA(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen) = 0 ;
	virtual HRESULT __stdcall GetCandidateListCountW(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen) = 0 ;
	virtual HRESULT __stdcall GetCandidateWindow(unsigned hIMC, unsigned dwIndex, /* out */ __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate) = 0 ;
	virtual HRESULT __stdcall GetCompositionFontA(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0003 &plf) = 0 ;
	virtual HRESULT __stdcall GetCompositionFontW(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0004 &plf) = 0 ;
	virtual HRESULT __stdcall GetCompositionStringA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf) = 0 ;
	virtual HRESULT __stdcall GetCompositionStringW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf) = 0 ;
	virtual HRESULT __stdcall GetCompositionWindow(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm) = 0 ;
	virtual HRESULT __stdcall GetContext(_RemotableHandle &hWnd, /* out */ unsigned &phIMC) = 0 ;
	virtual HRESULT __stdcall GetConversionListA(void * &hKL, unsigned hIMC, char * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetConversionListW(void * &hKL, unsigned hIMC, WideChar * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetConversionStatus(unsigned hIMC, /* out */ unsigned &pfdwConversion, /* out */ unsigned &pfdwSentence) = 0 ;
	virtual HRESULT __stdcall GetDefaultIMEWnd(_RemotableHandle &hWnd, /* out */ wireHWND &phDefWnd) = 0 ;
	virtual HRESULT __stdcall GetDescriptionA(void * &hKL, unsigned uBufLen, char * szDescription, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetDescriptionW(void * &hKL, unsigned uBufLen, WideChar * szDescription, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetGuideLineA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, char * pBuf, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetGuideLineW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, WideChar * pBuf, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetIMEFileNameA(void * &hKL, unsigned uBufLen, char * szFileName, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetIMEFileNameW(void * &hKL, unsigned uBufLen, WideChar * szFileName, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetOpenStatus(unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall GetProperty(void * &hKL, unsigned fdwIndex, /* out */ unsigned &pdwProperty) = 0 ;
	virtual HRESULT __stdcall GetRegisterWordStyleA(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0008 &pStyleBuf, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetRegisterWordStyleW(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0009 &pStyleBuf, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall GetStatusWindowPos(unsigned hIMC, /* out */ tagPOINT &pptPos) = 0 ;
	virtual HRESULT __stdcall GetVirtualKey(_RemotableHandle &hWnd, /* out */ unsigned &puVirtualKey) = 0 ;
	virtual HRESULT __stdcall InstallIMEA(char * szIMEFileName, char * szLayoutText, /* out */ void * &phKL) = 0 ;
	virtual HRESULT __stdcall InstallIMEW(WideChar * szIMEFileName, WideChar * szLayoutText, /* out */ void * &phKL) = 0 ;
	virtual HRESULT __stdcall IsIME(void * &hKL) = 0 ;
	virtual HRESULT __stdcall IsUIMessageA(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam) = 0 ;
	virtual HRESULT __stdcall IsUIMessageW(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam) = 0 ;
	virtual HRESULT __stdcall NotifyIME(unsigned hIMC, unsigned dwAction, unsigned dwIndex, unsigned dwValue) = 0 ;
	virtual HRESULT __stdcall REGISTERWORDA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister) = 0 ;
	virtual HRESULT __stdcall REGISTERWORDW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister) = 0 ;
	virtual HRESULT __stdcall ReleaseContext(_RemotableHandle &hWnd, unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall SetCandidateWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate) = 0 ;
	virtual HRESULT __stdcall SetCompositionFontA(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0003 &plf) = 0 ;
	virtual HRESULT __stdcall SetCompositionFontW(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0004 &plf) = 0 ;
	virtual HRESULT __stdcall SetCompositionStringA(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen) = 0 ;
	virtual HRESULT __stdcall SetCompositionStringW(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen) = 0 ;
	virtual HRESULT __stdcall SetCompositionWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm) = 0 ;
	virtual HRESULT __stdcall SetConversionStatus(unsigned hIMC, unsigned fdwConversion, unsigned fdwSentence) = 0 ;
	virtual HRESULT __stdcall SetOpenStatus(unsigned hIMC, int fOpen) = 0 ;
	virtual HRESULT __stdcall SetStatusWindowPos(unsigned hIMC, tagPOINT &pptPos) = 0 ;
	virtual HRESULT __stdcall SimulateHotKey(_RemotableHandle &hWnd, unsigned dwHotKeyID) = 0 ;
	virtual HRESULT __stdcall UnregisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szUnregister) = 0 ;
	virtual HRESULT __stdcall UnregisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szUnregister) = 0 ;
	virtual HRESULT __stdcall GenerateMessage(unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall LockIMC(unsigned hIMC, /* out */ PUserType12 &ppIMC) = 0 ;
	virtual HRESULT __stdcall UnlockIMC(unsigned hIMC) = 0 ;
	virtual HRESULT __stdcall GetIMCLockCount(unsigned hIMC, /* out */ unsigned &pdwLockCount) = 0 ;
	virtual HRESULT __stdcall CreateIMCC(unsigned dwSize, /* out */ unsigned &phIMCC) = 0 ;
	virtual HRESULT __stdcall DestroyIMCC(unsigned hIMCC) = 0 ;
	virtual HRESULT __stdcall LockIMCC(unsigned hIMCC, /* out */ void * &ppv) = 0 ;
	virtual HRESULT __stdcall UnlockIMCC(unsigned hIMCC) = 0 ;
	virtual HRESULT __stdcall ReSizeIMCC(unsigned hIMCC, unsigned dwSize, /* out */ unsigned &phIMCC) = 0 ;
	virtual HRESULT __stdcall GetIMCCSize(unsigned hIMCC, /* out */ unsigned &pdwSize) = 0 ;
	virtual HRESULT __stdcall GetIMCCLockCount(unsigned hIMCC, /* out */ unsigned &pdwLockCount) = 0 ;
	virtual HRESULT __stdcall GetHotKey(unsigned dwHotKeyID, /* out */ unsigned &puModifiers, /* out */ unsigned &puVKey, /* out */ void * &phKL) = 0 ;
	virtual HRESULT __stdcall SetHotKey(unsigned dwHotKeyID, unsigned uModifiers, unsigned uVKey, void * &hKL) = 0 ;
	virtual HRESULT __stdcall CreateSoftKeyboard(unsigned uType, _RemotableHandle &hOwner, int x, int y, /* out */ wireHWND &phSoftKbdWnd) = 0 ;
	virtual HRESULT __stdcall DestroySoftKeyboard(_RemotableHandle &hSoftKbdWnd) = 0 ;
	virtual HRESULT __stdcall ShowSoftKeyboard(_RemotableHandle &hSoftKbdWnd, int nCmdShow) = 0 ;
	virtual HRESULT __stdcall GetCodePageA(void * &hKL, /* out */ unsigned &uCodePage) = 0 ;
	virtual HRESULT __stdcall GetLangId(void * &hKL, /* out */ Word &plid) = 0 ;
	virtual HRESULT __stdcall KeybdEvent(Word lgidIME, Byte bVk, Byte bScan, unsigned dwFlags, unsigned dwExtraInfo) = 0 ;
	virtual HRESULT __stdcall LockModal(void) = 0 ;
	virtual HRESULT __stdcall UnlockModal(void) = 0 ;
	virtual HRESULT __stdcall AssociateContextEx(_RemotableHandle &hWnd, unsigned hIMC, unsigned dwFlags) = 0 ;
	virtual HRESULT __stdcall DisableIME(unsigned idThread) = 0 ;
	virtual HRESULT __stdcall GetImeMenuItemsA(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0010 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0010 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall GetImeMenuItemsW(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0011 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0011 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult) = 0 ;
	virtual HRESULT __stdcall EnumInputContext(unsigned idThread, /* out */ _di_IEnumInputContext &ppEnum) = 0 ;
	virtual HRESULT __stdcall RequestMessageA(unsigned hIMC, unsigned wParam, int lParam, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall RequestMessageW(unsigned hIMC, unsigned wParam, int lParam, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall SendIMCA(_RemotableHandle &hWnd, unsigned uMsg, unsigned wParam, int lParam, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall SendIMCW(_RemotableHandle &hWnd, unsigned uMsg, unsigned wParam, int lParam, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall IsSleeping(void) = 0 ;
};

__interface IActiveIME;
typedef System::DelphiInterface<IActiveIME> _di_IActiveIME;
__interface  INTERFACE_UUID("{6FE20962-D077-11D0-8FE7-00AA006BCC59}") IActiveIME  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Inquire(unsigned dwSystemInfoFlags, /* out */ __MIDL___MIDL_itf_dimm_0000_0014 &pIMEInfo, WideChar * szWndClass, /* out */ unsigned &pdwPrivate) = 0 ;
	virtual HRESULT __stdcall ConversionList(unsigned hIMC, WideChar * szSource, unsigned uFlag, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDest, /* out */ unsigned &puCopied) = 0 ;
	virtual HRESULT __stdcall Configure(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0002 &pRegisterWord) = 0 ;
	virtual HRESULT __stdcall Destroy(unsigned uReserved) = 0 ;
	virtual HRESULT __stdcall Escape(unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult) = 0 ;
	virtual HRESULT __stdcall SetActiveContext(unsigned hIMC, int fFlag) = 0 ;
	virtual HRESULT __stdcall ProcessKey(unsigned hIMC, unsigned uVirKey, unsigned lParam, Byte &pbKeyState) = 0 ;
	virtual HRESULT __stdcall Notify(unsigned hIMC, unsigned dwAction, unsigned dwIndex, unsigned dwValue) = 0 ;
	virtual HRESULT __stdcall Select(unsigned hIMC, int fSelect) = 0 ;
	virtual HRESULT __stdcall SetCompositionString(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen) = 0 ;
	virtual HRESULT __stdcall ToAsciiEx(unsigned uVirKey, unsigned uScanCode, Byte &pbKeyState, unsigned fuState, unsigned hIMC, /* out */ unsigned &pdwTransBuf, /* out */ unsigned &puSize) = 0 ;
	virtual HRESULT __stdcall RegisterWord(WideChar * szReading, unsigned dwStyle, WideChar * szString) = 0 ;
	virtual HRESULT __stdcall UnregisterWord(WideChar * szReading, unsigned dwStyle, WideChar * szString) = 0 ;
	virtual HRESULT __stdcall GetRegisterWordStyle(unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0009 &pStyleBuf, /* out */ unsigned &puBufSize) = 0 ;
	virtual HRESULT __stdcall EnumRegisterWord(WideChar * szReading, unsigned dwStyle, WideChar * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordW &ppEnum) = 0 ;
	virtual HRESULT __stdcall GetCodePageA(/* out */ unsigned &uCodePage) = 0 ;
	virtual HRESULT __stdcall GetLangId(/* out */ Word &plid) = 0 ;
};

__interface IActiveIME2;
typedef System::DelphiInterface<IActiveIME2> _di_IActiveIME2;
__interface  INTERFACE_UUID("{E1C4BF0E-2D53-11D2-93E1-0060B067B86E}") IActiveIME2  : public IActiveIME 
{
	
public:
	virtual HRESULT __stdcall Sleep(void) = 0 ;
	virtual HRESULT __stdcall Unsleep(int fDead) = 0 ;
};

class DELPHICLASS CoCActiveIMM;
class PASCALIMPLEMENTATION CoCActiveIMM : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	/*         class method */ static _di_IActiveIMMApp __fastcall Create(TMetaClass* vmt);
	/*         class method */ static _di_IActiveIMMApp __fastcall CreateRemote(TMetaClass* vmt, const AnsiString MachineName);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall CoCActiveIMM(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~CoCActiveIMM(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCActiveIMM;
class PASCALIMPLEMENTATION TCActiveIMM : public Oleserver::TOleServer 
{
	typedef Oleserver::TOleServer inherited;
	
private:
	_di_IActiveIMMApp FIntf;
	_di_IActiveIMMApp __fastcall GetDefaultInterface();
	
protected:
	virtual void __fastcall InitServerData(void);
	
public:
	__fastcall virtual TCActiveIMM(Classes::TComponent* AOwner);
	__fastcall virtual ~TCActiveIMM(void);
	virtual void __fastcall Connect(void);
	void __fastcall ConnectTo(_di_IActiveIMMApp svrIntf);
	virtual void __fastcall Disconnect(void);
	HRESULT __fastcall AssociateContext(_RemotableHandle &hWnd, unsigned hIME, /* out */ unsigned &phPrev);
	HRESULT __fastcall ConfigureIMEA(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0001 &pData);
	HRESULT __fastcall ConfigureIMEW(void * &hKL, _RemotableHandle &hWnd, unsigned dwMode, __MIDL___MIDL_itf_dimm_0000_0002 &pData);
	HRESULT __fastcall CreateContext(/* out */ unsigned &phIMC);
	HRESULT __fastcall DestroyContext(unsigned hIME);
	HRESULT __fastcall EnumRegisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordA &pEnum);
	HRESULT __fastcall EnumRegisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister, void * &pData, /* out */ _di_IEnumRegisterWordW &pEnum);
	HRESULT __fastcall EscapeA(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult);
	HRESULT __fastcall EscapeW(void * &hKL, unsigned hIMC, unsigned uEscape, void * &pData, /* out */ int &plResult);
	HRESULT __fastcall GetCandidateListA(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetCandidateListW(unsigned hIMC, unsigned dwIndex, unsigned uBufLen, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pCandList, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetCandidateListCountA(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen);
	HRESULT __fastcall GetCandidateListCountW(unsigned hIMC, /* out */ unsigned &pdwListSize, /* out */ unsigned &pdwBufLen);
	HRESULT __fastcall GetCandidateWindow(unsigned hIMC, unsigned dwIndex, /* out */ __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate);
	HRESULT __fastcall GetCompositionFontA(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0003 &plf);
	HRESULT __fastcall GetCompositionFontW(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0004 &plf);
	HRESULT __fastcall GetCompositionStringA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf);
	HRESULT __fastcall GetCompositionStringW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, /* out */ int &plCopied, /* out */ void * &pBuf);
	HRESULT __fastcall GetCompositionWindow(unsigned hIMC, /* out */ __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm);
	HRESULT __fastcall GetContext(_RemotableHandle &hWnd, /* out */ unsigned &phIMC);
	HRESULT __fastcall GetConversionListA(void * &hKL, unsigned hIMC, char * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetConversionListW(void * &hKL, unsigned hIMC, WideChar * pSrc, unsigned uBufLen, unsigned uFlag, /* out */ __MIDL___MIDL_itf_dimm_0000_0007 &pDst, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetConversionStatus(unsigned hIMC, /* out */ unsigned &pfdwConversion, /* out */ unsigned &pfdwSentence);
	HRESULT __fastcall GetDefaultIMEWnd(_RemotableHandle &hWnd, /* out */ wireHWND &phDefWnd);
	HRESULT __fastcall GetDescriptionA(void * &hKL, unsigned uBufLen, char * szDescription, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetDescriptionW(void * &hKL, unsigned uBufLen, WideChar * szDescription, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetGuideLineA(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, char * pBuf, /* out */ unsigned &pdwResult);
	HRESULT __fastcall GetGuideLineW(unsigned hIMC, unsigned dwIndex, unsigned dwBufLen, WideChar * pBuf, /* out */ unsigned &pdwResult);
	HRESULT __fastcall GetIMEFileNameA(void * &hKL, unsigned uBufLen, char * szFileName, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetIMEFileNameW(void * &hKL, unsigned uBufLen, WideChar * szFileName, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetOpenStatus(unsigned hIMC);
	HRESULT __fastcall GetProperty(void * &hKL, unsigned fdwIndex, /* out */ unsigned &pdwProperty);
	HRESULT __fastcall GetRegisterWordStyleA(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0008 &pStyleBuf, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetRegisterWordStyleW(void * &hKL, unsigned nItem, /* out */ __MIDL___MIDL_itf_dimm_0000_0009 &pStyleBuf, /* out */ unsigned &puCopied);
	HRESULT __fastcall GetStatusWindowPos(unsigned hIMC, /* out */ tagPOINT &pptPos);
	HRESULT __fastcall GetVirtualKey(_RemotableHandle &hWnd, /* out */ unsigned &puVirtualKey);
	HRESULT __fastcall InstallIMEA(char * szIMEFileName, char * szLayoutText, /* out */ void * &phKL);
	HRESULT __fastcall InstallIMEW(WideChar * szIMEFileName, WideChar * szLayoutText, /* out */ void * &phKL);
	HRESULT __fastcall IsIME(void * &hKL);
	HRESULT __fastcall IsUIMessageA(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam);
	HRESULT __fastcall IsUIMessageW(_RemotableHandle &hWndIME, unsigned msg, unsigned wParam, int lParam);
	HRESULT __fastcall NotifyIME(unsigned hIMC, unsigned dwAction, unsigned dwIndex, unsigned dwValue);
	HRESULT __fastcall REGISTERWORDA(void * &hKL, char * szReading, unsigned dwStyle, char * szRegister);
	HRESULT __fastcall REGISTERWORDW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szRegister);
	HRESULT __fastcall ReleaseContext(_RemotableHandle &hWnd, unsigned hIMC);
	HRESULT __fastcall SetCandidateWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0005 &pCandidate);
	HRESULT __fastcall SetCompositionFontA(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0003 &plf);
	HRESULT __fastcall SetCompositionFontW(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0004 &plf);
	HRESULT __fastcall SetCompositionStringA(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen);
	HRESULT __fastcall SetCompositionStringW(unsigned hIMC, unsigned dwIndex, void * &pComp, unsigned dwCompLen, void * &pRead, unsigned dwReadLen);
	HRESULT __fastcall SetCompositionWindow(unsigned hIMC, __MIDL___MIDL_itf_dimm_0000_0006 &pCompForm);
	HRESULT __fastcall SetConversionStatus(unsigned hIMC, unsigned fdwConversion, unsigned fdwSentence);
	HRESULT __fastcall SetOpenStatus(unsigned hIMC, int fOpen);
	HRESULT __fastcall SetStatusWindowPos(unsigned hIMC, tagPOINT &pptPos);
	HRESULT __fastcall SimulateHotKey(_RemotableHandle &hWnd, unsigned dwHotKeyID);
	HRESULT __fastcall UnregisterWordA(void * &hKL, char * szReading, unsigned dwStyle, char * szUnregister);
	HRESULT __fastcall UnregisterWordW(void * &hKL, WideChar * szReading, unsigned dwStyle, WideChar * szUnregister);
	HRESULT __fastcall Activate(int fRestoreLayout);
	HRESULT __fastcall Deactivate(void);
	HRESULT __fastcall OnDefWindowProc(_RemotableHandle &hWnd, unsigned msg, unsigned wParam, int lParam, /* out */ int &plResult);
	HRESULT __fastcall FilterClientWindows(Word &aaClassList, unsigned uSize);
	HRESULT __fastcall GetCodePageA(void * &hKL, /* out */ unsigned &uCodePage);
	HRESULT __fastcall GetLangId(void * &hKL, /* out */ Word &plid);
	HRESULT __fastcall AssociateContextEx(_RemotableHandle &hWnd, unsigned hIMC, unsigned dwFlags);
	HRESULT __fastcall DisableIME(unsigned idThread);
	HRESULT __fastcall GetImeMenuItemsA(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0010 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0010 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult);
	HRESULT __fastcall GetImeMenuItemsW(unsigned hIMC, unsigned dwFlags, unsigned dwType, __MIDL___MIDL_itf_dimm_0000_0011 &pImeParentMenu, /* out */ __MIDL___MIDL_itf_dimm_0000_0011 &pImeMenu, unsigned dwSize, /* out */ unsigned &pdwResult);
	HRESULT __fastcall EnumInputContext(unsigned idThread, /* out */ _di_IEnumInputContext &ppEnum);
	__property _di_IActiveIMMApp DefaultInterface = {read=GetDefaultInterface};
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint ActiveIMMMajorVersion = 0x0;
static const Shortint ActiveIMMMinorVersion = 0x1;
extern PACKAGE GUID LIBID_ActiveIMM;
extern PACKAGE GUID IID_IEnumRegisterWordA;
extern PACKAGE GUID IID_IEnumRegisterWordW;
extern PACKAGE GUID IID_IEnumInputContext;
extern PACKAGE GUID IID_IActiveIMMRegistrar;
extern PACKAGE GUID IID_IActiveIMMMessagePumpOwner;
extern PACKAGE GUID IID_IActiveIMMApp;
extern PACKAGE GUID IID_IActiveIMMIME;
extern PACKAGE GUID IID_IActiveIME;
extern PACKAGE GUID IID_IActiveIME2;
extern PACKAGE GUID CLASS_CActiveIMM;

}	/* namespace Activeimm_tlb */
using namespace Activeimm_tlb;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Activeimm_tlb
