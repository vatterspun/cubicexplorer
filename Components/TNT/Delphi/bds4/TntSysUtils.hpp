// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntsysutils.pas' rev: 10.00

#ifndef TntsysutilsHPP
#define TntsysutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntsysutils
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ETntUserError;
class PASCALIMPLEMENTATION ETntUserError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ETntUserError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ETntUserError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ETntUserError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ETntUserError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ETntUserError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ETntUserError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ETntUserError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ETntUserError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ETntUserError(void) { }
	#pragma option pop
	
};


class DELPHICLASS ETntGeneralError;
class PASCALIMPLEMENTATION ETntGeneralError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ETntGeneralError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ETntGeneralError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ETntGeneralError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ETntGeneralError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ETntGeneralError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ETntGeneralError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ETntGeneralError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ETntGeneralError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ETntGeneralError(void) { }
	#pragma option pop
	
};


class DELPHICLASS ETntInternalError;
class PASCALIMPLEMENTATION ETntInternalError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ETntInternalError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ETntInternalError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ETntInternalError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ETntInternalError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ETntInternalError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ETntInternalError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ETntInternalError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ETntInternalError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ETntInternalError(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TTntTextLineBreakStyle { tlbsLF, tlbsCRLF, tlbsCR };
#pragma option pop

typedef WideString TWideFileName;

struct TSearchRecW
{
	
public:
	int Time;
	__int64 Size;
	int Attr;
	WideString Name;
	int ExcludeAttr;
	unsigned FindHandle;
	_WIN32_FIND_DATAW FindData;
} ;

//-- var, const, procedure ---------------------------------------------------
static const WideChar CR = WideChar(0xd);
static const WideChar LF = WideChar(0xa);
#define CRLF L"\r\n"
static const WideChar WideLineSeparator = WideChar(0x2028);
extern PACKAGE bool Win32PlatformIsUnicode;
extern PACKAGE bool Win32PlatformIsXP;
extern PACKAGE bool Win32PlatformIs2003;
extern PACKAGE bool Win32PlatformIsVista;
extern PACKAGE unsigned _SettingChangeTime;
extern PACKAGE WideString __fastcall Tnt_WideUpperCase(const WideString S);
extern PACKAGE WideString __fastcall Tnt_WideLowerCase(const WideString S);
extern PACKAGE WideChar __fastcall TntWideLastChar(const WideString S);
extern PACKAGE WideString __fastcall Tnt_WideStringReplace(const WideString S, const WideString OldPattern, const WideString NewPattern, Sysutils::TReplaceFlags Flags, bool WholeWord = false);
extern PACKAGE int __fastcall TntAdjustLineBreaksLength(const WideString S, TTntTextLineBreakStyle Style = (TTntTextLineBreakStyle)(0x1));
extern PACKAGE WideString __fastcall TntAdjustLineBreaks(const WideString S, TTntTextLineBreakStyle Style = (TTntTextLineBreakStyle)(0x1));
extern PACKAGE WideString __fastcall WideWrapText(const WideString Line, const WideString BreakStr, const Sysutils::TSysCharSet &BreakChars, int MaxCol)/* overload */;
extern PACKAGE WideString __fastcall WideWrapText(const WideString Line, int MaxCol)/* overload */;
extern PACKAGE WideString __fastcall WideIncludeTrailingBackslash(const WideString S);
extern PACKAGE WideString __fastcall WideIncludeTrailingPathDelimiter(const WideString S);
extern PACKAGE WideString __fastcall WideExcludeTrailingBackslash(const WideString S);
extern PACKAGE WideString __fastcall WideExcludeTrailingPathDelimiter(const WideString S);
extern PACKAGE bool __fastcall WideIsDelimiter(const WideString Delimiters, const WideString S, int Index);
extern PACKAGE bool __fastcall WideIsPathDelimiter(const WideString S, int Index);
extern PACKAGE int __fastcall WideLastDelimiter(const WideString Delimiters, const WideString S);
extern PACKAGE WideString __fastcall WideChangeFileExt(const WideString FileName, const WideString Extension);
extern PACKAGE WideString __fastcall WideExtractFilePath(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractFileDir(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractFileDrive(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractFileName(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractFileExt(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractRelativePath(const WideString BaseName, const WideString DestName);
extern PACKAGE WideString __fastcall WideExpandFileName(const WideString FileName);
extern PACKAGE WideString __fastcall WideExtractShortPathName(const WideString FileName);
extern PACKAGE int __fastcall WideFileCreate(const WideString FileName);
extern PACKAGE int __fastcall WideFileOpen(const WideString FileName, unsigned Mode);
extern PACKAGE int __fastcall WideFileAge(const WideString FileName)/* overload */;
extern PACKAGE bool __fastcall WideFileAge(const WideString FileName, /* out */ System::TDateTime &FileDateTime)/* overload */;
extern PACKAGE bool __fastcall WideDirectoryExists(const WideString Name);
extern PACKAGE bool __fastcall WideFileExists(const WideString Name);
extern PACKAGE unsigned __fastcall WideFileGetAttr(const WideString FileName);
extern PACKAGE bool __fastcall WideFileSetAttr(const WideString FileName, int Attr);
extern PACKAGE bool __fastcall WideFileIsReadOnly(const WideString FileName);
extern PACKAGE bool __fastcall WideFileSetReadOnly(const WideString FileName, bool ReadOnly);
extern PACKAGE bool __fastcall WideForceDirectories(WideString Dir);
extern PACKAGE WideString __fastcall WideFileSearch(const WideString Name, const WideString DirList);
extern PACKAGE bool __fastcall WideRenameFile(const WideString OldName, const WideString NewName);
extern PACKAGE bool __fastcall WideDeleteFile(const WideString FileName);
extern PACKAGE bool __fastcall WideCopyFile(WideString FromFile, WideString ToFile, bool FailIfExists);
extern PACKAGE int __fastcall WideFindFirst(const WideString Path, int Attr, TSearchRecW &F);
extern PACKAGE int __fastcall WideFindNext(TSearchRecW &F);
extern PACKAGE void __fastcall WideFindClose(TSearchRecW &F);
extern PACKAGE bool __fastcall WideCreateDir(const WideString Dir);
extern PACKAGE bool __fastcall WideRemoveDir(const WideString Dir);
extern PACKAGE WideString __fastcall WideGetCurrentDir();
extern PACKAGE bool __fastcall WideSetCurrentDir(const WideString Dir);
extern PACKAGE bool __fastcall TntTryStrToDateTime(WideString Str, /* out */ System::TDateTime &DateTime);
extern PACKAGE bool __fastcall TntTryStrToDate(WideString Str, /* out */ System::TDateTime &DateTime);
extern PACKAGE bool __fastcall TntTryStrToTime(WideString Str, /* out */ System::TDateTime &DateTime);
extern PACKAGE bool __fastcall ValidDateTimeStr(WideString Str);
extern PACKAGE bool __fastcall ValidDateStr(WideString Str);
extern PACKAGE bool __fastcall ValidTimeStr(WideString Str);
extern PACKAGE System::TDateTime __fastcall TntStrToDateTimeDef(WideString Str, System::TDateTime Default);
extern PACKAGE System::TDateTime __fastcall TntStrToDateDef(WideString Str, System::TDateTime Default);
extern PACKAGE System::TDateTime __fastcall TntStrToTimeDef(WideString Str, System::TDateTime Default);
extern PACKAGE System::TDateTime __fastcall TntStrToDateTime(WideString Str);
extern PACKAGE System::TDateTime __fastcall TntStrToDate(WideString Str);
extern PACKAGE System::TDateTime __fastcall TntStrToTime(WideString Str);
extern PACKAGE WideString __fastcall TntCurrToStr(System::Currency Value, Windows::PCurrencyFmtW lpFormat = (void *)(0x0));
extern PACKAGE System::Currency __fastcall TntStrToCurr(const WideString S);
extern PACKAGE bool __fastcall ValidCurrencyStr(const WideString S);
extern PACKAGE System::Currency __fastcall TntStrToCurrDef(const WideString S, const System::Currency Default);
extern PACKAGE _currencyfmtW __fastcall GetDefaultCurrencyFmt();
extern PACKAGE WideString __fastcall WideGetLocaleStr(unsigned LocaleID, int LocaleType, const WideString Default);
extern PACKAGE WideString __fastcall WideSysErrorMessage(int ErrorCode);
extern PACKAGE WideString __fastcall WideLibraryErrorMessage(const WideString LibName, unsigned Dll, int ErrorCode);
extern PACKAGE unsigned __fastcall WinCheckH(unsigned RetVal);
extern PACKAGE unsigned __fastcall WinCheckFileH(unsigned RetVal);
extern PACKAGE void * __fastcall WinCheckP(void * RetVal);
extern PACKAGE WideString __fastcall WideGetModuleFileName(unsigned Instance);
extern PACKAGE unsigned __fastcall WideSafeLoadLibrary(const WideString Filename, unsigned ErrorMode = (unsigned)(0x8000));
extern PACKAGE unsigned __fastcall WideLoadPackage(const WideString Name);
extern PACKAGE bool __fastcall IsWideCharUpper(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharLower(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharDigit(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharSpace(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharPunct(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharCntrl(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharBlank(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharXDigit(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharAlpha(WideChar WC);
extern PACKAGE bool __fastcall IsWideCharAlphaNumeric(WideChar WC);
extern PACKAGE int __fastcall WideTextPos(const WideString SubStr, const WideString S);
extern PACKAGE WideString __fastcall ExtractStringArrayStr(WideChar * P);
extern PACKAGE WideString __fastcall ExtractStringFromStringArray(WideChar * &P, WideChar Separator = WideChar(0x0));
extern PACKAGE TWideStringDynArray __fastcall ExtractStringsFromStringArray(WideChar * P, WideChar Separator = WideChar(0x0));
extern PACKAGE bool __fastcall IsWideCharMappableToAnsi(const WideChar WC);
extern PACKAGE bool __fastcall IsWideStringMappableToAnsi(const WideString WS);
extern PACKAGE bool __fastcall IsRTF(const WideString Value);
extern PACKAGE WideString __fastcall ENG_US_FloatToStr(Extended Value);
extern PACKAGE Extended __fastcall ENG_US_StrToFloat(const WideString S);

}	/* namespace Tntsysutils */
using namespace Tntsysutils;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntsysutils
