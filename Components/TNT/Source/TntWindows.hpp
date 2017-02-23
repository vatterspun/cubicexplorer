// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntwindows.pas' rev: 10.00

#ifndef TntwindowsHPP
#define TntwindowsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Shellapi.hpp>	// Pascal unit
#include <Shlobj.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntwindows
{
//-- type declarations -------------------------------------------------------
struct TSHNameMappingHeaderA
{
	
public:
	unsigned cNumOfMappings;
	_SHNAMEMAPPINGA *lpNM;
} ;

typedef TSHNameMappingHeaderA *PSHNameMappingHeaderA;

struct TSHNameMappingHeaderW
{
	
public:
	unsigned cNumOfMappings;
	_SHNAMEMAPPINGW *lpNM;
} ;

typedef TSHNameMappingHeaderW *PSHNameMappingHeaderW;

//-- var, const, procedure ---------------------------------------------------
static const int DT_NOFULLWIDTHCHARBREAK = 0x80000;
static const unsigned INVALID_FILE_ATTRIBUTES = 0xffffffff;
static const char VQV_FIXEDFILEINFO = '\x5c';
#define VQV_VARFILEINFO_TRANSLATION "\\VarFileInfo\\Translation"
#define VQV_STRINGFILEINFO "\\StringFileInfo"
#define VER_COMMENTS "Comments"
#define VER_INTERNALNAME "InternalName"
#define VER_PRODUCTNAME "ProductName"
#define VER_COMPANYNAME "CompanyName"
#define VER_LEGALCOPYRIGHT "LegalCopyright"
#define VER_PRODUCTVERSION "ProductVersion"
#define VER_FILEDESCRIPTION "FileDescription"
#define VER_LEGALTRADEMARKS "LegalTrademarks"
#define VER_PRIVATEBUILD "PrivateBuild"
#define VER_FILEVERSION "FileVersion"
#define VER_ORIGINALFILENAME "OriginalFilename"
#define VER_SPECIALBUILD "SpecialBuild"
extern PACKAGE BOOL __fastcall Tnt_SetWindowTextW(HWND hWnd, WideChar * lpString);
extern PACKAGE BOOL __fastcall Tnt_RemoveDirectoryW(WideChar * lpPathName);
extern PACKAGE unsigned __fastcall Tnt_GetShortPathNameW(WideChar * lpszLongPath, WideChar * lpszShortPath, unsigned cchBuffer);
extern PACKAGE unsigned __fastcall Tnt_GetFullPathNameW(WideChar * lpFileName, unsigned nBufferLength, WideChar * lpBuffer, WideChar * &lpFilePart);
extern PACKAGE unsigned __fastcall Tnt_CreateFileW(WideChar * lpFileName, unsigned dwDesiredAccess, unsigned dwShareMode, Windows::PSecurityAttributes lpSecurityAttributes, unsigned dwCreationDisposition, unsigned dwFlagsAndAttributes, unsigned hTemplateFile);
extern PACKAGE unsigned __fastcall Tnt_FindFirstFileW(WideChar * lpFileName, _WIN32_FIND_DATAW &lpFindFileData);
extern PACKAGE BOOL __fastcall Tnt_FindNextFileW(unsigned hFindFile, _WIN32_FIND_DATAW &lpFindFileData);
extern PACKAGE unsigned __fastcall Tnt_GetFileAttributesW(WideChar * lpFileName);
extern PACKAGE BOOL __fastcall Tnt_SetFileAttributesW(WideChar * lpFileName, unsigned dwFileAttributes);
extern PACKAGE BOOL __fastcall Tnt_CreateDirectoryW(WideChar * lpPathName, Windows::PSecurityAttributes lpSecurityAttributes);
extern PACKAGE BOOL __fastcall Tnt_MoveFileW(WideChar * lpExistingFileName, WideChar * lpNewFileName);
extern PACKAGE BOOL __fastcall Tnt_CopyFileW(WideChar * lpExistingFileName, WideChar * lpNewFileName, BOOL bFailIfExists);
extern PACKAGE BOOL __fastcall Tnt_DeleteFileW(WideChar * lpFileName);
extern PACKAGE int __fastcall Tnt_DrawTextW(HDC hDC, WideChar * lpString, int nCount, Types::TRect &lpRect, unsigned uFormat);
extern PACKAGE BOOL __fastcall Tnt_GetDiskFreeSpaceW(WideChar * lpRootPathName, unsigned &lpSectorsPerCluster, unsigned &lpBytesPerSector, unsigned &lpNumberOfFreeClusters, unsigned &lpTotalNumberOfClusters);
extern PACKAGE BOOL __fastcall Tnt_GetVolumeInformationW(WideChar * lpRootPathName, WideChar * lpVolumeNameBuffer, unsigned nVolumeNameSize, PDWORD lpVolumeSerialNumber, unsigned &lpMaximumComponentLength, unsigned &lpFileSystemFlags, WideChar * lpFileSystemNameBuffer, unsigned nFileSystemNameSize);
extern PACKAGE unsigned __fastcall Tnt_GetModuleFileNameW(unsigned hModule, WideChar * lpFilename, unsigned nSize);
extern PACKAGE unsigned __fastcall Tnt_GetTempPathW(unsigned nBufferLength, WideChar * lpBuffer);
extern PACKAGE unsigned __fastcall Tnt_GetTempFileNameW(WideChar * lpPathName, WideChar * lpPrefixString, unsigned uUnique, WideChar * lpTempFileName);
extern PACKAGE unsigned __fastcall Tnt_GetWindowsDirectoryW(WideChar * lpBuffer, unsigned uSize);
extern PACKAGE unsigned __fastcall Tnt_GetSystemDirectoryW(WideChar * lpBuffer, unsigned uSize);
extern PACKAGE unsigned __fastcall Tnt_GetCurrentDirectoryW(unsigned nBufferLength, WideChar * lpBuffer);
extern PACKAGE BOOL __fastcall Tnt_SetCurrentDirectoryW(WideChar * lpPathName);
extern PACKAGE BOOL __fastcall Tnt_GetComputerNameW(WideChar * lpBuffer, unsigned &nSize);
extern PACKAGE BOOL __fastcall Tnt_GetUserNameW(WideChar * lpBuffer, unsigned &nSize);
extern PACKAGE unsigned __fastcall Tnt_ShellExecuteW(HWND hWnd, WideChar * Operation, WideChar * FileName, WideChar * Parameters, WideChar * Directory, int ShowCmd);
extern PACKAGE unsigned __fastcall Tnt_LoadLibraryW(WideChar * lpLibFileName);
extern PACKAGE unsigned __fastcall Tnt_LoadLibraryExW(WideChar * lpLibFileName, unsigned hFile, unsigned dwFlags);
extern PACKAGE BOOL __fastcall Tnt_CreateProcessW(WideChar * lpApplicationName, WideChar * lpCommandLine, Windows::PSecurityAttributes lpProcessAttributes, Windows::PSecurityAttributes lpThreadAttributes, BOOL bInheritHandles, unsigned dwCreationFlags, void * lpEnvironment, WideChar * lpCurrentDirectory, const Windows::_STARTUPINFOW &lpStartupInfo, _PROCESS_INFORMATION &lpProcessInformation);
extern PACKAGE int __fastcall Tnt_GetCurrencyFormatW(unsigned Locale, unsigned dwFlags, WideChar * lpValue, Windows::PCurrencyFmtW lpFormat, WideChar * lpCurrencyStr, int cchCurrency);
extern PACKAGE int __fastcall Tnt_CompareStringW(unsigned Locale, unsigned dwCmpFlags, WideChar * lpString1, int cchCount1, WideChar * lpString2, int cchCount2);
extern PACKAGE WideChar * __fastcall Tnt_CharUpperW(WideChar * lpsz);
extern PACKAGE unsigned __fastcall Tnt_CharUpperBuffW(WideChar * lpsz, unsigned cchLength);
extern PACKAGE WideChar * __fastcall Tnt_CharLowerW(WideChar * lpsz);
extern PACKAGE unsigned __fastcall Tnt_CharLowerBuffW(WideChar * lpsz, unsigned cchLength);
extern PACKAGE BOOL __fastcall Tnt_GetStringTypeExW(unsigned Locale, unsigned dwInfoType, WideChar * lpSrcStr, int cchSrc, void *lpCharType);
extern PACKAGE int __fastcall Tnt_LoadStringW(unsigned hInstance, unsigned uID, WideChar * lpBuffer, int nBufferMax);
extern PACKAGE BOOL __fastcall Tnt_InsertMenuItemW(HMENU hMenu, unsigned uItem, BOOL fByPosition, const tagMENUITEMINFOW &lpmii);
extern PACKAGE unsigned __fastcall Tnt_ExtractIconExW(WideChar * lpszFile, int nIconIndex, HICON &phiconLarge, HICON &phiconSmall, unsigned nIcons);
extern PACKAGE HICON __fastcall Tnt_ExtractAssociatedIconW(unsigned hInst, WideChar * lpIconPath, Word &lpiIcon);
extern PACKAGE unsigned __fastcall Tnt_GetFileVersionInfoSizeW(WideChar * lptstrFilename, unsigned &lpdwHandle);
extern PACKAGE BOOL __fastcall Tnt_GetFileVersionInfoW(WideChar * lptstrFilename, unsigned dwHandle, unsigned dwLen, void * lpData);
extern PACKAGE BOOL __fastcall Tnt_VerQueryValueW(void * pBlock, WideChar * lpSubBlock, void * &lplpBuffer, unsigned &puLen);
extern PACKAGE int __fastcall Tnt_SHFileOperationW(_SHFILEOPSTRUCTW &lpFileOp);
extern PACKAGE void __fastcall Tnt_SHFreeNameMappings(unsigned hNameMappings);
extern PACKAGE Shlobj::PItemIDList __fastcall Tnt_SHBrowseForFolderW(_browseinfoW &lpbi);
extern PACKAGE BOOL __fastcall Tnt_SHGetPathFromIDListW(Shlobj::PItemIDList pidl, WideChar * pszPath);
extern PACKAGE unsigned __fastcall Tnt_SHGetFileInfoW(WideChar * pszPath, unsigned dwFileAttributes, _SHFILEINFOW &psfi, unsigned cbFileInfo, unsigned uFlags);
extern PACKAGE bool __fastcall Tnt_Is_IntResource(WideChar * ResStr);
extern PACKAGE Word __fastcall LANGIDFROMLCID(unsigned lcid);
extern PACKAGE Word __fastcall MAKELANGID(Word usPrimaryLanguage, Word usSubLanguage);
extern PACKAGE unsigned __fastcall MAKELCID(Word wLanguageID, Word wSortID = (Word)(0x0));
extern PACKAGE Word __fastcall PRIMARYLANGID(Word lgid);
extern PACKAGE Word __fastcall SORTIDFROMLCID(unsigned lcid);
extern PACKAGE Word __fastcall SUBLANGID(Word lgid);

}	/* namespace Tntwindows */
using namespace Tntwindows;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntwindows
