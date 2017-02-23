// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntclasses.pas' rev: 10.00

#ifndef TntclassesHPP
#define TntclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Widestrings.hpp>	// Pascal unit
#include <Activex.hpp>	// Pascal unit
#include <Contnrs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntclasses
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTntStreamCharSet { csAnsi, csUnicode, csUnicodeSwapped, csUtf8 };
#pragma option pop

class DELPHICLASS TTntFileStream;
class PASCALIMPLEMENTATION TTntFileStream : public Classes::THandleStream 
{
	typedef Classes::THandleStream inherited;
	
public:
	__fastcall TTntFileStream(const WideString FileName, Word Mode);
	__fastcall virtual ~TTntFileStream(void);
};


class DELPHICLASS TTntMemoryStream;
class PASCALIMPLEMENTATION TTntMemoryStream : public Classes::TMemoryStream 
{
	typedef Classes::TMemoryStream inherited;
	
public:
	HIDESBASE void __fastcall LoadFromFile(const WideString FileName);
	HIDESBASE void __fastcall SaveToFile(const WideString FileName);
public:
	#pragma option push -w-inl
	/* TMemoryStream.Destroy */ inline __fastcall virtual ~TTntMemoryStream(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTntMemoryStream(void) : Classes::TMemoryStream() { }
	#pragma option pop
	
};


class DELPHICLASS TTntResourceStream;
class PASCALIMPLEMENTATION TTntResourceStream : public Classes::TCustomMemoryStream 
{
	typedef Classes::TCustomMemoryStream inherited;
	
private:
	unsigned HResInfo;
	unsigned HGlobal;
	void __fastcall Initialize(unsigned Instance, WideChar * Name, WideChar * ResType);
	
public:
	__fastcall TTntResourceStream(unsigned Instance, const WideString ResName, WideChar * ResType);
	__fastcall TTntResourceStream(unsigned Instance, Word ResID, WideChar * ResType);
	__fastcall virtual ~TTntResourceStream(void);
	virtual int __fastcall Write(const void *Buffer, int Count);
	HIDESBASE void __fastcall SaveToFile(const WideString FileName);
};


class DELPHICLASS TAnsiStrings;
class PASCALIMPLEMENTATION TAnsiStrings : public Classes::TStrings 
{
	typedef Classes::TStrings inherited;
	
public:
	HIDESBASE void __fastcall LoadFromFile(const WideString FileName);
	HIDESBASE void __fastcall SaveToFile(const WideString FileName);
	void __fastcall LoadFromFileEx(const WideString FileName, unsigned CodePage);
	void __fastcall SaveToFileEx(const WideString FileName, unsigned CodePage);
	virtual void __fastcall LoadFromStreamEx(Classes::TStream* Stream, unsigned CodePage) = 0 ;
	virtual void __fastcall SaveToStreamEx(Classes::TStream* Stream, unsigned CodePage) = 0 ;
public:
	#pragma option push -w-inl
	/* TStrings.Destroy */ inline __fastcall virtual ~TAnsiStrings(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TAnsiStrings(void) : Classes::TStrings() { }
	#pragma option pop
	
};


class DELPHICLASS TAnsiStringsForWideStringsAdapter;
class DELPHICLASS TTntStrings;
class PASCALIMPLEMENTATION TTntStrings : public Widestrings::TWideStrings 
{
	typedef Widestrings::TWideStrings inherited;
	
private:
	TTntStreamCharSet FLastFileCharSet;
	TAnsiStrings* FAnsiStrings;
	void __fastcall SetAnsiStrings(const TAnsiStrings* Value);
	HIDESBASE void __fastcall ReadData(Classes::TReader* Reader);
	void __fastcall ReadDataUTF7(Classes::TReader* Reader);
	void __fastcall ReadDataUTF8(Classes::TReader* Reader);
	void __fastcall WriteDataUTF7(Classes::TWriter* Writer);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	__fastcall TTntStrings(void);
	__fastcall virtual ~TTntStrings(void);
	virtual void __fastcall LoadFromFile(const WideString FileName);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	virtual void __fastcall LoadFromStream_BOM(Classes::TStream* Stream, bool WithBOM);
	virtual void __fastcall SaveToFile(const WideString FileName);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall SaveToStream_BOM(Classes::TStream* Stream, bool WithBOM);
	__property TTntStreamCharSet LastFileCharSet = {read=FLastFileCharSet, nodefault};
	
__published:
	__property TAnsiStrings* AnsiStrings = {read=FAnsiStrings, write=SetAnsiStrings, stored=false};
};


class PASCALIMPLEMENTATION TAnsiStringsForWideStringsAdapter : public TAnsiStrings 
{
	typedef TAnsiStrings inherited;
	
private:
	TTntStrings* FWideStrings;
	unsigned FAdapterCodePage;
	
protected:
	virtual AnsiString __fastcall Get(int Index);
	virtual void __fastcall Put(int Index, const AnsiString S);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	DYNAMIC unsigned __fastcall AdapterCodePage(void);
	
public:
	__fastcall TAnsiStringsForWideStringsAdapter(TTntStrings* AWideStrings, unsigned _AdapterCodePage);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Insert(int Index, const AnsiString S);
	virtual void __fastcall LoadFromStreamEx(Classes::TStream* Stream, unsigned CodePage);
	virtual void __fastcall SaveToStreamEx(Classes::TStream* Stream, unsigned CodePage);
public:
	#pragma option push -w-inl
	/* TStrings.Destroy */ inline __fastcall virtual ~TAnsiStringsForWideStringsAdapter(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTntStringList;
typedef int __fastcall (*TWideStringListSortCompare)(TTntStringList* List, int Index1, int Index2);

class PASCALIMPLEMENTATION TTntStringList : public TTntStrings 
{
	typedef TTntStrings inherited;
	
private:
	bool FUpdating;
	Widestrings::TWideStringItemList *FList;
	int FCount;
	int FCapacity;
	bool FSorted;
	Classes::TDuplicates FDuplicates;
	bool FCaseSensitive;
	Classes::TNotifyEvent FOnChange;
	Classes::TNotifyEvent FOnChanging;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow(void);
	void __fastcall QuickSort(int L, int R, TWideStringListSortCompare SCompare);
	void __fastcall SetSorted(bool Value);
	void __fastcall SetCaseSensitive(const bool Value);
	
protected:
	virtual void __fastcall Changed(void);
	virtual void __fastcall Changing(void);
	virtual WideString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity(void);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	virtual int __fastcall CompareStrings(const WideString S1, const WideString S2);
	virtual void __fastcall InsertItem(int Index, const WideString S, System::TObject* AObject);
	
public:
	__fastcall virtual ~TTntStringList(void);
	virtual int __fastcall Add(const WideString S);
	virtual int __fastcall AddObject(const WideString S, System::TObject* AObject);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const WideString S, int &Index);
	virtual int __fastcall IndexOf(const WideString S);
	virtual int __fastcall IndexOfName(const WideString Name);
	virtual void __fastcall Insert(int Index, const WideString S);
	virtual void __fastcall InsertObject(int Index, const WideString S, System::TObject* AObject);
	virtual void __fastcall Sort(void);
	virtual void __fastcall CustomSort(TWideStringListSortCompare Compare);
	__property Classes::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
public:
	#pragma option push -w-inl
	/* TTntStrings.Create */ inline __fastcall TTntStringList(void) : TTntStrings() { }
	#pragma option pop
	
};


typedef int __fastcall (*TListTargetCompare)(void * Item, void * Target);

class DELPHICLASS TBufferedAnsiString;
class PASCALIMPLEMENTATION TBufferedAnsiString : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	AnsiString FStringBuffer;
	int LastWriteIndex;
	
public:
	void __fastcall Clear(void);
	void __fastcall AddChar(const char wc);
	void __fastcall AddString(const AnsiString s);
	void __fastcall AddBuffer(char * Buff, int Chars);
	AnsiString __fastcall Value();
	char * __fastcall BuffPtr(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TBufferedAnsiString(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TBufferedAnsiString(void) { }
	#pragma option pop
	
};


class DELPHICLASS TBufferedWideString;
class PASCALIMPLEMENTATION TBufferedWideString : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	WideString FStringBuffer;
	int LastWriteIndex;
	
public:
	void __fastcall Clear(void);
	void __fastcall AddChar(const WideChar wc);
	void __fastcall AddString(const WideString s);
	void __fastcall AddBuffer(WideChar * Buff, int Chars);
	WideString __fastcall Value();
	WideChar * __fastcall BuffPtr(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TBufferedWideString(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TBufferedWideString(void) { }
	#pragma option pop
	
};


typedef DynamicArray<Byte >  TntClasses__11;

class DELPHICLASS TBufferedStreamReader;
class PASCALIMPLEMENTATION TBufferedStreamReader : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	Classes::TStream* FStream;
	int FStreamSize;
	DynamicArray<Byte >  FBuffer;
	int FBufferSize;
	int FBufferStartPosition;
	int FVirtualPosition;
	void __fastcall UpdateBufferFromPosition(int StartPos);
	
public:
	__fastcall TBufferedStreamReader(Classes::TStream* Stream, int BufferSize);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Write(const void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TBufferedStreamReader(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


typedef void __fastcall (__closure *TSetAnsiStrEvent)(const AnsiString Value);

class DELPHICLASS TWideComponentHelper;
class PASCALIMPLEMENTATION TWideComponentHelper : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	Classes::TComponent* FComponent;
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TWideComponentHelper(Classes::TComponent* AOwner);
	__fastcall TWideComponentHelper(Classes::TComponent* AOwner, Contnrs::TComponentList* ComponentHelperList);
public:
	#pragma option push -w-inl
	/* TComponent.Destroy */ inline __fastcall virtual ~TWideComponentHelper(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool RuntimeUTFStreaming;
extern PACKAGE void __fastcall TntPersistent_AfterInherited_DefineProperties(Classes::TFiler* Filer, Classes::TPersistent* Instance);
extern PACKAGE TTntStreamCharSet __fastcall AutoDetectCharacterSet(Classes::TStream* Stream);
extern PACKAGE bool __fastcall FindSortedListByTarget(Classes::TList* List, TListTargetCompare TargetCompare, void * Target, int &Index);
extern PACKAGE bool __fastcall ClassIsRegistered(const GUID &clsid);
extern PACKAGE WideString __fastcall GetSyncedWideString(WideString &WideStr, const AnsiString AnsiStr);
extern PACKAGE void __fastcall SetSyncedWideString(const WideString Value, WideString &WideStr, const AnsiString AnsiStr, TSetAnsiStrEvent SetAnsiStr);
extern PACKAGE TWideComponentHelper* __fastcall FindWideComponentHelper(Contnrs::TComponentList* ComponentHelperList, Classes::TComponent* Component);

}	/* namespace Tntclasses */
using namespace Tntclasses;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntclasses
