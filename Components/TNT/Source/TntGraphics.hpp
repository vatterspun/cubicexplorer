// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tntgraphics.pas' rev: 10.00

#ifndef TntgraphicsHPP
#define TntgraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tntgraphics
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTntPicture;
class PASCALIMPLEMENTATION TTntPicture : public Graphics::TPicture 
{
	typedef Graphics::TPicture inherited;
	
public:
	HIDESBASE void __fastcall LoadFromFile(const WideString Filename);
	HIDESBASE void __fastcall SaveToFile(const WideString Filename);
public:
	#pragma option push -w-inl
	/* TPicture.Create */ inline __fastcall TTntPicture(void) : Graphics::TPicture() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPicture.Destroy */ inline __fastcall virtual ~TTntPicture(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall WideCanvasTextRect(Graphics::TCanvas* Canvas, const Types::TRect &Rect, int X, int Y, const WideString Text);
extern PACKAGE void __fastcall WideCanvasTextOut(Graphics::TCanvas* Canvas, int X, int Y, const WideString Text);
extern PACKAGE tagSIZE __fastcall WideDCTextExtent(unsigned hDC, const WideString Text);
extern PACKAGE tagSIZE __fastcall WideCanvasTextExtent(Graphics::TCanvas* Canvas, const WideString Text);
extern PACKAGE int __fastcall WideCanvasTextWidth(Graphics::TCanvas* Canvas, const WideString Text);
extern PACKAGE int __fastcall WideCanvasTextHeight(Graphics::TCanvas* Canvas, const WideString Text);

}	/* namespace Tntgraphics */
using namespace Tntgraphics;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tntgraphics
