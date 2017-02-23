//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is dCE_Images.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit dCE_Images;

interface

uses
  // PNG Controls
  PngImageList,
  // System Units
  SysUtils, Classes, ImgList, Controls, GR32_Image;

type
  TCE_Images = class(TDataModule)
    MediumIcons: TPngImageList;
    SmallIcons: TPngImageList;
    BookmarkImages: TPngImageList;
    MiscImages: TPngImageList;
    QuickViewImages: TPngImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CE_Images: TCE_Images;

implementation

{$R *.dfm}

end.
