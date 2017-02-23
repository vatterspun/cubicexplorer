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
//  The Original Code is CE_Consts.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Consts;

interface

const
  CE_left = 'left';
  CE_top = 'top';
  CE_dockpos = 'dock_pos';
  CE_dockrow = 'dock_row';
  CE_dockedto = 'docked_to';
  CE_pageid = 'page_id';
  CE_floattop = 'float_top';
  CE_floatleft = 'float_left';
  CE_visible = 'visible';
  CE_width = 'width';
  CE_height = 'height';
  CE_size = 'size';
  CE_dockwidth = 'docked_width';
  CE_dockheight = 'docked_height';
  CE_floatwidth = 'float_width';
  CE_floatheight = 'float_height';
  CE_splitwidth = 'split_width';
  CE_splitheight = 'split_height';
  CE_windowstate = 'wndstate';

  CE_item = 'item';
  CE_submenu = 'submenu';
  CE_separator = 'separator';
  CE_action = 'action';
  CE_name = 'name';
  CE_toolbar = 'toolbar';
  CE_panel = 'panel';
  CE_theme = 'theme';

  CE_fileviewer = 'fileviewer';
  CE_viewstyle = 'viewstyle';

//  CE_quickviewer = 'quickviewer';
//  CE_viewtype = 'viewtype';
//  CE_image_exts = 'image_exts';
//  CE_text_exts = 'text_exts';
//  CE_hex_exts = 'hex_exts';

var
  CE_ProxyAddress: String = '';
  CE_UseProxy: Boolean = false;
  CE_UseSystemProxy: Boolean = true;
  CE_ProxyUsername: String = '';
  CE_ProxyPassword: String = '';

implementation

end.
