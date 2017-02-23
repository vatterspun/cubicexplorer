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
//  The Original Code is Documents.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

Unit Documents;

{###### Tab Page Layout ID's ######}
// FileView Tab = "FileView"
// FileSearch Tab = "FileSearch"


{###### Action ID's ######}

// **** General (100) ****
// 100 = Terminate Application
// 101 = Show/Hide Application
// 102 = Restart Application
// 103 = New instance


// **** Edit (200) ****
// 201 = Copy items to clipboard
// 202 = Cut items to clipboard
// 203 = Paste items from clipboard
// 204 = Delete selected items
// 205 = Select all
// 206 = Invert Selection
// 207 = Show properties of selected items
// 208 = Rename item
// 209 = Duplicate
// 210 = New
// 211 = Copy Path
// 212 = New Folder
// 213 = Paste to shortcut
// 214 = Create Symbolic Link
// 215 = Undo Delete
// 216 = Calculate folder size
// 217 = New Empty File

// **** View (300) ****
// 300 = Statusbar, toggle visibility
// 301 = Folders, toggle visibility.
// 302 = Bookmarks, toggle visibility.
// 303 = Quickview, toggle visibility.
// 304 = Filters, toggle visibility.
// 305 = Drop Stack, toggle visibility.
// 306 = Archiver, toggle visibility.
// 330 = Show Hints
// 331 = Smooth Scroll
// 332 = Show Hidden files
// 333 = Show column header always
// 334 = Show Extensions
// 335 = Always On Top
// 336 = InfoBar, toggle visibility
// 337 = CheckBox Selection
// 351 = The Listview's Large Icon Mode
// 352 = The Listview's Small Icon Mode
// 353 = The Listview's List Mode
// 354 = The Listview's Report (Details) Mode
// 355 = The Listview's Thumbnail Mode
// 356 = The Listview's Tile Mode
// 357 = The Listview's FilmStrip Mode
// 370 = fullscreen
// 371 = Load skin from file.
// 372 = Arrange By
// 373 = View Style
// 374 = Group By
// 375 = Filters menu
// 380 = DualView
// 390 = Lock Toolbars

// **** Tools (400) ****
// 401 = Customize UI
// 451 = Map Network drive
// 452 = Disconnect mapped drive
// 453 = Empty Recycle Bin
// 454 = Open CMD prompt
// 455 = System Power

// **** Help (500) ****
// 501 = Go to Home page
// 502 = Go to support forum
// 503 = About box
// 504 = Translate CE
// 505 = Go to Donate page
// 506 = Version Manager
// 507 = Check For Updates

// **** Navigation (600) ****
// 601 = Add new tab (FileView)
// 602 = Close selected tab
// 603 = Go back in history
// 604 = Go forward in history
// 605 = Folder Up
// 606 = Refresh
// 607 = Dublicate Tab
// 608 = Scroll Left
// 609 = Scroll Right
// 610 = Refresh current folder only.
// 650 = Add Text Editor tab.
// 651 = Add File Search tab.
// 652 = Add QuickView tab.

// **** Tabs (660) ****
// 661 = Close tab under mouse
// 662 = Close other tabs
// 663 = Add Tab
// 664 = Duplicate Tab
// 665 = Close Tabs On Left
// 666 = Close Tabs On Right
// 667 = Undo Close Tab
// 668 = Switch to Next Tab
// 669 = Switch to Previous Tab
// 670 = Tabs list

// **** QuickView (700) ****


// **** Bookmarks (800) ****
// 801 = Add Category
// 802 = Add Bookmark
// 803 = Rename
// 804 = Delete

// **** Sessions (850) ****
// 851 = Save Session
// 852 = Session Manager
// 853 = Add History Point
// 854 = Clear History
// 855 = Auto save history

// **** Misc (900) ****
// 901 = Clear Filters
// 902 = Strict filtering
// 903 = Exclude filtering

// **** Stack (920) ****
// 921 = Open
// 922 = Save
// 923 = Remove from stack
// 924 = Clear list
// 925 = Allow Move

// **** Focus (950) ****
// 951 = Set focus to address bar

// **** Quick Options (1000) ****


interface
implementation
end.
