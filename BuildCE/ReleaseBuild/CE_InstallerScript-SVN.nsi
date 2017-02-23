;=== pre process files ==========================================



;================================================================
;=== Compression ================================================
  SetCompressor "LZMA"
  SetCompress "auto"

;================================================================
;=== Include ====================================================

  !include "MUI.nsh"
  !include "FileFunc.nsh"


;================================================================
;=== Initialize =================================================

;---Version info---
  !define VERSION "SVN"
  !define VERSION_STR "SVN Snapshot"

;---General info---

  Name "CubicExplorer"
  Caption 'CubicExplorer ${VERSION_STR}'
  OutFile "Output\CubicExplorer_${VERSION}_Setup.exe"
  InstallDir "$PROGRAMFILES\CubicExplorer_dev"

;---Installer file desctiptions---
VIProductVersion "0.0.0.0"
VIAddVersionKey "ProductName" "CubicExplorer"
VIAddVersionKey "ProductVersion" "${VERSION_STR}"
VIAddVersionKey "CompanyName" "CubicReality Software"
;VIAddVersionKey "FileVersion" ""
VIAddVersionKey "FileDescription" "File Manager"
VIAddVersionKey "LegalCopyright" "Marko Savolainen"

;---Reserved Files---
  ReserveFile "ShortcutPage.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;---Variables---
  Var INI_VALUE

;---OnInit---
Function .onInit
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ShortcutPage.ini"
FunctionEnd

;================================================================
;=== Interface Settings =========================================
  ;!define MUI_HEADERIMAGE_BITMAP_NOSTRETCH
  !define MUI_ICON "Installer_Images\install.ico"
  !define MUI_UNICON "Installer_Images\uninstall.ico"
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "Installer_Images\header.bmp"
  !define MUI_BGCOLOR "f0f0f0"
  !define MUI_WELCOMEFINISHPAGE_BITMAP "Installer_Images\finnish.bmp"
  !define MUI_ABORTWARNING


;================================================================
;=== Custom Pages ===============================================

Function SelectShortcutsPage
  !insertmacro MUI_HEADER_TEXT "Create Shortcuts" "Choose what shortcuts you want to create."
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ShortcutPage.ini"
FunctionEnd

;================================================================
;=== Pages ======================================================

  !insertmacro MUI_PAGE_LICENSE "..\Snapshot\CubicExplorer_dev\License.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom SelectShortcutsPage
  !insertmacro MUI_PAGE_INSTFILES


  ; !define MUI_FINISHPAGE_RUN "$INSTDIR\CubicExplorer.exe"
  ; !define MUI_FINISHPAGE_RUN_TEXT "Launch CubicExplorer"
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;================================================================
;=== Languages ==================================================

  !insertmacro MUI_LANGUAGE "English"

;================================================================
;=== Functions ==================================================

Function CreateStartMenuShortcuts
  CreateDirectory "$SMPROGRAMS\CubicExplorer_dev"
  CreateShortCut "$SMPROGRAMS\CubicExplorer_dev\CubicExplorer.lnk" "$INSTDIR\CubicExplorer.exe"
  CreateShortCut "$SMPROGRAMS\CubicExplorer_dev\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
FunctionEnd

Function CreateDesktopShortcuts
  CreateShortCut "$DESKTOP\CubicExplorer_dev.lnk" "$INSTDIR\CubicExplorer.exe"
FunctionEnd

;================================================================
;=== Installer Sections =========================================

Section "Install"

  SetOutPath "$INSTDIR"

  ;---Override files---
  SetOverwrite "on"
  File "..\Snapshot\CubicExplorer_dev\CubicExplorer.exe"
  File "..\Snapshot\CubicExplorer_dev\7z.dll"
  File "..\Snapshot\CubicExplorer_dev\License.txt"
  File "..\Snapshot\CubicExplorer_dev\Readme.txt"
  File /r "..\Snapshot\CubicExplorer_dev\Locale"
  File /r "..\Snapshot\CubicExplorer_dev\Skins"

  ;---Don't override files---
  SetOverwrite "off"
  File "..\Snapshot\CubicExplorer_dev\bookmarks.xml"
  File "..\Snapshot\CubicExplorer_dev\layout.xml"
  File "..\Snapshot\CubicExplorer_dev\settings.xml"
  File "..\Snapshot\CubicExplorer_dev\sessions.xml"
  File "..\Snapshot_configs\Installer\settings.path"

  ;---Create uninstaller---
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ;---Create StartMenu shortcuts---
  !insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ShortcutPage.ini" "Field 1" "State"
  StrCmp $INI_VALUE "1" "" +2
    Call CreateStartMenuShortcuts

  ;---Create Desktop shortcuts---
  !insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ShortcutPage.ini" "Field 2" "State"
  StrCmp $INI_VALUE "1" "" +2
    Call CreateDesktopShortcuts

SectionEnd

;================================================================
;=== Uninstaller Section ========================================

Section "Uninstall"

  ### UnRegister as default file manager
  ReadRegStr $1 HKCR "Folder\shell" ""
  ReadRegStr $2 HKCR "Folder\shell\cubicexplorer" "OldDefaultValue"
  ReadRegStr $3 HKCR "Folder\shell\cubicexplorer\command" ""

  StrCmp $1 "cubicexplorer" unreg_vista_test unreg_done
    unreg_vista_test:
      StrCmp $3 "$\"$INSTDIR\CubicExplorer.exe$\" /shell $\"%1$\"" unreg unreg_xp_test
    unreg_xp_test:
      StrCmp $3 "$\"$INSTDIR\CubicExplorer.exe$\" /idlist,%I,%L" unreg unreg_done
    unreg:
      DeleteRegKey HKCR "Folder\shell\cubicexplorer"
      DetailPrint "Unregistering CubicExplorer as default file manager."
      WriteRegStr HKCR "Folder\shell" "" $2
  unreg_done:

  ### Delete files
  Delete "$SMPROGRAMS\CubicExplorer_dev\CubicExplorer.lnk"
  Delete "$SMPROGRAMS\CubicExplorer_dev\Uninstall.lnk"
  RMDir "$SMPROGRAMS\CubicExplorer_dev"
  Delete "$DESKTOP\CubicExplorer_dev.lnk"

  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\CubicExplorer.exe"
  Delete "$INSTDIR\CubicExplorer.exe.old"
  Delete "$INSTDIR\7z.dll"
  Delete "$INSTDIR\bookmarks.xml"
  Delete "$INSTDIR\settings.xml"
  Delete "$INSTDIR\sessions.xml"
  Delete "$INSTDIR\layout.xml"
  Delete "$INSTDIR\Readme.txt"
  Delete "$INSTDIR\License.txt"
  Delete "$INSTDIR\settings.path"

  RMDir /r "$INSTDIR\Locale"
  RMDir /r "$INSTDIR\Skins"
  RMDir "$INSTDIR"
SectionEnd

;================================================================