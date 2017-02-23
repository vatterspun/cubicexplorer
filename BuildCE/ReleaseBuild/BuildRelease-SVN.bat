REM upx -9 ..\Snapshot\CubicExplorer_dev\CubicExplorer.exe

DEL Output\*.* /Q
"C:\Program Files (x86)\7-Zip\7z.exe" a -tzip Output\CubicExplorer_SVN.zip ..\Snapshot\CubicExplorer_dev

"C:\Program Files (x86)\NSIS\makensis.exe" CE_InstallerScript-SVN.nsi

ECHO off
ECHO. 
ECHO.
ECHO.
ECHO === ALL DONE ===
ECHO.
pause