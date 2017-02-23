CD ..\source
"..\BuildCE\Tools\dxgettext\dxgettext" --delphi -o language_files
CD language_files
"..\..\BuildCE\Tools\dxgettext\msgremove" default.po -i ignore.po -o tmp.po
"..\..\BuildCE\Tools\dxgettext\msgcat" tmp.po include.po -o ..\..\Locale\default.pot
DEL tmp.po
CD ..\..\BuildCE

PAUSE