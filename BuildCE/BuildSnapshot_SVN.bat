RD /Q /S Snapshot\CubicExplorer_dev
MD Snapshot\CubicExplorer_dev

copy ..\CubicExplorer.exe Snapshot\CubicExplorer_dev
copy ..\7z.dll Snapshot\CubicExplorer_dev

copy Snapshot_configs\Snapshot\settings.xml Snapshot\CubicExplorer_dev
copy Snapshot_configs\layout.xml Snapshot\CubicExplorer_dev
copy Snapshot_configs\sessions.xml Snapshot\CubicExplorer_dev
copy Snapshot_configs\bookmarks.xml Snapshot\CubicExplorer_dev
copy Snapshot_configs\Portable\settings.path Snapshot\CubicExplorer_dev

copy ..\Documents\Readme.txt Snapshot\CubicExplorer_dev
copy ..\Documents\License.txt Snapshot\CubicExplorer_dev
xcopy ..\locale Snapshot\CubicExplorer_dev\Locale\ /Y /E /EXCLUDE:SnapshotExcludes.txt
xcopy ..\Skins Snapshot\CubicExplorer_dev\Skins\ /Y /E /EXCLUDE:SnapshotExcludes.txt

REM PAUSE