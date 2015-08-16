@echo off
call "rsvars.bat"
msbuild.exe /target:Build /p:config=Debug /p:Platform=Win32 /p:DCC_Define="ASCONSOLE;USE_JEDI_JCL" SimpleTest.dproj
