
@rem arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
set arch=%1
if "%arch%" == "86" goto OK
if "%arch%" == "64" goto OK
echo One arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
goto :ERR

:OK
if "%arch%" == "86" set sz=32
if "%arch%" == "64" set sz=64

mkdir obj\x%arch%
mkdir bin

cl /O2 -Iinclude -c /Foobj\x%arch%\aessample.obj src\aessample.c 
if ERRORLEVEL 1 goto :ERR

cl /O2 /Zi  -Iinclude -c /Foobj\x%arch%\aessampletiming.obj src\aessampletiming.cpp
if ERRORLEVEL 1 goto :ERR

cl -Iinclude /Febin\aessample%arch%.exe obj\x%arch%\aessample.obj lib\x%arch%\intel_aes%arch%.lib

cl -Iinclude /Febin\aessampletiming%arch%.exe obj\x%arch%\aessampletiming.obj lib\x%arch%\intel_aes%arch%.lib

echo compiled ok
goto :EOF

:ERR 
echo Got an error

