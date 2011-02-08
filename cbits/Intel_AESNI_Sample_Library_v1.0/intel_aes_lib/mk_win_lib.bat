
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

set yasm=..\yasm\yasm-0.8.0-win%sz%.exe

%yasm% -f win%sz% asm\x%arch%\do_rdtsc.s -o obj\x%arch%\do_rdtsc.obj

pushd .
set asm=iaesx%arch%
for %%i in (%asm%) do (
	%yasm% -f win%sz% asm/x%arch%/%%i.s -o obj/x%arch%/%%i.obj
	if ERRORLEVEL 1 goto :ERR
)
cl /O2 /Zi -Iinclude\ -c src\intel_aes.c /Foobj\x%arch%\intel_aes.obj
if ERRORLEVEL 1 goto :ERR
lib /out:lib\x%arch%\intel_aes%arch%.lib obj\x%arch%\*.obj
if ERRORLEVEL 1 goto :ERR
popd

goto :EOF

:ERR
echo got an error in mk_win_lib.bat
