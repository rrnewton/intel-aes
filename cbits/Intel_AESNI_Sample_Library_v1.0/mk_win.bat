
@rem arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
set arch=%1
if "%arch%" == "86" goto OK
if "%arch%" == "64" goto OK
echo One arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
goto :ERR

:OK
cd intel_aes_lib
call mk_win_lib%arch%.bat
if ERRORLEVEL 1 goto :ERR
call mk_win_bin%arch%.bat
if ERRORLEVEL 1 goto :ERR
cd ..

cd aes_example
call mk_win%arch%.bat
if ERRORLEVEL 1 goto :ERR
cd ..

cd aes_gladman_subset
call mk_win%arch%.bat
if ERRORLEVEL 1 goto :ERR
cd ..

goto :EOF

:ERR
Echo got an error

