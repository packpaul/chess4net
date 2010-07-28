SET PROJ_DIR=PosDB

rem -= Clean =-

RMDIR /S /Q ..\bin\%PROJ_DIR%

rem -= Environment =-

MD ..\bin\%PROJ_DIR%

rem -= Compilation =-

dcc32 -B PosDB.dpr

rem -= Build =-

MOVE ..\bin\PosDB.exe ..\bin\%PROJ_DIR%
