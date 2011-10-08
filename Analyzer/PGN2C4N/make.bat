rem -= Clean =-

SET PROJ_NAME=PGN2C4N

DEL ..\..\..\bin\%PROJ_NAME%.exe /Q

rem -= Compilation =-

dcc32 -B %PROJ_NAME%.dpr