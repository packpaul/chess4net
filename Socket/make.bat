rem -= Clean =-

DEL ..\bin\Chess4Net\*.* /Q
RD /S /Q ..\bin\Chess4Net\

rem -= Compilation =-

dcc32 -B Chess4Net.dpr

rem -= Build =-

MD ..\bin\Chess4Net
MOVE ..\bin\Chess4Net.exe ..\bin\Chess4Net\
COPY ..\Build\*.* ..\bin\Chess4Net\
COPY ..\Lang.ini ..\bin\Chess4Net\
COPY ..\Readme.txt ..\bin\Chess4Net\
COPY ..\Readme_RU.txt ..\bin\Chess4Net\