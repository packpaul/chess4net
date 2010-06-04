rem -= Clean =-

DEL ..\bin\Chess4Net_Skype\*.* /Q
RD /S /Q ..\bin\Chess4Net_Skype\

rem -= Compilation =-

rem dcc32 -DTESTING -B Chess4Net_Skype.dpr
dcc32 -B Chess4Net_Skype.dpr

rem -= Build =-

MD ..\bin\Chess4Net_Skype
MOVE ..\bin\Chess4Net_Skype.exe ..\bin\Chess4Net_Skype\
COPY ..\Build\*.* ..\bin\Chess4Net_Skype\
COPY ..\Lang.ini ..\bin\Chess4Net_Skype\
COPY ..\Readme.txt ..\bin\Chess4Net_Skype\
COPY ..\Readme_RU.txt ..\bin\Chess4Net_Skype\
COPY .\Chess4net.ini ..\bin\Chess4Net_Skype\