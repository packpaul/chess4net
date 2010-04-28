SET PROJ_DIR=IMEServices

rem -= Clean =-

RMDIR /S /Q ..\bin\%PROJ_DIR%

rem -= Environment =-

MD ..\bin\%PROJ_DIR%
MD ..\bin\%PROJ_DIR%\IMEClient1
MD ..\bin\%PROJ_DIR%\IMEClient1\Plugins

rem -= Compilation =-

CD .\IMEServer
dcc32 -B IMEServer.dpr
CD ..\IMEClient
dcc32 -B IMEClient.dpr
CD ..

rem -= Build =-

COPY .\ReadMe.txt ..\bin\%PROJ_DIR%
COPY .\IMEClient\IMEClient.ini ..\bin\%PROJ_DIR%\IMEClient1

MOVE ..\bin\IMEServer.exe ..\bin\%PROJ_DIR%
MOVE ..\bin\IMEClient.exe ..\bin\%PROJ_DIR%\IMEClient1
