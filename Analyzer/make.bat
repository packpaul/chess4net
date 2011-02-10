@ECHO OFF

SET PROJ_DIR=%CD%\..\bin\Chess4Net_Analyzer
SET OPENINGS_DBS_DIR=%PROJ_DIR%\DBs
SET ENGINES_DIR=%PROJ_DIR%\Engines

SET ROBBOLITO_ENGINE=Robbolito_009_w32

SET /P OPENINGS_DBS_REBUILD=Do you want to (re-)build openings DBs (This will take qite a time)? (y/n)

rem -= Clean =-

IF '%OPENINGS_DBS_REBUILD%'=='y' (
  DEL %OPENINGS_DBS_DIR%\*.* /Q
  RD /Q %OPENINGS_DBS_DIR%\
)

DEL %ENGINES_DIR%\*.* /Q
RD /Q %ENGINES_DIR%\

DEL %PROJ_DIR%\*.* /Q
RD /Q %PROJ_DIR%\

rem -= Environment =-

MD %PROJ_DIR%\
MD %ENGINES_DIR%\
MD %OPENINGS_DBS_DIR%\

rem -= Compilation =-

dcc32 -B -R..\res\Delphi -E%PROJ_DIR% Chess4Net_Analyzer.dpr

rem -= Build =-

unzip -o ..\Build\ChessEngines\%ROBBOLITO_ENGINE%.zip -d %ENGINES_DIR%
REN %ENGINES_DIR%\%ROBBOLITO_ENGINE%.exe Robbolito.exe

COPY build\*.* %PROJ_DIR%\ /Y

COPY ReadMe.txt %PROJ_DIR%\ /Y

IF '%OPENINGS_DBS_REBUILD%'=='y' (
  PUSHD ..\PosDB 
  
  CALL make.bat
  CD .\Bases
  CALL ProceedPGNOpenings.bat
  
  MOVE *.mov %OPENINGS_DBS_DIR%\
  MOVE *.pos %OPENINGS_DBS_DIR%\
  
  POPD
)