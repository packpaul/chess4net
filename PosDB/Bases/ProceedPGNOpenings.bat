SET POS_DB_PATH=..\..\bin\PosDB
SET PGN_PATH=.\PGNs\Openings

rem --------------- Sicilian -----------------------

del Sicilian.pos
del Sicilian.mov

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B21] Sicilian Smith-Morra gambit.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B21] Sicilian Smith-Morra gambit.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B23] Sicilian chameleon variation.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B23] Sicilian chameleon variation.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B23] Sicilian Grand Prix attack.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B23] Sicilian Grand Prix attack.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B30] Sicilian Nimzovich-Rossolimo attack.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B30] Sicilian Nimzovich-Rossolimo attack.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B33] Sicilian Pelikan, Chelyabinsk variation 1.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B33] Sicilian Pelikan, Chelyabinsk variation 1.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B33] Sicilian Pelikan, Chelyabinsk variation 2.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B33] Sicilian Pelikan, Chelyabinsk variation 2.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B53] Sicilian, Chekhover variation.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B53] Sicilian, Chekhover variation.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B60] Sicilian Richter-Rauzer.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B60] Sicilian Richter-Rauzer.PGN" Sicilian

del tmp.pos
del tmp.mov
%POS_DB_PATH%\PosDB.exe -E -U -C 60 %PGN_PATH%\Sicilian\"[B96] SicilianPolugayevsky variation.PGN" tmp
%POS_DB_PATH%\PosDB.exe -E -U -X+ -S -R tmp %PGN_PATH%\Sicilian\"[B96] SicilianPolugayevsky variation.PGN" Sicilian

del tmp.pos
del tmp.mov

rem ------------------------------------------------

PAUSE
