SET POS_DB_PATH=..\..\bin\PosDB
SET PGN_PATH=.\PGNs

rem --------------- ECO -----------------------

del eco.pos
del eco.mov
%POS_DB_PATH%\PosDB.exe -E %PGN_PATH%\ECO_Opening_Variations.pgn eco

rem --------------- Tal -----------------------

del tmp.pos
del tmp.mov
copy eco.pos tmp.pos
copy eco.mov tmp.mov

del Tal.pos
del Tal.mov
%POS_DB_PATH%\PosDB.exe -E -U -P "Tal Mikhail (LAT)" %PGN_PATH%\tal.pgn tmp
%POS_DB_PATH%\PosDB.exe -E -U -P "Tal Mikhail (LAT)" -X -S -R tmp %PGN_PATH%\tal.pgn Tal

rem --------------- Fischer -----------------------

del tmp.pos
del tmp.mov
copy eco.pos tmp.pos
copy eco.mov tmp.mov

del Fischer.pos
del Fischer.mov
%POS_DB_PATH%\PosDB.exe -E -U -P "Fischer, Robert James" %PGN_PATH%\fischer.pgn tmp
%POS_DB_PATH%\PosDB.exe -E -U -P "Fischer, Robert James" -X -S -R tmp %PGN_PATH%\fischer.pgn Fischer

rem --------------- Chigorin -----------------------

del tmp.pos
del tmp.mov
copy eco.pos tmp.pos
copy eco.mov tmp.mov

del Chigorin.pos
del Chigorin.mov
%POS_DB_PATH%\PosDB.exe -E -U -P "Chigorin, Mikhail" %PGN_PATH%\Chigorin_Mikhail.pgn tmp
%POS_DB_PATH%\PosDB.exe -E -U -P "Chigorin, Mikhail" -X -S -R tmp %PGN_PATH%\Chigorin_Mikhail.pgn Chigorin

del tmp.pos
del tmp.mov

PAUSE
