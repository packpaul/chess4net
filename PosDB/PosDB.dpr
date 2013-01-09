////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

program PosDB;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  PGNProcessorUnit in 'PGNProcessorUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  PGNTraverserUnit in '..\PGNTraverserUnit.pas',
  PosBaseCollectorUnit in 'PosBaseCollectorUnit.pas',
  NonRefInterfacedObjectUnit in '..\NonRefInterfacedObjectUnit.pas',
  MoveTreeCollectorUnit in 'MoveTreeCollectorUnit.pas',
  MoveTreeBaseUnit in '..\MoveTreeBaseUnit.pas',
  LoggerUnit in '..\LoggerUnit.pas',
  FileLogAppenderUnit in '..\FileLogAppenderUnit.pas',
  NullLogAppenderUnit in '..\NullLogAppenderUnit.pas';

procedure Help;
begin
  writeln;
  writeln('PosDB version 2013.0');
  writeln('Chess4Net DB builder');
  writeln;
  writeln('PosDB [-V] [-E -U] [-P <name>] [-W|B] [-C <number of plys>] [-O|-X[+] [-S]] [-R <referenced base>] [-T [-G]] [-L <log file>] <input PGN file> [<base>]');
  writeln;
  writeln('-V', #9, 'proceed also variants.');
  writeln('-E', #9, 'change estimation for moves.');
  writeln('-U', #9, 'include only unique positions per game into the move estimation.');
  writeln('-P', #9, 'proceed only positions played by the player <name>.');
  writeln('-W', #9, 'proceed only positions played by white.');
  writeln('-B', #9, 'proceed only positions played by black.');
  writeln('-C', #9, 'include compulsory <number of plys> plys into the base.');
  writeln('-O', #9, 'generate only opening lines.');
  writeln('-X', #9, 'generate extended opening lines.');
  writeln('-X+', #9, 'generate extended opening lines with simple positions.');
  writeln('-S', #9, 'use in opening lines statistical estimation for prunning.');
  writeln('-R', #9, 'use <referenced base> as a base for references.');
  writeln('-T', #9, 'build move tree DB.');
  writeln('-G', #9, 'include only unique games from PGN data.');
  writeln('-L', #9, 'log file.');
end;


procedure Error;
begin
  writeln('ERROR: wrong parameters or input file cannot be opened.');
  writeln;
  Halt(1);
end;

var
  i: integer;
  variants: boolean = FALSE;
  chngest: boolean = FALSE;
  uniquePos: boolean = FALSE;
  color: TFigureColors = [fcWhite, fcBlack];
  player_name: string = '';
  opening: TOpening = openNo;
  statPrunning: boolean = FALSE;
  numPlys: integer = 0;
  refBase: string = '';
  moveTreeDB: boolean = FALSE;
  logFileName: string = '';
  uniqueGames: boolean = FALSE;

begin
  if ParamCount = 0 then
    begin
      Help;
      Halt(0);
    end;
  i := 1;
  repeat
    if UpperCase(ParamStr(i)) = '-V' then
      variants := TRUE
    else
    if UpperCase(ParamStr(i)) = '-E' then
      chngest := TRUE
    else
    if UpperCase(ParamStr(i)) = '-U' then
      uniquePos := TRUE
    else
    if UpperCase(ParamStr(i)) = '-P' then
      begin
        inc(i);
        if i > ParamCount then
          Error;
        player_name := ParamStr(i);
      end
    else
    if UpperCase(ParamStr(i)) = '-W' then
      begin
        if fcWhite in color then
          color := [fcWhite];
      end
    else
    if UpperCase(ParamStr(i)) = '-B' then
      begin
        if fcBlack in color then
          color := [fcBlack];
      end
    else
      if UpperCase(ParamStr(i)) = '-C' then
        begin
          inc(i);
          try
            numPlys := StrToInt(ParamStr(i));
          except
            on EConvertError do
              Error;
          end;
        end
    else
      if UpperCase(ParamStr(i)) = '-O' then
        begin
          if opening = openNo then
            opening := openNormal;
        end
    else
      if UpperCase(ParamStr(i)) = '-X' then
        begin
          if opening = openNo then
            opening := openExtended;
        end
    else
      if UpperCase(ParamStr(i)) = '-X+' then
        begin
          if opening = openNo then
            opening := openExtendedPlus;
        end
    else
      if UpperCase(ParamStr(i)) = '-S' then
        begin
          if (opening = openNo) then
            Error;
          statPrunning := TRUE;
        end
    else
      if UpperCase(ParamStr(i)) = '-R' then
        begin
          inc(i);
          if i > ParamCount then
            Error;
          refBase := ParamStr(i);
        end
    else
      if UpperCase(ParamStr(i)) = '-T' then
        begin
          if i > ParamCount then
            Error;
          moveTreeDB := TRUE;
        end
    else
      if UpperCase(ParamStr(i)) = '-G' then
        begin
          if ((i > ParamCount) or (not moveTreeDB)) then
            Error;
          uniqueGames := TRUE;
        end
    else
      if UpperCase(ParamStr(i)) = '-L' then
        begin
          inc(i);
          if i > ParamCount then
            Error;
          logFileName := ParamStr(i);
        end
    else
      break;
    inc(i);
  until i > ParamCount;

  if i > ParamCount then
    Error;

  AssignFile(input, ParamStr(i));
{$I-}
  Reset(input);
  if IOResult <> 0 then
    Error;
{$I+}

  if (logFileName <> '') then
    TLogger.GetInstance.Appender := TFileLogAppender.Create(logFileName)
  else
    TLogger.GetInstance.Appender := TNullLogAppender.Create;

  TLogger.GetInstance.Info('Processing PGN: ' + ParamStr(i));

  if i = ParamCount then
    TPGNProcessor.Proceed(ChangeFileExt(ParamStr(i), ''), variants, chngest, uniquePos, color, numPlys, player_name, opening, statPrunning, refBase, moveTreeDB, uniqueGames)
  else // i < ParamCount
    TPGNProcessor.Proceed(ChangeFileExt(ParamStr(i + 1), ''), variants, chngest, uniquePos, color, numPlys, player_name, opening, statPrunning, refBase, moveTreeDB, uniqueGames);

  Close(input);
end.
