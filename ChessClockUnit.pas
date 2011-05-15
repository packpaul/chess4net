unit ChessClockUnit;

interface

type
  TChessClock = class
  public
    class function IsZeitnot(const time: TDateTime): boolean;
    class function ConvertToStr(const time: TDateTime): string;
    class function ConvertToFullStr(const time: TDateTime): string;
  end;

implementation

uses
  SysUtils;

const
  FULL_TIME_FORMAT = 'h":"n":"s"."z';
  HOUR_TIME_FORMAT = 'h":"nn":"ss';
  MIN_TIME_FORMAT = 'n":"ss';
  ZEITNOT_FORMAT = 's"."zzz';
  ZEITNOT_BOARDER = 10; // sec. - zeitnot border

////////////////////////////////////////////////////////////////////////////////
//  TChessClock

class function TChessClock.IsZeitnot(const time: TDateTime): boolean;
begin
  Result := ((time > 0) and (time < EncodeTime(0, 0, ZEITNOT_BOARDER, 0)));
end;


class function TChessClock.ConvertToStr(const time: TDateTime): string;
begin
  LongTimeFormat := MIN_TIME_FORMAT;
  if (time >= EncodeTime(1, 0, 0, 0)) then
    LongTimeFormat := HOUR_TIME_FORMAT
  else if (IsZeitnot(time)) then
    LongTimeFormat := ZEITNOT_FORMAT;

  Result := TimeToStr(time);
end;


class function TChessClock.ConvertToFullStr(const time: TDateTime): string;
begin
  LongTimeFormat := FULL_TIME_FORMAT;
  Result := TimeToStr(time);
end;

end.
