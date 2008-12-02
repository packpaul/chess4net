Unit pluginutil;
interface
uses
 types
 ;

function _int(i:integer):string; overload;
function _int(ints:array of integer):string; overload;
function _byte_at(p:pointer; ofs:integer=0):byte; overload;
function _byte_at(s:string; idx:integer=1):byte; overload;
function _int_at(p:pointer; ofs:integer=0):integer; overload;
function _int_at(s:string; idx:integer=1):integer; overload;
function _ptr_at(p:pointer; ofs:integer=0):pointer;
function _istring(s:string):string;
function _istring_at(p:pointer; ofs:integer=0):string; overload;
function _istring_at(s:string; idx:integer=1):string; overload;
function _intlist(a:array of integer):string;
function _intlist_at(p:pointer; ofs:integer=0):TintegerDynArray; overload;
function _intlist_at(s:string; idx:integer=1):TintegerDynArray; overload;
function _double(p:pointer; ofs:integer=0):double;

implementation
function _int(i:integer):string; overload;
begin
setLength(result, 4);
move(i, result[1], 4);
end; // _int

function _int(ints:array of integer):string; overload;
var
  i:integer;
begin
result:='';
for i:=0 to length(ints)-1 do
  result:=result+_int(ints[i]);
end; // _int

function _byte_at(p:pointer; ofs:integer=0):byte;
begin
inc( integer(p), ofs);
result:=byte(p^)
end;

function _byte_at(s:string; idx:integer=1):byte; overload;
begin result:=_byte_at(@s[idx]) end;

function _int_at(p:pointer; ofs:integer=0):integer; overload;
begin
inc( integer(p), ofs);
result:=integer(p^)
end;

function _int_at(s:string; idx:integer=1):integer; overload;
begin result:=_int_at(@s[idx]) end;

function _ptr_at(p:pointer; ofs:integer=0):pointer;
begin
inc( integer(p), ofs);
result:=pointer(_int_at(p))
end;

function _istring(s:string):string;
begin result:=_int(length(s))+s end;

function _istring_at(p:pointer; ofs:integer=0):string; overload;
begin
inc(integer(p), ofs);
setlength(result, integer(p^));
inc(integer(p), 4);
move(p^, result[1], length(result));
end; // _istring_at

function _istring_at(s:string; idx:integer=1):string; overload;
begin result:=_istring_at(@s[idx]) end;

function _intlist(a:array of integer):string;
begin result:=_int(length(a))+_int(a) end;

function _intlist_at(p:pointer; ofs:integer=0):TintegerDynArray; overload;
var
  n,i:integer;
begin
inc(integer(p), ofs);
n:=integer(p^);
setlength(result, n);
for i:=0 to n-1 do
  begin
  inc(integer(p),4);
  result[i]:=_int_at(p);
  end;
end; // _intlist_at

function _intlist_at(s:string; idx:integer=1):TintegerDynArray; overload;
begin result:=_intlist_at(@s[idx]) end;

function _dt(dt:Tdatetime):string;
begin
setLength(result, 8);
move(dt, result[1], 8);
end; // _dt

function _double(p:pointer; ofs:integer=0):double;
begin
inc(integer(p), ofs);
//setlength(result, integer(p^));
//inc(integer(p), 4);
move(p^, result, 8);
end; // _double


end.
