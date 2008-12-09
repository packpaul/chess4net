{*************************************}
{                                     }
{       QIP INFIUM SDK                }
{       Copyright(c) Ilham Z.         }
{       ilham@qip.ru                  }
{       http://www.qip.im             }
{                                     }
{*************************************}
{This module is common for core, protos and plugins}

unit u_common;

interface

uses Windows;

type
  {Plugin specific options which will be saved in profile}
  TPluginSpecific = record
    Bool1       : Boolean;
    Bool2       : Boolean;
    Bool3       : Boolean;
    Bool4       : Boolean;
    Bool5       : Boolean;
    Bool6       : Boolean;
    Bool7       : Boolean;
    Bool8       : Boolean;
    Bool9       : Boolean;
    Bool10      : Boolean;
    Param1      : DWord;
    Param2      : DWord;
    Param3      : DWord;
    Param4      : DWord;
    Param5      : DWord;
    Param6      : DWord;
    Param7      : DWord;
    Param8      : DWord;
    Param9      : DWord;
    Param10     : DWord;
    Wstr1       : WideString;
    Wstr2       : WideString;
    Wstr3       : WideString;
    Wstr4       : WideString;
    Wstr5       : WideString;
    Wstr6       : WideString;
    Wstr7       : WideString;
    Wstr8       : WideString;
    Wstr9       : WideString;
    Wstr10      : WideString;
  end;
  pPluginSpecific = ^TPluginSpecific;

  {Adding button below avatar in msg window}
  TAddBtnInfo = record
    BtnIcon : HICON;      //size must be 16x16, if you creating new HICON then after adding button dont forget to destroy it in your plugin.
    BtnPNG  : LongInt;    //size must be 16x16, LongInt(TPngObject) from PngImage library, delphi only. Else have to be 0. After adding button dont forget to destroy it in your plugin.
    BtnHint : WideString; //hint of the button
  end;
  pAddBtnInfo = ^TAddBtnInfo;



implementation

end.
