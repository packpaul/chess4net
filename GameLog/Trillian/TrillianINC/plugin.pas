{¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯}
{                                                                              }
{            Trillian Toolkit Plugin API interface Unit for Delphi             }
{                                                                              }
{ The Original Code is:                                                        }
{  plugin.h, released September 2002.                                          }
{ The Updated v2 Code is:                                                      }
{  plugin.h, released September 2003.                                          }
{ The Updated v3 code is:                                                      }
{  plugin.h, released December 2004.                                           }
{                                                                              }
{ The Original Delphi Code is:                                                 }
{  Plugin.pas, released September 2002.                                        }
{ The Updated v2 Delphi Code is:                                               }
{  Plugin.pas, released September 2003.                                        }
{ The Updated v3 Delphi Code is:                                               }
{  Plugin.pas, released December 2004.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is Cerulean Studios.              }
{ The Initial Developer of the Original Delphi Code is Nico Bendlin.           }
{ The Initial Developer of the Updated Delphi Code is Nathan Laff.             }
{                                                                              }
{ Portions created by Cerulean Studios are                                     }
{  Copyright (C) 1999-2002 Cerulean Studios. All Rights Reserved.              }
{ Portions created by Nico Bendlin are                                         }
{  Copyright (C) 2002 Nico Bendlin. All Rights Reserved.                       }
{ Portions created by Nathan Laff are                                          }
{  Copyright (C) 2003-2004 Nathan Laff. All Rights Reserved.                   }
{                                                                              }
{ Contributor(s):                                                              }
{  Nico Bendlin <nicode@gmx.net>                                               }
{  Thomas Weidenmueller <info@w3seek.de>                                       }
{  Nathan Laff <nlaff@rev23.com>                                               }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of      }
{ either the GNU General Public License Version 2 or later (the "GPL"), or     }
{ the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),     }
{ in which case the provisions of the GPL or the LGPL are applicable instead   }
{ of those above. If you wish to allow use of your version of this file only   }
{ under the terms of either the GPL or the LGPL, and not to allow others to    }
{ use your version of this file under the terms of the MPL, indicate your      }
{ decision by deleting the provisions above and replace them with the notice   }
{ and other provisions required by the GPL or the LGPL. If you do not delete   }
{ the provisions above, a recipient may use your version of this file under    }
{ the terms of any one of the MPL, the GPL or the LGPL.                        }
{______________________________________________________________________________}
{¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯}
{ Revision:                                                                    }
{                                                                              }
{  2002-09-12  1.0  Nico Bendlin [NicoDE]                                      }
{                   Initial release                                            }
{                                                                              }
{  2003-08-01  2.0  Nathan Laff [GwaRGuITaR]                                   }
{                   Updated for SDKv2 and Trillian Pro v2.0                    }
{                                                                              }
{  2004-01-30 2.01  Nathan Laff [GwaRGuITaR]                                   }
{                   Fixed struct_size of alert_t, skin_control_t, and          }
{                   skin_component_t.                                          }
{                   Fixed skin_enum_t.search_data to be pointer. Not PAnsiChar }
{                                                                              }
{  2004-03-08 2.02  Nathan Laff [GwaRGuITaR]                                   }
{                   Fixed plugin_external_t.data to be Pointer                 }
{                   Fixed list_bmp_t.skin_based to be PInteger                 }
{                                                                              }
{  2004-12-17  3.0  Nathan Laff [GwaRGuITaR]                                   }
{                   Updated for SDKv3 and Trillian Pro v3.0                    }
{                   Minor corrections to some type properties and names        }
{ 									                                                           }
{  2006-01-16 3.01  Nathan Laff [GwaRGuITaR]                                   }
{		                Fixed allows_contact_adding in medium_entry_t 	           }
{	                  Thanks quietbritishjim 			                               }
{                                                                              }
{  2006-05-22 3.02  Ingmar Runge [KingIR]                                      }
{                   Fixed name of skin_control_setting*s*_t                    }
{                   (it's actually skin_control_setting_t in the plugin.h file)}
{                   Added inline; keyword for trillianInitialize functions     }
{                   (for Delphi 2005, 2006 and up)                             }
{                                                                              }
{  2006-07-08 3.03  Ingmar Runge [KingIR]                                      }
{                   Fixed name of skin_control_emoti*c*on_t                    }
{______________________________________________________________________________}

unit plugin;

{$WEAKPACKAGEUNIT}

{$IFDEF CONDITIONALEXPRESSIONS}
  // check whether our Delphi compiler supports the inline keyword
  {$IF System.CompilerVersion >= 17}
    {$DEFINE INLINEFUNCS}
  {$IFEND}
{$ENDIF}

interface

{$HPPEMIT ''}
{$HPPEMIT '#include "plugin.h"'}
{$HPPEMIT ''}

uses
  Windows, SysUtils;

type                          
  ttkCallback = function(
    WindowID : Integer;
    SubWindow: PAnsiChar;
    Event    : PAnsiChar;
    Data     : Pointer;
    UserData : Pointer
    ): Integer; cdecl;
  {$EXTERNALSYM ttkCallback}
  TFNTtkCallback = ttkCallback;

type
  plugin_function = function(
    Guid : PAnsiChar;
    Event: PAnsiChar;
    Data : Pointer
    ): Integer; cdecl;
  {$EXTERNALSYM plugin_function}
  TFNTtkPluginFunction = plugin_function;

  plugin_function_send = function(
    Guid : PAnsiChar;
    Event: PAnsiChar;
    Data : Pointer
    ): Integer; cdecl;
  {$EXTERNALSYM plugin_function_send}
  TFNTtkPluginFunctionSend = plugin_function_send;

  plugin_function_main = function(
    Event: PAnsiChar;
    Data : Pointer
    ): Integer; cdecl;
  {$EXTERNALSYM plugin_function_main}
  TFNTtkPluginFunctionMain = plugin_function_main;

  plugin_function_version = function(): Integer; cdecl;
  {$EXTERNALSYM plugin_function_version}
  TFNTtkPluginFunctionVersion = plugin_function_version;

const
  TRILLIAN_PREFS_CANCEL = 0;
  {$EXTERNALSYM TRILLIAN_PREFS_CANCEL}
  TRILLIAN_PREFS_APPLY  = 1;
  {$EXTERNALSYM TRILLIAN_PREFS_APPLY}
  TRILLIAN_PREFS_OK     = 2;
  {$EXTERNALSYM TRILLIAN_PREFS_OK}

  TRILLIAN_PREFS_SHOW   = 1;
  {$EXTERNALSYM TRILLIAN_PREFS_SHOW}
  TRILLIAN_PREFS_HIDE   = 0;
  {$EXTERNALSYM TRILLIAN_PREFS_HIDE}

const
  MENU_TEXT             = 0;
  {$EXTERNALSYM MENU_TEXT}
  MENU_SEPARATOR        = 1;
  {$EXTERNALSYM MENU_SEPARATOR}
  MENU_POPUP            = 2;
  {$EXTERNALSYM MENU_POPUP}
  MENU_HEADING          = 3;
  {$EXTERNALSYM MENU_HEADING}
  MENU_CALLBACK         = 4;
  {$EXTERNALSYM MENU_CALLBACK}
  MENU_DEFAULT          = 5;
  {$EXTERNALSYM MENU_DEFAULT}

const
  CONTACT_WIZARD_OVERRIDE_GROUPS = $0001;

const
  MENU_TYPE_RIGHTCLICK  = 0;
  {$EXTERNALSYM MENU_TYPE_RIGHTCLICK}
  MENU_TYPE_BAR 	      = 1;
  {$EXTERNALSYM MENU_TYPE_BAR}

  MENU_TIMEOUT_DEFAULT  = 0;
  {$EXTERNALSYM MENU_TIMEOUT_DEFAULT}
  MENU_TIMEOUT_INFINITE = -1;
  {$EXTERNALSYM MENU_TIMEOUT_INFINITE}

const
  EVENTS_STATUS_UNKNOWN   = 0;
  {$EXTERNALSYM EVENTS_STATUS_UNKNOWN}
  EVENTS_STATUS_AWAY      = 1;
  {$EXTERNALSYM EVENTS_STATUS_AWAY}
  EVENTS_STATUS_BACK      = 2;
  {$EXTERNALSYM EVENTS_STATUS_BACK}
  EVENTS_STATUS_IDLE	    = 3;
  {$EXTERNALSYM EVENTS_STATUS_IDLE}
  EVENTS_STATUS_HIDDEN    = 4;
  {$EXTERNALSYM EVENTS_STATUS_HIDDEN}
  EVENTS_STATUS_INVISIBLE = 5;
  {$EXTERNALSYM EVENTS_STATUS_INVISIBLE}

const
  EVENTS_START          = 0;
  {$EXTERNALSYM EVENTS_START}
  EVENTS_EXECUTE        = 1;
  {$EXTERNALSYM EVENTS_EXECUTE}
  EVENTS_END            = 2;
  {$EXTERNALSYM EVENTS_END}

type
  PTtkPluginInfo = ^TTtkPluginInfo;
  plugin_info_t = record
    struct_size            : Cardinal;

    // Information given to the plugin
    config_directory       : PAnsiChar;
    skin_directory         : PAnsiChar;
    temp_directory         : PAnsiChar;

    plugin_send            : plugin_function_send;

    // Plugin Fills out this information
    guid                   : array[0..127] of AnsiChar;

    name                   : array[0..255] of AnsiChar;
    company                : array[0..255] of AnsiChar;
    version                : array[0..63] of AnsiChar;
    description            : array[0..1023] of AnsiChar;

    // 2.0 information: given to the plugin
    global_config_directory: PAnsiChar;
    trillian_directory     : PAnsiChar;
    language_directory     : PAnsiChar;

    // 3.0 information: more information for the plugin to fill out
    png_image_48            : PByteArray;
    png_image_48_len        : Cardinal;

    png_image_32            : PByteArray;
    png_image_32_len        : Cardinal;
  end;
  {$EXTERNALSYM plugin_info_t}
  TTtkPluginInfo = plugin_info_t;

type
  PTtkAlert = ^TTtkAlert;
  alert_t = record
    struct_size: Cardinal;

    text       : PAnsiChar;
    _type      : PAnsiChar;

    word_wrap  : Integer;
    formatted  : Integer;
    link       : Integer;

    callback   : ttkCallback;
    data       : Pointer;

    // 2.0
    persistent : Integer;
  end;
  {$EXTERNALSYM alert_t}
  TTtkAlert = alert_t;

type
  PTtkPluginPrefsInfo = ^TTtkPluginPrefsInfo;
  plugin_prefs_info_t = record
    struct_size: Cardinal;

    name       : array[0..127] of AnsiChar;
    description: array[0..1023] of AnsiChar;

    bitmap     : HBITMAP;
  end;
  {$EXTERNALSYM plugin_prefs_info_t}
  TTtkPluginPrefsInfo = plugin_prefs_info_t;

type
  PTtkPluginPrefsShow = ^TTtkPluginPrefsShow;
  plugin_prefs_show_t = record
    struct_size: Cardinal;

    show       : Integer;  // 1 shows, 0 hides

    pref_name  : PAnsiChar;
    sub_entry  : PAnsiChar;

    hwnd       : HWND;

    x          : Integer;
    y          : Integer;
    prefs_info : plugin_prefs_info_t;
  end;
  {$EXTERNALSYM plugin_prefs_show_t}
  TTtkPluginPrefsShow = plugin_prefs_show_t;

type
  PTtkPluginPrefsAction = ^TTtkPluginPrefsAction;
  plugin_prefs_action_t = record

    struct_size: Cardinal;

    _type      : Integer;  // 0 = cancel, 1 = apply, 2 = ok
  end;
  {$EXTERNALSYM plugin_prefs_action_t}
  TTtkPluginPrefsAction = plugin_prefs_action_t;

type
  PTtkPluginPrefsEntry = ^TTtkPluginPrefsEntry;
  plugin_prefs_entry_t = record
    struct_size: Cardinal;

    sub_name   : PAnsiChar;

    next       : Pointer;  // ^plugin_prefs_entry_t;
  end;
  {$EXTERNALSYM plugin_prefs_entry_t}
  TTtkPluginPrefsEntry = plugin_prefs_entry_t;

type
  PTtkPluginPrefs = ^TTtkPluginPrefs;
  plugin_prefs_t = record
    struct_size: Cardinal;

    enabled    : Integer;

    pref_name  : PAnsiChar;

    sub_entry  : ^plugin_prefs_entry_t;
  end;
  {$EXTERNALSYM plugin_prefs_t}
  TTtkPluginPrefs = plugin_prefs_t;

type
  PTtkAlias = ^TTtkAlias;
  alias_t = record
    struct_size: Cardinal;

    alias_id   : Integer;

    text       : PAnsiChar;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM alias_t}
  TTtkAlias = alias_t;

type
  PTtkAliasRequest = ^TTtkAliasRequest;
  alias_request_t = record
    struct_size   : Cardinal;

    alias_id      : Integer;

    alias         : PAnsiChar;
    text          : PAnsiChar;

    // 2.0
    formatted_text: PAnsiChar;
    medium        : PAnsiChar;
    connection_id : Integer;
    window_id     : Integer;
   end;
  {$EXTERNALSYM alias_request_t}
  TTtkAliasRequest = alias_request_t;

type
  PTtkKeyboardAlias = ^TTtkKeyboardAlias;
  keyboard_alias_t = record
    struct_size: Cardinal;

    alias_id   : Integer;

    alt        : Integer;
    ctrl       : Integer;
    shift      : Integer;

    key        : AnsiChar;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM keyboard_alias_t}
  TTtkKeyboardAlias = keyboard_alias_t;

type
  PTtkKeyboardAliasRequest = ^TTtkKeyboardAliasRequest;
  keyboard_alias_request_t = record
    struct_size: Cardinal;

    alias_id      : Integer;

    alt           : Integer;
    ctrl          : Integer;
    shift         : Integer;

    key           : AnsiChar;

    text          : PAnsiChar;  // To be inserted... if none, make this nil

    // 2.0
    medium        : PAnsiChar;
    connection_id : Integer;  
    window_id     : Integer;
  end;
  {$EXTERNALSYM keyboard_alias_request_t}
  TTtkKeyboardAliasRequest = keyboard_alias_request_t;

type
  PTtkListBmp = ^TTtkListBmp;
  list_bmp_t = record
    struct_size: Cardinal;

    num_states : Integer;

    bitmap     : ^HBITMAP;
    location   : PRect;

    // Animated support not available currently
    animated   : Integer;
    time       : Integer;
    loop       : Integer;
    cur_state  : Integer;

    // Skin based references
    skin_based : PInteger;

    skin_name  : ^PAnsiChar;
    skin_type  : ^PAnsiChar;
    skin_state : ^PAnsiChar;
  end;
  {$EXTERNALSYM list_bmp_t}
  TTtkListBmp = list_bmp_t;

type
  PTtkListFont = ^TTtkListFont;
  list_font_t = record
    struct_size      : Cardinal;

    flags            : Integer;  // $0001 skins, $0010 font file not the other stuff

    skin_name        : PAnsiChar;

    font             : HFONT;

    // Skin based
    hover_fore       : PAnsiChar;
    hover_back       : PAnsiChar;
    select_fore      : PAnsiChar;
    select_back      : PAnsiChar;
    normal_fore      : PAnsiChar;
    normal_back      : PAnsiChar;

    // Nonskin based
    hover_background : COLORREF;
    hover_foreground : COLORREF;
    select_background: COLORREF;
    select_foreground: COLORREF;
    background       : COLORREF;
    foreground       : COLORREF;
  end;
  {$EXTERNALSYM list_font_t}
  TTtkListFont = list_font_t;

type
  PTtkMenuEntry = ^TTtkMenuEntry;
  menu_entry_t = record
    struct_size: Cardinal;

    menu_id    : Integer;
    sub_menu_id: Integer;
    _type      : Integer;

    timeout    : Integer;
    disabled   : Integer;
    num_copies : Integer;

    icon       : PAnsiChar;
    text       : PAnsiChar;
    shortcut   : PAnsiChar;

    data       : Pointer;
    hwnd       : HWND;

    sub_menu   : Pointer;  // ^menu_entry_t;
    next_menu  : Pointer;  // ^menu_entry_t;

    callback   : ttkCallback;
  end;
  {$EXTERNALSYM menu_entry_t}
  TTtkMenuEntry = menu_entry_t;

type
  PTtkListEntry = ^TTtkListEntry;
  list_entry_t = record
    struct_size       : Cardinal;

    num_left_icons    : Integer;
    left_icons        : ^list_bmp_t;

    num_right_icons   : Integer;
    right_icons       : ^list_bmp_t;

    font              : list_font_t;

    section_id        : Integer;
    parent_id         : Integer;
    previous_id       : Integer;
    unique_id         : Integer;

    group             : Integer;
    section           : Integer;

    drag_and_drop     : Integer;
    inline_editing    : Integer;
    selectable        : Integer;
    expanded          : Integer;
    auto_expand       : Integer;
    no_group_icon     : Integer;
    no_double_click   : Integer;
    owner_drawn       : Integer;
    no_section_margins: Integer;

    floating          : Integer;
    floating_x        : Integer;
    floating_y        : Integer;

    tooltip           : PAnsiChar;
    text              : PAnsiChar;
    edit_string       : PAnsiChar;
    real_name         : PAnsiChar;

    data              : Pointer;

    menu_entry        : ^menu_entry_t;

    callback          : ttkCallback;

    // 2.0
    inline_editing_quick : Integer;

    // 3.0
    icon_mode         : Integer;
    icon_width        : Integer;
    icon_height       : Integer;

    icon_name         : PAnsiChar;
    panel             : PAnsiChar;
  end;
  {$EXTERNALSYM list_entry_t}
  TTtkListEntry = list_entry_t;

type
  PTtkDialogEntry = ^TTtkDialogEntry;
  dialog_entry_t = record
    struct_size: Cardinal;

    hwnd       : HWND;
  end;
  {$EXTERNALSYM dialog_entry_t}
  TTtkDialogEntry = dialog_entry_t;

type
  PTtkListUpdate = ^TTtkListUpdate;
  list_update_t = record
    struct_size      : Cardinal;

    hdc              : HDC;
    hwnd             : HWND;

    x                : Integer;
    y                : Integer;
    y_offset         : Integer;

    width            : Integer;
    height           : Integer;

    // Colors
    hover_background : COLORREF;
    hover_foreground : COLORREF;
    hover_border     : COLORREF;
    select_background: COLORREF;
    select_foreground: COLORREF;
    select_border    : COLORREF;
    background       : COLORREF;
    foreground       : COLORREF;

    // State
    selected         : Integer;
    hover            : Integer;
    floating         : Integer;
    black_and_white  : Integer;
  end;
  {$EXTERNALSYM list_update_t}
  TTtkListUpdate = list_update_t;

type
  PTtkConnectionEnum = ^TTtkConnectionEnum;
  connection_enum_t = record
    struct_size: Cardinal;

    medium     : PAnsiChar;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM connection_enum_t}
  TTtkConnectionEnum = connection_enum_t;

type
  PTtkConnectionEntry = ^TTtkConnectionEntry;
  connection_entry_t = record
    struct_size  : Cardinal;

    medium       : PAnsiChar;

    connection_id: Integer;

    name         : PAnsiChar;
    status       : PAnsiChar;

    // 2.0
    section      : PAnsiChar;
    menu_entry   : ^menu_entry_t;

    // 3.0
    status_warned     : Integer;
    status_time       : Integer;
    status_idle_time  : Integer;

    status_message    : PAnsiChar;
  end;
  {$EXTERNALSYM connection_entry_t}
  TTtkConnectionEntry = connection_entry_t;

type
  PTtkContactListEnum = ^TTtkContactListEnum;
  contactlist_enum_t = record
    struct_size  : Cardinal;

    medium       : PAnsiChar;

    connection_id: Integer;

    callback     : ttkCallback;
    data         : Pointer;

    // 2.0
    section      : PAnsiChar;
  end;
  {$EXTERNALSYM contactlist_enum_t}
  TTtkContactListEnum = contactlist_enum_t;

// 2.0 group_entry_t (moved to accomadate delphi)
type
  PTtkGroupEntry = ^TTtkGroupEntry;
  group_entry_t = record
    struct_size : Cardinal;

    name        : PAnsiChar;

    child       : Pointer; // group_entry_t

    // 3.0
    is_metacontact : Integer;
  end;
  {$EXTERNALSYM group_entry_t}
  TTtkGroupEntry = group_entry_t;

type
  PTtkContactListEntry = ^TTtkContactListEntry;
  contactlist_entry_t = record
    struct_size  : Cardinal;

    medium       : PAnsiChar;

    connection_id: Integer;

    name         : PAnsiChar;
    real_name    : PAnsiChar;
    status       : PAnsiChar;

    // 2.0 enhancement
    section      : PAnsiChar;
    uri          : PAnsiChar;
    tooltip      : PAnsiChar;

    status_score : Integer;
    status_idle  : Integer;
    status_block : Integer;

    group        : ^group_entry_t;
    menu_entry   : ^menu_entry_t;

    callback     : ttkCallback;
    data         : Pointer;
  end;
  {$EXTERNALSYM contactlist_entry_t}
  TTtkContactListEntry = contactlist_entry_t;

type
  PTtkMessage = ^TTtkMessage;
  message_t = record
    struct_size           : Cardinal;

    medium                : PAnsiChar;
    connection_id         : Integer;

    name                  : PAnsiChar;
    _type                 : PAnsiChar;
    text                  : PAnsiChar;

    location              : PAnsiChar;

    extra_information     : Pointer;

    // 2.0
    window_id             : Integer;
    extra_information_size: Cardinal;
    time_stamp            : Cardinal;

    display_name          : PAnsiChar;
    section		            : PAnsiChar;
  end;
  {$EXTERNALSYM message_t}
  TTtkMessage = message_t;

type
  PTtkMessageBroadcast = ^TTtkMessageBroadcast;
  message_broadcast_t = record
    struct_size : Cardinal;

    broadcast_id: Integer;

    callback    : ttkCallback;
    data        : Pointer;
  end;
  {$EXTERNALSYM message_broadcast_t}
  TTtkMessageBroadcast = message_broadcast_t;

// 2.0 STRUCTURES - NATHAN LAFF

type
  PTtkMediumEntry = ^TTtkMediumEntry;
  medium_entry_t = record
    scruct_size             : Cardinal;

    medium                  : PAnsiChar;
    description             : PAnsiChar;

    allows_accounts         : Integer;
    allows_connections      : Integer;
    allows_contacts         : Integer;
    allows_contact_importing: Integer;
    allows_message_sending  : Integer;
    allows_message_receiving: Integer;
    allows_file_sending     : Integer;
    allows_file_reveiving   : Integer;
    allows_massmessaging    : Integer;

    callback                : ttkCallback;
    data                    : Pointer;

    // 3.0
    allows_account_editing  : Integer;
    allows_account_removing : Integer;
    allows_contact_adding   : Integer;

    png_image_32            : PByteArray;
    png_image_32_len        : Cardinal;

    png_image_16            : PByteArray;
    png_image_16_len        : Cardinal;
  end;
  {$EXTERNALSYM medium_entry_t}
  TTtkMediumEntry = medium_entry_t;

type
  PTtkSkinEntry = ^TTtkSkinEntry;
  skin_entry_t = record
    struct_size : Cardinal;

    xml_text    : PAnsiChar;
  end;
  {$EXTERNALSYM skin_entry_t}
  TTtkSkinEntry = skin_entry_t;

type
  PTtkNicklistChange = ^TTtkNicklistChange;
  nicklist_change_t = record
    struct_size   : Cardinal;

    window_id     : Integer;
    connection_id : Integer;

    medium        : PAnsiChar;
    location      : PAnsiChar;

    nicklist      : Pointer; // ^nicklist_entry_t
  end;
  {$EXTERNALSYM nicklist_change_t}
  TTtkNicklistChange = nicklist_change_t;

type
  PTtkNicklistEntry = ^TTtkNicklistEntry;
  nicklist_entry_t = record
    struct_size        : Cardinal;

    name               : PAnsiChar;
    group              : PAnsiChar;
    tooltip            : PAnsiChar;

    group_score        : Integer;

    menu               : ^menu_entry_t;

    callback           : ttkCallback;
    data               : Pointer;

    next_nicklist_entry: Pointer; // ^nicklist_entry_t
  end;
  {$EXTERNALSYM nicklist_entry_t}
  TTtkNicklistEntry = nicklist_entry_t;

type
  PTtkMessageOptions = ^TTtkMessageOptions;
  message_options_t = record
    struct_size    : Cardinal;

    global_mode    : Integer;

    bold           : Integer;
    italics        : Integer;
    underline      : Integer;

    style          : Integer;
    size           : Integer;

    background     : Integer;
    foreground     : Integer;
    num_colors     : Integer;

    images         : Integer;
    image_status   : Integer;

    max_length     : Integer;
    disabled       : Integer;
    disable_message: PAnsiChar;

    logging_off    : Integer;
    logging_name   : PAnsiChar;

    echo_name      : PAnsiChar;

    echo_off       : Integer;
    is_contact     : Integer;
    initiated      : Integer;

    irc_style      : Integer;

    nicklist       : ^nicklist_entry_t;

    // 3.0
    video          : Integer;
    audio          : Integer;
  end;
  {$EXTERNALSYM message_options_t}
  TTtkMessageOptions = message_options_t;

type
  PTtkMessageToolbarAction = ^TTtkMessageToolbarAction;
  message_toolbar_action_t = record
    struct_size  : Cardinal;

    connection_id: Integer;
    window_id    : Integer;

    medum        : PAnsiChar;
    name         : PAnsiChar;
    location     : PAnsiChar;

    action       : PAnsiChar;

    // 3.0
    hwnd         : HWND;
  end;
  {$EXTERNALSYM message_toolbar_action_t}
  TTtkMessageToolbarAction = message_toolbar_action_t;

type
  PTtkMessageToolbarRegister = ^TTtkMessageToolbarRegister;
  message_toolbar_register_t = record
    struct_size  : Cardinal;

    connection_id: Integer;
    window_id    : Integer;

    medium       : PAnsiChar;
    name         : PAnsiChar;
    location     : PAnsiChar;

    button       : PAnsiChar;
    tooltip      : PAnsiChar;
    toolbar_id   : Integer;

    callback     : TTKCallback;
    data         : Pointer;
  end;
  {$EXTERNALSYM message_toolbar_register_t}
  TTtkMessageToolbarRegister = message_toolbar_register_t;
    

type
  PTtkMessageMenu = ^TTtkMessageMenu;
  message_menu_t = record
    struct_size  : Cardinal;

    connection_id: Integer;
    window_id    : Integer;

    medium       : PAnsiChar;
    name         : PAnsiChar;
    location     : PAnsiChar;

    menu_type    : Integer;

    menu_name    : PAnsiChar;
    menu_entry   : ^menu_entry_t;
  end;
  {$EXTERNALSYM message_menu_t}
  TTtkMessageMenu = message_menu_t;

type
  PTtkMessageState = ^TTtkMessageState;
  message_state_t = record
    struct_size  : Cardinal;

    connection_id: Integer;
    window_id    : Integer;

    medium       : PAnsiChar;
    name         : PAnsiChar;
    location     : PAnsiChar;

    control      : PAnsiChar;
    state        : PAnsiChar;
  end;
  {$EXTERNALSYM message_state_t}
  TTtkMessageState = message_state_t;

const
  IMAGE_ALL     = $0000;
  {$EXTERNALSYM IMAGE_ALL}
  IMAGE_PNG     = $0001;
  {$EXTERNALSYM IMAGE_PNG}
  IMAGE_GIF     = $0002;
  {$EXTERNALSYM IMAGE_GIF}
  IMAGE_JPG     = $0004;
  {$EXTERNALSYM IMAGE_JPG}
  IMAGE_JP2     = $0008;
  {$EXTERNALSYM IMAGE_JP2}
  IMAGE_ICO     = $0010;
  {$EXTERNALSYM IMAGE_ICO}
  IMAGE_BMP     = $0020;
  {$EXTERNALSYM IMAGE_BMP}

type
  PTtkAvatar = ^TTtkAvatar;
  avatar_t = record
    struct_size  : Cardinal;

    connection_id: Integer;
    window_id    : Integer;
    timestamp    : Integer;
    data_length  : Integer;

    medium       : PAnsiChar;
    name         : PAnsiChar;
    filename     : PAnsiChar;
    data         : PAnsiChar;

    // 3.0
    force_file_size         : Integer;
    force_image_dimensions  : Integer;
    allowed_image_type      : Integer;

    image_width   : Integer;
    image_height  : Integer;
    file_size     : Integer;
  end;
  {$EXTERNALSYM avatar_t}
  TTtkAvatar = avatar_t;

type
  PTtkEventActionRegister = ^TTtkEventActionRegister;
  event_action_register_t = record
    struct_size    : Cardinal;

    event          : PAnsiChar;
    description    : PAnsiChar;

    custom_settings: Integer;
    event_id       : Integer;

    callback       : ttkCallback;
    data           : Pointer;
  end;
  {$EXTERNALSYM event_action_register_t}
  TTtkEventActionRegister = event_action_register_t;

type
  PTtkEventActionEdit = ^TTtkEventActionEdit;
  event_action_edit_t = record
    struct_size: Cardinal;

    event      : PAnsiChar;

    action_data: Pointer;

    event_id   : Integer;
  end;
  {$EXTERNALSYM event_action_edit_t}
  TTtkEventActionEdit = event_action_edit_t;

type
  PTtkEventActionShow = ^TTtkEventActionShow;
  event_action_show_t = record
    struct_size: Cardinal;

    show       : Integer;  // 1 shows, 0 hides

    event      : PAnsiChar;

    action_data: Pointer;

    hwnd       : HWND;

    x          : Integer;
    y          : Integer;

    event_id   : Integer;
  end;
  {$EXTERNALSYM event_action_show_t}
  TTtkEventActionShow = event_action_show_t;

type
  PTtkEventActionAttribute = ^TTtkEventActionAttribute;
  event_action_attribute_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    value      : PAnsiChar;

    action_data: Pointer;
  end;
  {$EXTERNALSYM event_action_attribute_t}
  TTtkEventActionAttribute = event_action_attribute_t;

type
  PTtkEventStatusRegister = ^TTtkEventStatusRegister;
  event_status_register_t = record
    struct_size   : Cardinal;

    medium        : PAnsiChar;
    status        : PAnsiChar;
    description   : PAnsiChar;

    default_status: Integer;
    event_id      : Integer;

    callback      : ttkCallback;
    data          : Pointer;

    // 3.0
    png_image     : PByteArray;
    png_image_len : Cardinal;
  end;
  {$EXTERNALSYM event_status_register_t}
  TTtkEventStatusRegister = event_status_register_t;

type
  PTtkEventStatus = ^TTtkEventStatus;
  event_status_t = record
    struct_size : Cardinal;

    medium      : PAnsiChar;
    status      : PAnsiChar;
    text        : PAnsiChar;

    auto_respond: Integer;
    automatic   : Integer;
  end;
  {$EXTERNALSYM event_status_t}
  TTtkEventStatus = event_status_t;

type
  PTtkEventVariables = ^TTtkEventVariables;
  event_variables_t = record
    struct_size   : Cardinal;

    variable_name : PAnsiChar;
    variable_type : PAnsiChar;  // string, Integer, float, callback, unknown/custom

    variable_data : Pointer;
    variable_size : Cardinal;

    next_evt      : Pointer; // ^event_variables
  end;
  {$EXTERNALSYM event_variables_t}
  TTtkEventVariables = event_variables_t;

type
  PTtkEventActionExecute = ^TTtkEventActionExecute;
  event_action_execute_t = record
    struct_size: Cardinal;

    variables  : Pointer;
    
    action_data: Pointer;
  end;
  {$EXTERNALSYM event_action_execute_t}
  TTtkEventActionExecute = event_action_execute_t;

type
  PTtkEventEventRegister = ^TTtkEventEventRegister;
  event_event_register_t = record
    struct_size: Cardinal;

    _type      : PAnsiChar;
    description: PAnsiChar;
  end;
  {$EXTERNALSYM event_event_register_t}
  TTtkEventEventRegister = event_event_register_t;

type
  PTtkEventConnect = ^TTtkEventConnect;
  event_connect_t = record
    struct_size: Cardinal;

    _type      : PAnsiChar;

    event_id   : Integer;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM event_connect_t}
  TTtkEventConnect = event_connect_t;

type
  PTtkEvent = ^TTtkEvent;
  event_t = record
    struct_size: Cardinal;

    _type      : PAnsiChar;
    description: PAnsiChar;

    discrete   : Integer;
    event_id   : Integer;

    variables  : ^event_variables_t;
  end;
  {$EXTERNALSYM event_t}
  TTtkEvent = event_t;

type
  PTtkBrowserLocation = ^TTtkBrowserLocation;
  browser_location_t = record
    struct_size   : Cardinal;

    location      : PAnsiChar;

    new_window    : Integer;
    check_security: Integer;
  end;
  {$EXTERNALSYM browser_location_t}
  TTtkBrowserLocation = browser_location_t;

type
  PTtkBrowserEntry = ^TTtkBrowserEntry;
  browser_entry_t = record
    struct_size: Cardinal;

    protocol   : PAnsiChar;

    name       : PAnsiChar;
    description: PAnsiChar;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM browser_entry_t}
  TTtkBrowserEntry = browser_entry_t;

type
  PTtkAccountEntry = ^TTtkAccountEntry;
  account_entry_t = record
    struct_size   : Cardinal;

    account       : PAnsiChar;
    medium        : PAnsiChar;
    status        : PAnsiChar;
    section       : PAnsiChar;

    connected     : Integer;
    last_connected: Longint;

    icon          : HICON;
    window        : HWND;

    // 3.0
    callback      : ttkCallback;
    data          : Pointer;
  end;
  {$EXTERNALSYM account_entry_t}
  TTtkAccountEntry = account_entry_t;

type
  PTtkContactWizardInfo = ^TTtkContactWizardInfo;
  contact_wizard_info_t = record
    struct_size: Cardinal;

    name       : array[0..127] of AnsiChar;
    description: array[0..1023] of AnsiChar;

    bitmap     : HBITMAP;
  end;
  {$EXTERNALSYM contact_wizard_info_t}
  TTtkContactWizardInfo = contact_wizard_info_t;

type
  PTtkContactWizard = ^TTtkContactWizard;
  contact_wizard_t = record
    struct_size: Cardinal;

    show       : Integer; // 1 shows, 0 hides
    flags      : Integer;

    medium     : PAnsiChar;
    _type      : PAnsiChar; // contact, import

    hwnd       : HWND;

    x          : Integer;
    y          : Integer;

    section    : PAnsiChar;
    group      : ^group_entry_t;

    wizard_info: ^contact_wizard_info_t;
  end;
  {$EXTERNALSYM contact_wizard_t}
  TTtkContactWizard = contact_wizard_t;

type
  PTtkFileTransferUpdate = ^TTtkFileTransferUpdate;
  filetransfer_update_t = record
    struct_size       : Cardinal;

    medium            : PAnsiChar;

    connection_id     : Integer;
    filetransfer_id   : Integer;

    file_size         : Cardinal;
    start_size        : Cardinal;

    local_screen_name : PAnsiChar;
    remote_screen_name: PAnsiChar;
    filename          : PAnsiChar;
  end;
  {$EXTERNALSYM filetransfer_update_t}
  TTtkFileTransferUpdate = filetransfer_update_t;

type
  PTtkFileTransferInit = ^TTtkFileTransferInit;
  filetransfer_init_t = record
    struct_size       : Cardinal;

    medium            : PAnsiChar;

    connection_id     : Integer;
    filetransfer_id   : Integer;

    {* status of the transfer *}
    accepted          : Integer;
    resume            : Integer;

    {* preferences *}
    incoming          : Integer;
    ask_resume        : Integer;
    ask_location      : Integer;
    ask_screen_name   : Integer;
    ask_medium        : Integer;
    ask_description   : Integer;
    ask_confirmation  : Integer;

    in_contact_list   : Integer;

    file_size         : Cardinal;
    start_size        : Cardinal;  // for resuming

    local_screen_name : PAnsiChar;
    remote_screen_name: PAnsiChar;

    filename          : PAnsiChar;
    description       : PAnsiChar;

    callback          : ttkCallback;
    data              : Pointer;
  end;
  {$EXTERNALSYM filetransfer_init_t}
  TTtkFileTransferInit = filetransfer_init_t;

type
  PTtkFileTransferRequest = ^TTtkFileTransferRequest;
  filetransfer_request_t = record
    struct_size    : Cardinal;

    medium         : PAnsiChar;

    connection_id  : Integer;
    filetransfer_id: Integer;

    ask_description: Integer;

    // 3.0
    window_id           : Integer;
    remote_screen_name  : PAnsiChar;
  end;
  {$EXTERNALSYM filetransfer_request_t}
  TTtkFileTransferRequest = filetransfer_request_t;

type
  PTtkFileTransferStatus = ^TTtkFileTransferStatus;
  filetransfer_status_t = record
    struct_size    : Cardinal;

    medium         : PAnsiChar;

    connection_id  : Integer;
    filetransfer_id: Integer;

    _type          : PAnsiChar; // error, complete, progress...
    description    : PAnsiChar;

    bytes          : Integer;
  end;
  {$EXTERNALSYM filetransfer_status_t}
  TTtkFileTransferStatus = filetransfer_status_t;

type
  PTtkEditEvent = ^TTtkEditEvent;
  edit_event_t = record
    struct_size  : Cardinal;

    window_id    : Integer;
    edit_event_id: Integer;

    event        : PAnsiChar;

    callback     : ttkCallback;
    data         : Pointer;
  end;
  {$EXTERNALSYM edit_event_t}
  TTtkEditEvent = edit_event_t;

type
  PTtkEditMenu = ^TTtkEditMenu;
  edit_menu_t = record
    struct_size: Cardinal;

    menu_id    : Integer;

    meny_entry : ^menu_entry_t;
  end;
  {$EXTERNALSYM edit_menu_t}
  TTtkEditMenu = edit_menu_t;

type
  PTtkXMLAttribute = ^TTtkXMLAttribute;
  xml_attribute_t = record
    struct_size   : Cardinal;

    name          : PAnsiChar;
    value         : PAnsiChar;

    next_attribute: Pointer; // ^xml_attribute_t

    // 3.0
    previous      : Pointer; // ^xml_attribute_t
  end;
  {$EXTERNALSYM xml_attribute_t}
  TTtkXMLAttribute = xml_attribute_t;

type
  PTtkXMLTag = ^TTtkXMLTag;
  xml_tag_t = record
    struct_size: Cardinal;

    attributes : ^xml_attribute_t;

    children   : Pointer; // ^xml_tag_t

    text       : PAnsiChar;
    _type      : PAnsiChar; // "tag", "text"

    next_tag   : Pointer ; // ^xml_tag_t

    // 3.0
    parent     : Pointer; // ^xml_tag_t;
    previous   : Pointer; // ^xml_tag_t;
  end;
  {$EXTERNALSYM xml_tag_t}
  TTtkXMLTag = xml_tag_t;

type
  PTtkXMLTree = ^TTtkXMLTree;
  xml_tree_t = record
    struct_size: Cardinal;

    memory_id  : Integer;

    filename   : PAnsiChar;
    data       : PAnsiChar;

    root_tag   : ^xml_tag_t;
  end;
  {$EXTERNALSYM xml_tree_t}
  TTtkXMLTree = xml_tree_t;

type
  PTtkXMLString = ^TTtkXMLString;
  xml_string_t = record
    struct_size  : Cardinal;

    string_buffer: PAnsiChar;

    xml_tree     : ^xml_tree_t;
  end;
  {$EXTERNALSYM xml_string_t}
  TTtkXMLString = xml_string_t;

type
  PTtkPluginExternal = ^TTtkPluginExternal;
  plugin_external_t = record
    struct_size: Cardinal;

    guid       : PAnsiChar;

    event      : PAnsiChar;
    data       : Pointer;
  end;
  {$EXTERNALSYM plugin_external_t}
  TTtkPluginExternal = plugin_external_t;

type
  PTtkPluginExternalEnum = ^TTtkPluginExternalEnum;
  plugin_external_enum_t = record
    struct_size: Cardinal;

    _type      : PAnsiChar;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM plugin_external_enum_t}
  TTtkPluginExternalEnum = plugin_external_enum_t;

type
  PTtkConnectionBytes = ^TTtkConnectionBytes;
  connection_bytes_t = record
    struct_size      : Cardinal;

    medium           : PAnsiChar;

    connection_id    : Integer;

    sent_bytes_change: Integer;
    recv_bytes_change: Integer;
  end;
  {$EXTERNALSYM connection_bytes_t}
  TTtkConnectionBytes = connection_bytes_t;

type
  PTtkSkinLocation = ^TTtkSkinLocation;
  skin_location_t = record
    struct_size: Cardinal;

    num        : Integer;

    orientation: PAnsiChar;
    _type      : PAnsiChar;
    source     : PAnsiChar;
  end;
  {$EXTERNALSYM skin_location_t}
  TTtkSkinLocation = skin_location_t;

type
  PTtkSkinRect = ^TTtkSkinRect;
  skin_rect_t = record
    struct_size: Cardinal;

    left       : ^skin_location_t;
    top        : ^skin_location_t;
    right      : ^skin_location_t;
    bottom     : ^skin_location_t;
  end;
  {$EXTERNALSYM skin_rect_t}
  TTtkSkinRect = skin_rect_t;

type
  PTtkSkinTaskbarIcon = ^TTtkSkinTaskbarIcon;
  skin_taskbaricon_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _file      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_taskbaricon_t}
  TTtkSkinTaskbarIcon = skin_taskbaricon_t;

type
  PTtkSkinBitmap = ^TTtkSkinBitmap;
  skin_bitmap_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _file      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_bitmap_t}
  TTtkSkinBitmap = skin_bitmap_t;

type
  PTtkSkinFont = ^TTtkSkinFont;
  skin_font_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
     _file     : PAnsiChar;
  end;
  {$EXTERNALSYM skin_font_t}
  TTtkSkinFont = skin_font_t;

type
  PTtkSkinSound = ^TTtkSkinSound;
  skin_sound_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _file      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_sound_t}
  TTtkSkinSound = skin_sound_t;

type
  PTtkSkinWindow = ^TTtkSkinWindow;
  skin_window_t = record
    struct_size      : Cardinal;

    _type            : PAnsiChar;
    name             : PAnsiChar;
    source           : PAnsiChar;
    phase            : PAnsiChar;
    taskbar          : PAnsiChar;
    container_taskbar: PAnsiChar;
  end;
  {$EXTERNALSYM skin_window_t}
  TTtkSkinWindow = skin_window_t;

type
  PTtkSkinControl = ^TTtkSkinControl;
  skin_control_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _type      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_control_t}
  TTtkSkinControl = skin_control_t;

type
  PTtkSkinComponent = ^TTtkSkinComponent;
  skin_component_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;

    minx       : Integer;
    maxx       : Integer;
    miny       : Integer;
    maxy       : Integer;
  end;
  {$EXTERNALSYM skin_component_t}
  TTtkSkinComponent = skin_component_t;

type
  PTtkSkinIcontrol = ^TTtkSkinIcontrol;
  skin_icontrol_t = record
    struct_size: Cardinal;

    source     : PAnsiChar;
    name       : PAnsiChar;

    visible    : Integer;
  end;
  {$EXTERNALSYM skin_icontrol_t}
  TTtkSkinIcontrol = skin_icontrol_t;

type
  PTtkSkinFrame = ^TTtkSkinFrame;
  skin_frame_t = record
    struct_size: Cardinal;

    source     : PAnsiChar;
    name       : PAnsiChar;
  end;
  {$EXTERNALSYM skin_frame_t}
  TTtkSkinFrame = skin_frame_t;

type
  PTtkSkinDrawer = ^TTtkSkinDrawer;
  skin_drawer_t = record
    struct_size: Cardinal;

    source     : PAnsiChar;
    name       : PAnsiChar;
    attach     : PAnsiChar;

    visible    : Integer;
  end;
  {$EXTERNALSYM skin_drawer_t}
  TTtkSkinDrawer = skin_drawer_t;

type
  PTtkSkinSource = ^TTtkSkinSource;
  skin_source_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;

    left       : Integer;
    top        : Integer;
    right      : Integer;
    bottom     : Integer;
  end;
  {$EXTERNALSYM skin_source_t}
  TTtkSkinSource = skin_source_t;

type
  PTtkSkinControlEntry = ^TTtkSkinControlEntry;
  skin_control_entry_t = record
    struct_size: Cardinal;

    control    : ^skin_control_t;

    tag        : PAnsiChar;
    tag_data   : Pointer;
  end;
  {$EXTERNALSYM skin_control_entry_t}
  TTtkSkinControlEntry = skin_control_entry_t;

type
  PTtkSkinControlFont = ^TTtkSkinControlFont;
  skin_control_font_t = record
    struct_size: Cardinal;

    source     : PAnsiChar;
    _type      : PAnsiChar;
    name       : PAnsiChar;

    size       : Integer;
    bold       : Integer;
    italics    : Integer;
    underline  : Integer;
  end;
  {$EXTERNALSYM skin_control_font_t}
  TTtkSkinControlFont = skin_control_font_t;

type
  PTtkSkinControlForecolor = ^TTtkSkinControlForecolor;
  skin_control_forecolor_t = record
    struct_size: Cardinal;

    red        : Integer;
    green      : Integer;
    blue       : Integer;
  end;
  {$EXTERNALSYM skin_control_forecolor_t}
  TTtkSkinControlForeColor = skin_control_forecolor_t;

type
  PTtkSkinControlBackcolor = ^TTtkSkinControlBackcolor;
  skin_control_backcolor_t = record
    struct_size: Cardinal;

    red        : Integer;
    green      : Integer;
    blue       : Integer;
  end;
  {$EXTERNALSYM skin_control_backcolor_t}
  TTtkSkinControlBackColor = skin_control_backcolor_t;

type
  PTtkSkinControlSetting = ^TTtkSkinControlSetting;
  skin_control_setting_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    value      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_control_setting_t}
  TTtkSkinControlSetting = skin_control_setting_t;

type
  PTtkSkinControlColor = ^TTtkSkinControlColor;
  skin_control_color_t = record
    struct_size: Cardinal;
    name       : PAnsiChar;

    red        : Integer;
    green      : Integer;
    blue       : Integer;

    rect       : ^skin_rect_t;
  end;
  {$EXTERNALSYM skin_control_color_t}
  TTtkSkinControlColor = skin_control_color_t;

type
  PTtkSkinControlColors = ^TTtkSkinControlColors;
  skin_control_colors_t = record
    struct_size: Cardinal;

    _file      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_control_colors_t}
  TTtkSkinControlColors = skin_control_colors_t;

type
  PTtkSkinControlAction = ^TTtkSkinControlAction;
  skin_control_action_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    value      : PAnsiChar;
  end;
  {$EXTERNALSYM skin_control_action_t}
  TTtkSkinControlAction = skin_control_action_t;

type
  PTtkSkinControlToolTip = ^TTtkSkinControlTooltip;
  skin_control_tooltip_t = record
    struct_size: Cardinal;

    text       : PAnsiChar;
  end;
  {$EXTERNALSYM skin_control_tooltip_t}
  TTtkSkinControlTooltip = skin_control_tooltip_t;

type
  PTtkSkinEnum = ^TTtkSkinEnum;
  skin_enum_t = record
    struct_size: Cardinal;

    search_type: PAnsiChar;
    search_data: Pointer;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM skin_enum_t}
  TTtkSkinEnum = skin_enum_t;

type
  PTtkSkinControlIcon = ^TTtkSkinControlIcon;
  skin_control_icon_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _type      : PAnsiChar;
    state      : PAnsiChar;

    source     : ^skin_source_t;
  end;
  {$EXTERNALSYM skin_control_icon_t}
  TTtkSkinControlIcon = skin_control_icon_t;

type
  PTtkSkinControlBackground = ^TTtkSkinControlBackground;
  skin_control_background_t = record
    struct_size: Cardinal;

    name       : PAnsiChar;
    _type      : PAnsiChar;

    transparent: Integer;

    transred   : Integer;
    transgreen : Integer;
    transblue  : Integer;
    thresred   : Integer;
    thresgreen : Integer;
    thersblue  : Integer;

    source     : ^skin_source_t;
    rect       : ^skin_rect_t;
  end;
  {$EXTERNALSYM skin_control_background_t}
  TTtkSkinControlBackground = skin_control_background_t;

type
  PTtSkinControlEmoticon = ^TTtkSkinControlEmoticon;
  skin_control_emoticon_t = record
    struct_size: Cardinal;

    text       : PAnsiChar;
    sound      : PAnsiChar;
    display    : PAnsiChar;
    medium     : PAnsiChar;

    button     : Integer;

    source     : ^skin_source_t;
  end;
  {$EXTERNALSYM skin_control_emoticon_t}
  TTtkSkinControlEmoticon = skin_control_emoticon_t;

type
  PTtkSkinControlIcontrol = ^TTtkSkinControlIcontrol;
  skin_control_icontrol_t = record
    struct_size: Cardinal;

    source     : PAnsiChar;
    name       : PAnsiChar;

    visible    : Integer;

    rect       : ^skin_rect_t;
  end;
  {$EXTERNALSYM skin_control_icontrol_t}
  TTtkSkinControlIcontrol = skin_control_icontrol_t;

type
  PTtkContactListSubcontact = ^TTtkContactListSubcontact;
  contactlist_subcontact_t = record
    struct_size: Cardinal;

    parent     : ^contactlist_entry_t;
    subcontact : ^contactlist_entry_t;
  end;
  {$EXTERNALSYM contactlist_subcontact_t}
  TTtkContactListSubcontact = contactlist_subcontact_t;

type
  PTtkTooltipItem = ^TTtkTooltipItem;
  tooltip_item_t = record
    struct_size: Cardinal;

    _type      : PAnsiChar; // can be either text, or separator

    name       : PAnsiChar;
    value      : PAnsiChar;

    next_item  : Pointer; // tooltip_item_t
  end;
  {$EXTERNALSYM tooltip_item_t}
  TTtkTooltipItem = tooltip_item_t;

type
  PTtkTooltipEntry = ^TTtkTooltipEntry;
  tooltip_entry_t = record
    struct_size: Cardinal;

    title      : PAnsiChar;
    sub_title  : PAnsiChar;
    icon       : PAnsiChar;

    item_list  : ^tooltip_item_t;
  end;
  {$EXTERNALSYM tooltip_entry_t}
  TTtkTooltipEntry = tooltip_entry_t;

type
  PTtkTooltipRequest = ^TTtkTooltipRequest;
  tooltip_request_t = record
    struct_size: Cardinal;

    id         : Integer;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM tooltip_request_t}
  TTtkTooltipRequest = tooltip_request_t;

type
  PTtkContactlistTooltipRequest = ^TTtkContactlistTooltipRequest;
  contactlist_tooltip_request_t = record
    struct_size: Cardinal;

    contact    : ^contactlist_entry_t;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM contactlist_tooltip_request_t}
  TTtkContactlistTooltipRequest = contactlist_tooltip_request_t;

type
  PTtkContactlistTooltipRegister = ^TTtkContactlistTooltipRegister;
  contactlist_tooltip_register_t = record
    struct_size: Cardinal;

    tooltip_id : Integer;

    contact    : ^contactlist_entry_t;

    callback   : ttkCallback;
    data       : Pointer;
  end;
  {$EXTERNALSYM contactlist_tooltip_register_t}
  TTtkContactlistTooltipRegister = contactlist_tooltip_register_t;

type
  PTtkContactlistGroupChange = ^TTtkContactlistGroupChange;
  contactlist_group_change_t = record
    struct_size: Cardinal;

    contact    : ^contactlist_entry_t;

    old_group  : ^group_entry_t;
    new_group  : ^group_entry_t;
  end;
  {$EXTERNALSYM contactlist_group_change_t}
  TTtkContactlistGroupChange = contactlist_group_change_t;

type
  PTtkMessageMorph = ^TTtkMessageMorph;
  message_morph_t = record
    struct_size    : Cardinal;

    original_window: ^message_t;
    new_window     : ^message_t;
  end;
  {$EXTERNALSYM message_morph_t}
  TTtkMessageMorph = message_morph_t;

type
  PTtkHTTPRequest = ^TTtkHTTPRequest;
  http_request_t = record
    struct_size: Cardinal;

    url        : PAnsiChar;

    callback   : ttkCallback;
    data       : Pointer;

    // 3.0
    content_length  : Integer;

    content_type     : PAnsiChar;
    content_data     : PAnsiChar;
  end;
  {$EXTERNALSYM http_request_t}
  TTtkHTTPRequest = http_request_t;

type
  PTtkHTTPResult = ^TTtkHTTPResult;
  http_result_t = record
    struct_size: Cardinal;

    url        : PAnsiChar;

    buffer     : PAnsiChar;
    buffer_size: Integer;
  end;
  {$EXTERNALSYM http_result_t}
  TTtkHTTPResult = http_result_t;

type
  PTtkLanguageEntry = ^TTtkLanguageEntry;
  language_entry_t = record
    struct_size       : Cardinal;

    language_directory: PAnsiChar;
  end;
  {$EXTERNALSYM language_entry_t}
  TTtkLanguageEntry = language_entry_t;

// 2.01 structures

const
  INSTALL_WIZARD_FORWARD  = $0001;
  {$EXTERNALSYM INSTALL_WIZARD_FORWARD}
  INSTALL_WIZARD_BACKWARD = $0002;
  {$EXTERNALSYM INSTALL_WIZARD_BACKWARD}

type
  PTtkInstallWizard = ^TTtkInstallWizard;
  install_wizard_t = record
    struct_size   : Cardinal;

    show        : Integer; //1 shows, 0 hides
    flags       : Integer;

    hwnd        : HWND;

    x           : Integer;
    y           : Integer;

    // 3.0
    name        : array [0..127] of AnsiChar;
    description : array [0..1023] of AnsiChar;
  end;
  {$EXTERNALSYM install_wizard_t}
  TTtkInstallWizard = install_wizard_t;

// 3.0 structures

const
  AUDIO_CODEC_UNCOMPRESSED = 0;
  {$EXTERNALSYM AUDIO_CODEC_UNCOMPRESSED}
  AUDIO_CODEC_TRUESPEECH    = 1;
  {$EXTERNALSYM AUDIO_CODEC_TRUESPEECH}

type
  PTtkAudio = ^TTtkAudio;
  audio_t = record
    struct_size   : Cardinal;

    connection_id : Integer;
    window_id     : Integer;

    medium        : PAnsiChar;
    name          : PAnsiChar;
    location      : PAnsiChar;

    section       : PAnsiChar;

    initiated        : Integer;
    ask_confirmation : Integer;

    // Audio Information

    codec             : Integer;
    audio_sample_size : Integer;
    audio_sample_rate : Integer;
    audio_block_size  : Integer;
    audio_quality     : Integer;

    audio_data_length : Integer;

    audio_data        : PByteArray;

    callback          : ttkCallback;
    data              : Pointer;
  end;
  {$EXTERNALSYM audio_t}
  TTtkAudio = audio_t;

type
  PTtkAudioStatus = ^TTtkAudioStatus;
  audio_status_t = record
    struct_size   : Cardinal;

    connection_id : Integer;
    window_id     : Integer;

    medium        : PAnsiChar;
    name          : PAnsiChar;
    location      : PAnsiChar;

    _type         : PAnsiChar;
    status        : PAnsiChar;

    link          : PAnsiChar;
    tooltip       : PAnsiChar;
  end;
  {$EXTERNALSYM audio_status_t}
  TTtkAudioStatus = audio_status_t;

const
  VIDEO_CODEC_UNCOMPRESSED = 0;
  {$EXTERNALSYM VIDEO_CODEC_UNCOMPRESSED}
  VIDEO_CODEC_MJPG         = 1;
  {$EXTERNALSYM VIDEO_CODEC_MJPG}

type
  PTtkVideo = ^TTtkVideo;
  video_t = record
    struct_size   : Cardinal;

    connection_id : Integer;
    window_id     : Integer;

    medium        : PAnsiChar;
    name          : PAnsiChar;
    location      : PAnsiChar;

    section       : PAnsiChar;

    initiated       : Integer;
    ask_confirmaton : Integer;

    // Video Information

    codec         : Integer;
    video_width   : Integer;
    video_height  : Integer;
    video_depth   : Integer;
    video_quality : Integer;
    video_fps     : Integer;

    video_data_length : Integer;

    video_data        : PByteArray;

    callback      : ttkCallback;
    data          : Pointer;

  end;
  {$EXTERNALSYM video_t}
  TTtkVideo = video_t;

type
  PTtkVideoStatus = ^TTtkVideoStatus;
  video_status_t = record
    struct_size   : Cardinal;

    connection_id : Integer;
    window_id     : Integer;

    medium        : PAnsiChar;
    name          : PAnsiChar;
    location      : PAnsiChar;

    _type         : PAnsiChar;
    status        : PAnsiChar;

    link          : PAnsiChar;
    tooltip       : PAnsiChar;
  end;
  {$EXTERNALSYM video_status_t}
  TTtkVideoStatus = video_status_t;

const
  MESSAGE_SEARCH_NAME = $0001;
  {$EXTERNALSYM MESSAGE_SEARCH_NAME}
  MESSAGE_SEARCH_MESSAGE = $0002;
  {$EXTERNALSYM MESSAGE_SEARCH_MESSAGE}
  MESSAGE_SEARCH_TIME   = $0004;
  {$EXTERNALSYM MESSAGE_SEARCH_TIME}
  MESSAGE_SEARCH_TYPE   = $0008;
  {$EXTERNALSYM MESSAGE_SEARCH_TYPE}
  MESSAGE_SEARCH_MEDIUM = $0010;
  {$EXTERNALSYM MESSAGE_SEARCH_MEDIUM}
  MESSAGE_SEARCH_LIMITED = $0020;
  {$EXTERNALSYM MESSAGE_SEARCH_LIMITED}
  MESSAGE_SEARCH_LOCATION = $0040;
  {$EXTERNALSYM MESSAGE_SEARCH_LOCATION}

  // spaces separate words. so if you want to search for anything with bob and tom in it, search for "bob tom"

type
  PTtkMessageSearch = ^TTtkMessageSearch;
  message_search_t = record
    struct_size   : Cardinal;

    search_id     : Integer;

    search_flags  : Integer;

    case_sensitive: Integer;

    _type         : PAnsiChar; // message / video / conversation

    message_type  : PAnsiChar; // information_standard, etc

    name_fullmatch: Integer;
    name          : PAnsiChar;  // depending on the type of message, the name field may match different things..ie: status changes, name will be the 'who' field, but otherwise if could be the to of the from field.
    location      : PAnsiChar;

    message_fullmatch       : Integer;
    message_containing_links: Integer;
    message_fullwords       : Integer;
    _message                : PAnsiChar;

    medium       : PAnsiChar;

    time_start   : LongInt;
    time_end     : LongInt;

    callback     : ttkCallback;
    data         : Pointer;
  end;
  {$EXTERNALSYM message_search_t}
  TTtkMessageSearch = message_search_t;

type
  PTtkMessageSearchEntry = ^TTtkMessageSearchEntry;
  message_search_entry_t = record
    struct_size   : Cardinal;

    _to           : PAnsiChar;
    from          : PAnsiChar;
    from_display  : PAnsiChar;
    text          : PAnsiChar;
    link          : PAnsiChar;
    screenshot    : PAnsiChar;
    location      : PAnsiChar;

    _type         : PAnsiChar;

    message_type  : PAnsiChar;
    medium        : PAnsiChar;

    timestamp     : LongInt; // [GwaRGuITaR] convert this to TDateTime when you get it
  end;
  {$EXTERNALSYM message_search_entry_t}
  TTtkMessageSearchEntry = message_search_entry_t;

type
  PTtkProgress = ^TTtkProgress;
  progress_t = record
    struct_size   : Cardinal;

    position      : Cardinal;
    total         : Cardinal;
  end;
  {$EXTERNALSYM progress_t}
  TTtkProgress = progress_t;

type
  PTtkCapability = ^TTtkCapability;
  capability_t = record
    struct_size     : Cardinal;

    capability      : PAnsiChar;

    next_capability : Pointer; // ^capability_t
  end;
  {$EXTERNALSYM capability_t}
  TTtkCapability = capability_t;

type
  PTtkAccountInterfaceEntry = ^TTtkAccountInterfaceEntry;
  account_interface_entry_t = record
    struct_size   : Cardinal;

    account       : Pointer; // ^account_entry_t;

    name          : PAnsiChar;

    _type         : PAnsiChar;
    description   : PAnsiChar;
    value         : PAnsiChar;

    next_entry    : Pointer; // ^account_interface_entry_t
  end;
  {$EXTERNALSYM account_interface_entry_t}
  TTtkAccountInterfaceEntry = account_interface_entry_t;

type
  PTtkAccountInterfaceRequest = ^TTtkAccountInterfaceRequest;
  account_interface_request_t = record
    struct_size   : Cardinal;

    account       : Pointer; // ^account_entry_t

    callback      : ttkCallback;
    data          : Pointer;
  end;
  {$EXTERNALSYM account_interface_request_t}
  TTtkAccountInterfaceRequest = account_interface_request_t;

type
  PTtkPluginFile = ^TTtkPluginFile;
  plugin_file_t = record
    struct_size   : Cardinal;

    filename      : PAnsiChar;

    next_file     : Pointer; // ^plugin_file_t
  end;
  {$EXTERNALSYM plugin_file_t}
  TTtkPluginFile = plugin_file_t;

type
  PTtkPluginPrefsValueRegister = ^TTtkPluginPrefsValueRegister;
  plugin_prefs_value_register_t = record
    struct_size   : Cardinal;

    value         : PAnsiChar;

    callback      : ttkCallback;
    data          : Pointer;
  end;
  {$EXTERNALSYM plugin_prefs_value_register_t}
  TTtkPluginPrefsValueRegister = plugin_prefs_value_register_t;

type
  PTtkPluginPrefsValueRequest = ^TTtkPluginPrefsValueRequest;
  plugin_prefs_value_request_t = record
    struct_size   : Cardinal;

    value         : PAnsiChar;

    parameters    : Pointer; // ^event_variables_t

    callback      : ttkCallback;
    data          : Pointer;
  end;
  {$EXTERNALSYM plugin_prefs_value_request_t}
  TTtkPluginPrefsValueRequest = plugin_prefs_value_request_t;

type
  PTtkPluginPrefsValue = ^TTtkPluginPrefsValue;
  plugin_prefs_value_t = record
    struct_size   : Cardinal;

    value         : PAnsiChar;

    parameters    : Pointer; // ^event_variables_t
  end;
  {$EXTERNALSYM plugin_prefs_value_t}
  TTtkPluginPrefsValue = plugin_prefs_value_t;

type
  PTtkAwayMessageEnum = ^TTtkAwayMessageEnum;
  awaymessage_enum_t = record
    struct_size   : Cardinal;

    callback      : ttkCallback;
    data          : Pointer;
  end;
  {$EXTERNALSYM awaymessage_enum_t}
  TTtkAwayMessageEnum = awaymessage_enum_t;


type
  PTtkAwayMessageEntry = ^TTtkAwayMessageEntry;
  awaymessage_entry_t = record
    struct_size   : Cardinal;

    name          : PAnsiChar;
  end;
  {$EXTERNALSYM awaymessage_entry_t}
  TTtkAwayMessageEntry = awaymessage_entry_t;

procedure _trillianInitialize(out Structure; Size: Cardinal);
function isTrillianError(ErrorCode: Integer): Boolean;
{$EXTERNALSYM isTrillianError}
function isTrillianSuccess(ErrorCode: Integer): Boolean;
{$EXTERNALSYM isTrillianSuccess}
procedure trillianInitialize(out Info: plugin_info_t); overload;
procedure trillianInitialize(out Alert: alert_t); overload;
procedure trillianInitialize(out PrefsInfo: plugin_prefs_info_t); overload;
procedure trillianInitialize(out PrefsShow: plugin_prefs_show_t); overload;
procedure trillianInitialize(out PrefsAction: plugin_prefs_action_t); overload;
procedure trillianInitialize(out PrefsEntry: plugin_prefs_entry_t); overload;
procedure trillianInitialize(out Prefs: plugin_prefs_t); overload;
procedure trillianInitialize(out Alias: alias_t); overload;
procedure trillianInitialize(out AliasReq: alias_request_t); overload;
procedure trillianInitialize(out Keyb: keyboard_alias_t); overload;
procedure trillianInitialize(out KeybReq: keyboard_alias_request_t); overload;
procedure trillianInitialize(out ListBmp: list_bmp_t); overload;
procedure trillianInitialize(out ListFont: list_font_t); overload;
procedure trillianInitialize(out MenuEntry: menu_entry_t); overload;
procedure trillianInitialize(out DlgEntry: dialog_entry_t); overload;
procedure trillianInitialize(out ListUpd: list_update_t); overload;
procedure trillianInitialize(out ConnEnum: connection_enum_t); overload;
procedure trillianInitialize(out ConnEntry: connection_entry_t); overload;
procedure trillianInitialize(out ContListEnum: contactlist_enum_t); overload;
procedure trillianInitialize(out ContListEntry: contactlist_entry_t); overload;
procedure trillianInitialize(out Msg: message_t); overload;
procedure trillianInitialize(out MsgBroadcast: message_broadcast_t); overload;
// SDKv2 procedures
procedure trillianInitialize(out MedEntry : medium_entry_t); overload;
procedure trillianInitialize(out GroupEntry : group_entry_t); overload;
procedure trillianInitialize(out SkinEntry : skin_entry_t); overload;
procedure trillianInitialize(out NickListChange : nicklist_change_t); overload;
procedure trillianInitialize(out NickListEntry : nicklist_entry_t); overload;
procedure trillianInitialize(out MsgOptions : message_options_t); overload;
procedure trillianInitialize(out MsgToolbarAction : message_toolbar_action_t); overload;
procedure trillianInitialize(out MsgToolbarReg : message_toolbar_register_t); overload;
procedure trillianInitialize(out MsgMenu : message_menu_t); overload;
procedure trillianInitialize(out MsgState : message_state_t); overload;
procedure trillianInitialize(out Avatar : avatar_t); overload;
procedure trillianInitialize(out EventActionReg : event_action_register_t); overload;
procedure trillianInitialize(out EventActionEdit : event_action_edit_t); overload;
procedure trillianInitialize(out EventActionShow : event_action_show_t); overload;
procedure trillianInitialize(out EventActionAttr : event_action_attribute_t); overload;
procedure trillianInitialize(out EventStatusReg : event_status_register_t); overload;
procedure trillianInitialize(out EventStatus : event_status_t); overload;
procedure trillianInitialize(out EventVars : event_variables_t); overload;
procedure trillianInitialize(out EventEventReg : event_event_register_t); overload;
procedure trillianInitialize(out EventConnect : event_connect_t); overload;
procedure trillianInitialize(out Event : event_t); overload;
procedure trillianInitialize(out BrowserLoc : browser_location_t); overload;
procedure trillianInitialize(out BrowserEntry : browser_entry_t); overload;
procedure trillianInitialize(out AcctEntry : account_entry_t); overload;
procedure trillianInitialize(out ContactWizInfo : contact_wizard_info_t); overload;
procedure trillianInitialize(out ContactWiz : contact_wizard_t); overload;
procedure trillianInitialize(out FileXferUpdate : filetransfer_update_t); overload;
procedure trillianInitialize(out FileXferInit : filetransfer_init_t); overload;
procedure trillianInitialize(out FileXferReq : filetransfer_request_t); overload;
procedure trillianInitialize(out FileXferStatus : filetransfer_status_t); overload;
procedure trillianInitialize(out EditEvent : edit_event_t); overload;
procedure trillianInitialize(out EditMenu : edit_menu_t); overload;
procedure trillianInitialize(out XMLAttr : xml_attribute_t); overload;
procedure trillianInitialize(out XMLTag : xml_tag_t); overload;
procedure trillianInitialize(out XMLTree : xml_tree_t); overload;
procedure trillianInitialize(out XMLString : xml_string_t); overload;
procedure trillianInitialize(out PluginExternal : plugin_external_t); overload;
procedure trillianInitialize(out PluginExternalEnum : plugin_external_enum_t); overload;
procedure trillianInitialize(out ConnectionBytes : connection_bytes_t); overload;
procedure trillianInitialize(out SkinLocation : skin_location_t); overload;
procedure trillianInitialize(out SkinRect : skin_rect_t); overload;
procedure trillianInitialize(out SkinTaskbarIcon : skin_taskbaricon_t); overload;
procedure trillianInitialize(out SkinBitmap : skin_bitmap_t); overload;
procedure trillianInitialize(out SkinFont : skin_font_t); overload;
procedure trillianInitialize(out SkinSound : skin_sound_t); overload;
procedure trillianInitialize(out SkinWindow : skin_window_t); overload;
procedure trillianInitialize(out SkinControl : skin_control_t); overload;
procedure trillianInitialize(out SkinComponent : skin_component_t); overload;
procedure trillianInitialize(out SkinIcontrol : skin_icontrol_t); overload;
procedure trillianInitialize(out SkinFrame : skin_frame_t); overload;
procedure trillianInitialize(out SkinDrawer : skin_drawer_t); overload;
procedure trillianInitialize(out SkinSource : skin_source_t); overload;
procedure trillianInitialize(out SkinControlEntry : skin_control_entry_t); overload;
procedure trillianInitialize(out SkinControlFont : skin_control_font_t); overload;
procedure trillianInitialize(out SkinControlForecolor : skin_control_forecolor_t); overload;
procedure trillianInitialize(out SkinControlBackcolor : skin_control_backcolor_t); overload;
procedure trillianInitialize(out SkinControlSettings : skin_control_setting_t); overload;
procedure trillianInitialize(out SkinControlColor : skin_control_color_t); overload;
procedure trillianInitialize(out SkinControlColors : skin_control_colors_t); overload;
procedure trillianInitialize(out SkinControlAction : skin_control_action_t); overload;
procedure trillianInitialize(out SkinControlTooltip : skin_control_tooltip_t); overload;
procedure trillianInitialize(out SkinEnum : skin_enum_t); overload;
procedure trillianInitialize(out SkinControlIcon : skin_control_icon_t); overload;
procedure trillianInitialize(out SkinControlBackground : skin_control_background_t); overload;
procedure trillianInitialize(out SkinControlEmoticon : skin_control_emoticon_t); overload;
procedure trillianInitialize(out SkinControlIcontrol : skin_control_icontrol_t); overload;
procedure trillianInitialize(out ConcactListSubC : contactlist_subcontact_t); overload;
procedure trillianInitialize(out TooltipItem : tooltip_item_t); overload;
procedure trillianInitialize(out TooltipEntry : tooltip_entry_t); overload;
procedure trillianInitialize(out TooltipReq : tooltip_request_t); overload;
procedure trillianInitialize(out ContListTooltipReq : contactlist_tooltip_request_t); overload;
procedure trillianInitialize(out ContListTooltipReg : contactlist_tooltip_register_t); overload;
procedure trillianInitialize(out ContListGroupChange : contactlist_group_change_t); overload;
procedure trillianInitialize(out MessageMorph : message_morph_t); overload;
procedure trillianInitialize(out HTTPReq : http_request_t); overload;
procedure trillianInitialize(out HTTPResult : http_result_t); overload;
procedure trillianInitialize(out LanguageEntry : language_entry_t); overload;
// SDKv2.1 Procedures
procedure trillianInitialize(out InstallWizard : install_wizard_t); overload;
// SDKv3 Procedures
procedure trillianInitialize(out Audio : audio_t); overload;
procedure trillianInitialize(out AudioStatus : audio_status_t); overload;
procedure trillianInitialize(out Video : video_t); overload;
procedure trillianInitialize(out VideoStatus : video_status_t); overload;
procedure trillianInitialize(out MessageSerach : message_search_t); overload;
procedure trillianInitialize(out MessageSearchEntry : message_search_entry_t); overload;
procedure trillianInitialize(out Progress : progress_t); overload;
procedure trillianInitialize(out Capability : capability_t); overload;
procedure trillianInitialize(out AccountInterfaceEntry: account_interface_entry_t); overload;
procedure trillianInitialize(out AccountInterfaceReq: account_interface_request_t); overload;
procedure trillianInitialize(out PluginFile : plugin_file_t); overload;
procedure trillianInitialize(out PluginPrefsValueReg : plugin_prefs_value_register_t); overload;
procedure trillianInitialize(out PluginPrefsValueReq : plugin_prefs_value_request_t); overload;
procedure trillianInitialize(out PluginPrefsValue : plugin_prefs_value_t); overload;
procedure trillianInitialize(out AwayMessageEnum : awaymessage_enum_t); overload;
procedure trillianInitialize(out AwayMessageEntry : awaymessage_entry_t); overload;
{$EXTERNALSYM trillianInitialize}
procedure trillianListInitialize(out ListEntry: list_entry_t);
{$EXTERNALSYM trillianListInitialize}

implementation

function isTrillianError(ErrorCode: Integer): Boolean; {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  Result := (ErrorCode < 0);
end;

function isTrillianSuccess(ErrorCode: Integer): Boolean; {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  Result := (ErrorCode >= 0);
end;

procedure _trillianInitialize(out Structure; Size: Cardinal); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  FillChar(Structure, Size, 0);
  PCardinal(@Structure)^ := Size;  // [NicoDE] set first member (struct_size)
end;

procedure trillianInitialize(out Info: plugin_info_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Info, SizeOf(plugin_info_t));
end;
procedure trillianInitialize(out Alert: alert_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Alert, SizeOf(alert_t));
end;
procedure trillianInitialize(out PrefsInfo: plugin_prefs_info_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(PrefsInfo, SizeOf(plugin_prefs_info_t));
end;
procedure trillianInitialize(out PrefsShow: plugin_prefs_show_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(PrefsShow, SizeOf(plugin_prefs_show_t));
  _trillianInitialize(PrefsShow.prefs_info, SizeOf(plugin_prefs_info_t));
end;
procedure trillianInitialize(out PrefsAction: plugin_prefs_action_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(PrefsAction, SizeOf(plugin_prefs_action_t));
end;
procedure trillianInitialize(out PrefsEntry: plugin_prefs_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(PrefsEntry, SizeOf(plugin_prefs_entry_t));
end;
procedure trillianInitialize(out Prefs: plugin_prefs_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Prefs, SizeOf(plugin_prefs_t));
end;
procedure trillianInitialize(out Alias: alias_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Alias, SizeOf(alias_t));
end;
procedure trillianInitialize(out AliasReq: alias_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(AliasReq, SizeOf(alias_request_t));
end;
procedure trillianInitialize(out Keyb: keyboard_alias_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Keyb, SizeOf(keyboard_alias_t));
end;
procedure trillianInitialize(out KeybReq: keyboard_alias_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(KeybReq, SizeOf(keyboard_alias_request_t));
end;
procedure trillianInitialize(out ListBmp: list_bmp_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ListBmp, SizeOf(list_bmp_t));
end;
procedure trillianInitialize(out ListFont: list_font_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ListFont, SizeOf(list_font_t));
end;
procedure trillianInitialize(out MenuEntry: menu_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(MenuEntry, SizeOf(menu_entry_t));
end;
procedure trillianInitialize(out DlgEntry: dialog_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(DlgEntry, SizeOf(dialog_entry_t));
end;
procedure trillianInitialize(out ListUpd: list_update_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ListUpd, SizeOf(list_update_t));
end;
procedure trillianInitialize(out ConnEnum: connection_enum_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ConnEnum, SizeOf(connection_enum_t));
end;
procedure trillianInitialize(out ConnEntry: connection_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ConnEntry, SizeOf(connection_entry_t));
end;
procedure trillianInitialize(out ContListEnum: contactlist_enum_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ContListEnum, SizeOf(contactlist_enum_t));
end;
procedure trillianInitialize(out ContListEntry: contactlist_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(ContListEntry, SizeOf(contactlist_entry_t));
end;
procedure trillianInitialize(out Msg: message_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(Msg, SizeOf(message_t));
end;
procedure trillianInitialize(out MsgBroadcast: message_broadcast_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF}
begin
  _trillianInitialize(MsgBroadcast, SizeOf(message_broadcast_t));
end;
// SDKv2 Procedures
procedure trillianInitialize(out MedEntry : medium_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MedEntry, SizeOf(medium_entry_t));
end;
procedure trillianInitialize(out GroupEntry : group_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(GroupEntry, SizeOf(group_entry_t));
end;
procedure trillianInitialize(out SkinEntry : skin_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinEntry, SizeOf(skin_entry_t));
end;
procedure trillianInitialize(out NickListChange : nicklist_change_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(NickListChange, SizeOf(nicklist_change_t));
end;
procedure trillianInitialize(out NickListEntry : nicklist_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(NickListEntry, SizeOf(nicklist_entry_t));
end;
procedure trillianInitialize(out MsgOptions : message_options_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MsgOptions, SizeOf(message_options_t));
end;
procedure trillianInitialize(out MsgToolbarAction : message_toolbar_action_t ); overload;
begin
  _trillianInitialize(MsgToolbarAction, SizeOf(message_toolbar_action_t));
end;
procedure trillianInitialize(out MsgToolbarReg : message_toolbar_register_t ); overload;
begin
  _trillianInitialize(MsgToolbarReg, SizeOf(message_toolbar_register_t));
end;
procedure trillianInitialize(out MsgMenu : message_menu_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MsgMenu, SizeOf(message_menu_t));
end;
procedure trillianInitialize(out MsgState : message_state_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MsgState, SizeOf(message_state_t));
end;
procedure trillianInitialize(out Avatar : avatar_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Avatar, SizeOf(avatar_t));
end;
procedure trillianInitialize(out EventActionReg : event_action_register_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventActionReg, SizeOf(event_action_register_t));
end;
procedure trillianInitialize(out EventActionEdit : event_action_edit_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventActionEdit, SizeOf(event_action_edit_t));
end;
procedure trillianInitialize(out EventActionShow : event_action_show_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventActionShow, SizeOf(event_action_show_t));
end;
procedure trillianInitialize(out EventActionAttr : event_action_attribute_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventActionAttr, SizeOf(event_action_attribute_t));
end;
procedure trillianInitialize(out EventStatusReg : event_status_register_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventStatusReg, SizeOf(event_status_register_t));
end;
procedure trillianInitialize(out EventStatus : event_status_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventStatus, SizeOf(event_status_t));
end;
procedure trillianInitialize(out EventVars : event_variables_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventVars, SizeOf(event_variables_t));
end;
procedure trillianInitialize(out EventActionExecute : event_action_execute_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventActionExecute, SizeOf(event_action_execute_t));
end;
procedure trillianInitialize(out EventEventReg : event_event_register_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventEventReg, SizeOf(event_event_register_t));
end;
procedure trillianInitialize(out EventConnect : event_connect_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EventConnect, SizeOf(event_connect_t));
end;
procedure trillianInitialize(out Event : event_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Event, SizeOf(event_t));
end;
procedure trillianInitialize(out BrowserLoc : browser_location_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(BrowserLoc, SizeOf(browser_location_t));
end;
procedure trillianInitialize(out BrowserEntry : browser_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(BrowserEntry, SizeOf(browser_entry_t));
end;
procedure trillianInitialize(out AcctEntry : account_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AcctEntry, SizeOf(account_entry_t));
end;
procedure trillianInitialize(out ContactWizInfo : contact_wizard_info_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ContactWizInfo, SizeOf(contact_wizard_info_t));
end;
procedure trillianInitialize(out ContactWiz : contact_wizard_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ContactWiz, SizeOf(contact_wizard_t));
end;
procedure trillianInitialize(out FileXferUpdate : filetransfer_update_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(FileXferUpdate, SizeOf(filetransfer_update_t));
end;
procedure trillianInitialize(out FileXferInit : filetransfer_init_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(FileXferInit, SizeOf(filetransfer_init_t));
end;
procedure trillianInitialize(out FileXferReq : filetransfer_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(FileXferReq, SizeOf(filetransfer_request_t));
end;
procedure trillianInitialize(out FileXferStatus : filetransfer_status_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(FileXferStatus, SizeOf(filetransfer_status_t));
end;
procedure trillianInitialize(out EditEvent : edit_event_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EditEvent, SizeOf(edit_event_t));
end;
procedure trillianInitialize(out EditMenu : edit_menu_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(EditMenu, SizeOf(edit_menu_t));
end;
procedure trillianInitialize(out XMLAttr : xml_attribute_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(XMLAttr, SizeOf(xml_attribute_t));
end;
procedure trillianInitialize(out XMLTag : xml_tag_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(XMLTag, SizeOf(xml_tag_t));
end;
procedure trillianInitialize(out XMLTree : xml_tree_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(XMLTree, SizeOf(xml_tree_t));
end;
procedure trillianInitialize(out XMLString : xml_string_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(XMLString, SizeOf(xml_string_t));
end;
procedure trillianInitialize(out PluginExternal : plugin_external_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginExternal, SizeOf(plugin_external_t));
end;
procedure trillianInitialize(out PluginExternalEnum : plugin_external_enum_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginExternalEnum, SizeOf(plugin_external_enum_t));
end;
procedure trillianInitialize(out ConnectionBytes : connection_bytes_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ConnectionBytes, SizeOf(connection_bytes_t));
end;
procedure trillianInitialize(out SkinLocation : skin_location_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinLocation, SizeOf(skin_location_t));
end;
procedure trillianInitialize(out SkinRect : skin_rect_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinRect, SizeOf(skin_rect_t));
end;
procedure trillianInitialize(out SkinTaskbarIcon : skin_taskbaricon_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinTaskbarIcon, SizeOf(skin_taskbaricon_t));
end;
procedure trillianInitialize(out SkinBitmap : skin_bitmap_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinBitmap, SizeOf(skin_bitmap_t));
end;
procedure trillianInitialize(out SkinFont : skin_font_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinFont, SizeOf(skin_font_t));
end;
procedure trillianInitialize(out SkinSound : skin_sound_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinSound, SizeOf(skin_sound_t));
end;
procedure trillianInitialize(out SkinWindow : skin_window_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinWindow, SizeOf(skin_window_t));
end;
procedure trillianInitialize(out SkinControl : skin_control_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControl, SizeOf(skin_control_t));
end;
procedure trillianInitialize(out SkinComponent : skin_component_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinComponent, SizeOf(skin_component_t));
end;
procedure trillianInitialize(out SkinIcontrol : skin_icontrol_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinIcontrol, SizeOf(skin_icontrol_t));
end;
procedure trillianInitialize(out SkinFrame : skin_frame_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinFrame, SizeOf(skin_frame_t));
end;
procedure trillianInitialize(out SkinDrawer : skin_drawer_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinDrawer, SizeOf(skin_drawer_t));
end;
procedure trillianInitialize(out SkinSource : skin_source_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinSource, SizeOf(skin_source_t));
end;
procedure trillianInitialize(out SkinControlEntry : skin_control_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlEntry, SizeOf(skin_control_entry_t));
end;
procedure trillianInitialize(out SkinControlFont : skin_control_font_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlFont, SizeOf(skin_control_font_t));
end;
procedure trillianInitialize(out SkinControlForecolor : skin_control_forecolor_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlForecolor, SizeOf(skin_control_forecolor_t));
end;
procedure trillianInitialize(out SkinControlBackcolor : skin_control_backcolor_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlBackcolor, SizeOf(skin_control_backcolor_t));
end;
procedure trillianInitialize(out SkinControlSettings : skin_control_setting_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlSettings, SizeOf(skin_control_setting_t));
end;
procedure trillianInitialize(out SkinControlColor : skin_control_color_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlColor, SizeOf(skin_control_color_t));
end;
procedure trillianInitialize(out SkinControlColors : skin_control_colors_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlColors, SizeOf(skin_control_colors_t));
end;
procedure trillianInitialize(out SkinControlAction : skin_control_action_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlAction, SizeOf(skin_control_action_t));
end;
procedure trillianInitialize(out SkinControlTooltip : skin_control_tooltip_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlTooltip, SizeOf(skin_control_tooltip_t));
end;
procedure trillianInitialize(out SkinEnum : skin_enum_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinEnum, SizeOf(skin_enum_t));
end;
procedure trillianInitialize(out SkinControlIcon : skin_control_icon_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlIcon, SizeOf(skin_control_icon_t));
end;
procedure trillianInitialize(out SkinControlBackground : skin_control_background_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlBackground, SizeOf(skin_control_background_t));
end;
procedure trillianInitialize(out SkinControlEmoticon : skin_control_emoticon_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlEmoticon, SizeOf(skin_control_emoticon_t));
end;
procedure trillianInitialize(out SkinControlIcontrol : skin_control_icontrol_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(SkinControlIcontrol, SizeOf(skin_control_icontrol_t));
end;
procedure trillianInitialize(out ConcactListSubC : contactlist_subcontact_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ConcactListSubC, SizeOf(contactlist_subcontact_t));
end;
procedure trillianInitialize(out TooltipItem : tooltip_item_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(TooltipItem, SizeOf(tooltip_item_t));
end;
procedure trillianInitialize(out TooltipEntry : tooltip_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(TooltipEntry, SizeOf(tooltip_entry_t));
end;
procedure trillianInitialize(out TooltipReq : tooltip_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(TooltipReq, SizeOf(tooltip_request_t));
end;
procedure trillianInitialize(out ContListTooltipReq : contactlist_tooltip_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ContListTooltipReq, SizeOf(contactlist_tooltip_request_t));
end;
procedure trillianInitialize(out ContListTooltipReg : contactlist_tooltip_register_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ContListTooltipReg, SizeOf(contactlist_tooltip_register_t));
end;
procedure trillianInitialize(out ContListGroupChange : contactlist_group_change_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(ContListGroupChange, SizeOf(contactlist_group_change_t));
end;
procedure trillianInitialize(out MessageMorph : message_morph_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MessageMorph, SizeOf(message_morph_t));
end;
procedure trillianInitialize(out HTTPReq : http_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(HTTPReq, SizeOf(http_request_t));
end;
procedure trillianInitialize(out HTTPResult : http_result_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(HTTPResult, SizeOf(http_result_t));
end;
procedure trillianInitialize(out LanguageEntry : language_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(LanguageEntry, SizeOf(language_entry_t));
end;
// SDKv2.1 Procedures
procedure trillianInitialize(out InstallWizard : install_wizard_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(InstallWizard, SizeOf(install_wizard_t));
end;
// SDKv3 Procedure
procedure trillianInitialize(out Audio : audio_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Audio, SizeOf(audio_t));
end;
procedure trillianInitialize(out AudioStatus : audio_status_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AudioStatus, SizeOf(audio_status_t));
end;
procedure trillianInitialize(out Video : video_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Video, SizeOf(video_t));
end;
procedure trillianInitialize(out VideoStatus : video_status_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(VideoStatus, SizeOf(video_status_t));
end;
procedure trillianInitialize(out MessageSerach : message_search_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MessageSerach, SizeOf(message_search_t));
end;
procedure trillianInitialize(out MessageSearchEntry : message_search_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(MessageSearchEntry, SizeOf(message_search_entry_t));
end;
procedure trillianInitialize(out Progress : progress_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Progress, SizeOf(progress_t));
end;
procedure trillianInitialize(out Capability : capability_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(Capability, SizeOf(capability_t));
end;
procedure trillianInitialize(out AccountInterfaceEntry: account_interface_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AccountInterfaceEntry, SizeOf(account_interface_entry_t));
end;
procedure trillianInitialize(out AccountInterfaceReq: account_interface_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AccountInterfaceReq, SizeOf(account_interface_request_t));
end;
procedure trillianInitialize(out PluginFile : plugin_file_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginFile, SizeOf(plugin_file_t));
end;
procedure trillianInitialize(out PluginPrefsValueReg : plugin_prefs_value_register_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginPrefsValueReg, SizeOf(plugin_prefs_value_register_t));
end;
procedure trillianInitialize(out PluginPrefsValueReq : plugin_prefs_value_request_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginPrefsValueReq, SizeOf(plugin_prefs_value_request_t));
end;
procedure trillianInitialize(out PluginPrefsValue : plugin_prefs_value_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(PluginPrefsValue, SizeOf(plugin_prefs_value_t));
end;
procedure trillianInitialize(out AwayMessageEnum : awaymessage_enum_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AwayMessageEnum, SizeOf(awaymessage_enum_t));
end;
procedure trillianInitialize(out AwayMessageEntry : awaymessage_entry_t); {$IFDEF INLINEFUNCS}inline;{$ENDIF} overload;
begin
  _trillianInitialize(AwayMessageEntry, SizeOf(awaymessage_entry_t));
end;

procedure trillianListInitialize(out ListEntry: list_entry_t);
begin
  FillChar(ListEntry, SizeOf(list_entry_t), 0);
  with ListEntry do
  begin
    struct_size := SizeOf(list_entry_t);
    section_id  := -1;
    parent_id   := -1;
    previous_id := -1;
    unique_id   := -1;
    selectable  := 1;
    trillianInitialize(font);
    with font do
    begin
      flags             := 1;
      hover_background  := COLORREF(-1);
      hover_foreground  := COLORREF(-1);
      select_background := COLORREF(-1);
      select_foreground := COLORREF(-1);
      background        := COLORREF(-1);
      foreground        := COLORREF(-1);
    end;
  end;
end;

end.
