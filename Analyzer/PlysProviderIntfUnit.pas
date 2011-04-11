////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PlysProviderIntfUnit;

interface

uses
  Classes;

type
  TPlyStatus = (psMainLine, psUserLine);
  TPlyStatuses = set of TPlyStatus;

  IPlysProvider = interface
    function GetWhiteStarts: boolean;

    function GetPlysCount: integer;
    function GetPly(iIndex: integer): string;
    function GetPlysOffset: integer;
    function GetPlyStatus(iIndex: integer): TPlyStatuses;

    function GetComments(iIndex: integer): WideString;
    procedure SetComments(iIndex: integer; const wstrValue: WideString);

    function GetCurrentPlyIndex: integer;
    procedure SetCurrentPlyIndex(iValue: integer);

    function HasSeveralPlysForPlyIndex(iPlyIndex: integer): boolean;
    procedure GetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);
    function SetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    function GetInvalidationID: LongWord;

    property PlysCount: integer read GetPlysCount;
    property Plys[iIndex: integer]: string read GetPly;
    property Comments[iIndex: integer]: WideString read GetComments
                                                   write SetComments;
    property CurrentPlyIndex: integer read GetCurrentPlyIndex
                                      write SetCurrentPlyIndex;
    property PlysOffset: integer read GetPlysOffset;
    property WhiteStarts: boolean read GetWhiteStarts;

    property InvalidationID: LongWord read GetInvalidationID;
  end;

implementation

end.
