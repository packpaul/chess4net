unit PlysProviderIntfUnit;

interface

uses
  Classes;

type
  TPlyStatus = (psMainLine, psUserLine);
  TPlyStatuses = set of TPlyStatus;

  IPlysProvider = interface
    function GetPlysCount: integer;
    function GetPly(iIndex: integer): string;
    function GetPlyStatus(iIndex: integer): TPlyStatuses;

    function GetCurrentPlyIndex: integer;
    procedure SetCurrentPlyIndex(iValue: integer);

    function HasSeveralPlysForPlyIndex(iPlyIndex: integer): boolean;
    procedure GetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);
    function SetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    function GetInvalidationID: LongWord;

    property PlysCount: integer read GetPlysCount;
    property Plys[iIndex: integer]: string read GetPly;
    property CurrentPlyIndex: integer read GetCurrentPlyIndex
                                      write SetCurrentPlyIndex;
    property InvalidationID: LongWord read GetInvalidationID;
// TODO:    property WhiteStarts: boolean;
  end;

implementation

end.
