unit NonRefInterfacedObjectUnit;

interface

type
  TNonRefInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TNonRefInterfacedObject

function TNonRefInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;


function TNonRefInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;


function TNonRefInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

end.
