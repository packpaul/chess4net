unit SkypeAPI_Object;

interface

uses
  Classes;

type
  IObject = interface
    function GetObject: TObject;
    property _Object: TObject read GetObject;
  end;


  TObjectInterfacedObject = class(TInterfacedObject, IObject)
  protected
    function GetObject: TObject;
  end;


  TObjectInterfaceList = class(TInterfaceList, IObject)
  protected
    function GetObject: TObject;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TObjectInterfacedObject

function TObjectInterfacedObject.GetObject: TObject;
begin
  Result := self
end;

////////////////////////////////////////////////////////////////////////////////
// TObjectInterfaceList

function TObjectInterfaceList.GetObject: TObject;
begin
  Result := self;
end;

end.