unit SkypeTS.Skype;

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, SkypeTS_TLB, StdVcl;

type
  TSkype = class(TAutoObject, IConnectionPointContainer, ISkype)
  private
    { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FEvents: ISkypeEvents;
    { note: FEvents maintains a *single* event sink. For access to more
      than one event sink, use FConnectionPoint.SinkList, and iterate
      through the list of sinks. }

    procedure Attach(Protocol: Integer; Wait: WordBool); safecall;
    function Get_Application(const Name: WideString): IApplication; safecall;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; safecall;

  protected
    { Protected declarations }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

  public
    destructor Destroy; override;
    procedure Initialize; override;

    procedure TestEvent;

  end;

var
  g_Skype: TSkype = nil;

implementation

uses
  ComServ;

////////////////////////////////////////////////////////////////////////////////
// TSkype

destructor TSkype.Destroy;
begin
  g_Skype := nil;
  inherited;
end;


procedure TSkype.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ISkypeEvents;
end;


procedure TSkype.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;

  g_Skype := self;
end;


procedure TSkype.Attach(Protocol: Integer; Wait: WordBool);
begin
end;


function TSkype.Get_Application(const Name: WideString): IApplication;
begin
  Result := nil;
end;


function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
begin
  Result := nil;
end;


procedure TSkype.TestEvent;
begin
  FEvents.AttachmentStatus(apiAttachUnknown);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSkype, CLASS_Skype,
    ciMultiInstance, tmSingle);

end.
