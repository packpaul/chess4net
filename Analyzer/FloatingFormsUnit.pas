unit FloatingFormsUnit;

interface

uses
  Forms, Classes, Messages, Types;

type
  TChildFloatingForm = class;

  TMainFloatingForm = class(TForm)
  private
    m_lstChilds: TList;
    m_InitialPos: TPoint;
    procedure FAddChild(AChild: TChildFloatingForm);
    procedure FRemoveChild(AChild: TChildFloatingForm);
    procedure WMMoving(var Msg: TWMMoving); message WM_MOVING;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure FRecalculateOffsetForEachChild(const NewPos: TPoint);

    property InitialLeft: integer read m_InitialPos.X;
    property InitialTop: integer read m_InitialPos.Y;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TChildFloatingForm = class(TForm)
  private
    m_Main: TMainFloatingForm;
    m_OffsetPoint: TPoint;
    procedure FRecalculateOffset(const NewMainPos: TPoint);
    procedure FCalculateInitialOffset;
    procedure FUpdateOffsetedPosition; overload;
    procedure FUpdateOffsetedPosition(const NewMainPos: TPoint); overload;
    procedure WMMoving(var Msg: TWMMoving); message WM_MOVING;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent; AMainFloatingForm: TMainFloatingForm); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Windows;

////////////////////////////////////////////////////////////////////////////////
//  TChildFloatingForm

constructor TChildFloatingForm.Create(AOwner: TComponent;
  AMainFloatingForm: TMainFloatingForm);
begin
  m_Main := AMainFloatingForm;

  inherited Create(AOwner);

  m_Main.FAddChild(self);

  FCalculateInitialOffset;
  FUpdateOffsetedPosition;
end;


destructor TChildFloatingForm.Destroy;
begin
  m_Main.FRemoveChild(self);
  inherited;
end;


procedure TChildFloatingForm.FRecalculateOffset(const NewMainPos: TPoint);
begin
  m_OffsetPoint.X := Left - NewMainPos.X;
  m_OffsetPoint.Y := Top - NewMainPos.Y;
end;


procedure TChildFloatingForm.FCalculateInitialOffset;
begin
  m_OffsetPoint.X := Left - m_Main.InitialLeft;
  m_OffsetPoint.Y := Top - m_Main.InitialTop;
end;


procedure TChildFloatingForm.FUpdateOffsetedPosition;
begin
  FUpdateOffsetedPosition(Point(m_Main.Left, m_Main.Top))
end;


procedure TChildFloatingForm.FUpdateOffsetedPosition(const NewMainPos: TPoint);

  procedure NCheckOutOfDesktop(var R: TRect);
  var
    iDX, iDY: integer;
  begin
    iDX := 0;
    iDY := 0;

    with Screen.DesktopRect do
    begin
      if (R.Bottom > Bottom) then
        iDY := Bottom - R.Bottom;
      if (R.Right > Right) then
        iDX := Right - R.Right;
      if (R.Top < Top) then
        iDY := Top - R.Top;
      if (R.Left < Left) then
        iDX := Left - R.Left;
    end;

    OffsetRect(R, iDX, iDY);
  end;

var
  R: TRect;
  NewPos: TPoint;
begin // .FUpdateOffsetedPosition
  NewPos := Point(NewMainPos.X + m_OffsetPoint.X, NewMainPos.Y + m_OffsetPoint.Y);

  R := BoundsRect;
  OffsetRect(R, NewPos.X - R.Left, NewPos.Y - R.Top);

  NCheckOutOfDesktop(R);

  BoundsRect := R;
end;


procedure TChildFloatingForm.WMMoving(var Msg: TWMMoving);
begin
  FRecalculateOffset(Point(m_Main.Left, m_Main.Top));
  inherited;
end;


procedure TChildFloatingForm.WMSize(var Message: TWMSize);
begin
  FRecalculateOffset(Point(m_Main.Left, m_Main.Top));
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TMainFloatingForm

constructor TMainFloatingForm.Create(AOwner: TComponent);
begin
  inherited;
  m_lstChilds := TList.Create;
  m_InitialPos := Point(Left, Top);
end;


destructor TMainFloatingForm.Destroy;
begin
  m_lstChilds.Free;
  inherited;
end;


procedure TMainFloatingForm.FAddChild(AChild: TChildFloatingForm);
begin
  m_lstChilds.Add(AChild);
end;


procedure TMainFloatingForm.FRemoveChild(AChild: TChildFloatingForm);
begin
  m_lstChilds.Remove(AChild);
end;


procedure TMainFloatingForm.WMMoving(var Msg: TWMMoving);
var
  i: integer;
begin
  if ((GetKeyState(VK_CONTROL) and (not $7FFF)) = 0) then
  begin
    for i := 0 to m_lstChilds.Count - 1 do
      TChildFloatingForm(m_lstChilds[i]).FUpdateOffsetedPosition;
  end
  else
    FRecalculateOffsetForEachChild(Point(Msg.DragRect.Left, Msg.DragRect.Top));

  inherited;
end;


procedure TMainFloatingForm.WMSize(var Message: TWMSize);
begin
  FRecalculateOffsetForEachChild(Point(Left, Top));
  inherited;
end;


procedure TMainFloatingForm.FRecalculateOffsetForEachChild(const NewPos: TPoint);
var
  i: integer;
begin
  if (Assigned(m_lstChilds)) then
  begin
    for i := 0 to m_lstChilds.Count - 1 do
      TChildFloatingForm(m_lstChilds[i]).FRecalculateOffset(NewPos);
  end;
end;

end.
