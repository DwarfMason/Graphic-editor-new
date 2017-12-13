unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Math, UScale, UParam;

const
  cFigureIndexInvalid = -1;

type
  TPointArray = array of TPoint;

  { TBigFigureClass }

  TBigFigureClass = class
    FPoints: array of TFloatPoint;
  strict protected
    FWidth: integer;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    FRadius: integer;
  public
    selected: boolean;
    function PointsCount(): SizeInt;
    property Width: integer read FWidth write FWidth;
    property PenColor: TColor read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property Radius: integer read FRadius write FRadius;
    function InRectangle(SelectionTL, SelectionBR: TFloatPoint): boolean;
    procedure addPoint(AValue: TFloatPoint);
    function GetPoints(AIndex: SizeInt): TFloatPoint;
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure SetPoint(AIndex: SizeInt; AValue: TFloatPoint);
    function GetCanvasPoints(): TPointArray;
    function TopLeft(): TFloatPoint;
    function BottomRight(): TFloatPoint;
    procedure SelectionDraw(ACanvas: TCanvas);
    procedure MoveFigure(dX, dY: extended);
    procedure ResizeFigure(APointIndex: SizeInt; dX, dY: extended);
    procedure WideResizeL(dX, dY: extended);
    procedure WideResizeR(dX, dY: extended);
  end;


  { FFigureSelection }

  FFigureSelection = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { FFigureNone }

  FFigureNone = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TPencil }

  TPencil = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TLine }

  TLine = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TBigFigureClass)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TBigFigureClass)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRndRectangle }

  TRndRectangle = class(TBigFigureClass)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TCanvasFigure = class of TBigFigureClass;

function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
function GetFigure(AIndex: SizeInt): TBigFigureClass;
function FiguresCount(): SizeInt;
procedure DeleteLastFigure(AIndex: SizeInt);
procedure DeleteSelected();
procedure PSelectAll();
procedure UnSelectAll();
procedure MoveUp();
procedure MoveDown();

var
  FiguresData: array of TBigFigureClass;
  AMinCor, AMaxCor: TFloatPoint;
  pointed: boolean;
  SelectionTopLeft, SelectionBottomRight: TPoint;

implementation

procedure DeleteLastFigure(AIndex: SizeInt);
begin
  FreeAndNil(FiguresData[aIndex]);
  SetLength(FiguresData, Length(FiguresData) - 1);
end;

procedure DeleteSelected();
var
  i, j, k: integer;
begin
  j := 0;
  for i := Low(FiguresData) to High(FiguresData) do
    if (FiguresData[i] <> nil) and (FiguresData[i].Selected) then
    begin
      FreeAndNil(FiguresData[i]);
      Inc(j);
    end;
  for k := 1 to j do
    for i := Low(FiguresData) to High(FiguresData) do
    begin
      if (FiguresData[i] = nil) and (i + 1 < Length(FiguresData)) then
      begin
        FiguresData[i] := FiguresData[i + 1];
        FiguresData[i + 1] := nil;
      end;
    end;
  SetLength(FiguresData, Length(FiguresData) - j);
end;

procedure PSelectAll;
var
  i: SizeInt;
begin
  for i := 0 to FiguresCount() - 1 do
  begin
    GetFigure(i).selected := True;
  end;
end;

procedure UnselectAll;
var
  i: SizeInt;
begin
  for i := 0 to FiguresCount() - 1 do
  begin
    GetFigure(i).selected := False;
  end;
     end;

procedure MoveUp;
var
  i: SizeInt;
  t: TBigFigureClass;
begin
  for i := High(FiguresData) downto Low(FiguresData) do
  begin
    if FiguresData[i].Selected and (i + 1 < Length(FiguresData)) then
    begin
      t := FiguresData[i + 1];
      FiguresData[i + 1] := FiguresData[i];
      FiguresData[i] := t;
    end;
  end;
end;

procedure MoveDown;
var
  i: SizeInt;
  t: TBigFigureClass;
begin
  for i := Low(FiguresData) to High(FiguresData) do
  begin
    if FiguresData[i].Selected and (i - 1 >= 0) then
    begin
      t := FiguresData[i - 1];
      FiguresData[i - 1] := FiguresData[i];
      FiguresData[i] := t;
    end;
  end;
end;

function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
begin
  Result := Length(FiguresData);
  SetLength(FiguresData, Result + 1);
  FiguresData[Result] := AFigureClass.Create;
end;

function GetFigure(AIndex: SizeInt): TBigFigureClass;
begin
  Result := FiguresData[AIndex];
end;

function FiguresCount: SizeInt;
begin
  Result := Length(FiguresData);
end;

{ FFigureSelection }

procedure FFigureSelection.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;

begin
  ACanvas.Pen.color := clBlue;
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := BsClear;
  CPoints := GetCanvasPoints();
  ACanvas.Rectangle(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
  AMinCor.x := min(CPoints[0].x, CPoints[1].x);
  AMinCor.y := min(CPoints[0].y, CPoints[1].y);
  AMaxCor.x := max(CPoints[0].x, CPoints[1].x);
  AMaxCor.y := max(CPoints[0].y, CPoints[1].y);
end;

{ FFigureNone }

procedure FFigureNone.Draw(ACanvas: TCanvas);
begin

end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
  Counter: SizeInt;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  CPoints := GetCanvasPoints();
  ACanvas.Ellipse(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
  Counter: SizeInt;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  CPoints := GetCanvasPoints();
  ACanvas.Rectangle(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
  Counter: SizeInt;
begin
  inherited;
  CPoints := GetCanvasPoints();
  ACanvas.Line(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var
  Counter: SizeInt;
begin
  inherited;
  ACanvas.Polyline(GetCanvasPoints());
end;

{ TRndRectangle }

procedure TRndRectangle.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
  Counter: SizeInt;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  CPoints := GetCanvasPoints();
  ACanvas.RoundRect(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y,
    Radius, Radius);
end;

{ TBigFigureClass }

function TBigFigureClass.PointsCount: SizeInt;
begin
  Result := Length(FPoints);
end;

function TBigFigureClass.InRectangle(SelectionTL, SelectionBR: TFloatPoint): boolean;
const
  eps = 10;
var
  FigureTL, FigureBR: TFloatPoint;
  diag: double;
begin
  diag := sqrt(sqr(SelectionBR.x - SelectionTL.x) + sqr(SelectionTL.y - SelectionBR.y));
  FigureBR := WorldToScreen(BottomRight.x, BottomRight.y);
  FigureTL := WorldToScreen(TopLeft.x, TopLeft.y);
  if eps <= diag then
  begin
    Result := (SelectionTL.x <= FigureTL.x) and (SelectionTL.y <= FigureTL.y) and
      (SelectionBR.x >= FigureBR.x) and (SelectionBR.Y >= FigureBR.Y);
    pointed := False;
  end
  else
  begin
    Result := (SelectionTL.x >= FigureTL.x) and (SelectionTL.y >= FigureTL.y) and
      (SelectionTL.x <= FigureBR.x) and (SelectionTL.Y <= FigureBR.Y);
    pointed := True;
  end;
end;

procedure TBigFigureClass.addPoint(AValue: TFloatPoint);
var
  len: integer;
begin
  len := length(FPoints);
  SetLength(FPoints, len + 1);
  FPoints[len] := AValue;
end;

function TBigFigureClass.GetPoints(AIndex: SizeInt): TFloatPoint;
begin
  Result := FPoints[AIndex];
end;

procedure TBigFigureClass.Draw(ACanvas: TCanvas);
var
  counter: SizeInt;
  i: SizeInt;
begin
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
end;

procedure TBigFigureClass.SetPoint(AIndex: SizeInt; AValue: TFloatPoint);
begin
  FPoints[AIndex] := AValue;
end;

function TBigFigureClass.GetCanvasPoints: TPointArray;
var
  CanvasPoints: TPointArray;
  i: integer;
begin
  SetLength(CanvasPoints, Length(FPoints));
  for i := 0 to Length(FPoints) - 1 do
    CanvasPoints[i] := WorldToScreen(FPoints[i].x, FPoints[i].y);
  Result := CanvasPoints;
end;

function TBigFigureClass.BottomRight: TFloatPoint;
var
  i: TFloatPoint;
begin
  Result := fPoints[0];
  for i in fPoints do
  begin
    Result.x := max(Result.x, i.x);
    Result.y := max(Result.y, i.y);
  end;
end;

function TBigFigureClass.TopLeft: TFloatPoint;
var
  i: TFloatPoint;
begin
  Result.x := FPoints[0].x;
  Result.y := FPoints[0].y;
  for i in fPoints do
  begin
    Result.x := min(Result.x, i.x);
    Result.y := min(Result.y, i.y);
  end;
end;

procedure TBigFigureClass.SelectionDraw(ACanvas: TCanvas);
const
  PADDING = 5;
var
  i: Sizeint;
  FigureTL, FigureBR, AllFiguresTL, AllFiguresBR: TPoint;
  AnchorPoint: TPoint;
begin
  FigureTL := TopLeft;
  FigureBR := BottomRight;
  AllFiguresTL := Point(MaxInt, MaxInt);
  AllFiguresBR := Point(5 - MaxInt, 5 - MaxInt);
  for i := 0 to FiguresCount() - 1 do
  begin
    if (GetFigure(i).selected) and (GetFigure(i).TopLeft.x < AllFiguresTL.x) then
      AllFiguresTL.x := round(GetFigure(i).TopLeft.x);
    if (GetFigure(i).selected) and (GetFigure(i).TopLeft.y < AllFiguresTL.y) then
      AllFiguresTL.y := Round(GetFigure(i).TopLeft.y);
    if (GetFigure(i).selected) and (GetFigure(i).BottomRight.y > AllFiguresBR.y) then
      AllFiguresBR.y := Round(GetFigure(i).BottomRight.y);
    if (GetFigure(i).selected) and (GetFigure(i).BottomRight.x > AllFiguresBR.x) then
      AllFiguresBR.x := Round(GetFigure(i).BottomRight.x);
  end;
  AllFiguresTL := WorldToScreen(AllFiguresTL.x, AllFiguresTL.y);
  AllFiguresBR := WorldToScreen(AllFiguresBR.x, AllFiguresBr.y);
  FigureTL := WorldToScreen(FigureTL.x, FigureTL.y);
  FigureBR := WorldToScreen(FigureBR.x, FigureBR.y);
  with ACanvas do
  begin
    Pen.color := clBlack;
    Pen.Width := 2;
    Brush.Style := BsSolid;
    Brush.Color := clWhite;
    for i := low(FPoints) to High(FPoints) do
    begin
      AnchorPoint := WorldToScreen(FPoints[i].x, FPoints[i].y);
      Rectangle(AnchorPoint.x - PADDING, AnchorPoint.y - PADDING,
        AnchorPoint.x + PADDING, AnchorPoint.y + PADDING);
          Rectangle(AllFiguresTL.x - 2*PADDING, AllFiguresTL.y - 2*PADDING,
        AllFiguresTL.x, AllFiguresTL.y);
  Rectangle(AllFiguresBR.x, AllFiguresBR.y,
        AllFiguresBR.x + 2*PADDING, AllFiguresBR.y + 2*PADDING);
    end;
    Pen.color := clBlue;
    Pen.Width := 3;
    Pen.Style := psDash;
    Brush.Style := BsClear;
    Rectangle(FigureTL.x - (FWidth div 2), FigureTL.y -
      (FWidth div 2), FigureBR.x + (FWidth div 2),
      FigureBR.y + (FWidth div 2));
    Rectangle(AllFiguresTL.x - (FWidth div 2) - PADDING, AllFiguresTL.y - PADDING -
    (FWidth div 2), AllFiguresBR.x + (FWidth div 2) + PADDING, AllFiguresBR.y + PADDING + (FWidth div 2));
  end;
  SelectionBottomRight := AllFiguresBR;
  SelectionTopLeft := AllFiguresTL;
end;

procedure TBigFigureClass.MoveFigure(dX, dY: extended);
var
  i: SizeInt;
begin
  for i := Low(FPoints) to High(FPoints) do
  begin
    FPoints[i].x := FPoints[i].x + dx;
    FPoints[i].y := FPoints[i].y + dy;
  end;
end;

procedure TBigFigureClass.ResizeFigure(APointIndex: SizeInt; dX,
  dY: extended);
begin
  FPoints[APointIndex].x:=FPoints[APointIndex].x+dx;
  FPoints[APointIndex].y:=FPoints[APointIndex].y+dy;
end;

procedure TBigFigureClass.WideResizeL(dX, dY: extended);
var
  i:SizeInt;
begin
  for i := Low(FPoints) to High(FPoints) do
  begin
    FPoints[i].x:=FPoints[i].x +dx*(FPoints[i].x-SelectionBottomRight.x)/(SelectionTopLeft.x-SelectionBottomRight.x);
    FPoints[i].y:=FPoints[i].y +dy*(FPoints[i].y-SelectionBottomRight.y)/(SelectionTopLeft.y-SelectionBottomRight.y);
  end;
end;

procedure TBigFigureClass.WideResizeR(dX, dY: extended);
var
  i:SizeInt;
begin
  for i := Low(FPoints) to High(FPoints) do
  begin
    FPoints[i].x:=FPoints[i].x +dx*(FPoints[i].x-SelectionTopLeft.x)/(SelectionBottomRight.x-SelectionTopLeft.x);
    FPoints[i].y:=FPoints[i].y +dy*(FPoints[i].y-SelectionTopLeft.y)/(SelectionBottomRight.y-SelectionTopLeft.y);
  end;
end;

begin
  UnselectAll;
end.
