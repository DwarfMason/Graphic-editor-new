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
    Pointed: boolean;
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

  { TPolyLine }

  TPolyLine = class(TBigFigureClass)
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


implementation

var
  FiguresData: array of TBigFigureClass;
  AMinCor, AMaxCor: TFloatPoint;

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
begin
  AMinCor.x := 5 - MaxInt;
  AMinCor.y := 5 - MaxInt;
  AMaxCor.x := MaxInt;
  AMaxCor.y := MaxInt;

end;

procedure UnselectAll;
begin
  AMinCor.x := MaxInt;
  AMinCor.y := MaxInt;
  AMaxCor.x := 5 - MaxInt;
  AMaxCor.y := 5 - MaxInt;

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

{ TPolyLine }

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  counter: SizeInt;
begin
  inherited;
  ACanvas.Polyline(GetCanvasPoints());
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
const eps = 10;
var
  FigureTL, FigureBR: TFloatPoint;
  diag:Double;
begin
  diag:=sqrt(sqr(SelectionBR.x-SelectionTL.x)+sqr(SelectionTL.y-SelectionBR.y));
  FigureBR := WorldToScreen(BottomRight.x, BottomRight.y);
  FigureTL := WorldToScreen(TopLeft.x, TopLeft.y);
  If eps <= diag then begin
  Result := (SelectionTL.x <= FigureTL.x) and (SelectionTL.y <= FigureTL.y) and
    (SelectionBR.x >= FigureBR.x) and (SelectionBR.Y >= FigureBR.Y);
  pointed:= false;
  end
  else
  begin
    Result := (SelectionTL.x >= FigureTL.x) and (SelectionTL.y >= FigureTL.y)
    and
    (SelectionTL.x <= FigureBR.x) and (SelectionTL.Y <= FigureBR.Y);
    pointed:= true;
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
  i:SizeInt;
begin
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
  If pointed = false then
  for Counter := 0 to FiguresCount() - 1 do
    GetFigure(Counter).selected := GetFigure(Counter).InRectangle(AMinCor, AMaxCor)
    else begin
    for Counter := 0 to FiguresCount() - 1 do begin
    GetFigure(Counter).selected := GetFigure(Counter).InRectangle(AMinCor, AMaxCor);
    If GetFigure(Counter).selected then
    For i:=0 to Counter-1 do
    GetFigure(i).selected:=false;
    end;
    end;
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
var
  FigureTL, FigureBR: TPoint;
begin
  if Selected then
  begin
    FigureTL := WorldToScreen(TopLeft.x, TopLeft.y);
    FigureBR := WorldToScreen(BottomRight.x, BottomRight.y);
    ACanvas.Pen.color := clBlue;
    ACanvas.Pen.Width := 2;
    ACanvas.Pen.Style := psDash;
    ACanvas.Brush.Style := BsClear;
    ACanvas.Rectangle(FigureTL.x - (FWidth div 2) - 3, FigureTL.y -
      (FWidth div 2) - 3, FigureBR.x + (FWidth div 2) + 3, FigureBR.y + (FWidth div 2) + 3);
  end;

end;

begin
 UnselectAll;
end.

