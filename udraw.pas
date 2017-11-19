unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Math, UScale;

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
  public
    function PointsCount(): SizeInt;
    procedure addPoint(AValue: TFloatPoint);
    function GetPoints(AIndex: SizeInt): TFloatPoint;
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure SetPoint(AIndex: SizeInt; AValue: TFloatPoint);
    function GetCanvasPoints(): TPointArray;
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
  strict protected
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TBigFigureClass)
  strict protected
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRndRectangle }

  TRndRectangle = class(TBigFigureClass)
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    FRadius: integer;
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TCanvasFigure = class of TBigFigureClass;

function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
function GetFigure(AIndex: SizeInt): TBigFigureClass;
function FiguresCount(): SizeInt;


implementation

var
  FiguresData: array of TBigFigureClass;

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

{ FFigureNone }

procedure FFigureNone.Draw(ACanvas: TCanvas);
begin

end;

{ TPolyLine }

procedure TPolyLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(GetCanvasPoints());
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
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
begin
  inherited;
  CPoints := GetCanvasPoints();
  ACanvas.Line(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(GetCanvasPoints());
end;

{ TRndRectangle }

procedure TRndRectangle.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  CPoints := GetCanvasPoints();
  ACanvas.RoundRect(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y, 40, 40);
end;

{ TBigFigureClass }

function TBigFigureClass.PointsCount: SizeInt;
begin
  Result := Length(FPoints);
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

end.

