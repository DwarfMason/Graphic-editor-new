unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Math;

const
  cFigureIndexInvalid = -1;

type
  TPointArray = array of TPoint;

  { TBigFigureClass }

  TBigFigureClass = class
    FPoints: array of TPoint;
  strict protected
    FWidth: integer;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
  public
    function PointsCount(): SizeInt;
    procedure addPoint(AValue: TPoint);
    function GetPoints(AIndex: SizeInt): TPoint;
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure SetPoint(AIndex: SizeInt; AValue: TPoint);
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

  TCanvasFigure = class of TBigFigureClass ;

  function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
  function GetFigure(AIndex: SizeInt): TBigFigureClass;
  function FiguresCount(): SizeInt;


implementation

var
  FiguresData: array of TBigFigureClass;

function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
begin
  Result:= Length(FiguresData);
  SetLength(FiguresData, Result+1);
  FiguresData[Result] := AFigureClass.Create;
end;

function GetFigure(AIndex: SizeInt): TBigFigureClass;
begin
 Result:= FiguresData[AIndex];
end;

function FiguresCount: SizeInt;
begin
  Result:=Length(FiguresData);
end;

{ TPolyLine }

procedure TPolyLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(FPoints);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Ellipse(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Rectangle(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(FPoints);
end;

{ TRndRectangle }

procedure TRndRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.RoundRect(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y,5,5);
end;

{ TBigFigureClass }

function TBigFigureClass.PointsCount: SizeInt;
begin
  Result:=Length(FPoints);
end;

procedure TBigFigureClass.addPoint(AValue: TPoint);
var
  len: integer;
begin
  len := length(FPoints);
  SetLength(FPoints, len + 1);
  FPoints[len] := AValue;
end;

function TBigFigureClass.GetPoints(AIndex: SizeInt): TPoint;
begin
  Result := FPoints[AIndex];
end;

procedure TBigFigureClass.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := FWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Pen.Color := FPenColor;
end;

procedure TBigFigureClass.SetPoint(AIndex: SizeInt; AValue: TPoint);
begin
  FPoints[AIndex] := AValue;
end;

end.
