unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Math, UScale, UParam, typinfo;

const
  cFigureIndexInvalid = -1;

type
  TPointArray = array of TPoint;
  TClassList = array of TPersistentClass;

  { TBigFigureClass }

  TBigFigureClass = class(TPersistent)
  protected
    FWidth: TWidthParam;
    FPenStyle: TPenStyleParam;
    FPenColor: TPenColorParam;
    FBrushStyle: TBrushStyleParam;
    FBrushColor: TBrushColorParam;
    FRadius: TRadiusParam;
  public
    FPoints: array of TFloatPoint;
    selected: boolean;
    function PointsCount(): SizeInt;
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
    function GetParams: TParamList; virtual; abstract;
    function Clone(): TBigFigureClass;
  published
    property Width: TWidthParam read FWidth write FWidth;
    property PenColor: TPenColorParam read FPenColor write FPenColor;
    property PenStyle: TPenStyleParam read FPenStyle write FPenStyle;
    property BrushStyle: TBrushStyleParam read FBrushStyle write FBrushStyle;
    property BrushColor: TBrushColorParam read FBrushColor write FBrushColor;
    property Radius: TRadiusParam read FRadius write FRadius;
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
    function GetParams: TParamList; override;
  end;

  { TLine }

  TLine = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
    function GetParams: TParamList; override;
  end;

  { TRectangle }

  TRectangle = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
    function GetParams: TParamList; override;
  end;

  { TEllipse }

  TEllipse = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
    function GetParams: TParamList; override;
  end;

  { TRndRectangle }

  TRndRectangle = class(TBigFigureClass)
    procedure Draw(ACanvas: TCanvas); override;
    function GetParams: TParamList; override;
  end;

  TCanvasFigure = class of TBigFigureClass;
  TFigureArray = array of TBigFigureClass;

function AddFigure(AFigureClass: TCanvasFigure): SizeInt;
function GetFigure(AIndex: SizeInt): TBigFigureClass;
function FiguresCount(): SizeInt;
procedure DeleteLastFigure(AIndex: SizeInt);
procedure DeleteSelected();
procedure PSelectAll();
procedure UnSelectAll();
procedure MoveUp();
procedure MoveDown();
function GetSelectionParams: TParamList;
function StrToClassFigure(str: string): TCanvasFigure;
function GetProps(AObject: TObject; var APropList: PPropList): SizeInt;
function CloneFigures(Source: TFigureArray): TFigureArray;


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
  Param: TParam;
  ParamList: TParamList;
begin
  if FiguresData <> nil then
  begin
    ParamList := GetSelectionParams;
    if ParamList <> nil then
      for param in GetSelectionParams do
        Param.UnAttach();
    for i := 0 to FiguresCount() - 1 do
    begin
      GetFigure(i).selected := False;
    end;
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

function GetSelectionParams: TParamList;
var
  Figure: TBigFigureClass;
  FigureParams: TParamList;
  SameParams: array of TParamList;
  Param0, Param1: TParam;
  i, j, SelectionCount: SizeInt;
begin
  SelectionCount := 0;
  for Figure in FiguresData do
  begin
    if Figure.selected then
    begin
      Inc(SelectionCount);
      FigureParams := Figure.GetParams;
      if length(SameParams) = 0 then
      begin
        SetLength(SameParams, Length(FigureParams));
        for i := Low(FigureParams) to High(FigureParams) do
        begin
          Setlength(SameParams[i], 1);
          SameParams[i][0] := FigureParams[i];
        end;
      end
      else
      begin
        for Param0 in FigureParams do
        begin
          for i := Low(SameParams) to High(SameParams) do
          begin
            Param1 := SameParams[i][Length(SameParams[i]) - 1];
            if (Param1.ClassType = Param0.ClassType) then
            begin
              SetLength(SameParams[i], Length(SameParams[i]) + 1);
              SameParams[i][Length(SameParams[i]) - 1] := Param0;
            end;
          end;
        end;
      end;
    end;
  end;
  for i := Low(SameParams) to High(SameParams) do
  begin
    if Length(SameParams[i]) = SelectionCount then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[length(Result) - 1] := SameParams[i][0];
      for j := 0 to High(SameParams[i]) do
        Result[Length(Result) - 1].AttachParam(SameParams[i][j]);
    end;
  end;
end;

function StrToClassFigure(str: string): TCanvasFigure;
begin
  case str of
    'TPencil': Result := TPencil;
    'TLine': Result := TLine;
    'TRectangle': Result := TRectangle;
    'TEllipse': Result := TEllipse;
    'TRndRectangle': Result := TRndRectangle;
  end;
end;

function GetProps(AObject: TObject; var APropList: PPropList): SizeInt;
var
  Count: SizeInt;
begin
  Count := GetPropList(AObject.ClassInfo, tkAny, nil);
  Result := Count;
  GetMem(APropList, Count * SizeOf(PPropInfo));
  GetPropList(AObject.ClassInfo, tkAny, APropList);
end;

function CloneFigures(Source: TFigureArray): TFigureArray;
var
  i: SizeInt;
begin
  if Source = nil then
    exit(nil);
  for i := low(Source) to High(Source) do
  begin
    if Source[i] <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Source[i].Clone;
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
  ACanvas.Brush.Color := FBrushColor.Value;
  ACanvas.Brush.Style := FBrushStyle.Value;
  CPoints := GetCanvasPoints();
  ACanvas.Ellipse(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

function TEllipse.GetParams: TParamList;
begin
  Result := TParamList.Create(FPenColor, FBrushColor, FWidth, FPenStyle, FBrushStyle);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
  Counter: SizeInt;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor.Value;
  ACanvas.Brush.Style := FBrushStyle.Value;
  CPoints := GetCanvasPoints();
  ACanvas.Rectangle(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y);
end;

function TRectangle.GetParams: TParamList;
begin
  Result := TParamList.Create(FPenColor, FBrushColor, FWidth, FPenStyle, FBrushStyle);
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

function TLine.GetParams: TParamList;
begin
  Result := TParamList.Create(FPenColor, FWidth, FPenStyle);
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(GetCanvasPoints());
end;

function TPencil.GetParams: TParamList;
begin
  Result := TParamList.Create(FPenColor, FWidth, FPenStyle);
end;

{ TRndRectangle }

procedure TRndRectangle.Draw(ACanvas: TCanvas);
var
  CPoints: TPointArray;
begin
  inherited;
  ACanvas.Brush.Color := FBrushColor.Value;
  ACanvas.Brush.Style := FBrushStyle.Value;
  CPoints := GetCanvasPoints();
  ACanvas.RoundRect(CPoints[0].x, CPoints[0].y, CPoints[1].x, CPoints[1].y,
    FRadius.Value, FRadius.Value);
end;

function TRndRectangle.GetParams: TParamList;
begin
  Result := TParamList.Create(FPenColor, FBrushColor, FWidth, FPenStyle,
    FBrushStyle, FRadius);
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
begin
  ACanvas.Pen.Width := FWidth.Value;
  ACanvas.Pen.Style := FPenStyle.Value;
  ACanvas.Pen.Color := FPenColor.Value;
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
      Rectangle(AllFiguresTL.x - 2 * PADDING, AllFiguresTL.y - 2 * PADDING,
        AllFiguresTL.x, AllFiguresTL.y);
      Rectangle(AllFiguresBR.x, AllFiguresBR.y,
        AllFiguresBR.x + 2 * PADDING, AllFiguresBR.y + 2 * PADDING);
    end;
    Pen.color := clBlue;
    Pen.Width := 3;
    Pen.Style := psDash;
    Brush.Style := BsClear;
    Rectangle(FigureTL.x, FigureTL.y,
      FigureBR.x,
      FigureBR.y);
    Rectangle(AllFiguresTL.x - PADDING, AllFiguresTL.y - PADDING,
      AllFiguresBR.x + PADDING,
      AllFiguresBR.y + PADDING);
  end;
  SelectionBottomRight := Point(AllFiguresBR.x + PADDING, AllFiguresBR.y + PADDING);
  SelectionTopLeft := Point(AllFiguresTL.x - PADDING, AllFiguresTL.y - PADDING);
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

procedure TBigFigureClass.ResizeFigure(APointIndex: SizeInt; dX, dY: extended);
begin
  FPoints[APointIndex].x := FPoints[APointIndex].x + dx;
  FPoints[APointIndex].y := FPoints[APointIndex].y + dy;
end;

procedure TBigFigureClass.WideResizeL(dX, dY: extended);
var
  i: SizeInt;
  TempX, TempY: Float;
begin
  TempX := SelectionTopLeft.x - SelectionBottomRight.x;
  TempY := SelectionTopLeft.y - SelectionBottomRight.y;
  if TempX = 0 then
    TempX := -1;
  if TempY = 0 then
    TempY := -1;
  for i := Low(FPoints) to High(FPoints) do
  begin
    FPoints[i].x := FPoints[i].x + dx *
      (FPoints[i].x - SelectionBottomRight.x) / (TempX);
    FPoints[i].y := FPoints[i].y + dy *
      (FPoints[i].y - SelectionBottomRight.y) / (Tempy);
  end;
end;

procedure TBigFigureClass.WideResizeR(dX, dY: extended);
var
  i: SizeInt;
  TempX, TempY: Float;
begin
  TempX := SelectionBottomRight.x - SelectionTopLeft.x;
  TempY := SelectionBottomRight.y - SelectionTopLeft.y;
  if TempX = 0 then
    TempX := -1;
  if TempY = 0 then
    TempY := -1;
  for i := Low(FPoints) to High(FPoints) do
  begin
    FPoints[i].x := FPoints[i].x + dx * (FPoints[i].x - SelectionTopLeft.x) / (Tempx);
    FPoints[i].y := FPoints[i].y + dy * (FPoints[i].y - SelectionTopLeft.y) / (TempY);
  end;
end;

function TBigFigureClass.Clone: TBigFigureClass;
var
  i: TFloatPoint;
begin
  Result := Self.ClassType.Create as TBigFigureClass;
  for i in FPoints do
  begin
    SetLength(Result.FPoints, Length(Result.FPoints) + 1);
    Result.FPoints[High(Result.FPoints)].x := i.x;
    Result.FPoints[High(Result.FPoints)].y := i.y;
  end;
  if Width <> nil then
    Result.Width := Width.Copy;
  if PenColor <> nil then
    Result.PenColor := PenColor.Copy;
  if PenStyle <> nil then
    Result.PenStyle := PenStyle.Copy;
  if BrushColor <> nil then
    Result.BrushColor := BrushColor.Copy;
  if BrushStyle <> nil then
    Result.BrushStyle := BrushStyle.Copy;
  if Radius <> nil then
    Result.Radius := Radius.Copy;
end;

begin
  RegisterClasses(TClassList.Create(TPencil, TLine, TRectangle, TEllipse,
    TRndRectangle));
  UnselectAll;
end.
