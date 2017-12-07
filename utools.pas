unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDraw, UScale, Controls, UParam;

type

  { TBigTools }

  TBigTools = class
  private
    class function GetParams: TParamList; static; virtual;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); virtual; abstract;
    class property Params: TParamList read GetParams;
    class function GetName(): string; virtual; abstract;
    class function GetFigureClass(): TCanvasFigure; virtual; abstract;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); virtual;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; virtual;
    class function Step(AFigureIndex: SizeInt; AXY: Tpoint): boolean; virtual; abstract;
    class function Finish(AFigureIndex: SizeInt): boolean; virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TBigTools)
  private
    class var
    FParams: TParamList;
    class function GetParams: TParamList; static; override;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TLineTool }

  TLineTool = class(TBigTools)
  private
    class var
    FParams: TParamList;
    class function GetParams: TParamList; static; override;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TBigTools)
  private
    class var
    FParams: TParamList;
    class function GetParams: TParamList; static; override;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TBigTools)
  private
    class var
    FParams: TParamList;
    class function GetParams: TParamList; static; override;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TRndRectangleTool }

  TRndRectangleTool = class(TBigTools)
  private
    class var
    FParams: TParamList;
    class function GetParams: TParamList; static; override;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TZoomTool }

  TZoomTool = class(TBigTools)
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Finish(AFigureIndex: SizeInt): boolean; override;
  end;

  { THandTool }

  THandTool = class(TBigTools)
    class var
    CanDraw: boolean;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class procedure Start(AFigureIndex: SizeInt; AXY: TPoint); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Finish(AFigureIndex: SizeInt): boolean; override;
  end;

  { TSelectionTool }

  TSelectionTool = class(TBigTools)
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Finish(AFigureIndex: SizeInt): boolean; override;
  end;

  { TClickTool }

  TClickTool = class(TBigTools)
    class var
    StartX, StartY: double;
    IsDraggable: boolean;
    IsResizable:boolean;
    FigureIndex: SizeInt;
    AnchorIndex: SizeInt;
  public
    class procedure SetFigureParams(AFigureIndex: SizeInt); override;
    class procedure Start(AFigureIndex: SizeInt; AXY: TPoint); override;
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Finish(AFigureIndex: SizeInt): boolean; override;
  end;



  TTools = class of TBigTools;

function GetTool(AIndex: SizeInt): TTools;
function ToolsCount(): SizeInt;
procedure SetBtn(Button: TMouseButton);


implementation

type
  TToolsArray = array of TTools;

var
  ToolsClasses: TToolsArray;
  MBtn: TMouseButton;

procedure SetBtn(Button: TMouseButton);
begin
  MBtn := Button;
end;

{ TClickTool }

class procedure TClickTool.SetFigureParams(AFigureIndex: SizeInt);
begin

end;

class procedure TClickTool.Start(AFigureIndex: SizeInt; AXY: TPoint);
const
  PADDING=5;
var
  i,j:SizeInt;
  ScaleResize:TPoint;
begin
  inherited Start(AFigureIndex, AXY);
  if (SelectionTopLeft.x <= AXY.x) and (SelectionTopLeft.y <= AXY.y) and
    (SelectionBottomRight.x >= AXY.x) and (SelectionBottomRight.y >= AXY.y) then
  begin
    IsDraggable := True;
      StartX := AXY.x;
      StartY := AXY.y;
  end;
  For i:=0 to FiguresCount()-1 do begin
    If GetFigure(i).selected then
    For j:=Low(GetFigure(i).FPoints) to High(GetFigure(i).FPoints) do begin
      ScaleResize:=WorldToScreen(GetFigure(i).FPoints[j].x,GetFigure(i).FPoints[j].y);
      If (ScaleResize.x - PADDING < AXY.x) and
        (ScaleResize.x + PADDING > AXY.x) and
        (ScaleResize.y - PADDING < AXY.y) and
        (ScaleResize.y + PADDING > AXY.y) then begin
        FigureIndex:= i;
        AnchorIndex:= j;
        IsResizable := True;
        IsDraggable := False;
        end;
    end;
  end;
end;

class function TClickTool.GetName: string;
begin
  Result := 'Курсор';
end;

class function TClickTool.GetFigureClass: TCanvasFigure;
begin
  Result := FFigureNone;
end;

class function TClickTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
var

  i: SizeInt;
begin
  Result := True;
  if IsDraggable then
    for i := 0 to FiguresCount() - 1 do
    begin
      if GetFigure(i).selected then
      begin
        GetFigure(i).MoveFigure(AXY.x - StartX, AXY.y - StartY);
      end;
    end;
  If IsResizable then
  GetFigure(FigureIndex).ResizeFigure(AnchorIndex, AXY.x - StartX, AXY.y - StartY);
  StartX := AXY.x;
  StartY := AXY.y;
end;

class function TClickTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

class function TClickTool.Finish(AFigureIndex: SizeInt): boolean;
begin
  Result := inherited Finish(AFigureIndex);
  IsDraggable := False;
  IsResizable:=False;
  DeleteLastFigure(AFigureIndex);
end;

{ TSelectionTool }

class procedure TSelectionTool.SetFigureParams(AFigureIndex: SizeInt);
begin

end;

class function TSelectionTool.GetName: string;
begin
  Result := 'Выделитель';
end;

class function TSelectionTool.GetFigureClass: TCanvasFigure;
begin
  Result := FFigureSelection;
end;

class procedure TSelectionTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
end;

class function TSelectionTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).SetPoint(1, ScreenToWorld(AXY.x, AXY.y));
end;

class function TSelectionTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

class function TSelectionTool.Finish(AFigureIndex: SizeInt): boolean;
var
  i: SizeInt;
  Counter: SizeInt;
begin
  Result := inherited Finish(AFigureIndex);
  DeleteLastFigure(AFigureIndex);
  if not pointed then
    for Counter := 0 to FiguresCount() - 1 do
      GetFigure(Counter).selected := GetFigure(Counter).InRectangle(AMinCor, AMaxCor)
  else
  begin
    for Counter := 0 to FiguresCount() - 1 do
    begin
      GetFigure(Counter).selected := GetFigure(Counter).InRectangle(AMinCor, AMaxCor);
      if GetFigure(Counter).selected then
        for i := 0 to Counter - 1 do
          GetFigure(i).selected := False;
    end;
  end;

end;

{ THandTool }

class procedure THandTool.SetFigureParams(AFigureIndex: SizeInt);
begin

end;

class procedure THandTool.Start(AFigureIndex: SizeInt; AXY: TPoint);
begin
  inherited Start(AFigureIndex, AXY);
  CanDraw := True;
end;

class function THandTool.GetName: string;
begin
  Result := 'Рука';
end;

class function THandTool.GetFigureClass: TCanvasFigure;
begin
  Result := FFigureNone;
end;

class function THandTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  if not CanDraw then
    Exit(False);
  Result := True;
  ScreenOffset.x := ScreenOffset.x + GetFigure(AFigureIndex).GetPoints(0).x -
    ScreenToWorld(AXY.x, AXY.y).x;
  ScreenOffset.y := ScreenOffset.y + GetFigure(AFigureIndex).GetPoints(0).y -
    ScreenToWorld(AXY.x, AXY.y).y;
end;

class function THandTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
  CanDraw := False;
end;

class function THandTool.Finish(AFigureIndex: SizeInt): boolean;
begin
  Result := inherited Finish(AFigureIndex);
  DeleteLastFigure(AFigureIndex);
end;

{ TBigTools }

class function TBigTools.GetParams: TParamList;
begin
  Result := nil;
end;

class procedure TBigTools.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  with GetFigure(AFigureIndex) do
  begin
    AddPoint(ScreenToWorld(AXY.x, AXY.y));
    AddPoint(ScreenToWorld(AXY.x, AXY.y));
  end;
end;

class function TBigTools.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := AFigureIndex <> cFigureIndexInvalid;
end;

class function TBigTools.Finish(AFigureIndex: SizeInt): boolean;
begin
  Result := AFigureIndex <> cFigureIndexInvalid;
end;

{ TZoomTool }

class procedure TZoomTool.SetFigureParams(AFigureIndex: SizeInt);
begin

end;

class function TZoomTool.GetName: string;
begin
  Result := 'Зум';
end;

class procedure TZoomTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  if (MBtn = mbExtra1) or (MBtn = mbLeft) then
    ZoomPoint(ScreenToWorld(AXY.x, AXY.y), Zoom * 2);

  if (MBtn = mbExtra2) or (Mbtn = mbRight) then
    ZoomPoint(ScreenToWorld(AXY.x, AXY.y), Zoom / 2);
end;

class function TZoomTool.GetFigureClass: TCanvasFigure;
begin
  Result := FFigureNone;
end;

class function TZoomTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

class function TZoomTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

class function TZoomTool.Finish(AFigureIndex: SizeInt): boolean;
begin
  Result := inherited Finish(AFigureIndex);
  DeleteLastFigure(AFigureIndex);
end;


function GetTool(AIndex: SizeInt): TTools;
begin
  Result := ToolsClasses[AIndex];
end;

function ToolsCount: SizeInt;
begin
  Result := Length(ToolsClasses);
end;

{ TRndRectangleTool }

class function TRndRectangleTool.GetParams: TParamList;
begin
  Result := FParams;
end;

class procedure TRndRectangleTool.SetFigureParams(AFigureIndex: SizeInt);
var
  FFigure: TBigFigureClass;
begin
  FFigure := GetFigure(AFigureIndex);
  FFigure.PenColor := (FParams[0] as TPenColorParam).Value;
  FFigure.Width := (FParams[2] as TWidthParam).Value;
  FFigure.PenStyle := (FParams[3] as TPenStyleParam).Value;
  FFigure.BrushColor := (FParams[1] as TBrushColorParam).Value;
  FFigure.BrushStyle := (FParams[4] as TBrushStyleParam).Value;
  FFigure.Radius := (FParams[5] as TRadiusParam).Value;
end;

class function TRndRectangleTool.GetName: string;
begin
  Result := 'Круглый не круг';
end;

class function TRndRectangleTool.GetFigureClass: TCanvasFigure;
begin
  Result := TRndRectangle;
end;

class procedure TRndRectangleTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TRndRectangleTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).SetPoint(1, ScreenToWorld(AXY.x, AXY.y));
end;

class function TRndRectangleTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

{ TEllipseTool }

class function TEllipseTool.GetParams: TParamList;
begin
  Result := FParams;
end;

class procedure TEllipseTool.SetFigureParams(AFigureIndex: SizeInt);
var
  FFigure: TBigFigureClass;
begin
  FFigure := GetFigure(AFigureIndex);
  FFigure.PenColor := (FParams[0] as TPenColorParam).Value;
  FFigure.Width := (FParams[2] as TWidthParam).Value;
  FFigure.PenStyle := (FParams[3] as TPenStyleParam).Value;
  FFigure.BrushColor := (FParams[1] as TBrushColorParam).Value;
  FFigure.BrushStyle := (FParams[4] as TBrushStyleParam).Value;
end;

class function TEllipseTool.GetName: string;
begin
  Result := 'Эллипс';
end;

class function TEllipseTool.GetFigureClass: TCanvasFigure;
begin
  Result := TEllipse;
end;

class procedure TEllipseTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TEllipseTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).SetPoint(1, ScreenToWorld(AXY.x, AXY.y));

end;

class function TEllipseTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

{ TRectangleTool }

class function TRectangleTool.GetParams: TParamList;
begin
  Result := FParams;
end;

class procedure TRectangleTool.SetFigureParams(AFigureIndex: SizeInt);
var
  FFigure: TBigFigureClass;
begin
  FFigure := GetFigure(AFigureIndex);
  FFigure.PenColor := (FParams[0] as TPenColorParam).Value;
  FFigure.Width := (FParams[2] as TWidthParam).Value;
  FFigure.PenStyle := (FParams[3] as TPenStyleParam).Value;
  FFigure.BrushColor := (FParams[1] as TBrushColorParam).Value;
  FFigure.BrushStyle := (FParams[4] as TBrushStyleParam).Value;
end;

class function TRectangleTool.GetName: string;
begin
  Result := 'Прямоугольник';
end;

class procedure TRectangleTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TRectangleTool.GetFigureClass: TCanvasFigure;
begin
  Result := TRectangle;
end;

class function TRectangleTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).SetPoint(1, ScreenToWorld(AXY.x, AXY.y));

end;

class function TRectangleTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

{ TLineTool }

class function TLineTool.GetParams: TParamList;
begin
  Result := FParams;
end;

class procedure TLineTool.SetFigureParams(AFigureIndex: SizeInt);
var
  FFigure: TBigFigureClass;
begin
  FFigure := GetFigure(AFigureIndex);
  FFigure.PenColor := (FParams[0] as TPenColorParam).Value;
  FFigure.Width := (FParams[1] as TWidthParam).Value;
  FFigure.PenStyle := (FParams[2] as TPenStyleParam).Value;
end;

class function TLineTool.GetName: string;
begin
  Result := 'Линия';
end;

class function TLineTool.GetFigureClass: TCanvasFigure;
begin
  Result := TLine;
end;

class procedure TLineTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TLineTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).SetPoint(1, ScreenToWorld(AXY.x, AXY.y));
end;

class function TLineTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;

{ TPencilTool }

class function TPencilTool.GetParams: TParamList;
begin
  Result := FParams;
end;

class procedure TPencilTool.SetFigureParams(AFigureIndex: SizeInt);
var
  FFigure: TBigFigureClass;
begin
  FFigure := GetFigure(AFigureIndex);
  FFigure.PenColor := (FParams[0] as TPenColorParam).Value;
  FFigure.Width := (FParams[1] as TWidthParam).Value;
  FFigure.PenStyle := (FParams[2] as TPenStyleParam).Value;
end;

class function TPencilTool.GetName: string;
begin
  Result := 'Карандаш';
end;

class procedure TPencilTool.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TPencilTool.GetFigureClass: TCanvasFigure;
begin
  Result := TPencil;
end;

class function TPencilTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  GetFigure(AFigureIndex).AddPoint(ScreenToWorld(AXY.x, AXY.y));
end;

class function TPencilTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := False;
end;


initialization

  ToolsClasses := TToolsArray.Create(TClickTool, TSelectionTool,
    THandTool, TZoomTool, TRndRectangleTool, TEllipseTool, TRectangleTool,
    TLineTool, TPencilTool);

  TLineTool.FParams := TParamList.Create(TPenColorParam.Create,
    TWidthParam.Create, TPenStyleParam.Create);

  TPencilTool.FParams := TParamList.Create(TPenColorParam.Create,
    TWidthParam.Create, TPenStyleParam.Create);

  TRectangleTool.FParams := TParamList.Create(TPenColorParam.Create,
    TBrushColorParam.Create, TWidthParam.Create, TPenStyleParam.Create,
    TBrushStyleParam.Create);

  TEllipseTool.FParams := TParamList.Create(TPenColorParam.Create,
    TBrushColorParam.Create, TWidthParam.Create, TPenStyleParam.Create,
    TBrushStyleParam.Create);

  TRndRectangleTool.FParams :=
    TParamList.Create(TPenColorParam.Create, TBrushColorParam.Create,
    TWidthParam.Create, TPenStyleParam.Create, TBrushStyleParam.Create,
    TRadiusParam.Create);

end.
