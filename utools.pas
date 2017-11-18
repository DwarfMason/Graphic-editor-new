unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDraw;

type

  { TBigTools }

  TBigTools = class
  public
    class function GetName(): string; virtual; abstract;
    class function GetFigureClass(): TCanvasFigure; virtual; abstract;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); virtual;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; virtual;
    class function Step(AFigureIndex: SizeInt; AXY: Tpoint): boolean; virtual; abstract;
    class function Finish(AFigureIndex: SizeInt): boolean; virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TLineTool }

  TLineTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TPolyLineTool }

  TPolyLineTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  { TRndRectangleTool }

  TRndRectangleTool = class(TBigTools)
  public
    class function GetName(): string; override;
    class function GetFigureClass(): TCanvasFigure; override;
    class function Update(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
    class function Step(AFigureIndex: SizeInt; AXY: TPoint): boolean; override;
  end;

  TTools = class of TBigTools;

function GetTool(AIndex: SizeInt): TTools;
function ToolsCount(): SizeInt;


implementation

type
  TToolsArray = array of TTools;

var
  ToolsClasses: TToolsArray;

  { TBigTools }

class procedure TBigTools.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
  with GetFigure(AFigureIndex) do begin
    AddPoint(AXY);
    AddPoint(AXY);
  end;
end;

class function TBigTools.Update(AFigureIndex: SizeInt;
  AXY: TPoint): boolean;
begin
  Result := AFigureIndex <> cFigureIndexInvalid;
end;

class function TBigTools.Finish(AFigureIndex: SizeInt): boolean;
begin
  Result := AFigureIndex <> cFigureIndexInvalid;
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

class function TRndRectangleTool.GetName: string;
begin
  Result:= 'Круглый не круг'
end;

class function TRndRectangleTool.GetFigureClass: TCanvasFigure;
begin
  Result:= TRndRectangle;
end;

class function TRndRectangleTool.Update(AFigureIndex: SizeInt; AXY: TPoint
  ): boolean;
begin
    Result:=inherited; If not Result then Exit;
  GetFigure(AFigureIndex).SetPoint(1,AXY);

end;

class function TRndRectangleTool.Step(AFigureIndex: SizeInt; AXY: TPoint
  ): boolean;
begin
  Result:=False;
end;

{ TEllipseTool }

class function TEllipseTool.GetName: string;
begin
  Result:= 'Эллипс';
end;

class function TEllipseTool.GetFigureClass: TCanvasFigure;
begin
  Result:= TEllipse;
end;

class function TEllipseTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
    Result:=inherited; If not Result then Exit;
  GetFigure(AFigureIndex).SetPoint(1,AXY);

end;

class function TEllipseTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=False;
end;

{ TRectangleTool }

class function TRectangleTool.GetName: string;
begin
  Result:= 'Прямоугольник';
end;

class function TRectangleTool.GetFigureClass: TCanvasFigure;
begin
   Result:= TRectangle;
end;

class function TRectangleTool.Update(AFigureIndex: SizeInt; AXY: TPoint
  ): boolean;
begin
  Result:=inherited; If not Result then Exit;
  GetFigure(AFigureIndex).SetPoint(1,AXY);

end;

class function TRectangleTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=False;
end;

{ TPolyLineTool }

class function TPolyLineTool.GetName: string;
begin
  Result:='Ломаная';
end;

class function TPolyLineTool.GetFigureClass: TCanvasFigure;
begin
  Result:= TPolyLine;
end;

class function TPolyLineTool.Update(AFigureIndex: SizeInt; AXY: TPoint
  ): boolean;
var
  Figure: TBigFigureClass;
begin
  Result:=inherited; If not Result then Exit;
  Figure:= GetFigure(AFigureIndex);
  Figure.SetPoint(Figure.PointsCount()-1, AXY);
end;

class function TPolyLineTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result := AFigureIndex <> cFigureIndexInvalid;
  If not Result then Exit;
  GetFigure(AFigureIndex).AddPoint(AXY);
end;

{ TLineTool }

class function TLineTool.GetName: string;
begin
  Result:='Линия';
end;

class function TLineTool.GetFigureClass: TCanvasFigure;
begin
  Result:= TLine;
end;

class function TLineTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=inherited; If not Result then Exit;
  GetFigure(AFigureIndex).SetPoint(1,AXY);
end;

class function TLineTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=false;
end;

{ TPencilTool }

class function TPencilTool.GetName: string;
begin
  Result:= 'Карандаш';
end;

class function TPencilTool.GetFigureClass: TCanvasFigure;
begin
  Result:= TPencil;
end;

class function TPencilTool.Update(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=inherited; If not Result then Exit;
  GetFigure(AFigureIndex).AddPoint(AXY);
end;

class function TPencilTool.Step(AFigureIndex: SizeInt; AXY: TPoint): boolean;
begin
  Result:=false;
end;


initialization

 ToolsClasses := TToolsArray.create(
  TRndRectangleTool,
  TEllipseTool,
  TRectangleTool,
  TPolyLineTool,
  TLineTool,
  TPencilTool
  );

end.
