unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GraphMath;

function ScreenToWorld(x, y: integer): TFloatPoint;
function WorldToScreen(x, y: extended): TPoint;
procedure SetZoom(aZoom: double);
procedure ZoomPoint(aPoint: TFloatPoint; aZoom: double);

var
  ScreenOffset: TFloatPoint;
  Zoom: extended;

implementation

function ScreenToWorld(x, y: integer): TFloatPoint;
begin
  Result.x := x / Zoom + ScreenOffset.x;
  Result.y := y / Zoom + ScreenOffset.y;
end;

function WorldToScreen(x, y: extended): TPoint;
begin
  Result.x := round((x - ScreenOffset.x) * Zoom);
  Result.y := round((y - ScreenOffset.y) * Zoom);
end;

procedure ZoomPoint(aPoint: TFloatPoint; aZoom: double);
var
  PrevZoom: double;
  ScreenPos: Tpoint;
begin
  ScreenPos := WorldToScreen(aPoint.x, aPoint.y);
  PrevZoom := Zoom;
  SetZoom(aZoom);
  if Zoom = PrevZoom then
    exit;
  ScreenOffset.x := aPoint.x - (ScreenPos.x / Zoom);
  ScreenOffset.y := aPoint.y - (ScreenPos.y / Zoom);
end;

procedure SetZoom(aZoom: double);
begin
  if aZoom <= 0.001 then
    Zoom := 0.001
  else if aZoom >= 1000 then
    Zoom := 1000
  else
    Zoom := aZoom;
end;

initialization

  Zoom := 1;
  ScreenOffset := FloatPoint(0, 0);

end.
