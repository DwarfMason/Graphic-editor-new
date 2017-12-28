unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDraw, Math;

type
  THistoryItem = record
    Figures: TFigureArray;
    Saved: boolean;
  end;

  { THistory }

  THistory = class
  private
    FItems: array of THistoryItem;
    FIndex: SizeInt;
  public
    constructor Create;
    procedure Push(AState: THistoryItem);
    procedure Push;
    procedure PushToClipboard;
    procedure GetFromClipBoard;
    procedure Undo;
    procedure Redo;
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Clear;
    function GetCurrState: THistoryItem;
  end;

function HistoryItem(AFigures: TFigureArray): THistoryItem;

var
  History: THistory;
  ClipBoardFigures: TFigureArray;

implementation

function HistoryItem(AFigures: TFigureArray): THistoryItem;
begin
  Result.Figures := AFigures;
end;

{ THistory }

constructor THistory.Create;
begin
  FIndex := -1;
end;

procedure THistory.Push(AState: THistoryItem);
var
  i: SizeInt;
begin
  Inc(FIndex);
  SetLength(FItems, FIndex + 1);
  FItems[FIndex] := AState;
end;

procedure THistory.Push;
begin
  Push(HistoryItem(CloneFigures(FiguresData)));
end;

procedure THistory.PushToClipboard;
var
  i, j: SizeInt;
  AClipBoardFigures: TFigureArray;
begin
  j := 0;
  for i := 0 to high(FiguresData) do
  begin
    if GetFigure(i).selected then
    begin
      SetLength(AClipBoardFigures, Length(AClipBoardFigures) + 1);
      AClipboardFigures[j] := GetFigure(i).Clone;
      Inc(j);
    end;
  end;
  ClipBoardFigures:= AClipBoardFigures;
end;

procedure THistory.GetFromClipBoard;
var
  i, j: SizeInt;
  len: SizeInt;
begin
  Len := Length(FiguresData);
  j := 0;
  SetLength(FiguresData, Length(FiguresData) + Length(ClipBoardFigures));
  for i := Len to High(FiguresData) do
  begin
    FiguresData[i] := ClipBoardFigures[j].Clone;
    GetFigure(i).selected := True;
    Inc(j);
  end;
end;

procedure THistory.Undo;
begin
  PSelectAll();
  DeleteSelected();
  FIndex := max(0, FIndex - 1);
  FiguresData := CloneFigures(GetCurrState.Figures);
end;

procedure THistory.Redo;
begin
  PSelectAll();
  DeleteSelected();
  FIndex := min(FIndex + 1, High(FItems));
  FiguresData := CloneFigures(GetCurrState.Figures);
end;

function THistory.CanUndo: boolean;
begin
  Result := FIndex > 0;
end;

function THistory.CanRedo: boolean;
begin
  Result := (FIndex >= 0) and (FIndex < High(FItems));
end;

procedure THistory.Clear;
begin
  SetLength(FItems, 0);
  FIndex := -1;
  Push;
end;

function THistory.GetCurrState: THistoryItem;
begin
  Result := FItems[FIndex];
end;

begin
  History := THistory.Create;
end.

