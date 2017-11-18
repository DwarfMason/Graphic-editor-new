unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin,
  UDraw, UTools;

type

  { TForm1 }

  TForm1 = class(TForm)
    ZoomQ: TFloatSpinEdit;
    ToolLabel: TLabel;
    ToolsList: TListBox;
    WorkPlace: TPaintBox;
    ToolBar: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ToolsListSelectionChange(Sender: TObject; User: boolean);
    procedure WorkPlaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WorkPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WorkPlaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WorkPlacePaint(Sender: TObject);

  strict private
    { private declarations }
  FCurrentFigureIndex: SizeInt;
  FCurrentToolClass: TTools;
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  isDrawing:boolean;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
begin
  For i:= 0 to ToolsCount()-1 do
   ToolsList.Items.add(GetTool(i).GetName());
  FCurrentFigureIndex:=-1;
end;


procedure TForm1.WorkPlaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If FCurrentToolClass = nil then exit;
  FCurrentFigureIndex:= AddFigure(FCurrentToolClass.GetFigureClass());
  ToolBar.enabled:=false;
  FCurrentToolClass.Start(FCurrentFigureIndex, Point(X,Y));
  WorkPlace.invalidate();
end;

procedure TForm1.WorkPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  If FCurrentToolClass = nil then exit;
  If FCurrentToolClass.Update(FCurrentFigureIndex, Point(X,Y)) then
  WorkPlace.invalidate();
end;

procedure TForm1.WorkPlaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If FCurrentToolClass = nil then exit;
 If (not FCurrentToolClass.Step(FCurrentFigureIndex, Point(X,Y))) or
 (Button = mbRight) then
 begin
   If FCurrentToolClass.Finish(FCurrentFigureIndex) then begin
     FCurrentFigureIndex := cFigureIndexInvalid;
     ToolBar.enabled:=true;
     WorkPlace.invalidate();
   end;
 end;
end;

procedure TForm1.WorkPlacePaint(Sender: TObject);
var
i: SizeInt;
begin
Workplace.canvas.clear();
For i:=0 to FiguresCount()-1 do
GetFigure(i).Draw(WorkPlace.Canvas);
end;


procedure TForm1.ToolsListSelectionChange(Sender: TObject; User: boolean);
begin
 FCurrentToolClass := GetTool(ToolsList.ItemIndex);
end;

end.

