unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Menus, GraphMath,
  UDraw, UTools, UScale;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitBtn: TMenuItem;
    HelpMenu: TMenuItem;
    ZoomQ: TFloatSpinEdit;
    ToolLabel: TLabel;
    ToolsList: TListBox;
    WorkPlace: TPaintBox;
    ToolBar: TPanel;
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolsListSelectionChange(Sender: TObject; User: boolean);
    procedure WorkPlaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure WorkPlaceMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure WorkPlaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure WorkPlacePaint(Sender: TObject);
    procedure ZoomQChange(Sender: TObject);

  strict private
    { private declarations }
    FCurrentFigureIndex: SizeInt;
    FCurrentToolClass: TTools;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  isDrawing: boolean;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ToolsCount() - 1 do
    ToolsList.Items.add(GetTool(i).GetName());
  FCurrentFigureIndex := -1;
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close();
end;


procedure TMainForm.WorkPlaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SetBtn(Button);
  if FCurrentToolClass = nil then
    exit;
  FCurrentFigureIndex := AddFigure(FCurrentToolClass.GetFigureClass());
  ToolBar.Enabled := False;
  FCurrentToolClass.Start(FCurrentFigureIndex, Point(X, Y));
  WorkPlace.invalidate();
end;

procedure TMainForm.WorkPlaceMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FCurrentToolClass = nil then
    exit;
  if FCurrentToolClass.Update(FCurrentFigureIndex, Point(X, Y)) then
    WorkPlace.invalidate();
end;

procedure TMainForm.WorkPlaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FCurrentToolClass = nil then
    exit;
  if (not FCurrentToolClass.Step(FCurrentFigureIndex, Point(X, Y))) or
    (Button = mbRight) then
  begin
    if FCurrentToolClass.Finish(FCurrentFigureIndex) then
    begin
      FCurrentFigureIndex := cFigureIndexInvalid;
      ToolBar.Enabled := True;
      WorkPlace.invalidate();
    end;
  end;
end;

procedure TMainForm.WorkPlacePaint(Sender: TObject);
var
  i: SizeInt;
begin
  ZoomQ.Value := double(Zoom * 100);
  Workplace.canvas.Clear();
  for i := 0 to FiguresCount() - 1 do
    GetFigure(i).Draw(WorkPlace.Canvas);
end;

procedure TMainForm.ZoomQChange(Sender: TObject);
var
  Middle: TFloatPoint;
begin
  Middle := ScreenToWorld(WorkPlace.Width div 2, WorkPlace.Height div 2);
  ZoomPoint(Middle, ZoomQ.Value / 100);
  invalidate();
end;


procedure TMainForm.ToolsListSelectionChange(Sender: TObject; User: boolean);
begin
  FCurrentToolClass := GetTool(ToolsList.ItemIndex);
end;

end.
