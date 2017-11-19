unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Menus, GraphMath, Math,
  UDraw, UTools, UScale;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitBtn: TMenuItem;
    HelpMenu: TMenuItem;
    ScrollHorizontal: TScrollBar;
    ScrollVertical: TScrollBar;
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
    procedure ChangeBorders();
    procedure SetScrollBar();
    procedure ScrollHorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure ScrollVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
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
  WorldTopLeft, WorldBottomRight: TFloatPoint;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetScrollBar();
  for i := 0 to ToolsCount() - 1 do
    ToolsList.Items.add(GetTool(i).GetName());
  FCurrentFigureIndex := -1;
end;

procedure TMainForm.ChangeBorders;
var
  i: SizeInt;
begin
  if FiguresCount() = 0 then
  begin
    WorldTopLeft.x := 0;
    WorldTopLeft.y := 0;
    WorldBottomRight.x := 0;
    WorldBottomRight.y := 0;
    exit;
  end;
  WorldTopLeft.x := GetFigure(0).TopLeft().x;
  WorldBottomRight.x := GetFigure(0).BottomRight().x;
  WorldTopLeft.y := GetFigure(0).TopLeft().y;
  WorldBottomRight.y := GetFigure(0).BottomRight().y;
  for i := 1 to FiguresCount() - 1 do

  begin
    WorldTopLeft.x := min(WorldTopLeft.x, GetFigure(i).TopLeft().x);
    WorldTopLeft.y := min(WorldTopLeft.y, GetFigure(i).TopLeft().y);
    WorldBottomRight.x := max(WorldBottomRight.x, GetFigure(i).BottomRight().x);
    WorldBottomRight.y := max(WorldBottomRight.y, GetFigure(i).BottomRight().y);
  end;
end;

procedure TMainForm.SetScrollBar;
var
  ScreenRightEnd: TFloatPoint;
  HorizMin, HorizMax, VertMin, VertMax: integer;
begin
  ScreenRightEnd := ScreenToWorld(WorkPlace.Width, WorkPlace.Height);
  HorizMin := Round(Min(ScreenOffset.x, 0));
  HorizMax := Round(Max(ScreenRightEnd.x, WorkPlace.Width));
  VertMin := Round(Min(ScreenOffset.y, 0));
  VertMax := Round(Max(ScreenRightEnd.y, WorkPlace.Height));

  if FiguresCount() > 0 then
  begin
    ChangeBorders();
    HorizMin := Min(HorizMin, Round(WorldTopLeft.x));
    VertMin := Min(VertMin, Round(WorldTopLeft.y));
    HorizMax := Max(HorizMax, Round(WorldBottomRight.x));
    VertMax := Max(VertMax, Round(WorldBottomRight.y));
  end;

  ScrollHorizontal.SetParams(Round(ScreenOffset.x), HorizMin, HorizMax,
    Round(WorkPlace.Width / Zoom));
  ScrollVertical.SetParams(Round(ScreenOffset.y), VertMin, VertMax,
    Round(WorkPlace.Height / Zoom));
end;

procedure TMainForm.ScrollVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  ScreenOffset.Y := ScrollVertical.Position;
  WorkPlace.Invalidate();
end;

procedure TMainForm.ScrollHorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  ScreenOffset.X := ScrollHorizontal.Position;
  Invalidate;
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
  SetScrollBar();
  WorkPlace.invalidate();
end;

procedure TMainForm.WorkPlaceMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FCurrentToolClass = nil then
    exit;
  if FCurrentToolClass.Update(FCurrentFigureIndex, Point(X, Y)) then
    WorkPlace.invalidate();
    SetScrollBar();
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
  SetScrollBar();
  invalidate();
end;


procedure TMainForm.ToolsListSelectionChange(Sender: TObject; User: boolean);
begin
  FCurrentToolClass := GetTool(ToolsList.ItemIndex);
end;

end.