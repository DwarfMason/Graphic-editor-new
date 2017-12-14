unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Menus, GraphMath, Math,
  UDraw, UTools, UScale, UParam;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitBtn: TMenuItem;
    HelpMenu: TMenuItem;
    MenuItem1: TMenuItem;
    DeleteBtn: TMenuItem;
    NewFileBtn: TMenuItem;
    NoneSpace: TMenuItem;
    MoveUpBtn: TMenuItem;
    MoveDownBtn: TMenuItem;
    ParamPanel: TPanel;
    SelectAll: TMenuItem;
    Deselect: TMenuItem;
    ScrollHorizontal: TScrollBar;
    ScrollVertical: TScrollBar;
    ZoomQ: TFloatSpinEdit;
    ToolLabel: TLabel;
    ToolsList: TListBox;
    WorkPlace: TPaintBox;
    ToolBar: TPanel;
    procedure DeleteBtnClick(Sender: TObject);
    procedure DeselectClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure NewFileBtnClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
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
    procedure UpdateParams;
    procedure ZoomQChange(Sender: TObject);
    procedure ZoomQKeyPress(Sender: TObject; var Key: char);

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
  ParamsList: TParamList;

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
  ToolsList.ItemIndex := 0;
  FCurrentFigureIndex := -1;
  workplace.canvas.Brush.color := clWhite;
end;

procedure TMainForm.MoveDownBtnClick(Sender: TObject);
begin
  MoveDown();
  invalidate;
end;

procedure TMainForm.HelpMenuClick(Sender: TObject);
begin
  ShowMessage('Работа выполнена студентом 1 курса, Балашенко Игорем Евгеньевичем');
end;

procedure TMainForm.MoveUpBtnClick(Sender: TObject);
begin
  MoveUp();
  invalidate;
end;

procedure TMainForm.NewFileBtnClick(Sender: TObject);
begin
  PSelectAll();
  DeleteSelected();
  ZoomQ.Value:=100;
  invalidate;
end;

procedure TMainForm.SelectAllClick(Sender: TObject);
begin
  PSelectAll();
  UpdateParams;
  invalidate;
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

procedure TMainForm.ScrollHorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  ScreenOffset.X := ScrollHorizontal.Position;
  Invalidate;
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.DeselectClick(Sender: TObject);
begin
  UnSelectAll();
  invalidate;
end;

procedure TMainForm.DeleteBtnClick(Sender: TObject);
begin
  DeleteSelected();
  invalidate;
  UnSelectAll();
end;

procedure TMainForm.WorkPlaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SetBtn(Button);
  if FCurrentToolClass = nil then
    exit;
  FCurrentFigureIndex := AddFigure(FCurrentToolClass.GetFigureClass());
  FCurrentToolClass.SetFigureParams(FCurrentFigureIndex);
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

    if (FCurrentToolClass.Finish(FCurrentFigureIndex)) then
    begin
      FCurrentFigureIndex := cFigureIndexInvalid;
      ToolBar.Enabled := True;
      UpdateParams;
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
  begin
    GetFigure(i).Draw(WorkPlace.Canvas);
    if GetFigure(i).selected then
      GetFigure(i).SelectionDraw(WorkPlace.Canvas);
  end;
end;

procedure TMainForm.UpdateParams;
var
  i: TParam;
  l: TLabel;
begin
  FCurrentToolClass := GetTool(ToolsList.ItemIndex);
  ParamPanel.DestroyComponents;
  ParamsList := FCurrentToolClass.GetParams;
  if ParamsList = nil then
    ParamsList := GetSelectionParams;
  if ParamsList <> nil then
  begin
    for i in ParamsList do
    begin
      ParamPanel.Visible := False;
      i.ToControl(ParamPanel).Align := alBottom;
      l := TLabel.Create(ParamPanel);
      l.Parent := ParamPanel;
      l.Caption := i.Name;
      l.Align := alBottom;
      ParamPanel.Visible := True;
    end;
  end;
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

procedure TMainForm.ZoomQKeyPress(Sender: TObject; var Key: char);
begin
  key := #0;
end;

procedure TMainForm.ToolsListSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateParams;
  invalidate;
end;



end.
