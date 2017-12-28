unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Menus, GraphMath, Math,
  UDraw, UTools, UScale, UParam, fpjson, superobject, typinfo, UHistory;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitBtn: TMenuItem;
    HelpMenu: TMenuItem;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    DeleteBtn: TMenuItem;
    LoadBtn: TMenuItem;
    CopyBtn: TMenuItem;
    CutBtn: TMenuItem;
    PasteBtn: TMenuItem;
    NoneSpace2: TMenuItem;
    UndoBtn: TMenuItem;
    RedoBtn: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveBtn: TMenuItem;
    NewFileBtn: TMenuItem;
    NoneSpace: TMenuItem;
    MoveUpBtn: TMenuItem;
    MoveDownBtn: TMenuItem;
    ParamPanel: TPanel;
    SaveDialog: TSaveDialog;
    SelectAll: TMenuItem;
    Deselect: TMenuItem;
    ScrollHorizontal: TScrollBar;
    ScrollVertical: TScrollBar;
    ZoomQ: TFloatSpinEdit;
    ToolLabel: TLabel;
    ToolsList: TListBox;
    WorkPlace: TPaintBox;
    ToolBar: TPanel;
    procedure CopyBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DeselectClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure NewFileBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure ToolsListSelectionChange(Sender: TObject; User: boolean);
    procedure UndoBtnClick(Sender: TObject);
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
    procedure ParamsUpdate;
    procedure WorkPlacePaint(Sender: TObject);
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
  CurrentFileName: string;

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
  workplace.canvas.Brush.color := clWhite;
  History.Clear;
end;

procedure TMainForm.LoadBtnClick(Sender: TObject);
var
  f: TFileStream;
  i, k, PropCount: SizeInt;
  FName, temp: string;
  J: TJSONData;
  FigureProps, PropProps: PPropList;
  FFigureArray: array of TBigFigureClass;
  jArray, jPoints: TJSONArray;
  PropInfo: TPropInfo;
  z: TJSONEnum;
  FigureParam: TParam;
begin
  if OpenDialog.Execute then
  begin
    NewFileBtnClick(Sender);
    FName := OpenDialog.FileName;
    f := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
    J := GetJSON(f);
    jArray := TJSONArray(J.FindPath('Figure'));
    SetLength(FFigureArray, jArray.Count);
    for i := 0 to jArray.Count - 1 do
    begin
      temp := jArray.Objects[i].FindPath('FigureClass').AsString;
      FFigureArray[i] :=
        GetFigure(AddFigure(StrToClassFigure(temp)));
      jPoints := TJSONArray(jArray.Objects[i].FindPath('Points'));
      for k := 0 to jPoints.Count - 1 do
      begin
        FFigureArray[i].AddPoint(FloatPoint(jPoints.Arrays[k].Floats[0],
          jPoints.Arrays[k].Floats[1]));
      end;
      for z in jArray.Objects[i] do
      begin
        if (z.Key = 'FigureClass') or (z.Key = 'Points') then
          Continue;
        GetProps(FFigureArray[i], FigureProps);
        FigureParam := FindClass(GetPropInfo(FFigureArray[i],
          z.Key)^.PropType^.Name).Create as TParam;
        case z.Value.JSONType of
          jtNumber:
            SetInt64Prop(FigureParam, 'Value', z.Value.AsInteger);
          jtString:
            SetEnumProp(FigureParam, 'Value', z.Value.AsString);
        end;
        SetObjectProp(FFigureArray[i], z.Key, FigureParam);
      end;
      {PropCount := GetProps(FFigureArray[i], FigureProps);
      For k:= 0 to PropCount -1 do
      begin
        PropInfo:=FigureProps^[k]^;
        FigureParam:=FindClass(PropInfo.PropType^.Name).Create as TParam;
        SetObjectProp(FFigureArray[i], PropInfo.Name, FigureParam);
        GetProps(FigureParam, PropProps);
        case PropProps^[0]^.PropType^.Kind of
          tkInt64, tkInteger:
            SetInt64Prop(FigureParam, PropProps^[0]^.Name,
              jArray.Objects[i].FindPath(PropInfo.Name).AsInt64);
          tkEnumeration:
           SetEnumProp(FigureParam, PropProps^[0]^.Name,
              jArray.Objects[i].FindPath(PropInfo.Name).AsString);
        end;
      end;}
    end;
    FreeAndNil(f);
  end;
  CurrentFileName := FName;
  MainForm.Caption := (CurrentFileName);
  History.Clear;
  History.Push;
  Invalidate();
end;

procedure TMainForm.MoveDownBtnClick(Sender: TObject);
begin
  MoveDown();
  History.Push;
  invalidate;
end;

procedure TMainForm.HelpMenuClick(Sender: TObject);
begin
  ShowMessage('Работа выполнена студентом 1 курса, Балашенко Игорем Евгеньевичем');
end;

procedure TMainForm.MoveUpBtnClick(Sender: TObject);
begin
  MoveUp();
  History.Push;
  invalidate;
end;

procedure TMainForm.NewFileBtnClick(Sender: TObject);
begin
  PSelectAll();
  DeleteSelected();
  ZoomQ.Value := 100;
  MainForm.Caption := ('Новый');
  History.Clear;
  invalidate();
end;

procedure TMainForm.PasteBtnClick(Sender: TObject);
begin
  UnSelectAll;
  History.GetFromClipBoard;
   History.Push;
  invalidate;
end;

procedure TMainForm.RedoBtnClick(Sender: TObject);
begin
  History.Redo;
  RedoBtn.Enabled := History.CanRedo;
  Invalidate;
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
var
  i, j, len: integer;
  Points: TPointArray;
  jObject, jFigure: TJSONObject;
  jArray, jSecArray: TJSONArray;
  FName: string;
  FigureProps, PropProps: PPropList;
  PropCount: SizeInt;
  CurrentProp: TObject;
  PropInfo, PropPropInfo: TPropInfo;
begin
  SaveDialog.FileName := 'Test1.json';
  if SaveDialog.Execute then
  begin
    jObject := TJSONObject.Create;
    jObject.Add('Figure', TJSONArray.Create);
    for i := 0 to FiguresCount() - 1 do
    begin
      jFigure := TJSONObject.Create;
      jFigure.Add('FigureClass', GetFigure(i).ClassName);
      Points := GetFigure(i).GetCanvasPoints();
      len := Length(Points) - 1;
      jArray := TJSONArray.Create;
      for j := 0 to len do
      begin
        jSecArray := TJSONArray.Create;
        jSecArray.Add(Points[j].x);
        jSecArray.Add(Points[j].y);
        jArray.Add(jSecArray);
      end;
      jFigure.Add('Points', jArray);
      PropCount := GetProps(GetFigure(i), FigureProps);
      for J := 0 to PropCount - 1 do
      begin
        PropInfo := FigureProps^[j]^;
        CurrentProp := GetObjectProp(GetFigure(i), PropInfo.Name);
        if CurrentProp = nil then
          Continue;
        GetProps(CurrentProp, PropProps);
        PropPropInfo := PropProps^[0]^;
        case ProppropInfo.PropType^.Kind of
          tkInt64, tkInteger:
            jFigure.Add(PropInfo.Name, GetInt64Prop(CurrentProp, PropPropInfo.Name));
          tkEnumeration:
            jFigure.Add(PropInfo.Name, GetEnumProp(CurrentProp, PropPropInfo.Name));
        end;
      end;
      jObject.Arrays['Figure'].Add(jFigure);
    end;
    FName := SaveDialog.FileName;
    Memo1.Lines.Add(jObject.FormatJSON);
    Memo1.Lines.SaveToFile(FName);
    CurrentFileName := FName;
    MainForm.Caption := (CurrentFileName);
    History.Clear;
    History.Push;
  end;
end;

procedure TMainForm.SelectAllClick(Sender: TObject);
begin
  PSelectAll();
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
  UnSelectAll;
  History.Push;
end;

procedure TMainForm.CopyBtnClick(Sender: TObject);
begin
  History.PushToClipboard;
end;

procedure TMainForm.CutBtnClick(Sender: TObject);
begin
  History.PushToClipboard;
  DeleteSelected;
  History.Push;
  invalidate;
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
  if (FCurrentToolClass.Finish(FCurrentFigureIndex)) then
  begin
    FCurrentFigureIndex := cFigureIndexInvalid;
    ToolBar.Enabled := True;
    ParamsUpdate;
    WorkPlace.invalidate();
  end;
end;

procedure TMainForm.ParamsUpdate;
var
  CurrParam: TParam;
  i: integer;
  l: TLabel;
  c: TControl;
begin
  ParamPanel.DestroyComponents;
  ParamsList := FCurrentToolClass.GetParams;
  if ParamsList = nil then
    ParamsList := GetSelectionParams;
  if ParamsList <> nil then
  begin
    for i := High(ParamsList) downto Low(ParamsList) do
    begin
      CurrParam := ParamsList[i];
      ParamPanel.Visible := False;
      c := CurrParam.ToControl(ParamPanel);
      c.Align := alBottom;
      l := TLabel.Create(ParamPanel);
      l.Parent := ParamPanel;
      l.Caption := CurrParam.Name;
      l.Align := alBottom;
      ParamPanel.Visible := True;
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
  RedoBtn.Enabled := History.CanRedo;
  UndoBtn.Enabled := History.CanUndo;
  If History.CanUndo then MainForm.Caption := CurrentFileName + ' (*)'
  else MainForm.Caption := CurrentFileName;
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
  FCurrentToolClass := GetTool(ToolsList.ItemIndex);
  ParamsUpdate;
  if (FCurrentToolClass <> TZoomTool) and (FCurrentToolClass <> TSelectionTool) and
    (FCurrentToolClass <> TClickTool) and (FCurrentToolClass <> THandTool) then
    UnSelectAll();
  invalidate;
end;

procedure TMainForm.UndoBtnClick(Sender: TObject);
begin
  History.Undo;
  Invalidate;
end;

begin
  CurrentFileName:='Новый';

end.
