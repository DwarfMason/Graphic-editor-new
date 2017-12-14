unit UParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ColorBox, ExtCtrls, StdCtrls, ComCtrls;

type

  { TParam }

  TParam = class
  private
    FName: string;
    AttachedParams: array of TParam;
    procedure ChangeControl(Sender: TObject); virtual; abstract;
  public
    procedure AttachParam(AParam: TParam);
    procedure UnAttach();
    property Name: string read FName;
    function ToControl(AParentPanel: TPanel): TControl; virtual; abstract;
  end;

  TParamList = array of TParam;

  { TPenColorParam }

  TPenColorParam = class(TParam)
  private
    FPenColor: TColor;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: TColor read FPenColor;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TPenColorParam;
  end;

  { TBrushColorParam }

  TBrushColorParam = class(TParam)
  private
    FBrushColor: TColor;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: TColor read FBrushColor;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TBrushColorParam;
  end;

  { TWidthParam }

  TWidthParam = class(TParam)
  private
    FWidth: integer;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: integer read FWidth;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TWidthParam;
  end;

  { TRadiusParam }

  TRadiusParam = class(TParam)
  private
    FRadius: integer;
    procedure ChangeControl(Sender: TObject); override;
  public
    property Value: integer read FRadius;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TRadiusParam;
  end;

  { TBrushStyleParam }

  TBrushStyleParam = class(TParam)
  private
    FFillIndex: integer;
  const
    FFillStyles: array[0..7] of TBrushStyle = (bsSolid, bsClear,
      bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

    function FGetBrushStyle: TBrushStyle;
    procedure ChangeControl(Sender: TObject); override;
    procedure FDrawItem(Control: TWinControl; Index: integer; ARect: TRect;
      State: TOwnerDrawState);
  public
    property Value: TBrushStyle read FGetBrushStyle;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TBrushStyleParam;
  end;

  { TPenStyleParam }

  TPenStyleParam = class(TParam)
  private
    FLineIndex: integer;
  const
    FPenStyles: array[0..5] of TPenStyle = (psSolid, psClear,
      psDot, psDash, psDashDot, psDashDotDot);

    function FGetPenStyle: TPenStyle;
    procedure ChangeControl(Sender: TObject); override;
    procedure FDrawItem(Control: TWinControl; Index: integer; ARect: TRect;
      State: TOwnerDrawState);
  public
    property Value: TPenStyle read FGetPenStyle;
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Copy: TPenStyleParam;
  end;

implementation

{ TParam }

procedure TParam.AttachParam(AParam: TParam);
begin
  SetLength(AttachedParams, Length(AttachedParams) + 1);
  AttachedParams[High(AttachedParams)] := AParam;
end;

procedure TParam.UnAttach();
begin
  SetLength(AttachedParams, 0);
end;

{ TPenStyleParam }

function TPenStyleParam.FGetPenStyle: TPenStyle;
begin
  Result := FPenStyles[FLineIndex];
end;

procedure TPenStyleParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FLineIndex := (Sender as TComboBox).ItemIndex;
  if AttachedParams <> nil then
  begin
    for p in AttachedParams do
      (p as TPenStyleParam).FLineIndex := (Sender as TComboBox).ItemIndex;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

procedure TPenStyleParam.FDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := FPenStyles[Index];
    Pen.Width := 1;
    Line(ARect.left + 1, (ARect.Top + ARect.Bottom) div 2, ARect.Right - 1,
      (ARect.Top + ARect.Bottom) div 2);
  end;
end;

constructor TPenStyleParam.Create;
begin
  FName := 'Стиль линии';
end;

function TPenStyleParam.ToControl(AParentPanel: TPanel): TControl;
var
  i: TPenStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnChange := @ChangeControl;
    for i in FPenStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := True;
    ItemIndex := FLineIndex;
  end;
end;

function TPenStyleParam.Copy: TPenStyleParam;
begin
  Result := TPenStyleParam.Create;
  Result.FName := FName;
  Result.FLineIndex := FLineIndex;
end;

{ TBrushStyleParam }

function TBrushStyleParam.FGetBrushStyle: TBrushStyle;
begin
  Result := FFillStyles[FFillIndex];
end;

procedure TBrushStyleParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FFillIndex := (Sender as TComboBox).ItemIndex;
  if Length(AttachedParams) > 0 then
  begin
    for p in AttachedParams do
      (p as TBrushStyleParam).FFillIndex := (Sender as TComboBox).ItemIndex;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

procedure TBrushStyleParam.FDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := psClear;
    Pen.Width := 1;
    Brush.Style := FFillStyles[Index];
    if Index <> 1 then
      Brush.color := clBlack;
    Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Bottom - 1);
  end;
end;

constructor TBrushStyleParam.Create;
begin
  FName := 'Стиль заливки';
  FFillIndex := 1;
end;

function TBrushStyleParam.ToControl(AParentPanel: TPanel): TControl;
var
  i: TBrushStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnChange := @ChangeControl;
    for i in FFillStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := True;
    ItemIndex := FFillIndex;
  end;
end;

function TBrushStyleParam.Copy: TBrushStyleParam;
begin
  Result := TBrushStyleParam.Create;
  Result.FName := FName;
  Result.FFillIndex := FFillIndex;
end;

{ TRadiusParam }

procedure TRadiusParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FRadius := (Sender as TTrackBar).Position;
  if Length(AttachedParams) > 0 then
  begin
    for p in AttachedParams do
      (p as TRadiusParam).FRadius := (Sender as TTrackBar).Position;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

constructor TRadiusParam.Create;
begin
  FName := 'Радиус';
  FRadius := 5;
end;

function TRadiusParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TTrackBar.Create(AParentPanel);
  with Result as TTrackBar do
  begin
    OnChange := @ChangeControl;
    Parent := AParentPanel;
    Min := 5;
    Max := 100;
    PageSize := 1;
    Position := FRadius;
  end;
end;

function TRadiusParam.Copy: TRadiusParam;
begin
  Result := TRadiusParam.Create;
  Result.FName := FName;
  Result.FRadius := FRadius;
end;

{ TWidthParam }

procedure TWidthParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FWidth := (Sender as TTrackBar).Position;
  if Length(AttachedParams) > 0 then
  begin
    for p in AttachedParams do
      (p as TWidthParam).FWidth := (Sender as TTrackBar).Position;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

constructor TWidthParam.Create;
begin
  FName := 'Толщина линии';
  FWidth := 1;
end;

function TWidthParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TTrackBar.Create(AParentPanel);
  with Result as TTrackBar do
  begin
    OnChange := @ChangeControl;
    Parent := AParentPanel;
    Min := 1;
    Max := 20;
    PageSize := 1;
    Position := FWidth;
  end;
end;

function TWidthParam.Copy: TWidthParam;
begin
  Result := TWidthParam.Create;
  Result.FName := FName;
  Result.FWidth := FWidth;
end;

{ TBrushColorParam }

procedure TBrushColorParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FBrushColor := (Sender as TColorBox).Selected;
  if Length(AttachedParams) > 0 then
  begin
    for p in AttachedParams do
      (p as TBrushColorParam).FBrushColor := (Sender as TColorBox).Selected;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

constructor TBrushColorParam.Create;
begin
  FName := 'Цвет кисти';
  FBrushColor := clBlack;
end;

function TBrushColorParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorBox.Create(AParentPanel);
  with Result as TColorBox do
  begin
    Parent := AParentPanel;
    ColorRectWidth := 10;
    Style := [cbCustomColor, cbExtendedColors, cbPrettyNames, cbStandardColors];
    Selected := FBrushColor;
    OnSelect := @ChangeControl;
  end;
end;

function TBrushColorParam.Copy: TBrushColorParam;
begin
  Result := TBrushColorParam.Create;
  Result.FName := FName;
  Result.FBrushColor := FBrushColor;
end;

{ TPenColorParam }

procedure TPenColorParam.ChangeControl(Sender: TObject);
var
  p: TParam;
begin
  FPenColor := (Sender as TColorBox).Selected;
  if Length(AttachedParams) > 0 then
  begin
    for p in AttachedParams do
      (p as TPenColorParam).FPenColor := (Sender as TColorBox).Selected;
    (Sender as TControl).GetTopParent.Invalidate;
  end;
end;

constructor TPenColorParam.Create;
begin
  FName := 'Цвет линии';
  FPenColor := clBlack;
end;

function TPenColorParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorBox.Create(AParentPanel);
  with Result as TColorBox do
  begin
    Parent := AParentPanel;
    ColorRectWidth := 10;
    Style := [cbCustomColor, cbExtendedColors, cbPrettyNames, cbStandardColors];
    Selected := FPenColor;
    OnSelect := @ChangeControl;
  end;
end;

function TPenColorParam.Copy: TPenColorParam;
begin
  Result := TPenColorParam.Create;
  Result.FName := FName;
  Result.FPenColor := FPenColor;
end;

end.
