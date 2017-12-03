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
    procedure ChangeControl(Sender: TObject); virtual; abstract;
  public
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
  end;

implementation

{ TPenStyleParam }

function TPenStyleParam.FGetPenStyle: TPenStyle;
begin
  Result := FPenStyles[FLineIndex];
end;

procedure TPenStyleParam.ChangeControl(Sender: TObject);
begin
  FLineIndex := (Sender as TComboBox).ItemIndex;
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

{ TBrushStyleParam }

function TBrushStyleParam.FGetBrushStyle: TBrushStyle;
begin
  Result := FFillStyles[FFillIndex];
end;

procedure TBrushStyleParam.ChangeControl(Sender: TObject);
begin
  FFillIndex := (Sender as TComboBox).ItemIndex;
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
      Brush.color := clPurple;
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

{ TRadiusParam }

procedure TRadiusParam.ChangeControl(Sender: TObject);
begin
  FRadius := (Sender as TTrackBar).Position;
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

{ TWidthParam }

procedure TWidthParam.ChangeControl(Sender: TObject);
begin
  FWidth := (Sender as TTrackBar).Position;
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

{ TBrushColorParam }

procedure TBrushColorParam.ChangeControl(Sender: TObject);
begin
  FBrushColor := (Sender as TColorBox).Selected;
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

{ TPenColorParam }

procedure TPenColorParam.ChangeControl(Sender: TObject);
begin
  FPenColor := (Sender as TColorBox).Selected;
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

end.
