{Sample of Config Frame}
unit frameGeneral;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, Graphics,
  ConfigFrame;  //need to be used in a Config Frame

type

  TMyEnum = (First, Second, Third);

  { TfraConfig }

  TfraConfig = class(TFrame)
    ColorButton1: TColorButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RadButton1: TRadioButton;
    RadButton2: TRadioButton;
    RadButton3: TRadioButton;
    procedure ConfigEditor;
  private
    f: Tform;
  public
    //vars to manage
    MyText : string;
    MyNumber : integer;
    MyCol: TColor;
    MyEnum: TMyEnum;
    procedure Initiate(secINI0: string; form: TForm);
  end;

implementation
{$R *.lfm}

procedure TfraConfig.Initiate(secINI0: string; form: TForm);
begin
  secINI := secINI0;  //section INI
  f := form;  //needed reference
  OnUpdateChanges:=@ConfigEditor;  //changes handle
  //asociate vars to controls
  Asoc_Str_TEdit(@MyText, Edit1, 'MyText', '');
  Asoc_Int_TEdit(@MyNumber, Edit2, 'MyNumber', 5, 1,7);
  Asoc_Col_TColBut(@MyCol, ColorButton1, 'colFon',clWhite);
  Asoc_Enum_TRadBut(@MyEnum, SizeOf(TMyEnum),
                    [RadButton1, RadButton2, RadButton3],'MyEnum',0);
end;

procedure TfraConfig.ConfigEditor;
//Update changes of the vars
begin
  f.Color := MyCol;
end;

end.

