ConfigFrame 0.3
===============

ConfigFrame is a Lazarus unit that can be used for to create quick configuration dialogs.

This unit for Lazarus, contain a new TFrame class for replace to the common Lazarus TFrame definition.

This new TFrame includes predefined methods that facilitate the manipulation of variables (properties) of the application, so edit them in a config dialog, and save the changes to disk, are nearly transparent.

This library simplifies the creation of configuration dialogs.

It is assumed that we work with an INI file, where the work variables are saved.

With the "ConfigFrame" unit, you can create as simple configuration Frames
like this:
```
unit frameGeneral;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ConfigFrame;

type

  TfraConfig = class(TFrame)
    Edit1: TEdit;
    Edit2: TEdit;
  public
    //vars to manage
    MyText : string;
    MyNumber : integer;
    procedure Initiate(secINI0: string);
  end;

implementation
{$R *.lfm}

procedure TfraConfig.Initiate(secINI0: string);
begin
  secINI := secINI0;  //section INI
  //asociate vars to controls
  Asoc_Str_TEdit(@MyText, Edit1, 'MyText', '');
  Asoc_Int_TEdit(@MyNumber, Edit2, 'MyNumber', 5, 1,7);
end;

end.
```

And, even with this simple code, the frame allow you to edit the value of two variables
(MyText and MyNumber) on a frame, and save your changes to disk or read from there.

There are several methods for associate variables to controls:
```
procedure Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
						 defVal: integer; minVal, maxVal: integer);
procedure Asoc_Int_TSpnEdi(ptrInt: pointer; spEdit: TSpinEdit; etiq: string;
						 defVal, minVal, maxVal: integer);
procedure Asoc_Str_TEdit(ptrStr: pointer; edit: TCustomEdit; etiq: string;
						 defVal: string);
procedure Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
						 defVal: string);
procedure Asoc_StrList_TListBox(ptrStrList: pointer; lstBox: TlistBox; etiq: string);
procedure Asoc_Bol_TChkB(ptrBol: pointer; chk: TCheckBox; etiq: string;
						 defVal: boolean);
procedure Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
						 defVal: TColor);
procedure Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
				radButs: array of TRadioButton; etiq: string; defVal: integer);
procedure Asoc_Bol_TRadBut(ptrBol: pointer;
				radButs: array of TRadioButton; etiq: string; defVal: boolean);
```
Also, if we just want to save variables into the INI files, without showing, there are some other methods for that:

```
procedure Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
procedure Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
procedure Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
procedure Asoc_StrList(ptrStrList: pointer; etiq: string);
```

Two sample projects are included, one with one frame on a config form, and other with several frames on a config form.
