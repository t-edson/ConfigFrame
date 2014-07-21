ConfigFrame
=============

This unit for Lazarus, contain a new TFrame classs for replace to the Lazarus TFrame definition.

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

Two sample projects are included, one with one frame on a config form, and other with several frames on a config form.

