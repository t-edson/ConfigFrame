ConfigFrame 0.6
===============

ConfigFrame is a Lazarus unit that can be used to create quick configuration dialogs.

This unit for Lazarus, contain a Frame class for derives Configuration Frames.

The Configruation Frames are used to create configuration dialogs, according to the following structure:

```
                             +-------------------+
                             |                   | 
                         +---|   Configuration   | 
                         |   |       Frame       | 
                         |   +-------------------+
+-------------------+    |
|                   |----+   +-------------------+
|    Configuration  |        |                   | 
|      Dialog       |--------|   Configuration   |  
|                   |        |       Frame       |
|                   |----+   +-------------------+
+-------------------+    |
                         |   +-------------------+
                         |   |                   | 
                         +---|   Configuration   | 
                             |       Frame       | 
                             +-------------------+
```

This new Frame includes predefined methods that facilitate the manipulation of variables (properties) of the application, so edit them in a config dialog, and save the changes to disk, are nearly transparent.

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
  TfraConfig = class(TCfgFrame)
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

## How to use?

To use ConfigFrame, it's advisable to follow the suggested design model. It consists on first create a Configuration Form and one or more Configuration Frames, that be included in the Configuration Form.

Maybe, the simpler way to add a configuration form (with his configuration frames) to our project, is to use some of the samples, like Sample2, and copy the files  FormConfig.lfm, FormConfig.pas and all the files like FrameCfg\*.* to the folder of our project. Then, simply include the code to start and save the configuration, in our main program:

```
procedure TForm1.FormShow(Sender: TObject);
begin
  Config.Start(self);   //necesario para poder trabajar
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.WriteIniFile;  //guarda la configuración actual
end;
```

And, in some part of the program, the code to show the Configuration form:

```
  Config.Show;
```

Then, from that point, the Configuration frames can be modified in order to fit our needs.

It's important to ensure the form "FormConfig" is loaded when the program starts (or a runtime error will be raised).

## Flow of information

ConfigFrame works on the fact that information of properties (variables) moves according to the following flow:

```
 +-----------+                  +-------------+                 +------------+
 |           | ReadFileToProp() |             |  PropToWindow() |            |
 |           | ---------------> |             | --------------> |            |
 |   Disk    |                  | Properties  |                 |    Form    |
 |           | SavePropToFile() |             |  WindowToProp() |            |
 |           | <--------------- |             | <-------------- |            |
 +-----------+                  +-------------+                 +------------+
```

Above the arrows, is shown the name of the method of ConfigFrame that make the movement of the information.

The movement of the variables from disk, is usually done once, when the program start(calling to the instruction ReadFileToProp_AllFrames(), who call ReadFileToProp() of all the configuration frames). And the movement from data to disk is usually done when the program ends (calling to SavePropToFile_AllFrames()), but it can be done every time a propery is changed, to ensure the changes are updated in disk.

Visually what is shown to edit the properties, is the configuration form (actually the properties are edited in a configuration frame), and when the changes are accepted, the properties are updated.

Properties that are associated without a visual control (using Asoc_Bol, Asoc_Int, ...), have the following flow:
                                                          
```
+-----------+                  +-------------+
|           | ReadFileToProp() |             |
|           | ---------------> |             |
|   Disk    |                  | Properties  |
|           | SavePropToFile() |             |
|           | <--------------- |             |
+-----------+                  +-------------+
```

In this case, the methods PropToWindow() and WindowToProp(), have no effect on the associated properties or variables, because they have no controls associated.

## Methods for association

There are several methods for associate variables to controls:
```
    procedure Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
                             defVal: integer; minVal, maxVal: integer);
    procedure Asoc_Int_TSpinEdit(ptrInt: pointer; spEdit: TSpinEdit; etiq: string;
                             defVal, minVal, maxVal: integer);
    procedure Asoc_Str_TEdit(ptrStr: pointer; edit: TCustomEdit; etiq: string;
                             defVal: string);
    procedure Asoc_Str_TEditButton(ptrStr: pointer; edit: TCustomEditButton; etiq: string;
                             defVal: string);
    procedure Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
                             defVal: string);
    procedure Asoc_StrList_TListBox(ptrStrList: pointer; lstBox: TlistBox; etiq: string);
    procedure Asoc_Bol_TChkBox(ptrBol: pointer; chk: TCheckBox; etiq: string;
                             defVal: boolean);
    procedure Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
                             defVal: TColor);
    procedure Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
                    radButs: array of TRadioButton; etiq: string; defVal: integer);
    procedure Asoc_Enum_TRadGroup(ptrEnum: pointer; EnumSize: integer;
                    radGroup: TRadioGroup; etiq: string; defVal: integer);
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

ConfigFrame, can be see too, like a small Framework, because it defines some rules for the creation of Configuration's dialogs:

* It must be one INI file and one Config Dialog. Although it's possible to create several config files, it is recommended to maintain the relation: 
INI File <-> Configuración Dialog.

* One config dialog can contain one or more frames ('Configuration frame'). When we have more than one, we can use a ListBox control or some other control to select the  working frame.

* The Configuration frames must be created and destroyed dynamically on the Configuration frame.

* Each Configuration frame must contain one set of properties related. For example we can have a frame to save the general properties and other to save the editor properties, and ... 

* The Configuration frames are commonly created using the Lazarus visual editor, using the controls required to manage the variables of the frame.
 
* The Configuration frames must be created  created  inheriting from the component "CfgFrame" so they can behave as Configuration Frames properly.

It's also recommended use standard names for the objects. We can use these rules:

* The units where the  Configuration Frames are defined, must be called frameCfg{XXX}. Where {XXX} is the part of the name that identify the function of the frame. For Example frameCfgColors, frameCfgMainEdit

* The Config Dialog (Where the Configuration Frames are included), must have the name FormConfig, and the Form must have the name 'Config'.
