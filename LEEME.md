ConfigFrame 0.4
===============

ConfigFrame es una unidad de Lazarus, que puede ser usada para crear fácilmente formualrios de configuración.

Es una unidad (librería) desarrollada en Lazarus que contiene la definición de una clase para reemplazar a TFrame.

Esta nueva clase TFrame incluye métodos predefinidos que facilitan la manipulación de variables (propiedades) de la aplicación, de modo que editarlos en un diálogo y guardar los cambios a disco, se hacen de forma casi transparente.

Con esta librería se simplifica considerablemente, la creación de ventanas de configuración.

Se asume que se trabajará con un archivo INI, en donde se guardarán las variables de trabajo.

Con la unidad "ConfigFrame", se puede crear Frames de configuración tan simples
como este:


```
unit frameTexto;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ConfigFrame; 

type
  TfraTexto = class(TFrame)
    Edit1: TEdit;
  public
    //variables de propiedades
    texto : string;
    procedure Iniciar(secINI0: string); //Inicia el frame
  end;

implementation
{$R *.lfm}

procedure TfraTexto.Iniciar(secINI0: string);
begin
  secINI := secINI0;  //sección INI
  //asocia propiedades a controles
  Asoc_Str_TEdit(@texto, Edit1, 'texto', '');
end;

end.
```

Y aún con este código tan simple, el frame permitirá editar el valor de la variable
"texto" con el control "Edit1", y guardar los cambios a disco o leerlos desde allí.

Existen diversos métodos para asociar variables a controles:
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

También, si tan solo queremos almacenar las variables en un archivo INI, existen los métodos apropiados:

```
procedure Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
procedure Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
procedure Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
procedure Asoc_StrList(ptrStrList: pointer; etiq: string);
```

Se incluye un ejemplo sencillo en donde se implementa una ventana de configuración que usa dos Frames, que se han implementado con pocas líneas de código.

ConfigFrame, puede ser visto también, como un sencillo marco de trabajo (framework), porque define alguna reglas para la creación de ventanas de configuración:

* Se usará un solo archivo INI y una sola ventana de configuración. Aunque se podría manejar diversos archivos de configuración, se recomienda mantener la relación:  Archivo INI <-> Ventana de configuración.

* Una ventana de configuración puede manejar uno o más Frames, a los que se les llamará Frame de Configuración. Cuando se manejen más de uno, se puede usar un control de  lista o pestañas para elegir con que Frame(s) trabajar.

* Los frame de configuración se deben crear y destruir dinámicamente en el formulario de configuración.

* Cada Frame de configuración agrupa a un conjunto de propiedades que tengan relación entre sí. Por ejemplo, se puede tener un Frame para las propiedades generales, uno para las propiedades del editor, ... etc.

* Los Frames se crean normalmente con el editor visual de Lazarus, colocando los controles necesarios para manejar a las propiedades que se usan en ese Frame.
 
* La ventana de configuración, así como los Frames de configuración se deben crear incluyendo la unidad "ConfigFrame", para que puedan usar la nueva definición de TFrame. Esta unidad se debe incluir al final de la sección USES para lograr la interceptación de la clase TFrame.

Para los nombres de objetos, ee recomienda las siguientes normas:

* Las unidades donde se definen los frame de configuración deben llamarse frameCfg<XXX>. Donde <XXX> es la parte del nombre que define la función. Por ejemplo frameCfgColores, frameCfgMainEdit

* La unidad donde se define al formulario  de configuración (en donde se incluirán los frames de configuración), se llamará FormConfig, y el formulario se debe llamar Config.

* Los frame de configuración creados en el formulario de configuración, se deben llamar fc<XXX>.

Se pueden usar los programas de ejemplo como una plantilla de trabajo.
