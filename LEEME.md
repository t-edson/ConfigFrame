ConfigFrame
===========

ConfigFrame es una unidad de Lazarus, que puede ser usada para crear fácilmente formularios de configuración.

Es una unidad (librería) desarrollada en Lazarus que contiene un Frame que servirá como base para la creación de Frames de Configuración.

Los frames de configuración se usan para crear una ventana de configuración, de acuerdo a la siguiente estructura:

```
                             +-------------------+
                             |                   | 
                         +---|   Configuration   | 
                         |   |       Frame       | 
                         |   +-------------------+
+-------------------+    |
|                   |----+   +-------------------+
|    Configuration  |        |                   | 
|        Form       |--------|   Configuration   |  
|                   |        |       Frame       |
|                   |----+   +-------------------+
+-------------------+    |
                         |   +-------------------+
                         |   |                   | 
                         +---|   Configuration   | 
                             |       Frame       | 
                             +-------------------+
```

Este nuevo Frame incluye métodos predefinidos que facilitan la manipulación de variables (propiedades) de la aplicación, de modo que editarlos en un diálogo y guardar los cambios a disco, se hacen de forma casi transparente.

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
  TfraTexto = class(TCfgFrame)
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

## Modo de uso

Para usar ConfigFrame, se recomienda seguir el modelo de diseño sugerido. Que consiste en crear primero un formulario de configuración y luego uno o más Frames de configuración, que serán incluidos en el formulario de configuración.

Tal vez el método más sencillo de crear un formulario de configuración con sus respectivos frames, es usar uno de los proyectos ejemplos, como Sample3, y copiar los archivos FormConfig.lfm, FormConfig.pas y todos los archivos de tipo FrameCfg*.* a la carpeta de nuestro proyecto. Luego, simplemente incluir el código para iniciar y guardar el archivo de configuración en nuestro programa principal:

```
procedure TForm1.FormShow(Sender: TObject);
begin
  Config.Iniciar(self);   
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.escribirArchivoIni;  //guarda la configuración actual
end;
```

Y en alguna parte del programa, para mostrar el formulario de configuración, usar:

```
  Config.Mostrar;
```

A partir de allí se pueden modificar los frames de configuración, para adecuarlos a nuestras necesidades.

Es importante asegurarse que el formulario FormConfig, se carge al iniciar el programa (de otra forma se generará un error en tiempo de ejecución). Esto se puede hacer por código o usando el menú: "Proyecto>Opciones de Proyecto>Formulario".

## Creando un nuevo Frame de Configuración

Para crear un frame de configuración, se puede seguir este procedimiento:

1. Copiar los archivos de la librería a una carpeta determinada.
2. Abrir el Inspector de Proyecto e incluir el archivo "ConfigFrame.pas", desde la carpeta donde se guardó la librería.
3. Abrir el archivo "ConfigFrame.pas", en el editor y mostrar el Frame (F12).
4. En el menú principal, elegir "Archivo>Nuevo", luego en el cuadro mostrado, elegir "Componente heredado>Componente de proyecto heredado".
5. Elegir "ConfigFrame" y ya se tendrá un frame de configuración, listo para incluir los controles que se usarán para mostrar las propiedades.

Lo primero que se recomienda hacer con un nuevo frame de configuración es, cambiar el nombre de la unidad (que debe estar como UnitX) a algo como frameCfgGeneral. Luego se debe cambiar el nombre del frame mismo, usando el inspector de objetos. El nombre del frame (no de la unidad que lo contiene), debe ser algo así como fraCfgGeneral.

Una vez creado el frame y con los nombres configurados, se tendrá un código como este:

```
unit frameCfgGeneral;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ConfigFrame;

type
  TfraCfgGeneral = class(TConfigFrame)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fraCfgGeneral: TfraCfgGeneral;

implementation
{$R *.lfm}

end.
```

Sobre este código, se debe crear un método de inicio, que permita asociar los controles (o variables) al archivo INI. El código funcional,pero sin asociaciones, sería este:

```
unit FrameCfgGeneral;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ConfigFrame;

type

  { TfraCfgGeneral }
  TfraCfgGeneral = class(TCfgFrame)
    Edit1: TEdit;
  public
    { public declarations }
    procedure Iniciar(secINI0: string); //Inicia el frame
  end;

implementation
{$R *.lfm}

{ TfraCfgGeneral }
procedure TfraCfgGeneral.Iniciar(secINI0: string);
begin
  secINI := secINI0;  //sección INI
end;

end.
```

A partir de esta código mínimo, se deben ir agregando las variables y controles, que se desean preservar en el archivo INI, como se mostró en el primer código de ejemplo.

Para mayor información, revisar los códigos de ejemplo, de la página web.

## Flujo de información

ConfigFrame se basa en que la información de las propiedades (variables) se mueven de acuerdo al siguiente flujo:

Disco  <----> Propiedades <------> Formulario

Visualmente, lo que se ve para editar a las propiedades es el formulario (realmente se editan en el frame de configuración), y cuando se aceptan los cambios, se produce la actualizción de las propiedades.

El flujo de las variables a disco se produce cuando se desea hacer los cambios permanentes. Esto se debe hacer llamando a la instrucción SavePropToFile_AllFrames().

De igual forma para leer los datos de disco a las propiedades, se debe usar la instrucción ReadFileToProp_AllFrames().

## Métodos para asociar controles

Existen diversos métodos para asociar variables a controles:
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
 
* Los Frames de configuración se deben crear como componente heredado de "CfgFrame", para que puedan funcionar como Frames configuración.

Para los nombres de objetos, se recomienda las siguientes normas:

* Las unidades donde se definen los frame de configuración deben llamarse frameCfg{XXX}. Donde {XXX} es la parte del nombre que define la función. Por ejemplo frameCfgColores, frameCfgMainEdit

* La unidad donde se define al formulario  de configuración (en donde se incluirán los frames de configuración), se llamará FormConfig, y el formulario se debe llamar Config.

* Los frame de configuración creados en el formulario de configuración, se deben llamar fc{XXX}.

Se pueden usar los programas de ejemplo como una plantilla de trabajo.
