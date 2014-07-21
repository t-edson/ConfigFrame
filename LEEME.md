ConfigFrame
=============

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

Se incluye un ejemplo sencillo en donde se implementa una ventana de configuración que usa dos Frames, que se han implementado con pocas líneas de código.

El esquema de trabajo definido para el manejo de las propiedades de una aplicación es:

* Se usará un solo archivo INI y una sola ventana de configuración. Aunque se podría manejar diversos archivos de configuración, se recomienda mantener la relación:  Archivo INI <-> Ventana de configuración.

* Una ventana de configuración puede manejar uno o más Frames, a los que se les llamará Frame de Configuración. Cuando se manejen más de uno, se puede usar un control de  lista o pestañas para elegir con que Frame(s) trabajar.

* Cada Frame de configuración agrupa a un conjunto de propiedades que tengan relación entre sí. Por ejemplo, se puede tener un Frame para las propiedades generales, uno para las propiedades del editor, ... etc.

* Los Frames se crean normalmente con el editor visual de Lazarus, colocando los controles necesarios para manejar a las propiedades que se usan en ese Frame.
 
* La ventana de configuración, así como los Frames de configuración se deben crear incluyendo la unidad "ConfigFrame", para que puedan usar la nueva definición de TFrame. Esta unidad se debe incluir al final de la sección USES para lograr la interceptación de la clase TFrame.

Se puede usar el programa ejemplo como una plantilla de trabajo.
