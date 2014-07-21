{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, iniFiles,
   //deben incluirse todos los frames de propiedades a usar
  frameTexto, frameColores, frameNumeros,
  ConfigFrame;  //necesario para manejar los Frames de configuración

type

  { TConfig }

  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    lstCateg: TListBox;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstCategClick(Sender: TObject);
  private
    { private declarations }
  public
    msjError: string;    //para los mensajes de error
    arIni   : String;      //Archivo de configuración
    //************  Modificar Aquí ***************//
    //frames de configuración
    Texto: TfraTexto;
    Colores: TfraColores;
    Numeros: TfraNumeros;
    procedure escribirArchivoIni;
    procedure leerArchivoIni;
    procedure LeerDeVentana;
    procedure MostEnVentana;
    procedure Iniciar(f: TForm);
    procedure Mostrar;
  end;

var
  Config: TConfig;

implementation

{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  //************  Modificar Aquí ***************//
  //Crea dinámicamente los frames de configuración
  Texto:= TfraTexto.Create(Self);
  Texto.parent := self;
  Colores:= TfraColores.Create(Self);
  Colores.parent := self;
  Numeros := TfraNumeros.Create(Self);
  Numeros.parent := self;

  arIni := GetIniName;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if msjError='' then self.Close;  //sale si no hay error
end;

procedure TConfig.BitAplicarClick(Sender: TObject);
begin
  LeerDeVentana;       //Escribe propiedades de los frames
  if msjError<>'' then begin
    showmessage(msjError);
    exit;
  end;
  escribirArchivoIni;   //guarda propiedades en disco
end;

procedure TConfig.Iniciar(f: TForm);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //************  Modificar Aquí ***************//
  //inicia los Frames creados
  Texto.Iniciar('texto');
  Colores.Iniciar('colores',f);
  Numeros.Iniciar('numeros');

  LeerArchivoIni;  //lee parámetros del archivo de configuración.
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  MostEnVentana;   //carga las propiedades en el frame
end;

procedure TConfig.lstCategClick(Sender: TObject);
begin
  Hide_AllConfigFrames(self);   //oculta todos
  //************  Modificar Aquí ***************//
  if lstCateg.ItemIndex = 0 then Texto.ShowPos(120,0) ;
  if lstCateg.ItemIndex = 1 then Colores.ShowPos(120,0);
  if lstCateg.ItemIndex = 2 then Numeros.ShowPos(120,0);
end;

procedure TConfig.Mostrar;
//Muestra el formulario para configurarlo
begin
  lstCateg.ItemIndex:=0;   //define frame inicial
  lstCategClick(self);
  Showmodal;
end;

procedure TConfig.LeerDeVentana;
//Lee las propiedades de la ventana de configuración.
begin
  msjError := WindowToProp_AllFrames(self);
end;

procedure TConfig.MostEnVentana;
//Muestra las propiedades en la ventana de configuración.
begin
  msjError := PropToWindow_AllFrames(self);
end;

procedure TConfig.leerArchivoIni;
//Lee el archivo de configuración
begin
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.escribirArchivoIni;
//Escribe el archivo de configuración
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

