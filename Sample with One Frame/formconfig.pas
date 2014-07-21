{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, iniFiles, frameGeneral,
  ConfigFrame;  //needed for use the config Frames

type

  { TConfig }

  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    msjError: string;    //Error string
    arIni   : String;    //INI file
    General: TfraConfig;  //Config Frames
    procedure Initiate(f: TForm);
    procedure WindowToProp;
    procedure PropToWindow;
    procedure ReadFromFile;
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  General:= TfraConfig.Create(Self);
  General.parent := self;
  arIni := GetIniName;
end;

procedure TConfig.Initiate(f: TForm);
begin
  //inicia los Frames creados
  General.Initiate('general', f);
  General.ShowPos(120,0);
  ReadFromFile;  //lee parámetros del archivo de configuración.
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  PropToWindow;   //carga las propiedades en el frame
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  WindowToProp;       //Escribe propiedades de los frames
  if msjError<>'' then begin
    showmessage(msjError);
    exit;
  end;
  SaveToFile;   //guarda propiedades en disco
  self.Close;
end;

procedure TConfig.WindowToProp;
begin
  msjError := WindowToProp_AllFrames(self);
end;

procedure TConfig.PropToWindow;
begin
  msjError := PropToWindow_AllFrames(self);
end;

procedure TConfig.ReadFromFile;
begin
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.SaveToFile;
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

