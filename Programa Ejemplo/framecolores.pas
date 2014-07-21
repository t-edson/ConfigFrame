{Ejemplo de Frame de propiedades }
unit FrameColores;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls,
  Graphics,
  ConfigFrame;  //necesario para manejar los Frames de configuración

type

  TfraColores = class(TFrame)
    ColorButton1: TColorButton;
    Label1: TLabel;
    procedure ConfigEditor;
  private
    f: Tform;  //referencia al objeto que modifica
  public
    //variables de propiedades
    colFon: TColor;
    procedure Iniciar(secINI0: string; form: TForm); //Inicia el frame
  end;

implementation
{$R *.lfm}

procedure TfraColores.ConfigEditor;
//actualiza los cambios de las propiedades
begin
  f.Color := colFon;
end;

procedure TfraColores.Iniciar(secINI0: string; form: TForm);
begin
  secINI := secINI0;  //sección INI
  //asigna referencia necesarias
  f := form;
  OnUpdateChanges:=@ConfigEditor;  //manejador de cambios
  //asocia propiedades a controles
  Asoc_Col_TColBut(@colFon, ColorButton1, 'colFon',clWhite);
end;

end.

