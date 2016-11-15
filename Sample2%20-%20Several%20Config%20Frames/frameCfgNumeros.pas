unit frameCfgNumeros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ConfigFrame;  //necesario para manejar los Frames de configuración

type

  { TFraNumeros }

  TFraNumeros = class(TCfgFrame)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  public
    //variables de propiedades
    numero : integer;
    numero2: integer;
    procedure Iniciar(secINI0: string); //Inicia el frame
  end;

implementation
{$R *.lfm}

procedure TFraNumeros.Iniciar(secINI0: string);
begin
  secINI := secINI0;  //sección INI
  //asocia propiedades a controles
  Asoc_Int_TEdit(@numero, Edit1, 'numero', 0, 0, 10);
  Asoc_Int_TEdit(@numero2, Edit2, 'numero2', 0, 1, 10);
end;

end.

