{Modelo de formulario de configuración que usa solo un Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  frameCfgGeneral, ConfigFrame;  //needed for use the config Frames

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
    fraError: TCfgFrame;
    arIni   : String;    //INI file

    fcGeneral: TfraConfig;  //Config Frames
    procedure Initiate(f: TForm);
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  fcGeneral:= TfraConfig.Create(Self);
  fcGeneral.parent := self;
  arIni := GetIniName;
end;

procedure TConfig.Initiate(f: TForm);
begin
  fcGeneral.Initiate('general', f);
  fcGeneral.ShowPos(120,0);
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //free all config frames
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  fraError := PropToWindow_AllFrames(self);
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  fraError := WindowToProp_AllFrames(self);
  if fraError<>nil then begin
    showmessage(fraError.MsjErr);
    exit;
  end;
  SaveToFile;
  self.Close;
end;

procedure TConfig.SaveToFile;
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

