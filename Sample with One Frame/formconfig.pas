{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, frameGeneral,
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
  General.Initiate('general', f);
  General.ShowPos(120,0);
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //free all config frames
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  msjError := PropToWindow_AllFrames(self);
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  msjError := WindowToProp_AllFrames(self);
  if msjError<>'' then begin
    showmessage(msjError);
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

