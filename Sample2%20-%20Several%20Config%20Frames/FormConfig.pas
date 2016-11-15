{Model of Configuration Form with 2 Configuration Frames}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls,
   //config frames here
  frameCfgTexto, frameCfgColores, frameCfgNumeros,
  ConfigFrame;  //need to work

type

  { TConfig }

  TConfig = class(TForm)
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    btnAccept: TBitBtn;
    lstCateg: TListBox;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstCategClick(Sender: TObject);
  private
    { private declarations }
  public
    msjError: string;    //for error message
    fraError: TCfgFrame;
    arIni   : String;      //Ini file
    //list of configuration frames
    fcTexto: TfraTexto;
    fcColores: TfraColores;
    fcNumeros: TfraNumeros;
    procedure WriteIniFile;
    procedure ReadIniFile;
    procedure WindowToProp;
    procedure PropToWindow;
    procedure Start(f: TForm);
    procedure Show;
  end;

var
  Config: TConfig;

implementation

{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  //Create dynamically the frames
  fcTexto:= TfraTexto.Create(Self);
  fcTexto.parent := self;
  fcColores:= TfraColores.Create(Self);
  fcColores.parent := self;
  fcNumeros := TfraNumeros.Create(Self);
  fcNumeros.parent := self;

  arIni := GetIniName;
end;

procedure TConfig.btnAcceptClick(Sender: TObject);
begin
  btnApplyClick(Self);
  if fraError<>nil then exit;  //error?
  self.Close;  //exit if no error
end;

procedure TConfig.btnApplyClick(Sender: TObject);
begin
  WindowToProp;       //Escribe propiedades de los frames
  if fraError<>nil then begin
    showmessage(fraError.MsjErr);
    exit;
  end;
  WriteIniFile;   //save properties to disk
end;

procedure TConfig.Start(f: TForm);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //Start our frames
  fcTexto.Iniciar('texto');
  fcColores.Iniciar('colores',f);
  fcNumeros.Iniciar('numeros');

  ReadIniFile;  //read properties from the Ini file.
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  PropToWindow;   //Load properties to the frame
end;

procedure TConfig.lstCategClick(Sender: TObject);
begin
  Hide_AllConfigFrames(self);   //hide all
  if lstCateg.ItemIndex = 0 then fcTexto.ShowPos(120,0) ;
  if lstCateg.ItemIndex = 1 then fcColores.ShowPos(120,0);
  if lstCateg.ItemIndex = 2 then fcNumeros.ShowPos(120,0);
end;

procedure TConfig.Show;
//Show the form to config
begin
  lstCateg.ItemIndex:=0;   //define first frame
  lstCategClick(self);
  Showmodal;
end;

procedure TConfig.WindowToProp;
begin
  fraError := WindowToProp_AllFrames(self);
end;

procedure TConfig.PropToWindow;
begin
  fraError := PropToWindow_AllFrames(self);
end;

procedure TConfig.ReadIniFile;
begin
  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.WriteIniFile;
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

