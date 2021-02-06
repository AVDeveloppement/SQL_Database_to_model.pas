Unit Preview;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IniFiles,
  Settings, Projects;

Type
  TfmPreview = Class(TForm)
    mPreview: TMemo;
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Show; Reintroduce;
    Constructor Create(AOwner: TComponent; AConfigPath: String); Reintroduce; Overload;
  Private
    ConfigPath: String;
  End;

Var
  fmPreview: TfmPreview = Nil;

Implementation

{$R *.dfm}


Constructor TfmPreview.Create(AOwner: TComponent; AConfigPath: String);
Begin
  Inherited Create(AOwner);
  ConfigPath := AConfigPath;
End;

Procedure TfmPreview.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  Action := caFree;
End;

Procedure TfmPreview.FormDestroy(Sender: TObject);
Begin
  With TMemIniFile.Create(ConfigPath) Do
    Try
      EraseSection('fmPreview');

      If WindowState = TWindowState.WsNormal Then
      Begin
        WriteInteger('fmPreview', 'Width', Width);
        WriteInteger('fmPreview', 'Height', Height);

        WriteInteger('fmPreview', 'Left', Left);
        WriteInteger('fmPreview', 'Top', Top);
      End;

      WriteInteger('fmPreview', 'WindowState', Integer(WindowState));

      UpdateFile;
    Finally
      Free;
    End;
  fmPreview := Nil;
End;

Procedure TfmPreview.FormShow(Sender: TObject);
Var
  MemIniFile: TMemIniFile;
Begin
  MemIniFile := TMemIniFile.Create(ConfigPath);
  Try
    With MemIniFile Do
    Begin
      Width       := ReadInteger('fmPreview', 'Width', Width);
      Height      := ReadInteger('fmPreview', 'Height', Height);

      Left        := ReadInteger('fmPreview', 'Left', Left);
      Top         := ReadInteger('fmPreview', 'Top', Top);

      WindowState := TWindowState(ReadInteger('fmPreview', 'WindowState', Integer(WindowState)));
    End;
  Finally
    MemIniFile.Free;
  End;
End;

Procedure TfmPreview.Show;
Begin
  Caption       := Project.FileName;
  mPreview.Text := Project.GenerateText;

  Inherited Show;

  If WindowState = TWindowState.wsMinimized Then
    WindowState := TWindowState.WsNormal;
End;

End.
