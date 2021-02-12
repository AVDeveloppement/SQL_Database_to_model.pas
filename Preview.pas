Unit Preview;

Interface

Uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IniFiles,
  Projects, SynEdit, SynMemo;

Type
  TfmPreview = Class(TForm)
    smMemo: TSynMemo;
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

Uses
  MemoHighlight;

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
      EraseSection(Name);

      If WindowState = TWindowState.WsNormal Then
      Begin
        WriteInteger(Name, 'Width', Width);
        WriteInteger(Name, 'Height', Height);

        WriteInteger(Name, 'Left', Left);
        WriteInteger(Name, 'Top', Top);
      End;

      WriteInteger(Name, 'WindowState', Integer(WindowState));

      UpdateFile;
    Finally
      Free;
    End;

  fmPreview := Nil;
End;

Procedure TfmPreview.FormShow(Sender: TObject);
Begin
  With TMemIniFile.Create(ConfigPath) Do
    Try
      Width       := ReadInteger(Name, 'Width', Width);
      Height      := ReadInteger(Name, 'Height', Height);

      Left        := ReadInteger(Name, 'Left', Left);
      Top         := ReadInteger(Name, 'Top', Top);

      WindowState := TWindowState(ReadInteger(Name, 'WindowState', Integer(WindowState)));
    Finally
      Free;
    End;
End;

Procedure TfmPreview.Show;
Begin
  SetSynMemoHighlighter(smMemo, Project.FModelHighlight);

  Caption     := Project.FileName;

  smMemo.Text := Project.GenerateText;

  Inherited Show;

  If WindowState = TWindowState.wsMinimized Then
    WindowState := TWindowState.WsNormal;
End;

End.
