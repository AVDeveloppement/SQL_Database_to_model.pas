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
    Procedure FormCanResize(Sender: TObject; Var NewWidth, NewHeight: Integer;
      Var Resize: Boolean);
  Private
    ConfigPath: String;
    LastWindowState: TWindowState;
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
  ConfigPath      := AConfigPath;
  LastWindowState := TWindowState.wsNormal;
End;

Procedure TfmPreview.FormCanResize(Sender: TObject; Var NewWidth,
  NewHeight: Integer; Var Resize: Boolean);
Begin
  If WindowState <> TWindowState.wsMinimized Then
    LastWindowState := WindowState;
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

      If WindowState = TWindowState.wsNormal Then
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

  SetSynMemoHighlighter(smMemo, Project.FModelHighlight);

  Caption     := Project.FileName;

  smMemo.Text := Project.GenerateText;
End;

Procedure TfmPreview.Show;
Begin
  Inherited Show;

  If WindowState = TWindowState.wsMinimized Then
    WindowState := LastWindowState;
End;

End.
