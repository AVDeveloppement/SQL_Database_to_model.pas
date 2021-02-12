Unit MemoHighlight;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, SynMemo, Vcl.Buttons, System.IniFiles,
  Projects, Highlighters;

Type
  TfmMemoHighlight = Class(TForm)
    sbOK: TSpeedButton;
    sbCancel: TSpeedButton;
    smMemo: TSynMemo;
    Procedure sbCancelClick(Sender: TObject);
    Procedure sbOKClick(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Constructor Create(AOwner: TComponent; AConfigPath: String); Reintroduce; Overload;
    Procedure FormShow(Sender: TObject);
  Private
    ConfigPath: String;
  End;

Var
  fmMemoHighlight: TfmMemoHighlight;

Procedure SetSynMemoHighlighter(ASynMemo: TSynMemo;
  AHighlighter: TProjectSynMemoHighlighter);

Implementation

{$R *.dfm}


Procedure SetSynMemoHighlighter(ASynMemo: TSynMemo;
  AHighlighter: TProjectSynMemoHighlighter);
Begin
  With dmHighlighters Do
  Begin
    Case AHighlighter Of
      psmhCNET:
        ASynMemo.Highlighter := shCSS;
      psmhC:
        ASynMemo.Highlighter := shCPP;
      psmhHTML:
        ASynMemo.Highlighter := shHTML;
      psmhINI:
        ASynMemo.Highlighter := shINI;
      psmhInnoSetupScript:
        ASynMemo.Highlighter := shINNO;
      psmhJava:
        ASynMemo.Highlighter := shJAVA;
      psmhJavaScript:
        ASynMemo.Highlighter := shJSCRIPT;
      psmhObjectPascal:
        ASynMemo.Highlighter := shPAS;
      psmhPerl:
        ASynMemo.Highlighter := shPERL;
      psmhPHP:
        ASynMemo.Highlighter := shPHP;
      psmhSQL:
        ASynMemo.Highlighter := shSQL;
      psmhUNIXShellScript:
        ASynMemo.Highlighter := shUNIX;
      Else
        ASynMemo.Highlighter := Nil;
    End;
  End;
End;

{ TfmMemoHighlight }

Constructor TfmMemoHighlight.Create(AOwner: TComponent; AConfigPath: String);
Begin
  Inherited Create(AOwner);
  ConfigPath := AConfigPath;
End;

Procedure TfmMemoHighlight.FormCloseQuery(Sender: TObject;
  Var CanClose: Boolean);
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

      UpdateFile;
    Finally
      Free;
    End;
End;

Procedure TfmMemoHighlight.FormShow(Sender: TObject);
Begin
  With TMemIniFile.Create(ConfigPath) Do
    Try
      Width  := ReadInteger(Name, 'Width', Width);
      Height := ReadInteger(Name, 'Height', Height);

      Left   := ReadInteger(Name, 'Left', Left);
      Top    := ReadInteger(Name, 'Top', Top);
    Finally
      Free;
    End;
End;

Procedure TfmMemoHighlight.sbCancelClick(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TfmMemoHighlight.sbOKClick(Sender: TObject);
Begin
  ModalResult := mrOk;
End;

End.
