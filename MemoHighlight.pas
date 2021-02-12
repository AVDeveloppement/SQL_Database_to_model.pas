Unit MemoHighlight;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, SynMemo, Vcl.Buttons, System.IniFiles;

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

Implementation

{$R *.dfm}


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
