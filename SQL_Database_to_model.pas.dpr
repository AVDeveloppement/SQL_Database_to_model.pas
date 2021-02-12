Program SQL_Database_to_model.pas;

Uses
  Vcl.Forms,
  Main In 'Main.pas' {fmMain} ,
  Settings In 'Settings.pas' {fmSettings} ,
  Preview In 'Preview.pas' {fmPreview} ,
  Projects In 'Projects.pas',
  CommandLine In 'CommandLine.pas',
  Vcl.Themes,
  Vcl.Styles,
  MemoHighlight In 'MemoHighlight.pas' {fmMemoHighlight};

{$R *.res}


Begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  // if app called with registry command line, processed then do exit
  If CmdLine.RegistryAssocProcessed Then
    Exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.Run;

End.
