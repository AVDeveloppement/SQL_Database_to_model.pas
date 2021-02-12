Unit Settings;

Interface

Uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.StdCtrls, Projects, Vcl.Samples.Spin, System.IniFiles, Vcl.Mask, Winapi.ShellApi,
  System.UITypes, CommandLine, System.ImageList, Vcl.ImgList;

Type
  TfmSettings = Class(TForm)
    sbClose: TSpeedButton;
    pcSettings: TPageControl;
    tsSettingsGeneral: TTabSheet;
    tsSettingsProject: TTabSheet;
    cbLastProjectAutoOpen: TCheckBox;
    bProjectPathHistoryClear: TButton;
    seProjectPathHistoryMax: TSpinEdit;
    sePathHistoryMax: TSpinEdit;
    bClearPathHistory: TButton;
    LabelMaxPath: TLabel;
    LabelMaxProjectPath: TLabel;
    cbActualProjectAutoSave: TCheckBox;
    cbReplaceInFilename: TCheckBox;
    meProjectExt: TMaskEdit;
    LabelProjectExt: TLabel;
    bRegisterProjectExt: TButton;
    ImageList: TImageList;
    LabelModelHighligth: TLabel;
    cbModelHighlight: TComboBox;
    LabelEchangeitemDescribeHighlight: TLabel;
    LabelEchangeitemReplaceHighlight: TLabel;
    cbExchangeItemDescribeHighlight: TComboBox;
    cbExchangeItemReplaceHighlight: TComboBox;
    LabelEchangeitemByHighlight: TLabel;
    cbExchangeItemByHighlight: TComboBox;
    Procedure sbCloseClick(Sender: TObject);
    Procedure sePathHistoryMaxChange(Sender: TObject);
    Procedure bProjectPathHistoryClearClick(Sender: TObject);
    Procedure bClearPathHistoryClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure cbReplaceInFilenameClick(Sender: TObject);
    Procedure bRegisterProjectExtClick(Sender: TObject);
    Procedure meProjectExtExit(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure cbModelHighlightChange(Sender: TObject);
    Procedure cbExchangeItemDescribeHighlightChange(Sender: TObject);
    Procedure cbExchangeItemReplaceHighlightChange(Sender: TObject);
    Procedure cbExchangeItemByHighlightChange(Sender: TObject);
  Private
    FCanClose, FNeedRefresh: Boolean;
    Procedure RunAsAdminAndWaitFinnish(HWND: HWND; FileName, Parameters: String);
    Function GetProjectExt: String;
    Procedure SetProjectExt(Const Value: String);
    Function GetActualProjectAutoSave: Boolean;
    Function GetLastProjectAutoOpen: Boolean;
    Function GetProjectPathHistoryMax: Integer;
    Procedure SetActualProjectAutoSave(Const Value: Boolean);
    Procedure SetLastProjectAutoOpen(Const Value: Boolean);
    Procedure SetProjectPathHistoryMax(Const Value: Integer);
    Procedure SetStateProjectExtRegistered;
  Public
    Procedure LoadFromIni(AMemIniFile: TMemIniFile);
    Procedure SaveToIni(AMemIniFile: TMemIniFile);
    Property ActualProjectAutoSave: Boolean Read GetActualProjectAutoSave Write SetActualProjectAutoSave;
    Property LastProjectAutoOpen: Boolean Read GetLastProjectAutoOpen Write SetLastProjectAutoOpen;
    Property NeedRefresh: Boolean Read FNeedRefresh;
    Property ProjectExt: String Read GetProjectExt Write SetProjectExt;
    Property ProjectPathHistoryMax: Integer Read GetProjectPathHistoryMax Write SetProjectPathHistoryMax;
  End;

Var
  fmSettings: TfmSettings;

Implementation

{$R *.dfm}


Procedure TfmSettings.bClearPathHistoryClick(Sender: TObject);
Begin
  Project.FPathHistory.Clear;

  FNeedRefresh := True;
End;

Procedure TfmSettings.bProjectPathHistoryClearClick(Sender: TObject);
Begin
  ProjectPathHistory.Clear;

  FNeedRefresh := True;
End;

Procedure TfmSettings.bRegisterProjectExtClick(Sender: TObject);
Var
  sState: String;
Begin
  If Not CmdLine.RegistryReadAppEntry Then
    sState := '/registerAssoc'
  Else
    sState := '/unregisterAssoc';

  RunAsAdminAndWaitFinnish(Handle, Application.ExeName, sState + ' ' + ProjectExt);

  SetStateProjectExtRegistered;
End;

Procedure TfmSettings.cbExchangeItemByHighlightChange(Sender: TObject);
Begin
  Project.FExchangeItemByHighlight := TProjectSynMemoHighlighter(cbExchangeItemByHighlight.ItemIndex);
End;

Procedure TfmSettings.cbExchangeItemDescribeHighlightChange(Sender: TObject);
Begin
  Project.FExchangeItemDescribeHighlight := TProjectSynMemoHighlighter
    (cbExchangeItemDescribeHighlight.ItemIndex);
End;

Procedure TfmSettings.cbExchangeItemReplaceHighlightChange(Sender: TObject);
Begin
  Project.FExchangeItemReplaceHighlight := TProjectSynMemoHighlighter
    (cbExchangeItemReplaceHighlight.ItemIndex);
End;

Procedure TfmSettings.cbModelHighlightChange(Sender: TObject);
Begin
  Project.FModelHighlight := TProjectSynMemoHighlighter(cbModelHighlight.ItemIndex);

  FNeedRefresh            := True;
End;

Procedure TfmSettings.cbReplaceInFilenameClick(Sender: TObject);
Begin
  Project.FReplaceInFilename := cbReplaceInFilename.Checked;
End;

Procedure TfmSettings.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  CanClose := FCanClose;
End;

Procedure TfmSettings.FormCreate(Sender: TObject);
Begin
  FCanClose := True;
End;

Procedure TfmSettings.FormShow(Sender: TObject);
Begin
  FNeedRefresh                              := False;

  sePathHistoryMax.Value                    := Project.FPathHistoryMax;
  cbReplaceInFilename.Checked               := Project.FReplaceInFilename;
  cbModelHighlight.ItemIndex                := Integer(Project.FModelHighlight);
  cbExchangeItemDescribeHighlight.ItemIndex := Integer(Project.FExchangeItemDescribeHighlight);
  cbExchangeItemReplaceHighlight.ItemIndex  := Integer(Project.FExchangeItemReplaceHighlight);
  cbExchangeItemByHighlight.ItemIndex       := Integer(Project.FExchangeItemByHighlight);

  SetStateProjectExtRegistered;
End;

Function TfmSettings.GetActualProjectAutoSave: Boolean;
Begin
  Result := cbActualProjectAutoSave.Checked;
End;

Function TfmSettings.GetLastProjectAutoOpen: Boolean;
Begin
  Result := cbLastProjectAutoOpen.Checked;
End;

Function TfmSettings.GetProjectPathHistoryMax: Integer;
Begin
  Result := seProjectPathHistoryMax.Value;
End;

Function TfmSettings.GetProjectExt: String;
Begin
  Result := meProjectExt.Text;
  Result := Result.Trim;
End;

Procedure TfmSettings.LoadFromIni(AMemIniFile: TMemIniFile);
Begin
  With AMemIniFile Do
  Begin
    ProjectPathHistoryMax := ReadInteger('fmSettings', 'ProjectPathHistoryMax',
      ProjectPathHistoryMax);
    LastProjectAutoOpen   := ReadBool('fmSettings', 'LastProjectAutoOpen', LastProjectAutoOpen);
    ActualProjectAutoSave := ReadBool('fmSettings', 'ActualProjectAutoSave',
      ActualProjectAutoSave);
    ProjectExt            := ReadString('fmSettings', 'ProjectExt', ProjectExt);
  End;
End;

Procedure TfmSettings.meProjectExtExit(Sender: TObject);
Begin
  FCanClose := CmdLine.ProjectExtIsValid(ProjectExt);
  If Not FCanClose Then
  Begin
    pcSettings.ActivePage := tsSettingsGeneral;
    meProjectExt.SetFocus;
    MessageDlg('ERROR: Project extension are incorrect, use characters a-Z, 0-9 and size 3-7', mtError,
      [mbOK], 0);
  End;
End;

Procedure TfmSettings.RunAsAdminAndWaitFinnish(HWND: HWND; FileName: String; Parameters: String);
Var
  seiDetails: TShellExecuteInfo;
  ExitCode: Cardinal;
Begin
  Fillchar(seiDetails, SizeOf(seiDetails), 0);
  With seiDetails Do
  Begin
    cbSize       := SizeOf(seiDetails);
    Wnd          := HWND;
    fMask        := SEE_MASK_FLAG_DDEWAIT Or SEE_MASK_FLAG_NO_UI;
    lpfile       := PChar(FileName);
    lpVerb       := 'runas';
    lpParameters := PChar(Parameters);
    nShow        := SW_SHOWNORMAL;
  End;
  If ShellExecuteEx(@seiDetails) Then
  Begin
    Repeat
      Sleep(100);
      Application.ProcessMessages;
      GetExitCodeProcess(seiDetails.hProcess, ExitCode);
    Until (ExitCode <> STILL_ACTIVE) Or Application.Terminated;
    SetStateProjectExtRegistered; // when app exit we update state
  End;
End;

Procedure TfmSettings.SaveToIni(AMemIniFile: TMemIniFile);
Begin
  With AMemIniFile Do
  Begin
    EraseSection('fmSettings');

    WriteInteger('fmSettings', 'ProjectPathHistoryMax', ProjectPathHistoryMax);
    WriteBool('fmSettings', 'LastProjectAutoOpen', LastProjectAutoOpen);
    WriteBool('fmSettings', 'ActualProjectAutoSave', ActualProjectAutoSave);
    WriteString('fmSettings', 'ProjectExt', ProjectExt);
  End;
End;

Procedure TfmSettings.sbCloseClick(Sender: TObject);
Begin
  If pcSettings.ActivePage = tsSettingsGeneral Then
    cbLastProjectAutoOpen.SetFocus; // for event onexit apply before close

  Close;
End;

Procedure TfmSettings.sePathHistoryMaxChange(Sender: TObject);
Begin
  Project.FPathHistoryMax := sePathHistoryMax.Value;
End;

Procedure TfmSettings.SetActualProjectAutoSave(Const Value: Boolean);
Begin
  cbActualProjectAutoSave.Checked := Value;
End;

Procedure TfmSettings.SetLastProjectAutoOpen(Const Value: Boolean);
Begin
  cbLastProjectAutoOpen.Checked := Value;
End;

Procedure TfmSettings.SetProjectPathHistoryMax(Const Value: Integer);
Begin
  seProjectPathHistoryMax.Value := Value;
End;

Procedure TfmSettings.SetStateProjectExtRegistered;
Begin
  If CmdLine.RegistryReadAppEntry Then
  Begin
    meProjectExt.ReadOnly       := True;
    meProjectExt.Enabled        := False;
    bRegisterProjectExt.Caption := 'Unregister this extension with this program';
  End
  Else
  Begin
    meProjectExt.ReadOnly       := False;
    meProjectExt.Enabled        := True;
    bRegisterProjectExt.Caption := 'Register this extension with this program';
  End;
End;

Procedure TfmSettings.SetProjectExt(Const Value: String);
Begin
  meProjectExt.Text := Value;
End;

End.
