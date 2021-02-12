Unit Main;

Interface

Uses
  Winapi.Windows, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Themes,
  Vcl.Dialogs, Vcl.StdCtrls, System.Types, System.UITypes, System.IniFiles,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus, Vcl.Buttons,
  Vcl.Grids, System.Actions, Vcl.ActnList, Vcl.ComCtrls,
  Settings, Projects, Preview, CommandLine, System.IOUtils, SynEditMiscClasses,
  SynEditSearch, SynEdit, SynMemo;

Type

  TCustomGridHelper = Class Helper For TCustomGrid
    Function GetGridState: TGridState;
  End;

  TfmMain = Class(TForm)
    ImageList: TImageList;
    bePath: TButtonedEdit;
    FileSaveDialogPath: TFileSaveDialog;
    sbCreateFile: TSpeedButton;
    sbProjects: TSpeedButton;
    pmPathHistory: TPopupMenu;
    pmProjectsMenu: TPopupMenu;
    miOpenProject: TMenuItem;
    miSaveProjectAs: TMenuItem;
    miSeparator: TMenuItem;
    miProjectHistory: TMenuItem;
    miSaveProject: TMenuItem;
    miSettings: TMenuItem;
    miSettingsGeneral: TMenuItem;
    miSettingsProject: TMenuItem;
    miHistoryEmpty: TMenuItem;
    pmExchangeItem: TPopupMenu;
    miExchangeItemAdd: TMenuItem;
    miExchangeItemDelete: TMenuItem;
    FileSaveDialogProject: TFileSaveDialog;
    FileOpenDialogProject: TFileOpenDialog;
    miEmpty: TMenuItem;
    pcProject: TPageControl;
    tsExchangeItem: TTabSheet;
    tsModel: TTabSheet;
    sgExchangeItem: TStringGrid;
    sbPreview: TSpeedButton;
    miNewProject: TMenuItem;
    smModel: TSynMemo;
    sesSearch: TSynEditSearch;
    Procedure sbCreateFileClick(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormShow(Sender: TObject);
    Procedure sbProjectsClick(Sender: TObject);
    Procedure sgExchangeItemSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure bePathLeftButtonClick(Sender: TObject);
    Procedure bePathRightButtonClick(Sender: TObject);
    Procedure miSettingsGeneralClick(Sender: TObject);
    Procedure miSettingsProjectClick(Sender: TObject);
    Procedure miExchangeItemAddClick(Sender: TObject);
    Procedure miOpenProjectClick(Sender: TObject);
    Procedure miSaveProjectAsClick(Sender: TObject);
    Procedure miExchangeItemDeleteClick(Sender: TObject);
    Procedure sgExchangeItemSetEditText(Sender: TObject; ACol, ARow: Integer;
      Const Value: String);
    Procedure sgExchangeItemMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormCreate(Sender: TObject);
    Procedure sbPreviewClick(Sender: TObject);
    Procedure miSaveProjectClick(Sender: TObject);
    Procedure bePathExit(Sender: TObject);
    Procedure sgExchangeItemDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure sgExchangeItemRowMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    Procedure sgExchangeItemClick(Sender: TObject);
    Procedure sgExchangeItemDblClick(Sender: TObject);
    Procedure PathHistoryClick(Sender: TObject);
    Procedure ProjectPathHistoryClick(Sender: TObject);
    Procedure miNewProjectClick(Sender: TObject);
    Procedure smModelChange(Sender: TObject);
  Private
    ConfigPath, ActualProjectPath: String;
    Procedure AddProjectInHistory(AProjectFileName: String);
    Procedure CheckBoxGrid(Checked: Boolean; StringGrid: TStringGrid;
      Rect: TRect; State: TGridDrawState);
    Function CreateGUID: String;
    Procedure LoadSettings;
    Procedure LoadProject(AProjectFileName: String = '');
    Procedure SaveSettings;
    Procedure SetActualProjectPath(AProjectFileName: String = '');
    Procedure sgExchangeItemDefault;
    Procedure sgExchangeItemSetList;
    Procedure SetPathAndHistory(APathFileName: String = '');
    Procedure SetProjectHistory;
    Procedure SaveProject;
    Procedure SaveProjectAs;
    Function ShowMemoHighlight(AHighlighter: TProjectSynMemoHighlighter;
      Var AValue: String): Boolean;
    Procedure ShowSettings;
    Procedure sgExchangeItemSetId;
  Public
  End;

Var
  fmMain: TfmMain;

Implementation

Uses
  MemoHighlight;

{$R *.dfm}


Const
  cCOL_WIDTHS_MIN = 20;
  cROW_HEIGHT_MIN = 20;

  { TCustomGridHelper }

Function TCustomGridHelper.GetGridState: TGridState;
Begin
  Result := Self.FGridState;
End;

{ TfmMain }

Procedure TfmMain.AddProjectInHistory(AProjectFileName: String);
Var
  iPos: Integer;
Begin
  // if already in we delete it
  iPos := ProjectPathHistory.IndexOf(AProjectFileName);
  If iPos <> -1 Then
    ProjectPathHistory.Delete(iPos);

  // add bottom list
  ProjectPathHistory.Add(AProjectFileName);

  // check limit of path are under settings choosen
  While ProjectPathHistory.Count > fmSettings.ProjectPathHistoryMax Do
    ProjectPathHistory.Delete(0);

  // show project history
  SetProjectHistory;
End;

Procedure TfmMain.bePathExit(Sender: TObject);
Begin
  If (bePath.Text <> '') And (ExtractFileExt(bePath.Text) = '') Then
    bePath.Text := bePath.Text + '.' + FileSaveDialogPath.DefaultExtension;

  If (Project.FPathHistory.Count > 0) And
    (Project.FPathHistory.Last = bePath.Text) Then
    Exit;

  SetPathAndHistory(bePath.Text);
End;

Procedure TfmMain.bePathLeftButtonClick(Sender: TObject);
Var
  pt: TPoint;
Begin
  // show menu under button
  pt.X := 0;
  pt.Y := -Round(21 * pmPathHistory.Items.Count);
  pt   := bePath.ClientToScreen(pt);
  pmPathHistory.Popup(pt.X, pt.Y);
End;

Procedure TfmMain.bePathRightButtonClick(Sender: TObject);
Begin
  If Not FileSaveDialogPath.Execute Then
    Exit;

  SetPathAndHistory(FileSaveDialogPath.FileName);
End;

Procedure TfmMain.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Try
    SaveSettings;
  Except
    On E: Exception Do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  End;

  Try
    If fmSettings.ActualProjectAutoSave Then
      SaveProject;
  Except
    On E: Exception Do
    Begin
      If MessageDlg('Error occured when saving project:' + sLineBreak +
        E.Message + sLineBreak + 'Do you want quit anyway?',
        mtError, [mbYes, mbCancel], 0, mbCancel) <> mrYes Then
      Begin
        CanClose := False;
        Exit;
      End;
    End;
  End;

  Project.Free;
  ProjectPathHistory.Free;
End;

Procedure TfmMain.FormCreate(Sender: TObject);
Begin
  Project            := TProject.Create;
  ProjectPathHistory := TProjectPathHistory.Create;

  // if fileexist in appdata roaming else use in app dir
  ConfigPath := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) +
    cPRODUCT_NAME + '\configuration.ini';
  If Not FileExists(ConfigPath) Then
    ConfigPath := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) +
      'configuration.ini';

  pcProject.TabIndex              := 0;
  ActualProjectPath               := '';

  sgExchangeItem.DefaultColWidth  := cCOL_WIDTHS_DEFAULT;
  sgExchangeItem.DefaultRowHeight := cROW_HEIGHT_DEFAULT;
End;

Procedure TfmMain.sgExchangeItemClick(Sender: TObject);
Begin
  If (sgExchangeItem.Col = cIDX_COLUMN_IN_MODEL) Or
    (sgExchangeItem.Col = cIDX_COLUMN_IN_EXCHANGE_ITEM) Then
    sgExchangeItem.EditorMode := False;
End;

Procedure TfmMain.sgExchangeItemDblClick(Sender: TObject);
Begin
  If sgExchangeItem.Row = 0 Then
    Exit;

  If sgExchangeItem.Col = cIDX_COLUMN_DESCRIBE Then
  Begin
    If ShowMemoHighlight(Project.FExchangeItemDescribeHighlight,
      Project.FExchangeItemList[sgExchangeItem.Row - 1].FDescribe) Then
      sgExchangeItem.Cells[cIDX_COLUMN_DESCRIBE, sgExchangeItem.Row] := Project.FExchangeItemList
        [sgExchangeItem.Row - 1].FDescribe;
  End
  Else If sgExchangeItem.Col = cIDX_COLUMN_REPLACE Then
  Begin
    If ShowMemoHighlight(Project.FExchangeItemReplaceHighlight,
      Project.FExchangeItemList[sgExchangeItem.Row - 1].FReplace) Then
      sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, sgExchangeItem.Row] := Project.FExchangeItemList
        [sgExchangeItem.Row - 1].FReplace;
  End
  Else If sgExchangeItem.Col = cIDX_COLUMN_BY Then
  Begin
    If ShowMemoHighlight(Project.FExchangeItemByHighlight,
      Project.FExchangeItemList[sgExchangeItem.Row - 1].FBy) Then
      sgExchangeItem.Cells[cIDX_COLUMN_BY, sgExchangeItem.Row] := Project.FExchangeItemList
        [sgExchangeItem.Row - 1].FBy;
  End
  Else If sgExchangeItem.Col = cIDX_COLUMN_IN_MODEL Then
  Begin
    Project.FExchangeItemList[sgExchangeItem.Row - 1].FInModel := Not Project.FExchangeItemList
      [sgExchangeItem.Row - 1].FInModel;
    sgExchangeItem.Invalidate;
  End
  Else If sgExchangeItem.Col = cIDX_COLUMN_IN_EXCHANGE_ITEM Then
  Begin
    Project.FExchangeItemList[sgExchangeItem.Row - 1].FInExchangeItem := Not Project.FExchangeItemList
      [sgExchangeItem.Row - 1].FInExchangeItem;
    sgExchangeItem.Invalidate;
  End
End;

Procedure TfmMain.sgExchangeItemDefault;
Begin
  sgExchangeItem.RowCount                                := 2; // min 2 else first row can be edit

  sgExchangeItem.Cells[cIDX_COLUMN_ID, 0]                := '#';
  sgExchangeItem.Cells[cIDX_COLUMN_DESCRIBE, 0]          := 'Describe';
  sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, 0]           := 'Replace';
  sgExchangeItem.Cells[cIDX_COLUMN_BY, 0]                := 'By';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_MODEL, 0]          := 'In model';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_EXCHANGE_ITEM, 0]  := 'In exchange item';
  sgExchangeItem.Cells[cIDX_COLUMN_ID, 1]                := '';
  sgExchangeItem.Cells[cIDX_COLUMN_DESCRIBE, 4]          := '';
  sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, 1]           := '';
  sgExchangeItem.Cells[cIDX_COLUMN_BY, 1]                := '';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_MODEL, 1]          := '';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_EXCHANGE_ITEM, 1]  := '';

  sgExchangeItem.ColWidths[cIDX_COLUMN_ID]               := Project.FColWidths[cIDX_COLUMN_ID];
  sgExchangeItem.ColWidths[cIDX_COLUMN_DESCRIBE]         := Project.FColWidths[cIDX_COLUMN_DESCRIBE];
  sgExchangeItem.ColWidths[cIDX_COLUMN_REPLACE]          := Project.FColWidths[cIDX_COLUMN_REPLACE];
  sgExchangeItem.ColWidths[cIDX_COLUMN_BY]               := Project.FColWidths[cIDX_COLUMN_BY];
  sgExchangeItem.ColWidths[cIDX_COLUMN_IN_MODEL]         := Project.FColWidths[cIDX_COLUMN_IN_MODEL];
  sgExchangeItem.ColWidths[cIDX_COLUMN_IN_EXCHANGE_ITEM] :=
    Project.FColWidths[cIDX_COLUMN_IN_EXCHANGE_ITEM];
End;

Procedure TfmMain.sgExchangeItemDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Begin
  With (Sender As TStringGrid), Canvas Do
  Begin
    If StyleServices.Enabled Then
      Brush.Color := StyleServices.GetSystemColor(clWindow)
    Else
      Brush.Color := Color;

    SetBKMode(Handle, TRANSPARENT);
    FillRect(Rect);
    InflateRect(Rect, -2, -2);
    If (gdFocused In State) Then
      DrawFocusRect(Rect);

    If (ARow = 0) Or (ACol = 0) Then
    Begin
      Font.Style := [fsBold];
      DrawText(Handle, PChar(Cells[ACol, ARow]), -1, Rect, DT_CENTER Or DT_WORDBREAK);
    End
    Else
    Begin
      Font.Style := [];
      If ((ACol = cIDX_COLUMN_DESCRIBE) Or (ACol = cIDX_COLUMN_REPLACE) Or (ACol = cIDX_COLUMN_BY))
        And (Pos(sLineBreak, sgExchangeItem.Cells[ACol, ARow]) > 0) Then
        DrawText(Handle, PChar(Cells[ACol, ARow]), -1, Rect, DT_NOPREFIX Or DT_WORDBREAK)
      Else If ACol = cIDX_COLUMN_IN_MODEL Then
        CheckBoxGrid(Project.FExchangeItemList[ARow - 1].FInModel, sgExchangeItem, Rect, State)
      Else If ACol = cIDX_COLUMN_IN_EXCHANGE_ITEM Then
        CheckBoxGrid(Project.FExchangeItemList[ARow - 1].FInExchangeItem, sgExchangeItem, Rect, State)
      Else
        DrawText(Handle, PChar(Cells[ACol, ARow]), -1, Rect, DT_NOPREFIX);
    End;
  End;
End;

// https://stackoverflow.com/questions/42835758/delphi-place-a-checkbox-inside-a-dbgrid-themed
Procedure TfmMain.CheckBoxGrid(Checked: Boolean; StringGrid: TStringGrid;
  Rect: TRect; State: TGridDrawState);
Const
  CtrlState: Array [Boolean] Of Integer         = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK Or DFCS_CHECKED);
  CtrlStateXP: Array [Boolean] Of TThemedButton = (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal);
Var
  Details: TThemedElementDetails;
Begin
  If StyleServices.Enabled Then
  Begin
    Details := StyleServices.GetElementDetails(CtrlStateXP[Checked]);
    StyleServices.DrawElement(StringGrid.Canvas.Handle, Details, Rect);
  End
  Else
  Begin
    DrawFrameControl(StringGrid.Canvas.Handle, Rect, DFC_BUTTON, CtrlState[Checked]);
  End;
End;

Function TfmMain.CreateGUID: String;
Var
  GuidDialog: TGUID;
Begin
  If System.SysUtils.CreateGUID(GuidDialog) = 0 Then
    Result := GUIDToString(GuidDialog)
  Else
    Result := '';
End;

Procedure TfmMain.sgExchangeItemMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  // if grid is resizing
  If (Button = mbLeft) Then
  Begin
    // check size are ok and keep in project
    If (TStringGrid(Sender).GetGridState = gsColSizing) Then
    Begin
      For Var I := 1 To sgExchangeItem.ColCount - 1 Do
      Begin
        If sgExchangeItem.ColWidths[I] < cCOL_WIDTHS_MIN Then
          sgExchangeItem.ColWidths[I] := cCOL_WIDTHS_MIN;
      End;

      // update project with new widths and heights
      Project.FColWidths[cIDX_COLUMN_ID]       := sgExchangeItem.ColWidths[cIDX_COLUMN_ID];
      Project.FColWidths[cIDX_COLUMN_DESCRIBE] := sgExchangeItem.ColWidths[cIDX_COLUMN_DESCRIBE];
      Project.FColWidths[cIDX_COLUMN_REPLACE]  := sgExchangeItem.ColWidths[cIDX_COLUMN_REPLACE];
      Project.FColWidths[cIDX_COLUMN_BY]       := sgExchangeItem.ColWidths[cIDX_COLUMN_BY];
      Project.FColWidths[cIDX_COLUMN_IN_MODEL] := sgExchangeItem.ColWidths[cIDX_COLUMN_IN_MODEL];
      Project.FColWidths[cIDX_COLUMN_IN_EXCHANGE_ITEM] := sgExchangeItem.ColWidths
        [cIDX_COLUMN_IN_EXCHANGE_ITEM];
    End
    Else If (TStringGrid(Sender).GetGridState = gsRowSizing) Then
    Begin
      For Var I := 1 To sgExchangeItem.RowCount - 1 Do
      Begin
        If sgExchangeItem.RowHeights[I] < cROW_HEIGHT_MIN Then
          sgExchangeItem.RowHeights[I]              := cROW_HEIGHT_MIN;

        Project.FExchangeItemList[I - 1].FRowHeight := sgExchangeItem.RowHeights[I];
      End;
    End;
  End;
End;

Procedure TfmMain.sgExchangeItemRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
Begin
  // when user move row we update in project
  Project.FExchangeItemList.Move(FromIndex - 1, ToIndex - 1);
  // and correct id
  sgExchangeItemSetId;
End;

Procedure TfmMain.sgExchangeItemSelectCell(Sender: TObject; ACol, ARow: Integer;
  Var CanSelect: Boolean);
Begin
  // allow delete if selection are on a exchange item
  miExchangeItemDelete.Enabled := (sgExchangeItem.RowCount > 2) And (ARow > 0) And (ACol > 0);
End;

Procedure TfmMain.sgExchangeItemSetEditText(Sender: TObject; ACol,
  ARow: Integer; Const Value: String);
Begin
  // row 0 is title collumn
  If ARow = 0 Then
    Exit;

  // if edit a exchange item we update it in project
  Case ACol Of
    cIDX_COLUMN_DESCRIBE:
      Project.FExchangeItemList[ARow - 1].FDescribe := Value;
    cIDX_COLUMN_REPLACE:
      Project.FExchangeItemList[ARow - 1].FReplace  := Value;
    cIDX_COLUMN_BY:
      Project.FExchangeItemList[ARow - 1].FBy       := Value;
  End;
End;

Procedure TfmMain.sgExchangeItemSetId;
Begin
  With sgExchangeItem Do
  Begin
    If RowCount < 2 Then
      Exit;

    For Var I                  := 1 To RowCount - 1 Do
      Cells[cIDX_COLUMN_ID, I] := IntToStr(I);
  End;
End;

Procedure TfmMain.sgExchangeItemSetList;
Var
  IdxRow: Integer;
Begin
  sgExchangeItemDefault;

  With Project.FExchangeItemList Do
  Begin
    If Count = 0 Then
      Exit;

    sgExchangeItem.RowCount := Count + 1;
    IdxRow                  := 0;
    For Var I               := 0 To Count - 1 Do
    Begin
      Inc(IdxRow);
      sgExchangeItem.Cells[cIDX_COLUMN_ID, IdxRow]       := IntToStr(IdxRow);
      sgExchangeItem.Cells[cIDX_COLUMN_DESCRIBE, IdxRow] := Items[I].FDescribe;
      sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, IdxRow]  := Items[I].FReplace;
      sgExchangeItem.Cells[cIDX_COLUMN_BY, IdxRow]       := Items[I].FBy;
      sgExchangeItem.RowHeights[IdxRow]                  := Items[I].FRowHeight;
    End;
  End;

  miExchangeItemDelete.Enabled := False; // by default disable delete, user must select a row
End;

Procedure TfmMain.FormShow(Sender: TObject);
Var
  CommandLineProject: String;
Begin
  LoadSettings;

  SetProjectHistory;

  CommandLineProject := CmdLine.ProjectInParam;
  If CommandLineProject <> '' Then
    LoadProject(CommandLineProject)      // load project from command line
  Else If fmSettings.LastProjectAutoOpen And (ProjectPathHistory.Count > 0) Then
    LoadProject(ProjectPathHistory.Last) // load last project
  Else
    LoadProject;                         // load default value
End;

Procedure TfmMain.LoadProject(AProjectFileName: String);
Begin
  // if want load a file
  If (AProjectFileName <> '') Then
  Begin
    If Not FileExists(AProjectFileName) Then
    Begin
      MessageDlg(Format('ERROR: project file %s does not exist',
        [ExtractFileName(AProjectFileName)]), mtError, [mbOK], 0);
      Exit;
    End;

    // try load file
    Try
      Project.LoadFromFile(AProjectFileName);

      AddProjectInHistory(AProjectFileName);
    Except
      On E: Exception Do
      Begin
        MessageDlg('Error occured cannot load project:' + sLineBreak +
          E.Message, mtError, [mbOK], 0);
        Exit;
      End;
    End;
  End;

  // show project value
  SetSynMemoHighlighter(smModel, Project.FModelHighlight);
  smModel.Text := Project.FModel;

  sgExchangeItemSetList;

  SetPathAndHistory;

  SetActualProjectPath(AProjectFileName);
End;

Procedure TfmMain.LoadSettings;
Var
  MemIniFile: TMemIniFile;
Begin
  MemIniFile := TMemIniFile.Create(ConfigPath);
  Try
    With MemIniFile Do
    Begin
      Width              := ReadInteger(Name, 'Width', Width);
      Height             := ReadInteger(Name, 'Height', Height);

      Left               := ReadInteger(Name, 'Left', Left);
      Top                := ReadInteger(Name, 'Top', Top);

      WindowState        := TWindowState(ReadInteger(Name, 'WindowState', Integer(WindowState)));

      pcProject.TabIndex := ReadInteger(Name, 'pcProject.TabIndex', pcProject.TabIndex);

      FileSaveDialogProject.ClientGuid := ReadString(Name, 'FileSaveDialogProject.ClientGuid', '');

      FileSaveDialogPath.ClientGuid := ReadString(Name, 'FileSaveDialogPath.ClientGuid', '');

      ProjectPathHistory.LoadFromIni(MemIniFile);

      fmSettings.LoadFromIni(MemIniFile);
    End;
  Finally
    MemIniFile.Free;
  End;

  FileSaveDialogProject.DefaultExtension      := fmSettings.ProjectExt;
  FileSaveDialogProject.FileTypes[0].FileMask := '*.' + fmSettings.ProjectExt;
  // ensure we have a GUID
  If FileSaveDialogProject.ClientGuid = '' Then
    FileSaveDialogProject.ClientGuid          := CreateGUID;

  FileOpenDialogProject.DefaultExtension      := fmSettings.ProjectExt;
  FileOpenDialogProject.FileTypes[0].FileMask := '*.' + fmSettings.ProjectExt;
  FileOpenDialogProject.ClientGuid            := FileSaveDialogProject.ClientGuid;

  // ensure we have a GUID
  If FileSaveDialogPath.ClientGuid = '' Then
    FileSaveDialogPath.ClientGuid := CreateGUID;
End;

Procedure TfmMain.miExchangeItemAddClick(Sender: TObject);
Begin
  Project.FExchangeItemList.Add(TExchangeItem.Create); // add a item
  sgExchangeItemSetList;                               // recreate list
End;

Procedure TfmMain.miExchangeItemDeleteClick(Sender: TObject);
Begin
  If Project.FExchangeItemList.Count = 1 Then
  Begin
    MessageDlg('You cannot empty list', mtError, [mbOK], 0);
    Exit;
  End;

  If MessageDlg(Format('Are you sure you want to delete item number %d?',
    [sgExchangeItem.Row]), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes Then
  Begin
    Project.FExchangeItemList.Delete(sgExchangeItem.Row - 1); // Delete a item
    sgExchangeItemSetList;                                    // recreate list
  End;
End;

Procedure TfmMain.miNewProjectClick(Sender: TObject);
Begin
  Project.Default; // Reset value
  LoadProject;     // show value
End;

Procedure TfmMain.miOpenProjectClick(Sender: TObject);
Begin
  If FileOpenDialogProject.Execute Then
    LoadProject(FileOpenDialogProject.FileName);
End;

Procedure TfmMain.miSaveProjectAsClick(Sender: TObject);
Begin
  SaveProjectAs;
End;

Procedure TfmMain.miSaveProjectClick(Sender: TObject);
Begin
  SaveProject;
End;

Procedure TfmMain.SaveProject;
Begin
  // need check if focused need call event onexit by set focus on others component
  If bePath.Focused Then
    pcProject.SetFocus;

  If ActualProjectPath <> '' Then
    Project.SaveToFile(ActualProjectPath);
End;

Procedure TfmMain.SaveProjectAs;
Begin
  // need check if focused need call event onexit by set focus on others component
  If bePath.Focused Then
    pcProject.SetFocus;

  If Not FileSaveDialogProject.Execute Then
    Exit;

  If FileExists(FileSaveDialogProject.FileName) Then
  Begin
    If MessageDlg(Format('The file %s already exist, overwrite?',
      [ExtractFileName(FileSaveDialogProject.FileName)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      If Not DeleteFile(FileSaveDialogProject.FileName) Then
      Begin
        MessageDlg('ERROR: Cannot delete file', mtError, [mbOK], 0);
        Exit;
      End;
    End
    Else
      Exit;
  End;

  If Project.SaveToFile(FileSaveDialogProject.FileName) Then
  Begin
    AddProjectInHistory(FileSaveDialogProject.FileName);

    SetActualProjectPath(FileSaveDialogProject.FileName);
  End;
End;

Procedure TfmMain.miSettingsGeneralClick(Sender: TObject);
Begin
  fmSettings.pcSettings.ActivePage := fmSettings.tsSettingsGeneral;
  ShowSettings;
End;

Procedure TfmMain.miSettingsProjectClick(Sender: TObject);
Begin
  fmSettings.pcSettings.ActivePage := fmSettings.tsSettingsProject;
  ShowSettings;
End;

Function TfmMain.ShowMemoHighlight(AHighlighter: TProjectSynMemoHighlighter;
  Var AValue: String): Boolean;
Begin
  With TfmMemoHighlight.Create(Self, ConfigPath) Do
    Try
      SetSynMemoHighlighter(smMemo, AHighlighter);
      smMemo.Text := AValue;
      If ShowModal = mrOk Then
      Begin
        AValue := smMemo.Text;
        Exit(True);
      End
      Else
        Exit(False);
    Finally
      Free;
    End;
End;

Procedure TfmMain.ShowSettings;
Begin
  fmSettings.ShowModal;
  If fmSettings.NeedRefresh Then
  Begin
    SetSynMemoHighlighter(smModel, Project.FModelHighlight);
    SetProjectHistory;
    SetPathAndHistory;
  End;
End;

Procedure TfmMain.smModelChange(Sender: TObject);
Begin
  Project.FModel := smModel.Text;
End;

Procedure TfmMain.PathHistoryClick(Sender: TObject);
Begin
  SetPathAndHistory(Project.FPathHistory[(Sender As TMenuItem).Tag]);
End;

Procedure TfmMain.ProjectPathHistoryClick(Sender: TObject);
Begin
  LoadProject(ProjectPathHistory[(Sender As TMenuItem).Tag]);
End;

Procedure TfmMain.SaveSettings;
Var
  MemIniFile: TMemIniFile;
Begin
  MemIniFile := TMemIniFile.Create(ConfigPath);
  Try
    With MemIniFile Do
    Begin
      EraseSection(Name);

      If WindowState = TWindowState.WsNormal Then
      Begin
        WriteInteger(Name, 'Width', Width);
        WriteInteger(Name, 'Height', Height);

        WriteInteger(Name, 'Left', Left);
        WriteInteger(Name, 'Top', Top);
      End;

      WriteInteger(Name, 'WindowState', Integer(WindowState));

      WriteInteger(Name, 'pcProject.TabIndex', pcProject.TabIndex);

      WriteString(Name, 'FileSaveDialogProject.ClientGuid', FileSaveDialogProject.ClientGuid);

      WriteString(Name, 'FileSaveDialogPath.ClientGuid', FileSaveDialogPath.ClientGuid);

      ProjectPathHistory.SaveToIni(MemIniFile);

      fmSettings.SaveToIni(MemIniFile);

      UpdateFile;
    End;
  Finally
    MemIniFile.Free;
  End;
End;

Procedure TfmMain.sbCreateFileClick(Sender: TObject);
Begin
  // need check if focused need call event onexit by set focus on others component
  If bePath.Focused Then
    pcProject.SetFocus;

  If bePath.Text = '' Then
  Begin
    MessageDlg('ERROR: You must specify a filename', mtError, [mbOK], 0);
    bePath.SetFocus;
    Exit;
  End;

  If FileExists(bePath.Text) Then
  Begin
    If MessageDlg(Format('The file %s already exist, overwrite?', [ExtractFileName(bePath.Text)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      If Not DeleteFile(bePath.Text) Then
      Begin
        MessageDlg('ERROR: Cannot delete file', mtError, [mbOK], 0);
        bePath.SetFocus;
        Exit;
      End;
    End
    Else
      Exit;
  End;

  With TStringList.Create Do
    Try
      Text := Project.GenerateText;
      SaveToFile(Project.FileName);
    Finally
      Free;
    End;
End;

Procedure TfmMain.sbPreviewClick(Sender: TObject);
Begin
  // need check if focused need call event onexit by set focus on others component
  If bePath.Focused Then
    pcProject.SetFocus;

  If Not assigned(fmPreview) Then
    fmPreview := TfmPreview.Create(Self, ConfigPath);

  fmPreview.Show;
End;

Procedure TfmMain.sbProjectsClick(Sender: TObject);
Var
  pt: TPoint;
Begin
  pt.X := 0;
  pt.Y := -Round(21 * pmProjectsMenu.Items.Count);
  pt   := sbProjects.ClientToScreen(pt);
  pmProjectsMenu.Popup(pt.X, pt.Y);
End;

Procedure TfmMain.SetActualProjectPath(AProjectFileName: String);
Begin
  ActualProjectPath := AProjectFileName;

  If AProjectFileName <> '' Then
  Begin
    Caption               := Format(cFORM_TITLE, [ExtractFileName(AProjectFileName), AProjectFileName]);
    miSaveProject.Enabled := True;
  End
  Else
  Begin
    Caption               := cPRODUCT_NAME;
    miSaveProject.Enabled := False;
  End;
End;

Procedure TfmMain.SetPathAndHistory(APathFileName: String);
Var
  mi: TMenuItem;
Begin
  With Project, pmPathHistory Do
  Begin
    FileName := APathFileName;

    Items.Clear;

    If FPathHistory.Count = 0 Then
    Begin
      mi         := TMenuItem.Create(miProjectHistory);
      mi.Caption := 'Empty';
      mi.Enabled := False;
      pmPathHistory.Items.Add(mi);

      bePath.Text := '';
    End
    Else
    Begin
      For Var I    := 0 To FPathHistory.Count - 1 Do
      Begin
        mi         := TMenuItem.Create(pmPathHistory);
        mi.Caption := FPathHistory[I];
        mi.Tag     := I;
        mi.OnClick := PathHistoryClick;
        pmPathHistory.Items.Add(mi);
      End;

      bePath.Text := FPathHistory.Last;
    End;
  End;
End;

Procedure TfmMain.SetProjectHistory;
Var
  mi: TMenuItem;
Begin
  miProjectHistory.Clear;

  If ProjectPathHistory.Count = 0 Then
  Begin
    mi         := TMenuItem.Create(miProjectHistory);
    mi.Caption := 'Empty';
    mi.Enabled := False;
    miProjectHistory.Add(mi);

    SetActualProjectPath;
  End
  Else
  Begin
    For Var I    := 0 To ProjectPathHistory.Count - 1 Do
    Begin
      mi         := TMenuItem.Create(miProjectHistory);
      mi.Caption := ProjectPathHistory[I];
      mi.Tag     := I;
      mi.OnClick := ProjectPathHistoryClick;
      miProjectHistory.Add(mi);
    End;
  End;
End;

End.
