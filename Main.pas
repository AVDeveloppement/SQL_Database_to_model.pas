Unit Main;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Themes,
  Vcl.Dialogs, Vcl.StdCtrls, System.Types, System.UITypes, System.IniFiles,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus, Vcl.Buttons,
  Vcl.Grids, System.Actions, Vcl.ActnList, Vcl.ComCtrls,
  Settings, Projects, Preview, CommandLine;

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
    mModel: TMemo;
    sbPreview: TSpeedButton;
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
    Procedure mModelChange(Sender: TObject);
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
  Private
    ConfigPath, ActualProjectPath: String;
    Procedure AddProjectInHistory(AProjectFileName: String);
    Procedure CheckBoxGrid(Checked: Boolean; StringGrid: TStringGrid;
      Rect: TRect; State: TGridDrawState);
    Procedure LoadSettings;
    Procedure LoadProject(AProjectFileName: String); Overload;
    Procedure LoadProject; Overload;
    Procedure SaveSettings;
    Procedure sgExchangeItemDefault;
    Procedure sgExchangeItemSetList;
    Procedure SetPathAndHistory(APathFileName: String = '');
    Procedure PathHistoryClick(Sender: TObject);
    Procedure ProjectPathHistoryClick(Sender: TObject);
    Procedure SetProjectHistory;
    Procedure SaveProject;
    Procedure SaveProjectAs;
    Procedure ShowSettings;
    Procedure sgExchangeItemSetId;
  Public
  End;

Var
  fmMain: TfmMain;

Implementation

{$R *.dfm}
{ TCustomGridHelper }

Function TCustomGridHelper.GetGridState: TGridState;
Begin
  Result := Self.FGridState;
End;

{ TfmMain }

Procedure TfmMain.AddProjectInHistory(AProjectFileName: String);
Begin
  ProjectPathHistory.Add(AProjectFileName);

  While ProjectPathHistory.Count > fmSettings.ProjectPathHistoryMax Do
    ProjectPathHistory.Delete(0);

  SetProjectHistory;

  ActualProjectPath := AProjectFileName;
  Caption           := Format(cFORM_TITLE, [ExtractFileName(AProjectFileName), AProjectFileName]);
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
  SaveSettings;

  If fmSettings.ActualProjectAutoSave Then
    SaveProject;

  Project.Free;
  ProjectPathHistory.Free;
End;

Procedure TfmMain.FormCreate(Sender: TObject);
Begin
  Project            := TProject.Create;
  ProjectPathHistory := TProjectPathHistory.Create;

  ConfigPath         := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'configuration.ini';
  pcProject.TabIndex := 0;
  ActualProjectPath  := '';
End;

Procedure TfmMain.sgExchangeItemClick(Sender: TObject);
Begin
  If sgExchangeItem.Col = cIDX_COLUMN_IN_MODEL Then
    sgExchangeItem.EditorMode := false;
End;

Procedure TfmMain.sgExchangeItemDblClick(Sender: TObject);
Begin
  If (sgExchangeItem.Row > 0) And (sgExchangeItem.Col = cIDX_COLUMN_IN_MODEL) Then
  Begin
    Project.FExchangeItemList[sgExchangeItem.Row - 1].FInModel := Not Project.FExchangeItemList
      [sgExchangeItem.Row - 1].FInModel;
    sgExchangeItem.Invalidate;
  End;
End;

Procedure TfmMain.sgExchangeItemDefault;
Begin
  sgExchangeItem.RowCount                        := 2; // min 2 else first row can be edit

  sgExchangeItem.Cells[cIDX_COLUMN_ID, 0]        := '#';
  sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, 0]   := 'Replace';
  sgExchangeItem.Cells[cIDX_COLUMN_BY, 0]        := 'By';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_MODEL, 0]  := 'In model';
  sgExchangeItem.Cells[cIDX_COLUMN_ID, 1]        := '';
  sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, 1]   := '';
  sgExchangeItem.Cells[cIDX_COLUMN_BY, 1]        := '';
  sgExchangeItem.Cells[cIDX_COLUMN_IN_MODEL, 1]  := '';

  sgExchangeItem.ColWidths[cIDX_COLUMN_ID]       := Project.FColWidths[cIDX_COLUMN_ID];
  sgExchangeItem.ColWidths[cIDX_COLUMN_REPLACE]  := Project.FColWidths[cIDX_COLUMN_REPLACE];
  sgExchangeItem.ColWidths[cIDX_COLUMN_BY]       := Project.FColWidths[cIDX_COLUMN_BY];
  sgExchangeItem.ColWidths[cIDX_COLUMN_IN_MODEL] := Project.FColWidths[cIDX_COLUMN_IN_MODEL];
End;

Procedure TfmMain.sgExchangeItemDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Begin
  If (ARow > 0) And (ACol = cIDX_COLUMN_IN_MODEL) Then
    CheckBoxGrid(Project.FExchangeItemList[ARow - 1].FInModel, sgExchangeItem, Rect, State);
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
  StringGrid.Canvas.FillRect(Rect);

  If StyleServices.Enabled Then
  Begin
    Details := StyleServices.GetElementDetails(CtrlStateXP[Checked]);
    StyleServices.DrawElement(StringGrid.Canvas.Handle, Details, Rect);

    If Not(gdFocused In State) Then
      StringGrid.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
  End
  Else
  Begin
    InflateRect(Rect, -2, -2);
    DrawFrameControl(StringGrid.Canvas.Handle, Rect, DFC_BUTTON, CtrlState[Checked]);
  End;
End;

Procedure TfmMain.sgExchangeItemMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  // if grid is resizing
  If (Button = mbLeft) Then
  Begin
    If (TStringGrid(Sender).GetGridState = gsColSizing) Then
    Begin
      For Var I := 1 To sgExchangeItem.ColCount - 1 Do
      Begin
        If sgExchangeItem.ColWidths[I] < 30 Then
          sgExchangeItem.ColWidths[I] := 30;
      End;

      // update project with new widths and heights
      Project.FColWidths[cIDX_COLUMN_ID]       := sgExchangeItem.ColWidths[cIDX_COLUMN_ID];
      Project.FColWidths[cIDX_COLUMN_REPLACE]  := sgExchangeItem.ColWidths[cIDX_COLUMN_REPLACE];
      Project.FColWidths[cIDX_COLUMN_BY]       := sgExchangeItem.ColWidths[cIDX_COLUMN_BY];
      Project.FColWidths[cIDX_COLUMN_IN_MODEL] := sgExchangeItem.ColWidths[cIDX_COLUMN_IN_MODEL];
    End
    Else If (TStringGrid(Sender).GetGridState = gsRowSizing) Then
    Begin
      For Var I := 1 To sgExchangeItem.RowCount - 1 Do
      Begin
        If sgExchangeItem.RowHeights[I] < 30 Then
          sgExchangeItem.RowHeights[I]              := 30;

        Project.FExchangeItemList[I - 1].FRowHeight := sgExchangeItem.RowHeights[I];
      End;
    End;
  End;
End;

Procedure TfmMain.sgExchangeItemRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
Begin
  Project.FExchangeItemList.Move(FromIndex - 1, ToIndex - 1);
  sgExchangeItemSetId;
End;

Procedure TfmMain.sgExchangeItemSelectCell(Sender: TObject; ACol, ARow: Integer;
  Var CanSelect: Boolean);
Begin
  // sgExchangeItem.Cells[ACol, ARow] := IntToStr(sgExchangeItem.RowHeights[ARow]) + ' ' +
  // IntToStr(sgExchangeItem.ColWidths[ACol]);

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
    cIDX_COLUMN_REPLACE:
      Project.FExchangeItemList[ARow - 1].FReplace := Value;
    cIDX_COLUMN_BY:
      Project.FExchangeItemList[ARow - 1].FBy      := Value;
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
      sgExchangeItem.Cells[cIDX_COLUMN_ID, IdxRow]      := IntToStr(IdxRow);
      sgExchangeItem.Cells[cIDX_COLUMN_REPLACE, IdxRow] := Items[I].FReplace;
      sgExchangeItem.Cells[cIDX_COLUMN_BY, IdxRow]      := Items[I].FBy;
      sgExchangeItem.RowHeights[IdxRow]                 := Items[I].FRowHeight;
    End;
  End;

  miExchangeItemDelete.Enabled := false; // by default disable delete, user must select a row
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
  Project.LoadFromFile(AProjectFileName);

  LoadProject;

  AddProjectInHistory(AProjectFileName);
End;

Procedure TfmMain.LoadProject;
Begin
  mModel.Text := Project.FModel;

  sgExchangeItemSetList;

  SetPathAndHistory;
End;

Procedure TfmMain.LoadSettings;
Var
  MemIniFile: TMemIniFile;
Begin
  MemIniFile := TMemIniFile.Create(ConfigPath);
  Try
    With MemIniFile Do
    Begin
      Width              := ReadInteger('fmMain', 'Width', Width);
      Height             := ReadInteger('fmMain', 'Height', Height);

      Left               := ReadInteger('fmMain', 'Left', Left);
      Top                := ReadInteger('fmMain', 'Top', Top);

      WindowState        := TWindowState(ReadInteger('fmMain', 'WindowState', Integer(WindowState)));

      pcProject.TabIndex := ReadInteger('pcProject', 'TabIndex', pcProject.TabIndex);

      ProjectPathHistory.LoadFromIni(MemIniFile);

      fmSettings.LoadFromIni(MemIniFile);

      FileSaveDialogProject.DefaultExtension      := fmSettings.ProjectExt;
      FileSaveDialogProject.FileTypes[0].FileMask := '*.' + fmSettings.ProjectExt;

      FileOpenDialogProject.DefaultExtension      := fmSettings.ProjectExt;
      FileOpenDialogProject.FileTypes[0].FileMask := '*.' + fmSettings.ProjectExt;
    End;
  Finally
    MemIniFile.Free;
  End;
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
  If ActualProjectPath <> '' Then
    Project.SaveToFile(ActualProjectPath);
End;

Procedure TfmMain.SaveProjectAs;
Begin
  If Not FileSaveDialogProject.Execute Then
    Exit;

  If FileExists(FileSaveDialogProject.FileName) Then
  Begin
    If MessageDlg(Format('The file %s already exist, overwrite?', [ExtractFileName(FileSaveDialogProject.FileName)]),
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

  Project.SaveToFile(FileSaveDialogProject.FileName);

  AddProjectInHistory(FileSaveDialogProject.FileName);
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

Procedure TfmMain.ShowSettings;
Begin
  fmSettings.ShowModal;
  If fmSettings.NeedRefresh Then
  Begin
    SetProjectHistory;
    SetPathAndHistory;
  End;
End;

Procedure TfmMain.mModelChange(Sender: TObject);
Begin
  Project.FModel := mModel.Text;
End;

Procedure TfmMain.PathHistoryClick(Sender: TObject);
Begin
  SetPathAndHistory(Project.FPathHistory[(Sender As TMenuItem).Tag]);
End;

Procedure TfmMain.ProjectPathHistoryClick(Sender: TObject);
Var
  ProjectFileName: String;
Begin
  ProjectFileName := ProjectPathHistory[(Sender As TMenuItem).Tag];
  ProjectPathHistory.Delete((Sender As TMenuItem).Tag);
  LoadProject(ProjectFileName);
End;

Procedure TfmMain.SaveSettings;
Var
  MemIniFile: TMemIniFile;
Begin
  MemIniFile := TMemIniFile.Create(ConfigPath);
  Try
    With MemIniFile Do
    Begin
      EraseSection('fmMain');

      If WindowState = TWindowState.WsNormal Then
      Begin
        WriteInteger('fmMain', 'Width', Width);
        WriteInteger('fmMain', 'Height', Height);

        WriteInteger('fmMain', 'Left', Left);
        WriteInteger('fmMain', 'Top', Top);
      End;

      WriteInteger('fmMain', 'WindowState', Integer(WindowState));

      WriteInteger('fmMain', 'pcProject.TabIndex', pcProject.TabIndex);

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

  If FileExists(bePath.Text) Then
  Begin
    If MessageDlg(Format('The file %s already exist, overwrite?', [ExtractFileName(bePath.Text)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      If Not DeleteFile(bePath.Text) Then
      Begin
        MessageDlg('ERROR: Cannot delete file', mtError, [mbOK], 0);
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

Procedure TfmMain.SetPathAndHistory(APathFileName: String);
Var
  mi: TMenuItem;
Begin

  With Project, pmPathHistory Do
  Begin
    FileName := APathFileName;

    While FPathHistory.Count > FPathHistoryMax Do
      FPathHistory.Delete(0);

    Items.Clear;

    If FPathHistory.Count = 0 Then
    Begin
      mi         := TMenuItem.Create(miProjectHistory);
      mi.Caption := 'Empty';
      mi.Enabled := false;
      pmPathHistory.Items.Add(mi);
      Exit;
    End;

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

Procedure TfmMain.SetProjectHistory;
Var
  mi: TMenuItem;
Begin
  With miProjectHistory Do
  Begin
    Clear;

    If ProjectPathHistory.Count = 0 Then
    Begin
      mi         := TMenuItem.Create(miProjectHistory);
      mi.Caption := 'Empty';
      mi.Enabled := false;
      miProjectHistory.Add(mi);

      miSaveProject.Enabled := false;

      Exit;
    End;

    For Var I    := 0 To ProjectPathHistory.Count - 1 Do
    Begin
      mi         := TMenuItem.Create(miProjectHistory);
      mi.Caption := ProjectPathHistory[I];
      mi.Tag     := I;
      mi.OnClick := ProjectPathHistoryClick;
      miProjectHistory.Add(mi);
    End;

    miSaveProject.Enabled := True;
  End;
End;

End.
