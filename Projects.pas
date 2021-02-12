Unit Projects;

Interface

Uses System.Classes, System.Generics.Collections, System.JSON, System.SysUtils,
  System.IniFiles, System.JSON.Types, System.JSON.Builders, System.JSON.Writers,
  System.JSON.Readers;

Type
  TExchangeItem = Class
    FRowHeight: Integer;
    FDescribe, FReplace, FBy: String;
    FInModel, FInExchangeItem: Boolean;
    Procedure Assign(AExchangeItem: TExchangeItem);
    Constructor Create;
  End;

  TExchangeItemList = Class(TObjectList<TExchangeItem>)
    Procedure Assign(AExchangeItemList: TExchangeItemList);
  End;

  TProjectSynMemoHighlighter = (psmhNone, psmhCNET, psmhC, psmhHTML, psmhINI, psmhInnoSetupScript,
    psmhJava, psmhJavaScript, psmhObjectPascal, psmhPerl, psmhPHP, psmhSQL, psmhUNIXShellScript);

  TProject = Class
    FModel: String;
    FColWidths: TList<Integer>;
    FExchangeItemList: TExchangeItemList;
    FPathHistory: TList<String>;
    FPathHistoryMax: Integer;
    FReplaceInFilename: Boolean;
    FModelHighlight, FExchangeItemDescribeHighlight,
      FExchangeItemReplaceHighlight, FExchangeItemByHighlight: TProjectSynMemoHighlighter;
  Private
    Procedure CheckColsWidths;
    Function GenerateFileName: String;
    Function GetFileName: String;
    Function GetParsedExchangeItemList: TExchangeItemList;
    Procedure SetFileName(Const Value: String);
  Public
    Constructor Create;
    Procedure Default;
    Destructor Destroy; Override;
    Function GenerateText: String;
    Function LoadFromFile(AFileName: String): Boolean;
    Procedure LoadFromJson(AJson: String);
    Function SaveAsJson: String;
    Function SaveToFile(AFileName: String): Boolean;
    Property FileName: String Read GetFileName Write SetFileName;
  End;

  TProjectPathHistory = Class(TList<String>)
    Function Add(Value: String): Integer;
    Procedure LoadFromIni(AMemIniFile: TMemIniFile);
    Procedure SaveToIni(AMemIniFile: TMemIniFile);
  End;

Var
  Project: TProject;
  ProjectPathHistory: TProjectPathHistory;

Const
  cIDX_COLUMN_ID               = 0;
  cIDX_COLUMN_DESCRIBE         = 1;
  cIDX_COLUMN_REPLACE          = 2;
  cIDX_COLUMN_BY               = 3;
  cIDX_COLUMN_IN_MODEL         = 4;
  cIDX_COLUMN_IN_EXCHANGE_ITEM = 5;
  cNB_COL_WIDTHS               = 6;
  cCOL_WIDTHS_DEFAULT          = 100;
  cROW_HEIGHT_DEFAULT          = 30;

Implementation

{ TExchangeItem }

Procedure TExchangeItem.Assign(AExchangeItem: TExchangeItem);
Begin
  FDescribe       := AExchangeItem.FDescribe;
  FReplace        := AExchangeItem.FReplace;
  FBy             := AExchangeItem.FBy;

  FRowHeight      := AExchangeItem.FRowHeight;

  FInModel        := AExchangeItem.FInModel;
  FInExchangeItem := AExchangeItem.FInExchangeItem;
End;

Constructor TExchangeItem.Create;
Begin
  FDescribe       := '';
  FReplace        := '';
  FBy             := '';

  FRowHeight      := cROW_HEIGHT_DEFAULT;

  FInModel        := True;
  FInExchangeItem := False;
End;

{ TExchangeItemList }

Procedure TExchangeItemList.Assign(AExchangeItemList: TExchangeItemList);
Var
  ExchangeItem: TExchangeItem;
Begin
  Clear;
  For Var I      := 0 To AExchangeItemList.Count - 1 Do
  Begin
    ExchangeItem := TExchangeItem.Create;
    ExchangeItem.Assign(AExchangeItemList[I]);
    Add(ExchangeItem);
  End;
End;

{ TProject }

Procedure TProject.CheckColsWidths;
Begin
  While FColWidths.Count < cNB_COL_WIDTHS Do
    FColWidths.Add(cCOL_WIDTHS_DEFAULT);
End;

Constructor TProject.Create;
Begin
  FColWidths        := TList<Integer>.Create;
  FExchangeItemList := TExchangeItemList.Create;
  FPathHistory      := TList<String>.Create;

  Default;
End;

Procedure TProject.Default;
Begin
  FColWidths.Clear;
  FColWidths.Add(30);  // id
  FColWidths.Add(250); // describe
  FColWidths.Add(150); // replace
  FColWidths.Add(300); // by
  FColWidths.Add(100); // in model
  FColWidths.Add(100); // in exchange item

  FExchangeItemList.Clear;
  FExchangeItemList.Add(TExchangeItem.Create);
  With FExchangeItemList.Last Do
  Begin
    FReplace        := '''';
    FBy             := '''''';
    FInModel        := False;
    FInExchangeItem := True;
  End;
  FExchangeItemList.Add(TExchangeItem.Create);
  With FExchangeItemList.Last Do
  Begin
    FReplace   := '%SQL%';
    FRowHeight := 100;
  End;

  FPathHistory.Clear;
  FPathHistoryMax := 10;

  FModel          := 'Unit DBModel;' + sLineBreak + sLineBreak +
    'Interface' + sLineBreak + sLineBreak +
    'Const' + sLineBreak +
    '  cDB_MODEL = ''%SQL%'';' + sLineBreak + sLineBreak +
    'Implementation' + sLineBreak + sLineBreak +
    'End.';

  FReplaceInFilename             := True;

  FModelHighlight                := psmhObjectPascal;
  FExchangeItemDescribeHighlight := psmhNone;
  FExchangeItemReplaceHighlight  := psmhNone;
  FExchangeItemByHighlight       := psmhSQL;
End;

Destructor TProject.Destroy;
Begin
  FColWidths.Free;
  FExchangeItemList.Free;
  FPathHistory.Free;

  Inherited;
End;

Function TProject.GenerateFileName: String;
Var
  TMPExchangeItemList: TExchangeItemList;
Begin
  If FPathHistory.Count = 0 Then
  Begin
    Result := '';
    Exit;
  End;

  Result              := FPathHistory.Last;

  TMPExchangeItemList := GetParsedExchangeItemList;
  Try
    For Var I         := 0 To TMPExchangeItemList.Count - 1 Do
    Begin
      If TMPExchangeItemList.Items[I].FReplace.StartsWith('%') And
        TMPExchangeItemList.Items[I].FReplace.EndsWith('%') Then
        Result := StringReplace(Result, TMPExchangeItemList.Items[I].FReplace,
          TMPExchangeItemList.Items[I].FBy,
          [rfReplaceAll, rfIgnoreCase]);
    End;
  Finally
    TMPExchangeItemList.Free;
  End;
End;

Function TProject.GenerateText: String;
Var
  TMPExchangeItemList: TExchangeItemList;
Begin
  TMPExchangeItemList := GetParsedExchangeItemList; // get list with replacement done in exchange item
  Try
    Result            := FModel;
    // replace in model
    For Var I         := 0 To TMPExchangeItemList.Count - 1 Do
    Begin
      If TMPExchangeItemList[I].FInModel Then
        Result := StringReplace(Result, TMPExchangeItemList[I].FReplace,
          TMPExchangeItemList[I].FBy, [rfReplaceAll, rfIgnoreCase]);
    End;
  Finally
    TMPExchangeItemList.Free;
  End;
End;

Function TProject.GetFileName: String;
Begin
  If FPathHistory.Count > 0 Then
  Begin
    If Not FReplaceInFilename Then
      Result := FPathHistory.Last
    Else
      Result := GenerateFileName;
  End
  Else
    Result := '';
End;

Function TProject.GetParsedExchangeItemList: TExchangeItemList;
Begin
  Result := TExchangeItemList.Create;
  Try
    Result.Assign(FExchangeItemList);

    // parse list
    For Var I := 0 To Result.Count - 1 Do
    Begin
      // apply replace exchange item
      If Result[I].FInExchangeItem Then
      Begin
        // order is important, apply only on item below
        For Var N := I To Result.Count - 1 Do
        Begin
          // on item of list with inmodel true
          If Result[N].FInModel Then
            Result[N].FBy := StringReplace(Result[N].FBy,
              Result[I].FReplace, Result[I].FBy, [rfReplaceAll, rfIgnoreCase]);
        End;
      End;
    End;
  Except
    FreeAndNil(Result);
    Raise;
  End;
End;

Function TProject.LoadFromFile(AFileName: String): Boolean;
Begin
  If Not FileExists(AFileName) Then
    Exit(False);

  With TStringList.Create Do
    Try
      LoadFromFile(AFileName);

      LoadFromJson(Text);

      Exit(True);
    Finally
      Free;
    End;
End;

Procedure TProject.LoadFromJson(AJson: String);
Var
  StringReader: TStringReader;
  JsonReader: TJsonTextReader;
  PropertyName: String;
  ExchangeItem: TExchangeItem;
Begin
  StringReader := TStringReader.Create(AJson);
  JsonReader   := TJsonTextReader.Create(StringReader);
  Try
    While JsonReader.read Do
    Begin
      // check each property and read it if found
      If JsonReader.TokenType = TJsonToken.PropertyName Then
        PropertyName := JsonReader.Value.AsString
      Else
        Continue;

      If PropertyName = 'Model' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.String) Then
          FModel := JsonReader.Value.AsString;
      End
      Else If PropertyName = 'ReplaceInFilename' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Boolean) Then
          FReplaceInFilename := JsonReader.Value.AsBoolean;
      End
      Else If PropertyName = 'PathHistoryMax' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
          FPathHistoryMax := JsonReader.Value.AsInteger;
      End
      Else If PropertyName = 'ModelHighlight' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
          FModelHighlight := TProjectSynMemoHighlighter(JsonReader.Value.AsInteger);
      End
      Else If PropertyName = 'ExchangeItemDescribeHighlight' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
          FExchangeItemDescribeHighlight := TProjectSynMemoHighlighter(JsonReader.Value.AsInteger);
      End
      Else If PropertyName = 'ExchangeItemReplaceHighlight' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
          FExchangeItemReplaceHighlight := TProjectSynMemoHighlighter(JsonReader.Value.AsInteger);
      End
      Else If PropertyName = 'ExchangeItemByHighlight' Then
      Begin
        // get value
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
          FExchangeItemByHighlight := TProjectSynMemoHighlighter(JsonReader.Value.AsInteger);
      End
      Else If PropertyName = 'PathHistory' Then
      Begin
        // get array
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.StartArray) Then
        Begin
          FPathHistory.Clear; // empty actual list
          // parse array
          While JsonReader.read And (JsonReader.TokenType = TJsonToken.String) Do
            FPathHistory.Add(JsonReader.Value.AsString);
        End;
      End
      Else If PropertyName = 'ColWidths' Then
      Begin
        // get array
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.StartArray) Then
        Begin
          FColWidths.Clear; // empty actual list
          // parse array
          While JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Do
            FColWidths.Add(JsonReader.Value.AsInteger);

          CheckColsWidths;
        End;
      End
      Else If PropertyName = 'ExchangeItemList' Then
      Begin
        // get array of exchange item
        If JsonReader.read And (JsonReader.TokenType = TJsonToken.StartArray) Then
        Begin
          FExchangeItemList.Clear; // empty actual list
          // parse array of object, break on end array
          While JsonReader.read And (JsonReader.TokenType = TJsonToken.StartObject) Do
          Begin
            ExchangeItem := TExchangeItem.Create;
            Try
              // parse property of object
              While JsonReader.read And (JsonReader.TokenType = TJsonToken.PropertyName) Do
              Begin
                If JsonReader.Value.AsString = 'RowHeight' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.Integer) Then
                    ExchangeItem.FRowHeight := JsonReader.Value.AsInteger;
                End
                Else If JsonReader.Value.AsString = 'Describe' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.String) Then
                    ExchangeItem.FDescribe := JsonReader.Value.AsString;
                End
                Else If JsonReader.Value.AsString = 'Replace' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.String) Then
                    ExchangeItem.FReplace := JsonReader.Value.AsString;
                End
                Else If JsonReader.Value.AsString = 'By' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.String) Then
                    ExchangeItem.FBy := JsonReader.Value.AsString;
                End
                Else If JsonReader.Value.AsString = 'InModel' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.Boolean) Then
                    ExchangeItem.FInModel := JsonReader.Value.AsBoolean;
                End
                Else If JsonReader.Value.AsString = 'InExchangeItem' Then
                Begin
                  // get value
                  If JsonReader.read And (JsonReader.TokenType = TJsonToken.Boolean) Then
                    ExchangeItem.FInExchangeItem := JsonReader.Value.AsBoolean;
                End;
              End;
            Except
              ExchangeItem.Free;
              Raise;
            End;
            FExchangeItemList.Add(ExchangeItem); // add item loaded

            While (JsonReader.TokenType <> TJsonToken.EndObject) Do
              JsonReader.read; // ensure go to end object
          End;
        End;
      End;
    End;
  Finally
    JsonReader.Free;
    StringReader.Free;
  End;
End;

Function TProject.SaveAsJson: String;
Var
  StringWriter: TStringWriter;
  JsonWriter: TJsonTextWriter;
  JsonBuilder: TJSONObjectBuilder;
Begin
  StringWriter            := TStringWriter.Create;
  JsonWriter              := TJsonTextWriter.Create(StringWriter);
  JsonBuilder             := TJSONObjectBuilder.Create(JsonWriter);
  Try
    JsonWriter.Formatting := TJsonFormatting.Indented;
    With JsonBuilder.BeginObject Do
    Begin
      Add('Model', FModel);

      Add('ReplaceInFilename', FReplaceInFilename);

      Add('ModelHighlight', FModelHighlight);

      Add('ExchangeItemDescribeHighlight', FExchangeItemDescribeHighlight);

      Add('ExchangeItemReplaceHighlight', FExchangeItemReplaceHighlight);

      Add('ExchangeItemByHighlight', FExchangeItemByHighlight);

      Add('PathHistoryMax', FPathHistoryMax);
      If FPathHistory.Count > 0 Then
      Begin
        With BeginArray('PathHistory') Do
        Begin
          For Var I := 0 To FPathHistory.Count - 1 Do
            Add(FPathHistory[I]);
          EndArray;
        End;
      End;

      If FColWidths.Count > 0 Then
      Begin
        With BeginArray('ColWidths') Do
        Begin
          For Var I := 0 To FColWidths.Count - 1 Do
            Add(FColWidths[I]);
          EndArray;
        End;
      End;

      If FExchangeItemList.Count > 0 Then
      Begin
        With BeginArray('ExchangeItemList') Do
        Begin
          For Var I := 0 To FExchangeItemList.Count - 1 Do
          Begin
            BeginObject
              .Add('Describe', FExchangeItemList[I].FDescribe)
              .Add('RowHeight', FExchangeItemList[I].FRowHeight)
              .Add('Replace', FExchangeItemList[I].FReplace)
              .Add('By', FExchangeItemList[I].FBy)
              .Add('InModel', FExchangeItemList[I].FInModel)
              .Add('InExchangeItem', FExchangeItemList[I].FInExchangeItem)
              .EndObject;
          End;
          EndArray;
        End;
      End;

      EndObject;
    End;

    Result := StringWriter.ToString;
  Finally
    JsonBuilder.Free;
    JsonWriter.Free;
    StringWriter.Free;
  End;
End;

Function TProject.SaveToFile(AFileName: String): Boolean;
Begin
  With TStringList.Create Do
    Try
      Text := SaveAsJson;

      SaveToFile(AFileName);
    Finally
      Free;
    End;

  Exit(True);
End;

Procedure TProject.SetFileName(Const Value: String);
Var
  ValueLowerCase: String;
Begin
  If Value = '' Then
    Exit;

  ValueLowerCase := AnsiLowerCase(Value);
  For Var I      := FPathHistory.Count - 1 Downto 0 Do
  Begin
    If AnsiLowerCase(FPathHistory[I]) = ValueLowerCase Then
      FPathHistory.Delete(I);
  End;

  FPathHistory.Add(Value);

  While FPathHistory.Count > FPathHistoryMax Do
    FPathHistory.Delete(0);
End;

{ TProjectPathHistory }

Function TProjectPathHistory.Add(Value: String): Integer;
Var
  ValueLowerCase: String;
Begin
  If Value = '' Then
    Exit(-1);

  ValueLowerCase := AnsiLowerCase(Value);
  For Var I      := Count - 1 Downto 0 Do
  Begin
    If AnsiLowerCase(Items[I]) = ValueLowerCase Then
      Delete(I);
  End;

  Result := Inherited Add(Value);
End;

Procedure TProjectPathHistory.LoadFromIni(AMemIniFile: TMemIniFile);
Begin
  For Var I := 0 To AMemIniFile.ReadInteger('ProjectPathHistory', 'Count', Count) - 1 Do
    Add(AMemIniFile.ReadString('ProjectPathHistory', 'item_' + IntToStr(I), ''));
End;

Procedure TProjectPathHistory.SaveToIni(AMemIniFile: TMemIniFile);
Begin
  With AMemIniFile Do
  Begin
    EraseSection('ProjectPathHistory');

    WriteInteger('ProjectPathHistory', 'Count', Count);

    For Var I := 0 To Count - 1 Do
      WriteString('ProjectPathHistory', 'item_' + IntToStr(I), Items[I]);
  End;
End;

End.
