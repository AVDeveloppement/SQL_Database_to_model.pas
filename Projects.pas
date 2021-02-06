Unit Projects;

Interface

Uses System.Classes, System.Generics.Collections, System.JSON, System.SysUtils,
  System.IniFiles, System.JSON.Types, System.JSON.Builders, System.JSON.Writers,
  System.JSON.Readers;

Type
  TExchangeItem = Class
    FRowHeight: Integer;
    FReplace, FBy: String;
    FInModel: Boolean;
    Procedure Assign(AExchangeItem: TExchangeItem);
    Constructor Create;
  End;

  TExchangeItemList = Class(TObjectList<TExchangeItem>)
    Procedure Assign(AExchangeItemList: TExchangeItemList);
  End;

  TProject = Class
    FModel: String;
    FColWidths: TList<Integer>;
    FExchangeItemList: TExchangeItemList;
    FPathHistory: TList<String>;
    FPathHistoryMax: Integer;
    FReplaceInFilename: Boolean;
  Private
    Function GenerateFileName: String;
    Function GetFileName: String;
    Procedure SetFileName(Const Value: String);
  Public
    Constructor Create;
    Procedure Default;
    Destructor Destroy; Override;
    Function GenerateText: String;
    Procedure LoadFromFile(AFileName: String);
    Procedure LoadFromJson(AJson: String);
    Function SaveAsJson: String;
    Procedure SaveToFile(AFileName: String);
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
  cIDX_COLUMN_ID       = 0;
  cIDX_COLUMN_REPLACE  = 1;
  cIDX_COLUMN_BY       = 2;
  cIDX_COLUMN_IN_MODEL = 3;

Implementation

{ TExchangeItem }

Procedure TExchangeItem.Assign(AExchangeItem: TExchangeItem);
Begin
  FReplace   := AExchangeItem.FReplace;
  FBy        := AExchangeItem.FBy;

  FRowHeight := AExchangeItem.FRowHeight;

  FInModel   := AExchangeItem.FInModel;
End;

Constructor TExchangeItem.Create;
Begin
  FReplace   := '';
  FBy        := '';

  FRowHeight := 30;

  FInModel   := True;
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
  FColWidths.Add(150); // replace
  FColWidths.Add(300); // by
  FColWidths.Add(100); // in model

  FExchangeItemList.Clear;
  FExchangeItemList.Add(TExchangeItem.Create);
  With FExchangeItemList.Last Do
  Begin
    FReplace   := '%SQL%';
    FRowHeight := 100;
  End;
  FExchangeItemList.Add(TExchangeItem.Create);
  With FExchangeItemList.Last Do
  Begin
    FReplace   := '''';
    FBy        := '''''';
    FRowHeight := 30;
    FInModel   := false;
  End;

  FPathHistory.Clear;
  FPathHistoryMax := 10;

  FModel          := 'Unit DBModel;' + sLineBreak + sLineBreak +
    'Interface' + sLineBreak + sLineBreak +
    'Const' + sLineBreak +
    '  cDB_MODEL = ''%SQL%'';' + sLineBreak + sLineBreak +
    'Implementation' + sLineBreak + sLineBreak +
    'End.';

  FReplaceInFilename := True;
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
  sExchangeItemReplace, t: String;
  isExchangeItemReplaceLength: Integer;
Begin
  If FPathHistory.Count = 0 Then
  Begin
    Result := '';
    Exit;
  End;

  Result                        := FPathHistory.Last;

  For Var I                     := 0 To FExchangeItemList.Count - 1 Do
  Begin
    sExchangeItemReplace        := FExchangeItemList.Items[I].FReplace;
    isExchangeItemReplaceLength := Length(sExchangeItemReplace);
    t                           := Copy(sExchangeItemReplace, 1, 1);
    t                           := Copy(sExchangeItemReplace, isExchangeItemReplaceLength, 1);
    If (isExchangeItemReplaceLength > 2) And (Copy(sExchangeItemReplace, 1, 1) = '%')
      And (Copy(sExchangeItemReplace, isExchangeItemReplaceLength, 1) = '%') Then
      Result := StringReplace(Result, FExchangeItemList.Items[I].FReplace, FExchangeItemList.Items[I].FBy,
        [rfReplaceAll, rfIgnoreCase]);
  End;
End;

Function TProject.GenerateText: String;
Var
  TMPExchangeItemList: TExchangeItemList;
Begin
  // work on a tmp list so we can modify item
  TMPExchangeItemList := TExchangeItemList.Create;
  Try
    TMPExchangeItemList.Assign(FExchangeItemList);

    // parse list
    For Var I := 0 To TMPExchangeItemList.Count - 1 Do
    Begin
      // apply exchange item with inmodel false
      If Not TMPExchangeItemList[I].FInModel Then
      Begin
        // order is important, apply only on item below
        For Var N := I To TMPExchangeItemList.Count - 1 Do
        Begin
          // on item of list with inmodel true
          If TMPExchangeItemList[N].FInModel Then
            TMPExchangeItemList[N].FBy := StringReplace(TMPExchangeItemList[N].FBy,
              TMPExchangeItemList[I].FReplace, TMPExchangeItemList[I].FBy, [rfReplaceAll, rfIgnoreCase]);
        End;
      End;
    End;

    Result    := FModel;
    // finally replace in model
    For Var I := 0 To TMPExchangeItemList.Count - 1 Do
    Begin
      If TMPExchangeItemList[I].FInModel Then
        Result := StringReplace(Result, TMPExchangeItemList[I].FReplace, TMPExchangeItemList[I].FBy,
          [rfReplaceAll, rfIgnoreCase]);
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

Procedure TProject.LoadFromFile(AFileName: String);
Begin
  If Not FileExists(AFileName) Then
    Exit;

  With TStringList.Create Do
    Try
      LoadFromFile(AFileName);
      LoadFromJson(Text);
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
              .Add('RowHeight', FExchangeItemList[I].FRowHeight)
              .Add('Replace', FExchangeItemList[I].FReplace)
              .Add('By', FExchangeItemList[I].FBy)
              .Add('InModel', FExchangeItemList[I].FInModel)
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

Procedure TProject.SaveToFile(AFileName: String);
Begin
  With TStringList.Create Do
    Try
      Text := SaveAsJson;
      SaveToFile(AFileName);
    Finally
      Free;
    End;
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
