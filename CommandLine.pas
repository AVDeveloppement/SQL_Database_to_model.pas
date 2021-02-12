Unit CommandLine;

Interface

Uses
  Winapi.Windows, System.Win.Registry, System.SysUtils, Vcl.Forms;

Type
  TCommandLine = Class
    Function IsAdmin: Boolean;
    Function ProjectExtIsValid(AProjectExt: String): Boolean;
    Function ProjectInParam: String;
    Procedure RegisterRegEntries(AProjectExt: String);
    Function RegistryAssocProcessed: Boolean;
    Function RegistryReadAppEntry: Boolean;
    Procedure UnregisterRegEntries(AProjectExt: String);
  End;

Const
  cPRODUCT_NAME          = 'SQL Database Model to Model.pas';
  cFORM_TITLE            = cPRODUCT_NAME + ' - %s (%s)';
  cREGISTRY_PRODUCT_NAME = 'AVDeveloppement.eu.SQL.Database.Model.to.Model.pas';

Var
  CmdLine: TCommandLine;

Implementation

Const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID                    = $00000020;
  DOMAIN_ALIAS_RID_ADMINS                        = $00000220;
  SE_GROUP_ENABLED                               = $00000004;

  { TCommandLine }

Function TCommandLine.IsAdmin: Boolean;
Var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  bSuccess: Boolean;
Begin
  Result     := False;

  bSuccess   := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  If Not bSuccess And (GetlastError = ERROR_NO_TOKEN) Then
    bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);

  If bSuccess Then
  Begin
    GetTokenInformation(hAccessToken, TokenGroups, Nil, 0, dwInfoBufferSize);
    ptgGroups  := GetMemory(dwInfoBufferSize);
    Try
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, dwInfoBufferSize,
        dwInfoBufferSize);
      CloseHandle(hAccessToken);
      If bSuccess Then
      Begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID,
          DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
        For Var I := 0 To ptgGroups.GroupCount - 1 Do
        Begin
          If (SE_GROUP_ENABLED = (ptgGroups.Groups[I].Attributes And SE_GROUP_ENABLED)) And
            EqualSid(psidAdministrators, ptgGroups.Groups[I].Sid) Then
            Exit(True);
        End;
        FreeSid(psidAdministrators);
      End;
    Finally
      FreeMem(ptgGroups);
    End;
  End;
End;

Function TCommandLine.ProjectExtIsValid(AProjectExt: String): Boolean;
Const
  cVALID_CHAR = ['0' .. '9', 'a' .. 'z', 'A' .. 'Z'];
Begin
  For Var I := 1 To Length(AProjectExt) Do
  Begin
    If Not CharInSet(AProjectExt[I], cVALID_CHAR) Then
      Exit(False);
  End;

  Result := True;
End;

Function TCommandLine.ProjectInParam: String;
Var
  sExt: String;
Begin
  If ParamCount <> 1 Then
    Exit('');

  Result := ParamStr(1);

  // file must exist
  If (Length(Result) < 2) Or Not FileExists(Result) Then
    Exit('');

  // ext must be same as settings
  sExt := ExtractFileExt(Result);
  If (Length(sExt) < 2) Or
    Not ProjectExtIsValid(Copy(sExt, 2)) Then
    Exit('');
End;

Procedure TCommandLine.RegisterRegEntries(AProjectExt: String);
Begin
  If Not IsAdmin Then
    Exit;

  With TRegistry.Create Do
    Try
      RootKey := HKEY_CLASSES_ROOT;

      If OpenKey(cREGISTRY_PRODUCT_NAME, True) Then
      Begin
        WriteString('', cPRODUCT_NAME);
        CloseKey;
      End;

      If OpenKey(cREGISTRY_PRODUCT_NAME + '\Shell\Open\Command', True) Then
      Begin
        WriteString('', Application.ExeName + ' "%1"');
        CloseKey;
      End;

      If OpenKey('.' + AProjectExt + '\OpenWithProgids', True) Then
      Begin
        WriteString(cREGISTRY_PRODUCT_NAME, '');
        CloseKey;
      End;

      RootKey := HKEY_CURRENT_USER;

      If OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\OpenWithProgids', True) Then
      Begin
        WriteString(cREGISTRY_PRODUCT_NAME, '');
        CloseKey;
      End;

      // Cannot write/delete ProgId, need trying delete key
      If KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\UserChoice') And Not
        DeleteKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\UserChoice') Then
        Exit;

      If OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\UserChoice', True) Then
      Begin
        WriteString('ProgId', cREGISTRY_PRODUCT_NAME);
        CloseKey;
      End;
    Finally
      Free;
    End;
End;

Function TCommandLine.RegistryAssocProcessed: Boolean;
Var
  sCommand, sProjectExt: String;
Begin
  If ParamCount <> 2 Then
    Exit(False);

  sCommand := AnsiLowerCase(ParamStr(1));
  If (Length(sCommand) < 2) Or (sCommand[1] <> '/') Then
    Exit(False);

  sCommand    := Copy(sCommand, 2);

  sProjectExt := ParamStr(2);
  If Not ProjectExtIsValid(sProjectExt) Then
    Exit(False);

  // need to have admin right for write registry ext run with
  If Not IsAdmin Then
    Exit(False);

  If sCommand = 'registerassoc' Then
  Begin
    RegisterRegEntries(sProjectExt);
    RegistryReadAppEntry;
    Exit(True);
  End
  Else If sCommand = 'unregisterassoc' Then
  Begin
    UnregisterRegEntries(sProjectExt);
    Exit(True);
  End
  Else
    Exit(False);
End;

Function TCommandLine.RegistryReadAppEntry: Boolean;
Begin
  Result := False;

  With TRegistry.Create Do
    Try
      RootKey := HKEY_CLASSES_ROOT;

      If KeyExists(cREGISTRY_PRODUCT_NAME) And OpenKeyReadOnly(cREGISTRY_PRODUCT_NAME) Then
      Begin
        If ReadString('') = cPRODUCT_NAME Then
          Result := True;
        CloseKey;
      End;
    Finally
      Free;
    End;
End;

Procedure TCommandLine.UnregisterRegEntries(AProjectExt: String);
Begin
  If Not IsAdmin Then
    Exit;

  With TRegistry.Create Do
    Try
      RootKey := HKEY_CLASSES_ROOT;

      If KeyExists(cREGISTRY_PRODUCT_NAME) Then
        DeleteKey(cREGISTRY_PRODUCT_NAME);

      If KeyExists('.' + AProjectExt + '\OpenWithProgids') And
        OpenKey('.' + AProjectExt + '\OpenWithProgids', False) Then
      Begin
        DeleteValue(cREGISTRY_PRODUCT_NAME);
        CloseKey;
      End;

      RootKey := HKEY_CURRENT_USER;

      If KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\OpenWithProgids') And
        OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\OpenWithProgids', False) Then
      Begin
        DeleteValue(cREGISTRY_PRODUCT_NAME);
        CloseKey;
      End;

      If KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
        AProjectExt + '\UserChoice') Then
        DeleteKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' +
          AProjectExt + '\UserChoice');
    Finally
      Free;
    End;
End;

Initialization

CmdLine := TCommandLine.Create;

Finalization

CmdLine.Free;

End.
