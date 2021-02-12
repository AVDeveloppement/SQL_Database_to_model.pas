Unit Highlighters;

Interface

Uses
  System.SysUtils, System.Classes, SynEditMiscClasses, SynEditSearch,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas, SynEdit, SynMemo,
  SynHighlighterUNIXShellScript, SynHighlighterSQL, SynHighlighterPHP,
  SynHighlighterPerl, SynHighlighterDWS, SynHighlighterJScript,
  SynHighlighterJava, SynHighlighterInno, SynHighlighterIni, SynHighlighterHtml,
  SynHighlighterCpp, SynHighlighterCS;

Type
  TdmHighlighters = Class(TDataModule)
    shUNIX: TSynUNIXShellScriptSyn;
    shSQL: TSynSQLSyn;
    shPHP: TSynPHPSyn;
    shPERL: TSynPerlSyn;
    shPAS: TSynPasSyn;
    shJSCRIPT: TSynJScriptSyn;
    shJAVA: TSynJavaSyn;
    shINNO: TSynInnoSyn;
    shINI: TSynIniSyn;
    shHTML: TSynHTMLSyn;
    shCPP: TSynCppSyn;
    shCSS: TSynCSSyn;
  Private
    { D�clarations priv�es }
  Public
    { D�clarations publiques }
  End;

Var
  dmHighlighters: TdmHighlighters;

Implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

End.
