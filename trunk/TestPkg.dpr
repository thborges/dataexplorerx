program TestPkg;

uses
  Forms,
  DataExplorerTree in 'DataExplorerTree.pas' {DETree},
  NodeTypes in 'NodeTypes.pas',
  ExecuteQueryForm in 'ExecuteQueryForm.pas' {ExecuteQuery},
  MetaDataWrapers in 'MetaDataWrapers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDETree, DETree);
  Application.Run;
end.
