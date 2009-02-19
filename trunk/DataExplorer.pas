unit DataExplorer;

interface

uses ToolsAPI, Forms, Dialogs, DataExplorerTree;

type

  TDataExplorerX = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  public
    procedure Execute;
    function GetIDString: String;
    function GetName: String;
    function GetState: TWizardState;
    function GetMenuText: String;
  end;


implementation

{ TDataExplorerX }

procedure TDataExplorerX.Execute;
begin
  if not Assigned(DETree) then
    DETree := TDETree.Create(Application);
  DETree.Show;
end;

function TDataExplorerX.GetIDString: String;
begin
  Result := 'DataExplorerX';
end;

function TDataExplorerX.GetMenuText: String;
begin
  Result := 'DataExplorerX';
end;

function TDataExplorerX.GetName: String;
begin
  Result := 'DataExplorerX';
end;

function TDataExplorerX.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
