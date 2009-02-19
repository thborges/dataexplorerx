unit DataExplorerTree;

interface

uses
  {$IFNDEF TESTTOOL}
  DockForm, ToolsAPI,
  {$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, SQLExpr, Menus, ActnList,
  NodeTypes;

type
  {$IFDEF TESTTOOL}
   TDockableForm = TForm;
  {$ENDIF}

  TDETree = class(TDockableForm)
    ImageList: TImageList;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actEditConnection: TAction;
    Editconnection1: TMenuItem;
    TreeView: TTreeView;
    actNewConnection: TAction;
    Newconnection1: TMenuItem;
    actDeleteConnection: TAction;
    Deleteconnection1: TMenuItem;
    actSelectFrom: TAction;
    selectfrom1: TMenuItem;
    actSourceCode: TAction;
    Showsourcecode1: TMenuItem;
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure actEditConnectionExecute(Sender: TObject);
    procedure TreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure actEditConnectionUpdate(Sender: TObject);
    procedure TreeViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure actNewConnectionUpdate(Sender: TObject);
    procedure actNewConnectionExecute(Sender: TObject);
    procedure actDeleteConnectionUpdate(Sender: TObject);
    procedure actDeleteConnectionExecute(Sender: TObject);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure actSelectFromUpdate(Sender: TObject);
    procedure actSelectFromExecute(Sender: TObject);
    procedure actSourceCodeExecute(Sender: TObject);
  private
    procedure AddConnectionNode(Node: TTreeNode; Name: String);
  protected
    procedure LoadDrivers; virtual;
    procedure UpdateActFor(Action: TAction; NodeType: TDENodeClass); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

const
  {$IFDEF VER150} SDelphiReg = '\Software\Borland\DBExpress'; {$ENDIF}
  {$IFDEF VER180} SDelphiReg = '\Software\Borland\BDS\5.0\DBExpress'; {$ENDIF}
  {$IFDEF VER190} SDelphiReg = '\Software\CodeGear\BDS\6.0\DBExpress'; {$ENDIF}

var
  DETree: TDETree;

implementation

uses ValueEdit, WideStrings, ExecuteQueryForm;

{$R *.dfm}

{ TDETree }

procedure TDETree.actEditConnectionUpdate(Sender: TObject);
begin
  UpdateActFor(actEditConnection, TDENodeConnection);
end;

procedure TDETree.actNewConnectionExecute(Sender: TObject);
var
  Name: String;
begin
  if InputQuery('New connection', 'Name', Name) then
  begin
    ConnectionAdmin.AddConnection(Name, TreeView.Selected.Text);
    AddConnectionNode(TreeView.Selected, Name);
  end;
end;

procedure TDETree.actNewConnectionUpdate(Sender: TObject);
begin
  UpdateActFor(actNewConnection, TDENodeDriver);
end;

procedure TDETree.actSelectFromExecute(Sender: TObject);
var
  Form: TExecuteQuery;
  SQL: String;
begin
  with TTreeNodeDex(TreeView.Selected).DataObject do
  begin
    SQL := Format('select * from %s', [TreeView.Selected.Text]);
    Form := TExecuteQuery.Create(Connection, SQL, Connection);
    Form.Show;
  end;
end;

procedure TDETree.actSelectFromUpdate(Sender: TObject);
begin
  UpdateActFor(actSelectFrom, TDENodeTableView);
end;

procedure TDETree.actSourceCodeExecute(Sender: TObject);
begin
  if Assigned(TreeView.Selected) and Assigned(TreeView.Selected.Data) then
    TDENode(TreeView.Selected.Data).ShowSourceCode(TTreeNodeDex(TreeView.Selected));
end;

procedure TDETree.LoadDrivers;
var
//  Svcs: IOTAServices;
  i: Integer;
  Sections: TStrings;
  Item: TTreeNodeDex;
begin
//  Svcs := (BorlandIDEServices as IOTAServices);
  Sections := TStringList.Create;
  try
    ConnectionAdmin.GetDriverNames(Sections);
    TreeView.Items.Clear;

    for i := 0 to Sections.Count -1 do
    begin
      Item := TTreeNodeDex(TreeView.Items.Add(nil, Sections[i]));
      Item.DataObject := TDENodeDriver.Create(Item);
      Item.ImageIndex := 0;
      Item.SelectedIndex := 0;
      TreeView.Items.AddChild(Item, '');
    end;

  finally
    Sections.Free;
  end;
end;

procedure TDETree.TreeViewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  TreeView.Select(TreeView.GetNodeAt(MousePos.X, MousePos.Y));
  Handled := False;
end;

procedure TDETree.TreeViewCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TTreeNodeDex;
end;

procedure TDETree.TreeViewEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if TDENode(Node.Data) is TDENodeConnection then
    ConnectionAdmin.RenameConnection(Node.Text, S);
end;

procedure TDETree.TreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := TDENode(Node.Data) is TDENodeConnection;
end;

procedure TDETree.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  try
    if TTreeNodeDex(Node).DataObject <> nil then
        TTreeNodeDex(Node).DataObject.FillChildrens(TTreeNodeDex(Node));
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      AllowExpansion := False;
    end;
  end;
end;

procedure TDETree.UpdateActFor(Action: TAction; NodeType: TDENodeClass);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  Action.Visible := Assigned(Node) and (TDENode(Node.Data) is NodeType);
end;

procedure TDETree.AddConnectionNode(Node: TTreeNode; Name: String);
var
  ConnNode: TTreeNodeDex;
  Params: TWideStrings;
begin
  Params := TWideStringList.Create;
  try
    ConnNode := AddChildNode(TTreeNodeDex(Node), TDENodeConnection, itConnectionClose, Name);
    AddChildNode(ConnNode, TDENodeNone, itDriver);
  finally
    Params.Free;
  end;
end;

constructor TDETree.Create(AOwner: TComponent);
begin
  inherited;
  DeskSection := Name;
  LoadDrivers;
end;

procedure TDETree.actDeleteConnectionExecute(Sender: TObject);
begin
  ConnectionAdmin.DeleteConnection(TreeView.Selected.Text);
  TreeView.Items.Delete(TreeView.Selected);
end;

procedure TDETree.actDeleteConnectionUpdate(Sender: TObject);
begin
  UpdateActFor(actDeleteConnection, TDENodeConnection);
end;

procedure TDETree.actEditConnectionExecute(Sender: TObject);
var
  Params: TWideStrings;
  Editor: TValueEditDlg;
begin
  Editor := ValueEdit.TValueEditDlg.Create(nil);
  try
    Params := TDENodeConnection(TreeView.Selected.Data).Connection.Params;
    Editor.ValueListEditor1.Strings.Text := Params.Text;
    if Editor.ShowModal = mrOk then
    begin
      Params.Text := Editor.ValueListEditor1.Strings.Text;
      ConnectionAdmin.ModifyConnection(TreeView.Selected.Text, Params);
    end;
  finally
    Editor.Free;
  end;
end;

end.
