unit NodeTypes;

interface

uses SysUtils, ComCtrls, SQLExpr, DBXMetaDataCommandFactory,
  DBXMetaDataReader, ClassRegistry, DBXTableStorage, Forms, Controls,
  DBXMetaDataNames, Classes, WideStrings, DBConnAdmin,
  DBXOracleReadOnlyMetaData;

type
  TImageType = (itDriver, itConnectionClose, itConnectionOpen, itTables,
    itView, itProceduresFuncs, itSynonyms, itIndex, itColumns, itSysTable);

  TDENode = class;
  TTreeNodeDex = class(TTreeNode)
  public
    DataObject: TDENode;
    destructor Destroy; override;
  end;
  
  TDENodeClass = class of TDENode;
  TDENode = class
  private
    FNode: TTreeNodeDex;
    FMetaReader: TDBXBaseMetaDataReader;
  protected
    FSQLConn: TSQLConnection;
    procedure InternalFillChildrens(Node: TTreeNodeDex); virtual;
    procedure GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings); virtual;
  public
    procedure FillChildrens(Node: TTreeNodeDex);
    procedure ShowSourceCode(Node: TTreeNodeDex); virtual;
    constructor Create(Node: TTreeNodeDex); virtual;
    function GetSchema(Node: TTreeNodeDex): String;
    property Connection: TSQLConnection read FSQLConn write FSQLConn;
    property MetaReader: TDBXBaseMetaDataReader read FMetaReader write FMetaReader;
  end;

  TDENodeNone = class(TDENode)
  protected
  end;

  TDENodeDriver = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeConnection = class(TDENode)
  protected
    FMetaDataPackageLoader: String;
    FMetadataCommandFactory: TDBXMetaDataCommandFactory;
    FDBXContext: TDBXDataExpressProviderContext;
    procedure CheckConnection; virtual;
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  public
    constructor Create(Node: TTreeNodeDex); override;
    destructor Destroy; override;
  end;

  TDENodeSchema = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeTableList = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeViewList = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeTableView = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeProcedureList = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeFunctionList = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeProcFunc = class(TDENode)
  protected
    procedure GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings); override;
  end;

  TDENodeTableViewColumns = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeIndices = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeIndicesColumns = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeForeignKeys = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeForeignKeysColumns = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeSystemTables = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodeSynonyms = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodePackageList = class(TDENode)
  protected
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

  TDENodePackage = class(TDENode)
  protected
    procedure GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings); override;
    procedure InternalFillChildrens(Node: TTreeNodeDex); override;
  end;

function AddChildNode(Parent: TTreeNodeDex; ItemClass: TDENodeClass;
  ImageIndex: TImageType; Text: String = ''; AddEmptyChild: Boolean = True): TTreeNodeDex;

const
  SDefaultSchema = 'Default Schema';

var
  ConnectionAdmin: IConnectionAdmin;

implementation

uses ExecuteQueryForm, MetaDataWrapers;

function AddChildNode(Parent: TTreeNodeDex; ItemClass: TDENodeClass;
  ImageIndex: TImageType; Text: String; AddEmptyChild: Boolean): TTreeNodeDex;
begin
  Result := TTreeNodeDex(TTreeView(Parent.TreeView).Items.AddChild(Parent, Text));
  Result.DataObject := ItemClass.Create(Result);
  Result.ImageIndex := Ord(ImageIndex);
  Result.SelectedIndex := Ord(ImageIndex);

  if Assigned(TTreeNodeDex(Parent).DataObject.Connection) then
  begin
    Result.DataObject.Connection := TTreeNodeDex(Parent).DataObject.Connection;
    Result.DataObject.MetaReader := TTreeNodeDex(Parent).DataObject.MetaReader;
  end;

  if AddEmptyChild then
    TTreeView(Parent.TreeView).Items.AddChild(Result, '');
end;

{ TDENodeDriver }

procedure TDENodeDriver.InternalFillChildrens(Node: TTreeNodeDex);
var
  i: Integer;
  Sections: TStrings;
  DriverName: String;
begin
  inherited;
  DriverName := Node.Text;
  Sections := TStringList.Create;
  try
    ConnectionAdmin.GetConnectionNames(Sections, DriverName);

    Node.DeleteChildren;
    for i := 0 to Sections.Count - 1 do
      AddChildNode(Node, TDENodeConnection, itConnectionClose, Sections[i]);

  finally
    Sections.Free;
  end;
end;

{ TDENodeConnection }

constructor TDENodeConnection.Create(Node: TTreeNodeDex);
var
  Params: TWideStrings;
begin
  inherited;
  FSQLConn := TSQLConnection.Create(nil);
  FSQLConn.LoadParamsOnConnect := True;
  FSQLConn.LoginPrompt := False;

  Params := TWideStringList.Create;
  try
    ConnectionAdmin.GetDriverParams(Node.Parent.Text, Params);
    FMetadataPackageLoader := Params.Values['MetaDataPackageLoader'];
  finally
    Params.Free;
  end;

  Connection.DriverName := Node.Parent.Text;
  Connection.ConnectionName := Node.Text;
  Connection.LoadParamsFromIniFile;

  FMetadataCommandFactory := nil;
end;

destructor TDENodeConnection.Destroy;
begin
  FMetadataCommandFactory.Free;
  FMetaReader.Free;
  FSQLConn.Free;
  inherited;
end;

procedure TDENodeConnection.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Empty: Boolean;
  SchemaNode: TTreeNodeDex;
begin
  inherited;
  CheckConnection;
  DbxTable := MetaReader.FetchSchemas('');
  try
    Node.DeleteChildren;
    
    Empty := True;
    while DbxTable.Next do
    begin
      if Empty then
        Empty := False;
      AddChildNode(Node, TDENodeSchema, itDriver, DbxTable.GetAsString(1));
    end;

    if Empty then
    begin
      SchemaNode := AddChildNode(Node, TDENodeSchema, itDriver, SDefaultSchema);
      SchemaNode.Expand(False);
    end;

  finally
    DbxTable.Free;
  end;
end;

procedure TDENodeConnection.CheckConnection;
var
  PackageName, ClassName: String;
  PosSep: Integer;
  OldCursor: TCursor;
begin
  if not FSQLConn.Connected then
  begin
    PosSep := Pos(',', FMetaDataPackageLoader);
    ClassName := Copy(FMetaDataPackageLoader, 1, PosSep-1);
    PackageName := Copy(FMetaDataPackageLoader, PosSep+1, Length(FMetaDataPackageLoader));

    OldCursor := Screen.Cursor;
    try
      FSQLConn.Open;
    finally
      Screen.Cursor := OldCursor;
    end;
    
    FNode.ImageIndex := Ord(itConnectionOpen);
    FNode.SelectedIndex := Ord(itConnectionOpen);

    LoadPackage(PackageName);
    
    if not TClassRegistry.GetClassRegistry.HasClass(ClassName) then
    begin
      FSQLConn.Close;
      raise Exception.Create('Can''t not find the package/class ' + FMetaDataPackageLoader)
    end
    else
    begin
      FMetadataCommandFactory := TDBXMetaDataCommandFactory(TClassRegistry.GetClassRegistry.CreateInstance(ClassName));
      if FMetadataCommandFactory is TDBXOracleMetaDataCommandFactory then
        FMetaReader := TDBXBaseMetaDataReaderOraWraper.Create
      else
        FMetaReader := TDBXBaseMetaDataReader(FMetadataCommandFactory.CreateMetaDataReader);

      FDBXContext := TDBXDataExpressProviderContext.Create;
      FDBXContext.Connection := FSQLConn.DBXConnection;
      FDBXContext.UseAnsiStrings := True;
      FDBXContext.RemoveIsNull := True;
      MetaReader.Context := FDBXContext;
    end;
  end;
end;

{ TDENodeTable }

procedure TDENodeTableView.InternalFillChildrens(Node: TTreeNodeDex);
begin
  Node.DeleteChildren;
  AddChildNode(Node, TDENodeTableViewColumns, itColumns, 'Columns');
  AddChildNode(Node, TDENodeIndices, itIndex, 'Indexes');
  AddChildNode(Node, TDENodeForeignKeys, itIndex, 'Foreign Keys');
end;

{ TDENodeView }

procedure TDENodeViewList.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Schema: String;
begin
  inherited;

  Schema := Node.Parent.Text;
  if Schema = SDefaultSchema then
    Schema := '';

  DbxTable := MetaReader.FetchTables('', Schema, '', TDBXTableType.View);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeTableView, itView, DbxTable.GetAsString(TDBXTablesIndex.TableName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeTable }

procedure TDENodeTableViewColumns.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Table, Schema: String;
begin
  inherited;

  Table := Node.Parent.Text;
  Schema := Node.Parent.Parent.Parent.Text;
  if Schema = SDefaultSchema then
    Schema := '';

  DbxTable := MetaReader.FetchColumns('', Schema, Table);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeNone, itColumns, DbxTable.GetAsString(TDBXColumnsIndex.ColumnName), False);
  finally
    DbxTable.Free;
  end;
end;

{ TDENode }

procedure TDENode.FillChildrens(Node: TTreeNodeDex);
begin
  Screen.Cursor := crSQLWait;
  try
    InternalFillChildrens(Node);
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TDENode.GetSchema(Node: TTreeNodeDex): String;
begin
  while Assigned(Node) and not (Node.DataObject is TDENodeSchema) do
    Node := TTreeNodeDex(Node.Parent);

  if Assigned(Node) then
    Result := Node.Text
  else
    Result := '';

  if Result = SDefaultSchema then
    Result := '';
end;

procedure TDENode.GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings);
begin
end;

procedure TDENode.InternalFillChildrens(Node: TTreeNodeDex);
begin
end;

procedure TDENode.ShowSourceCode(Node: TTreeNodeDex);
var
  Form: TExecuteQuery;
  SQL: TStrings;
begin
  SQL := TStringList.Create;
  try
    GetSQLForObject(Node, SQL);
    if Trim(SQL.Text) <> '' then
    begin
      Form := TExecuteQuery.Create(Connection, SQL.Text, Connection);
      Form.Show;
    end;
  finally
    SQL.Free;
  end;
end;

constructor TDENode.Create(Node: TTreeNodeDex);
begin
  inherited Create;
  FNode := Node;
  FMetaReader := nil;
end;

{ TDENodeSchema }

procedure TDENodeSchema.InternalFillChildrens(Node: TTreeNodeDex);
begin
  inherited;
  Node.DeleteChildren;
  AddChildNode(Node, TDENodeTableList, itTables, 'Tables');
  AddChildNode(Node, TDENodeViewList, itView, 'Views');
  AddChildNode(Node, TDENodeProcedureList, itProceduresFuncs, 'Procedures');
  AddChildNode(Node, TDENodeFunctionList, itProceduresFuncs, 'Functions');
  AddChildNode(Node, TDENodeSynonyms, itSynonyms, 'Synonyms');
  AddChildNode(Node, TDENodeSystemTables, itSysTable, 'System tables');
  AddChildNode(Node, TDENodePackageList, itSysTable, 'Packages');
end;

{ TDENodeForeignKeys }

procedure TDENodeForeignKeys.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Table: String;
begin
  inherited;
  Table := Node.Parent.Text;
  DbxTable := MetaReader.FetchForeignKeys('', GetSchema(Node), Table);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeNone, itColumns, DbxTable.GetAsString(TDBXForeignKeyColumnsIndex.ForeignKeyName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeIndices }

procedure TDENodeIndices.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Table: String;
begin
  inherited;

  Table := Node.Parent.Text;

  DbxTable := MetaReader.FetchIndexes('', GetSchema(Node), Table);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeIndicesColumns, itColumns, DbxTable.GetAsString(TDBXIndexColumnsIndex.IndexName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeTableList }

procedure TDENodeTableList.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;
  DbxTable := MetaReader.FetchTables('', GetSchema(Node), '', TDBXTableType.Table);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeTableView, itTables, DbxTable.GetAsString(TDBXTablesIndex.TableName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeIndicesColumns }

procedure TDENodeIndicesColumns.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Table: String;
begin
  inherited;

  Table := Node.Parent.Parent.Text;

  DbxTable := MetaReader.FetchIndexColumns('', GetSchema(Node), Table, Node.Text);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeNone, itColumns, DbxTable.GetAsString(TDBXIndexColumnsIndex.ColumnName), False);
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeForeignKeysColumns }

procedure TDENodeForeignKeysColumns.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
  Table: String;
begin
  inherited;

  Table := Node.Parent.Parent.Text;

  DbxTable := MetaReader.FetchForeignKeyColumns('', GetSchema(Node), Table, Node.Text, '', '', '', '');
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeNone, itColumns, DbxTable.GetAsString(TDBXForeignKeyColumnsIndex.ColumnName), False);
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeProcedureList }

procedure TDENodeProcedureList.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;

  DbxTable := MetaReader.FetchProcedures('', GetSchema(Node), '', TDBXProcedureType.ProcedureType);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeProcFunc, itProceduresFuncs, DbxTable.GetAsString(TDBXProceduresIndex.ProcedureName), False);
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeFunctionList }

procedure TDENodeFunctionList.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;

  DbxTable := MetaReader.FetchProcedures('', GetSchema(Node), '', TDBXProcedureType.FunctionType);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeProcFunc, itProceduresFuncs, DbxTable.GetAsString(TDBXProceduresIndex.ProcedureName), False);
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeSystemTables }

procedure TDENodeSystemTables.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;
  DbxTable := MetaReader.FetchTables('', GetSchema(Node), '', TDBXTableType.SystemTable);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeTableView, itSysTable, DbxTable.GetAsString(TDBXTablesIndex.TableName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeSynonyms }

procedure TDENodeSynonyms.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;
  DbxTable := MetaReader.FetchTables('', GetSchema(Node), '', TDBXTableType.Synonym);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodeTableView, itTables, DbxTable.GetAsString(TDBXTablesIndex.TableName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodePackages }

procedure TDENodePackageList.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;
  DbxTable := MetaReader.FetchPackages('', GetSchema(Node), '');
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodePackage, itTables, DbxTable.GetAsString(TDBXPackagesIndex.PackageName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodePackage }

procedure TDENodePackage.GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings);
var
  DbxTable: TDBXTableStorage;
begin
  DbxTable := MetaReader.FetchPackageSources('', GetSchema(Node), Node.Text);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      SQL.Append(DbxTable.GetAsString(TDBXPackageSourcesIndex.Definition));
  finally
    DbxTable.Free;
  end;
end;

procedure TDENodePackage.InternalFillChildrens(Node: TTreeNodeDex);
var
  DbxTable: TDBXTableStorage;
begin
  inherited;
  DbxTable := MetaReader.FetchPackageProcedures('', GetSchema(Node), Node.Text, '', '');
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      AddChildNode(Node, TDENodePackage, itProceduresFuncs, DbxTable.GetAsString(TDBXPackageProceduresIndex.ProcedureName));
  finally
    DbxTable.Free;
  end;
end;

{ TDENodeProcFunc }

procedure TDENodeProcFunc.GetSQLForObject(Node: TTreeNodeDex; SQL: TStrings);
var
  DbxTable: TDBXTableStorage;
begin
  DbxTable := MetaReader.FetchProcedureSources('', GetSchema(Node), Node.Text);
  try
    Node.DeleteChildren;
    while DbxTable.Next do
      SQL.Append(DbxTable.GetString(TDBXProcedureSourcesIndex.Definition));
  finally
    DbxTable.Free;
  end;
end;

{ TTreeNodeDexDex }

destructor TTreeNodeDex.Destroy;
begin
  DataObject.Free;
  inherited;
end;

initialization
  ConnectionAdmin := GetConnectionAdmin;

end.
