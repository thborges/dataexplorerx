unit MetaDataWrapers;

interface

uses
  DBXOracleMetaDataReader,
  DBXMetaDataNames,
  DBXMetaDataReader,
  DBXPlatformUtil,
  DBXTableStorage;

type

  TDBXBaseMetaDataReaderOraWraper = class(TDBXOracleMetaDataReader)
  public
    function FetchProcedureSources(const Catalog, Schema,
      &Procedure: WideString): TDBXTableStorage; override;
  end;

implementation

function TDBXBaseMetaDataReaderOraWraper.FetchProcedureSources(const Catalog: WideString; const Schema: WideString; const &Procedure: WideString): TDBXTableStorage;
var
  ParameterNames: TDBXWideStringArray;
  ParameterValues: TDBXWideStringArray;
  Cursor: TDBXTableStorage;
  Columns: TDBXColumnDescriptorArray;
begin
  SetLength(ParameterNames,3);
  ParameterNames[0] := TDBXParameterName.CatalogName;
  ParameterNames[1] := TDBXParameterName.SchemaName;
  ParameterNames[2] := TDBXParameterName.ProcedureName;
  SetLength(ParameterValues,3);
  ParameterValues[0] := Catalog;
  ParameterValues[1] := Schema;
  ParameterValues[2] := &Procedure;
  Cursor := FContext.ExecuteQuery(SqlForProcedureSources, ParameterNames, ParameterValues);
  Columns := TDBXMetaDataCollectionColumns.CreateProcedureSourcesColumns;
  Result := TDBXBaseMetaDataReader.TDBXSanitizedTableCursor.Create(FContext, TDBXMetaDataCollectionIndex.ProcedureSources, TDBXMetaDataCollectionName.ProcedureSources, Columns, Cursor);
end;

end.
