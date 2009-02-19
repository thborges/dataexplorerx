unit ExecuteQueryForm;

interface

uses
  {$IFNDEF TESTTOOL} DockForm, {$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, StdCtrls, Grids, DBGrids, ExtCtrls, DB,
  DBClient, SimpleDS, ActnList, XPStyleActnCtrls, SQLExpr;

type
  {$IFDEF TESTTOOL}
//  TEditWindow = TForm;
  {$ENDIF}

  TExecuteQuery = class(TForm)
    MemoSQL: TMemo;
    ActionToolBar1: TActionToolBar;
    Splitter1: TSplitter;
    DBGrid1: TDBGrid;
    ActionManager1: TActionManager;
    actExecuteDirect: TAction;
    actExecute: TAction;
    SimpleDataSet: TSimpleDataSet;
    DataSource1: TDataSource;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actExecuteDirectExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure SimpleDataSetAfterOpen(DataSet: TDataSet);
  private
  protected
    procedure OnGetText(Sender: TField; var Text: string; DisplayText: Boolean); virtual;
  public
    constructor Create(Connection: TSQLConnection; SQL: String; AOwner: TComponent); reintroduce;
  end;

var
  ExecuteQuery: TExecuteQuery;

implementation

{$R *.dfm}

{ TExecuteQuery }

procedure TExecuteQuery.actExecuteDirectExecute(Sender: TObject);
begin
//
end;

procedure TExecuteQuery.actExecuteExecute(Sender: TObject);
begin
  SimpleDataSet.Close;

  if MemoSQL.SelLength > 0 then
    SimpleDataSet.DataSet.CommandText := Copy(MemoSQL.Text, MemoSQL.SelStart, MemoSQL.SelLength)
  else
    SimpleDataSet.DataSet.CommandText := MemoSQL.Text;

  SimpleDataSet.Active := True;
end;

constructor TExecuteQuery.Create(Connection: TSQLConnection; SQL: String; AOwner: TComponent);
begin
  inherited Create(AOwner);
  SimpleDataSet.Connection := Connection;
  MemoSQL.Text := SQL;
end;

procedure TExecuteQuery.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TExecuteQuery.OnGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

procedure TExecuteQuery.SimpleDataSetAfterOpen(DataSet: TDataSet);
var
  i: Integer;
begin
  for i := 0 to DataSet.Fields.Count - 1 do
  begin
    if DataSet.Fields[i].DataType in [ftBlob, ftMemo, ftWideMemo, ftOraClob] then
      DataSet.Fields[i].OnGetText := OnGetText;
  end;
end;

end.
