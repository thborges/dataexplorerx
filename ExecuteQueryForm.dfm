object ExecuteQuery: TExecuteQuery
  Left = 0
  Top = 0
  Caption = 'Execute Query'
  ClientHeight = 252
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 123
    Width = 480
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 129
  end
  object MemoSQL: TMemo
    Left = 0
    Top = 23
    Width = 480
    Height = 100
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 480
    Height = 23
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    ColorMap.HighlightColor = 15660791
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 15660791
    Spacing = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 128
    Width = 480
    Height = 124
    Align = alBottom
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actExecuteDirect
            Caption = '&Execute Direct'
          end
          item
            Action = actExecute
            Caption = '&Select'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 232
    Top = 128
    StyleName = 'XP Style'
    object actExecuteDirect: TAction
      Caption = 'Execute Direct'
      OnExecute = actExecuteDirectExecute
    end
    object actExecute: TAction
      Caption = 'Select'
      OnExecute = actExecuteExecute
    end
  end
  object SimpleDataSet: TSimpleDataSet
    Aggregates = <>
    DataSet.MaxBlobSize = -1
    DataSet.Params = <>
    PacketRecords = 100
    Params = <>
    AfterOpen = SimpleDataSetAfterOpen
    Left = 224
    Top = 56
  end
  object DataSource1: TDataSource
    DataSet = SimpleDataSet
    Left = 256
    Top = 56
  end
end
