object FindMethods: TFindMethods
  Left = 0
  Top = 0
  Caption = 'Find Method'
  ClientHeight = 359
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 267
    Width = 597
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 597
    Height = 41
    Align = alTop
    TabOrder = 0
    object Edit1: TEdit
      Left = 16
      Top = 11
      Width = 401
      Height = 21
      TabOrder = 0
      OnKeyPress = Edit1KeyPress
    end
    object Button1: TButton
      Left = 423
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Search'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 41
    Width = 597
    Height = 226
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 270
    Width = 597
    Height = 89
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
