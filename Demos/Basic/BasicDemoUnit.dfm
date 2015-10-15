object PascalDemo: TPascalDemo
  Left = 0
  Top = 0
  Caption = 'TeePascal Demo'
  ClientHeight = 510
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 232
    ExplicitTop = 256
    ExplicitWidth = 185
    object LTime: TLabel
      Left = 200
      Top = 11
      Width = 3
      Height = 13
    end
    object BParseVCL: TButton
      Left = 24
      Top = 10
      Width = 129
      Height = 25
      Caption = '&Parse VCL Forms.pas'
      Enabled = False
      TabOrder = 0
      OnClick = BParseVCLClick
    end
  end
  object ListIDEs: TListBox
    Left = 0
    Top = 41
    Width = 193
    Height = 469
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListIDEsClick
  end
  object Panel2: TPanel
    Left = 193
    Top = 41
    Width = 431
    Height = 469
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 209
    ExplicitTop = 47
    object Log: TMemo
      Left = 16
      Top = 0
      Width = 393
      Height = 113
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object BModules: TButton
      Left = 16
      Top = 144
      Width = 75
      Height = 25
      Caption = '&Browse...'
      Enabled = False
      TabOrder = 1
      OnClick = BModulesClick
    end
  end
end
