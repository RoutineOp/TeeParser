object CodeBrowser: TCodeBrowser
  Left = 0
  Top = 0
  Caption = 'CodeBrowser'
  ClientHeight = 362
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 280
    Top = 0
    Width = 391
    Height = 362
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object CodeMemo: TMemo
      Left = 0
      Top = 0
      Width = 391
      Height = 362
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
end
