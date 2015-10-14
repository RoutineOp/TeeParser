object CodeModules: TCodeModules
  Left = 0
  Top = 0
  Caption = 'Units'
  ClientHeight = 345
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 41
    Align = alTop
    TabOrder = 0
    object CBSection: TComboBox
      Left = 13
      Top = 10
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'By section'
      OnChange = CBSectionChange
      Items.Strings = (
        'By section'
        'Interface'
        'Implementation'
        'All')
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 138
    Width = 360
    Height = 207
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'By Name'
      object LBUnits: TListBox
        Left = 0
        Top = 0
        Width = 352
        Height = 179
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBUnitsClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'By Uses'
      ImageIndex = 1
      object ByUses: TTreeView
        Left = 0
        Top = 0
        Width = 352
        Height = 179
        Align = alClient
        Indent = 19
        TabOrder = 0
        OnChange = ByUsesChange
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Used By'
      ImageIndex = 2
      object UsedBy: TTreeView
        Left = 0
        Top = 0
        Width = 352
        Height = 179
        Align = alClient
        Indent = 19
        TabOrder = 0
        OnChange = UsedByChange
        OnExpanding = UsedByExpanding
      end
    end
  end
  object TreePackages: TTreeView
    Left = 0
    Top = 41
    Width = 360
    Height = 97
    Align = alTop
    Indent = 19
    TabOrder = 2
    OnClick = TreePackagesClick
  end
end
