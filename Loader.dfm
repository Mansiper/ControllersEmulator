object fLoader: TfLoader
  Left = 1463
  Top = 314
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1042#1099#1073#1086#1088' '#1101#1084#1091#1083#1103#1090#1086#1088#1072
  ClientHeight = 275
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    348
    275)
  PixelsPerInch = 96
  TextHeight = 14
  object Label2: TLabel
    Left = 191
    Top = 4
    Width = 134
    Height = 14
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1101#1084#1091#1083#1103#1090#1086#1088#1072
  end
  object Label3: TLabel
    Left = 18
    Top = 202
    Width = 313
    Height = 14
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = #1042#1099#1073#1086#1088' '#1101#1084#1091#1083#1103#1090#1086#1088#1072
  end
  object lbDevices: TListBox
    Left = 2
    Top = 2
    Width = 183
    Height = 194
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 14
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object cbDevices: TComboBox
    Left = 191
    Top = 56
    Width = 151
    Height = 22
    Style = csDropDownList
    TabOrder = 2
    Items.Strings = (
      #1052#1044#1042#1042
      #1052#1042'110-16'#1044
      #1052#1042'110-32'#1044#1053
      #1052#1059'110-32'#1056
      #1050#1042'-001 v081'
      #1050#1042'-001 v091'
      #1050#1042'-001 '#1044
      #1050#1042'-001 v11.02'
      #1055#1058#1062'-001'
      #1052#1072#1089#1090#1077#1088' 110.4'
      #1050#1043#1044)
  end
  object bDevAdd: TButton
    Left = 191
    Top = 82
    Width = 151
    Height = 23
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1086
    TabOrder = 3
    OnClick = bDevAddClick
  end
  object bDevDel: TButton
    Left = 191
    Top = 120
    Width = 151
    Height = 23
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1086
    TabOrder = 4
    OnClick = bDevDelClick
  end
  object eName: TEdit
    Left = 191
    Top = 18
    Width = 151
    Height = 21
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnChange = eNameChange
  end
  object cbEmulators: TComboBox
    Left = 18
    Top = 218
    Width = 313
    Height = 21
    Cursor = crHandPoint
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = cbEmulatorsChange
  end
  object bOk: TButton
    Left = 97
    Top = 245
    Width = 75
    Height = 23
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = #1042#1099#1073#1088#1072#1090#1100
    Default = True
    TabOrder = 5
    OnClick = bOkClick
  end
  object bCancel: TButton
    Left = 179
    Top = 245
    Width = 75
    Height = 23
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 6
  end
  object bEmulDel: TButton
    Left = 330
    Top = 202
    Width = 17
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'x'
    TabOrder = 7
    OnClick = bEmulDelClick
  end
end
