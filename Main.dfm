object fMain: TfMain
  Left = 1335
  Top = 285
  Caption = #1069#1084#1091#1083#1103#1090#1086#1088' '#1082#1086#1085#1090#1088#1086#1083#1083#1077#1088#1086#1074
  ClientHeight = 417
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Verdana'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 468
    Height = 417
    ActivePage = TabSheet6
    Align = alClient
    HotTrack = True
    MultiLine = True
    Style = tsFlatButtons
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #1050#1042'-001 091'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label11: TLabel
        Left = 6
        Top = 25
        Width = 69
        Height = 14
        Caption = #1044#1077#1089'. '#1090#1086#1095#1082#1072
      end
      object SpeedButton6: TSpeedButton
        Left = 6
        Top = 0
        Width = 5
        Height = 5
        Cursor = crHandPoint
        Hint = #1042#1082#1083#1102#1095#1077#1085#1080#1077'/'#1074#1099#1082#1083#1102#1095#1077#1085#1080#1077' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1072
        AllowAllUp = True
        GroupIndex = 1
        Down = True
      end
      object Label12: TLabel
        Left = 6
        Top = 53
        Width = 55
        Height = 14
        Caption = #1059#1087#1088#1077#1078#1076'.'
      end
      object Label13: TLabel
        Left = 6
        Top = 81
        Width = 55
        Height = 14
        Caption = #1047#1072#1076#1072#1085#1080#1077
      end
      object Label14: TLabel
        Left = 6
        Top = 180
        Width = 87
        Height = 14
        Caption = #1058#1077#1082#1091#1097#1080#1081' '#1074#1077#1089':'
      end
      object Label15: TLabel
        Left = 96
        Top = 180
        Width = 8
        Height = 14
        Caption = '0'
      end
      object Label16: TLabel
        Left = 283
        Top = 220
        Width = 8
        Height = 14
        Alignment = taRightJustify
        Caption = '0'
      end
      object Label17: TLabel
        Left = 11
        Top = 220
        Width = 8
        Height = 14
        Caption = '0'
      end
      object TrackBar1: TTrackBar
        Left = 3
        Top = 200
        Width = 288
        Height = 25
        Max = 100
        SelEnd = 90
        TabOrder = 10
        ThumbLength = 16
        TickStyle = tsManual
      end
      object iLedRound6: TiLedRound
        Left = 0
        Top = 0
        Width = 5
        Height = 5
        Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1086#1073#1088#1072#1097#1077#1085#1080#1103' '#1082' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091
        BevelStyle = ibsNone
      end
      object stResIM4: TStaticText
        Left = 81
        Top = 22
        Width = 70
        Height = 22
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Verdana'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabOrder = 0
      end
      object Edit1: TEdit
        Left = 157
        Top = 22
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 1
      end
      object Button1: TButton
        Left = 233
        Top = 22
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 2
      end
      object StaticText1: TStaticText
        Left = 81
        Top = 50
        Width = 70
        Height = 22
        Hint = #1056#1077#1072#1083#1100#1085#1099#1081' '#1086#1090#1076#1086#1079#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1074#1077#1089
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 3
      end
      object Edit2: TEdit
        Left = 157
        Top = 50
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 4
      end
      object Button2: TButton
        Left = 233
        Top = 50
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 5
      end
      object StaticText2: TStaticText
        Left = 81
        Top = 78
        Width = 70
        Height = 22
        Hint = #1056#1077#1072#1083#1100#1085#1099#1081' '#1086#1090#1076#1086#1079#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1074#1077#1089
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 6
      end
      object Edit3: TEdit
        Left = 157
        Top = 78
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 7
      end
      object Button3: TButton
        Left = 233
        Top = 78
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 8
      end
      object Button4: TButton
        Left = 175
        Top = 180
        Width = 128
        Height = 17
        Caption = #1057#1073#1088#1086#1089#1080#1090#1100' '#1053#1045#1058#1058#1054
        TabOrder = 11
      end
      object GridPanel1: TGridPanel
        Left = 6
        Top = 110
        Width = 297
        Height = 60
        BevelInner = bvRaised
        BevelOuter = bvLowered
        ColumnCollection = <
          item
            Value = 49.999999999999960000
          end
          item
            Value = 50.000000000000040000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = RadioButton1
            Row = 0
          end
          item
            Column = 1
            Control = RadioButton2
            Row = 0
          end
          item
            Column = 0
            Control = RadioButton3
            Row = 1
          end
          item
            Column = 1
            Control = RadioButton4
            Row = 1
          end
          item
            Column = 0
            Control = RadioButton5
            Row = 2
          end>
        RowCollection = <
          item
            Value = 33.333333333333350000
          end
          item
            Value = 33.333333333333330000
          end
          item
            Value = 33.333333333333320000
          end>
        TabOrder = 12
        object RadioButton1: TRadioButton
          Left = 2
          Top = 2
          Width = 146
          Height = 18
          Align = alClient
          Caption = #1054#1078#1080#1076#1072#1085#1080#1077
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton2: TRadioButton
          Left = 148
          Top = 2
          Width = 147
          Height = 18
          Align = alClient
          Caption = #1044#1086#1079#1080#1088#1086#1074#1072#1085#1080#1077
          TabOrder = 1
        end
        object RadioButton3: TRadioButton
          Left = 2
          Top = 20
          Width = 146
          Height = 18
          Align = alClient
          Caption = #1055#1086#1076#1089#1099#1087#1082#1072
          TabOrder = 2
        end
        object RadioButton4: TRadioButton
          Left = 148
          Top = 20
          Width = 147
          Height = 18
          Align = alClient
          Caption = #1054#1096#1080#1073#1082#1072
          TabOrder = 3
        end
        object RadioButton5: TRadioButton
          Left = 2
          Top = 38
          Width = 146
          Height = 20
          Align = alClient
          Caption = #1047#1072#1074#1077#1088#1096#1077#1085#1086
          TabOrder = 4
        end
      end
      object CheckBox1: TCheckBox
        Left = 291
        Top = 201
        Width = 12
        Height = 12
        Checked = True
        State = cbChecked
        TabOrder = 13
      end
      object Edit9: TEdit
        Left = 17
        Top = 0
        Width = 200
        Height = 18
        TabStop = False
        AutoSize = False
        BevelInner = bvNone
        BevelWidth = 2
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 14
        Text = #1053#1072#1079#1074#1072#1085#1080#1077
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1052#1059' 110'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 53
      ExplicitWidth = 0
      ExplicitHeight = 360
    end
    object TabSheet3: TTabSheet
      Caption = #1052#1042' 110'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 53
      ExplicitWidth = 0
      ExplicitHeight = 360
    end
    object TabSheet4: TTabSheet
      Caption = #1052#1044#1042#1042
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 53
      ExplicitWidth = 0
      ExplicitHeight = 360
      object SpeedButton1: TSpeedButton
        Left = 6
        Top = 0
        Width = 5
        Height = 5
        Cursor = crHandPoint
        Hint = #1042#1082#1083#1102#1095#1077#1085#1080#1077'/'#1074#1099#1082#1083#1102#1095#1077#1085#1080#1077' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1072
        AllowAllUp = True
        GroupIndex = 1
        Down = True
      end
      object Label1: TLabel
        Left = 6
        Top = 24
        Width = 56
        Height = 14
        Caption = #1042#1099#1093#1086#1076#1099
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 232
        Top = 24
        Width = 45
        Height = 14
        Caption = #1042#1093#1086#1076#1099
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object iLedRound1: TiLedRound
        Left = 0
        Top = 0
        Width = 5
        Height = 5
        Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1086#1073#1088#1072#1097#1077#1085#1080#1103' '#1082' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091
        BevelStyle = ibsNone
      end
      object ScrollBox1: TScrollBox
        Left = 6
        Top = 48
        Width = 223
        Height = 271
        BorderStyle = bsNone
        TabOrder = 1
        object SpeedButton2: TSpeedButton
          Left = 38
          Top = 0
          Width = 19
          Height = 19
          Caption = #1050
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Layout = blGlyphBottom
          ParentFont = False
        end
        object SpeedButton3: TSpeedButton
          Tag = 32
          Left = 38
          Top = 22
          Width = 19
          Height = 19
          Caption = #1050
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Layout = blGlyphBottom
          ParentFont = False
        end
        object Label7: TLabel
          Left = 0
          Top = 2
          Width = 16
          Height = 14
          Alignment = taCenter
          AutoSize = False
          Caption = '0'
        end
        object Label8: TLabel
          Left = 0
          Top = 24
          Width = 16
          Height = 14
          Caption = '32'
        end
        object iLedRound2: TiLedRound
          Left = 19
          Top = 0
          Width = 19
          Height = 19
        end
        object iLedRound3: TiLedRound
          Tag = 32
          Left = 19
          Top = 22
          Width = 19
          Height = 19
        end
        object Edit4: TEdit
          Left = 63
          Top = 0
          Width = 156
          Height = 19
          TabStop = False
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkSoft
          BevelWidth = 2
          BorderStyle = bsNone
          TabOrder = 2
          Text = #1054#1087#1080#1089#1072#1085#1080#1077' '#1080#1079#1084#1077#1085#1103#1077#1090#1089#1103
        end
        object Edit5: TEdit
          Left = 63
          Top = 22
          Width = 156
          Height = 19
          TabStop = False
          AutoSize = False
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelWidth = 2
          BorderStyle = bsNone
          Color = clBtnFace
          TabOrder = 3
          Text = #1054#1087#1080#1089#1072#1085#1080#1077' '#1075#1086#1090#1086#1074#1086
        end
      end
      object Edit6: TEdit
        Left = 17
        Top = 0
        Width = 200
        Height = 18
        TabStop = False
        AutoSize = False
        BevelInner = bvNone
        BevelWidth = 2
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 2
        Text = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object ScrollBox2: TScrollBox
        Left = 234
        Top = 48
        Width = 223
        Height = 271
        BorderStyle = bsNone
        TabOrder = 3
        object SpeedButton4: TSpeedButton
          Left = 38
          Top = 0
          Width = 19
          Height = 19
          Caption = #1050
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Layout = blGlyphBottom
          ParentFont = False
        end
        object SpeedButton5: TSpeedButton
          Tag = 32
          Left = 38
          Top = 22
          Width = 19
          Height = 19
          Caption = #1050
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Layout = blGlyphBottom
          ParentFont = False
        end
        object Label3: TLabel
          Left = 0
          Top = 2
          Width = 16
          Height = 14
          Alignment = taCenter
          AutoSize = False
          Caption = '0'
        end
        object Label4: TLabel
          Left = 0
          Top = 24
          Width = 16
          Height = 14
          Caption = '32'
        end
        object iLedRound4: TiLedRound
          Left = 19
          Top = 0
          Width = 19
          Height = 19
        end
        object iLedRound5: TiLedRound
          Tag = 32
          Left = 19
          Top = 22
          Width = 19
          Height = 19
        end
        object Edit7: TEdit
          Left = 63
          Top = 0
          Width = 156
          Height = 19
          TabStop = False
          AutoSize = False
          BevelInner = bvNone
          BevelKind = bkSoft
          BevelWidth = 2
          BorderStyle = bsNone
          TabOrder = 2
          Text = #1054#1087#1080#1089#1072#1085#1080#1077' '#1080#1079#1084#1077#1085#1103#1077#1090#1089#1103
        end
        object Edit8: TEdit
          Left = 63
          Top = 22
          Width = 156
          Height = 19
          TabStop = False
          AutoSize = False
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelWidth = 2
          BorderStyle = bsNone
          Color = clBtnFace
          TabOrder = 3
          Text = #1054#1087#1080#1089#1072#1085#1080#1077' '#1075#1086#1090#1086#1074#1086
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = #1052#1072#1089#1090#1077#1088' 110.4'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 6
        Top = 25
        Width = 69
        Height = 14
        Caption = #1044#1077#1089'. '#1090#1086#1095#1082#1072
      end
      object SpeedButton7: TSpeedButton
        Left = 6
        Top = 0
        Width = 5
        Height = 5
        Cursor = crHandPoint
        Hint = #1042#1082#1083#1102#1095#1077#1085#1080#1077'/'#1074#1099#1082#1083#1102#1095#1077#1085#1080#1077' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1072
        AllowAllUp = True
        GroupIndex = 1
        Down = True
      end
      object Label6: TLabel
        Left = 6
        Top = 53
        Width = 55
        Height = 14
        Caption = #1059#1087#1088#1077#1078#1076'.'
      end
      object Label9: TLabel
        Left = 6
        Top = 81
        Width = 55
        Height = 14
        Caption = #1047#1072#1076#1072#1085#1080#1077
      end
      object Label10: TLabel
        Left = 9
        Top = 268
        Width = 87
        Height = 14
        Caption = #1058#1077#1082#1091#1097#1080#1081' '#1074#1077#1089':'
      end
      object Label18: TLabel
        Left = 99
        Top = 268
        Width = 8
        Height = 14
        Caption = '0'
      end
      object Label19: TLabel
        Left = 286
        Top = 308
        Width = 8
        Height = 14
        Alignment = taRightJustify
        Caption = '0'
      end
      object Label20: TLabel
        Left = 14
        Top = 308
        Width = 8
        Height = 14
        Caption = '0'
      end
      object iLedRound7: TiLedRound
        Left = 0
        Top = 0
        Width = 5
        Height = 5
        Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1086#1073#1088#1072#1097#1077#1085#1080#1103' '#1082' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091
        BevelStyle = ibsNone
      end
      object StaticText3: TStaticText
        Left = 81
        Top = 22
        Width = 70
        Height = 22
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Verdana'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabOrder = 1
      end
      object Edit10: TEdit
        Left = 157
        Top = 22
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 2
      end
      object Button5: TButton
        Left = 233
        Top = 22
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 3
      end
      object StaticText4: TStaticText
        Left = 81
        Top = 50
        Width = 70
        Height = 22
        Hint = #1056#1077#1072#1083#1100#1085#1099#1081' '#1086#1090#1076#1086#1079#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1074#1077#1089
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 4
      end
      object Edit11: TEdit
        Left = 157
        Top = 50
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 5
      end
      object Button6: TButton
        Left = 233
        Top = 50
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 6
      end
      object StaticText5: TStaticText
        Left = 81
        Top = 78
        Width = 70
        Height = 22
        Hint = #1056#1077#1072#1083#1100#1085#1099#1081' '#1086#1090#1076#1086#1079#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1074#1077#1089
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 7
      end
      object Edit12: TEdit
        Left = 157
        Top = 78
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 8
      end
      object Button7: TButton
        Left = 233
        Top = 78
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 9
      end
      object TrackBar2: TTrackBar
        Left = 6
        Top = 288
        Width = 288
        Height = 25
        Max = 100
        SelEnd = 90
        TabOrder = 10
        ThumbLength = 16
        TickStyle = tsManual
      end
      object Button8: TButton
        Left = 178
        Top = 268
        Width = 128
        Height = 17
        Caption = #1057#1073#1088#1086#1089#1080#1090#1100' '#1053#1045#1058#1058#1054
        TabOrder = 11
      end
      object GridPanel2: TGridPanel
        Left = 6
        Top = 110
        Width = 297
        Height = 141
        BevelInner = bvRaised
        BevelOuter = bvLowered
        ColumnCollection = <
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = CheckBox3
            Row = 0
          end
          item
            Column = 0
            Control = CheckBox4
            Row = 1
          end
          item
            Column = 0
            Control = CheckBox5
            Row = 2
          end
          item
            Column = 0
            Control = CheckBox6
            Row = 3
          end
          item
            Column = 0
            Control = CheckBox7
            Row = 4
          end
          item
            Column = 0
            Control = CheckBox8
            Row = 5
          end
          item
            Column = 0
            Control = CheckBox9
            Row = 6
          end
          item
            Column = 0
            Control = CheckBox10
            Row = 7
          end>
        RowCollection = <
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end
          item
            Value = 12.500000000000000000
          end>
        TabOrder = 12
        object CheckBox3: TCheckBox
          Left = 2
          Top = 2
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1057#1090#1072#1088#1090' '#1076#1086#1079#1080#1088#1086#1074#1072#1085#1080#1103
          TabOrder = 0
        end
        object CheckBox4: TCheckBox
          Left = 2
          Top = 19
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1048#1076#1105#1090' '#1088#1072#1079#1075#1088#1091#1079#1082#1072
          TabOrder = 1
        end
        object CheckBox5: TCheckBox
          Left = 2
          Top = 36
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1054#1078#1080#1076#1072#1085#1080#1077' '#1087#1088#1086#1076#1086#1083#1078#1077#1085#1080#1103' '#1076#1086#1079#1080#1088#1086#1074#1072#1085#1080#1103
          TabOrder = 2
        end
        object CheckBox6: TCheckBox
          Left = 2
          Top = 53
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1048#1076#1105#1090' '#1076#1086#1079#1080#1088#1086#1074#1072#1085#1080#1077
          TabOrder = 3
        end
        object CheckBox7: TCheckBox
          Left = 2
          Top = 70
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1044#1086#1079#1072' '#1085#1072#1073#1088#1072#1085#1072
          TabOrder = 4
        end
        object CheckBox8: TCheckBox
          Left = 2
          Top = 87
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1048#1085#1076#1080#1082#1072#1094#1080#1103' '#1074#1077#1089#1072' '#1053#1045#1058#1058#1054
          TabOrder = 5
        end
        object CheckBox9: TCheckBox
          Left = 2
          Top = 104
          Width = 293
          Height = 17
          Align = alClient
          Caption = #1042#1082#1083#1102#1095#1077#1085#1072' '#1082#1072#1083#1080#1073#1088#1086#1074#1082#1072' '#1074#1088#1077#1084#1077#1085#1080' '#1091#1087#1088#1077#1078#1076#1077#1085#1080#1103
          TabOrder = 6
        end
        object CheckBox10: TCheckBox
          Left = 2
          Top = 121
          Width = 293
          Height = 18
          Align = alClient
          Caption = #1042#1077#1089' '#1079#1072#1092#1080#1082#1089#1080#1088#1086#1074#1072#1085
          TabOrder = 7
        end
      end
      object CheckBox2: TCheckBox
        Left = 294
        Top = 289
        Width = 12
        Height = 12
        Checked = True
        State = cbChecked
        TabOrder = 13
      end
      object Edit13: TEdit
        Left = 17
        Top = 0
        Width = 200
        Height = 18
        TabStop = False
        AutoSize = False
        BevelInner = bvNone
        BevelWidth = 2
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 14
        Text = #1053#1072#1079#1074#1072#1085#1080#1077
      end
    end
    object TabSheet6: TTabSheet
      Caption = #1050#1043#1044
      ImageIndex = 5
      object Label21: TLabel
        Left = 6
        Top = 25
        Width = 39
        Height = 14
        Caption = #1042#1088#1077#1084#1103
      end
      object SpeedButton8: TSpeedButton
        Left = 6
        Top = 0
        Width = 5
        Height = 5
        Cursor = crHandPoint
        Hint = #1042#1082#1083#1102#1095#1077#1085#1080#1077'/'#1074#1099#1082#1083#1102#1095#1077#1085#1080#1077' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1072
        AllowAllUp = True
        GroupIndex = 1
        Down = True
      end
      object Label24: TLabel
        Left = 6
        Top = 84
        Width = 119
        Height = 14
        Caption = #1054#1073#1088#1072#1090#1085#1099#1081' '#1086#1090#1089#1095#1105#1090':'
      end
      object Label25: TLabel
        Left = 128
        Top = 84
        Width = 8
        Height = 14
        Caption = '0'
      end
      object iLedRound8: TiLedRound
        Left = 0
        Top = 0
        Width = 5
        Height = 5
        Hint = #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1086#1073#1088#1072#1097#1077#1085#1080#1103' '#1082' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091
        BevelStyle = ibsNone
      end
      object StaticText6: TStaticText
        Left = 81
        Top = 22
        Width = 70
        Height = 22
        Alignment = taCenter
        AutoSize = False
        BorderStyle = sbsSunken
        Color = clBlack
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Verdana'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabOrder = 1
      end
      object Edit14: TEdit
        Left = 157
        Top = 22
        Width = 70
        Height = 22
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        BevelWidth = 4
        BorderStyle = bsNone
        TabOrder = 2
      end
      object Button9: TButton
        Left = 233
        Top = 22
        Width = 70
        Height = 22
        Caption = #1047#1072#1076#1072#1090#1100
        TabOrder = 3
      end
      object Button12: TButton
        Left = 193
        Top = 84
        Width = 92
        Height = 17
        Caption = #1047#1072#1074#1077#1088#1096#1080#1090#1100
        TabOrder = 4
      end
      object GridPanel3: TGridPanel
        Left = 6
        Top = 54
        Width = 297
        Height = 20
        BevelInner = bvRaised
        BevelOuter = bvLowered
        ColumnCollection = <
          item
            Value = 49.999999999999960000
          end
          item
            Value = 50.000000000000040000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = RadioButton6
            Row = 0
          end
          item
            Column = 1
            Control = RadioButton7
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 5
        object RadioButton6: TRadioButton
          Left = 2
          Top = 2
          Width = 146
          Height = 16
          Align = alClient
          Caption = #1054#1078#1080#1076#1072#1085#1080#1077
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton7: TRadioButton
          Left = 148
          Top = 2
          Width = 147
          Height = 16
          Align = alClient
          Caption = #1048#1079#1084#1077#1088#1077#1085#1080#1077
          TabOrder = 1
        end
      end
      object CheckBox11: TCheckBox
        Left = 291
        Top = 86
        Width = 12
        Height = 12
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object Edit17: TEdit
        Left = 17
        Top = 0
        Width = 200
        Height = 18
        TabStop = False
        AutoSize = False
        BevelInner = bvNone
        BevelWidth = 2
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 7
        Text = #1053#1072#1079#1074#1072#1085#1080#1077
      end
    end
  end
  object alMain: TActionList
    Left = 396
    Top = 8
    object actClose: TAction
      Caption = #1042#1099#1093#1086#1076
      Hint = #1042#1099#1093#1086#1076' '#1080#1079' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      ShortCut = 16465
      OnExecute = actCloseExecute
    end
  end
end
