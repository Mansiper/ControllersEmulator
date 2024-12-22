unit Devices;
(*
  Для передачи массива указателем возможно использование только статических массивов!

  SBox.Tag - 1 для входов, 0 для выходов
  SB.Tag и Led.Tag - номер входа/выхода устройств ввода-вывода
*)
(*  XML с настройками устройств данного эмулятора
  <Devices>
    <FormSettings height width />
    <Device address name />
      <Exit num caption />
      <Enter num caption />
      <Param num value />
      <AutoDos checked />
    </Device>
  </Devices>
*)

interface

uses Classes, ComCtrls, Generics.Collections, iLedRound, iTypes, Buttons, Controls, SysUtils,
  StdCtrls, Graphics, Forms, ExtCtrls, XMLDoc, XMLIntf, Variants, Windows;

type
    //Типы устройств
  TDevTypes = (dtNone,
    dtDIOM,
    dtDIM_16D, dtDIM_32DN,
    dtDOM_32R,
    dtKB001_081, dtKB001_091, dtKB001_D, dtKB001_1102, dtPTC001,
    dtMaster110_4,
    dtKGD);

  TSomeClass = class
    class procedure OnEditedLabelEnter(Sender: TObject);
    class procedure OnEditedLabelExit(Sender: TObject);
  end;
    //Устройство
  TDevice = class(TTabSheet)
  private
    FAddress: Byte;
    FName: String;
    FDevType: TDevTypes;
    FIsInWork: Boolean;
    FIndicator: TiLedRound;
    FNameControl: TEdit;

    procedure OnDeviceTurn(Sender: TObject);  //Выкл/Вкл опроса устройства по кнопке
  public
    constructor Create(AOwner: TComponent); override;

    property Address: Byte read FAddress write FAddress;
    property Name: String read FName write FName;
    property DevType: TDevTypes read FDevType write FDevType;
    property IsInWork: Boolean read FIsInWork write FIsInWork;

    function Process(Request: Array of Byte): String; virtual; abstract;  //Обработка запроса устройству, создание строки ответа
  end;

  TDIOM = class(TDevice)
  private
    const
      cInCnt = 12;
      cOutCnt = 8;
    var
      FIns: Array of Boolean;
      FOuts: Array of Boolean;
      FInsParent: TWinControl;
      FOutsParent: TWinControl;

    function GetIns(Index: Byte): Boolean;
    procedure SetIns(Index: Byte; const Value: Boolean);
    function GetOuts(Index: Byte): Boolean;
    procedure SetOuts(Index: Byte; const Value: Boolean);

    procedure OnBtnInClick(Sender: TObject);  //Действие на переключение входа по кнопке
    procedure OnBtnOutClick(Sender: TObject); //Действие на переключение выхода по кнопке

    procedure SetOutVals(Value: Byte);  //Запись новых значений в выходы
    procedure UpdOutLeds; //Обновление индикаторов выходов
    function GetInVals: Word;  //Возвращает значение включенных входов
    function GetOutVals: Word;  //Возвращает значение включенных выходов

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Ins[Index: Byte]: Boolean read GetIns write SetIns;
    property Outs[Index: Byte]: Boolean read GetOuts write SetOuts;

    function Process(Request: Array of Byte): String; override;
  end;

  TDIM = class(TDevice)
  private
    FIns: Array of Boolean;
    FInsParent: TWinControl;

    function GetIns(Index: Byte): Boolean;
    procedure SetIns(Index: Byte; const Value: Boolean);

    procedure OnBtnInClick(Sender: TObject);  //Действие на переключение входа по кнопке

    procedure LoadSettings;
  public
    destructor Destroy; override;

    property Ins[Index: Byte]: Boolean read GetIns write SetIns;
  end;

  TDIM_16D = class(TDIM)
  private
    const
      cInCnt = 16;

    function GetInVals: Word;  //Возвращает значение включенных входов
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    function Process(Request: Array of Byte): String; override;
  end;

  TDIM_32DN = class(TDIM)
  private
    const
      cInCnt = 32;

    function GetInVals(Reg: Byte): Word;  //Возвращает значение включенных входов
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);

    function Process(Request: Array of Byte): String; override;
  end;

  TDOM = class(TDevice)
  private
    FOuts: Array of Boolean;
    FOutsParent: TWinControl;

    function GetOuts(Index: Byte): Boolean;
    procedure SetOuts(Index: Byte; const Value: Boolean);

    procedure OnBtnOutClick(Sender: TObject); //Действие на переключение выхода по кнопке

    procedure LoadSettings;
  public
    destructor Destroy; override;

    property Outs[Index: Byte]: Boolean read GetOuts write SetOuts;
  end;

  TDOM_32R = class(TDOM)
  private
    const
      cOutCnt = 32;

    procedure SetOutVals(Value: Word; Reg: Byte); //Запись новых значений в выходы
    procedure UpdOutLeds; //Обновление индикаторов выходов
    function GetOutVals(Reg: Byte): Word; //Возвращает значение включенных выходов
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);

    function Process(Request: Array of Byte): String; override;
  end;

  TKB001_081 = class(TDevice)
  private
    type
      TStates = (s081vWaiting=2, s081vDosing=3, s081vImpulse=5, s081vError=7);
      TRegs = (r081vWeight=0, r081vADC=2, r081vState=3, r081vADCRange=3, r081vADCFrequency=4,
        r081vDiscreteIns=4, r081vFloatDigits=5, r081vMaxWeight=6, r081vCalibrWeight=8,
        r081vCalibrCoeff=10, r081vZeroCode=12, r081vTarget=14, r081vAdvanceHard=16,
        r081vAdvanceSoft=18, r081vZeroZone=20, r081vZeroZoneMax=22, r081vZeroTime=24,
        r081vDecayTime=26, r081vDosingMode=28, r081vFilterSize1=28, r081vFilterSize2=29,
        r081vNetNumber=29, r081vNetSpeed=30, r081vControlMode=30, r081vAutoZeroing=31,
        r081vShippedWeight=40, r081vWeightCount=42, r081vSensorPower=44);
      TCommands = (c081vResetDose=1, c081vStartDosing=2, c081vStopDosing=3);
    var
      FDecimal: Byte;     //Десятичная точка
      FAdvance: Single;   //Упреждение
      FTask: Single;      //Задание
      FState: TStates;    //Состояние
      FWeightCurrent: Single; //Текущий вес
      FWeightTotal: Single;   //Общий вес

      FAutoDosing: TTimer;  //Автоматическое дозирование

      FParentDec: TControl;       //Кнопка записи десятичной точки
      FParentAdv: TControl;       //Кнопка записи упреждения
      FParentTask: TControl;      //Кнопка записи задания
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentWeight: TControl;    //Связующий компонент с компонентами изменения веса

    procedure SetState(Value: TStates);
    procedure SetDecimal(Value: Byte);
    procedure SetAdvance(Value: Single);
    procedure SetTask(Value: Single);

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateRBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnWeightTBChange(Sender: TObject);  //Действие на изменение веса ползунком
    procedure OnWeightBtnClick(Sender: TObject);  //Действие на нажатие кнопки сброса веса
    procedure OnAutoDosing(Sender: TObject);  //Автоматическое дозирование

    function BytesToSingle(Data: Array of Byte): Single;
    function SingleToBytes(Value: Single): TBytes;

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Decimal: Byte read FDecimal write SetDecimal;
    property Advance: Single read FAdvance write SetAdvance;
    property Task: Single read FTask write SetTask;
    property State: TStates read FState write SetState;
    property WeightCurrent: Single read FWeightCurrent write FWeightCurrent;
    property WeightTotal: Single read FWeightTotal write FWeightTotal;

    function Process(Request: Array of Byte): String; override;
  end;

  TKB001_091 = class(TDevice)
  private
    type
      TStates = (s091vWaiting=2, s091vDosing=3, s091vImpulse=5, s091vError=7);
      TRegs = (r091vWeight=0, r091vADC=2, r091vState=3, r091vDiscreteOuts = 4,
        r091vDiscreteIns = 104, r091vADCRange=10, r091vADCFrequency=10, r091vDiscreteness=11,
        r091vFloatDigits=12, r091vPolarity=12, r091vMaxWeight=13, r091vCalibrWeight=15,
        r091vCalibrCoeff=17, r091vZeroCode=19, r091vTarget1=30, r091vTarget2=32, r091vTarget3=34,
        r091vTarget4=36, r091vTarget5=38, r091vTarget6=40, r091vTarget7=42, r091vTarget8=44,
        r091vTarget9=46, r091vAdvanceHard=48, r091vAdvanceSoft=50, r091vTare1=52, r091vTare2=54,
        r091vTare3=56, r091vTare4=58, r091vTare5=60, r091vTare6=62, r091vTare7=64, r091vTare8=66,
        r091vTare9=68, r091vTarePrecisionRange=70, r091vCurrentTarget=72, r091vZeroTime=85,
        r091vDecayTime=87, r091vDosingMode=89, r091vSoftOutMode=90, r091vFilterSize1=91,
        r091vFilterSize2=92, r091vNetNumber=93, r091vNetSpeed=94, r091vControlMode=95,
        r091vAutoZeroing=96, r091vWeightCount=125, r091vDosingCount=127, r091vWeightLast=128);
      TCommands = (c091vResetDose=1, c091vStartDosing=2, c091vStopDosing=3);
    var
      FDecimal: Byte;     //Десятичная точка
      FAdvance: Single;   //Упреждение
      FTask: Single;      //Задание
      FState: TStates;    //Состояние
      FWeightCurrent: Single; //Текущий вес
      FWeightTotal: Single;   //Общий вес

      FAutoDosing: TTimer;  //Автоматическое дозирование

      FParentDec: TControl;       //Кнопка записи десятичной точки
      FParentAdv: TControl;       //Кнопка записи упреждения
      FParentTask: TControl;      //Кнопка записи задания
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentWeight: TControl;    //Связующий компонент с компонентами изменения веса

    procedure SetState(Value: TStates);
    procedure SetDecimal(Value: Byte);
    procedure SetAdvance(Value: Single);
    procedure SetTask(Value: Single);

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateRBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnWeightTBChange(Sender: TObject);  //Действие на изменение веса ползунком
    procedure OnWeightBtnClick(Sender: TObject);  //Действие на нажатие кнопки сброса веса
    procedure OnAutoDosing(Sender: TObject);  //Автоматическое дозирование

    function BytesToSingle(Data: Array of Byte): Single;
    function SingleToBytes(Value: Single): TBytes;

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Decimal: Byte read FDecimal write SetDecimal;
    property Advance: Single read FAdvance write SetAdvance;
    property Task: Single read FTask write SetTask;
    property State: TStates read FState write SetState;
    property WeightCurrent: Single read FWeightCurrent write FWeightCurrent;
    property WeightTotal: Single read FWeightTotal write FWeightTotal;

    function Process(Request: Array of Byte): String; override;
  end;

  TKB001_1102 = class(TDevice)
  private
    type
      TStates = (s1102vMenu=0, s1102vCalibr=1, s1102vWaiting=2, s1102vDosing1=3,
        s1102vPause1=4, s1102vImpulse1=5, s1102vDosing2=6, s1102vPause2=7, s1102vImpulse2=8,
        s1102vDosing3=9, s1102vPause3=10, s1102vImpulse3=11, s1102vWeightGone=12,
        s1102vDosingFinished=13, s1102vUnload=14, s1102vError=15, s1102vUnknownError=255);
      TRegs = (r1102vWeight=0, r1102vADC=2, r1102vState=3, r1102vADCRange=3,
        r1102vADCFrequency=4, r1102vDiscreteness=104, r1102vFloatDigits=5, r1102vMaxWeight=6,
        r1102vCalibrWeight=8, r1102vCalibrCoeff=10, r1102vZeroCode=12, r1102vTarget1=14,
        r1102vTarget2=16, r1102vTarget3=18, r1102vAdvanceHard1=20, r1102vAdvanceHard2=22,
        r1102vAdvanceHard3=24, r1102vAdvanceSoft1=26, r1102vAdvanceSoft2=28, r1102vAdvanceSoft3=30,
        r1102vImpulsePause1=32, r1102vImpulsePause2=34, r1102vImpulsePause3=36, r1102vZeroZone=38,
        r1102vZeroTime=40, r1102vDecayTime=42, r1102vFilterSize1=44, r1102vFilterSize2=45,
        r1102vNetNumber=145, r1102vNetSpeed=46, r1102vDiscreteOperMode=146, r1102vAutoZeroing=47,
        r1102vWeightGoneTime=147);
      TCommands = (c1102vResetDose=1, c1102vStartDosing=2, c1102vStopDosing=4);
    var
      FDecimal: Byte;     //Десятичная точка
      FAdvance: Single;   //Упреждение
      FTask: Single;      //Задание
      FState: TStates;    //Состояние
      FWeightCurrent: Single; //Текущий вес
      FWeightTotal: Single;   //Общий вес

      FAutoDosing: TTimer;  //Автоматическое дозирование

      FParentDec: TControl;       //Кнопка записи десятичной точки
      FParentAdv: TControl;       //Кнопка записи упреждения
      FParentTask: TControl;      //Кнопка записи задания
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentWeight: TControl;    //Связующий компонент с компонентами изменения веса

    procedure SetState(Value: TStates);
    procedure SetDecimal(Value: Byte);
    procedure SetAdvance(Value: Single);
    procedure SetTask(Value: Single);

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateRBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnWeightTBChange(Sender: TObject);  //Действие на изменение веса ползунком
    procedure OnWeightBtnClick(Sender: TObject);  //Действие на нажатие кнопки сброса веса
    procedure OnAutoDosing(Sender: TObject);  //Автоматическое дозирование

    function BytesToSingle(Data: Array of Byte): Single;
    function SingleToBytes(Value: Single): TBytes;

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Decimal: Byte read FDecimal write SetDecimal;
    property Advance: Single read FAdvance write SetAdvance;
    property Task: Single read FTask write SetTask;
    property State: TStates read FState write SetState;
    property WeightCurrent: Single read FWeightCurrent write FWeightCurrent;
    property WeightTotal: Single read FWeightTotal write FWeightTotal;

    function Process(Request: Array of Byte): String; override;
  end;

  TPTC001 = class(TDevice)
  private
    type
      TStates = (sPTCvWaiting=0, sPTCvDosing=1, sPTCvImpulse=2, sPTCvUnknownError=255);
      TRegs = (rPTCvWeight=0, rPTCvADC=2, rPTCvState=3, rPTCvADCRange=103, rPTCvADCFrequency=4,
        rPTCvOutType=104, rPTCvCalibrCoeff=5, rPTCvZeroCode=7, rPTCvPolarity=9,
        rPTCvFilterSize1=109, rPTCvFilterSize2=10, rPTCvNetNumber=110, rPTCvNetSpeed=11,
        rPTCvADCCanal=111, rPTCvAllowDosing=12, rPTCvAutoZeroing=112, rPTCvTarget=13,
        rPTCvAdvanceHard=15, rPTCvAdvanceSoft=17, rPTCvInputs=32, rPTCvOutputs=132);
      TCommands = (cPTCvResetDose=1, cPTCvStartDosing=2, cPTCvStopDosing=4);
    var
      FAdvance: Single;   //Упреждение
      FTask: Single;      //Задание
      FState: TStates;    //Состояние
      FWeightCurrent: Single; //Текущий вес
      FWeightTotal: Single;   //Общий вес

      FAutoDosing: TTimer;  //Автоматическое дозирование

      FParentAdv: TControl;       //Кнопка записи упреждения
      FParentTask: TControl;      //Кнопка записи задания
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentWeight: TControl;    //Связующий компонент с компонентами изменения веса

    procedure SetState(Value: TStates);
    procedure SetAdvance(Value: Single);
    procedure SetTask(Value: Single);

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateRBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnWeightTBChange(Sender: TObject);  //Действие на изменение веса ползунком
    procedure OnWeightBtnClick(Sender: TObject);  //Действие на нажатие кнопки сброса веса
    procedure OnAutoDosing(Sender: TObject);  //Автоматическое дозирование

    function BytesToSingle(Data: Array of Byte): Single;
    function SingleToBytes(Value: Single): TBytes;

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Advance: Single read FAdvance write SetAdvance;
    property Task: Single read FTask write SetTask;
    property State: TStates read FState write SetState;
    property WeightCurrent: Single read FWeightCurrent write FWeightCurrent;
    property WeightTotal: Single read FWeightTotal write FWeightTotal;

    function Process(Request: Array of Byte): String; override;
  end;

  TMaster110_4 = class(TDevice)
  private
    type
      TCommands = (cm_4StartDosing=1, cm_4StopDosing=2, cm_4Unload=3, cm_4ErrorReset=6, cm_4Flash=7,
        cm_4ResetDose=11, cm_4Netto=14,
        cm_4State=13, cm_4Version=15, cm_4Weight=16);
    var
      FDecimal: Byte;     //Десятичная точка
      FAdvance: Byte;     //Упреждение
      FTask: Word;        //Задание
      FWeightCurrent: Longint;  //Текущий вес
      FWeightTotal: Longint;    //Общий вес
      FWeightLast: Longint;     //Последний отдозированный вс
      FState: Array [0..7] of Boolean;  //Состояния

      FAutoDosing: TTimer;  //Автоматическое дозирование

      FParentDec: TControl;       //Кнопка записи десятичной точки
      FParentAdv: TControl;       //Кнопка записи упреждения
      FParentTask: TControl;      //Кнопка записи задания
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentWeight: TControl;    //Связующий компонент с компонентами изменения веса

    procedure SetDecimal(Value: Byte);
    procedure SetAdvance(Value: Byte);
    procedure SetTask(Value: Word);
    procedure SetState(Index: Byte; Value: Boolean);
    function GetState(Index: Byte): Boolean;

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateCBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnWeightTBChange(Sender: TObject);  //Действие на изменение веса ползунком
    procedure OnWeightBtnClick(Sender: TObject);  //Действие на нажатие кнопки сброса веса
    procedure OnAutoDosing(Sender: TObject);  //Автоматическое дозирование

    procedure CommandReaction(Command: TCommands);
    function StatesToByte: Byte;

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property Decimal: Byte read FDecimal write SetDecimal;
    property Advance: Byte read FAdvance write SetAdvance;
    property Task: Word read FTask write SetTask;
    property WeightCurrent: Longint read FWeightCurrent write FWeightCurrent;
    property WeightTotal: Longint read FWeightTotal write FWeightTotal;
    property WeightLast: Longint read FWeightTotal write FWeightTotal;
    property State[Index: Byte]: Boolean read GetState write SetState;

    function Process(Request: Array of Byte): String; override;
  end;

  TKGD = class(TDevice)
  private
    type
      TStates = (sKGDsWaiting, sKGDsMeasuring);
      TCommands = (cKGDcStart=1, cKGDcStop=2, cKGDcResult=3, cKGDcSetTime=4);
    var
      FMeasTime: Cardinal;  //Время измерения
      FState: TStates;      //Состояние
      FEndTime: Cardinal;   //Время завершения измерения
      FResult: ShortInt;    //Результат измерений: -1 - work, 0 - good, 1 - bad

      FAutoMeasuring: TTimer;   //Автоматическое измерение

      FParentMeasTime: TControl;  //Кнопка записи времени измерения
      FParentState: TWinControl;  //Родительский компонент набора состояний
      FParentMeas: TControl;      //Связующий компонент с компонентами измерения

    procedure SetState(Value: TStates);
    procedure SetMeasTime(Value: Cardinal);

    procedure OnParamBtnClick(Sender: TObject); //Действие на изменению параметра
    procedure OnStateRBClick(Sender: TObject);  //Действие по изменению состояния вручную
    procedure OnMeasBtnClick(Sender: TObject);  //Действие на нажатие кнопки завершения измерения
    procedure OnAutoMeasuring(Sender: TObject);  //Автоматическое измрение

    procedure LoadSettings;
  public
    constructor Create(AOwner: TComponent; AAddress: Byte);
    destructor Destroy; override;

    property MeasTime: Cardinal read FMeasTime write SetMeasTime;
    property State: TStates read FState write SetState;

    function Process(Request: AnsiString): String; reintroduce;
  end;

const
  cDXMBtnBoxHeight = 261; //Точно на 12
  cXMLRootName = 'Settings';
  cXMLDevice = 'Device';
  cXMLFormSettings = 'FormSettings';
var
  CntDIOM, CntDIM_16D, CntDIM_32DN, CntDOM_32R, CntTB001_081, CntTB001_091, CntTB001_1102,
  CntPTC001, CntMaster110_4: Byte;  //Счётчики номеров устройств
  EmulatorName: String; //Название эмулятора (по нему будут искаться настройки)

//--------------------------------------------------------------------------------------------------

  //Перевод числового ответа в строковый
function ResponseToStr(pResp: PByteArray; Len: Byte): String;
function SaveSettingPrepare(Path: String; Address: Byte; out Doc: IXMLDocument;
  out Device: IXMLNode): Boolean;

function CalcCRC16_DIOM(pBuf: PByteArray; len: Integer): Word;
function CalcCRC_Master(pBuf: PByteArray): Word;
function CalcCRC_KB(pBuf: PByteArray; len: Byte): Word;

  //Создание выходов для МДВВ и МУ
procedure CreateOutputs(var Parent: TWinControl; BtnClick: TNotifyEvent; Count: Byte;
  MarginLeft: Integer; Capts: Array of String);
  //Создание входов для МДВВ и МВ
procedure CreateInputs(var Parent: TWinControl; BtnClick: TNotifyEvent; Count: Byte;
  MarginLeft: Integer; Capts: Array of String);
  //Создание редактора параметров весовых контроллеров
procedure CreateParamEditor(var Parent: TControl; Name: String; MarginTop: Integer;
  ParamNum: Integer; OnBtnClick: TNotifyEvent);
  //Создание списка состояний (RadioButtons)
procedure CreateStatesChooser(var Parent: TWinControl; MarginTop: Integer; Names: Array of String;
  ColCnt: Byte; DefItemIndex: Integer; OnRBClick: TNotifyEvent);
  //Создание списка состояний (CheckBoxes)
procedure CreateStatesChecker(var Parent: TWinControl; MarginTop: Integer; Names: Array of String;
  ColCnt: Byte; DefItemIndex: Integer; OnRBClick: TNotifyEvent);
  //Создание компонентов по изменению веса
procedure CreateWeightChanger(var Parent: TControl; MarginTop: Integer; OnTBChange,
  OnBtnClick: TNotifyEvent);
  //Создание метки с возвожностью изменения
function CreateEditedLabel(Parent: TWinControl; AText: String; ATop, ALeft, AHeight, AWidth,
  ATag: Integer): TEdit;
  //Создание компонентов для измерения
procedure CreateMeasurer(var Parent: TControl; MarginTop: Integer; OnBtnClick: TNotifyEvent);
  //Поиск индикатора по порядковому номеру в устройствах ввода-вывода
function FindDXMLed(Num: Byte; Parent: TWinControl): TiLedRound;

implementation

uses RazMsgDlgs, RazFuncs;

//==================================================================================================
//___________________________________________TSomeClass_____________________________________________
//==================================================================================================

class procedure TSomeClass.OnEditedLabelEnter(Sender: TObject);
begin
  With TEdit(Sender) do
  Begin
    BevelKind:=bkSoft;
    Color:=clWindow;
  End;
end;

class procedure TSomeClass.OnEditedLabelExit(Sender: TObject);
begin
  With TEdit(Sender) do
  Begin
    BevelKind:=bkNone;
    Color:=clBtnFace;
  End;
end;

//==================================================================================================
//_____________________________________________TDevice______________________________________________
//==================================================================================================

procedure TDevice.OnDeviceTurn(Sender: TObject);
begin
  FIsInWork:=TSpeedButton(Sender).Down;
end;

//--------------------------------------------------------------------------------------------------

constructor TDevice.Create(AOwner: TComponent);
var
  SB: TSpeedButton;
begin
  inherited Create(AOwner);
  FAddress:=0;
  FName:='';
  FDevType:=dtNone;
  FIsInWork:=True;

    //Индикатор связи
  FIndicator:=TiLedRound.Create(Self);
  With FIndicator do
  Begin
    BevelStyle:=ibsNone;
    Hint:='Индикатор обращения к устройству';
    Left:=0;
    Top:=0;
    Height:=5;
    Width:=5;
  End;
  InsertControl(FIndicator);

    //Кнопка включения/выключения
  SB:=TSpeedButton.Create(Self);
  With SB do
  Begin
    AllowAllUp:=True;
    Cursor:=crHandPoint;
    GroupIndex:=TPageControl(AOwner).PageCount+1;
    Down:=True;
    Hint:='Включение/выключение устройства';
    Caption:='';
    Left:=6;
    Top:=0;
    Height:=5;
    Width:=5;
    OnClick:=OnDeviceTurn;
  End;
  InsertControl(SB);

    //Название контроллера
  FNameControl:=CreateEditedLabel(Self, '', 0, 17, 16, 200, 0);
end;

//==================================================================================================
//_____________________________________________TDIOM________________________________________________
//==================================================================================================

function TDIOM.GetIns(Index: Byte): Boolean;
begin
  Result:=FIns[Index];
end;

procedure TDIOM.SetIns(Index: Byte; const Value: Boolean);
begin
  FIns[Index]:=Value;
end;

function TDIOM.GetOuts(Index: Byte): Boolean;
begin
  Result:=FOuts[Index];
end;

procedure TDIOM.SetOuts(Index: Byte; const Value: Boolean);
begin
  FOuts[Index]:=Value;
end;

procedure TDIOM.OnBtnInClick(Sender: TObject);
var
  LR: TiLedRound;
begin
  FIns[TControl(Sender).Tag]:=not FIns[TControl(Sender).Tag];
  LR:=FindDXMLed(TControl(Sender).Tag, FInsParent);
  If Assigned(LR) then
    LR.Active:=FIns[TControl(Sender).Tag];
end;

procedure TDIOM.OnBtnOutClick(Sender: TObject);
var
  LR: TiLedRound;
begin
  FOuts[TControl(Sender).Tag]:=not FOuts[TControl(Sender).Tag];
  LR:=FindDXMLed(TControl(Sender).Tag, FOutsParent);
  If Assigned(LR) then
    LR.Active:=FOuts[TControl(Sender).Tag];
end;

procedure TDIOM.SetOutVals(Value: Byte);
var
  i, n: Byte;
begin
  i := High(FOuts);
  While i <= High(FOuts) do
  Begin
    n := Round(Exp(i * Ln(2)));
    if Value >= n then
    begin
      Dec(Value, n);
      FOuts[i] := True;
    end
    else FOuts[i] := False;
    Dec(i);
  End;
end;

procedure TDIOM.UpdOutLeds;
var
  i: Byte;
  LR: TiLedRound;
begin
  For i:=Low(FOuts) to High(FOuts) do
  Begin
    LR:=FindDXMLed(i, FOutsParent);
    if Assigned(LR) then
      LR.Active:=FOuts[i];
  End;
end;

function TDIOM.GetInVals: Word;
var
  i: Byte;
begin
  Result := 0;
  For i := Low(FIns) to High(FIns) do
    if FIns[i] then
      Inc(Result, Round(Exp(i * Ln(2))));
end;

function TDIOM.GetOutVals: Word;
var
  i: Byte;
begin
  Result := 0;
  For i := Low(FOuts) to High(FOuts) do
    if FOuts[i] then
      Inc(Result, Round(Exp(i * Ln(2))));
end;

procedure TDIOM.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  procedure _SetEditText(Parent: TWinControl; Num, TagMin: Byte; Text: String);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        TEdit(Parent.Controls[ii]).Text:=Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes.Nodes[j].NodeName = 'enter' then
            _SetEditText(FInsParent, Device.ChildNodes[j].GetAttributeNS('num', ''), High(FIns),
              Device.ChildNodes.Nodes[j].GetAttributeNS('caption', ''))
          else if Device.ChildNodes[j].NodeName = 'exit' then
            _SetEditText(FOutsParent, Device.ChildNodes[j].GetAttributeNS('num', ''), High(FOuts),
              Device.ChildNodes[j].GetAttributeNS('caption', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TDIOM.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtDIOM;
  SetLength(FIns, cInCnt);
  SetLength(FOuts, cOutCnt);
  Inc(CntDIOM);
  Caption:='МДВВ №'+IntToStr(CntDIOM);

    //Создание выходов
  Parent:=Self;
  CreateOutputs(Parent, OnBtnOutClick, cOutCnt, 6, []);
  FOutsParent:=Parent;

    //Создание входов
  Parent:=Self;
  CreateInputs(Parent, OnBtnInClick, cInCnt, 232, []);
  FInsParent:=Parent;

  LoadSettings;
end;

destructor TDIOM.Destroy;
var
  i: Integer;
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
  Str: String;

  procedure _GetEditText(Parent: TWinControl; Num, TagMin: Byte);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        Str:=TEdit(Parent.Controls[ii]).Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  For i:=Low(FOuts) to High(FOuts) do
  Begin
    _GetEditText(FOutsParent, i, High(FOuts));
    if Str<>'' then
      with Device.AddChild('exit') do
      begin
        SetAttributeNS('num', '', i);
        SetAttributeNS('caption', '', Str);
      end;
  End;
  For i:=Low(FIns) to High(FIns) do
  Begin
    _GetEditText(FInsParent, i, High(FIns));
    if Str<>'' then
      with Device.AddChild('enter') do
      begin
        SetAttributeNS('num', '', i);
        SetAttributeNS('caption', '', Str);
      end;
  End;

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TDIOM.Process(Request: Array of Byte): String;
var
  i: Byte;
  Val: Word;
  Response: Array [0..7] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Команда на запись
  If (Request[1]=16) and (Request[2]=0) and (Request[3]=50) and (Request[4]=0) and
    (Request[5]=1) and (Request[6]=0) and (Request[7]=0) then //Контрольная сумма не проверяется
  Begin
    SetOutVals(Request[8]);
    UpdOutLeds;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC16_DIOM(@Response, 6));
    Response[7]:=Hi(CalcCRC16_DIOM(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Команда на чтение
  Else if (Request[1] in [3, 4]) and (Request[2]=0) and (Request[4]=0) and (Request[5]=1) then
  Begin
    Val:=0;
    if Request[3]=50 then
      Val:=GetOutVals
    else if Request[3]=51 then
      Val:=GetInVals;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=2;
    Response[3]:=Hi(Val);
    Response[4]:=Lo(Val);
    Response[5]:=Lo(CalcCRC16_DIOM(@Response, 5));
    Response[6]:=Hi(CalcCRC16_DIOM(@Response, 5));

    Result:=ResponseToStr(@Response, 7);
  End;
end;

//==================================================================================================
//______________________________________________TDIM________________________________________________
//==================================================================================================

function TDIM.GetIns(Index: Byte): Boolean;
begin
  Result:=FIns[Index];
end;

procedure TDIM.SetIns(Index: Byte; const Value: Boolean);
begin
  FIns[Index]:=Value;
end;

procedure TDIM.OnBtnInClick(Sender: TObject);
var
  LR: TiLedRound;
begin
  FIns[TControl(Sender).Tag]:=not FIns[TControl(Sender).Tag];
  LR:=FindDXMLed(TControl(Sender).Tag, FInsParent);
  If Assigned(LR) then
    LR.Active:=FIns[TControl(Sender).Tag];
end;

procedure TDIM.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  procedure _SetEditText(Parent: TWinControl; Num, TagMin: Byte; Text: String);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        TEdit(Parent.Controls[ii]).Text:=Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and 
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          _SetEditText(FInsParent, Device.ChildNodes[j].GetAttributeNS('num', ''), High(FIns),
            Device.ChildNodes[j].GetAttributeNS('caption', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

destructor TDIM.Destroy;
var
  i: Integer;
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
  Str: String;

  procedure _GetEditText(Parent: TWinControl; Num, TagMin: Byte);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        Str:=TEdit(Parent.Controls[ii]).Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  For i:=Low(FIns) to High(FIns) do
  Begin
    _GetEditText(FInsParent, i, High(FIns));
    if Str<>'' then
      with Device.AddChild('enter') do
      begin
        SetAttributeNS('num', '', i);
        SetAttributeNS('caption', '', Str);
      end;
  End;

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

//==================================================================================================
//____________________________________________TDIM_16D______________________________________________
//==================================================================================================

function TDIM_16D.GetInVals: Word;
var
  i: Byte;
begin
  Result := 0;
  For i := Low(FIns) to High(FIns) do
    if FIns[i] then
      Inc(Result, Round(Exp(i * Ln(2))));
end;

//--------------------------------------------------------------------------------------------------

constructor TDIM_16D.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dTDIM_16D;
  SetLength(FIns, cInCnt);
  Inc(CntDIM_16D);
  Caption:='МВ110-16 №'+IntToStr(CntDIM_16D);

    //Создание входов
  Parent:=Self;
  CreateInputs(Parent, OnBtnInClick, cInCnt, 232, []);
  FInsParent:=Parent;

  LoadSettings;
end;

function TDIM_16D.Process(Request: Array of Byte): String;
var
  Val: Word;
  Response: Array [0..6] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Команда на чтение
  If (Request[1] in [3, 4]) and (Request[2]=0) and (Request[4]=0) and (Request[5]=1) then
  Begin
    Val:=0;
    if Request[3]=51 then
      Val:=GetInVals;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=2;
    Response[3]:=Hi(Val);
    Response[4]:=Lo(Val);
    Response[5]:=Lo(CalcCRC16_DIOM(@Response, 5));
    Response[6]:=Hi(CalcCRC16_DIOM(@Response, 5));

    Result:=ResponseToStr(@Response, 7);
  End;
end;

//==================================================================================================
//____________________________________________TDIM_32DN_____________________________________________
//==================================================================================================

function TDIM_32DN.GetInVals(Reg: Byte): Word;
var
  i: Byte;
  Res: Int64;
begin
  Res := 0;
  Result := 0;
  For i := Low(FIns) to High(FIns) do
    if FIns[i] then
      Inc(Res, Round(Exp(i * Ln(2))));

    //Убираем старшие 16 бит
  If Reg=99 then  //16-31
    Result := (Res shr 16) and Word(-1) //0111 1101 -> 0000 0111
  Else if Reg=100 then  //0-15
    Result := Res and Word(-1);         //1010 1111 -> 0000 1111
end;

//--------------------------------------------------------------------------------------------------

constructor TDIM_32DN.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtDIM_32DN;
  SetLength(FIns, cInCnt);
  Inc(CntDIM_32DN);
  Caption:='МВ110-32 №'+IntToStr(CntDIM_32DN);

    //Создание входов
  Parent:=Self;
  CreateInputs(Parent, OnBtnInClick, cInCnt, 232, []);
  FInsParent:=Parent;

  LoadSettings;
end;

function TDIM_32DN.Process(Request: Array of Byte): String;
var
  Val: Word;
  Response: Array [0..6] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Команда на чтение
  If (Request[1] in [3, 4]) and (Request[2]=0) and (Request[4]=0) and (Request[5]=1) then
  Begin
    Val:=GetInVals(Request[3]);

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=2;
    Response[3]:=Hi(Val);
    Response[4]:=Lo(Val);
    Response[5]:=Lo(CalcCRC16_DIOM(@Response, 5));
    Response[6]:=Hi(CalcCRC16_DIOM(@Response, 5));

    Result:=ResponseToStr(@Response, 7);
  End;
end;

//==================================================================================================
//______________________________________________TDOM________________________________________________
//==================================================================================================

function TDOM.GetOuts(Index: Byte): Boolean;
begin
  Result:=FOuts[Index];
end;

procedure TDOM.SetOuts(Index: Byte; const Value: Boolean);
begin
  FOuts[Index]:=Value;
end;

procedure TDOM.OnBtnOutClick(Sender: TObject);
var
  LR: TiLedRound;
begin
  FOuts[TControl(Sender).Tag]:=not FOuts[TControl(Sender).Tag];
  LR:=FindDXMLed(TControl(Sender).Tag, FOutsParent);
  If Assigned(LR) then
    LR.Active:=FOuts[TControl(Sender).Tag];
end;

procedure TDOM.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  procedure _SetEditText(Parent: TWinControl; Num, TagMin: Byte; Text: String);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        TEdit(Parent.Controls[ii]).Text:=Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and 
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          _SetEditText(FOutsParent, Device.ChildNodes[j].GetAttributeNS('num', ''), High(FOuts),
            Device.ChildNodes[j].GetAttributeNS('caption', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

destructor TDOM.Destroy;
var
  i: Integer;
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
  Str: String;

  procedure _GetEditText(Parent: TWinControl; Num, TagMin: Byte);
  var
    ii: Byte;
  begin
    For ii:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[ii].Tag > TagMin) and
        //(TControl(Parent.Controls[ii].Tag) is TEdit) and
        (TControl(Parent.Controls[ii].Tag).Tag = Num) then
      begin
        Str:=TEdit(Parent.Controls[ii]).Text;
        Break;
      end;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  For i:=Low(FOuts) to High(FOuts) do
  Begin
    _GetEditText(FOutsParent, i, High(FOuts));
    if Str<>'' then
      with Device.AddChild('exit') do
      begin
        SetAttributeNS('num', '', i);
        SetAttributeNS('caption', '', Str);
      end;
  End;

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

//==================================================================================================
//____________________________________________TDOM_32R______________________________________________
//==================================================================================================

procedure TDOM_32R.SetOutVals(Value: Word; Reg: Byte);
var
  i, n, dif, stop: Word;
begin
  If Reg=97 then  //16-31
  Begin
    i := High(FOuts);
    stop := 15;
    dif := 16;
  End
  Else if Reg=98 then //0-15
  Begin
    i := 15;
    stop := Word(-1);
    dif := 0;
  End
  Else Exit;

  While i <= High(FOuts) do
  Begin
    n := Round(Exp((i - dif) * Ln(2)));
    if Value >= n then
    begin
      Dec(Value, n);
      FOuts[i] := True;
    end
    else FOuts[i] := False;
    Dec(i);
    if i = stop then Exit;
  End;
end;

procedure TDOM_32R.UpdOutLeds;
var
  i: Byte;
  LR: TiLedRound;
begin
  For i:=Low(FOuts) to High(FOuts) do
  Begin
    LR:=FindDXMLed(i, FOutsParent);
    if Assigned(LR) then
      LR.Active:=FOuts[i];
  End;
end;

function TDOM_32R.GetOutVals(Reg: Byte): Word;
var
  i: Byte;
  Res: Int32;
begin
  Res := 0;
  Result := 0;
  For i := Low(FOuts) to High(FOuts) do
    if FOuts[i] then
      Inc(Res, Round(Exp(i * Ln(2))));

    //Убираем старшие 16 бит
  If Reg=97 then  //16-31
    Result := (Res shr 16) and Word(-1) //1001 0111 1010 1101 -> 0000 0000 1001 0111
  Else if Reg=98 then //0-15
    Result := Res and Word(-1);         //1010 0110 1111 1111 -> 0000 0000 1111 1111
end;

//--------------------------------------------------------------------------------------------------

constructor TDOM_32R.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtDIOM;
  SetLength(FOuts, cOutCnt);
  Inc(CntDOM_32R);
  Caption:='МУ 110-32 №'+IntToStr(CntDOM_32R);

    //Создание выходов
  Parent:=Self;
  CreateOutputs(Parent, OnBtnOutClick, cOutCnt, 6, []);
  FOutsParent:=Parent;

  LoadSettings;
end;

function TDOM_32R.Process(Request: Array of Byte): String;
var
  i: Byte;
  Val: Int32;
  Response: Array [0..7] of Byte;
  aVal: Array [0..3] of Byte absolute Val;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Команда на запись
  If (Request[1]=16) and (Request[2]=0) and (Request[3] in [97, 98]) and (Request[4]=0) and
    (Request[5]=1) and (Request[6]=0) then //Контрольная сумма не проверяется
  Begin
    SetOutVals(Request[8] + Request[7] * 256, Request[3]);
    UpdOutLeds;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC16_DIOM(@Response, 6));
    Response[7]:=Hi(CalcCRC16_DIOM(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Команда на чтение
  Else if (Request[1] in [3, 4]) and (Request[2]=0) and (Request[4]=0) and (Request[5]=1) then
  Begin
    Val:=GetOutVals(Request[3]);

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=2;
    Response[3]:=Hi(Val);
    Response[4]:=Lo(Val);
    Response[5]:=Lo(CalcCRC16_DIOM(@Response, 5));
    Response[6]:=Hi(CalcCRC16_DIOM(@Response, 5));

    Result:=ResponseToStr(@Response, 7);
  End;
end;

//==================================================================================================
//___________________________________________TTB001_081_____________________________________________
//==================================================================================================

procedure TKB001_081.SetState(Value: TStates);
  procedure _CheckRB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TRadioButton(FParentState.Controls[i]).Checked:=True;
        Break;
      end;
  end;
begin
  FState:=Value;
  Case FState of
    s081vWaiting: begin
                    FWeightTotal := FWeightTotal + FWeightCurrent;
                    with TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
                    begin
                      Max:=Round(FWeightTotal);
                      Position:=Max;
                      SelStart:=0;
                      SelEnd:=0;
                      SelEnd:=Round(FWeightTotal - FAdvance);
                    end;
                    _CheckRB(0);
                  end;
    s081vDosing:  _CheckRB(1);
    s081vImpulse: _CheckRB(2);
    s081vError:   _CheckRB(3);
  End;
end;

procedure TKB001_081.SetDecimal(Value: Byte);
begin
  FDecimal:=Value;
  TStaticText(TControl(FParentDec.Tag).Tag).Caption:=IntToStr(FDecimal);
end;

procedure TKB001_081.SetAdvance(Value: Single);
begin
  FAdvance:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(FTask - Value);
  End;
  TStaticText(TControl(FParentAdv.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_081.SetTask(Value: Single);
begin
  FTask:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    Max:=Round(Value);
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(Value - FAdvance);
  End;
  TStaticText(TControl(FParentTask.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
  TLabel(FParentWeight).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_081.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Integer;
  ValF: Single;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToInt(E.Text, ValI) then
        begin
          FDecimal:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
    2:  if TryStrToFloat(E.Text, ValF) then
        begin
          Advance:=ValF;
          ST.Caption:=CurrToStrF(FAdvance, ffGeneral, FDecimal);
        end;
    3:  if TryStrToFloat(E.Text, ValF) then
        begin
          Task:=ValF;
          ST.Caption:=CurrToStrF(FTask, ffGeneral, FDecimal);
        end;
  End;
end;

procedure TKB001_081.OnStateRBClick(Sender: TObject);
begin
  Case TControl(Sender).Tag of
    0:  FState:=s081vWaiting;
    1:  FState:=s081vDosing;
    2:  FState:=s081vImpulse;
    3:  FState:=s081vError;
  End;
end;

procedure TKB001_081.OnWeightTBChange(Sender: TObject);
var
  L: TLabel;
begin
  L:=TLabel(TControl(TControl(Sender).Tag).Tag);  //TB -> B -> L
  L.Caption:=IntToStr(TTrackBar(Sender).Position);
  FWeightCurrent:=TTrackBar(Sender).Position;
end;

procedure TKB001_081.OnWeightBtnClick(Sender: TObject);
begin
  TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
  //TLabel(FParentWeight).Caption:='0';
  FWeightCurrent:=0;
  FWeightTotal:=0;
end;

procedure TKB001_081.OnAutoDosing(Sender: TObject);
const
  cDosTime = 10;  //секунд
var
  Val, Adv: Integer;
  TB: TTrackBar;

  procedure _Done;
  begin
    FState:=s081vWaiting; //Дозирование завершено, фиксация веса
    FAutoDosing.Enabled:=False;
    Delay(2500);
    FAutoDosing.Enabled:=True;

      //Окончание
    State:=s081vWaiting;
    TLabel(TControl(TControl(TControl(FParentWeight.Tag).Tag).Tag).Tag).Caption:=
      CurrToStrF(FWeightTotal, ffGeneral, FDecimal);
    TLabel(FParentWeight).Caption:=IntToStr(Round(FWeightTotal));
    TB.Max:=Round(FWeightTotal);
    TB.Position:=TB.Max;
  end;

begin
  If not TCheckBox(FParentWeight.Tag).Checked or not (FState in [s081vDosing, s081vImpulse]) then
    Exit;
  TB := TTrackBar(TControl(FParentWeight.Tag).Tag);

  If TB.Position = TB.Max then
  Begin
    _Done;
    Exit;
  End
  Else if TB.Position >= TB.SelEnd then
  Begin
    Adv:=3;
    State:=s081vImpulse;
  End
  Else
  Begin
    Adv:=1;
    State:=s081vDosing;
  End;

  Val := Random(Round(FTask / cDosTime / Adv{ * Pow(10, FDecimal)}));
  If Val = 0 then Val := 1;
  TB.Position := TB.Position + Val;
  If TB.Position = TB.Max then
    _Done;
end;

function TKB001_081.BytesToSingle(Data: Array of Byte): Single;
var
  ResBytes: Array [0..3] of Byte absolute Result;
begin
  ResBytes[0]:=Data[3];
  ResBytes[1]:=Data[2];
  ResBytes[2]:=Data[1];
  ResBytes[3]:=Data[0];
end;

function TKB001_081.SingleToBytes(Value: Single): TBytes;
var
  ResBytes: Array [0..3] of Byte absolute Value;
begin
  SetLength(Result, 4);
  Result[0]:=ResBytes[3];
  Result[1]:=ResBytes[2];
  Result[2]:=ResBytes[1];
  Result[3]:=ResBytes[0];
end;

procedure TKB001_081.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetSingle(Val: OleVariant): Single;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetInt(Val: OleVariant): Integer;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean;
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: Decimal:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
              2: Advance:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
              3: Task:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentWeight.Tag).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TKB001_081.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtKB001_081;
  FDecimal:=0;
  FAdvance:=0;
  FTask:=0;
  FState:=s081vWaiting;
  FWeightCurrent:=0;
  FWeightTotal:=0;
  FAutoDosing:=TTimer.Create(Self);
  FAutoDosing.OnTimer:=OnAutoDosing;
  Inc(CntTB001_081);
  Caption:='КВ-001 081 №'+IntToStr(CntTB001_081);

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Дес. точка', 22, 1, OnParamBtnClick);
    FParentDec:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Упрежд.', 50, 2, OnParamBtnClick);
    FParentAdv:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Задание', 78, 3, OnParamBtnClick);
    FParentTask:=Parent;
    WinParent:=Self;
  CreateStatesChooser(WinParent, 110, ['Ожидание', 'Дозирование', 'Подсыпка', 'Ошибка'],
  2, 0, OnStateRBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateWeightChanger(Parent, 160, OnWeightTBChange, OnWeightBtnClick);
    FParentWeight:=Parent;

  LoadSettings;
end;

destructor TKB001_081.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FDecimal);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 2);
    SetAttributeNS('value', '', FAdvance);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 3);
    SetAttributeNS('value', '', FTask);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentWeight.Tag).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TKB001_081.Process(Request: Array of Byte): String;
var
  i, j, l, ByteCnt: Byte;
  RegAdr, RegCnt: Word;
  Val: Cardinal;
  absVal: Array [0..3] of Byte absolute Val;
  arVal: TBytes;
  Weight: Single;
  Response: Array [0..11] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Чтение
  If Request[1] = 3 then  //Контрольная сумма не проверяется
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
{   OutData[0]:=Address;
    OutData[1]:=Funct;
    OutData[2]:=Hi(RegData);
    OutData[3]:=Lo(RegData);
    OutData[4]:=Hi(RegCnt);
    OutData[5]:=Lo(RegCnt);
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[6]:=Lo(CRC);
    OutData[7]:=Hi(CRC);  }

    case TRegs(RegAdr) of
      r081vWeight:      begin
                          arVal:=SingleToBytes(FWeightCurrent + Random(100)/100); //С имитацией дробной части
                          for i:=Low(arVal) to High(arVal) do
                            absVal[i]:=arVal[i];
                        end;
      r081vState:       begin
                          Val:=Byte(FState);
                          i:=absVal[0];
                          absVal[0]:=absVal[1];
                          absVal[1]:=i;
                        end;
      r081vFloatDigits: Val:=FDecimal;
    end;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=RegCnt * 2;
    j:=0;
    for i:=RegCnt * 2 - 1 downto 0 do
    begin
      Response[i+3]:=absVal[j];
      Inc(j);
    end;
    l:=3 + RegCnt * 2;
    Response[l]:=Lo(CalcCRC_KB(@Response, l));
    Response[l+1]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, l+2);
  End

    //Команда
  Else if (Request[1] = 6) and (Request[2] = 0) and (Request[3] = 0) and (Request[4] = 0) then
  Begin
    case TCommands(Request[5]) of
      c081vResetDose:   begin
                          Weight:=FWeightTotal; //Общий вес при сбросе сохраняется
                          OnWeightBtnClick(nil);
                          FWeightTotal:=Weight;
                          //SetTask(FTask);
                        end;
      c081vStartDosing: begin
                          Weight:=FWeightTotal; //Общий вес на старте сохраняется
                          OnWeightBtnClick(nil);
                          FWeightTotal:=Weight;
                          State:=s081vDosing;
                        end;
      c081vStopDosing:  State:=s081vWaiting;
    end;
{   OutData[0]:=Address;
    OutData[1]:=6;
    OutData[2]:=0;
    OutData[3]:=0;
    OutData[4]:=0;
    OutData[5]:=Funct;
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[6]:=Lo(CRC);
    OutData[7]:=Hi(CRC);  }

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, 6));
    Response[7]:=Hi(CalcCRC_KB(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Запись
  Else if Request[1] = 16 then
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
    ByteCnt:=Request[6];
    if ByteCnt > 0 then
    begin
      j := ByteCnt - 1;
      for i:=7 to 7 + ByteCnt{ * 2} - 1 do
      begin
        //absVal[i-7] := Request[i];
        absVal[j] := Request[i];
        Dec(j);
      end;
    end;
    case TRegs(RegAdr) of
      r081vAdvanceHard: Advance:=BytesToSingle(absVal);
      r081vTarget:      Task:=BytesToSingle(absVal);
    end;
{   OutData[0]:=Address;
    OutData[1]:=Funct;
    OutData[2]:=Hi(RegData);
    OutData[3]:=Lo(RegData);
    OutData[4]:=Hi(RegCnt);
    OutData[5]:=Lo(RegCnt);
    OutData[6]:=DataLen;
    for i:=7 to 7+DataLen-1 do
    begin
      OutData[i]:=Data[j];
      Inc(j);
    end;
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[y]:=Lo(CRC);
    OutData[z]:=Hi(CRC);  }

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, l));
    Response[7]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, 8);
  End;
end;

//==================================================================================================
//___________________________________________TTB001_091_____________________________________________
//==================================================================================================

procedure TKB001_091.SetState(Value: TStates);
  procedure _CheckRB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TRadioButton(FParentState.Controls[i]).Checked:=True;
        Break;
      end;
  end;
begin
  FState:=Value;
  Case FState of
    s091vWaiting: _CheckRB(0);
    s091vDosing:  _CheckRB(1);
    s091vImpulse: _CheckRB(2);
    s091vError:   _CheckRB(3);
  End;
end;

procedure TKB001_091.SetDecimal(Value: Byte);
begin               
  FDecimal:=Value;
  TStaticText(TControl(FParentDec.Tag).Tag).Caption:=IntToStr(FDecimal);
end;

procedure TKB001_091.SetAdvance(Value: Single);
begin
  FAdvance:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(FTask - Value);
  End;
  TStaticText(TControl(FParentAdv.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_091.SetTask(Value: Single);
begin
  FTask:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    Max:=Round(Value);
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(Value - FAdvance);
  End;
  TStaticText(TControl(FParentTask.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
  TLabel(FParentWeight).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_091.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Integer;
  ValF: Single;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToInt(E.Text, ValI) then
        begin
          FDecimal:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
    2:  if TryStrToFloat(E.Text, ValF) then
        begin
          Advance:=ValF;
          ST.Caption:=CurrToStrF(FAdvance, ffGeneral, FDecimal);
        end;
    3:  if TryStrToFloat(E.Text, ValF) then
        begin
          Task:=ValF;
          ST.Caption:=CurrToStrF(FTask, ffGeneral, FDecimal);
        end;
  End;
end;

procedure TKB001_091.OnStateRBClick(Sender: TObject);
begin
  Case TControl(Sender).Tag of
    0:  FState:=s091vWaiting;
    1:  FState:=s091vDosing;
    2:  FState:=s091vImpulse;
    3:  FState:=s091vError;
  End;
end;

procedure TKB001_091.OnWeightTBChange(Sender: TObject);
var
  L: TLabel;
begin
  L:=TLabel(TControl(TControl(Sender).Tag).Tag);  //TB -> B -> L
  L.Caption:=IntToStr(TTrackBar(Sender).Position);
  FWeightCurrent:=TTrackBar(Sender).Position;
end;

procedure TKB001_091.OnWeightBtnClick(Sender: TObject);
begin
  TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
  //TLabel(FParentWeight).Caption:='0';
  FWeightCurrent:=0;
  FWeightTotal:=0;
end;

procedure TKB001_091.OnAutoDosing(Sender: TObject);
const
  cDosTime = 10;  //секунд
var
  Val, Adv: Integer;
  TB: TTrackBar;

  procedure _Done;
  begin
    State := s091vWaiting;
    FWeightTotal := FWeightTotal + FWeightCurrent;
    TLabel(TControl(TControl(TControl(FParentWeight.Tag).Tag).Tag).Tag).Caption:=
      CurrToStrF(FWeightTotal, ffGeneral, FDecimal);
  end;

begin
  If not TCheckBox(FParentWeight.Tag).Checked or not (FState in [s091vDosing, s091vImpulse]) then
    Exit;
  TB := TTrackBar(TControl(FParentWeight.Tag).Tag);

  If TB.Position = TB.Max then
  Begin
    _Done;
    Exit;
  End
  Else if TB.Position >= TB.SelEnd then
  Begin
    Adv:=3;
    State:=s091vImpulse;
  End
  Else
  Begin
    Adv:=1;
    State:=s091vDosing;
  End;

  Val := Random(Round(FTask / cDosTime / Adv{ * Pow(10, FDecimal)}));
  If Val = 0 then Val := 1;
  TB.Position := TB.Position + Val;
  If TB.Position = TB.Max then
    _Done;
end;

function TKB001_091.BytesToSingle(Data: Array of Byte): Single;
var
  ResBytes: Array [0..3] of Byte absolute Result;
begin
  ResBytes[0]:=Data[3];
  ResBytes[1]:=Data[2];
  ResBytes[2]:=Data[1];
  ResBytes[3]:=Data[0];
end;

function TKB001_091.SingleToBytes(Value: Single): TBytes;
var
  ResBytes: Array [0..3] of Byte absolute Value;
begin
  SetLength(Result, 4);
  Result[0]:=ResBytes[3];
  Result[1]:=ResBytes[2];
  Result[2]:=ResBytes[1];
  Result[3]:=ResBytes[0];
end;

procedure TKB001_091.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetSingle(Val: OleVariant): Single;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetInt(Val: OleVariant): Integer;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean; 
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and 
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: Decimal:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
              2: Advance:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
              3: Task:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentWeight.Tag).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TKB001_091.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtKB001_091;
  FDecimal:=0;
  FAdvance:=0;
  FTask:=0;
  FState:=s091vWaiting;
  FWeightCurrent:=0;
  FWeightTotal:=0;
  FAutoDosing:=TTimer.Create(Self);
  FAutoDosing.OnTimer:=OnAutoDosing;
  Inc(CntTB001_091);
  Caption:='КВ-001 091 №'+IntToStr(CntTB001_091);

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Дес. точка', 22, 1, OnParamBtnClick);
    FParentDec:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Упрежд.', 50, 2, OnParamBtnClick);
    FParentAdv:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Задание', 78, 3, OnParamBtnClick);
    FParentTask:=Parent;
    WinParent:=Self;
  CreateStatesChooser(WinParent, 110, ['Ожидание', 'Дозирование', 'Подсыпка', 'Ошибка'],
  2, 0, OnStateRBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateWeightChanger(Parent, 160, OnWeightTBChange, OnWeightBtnClick);
    FParentWeight:=Parent;

  LoadSettings;
end;

destructor TKB001_091.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FDecimal);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 2);
    SetAttributeNS('value', '', FAdvance);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 3);
    SetAttributeNS('value', '', FTask);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentWeight.Tag).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TKB001_091.Process(Request: Array of Byte): String;
var
  i, j, l, ByteCnt: Byte;
  RegAdr, RegCnt: Word;
  Val: Cardinal;
  absVal: Array [0..3] of Byte absolute Val;
  arVal: TBytes;
  Weight: Single;
  Response: Array [0..11] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Чтение
  If Request[1] = 3 then  //Контрольная сумма не проверяется
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
{   OutData[0]:=Address;
    OutData[1]:=Funct;
    OutData[2]:=Hi(RegData);
    OutData[3]:=Lo(RegData);
    OutData[4]:=Hi(RegCnt);
    OutData[5]:=Lo(RegCnt);
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[6]:=Lo(CRC);
    OutData[7]:=Hi(CRC);  }

    case TRegs(RegAdr) of
      r091vWeight:      begin
                          arVal:=SingleToBytes(FWeightCurrent);
                          for i:=Low(arVal) to High(arVal) do
                            absVal[i]:=arVal[i];
                        end;
      r091vState:       begin
                          Val:=Byte(FState);
                          i:=absVal[0];
                          absVal[0]:=absVal[1];
                          absVal[1]:=i;
                        end;
      r091vFloatDigits: Val:=FDecimal;
    end;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=RegCnt * 2;
    j:=0;
    for i:=RegCnt * 2 - 1 downto 0 do
    begin
      Response[i+3]:=absVal[j];
      Inc(j);
    end;
    l:=3 + RegCnt * 2;
    Response[l]:=Lo(CalcCRC_KB(@Response, l));
    Response[l+1]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, l+2);
  End

    //Команда
  Else if (Request[1] = 6) and (Request[2] = 0) and (Request[3] = 0) and (Request[4] = 0) then
  Begin
    case TCommands(Request[5]) of
      c091vResetDose:   OnWeightBtnClick(nil);
      c091vStartDosing: begin
                          {FWeightCurrent:=0;
                          TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
                          State:=s091vDosing;}
                          Weight:=FWeightTotal;
                          OnWeightBtnClick(nil);
                          FWeightTotal:=Weight;
                          State:=s091vDosing;
                        end;
      c091vStopDosing:  State:=s091vWaiting;
    end;
{   OutData[0]:=Address;
    OutData[1]:=6;
    OutData[2]:=0;
    OutData[3]:=0;
    OutData[4]:=0;
    OutData[5]:=Funct;
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[6]:=Lo(CRC);
    OutData[7]:=Hi(CRC);  }

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, 6));
    Response[7]:=Hi(CalcCRC_KB(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Запись
  Else if Request[1] = 16 then
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
    ByteCnt:=Request[6];
    if ByteCnt > 0 then
      for i:=7 to 7 + ByteCnt{ * 2} - 1 do
        absVal[i-7] := Request[i];
    case TRegs(RegAdr) of
      r091vAdvanceHard: Advance:=BytesToSingle(absVal);
      r091vTarget1:     Task:=BytesToSingle(absVal);
    end;
{   OutData[0]:=Address;
    OutData[1]:=Funct;
    OutData[2]:=Hi(RegData);
    OutData[3]:=Lo(RegData);
    OutData[4]:=Hi(RegCnt);
    OutData[5]:=Lo(RegCnt);
    OutData[6]:=DataLen;
    for i:=7 to 7+DataLen-1 do
    begin
      OutData[i]:=Data[j];
      Inc(j);
    end;
      CRC:=CalcCRC_KB(@OutData, l);
    OutData[y]:=Lo(CRC);
    OutData[z]:=Hi(CRC);  }

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, l));
    Response[7]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, 8);
  End;
end;

//==================================================================================================
//___________________________________________TTB001_1102____________________________________________
//==================================================================================================

procedure TKB001_1102.SetState(Value: TStates);
  procedure _CheckRB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TRadioButton(FParentState.Controls[i]).Checked:=True;
        Break;
      end;
  end;
begin
  FState:=Value;
  Case FState of
    s1102vWaiting:        _CheckRB(0);
    s1102vDosing1:        _CheckRB(1);
    s1102vImpulse1:       _CheckRB(2);
    s1102vDosingFinished: _CheckRB(3);
    s1102vError:          _CheckRB(4);
  End;
end;

procedure TKB001_1102.SetDecimal(Value: Byte);
begin
  FDecimal:=Value;
  TStaticText(TControl(FParentDec.Tag).Tag).Caption:=IntToStr(FDecimal);
end;

procedure TKB001_1102.SetAdvance(Value: Single);
begin
  FAdvance:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(FTask - Value);
  End;
  TStaticText(TControl(FParentAdv.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_1102.SetTask(Value: Single);
begin
  FTask:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    Max:=Round(Value);
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(Value - FAdvance);
  End;
  TStaticText(TControl(FParentTask.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
  TLabel(FParentWeight).Caption:=CurrToStrF(Value, ffGeneral, FDecimal);
end;

procedure TKB001_1102.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Integer;
  ValF: Single;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToInt(E.Text, ValI) then
        begin
          FDecimal:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
    2:  if TryStrToFloat(E.Text, ValF) then
        begin
          Advance:=ValF;
          ST.Caption:=CurrToStrF(FAdvance, ffGeneral, FDecimal);
        end;
    3:  if TryStrToFloat(E.Text, ValF) then
        begin
          Task:=ValF;
          ST.Caption:=CurrToStrF(FTask, ffGeneral, FDecimal);
        end;
  End;
end;

procedure TKB001_1102.OnStateRBClick(Sender: TObject);
begin
  Case TControl(Sender).Tag of
    0:  FState:=s1102vWaiting;
    1:  FState:=s1102vDosing1;
    2:  FState:=s1102vImpulse1;
    3:  FState:=s1102vDosingFinished;
    4:  FState:=s1102vError;
  End;
end;

procedure TKB001_1102.OnWeightTBChange(Sender: TObject);
var
  L: TLabel;
begin
  L:=TLabel(TControl(TControl(Sender).Tag).Tag);  //TB -> B -> L
  L.Caption:=IntToStr(TTrackBar(Sender).Position);
  FWeightCurrent:=TTrackBar(Sender).Position;
end;

procedure TKB001_1102.OnWeightBtnClick(Sender: TObject);
begin
  TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
  //TLabel(FParentWeight).Caption:='0';
  FWeightCurrent:=0;
  FWeightTotal:=0;
end;

procedure TKB001_1102.OnAutoDosing(Sender: TObject);
const
  cDosTime = 10;  //секунд
var
  Val, Adv: Integer;
  TB: TTrackBar;

  procedure _Done;
  begin
    State := s1102vDosingFinished;
//    FWeightTotal := FWeightTotal + FWeightCurrent;
//    TLabel(TControl(TControl(TControl(FParentWeight.Tag).Tag).Tag).Tag).Caption:=
//      CurrToStrF(FWeightTotal, ffGeneral, FDecimal);
  end;

begin
  If not TCheckBox(FParentWeight.Tag).Checked or not (FState in [s1102vDosing1, s1102vImpulse1]) then
    Exit;
  TB := TTrackBar(TControl(FParentWeight.Tag).Tag);

  If TB.Position >= TB.Max then
  Begin
    _Done;
    Exit;
  End
  Else if TB.Position >= TB.SelEnd then
  Begin
    Adv:=3;
    State:=s1102vImpulse1;
  End
  Else
  Begin
    Adv:=1;
    State:=s1102vDosing1;
  End;

  Val := Random(Round(FTask / cDosTime / Adv{ * Pow(10, FDecimal)}));
  If Val = 0 then Val := 1;
  TB.Position := TB.Position + Val;
  If TB.Position = TB.Max then
    _Done;
end;

function TKB001_1102.BytesToSingle(Data: Array of Byte): Single;
var
  ResBytes: Array [0..3] of Byte absolute Result;
begin
  ResBytes[0]:=Data[3];
  ResBytes[1]:=Data[2];
  ResBytes[2]:=Data[1];
  ResBytes[3]:=Data[0];
end;

function TKB001_1102.SingleToBytes(Value: Single): TBytes;
var
  ResBytes: Array [0..3] of Byte absolute Value;
begin
  SetLength(Result, 4);
  Result[0]:=ResBytes[3];
  Result[1]:=ResBytes[2];
  Result[2]:=ResBytes[1];
  Result[3]:=ResBytes[0];
end;

procedure TKB001_1102.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetSingle(Val: OleVariant): Single;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetInt(Val: OleVariant): Integer;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean;
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: Decimal:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
              2: Advance:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
              3: Task:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentWeight.Tag).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TKB001_1102.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtKB001_1102;
  FDecimal:=0;
  FAdvance:=0;
  FTask:=0;
  FState:=s1102vWaiting;
  FWeightCurrent:=0;
  FWeightTotal:=0;
  FAutoDosing:=TTimer.Create(Self);
  FAutoDosing.OnTimer:=OnAutoDosing;
  Inc(CntTB001_1102);
  Caption:='КВ-001 11.02 №'+IntToStr(CntTB001_1102);

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Дес. точка', 22, 1, OnParamBtnClick);
    FParentDec:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Упрежд.', 50, 2, OnParamBtnClick);
    FParentAdv:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Задание', 78, 3, OnParamBtnClick);
    FParentTask:=Parent;
    WinParent:=Self;
  CreateStatesChooser(WinParent, 110,
  ['Ожидание', 'Дозирование', 'Подсыпка', 'Завершено', 'Ошибка'], 2, 0, OnStateRBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateWeightChanger(Parent, 180, OnWeightTBChange, OnWeightBtnClick);
    FParentWeight:=Parent;

  LoadSettings;
end;

destructor TKB001_1102.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FDecimal);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 2);
    SetAttributeNS('value', '', FAdvance);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 3);
    SetAttributeNS('value', '', FTask);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentWeight.Tag).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TKB001_1102.Process(Request: Array of Byte): String;
var
  i, j, l, ByteCnt: Byte;
  RegAdr, RegCnt: Word;
  Val: Cardinal;
  absVal: Array [0..3] of Byte absolute Val;
  arVal: TBytes;
  Weight: Single;
  Response: Array [0..11] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Чтение
  If Request[1] = 3 then  //Контрольная сумма не проверяется
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];

    case TRegs(RegAdr) of
      r1102vWeight:       begin
                            arVal:=SingleToBytes(FWeightCurrent);
                            for i:=Low(arVal) to High(arVal) do
                              absVal[i]:=arVal[i];
                          end;
      r1102vState:        begin
                            Val:=Byte(FState);
                            i:=absVal[0];
                            absVal[0]:=absVal[1];
                            absVal[1]:=i;
                          end;
      r1102vFloatDigits:  Val:=FDecimal;
    end;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=RegCnt * 2;
    j:=0;
    for i:=RegCnt * 2 - 1 downto 0 do
    begin
      Response[i+3]:=absVal[j];
      Inc(j);
    end;
    l:=3 + RegCnt * 2;
    Response[l]:=Lo(CalcCRC_KB(@Response, l));
    Response[l+1]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, l+2);
  End

    //Команда
  Else if (Request[1] = 6) and (Request[2] = 0) and (Request[3] = 0) and (Request[4] = 0) then
  Begin
    case TCommands(Request[5]) of
      c1102vResetDose:
        OnWeightBtnClick(nil);
      c1102vStartDosing:
        begin
          {FWeightCurrent:=0;
          TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
          State:=s1102vDosing1;}
          Weight:=FWeightTotal;
          OnWeightBtnClick(nil);
          FWeightTotal:=Weight;
          State:=s1102vDosing1;
        end;
      c1102vStopDosing:
        begin
          if FState in [s1102vDosing1, s1102vPause1, s1102vImpulse1, s1102vDosingFinished] then
            FWeightTotal:=FWeightTotal+FWeightCurrent;
          State:=s1102vWaiting;
          SetTask(FWeightTotal);
          TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=Round(FWeightTotal);
        end;
    end;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, 6));
    Response[7]:=Hi(CalcCRC_KB(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Запись
  Else if Request[1] = 16 then
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
    ByteCnt:=Request[6];
    if ByteCnt > 0 then
    begin
      j := ByteCnt - 1;
      for i:=7 to 7 + ByteCnt{ * 2} - 1 do
      begin
        //absVal[i-7] := Request[i];
        absVal[j] := Request[i];
        Dec(j);
      end;
    end;
    case TRegs(RegAdr) of
      r1102vAdvanceHard1: Advance:=BytesToSingle(absVal);
      r1102vTarget1:      Task:=BytesToSingle(absVal);
    end;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, l));
    Response[7]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, 8);
  End;
end;

//==================================================================================================
//_____________________________________________TPTC001______________________________________________
//==================================================================================================

procedure TPTC001.SetState(Value: TStates);
  procedure _CheckRB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TRadioButton(FParentState.Controls[i]).Checked:=True;
        Break;
      end;
  end;
begin
  FState:=Value;
  Case FState of
    sPTCvWaiting:   _CheckRB(0);
    sPTCvDosing:    _CheckRB(1);
    sPTCvImpulse:   _CheckRB(2);
  End;
end;

procedure TPTC001.SetAdvance(Value: Single);
begin
  FAdvance:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(FTask - Value);
  End;
  TStaticText(TControl(FParentAdv.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, 2);
end;

procedure TPTC001.SetTask(Value: Single);
begin
  FTask:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    Max:=Round(Value);
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Round(Value - FAdvance);
  End;
  TStaticText(TControl(FParentTask.Tag).Tag).Caption:=CurrToStrF(Value, ffGeneral, 2);
  TLabel(FParentWeight).Caption:=CurrToStrF(Value, ffGeneral, 2);
end;

procedure TPTC001.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Integer;
  ValF: Single;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToFloat(E.Text, ValF) then
        begin
          Advance:=ValF;
          ST.Caption:=CurrToStrF(FAdvance, ffGeneral, 2);
        end;
    2:  if TryStrToFloat(E.Text, ValF) then
        begin
          Task:=ValF;
          ST.Caption:=CurrToStrF(FTask, ffGeneral, 2);
        end;
  End;
end;

procedure TPTC001.OnStateRBClick(Sender: TObject);
begin
  Case TControl(Sender).Tag of
    0:  FState:=sPTCvWaiting;
    1:  FState:=sPTCvDosing;
    2:  FState:=sPTCvImpulse;
  End;
end;

procedure TPTC001.OnWeightTBChange(Sender: TObject);
var
  L: TLabel;
begin
  L:=TLabel(TControl(TControl(Sender).Tag).Tag);  //TB -> B -> L
  L.Caption:=IntToStr(TTrackBar(Sender).Position);
  FWeightCurrent:=TTrackBar(Sender).Position;
end;

procedure TPTC001.OnWeightBtnClick(Sender: TObject);
begin
  TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
  //TLabel(FParentWeight).Caption:='0';
  FWeightCurrent:=0;
  FWeightTotal:=0;
end;

procedure TPTC001.OnAutoDosing(Sender: TObject);
const
  cDosTime = 10;  //секунд
var
  Val, Adv: Integer;
  TB: TTrackBar;

  procedure _Done;
  begin
    State := sPTCvWaiting;
//    FWeightTotal := FWeightTotal + FWeightCurrent;
//    TLabel(TControl(TControl(TControl(FParentWeight.Tag).Tag).Tag).Tag).Caption:=
//      CurrToStrF(FWeightTotal, ffGeneral, FDecimal);
  end;

begin
  If not TCheckBox(FParentWeight.Tag).Checked or not (FState in [sPTCvDosing, sPTCvImpulse]) then
    Exit;
  TB := TTrackBar(TControl(FParentWeight.Tag).Tag);

  If TB.Position >= TB.Max then
  Begin
    _Done;
    Exit;
  End
  Else if TB.Position >= TB.SelEnd then
  Begin
    Adv:=3;
    State:=sPTCvImpulse;
  End
  Else
  Begin
    Adv:=1;
    State:=sPTCvDosing;
  End;

  Val := Random(Round(FTask / cDosTime / Adv));
  If Val = 0 then Val := 1;
  TB.Position := TB.Position + Val;
  If TB.Position = TB.Max then
    _Done;
end;

function TPTC001.BytesToSingle(Data: Array of Byte): Single;
var
  ResBytes: Array [0..3] of Byte absolute Result;
begin
  ResBytes[0]:=Data[3];
  ResBytes[1]:=Data[2];
  ResBytes[2]:=Data[1];
  ResBytes[3]:=Data[0];
end;

function TPTC001.SingleToBytes(Value: Single): TBytes;
var
  ResBytes: Array [0..3] of Byte absolute Value;
begin
  SetLength(Result, 4);
  Result[0]:=ResBytes[3];
  Result[1]:=ResBytes[2];
  Result[2]:=ResBytes[1];
  Result[3]:=ResBytes[0];
end;

procedure TPTC001.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetSingle(Val: OleVariant): Single;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetInt(Val: OleVariant): Integer;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean;
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: Advance:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
              2: Task:=_GetSingle(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentWeight.Tag).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TPTC001.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtPTC001;
  FAdvance:=0;
  FTask:=0;
  FState:=sPTCvWaiting;
  FWeightCurrent:=0;
  FWeightTotal:=0;
  FAutoDosing:=TTimer.Create(Self);
  FAutoDosing.OnTimer:=OnAutoDosing;
  Inc(CntPTC001);
  Caption:='ПТЦ-001 №'+IntToStr(CntPTC001);

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Упрежд.', 22, 1, OnParamBtnClick);
    FParentAdv:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Задание', 50, 2, OnParamBtnClick);
    FParentTask:=Parent;
    WinParent:=Self;
  CreateStatesChooser(WinParent, 82, ['Ожидание', 'Дозирование', 'Подсыпка'], 2, 0, OnStateRBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateWeightChanger(Parent, 132, OnWeightTBChange, OnWeightBtnClick);
    FParentWeight:=Parent;

  LoadSettings;
end;

destructor TPTC001.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FAdvance);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 2);
    SetAttributeNS('value', '', FTask);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentWeight.Tag).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TPTC001.Process(Request: Array of Byte): String;
var
  i, j, l, ByteCnt: Byte;
  RegAdr, RegCnt: Word;
  Val: Cardinal;
  absVal: Array [0..3] of Byte absolute Val;
  arVal: TBytes;
  Weight: Single;
  Response: Array [0..11] of Byte;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Request[0] <> FAddress then Exit;

  FIndicator.Active:=not FIndicator.Active;

    //Чтение
  If Request[1] = 3 then  //Контрольная сумма не проверяется
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];

    case TRegs(RegAdr) of
      rPTCvWeight:        begin
                            arVal:=SingleToBytes(FWeightCurrent);
                            for i:=Low(arVal) to High(arVal) do
                              absVal[i]:=arVal[i];
                          end;
      rPTCvState:         begin
                            Val:=Byte(FState);
                            i:=absVal[0];
                            absVal[0]:=absVal[1];
                            absVal[1]:=i;
                          end;
    end;

    Response[0]:=Request[0];
    Response[1]:=Request[1];
    Response[2]:=RegCnt * 2;
    j:=0;
    for i:=RegCnt * 2 - 1 downto 0 do
    begin
      Response[i+3]:=absVal[j];
      Inc(j);
    end;
    l:=3 + RegCnt * 2;
    Response[l]:=Lo(CalcCRC_KB(@Response, l));
    Response[l+1]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, l+2);
  End

    //Команда
  Else if (Request[1] = 6) and (Request[2] = 0) and (Request[3] = 0) and (Request[4] = 0) then
  Begin
    case TCommands(Request[5]) of
      cPTCvResetDose:
        OnWeightBtnClick(nil);
      cPTCvStartDosing:
        begin
          {FWeightCurrent:=0;
          TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
          State:=s1102vDosing1;}
          Weight:=FWeightTotal;
          OnWeightBtnClick(nil);
          FWeightTotal:=Weight;
          State:=sPTCvDosing;
        end;
      cPTCvStopDosing:
        begin
          if FState in [sPTCvDosing, sPTCvImpulse] then
            FWeightTotal:=FWeightTotal+FWeightCurrent;
          State:=sPTCvWaiting;
          SetTask(FWeightTotal);
          TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=Round(FWeightTotal);
        end;
    end;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, 6));
    Response[7]:=Hi(CalcCRC_KB(@Response, 6));

    Result:=ResponseToStr(@Response, 8);
  End

    //Запись
  Else if Request[1] = 16 then
  Begin
    RegAdr:=Request[2] * 256 + Request[3];
    RegCnt:=Request[4] * 256 + Request[5];
    ByteCnt:=Request[6];
    if ByteCnt > 0 then
    begin
      j := ByteCnt - 1;
      for i:=7 to 7 + ByteCnt{ * 2} - 1 do
      begin
        //absVal[i-7] := Request[i];
        absVal[j] := Request[i];
        Dec(j);
      end;
    end;
    case TRegs(RegAdr) of
      rPTCvAdvanceHard:   Advance:=BytesToSingle(absVal);
      rPTCvTarget:        Task:=BytesToSingle(absVal);
    end;

    for i:=0 to 5 do
      Response[i]:=Request[i];
    Response[6]:=Lo(CalcCRC_KB(@Response, l));
    Response[7]:=Hi(CalcCRC_KB(@Response, l));

    Result:=ResponseToStr(@Response, 8);
  End;
end;

//==================================================================================================
//__________________________________________TMaster110_4____________________________________________
//==================================================================================================

procedure TMaster110_4.SetDecimal(Value: Byte);
begin
  FDecimal:=Value;
  TStaticText(TControl(FParentDec.Tag).Tag).Caption:=IntToStr(FDecimal);
end;

procedure TMaster110_4.SetAdvance(Value: Byte);
begin
  FAdvance:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=FTask - Value;
  End;
  TStaticText(TControl(FParentAdv.Tag).Tag).Caption:=IntToStr(Value);
end;

procedure TMaster110_4.SetTask(Value: Word);
begin
  FTask:=Value;
  With TTrackBar(TControl(FParentWeight.Tag).Tag) do  //L -> CB -> TB
  Begin
    Max:=Value;
    SelStart:=0;
    SelEnd:=0;
    SelEnd:=Value - FAdvance;
  End;
  TStaticText(TControl(FParentTask.Tag).Tag).Caption:=IntToStr(Value);
  TLabel(FParentWeight).Caption:=IntToStr(Value);
end;

procedure TMaster110_4.SetState(Index: Byte; Value: Boolean);
  procedure _CheckCB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TCheckBox(FParentState.Controls[i]).Checked:=Value;
        Break;
      end;
  end;
begin
  FState[Index]:=Value;
  _CheckCB(Index);
  If (Index=4) and Value then
    State[3]:=False;
end;

function TMaster110_4.GetState(Index: Byte): Boolean;
begin
  Result:=FState[Index];
end;

procedure TMaster110_4.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Int64;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToInt64(E.Text, ValI) then
        begin
          FDecimal:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
    2:  if TryStrToInt64(E.Text, ValI) then
        begin
          Advance:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
    3:  if TryStrToInt64(E.Text, ValI) then
        begin
          Task:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
  End;
end;

procedure TMaster110_4.OnStateCBClick(Sender: TObject);
begin
  State[TControl(Sender).Tag]:=TCheckBox(Sender).Checked;
end;

procedure TMaster110_4.OnWeightTBChange(Sender: TObject);
var
  L: TLabel;
begin
  L:=TLabel(TControl(TControl(Sender).Tag).Tag);  //TB -> B -> L
  L.Caption:=IntToStr(TTrackBar(Sender).Position);
  FWeightCurrent:=TTrackBar(Sender).Position;
end;

procedure TMaster110_4.OnWeightBtnClick(Sender: TObject);
begin
  TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;
  //TLabel(FParentWeight).Caption:='0';
  FWeightCurrent:=0;
  FWeightTotal:=0;
end;

procedure TMaster110_4.OnAutoDosing(Sender: TObject);
const
  cDosTime = 10;  //секунд
var
  Val, Adv: Integer;
  TB: TTrackBar;

  procedure _Done;
  begin
    State[0] := False;
    State[3] := False;
    State[4] := True;
    FWeightLast:=FWeightCurrent;
    FWeightTotal := FWeightTotal + FWeightCurrent;
    TLabel(TControl(TControl(TControl(FParentWeight.Tag).Tag).Tag).Tag).Caption:=
      CurrToStrF(FWeightTotal, ffGeneral, FDecimal);
  end;

begin
  If not TCheckBox(FParentWeight.Tag).Checked or not FState[3] then Exit;
  TB := TTrackBar(TControl(FParentWeight.Tag).Tag);

  If TB.Position = TB.Max then
  Begin
    _Done;
    Exit;
  End
  Else if TB.Position >= TB.SelEnd then
    Adv:=3
  Else
  Begin
    Adv:=1;
    State[3]:=True;
    State[4]:=False;
  End;

  Val := Random(Round(FTask / cDosTime / Adv{ * Pow(10, FDecimal)}));
  If Val = 0 then Val := 1;
  TB.Position := TB.Position + Val;
  If TB.Position = TB.Max then
    _Done;
end;

procedure TMaster110_4.CommandReaction(Command: TCommands);
begin
  Case Command of
    cm_4StartDosing:
      begin
        State[0]:=True;
        State[3]:=True;
        TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;   //L -> CB -> TB
      end;
    cm_4StopDosing:
      begin
        State[0]:=False;
        State[3]:=False;
      end;
    cm_4Unload:     ;
    cm_4ErrorReset: ;
    cm_4Flash:      ;
    cm_4ResetDose:
      begin
        if FState[5] then
          Inc(FWeightTotal, FWeightLast)
        else Inc(FWeightTotal, FWeightCurrent);
        TTrackBar(TControl(FParentWeight.Tag).Tag).Position:=0;   //L -> CB -> TB
      end;
    cm_4Netto:
      begin
        State[5]:=not State[5];
        if State[5] then
        begin
          FWeightLast:=FWeightCurrent;
          with TTrackBar(TControl(FParentWeight.Tag).Tag) do
          begin
            Max:=FWeightTotal + FWeightCurrent;
            Position:=Max;
          end;
        end
        else
          with TTrackBar(TControl(FParentWeight.Tag).Tag) do
          begin
            Max:=FTask;
            Position:=FWeightLast;
          end;
      end;
  End;
end;

function TMaster110_4.StatesToByte: Byte;
var
  i: Byte;
begin
  Result:=0;
  For i:=Low(FState) to High(FState) do
    if FState[i] then
      Inc(Result, Round(Pow(2, i)));
end;

procedure TMaster110_4.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetInt(Val: OleVariant): Int64;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean;
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: Decimal:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
              2: Advance:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
              3: Task:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentWeight.Tag).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TMaster110_4.Create(AOwner: TComponent; AAddress: Byte);
var
  i: Byte;
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtMaster110_4;
  FDecimal:=0;
  FAdvance:=0;
  FTask:=0;
  For i:=Low(FState) to High(FState) do
    FState[i]:=False;
  FWeightCurrent:=0;
  FWeightTotal:=0;
  FAutoDosing:=TTimer.Create(Self);
  FAutoDosing.OnTimer:=OnAutoDosing;
  Inc(CntMaster110_4);
  Caption:='Мастер 110.4 №'+IntToStr(CntMaster110_4);

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Дес. точка', 22, 1, OnParamBtnClick);
    FParentDec:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Упрежд.', 50, 2, OnParamBtnClick);
    FParentAdv:=Parent;
    Parent:=Self;
  CreateParamEditor(Parent, 'Задание', 78, 3, OnParamBtnClick);
    FParentTask:=Parent;
    WinParent:=Self;
  CreateStatesChecker(WinParent, 110, ['Старт дозирования', 'Идёт разгрузка',
  'Ожидание продолжения дозирования', 'Идёт дозирование', 'Доза набрана', 'Индикация веса НЕТТО',
  'Включена калибровка времени упреждения', 'Вес зафиксирован'], 1, -1, OnStateCBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateWeightChanger(Parent, 278, OnWeightTBChange, OnWeightBtnClick);
    FParentWeight:=Parent;

  LoadSettings;
end;

destructor TMaster110_4.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FDecimal);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 2);
    SetAttributeNS('value', '', FAdvance);
  End;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 3);
    SetAttributeNS('value', '', FTask);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentWeight.Tag).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TMaster110_4.Process(Request: Array of Byte): String;
var
  Response: Array [0..4] of Byte;
  Val: Word;
  LoByte, HiByte: Byte;

  function _CRC: Byte;
  begin
    Result:=(Response[1] + Response[2] + Response[3]) mod 256;
  end;

begin
  Result:='';

  If not FIsInWork then Exit;

    //Запись в ОЗУ
  If (Request[0] = $F0) and (Request[1] - $80 = FAddress) then
  Begin
    case Request[2] of
      $42:  Decimal:=Request[3];
      $43:  Advance:=Request[3];
      $44:  WordRec(FTask).Lo:=Request[3];
      $45:  WordRec(FTask).Hi:=Request[3];
    end;

    Response[0]:=Request[0];
    Response[1]:=FAddress+$40;
    Response[2]:=Request[4];
    Response[3]:=Request[3];
    Response[4]:=_CRC;

    Result:=ResponseToStr(@Response, 5);
  End

    //Чтение из ОЗУ
  Else if (Request[0] = $F0) and (Request[1] = FAddress) then
  Begin
    Val:=0;
    case Request[2] of
      $42:  Val:=Decimal;
      $43:  Val:=Advance;
      $44:  Val:=Task;
    end;

    Response[0]:=Request[0];
    Response[1]:=FAddress+$40;
    Response[2]:=Lo(Val);
    Response[3]:=Hi(Val);
    Response[4]:=_CRC;

    Result:=ResponseToStr(@Response, 5);
  End

    //Управляющие команды
  Else if (Request[0] = $F0) and (Request[1]-$60 = FAddress) and
    (TCommands(Request[2]) in [cm_4StartDosing, cm_4StopDosing, cm_4Unload, cm_4ErrorReset,
    cm_4Flash, cm_4ResetDose, cm_4Netto]) then
  Begin
    CommandReaction(TCommands(Request[2]));

    Response[0]:=Request[0];
    Response[1]:=FAddress+$40;
    Response[2]:=Request[4];
    Response[3]:=Request[2];
    Response[4]:=_CRC;

    Result:=ResponseToStr(@Response, 5);
  End

    //Информационные команды
  Else if (Request[0] = $F0) and (Request[1]-$60 = FAddress) and
    (TCommands(Request[2]) in [cm_4State, cm_4Version, cm_4Weight]) then
  Begin
    LoByte:=0;
    HiByte:=0;
    case TCommands(Request[2]) of
      cm_4State:
        HiByte:=StatesToByte;
      cm_4Version:  ;
      cm_4Weight:
        begin
          Val:=FWeightCurrent;
          LoByte:=WordRec(Val).Lo;
          HiByte:=WordRec(Val).Hi;
        end;
    end;

    Response[0]:=Request[0];
    Response[1]:=FAddress+$40;
    Response[2]:=LoByte;
    Response[3]:=HiByte;
    Response[4]:=_CRC;

    Result:=ResponseToStr(@Response, 5);
  End

  Else Exit;

  FIndicator.Active:=not FIndicator.Active;
end;

//==================================================================================================
//_______________________________________________TKGD_______________________________________________
//==================================================================================================

procedure TKGD.SetState(Value: TStates);
  procedure _CheckRB(Val: Integer);
  var
    i: Integer;
  begin
    For i:=0 to FParentState.ControlCount-1 do
      if FParentState.Controls[i].Tag=Val then
      begin
        TRadioButton(FParentState.Controls[i]).Checked:=True;
        Break;
      end;
  end;
begin
  FState:=Value;
  Case FState of
    sKGDsWaiting:     _CheckRB(0);
    sKGDsMeasuring:   _CheckRB(1);
  End;
end;

procedure TKGD.SetMeasTime(Value: Cardinal);
begin
  FMeasTime:=Value;
  TStaticText(TControl(FParentMeasTime.Tag).Tag).Caption:=IntToStr(FMeasTime);
end;

procedure TKGD.OnParamBtnClick(Sender: TObject);
var
  E: TEdit;
  ST: TStaticText;
  ValI: Integer;
begin
  E:=TEdit(TControl(Sender).Tag);
  ST:=TStaticText(E.Tag);
  Case ST.Tag of
    1:  if TryStrToInt(E.Text, ValI) then
        begin
          FMeasTime:=ValI;
          ST.Caption:=IntToStr(ValI);
        end;
  End;
end;

procedure TKGD.OnStateRBClick(Sender: TObject);
begin
  Case TControl(Sender).Tag of
    0:  FState:=sKGDsWaiting;
    1:  FState:=sKGDsMeasuring;
  End;
end;

procedure TKGD.OnMeasBtnClick(Sender: TObject);
begin
  TLabel(TControl(FParentMeas.Tag).Tag).Caption:='0';
  FEndTime:=1;  //Таймер завершит самостоятельно, как надо
  //State:=sKGDsWaiting;
end;

procedure TKGD.OnAutoMeasuring(Sender: TObject);
begin
  If not TCheckBox(FParentMeas).Checked or not (FState in [sKGDsMeasuring]) then
    Exit;

  If FEndTime = 0 then
  Begin
    TLabel(TControl(FParentMeas.Tag).Tag).Color:=clBtnFace;
    FEndTime:=GetTickCount + FMeasTime;
    FResult:=-1;
  End;
  If FEndTime >= GetTickCount then
    TLabel(TControl(FParentMeas.Tag).Tag).Caption:=IntToStr(Round((FEndTime - GetTickCount) / 1000))
  Else
  Begin
    TLabel(TControl(FParentMeas.Tag).Tag).Caption:='0';
    State:=sKGDsWaiting;
    FEndTime:=0;
    FResult:=Random(2);
    if FResult = 0 then
      TLabel(TControl(FParentMeas.Tag).Tag).Color:=clLime
    else TLabel(TControl(FParentMeas.Tag).Tag).Color:=clRed;
  End;
end;

procedure TKGD.LoadSettings;
var
  i, j: Byte;
  Path: String;
  Doc: IXMLDocument;
  Root, Device: IXMLNode;

  function _GetInt(Val: OleVariant): Integer;
  begin
    If Val=NULL then
      Result:=0
    Else Result:=Val;
  end;
  function _GetBool(Val: OleVariant): Boolean;
  begin
    If Val=NULL then
      Result:=False
    Else Result:=Val;
  end;

begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and
      (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(FAddress)) then
    begin
      Device:=Root.ChildNodes.Nodes[i];
      FNameControl.Text:=Device.GetAttributeNS('name', '');
      if Device.ChildNodes.Count > 0 then
        for j:=0 to Device.ChildNodes.Count-1 do
          if Device.ChildNodes[j].NodeName = 'param' then
          begin
            case Device.ChildNodes[j].GetAttributeNS('num', '') of
              1: MeasTime:=_GetInt(Device.ChildNodes[j].GetAttributeNS('value', ''));
            end;
          end
          else if Device.ChildNodes[j].NodeName = 'autodos' then
            TCheckBox(FParentMeas).Checked:=
              _GetBool(Device.ChildNodes[j].GetAttributeNS('checked', ''));
    end;
end;

//--------------------------------------------------------------------------------------------------

constructor TKGD.Create(AOwner: TComponent; AAddress: Byte);
var
  Parent: TControl;
  WinParent: TWinControl;
begin
  inherited Create(AOwner);
  FAddress:=AAddress;
  FDevType:=dtKGD;
  FMeasTime:=0;
  FState:=sKGDsWaiting;
  FEndTime:=0;
  FAutoMeasuring:=TTimer.Create(Self);
  FAutoMeasuring.Interval:=50;
  FAutoMeasuring.OnTimer:=OnAutoMeasuring;
  Caption:='КГД';

    //Создание компонентов
    Parent:=Self;
  CreateParamEditor(Parent, 'Время', 22, 1, OnParamBtnClick);
    FParentMeasTime:=Parent;
    WinParent:=Self;
  CreateStatesChooser(WinParent, 54, ['Ожидание', 'Измерение'], 2, 0, OnStateRBClick);
    FParentState:=WinParent;
    Parent:=Self;
  CreateMeasurer(Parent, 84, OnMeasBtnClick);
    FParentMeas:=Parent;

  LoadSettings;
end;

destructor TKGD.Destroy;
var
  Doc: IXMLDocument;
  Device: IXMLNode;
  Path: String;
begin
  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  SaveSettingPrepare(Path, FAddress, Doc, Device);
    //Заполнение
  Device.SetAttributeNS('address', '', FAddress);
  Device.SetAttributeNS('name', '', FNameControl.Text);
  Device.ChildNodes.Clear;
  With Device.AddChild('param') do
  Begin
    SetAttributeNS('num', '', 1);
    SetAttributeNS('value', '', FMeasTime);
  End;
  Device.AddChild('autodos').SetAttributeNS('checked', '', TCheckBox(FParentMeas).Checked);

    //Запись
  Doc.SaveToFile(Path);

  inherited Destroy;
end;

function TKGD.Process(Request: AnsiString): String;
begin
  Result:='';

  If not FIsInWork then Exit;

    //Проверка, адресован ли запрос устройству
  If Pos('kgd', Request) <> 1 then Exit;

  FIndicator.Active:=not FIndicator.Active;

  Result:=Copy(Request, 1, 5);
  Case TCommands(StrToInt(Copy(Request, 4, 2))) of
    cKGDcStart:   begin
                    State:=sKGDsMeasuring;
                    FResult:=-1;
                  end;
    cKGDcStop:    TButton(FParentMeas.Tag).Click;
    cKGDcResult:  case FResult of
                    -1: Result:=Result+':work';
                     0: Result:=Result+':good';
                     1: Result:=Result+':bad';
                  end;
    cKGDcSetTime: SetMeasTime(StrToInt(Copy(Request, 7, 6)));
  Else
    Result:='kgd:error';
  End;
end;

//____________________________________________Функции_______________________________________________
//==================================================================================================

  //Перевод числового ответа в строковый
function ResponseToStr(pResp: PByteArray; Len: Byte): String;
var
  i: Byte;
begin
  For i:=0 to Len-1 do
    Result:=Result + IntToStr(pResp^[i]) + ' ';
  Delete(Result, Length(Result), 1);
end;

function SaveSettingPrepare(Path: String; Address: Byte; out Doc: IXMLDocument;
  out Device: IXMLNode): Boolean;
var
  i: Byte;
  Root: IXMLNode;
begin
  If FileExists(Path) then
  Begin
    Doc:=LoadXMLDocument(Path);
    Root:=Doc.ChildNodes.FindNode(cXMLRootName);
    for i:=0 to Root.ChildNodes.Count-1 do
      if (Root.ChildNodes.Nodes[i].NodeName = cXMLDevice) and 
        (Root.ChildNodes.Nodes[i].GetAttributeNS('address', '') = IntToStr(Address)) then
      begin
        Device:=Root.ChildNodes.Nodes[i];
        Break;
      end;
    if not Assigned(Device) then
      Device:=Root.AddChild(cXMLDevice);
  End
  Else
  Begin
    Doc:=TXMLDocument.Create(nil);
    Doc.Active:=True;
    Doc.Encoding:='UTF-8';
    Root:=Doc.AddChild(cXMLRootName);
    Device:=Root.AddChild(cXMLDevice);
  End;
end;

function CalcCRC16_DIOM(pBuf: PByteArray; len: Integer): Word;
const
  CRCHi: Array [0..255] Of Byte = (
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
  $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40);

  CRCLo: Array [0..255] Of Byte = (
  $00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05, $C5, $C4, $04,
  $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA, $CB, $0B, $C9, $09, $08, $C8,
  $D8, $18, $19, $D9, $1B, $DB, $DA, $1A, $1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC,
  $14, $D4, $D5, $15, $D7, $17, $16, $D6, $D2, $12, $13, $D3, $11, $D1, $D0, $10,
  $F0, $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35, $34, $F4,
  $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B, $FB, $39, $F9, $F8, $38,
  $28, $E8, $E9, $29, $EB, $2B, $2A, $EA, $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C,
  $E4, $24, $25, $E5, $27, $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0,
  $A0, $60, $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64, $A4,
  $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68,
  $78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C,
  $B4, $74, $75, $B5, $77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0,
  $50, $90, $91, $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54,
  $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99, $59, $58, $98,
  $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F, $8D, $4D, $4C, $8C,
  $44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43, $83, $41, $81, $80, $40);
var
  i: Integer;
  ind, Cyc_Hi, Cyc_Lo: Byte;
  CRC: Word;
begin
  Cyc_Hi:=$FF;
  Cyc_Lo:=$FF;
  For i:=0 to len-1  do
  Begin
    ind:=Cyc_Lo xor pBuf^[i];
    Cyc_Lo:=Cyc_Hi xor CRCHi[ind];
    Cyc_Hi:=CRCLo[ind];
  End;
  WordRec(CRC).Lo:=Cyc_Lo;
  WordRec(CRC).Hi:=Cyc_Hi;
  Result:=CRC;
end;

function CalcCRC_Master(pBuf: PByteArray): Word;
var
  i: Integer;
begin
  Result:=0;
  For i:=1 to 3 do
    Result:=Result+pBuf^[i];
  Result:=Result MOD 256;
end;

function CalcCRC_KB(pBuf: PByteArray; len: Byte): Word;
var
  i: Integer;

  function _CalcCRC(crc_f: Word; b: Byte): Word;
  var
    ii: Integer;
    x, y: Word;
    ff: Boolean;
  begin
    y:=b;
    x:=crc_f;
    x:=x xor y;

    For ii:=1 to 8 do
    Begin
      ff:=(x and $0001) = 1;
      x:=x shr 1;
      x:=x and $7FFF;
      if ff then
        x:=x xor $A001;
    End;
    Result:=x;
  end;

begin
  Result:=$FFFF;
  For i:=Low(pBuf^) to len-1 do
    Result:=_CalcCRC(Result, pBuf^[i]);
end;

//--------------------------------------------------------------------------------------------------

procedure CreateOutputs(var Parent: TWinControl; BtnClick: TNotifyEvent; Count: Byte;
  MarginLeft: Integer; Capts: Array of String);
var
  i: Byte;
  SBox: TScrollBox;
  L: TLabel;
  LR: TiLedRound;
  SB: TSpeedButton;
  Text: String;
begin
  (* Создание выходов для МДВВ и МУ *)
  //В Parent приходит TTabSheet, а уходит TScrollBox

  L:=TLabel.Create(Parent);
  L.Caption:='Выходы';
  L.Font.Style:=[fsBold];
  L.Left:=MarginLeft;
  L.Top:=24;
  Parent.InsertControl(L);

  SBox:=TScrollBox.Create(Parent);
  SBox.BorderStyle:=bsNone;
  SBox.Height:=cDXMBtnBoxHeight;
  SBox.Width:=223;
  SBox.Top:=48;
  SBox.Left:=MarginLeft;
  Parent.InsertControl(SBox);

  For i:=0 to Count-1 do
  Begin
    L:=TLabel.Create(Parent);
    L.Alignment:=taCenter;
    L.AutoSize:=False;
    L.Caption:=IntToStr(i);
    L.Left:=0;//MarginLeft;
    L.Top:=22 * i;
    L.Width:=16;
    SBox.InsertControl(L);

    LR:=TiLedRound.Create(Parent);
    LR.Left:={MarginLeft + }19;
    LR.Top:=22 * i;
    LR.Width:=19;
    LR.Tag:=i;
    SBox.InsertControl(LR);

    SB:=TSpeedButton.Create(Parent);
    SB.Caption:='К';
    SB.Cursor:=crHandPoint;
    SB.Font.Size:=8;
    SB.Layout:=blGlyphBottom;
    SB.Left:={MarginLeft + }38;
    SB.Top:=22 * i;
    SB.Height:=19;
    SB.Width:=19;
    SB.Tag:=i;
    SB.OnClick:=BtnClick;
    SBox.InsertControl(SB);

    Text:='';
    if (Length(Capts) > 0) and (i < Length(Capts)) then
      Text:=Capts[i];
    CreateEditedLabel(SBox, Text, 22 * i, 63, 19, 144, Integer(SB));
  End;
  Parent:=SBox;
end;

procedure CreateInputs(var Parent: TWinControl; BtnClick: TNotifyEvent; Count: Byte;
  MarginLeft: Integer; Capts: Array of String);
var
  i: Byte;
  SBox: TScrollBox;
  L: TLabel;
  LR: TiLedRound;
  SB: TSpeedButton;
  Text: String;
begin
  (* Создание входов для МДВВ и МВ *)
  //В Parent приходит TTabSheet, а уходит TScrollBox

  L:=TLabel.Create(Parent);
  L.Caption:='Входы';
  L.Font.Style:=[fsBold];
  L.Left:=MarginLeft;
  L.Top:=24;
  Parent.InsertControl(L);

  SBox:=TScrollBox.Create(Parent);
  SBox.BorderStyle:=bsNone;
  SBox.Height:=cDXMBtnBoxHeight;
  SBox.Width:=223;
  SBox.Top:=48;
  SBox.Left:=MarginLeft;
  SBox.Tag:=1;
  Parent.InsertControl(SBox);

  For i:=0 to Count-1 do
  Begin
    L:=TLabel.Create(Parent);
    L.Alignment:=taCenter;
    L.AutoSize:=False;
    L.Caption:=IntToStr(i);
    L.Left:=0;
    L.Top:=22 * i;
    L.Width:=16;
    SBox.InsertControl(L);

    LR:=TiLedRound.Create(Parent);
    LR.Left:=19;
    LR.Top:=22 * i;
    LR.Width:=19;
    LR.Tag:=i;
    SBox.InsertControl(LR);

    SB:=TSpeedButton.Create(Parent);
    SB.Caption:='К';
    SB.Cursor:=crHandPoint;
    SB.Font.Size:=8;
    SB.Layout:=blGlyphBottom;
    SB.Left:=38;
    SB.Top:=22 * i;
    SB.Height:=19;
    SB.Width:=19;
    SB.Tag:=i;
    SB.OnClick:=BtnClick;
    SBox.InsertControl(SB);

    Text:='';
    if (Length(Capts) > 0) and (i < Length(Capts)) then
      Text:=Capts[i];
    CreateEditedLabel(SBox, Text, 22 * i, 63, 19, 144, Integer(SB));
  End;
  Parent:=SBox;
end;

procedure CreateParamEditor(var Parent: TControl; Name: String; MarginTop: Integer;
  ParamNum: Integer; OnBtnClick: TNotifyEvent);
var
  L: TLabel;
  ST: TStaticText;
  E: TEdit;
  B: TButton;
begin
  (* Создание редактора параметров весовых контроллеров *)
  //B.Tag - ссылка на E
  //E.Tag - ссылка на ST
  //ST.Tag - номер параметра

  L:=TLabel.Create(Parent);
  L.Caption:=Name;
  L.Left:=6;
  L.Top:=MarginTop + 3;
  TWinControl(Parent).InsertControl(L);

  ST:=TStaticText.Create(Parent);
  ST.Alignment:=taCenter;
  ST.AutoSize:=False;
  ST.BorderStyle:=sbsSunken;
  ST.Caption:='';
  ST.Color:=clBlack;
  ST.Font.Color:=clWhite;
  ST.Font.Size:=12;
  ST.Height:=22;
  ST.Left:=81;
  ST.Tag:=ParamNum;
  ST.Top:=MarginTop;
  ST.Width:=70;
  TWinControl(Parent).InsertControl(ST);

  E:=TEdit.Create(Parent);
  E.BevelInner:=bvLowered;
  E.BevelKind:=bkSoft;
  E.BevelOuter:=bvNone;
  E.BevelWidth:=4;
  E.BorderStyle:=bsNone;
  E.Height:=22;
  E.Left:=157;
  E.Text:='';
  E.Tag:=Integer(ST);
  E.Top:=MarginTop;
  E.Width:=70;
  TWinControl(Parent).InsertControl(E);

  B:=TButton.Create(Parent);
  B.Caption:='Задать';
  B.Height:=22;
  B.Left:=233;
  B.Tag:=Integer(E);
  B.Top:=MarginTop;
  B.Width:=70;
  B.OnClick:=OnBtnClick;
  TWinControl(Parent).InsertControl(B);

  Parent:=B;
end;

procedure CreateStatesChooser(var Parent: TWinControl; MarginTop: Integer; Names: Array of String;
  ColCnt: Byte; DefItemIndex: Integer; OnRBClick: TNotifyEvent);
var
  GP: TGridPanel;
  RB: TRadioButton;
  i, RowCnt: Integer;
begin
  (* Создание списка состояний *)
  //Нумерация начинается с 0

  RowCnt:=Length(Names) div ColCnt + Integer(Boolean(Length(Names) mod ColCnt));
  GP:=TGridPanel.Create(Parent);
  GP.BevelInner:=bvRaised;
  GP.BevelOuter:=bvLowered;
  GP.Caption:='';
  GP.Height:=RowCnt * 20;
  GP.Left:=6;
  GP.Top:=MarginTop;
  GP.Width:=297;

  GP.ColumnCollection.Clear;
  GP.RowCollection.Clear;
  For i:=0 to ColCnt-1 do
    GP.ColumnCollection.Add.Value:=100 / ColCnt;
  For i:=0 to RowCnt-1 do
    GP.RowCollection.Add.Value:=100 / RowCnt;
  For i:=Low(Names) to High(Names) do
  Begin
    RB:=TRadioButton.Create(GP);
    RB.Align:=alClient;
    RB.Caption:=Names[i];
    if i=DefItemIndex then
      RB.Checked:=True;
    RB.Tag:=i;
    RB.OnClick:=OnRBClick;
    GP.InsertControl(RB);
  End;
  Parent.InsertControl(GP);

  Parent:=GP;
end;

procedure CreateStatesChecker(var Parent: TWinControl; MarginTop: Integer; Names: Array of String;
  ColCnt: Byte; DefItemIndex: Integer; OnRBClick: TNotifyEvent);
var
  GP: TGridPanel;
  CB: TCheckBox;
  i, RowCnt: Integer;
begin
  (* Создание списка состояний *)
  //Нумерация начинается с 0

  RowCnt:=Length(Names) div ColCnt + Integer(Boolean(Length(Names) mod ColCnt));
  GP:=TGridPanel.Create(Parent);
  GP.BevelInner:=bvRaised;
  GP.BevelOuter:=bvLowered;
  GP.Caption:='';
  GP.Height:=RowCnt * 20;
  GP.Left:=6;
  GP.Top:=MarginTop;
  GP.Width:=297;

  GP.ColumnCollection.Clear;
  GP.RowCollection.Clear;
  For i:=0 to ColCnt-1 do
    GP.ColumnCollection.Add.Value:=100 / ColCnt;
  For i:=0 to RowCnt-1 do
    GP.RowCollection.Add.Value:=100 / RowCnt;
  For i:=Low(Names) to High(Names) do
  Begin
    CB:=TCheckBox.Create(GP);
    CB.Align:=alClient;
    CB.Caption:=Names[i];
    if i=DefItemIndex then
      CB.Checked:=True;
    CB.Tag:=i;
    CB.OnClick:=OnRBClick;
    GP.InsertControl(CB);
  End;
  Parent.InsertControl(GP);

  Parent:=GP;
end;

procedure CreateWeightChanger(var Parent: TControl; MarginTop: Integer; OnTBChange,
  OnBtnClick: TNotifyEvent);
var
  L: TLabel;
  B: TButton;
  TB: TTrackBar;
  CB: TCheckBox;
begin
  (* Создание компонентов по изменению веса *)
  //L.Tag - ссылка на CB    //последняя
  //CB.Tag - ссылка на TB
  //TB.Tag - ссылка на B
  //B.Tag - ссылка на L     //третья

  L:=TLabel.Create(Parent);
  L.Caption:='Текущий вес:';
  L.Left:=6;
  L.Top:=MarginTop;
  TWinControl(Parent).InsertControl(L);

  L:=TLabel.Create(Parent); //Начало
  L.Caption:='0';
  L.Left:=11;
  L.Top:=MarginTop + 40;
  TWinControl(Parent).InsertControl(L);

  L:=TLabel.Create(Parent); //Вес
  L.Caption:='0';
  L.Left:=96;
  L.Top:=MarginTop;
  TWinControl(Parent).InsertControl(L);

  B:=TButton.Create(Parent);
  B.Caption:='Сбросить НЕТТО';
  B.Height:=17;
  B.Left:=175;
  B.Tag:=Integer(L);
  B.Top:=MarginTop;
  B.Width:=128;
  B.OnClick:=OnBtnClick;
  TWinControl(Parent).InsertControl(B);

  TB:=TTrackBar.Create(nil);
  TB.Height:=25;
  TB.Left:=6;
  TB.Max:=0;
  TB.ThumbLength:=16;
  TB.TickStyle:=tsManual;
  TB.Tag:=Integer(B);
  TB.Top:=MarginTop + 18;
  TB.Width:=288;
  TB.OnChange:=OnTBChange;
  TWinControl(Parent).InsertControl(TB);

  CB:=TCheckBox.Create(Parent);
  CB.Caption:='';
  CB.Checked:=True;
  CB.Height:=12;
  CB.Left:=291;
  CB.Top:=MarginTop + 21;
  CB.Width:=12;
  CB.Tag:=Integer(TB);
  TWinControl(Parent).InsertControl(CB);

  L:=TLabel.Create(Parent); //Конец
  L.Alignment:=taRightJustify;
  L.Caption:='0';
  L.Left:=283;
  L.Tag:=Integer(CB);
  L.Top:=MarginTop + 40;
  TWinControl(Parent).InsertControl(L);

  Parent:=L;
end;

function CreateEditedLabel(Parent: TWinControl; AText: String; ATop, ALeft, AHeight, AWidth,
  ATag: Integer): TEdit;
begin
  (* Создание метки с возвожностью изменения *)

  Result:=TEdit.Create(Parent);
  With Result do
  Begin
    AutoSize:=False;
    BevelInner:=bvNone;
    BevelKind:=bkNone;//bkSoft;
//    BevelWidth:=2;
    BorderStyle:=bsNone;
    Color:=clBtnFace;//clWindow;
    Height:=AHeight;
    Left:=ALeft;
    TabStop:=False;
    Tag:=ATag;
    Text:=AText;
    Top:=ATop;
    Width:=AWidth;
    OnEnter:=TSomeClass.OnEditedLabelEnter;
    OnExit:=TSomeClass.OnEditedLabelExit;
  End;
  Parent.InsertControl(Result);
end;

procedure CreateMeasurer(var Parent: TControl; MarginTop: Integer; OnBtnClick: TNotifyEvent);
var
  L: TLabel;
  B: TButton;
  TB: TTrackBar;
  CB: TCheckBox;
begin
  (* Создание компонентов для измерения *)
  //CB.Tag - ссылка на B
  //B.Tag - ссылка на L (таймер)

  L:=TLabel.Create(Parent);
  L.Caption:='Обратный отсчёт:';
  L.Left:=6;
  L.Top:=MarginTop;
  TWinControl(Parent).InsertControl(L);

  L:=TLabel.Create(Parent); //Таймер
  L.Caption:='0';
  L.Left:=128;
  L.Top:=MarginTop;
  TWinControl(Parent).InsertControl(L);

  B:=TButton.Create(Parent);
  B.Caption:='Завершить';
  B.Height:=17;
  B.Left:=193;
  B.Tag:=Integer(L);
  B.Top:=MarginTop;
  B.Width:=92;
  B.OnClick:=OnBtnClick;
  TWinControl(Parent).InsertControl(B);

  CB:=TCheckBox.Create(Parent);
  CB.Caption:='';
  CB.Checked:=True;
  CB.Height:=12;
  CB.Left:=291;
  CB.Top:=MarginTop + 2;
  CB.Width:=12;
  CB.Tag:=Integer(B);
  TWinControl(Parent).InsertControl(CB);

  Parent:=CB;
end;

function FindDXMLed(Num: Byte; Parent: TWinControl): TiLedRound;
var
  i: Integer;
begin
  (* Поиск индикатора по порядковому номеру в устройствах ввода-вывода *)

  Result:=nil;
  For i:=0 to Parent.ControlCount-1 do
    if Parent.Controls[i] is TiLedRound then
      if TiLedRound(Parent.Controls[i]).Tag=Num then
        Exit(TiLedRound(Parent.Controls[i]));
end;

//==================================================================================================

initialization
  CntDIOM:=0;
  CntDIM_16D:=0;
  CntDIM_32DN:=0;
  CntDOM_32R:=0;
  CntTB001_081:=0;
  CntTB001_091:=0;
  CntTB001_1102:=0;
  CntPTC001:=0;
  CntMaster110_4:=0;

finalization

end.

По окончанию дозирования КВ-001 переходит в паузу, сохраняя отдозированный вес
После команды Стоп отображается общий вес
