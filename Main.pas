unit Main;

interface

uses
  SysUtils, Forms, ComCtrls, Classes, Controls, ActnList, ScktComp, iComponent, iVCLComponent,
  iCustomComponent, iLed, iLedRound, Buttons, StdCtrls, StrUtils, XMLDoc, XMLIntf, ExtCtrls;

type
  TfMain = class(TForm)
    pcMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    alMain: TActionList;
    actClose: TAction;
    TabSheet4: TTabSheet;
    iLedRound1: TiLedRound;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBox1: TScrollBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label7: TLabel;
    Label8: TLabel;
    iLedRound2: TiLedRound;
    iLedRound3: TiLedRound;
    Label11: TLabel;
    SpeedButton6: TSpeedButton;
    iLedRound6: TiLedRound;
    stResIM4: TStaticText;
    Edit1: TEdit;
    Button1: TButton;
    Label12: TLabel;
    StaticText1: TStaticText;
    Edit2: TEdit;
    Button2: TButton;
    Label13: TLabel;
    StaticText2: TStaticText;
    Edit3: TEdit;
    Button3: TButton;
    TrackBar1: TTrackBar;
    Label14: TLabel;
    Label15: TLabel;
    Button4: TButton;
    GridPanel1: TGridPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    CheckBox1: TCheckBox;
    Label16: TLabel;
    Label17: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    ScrollBox2: TScrollBox;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Label3: TLabel;
    Label4: TLabel;
    iLedRound4: TiLedRound;
    iLedRound5: TiLedRound;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    TabSheet5: TTabSheet;
    Label5: TLabel;
    SpeedButton7: TSpeedButton;
    Label6: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    iLedRound7: TiLedRound;
    StaticText3: TStaticText;
    Edit10: TEdit;
    Button5: TButton;
    StaticText4: TStaticText;
    Edit11: TEdit;
    Button6: TButton;
    StaticText5: TStaticText;
    Edit12: TEdit;
    Button7: TButton;
    TrackBar2: TTrackBar;
    Button8: TButton;
    GridPanel2: TGridPanel;
    CheckBox2: TCheckBox;
    Edit13: TEdit;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    RadioButton5: TRadioButton;
    TabSheet6: TTabSheet;
    Label21: TLabel;
    SpeedButton8: TSpeedButton;
    Label24: TLabel;
    Label25: TLabel;
    iLedRound8: TiLedRound;
    StaticText6: TStaticText;
    Edit14: TEdit;
    Button9: TButton;
    Button12: TButton;
    GridPanel3: TGridPanel;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    CheckBox11: TCheckBox;
    Edit17: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SS: TServerSocket;
    procedure SSClientOnRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure SSClientOnError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

uses Devices, Loader;

//_____________________________________________Форма________________________________________________
//==================================================================================================

procedure TfMain.FormCreate(Sender: TObject);
begin
  (* Создание окна *)

    //Удаление тестовых
  While pcMain.PageCount>0 do pcMain.Pages[0].Free;

    //Загрузка данных
  If not ShowLoader then
  Begin
    Application.Terminate;
    Abort;
    Exit;
  End;

  pcMain.ActivePageIndex:=0;

  SS:=TServerSocket.Create(nil);
  //SS.Port:=1200;
  SS.Port:=1333;
  SS.OnClientRead:=SSClientOnRead;
  SS.OnClientError:=SSClientOnError;
  SS.Open;
end;

procedure TfMain.FormShow(Sender: TObject);
var
  i: Integer;
  Path: String;
  Doc: IXMLDocument;
  Root: IXMLNode;
begin
  (* Показ окна *)

  Top:=(Screen.Height - Height) div 2;
  Left:=(Screen.Width - Width) div 2;

  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cXMLRootName);
  For i:=0 to Root.ChildNodes.Count-1 do
    if (Root.ChildNodes.Nodes[i].NodeName = cXMLFormSettings) then
      with Root.ChildNodes.Nodes[i] do
      begin
        Height:=GetAttributeNS('height', '');
        Width:=GetAttributeNS('width', '');
        Top:=GetAttributeNS('top', '');
        Left:=GetAttributeNS('left', '');
      end;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
  Doc: IXMLDocument;
  Root, Sets: IXMLNode;
  Path: String;
begin
  (* Закрытие окна *)

  SS.Close;
  FreeAndNil(SS);

  Path:=ExtractFilePath(ParamStr(0))+'Settings\'+EmulatorName+'.xml';
  If FileExists(Path) then
  Begin
    Doc:=LoadXMLDocument(Path);
    Root:=Doc.ChildNodes.FindNode(cXMLRootName);
    for i:=0 to Root.ChildNodes.Count-1 do
      if Root.ChildNodes.Nodes[i].NodeName = cXMLFormSettings then
      begin
        Sets:=Root.ChildNodes.Nodes[i];
        Break;
      end;
    if not Assigned(Sets) then
      Sets:=Root.AddChild(cXMLFormSettings);
  End
  Else
  Begin
    Doc:=TXMLDocument.Create(nil);
    Doc.Active:=True;
    Doc.Encoding:='UTF-8';
    Root:=Doc.AddChild(cXMLRootName);
    Sets:=Root.AddChild(cXMLFormSettings);
  End;
    //Заполнение
  Sets.SetAttributeNS('height', '', Height);
  Sets.SetAttributeNS('width', '', Width);
  Sets.SetAttributeNS('top', '', Top);
  Sets.SetAttributeNS('left', '', Left);
    //Запись
  Doc.SaveToFile(Path);
end;

//____________________________________________Действия______________________________________________
//==================================================================================================

procedure TfMain.actCloseExecute(Sender: TObject);
begin
  Close;
end;

//_____________________________________________Методы_______________________________________________
//==================================================================================================

procedure TfMain.SSClientOnRead(Sender: TObject; Socket: TCustomWinSocket);
const
  cMark = #1;
var
  ReceiveText, Str, Val, SendText: AnsiString;
  p, i: Integer;
  Request: Array of Byte;
  DigitsOnly: Boolean;
begin
  ReceiveText:=Socket.ReceiveText;
  If (ReceiveText='') or (ReceiveText[1]<>cMark) then Exit;

  While Pos(cMark, ReceiveText)=1 do
  Begin
      //Извлечение сообщения
    p:=PosEx(cMark, ReceiveText, 2);
    if p=0 then p:=Length(ReceiveText)+1;
    Str:=Copy(ReceiveText, 2, p-2);

    DigitsOnly:=True;
    for i:=1 to Length(Str) do
      DigitsOnly:=DigitsOnly and (Str[i] in ['0'..'9', ' ']); //And space

      //Преобразование сообщения
    if DigitsOnly then
    begin
      Str:=Str+' ';
      Val:='';
      for i:=1 to Length(Str) do
      begin
        if Str[i]<>' ' then
          Val:=Val+Str[i]
        else
          if Val<>'' then
          begin
            SetLength(Request, Length(Request)+1);
            Request[Length(Request)-1]:=StrToInt(Val);
            Val:='';
          end;
      end;
    end;

    SendText:='';
    for i:=0 to pcMain.PageCount-1 do
    begin
        //Попытка выполнить команду
      case TDevice(pcMain.Pages[i]).DevType of
        dtDIOM:         SendText:=TDIOM(pcMain.Pages[i]).Process(Request);
        dtDIM_16D:      SendText:=TDIM_16D(pcMain.Pages[i]).Process(Request);
        dtDIM_32DN:     SendText:=TDIM_32DN(pcMain.Pages[i]).Process(Request);
        dtDOM_32R:      SendText:=TDOM_32R(pcMain.Pages[i]).Process(Request);
        dtKB001_081:    SendText:=TKB001_081(pcMain.Pages[i]).Process(Request);
        dtKB001_091:    SendText:=TKB001_091(pcMain.Pages[i]).Process(Request);
        dtKB001_1102:   SendText:=TKB001_1102(pcMain.Pages[i]).Process(Request);
        dtPTC001:       SendText:=TPTC001(pcMain.Pages[i]).Process(Request);
        dtMaster110_4:  SendText:=TMaster110_4(pcMain.Pages[i]).Process(Request);
        dtKGD:          SendText:=TKGD(pcMain.Pages[i]).Process(Str);
      end;

        //Отправка сообщения
      if SendText<>'' then
      begin
        Socket.SendText(#1+SendText);
        Break;
      end;
    end;
    Delete(ReceiveText, 1, Length(Str)+1);
  End;
end;

procedure TfMain.SSClientOnError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  ErrorCode:=0;
end;

end.
