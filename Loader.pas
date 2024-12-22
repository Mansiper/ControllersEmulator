unit Loader;

interface

uses XMLDoc, XMLIntf, SysUtils, Forms, StdCtrls, Controls, Classes, Generics.Collections, ComCtrls;

type
  TxDevices = (dDIOM, dDIM_16D, dDIM_32DN, dDOM_32R, dKB001_081, dKB001_091, dKB001_D, dKB001_1102,
    dPTC001, dMaster110_4, dKGD);
  TxDevice = class
    DevType: TxDevices;
    Address: Byte;
  end;
  TxEmulator = class
    Name: String;
    Devices: TObjectList<TxDevice>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TfLoader = class(TForm)
    lbDevices: TListBox;
    cbDevices: TComboBox;
    bDevAdd: TButton;
    bDevDel: TButton;
    Label2: TLabel;
    eName: TEdit;
    Label3: TLabel;
    cbEmulators: TComboBox;
    bOk: TButton;
    bCancel: TButton;
    bEmulDel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bOkClick(Sender: TObject);
    procedure bDevAddClick(Sender: TObject);
    procedure bDevDelClick(Sender: TObject);
    procedure cbEmulatorsChange(Sender: TObject);
    procedure bEmulDelClick(Sender: TObject);
    procedure eNameChange(Sender: TObject);
  private
    const
      cDataFileName = 'Settings\emulators.xml';
      cRootName = 'emulators';
    var
      FEmuls: TObjectList<TxEmulator>;
      FDevs: TObjectList<TxDevice>;
      FActiveEmul: Integer;
      FModified: Boolean;

    function SaveEmulator: Boolean;
  end;

var
  fLoader: TfLoader;

function ShowLoader: Boolean;

implementation

{$R *.dfm}

uses RazMsgDlgs, Main, Devices;

//==================================================================================================

function ShowLoader: Boolean;
begin
  (* Показ окна загрузки *)

  Result:=False;
  Try
    fLoader:=TfLoader.Create(nil);
    Result:=fLoader.ShowModal=mrOk;
    FreeAndNil(fLoader);
  Except
    on E: Exception do
    begin
      MsgDlg(mzError, 'Не удалось показать окно "'+fLoader.Caption+'": '+E.Message,
        mtError, mbsOk, mnsOk);
      FreeAndNil(fLoader);
    end;
  End;
end;

//____________________________________________Классы________________________________________________
//==================================================================================================

constructor TxEmulator.Create;
begin
  Devices:=TObjectList<TxDevice>.Create;
end;

destructor TxEmulator.Destroy;
begin

  inherited;
end;

//____________________________________________Форма_________________________________________________
//==================================================================================================

procedure TfLoader.FormCreate(Sender: TObject);
var
  Doc: IXMLDocument;
  Root, Emul: IXMLNode;
  Path: String;
  i, j: Integer;
  E: TxEmulator;
  D: TxDevice;
begin
  (* Создание окна *)

  FEmuls:=TObjectList<TxEmulator>.Create;
  FDevs:=TObjectList<TxDevice>.Create;
  FActiveEmul:=-1;
  FModified:=False;

    //Загрузка файла (в конце конструктора!)
  Path:=ExtractFilePath(ParamStr(0))+cDataFileName;
  If not FileExists(Path) then Exit;

  Doc:=LoadXMLDocument(Path);
  Root:=Doc.ChildNodes.FindNode(cRootName);
  If not Assigned(Root) then Exit;
  (*
    <Emulators>
      <Emul name>
        <Device type address />
      </Emul>
    </Emulators>
  *)
  For i:=0 to Root.ChildNodes.Count-1 do
  Begin
    Emul:=Root.ChildNodes.Nodes[i];
    E:=TxEmulator.Create;
    E.Name:=Emul.GetAttributeNS('name', '');
    FEmuls.Add(E);
    for j:=0 to Emul.ChildNodes.Count-1 do
      with Emul.ChildNodes.Nodes[j] do
      begin
        D:=TxDevice.Create;
        D.DevType:=TxDevices(GetAttributeNS('type', ''));
        D.Address:=GetAttributeNS('address', '');
        FEmuls[i].Devices.Add(D);
      end;
    cbEmulators.Items.Add(E.Name);
  End;
  Doc:=nil;
end;

procedure TfLoader.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  (* Закрытие окна *)

  {While FEmuls.Count>0 do
  Begin
    while FEmuls[0].Devices.Count>0 do
    begin
      FEmuls[0].Devices[0].Free;
      FEmuls[0].Devices.Delete(0);
    end;
    FEmuls[0].Free;
    FEmuls.Delete(0);
  End;
  While FDevs.Count>0 do
  Begin
    FDevs[0].Free;
    FDevs.Delete(0);
  End;}
  FreeAndNil(FEmuls);
  FreeAndNil(FDevs);
end;

//___________________________________________Методы_________________________________________________
//==================================================================================================

function TfLoader.SaveEmulator: Boolean;
var
  i: Integer;
  OldName, NewName: String;
begin
  (* Сохранение изменений в эмуляторе *)

  If not FModified and (FEmuls.Count=0) and (FDevs.Count=0) then
  Begin
    MsgDlg(mzInform, 'Создайте эмулятор с хотя бы одним устройством.', mtInformation, mbsOk, mnsOk);
    Exit(False);
  End;

  If not FModified then Exit(True);

  Result:=False;
  If MsgDlg(mzConfirm, 'Эмулятор был изменён. Сохранить изменения?', mtConfirmation,
  mbsYesNo, mnsYesNo)<>mrYes then
    Exit;

  If eName.Text='' then
  Begin
    MsgDlg(mzInform, 'Введите название эмулятора.', mtInformation, mbsOk, mnsOk);
    eName.SetFocus;
    Exit;
  End
  Else if lbDevices.Count=0 then
  Begin
    MsgDlg(mzInform, 'Добавьте устройства.', mtInformation, mbsOk, mnsOk);
    cbDevices.SetFocus;
    Exit;
  End;

  If FActiveEmul=-1 then
  Begin
    FEmuls.Add(TxEmulator.Create);
    FActiveEmul:=FEmuls.Count-1;
  End;

  OldName:=FEmuls[FActiveEmul].Name;
  NewName:=eName.Text;
  FEmuls[FActiveEmul].Name:=eName.Text;
  Try
    FEmuls[FActiveEmul].Devices.Clear;
  Except
  End;
  FEmuls[FActiveEmul].Devices.Free;
  FEmuls[FActiveEmul].Devices:=TObjectList<TxDevice>.Create;
  For i:=0 to FDevs.Count-1 do
  Begin
    FEmuls[FActiveEmul].Devices.Add(TxDevice.Create);
    FEmuls[FActiveEmul].Devices.Last.DevType:=FDevs[i].DevType;
    FEmuls[FActiveEmul].Devices.Last.Address:=FDevs[i].Address;
  End;
  FDevs.Clear;

    //Переименование файла настроек эмулятора
  If OldName<>NewName then
    RenameFile(ExtractFilePath(ParamStr(0))+'Settings\'+OldName+'.xml',
               ExtractFilePath(ParamStr(0))+'Settings\'+NewName+'.xml');

  Result:=True;
end;

//_____________________________________Компоненты_на_форме__________________________________________
//==================================================================================================

procedure TfLoader.eNameChange(Sender: TObject);
begin
  (* Изменение названия эмулятора *)
  FModified:=True;
end;

procedure TfLoader.bDevAddClick(Sender: TObject);
var
  i: Integer;
  Str: String;
  Address: Integer;
  Dev: TxDevice;

  function _GetType: TxDevices;
  begin
    (* Чтобы добавление нового устройства в любое место не требовало изменений номеров в настройках *)
    //Result:=TxDevices(cbDevices.ItemIndex);

         If cbDevices.Text='МДВВ' then          Result:=dDIOM
    Else if cbDevices.Text='МВ110-16Д' then     Result:=dDIM_16D
    Else if cbDevices.Text='МВ110-32ДН' then    Result:=dDIM_32DN
    Else if cbDevices.Text='МУ110-32Р' then     Result:=dDOM_32R
    Else if cbDevices.Text='КВ-001 v081' then   Result:=dKB001_081
    Else if cbDevices.Text='КВ-001 v091' then   Result:=dKB001_091
    Else if cbDevices.Text='КВ-001 Д' then      Result:=dKB001_D
    Else if cbDevices.Text='КВ-001 v11.02' then Result:=dKB001_1102
    Else if cbDevices.Text='ПТЦ-001' then       Result:=dPTC001
    Else if cbDevices.Text='Мастер 110.4' then  Result:=dMaster110_4
    Else if cbDevices.Text='КГД' then           Result:=dKGD;
  end;

begin
  (* Добавить устройство *)

  If cbDevices.Text='' then
  Begin
    MsgDlg(mzInform, 'Выберите устройство.', mtInformation, mbsOk, mnsOk);
    cbDevices.SetFocus;
    Exit;
  End;

  Address:=0;
  Str:=InputDlg('Добавление устройства '+cbDevices.Text, 'Введите сетевой адрес', '', #1);
  If Str=#1 then Exit;
  If not TryStrToInt(Str, Address) or (Address<1) or (Address>247) then
  Begin
    MsgDlg(mzError, 'Введите число от 1 до 247!', mtError, mbsOk, mnsOk);
    Exit;
  End;

  For i:=0 to FDevs.Count-1 do
    if FDevs[i].Address=Address then
    begin
      MsgDlg(mzError, 'Устройство с указанным адресом уже существует.', mtError, mbsOk, mnsOk);
      Exit;
    end;

  Dev:=TxDevice.Create;
  Dev.DevType:=_GetType;
  Dev.Address:=Address;
  FDevs.Add(Dev);
  lbDevices.Items.Add(cbDevices.Text+' ('+IntToStr(Address)+')');
  If lbDevices.Count>0 then
    lbDevices.Selected[0]:=True;

  FModified:=True;
end;

procedure TfLoader.bDevDelClick(Sender: TObject);
begin
  (* Удалить устройство *)

  If lbDevices.ItemIndex=-1 then Exit;
  FDevs.Delete(lbDevices.ItemIndex);
  lbDevices.Items.Delete(lbDevices.ItemIndex);
  FModified:=True;
  If lbDevices.Count>0 then
    lbDevices.Selected[0]:=True;
end;

procedure TfLoader.cbEmulatorsChange(Sender: TObject);
var
  i: Integer;
begin
  (* Выбор эмулятора *)

  If not SaveEmulator then Exit;

    //Загрузка
  FActiveEmul:=cbEmulators.ItemIndex;
  eName.Text:=FEmuls[FActiveEmul].Name;
  FDevs.Clear;
  lbDevices.Clear;
  For i:=0 to FEmuls[FActiveEmul].Devices.Count-1 do
  Begin
    FDevs.Add(TxDevice.Create);
    FDevs[i].DevType:=FEmuls[FActiveEmul].Devices[i].DevType;
    FDevs[i].Address:=FEmuls[FActiveEmul].Devices[i].Address;

    lbDevices.Items.Add(cbDevices.Items[Integer(FEmuls[FActiveEmul].Devices[i].DevType)]+
      ' ('+IntToStr(FEmuls[FActiveEmul].Devices[i].Address)+')' );
  End;
  If lbDevices.Count>0 then
    lbDevices.Selected[0]:=True;

  FModified:=False;
end;

procedure TfLoader.bEmulDelClick(Sender: TObject);
begin
  (* Удалить эмулятор *)

  If cbEmulators.Items.Count=0 then Exit;
  FEmuls.Delete(cbEmulators.ItemIndex);
  cbEmulators.Items.Delete(cbEmulators.ItemIndex);
  cbEmulators.Text:='';
  lbDevices.Clear;
  FActiveEmul:=-1;
  FModified:=False;
end;

procedure TfLoader.bOkClick(Sender: TObject);
var
  i, j: Integer;
  Doc: TXMLDocument;
  Root, Emul: IXMLNode;
  TS: TTabSheet;
begin
  (* Выбрать/Сохранить *)

  If not SaveEmulator then Exit;

  If FActiveEmul=-1 then Exit;

    //Создание
  Doc:=TXMLDocument.Create(nil);
  Doc.Active:=True;
  Doc.Encoding:='UTF-8';
  Root:=Doc.AddChild(cRootName);
  (*
    <Emulators>
      <Emul name>
        <Device type address />
      </Emul>
    </Emulators>
  *)
    //Заполнение
  For i:=0 to FEmuls.Count-1 do
  Begin
    Emul:=Root.AddChild('emul');
      Emul.SetAttributeNS('name', '', FEmuls[i].Name);  //ToDo 1: переименовывать файл настроек
      for j:=0 to FEmuls[i].Devices.Count-1 do
        with Emul.AddChild('device') do
        begin
          SetAttributeNS('type', '', Integer(FEmuls[i].Devices[j].DevType));
          SetAttributeNS('address', '', FEmuls[i].Devices[j].Address);
        end;
  End;

    //Запись
  Doc.SaveToFile(ExtractFilePath(ParamStr(0))+cDataFileName);

    //Создание вкладок
  EmulatorName:=FEmuls[FActiveEmul].Name;
  TS:=nil;
  For i:=0 to FEmuls[FActiveEmul].Devices.Count-1 do
  Begin
    case FEmuls[FActiveEmul].Devices[i].DevType of
      dDIOM:        TS:=TDIOM.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dDIM_16D:     TS:=TDIM_16D.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dDIM_32DN:    TS:=TDIM_32DN.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dDOM_32R:     TS:=TDOM_32R.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dKB001_081:   TS:=TKB001_081.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dKB001_091:   TS:=TKB001_091.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dKB001_1102:  TS:=TKB001_1102.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dMaster110_4: TS:=TMaster110_4.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dKGD:         TS:=TKGD.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
      dPTC001:      TS:=TPTC001.Create(fMain.pcMain, FEmuls[FActiveEmul].Devices[i].Address);
    else
      Continue;
    end;
    TS.PageControl:=fMain.pcMain;
  End;

  fMain.Caption:='Эмулятор контроллеров / '+EmulatorName;

  ModalResult:=mrOk;
end;

end.

Добавить возможность изменения порядка
