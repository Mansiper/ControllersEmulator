unit RazMsgDlgs;

  //Если опредено, текст сообщения пишется в список последних сообщений
{$DEFINE ADD_TO_MSGSLIST}

interface

uses Dialogs, Forms, StdCtrls, ExtCtrls, SysUtils, Graphics, Types, Windows,
  Controls, Variants, Classes, Consts;

type
  TTimerEvent = (teClick, teEnable);

  TRazTimer = class
    destructor Destroy; override;
  private
    FSec: Word;         //Количество секунд
    FWindow: TForm;     //Форма сообщения
    FTimer: TTimer;     //Таймер (для обратного отсчёта или переноса фокуса)
    FButton: TButton;   //Кнопка для нажатия
    FBtnCaption: String;//Заголовок кнопки (без цифр времени)
    FTimerAB: TTimer;   //Таймер для уменьшения прозрачности
    FABSec: Word;       //Время, с которого начинать уменьшать прозрачность
    FABDecValue: Byte;  //Величина, на которую уменьшать прозрачность при срабатывании таймера прозрачности
    FTimerEvent: TTimerEvent; //Событие по срабатыванию таймера

    constructor Create(FocusedBtn: TButton); overload;
    constructor Create(Sec: Word; Button: TButton; BtnCaption: String;
      ABPerSec: Word=0; ABMinValue: Byte=0; Form: TForm=nil;
      TimerEvent: TTimerEvent=teClick); overload;
    procedure OnTimer(Sender: TObject);
    procedure OnTimerAB(Sender: TObject);
    procedure OnTimerFB(Sender: TObject);

    procedure OnCloseQuery(Sender: TObject; var CanClose: Boolean);
  end;

  TRazSomeClass = class
  private
    procedure OnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  end;

  TRazMsgNotify = procedure(MsgText: String);
  TRazMsgNotifyEx = procedure(MsgText: String) of object;

const
    //Переопределяю типы, чтобы не подключать Dialogs в модуле,
    //в котором будет вызываться MsgDlg.
    //В старых IDE некоторых значений может не оказаться. Просто закомментируйте их.
  mtWarning       = mtWarning;
  mtError         = mtError;
  mtInformation   = mtInformation;
  mtConfirmation  = mtConfirmation;
  mtCustom        = mtCustom;
  mbOK            = mbOK;
  mbCancel        = mbCancel;
  mbYes           = mbYes;
  mbNo            = mbNo;
  mbAbort         = mbAbort;
  mbRetry         = mbRetry;
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;
  mrHelp     = idHelp;
  mrTryAgain = idTryAgain;
  mrContinue = idContinue;
  mrAll      = mrNo + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;
  mrClose    = mrYesToAll + 1;
    //Стандартные заголовки
  mzError   = 'Ошибка!';
  mzInform  = 'Информация';
  mzConfirm = 'Вопрос';
  mzWarning = 'Внимание!';
  mzCustom  = 'Сообщение';
    //Стандартные названия кнопок
  mbnYes      = '&Да';
  mbnNo       = '&Нет';
  mbnOK       = '&OК';
  mbnCancel   = '&Отмена';
  mbnAbort    = '&Прервать';
  mbnRetry    = 'П&овторить';
  mbnIgnore   = 'П&ропустить';
  mbnAll      = '&Все';
  mbnNoToAll  = 'Н&ет для всех';
  mbnYesToAll = 'Д&а для всех';
  mbnHelp     = '&Справка';
  mbnClose    = '&Закрыть';
  mbnHelpNone = 'Нет &доступной справки';
  mbnHelpHelp = 'Справ&ка';
  mbnTryAgain = 'Ещё раз';
  mbnContinue = 'Продолжить';
    //Стандартные сочетания кнопок
  mbsOk           = [mbOK];
  mbsOkCancel     = [mbOK, mbCancel];
  mbsYesNo        = [mbYes, mbNo];
  mbsYesNoCancel  = [mbYes, mbNo, mbCancel];
  mbsYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbsAbortIgnore  = [mbAbort, mbIgnore];
  mbsAbortRetry   = [mbAbort, mbRetry];
  mbsAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
    //Стандартные сочетания заголовков кнопок
  mnsOk:          Array [1..1] of String = (mbnOk);
  mnsOkCancel:    Array [1..2] of String = (mbnOk, mbnCancel);
  mnsYesNo:       Array [1..2] of String = (mbnYes, mbnNo);
  mnsYesNoCancel: Array [1..3] of String = (mbnYes, mbnNo, mbnCancel);
  mnsYesAllNoAllCancel: Array [1..5] of String = (mbnYes, mbnYesToAll, mbnNo, mbnNoToAll, mbnCancel);
  mnsAbortIgnore: Array [1..2] of String = (mbnAbort, mbnIgnore);
  mnsAbortRetry:  Array [1..2] of String = (mbnAbort, mbnRetry);
  mnsAbortRetryIgnore: Array [1..3] of String = (mbnAbort, mbnRetry, mbnIgnore);

    //Размер списка последних сообщений
  RazMaxMsgsListSize = MAXBYTE; //Здесь можете задать моксимальный размер списка

var
  List_LastMessages: TStringList; //Список последних сообщений
  OnBeforeShowMsgDlg: TRazMsgNotify;  //Событие перед показом сообщения
  OnBeforeShowMsgDlgEx: TRazMsgNotifyEx;  //Событие перед показом сообщения (для классов)

  (*
    Caption       - заголовок диалогового окна.
    Text          - текст сообщения.
    Params        - массив параметров для замены (%0, %1 и т.д.). Количество не
                    проверяется, неверное количество не приводит к ошибкам.
    Type          - тип сообщения (картинка слева).
    Buttons       - набор кнопок.
    BtnCaptions   - массив заголовков кнопок.
    FocusedBtnNum - номер кнопки в фокусе
    Form          - форма-родитель окна (в её центре появится это окно. Иначе
                    в центре экрана).
    Timer         - время таймера обратного отсчёта на кнопке в секундах
                    (минимум - 3, меньше - не использовать).
                    Таймер не точный, а примерный.
    TimerBtnNum   - номер кнопки, на которой будет идти отсчёт. Кнопки считаются
                    слева направо, начиная с 0.
    AlphaBlendSec - секунда, с которой начинает уменьшаться величина
                    прозрачности у окна сообщения.
    AlphaBlendMinValue -
                    минимальное значение прозрачности на последней секунде.
    TimerEvent    - событие по срабатыванию таймера. При активации кнопок
                    прозрачность не работает. Должен быть установлен таймер.
    XOffset       - смещение по горизонтали относительно центра выбранной формы
                    или экрана, если форма не выбрана.
    YOffset       - смещение по вертикали относительно центра выбранной формы
                    или экрана, если форма не выбрана.
  *)
function MsgDlg(Caption, Text: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; BtnCaptions: Array of String; FocusedBtnNum: Byte=0;
  Form: TForm=nil; Timer: Word=0; TimerBtnNum: Byte=0; AlphaBlendPerSec: Word=0;
  AlphaBlendMinValue: Byte=0; TimerEvent: TTimerEvent=teClick;
  XOffset: Integer=0; YOffset: Integer=0): Byte;

function MsgDlgPar(Caption, Text: String; Params: Array of Variant;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; BtnCaptions: Array of String;
  FocusedBtnNum: Byte=0; Form: TForm=nil; Timer: Word=0; TimerBtnNum: Byte=0;
  AlphaBlendPerSec: Word=0; AlphaBlendMinValue: Byte=0;
  TimerEvent: TTimerEvent=teClick; XOffset: Integer=0; YOffset: Integer=0): Byte;

  (*
    ACaption      - заголовок окна.
    APromt        - подпись над полем ввода.
    ADefault      - начальное значение.
    ATextOnCancel - возвращаемый текст при отмене.
    Font          - шрифт сообщения.
    PasswordChar  - символ замены для воода пароля.
  *)
function InputDlg(const ACaption, APrompt, ADefault: string;
  ATextOnCancel: String=''; Font: TFont=nil; PasswordChar: Char=#0): string;

implementation

//_______________________________Окно_диалога___________________________________
//==============================================================================

function MsgDlg(Caption, Text: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; BtnCaptions: Array of String; FocusedBtnNum: Byte=0;
  Form: TForm=nil; Timer: Word=0; TimerBtnNum: Byte=0; AlphaBlendPerSec: Word=0;
  AlphaBlendMinValue: Byte=0; TimerEvent: TTimerEvent=teClick;
  XOffset: Integer=0; YOffset: Integer=0): Byte;
begin
  (* Функция вывода диалоговых сообщений *)

  Result:= MsgDlgPar(Caption, Text, [], DlgType, Buttons, BtnCaptions,
    FocusedBtnNum, Form, Timer, TimerBtnNum, AlphaBlendPerSec,
    AlphaBlendMinValue, TimerEvent, XOffset, YOffset);
end;

function MsgDlgPar(Caption, Text: String; Params: Array of Variant;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; BtnCaptions: Array of String;
  FocusedBtnNum: Byte=0; Form: TForm=nil; Timer: Word=0; TimerBtnNum: Byte=0;
  AlphaBlendPerSec: Word=0; AlphaBlendMinValue: Byte=0;
  TimerEvent: TTimerEvent=teClick; XOffset: Integer=0; YOffset: Integer=0): Byte;
type
  TBtn = record
    Button: TButton;
    Width: Integer;
  end;
const
  MinTaimer = 3;          //Мнимальное значение таймера (меньше делать нет смысла)
  cDefBtnWidth = 63;      //Ширина кнопки по умолчанию
  cDefBtnSpace = 5;       //Расстояние между кнопками
  cDefMinLeftMargin = 27; //Минимальный отступ слева крайней левой кнопки по умолчанию

var
  i, k: Byte;
  r: Word;
  l: Integer;
  MD: TForm;
  B: TButton;
  Btns: Array of TBtn;
  tX, tF: TRazTimer;
  Btn, FocBtn: TButton;
  BtnCap: String;
  SL: TStringList;
  CaptSize: Integer;
  MaxStringCount: Integer;
  Monitor: TMonitor;

  function _GetMaxStringLength(Text: String): Integer;
  var
    ii, val: Byte;
  begin
    (* Получение максимальной длины строки *)

    Result:=0;
    If Text='' then Exit;
    SL.Text:=Text;
    If MaxStringCount<SL.Count then MaxStringCount:=SL.Count;
    For ii:=0 to SL.Count-1 do
    Begin
      val:=MD.Canvas.TextWidth(SL.Strings[ii]);
      if val>Result then Result:=val;
    End;
  end;

  function _AlingTextByCenter(Text: String): String;
  var
    ii: Integer;
    MaxLen, SpaceWd, SpaceCnt: Integer;
  begin
    (* Выравнивание текста по центру *)

    Result:=Text;
    MaxLen:=_GetMaxStringLength(Text);
    If SL.Count<2 then Exit;

    SpaceWd:=MD.Canvas.TextWidth(' ');
    For ii:=0 to SL.Count-1 do
    Begin
      SpaceCnt:=Round((MaxLen - MD.Canvas.TextWidth(SL.Strings[ii])) / SpaceWd / 2);
      while SpaceCnt>0 do
      begin
        SL.Strings[ii]:=' '+SL.Strings[ii];
        Dec(SpaceCnt);
      end;
    End;
    Result:=SL.Text;
  end;

begin
  (* Функция вывода диалоговых сообщений с параметрами в сообщении.
    Параметры нумеруются с 0 и начинаются символом '%', например '%0'.
    Если реальное значение совпадает с параметром, то нужно просто повторить
  значение в параметре.
    Одинаковые параметры заменятся на одинаковое значение (т.к. rfReplaceAll).
  *)

    //События на показ сообщения
    //(например, для логирования показа, дабы пользователи не смогли отмазаться, что сообщения не было)
  If Assigned(OnBeforeShowMsgDlg) then OnBeforeShowMsgDlg(Text);
  If Assigned(OnBeforeShowMsgDlgEx) then OnBeforeShowMsgDlgEx(Text);

  If Length(Params)>0 then
    for i:=0 to Length(Params)-1 do
      Text:=StringReplace(Text, '%'+IntToStr(i), VarToStr(Params[i]), [rfReplaceAll]);

    //Создание диалогового окна
  MD:=CreateMessageDialog(Text, DlgType, Buttons);
  MD.Caption:=Caption;
  MD.FormStyle:=fsStayOnTop;
  If AlphaBlendPerSec>0 then MD.AlphaBlend:=True;

  SL:=TStringList.Create;
  MaxStringCount:=0;

  l:=Length(BtnCaptions)-1; //Индекс последнего элемента
  k:=0;
  Btn:=nil;
  BtnCap:='';
  For i:=0 to MD.ControlCount-1 do
  Begin
    if MD.Controls[i] is TButton then
    begin
      B:=(MD.Controls[i] as TButton);
      //B.Cursor:=crHandPoint;  //Меняет курсор на палец
      if k>l then
      begin
             if SameText(B.Caption, SMsgDlgOK)        then B.Caption:=mbnOK
        else if SameText(B.Caption, SMsgDlgCancel)    then B.Caption:=mbnCancel
        else if SameText(B.Caption, SMsgDlgYes)       then B.Caption:=mbnYes
        else if SameText(B.Caption, SMsgDlgNo)        then B.Caption:=mbnNo
        else if SameText(B.Caption, SMsgDlgAbort)     then B.Caption:=mbnAbort
        else if SameText(B.Caption, SMsgDlgRetry)     then B.Caption:=mbnRetry
        else if SameText(B.Caption, SMsgDlgIgnore)    then B.Caption:=mbnIgnore
        else if SameText(B.Caption, SMsgDlgYesToAll)  then B.Caption:=mbnYesToAll
        else if SameText(B.Caption, SMsgDlgNoToAll)   then B.Caption:=mbnNoToAll
        else if SameText(B.Caption, SMsgDlgAll)       then B.Caption:=mbnAll
        else if SameText(B.Caption, SMsgDlgHelp)      then B.Caption:=mbnHelp
        else if SameText(B.Caption, SMsgDlgClose)     then B.Caption:=mbnClose
        else if SameText(B.Caption, SMsgDlgHelpNone)  then B.Caption:=mbnHelpNone
        else if SameText(B.Caption, SMsgDlgHelpHelp)  then B.Caption:=mbnHelpHelp;
      end
      else
        B.Caption:=_AlingTextByCenter(BtnCaptions[k]);

        //Получаю необходимую кнопку для таймера
      if (Timer>=MinTaimer) and (k=TimerBtnNum) then
      begin
        Btn:=B;
        BtnCap:=B.Caption;
        //B.Caption:=B.Caption+' ('+IntToStr(Taimer)+')';
      end;
        //Получаю необходимую кнопку для фокуса
      if k=FocusedBtnNum then
        FocBtn:=B;

        //Меняю размеры кнопок
      SetLength(Btns, Length(Btns)+1);
      Btns[Length(Btns)-1].Button:=B;

      CaptSize:=_GetMaxStringLength(B.Caption+' ('+IntToStr(Timer)+')');
      if CaptSize>50 then
        Btns[Length(Btns)-1].Width:=CaptSize+13
      else Btns[Length(Btns)-1].Width:=B.Width;

        //Активность кнопок
      if (TimerEvent=teEnable) and (Timer>=MinTaimer) then
        B.Enabled:=False;

      Inc(k);
    end;
  End;//For

    //Новая ширина кнопкам
  r:=0;
  For i:=0 to Length(Btns)-1 do
    if Btns[i].Width>r then r:=Btns[i].Width;
  CaptSize:=0;
  For i:=0 to MD.ControlCount-1 do
    if MD.Controls[i] is TButton then
    begin
      MD.Controls[i].Width:=r;
      TButton(MD.Controls[i]).WordWrap:=MaxStringCount>1;

      CaptSize:=-TButton(MD.Controls[i]).Font.Height;
      if MaxStringCount>1 then
        //Было бы неплохо как-то рассчитать межстрочный интервал и учесть его
        MD.Controls[i].Height:=
          MaxStringCount * (CaptSize-1) + (MD.Controls[i].Height - CaptSize);
    end;
  If MaxStringCount>1 then
    MD.Height:=MD.Height + (MaxStringCount-1) * (CaptSize-1);

  FreeAndNil(SL);

    //Новое расположение кнопок, если ширина кнопки больше ширины по умолчанию
  If r>cDefBtnWidth then
  Begin
    k:=Length(Btns);
    r:=k * r + (k - 1) * cDefBtnSpace;  //Расстояние от левого края первой кнопки до правого последней
    if (MD.Width < r + cDefMinLeftMargin * 2) then
    begin
      MD.Width:=r + cDefMinLeftMargin * 2;
      l:=cDefMinLeftMargin;   //Начало отсчёта положения кнопок
    end
    else
      l:=(MD.Width-r) DIV 2;  //Начало отсчёта положения кнопок

    for i:=0 to Length(Btns)-1 do
    begin
      B:=Btns[i].Button;
      B.Left:=l;
      if i<Length(Btns)-1 then
        l:=l+B.Width + cDefBtnSpace;
    end;
  End;

    //Вывод сообщения по центру указанной формы или по центру экрана (если форма не указана)
  If Form<>nil then
  Begin
    MD.Top:=Form.Height DIV 2 - MD.Height DIV 2 + Form.Top + YOffset;
    MD.Left:=Form.Width DIV 2 - MD.Width DIV 2 + Form.Left + XOffset;
  End
  Else if Form=nil then
  Begin

    //ToDo -cMsgDlgs: Выбор монитора, на котором будет отображаться сообщение
    {   Не всё так просто, как кажется. Надо ещё поразбираться

    MonitorNumber - номер монитора, на котором будет отображаться сообщение.
                    При -1 и не выбранной форме показывает на том мониторе, на
                    котором находится главная форма приложения.
                    При -1 и выбранной форме показывает на том мониторе, на
                    котором находится выбранная форма.

    if MonitorNumber in [0..Screen.MonitorCount-1] then
      Monitor:=Screen.Monitors[MonitorNumber]
    else  //если по умолчанию (-1) или уходит за пределы
    begin
      if Form=nil then
        Monitor:=Screen.MonitorFromWindow(Application.MainFormHandle)
      else Monitor:=Screen.MonitorFromWindow(Form.Handle);
    end;}

    MD.Top:=Screen.Height DIV 2 - MD.Height DIV 2 + YOffset;
    MD.Left:=Screen.Width DIV 2 - MD.Width DIV 2 + XOffset;
  End;

    //Создание таймера обратного отсчёта
  tX:=nil;
  If (Timer>=MinTaimer) and (Btn<>nil) and (BtnCap<>'') then
  Begin
    tX:=TRazTimer.Create(Timer, Btn, BtnCap, AlphaBlendPerSec,
      AlphaBlendMinValue, MD, TimerEvent);
    Btn.Caption:=BtnCap+' ('+IntToStr(Timer)+')';
  End;
    //Создания таймера переноса фокуса
  tF:=TRazTimer.Create(FocBtn);
    //Событие попытки закрытия окна
  If TimerEvent=teEnable then
  Begin
    tF.FWindow:=MD;
    MD.OnCloseQuery:=tF.OnCloseQuery;
  End;

    //Запись текста сообщения
  {$IF Defined(ADD_TO_MSGSLIST)}
    List_LastMessages.Add(Text);
    If List_LastMessages.Count>RazMaxMsgsListSize then
      List_LastMessages.Delete(0);
  {$IFEND}

  If tX<>nil then tX.FTimer.Enabled:=True;      //Запуск таймера
  MD.ShowModal;                                 //Показ окна
  If tX<>nil then tX.Destroy;                   //Уничтожение таймера
  tF.Destroy;
  Result:=MD.ModalResult;                       //Результат работы
end;

//------------------------------------------------------------------------------

constructor TRazTimer.Create(FocusedBtn: TButton);
begin
  (* Создание экземпляра класса для переноса фокуса на выбранную кнопку *)
  (* Таймер создаётся в связи с тем, что пока я не покажу окно,
    перенести фокус не смогу, но и после показа тоже прочими средствами того же
    сделать не смогу. *)

  FButton:=FocusedBtn;
  FTimer:=TTimer.Create(nil);
  FTimer.Enabled:=False;
  FTimer.Interval:=10;
  FTimer.OnTimer:=OnTimerFB;
  FTimer.Enabled:=True;
end;

constructor TRazTimer.Create(Sec: Word; Button: TButton; BtnCaption: String;
  ABPerSec: Word=0; ABMinValue: Byte=0; Form: TForm=nil;
  TimerEvent: TTimerEvent=teClick);
const
  TABInterval = 60;
begin
  (* Создание экземпляра класса таймера для отсчёта времени и прозрачности *)
  (*
    Sec         - количество секунд для отсчёта (минимум 3).
    Button      - кнопка, на которой будет идти отсчёт (она и нажмётся).
    BtnCaption  - заголовок кнопки.
    ABSec       - время, с которого начинается уменьшаться прозрачность.
    ABMinValue  - минимальное значение прозрачности на последней секунде.
    Form        - форма, на которой будет изменяться прозрачность.
  *)

  FSec:=Sec;
  FButton:=Button;
  FBtnCaption:=BtnCaption;
  FWindow:=Form;
  FTimerEvent:=TimerEvent;
  FTimer:=TTimer.Create(nil);
  FTimer.Enabled:=False;
  FTimer.Interval:=1000;
  FTimer.OnTimer:=OnTimer;

    //Таймер для уменьшения прозрачности
  If (ABPerSec>0) and (Form<>nil) then
  Begin
    if ABPerSec>Sec then ABPerSec:=Sec;
    FABSec:=ABPerSec;
    FABDecValue:=Round((255-ABMinValue)/(ABPerSec*1000/TABInterval));
    FTimerAB:=TTimer.Create(nil);
    FTimerAB.Enabled:=False;
    FTimerAB.Interval:=TABInterval;
    FTimerAB.OnTimer:=OnTimerAB;
  End;
end;

destructor TRazTimer.Destroy;
begin
  FButton:=nil;
  FTimer.Free;
  FTimerAB.Free;
end;

procedure TRazTimer.OnTimer(Sender: TObject);
var
  i: Byte;
begin
  (* Событие на срабатывание таймера отсчёта *)

  Dec(FSec);
  If FSec=0 then
  Begin
    FTimer.Enabled:=False;
    if FTimerAB<>nil then
      FTimerAB.Enabled:=False;
    if FTimerEvent=teClick then
      FButton.Click
    else
    begin
        //Активация кнопок
      for i:=0 to FWindow.ControlCount-1 do
        if FWindow.Controls[i] is TButton then
          TButton(FWindow.Controls[i]).Enabled:=True;
      FButton.Caption:=FBtnCaption;
        //Фокус на необходимую кнопку
      if FButton.CanFocus then
        FButton.SetFocus;
      Exit;
    end;
  End;
  If (FSec=FABSec) and (FTimerAB<>nil) and (FTimerEvent=teClick) then
    FTimerAB.Enabled:=True;
  FButton.Caption:=FBtnCaption+' ('+IntToStr(FSec)+')';
end;

procedure TRazTimer.OnTimerAB(Sender: TObject);
var
  ab: Smallint;
begin
  (* Событие на срабатывание таймера прозрачности *)

  If FTimerEvent<>teClick then
  Begin
    FTimerAB.Enabled:=False;
    Exit;
  End;
  ab:=FWindow.AlphaBlendValue-FABDecValue;
  If ab>0 then
    FWindow.AlphaBlendValue:=ab
  Else FWindow.AlphaBlendValue:=0;
end;

procedure TRazTimer.OnTimerFB(Sender: TObject);
begin
  (* Событие на срабатывание таймера фокуса кнопки *)

  If FButton.CanFocus then
  Begin
    FTimer.Enabled:=False;
    FButton.SetFocus;
  End;
end;

procedure TRazTimer.OnCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Byte;
begin
  (* Событие попытки закрытия диалогового окна *)
  (* Т.к. таймер фокуса кнопки создаётся всегда, поэтому это событие находится
    внутри этого класса *)

    //Закрыть можно только когда активны кнопки
   For i:=0 to FWindow.ControlCount-1 do
    if FWindow.Controls[i] is TButton then
    begin
      CanClose:=TButton(FWindow.Controls[i]).Enabled;
      Break;
    end;
end;

(*  Пример:
  MsgDlgPar('Проверка',
    'Всё работает просто %0!'+#13#10+'Мне безумно нравится! :)', ['отлично'],
    mtInformation, mbsOkCancel, [mbnOK, 'Просто'+#13#10+'&Офигительно!'],
    1, Self, 10, 1, 5, 70, teClick, -100, -100);
*)

//______________________Диалоговое_окно_ввода_текста____________________________
//==============================================================================

procedure TRazSomeClass.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function LowCase(const S: String): String;  //Взято из Globals
  var
    Ch: Char;
    L: Integer;
    Source, Dest: PChar;
  begin
    (* Перевод букв в нижний регистр *)
    (* Аналог LowerCase, но включает русские буквы *)

    L:=Length(S);
    SetLength(Result, L);
    Source:=Pointer(S);
    Dest:=Pointer(Result);
    While L<>0 do
    Begin
      Ch:=Source^;

      (* а - 224, ё - 184, ж - 230, я - 255.
        a - 97, z - 122   *)
      if ( (Ch >= 'A') and (Ch <= 'Z') ) OR
      ( (Ch >= 'А') and (Ch <= 'Я') ) then Inc(Ch, 32)
      else if Ch = 'Ё' then Ch:='ё';

      Dest^:=Ch;
      Inc(Source);
      Inc(Dest);
      Dec(L);
    End;
  end;

  function UprCase(const S: String): String;  //Взято из Globals
  var
    I, Len: Integer;
    DstP, SrcP: PChar;
    Ch: Char;
  begin
    (* Перевод букв в верхний регистр *)
    (* Аналог UpperCase, но включает русские буквы *)

    Len:=Length(S);
    SetLength(Result, Len);
    If Len>0 then
    Begin
      DstP:=PChar(Pointer(Result));
      SrcP:=PChar(Pointer(S));
      for I:=Len downto 1 do
      begin
        Ch:=SrcP^;
        case Ch of
          'a'..'z':
            Ch:=Char(Word(Ch) xor $0020);
          'а'..'я':
            Dec(Ch, 32);
          'ё':
            Ch:='Ё';
        end;
        DstP^:=Ch;
        Inc(DstP);
        Inc(SrcP);
      end;
    End;
  end;

begin
  (* Функция для обработки нажатия в поле ввода *)
  (* Ctrl+Вверх или Page Up - перевод текста в верхний регистр,
     Ctrl+Вниз или Page Down - перевод текста в нижний регистр *)

  If not (ssShift in Shift) and not (ssAlt in Shift) and
    not (ssCtrl in Shift) then
  Begin
    if Key=VK_PRIOR then
      TEdit(Sender).Text:=UprCase(TEdit(Sender).Text)
    else if Key=VK_NEXT then
      TEdit(Sender).Text:=LowCase(TEdit(Sender).Text);
  End
  Else if not (ssShift in Shift) and not (ssAlt in Shift) and
    (ssCtrl in Shift) then
  Begin
    if Key=VK_UP then
      TEdit(Sender).Text:=UprCase(TEdit(Sender).Text)
    else if Key=VK_DOWN then
      TEdit(Sender).Text:=LowCase(TEdit(Sender).Text);
  End;
end;

//------------------------------------------------------------------------------

function _InputBox(const ACaption, APrompt, ATextOnCancel: string;
  var Value: string; NewFont: TFont=nil; PassChar: Char=#0): Boolean;

  function GetAveCharSize(Canvas: TCanvas): TPoint;
  var
    I: Integer;
    Buffer: array[0..51] of Char;
  begin
    for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    Result.X := Result.X div 52;
  end;

const
  cMaxLength = 600;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  BOk, BCancel: TButton;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  len: Integer;
  SomeClass: TRazSomeClass;
begin
  Result:=False;
  Form:=TForm.Create(Application);
  SomeClass:=TRazSomeClass.Create;
  with Form do
    try
      if NewFont<>nil then
        Font:=NewFont
      else
      begin
        Font.Name:='Verdana';
        Font.Size:=9;
      end;
      Canvas.Font:=Font;
      DialogUnits:=GetAveCharSize(Canvas);
      BorderStyle:=bsSizeable;
      BorderIcons:=[biSystemMenu];
      Caption:=ACaption;
      ClientWidth:=MulDiv(240, DialogUnits.X, 4);
      Position:=poScreenCenter;
      Prompt:=TLabel.Create(Form);
      with Prompt do
      begin
        Parent:=Form;
        Left:=MulDiv(8, DialogUnits.X, 5);
        Top:=MulDiv(8, DialogUnits.Y, 8);
        //WordWrap := True;
        Anchors:=[akLeft, akTop, akRight];
        ParentFont:=True;
          {AutoSize:=True;}
        Caption:=APrompt;
          {len:=Width;
          AutoSize:=False;}
        Width:=MulDiv(232, DialogUnits.X, 4);
      end;
      Edit:=TEdit.Create(Form);
      with Edit do
      begin
        Parent:=Form;
        Left:=Prompt.Left;
        Top:=Prompt.Top + Prompt.Height + 5;
        Width:=MulDiv(232, DialogUnits.X, 4);
        MaxLength:=255;
        Text:=Value;
        Anchors:=[akLeft, akTop, akRight];
        SelectAll;
        ParentFont:=True;
        PasswordChar:=PassChar;
        OnKeyDown:=SomeClass.OnKeyDown;
      end;
      ButtonTop:=Edit.Top + Edit.Height + 10;
      ButtonWidth:=MulDiv(56, DialogUnits.X, 4);
      ButtonHeight:=MulDiv(14, DialogUnits.Y, 8);
      BOk:=TButton.Create(Form);
      with BOk do
      begin
        Parent:=Form;
        Caption:=mbnOK;
        ModalResult:=mrOk;
        Default:=True;
        SetBounds(MulDiv(30, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
        Anchors:=[akLeft, akTop];
        ParentFont:=True;
      end;
      BCancel:=TButton.Create(Form);
      with BCancel do
      begin
        Parent:=Form;
        Caption:=mbnCancel;
        ModalResult:=mrCancel;
        Cancel:=True;
        SetBounds(MulDiv(160, DialogUnits.X, 4), ButtonTop,
          ButtonWidth, ButtonHeight);
        Anchors:=[akTop, akRight];
        Form.ClientHeight:=Top+Height+10;
        Form.Width:=MulDiv(30, DialogUnits.X, 4)+10+Width+100;
        ParentFont:=True;
      end;
      if Canvas.TextWidth(APrompt)+Prompt.Left*2+15>Form.Width then
        Form.Width:=Canvas.TextWidth(APrompt)+Prompt.Left*2+15;
      if BOk.Left+BOk.Width>=BCancel.Left then
        Form.Width:=Form.Width+BOk.Left+BOk.Width-BCancel.Left+BOk.Width DIV 2;
      if Length(Value)>0 then
      begin
        len:=Canvas.TextWidth(Value)+Edit.Left+ClientWidth-Edit.Width;
        if (len>Width) and (len<cMaxLength) then
          Width:=len
        else if len>cMaxLength then
          Width:=cMaxLength;
      end;
      Constraints.MinHeight:=Height;
      Constraints.MaxHeight:=Height;
      Constraints.MinWidth:=Width;
      Constraints.MaxWidth:=Screen.Width;
      if ShowModal=mrOk then
      begin
        Value:=Edit.Text;
        Result:=True;
      end
      else
        Value:=ATextOnCancel;
    finally
      Form.Free;
    end;
end;

//------------------------------------------------------------------------------

function InputDlg(const ACaption, APrompt, ADefault: string;
  ATextOnCancel: String=''; Font: TFont=nil; PasswordChar: Char=#0): string;
begin
  (* Диалоговое окно ввода текста *)
  (*
    ACaption      - заголовок;
    APrompt       - сообщение;
    ADefault      - значение по умолчанию;
    ATextOnCancel - значение при отмене ввода.
    Font          - используемый шрифт
    PasswordChar  - символ скрытия пароля
  *)
  (* При отмене возвращает символ #1 - это чтобы было проще проверять *)

  Result:=ADefault;
  _InputBox(ACaption, APrompt, ATextOnCancel, Result, Font, PasswordChar);
end;

initialization
  List_LastMessages:=TStringList.Create;
finalization
  FreeAndNil(List_LastMessages);
end.
