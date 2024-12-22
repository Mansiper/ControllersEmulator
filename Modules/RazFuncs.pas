(*
	Набор разных полезных функций, накопившихся за годы разработки ещё с Delphi 7
	(в частности LowCase и UprCase в новых версия уже не нужны).
	На WordNumberCase стоит обратить особое внимание тем, кто не любит учитывать
	окончания существительных после числительных (и выходят позорные "3 штук" или "штук: 3").
*)

unit RazFuncs;

interface

uses SysUtils, Windows, Forms, ShlObj, Math;

type
    //Тип получаемой из файла информации
  TFileInfoData=(fidAll, fidCompany, fidVersion, fidCopyright, fidProduct, fidCertificate,
    fidFileDescription, fidDbVersion);
  TAddNumberFlags = set of (anfBegin, anfEnd);
  TValTypes=(vatString, vatInteger, vatFloat, vatDate, vatTime, vatDateTime, vatBoolean, vatRange,
    vatFile, vatColor);
    //Данные по словам
  TWordsInfo = record
    WordStr: String;
    IsDigit: Boolean;
  end;

  //Перевод строки в нижний регистр
function LowCase(const S: String): String;
  //Перевод строки в верхний регистр
function UprCase(const S: String): String;
  //Правильное выставление падежа
function WordNumberCase(Number: Integer; IfOne, IfTwo, IfFive: String;
  Flags: TAddNumberFlags): String;
  //Получение значения переменной окружения
function GetEnvironmentValue(Str: String; EmptyIfNotExists: Boolean=False): String;
  //Получение пути к стандартным каталогам Windows
function GetSpecialPath(CSIDL: Word): String;
  //Замена кусков текста в строке
function StringsReplace(const S: String;
  const OldPattern, NewPattern: Array of String; Flags: TReplaceFlags): String; overload;
function StringsReplace(const S: String;
  const OldPattern: Array of String; NewStr: String; Flags: TReplaceFlags): String; overload;
  //Получение регионального разделителя, установленного в системе
function GetLocaleDecimal: Char;
  //Проверяет наличие уже запущенного экземпляра указанного приложения
function AlreadyWorks(Str: PWideChar): Boolean;
  //Преобразовывает коротки путь в длинный
function ShortToLongFileName(FileName: String): String;
  //Пауза на указанное количество миллисекунд
procedure Delay(ms: Cardinal);
  //Звуковой сигнал
procedure DoBeep;
  //Вызов диалога выбора каталога (найдена в инете)
function BrowseForFolder(const Caption: String; Folder: String; FormHandle: THandle): String;
  //Очистка имени и пути файла от запрещённых символов
function PathClear(Path: String): String;
{  //Экспорт ресурса в файл
function ResourceExport(ResName: String; FileName: String; var Path: String): Boolean; overload;
  //Экспорт ресурса в файл с заменой текста
function ResourceExport(ResName: String; FileName: String; var Path: String;
  const OldPattern, NewPattern: Array of String; Flags: TReplaceFlags): Boolean; overload;}
  //Получение информации о файле
function GetFileInfo(FileName: String; FileInfoData: TFileInfoData=fidAll): String;
  //Перевод десятичного числа в 8-ричное
//function IntToOct(Val: Integer): String;
function DecimalToXStr(aBase: Byte; Precision: Byte; aVal: Extended): String;
  //Разделение предложения на слова
function SplitWords(Sentence: String): TArray<TWordsInfo>;
  //Возведение числа в степень
function Pow(Base: Extended; Power: Integer): Extended;
  //Повторитель текста
function RepeatStr(Source: String; Count: Cardinal): String;
  //32/64-битная Windwos
function Is64bit: Boolean;

implementation

//==================================================================================================

function LowCase(const S: String): String;
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
    else if Ch = 'Ё' then Ch:='ё';//Chr(184);

    Dest^:=Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  End;
end;

function UprCase(const S: String): String;
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
          Ch:='Ё';//Chr(168); //Ё
      end;
      DstP^:=Ch;
      Inc(DstP);
      Inc(SrcP);
    end;
  End;
end;

function WordNumberCase(Number: Integer; IfOne, IfTwo, IfFive: String;
  Flags: TAddNumberFlags): String;
  function _SetIfFive: String;
  begin
    If IfFive='' then
      Result:=IfTwo
    Else Result:=IfFive;
  end;
begin
  (* Правильное выставление падежа *)
  //Если IfFive пусто, то берётся значение из IfTwo

  Case Number mod 10 of
       1: Result:=IfOne;
    2..4: Result:=IfTwo;
  Else
    Result:=_SetIfFive;
  End;
  If Number MOD 100 in [11..14] then
    Result:=_SetIfFive;
  If anfBegin in Flags then
    Result:=IntToStr(Number)+Result;
  If anfEnd in Flags then
    Result:=Result+IntToStr(Number);
end;

function GetEnvironmentValue(Str: String; EmptyIfNotExists: Boolean=False): String;
var
  Data: Array[0..$FF] of WideChar;
begin
  (* Получение значения переменной окружения *)

  Result:='';
  If Str='' then Exit;
  If Str[1]<>'%' then Str:='%'+Str;
  If Str[Length(Str)]<>'%' then Str:=Str+'%';

  ExpandEnvironmentStrings(PWideChar(Str), Data, $FF);
  If Data=nil then
  Begin
    {WriteLog('Не удалось получить данные переменной окружения "'+
      Str[1]+Str[Length(Str)]+IntToStr(Length(Str)-2)+'".', rtError);}
  End
  Else
  Begin
    if EmptyIfNotExists and (String(Data)=Str) then
      Exit
    else Result:=Data;
  End;
  If Result[Length(Result)]<>'\' then Result:=Result+'\';
end;

function GetSpecialPath(CSIDL: Word): String;
var
  s: String;
begin
  (* Получение пути к стандартным каталогам Windows *)
  //Взято отсюда: http://www.delphilab.ru/content/view/160/85/

  SetLength(s, MAX_PATH);
  If not SHGetSpecialFolderPath(0, PChar(s), CSIDL, True) then
    s:='';
  Result:=PChar(s);
end;

function StringsReplace(const S: String;
  const OldPattern, NewPattern: Array of String; Flags: TReplaceFlags): String;
var
  i : Integer;
begin
  (* Замена кусков текста в строке. Взято из IdGlobal (Indy10) и доработано *)

  Result:=S;
  For i:=Low(OldPattern) to High(OldPattern) do
    Result:=StringReplace(Result, OldPattern[i], NewPattern[i], Flags);
end;

function StringsReplace(const S: String;
  const OldPattern: Array of String; NewStr: String; Flags: TReplaceFlags): String;
var
  i: Integer;
begin
  (* Замена кусков текста в строке *)

  Result:=S;
  For i:=Low(OldPattern) to High(OldPattern) do
    Result:=StringReplace(Result, OldPattern[i], NewStr, Flags);
end;

function GetLocaleDecimal: Char;
begin
  (* Получение регионального разделителя, установленного в системе *)
  Result:=GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL, ',');
end;

function AlreadyWorks(Str: PWideChar): Boolean;
var
  Hdle: THandle;
begin
  (* Проверяет наличие уже запущенного указанного экземпляра приложения *)

    //Открытие Мьютекса (виртуального файла)
  Hdle:=OpenMutex(MUTEX_ALL_ACCESS, False, Str);
    //... в памяти
  Result:=(Hdle<>0);
    //Если ещё не был создан, то создаём
  If Hdle=0 then CreateMutex(nil, False, Str);
end;

function ShortToLongFileName(FileName: String): String;
var
  KernelHandle: THandle;
  FindData: TWin32FindData;
  Search: THandle;
  GetLongPathName: function(lpszShortPath: PChar; lpszLongPath: PChar;
                            cchBuffer: DWORD): DWORD; stdcall;
begin
  (* Возвращает FileName преобразованное в соответствующее длинное имя *)
    //articles.org.ru/cn/showdetail.php?cid=9022

  KernelHandle := GetModuleHandle('KERNEL32');
  If KernelHandle <> 0 then
    @GetLongPathName := GetProcAddress(KernelHandle, 'GetLongPathNameA');
  // Использю GetLongPathName доступную в windows 98 и выше чтобы
  // избежать проблем доступа к путям UNC в системах NT/2K/XP
  If Assigned(GetLongPathName) then
  Begin
    SetLength(Result, MAX_PATH + 1);
    SetLength(Result, GetLongPathNameW(PWideChar(FileName), @Result[1], MAX_PATH));
  End
  Else
  Begin
    Result := '';
    // Поднимаюсь на одну дирректорию выше от пути к файлу и запоминаю
    // в result.  FindFirstFile возвратит длинное имя файла полученное
    // из короткого.
    while (True) do
    begin
      Search := Windows.FindFirstFile(PChar(FileName), FindData);
      if Search = INVALID_HANDLE_VALUE then Break;
      Result := String('') + FindData.cFileName + Result;
      FileName := ExtractFileDir(FileName);
      Windows.FindClose(Search);
      // Нахожу имя диска с двоеточием.
      if Length(FileName) <= 2 then Break;
    end;
    Result := ExtractFileDrive(FileName) + Result;
  End;
end;

procedure Delay(ms: Cardinal);
var
  FinTime: Cardinal;
begin
  (* Пауза на указанное количество миллисекунд *)

  FinTime:=GetTickCount+ms;
  While GetTickCount<FinTime do Application.ProcessMessages;
end;

procedure DoBeep;
begin
  (* Звуковой сигнал *)

  SystemParametersInfo(SPI_SETBEEP, 1, nil, 0); //Выключение звука в TEdit
  SysUtils.Beep;
  Windows.Beep(1000, 200);
end;

function BrowseForFolder(const Caption: String; Folder: String; FormHandle: THandle): String;

  function BFFCallbackProc(hwnd: HWND; uMsg: UINT; lParam: LPARAM;
    lpData: LPARAM): Integer; stdcall;
  begin
    If uMsg=BFFM_INITIALIZED then
      SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
    Result:=0;
  end;

var
  TitleName: String;
  lpItemID: PItemIDList;
  BrowseInfo: TBrowseInfo;
  DisplayName: Array [0..MAX_PATH] of char;
  TempPath: Array [0..MAX_PATH] of char;
  SysDrv: String;
begin
  (* Вызов диалога выбора каталога (найдена в инете) *)

  If Folder='' then
  Begin
    SysDrv:=GetEnvironmentValue('%SystemDrive%');
    Folder:=String(SysDrv)+'\';
  End;

  FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
  BrowseInfo.hwndOwner:=FormHandle;
  BrowseInfo.pszDisplayName:=@DisplayName;
  TitleName:=Caption;
  BrowseInfo.lpszTitle:=PChar(TitleName);
  BrowseInfo.ulFlags:=BIF_NEWDIALOGSTYLE;
  BrowseInfo.lpfn:=@BFFCallbackProc;
  BrowseInfo.lParam:=DWORD(PChar(Folder));
  lpItemID:=SHBrowseForFolder(BrowseInfo);
  If lpItemID<>nil then
  Begin
    SHGetPathFromIDList(lpItemID, TempPath);
    GlobalFreePtr(lpItemID);
    Result:=TempPath;
  End
  Else Result:='';
end;

function PathClear(Path: String): String;
begin
  (* Очистка имени и пути файла от запрещённых символов *)
  Result:=StringsReplace(Path, ['/', '\', ':', '*', '?', '"', '<', '>', '|'], '', [rfReplaceAll]);
end;

{function ResourceExport(ResName: String; FileName: String;
  var Path: String): Boolean;
var
  ResStream: TResourceStream;
  TmpPath: Array [0..$FF] of WideChar;
begin
  (* Экспорт ресурса в файл *)

  Result:=False;
    //Путь ко временной папке
  If LowCase(Path)='' then
  Begin
    ExpandEnvironmentStrings('%TEMP%', TmpPath, $FF);
    Path:=TmpPath;
    if not DirectoryExists(FileName, False) then
    begin
      ExpandEnvironmentStrings('%windir%', TmpPath, $FF);
      Path:=TmpPath+'\Temp';
    end;
    Path:=Path+cfResOutDir;
  End;
  If Path[Length(Path)]<>'\' then Path:=Path+'\'+cfResOutDir;
  If not DirectoryExists(Path) then
    ForceDirectories(Path);

  Try
    ResStream:=TResourceStream.Create(HInstance, ResName, RT_RCDATA);
    ResStream.SaveToFile(Path+FileName);
    ResStream.Free;
    Result:=True;
  Except
    Result:=False;
  End;
end;

function ResourceExport(ResName: String; FileName: String; var Path: String;
  const OldPattern, NewPattern: Array of String; Flags: TReplaceFlags): Boolean;
const
  cDir = '\XXView\';
var
  ResStream: TResourceStream;
  TmpPath: Array [0..$FF] of WideChar;
  SL: TStringList;
begin
  (* Экспорт ресурса в файл с заменой текста *)

  Result:=False;
    //Путь ко временной папке
  If LowCase(Path)='' then
  Begin
    ExpandEnvironmentStrings('%TEMP%', TmpPath, $FF);
    Path:=TmpPath;
    if not DirectoryExists(FileName, False) then
    begin
      ExpandEnvironmentStrings('%windir%', TmpPath, $FF);
      Path:=TmpPath+'\Temp';
    end;
    Path:=Path+cDir;
  End;
  If Path[Length(Path)]<>'\' then Path:=Path+'\'+cDir;
  If not DirectoryExists(Path) then
    ForceDirectories(Path);

  SL:=TStringList.Create;
  Try
    ResStream:=TResourceStream.Create(HInstance, ResName, RT_RCDATA);
    SL.LoadFromStream(ResStream);
    SL.Text:=StringsReplace(SL.Text, OldPattern, NewPattern, Flags);
    SL.SaveToFile(Path+FileName);
    ResStream.Free;
    Result:=True;
  Except
    Result:=False;
  End;
  FreeAndNil(SL);
end;}

function GetFileInfo(FileName: String; FileInfoData: TFileInfoData=fidAll): String;
type
  PDWORD = ^DWORD;
  PLangAndCodePage = ^TLangAndCodePage;
  TLangAndCodePage = packed record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
  PLangAndCodePageArray = ^TLangAndCodePageArray;
  TLangAndCodePageArray = array[0..0] of TLangAndCodePage;
var
  loc_InfoBufSize: DWORD;
  loc_InfoBuf: PChar;
  loc_VerBufSize: DWORD;
  loc_VerBuf: PChar;
  cbTranslate: DWORD;
  lpTranslate: PDWORD;
  i: DWORD;

  function _GetValue(From: String): String;
  begin
    if VerQueryValue(loc_InfoBuf, PChar(Format(
      'StringFileInfo\0%x0%x\'+From, [
      PLangAndCodePageArray(lpTranslate)[i].wLanguage,
        PLangAndCodePageArray(lpTranslate)[i].wCodePage])),
    Pointer(loc_VerBuf), DWORD(loc_VerBufSize)) then
      Result:=loc_VerBuf
    else
      Result:='';
  end;

begin
  (* Получение информации о файле *)
  //http://www.delphisources.ru/pages/faq/base/get_file_version.html

  Result:='';
  If (Length(FileName) = 0) or (not Fileexists(FileName)) then
    Exit;
  loc_InfoBufSize := GetFileVersionInfoSize(PChar(FileName), loc_InfoBufSize);
  If loc_InfoBufSize > 0 then
  Begin
    loc_VerBuf := nil;
    loc_InfoBuf := AllocMem(loc_InfoBufSize);
    try
      if not GetFileVersionInfo(PChar(FileName), 0,
      loc_InfoBufSize, loc_InfoBuf) then
        Exit;
      if not VerQueryValue(loc_InfoBuf, '\\VarFileInfo\\Translation',
      Pointer(lpTranslate), DWORD(cbTranslate)) then
        Exit;
      for i:=0 to (cbTranslate DIV SizeOf(TLangAndCodePage))-1 do
      begin
        case FileInfoData of
          fidAll:
            begin
              Result:={0}_GetValue('CompanyName')+#13#10+
                      {1}_GetValue('FileVersion')+#13#10+
                      {2}_GetValue('LegalCopyright')+#13#10+
                      {3}_GetValue('ProductName')+#13#10+
                      {4}_GetValue('Certificate')+#13#10+
                      {5}_GetValue('FileDescription')+#13#10+
                      {6}_GetValue('DbVersion');
            end;
          fidCompany:
            Result:=_GetValue('CompanyName');
          fidVersion:
            Result:=_GetValue('FileVersion');
          fidCopyright:
            Result:=_GetValue('LegalCopyright');
          fidProduct:
            Result:=_GetValue('ProductName');
          fidCertificate:
            Result:=_GetValue('Certificate');
          fidFileDescription:
            Result:=_GetValue('FileDescription');
          fidDbVersion:
            Result:=_GetValue('DbVersion');
        end;
        Break;
      end;
    finally
      FreeMem(loc_InfoBuf, loc_InfoBufSize);
    end;
  End;
end;

{function IntToOct(Val: Integer): String;
var
  i, x: Integer;
begin
  (* Перевод десятичного числа в 8-ричное *)

  i:=-1;
  While x<Val do
  Begin
    Inc(i);
    x:=Power(8, i);
  End;
end;}

function DecimalToXStr(aBase: Byte; Precision: Byte; aVal: Extended): String;
var
  Val: Extended;
  IntVal: Int64;
  FracVal: Extended;
  StrInt: String;
  StrFrac: String;
  i: Integer;

  function _IntToDigit(aVal: Byte): Char;
  const
    cNums = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  begin
    Result:=cNums[aVal+1];
    {Case aVal of
      0..9 : Result := IntToStr(aVal);
      10   : Result := 'A';
      11   : Result := 'B';
      12   : Result := 'C';
      13   : Result := 'D';
      14   : Result := 'E';
      15   : Result := 'F';
    End;}
  end;

begin
  (* Перевод числа в n-ричную систему счисления *)
  //Взято отсюда: http://www.cyberforum.ru/delphi-beginners/thread51338.html

    //Получаем целую и дробную части числа.
  IntVal := Trunc(aVal);
  FracVal := Frac(aVal);

    //Переводим целую часть.
  StrInt := '';
  Repeat
    StrInt := _IntToDigit(IntVal mod aBase) + StrInt;
    IntVal := IntVal div aBase;
  Until IntVal = 0;

    //Если дробная часть = 0, то перевод закончен.
  If FracVal = 0 then begin
    Result := StrInt;
    exit;
  End;

    //Переводим дробную часть. Точность - до Precision цифр после запятой.
  StrFrac := '';
  For i := 1 to Precision do begin
    Val := FracVal * aBase;
    StrFrac := StrFrac + _IntToDigit(Trunc(Val));
    FracVal := Frac(Val);
      //Если дробная часть = 0, то перевод закончен.
    if FracVal = 0 then Break;
  End;

  Result := StrInt + ',' + StrFrac;
end;

function SplitWords(Sentence: String): TArray<TWordsInfo>;
var
  i: Integer;
  c: Char;
  Str: String;
  IsDigit: Boolean;
  Chars: set of Char;
begin
  (* Разделение предложения на слова *)

  Str:='';
  Chars:=[' ', ',', '.', '\', '/', '|', '"', ':', ';', '?', '!', '(', ')', '<', '>',
    '{', '}', '=', '+', '*', #13, #10, #9];
  Sentence:=Sentence+' ';
  IsDigit:=False;
  For i:=1 to Length(Sentence) do
  Begin
    c:=Sentence[i];
    if c in Chars then
    begin
      if Str='' then Continue;
      if IsDigit and CharInSet(c, [',', '.']) then
      begin
        Str:=Str+c;
        Continue;
      end;

      while CharInSet(Str[1], Chars) do Delete(Str, 1, 1);
      while CharInSet(Str[Length(Str)], Chars) do Delete(Str, Length(Str), 1);
      if not IsDigit then
      begin
        while (Length(Str)>0) and (Str[1]='-') do Delete(Str, 1, 1);
        while (Length(Str)>0) and (Str[Length(Str)]='-') do Delete(Str, Length(Str), 1);
      end;
      if Str='' then Continue;

      if Str='-' then
      begin
        Str:='';
        Continue;
      end
      else
      begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1].WordStr:=Str;
        Result[Length(Result)-1].IsDigit:=IsDigit;
        Str:='';
      end;
    end
    else
    begin
      if Str='' then IsDigit:=True;
      IsDigit:=IsDigit and ( CharInSet(c, ['0'..'9']) or ((Str='') and (c='-')) );
      Str:=Str+c;
    end;
  End;
end;

function Pow(Base: Extended; Power: Integer): Extended;
begin
  (* Возведение числа в степень *)
  Result:=Exp(Power * Ln(Base));
end;

function RepeatStr(Source: String; Count: Cardinal): String;
var
  i: Cardinal;
begin
  (* Повторитель текста *)

  Result:='';
  If Count>0 then
    for i:=1 to Count do
      Result:=Result+Source;
end;

function Is64bit: Boolean;
begin
  (* 32/64-битная Windwos *)
  Result:=GetEnvironmentValue('%ProgramW6432%', True)<>'';
end;

end.
