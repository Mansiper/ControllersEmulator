program Emulator;

uses
  Forms,
  Main in 'Main.pas' {fMain},
  RazFuncs in '..\..\Готовые модули и формы\RazFuncs.pas',
  RazMsgDlgs in '..\..\Готовые модули и формы\RazMsgDlgs.pas',
  Devices in 'Devices.pas',
  Loader in 'Loader.pas' {fLoader};

{$R *.res}

begin
  If AlreadyWorks('ControllersEmulator') then
  Begin
    MsgDlg(mzInform, 'Приложение уже запущено.', mtInformation, mbsOk, mnsOk);
    Exit;
  End;
  Randomize;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Эмулятор контроллеров';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
