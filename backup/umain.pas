unit uMain;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Windows, LazUTF8;

type

  { TfMain }

  TfMain = class(TForm)
    bStart: TButton;
    bStop: TButton;
    ImageList1: TImageList;
    mLog: TMemo;
    StatusBar1: TStatusBar;
    TrayIcon1: TTrayIcon;
    TrayIcon2: TTrayIcon;
    procedure bStartClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function Start: boolean;
    function Stop: Boolean;
    function IsStarted: Boolean;
    function TranslateVirtualKey(VirtualKey: integer): WideString;
    { private declarations }
  public
    { public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

function ToUnicodeEx(wVirtKey, wScanCode: UINT; lpKeyState: PByte;
   pwszBuff: PWideChar; cchBuff: Integer; wFlags: UINT; dwhkl: HKL): Integer; stdcall; external 'user32.dll';

const
  LLKHF_ALTDOWN = KF_ALTDOWN shr 8;
  WH_KEYBOARD_LL = 13;

type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: DWORD;
  end;


var
  llKeyboardHook: HHOOK = 0;
  AltDown, ShiftDown, CtrlDown: Boolean;
  KeyBoardState: TKeyboardState;
  KeyBoardLayOut: HKL;
{ TfMain }

function LowLevelKeyboardHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
var
  pkbhs: PKBDLLHOOKSTRUCT;
  AChr: array[0..1] of WideChar;
  VirtualKey: integer;
  ScanCode: integer;
  ConvRes: integer;
  ActiveWindow: HWND;
  ActiveThreadID: DWord;
  Str: widestring;
begin
  pkbhs := PKBDLLHOOKSTRUCT(Pointer(lParam));
  {if nCode = HC_ACTION then
  begin
    VirtualKey := pkbhs^.vkCode;

    Str := '';
    //Alt key, log once on keydown
    if LongBool(pkbhs^.flags and LLKHF_ALTDOWN) and (not AltDown) then
    begin
      Str := '[Alt]';
      AltDown := True;
      fmain.statusbar1.Panels[0].Text:='[Alt]';
    end;
    if (not LongBool(pkbhs^.flags and LLKHF_ALTDOWN)) and (AltDown) then  begin
      AltDown := False;
      fmain.statusbar1.Panels[0].Text:='';
    end;

    //Ctrl key, log once on keydown
    if (WordBool(GetAsyncKeyState(VK_CONTROL) and $8000)) and (not CtrlDown) then
    begin
      Str := '[Ctrl]';
      CtrlDown := True;
      fmain.statusbar1.Panels[1].Text:='[Ctrl]';
    end;
    if (not WordBool(GetAsyncKeyState(VK_CONTROL) and $8000)) and (CtrlDown) then begin
      CtrlDown := False;
      fmain.statusbar1.Panels[1].Text:='';
    end;

    //Shift key, log once on keydown
    if ((VirtualKey = VK_LSHIFT) or (VirtualKey = VK_RSHIFT)) and (not ShiftDown) then
    begin
      Str := '[Shift]';
      ShiftDown := True;
      fmain.statusbar1.Panels[2].Text:='[Shift]';
    end;
    if (wParam = WM_KEYUP) and ((VirtualKey = VK_LSHIFT) or (VirtualKey = VK_RSHIFT)) then begin
      ShiftDown := False;
      fmain.statusbar1.Panels[2].Text:='';
    end;

    //Other Virtual Keys, log once on keydown
    if (wParam = WM_KEYDOWN) and
          ((VirtualKey <> VK_LMENU) and (VirtualKey <> VK_RMENU)) and  //not Alt
           (VirtualKey <> VK_LSHIFT) and (VirtualKey <> VK_RSHIFT) and // not Shift
            (VirtualKey <> VK_LCONTROL) and (VirtualKey <> VK_RCONTROL) then //not Ctrl
    begin
      Str := fMain.TranslateVirtualKey(VirtualKey);
      if Str = '' then
      begin
        ActiveWindow := GetForegroundWindow;
        ActiveThreadID := GetWindowThreadProcessId(ActiveWindow, nil);
        GetKeyboardState(KeyBoardState);
        KeyBoardLayOut := GetKeyboardLayout(ActiveThreadID);
        ScanCode := MapVirtualKeyEx(VirtualKey, 0, KeyBoardLayOut);
        if ScanCode <> 0 then
        begin
          ConvRes := ToUnicodeEx(VirtualKey, ScanCode, @KeyBoardState, @AChr, SizeOf(Achr), 0, KeyBoardLayOut);
          if ConvRes > 0 then
            Str := AChr;
        end;
      end;
    end;
    //do whatever you have to do with Str, add to memo, write to file, etc...
    if Str <> '' then
      fMain.mLog.Text :=  fMain.mLog.Text + UTF16ToUTF8(Str);
  end;  }
  Result := CallNextHookEx(llKeyboardHook, nCode, wParam, lParam);
  if Odd(GetKeyState(VK_CAPITAL)) then
  begin
    fMain.ImageList1.GetIcon(2, fMain.TrayIcon1.Icon);
  end else
  begin
    fMain.ImageList1.GetIcon(3, fMain.TrayIcon1.Icon);
  end;
  if Odd(GetKeyState(VK_NUMLOCK)) then
  begin
    fMain.ImageList1.GetIcon(6, fMain.TrayIcon2.Icon);
  end else
  begin
    fMain.ImageList1.GetIcon(7, fMain.TrayIcon2.Icon);
  end;
end;

function TfMain.Start: boolean;
begin
  if llKeyboardHook = 0 then
    llKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardHook, HInstance, 0);
  Result := (llKeyboardHook <> 0)
end;

function TfMain.Stop: Boolean;
begin
  Result := False;
  if (llKeyboardHook <> 0) and UnhookWindowsHookEx(llKeyboardHook) then
  begin
    llKeyboardHook := 0;
    Result := True;
  end;
end;

function TfMain.IsStarted: Boolean;
begin
  Result := (llKeyboardHook <> 0)
end;

function TfMain.TranslateVirtualKey(VirtualKey: integer): WideString;
begin
  Result := '';
  {$Region 'Translate VirtualKey'}
  case VirtualKey of
    VK_RETURN:   Result := sLineBreak;
    VK_TAB:      Result := '     ';
    VK_BACK:     Result := '[BackSpace]';
    VK_SHIFT:    Result := '[Shift]';
    VK_CONTROL:  Result := '[Ctrl]';
    VK_MENU:     Result := '[Alt]';
    VK_ESCAPE:   Result := '[Esc]';
    VK_PAUSE: 	 Result := '[Pause]';
    VK_CAPITAL:  Result := '[Caps Lock]';
    VK_PRIOR: 	 Result := '[Page Up]';
    VK_NEXT: 	 Result := '[Page Down]';
    VK_END: 	 Result := '[End]';
    VK_HOME: 	 Result := '[Home]';
    VK_LEFT:     Result := '[Left Arrow]';
    VK_UP: 	 Result := '[Up Arrow]';
    VK_RIGHT: 	 Result := '[Right Arrow]';
    VK_DOWN: 	 Result := '[Down Arrow]';
    VK_SELECT: 	 Result := '[Select]';
    VK_PRINT: 	 Result := '[Print Screen]';
    VK_EXECUTE:  Result := '[Execute]';
    VK_SNAPSHOT: Result := '[Print]';
    VK_INSERT: 	 Result := '[Ins]';
    VK_DELETE:   Result := '[Del]';
    VK_HELP: 	 Result := '[Help]';
    VK_F1:   	 Result := '[F1]';
    VK_F2: 	 Result := '[F2]';
    VK_F3: 	 Result := '[F3]';
    VK_F4: 	 Result := '[F4]';
    VK_F5: 	 Result := '[F5]';
    VK_F6: 	 Result := '[F6]';
    VK_F7: 	 Result := '[F7]';
    VK_F8: 	 Result := '[F8]';
    VK_F9: 	 Result := '[F9]';
    VK_F10: 	 Result := '[F10]';
    VK_F11: 	 Result := '[F11]';
    VK_F12: 	 Result := '[F12]';
    VK_NUMPAD0:  Result := '0';
    VK_NUMPAD1:  Result := '1';
    VK_NUMPAD2:  Result := '2';
    VK_NUMPAD3:  Result := '3';
    VK_NUMPAD4:  Result := '4';
    VK_NUMPAD5:  Result := '5';
    VK_NUMPAD6:  Result := '6';
    VK_NUMPAD7:  Result := '7';
    VK_NUMPAD8:  Result := '8';
    VK_NUMPAD9:  Result := '9';
    VK_SEPARATOR:Result := '+';
    VK_SUBTRACT: Result := '-';
    VK_DECIMAL:  Result := '.';
    VK_DIVIDE:   Result := '/';
    VK_NUMLOCK:  Result := '[Num Lock]';
    VK_SCROLL: 	 Result := '[Scroll Lock]';
    VK_PLAY:     Result := '[Play]';
    VK_ZOOM:     Result := '[Zoom]';
    VK_LWIN,
    VK_RWIN:     Result := '[Win Key]';
    VK_APPS:     Result := '[Menu]'
   end;
   {$EndRegion}
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
   if (llKeyboardHook <> 0) and UnhookWindowsHookEx(llKeyboardHook) then
  begin
    llKeyboardHook := 0;
   end;
end;

procedure TfMain.bStartClick(Sender: TObject);
begin
  if not Start then
    MessageDlg('Cannot start the hook!', mtError, [mbOk], 0)
  else
    MessageDlg('Hook successfully started!', mtInformation, [mbOk], 0);
end;

procedure TfMain.bStopClick(Sender: TObject);
begin
  if not Stop then
    MessageDlg('Cannot stop the hook!', mtError, [mbOk], 0)
  else
    MessageDlg('Hook successfully stopped!', mtInformation, [mbOk], 0);
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
 // ShowMessage(InttoStr());
  if Odd(GetKeyState(VK_CAPITAL)) then
  begin
    fMain.ImageList1.GetIcon(2, fMain.TrayIcon1.Icon);
  end else
  begin
    fMain.ImageList1.GetIcon(3, fMain.TrayIcon1.Icon);
  end;
  if Odd(GetKeyState(VK_NUMLOCK)) then
  begin
    fMain.ImageList1.GetIcon(6, fMain.TrayIcon2.Icon);
  end else
  begin
    fMain.ImageList1.GetIcon(7, fMain.TrayIcon2.Icon);
  end;
    if llKeyboardHook = 0 then
    llKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardHook, HInstance, 0);
end;


end.

