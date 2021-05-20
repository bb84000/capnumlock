//******************************************************************************
// Capnumlock : place caps lock and nums lock in tray
// bb - sdtp - May 2021
//******************************************************************************
unit capsnumlock1;

{$mode objfpc}{$H+}

interface

uses
    {$IFDEF WINDOWS}
  Win32Proc,
  {$ENDIF}Classes, Graphics, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, UniqueInstance, Windows, LazUTF8,
  lazbbcontrols, lazbbinifiles, lazbbutils, lazbbautostart;

type

  { TfCapnumlock }

  TfCapnumlock = class(TForm)
    BtnOK: TButton;
    BtnQuit: TButton;
    CBStartWin: TCheckBox;
    CBStartMini: TCheckBox;
    ColorPicker1: TColorPicker;
    ENoNums: TEdit;
    ECaps: TEdit;
    EBdc: TEdit;
    ENums: TEdit;
    ImageList1: TImageList;
    LNoNum: TLabel;
    LCaps: TLabel;
    LBdc: TLabel;
    LNums: TLabel;
    LColorTray: TLabel;
    Panel1: TPanel;
    PTrayMnuRestore: TMenuItem;
    PTrayMnuQuit: TMenuItem;
    PTrayMnu: TPopupMenu;
    TrayIcon1: TTrayIcon;
    TrayIcon2: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure BtnOKClick(Sender: TObject);
    procedure CBStartWinClick(Sender: TObject);
    procedure ColorPicker1change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PTrayMnuQuitClick(Sender: TObject);
    procedure PTrayMnuRestoreClick(Sender: TObject);
  private
    OS, OSTarget: String;
    LangStr: String;
    UserAppsDataPath, CnlAppDataPath: String;
    ProgName: String;
    PrevLeft, PrevTop: Integer;
    Iconized: Boolean;
    pict: TPicture;
    coltray: TColor;
    cfgfile: TBbInifile;
    aLetters: array [0..3] of TEdit;
    procedure OnAppMinimize(Sender: TObject);
    function HideOnTaskbar: boolean;
    procedure DrawTheIcon(Bmp: Graphics.Tbitmap; letter: String; LetterColor: TColor);
    procedure updatetray;
    { private declarations }
  public
    { public declarations }
  end;

var
  fCapnumlock: TfCapnumlock;

const
  aCapslock: array of String = ('Majuscules désactivées','Majuscules activées');
  aNumslock: array of String = ('Pavé numérique désactivé','Pavé numérique activé');

implementation

{$R *.lfm}

function ToUnicodeEx(wVirtKey, wScanCode: UINT; lpKeyState: PByte;
   pwszBuff: PWideChar; cchBuff: Integer; wFlags: UINT; dwhkl: HKL): Integer; stdcall; external 'user32.dll';

const
  WH_KEYBOARD_LL = 13;

var
  llKeyboardHook: HHOOK = 0;

{ TfCapnumlock }

function LowLevelKeyboardHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := CallNextHookEx(llKeyboardHook, nCode, wParam, lParam);
  fCapnumlock.ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_CAPITAL))), fCapnumlock.TrayIcon1.Icon);
  fCapnumlock.TrayIcon1.Hint:= aCapslock[Integer(Odd(GetKeyState(VK_CAPITAL)))];
  fCapnumlock.ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_NUMLOCK)))+2, fCapnumlock.TrayIcon2.Icon);
  fCapnumlock.TrayIcon2.Hint:= aNumslock[Integer(Odd(GetKeyState(VK_NUMLOCK)))];;
end;

procedure TfCapnumlock.FormCreate(Sender: TObject);
var
  s: string;
 {$IFDEF Linux}
    x: Integer;
 {$ENDIF}
begin
  inherited;
  Application.OnMinimize:= @OnAppMinimize;
  pict:= TPicture.Create;
  pict.Bitmap.width:= 48;
  pict.Bitmap.height:= 48;

  aLetters[0]:= EBdc;
  aLetters[1]:= ECaps;
  aLetters[2]:= ENoNums;
  aLetters[3]:= ENums;
  {$IFDEF CPU32}
     OSTarget := '32 bits';
  {$ENDIF}
  {$IFDEF CPU64}
     OSTarget := '64 bits';
  {$ENDIF}
  {$IFDEF Linux}
    OS := 'Linux';
    LangStr := GetEnvironmentVariable('LANG');
    x := pos('.', LangStr);
    LangStr := Copy(LangStr, 0, 2);
    //wxbitsrun := 0;
    //OSTarget:= '';
    UserAppsDataPath := GetUserDir;
    // Get mail client
  {$ENDIF}
  {$IFDEF WINDOWS}
    OS := 'Windows ';
    // get user data folder
    s := ExtractFilePath(ExcludeTrailingPathDelimiter(GetAppConfigDir(False)));
    if Ord(WindowsVersion) < 7 then
      UserAppsDataPath := s                     // NT to XP
    else
    UserAppsDataPath := ExtractFilePath(ExcludeTrailingPathDelimiter(s)) + 'Roaming'; // Vista to W10
    LazGetShortLanguageID(LangStr);
  {$ENDIF}
  ProgName:= 'capnumlock';
  CnlAppDataPath := UserAppsDataPath + PathDelim + ProgName + PathDelim;
  cfgfile:= TBbInifile.Create(CnlAppDataPath+'capnumlock.config');
end;

procedure TfCapnumlock.OnAppMinimize(Sender: TObject);
begin
  PrevLeft:=self.left;
  PrevTop:= self.top;
  WindowState:= wsMinimized;
  Iconized:= HideOnTaskbar;
end;

function TfCapnumlock.HideOnTaskbar: boolean;
begin
  result:= false;
  if (WindowState=wsMinimized) then
  begin
    result:= true;
    visible:= false;
  end;
end;

procedure TfCapnumlock.FormDestroy(Sender: TObject);
begin
   if (llKeyboardHook <> 0) and UnhookWindowsHookEx(llKeyboardHook) then
  begin
    llKeyboardHook := 0;
   end;
   if Assigned(pict) then FreeAndNil(pict);
end;

procedure TfCapnumlock.FormActivate(Sender: TObject);
begin

  Coltray:= StringToColor(cfgfile.ReadString('settings', 'trayiconcolor', 'clWhite'));
  ColorPicker1.Color:= coltray;
  EBdc.Text:= cfgfile.ReadString('settings', 'bdc', 'a');
  ECaps.text:= cfgfile.ReadString('settings', 'caps', 'A');
  ENums.Text:= cfgfile.ReadString('settings', 'nums', '1');
  ENoNums.Text:= cfgfile.ReadString('settings', 'nonums', '─');
  CBStartWin.Checked:= StringToBool(cfgfile.ReadString('settings', 'startwin', 'False'));
  CBStartMini.Checked:= StringToBool(cfgfile.ReadString('settings', 'startmini', 'False'));
  CBStartWinClick(self);
  if CBStartMini.checked then Application.Minimize;
  updatetray;
  ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_CAPITAL))), TrayIcon1.Icon);
  TrayIcon1.Hint:= aCapslock[Integer(Odd(GetKeyState(VK_CAPITAL)))];
  ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_NUMLOCK)))+2, TrayIcon2.Icon);
  TrayIcon2.Hint:= aNumslock[Integer(Odd(GetKeyState(VK_NUMLOCK)))];;
  if llKeyboardHook = 0 then
      llKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardHook, HInstance, 0);
end;

procedure TfCapnumlock.ColorPicker1change(Sender: TObject);
begin
  Coltray:= ColorPicker1.Color;
  updatetray;
  ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_CAPITAL))), TrayIcon1.Icon);
  ImageList1.GetIcon(Integer(Odd(GetKeyState(VK_NUMLOCK)))+2, TrayIcon2.Icon);
end;

procedure TfCapnumlock.BtnOKClick(Sender: TObject);
begin
    Application.Minimize;
end;

procedure TfCapnumlock.CBStartWinClick(Sender: TObject);
begin
  if CBStartWin.Checked then SetAutostart('capnumlock', Application.ExeName)
  else UnsetAutostart('capnumlock');
end;

procedure TfCapnumlock.updatetray;
var
  i: Integer;
begin
  ImageList1.Clear;
  for i:= 0 to 3 do
  begin
    DrawTheIcon(pict.Bitmap, aLetters[i].Text, coltray);
    ImageList1.AddMasked(pict.Bitmap, $000001);
  end;
end;

procedure TfCapnumlock.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  cfgfile.WriteString('settings', 'trayiconcolor', ColorToString(coltray));
  cfgfile.WriteString('settings', 'bdc', EBdc.Text);
  cfgfile.WriteString('settings', 'caps', ECaps.text);
  cfgfile.WriteString('settings', 'nums', ENums.Text);
  cfgfile.WriteString('settings', 'nonums', ENoNums.Text);
  cfgfile.WriteString('settings', 'startwin', BoolToString(CBStartWin.Checked));
  cfgfile.WriteString('settings', 'startmini', BoolToString(CBStartMini.Checked));
  cfgfile.UpdateFile;
  cfgfile.free;
end;


procedure TfCapnumlock.PTrayMnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfCapnumlock.PTrayMnuRestoreClick(Sender: TObject);
begin
  iconized:= false;
  visible:= true;
  WindowState:=wsNormal;
 //Need to reload position as it can change during hide in taskbar process
  left:= PrevLeft;
  top:= PrevTop;
  Application.BringToFront;
end;

procedure TfCapnumlock.DrawTheIcon(Bmp: Graphics.Tbitmap; Letter: String; LetterColor: TColor);
var
  tmpBmp: Graphics.TBitmap;
  style: TTextStyle;
begin
  // we draw on the tmpBmp and finally assign it to input bmp
  tmpBmp:= Graphics.Tbitmap.Create;
  tmpBmp.PixelFormat := pf24bit;
  tmpBmp.SetSize(Bmp.Width, Bmp.Height);
  tmpBmp.Canvas.Brush.Color := $000001;
  tmpBmp.Canvas.FillRect(0, 0, tmpBmp.Width, tmpBmp.Height);
  With tmpBmp.Canvas do
  begin
    // font
    Font.Name := 'Arial';
    Font.Style := [fsBold];
    Font.Color := LetterColor;
    Font.Size := 48;
    Brush.Style := bsClear;
    style.SystemFont:= false;
    style.Alignment:= taCenter;
    style.Layout:= tlCenter;
    TextRect(Classes.Rect(0,0,48,48),0,0,letter, style);
  end;
  Bmp.Assign(tmpBmp);
  if Assigned(tmpBmp) then tmpBmp.free;
end;



end.

