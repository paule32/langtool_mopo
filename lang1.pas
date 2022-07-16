// ----------------------------------------------------------------------------
// File:        lang1.pas
// Author:      Jens Kallup - paule32 <kallup-dev@web.de>
// Copyright:   (c) 2022 kallup non-profit
//
// License:     non-profit * only for education, and trial !!!
//              All modifications needs to query to the original author's (me).
//              You can use it at Your school as teacher for education.
//              But commercial usage is not allowed.
// ----------------------------------------------------------------------------
unit lang1;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, Grids, IniFiles, DBCtrls, EditBtn, ComCtrls, Buttons, DBGrids,
  ExtCtrls, process, DB, dbf;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckGroup1: TCheckGroup;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    Dbf1DEU: TWideStringField;
    Dbf1ENG: TWideStringField;
    Dbf1MSGID: TWideStringField;
    Dbf1POL: TWideStringField;
    Dbf1RECID: TAutoIncField;
    Dbf1RUS: TWideStringField;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Process1: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure DBGrid1ColEnter(Sender: TObject);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DBGrid1KeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    poFolder: String;
    jsrFile : String;
    iniFile : TIniFile;

    f_path  : String;
    f_name  : String;
    f_inif  : String;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

// ----------------------------------------------------------------------------
// @brief  This event is fired, when the user push the menu item "Exit" on the
//         menubar by using the mouse, or by using the keyboard.
//         It simply does close the application window.
//
// @param  Sender - TObject
//         # the Sender is the MenuItem object that was pushed by the
//         # user; either with the mouse, or by using the keyboard.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

// ----------------------------------------------------------------------------
// @brief  This event is fired, when the form is create. We try to initialize
//         a database table, when it is not exists, and set the object data
//         fields with default values unless the user has change it on second
//         call/open this application.  All data informations will be try to
//         save into the application directory.
//
// @param  Sender - TObject
//         # the Sender is the Form class that was open, when it is start
//         # with mouse click on the desktop icon, or by starting with
//         # explicit start parameters on the terminal.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  f_mdbf: TDbf;
begin
  f_path := ExtractFilePath(Application.ExeName);
  f_name := ExtractFileName(Application.ExeName);
  f_inif := ChangeFileExt(f_name,'.ini');
  f_name := f_path + f_inif;
  try
    iniFile := TIniFile.Create(f_name);
    edit1.Text := iniFile.ReadString('common','path' , f_path);

    edit2.Text := iniFile.ReadString('common','poout', 'application.po');
    edit3.Text := iniFile.ReadString('common','moout', 'application.mo');

    CheckGroup1.Checked[0] := iniFile.ReadBool('common','outENG', false);
    CheckGroup1.Checked[1] := iniFile.ReadBool('common','outDEU', false);
    CheckGroup1.Checked[2] := iniFile.ReadBool('common','outRUS', false);
    CheckGroup1.Checked[3] := iniFile.ReadBool('common','outPOL', false);

    dbf1.Close;
    dbf1.FilePath     := edit1.Text;
    dbf1.FilePathFull := edit1.Text;
    dbf1.TableName := iniFile.ReadString('common', 'table', 'podata.dbf');

    if not FileExists(dbf1.FilePathFull + dbf1.TableName) then
    begin
      f_mdbf := TDbf.Create(nil);
      try
        f_mdbf.FilePath := f_path;
        f_mdbf.TableLevel := 7;
        f_mdbf.Exclusive := true;
        f_mdbf.TableName := dbf1.TableName;
        with f_mdbf.FieldDefs do begin
          Add('recid', ftAutoInc, 0, true);
          Add('msgid', ftString, 250, true);
          Add('eng', ftString, 250, false);
          Add('deu', ftString, 250, false);
          Add('rus', ftString, 250, false);
          Add('pol', ftString, 250, false);
        end;
        f_mdbf.CreateTable;
        f_mdbf.Open;
        f_mdbf.AddIndex('dataid', 'recid', [ixPrimary, ixUnique]);
        f_mdbf.Close;
        ShowMessage('data file created.');
      finally
        f_mdbf.Free;
      end;
    end;
    dbf1.Open;
    iniFile.Free;
  except
    on E: Exception do begin
      ShowMessage('Error: ' + E.Message);
      Close;
      exit;
    end;
  end;
end;

// ----------------------------------------------------------------------------
// @brief  This Event is fired, when the form is closed.
//         We try here, to save the user data. So, the user can work later
//         on the same project like he has exit it.
//
// @param  Sender - TObject
//         # the Sender is the Form class that was clicked on the X icon
//         # in the right-upper area of the window, or by pressing the
//         # alt and F4 key, when using the keyboard.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  iniFile := TIniFile.Create(f_name);

  iniFile.WriteString('common','table',dbf1.TableName);

  iniFile.WriteString('common','poout',Trim(edit2.Text));
  iniFile.WriteString('common','moout',Trim(edit3.Text));

  iniFile.WriteBool('common','outENG', CheckGroup1.Checked[0]);
  iniFile.WriteBool('common','outDEU', CheckGroup1.Checked[1]);
  iniFile.WriteBool('common','outRUS', CheckGroup1.Checked[2]);
  iniFile.WriteBool('common','outPOL', CheckGroup1.Checked[3]);

  iniFile.Free;
end;

// ----------------------------------------------------------------------------
// @brief  create the .mo, and .po file on the given data in the database
//         file (podata.dbf).
//
// @param  Sender - TObject
//         # the Sender is the Button class that was clicked
//         # with the mouse, or by pressing the return key when
//         # using the keyboard.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
begin
  with process1 do
  begin
    Executable := 'msgfmt.exe';
    with Parameters do
    begin
      Add('-o');
      Add('lib\i386-win32\application-de.mo');
      Add('lib\i386-win32\application-de.po');
    end;
    Options := process1.Options + [poWaitOnExit];
    Execute;
  end;
end;

procedure TForm1.DBGrid1ColEnter(Sender: TObject);
begin
  dbf1.Edit;
end;

procedure TForm1.DBGrid1ColExit(Sender: TObject);
begin
  dbf1.Edit;
  dbf1.Post;
end;

procedure TForm1.DBGrid1Enter(Sender: TObject);
begin
  dbf1.Edit;
end;

procedure TForm1.DBGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_RETURN) then
  begin
    dbf1.Edit;
    dbf1.Post;
    beep;
  end;
end;

procedure TForm1.DBGrid1KeyPress(Sender: TObject; var Key: char);
begin

end;

// ----------------------------------------------------------------------------
// @brief  File->Open JSON - opens the .jsr file that was produce by fpc.
//         This file can be found in the binary directory: .\lib\i386-win32
//
// @param  Sender - TObject
//         # the Sender is the MenuItem class that was clicked
//         # with the mouse, or by pressing the return key when
//         # using the keyboard.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  openDialog1.InitialDir := f_path;
  if not openDialog1.Execute then
  begin
    ShowMessage('Error occur');
    Button1.Enabled := false;
    dbf1.Close;
    exit;
  end;
  jsrFile := openDialog1.FileName;
  if ExtractFileExt(jsrFile) <> 'jsr' then
  begin
    ShowMessage('.jsr file seems not okay.');
    Button1.Enabled := false;
    dbf1.Close;
    exit;
  end;
  dbf1.Open;
  Button1.Enabled := true;
end;

// ----------------------------------------------------------------------------
// @brief  Select the input, and output directory for the data file's.
//         The following objects are set by defailt:
//
//         edit1: becomes the path.
//         edit2: becomes the .po output file name.
//         edit3: becomes the .mo output file name.
//
// @param  Sender - TObject
//         # the Sender is the Button class that was clicked
//         # with the mouse, or by pressing the return key when
//         # using the keyboard.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if Self.SelectDirectoryDialog1.Execute = false then
  begin
    ShowMessage('Error occur');
    exit;
  end;
  poFolder   := self.SelectDirectoryDialog1.FileName;
  Edit1.Text := poFolder;
  DBF1.FilePathFull:= poFolder;
end;

end.

