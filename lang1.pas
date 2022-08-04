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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, Grids, IniFiles, DBCtrls, EditBtn, ComCtrls, Buttons, DBGrids,
  ExtCtrls, process, LazFileUtils, DB, dbf;

type
  { TForm1 }
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckGroup1: TCheckGroup;
    Dbf1: TDbf;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    poDataFile: String;
    poFolder  : String;
    jsrFile   : String;
    iniFile   : TIniFile;

    f_path  : String;
    f_name  : String;
    f_inif  : String;

    dataModified : Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// ----------------------------------------------------------------------------
// platform specification stuff ...
// ----------------------------------------------------------------------------
const
sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
             {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};

resourcestring
  RCS_CreateDataFile = 'data file created.';
  RCS_Test           = 'this is a test';

{ TForm1 }

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
  t_row : Integer;
begin
  ShowMessage(RCS_Test);
  try
    f_path := ExtractFilePath(Application.ExeName);
    f_name := ExtractFileName(Application.ExeName);
    f_inif := ChangeFileExt(f_name,'.ini');
    f_name := f_path + f_inif;
    try
      iniFile := TIniFile.Create(f_name);
      edit1.Text := iniFile.ReadString('common','path' , f_path);

      edit2.Text := iniFile.ReadString('common','poout', 'application.po');
      edit3.Text := iniFile.ReadString('common','moout', 'application.mo');
      edit5.Text := iniFile.ReadString('common','dbout', 'podata.dbf');

      CheckGroup1.Checked[0] := iniFile.ReadBool('locale','outENG', false);
      CheckGroup1.Checked[1] := iniFile.ReadBool('locale','outDEU', false);
      CheckGroup1.Checked[2] := iniFile.ReadBool('locale','outRUS', false);
      CheckGroup1.Checked[3] := iniFile.ReadBool('locale','outPOL', false);

      edit6 .Text := iniFile.ReadString('project','idversion'  , '');
      edit7 .Text := iniFile.ReadString('project','msgbugsto'  , '');
      edit8 .Text := iniFile.ReadString('project','potdate'    , '');
      edit9 .Text := iniFile.ReadString('project','porevision' , '');
      edit10.Text := iniFile.ReadString('project','translator' , '');
      edit11.Text := iniFile.ReadString('project','langteam'   , '');
      edit12.Text := iniFile.ReadString('project','contenttype', '');

      edit13.Text := iniFile.ReadString('utils','msgfmt','msgfmt.exe');

      dbf1.Close;
      dbf1.FilePath     := edit1.Text;
      dbf1.FilePathFull := edit1.Text;
      dbf1.TableName    := edit5.Text;

      if not FileExists(dbf1.TableName) then
      begin
        f_mdbf := TDbf.Create(nil);
        try
          f_mdbf.FilePath := f_path;
          f_mdbf.TableLevel := 7;
          f_mdbf.Exclusive := true;
          f_mdbf.TableName := dbf1.TableName;
          with f_mdbf.FieldDefs do
          begin
            Add('recid', ftAutoInc,   0, true );
            Add('msgid', ftString , 125, true );
            Add('eng'  , ftString , 125, false);
            Add('deu'  , ftString , 125, false);
            Add('rus'  , ftString , 125, false);
            Add('pol'  , ftString , 125, false);
          end;
          f_mdbf.CreateTable;
          f_mdbf.Open;
          f_mdbf.AddIndex('dataid', 'recid', [ixPrimary, ixUnique]);
          f_mdbf.Close;
          ShowMessage(RCS_CreateDataFile);
        finally
          f_mdbf.Free;
        end;
      end;
      iniFile.Free;

      // -------------------------------
      // look, if data exists, then add
      // it to the StringGrid ...
      // -------------------------------
      dbf1.Open;
      dbf1.First;

      while not dbf1.EOF do
      begin
        with Form1.StringGrid1 do
        begin
          with dbf1 do
          begin
            t_row := stringGrid1.RowCount;
            stringGrid1.RowCount := t_row + 1;
            Cells[0, t_row] := FieldByName('msgid').AsString;
            Cells[1, t_row] := FieldByName('eng').AsString;
            Cells[2, t_row] := FieldByName('deu').AsString;
            Cells[3, t_row] := FieldByName('rus').AsString;
            Cells[4, t_row] := FieldByName('pol').AsString;
          end;
        end;
        dbf1.Next;
      end;

      // -----------------------------------
      // set flag, when data is modified by
      // the user. If so, then save it after
      // the end of the application ...
      // -----------------------------------
      dataModified := false;

    except
      // ------------------------------------
      // here, we try to "catch" application
      // exception, and do things, to hold
      // the system healthy ...
      // ------------------------------------
      on E: Exception do begin
        ShowMessage('Error: ' + E.Message);
        Close;
        exit;
      end;
    end;
  finally
    // -----------------------------------
    // at end, close the database file.
    // we write data back, if it modified
    // in the FormDestroy Event ...
    // -----------------------------------
    dbf1.Close;
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
  iniFile.WriteString('common','dbout',Trim(edit5.Text));

  iniFile.WriteBool('locale','outENG', CheckGroup1.Checked[0]);
  iniFile.WriteBool('locale','outDEU', CheckGroup1.Checked[1]);
  iniFile.WriteBool('locale','outRUS', CheckGroup1.Checked[2]);
  iniFile.WriteBool('locale','outPOL', CheckGroup1.Checked[3]);

  iniFile.WriteString('project','idversion'  , edit6 .Text);
  iniFile.WriteString('project','msgbugsto'  , edit7 .Text);
  iniFile.WriteString('project','potdate'    , edit8 .Text);
  iniFile.WriteString('project','porevision' , edit9 .Text);
  iniFile.WriteString('project','translator' , edit10.Text);
  iniFile.WriteString('project','langteam'   , edit11.Text);
  iniFile.WriteString('project','contenttype', edit12.Text);

  iniFile.WriteString('utils','msgfmt', edit13.Text);
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
var
  poLocales: Array[0..3] of TFileStream;
  poArray  : Array[0..3] of String = ('en', 'de', 'ru', 'pl');
  oxt      : String;
  idx      : Integer;
  ox1, ox2 : String;
  moproc   : TProcess;
begin
  for idx := 0 to Length(poLocales)-1 do
  begin
    if CheckGroup1.Checked[idx] then
    begin
      ox1 := ExtractFileName(ChangeFileExt(edit2.Text,'')) + '-' + poArray[idx] + '.mo';
      ox2 := ExtractFileName(ChangeFileExt(edit3.Text,'')) + '-' + poArray[idx] + '.po';

      if FileExists(ox1) then if not DeleteFile(ox1) then begin
        ShowMessage(Format('File: "%s" could not delete.',[ox1]));
        exit;
      end;
      if FileExists(ox2) then if not DeleteFile(ox2) then begin
        ShowMessage(Format('File: "%s" could not delete.',[ox2]));
        exit;
      end;

      // write .po file
      poLocales[idx] := TFileStream.Create(ox2,
      fmCreate or fmOpenWrite);

      oxt := ''
      + 'msgid ""'                                             + sLineBreak
      + 'msgstr ""'                                            + sLineBreak
      + Format('"Project-Id-Version: %s\n"'  , [edit6 .Text] ) + sLineBreak
      + Format('"Report-Msgid-Bugs-To: %s\n"', [edit7 .Text] ) + sLineBreak
      + Format('"POT-Creation-Date: %s\n"'   , [edit8 .Text] ) + sLineBreak
      + Format('"PO-Revision-Date: %s\n"'    , [edit9 .Text] ) + sLineBreak
      + Format('"Last-Translator: %s\n"'     , [edit10.Text] ) + sLineBreak
      + Format('"Language-Team: %s\n"'       , [edit11.Text] ) + sLineBreak
      + Format('"Language: %s\n"'            , [poArray[idx]]) + sLineBreak
      + Format('"MIME-Version: 1.0\n"'       , []            ) + sLineBreak
      + Format('"Content-Type: %s\n"'        , [edit12.Text] ) + sLineBreak
      + Format('"Content-Transfer-Encoding: 8bit\n"', [])      + sLineBreak;

      poLocales[idx].Write(Pointer(oxt)^,Length(oxt));

      if not dbf1.Active then
      dbf1.Open;
      dbf1.First;

      while not dbf1.EOF do
      begin
        oxt := Format('msgid "%s"', [dbf1.FieldByName('msgid').AsString]);
        oxt += sLineBreak;
        with dbf1 do
        begin
          case idx of
          0: begin oxt += Format('msgstr "%s"', [FieldByName('eng').AsString]); end;
          1: begin oxt += Format('msgstr "%s"', [FieldByName('deu').AsString]); end;
          2: begin oxt += Format('msgstr "%s"', [FieldByName('rus').AsString]); end;
          3: begin oxt += Format('msgstr "%s"', [FieldByName('pol').AsString]); end;
          end;
        end;
        oxt += sLineBreak;
        poLocales[idx].Write(Pointer(oxt)^,Length(oxt));
        dbf1.Next;
      end;
      poLocales[idx].Free;

      try
        moproc := TProcess.Create(nil);
        moproc.Executable  := edit13.Text;
        moproc.Parameters.Add('-o');
        moproc.Parameters.Add(ox1 );
        moproc.Parameters.Add(ox2 );
        moproc.Options := [poWaitOnExit];
        moproc.Execute;
      finally
        moproc.Free;
      end;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CheckGroup1.Items.Add(Edit4.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Edit6 .Enabled := false;
  Edit7 .Enabled := false;
  Edit8 .Enabled := false;
  Edit9 .Enabled := false;
  Edit10.Enabled := false;
  Edit11.Enabled := false;
  Edit12.Enabled := false;

  Panel4.Enabled := false;
  Panel4.Hide;

  Panel2.Enabled := true;
  Panel2.Show;

  BitBtn1.Enabled := true;  BitBtn1.Show;
  BitBtn2.Enabled := true;  BitBtn2.Show;
  BitBtn3.Enabled := true;  BitBtn3.Show;

  Edit1.Enabled := true; Edit1.Show;
  Edit5.Enabled := true; Edit5.Show;

  SpeedButton1.Enabled := true; SpeedButton1.Show;
  SpeedButton2.Enabled := true; SpeedButton2.Show;

  StringGrid1.Enabled := true; StringGrid1.Show;

end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  BitBtn1.Enabled := false;  BitBtn1.Hide;
  BitBtn2.Enabled := false;  BitBtn2.Hide;
  BitBtn3.Enabled := false;  BitBtn3.Hide;

  Edit1.Enabled := false; Edit1.Hide;
  Edit5.Enabled := false; Edit5.Hide;

  SpeedButton1.Enabled := false; SpeedButton1.Hide;
  SpeedButton2.Enabled := false; SpeedButton2.Hide;

  StringGrid1.Enabled := false; StringGrid1.Hide;

  Panel2.Hide;
  Panel2.Enabled := false;

  Panel4.Enabled := true;
  Panel4.Left := 8;
  Panel4.Show;

  Edit6 .Enabled := true;
  Edit7 .Enabled := true;
  Edit8 .Enabled := true;
  Edit9 .Enabled := true;
  Edit10.Enabled := true;
  Edit11.Enabled := true;
  Edit12.Enabled := true;
end;

procedure TForm1.Edit4Change(Sender: TObject);
begin
  if Length(Trim(Edit4.Text)) < 1 then
  Button3.Enabled := false else
  Button3.Enabled := true;
end;

// ----------------------------------------------------------------------------
// @brief  add button - add a new row (record) to the end of the StringGrid1.
//         The data will be saved after the end of the application, or on
//         action the "post" button.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.BitBtn1Click(Sender: TObject);
var
  row: integer;
begin
  row := Form1.StringGrid1.RowCount;
  Form1.StringGrid1.RowCount := row + 1;
end;

// ----------------------------------------------------------------------------
// @brief  delete button - delete the current row (record) from the position
//         of the StringGrid1.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.BitBtn2Click(Sender: TObject);
var
  i: integer;
begin
  with Form1.StringGrid1 do
  begin
    if RowCount = 1 then
    exit;

    for i := Row to RowCount - 2 do
    Rows[i].Assign(Rows[i + 1]);
    RowCount := RowCount - 1;
  end;
end;

// ----------------------------------------------------------------------------
// @brief  post button - save the current dataset (StringGrid1) to the created
//         database file (podata.dbf).
//         The time, that is needed for it depends on the sum of data, and
//         speed of the computer, and some operating system things.
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.BitBtn3Click(Sender: TObject);
var
  t_row: Integer;
begin
  if not dbf1.Active then
  dbf1.Open;
  dbf1.First;

  progressBar1.Position := 0;
  progressBar1.Max := dbf1.RecordCount;

  // ATTENTION: All data will be delete !!!
  with dbf1 do
  begin
    if stringGrid1.RowCount > 1 then
    begin
      for t_row := stringGrid1.RowCount - 1 downto 1 do
      begin
        showmessage('111');
        ProgressBar1.Position :=
        ProgressBar1.Max - t_row;
        Edit; Delete;
        Refresh;
        PackTable;
      end;
    end;
  end;

  progressBar1.Max := stringGrid1.RowCount - 1;
  progressBar1.Position := 0;

    t_row := 0;
    while true do
    begin
      if (t_row > progressBar1.Max-1) then
      break;
      progressBar1.Position := t_row + 1;

      with Form1.dbf1 do
      begin
        Insert; Edit;
        FieldByName('MSGID').AsString := stringGrid1.Cells[0, t_row + 1];
        FieldByName('ENG'  ).AsString := stringGrid1.Cells[1, t_row + 1];
        FieldByName('DEU'  ).AsString := stringGrid1.Cells[2, t_row + 1];
        FieldByName('RUS'  ).AsString := stringGrid1.Cells[3, t_row + 1];
        FieldByName('POL'  ).AsString := stringGrid1.Cells[4, t_row + 1];
        Post;
      end;

      inc(t_row);
    end;
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
var openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(Application);
  try
    openDialog.InitialDir := f_path;
    if not openDialog.Execute then
    begin
      ShowMessage('Error occur');
      dbf1.Close;
      exit;
    end;
    jsrFile := openDialog.FileName;
    if ExtractFileExt(jsrFile) <> 'jsr' then
    begin
      ShowMessage('.jsr file seems not okay.');
      dbf1.Close;
      exit;
    end;
    dbf1.Open;
  finally
    openDialog.Free;
  end;
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
var
  selDialog: TSelectDirectoryDialog;
begin
  selDialog := TSelectDirectoryDialog.Create(Application);
  try
    if not selDialog.Execute then
    begin
      ShowMessage('Error occur');
      exit;
    end;
    poFolder   := selDialog.FileName;
    Edit1.Text := poFolder;
    DBF1.FilePathFull:= AppendPathDelim(poFolder);
  finally
    selDialog.Free;
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  openDialog: TOpenDialog;
  t_row: Integer;
begin
  openDialog := TOpenDialog.Create(Application);
  try
    if OpenDialog.Execute = false then
    begin
      ShowMessage('Error occur');
      exit;
    end;
    poDataFile := OpenDialog.FileName;
    Edit5.Text := poDataFile;
    DBF1.Close;
    DBF1.TableName:= poDataFile;
    dbf1.Open;
    dbf1.First;

    StringGrid1.Clear;
    while not dbf1.EOF do
    begin
      with Form1.StringGrid1 do
      begin
        with dbf1 do
        begin
          t_row := stringGrid1.RowCount;
          stringGrid1.RowCount := t_row + 1;
          Cells[0, t_row] := FieldByName('msgid').AsString;
          Cells[1, t_row] := FieldByName('eng').AsString;
          Cells[2, t_row] := FieldByName('deu').AsString;
          Cells[3, t_row] := FieldByName('rus').AsString;
          Cells[4, t_row] := FieldByName('pol').AsString;
        end;
      end;
      dbf1.Next;
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(Application);
  try
    openDialog.InitialDir := AppendPathDelim(Application.ExeName);
    openDialog.Filter := '"All files|*.*|Executables|*.exe|msgfmt.exe|msgfmt.exe"';
    openDialog.DefaultExt := '.exe';
    if openDialog.Execute = false then
    begin
      ShowMessage('open error occur');
      exit;
    end;
    if not openDialog.FileName.Contains(LowerCase('msgfmt.exe'))
    or not FileExists(openDialog.FileName) then
    begin
      ShowMessage('open failed error.');
      exit;
    end;
    edit13.Text := openDialog.FileName;
  finally
    openDialog.Free;
  end;
end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
begin
  dataModified := true;
end;

// ----------------------------------------------------------------------------
// @brief
//
// @return Nothing - procedures does not give return values.
// @author (c) 2022 by paule32
// ----------------------------------------------------------------------------
procedure TForm1.StringGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 13) then
  begin
    //beep;
  end;
end;

end.

