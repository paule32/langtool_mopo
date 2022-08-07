// ----------------------------------------------------------------------------
// File:        langtool.pas
// Author:      Jens Kallup - paule32 <kallup-dev@web.de>
// Copyright:   (c) 2022 kallup non-profit
//
// License:     non-profit * only for education, and trial !!!
//              All modifications needs to query to the original author's (me).
//              You can use it at Your school as teacher for education.
//              But commercial usage is not allowed.
// ----------------------------------------------------------------------------
program langtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, gettext, DOS, Dialogs,
  Forms, dbflaz, lang1;

{$R *.res}

begin
  // locale: LANG='de_DE.UTF-8' ./app
  translateResourceStrings('.\application-de.mo'); // +
//  Copy(GetEnvironmentVariable('LANG'),1,2) + '.mo');

  RequireDerivedFormResource := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

