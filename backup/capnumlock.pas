program capnumlock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, capsnumlock1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='capsnumlock';
  Application.Initialize;
  Application.CreateForm(TfCapnumlock, fCapnumlock);
  Application.Run;
end.

