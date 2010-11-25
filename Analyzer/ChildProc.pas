unit ChildProc;

interface

uses
  Windows, SysUtils, Classes;

type
  TReadFromPipe = procedure(Sender: TObject; strData: string) of object;

  EChildProc = class(Exception);
  TChildProc = class
  private
    m_ProcInfo: TProcessInformation;
    m_bProcessCreated: boolean;
    m_hChildStdoutRead, m_hChildStdoutWrite: THandle;
    m_hChildStdinRead, m_hChildStdinWrite: THandle;

    FOnTerminateProcess: TNotifyEvent;
    FOnReadFromPipe: TReadFromPipe;

    procedure FCreatePipes;
    procedure FDestroyPipes;
    procedure FCreateChildProcess(const ProcessFileName: TFileName;
      const strCommandLine, strProcessFilePath: string);
    procedure FDestroyChildProcess;
    function FReadFromPipe(out strData: string): boolean;

    procedure FDoTerminateProcess; // leave static
    procedure FDoReadFromPipe(const strData: string);

    procedure FCreateListnerThread;

  public
    destructor Destroy; override;
    procedure CreateProcess(const ProcessFileName: TFileName;
      const strCommandLine: string = '');
    procedure TerminateProcess;
    function WriteToPipe(strData: string): boolean;
    property ProcessCreated: boolean read m_bProcessCreated;
    property OnTerminateProcess: TNotifyEvent read FOnTerminateProcess write FOnTerminateProcess;
    property OnReadFromPipe: TReadFromPipe read FOnReadFromPipe write FOnReadFromPipe;
  end;

implementation

type
  TListnerThread = class(TThread)
  private
    m_ChildProc: TChildProc;
    m_strData: string;
    procedure FDoReadFromPipe;
    procedure FDoTerminateProcess;
    procedure FCheckOnProcessTermination;
    property ChildProc: TChildProc read m_ChildProc;
  protected
    procedure Execute; override;
  public
    constructor Create(AChildProc: TChildProc);
  end;

////////////////////////////////////////////////////////////////////////////////
// TChildProc

destructor TChildProc.Destroy;
begin
  TerminateProcess;
  inherited;
end;


procedure TChildProc.CreateProcess(const ProcessFileName: TFileName;
  const strCommandLine: string = '');
begin
  if (ProcessCreated) then
    TerminateProcess;

  if (not FileExists(ProcessFileName)) then
    exit;
  FCreatePipes;
  FCreateChildProcess(ProcessFileName, strCommandLine, ExtractFilePath(ProcessFileName));
  if (ProcessCreated) then
    FCreateListnerThread
  else
    FDestroyPipes;
end;


procedure TChildProc.TerminateProcess;
begin
  if (not ProcessCreated) then
    exit;

  FDestroyChildProcess;
  FDestroyPipes;
end;


procedure TChildProc.FCreatePipes;
var
  SecurityAttributes: SECURITY_ATTRIBUTES;
begin
  ZeroMemory(@SecurityAttributes, SizeOf(SECURITY_ATTRIBUTES));
  with SecurityAttributes do
  begin
    nLength := SizeOf(SECURITY_ATTRIBUTES);
    bInheritHandle := TRUE;
    lpSecurityDescriptor := nil;
  end;

  try
    // Reading pipe:
    if (not CreatePipe(m_hChildStdoutRead, m_hChildStdoutWrite, @SecurityAttributes, 0)) then
      raise EChildProc.Create('Unable to create reading pipe!');
    SetHandleInformation(m_hChildStdoutRead, HANDLE_FLAG_INHERIT, 0);
    // Ensure the read handle to the pipe for STDOUT is not inherited
    if (not SetHandleInformation(m_hChildStdoutRead, HANDLE_FLAG_INHERIT, 0)) then
      raise EChildProc.Create('Unable to unset inherited flag from STDOUT reading pipe!');

    // Writing pipe
    if (not CreatePipe(m_hChildStdinRead, m_hChildStdinWrite, @SecurityAttributes, 0)) then
      raise EChildProc.Create('Unable to create writing pipe!');
    // Ensure the write handle to the pipe for STDIN is not inherited
    if (not SetHandleInformation(m_hChildStdinWrite, HANDLE_FLAG_INHERIT, 0)) then
      raise EChildProc.Create('Unable to unset inherited flag from STDIN writing pipe!');

  except
    FDestroyPipes;
    raise;
  end;
end;


procedure TChildProc.FDestroyPipes;
begin
  if (m_hChildStdoutRead <> 0) then
    CloseHandle(m_hChildStdoutRead);
  if (m_hChildStdoutWrite <> 0) then
    CloseHandle(m_hChildStdoutWrite);
  if (m_hChildStdinRead <> 0) then
    CloseHandle(m_hChildStdinRead);
  if (m_hChildStdinWrite <> 0) then
    CloseHandle(m_hChildStdinWrite);

  m_hChildStdoutRead := 0;
  m_hChildStdoutWrite := 0;
  m_hChildStdinRead := 0;
  m_hChildStdinWrite := 0;
end;


procedure TChildProc.FCreateChildProcess(const ProcessFileName: TFileName;
  const strCommandLine, strProcessFilePath: string);
var
  StartInfo: TStartupInfo;
begin
  // Set up members of STARTUPINFO structure.
  ZeroMemory(@StartInfo, SizeOf(TStartupInfo));
  with StartInfo do
  begin
    cb := SizeOf(TStartupInfo);
    hStdError := m_hChildStdoutWrite;
    hStdOutput := m_hChildStdoutWrite;
    hStdInput := m_hChildStdInRead;
    dwFlags := dwFlags or STARTF_USESTDHANDLES;
  end;

  ZeroMemory(@m_ProcInfo, SizeOf(TProcessInformation));

  // TODO: examine how to create the process invisible in tray
  m_bProcessCreated := Windows.CreateProcess(
    PChar(ProcessFileName),    // application name
    PChar(strCommandLine),     // command line
    nil,                       // process security attributes
    nil,                       // primary thread security attributes
    TRUE,                      // handles are inherited
    CREATE_NO_WINDOW,          // creation flags
    nil,                       // use parent's environment
    PChar(strProcessFilePath), // use parent's current directory
    StartInfo,
    m_ProcInfo
  );

end;


procedure TChildProc.FDestroyChildProcess;
var
  cdExitCode: Cardinal;
begin
  if (not GetExitCodeProcess(m_ProcInfo.hProcess, cdExitCode)) then
    raise EChildProc.Create('Cannot get the child process exit code!');
  if (cdExitCode = STILL_ACTIVE) then
  begin
    if (not Windows.TerminateProcess(m_ProcInfo.hProcess, cdExitCode)) then
      raise EChildProc.Create('Cannot terminate the child process!');;
  end;

  m_bProcessCreated := FALSE;
end;


function TChildProc.FReadFromPipe(out strData: string): boolean;
var
  iBufferSize: integer;
  lwRead: LongWord;
begin
  Result := FALSE;
  strData := '';
  if (not ProcessCreated) then
    exit; 
  if (not PeekNamedPipe(m_hChildStdoutRead, nil, 0, nil, @iBufferSize, nil)) then
    exit;
  if (iBufferSize = 0) then
    exit;

  strData := StringOfChar(' ', iBufferSize);
  Result := ReadFile(m_hChildStdoutRead, strData[1], iBufferSize, lwRead, nil);
end;


function TChildProc.WriteToPipe(strData: string): boolean;
var
  lwWritten: LongWord;
  i, iSize: integer;
begin
  strData := TrimRight(strData) + sLineBreak;
  iSize := length(strData);
  i := 1;
  repeat
    Result := WriteFile(m_hChildStdinWrite, strData[i], iSize, lwWritten, nil);
    inc(i, lwWritten);
    dec(iSize, lwWritten);
  until (not (Result and (iSize > 0)));
end;


procedure TChildProc.FDoTerminateProcess;
begin
  if (Assigned(self) and Assigned(FOnTerminateProcess)) then
    FOnTerminateProcess(self);
end;


procedure TChildProc.FDoReadFromPipe(const strData: string);
begin
  if (Assigned(FOnReadFromPipe)) then
    FOnReadFromPipe(self, strData);
end;


procedure TChildProc.FCreateListnerThread;
begin
  TListnerThread.Create(self);
end;

////////////////////////////////////////////////////////////////////////////////
// TListnerThread

constructor TListnerThread.Create(AChildProc: TChildProc);
begin
  inherited Create(TRUE);
  m_ChildProc := AChildProc;
  FreeOnTerminate := TRUE;
  Resume;
end;


procedure TListnerThread.Execute;
begin
  while (ChildProc.ProcessCreated) do
  begin
    if (ChildProc.FReadFromPipe(m_strData)) then
      Synchronize(FDoReadFromPipe);
    Sleep(10);
    if (Terminated) then
      break;
    FCheckOnProcessTermination;
  end;
  Synchronize(FDoTerminateProcess);
end;


procedure TListnerThread.FDoReadFromPipe;
begin
  ChildProc.FDoReadFromPipe(m_strData);
end;


procedure TListnerThread.FDoTerminateProcess;
begin
  ChildProc.FDoTerminateProcess;
end;


procedure TListnerThread.FCheckOnProcessTermination;
var
  cdExitCode: Cardinal;
begin
  with ChildProc do
  begin
    if (GetExitCodeProcess(m_ProcInfo.hProcess, cdExitCode) and (cdExitCode <> STILL_ACTIVE)) then
      TerminateProcess;
  end;
end;

end.
