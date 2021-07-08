unit ksAutoUpdater;

interface

type
  TksUpdateAvailableEvent = reference to procedure(Sender: TObject; AETag, AVersion: string);

  IksAutoUpdater = interface
    function GetETag: string;
    function GetUpdateUrl: string;
    procedure SetUpdateUrl(const Value: string);
    function CheckForUpdate: Boolean;
    function UpdateAvailable: Boolean;
    procedure DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
    property UpdateURL: string read GetUpdateUrl write SetUpdateUrl;
    property ETag: string read GetETag;
  end;


  function CreateAutoUpdater(AUrl,
                             ACurrentETag: string;
                             AIntervalSeconds: integer;
                             AUpdateAvailableEvent: TksUpdateAvailableEvent;
                             const ACheckBuild: Boolean = True): IksAutoUpdater;

implementation

uses Windows, Messages, Forms, System.Net.HttpClient, ExtCtrls, Classes,
  SysUtils, System.IOUtils, ShellAPi, IniFiles, DateUtils;

type
  TksAutoUpdater = class(TInterfacedObject, IksAutoUpdater)
  private
    FTimer: TTimer;
    FUpdateUrl: string;
    FIntervalSeconds: integer;
    FNewFile: string;
    FUpdateAvailable: Boolean;
    FETag: string;
    FCheckBuild: Boolean;
    FUpdateAvailableEvent: TksUpdateAvailableEvent;
    function GetETag: string;
    procedure CreateTimer;
    function GetUpdateUrl: string;
    procedure SetUpdateUrl(const Value: string);
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(AUpdateUrl: string;
                       ACheckIntervalSeconds: integer;
                       AETag: string;
                       ACheckBuild: Boolean;
                       AOnUpdateAvailable: TksUpdateAvailableEvent); virtual;
    destructor Destroy; override;
    function UpdateAvailable: Boolean;
    function CheckForUpdate: Boolean;
    procedure DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
    property UpdateUrl: string read GetUpdateUrl write SetUpdateUrl;
    property ETag: string read GetETag;
  end;

function CreateAutoUpdater(AUrl,
                           ACurrentETag: string;
                           AIntervalSeconds: integer;
                           AUpdateAvailableEvent: TksUpdateAvailableEvent;
                           const ACheckBuild: Boolean = True): IksAutoUpdater;
begin
  Result := TksAutoUpdater.Create(AUrl, AIntervalSeconds, ACurrentETag,  ACheckBuild, AUpdateAvailableEvent);
end;

procedure GetApplicationVersion(var AMajor, AMinor, ARelease, ABuild: integer; const AExe: string = '');
var
	VerInfoSize, VerValueSize, DUMMY: DWORD;
	VerInfo:pointer;
	VerValue: PVSFixedFileInfo;
  AFilename: string;
begin
  try
    AMajor := 0;
    AMinor := 0;
    ARelease := 0;
    ABuild := 0;
    AFileName := AExe;

    if AFileName = '' then
      AFileName := ParamStr(0);
    if FileExists(AFilename) = False then
      Exit;
    VerInfoSize:=GetFileVersionInfoSize(Pchar(AFilename), DUMMY);
    GetMem(verinfo, verinfosize);
    GetFileVersionInfo(pchar(AFilename),0,VerInfoSize, VerInfo);
    VerQueryValue(VerInfo,'\',Pointer(VerValue), VerValueSize);
    With VerValue^ do
    begin
      AMajor := dwFileVersionMS shr 16;				//Major
      AMinor := dwFileVersionMS and $FFFF;		//Minor
      ARelease := dwFileVersionLS shr 16;				//Release
      ABuild := dwFileVersionLS and $FFFF;    //Build
    end;
    FreeMem(VerInfo, VerInfoSize);
  except
    // corrupt *.exe?
  end;
end;

function GetApplicationBuild(const AExe: string = ''): integer;
var
  v1,v2,v3,v4: integer;
begin
  GetApplicationVersion(v1, v2, v3, v4, AExe);
  Result := v4;
end;

function GetBuildFromVersionStr(AVersionStr: string): integer;
var
  AStrings: TStrings;
begin
  Result := 0;
  AStrings := TStringList.Create;
  try
    AStrings.Text := Trim(StringReplace(AVersionStr, '.', #13, [rfReplaceAll]));
    if AStrings.Count = 0 then
      Exit;
    Result := StrToIntDef(AStrings[AStrings.Count-1], 0);
  finally
    AStrings.Free;
  end;
end;

{ TksAutoUpdater }

constructor TksAutoUpdater.Create(//AAppID: string;
                                AUpdateUrl: string;
                                ACheckIntervalSeconds: integer;
                                AETag: string;
                                ACheckBuild: Boolean;
                                AOnUpdateAvailable: TksUpdateAvailableEvent);
begin
  FETag := AETag;
  FUpdateUrl := AUpdateUrl;
  FUpdateAvailable := False;
  FIntervalSeconds := ACheckIntervalSeconds;
  FCheckBuild := ACheckBuild;
  FUpdateAvailableEvent := AOnUpdateAvailable;
  CreateTimer;
end;

destructor TksAutoUpdater.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TksAutoUpdater.DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
begin
  if AReplaceRunningExe then
  begin
    if FNewFile = '' then
    begin
      FUpdateAvailable := False;
      Exit;
    end;

    DeleteFile(ChangeFileExt(ParamStr(0), '.old'));
    RenameFile(ParamStr(0), ChangeFileExt(ParamStr(0), '.old'));
    if CopyFile(PWideChar(FNewFile), PWideChar(ParamStr(0)), False) then
    begin
      Sleep(1000);
      ShellExecute(0, nil, PChar(ParamStr(0)), PWideChar(AParams), nil, SW_SHOWNORMAL);
      DeleteFile(FNewFile);
      FUpdateAvailable := False;
      FNewFile := '';
      Sleep(1000);
      PostMessage(Application.MainForm.Handle, WM_QUIT, 0, 0);
    end;
  end
  else
  begin
    ShellExecute(0, nil, PChar(FNewFile), PChar('/SILENT'), nil, SW_SHOWNORMAL);
  end;
end;

function TksAutoUpdater.GetETag: string;
begin
  Result := FETag;
end;

function TksAutoUpdater.GetUpdateUrl: string;
begin
  Result := FUpdateUrl;
end;

procedure TksAutoUpdater.SetUpdateUrl(const Value: string);
begin
  FUpdateUrl := Value;
end;

function TksAutoUpdater.CheckForUpdate: Boolean;
var
  AHttp: THTTPClient;
  AResponse: IHTTPResponse;
  AStream: TMemoryStream;
begin
  Result := False;
  try
    AHttp := THTTPClient.Create;
    try
      AResponse := AHttp.Head(FUpdateUrl);
      if (AResponse.StatusCode = 200) and (AResponse.HeaderValue['ETag'] <> FETag) then
      begin
        AStream := TMemoryStream.Create;
        try
          AResponse := AHttp.Get(FUpdateUrl, AStream);
          if AResponse.StatusCode = 200 then
          begin
            FETag := AResponse.HeaderValue['ETag'];
            FNewFile := ChangeFileExt(TPath.GetTempFileName, '.exe');
            AStream.SaveToFile(FNewFile);
            if (GetApplicationBuild(FNewFile) > GetApplicationBuild) or (FCheckBuild = False) then
            begin
              Result := True;
              TThread.Queue(nil,
                procedure
                begin
                  if Assigned(FUpdateAvailableEvent) then
                    FUpdateAvailableEvent(Self, FETag, GetApplicationBuild(FNewFile).ToString);
                end
              );
            end;
          end;
        finally
          AStream.Free;
        end;
      end;
    finally
      AHttp.Free;
    end;
  except
    //
  end;
end;

procedure TksAutoUpdater.OnTimer(Sender: TObject);
begin
  if FUpdateAvailable  then
    Exit;

  FTimer.Enabled := False;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FUpdateAvailable := CheckForUpdate;

      if FUpdateAvailable = False then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            FTimer.Interval := FIntervalSeconds*1000;
            FTimer.Enabled := True;
          end
        );
      end;


    end
  ).Start;
end;

procedure  TksAutoUpdater.CreateTimer;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 3000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
end;

function TksAutoUpdater.UpdateAvailable: Boolean;
begin
  Result := FUpdateAvailable;
end;

end.


