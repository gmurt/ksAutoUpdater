# ksAutoUpdater
Easy to use interface for adding auto-update functionality to your Delphi applications.

The interface uses the ETag of the remote file to determine if it is different to the one running. If it finds a different ETag to the one stored locally, then it will download the updated version and can also check the build of the updated version before updating to ensure it's a newer version.

It will check for an update 5 seconds after creation, then it will use the "interval" seconds which was passed as the parameter to the CreateAutoUpdater call.

Note: I use the Amazon S3 service to host my update files. If you plan on using another storage service, this should work provided it supports the ETag property.

#### Example use

```
uses ksAutoUpdater;

type
  TForm1 = class(TForm)
  private
    FAutoUpdater: IksAutoUpdater;
    procedure DoUpdateFound(Sender: TObject; AETag: string);
  public
    ...
  end;


constructor TForm1.Create(AOwner: TComponent);
begin
  FAutoUpdater := CreateAutoUpdater('https://eres.s3-eu-west-1.amazonaws.com/blah/blah/yourExe.exe',
	    			    LastStoredETag,
				    10,
				    DoUpdateFound);
end;


procedure TForm1.DoUpdateFound(Sender: TObject; AETag: string);
begin
  if (MessageDlg('Update found!'+#13+#10+''+#13+#10+'Install update now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then 
  begin
    // you will  want to store the AEtag parameter in an ini file (or DB) so it can be read the next time the application loads 
    // to prevent unnessessary downloading of the same *.exe again.

    FAutoUpdater.DoUpdate(True);	
  end;
end;

 ```
