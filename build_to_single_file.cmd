@REM ... culture is wack
dotnet publish -r win-x64 -c Release -p:PublishSingleFile=true -p:PublishReadyToRun=true --self-contained true -p:PublishTrimmed=true
