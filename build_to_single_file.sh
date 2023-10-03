# .. lets hope this works :D
dotnet publish -r win-x64 -c Release -p:PublishSingleFile=true -p:PublishReadyToRun=true --self-contained true -p:PublishTrimmed=true
dotnet publish -r linux-x64 -c Release -p:PublishSingleFile=true -p:PublishReadyToRun=true --self-contained true -p:PublishTrimmed=true
dotnet publish -r osx-arm64 -c Release -p:PublishSingleFile=true -p:PublishReadyToRun=true --self-contained true -p:PublishTrimmed=true
dotnet publish -r osx-x64 -c Release -p:PublishSingleFile=true -p:PublishReadyToRun=true --self-contained true -p:PublishTrimmed=true