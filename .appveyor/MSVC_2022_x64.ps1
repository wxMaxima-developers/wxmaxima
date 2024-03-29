# A script that pulls in Visual Studio variables
echo '- Setting environment variables for Visual Studio'
$ENV_LIST = "$env:temp\vcvars.txt"
if (!(Test-Path $ENV_LIST)) {
   cmd.exe /c "call `"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat`" && set > $ENV_LIST"
}
Get-Content "$ENV_LIST" | Foreach-Object {
  if ($_ -match "^(.*?)=(.*)$") {
    Set-Content "env:\$($matches[1])" $matches[2]
  }
}
