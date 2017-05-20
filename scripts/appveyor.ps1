Function InstallPythonMods {
  $cmd = $env:PYTHON + "\Scripts\conda.exe install -c alchayward keras=0.1.2"
  Invoke-Expression $cmd
}
