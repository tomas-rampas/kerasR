Function InstallPythonMods {
  $cmd = $env:PYTHON + "\python.exe -m pip install keras"
  Invoke-Expression $cmd
}
