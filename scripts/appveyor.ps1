Function InstallPythonMods {
  $cmd = $env:PYTHON + "\conda.exe conda install -c alchayward keras=0.1.2"
  Invoke-Expression $cmd
}
