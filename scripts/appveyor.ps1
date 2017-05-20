Function InstallPythonMods {
  $cmd = "C:\\Python27\python.exe -m pip install numpy"
  Invoke-Expression $cmd
  $cmd = $env:PYTHON + "\python.exe -m pip install numpy"
  Invoke-Expression $cmd
  $cmd = $env:PYTHON + "\python.exe -m pip install scipy"
  Invoke-Expression $cmd
}
