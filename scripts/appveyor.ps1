Function InstallPythonMods {
  $cmd = $env::PYTHON + "\python.exe -m pip install numpy"
  Exec{ $cmd }
  $cmd = $env::PYTHON + "\python.exe -m pip install scipy"
  Exec{ $cmd }
}
