Function InstallPythonMods {
  $cmd = $env::PYTHON + "\python.exe"
  Exec{ $cmd -m pip install numpy }
  Exec{ $cmd -m pip install scipy }
}
