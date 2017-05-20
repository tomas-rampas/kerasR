Function InstallPythonMods {
  Exec{ $env::PYTHON\python.exe -m pip install numpy }
  Exec{ $env::PYTHON\python.exe -m pip install scipy }
}
