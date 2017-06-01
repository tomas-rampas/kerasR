Function InstallPythonMods {
  $cmd = $env:PYTHON + "\Scripts\conda.exe config --set always_yes yes --set changeps1 no"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "\Scripts\conda.exe update -q conda"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "\Scripts\conda.exe info -a"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "\Scripts\conda.exe create -q -n test-environment python=%PYTHON_VERSION% numpy scipy nose"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "\Scripts\activate test-environment"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "\Scripts\pip.exe install keras"
  Invoke-Expression $cmd
}


