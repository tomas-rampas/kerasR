Function InstallPythonMods {
  $cmd = $env:PYTHON + "conda config --set always_yes yes --set changeps1 no"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "conda update -q conda"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "conda info -a"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "conda create -q -n test-environment python=%PYTHON_VERSION% numpy scipy nose"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "activate test-environment"
  Invoke-Expression $cmd

  $cmd = $env:PYTHON + "pip install keras"
  Invoke-Expression $cmd
}


