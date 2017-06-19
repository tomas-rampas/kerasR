kerasR 0.7.0 (2017-06-19)
=================

The test suite is built out to include coverage
for all available user-facing functions. We also
now explicitly check and warn users if they attempt
to call a function without first installing keras.
Explicit pip commands are given for accomplishing
this.

We also now print the path to python that reticulate
is currently linked to. The reticulate package cannot
be restarted and because it will silently ignore
incorrect paths, this has lead to a lot of bug reports
that boil down to users not selecting the correct
version of python. Hopefully this change will make it
more obvious that this is the issue.



kerasR 0.6.1 (2017-06-01)
=================

This version adding a testing suite for
of the core functions in the library and
conforming with the goodpractice::gp()
recommendations. Also adds hooks to
TravisCL and AppVeyor for integrated
testing.


kerasR 0.5.0 (2017-04-26)
=================

Adding the following functions to make it easier
to initalize and check that keras is properly
installed; these also make it possible to restart
the python engine if needed.

* keras_available()
* keras_init()


kerasR 0.4.1 (2017-03-20)
===================

This is the inital working version that was publicly
pushed to GitHub. Older versions only ran locally
on the development machine. Currently all of the
layers in the keras library other than the functional
layers are wrapped and exported.

