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

