****************************
Mopidy-Moparty
****************************

.. image:: https://img.shields.io/pypi/v/Mopidy-Moparty
    :target: https://pypi.org/project/Mopidy-Moparty/
    :alt: Latest PyPI version

.. image:: https://github.com/pingiun/mopidy-moparty/workflows/Linting%20and%20Testing/badge.svg
    :target: https://circleci.com/gh/pingiun/mopidy-moparty
    :alt: GitHub Actions build status

Extended version of party extension, with Elm frontend


Installation
============

Install by running::

    python3 -m pip install Mopidy-Moparty

See https://mopidy.com/ext/moparty/ for alternative installation methods.


Configuration
=============

No configuration is currently supported


Local development
=================

#. `Install Elm <https://guide.elm-lang.org/install/elm.html>`_
#. `Install Mopidy <https://docs.mopidy.com/en/latest/installation/>`_
#. Create a virtual environment (with Python 3.7 or higher): ``virtualenv --system-site-packages --python python3 venv``
#. Activate virtual environment: ``source venv/bin/activate``
#. Install moparty in "develop" mode: ``pip install -e .``

You will need to compile the Elm code before you can use the client locally, the setup.py build_elm command requires
uglifyjs, but this is not needed for local development. For continuous compilation
I use `elm-live <https://github.com/wking-io/elm-live>`_. Here is the command I use::

    elm-live --no-server --no-reload -- --debug --output=mopidy_moparty/static/moparty.min.js frontend/Main.elm

Another option is using fswatch, then you can use this command::

    fswatch -o frontend | xargs -I{} -n1 elm make --debug --output=mopidy_moparty/static/moparty.min.js frontend/Main.elm

Project resources
=================

- `Source code <https://github.com/pingiun/mopidy-moparty>`_
- `Issue tracker <https://github.com/pingiun/mopidy-moparty/issues>`_
- `Changelog <https://github.com/pingiun/mopidy-moparty/blob/master/CHANGELOG.rst>`_


Credits
=======

- Original author: `Jelle Besseling <https://github.com/pingiun>`__
- Current maintainer: `Jelle Besseling <https://github.com/pingiun>`__
- `Contributors <https://github.com/pingiun/mopidy-moparty/graphs/contributors>`_
