==================
Erlang Backend App
==================

An Erlang OTP backend app using Cowboy_ and craterl_.

Prerequisites
=============

You will need `Erlang 18`_.

Rebar3_ is bundled with the app as ``rebar3``.

Build
=====

Build the app like so::

    $ ./rebar3 compile

Run
===

Run the app with an attached Erlang shell, like so::

    $ ./rebar3 shell --apps guestbook

You can create a production-ready release that doesn't need Erlang installed to
run. Do that like so::

    $ ./rebar3 as prod release
    $ ./_build/prod/rel/guestbook/bin/guestbook foreground

Stop the app with *Ctrl-c*.

.. _Cowboy: https://github.com/ninenines/cowboy
.. _craterl: https://github.com/crate/craterl
.. _Erlang 18: http://www.erlang.org/download.html
.. _Rebar3: http://www.rebar3.org/
