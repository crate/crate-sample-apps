.. highlight:: sh

==================
Erlang Backend App
==================

An Erlang OTP backend application, based on `Cowboy`_ and `craterl`_.

Prerequisites
=============

- You will need `Erlang`_, and `Rebar3`_.

Build
=====

Build the application::

    rebar3 compile

Run
===

Run the application with an attached Erlang shell::

    rebar3 shell --apps guestbook

You can also create a production-ready release that doesn't need Erlang
installed to run::

    rebar3 as prod release
    ./_build/prod/rel/guestbook/bin/guestbook foreground

Stop the app with *Ctrl-c*.

Maintenance
===========

When looking for more recent versions of dependency packages, try
https://hex.pm/, ``rebar3 update``, or ``rebar3 upgrade``.

.. _Cowboy: https://github.com/ninenines/cowboy
.. _craterl: https://github.com/crate/craterl
.. _Erlang: https://www.erlang.org/
.. _Rebar3: https://github.com/erlang/rebar3
