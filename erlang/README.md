# The erlang sample app

An OTP application exposing a REST API using the [Cowboy][3] http server library.

## Requirements

    [Erlang 18][1]

## Usage

To build and run the application [Rebar3][2] is used. 
It is bundled with the example app as ``rebar3``.

### Build
    
    $ ./rebar3 compile

### Run

Run the http app with an attached erlang shell

    $ ./rebar3 shell --apps guestbook

Create a full blown production-ready release, that can be put anywhere in order to run
this application (no erlang needs to be installed)

    $ ./rebar3 as prod release
    $ ./_build/prod/rel/guestbook/bin/guestbook foreground
    
Stop it with <Ctrl-C>.

[1]: [http://www.erlang.org/download.html]
[2]: [http://www.rebar3.org/]
[3]: [https://github.com/ninenines/cowboy]
