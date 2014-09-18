# InfluxDB client for Erlang

InfluxDB client for Erlang. Work in progress.

## Installation

Add dependency:

    {deps, [
      {erflux, ".*",
        {git, "git://github.com/benoitc/hackney.git", {branch, "master"}}}
    }],

## Install

    ./rebar get-deps

Starting with default settings:

    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(idna),
    application:start(hackney),
    application:start(jsx),
    application:start(erflux),
    erflux_http:get_databases().

Starting with additional configuration:

    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(idna),
    application:start(hackney),
    application:start(jsx),
    application:load(erflux),
    application:set_env(erflux, host, <<"192.168.50.115">>),
    application:set_env(erflux, port, 8086),
    application:set_env(erflux, username, <<"root">>),
    application:set_env(erflux, password, <<"root">>),
    application:start(erflux),
    erflux_http:get_databases().

## Writing data

    erflux_http:write_series(<<"erfluxtest">>, [
      [
        { points, [ [ 1, 2, 3 ] ] },
        { name, <<"testseries">> },
        { columns, [ <<"A">>, <<"B">>, <<"C">> ] }
      ]
    ]).

or

    erflux_http:write_point(<<"erfluxtest">>, <<"testseries">>, [
      { <<"A">>, 1 },
      { <<"B">>, 2 },
      { <<"C">>, 3 }
    ]).

## Reading data

Reading many columns:

    erflux_http:read_point(<<"erfluxtest">>, [<<"A">>, <<"B">>], <<"testseries">>).

or a single column:

    erflux_http:read_point(<<"erfluxtest">>, <<"A">>, <<"testseries">>).

More complex queries like this:

    erflux_http:q(<<"erfluxtest">>, <<"select A from testseries limit 1">>).

## Other operations

Unit tests contain the full doverage of all other available operations. Unit tests can be found in the `test/` directory.

## Running unit tests

    ./rebar get-deps
    ./rebar compile
    ./rebar eunit

## Author

Radoslaw Gruchalski <radek@gruchalski.com>
https://github.com/radekg

## License

The MIT License (MIT)

Copyright (c) 2014 Radoslaw Gruchalski <radek@gruchalski.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.