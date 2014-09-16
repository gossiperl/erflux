# InfluxDB client for Erlang

InfluxDB client for Erlang. Work in progress.

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
    application:start(erflux),
    erflux_http:get_databases().

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