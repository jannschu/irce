# irce

irce is a small, new IRC server written in Erlang. _Sometime_ it might be
[RFC 2812][] compliant and has several linking protocols. And use of course
Erlang's distribution features. If you know a better name, let me know.

## How to start

Go in your terminal in the directory of irce (the cloned repo) and type this:
	erl -make
	erl -pa ebin

It should compile without errors and start the Erlang Shell, now just do
	mnesia:start().
	application:start(irce).

Maybe I will add a boot file (reminder).

## Why another IRCd?

I don't know. You don't have to use it. I'm doing it because of Erlang.

[RFC 2812]: http://tools.ietf.org/html/rfc2812