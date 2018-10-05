rebar3 compile
erl -sname $1 -pa `rebar3 path` -run demo
