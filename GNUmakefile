.PHONY:	all compile get-deps doc check xref test eunit ct clean

all: get-deps compile

compile:
	./rebar compile

get-deps:
	./rebar get-deps

doc:
	./rebar doc

check:
	./rebar check-plt
	./rebar dialyze

xref:
	./rebar xref

test: eunit

ct:
	./rebar ct

eunit:
	./rebar skip_deps=true eunit

clean:
	./rebar clean