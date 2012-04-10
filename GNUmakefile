.PHONY:	all compile doc check xref test eunit clean

all: compile xref

compile:
	./rebar compile

doc:
	./rebar doc

check:
	./rebar check-plt
	./rebar dialyze

xref:
	./rebar xref

test: eunit

eunit:
	./rebar eunit

clean:
	./rebar clean