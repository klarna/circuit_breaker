suite=$(if $(SUITE), suite=$(SUITE), )

.PHONY:	all compile get-deps docs xref test eunit clean

all: get-deps compile

compile:
	./rebar compile

get-deps:
	./rebar get-deps

docs:
	./rebar doc skip_deps=true

xref:
	./rebar xref skip_deps=true

test: eunit

eunit:
	./rebar eunit $(suite) skip_deps=true

conf_clean:
	@:

clean:
	./rebar clean
	$(RM) doc/*.html
	$(RM) doc/*.png
	$(RM) doc/*.css
	$(RM) doc/edoc-info
	$(RM) ebin/*.d