REBAR := rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
ERL       ?= erl

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) compile

shell: $(REBAR)
	$(REBAR) shell

test: $(REBAR)
	$(REBAR) do compile,eunit,dialyzer

clean: $(REBAR)
	$(REBAR) clean

distclean:
	rm -rf _build
	rm -rf ebin
	rm $(REBAR)

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3
