REBAR=$(shell which rebar || echo ./rebar)

all: $(REBAR)
	$(REBAR) get-deps compile

tests:  $(REBAR)
	@echo Tests are not yet converted to Eunit!
	$(REBAR) eunit skip_deps=true suite=yamerl

sh: all
	erl -pa ebin/ -pa .eunit/ -pa deps/*/ebin -eval 'shell_default:m(yamerl_constr).'

test: tests

clean:
	$(REBAR) clean skip_deps=true

# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
		-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean:
	rm -f ./rebar
