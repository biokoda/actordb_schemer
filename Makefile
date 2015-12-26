.PHONY: all clean deps app

all: deps compile escriptize

deps:
	./rebar get-deps

compile:
	./rebar compile

escriptize:
	./escriptize.escript

clean:
	./rebar delete-deps
	./rebar clean
