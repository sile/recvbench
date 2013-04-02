all: compile xref eunit                                                   

compile:
	@rebar compile skip_deps=true

xref:
	@rebar xref skip_deps=true

clean:
	@rebar clean skip_deps=true

eunit:
	@rebar eunit skip_deps=true

edoc:
	@rebar doc skip_deps=true

shell: compile
	erl -pz ebin apps/*/ebin deps/*/ebin +K true

start: compile
	erl -pz ebin apps/*/ebin deps/*/ebin +K true -eval 'application:start(recvbench).'
