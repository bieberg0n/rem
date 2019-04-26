run:
	erlc -o ebin src/*.erl
	erl -pa ./ebin -noshell -s main main "" -s init stop
