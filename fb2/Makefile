all:
	# gcc -I/usr/local/lib/erlang/lib/erl_interface-3.5.9/include conv_enc.c -o conv_enc
	erlc +debug_info fb2.erl
	erlc +debug_info win1251.erl
	erl -s fb2 start -s init stop -noshell
