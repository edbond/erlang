all:
	erlc +debug_info utf8.erl
	erlc +debug_info win1251.erl
	erlc +debug_info win1252.erl
	erlc +debug_info fb2.erl
	erlc +debug_info fb2_db.erl
	erlc +debug_info fb2_web.erl
	erlc win1251.erl
	#erl -s fb2 start -s init stop -noshell
	erl -s fb2 start +A128 -noshell
