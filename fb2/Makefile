all:
	erlc utf8.erl
	erlc win1251.erl
	erlc koi8r.erl
	erlc win1252.erl
	erlc iso88591.erl
	erlc fb2.erl
	erlc fb2_db.erl
	erlc fb2_web.erl
	erlc win1251.erl
	#erl -s fb2 start -s init stop -noshell
	erl -s fb2 start +A128 -noshell
