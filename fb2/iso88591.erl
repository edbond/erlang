-module(iso88591).
-compile(export_all).

decode(T) ->
  {ok, Utf8} = utf8:to_binary(T),
  binary_to_list(Utf8).
