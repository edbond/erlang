-module(fb2_web).
-compile(export_all).

-record(fb2_info, {authors=[], title="", annotation="", encoding="", filename=""}).

start() ->
  application:start(mochiweb),
  Loop=fun(Req) -> ?MODULE:dispatch_requests(Req) end,
  mochiweb_http:start([{name, ?MODULE}, {ip, "127.0.0.1"}, {port, 8080}, {loop, Loop}]),
  receive
  after
    infinity ->
      ok
  end,
  ok.

stop() ->
  mochiweb_http:stop().

dispatch_requests(Request) ->
  Path = Request:get(path),
  Method = Request:get(method),
  Post = Request:parse_post(),
  io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
  Response = handle(Method, Path, Post),
  Request:respond(Response).

handle('GET', "/", _Post) ->
  Response = "<ul>" ++ lists:map(
    fun(Book) ->
        io_lib:format("<li>~s (~s)</li>", [Book#fb2_info.title, Book#fb2_info.annotation])
    end,
    fb2_db:get_all()
  ) ++ "</ul>",
  {200, [{"Content-Type", "text/html; charset=UTF-8"}], Response}.
