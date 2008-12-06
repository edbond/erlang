-module(prime).
-author("edbond@gmail.com").

-export([init/1]).

prime(N, Nfac1) ->
        Nfac = Nfac1 * N,
        Prime = 2 + ((2 * Nfac) rem (N+1)),
        case Prime of
                2 -> omit;
                _ ->
                        io:format("~p\r", [Prime])
        end,
        prime(N+1, Nfac).

init(Args) ->
  prime(1,1).
