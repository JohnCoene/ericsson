-module(readme).
-export([fac/1]).

%% factorial
fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).