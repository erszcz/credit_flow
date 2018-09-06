-module(credit_flow_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%%
%%' Common Test prelude
%%

all() ->
    [running_out_of_credit_blocks_sender].

init_per_suite(Config) ->
    set_default_credit(#{initial => 3, more => 1}),
    Config.

end_per_suite(Config) ->
    Config.

%%.
%%' Tests
%%

running_out_of_credit_blocks_sender(_) ->
    N = 3,
    Receiver = spawn_link(?MODULE, receiver_loop, [N]),
    [ begin
          credit_flow:send(Receiver)
      end || _ <- lists:seq(1, N) ],
    ?assert(credit_flow:blocked()).

%%.
%%' Helpers
%%

receiver_loop(0) -> ok;
receiver_loop(N) ->
    receive
        _M ->
            receiver_loop(N-1)
        after timer:seconds(1) ->
            ct:fail(receiver_timeout)
    end.

set_default_credit(#{initial := Initial, more := More}) ->
    application:load(credit_flow),
    application:set_env(credit_flow, default_credit, {Initial, More}).

%%. vim: foldmethod=marker foldmarker=%%',%%.
