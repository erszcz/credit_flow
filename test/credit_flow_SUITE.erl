-module(credit_flow_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%%
%%' Common Test prelude
%%

all() ->
    [running_out_of_credit_blocks_sender,
     acking_by_the_receiver_unblocks_sender].

init_per_suite(Config) ->
    set_default_credit(#{initial => 3, more => 1}),
    Config.

end_per_suite(Config) ->
    Config.

%%.
%%' Tests
%%

running_out_of_credit_blocks_sender(_) ->
    %% given
    N = 3,
    Receiver = spawn_link(?MODULE, receiver_loop, [N]),
    %% when
    [ begin
          credit_flow:send(Receiver)
      end || _ <- lists:seq(1, N) ],
    %% then
    ?assert(credit_flow:blocked()).

acking_by_the_receiver_unblocks_sender(_) ->
    %% given
    N = 3,
    Receiver = spawn_link(?MODULE, receiver_loop, [N + 1, fun ack/1]),
    %% when
    [ begin
          credit_flow:send(Receiver)
      end || _ <- lists:seq(1, N) ],
    %% then
    ?assert(credit_flow:blocked()),
    %% when
    Receiver ! {ack_please, self()},
    receive
        {bump_credit, Credit} ->
            credit_flow:handle_bump_msg(Credit)
        after timer:seconds(1) ->
            ct:fail(sender_timeout)
    end,
    %% then
    ?assert(not credit_flow:blocked()).

%%.
%%' Helpers
%%

receiver_loop(N) -> receiver_loop(N, fun no_op/1).

receiver_loop(0, _) -> ok;
receiver_loop(N, HandleMessage) ->
    receive
        M ->
            HandleMessage(M),
            receiver_loop(N-1, HandleMessage)
        after timer:seconds(1) ->
            ct:fail(receiver_timeout)
    end.

no_op(_) -> ok.

ack({ack_please, Pid}) ->
    credit_flow:ack(Pid);
ack(_) ->
    ok.

set_default_credit(#{initial := Initial, more := More}) ->
    application:load(credit_flow),
    application:set_env(credit_flow, default_credit, {Initial, More}).

%%. vim: foldmethod=marker foldmarker=%%',%%.
