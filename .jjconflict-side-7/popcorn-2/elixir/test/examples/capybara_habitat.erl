-module(capybara_habitat).
-export([start/0, stop/0, create_capybara/2, environment/0]).
-export([capybara/2, feeder/1]).

start() ->
    register(environment, spawn(capybara_habitat, environment, [])),
    register(feeder, spawn(capybara_habitat, feeder, [environment])).

stop() ->
    environment ! stop,
    feeder ! stop,
    unregister(environment),
    unregister(feeder).

create_capybara(Name, Activity) ->
    spawn(capybara_habitat, capybara, [Name, Activity]).

environment() ->
    loop_environment(10).

loop_environment(Food) ->
    receive
        {request_food, CapybaraPid} ->
            if Food > 0 ->
                CapybaraPid ! {food, 1},
                loop_environment(Food - 1);
               true ->
                CapybaraPid ! {food, 0},
                loop_environment(Food)
            end;
        add_food ->
            loop_environment(Food + 5);
        stop ->
            ok
        after 10000 ->
            io:format("Environment is stable with remaining ~p units of food.~n",[Food]),
            loop_environment(Food)
    end.

feeder(EnvironmentPid) ->
    loop_feeder(EnvironmentPid).

loop_feeder(EnvironmentPid) ->
    timer:sleep(30000),
    EnvironmentPid ! add_food,
    loop_feeder(EnvironmentPid).

capybara(Name, Activity) ->
    loop_capybara(Name, Activity, hungry).

loop_capybara(Name, Activity, hungry) ->
    environment ! {request_food, self()},
    receive
        {food, Amount} when Amount > 0 ->
            io:format("~s is now eating.~n", [Name]),
            loop_capybara(Name, Activity, full);
        {food, 0} ->
            io:format("~s found no food and continues to be hungry.~n", [Name]),
            loop_capybara(Name, Activity, hungry)
    end;
loop_capybara(Name, Activity, full) ->
    timer:sleep(5000), % Simulate time taken to rest or play
    io:format("~s is now ~s.~n", [Name, Activity]),
    loop_capybara(Name, Activity, hungry).
