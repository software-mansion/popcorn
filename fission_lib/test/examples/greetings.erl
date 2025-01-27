-module(greetings).
-export([start/0]).

start() ->
User = "Jose",

% Define and spawn processes
Pid1 = spawn(fun() ->
    receive
        {Caller, request} ->
            Languages = ["Erlang", "Haskell", "Prolog", "Scala", "Clojure", "Elixir"],
            RandomIndex = rand:uniform(length(Languages)),
            Language = lists:nth(RandomIndex, Languages), 
            Caller ! {self(), Language}
    end
end),
Pid2 = spawn(fun() ->
    receive
        {Caller, request} ->
            Tasks = [
                "solve the halting problem",
                "automate boring human tasks",
                "optimize a bubble sort algorithm",
                "code up skynet",
                "spellcheck an entire website",
                "encrypt the internet"
            ],
            RandomIndex = rand:uniform(length(Tasks)),
            Task = lists:nth(RandomIndex, Tasks),
            Caller ! {self(), Task}
    end
end),
Pid3 = spawn(fun() ->
    receive
        {Caller, request} ->
            Compliments = [
                "you are a software maestro",
                "your code inspires baby unicorns",
                "you write bugs-free code",
                "you're the Jedi of programming",
                "you could out-code a machine",
                "you make recursion look easy"
            ],
            RandomIndex = rand:uniform(length(Compliments)),
            Compliment = lists:nth(RandomIndex, Compliments),
            Caller ! {self(), Compliment}
    end
end),

% Send requests to each process
Pid1 ! {self(), request},
Pid2 ! {self(), request},
Pid3 ! {self(), request},

% Collect results and display the final message
receive {Pid1, Language} ->
    receive {Pid2, Task} ->
        receive {Pid3, Compliment} ->
            erlang:display(User ++ ", your mission, if you choose to accept it, is to use "++ Language ++ " to " ++ Task ++ ". Also, " ++ Compliment)
        end
    end
end.
