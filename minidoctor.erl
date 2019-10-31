%% File: minidoctor.erl
%% Author: Ken Lambert
%% Purpose: provides a reply function to respond to a patient
%% inputs.

-module(minidoctor).

-export([reply/7, start/0]).

-import(lists, [map/2]).
-import(maps, [find/2, from_list/1]).
-import(string, [join/2, tokens/2, to_lower/1, to_upper/1]).
-import(myrandom, [random_int/1, probability/1, pick_random/1]).

%% Holds the value for the boolean returned by the probability function
%% with the parameter 0.50
probabilityA() -> probability(0.50).

%% Holds the value for the boolean returned by the probability function
%% with the parameter 0.50
probabilityB() -> probability(0.50).

%% Creates the map of replacements
replacements() -> from_list([{"i", "you"}, {"my", "your"}, {"your", "my"},
                             {"you", "I"}, {"me", "you"}]).
%% Creates the list of qualifiers
qualifiers() -> pick_random(["Why do you say that", "What makes you think that",
                            "What is causing you to believe that"]).

%% Creates a list of Hedges
hedges() -> pick_random(["Please tell me more", "Would you care to elaborate?","What else?"]).

%% Creates a list of previous topics
history() -> [].


%% Returns Value if not error, or DefaultValue otherwise
%% Calling forms: maybe(DefaultValue, {ok, Value})
%% Calling forms: maybe(DefaultValue, error)
%% Example calls:
%% maybe("No way!", error)
%% "No way!"
%% maybe("No way!", {ok, "Yes way!"})
%% "Yes way!"
maybe(DefaultValue, error) -> DefaultValue;
maybe(_DefaultValue, {ok, Value}) -> Value.

%% Returns replacement if Word is present, or Word otherwise.
my_lookup(Word, Replacements) -> maybe(Word, find(to_lower(Word),
                                                  Replacements)).

%% Switches personal pronouns in a sentence
%% Calling form: change_person(sentence)
%% Example call:
%% change_person("My mother hates me")
%% "your mother hates you"
change_person(Sentence, Replacements) ->
    join(map(fun(Word) -> my_lookup(Word, Replacements) end,
                                    tokens(Sentence, " ")), " ").

%% Replies to a sentence by changing persons.
%% and prepending a qualifier.
%% Calling form: reply(sentence)
%% Example call:
%% reply("My mother hates me")
%% "Why do you say that your mother hates you?"
reply(Sentence, History, Replacements, Qualifiers, Hedges, PA, PB) ->
    if
        PA ->
            if
                PB ->
                    Qualifiers++" "++change_person(Sentence,  Replacements) ++ "?";
                true ->
                    if
                        length(History) > 2 ->
                            "Earlier you said that "++change_person(Sentence, pick_random(History));
                        true ->
                            Qualifiers++" "++change_person(Sentence, Replacements) ++ "?"
                    end
            end;
      true ->
            Hedges
    end.

start() ->
    io:format("Good day, how can I help you (type 'quit' to exit)?~n"),
    driver_loop(history(), replacements(), qualifiers(), hedges()).

driver_loop(History, Replacements, Qualifiers, Hedges) ->
    PatientInput = string:strip(io:get_line(">>> "), right, $\n),
    Exit = to_upper(PatientInput),
    PA = probabilityA(),
    PB = probabilityB(),
    if
        Exit == "QUIT" ->
            io:format("Have a nice day!~n"),
            done;
        true -> io:format(reply(PatientInput, History, Replacements, Qualifiers, Hedges, PA, PB) ++ "\n"),
                driver_loop([PatientInput | History], Replacements, Qualifiers, Hedges)
        end.
