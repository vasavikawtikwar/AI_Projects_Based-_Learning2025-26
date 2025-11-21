% Rock Paper Scissors Predictor Game - Prolog Implementation
% Author: AI Game Engine
% Description: An intelligent RPS game that learns player patterns and predicts moves

:- dynamic game_history/3.
:- dynamic choice_frequency/2.
:- dynamic pattern_sequence/2.
:- dynamic game_stats/4.

% Initialize game state
init_game :-
    retractall(game_history(_, _, _)),
    retractall(choice_frequency(_, _)),
    retractall(pattern_sequence(_, _)),
    retractall(game_stats(_, _, _, _)),
    assertz(choice_frequency(rock, 0)),
    assertz(choice_frequency(paper, 0)),
    assertz(choice_frequency(scissors, 0)),
    assertz(game_stats(0, 0, 0, 0)). % wins, losses, draws, total

% Main game entry point
start_game :-
    write('========================================'), nl,
    write('   ROCK PAPER SCISSORS PREDICTOR AI'), nl,
    write('========================================'), nl,
    write('The AI learns your patterns!'), nl,
    write('Can you outsmart it?'), nl, nl,
    init_game,
    game_loop.

% Main game loop
game_loop :-
    game_stats(Wins, Losses, Draws, Total),
    nl,
    write('--- CURRENT STATS ---'), nl,
    format('Your Wins: ~w | AI Wins: ~w | Draws: ~w | Total Games: ~w~n', 
           [Wins, Losses, Draws, Total]),
    (Total > 0 -> 
        WinRate is (Wins / Total) * 100,
        format('Your Win Rate: ~2f%~n', [WinRate])
    ; true),
    nl,
    write('Choose your move:'), nl,
    write('1. Rock'), nl,
    write('2. Paper'), nl,
    write('3. Scissors'), nl,
    write('4. Show Pattern Analysis'), nl,
    write('5. Reset Game'), nl,
    write('6. Quit'), nl,
    write('Enter choice (1-6): '),
    read(Choice),
    handle_choice(Choice).

% Handle user menu choice
handle_choice(1) :- play_round(rock), game_loop.
handle_choice(2) :- play_round(paper), game_loop.
handle_choice(3) :- play_round(scissors), game_loop.
handle_choice(4) :- show_pattern_analysis, game_loop.
handle_choice(5) :- init_game, write('Game reset! AI memory cleared.'), nl, game_loop.
handle_choice(6) :- 
    write('Thanks for playing! Final stats:'), nl,
    game_stats(W, L, D, T),
    format('Wins: ~w | Losses: ~w | Draws: ~w | Total: ~w~n', [W, L, D, T]),
    !.
handle_choice(_) :- 
    write('Invalid choice! Please enter 1-6.'), nl,
    game_loop.

% Play a round
play_round(PlayerChoice) :-
    predict_next_move(Prediction, Confidence),
    nl,
    write('========================================'), nl,
    format('AI PREDICTION: I think you will play ~w~n', [Prediction]),
    format('AI Confidence: ~w%~n', [Confidence]),
    write('========================================'), nl,
    get_counter_move(Prediction, AIChoice),
    format('You played: ~w~n', [PlayerChoice]),
    format('AI played: ~w~n', [AIChoice]),
    determine_winner(PlayerChoice, AIChoice, Result),
    display_result(Result, Prediction),
    update_game_state(PlayerChoice, AIChoice, Result),
    update_patterns(PlayerChoice).

% Predict the next move based on patterns
predict_next_move(Prediction, Confidence) :-
    game_stats(_, _, _, Total),
    (Total < 2 ->
        % Not enough history, random prediction
        random_member(Prediction, [rock, paper, scissors]),
        Confidence is 33
    ;
        % Try pattern-based prediction
        (get_recent_pattern_prediction(Prediction, Confidence) ->
            true
        ;
            % Fall back to frequency-based prediction
            get_frequency_prediction(Prediction, Confidence)
        )
    ).

% Get prediction based on recent 3-move patterns
get_recent_pattern_prediction(Prediction, Confidence) :-
    game_stats(_, _, _, Total),
    Total >= 5,
    findall(Choice, (game_history(_, Choice, _), _), History),
    length(History, Len),
    Len >= 3,
    append(_, [C1, C2, C3], History),
    atom_concat(C1, ',', Temp1),
    atom_concat(Temp1, C2, Temp2),
    atom_concat(Temp2, ',', Temp3),
    atom_concat(Temp3, C3, Pattern),
    pattern_sequence(Pattern, Stats),
    get_max_from_stats(Stats, Prediction, Confidence).

% Get prediction based on overall frequency
get_frequency_prediction(Prediction, Confidence) :-
    choice_frequency(rock, RockCount),
    choice_frequency(paper, PaperCount),
    choice_frequency(scissors, ScissorsCount),
    Total is RockCount + PaperCount + ScissorsCount,
    (Total > 0 ->
        MaxCount is max(RockCount, max(PaperCount, ScissorsCount)),
        (RockCount = MaxCount -> 
            Prediction = rock
        ; PaperCount = MaxCount -> 
            Prediction = paper
        ; 
            Prediction = scissors
        ),
        Confidence is min(85, (MaxCount / Total) * 100)
    ;
        random_member(Prediction, [rock, paper, scissors]),
        Confidence is 33
    ).

% Get maximum stat from pattern statistics
get_max_from_stats([rock-R, paper-P, scissors-S], Prediction, Confidence) :-
    Total is R + P + S,
    MaxCount is max(R, max(P, S)),
    (R = MaxCount -> 
        Prediction = rock
    ; P = MaxCount -> 
        Prediction = paper
    ; 
        Prediction = scissors
    ),
    Confidence is min(95, (MaxCount / Total) * 100).

% Get counter move to beat predicted move
get_counter_move(rock, paper).
get_counter_move(paper, scissors).
get_counter_move(scissors, rock).

% Determine winner
determine_winner(Same, Same, draw).
determine_winner(rock, scissors, win).
determine_winner(paper, rock, win).
determine_winner(scissors, paper, win).
determine_winner(_, _, lose).

% Display result
display_result(win, _) :-
    write('>>> YOU WIN! 🎉 <<<'), nl,
    write('You outsmarted the AI!'), nl.
display_result(lose, Prediction) :-
    write('>>> AI WINS! 🤖 <<<'), nl,
    format('The AI predicted you would play ~w!~n', [Prediction]).
display_result(draw, _) :-
    write('>>> DRAW! 🤝 <<<'), nl,
    write('Try again!'), nl.

% Update game statistics
update_game_state(PlayerChoice, AIChoice, Result) :-
    retract(game_stats(W, L, D, T)),
    (Result = win -> NewW is W + 1, NewL = L, NewD = D
    ; Result = lose -> NewW = W, NewL is L + 1, NewD = D
    ; NewW = W, NewL = L, NewD is D + 1
    ),
    NewT is T + 1,
    assertz(game_stats(NewW, NewL, NewD, NewT)),
    assertz(game_history(NewT, PlayerChoice, AIChoice)).

% Update pattern frequencies
update_patterns(PlayerChoice) :-
    retract(choice_frequency(PlayerChoice, Count)),
    NewCount is Count + 1,
    assertz(choice_frequency(PlayerChoice, NewCount)),
    update_sequence_patterns(PlayerChoice).

% Update 3-move sequence patterns
update_sequence_patterns(PlayerChoice) :-
    findall(Choice, game_history(_, Choice, _), History),
    length(History, Len),
    (Len >= 3 ->
        append(_, [C1, C2, C3], History),
        atom_concat(C1, ',', Temp1),
        atom_concat(Temp1, C2, Temp2),
        atom_concat(Temp2, ',', Temp3),
        atom_concat(Temp3, C3, Pattern),
        update_pattern_stats(Pattern, PlayerChoice)
    ; true).

% Update statistics for a pattern
update_pattern_stats(Pattern, Choice) :-
    (retract(pattern_sequence(Pattern, Stats)) ->
        update_stats_list(Stats, Choice, NewStats),
        assertz(pattern_sequence(Pattern, NewStats))
    ;
        create_initial_stats(Choice, InitialStats),
        assertz(pattern_sequence(Pattern, InitialStats))
    ).

% Update stats list
update_stats_list([rock-R, paper-P, scissors-S], rock, [rock-NewR, paper-P, scissors-S]) :-
    NewR is R + 1.
update_stats_list([rock-R, paper-P, scissors-S], paper, [rock-R, paper-NewP, scissors-S]) :-
    NewP is P + 1.
update_stats_list([rock-R, paper-P, scissors-S], scissors, [rock-R, paper-P, scissors-NewS]) :-
    NewS is S + 1.

% Create initial stats
create_initial_stats(rock, [rock-1, paper-0, scissors-0]).
create_initial_stats(paper, [rock-0, paper-1, scissors-0]).
create_initial_stats(scissors, [rock-0, paper-0, scissors-1]).

% Show pattern analysis
show_pattern_analysis :-
    nl,
    write('========================================'), nl,
    write('       PATTERN ANALYSIS'), nl,
    write('========================================'), nl,
    choice_frequency(rock, RockCount),
    choice_frequency(paper, PaperCount),
    choice_frequency(scissors, ScissorsCount),
    Total is RockCount + PaperCount + ScissorsCount,
    (Total > 0 ->
        RockPct is (RockCount / Total) * 100,
        PaperPct is (PaperCount / Total) * 100,
        ScissorsPct is (ScissorsCount / Total) * 100,
        format('Rock:     ~w plays (~2f%)~n', [RockCount, RockPct]),
        format('Paper:    ~w plays (~2f%)~n', [PaperCount, PaperPct]),
        format('Scissors: ~w plays (~2f%)~n', [ScissorsCount, ScissorsPct]),
        nl,
        get_most_played(RockCount, PaperCount, ScissorsCount, MostPlayed),
        format('💡 You play ~w most often~n', [MostPlayed]),
        game_stats(Wins, Losses, _, GamesPlayed),
        (GamesPlayed > 0 ->
            AIWinRate is (Losses / GamesPlayed) * 100,
            format('🎯 AI Win Rate: ~2f%~n', [AIWinRate])
        ; true),
        (GamesPlayed >= 5 ->
            write('⚠️  The AI is learning! Mix up your strategy.'), nl
        ; true)
    ;
        write('No data yet. Play some rounds first!'), nl
    ),
    write('========================================'), nl.

% Get most played choice
get_most_played(R, P, S, rock) :- R >= P, R >= S.
get_most_played(R, P, S, paper) :- P > R, P >= S.
get_most_played(_, _, _, scissors).

% Helper to get random member
random_member(X, List) :-
    length(List, Len),
    random(0, Len, Index),
    nth0(Index, List, X).

% Start message
:- write('Rock Paper Scissors Predictor loaded!'), nl,
   write('Type "start_game." to begin playing.'), nl.