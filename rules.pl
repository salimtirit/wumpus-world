:- use_module(library(clpfd)).
:- style_check(-singleton).
:- discontiguous location/3.
:- discontiguous dir/2.
:- discontiguous action/2.
:- discontiguous bump/1.

dir(1, east).
isWinner(0) :- false.
bump(0) :- false.

dir(T1,north) :-
    T0 is T1 - 1,
    (
        ((action(T0,hit); action(T0,forward)), dir(T0,north));
        (action(T0,clockWise), dir(T0,west));
        (action(T0,counterClockWise), dir(T0,east))
    ).

dir(T1,south) :-
    T0 is T1 - 1,
    (
        ((action(T0,hit); action(T0,forward)), dir(T0,south));
        (action(T0,clockWise), dir(T0,east));
        (action(T0,counterClockWise), dir(T0,west))
    ).

dir(T1,east) :-
    T0 is T1 - 1,
    (
        ((action(T0,hit); action(T0,forward)), dir(T0,east));
        (action(T0,clockWise), dir(T0,north));
        (action(T0,counterClockWise), dir(T0,south))
    ).

dir(T1,west) :-
    T0 is T1 - 1,
    (
        ((action(T0,hit); action(T0,forward)), dir(T0,west));
        (action(T0,clockWise), dir(T0,south));
        (action(T0,counterClockWise), dir(T0,north))
    ).


% location son hamlenin bir sonrakinde de ileri gitmiş gibi görüyor 
location(1,1,1).
location(T1,R,C) :-
    T0 #= T1 - 1,
    RN #= R - 1,
    RS #= R + 1,
    CW #= C - 1,
    CE #= C + 1,

    (
        ((action(T0,clockWise); action(T0,counterClockWise)), location(T0,R,C));
        ((action(T0,hit); action(T0,forward)), bump(T1), location(T0,R,C));
        ((action(T0,hit); action(T0,forward)), dir(T0,north), not(bump(T1)), location(T0,RS,C));
        ((action(T0,hit); action(T0,forward)), dir(T0,south), not(bump(T1)), location(T0,RN,C));
        ((action(T0,hit); action(T0,forward)), dir(T0,west), not(bump(T1)), location(T0,R,CE));
        ((action(T0,hit); action(T0,forward)), dir(T0,east), not(bump(T1)), location(T0,R,CW))
    ).

wallInFront(T1) :-
    location(T1,R,C),
    (
        bump(T1); % bura yanlış olabilir
        %north
        (
            dir(T1, north),
            RNN #= R - 2,
            RN #= R - 1,
            CW #= C - 1,
            CE #= C + 1,
            (
                (location(X, R, C), dir(X, north), bump(X));
                (location(X, RN, CW), dir(X, east), bump(X));
                (location(X, RNN, C), dir(X, south), bump(X));
                (location(X, RN, CE), dir(X, west), bump(X))
            )
        );
        %east
        (
            dir(T1, east),
            RN #= R - 1,
            RS #= R + 1,
            CE #= C + 1,
            CEE #= C + 2,
            (
                (location(X, R, C), dir(X, east), bump(X));
                (location(X, RN, CE), dir(X, south), bump(X));
                (location(X, R, CEE), dir(X, west), bump(X));
                (location(X, RS, CE), dir(X, north), bump(X))  
            )
        );
        %south
        (
            dir(T1, south),
            RS #= R + 1,
            RSS #= R + 2,
            CW #= C - 1,
            CE #= C + 1,
            (
                (location(X, R, C), dir(X, south), bump(X));
                (location(X, RS, CW), dir(X, east), bump(X));
                (location(X, RSS, C), dir(X, north), bump(X));
                (location(X, RS, CE), dir(X, west), bump(X))
            )
        );
        %west
        (
            dir(T1, west),
            RN #= R - 1,      
            RS #= R + 1,
            CW #= C - 1,
            CWW #= C - 2,
            (
                (location(X, R, C), dir(X, west), bump(X));
                (location(X, RN, CW), dir(X, south), bump(X));
                (location(X, R, CWW), dir(X, east), bump(X));
                (location(X, RS, CW), dir(X, north), bump(X)) 
            )
        )
    ),!.

isInSideSight(T1, R, C) :-
    RN4 #= R - 4,
    RN3 #= R - 3,
    RN2 #= R - 2,
    RN1 #= R - 1,

    RS1 #= R + 1,
    RS2 #= R + 2,
    RS3 #= R + 3,
    RS4 #= R + 4,

    CW4 #= C - 4,
    CW3 #= C - 3,
    CW2 #= C - 2,
    CW1 #= C - 1,

    CE1 #= C + 1,
    CE2 #= C + 2,
    CE3 #= C + 3,
    CE4 #= C + 4,

    X #>= 0,
    X #=< T1,
    Y #>= 0,
    Y #< T1,
    not(location(Y,R,C)),
    wumpusSight(X),
    (
        %north
        (
            dir(X, north),
            (
                location(X, RS1, C);
                location(X, RS2, C);
                location(X, RS3, C);
                location(X, RS4, C)
            )
        );
        %east
        (
            dir(X, east),
            (
                location(X, R, CW1);
                location(X, R, CW2);
                location(X, R, CW3);
                location(X, R, CW4)
            )
        );
        %south
        (
            dir(X, south),
            (
                location(X, RN1, C);
                location(X, RN2, C);
                location(X, RN3, C);
                location(X, RN4, C)
            )
        );
        %west
        (
            dir(X, west),
            (
                location(X, R, CE1);
                location(X, R, CE2);
                location(X, R, CE3);
                location(X, R, CE4)
            )
        )
    ),!.

isWinner(T1) :-
        (
            action(T1, hit),
            location(T1, R, C),
            wumpusSmell(T1),

            RN5 #= R - 5,
            RN4 #= R - 4,
            RN3 #= R - 3,
            RN2 #= R - 2,
            RN1 #= R - 1,

            RS1 #= R + 1,
            RS2 #= R + 2,
            RS3 #= R + 3,
            RS4 #= R + 4,
            RS5 #= R + 5,

            CW5 #= C - 5,
            CW4 #= C - 4,
            CW3 #= C - 3,
            CW2 #= C - 2,
            CW1 #= C - 1,

            CE1 #= C + 1,
            CE2 #= C + 2,
            CE3 #= C + 3,
            CE4 #= C + 4,
            CE5 #= C + 5,

            X #>= 0,
            X #=< T1,
            %north
            ((
                dir(T1, north),
                not(isInSideSight(T1,R, CW1)),
                not(isInSideSight(T1,R, CE1)),
                not(isInSideSight(T1,RS1, C)),
                (      
                    (wumpusSight(X),location(X, RS1, C), dir(X, north));
                    (wumpusSight(X),location(X, RS2, C), dir(X, north));
                    (wumpusSight(X),location(X, RS3, C), dir(X, north));
                    (wumpusSight(X),location(X, RN1, CW1), dir(X, east));
                    (wumpusSight(X),location(X, RN1, CW2), dir(X, east));
                    (wumpusSight(X),location(X, RN1, CW3), dir(X, east));
                    (wumpusSight(X),location(X, RN1, CW4), dir(X, east));
                    (wumpusSight(X),location(X, RN2, C), dir(X, south));
                    (wumpusSight(X),location(X, RN3, C), dir(X, south));
                    (wumpusSight(X),location(X, RN4, C), dir(X, south));
                    (wumpusSight(X),location(X, RN5, C), dir(X, south));
                    (wumpusSight(X),location(X, RN1, CE1), dir(X, west));
                    (wumpusSight(X),location(X, RN1, CE2), dir(X, west));
                    (wumpusSight(X),location(X, RN1, CE3), dir(X, west));
                    (wumpusSight(X),location(X, RN1, CE4), dir(X, west))          
                )
            );
            %east
            (
                dir(T1, east),
                not(isInSideSight(T1,R, CW1)),
                not(isInSideSight(T1,RN1, C)),
                not(isInSideSight(T1,RS1, C)),
                (     
                    ( wumpusSight(X), location(X, R, CW1), dir(X, east));
                    ( wumpusSight(X), location(X, R, CW2), dir(X, east));
                    ( wumpusSight(X), location(X, R, CW3), dir(X, east));
                    ( wumpusSight(X), location(X, RN1, CE1), dir(X, south));
                    ( wumpusSight(X), location(X, RN2, CE1), dir(X, south));
                    ( wumpusSight(X), location(X, RN3, CE1), dir(X, south));
                    ( wumpusSight(X), location(X, RN4, CE1), dir(X, south));
                    ( wumpusSight(X), location(X, R, CE2), dir(X, west));
                    ( wumpusSight(X), location(X, R, CE3), dir(X, west));
                    ( wumpusSight(X), location(X, R, CE4), dir(X, west));
                    ( wumpusSight(X), location(X, R, CE5), dir(X, west));    
                    ( wumpusSight(X), location(X, RS1, CE1), dir(X, north));  
                    ( wumpusSight(X), location(X, RS2, CE1), dir(X, north));  
                    ( wumpusSight(X), location(X, RS3, CE1), dir(X, north));  
                    ( wumpusSight(X), location(X, RS4, CE1), dir(X, north))      
                )
            );
            %south
            (
                dir(T1, south),
                not(isInSideSight(T1,R, CW1)),
                not(isInSideSight(T1,R, CE1)),
                not(isInSideSight(T1,RN1, C)),
                (      
                    ( wumpusSight(X),location(X, RN1, C), dir(X, south));
                    ( wumpusSight(X),location(X, RN2, C), dir(X, south));
                    ( wumpusSight(X),location(X, RN3, C), dir(X, south));
                    ( wumpusSight(X),location(X, RS1, CW1), dir(X, east));
                    ( wumpusSight(X),location(X, RS1, CW2), dir(X, east));
                    ( wumpusSight(X),location(X, RS1, CW3), dir(X, east));
                    ( wumpusSight(X),location(X, RS1, CW4), dir(X, east));
                    ( wumpusSight(X),location(X, RS2, C), dir(X, north));
                    ( wumpusSight(X),location(X, RS3, C), dir(X, north));
                    ( wumpusSight(X),location(X, RS4, C), dir(X, north));
                    ( wumpusSight(X),location(X, RS5, C), dir(X, north));
                    ( wumpusSight(X),location(X, RS1, CE1), dir(X, west));
                    ( wumpusSight(X),location(X, RS1, CE2), dir(X, west));
                    ( wumpusSight(X),location(X, RS1, CE3), dir(X, west));
                    ( wumpusSight(X),location(X, RS1, CE4), dir(X, west))
                )
            );
            %west
            (
                dir(T1, west),
                not(isInSideSight(T1,R, CE1)),
                not(isInSideSight(T1,RN1, C)),
                not(isInSideSight(T1,RS1, C)),
                (  
                    ( wumpusSight(X), location(X, R, CE1), dir(X, west));
                    ( wumpusSight(X), location(X, R, CE2), dir(X, west));
                    ( wumpusSight(X), location(X, R, CE3), dir(X, west));
                    ( wumpusSight(X), location(X, RN1, CW1), dir(X, south));
                    ( wumpusSight(X), location(X, RN2, CW1), dir(X, south));
                    ( wumpusSight(X), location(X, RN3, CW1), dir(X, south));
                    ( wumpusSight(X), location(X, RN4, CW1), dir(X, south));
                    ( wumpusSight(X), location(X, R, CW2), dir(X, east));
                    ( wumpusSight(X), location(X, R, CW3), dir(X, east));
                    ( wumpusSight(X), location(X, R, CW4), dir(X, east));
                    ( wumpusSight(X), location(X, R, CW5), dir(X, east));
                    ( wumpusSight(X), location(X, RS1, CW1), dir(X, north)); 
                    ( wumpusSight(X), location(X, RS2, CW1), dir(X, north));
                    ( wumpusSight(X), location(X, RS3, CW1), dir(X, north));
                    ( wumpusSight(X), location(X, RS4, CW1), dir(X, north)) 
                )
            ))
        ),!.