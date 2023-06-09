# WumpusWorld

This project is an implementation of the WumpusWorld game, which is a logic-based game. The agent starts from grid (1,1) facing to the east, and it should navigate the grid while avoiding the Wumpus and finding the gold.

## Game Rules

- Each grid (except (1,1)) might contain a Wumpus that:
  - Creates small in its 4 neighbor grids.
  - Can be seen from a grid if:
    - That grid is in the same row or column (direct view for the agent).
    - The distance between two grids is 4 or less depending on the agent's view.
  - The Wumpus becomes invisible after being seen.
- At each time-step, the agent can take the following actions:
  - Forward: move the agent one grid forward in the direction it is currently facing.
  - CounterClockWise: turn the agent 90 degrees counter-clockwise.
  - ClockWise: turn the agent 90 degrees clockwise.
  - Hit: kill the Wumpus if it is next to the agent and facing it.
- At each time-step, the agent might receive:
  - WumpusSmell: indicate the presence of a Wumpus in one of the neighboring grids.
  - WumpusSight: indicate the presence of a Wumpus in one of the grids the agent can see.
  - Bump: indicate that the agent has hit a wall and cannot move further in the current direction.
- The agent learns the location of the walls after bumping. After learning the location, its wallInFront predicate should work correctly.

## Implementation

This project implements the logical rules for the domain using Prolog. The program is submitted as a single file, `rules.pl`. The program should be evaluated by concatenating `rules.pl` with some experience in `kb.pl` and loading `kb.pl` in SWI-Prolog. The `isWinner()` predicate is used to evaluate the program's correctness.

## Experience Examples

```prolog
wumpusSight(1).
action(1,clockWise).
action(2,forward).
action(3,counterClockWise).
action(4,counterClockWise).
action(5,forward).
action(6,clockWise).
action(7,forward).
action(8,forward).
wumpusSmell(9).
action(9,hit).

% Here isWinner(9) should be True
```

```prolog
wumpusSight(1).
action(1,clockWise).
action(2,forward).
action(3,counterClockWise).
wumpusSight(4).
action(4,counterClockWise).
action(5,forward).
action(6,clockWise).
action(7,forward).
action(8,forward).
wumpusSmell(9).
action(9,hit).

% Here isWinner(9) should not be True
```

```prolog
action(1,forward).
bump(2).
action(2,clockWise).
action(3,forward).
action(4,counterClockWise).
action(5,forward).
action(6,counterClockWise).

% Here wallInFront(7) should be True.
```

## How to Use

To use this program, you need to have SWI-Prolog installed on your computer. Once you have it installed, do the following:

1. Clone this repository to your local machine.
2. Open a terminal window and navigate to the directory where you cloned the repository.
3. Open the SWI-Prolog
