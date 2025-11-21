# ul2025app68 : Smile Life

## Proposal

### User stories

As a player, when I am in a game room, I want to see the state of the game and the cards that I and the other players have played, so that I can understand the situation of the match.

As a player, when it is my turn, I want to be able to either draw and play a card, or play a card from the discard pile, in order to make the game progress.

As a player, when I play a card, I want to know what this card does and whether I am allowed to play it according to the rules of the game.

As a player, when reaching the end of the game, I want to know who won by seeing the cards of each player and the amount of points they have.

### Requirements

The game must allow multiple players to join and interact with the same evolving game state. Cards in a player's hand must be visible only to that specific player. The app must show all cards that have been played to all players, as well as the number of points each of them has. The game must inform all players whose turn it is and when the game is over.

Gameplay will be controlled by a finite state machine enforcing the rules and managing the players’ actions. This includes action requirements (whether a player is allowed to place a specific card), smile counting, turn order, and game-state transitions. The projection of the game state to the UI will also be handled remotely by the server.

Each card type must apply its respective effects, which can be separated into three groups, and these effects must be handled by the server:
- Cards with points (smiles) must only be placed in front of the player who plays them, if allowed (some cards require prerequisites). When placed, they must update the UI, the game state, and the point counter if necessary.
- Malus cards do not grant points and can only be placed on other players if allowed. When placed, they must update the UI, the game state, and the point counter if necessary.
- Bonus cards may or may not grant points. They are always placed in front of the player who plays them and must update the UI, the game state, and the point counter if necessary.

### Roles

Matthias will handle the UI, Guillaume will manage the Wire component, and Corentin and Guillaume will collaborate on the Logic portion, distributing tasks evenly to ensure equal contribution from all team members

### Mock-ups

![](mockups/app.png)
![](mockups/app_2.png)
