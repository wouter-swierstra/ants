# lambda-wars
===========

Asteroids inspired game written in Haskell

### DrawHelper
This file contains functions that help with -- but are not limited to --- the drawing, translating and rotating of pictures.

### Config
This file supports all the global constants, which may also be derived from other constants. Those have a special section in this file.

## Implemented features
1. Spaceship is drawn as an arrow. It is saved as a constant `unit spaceship' that is scaled at compile-time
2. The tail of the spaceship is drawn as green dots. It is saved as a constant shape that is determined at compile time with respect to the size of the spaceship.
3. A new event has been added: When pressing the down-key, the spaceship slows down so it can make tighter bends.
4. Instead of limiting the player to a boundary, the player is transported to the other side of the viewport as if the playing field were spherical.

## Library extensions
1. Gloss' vector library was found insufficient and was extended with VectorExtensions.hs to support addition, subtraction, negation, arbitray generation (from point to point, from origin to point and ...) and generation of a list of random vectors.
2. List algorithms that remove duplicates, remove list elements as specified by an index or a list of indices, grouping by length and random list generation that either does or does not tracks the generator

## Known bugs
* Player may become trapped inside the transition zone in the boundary
* Bullets that hit enemies are not always destroyed, and vice versa. The first seems to occur more often ~(cause unknown)~ Cause: Update frequency high enough to spawn more than one bullet
