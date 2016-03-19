#Fantasy Weapons
A procedural generation project by Max Goldstein

## Run it Yourself
First, [install Elm](http://elm-lang.org/install). Second, clone the repo, `cd` into it, and `elm package install
--yes`. Then run `elm reactor`. Then open your browser to localhost port 8000 and click `Main.elm`. Space to advance.

On randomness: the PRNG is deterministic, but uses a seed every frame to determine if it should spawn a particle. So
after the first one, all weapons are effectively unique, never to be seen again.

## Code
* `Names.elm` generates the name of the weapon, and the typeface.
* `Wand.elm` generates the wand. Currently a bit of a mess.
* `Particle.elm` defines both individual particles and the particle system that describe the conditions for making particles.
* `Main.elm` puts it all together.
