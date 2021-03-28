# apfel - Automated Partitioning of Factories for Efficient Layouts

I want to automatically provide assistance to players of [Factorio](https://factorio.com/) who design and build complex factories. The ultimate goal would be a program that, given a list of recipes, generates a blueprint of a factory. I am still very far away from that. Right now, this code does some useful things, but it is not at all usable. However, I have been [streaming](https://www.twitch.tv/suyjuris) some of its development, so I wanted to make the code public for reference.

## Structure

Brief high-level introduction:

* `construct.cpp` Contains code to partition lists of recipes (both the spatial partition and sparsest cut variants), as well as a simplified layout generation. Right now, my focus is here.
* `factorio.cpp`, `factorio_gui.cpp` A SAT instance to generate belt balancers, together with a GUI.
* `sat.cpp` My C++ API to generate SAT instances.
* other `*.cpp` files are my personal libraries, mostly re-used from previous projects
* `stb_truetype.h` [Sean Barretts](https://github.com/nothings/stb/) excellent font rasterisation and loading library. I wrote my own font rasteriser (in `font.cpp`) but still use it as a convenient `ttf` parser.
