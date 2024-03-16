# Republic

## Goal
In Plato's work "The Republic", the Greek philosopher outlines his view for the ideal society. His view contains many things we would consider very dystopian, but nonetheless it is an interesting
experiment to think about how effective it would really be to other forms of government, and also importantly, whether or not it is sustainable. This project aims to create a simulation of this society
in action to see how it would play out in different scenarios.

## Technology stack
I chose Haskell for this as I really felt like learning it and a functional language seems like a good choice for a system that effectively involves applying a function `f(t + 1, f(t, s_{t-1})) = s_{t+1}` where `t` is the current time, `s_i` is the state at time `i` over and over again.

## Design
This design will evolve over time and the scope is honestly infinite with a project like this. I would like to have concrete milestones so I can move forward with a goal in mind.

### Milestone 1. Society instantiation
Here we need to be able to create the society in its initial state. This involves creating the starting population made up of the three classes:
1. The Guardians (gold)
2. The Auxilaries (silver)
3. The Workers (iron)

Each citizen will have a set of stats that places them in a particular class. For now the stats will be simplified to just specify the metal, and for each worker, their chosen profession.

The number of citizens in each class will be determined by percentages, in fact the starting proportions will likely just be hard coded to start.

For example:
- Total size: 1000
- 100 Guardians
- 200 Auxilaries
- 700 Workers
    - Evenly split across all professions

Starting professions will be
- Farmer
- Baker
- Miner
- Blacksmith

We also need a change-of-state function I will henceforth just refer to as `f(t, s)` which will be responsible for moving the republic through time. For now it will just update the age of each citizen. A starting timestep can be week but I would like the program to work with customizable timesteps.


### Milestone 2. Setup basic internal economy
Each working citizen will produce some resource by consuming some resource (Plato's Republic does not use currency which simplfies things). This could be modelled as a graph where each node represents a worker, input edges represent the amount of resources necessary to flow in and output edges representing the output resource.
For example, a farmer would produce wheat which a baker would use to produce bread, which would be used to feed everyone. To keep things simple we will just have wheat and bread as the unit for food. Water will be unaccounted for the moment. The resources for this milestone will be
- Wheat
- Bread
- Iron
- Tools

But the goal is to have a very generic system that will allow creating a infinitely complex resource tree. It just involves constructing a graph with the difficult bit being balancing the input/output values.


### Milestone 3. Procreation and promonting/demoting


### Milestone 4. War with other cities
