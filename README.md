# Haskell SmartRockets

A remake of [this project](https://github.com/Archaversine/Neuroevolution-Missiles),
which was based off of [TheCodingTrain's p5js project](https://thecodingtrain.com/challenges/29-smart-rockets-in-p5js).

## Genetic Algorithm Parameters

The following parameters are used for the genetic algorithm:

| Parameter         | Value        |
|-------------------|--------------|
| Population Size   | 100          |
| Lifespan (Frames) | 500          |
| Crossover Rate    | 80%          |
| Crossover Type    | Single Point |
| Mutation Rate     | 1%           |
| Elitism           | Top 1        |

## Graphics

This project was implemented using [Gloss](http://gloss.ouroborus.net/). The 
graphics are simple, but convey the point, here is a video of what the 
genetic algorithm looks like in action:

![Download .mp4 here](https://raw.githubusercontent.com/Archaversine/HaskellSmartRockets/main/haskell-smartrockets.mp4)

### Screenshot

![Screenshot](https://raw.githubusercontent.com/Archaversine/HaskellSmartRockets/main/screenshot.png)
