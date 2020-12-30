# TODO

## How should the rules be stored?

Map:
```erlang
#{"white" => [{"yellow", 2}, {"blue", 3}],
  "yellow" => [{"blue", 1}],
  "blue" => []
}

#{"white" => #{"yellow" => 2, "blue" => 3},
  "yellow" => #{"blue" => 1},
  "blue" => #{}
}
```

## How do we find which bags that can hold shiny gold bags?

Base case 1: The bag can't hold any bags, return false;
Base case 2: The bag has already been searched, return false.

The bag can hold the sought bag, return true;
Else, check if the bags it can hold can hold it.

## Part 2

Count the total number of bags a shiny gold bag can hold.

Base case: Bag can hold 0 bags -> return 1
Else: Return the label on an edge times that vertex return value

gold -> 1 olive, 2 plum
    olive -> 3 blue, 4 black
    plum -> 5 blue, 6 black
        blue -> 0
        black -> 0

        blue returns 1
        black returns 1
    olive returns 3x1 + 4x1 = 7
    plum returns 5x1 + 6x1 = 11
gold returns 1 + 1x7 + 2 + 2x11 = 32

Need to keep track of the multiplier.
