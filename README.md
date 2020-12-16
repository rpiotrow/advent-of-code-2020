# Solutions for Advent of Code 2020 puzzles

Puzzles available at https://adventofcode.com/2020/

## Run

Run all:
```
$ sbt run
```

Run selected day:
```
$ sbt run -d <day_number>
```

## Tests

Run unit tests:
```
$ sbt test
```

Run regression tests (check solution answers):
```
$ sbt regression:test
```

## Used libraries
 * [zio](https://zio.dev/)
 * [zio-streams](https://zio.dev/docs/datatypes/datatypes_stream)
 * [cats](https://typelevel.org/cats/)
 * [scopt](https://github.com/scopt/scopt)
