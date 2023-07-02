# Scala

Build and run with:

    sbt run

Depends on sbt, scala native, java and SDL2.

When upgrading the scala stdlib version and the scala native version, view the changelog for the latest version here:

https://scala-native.org/en/stable/

Then, in the table of `Scala binary version` and `Scala release`, use the ones at the bottom row to update `build.sbt` and `project/plugins.sbt`.

Then see if it builds by running `sbt run`.
