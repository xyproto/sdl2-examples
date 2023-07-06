scalaVersion := "3.3.0"

enablePlugins(ScalaNativePlugin)

// needed to be able to set the build options below
import scala.scalanative.build._

// compilation options, with available options shown in the comments
nativeConfig ~= { c =>
  c.withLTO(LTO.thin) // none | full | thin
    .withMode(Mode.releaseFast) // debug | releaseFast | releaseSize | releaseFull
    .withGC(GC.immix) // none | boehm | immix | commix
    .withBuildTarget(BuildTarget.application) // application | libraryDynamic | libraryStatic
}

// nativeLinkingOptions += "-static"
