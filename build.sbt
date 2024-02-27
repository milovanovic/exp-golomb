scalaVersion := "2.13.10"
scalacOptions += "-language:higherKinds"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

scalacOptions += "-Ydelambdafy:inline"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
//  "-Xfatal-warnings",
  "-language:reflectiveCalls",
  "-Ymacro-annotations"
)
val chiselVersion = "3.6.0"
addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "edu.berkeley.cs" %% "chiseltest" % "0.6.2"
)

Test / parallelExecution := false