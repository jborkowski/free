name := "free"

organization := "com.jobo"

version := "1.0"

scalaVersion := "2.12.3"

resolvers ++= (
  if (scalaVersion.value.contains("-bin"))
    List("scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/")
  else Nil
  )

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary)


libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.0-MF"
libraryDependencies += "com.chuusai" % "shapeless_2.12" % "2.3.2"
