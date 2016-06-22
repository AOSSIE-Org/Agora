name := "countvotes"

organization := ""

version := "1.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
	"com.github.scopt" %% "scopt" % "3.3.0"
	)
