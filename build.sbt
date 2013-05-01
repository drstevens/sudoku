Revolver.settings

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.4",
  "org.slf4j" % "slf4j-api" % "1.6.6",
  "ch.qos.logback" % "logback-classic" % "1.0.6",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.specs2" %% "specs2" % "1.14" % "test"
)
 
initialCommands in console := "import scalaz._, Scalaz._"
