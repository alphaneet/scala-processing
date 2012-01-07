resolvers += "github Maven Repository" at "https://github.com/alphaneet-debu/maven/raw/master"

libraryDependencies ++= Seq(
  "processing" %% "core" % "1.5.1",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"  
)
