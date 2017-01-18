scalaVersion in ThisBuild := "2.11.8"
version in ThisBuild := "0.0.0-SNAPSHOT"
ensimeIgnoreMissingDirectories in ThisBuild := true

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

// to be shared... %%%
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "scalatags" % "0.6.1",
  "com.lihaoyi" %% "upickle"   % "0.4.3",
  "com.lihaoyi" %% "pprint"    % "0.4.3",
  "com.chuusai" %% "shapeless" % "2.3.1"
)

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick"     % "3.2.0-M2",
  //"com.typesafe.slick" %% "slick-hikaricp" % "3.1.1", // "connection pool", will find out.
  //"io.underscore"      %% "slickless" % "0.3.0", // 0.3.1 for 2.12. - MAY NOT WORK W/ slick 3.2!
  "com.h2database" % "h2" % "1.4.187",
  
  "com.typesafe.akka"  %% "akka-http" % "10.0.1",
  
  "org.slf4j" % "slf4j-simple" % "1.7.22"
)