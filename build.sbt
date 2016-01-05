name := "starfigher"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  Seq(
    "com.github.nscala-time"      %% "nscala-time"      % "2.6.0",
    "net.databinder.dispatch"     %% "dispatch-core"    % "0.11.2",
    "com.typesafe"                %  "config"           % "1.3.0",
    "io.spray"                    %%  "spray-json"      % "1.3.2",
    "org.scalatest"               %% "scalatest"        % "2.2.4"   % "test",
    "com.github.tomakehurst"      %  "wiremock"         % "1.33"    % "test"
  )
}
