import org.nlogo.build.{ NetLogoExtension, ExtensionDocumentationPlugin }

enablePlugins(NetLogoExtension, ExtensionDocumentationPlugin)

name := "reflection"
version := "0.1.1-SNAPSHOT"
isSnapshot := true

scalaVersion := "2.12.16"
Compile / scalaSource := baseDirectory.value / "src" / "main"
Test / scalaSource := baseDirectory.value / "src" / "test"
scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-Xfatal-warnings", "-encoding", "UTF8", "-release", "11")

netLogoExtName := "reflection"
netLogoClassManager := "org.nlogo.extensions.reflection.NetLogoReflectionScala"
netLogoVersion := "6.3.0"
