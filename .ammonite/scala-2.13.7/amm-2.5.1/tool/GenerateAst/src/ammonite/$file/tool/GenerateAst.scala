
package ammonite
package $file.tool
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}


object GenerateAst{
/*<script>*/import java.io.PrintWriter

@main
def generateAst(dir: String) = ???

def defineAst(
  outDir: String,
  baseName: String,
  className: String,
  types: List[String]
) = {
  val path = s"$outDir/$baseName.scala"
  val writer = new PrintWriter(path, "UTF-8")

  writer.println(s"trait $baseName {")

  types.foreach { t =>
    val clName = t.split(":")(0).trim()
    val fields = t.split(":")(0).trim()

    defineType(writer, baseName, className, fields)
  }

  writer.println("}")
  writer.close()
}

def defineType(
  writer: PrintWriter,
  baseName: String,
  className: String,
  fieldList: String
) = {
  val fields = fieldList.split(", ")
  writer.println(s"  final case class $className() extends $baseName {")
}/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "GenerateAst"
  /*</generated>*/
}
