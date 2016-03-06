import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseTransformerFactory
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import sbt.ProjectRef
import sbt.State
import sbt.ThisBuild
import scala.xml._
import scala.xml.transform.RewriteRule
import scalaz.Validation
import scalaz.syntax.validation._

object SbtEclipseForBuild {

  def settings = Seq(
    EclipseKeys.classpathTransformerFactories := Seq( RootSourceDirectory ),
    EclipseKeys.withBundledScalaContainers := false
  )

  object RootSourceDirectory extends EclipseTransformerFactory[RewriteRule] {
    override def createTransformer( ref : ProjectRef, state : State ) = RootSourceDirectoryRule.success

    override def toString : String = "RootSourceDirectory (EclipseTransfomerFactory)"
  }

  object RootSourceDirectoryRule extends RewriteRule {
    private val Cp = "classpath"
    private val CpEntry = "classpathentry"
    private val PathAttr = "path"

    override def transform( node : Node ) : Seq[Node] = node match {
      case Elem( pf, Cp, attrs, scope, child @ _* ) =>
        Elem( pf, Cp, attrs, scope, true, srcRoot( pf, scope ) ++ child : _* )
      case other => other
    }

    private def srcRoot( pf : String, scope : NamespaceBinding ) : Node = {
      val attribs = Attribute( "kind", Text( "src" ),
        Attribute( "path", Text( "" ),
          Attribute( "excluding", Text( ".settings/|bin/|project/|target/" ), Null ) ) )

      Elem( pf, CpEntry, attribs, scope, true )
    }

  }

}
