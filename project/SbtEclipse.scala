import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseCreateSrc
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseProjectFlavor
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseTransformerFactory
import sbt.ProjectRef
import sbt.State
import sbt.ThisBuild
import scala.xml._
import scala.xml.transform.RewriteRule
import scalaz.Validation
import scalaz.syntax.validation._

object SbtEclipse {

  def buildSettings = Seq(
    EclipseKeys.projectFlavor := EclipseProjectFlavor.ScalaIDE,
    EclipseKeys.classpathTransformerFactories in ThisBuild := Seq(OptionalSources),
    EclipseKeys.withSource := true,
    EclipseKeys.withJavadoc := true
  )

  object OptionalSources extends EclipseTransformerFactory[RewriteRule] {
    override def createTransformer(ref: ProjectRef, state: State) = OptionalSourcesRule.success
  }

  object OptionalSourcesRule extends RewriteRule {
    private val CpEntry = "classpathentry"

    override def transform(node: Node): Seq[Node] = node match {
      case Elem(pf, CpEntry, attrs, scope, child @ _*) if isSrc(attrs) =>
        Elem(pf, CpEntry, attrs, scope, true, child ++ attributes(attrs, pf, scope): _*)
      case other => other
    }

    private def isSrc(attrs: MetaData) = attrs("kind") == Text("src")

    private def attributes(attrs: MetaData, pf: String, scope: NamespaceBinding) = {
      val children =
        if (isProtobuf(attrs))
          ignoreProtobufWarnings(pf, scope) :: optional(pf, scope) :: Nil
        else
          optional(pf, scope) :: Nil

      Elem(pf, "attributes", Null, scope, true, children: _*)
    }

    private def optional(pf: String, scope: NamespaceBinding) =
      attribute("optional", "true", pf, scope)

    private def isProtobuf(attrs: MetaData) = attrs("path") match {
      case Text(data) => data.endsWith("compiled_protobuf")
    }

    private def ignoreProtobufWarnings(pf: String, scope: NamespaceBinding) =
      attribute("ignore_optional_problems", "true", pf, scope)

    private def attribute(name: String, value: String, pf: String, scope: NamespaceBinding) =
      Elem(pf, "attribute", Attribute("name", Text(name), Attribute("value", Text(value), Null)), scope, true)

  }

}
