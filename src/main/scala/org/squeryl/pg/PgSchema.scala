package org.squeryl.pg

import org.squeryl._
import internals.{StatementWriter, FieldMapper}
import dsl.ast.{ViewExpressionNode, ExpressionNode}


class SrfView[T](
                  name: String,
                  classOfT: Class[T],
                  schema: Schema,
                  prefix: Option[String],
                  args: Iterable[ExpressionNode])
  extends View[T](
    name,
    classOfT,
    schema,
    prefix,
    None) {
  override def viewExpressionNode: ViewExpressionNode[T] = new SrfViewExpressionNode[T](this, args)
}

class SrfViewExpressionNode[T](view: View[T], args: Iterable[ExpressionNode]) extends ViewExpressionNode(view) {
  override def doWrite(sw: StatementWriter) = {
    sw.write(sw.quoteName(view.prefixedName))
    sw.write("(")
    sw.writeNodesWithSeparator(args, ",", false)
    sw.write(")")
  }
}
