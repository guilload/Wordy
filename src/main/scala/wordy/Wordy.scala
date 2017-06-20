package wordy

import fastparse.all._


sealed trait Expression {
  def eval(): Double
}

case class Number(value: Double) extends Expression {
  def eval(): Double = value
}


case class BinOpExpr(lhs: Expression, operator: Operator, rhs: Expression) extends Expression {

  def eval(): Double = {

    type BinOp = (Double, Double) => Double

    val f: BinOp = operator match {
      case Addition => _ + _
      case Exponentiation => math.pow
      case Division => _ / _
      case Multiplication => _ * _
      case Subtraction => _ - _
    }

    f(lhs.eval(), rhs.eval())
  }

}


sealed trait Operator

case object Addition extends Operator
case object Division extends Operator
case object Exponentiation extends Operator
case object Multiplication extends Operator
case object Subtraction extends Operator


object Wordy {

  val ws = P( " ".rep(1) )

  val number: Parser[Number] = P( "-".? ~ CharIn('0' to '9').rep(1)).!.map(digits => Number(digits.toDouble))

  val addition: Parser[Addition.type] = P( "plus" ).!.map(_ => Addition)
  val subtraction: Parser[Subtraction.type] = P( "minus" ).!.map(_ => Subtraction)
  val addSub: Parser[Operator] = P ( addition | subtraction )

  val expression: Parser[Expression] = P ( factor ~ P ( ws ~ addSub ~ ws ~ factor ).rep() )
    .map { case (lhs, rhs) => rhs.foldLeft(lhs: Expression){ case (acc, (op, expr)) => BinOpExpr(acc, op, expr) } }

  val division: Parser[Division.type] = P( "divided by" ).!.map(_ => Division)
  val multiplication: Parser[Multiplication.type] = P( "multiplied by" ).!.map(_ => Multiplication)
  val divMul: Parser[Operator] = P ( division | multiplication )

  val factor: Parser[Expression] = P ( power ~ P ( ws ~ divMul ~ ws ~ power ).rep() )
    .map { case (lhs, rhs) => rhs.foldLeft(lhs: Expression){ case (acc, (op, expr)) => BinOpExpr(acc, op, expr) } }

  val exponentiation: Parser[Exponentiation.type] = P( "raised to the power of" ).!.map(_ => Exponentiation)

  val power: Parser[Expression] = P ( number ~ P ( ws ~ exponentiation ~ ws ~ number).rep() )
    .map { case (lhs, rhs) => rhs.foldLeft(lhs: Expression){ case (acc, (op, expr)) => BinOpExpr(acc, op, expr) } }

  val qMark: Parser[Unit] = P( "?" ~ End )
  val whatIs: Parser[Unit] = P( "What is")

  val parser: Parser[Expression] = P( whatIs ~ ws ~ expression ~ qMark)

  def answer(command: String): Either[Unit, Double] = {
    parser.parse(command) match {
      case Parsed.Success(expr, _) => Right(expr.eval())
      case _ => Left(())
    }
  }

}
