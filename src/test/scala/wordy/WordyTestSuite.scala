package wordy

import org.scalatest.FunSuite

import fastparse.all._


class WordyTestSuite extends FunSuite {

  test("parse number") {
    assert(Wordy.number.parse("0") === Parsed.Success(Number(0f), 1))
    assert(Wordy.number.parse("99") === Parsed.Success(Number(99f), 2))
    assert(Wordy.number.parse("-0") === Parsed.Success(Number(0f), 2))
    assert(Wordy.number.parse("-99") === Parsed.Success(Number(-99f), 3))
  }

  test("parse operator") {
    assert(Wordy.addition.parse("plus") === Parsed.Success(Addition, 4))
    assert(Wordy.subtraction.parse("minus") === Parsed.Success(Subtraction, 5))

    assert(Wordy.division.parse("divided by") === Parsed.Success(Division, 10))
    assert(Wordy.multiplication.parse("multiplied by") === Parsed.Success(Multiplication, 13))

  }

  test("parse expression") {
    assert(Wordy.expression.parse("0") === Parsed.Success(Number(0f), 1))
    assert(Wordy.expression.parse("0 plus 1") === Parsed.Success(BinOpExpr(Number(0f), Addition, Number(1f)), 8))
    assert(Wordy.expression.parse("0 plus 1 plus 2") === Parsed.Success(BinOpExpr(BinOpExpr(Number(0f), Addition, Number(1f)), Addition, Number(2f)), 15))
    assert(Wordy.expression.parse("1 plus 2 multiplied by -3") === Parsed.Success(BinOpExpr(Number(1f), Addition, BinOpExpr(Number(2f), Multiplication, Number(-3f))), 25))
    assert(Wordy.expression.parse("1 plus 2 multiplied by -3 raised to the power of 2") === Parsed.Success(BinOpExpr(Number(1f), Addition, BinOpExpr(Number(2f), Multiplication, BinOpExpr(Number(-3f), Exponentiation, Number(2f)))), 50))
  }

  test("eval expression") {
    assert(Number(0f).eval() === 0f)
    assert(BinOpExpr(Number(0f), Addition, Number(1f)).eval() === 1f)
    assert(BinOpExpr(BinOpExpr(Number(0f), Addition, Number(1f)), Addition, Number(2f)).eval() === 3f)
    assert(BinOpExpr(Number(1f), Addition, BinOpExpr(Number(2f), Multiplication, Number(3f))).eval() === 7f)
  }

  test("answer") {
    assert(Wordy.answer("What is 0 plus 1?") === Right(1))
  }

}
