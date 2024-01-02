package kuplrg

import org.scalatest.flatspec.AnyFlatSpec
import scala.Console.*
import scala.quoted.*
import java.rmi.UnexpectedException

trait SpecBase extends AnyFlatSpec {
  import SpecBase.*

  def check[T](
    exprCode: String,
    answerCode: String,
    result: => T,
    expected: => T,
  ): Unit = exprCode should "be " ++ answerCode in {
    info(s"""$exprCode should be $answerCode""")
    checkNoImpl(result)
    if (result != expected) failed(s"$result != $expected")
    else passed
  }

  inline def test[T](inline expr: T, inline answer: T): Unit =
    ${ testImpl('expr, 'answer, 'check) }

  def checkExc[T](
    exprCode: String,
    result: => T,
    errMsg: => String,
  ): Unit = exprCode should "throw an error with \"" ++ errMsg + '\"' in {
    info(s"""$exprCode should throw an error with a message "$errMsg"""")
    checkNoImpl(result)
    val msg = try
      Some(s"""It must throw an error but returns $result without errors""")
    catch
      case err: PLError =>
        val msg = err.msg
        if (!err.msg.contains(errMsg))
          Some(s"""Error message should contain "$errMsg" but got "$msg"""")
        else None
      case err: Throwable => Some(err.toString)
    msg.fold(passed)(failed(_))
  }

  inline def testExc[T](inline expr: T, inline errMsg: String): Unit =
    ${ testExcImpl('expr, 'errMsg, 'checkExc) }

  private def checkNoImpl[T](result: => T): Unit = try result catch
    case err: NotImplementedError =>
      val elem = err.getStackTrace.apply(1)
      val line = elem.getLineNumber
      val methodName = elem.getMethodName
      failed(s"Please implement `$methodName` (Implementaion.scala:$line).")
    case _: Throwable =>

  private def info(msg: String): Unit =
    printColor(CYAN)(s"[test] ")
    print(msg)

  private def failed(err: Throwable): Nothing =
    failed(s"[ERROR] $err", Some(err))
  private def failed(msg: String, err: Option[Throwable] = None): Nothing =
    printlnColor(RED)(" - FAIL")
    printlnColor(RED)(s"       $msg")
    for {
      e <- err
      elem <- e.getStackTrace.take(5).toVector :+ "..."
    } printlnColor(RED)(s"       $elem")
    fail(msg)

  private def passed: Unit = printlnColor(GREEN)(" - PASS")
}

private object SpecBase:
  def testImpl[T](
    expr: Expr[T],
    answer: Expr[T],
    check: Expr[(String, String, => T, => T) => Unit],
  )(using Type[T], Quotes): Expr[Unit] =
    val exprCode = normCode(expr)
    val answerCode = normCode(answer)
    '{ $check($exprCode, $answerCode, $expr, $answer) }

  def testExcImpl[T](
    expr: Expr[T],
    errMsg: Expr[String],
    checkExc: Expr[(String, => T, => String) => Unit],
  )(using Type[T], Quotes): Expr[Unit] = '{
    $checkExc(${ normCode(expr) }, $expr, $errMsg)
  }

  val len = 70

  def normCode[T](expr: Expr[T])(using Quotes): Expr[String] =
    Expr(norm(expr.show))

  def norm(expr: Any): String =
    val str = expr.toString
      .replaceAll("this.", "")
      .replaceAll("kuplrg.", "")
      .replaceAll("scala.", "")
      .replaceAll("Spec.", "")
      .replaceAll("Implementation.", "")
      .replaceAll(".apply", "")
    if (str.length <= len) str
    else s"${str.substring(0, len)} ..."

  /** show colored message */
  def setColor(color: String): Any => String =
    if (color == "") x => x.toString else x => color + x.toString + RESET
  def printColor(color: String): Any => Unit = x => print(setColor(color)(x))
  def printlnColor(color: String): Any => Unit = x =>
    println(setColor(color)(x))
