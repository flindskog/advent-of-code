package aoc.syntax

import com.microsoft.z3.*

//noinspection NoTargetNameAnnotationForOperatorLikeDefinition
trait Z3Syntax {
  extension [T <: ArithSort](ae: ArithExpr[T]) {
    def <(that: ArithExpr[_])(using ctx: Context): BoolExpr     = ctx.mkLt(ae, that)
    def <=(that: ArithExpr[_])(using ctx: Context): BoolExpr    = ctx.mkLe(ae, that)
    def >(that: ArithExpr[_])(using ctx: Context): BoolExpr     = ctx.mkGt(ae, that)
    def >=(that: ArithExpr[_])(using ctx: Context): BoolExpr    = ctx.mkGe(ae, that)
    def +(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkAdd(ae, that)
    def -(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkSub(ae, that)
    def *(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkMul(ae, that)
    def /(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkDiv(ae, that)
    def unary_-(using ctx: Context): ArithExpr[T]               = ctx.mkUnaryMinus(ae)
  }

  extension (be: BoolExpr) {
    def &&(that: BoolExpr)(using ctx: Context): BoolExpr = ctx.mkAnd(be, that)
    def ||(that: BoolExpr)(using ctx: Context): BoolExpr = ctx.mkOr(be, that)
    def unary_!(using ctx: Context): BoolExpr            = ctx.mkNot(be)
  }

  extension (e: Expr[_]) {
    def ===(that: Expr[_])(using ctx: Context): BoolExpr = ctx.mkEq(e, that)
  }

  extension (l: Long) {
    def int(using ctx: Context): IntNum  = ctx.mkInt(l)
    def real(using ctx: Context): RatNum = ctx.mkReal(l)
  }

  extension (i: Int) {
    def int(using ctx: Context): IntNum  = ctx.mkInt(i)
    def real(using ctx: Context): RatNum = ctx.mkReal(i)
  }

  extension (s: String) {
    def intConstant(using ctx: Context): IntExpr   = ctx.mkIntConst(s)
    def realConstant(using ctx: Context): RealExpr = ctx.mkRealConst(s)
  }
}

trait Z3RealConversions {
  this: Z3Syntax =>
  implicit def intToRealExpr(i: Int)(using Context): RealExpr   = i.real
  implicit def longToRealExpr(l: Long)(using Context): RealExpr = l.real

  def mkConstant(name: String)(using Context): RealExpr         = name.realConstant
  def mkConstants(names: String*)(using Context): Seq[RealExpr] = names.map(_.realConstant)
}

trait Z3IntConversions {
  this: Z3Syntax =>
  implicit def intToIntExpr(i: Int)(using Context): IntExpr   = i.int
  implicit def longToIntExpr(l: Long)(using Context): IntExpr = l.int

  def mkConstant(name: String)(using Context): IntExpr         = name.intConstant
  def mkConstants(names: String*)(using Context): Seq[IntExpr] = names.map(_.intConstant)
}

object z3     extends Z3Syntax
object z3real extends Z3RealConversions with Z3Syntax
object z3int  extends Z3IntConversions with Z3Syntax
