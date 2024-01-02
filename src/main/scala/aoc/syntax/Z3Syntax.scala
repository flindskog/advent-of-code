package aoc.syntax

import com.microsoft.z3.*

//noinspection NoTargetNameAnnotationForOperatorLikeDefinition
trait Z3Syntax {
  extension [T <: ArithSort](re: ArithExpr[T]) {
    def <(that: ArithExpr[_])(using ctx: Context): BoolExpr     = ctx.mkLt(re, that)
    def <=(that: ArithExpr[_])(using ctx: Context): BoolExpr    = ctx.mkLe(re, that)
    def >(that: ArithExpr[_])(using ctx: Context): BoolExpr     = ctx.mkGt(re, that)
    def >=(that: ArithExpr[_])(using ctx: Context): BoolExpr    = ctx.mkGe(re, that)
    def +(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkAdd(re, that)
    def -(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkSub(re, that)
    def *(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkMul(re, that)
    def /(that: ArithExpr[T])(using ctx: Context): ArithExpr[T] = ctx.mkDiv(re, that)
    def unary_-(using ctx: Context): ArithExpr[T]               = ctx.mkUnaryMinus(re)
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
    def int(using ctx: Context): IntExpr = ctx.mkInt(l)
    def real(using ctx: Context): RatNum = ctx.mkReal(l)
  }

  extension (i: Int) {
    def int(using ctx: Context): IntExpr = ctx.mkInt(i)
    def real(using ctx: Context): RatNum = ctx.mkReal(i)
  }

  extension (s: String) {
    def intConstant(using ctx: Context): IntExpr   = ctx.mkIntConst(s)
    def realConstant(using ctx: Context): RealExpr = ctx.mkRealConst(s)
  }
}

object z3 extends Z3Syntax
