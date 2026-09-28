package FloatingPoint
import chisel3._

trait FloatingPointFormat {
  def exponent: Int
  def mantissa: Int
  def bw: Int = exponent + mantissa + 1
  def max_exp = (BigInt(2).pow(exponent) - 2)
  def min_exp = 1
  def max_frac = (BigInt(2).pow(mantissa) - 1)
  def min_frac = 0
  def bias = (BigInt(2).pow(exponent - 1) - 1)
}

case object FP16 extends FloatingPointFormat {
  val exponent = 5
  val mantissa = 10
}

case object BF16 extends FloatingPointFormat {
  val exponent = 8
  val mantissa = 7
}

case object FP32 extends FloatingPointFormat {
  val exponent = 8
  val mantissa = 23
}

case object FP64 extends FloatingPointFormat {
  val exponent = 11
  val mantissa = 52
}

case object FP128 extends FloatingPointFormat {
  val exponent = 15
  val mantissa = 112
}

case class CustomFormat(exponent: Int, mantissa: Int) extends FloatingPointFormat


class FPModule(FORMAT: FloatingPointFormat) extends Module {
  val bw = FORMAT.bw
  val exponent = FORMAT.exponent
  val mantissa = FORMAT.mantissa
  val bias = FORMAT.bias.U(exponent.W)
  val max_exp = FORMAT.max_exp.U(exponent.W)
  val min_exp = FORMAT.min_exp.U(exponent.W)
  val max_frac = FORMAT.max_frac.U(mantissa.W)
  val min_frac = FORMAT.min_frac.U(mantissa.W)
}