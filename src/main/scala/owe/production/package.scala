package owe

package object production {
  final case class Commodity(name: String) extends AnyVal

  final case class CommodityAmount(value: Int) extends AnyVal {
    def +(amount: CommodityAmount): CommodityAmount = CommodityAmount(value + amount.value)
    def -(amount: CommodityAmount): CommodityAmount = CommodityAmount(value - amount.value)
  }

  final case class CommodityUsageRate(value: Int) extends AnyVal

  sealed trait CommodityState
  object CommodityState {
    case object Produced extends CommodityState
    case object Used extends CommodityState
    case object Lost extends CommodityState
  }
}
