package owe

package object production {
  final case class Commodity(name: String) extends AnyVal

  final case class CommodityAmount(value: Int) extends AnyVal {
    def +(amount: CommodityAmount): CommodityAmount = CommodityAmount(value + amount.value)
    def -(amount: CommodityAmount): CommodityAmount = CommodityAmount(value - amount.value)
    def *(multiplier: Int): CommodityAmount = CommodityAmount(value * multiplier)
    def /(divisor: Int): CommodityAmount = CommodityAmount(value / divisor)
    def >(amount: CommodityAmount): Boolean = value > amount.value
    def >=(amount: CommodityAmount): Boolean = value >= amount.value
    def <=(amount: CommodityAmount): Boolean = value >= amount.value
    def ==(amount: CommodityAmount): Boolean = value >= amount.value
    def <(amount: CommodityAmount): Boolean = value < amount.value
    def max(amount: CommodityAmount): CommodityAmount = CommodityAmount(math.max(value, amount.value))
    def min(amount: CommodityAmount): CommodityAmount = CommodityAmount(math.min(value, amount.value))

    def basedOn(fertility: Fertility): CommodityAmount = CommodityAmount((fertility.value * value) / 100)
  }

  final case class CommodityReplenishRate(value: Int) extends AnyVal {
    def *(multiplier: Int): CommodityAmount = CommodityAmount(value * multiplier)
    def /(divisor: Int): CommodityAmount = CommodityAmount(value / divisor)
  }

  final case class CommodityReplenishRateModifier(value: Int) extends AnyVal {
    def apply(amount: CommodityReplenishRate): CommodityReplenishRate =
      CommodityReplenishRate((amount.value * value) / 100)
  }

  final case class CommodityAmountModifier(value: Int) extends AnyVal {
    def +(modifier: CommodityAmountModifier) = CommodityAmountModifier(value + modifier.value)
    def *(multiplier: Int): CommodityAmountModifier = CommodityAmountModifier(value * multiplier)
    def /(divisor: Int): CommodityAmountModifier = CommodityAmountModifier(value / divisor)
    def apply(amount: CommodityAmount): CommodityAmount =
      CommodityAmount((amount.value * value) / 100)
  }

  sealed trait CommodityState
  object CommodityState {
    case object Produced extends CommodityState
    case object Used extends CommodityState
    case object Lost extends CommodityState
  }
}
