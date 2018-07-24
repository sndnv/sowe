package owe.production

import owe.map.Cell.Fertility

final case class Commodity(name: String) extends AnyVal

object Commodity {
  final case class Amount(value: Int) extends AnyVal {
    def +(amount: Amount): Amount = Amount(value + amount.value)
    def -(amount: Amount): Amount = Amount(value - amount.value)
    def *(multiplier: Int): Amount = Amount(value * multiplier)
    def /(divisor: Int): Amount = Amount(value / divisor)
    def >(amount: Amount): Boolean = value > amount.value
    def <(amount: Amount): Boolean = value < amount.value
    def >=(amount: Amount): Boolean = value >= amount.value
    def <=(amount: Amount): Boolean = value <= amount.value
    def ==(amount: Amount): Boolean = value == amount.value
    def max(amount: Amount): Amount = Amount(math.max(value, amount.value))
    def min(amount: Amount): Amount = Amount(math.min(value, amount.value))

    def basedOn(fertility: Fertility): Amount = Amount((fertility.value * value) / 100)
  }

  final case class AmountModifier(value: Int) extends AnyVal {
    def +(modifier: AmountModifier) = AmountModifier(value + modifier.value)
    def -(modifier: AmountModifier) = AmountModifier(value - modifier.value)
    def *(multiplier: Int): AmountModifier = AmountModifier(value * multiplier)
    def /(divisor: Int): AmountModifier = AmountModifier(value / divisor)
    def apply(amount: Amount): Amount =
      Amount((amount.value * value) / 100)
  }

  sealed trait State
  object State {
    case object Produced extends State
    case object Used extends State
    case object Lost extends State
  }

  implicit class CommodityMap(map: Map[Commodity, Amount]) {
    def mergeWithLimits(
      otherMap: Map[Commodity, Amount],
      limits: Map[Commodity, Amount]
    ): Map[Commodity, Amount] =
      map ++ otherMap.map {
        case (commodity, amount) =>
          val limitAmount = limits.getOrElse(commodity, Commodity.Amount(0))
          val updatedCommodityAmount =
            (map.getOrElse(commodity, Commodity.Amount(0)) + amount)
              .min(limitAmount)
              .max(Commodity.Amount(0))

          (commodity, updatedCommodityAmount)
      }
  }
}
