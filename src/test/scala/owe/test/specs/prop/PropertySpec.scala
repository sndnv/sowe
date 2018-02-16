package owe.test.specs.prop

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, fixture}

abstract class PropertySpec
    extends fixture.FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
