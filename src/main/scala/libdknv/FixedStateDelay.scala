package libdknv

import spinal.core._
import spinal.lib.fsm._
import scala.math._, BigDecimal._

class FixedStateDelay(cyclesCount: UInt)(implicit stateMachineAccessor: StateMachineAccessor) extends State with StateCompletionTrait {

  /** Create a StateDelay with an TimeNumber */
  def this(time: TimeNumber, addCycles: Int = 0)(implicit stateMachineAccessor: StateMachineAccessor){
    this((time * ClockDomain.current.samplingRate.getValue).setScale(0, RoundingMode.CEILING).toBigInt + addCycles)
  }

  val cache = stateMachineAccessor.cacheGetOrElseUpdate(StateMachineSharableUIntKey, new StateMachineSharableRegUInt).asInstanceOf[StateMachineSharableRegUInt]
  cache.addMinWidth(cyclesCount.getWidth)

  val isCompleted = (cache.value <= 1)

  onEntry{
    cache.value := cyclesCount
  }

  whenIsActive{
    cache.value := cache.value - 1
    when(isCompleted) {
      doWhenCompletedTasks()
    }
  }
}