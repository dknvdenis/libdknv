package libdknv.boards

import java.nio.file._
import spinal.core._

object QMTECH_XC7A100T {
  def generate[T <: Component](gen: => T) {
    val targetDirectory = Paths.get("generated")
    if (!Files.exists(targetDirectory)) {
      Files.createDirectory(targetDirectory)
    }
    new SpinalConfig(
      defaultClockDomainFrequency = FixedFrequency(50 MHz),
      defaultConfigForClockDomains = ClockDomainConfig(
        resetKind = ASYNC,
        resetActiveLevel = LOW
      ),
      targetDirectory = targetDirectory.toString()
    ).generateVerilog(gen)
  }
}
