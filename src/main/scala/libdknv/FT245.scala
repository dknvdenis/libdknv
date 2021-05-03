package libdknv

import libdknv._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.{InOutWrapper, TriStateArray}

case class FT245Bus() extends Bundle with IMasterSlave {
  val data = TriStateArray(8 bits)
  val rd_n = Bool
  val wr_n = Bool
  val rxf_n = Bool
  val txe_n = Bool
  val pwren_n = Bool
  val rst_n = Bool

  override def asMaster() = {
    out(rd_n, wr_n, rst_n)
    in(rxf_n, txe_n, pwren_n)
    master(data)
  }
}

class FT245 extends Component {
  val io = new Bundle {
    val bus = master(FT245Bus())

    val write = slave Stream(UInt(8 bits))
    val read = master Stream(UInt(8 bits))
  }

  io.bus.rst_n := RegNext(ClockDomain.current.readResetWire)

  // ------------------------------

  val readReq = RegInit(False)
  val writeReq = RegInit(False)

  io.bus.rd_n := ~readReq
  io.bus.wr_n := writeReq

  readReq := False
  writeReq := False

  // ------------------------------

  val writeData = Reg(UInt(8 bits)) init(0)
  val busInWriteMode = RegInit(False)

  io.bus.data.write := writeData.asBits
  io.bus.data.writeEnable.setAllTo(busInWriteMode)

  busInWriteMode := False

  // ------------------------------

  val writeStreamReady = RegInit(False)
  io.write.ready := writeStreamReady

  writeStreamReady := False

  // ------------------------------

  val readStreamPayload = Reg(UInt(8 bits))
  val readStreamValid = RegInit(False)

  io.read.valid := readStreamValid
  io.read.payload := readStreamPayload

  // ------------------------------

  // Commit read transaction
  when(readStreamValid && io.read.ready) {
    readStreamValid := False
  }

  // ------------------------------

  val rxFifoValid_n = Delay(io.bus.rxf_n, 2)
  val txFifoValid_n = Delay(io.bus.txe_n, 2)
  val pwrEn_n = Delay(io.bus.pwren_n, 2)

  // ------------------------------

  val fsm = new StateMachine {
    val sIdle = new State with EntryPoint

    val sRead1 = new FixedStateDelay(time = 100 ns)
    val sRead2 = new FixedStateDelay(time = 100 ns)

    val sWrite1 = new FixedStateDelay(time = 50 ns)
    val sWrite2 = new FixedStateDelay(time = 50 ns)
    val sWrite3 = new FixedStateDelay(time = 100 ns)

    val readPriority = RegInit(True)

    def gotoWrite(): Unit = {
      writeData := io.write.payload
      writeStreamReady := True
      goto(sWrite1)
    }

    sIdle
      .whenIsActive {
        val needRead = ~pwrEn_n && ~readStreamValid && ~rxFifoValid_n
        val needWrite = ~pwrEn_n && io.write.valid && ~txFifoValid_n

        when(readPriority) {
          when(needRead) {
            goto(sRead1)
          } elsewhen(needWrite) {
            gotoWrite()
          }
        } otherwise {
          when(needWrite) {
            gotoWrite()
          } elsewhen(needRead) {
            goto(sRead1)
          }
        }

        when(needRead || needWrite) {
          readPriority := ~readPriority
        }
      }

//    val cf = CounterFreeRun(256)

    sRead1
      .whenIsActive {
        readReq := True
      }
      .whenCompleted {
        readStreamPayload := io.bus.data.read.asUInt
//        readStreamPayload := cf.value
        readStreamValid := True
        goto(sRead2)
      }

    sRead2
      .whenCompleted {
        goto(sIdle)
      }

    sWrite1
      .whenIsActive {
        busInWriteMode := True
        writeReq := True
      }
      .whenCompleted {
        goto(sWrite2)
      }

    sWrite2
      .whenIsActive {
        busInWriteMode := True
      }
      .whenCompleted {
        goto(sWrite3)
      }

    sWrite3
      .whenCompleted {
        goto(sIdle)
      }
  }
}

object FT245 {
  def main(args: Array[String]) = boards.QMTECH_XC7A100T.generate(new FT245())
}
