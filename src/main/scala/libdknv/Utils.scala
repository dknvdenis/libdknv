package libdknv

import spinal.core.{B, SInt, UInt}

object Utils {

  def ConvertToTwoComplement(value: UInt): SInt = ((B(1) ## ~value).asUInt + B(1).asUInt).asSInt

}
