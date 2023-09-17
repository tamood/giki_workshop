import sevenSegmentDriver.NexysSevenSegmentDriver

import chisel3._
import chisel3.util._
class NexysA7FPGAShell extends RawModule {
  val CLK100MHZ = IO(Input(Clock()))
  val CPU_RESETN = IO(Input(Bool()))

  val CA = IO(Output(Bool()))
  val CB = IO(Output(Bool()))
  val CC = IO(Output(Bool()))
  val CD = IO(Output(Bool()))
  val CE = IO(Output(Bool()))
  val CF = IO(Output(Bool()))
  val CG = IO(Output(Bool()))
  val AN = IO(Output(UInt(8.W)))

  val freq = 100 * 1000000

  withClockAndReset(CLK100MHZ, (~CPU_RESETN).asBool){
    val driver = Module(new NexysSevenSegmentDriver)
    AN := driver.io.displayIO.com
    Seq(CA, CB, CC, CD, CE, CF, CG) zip driver.io.displayIO.leds.asUInt.asBools foreach {case (o, i)=> o := i}

    //BCD counters
    val bumps = Seq.fill(8)(Wire(Bool()))
    val bcd_counters = bumps map { Counter(_, 10) }
    1 to 7 foreach {i=> bumps(i) := bcd_counters(i - 1)._2}
    driver.io.displayIO.bcd := VecInit(bcd_counters map {case (value, wrap) => value}).asUInt
    bumps(0) := true.B

    //Second counter
    val sec_cnt = Counter(freq)
    bumps(0) := sec_cnt.inc()

    //25Hz counter
    val cnt_25 = Counter(freq/25)
    driver.io.sync := cnt_25.inc()
  }
}

object FPGAMaker extends App {
  emitVerilog(new NexysA7FPGAShell, Array("--target-dir", "generated"))
}
