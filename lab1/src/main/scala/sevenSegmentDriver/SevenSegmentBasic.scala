package sevenSegmentDriver

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

/** A seven segment display driver has essentially 7 + 1 outputs
  * @param w
  */

class SevenSegmentLedsBundle extends Bundle {
  val g = Bool()
  val f = Bool()
  val e = Bool()
  val d = Bool()
  val c = Bool()
  val b = Bool()
  val a = Bool()
}

class BCD2SevenSegmentLedsBundle extends Bundle {
  val leds = Output(new SevenSegmentLedsBundle)
  val bcd = Input(UInt(4.W))
}

class BCD2SevenSegmentMultiBundle(n: Int) extends BCD2SevenSegmentLedsBundle {
  override val bcd = Input(UInt((n * 4).W))
  val com = Output(UInt(n.W))
}

class BCD2SevenSegmentMultiSelBundle(n: Int)
    extends BCD2SevenSegmentMultiBundle(n) {
  val sel = Input(UInt(n.W))
}

class SevenSegmentBasic extends RawModule {
  val io = IO(new BCD2SevenSegmentLedsBundle)

  def commonAnode: Boolean = false

  val bcd2SevenSegmentTable = 0 to 9 map { i => new SevenSegmentDigit(i) } map {
    digit =>
      {
        if (commonAnode) digit.getCommonAnodePattern()
        else digit.getCommonCathodePattern()
      }
  }

  val vecTable = VecInit(bcd2SevenSegmentTable map { digit =>
    VecInit(digit.map(_.B))
  })

  io.leds := vecTable(io.bcd).asTypeOf(io.leds)
}

// Mixins
trait WithCommonAnode1 { self: SevenSegmentBasic =>
  io.leds := (~vecTable(io.bcd).asUInt).asTypeOf(io.leds)
}

trait WithCommonAnode2 { self: SevenSegmentBasic =>
  override def commonAnode: Boolean = true
}

class SevenSegmentMulti(n: Int) extends RawModule {
  val io = IO(new BCD2SevenSegmentMultiBundle(n))
  val sel = IO(Input(UInt(n.W)))

  val bcds = io.bcd.asTypeOf(Vec(n, UInt(4.W)))

  val ledsSeq = 0 until n map { i =>
    val display = Module(new SevenSegmentBasic)
    display.io.bcd := bcds(i)
    display.io.leds
  }
  val leds = ledsSeq zip sel.asBools map { case (leds, s) =>
    Fill(7, s).asUInt & leds.asUInt
  } reduce ((a, b) => a | b)
  io.leds := (~leds) asTypeOf io.leds
  io.com := ~sel
}

class NexysSevenSegmentDriver extends Module {
  val io = IO(new Bundle {
    val displayIO = new BCD2SevenSegmentMultiBundle(8)
    val sync = Input(Bool())
  })

  val display = Module(new SevenSegmentMulti(8))
  display.io <> io.displayIO

  val (cnt_value, _) = Counter(io.sync, 8)
  display.sel := 1.U(8.W) << cnt_value
}

object SevenSegmentMaker extends App {
  ChiselStage.emitSystemVerilogFile(
    new SevenSegmentBasic(),
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
  ChiselStage.emitSystemVerilogFile(
    new SevenSegmentBasic() with WithCommonAnode1 {
      override val desiredName = "SevenSegmentBasicWithCommonAnode1"
    },
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
  ChiselStage.emitSystemVerilogFile(
    new SevenSegmentBasic() with WithCommonAnode2 {
      override val desiredName = "SevenSegmentBasicWithCommonAnode2"
    },
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
  ChiselStage.emitSystemVerilogFile(
    new SevenSegmentMulti(4),
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
  ChiselStage.emitSystemVerilogFile(
    new NexysSevenSegmentDriver,
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
}
