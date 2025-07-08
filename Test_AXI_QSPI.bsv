package Test_AXI_QSPI;

import AXI_QSPI::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;

(* synthesize *)
module mkTest_AXI_QSPI(Empty);

  // Instantiate DUT
  Ifc_AXI_QSPI qspi <- mkAXI_QSPI;

  // FIFOs to drive the AXI inputs
  FIFO#(AXI4_Lite_Write_Addr) awfifo <- mkFIFO();
  FIFO#(AXI4_Lite_Write_Data) wfifo <- mkFIFO();
  FIFO#(AXI4_Lite_Read_Addr) arfifo <- mkFIFO();
  FIFOF#(AXI4_Lite_Resp) bfifo <- mkFIFOF();
  FIFOF#(AXI4_Lite_Read_Data) rfifo <- mkFIFOF();

  // Connect testbench FIFOs to DUT AXI interface
  mkConnection(toPut(awfifo), qspi.axi.awvalid);
  mkConnection(toPut(wfifo), qspi.axi.wvalid);
  mkConnection(qspi.axi.b,   toGet(bfifo));
  mkConnection(toPut(arfifo), qspi.axi.arvalid);
  mkConnection(qspi.axi.r,   toGet(rfifo));

  Reg#(Bit#(32)) write_data <- mkReg(32'hA5A5A5A5);
  Reg#(Bit#(8))  data_count <- mkReg(0);
  Reg#(Bit#(4))  phase      <- mkReg(0);
  Reg#(Bool)     started    <- mkReg(False);
  Reg#(Bool)     done       <- mkReg(False);

  // ————————————
  // Waveform dump rule (legal Bluespec syntax)
  rule dump_once (True);
    $dumpfile("dump.vcd");
    $dumpvars;    // no arguments in BSV
    $dumpon;
    noAction;
  endrule

  // Mode‐string helper
  function String getModeString(Bit#(2) mode);
    case (mode)
      2'b00: return "Single SPI";
      2'b01: return "Dual SPI";
      2'b10: return "Quad SPI";
      2'b11: return "Quad SPI DDR";
      default: return "Unknown";
    endcase
  endfunction

  // Perform an AXI write via FIFOs
  function Action do_write(Bit#(32) addr, Bit#(32) data);
    return action
      awfifo.enq(AXI4_Lite_Write_Addr{awaddr: addr});
      wfifo.enq(AXI4_Lite_Write_Data{wdata: data});
    endaction;
  endfunction

  // ————————————
  // Single‐shot startup for Quad SPI
  rule start_once (!started);
    $display("\n[Cycle %0t] Begin Mode Quad SPI (%s)", $time, getModeString(2'b10));
    started <= True;
    phase   <= 1;
  endrule

  rule phase1 (phase == 1);
    do_write(32'h04, 32'h6B);  // Quad read opcode
    $display("[Cycle %0t] Opcode written: 6B", $time);
    phase <= 2;
  endrule

  rule phase2 (phase == 2);
    do_write(32'h08, 32'h00A00000);
    $display("[Cycle %0t] Address: 0x00A00000", $time);
    phase <= 3;
  endrule

  rule phase3 (phase == 3);
    do_write(32'h0C, 32'd4);
    $display("[Cycle %0t] Bytes to write: 4", $time);
    phase <= 4;
  endrule

  rule phase4 (phase == 4);
    do_write(32'h10, 32'd2);
    $display("[Cycle %0t] Dummy cycles: 2", $time);
    phase <= 5;
  endrule

  rule phase5 (phase == 5);
    if (data_count < 4) begin
      do_write(32'h14, write_data);
      $display("[Cycle %0t] Writing data[%0d]: %h", $time, data_count, write_data);
      data_count <= data_count + 1;
    end else begin
      data_count <= 0;
      phase <= 6;
    end
  endrule

  rule phase6 (phase == 6);
    Bit#(32) ctrl = 32'b0;
    ctrl[0] = 1;                 // enable
    ctrl[1] = 1;                 // start
    ctrl[3:2] = 2'b10;           // Quad mode
    do_write(32'h00, ctrl);
    $display("[Cycle %0t] Transaction Triggered", $time);
    phase <= 7;
  endrule

  rule read_status (phase == 7);
    if (rfifo.notEmpty) begin
      let resp = rfifo.first;
      rfifo.deq;
      $display("[Cycle %0t] Read Data: %h", $time, resp.rdata);
      done  <= True;
      phase <= 8;
    end
  endrule

  rule finish (done);
    $display("[Cycle %0t] Test Completed. Exiting...", $time);
    $finish;
  endrule

endmodule

endpackage











































