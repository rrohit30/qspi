package AXI_QSPI;

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Vector::*;
import Clocks::*;
import ConfigReg::*;
import BUtils::*;
import DReg::*;
import DefaultValue::*;
import RegFile::*;
import GetPut::*;

typedef struct {
    Bit#(32) awaddr;
} AXI4_Lite_Write_Addr deriving (Bits, Eq);

typedef struct {
    Bit#(32) wdata;
} AXI4_Lite_Write_Data deriving (Bits, Eq);

typedef struct {
    Bit#(2) bresp;
} AXI4_Lite_Resp deriving (Bits, Eq);

typedef struct {
    Bit#(32) araddr;
} AXI4_Lite_Read_Addr deriving (Bits, Eq);

typedef struct {
    Bit#(32) rdata;
    Bit#(2) rresp;
} AXI4_Lite_Read_Data deriving (Bits, Eq);

interface AXI4_Lite_Slave_IFC#(numeric type addrWidth, numeric type dataWidth);
    interface Get#(AXI4_Lite_Write_Addr) awvalid;
    interface Get#(AXI4_Lite_Write_Data) wvalid;
    interface Put#(AXI4_Lite_Resp) b;
    interface Get#(AXI4_Lite_Read_Addr) arvalid;
    interface Put#(AXI4_Lite_Read_Data) r;
endinterface

typedef enum { MODE_SINGLE, MODE_DUAL, MODE_QUAD } SPIMode deriving (Bits, Eq, FShow);
typedef enum { PHASE_IDLE, PHASE_INSTR, PHASE_ADDR, PHASE_DUMMY, PHASE_DATA_TX, PHASE_DATA_RX, PHASE_DONE, PHASE_XIP } SPIPhase deriving (Bits, Eq, FShow);

interface QSPI_IO;
    method Bit#(1) clk_o;
    method Bit#(1) ncs_o;
    method Bit#(4) io_o;
    method Bit#(4) io_en;
    method Action io_i(Bit#(4) i);
endinterface

interface Ifc_AXI_QSPI;
    interface AXI4_Lite_Slave_IFC#(32, 32) axi;
    interface QSPI_IO io;
endinterface

module mkAXI_QSPI(Ifc_AXI_QSPI);

    Reg#(SPIPhase) phase <- mkReg(PHASE_IDLE);
    Reg#(SPIMode) mode <- mkReg(MODE_SINGLE);
    Reg#(Bit#(8)) reg_instr <- mkReg(0);
    Reg#(Bit#(32)) reg_addr <- mkReg(0);
    Reg#(Bit#(16)) reg_len <- mkReg(0);
    Reg#(Bit#(8)) dummy_cycles <- mkReg(0);
    Reg#(Bool) is_write <- mkReg(False);
    Reg#(Bool) xip_enable <- mkReg(False);

    Reg#(Bit#(3)) bit_cnt <- mkReg(0);
    Reg#(Bit#(8)) shift_out <- mkReg(0);
    Reg#(Bit#(8)) shift_in <- mkReg(0);

    Reg#(Bit#(4)) io_out <- mkReg(0);
    Reg#(Bit#(4)) io_en_reg <- mkReg(0);
    Wire#(Bit#(4)) io_in <- mkDWire(0);
    Reg#(Bit#(1)) clk <- mkReg(0);
    Reg#(Bit#(1)) ncs <- mkReg(1);

    FIFOF#(Bit#(8)) txfifo <- mkSizedFIFOF(16);
    FIFOF#(Bit#(8)) rxfifo <- mkSizedFIFOF(16);

    RegFile#(Bit#(5), Bit#(32)) regfile <- mkRegFileFull();

    FIFO#(AXI4_Lite_Write_Addr) awfifo <- mkFIFO();
    FIFO#(AXI4_Lite_Write_Data) wfifo <- mkFIFO();
    FIFO#(AXI4_Lite_Read_Addr) arfifo <- mkFIFO();
    FIFO#(AXI4_Lite_Resp) bfifo <- mkFIFO();
    FIFO#(AXI4_Lite_Read_Data) rfifo <- mkFIFO();

    rule toggle_clk;
        clk <= ~clk;
    endrule

    rule set_io_mode (phase != PHASE_IDLE);
        io_en_reg <= (mode == MODE_QUAD) ? 4'b1111 :
                     (mode == MODE_DUAL) ? 4'b0011 : 4'b0001;
    endrule

    rule do_instr (phase == PHASE_INSTR && clk == 1);
        shift_out <= reg_instr;
        bit_cnt <= 2;
        phase <= PHASE_ADDR;
    endrule

    rule do_addr (phase == PHASE_ADDR && clk == 1);
        shift_out <= truncate(reg_addr[31:24]);
        reg_addr <= reg_addr << 8;
        bit_cnt <= 2;
        phase <= (dummy_cycles > 0) ? PHASE_DUMMY : (is_write ? PHASE_DATA_TX : PHASE_DATA_RX);
    endrule

    rule do_dummy (phase == PHASE_DUMMY && clk == 1);
        if (dummy_cycles > 0)
            dummy_cycles <= dummy_cycles - 1;
        else
            phase <= is_write ? PHASE_DATA_TX : PHASE_DATA_RX;
    endrule

    rule do_data_tx (phase == PHASE_DATA_TX && clk == 1);
        if (bit_cnt == 0 && txfifo.notEmpty) begin
            shift_out <= txfifo.first;
            txfifo.deq;
            reg_len <= reg_len - 1;
            bit_cnt <= 2;
            if (reg_len == 1 && !xip_enable) phase <= PHASE_DONE;
            else if (reg_len == 1 && xip_enable) phase <= PHASE_XIP;
        end else begin
            bit_cnt <= bit_cnt - 1;
        end
    endrule

    rule do_data_rx_shift (phase == PHASE_DATA_RX && clk == 1 && bit_cnt != 1);
        shift_in <= {shift_in[3:0], io_in};
        bit_cnt <= bit_cnt + 1;
    endrule

    rule do_data_rx_store (phase == PHASE_DATA_RX && clk == 1 && bit_cnt == 1);
        shift_in <= {shift_in[3:0], io_in};
        rxfifo.enq(shift_in);
        reg_len <= reg_len - 1;
        bit_cnt <= 0;
        if (reg_len == 1 && !xip_enable) phase <= PHASE_DONE;
        else if (reg_len == 1 && xip_enable) phase <= PHASE_XIP;
    endrule

    rule do_xip (phase == PHASE_XIP && clk == 1);
        shift_in <= {shift_in[3:0], io_in};
        rxfifo.enq(shift_in);
    endrule

    rule do_done (phase == PHASE_DONE);
        io_en_reg <= 0;
        ncs <= 1;
        phase <= PHASE_IDLE;
    endrule

    rule process_axi_write;
        let req = awfifo.first;
        awfifo.deq;
        let wdata = wfifo.first;
        wfifo.deq;

        Bit#(5) addr = truncate(req.awaddr[6:2]);
        regfile.upd(addr, wdata.wdata);

        case (addr)
            0: begin
                if (wdata.wdata[0] == 1 && phase == PHASE_IDLE) begin
                    reg_instr <= truncate(regfile.sub(1));
                    reg_addr <= regfile.sub(2);
                    reg_len <= truncate(regfile.sub(3));
                    dummy_cycles <= truncate(regfile.sub(4));
                    is_write <= (wdata.wdata[1] == 1);
                    xip_enable <= (wdata.wdata[5] == 1);
                    case (wdata.wdata[3:2])
                        0: mode <= MODE_SINGLE;
                        1: mode <= MODE_DUAL;
                        2: mode <= MODE_QUAD;
                        default: mode <= MODE_SINGLE;
                    endcase
                    ncs <= 0;
                    phase <= PHASE_INSTR;
                end
            end
            5: begin
                if (txfifo.notFull) begin
                    txfifo.enq(truncate(wdata.wdata));
                end
            end
            default: noAction;
        endcase

        AXI4_Lite_Resp bresp = AXI4_Lite_Resp { bresp: 2'b00 };
        bfifo.enq(bresp);
    endrule

    rule process_axi_read;
        let req = arfifo.first;
        arfifo.deq;
        Bit#(5) addr = truncate(req.araddr[6:2]);
        Bit#(32) rdata = 0;

        case (addr)
            6: if (rxfifo.notEmpty) begin rdata = zeroExtend(rxfifo.first); rxfifo.deq; end
            7: begin
                rdata[0] = pack(phase == PHASE_IDLE);
                rdata[1] = pack(txfifo.notFull);
                rdata[2] = pack(rxfifo.notEmpty);
                rdata[4:3] = pack(mode);
                rdata[5] = pack(xip_enable);
            end
            default: rdata = regfile.sub(addr);
        endcase

        AXI4_Lite_Read_Data rresp = AXI4_Lite_Read_Data { rdata: rdata, rresp: 2'b00 };
        rfifo.enq(rresp);
    endrule

    interface QSPI_IO io;
        method Bit#(1) clk_o = clk;
        method Bit#(1) ncs_o = ncs;
        method Bit#(4) io_o = io_out;
        method Bit#(4) io_en = io_en_reg;
        method Action io_i(Bit#(4) i);
            io_in <= i;
        endmethod
    endinterface

    interface AXI4_Lite_Slave_IFC axi;
        interface awvalid = toGet(awfifo);
        interface wvalid  = toGet(wfifo);
        interface b       = toPut(bfifo);
        interface arvalid = toGet(arfifo);
        interface r       = toPut(rfifo);
    endinterface

endmodule

endpackage

