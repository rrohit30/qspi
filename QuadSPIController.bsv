package QuadSPIController;

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import GetPut::*;
import ClientServer::*;
import StmtFSM::*;
import Vector::*;

typedef enum {
    SINGLE,
    DUAL,
    QUAD
} SPIMode deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(32) addr;
    Bit#(8)  data;
    SPIMode  mode;
    Bool     writeProtected; // Indicates if write protection should be enabled
} WriteRequest deriving (Bits, FShow);

typedef struct {
    Bit#(32) addr;
    SPIMode  mode;
    Bool     holdActive;     // Indicates if hold function should be used
} ReadRequest deriving (Bits, FShow);

typedef struct {
    Bit#(8) data;
} ReadResponse deriving (Bits, FShow);

interface QuadSPI;
    interface Client#(WriteRequest, Bit#(0)) writeClient;
    interface Client#(ReadRequest, ReadResponse) readClient;
    interface Put#(SPIMode) modeConfig;
    method Bit#(1) sclk;
    method Bit#(1) cs_n;
    method Bit#(4) io;
    method Bit#(4) io_oe;    // Output enable for each IO pin
    method Bit#(1) wp_n;     // Write protect pin (active low)
    method Bit#(1) hold_n;   // Hold pin (active low)
endinterface

(* synthesize *)
module mkQuadSPIController(QuadSPI);
    // Configuration registers
    Reg#(SPIMode) spiMode <- mkReg(SINGLE);
    
    // Control signals
    Reg#(Bit#(1)) csReg      <- mkReg(1);
    Reg#(Bit#(1)) sclkReg    <- mkReg(1);
    Reg#(Bit#(4)) ioReg      <- mkReg(0);
    Reg#(Bit#(4)) ioOeReg    <- mkReg(0); // Output enable
    Reg#(Bit#(1)) wpReg      <- mkReg(1); // Write protect (active low, 1=unprotected)
    Reg#(Bit#(1)) holdReg    <- mkReg(1); // Hold (active low, 1=normal operation)
    
    // State machine control
    FIFOF#(WriteRequest) writeFifo <- mkBypassFIFOF;
    FIFOF#(ReadRequest) readFifo <- mkBypassFIFOF;
    FIFOF#(Bit#(0)) writeResponseFifo <- mkBypassFIFOF;
    FIFOF#(ReadResponse) readResponseFifo <- mkBypassFIFOF;
    
    // Clock divider for SPI clock
    Reg#(Bit#(8)) clkDivCounter <- mkReg(0);
    Reg#(Bit#(1)) clkDivOut <- mkReg(0);
    
    // SPI state machine
    // SPI state machine
Reg#(Bit#(4)) state <- mkReg(0);
Reg#(Bit#(32)) shiftReg <- mkReg(0);
Reg#(Bit#(5)) bitCounter <- mkReg(0);
Reg#(Bit#(32)) addrReg <- mkReg(0);
Reg#(Bool) writeProtectActive <- mkReg(False);
Reg#(Bool) holdActive <- mkReg(False);
    
    // Clock divider rule
    rule updateClkDiv;
        clkDivCounter <= clkDivCounter + 1;
        if (clkDivCounter == 0) begin
            clkDivOut <= ~clkDivOut;
        end
    endrule
    
    // Manage write protection based on requests
    rule manageWriteProtection;
        if (writeFifo.notEmpty) begin
            writeProtectActive <= writeFifo.first.writeProtected;
            wpReg <= writeFifo.first.writeProtected ? 0 : 1; // Active low
        end
    endrule
    
    // Manage hold function based on read requests
    rule manageHoldFunction;
        if (readFifo.notEmpty) begin
            holdActive <= readFifo.first.holdActive;
            holdReg <= readFifo.first.holdActive ? 0 : 1; // Active low
        end
    endrule
    
    // Main SPI state machine
rule spiStateMachine (state != 0);
    if (clkDivCounter == 0) begin
        case (state)
            // Command phase (8 bits)
            1: begin
                if (bitCounter < 8) begin
                    // Output command bits on falling edge
                    if (clkDivOut == 0) begin
                        sclkReg <= 0;
                        case (spiMode)
                            SINGLE: action
                                ioReg <= {3'b0, shiftReg[31]};
                                ioOeReg <= 4'b0001;
                                shiftReg <= shiftReg << 1;
                            endaction
                            DUAL: action
                                ioReg <= {2'b0, shiftReg[31:30]};
                                ioOeReg <= 4'b0011;
                                shiftReg <= shiftReg << 2;
                            endaction
                            QUAD: action
                                let io3_oe = holdActive ? 0 : 1;
                                ioReg <= {shiftReg[31:29], wpReg};
                                ioOeReg <= {3'b111, io3_oe};
                                shiftReg <= shiftReg << 4;
                            endaction
                        endcase
                    end // clkDivOut == 0
                    // Sample on rising edge
                    else action
                        sclkReg <= 1;
                        bitCounter <= bitCounter + case (spiMode)
                            SINGLE: 1;
                            DUAL:   2;
                            QUAD:   4;
                        endcase;
                    endaction
                end // bitCounter < 8
                else action
                    bitCounter <= 0;
                    state <= 2; // Move to address phase
                    shiftReg <= addrReg;
                endaction
            end // state 1

            // Address phase (24 bits)
            2: begin
                if (bitCounter < 24) begin
                    // Output address bits on falling edge
                    if (clkDivOut == 0) begin
                        sclkReg <= 0;
                        case (spiMode)
                            SINGLE: action
                                ioReg <= {3'b0, shiftReg[31]};
                                ioOeReg <= 4'b0001;
                                shiftReg <= shiftReg << 1;
                            endaction
                            DUAL: action
                                ioReg <= {2'b0, shiftReg[31:30]};
                                ioOeReg <= 4'b0011;
                                shiftReg <= shiftReg << 2;
                            endaction
                            QUAD: action
                                let io3_is_data = (bitCounter < 21);
                                let io3_oe = io3_is_data ? 1 : (holdActive ? 0 : 1);
                                ioReg <= {shiftReg[31:29], (io3_is_data ? shiftReg[28] : wpReg)};
                                ioOeReg <= {3'b111, io3_oe};
                                shiftReg <= shiftReg << 4;
                            endaction
                        endcase
                    end // clkDivOut == 0
                    // Sample on rising edge
                    else action
                        sclkReg <= 1;
                        bitCounter <= bitCounter + case (spiMode)
                            SINGLE: 1;
                            DUAL:   2;
                            QUAD:   4;
                        endcase;
                    endaction
                end // bitCounter < 24
                else action
                    bitCounter <= 0;
                    if (writeFifo.notEmpty) begin
                        state <= 3; // Write data phase
                        shiftReg <= zeroExtend(writeFifo.first.data);
                    end
                    else begin
                        state <= 4; // Read dummy cycles then data
                    end
                endaction
            end // state 2

            // Write data phase (8 bits)
            3: begin
                if (bitCounter < 8) begin
                    // Output data bits on falling edge
                    if (clkDivOut == 0) begin
                        sclkReg <= 0;
                        case (spiMode)
                            SINGLE: action
                                ioReg <= {3'b0, shiftReg[31]};
                                ioOeReg <= 4'b0001;
                                shiftReg <= shiftReg << 1;
                            endaction
                            DUAL: action
                                ioReg <= {2'b0, shiftReg[31:30]};
                                ioOeReg <= 4'b0011;
                                shiftReg <= shiftReg << 2;
                            endaction
                            QUAD: action
                                let io3_oe = holdActive ? 0 : 1;
                                ioReg <= {shiftReg[31:29], wpReg};
                                ioOeReg <= {3'b111, io3_oe};
                                shiftReg <= shiftReg << 4;
                            endaction
                        endcase
                    end // clkDivOut == 0
                    // Sample on rising edge
                    else action
                        sclkReg <= 1;
                        bitCounter <= bitCounter + case (spiMode)
                            SINGLE: 1;
                            DUAL:   2;
                            QUAD:   4;
                        endcase;
                    endaction
                end // bitCounter < 8
                else action
                    writeResponseFifo.enq(?);
                    writeFifo.deq;
                    state <= 0; // Transaction complete
                    csReg <= 1;
                    writeProtectActive <= False;
                    wpReg <= 1; // Disable write protection after write
                endaction
            end // state 3

            // Read dummy cycles
            4: begin
                if (bitCounter < 8) begin // 2 dummy cycles in quad mode
                    if (clkDivOut == 0) begin
                        sclkReg <= 0;
                        ioOeReg <= case (spiMode)
                            QUAD: {3'b0, (holdActive ? 0 : 1)};
                            default: 4'b0;
                        endcase;
                    end
                    else action
                        sclkReg <= 1;
                        bitCounter <= bitCounter + 1;
                    endaction
                end // bitCounter < 8
                else action
                    bitCounter <= 0;
                    state <= 5; // Move to read data phase
                    shiftReg <= 0;
                endaction
            end // state 4

            // Read data phase (8 bits)
            5: begin
                if (bitCounter < 8) begin
                    if (clkDivOut == 0) begin
                        sclkReg <= 0;
                        ioOeReg <= case (spiMode)
                            QUAD: {3'b0, (holdActive ? 0 : 1)};
                            default: 4'b0;
                        endcase;
                    end
                    else action
                        sclkReg <= 1;
                        // Sample input bits on rising edge
                        case (spiMode)
                            SINGLE: shiftReg <= {shiftReg[30:0], pack(ioReg)[0]};
                            DUAL: shiftReg <= {shiftReg[29:0], pack(ioReg)[1:0]};
                            QUAD: shiftReg <= {shiftReg[27:0], pack(ioReg)};
                        endcase
                        bitCounter <= bitCounter + case (spiMode)
                            SINGLE: 1;
                            DUAL: 2;
                            QUAD: 4;
                        endcase;
                    endaction
                end // bitCounter < 8
                else action
                    readResponseFifo.enq(ReadResponse { data: truncate(shiftReg) });
                    readFifo.deq;
                    state <= 0; // Transaction complete
                    csReg <= 1;
                    holdActive <= False;
                    holdReg <= 1; // Release hold after read
                endaction
            end // state 5
        endcase // state case
    end // clkDivCounter == 0
endrule
    // Start new transaction when idle
    rule startWriteTransaction (state == 0 && writeFifo.notEmpty);
        state <= 1;
        csReg <= 0;
        addrReg <= writeFifo.first.addr;
        // Load command into shift register (Page Program command)
        shiftReg <= (spiMode == QUAD) ? 32'h38000000 :  // Quad Page Program
                   (spiMode == DUAL) ? 32'hA2000000 :   // Dual Page Program
                   32'h02000000;                       // Standard Page Program
        bitCounter <= 0;
    endrule
    
    rule startReadTransaction (state == 0 && readFifo.notEmpty);
        state <= 1;
        csReg <= 0;
        addrReg <= readFifo.first.addr;
        // Load command into shift register (Read command)
        shiftReg <= (spiMode == QUAD) ? 32'hEB000000 :  // Quad Output Fast Read
                   (spiMode == DUAL) ? 32'hBB000000 :   // Dual Output Fast Read
                   32'h03000000;                      // Standard Read
        bitCounter <= 0;
    endrule
    
    // Interface definitions
    interface writeClient = toGPClient(writeFifo, writeResponseFifo);
    interface readClient = toGPClient(readFifo, readResponseFifo);
    
    interface Put modeConfig;
        method Action put(SPIMode mode);
            spiMode <= mode;
        endmethod
    endinterface
    
    method Bit#(1) sclk = sclkReg;
    method Bit#(1) cs_n = csReg;
    method Bit#(4) io = ioReg;
    method Bit#(4) io_oe = ioOeReg;
    method Bit#(1) wp_n = wpReg;
    method Bit#(1) hold_n = holdReg;
endmodule

endpackage
