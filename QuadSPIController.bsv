package QuadSPIController;

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import GetPut::*;
import ClientServer::*;
import StmtFSM::*;
import Vector::*;

// New transaction mode type
typedef enum {
    NO_TRANSACTION = 2'b00,
    SINGLE_MODE    = 2'b01,
    DUAL_MODE      = 2'b10,
    QUAD_MODE      = 2'b11
} SPITransactionMode deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(32) addr;
    Bit#(8)  data;
    SPITransactionMode mode;  // Changed to new enum type
    Bool     writeProtected;
} WriteRequest deriving (Bits, FShow);

typedef struct {
    Bit#(32) addr;
    SPITransactionMode mode;  // Changed to new enum type
    Bool     isFastRead;      // Added to distinguish fast read
} ReadRequest deriving (Bits, FShow);

typedef struct {
    Bit#(8) data;
} ReadResponse deriving (Bits, FShow);

interface QuadSPI;
    interface Client#(WriteRequest, Bit#(0)) writeClient;
    interface Client#(ReadRequest, ReadResponse) readClient;
    method Action startTransaction(Bool enable);  // Added enable pin control
    method Bit#(1) sclk;
    method Bit#(1) cs_n;
    method Bit#(4) io;
    method Bit#(4) io_oe;
    method Action io_in(Bit#(4) i);
endinterface

(* synthesize *)
module mkQuadSPIController(QuadSPI);
    // Configuration registers
    Reg#(Bool) transactionEnabled <- mkReg(False);  // Added enable control
    
    // Control signals
    Reg#(Bit#(1)) csReg      <- mkReg(1);
    Reg#(Bit#(1)) sclkReg    <- mkReg(1);
    Reg#(Bit#(4)) ioReg      <- mkReg(0);
    Reg#(Bit#(4)) ioOeReg    <- mkReg(0);
    Reg#(Bit#(4)) ioInReg    <- mkReg(0);
    
    // State machine registers
    Reg#(Bit#(4)) state <- mkReg(0);
    Reg#(Bit#(32)) shiftReg <- mkReg(0);
    Reg#(Bit#(5)) bitCounter <- mkReg(0);
    Reg#(Bit#(32)) addrReg <- mkReg(0);
    Reg#(SPITransactionMode) currentMode <- mkReg(NO_TRANSACTION);
    Reg#(Bool) isFastRead <- mkReg(False);
    
    // FIFOs
    FIFOF#(WriteRequest) writeFifo <- mkBypassFIFOF;
    FIFOF#(ReadRequest) readFifo <- mkBypassFIFOF;
    FIFOF#(Bit#(0)) writeResponseFifo <- mkBypassFIFOF;
    FIFOF#(ReadResponse) readResponseFifo <- mkBypassFIFOF;
    
    // Clock divider
    Reg#(Bit#(8)) clkDivCounter <- mkReg(0);
    Reg#(Bit#(1)) clkDivOut <- mkReg(0);
    
    // Command encodings for different modes
    function Bit#(8) getWriteCmd(SPITransactionMode mode);
        case (mode)
            SINGLE_MODE: return 8'h02;  // Page Program
            DUAL_MODE:   return 8'hA2;  // Dual Input Fast Program
            QUAD_MODE:   return 8'h32;  // Quad Input Fast Program
            default:     return 8'h00;
        endcase
    endfunction
    
    function Bit#(8) getReadCmd(SPITransactionMode mode, Bool isFast);
        case (mode)
            SINGLE_MODE: return isFast ? 8'h0B : 8'h03;  // Fast/Slow Read
            DUAL_MODE:   return isFast ? 8'hBB : 8'h3B;  // Dual Output Fast Read
            QUAD_MODE:   return isFast ? 8'hEB : 8'h6B;  // Quad Output Fast Read
            default:     return 8'h00;
        endcase
    endfunction
    
    // Clock divider rule
    rule updateClkDiv;
        clkDivCounter <= clkDivCounter + 1;
        if (clkDivCounter == 0) begin
            clkDivOut <= ~clkDivOut;
        end
    endrule
    
    // Main SPI state machine
    rule spiStateMachine (state != 0 && transactionEnabled);
        if (clkDivCounter == 0) begin
            case (state)
                // Command phase (8 bits)
                1: begin
                    if (bitCounter < 8) begin
                        if (clkDivOut == 0) begin
                            sclkReg <= 0;
                            case (currentMode)
                                SINGLE_MODE: action
                                    ioReg <= {3'b0, shiftReg[31]};
                                    ioOeReg <= 4'b0001;
                                    shiftReg <= shiftReg << 1;
                                endaction
                                DUAL_MODE: action
                                    ioReg <= {2'b0, shiftReg[31:30]};
                                    ioOeReg <= 4'b0011;
                                    shiftReg <= shiftReg << 2;
                                endaction
                                QUAD_MODE: action
                                    ioReg <= shiftReg[31:28];
                                    ioOeReg <= 4'b1111;
                                    shiftReg <= shiftReg << 4;
                                endaction
                                default: noAction;
                            endcase
                        end
                        else begin
                            sclkReg <= 1;
                            bitCounter <= bitCounter + case (currentMode)
                                SINGLE_MODE: 1;
                                DUAL_MODE:   2;
                                QUAD_MODE:   4;
                                default:     0;
                            endcase;
                        end
                    end
                    else begin
                        bitCounter <= 0;
                        state <= 2; // Move to address phase
                        shiftReg <= addrReg;
                    end
                end
                
                // Address phase (24 bits)
                2: begin
                    if (bitCounter < 24) begin
                        if (clkDivOut == 0) begin
                            sclkReg <= 0;
                            case (currentMode)
                                SINGLE_MODE: action
                                    ioReg <= {3'b0, shiftReg[31]};
                                    ioOeReg <= 4'b0001;
                                    shiftReg <= shiftReg << 1;
                                endaction
                                DUAL_MODE: action
                                    ioReg <= {2'b0, shiftReg[31:30]};
                                    ioOeReg <= 4'b0011;
                                    shiftReg <= shiftReg << 2;
                                endaction
                                QUAD_MODE: action
                                    ioReg <= shiftReg[31:28];
                                    ioOeReg <= 4'b1111;
                                    shiftReg <= shiftReg << 4;
                                endaction
                                default: noAction;
                            endcase
                        end
                        else begin
                            sclkReg <= 1;
                            bitCounter <= bitCounter + case (currentMode)
                                SINGLE_MODE: 1;
                                DUAL_MODE:   2;
                                QUAD_MODE:   4;
                                default:     0;
                            endcase;
                        end
                    end
                    else begin
                        bitCounter <= 0;
                        if (writeFifo.notEmpty) begin
                            state <= 3; // Write data phase
                            shiftReg <= zeroExtend(writeFifo.first.data);
                        end
                        else if (isFastRead) begin
                            state <= 4; // Dummy cycles for fast read
                        end
                        else begin
                            state <= 5; // Direct to read data
                        end
                    end
                end
                
                // Write data phase (8 bits)
                3: begin
                    if (bitCounter < 8) begin
                        if (clkDivOut == 0) begin
                            sclkReg <= 0;
                            case (currentMode)
                                SINGLE_MODE: action
                                    ioReg <= {3'b0, shiftReg[31]};
                                    ioOeReg <= 4'b0001;
                                    shiftReg <= shiftReg << 1;
                                endaction
                                DUAL_MODE: action
                                    ioReg <= {2'b0, shiftReg[31:30]};
                                    ioOeReg <= 4'b0011;
                                    shiftReg <= shiftReg << 2;
                                endaction
                                QUAD_MODE: action
                                    ioReg <= shiftReg[31:28];
                                    ioOeReg <= 4'b1111;
                                    shiftReg <= shiftReg << 4;
                                endaction
                                default: noAction;
                            endcase
                        end
                        else begin
                            sclkReg <= 1;
                            bitCounter <= bitCounter + case (currentMode)
                                SINGLE_MODE: 1;
                                DUAL_MODE:   2;
                                QUAD_MODE:   4;
                                default:     0;
                            endcase;
                        end
                    end
                    else begin
                        writeResponseFifo.enq(?);
                        writeFifo.deq;
                        state <= 0;
                        csReg <= 1;
                        currentMode <= NO_TRANSACTION;
                    end
                end
                
                // Dummy cycles (only for fast read)
                4: begin
                    if (bitCounter < 8) begin // 2 dummy cycles in quad mode
                        if (clkDivOut == 0) begin
                            sclkReg <= 0;
                            ioOeReg <= case (currentMode)
                                QUAD_MODE: 4'b0000;
                                DUAL_MODE: 4'b0000;
                                default:   4'b0000;
                            endcase;
                        end
                        else begin
                            sclkReg <= 1;
                            bitCounter <= bitCounter + 1;
                        end
                    end
                    else begin
                        bitCounter <= 0;
                        state <= 5; // Move to read data phase
                        shiftReg <= 0;
                    end
                end
                
                // Read data phase (8 bits)
                5: begin
                    if (bitCounter < 8) begin
                        if (clkDivOut == 0) begin
                            sclkReg <= 0;
                            ioOeReg <= case (currentMode)
                                QUAD_MODE: 4'b0000;
                                DUAL_MODE: 4'b0000;
                                default:   4'b0000;
                            endcase;
                        end
                        else begin
                            sclkReg <= 1;
                            // Sample input bits on rising edge
                            case (currentMode)
                                SINGLE_MODE: shiftReg <= {shiftReg[30:0], ioInReg[0]};
                                DUAL_MODE: shiftReg <= {shiftReg[29:0], ioInReg[1:0]};
                                QUAD_MODE: shiftReg <= {shiftReg[27:0], ioInReg};
                                default: noAction;
                            endcase
                            bitCounter <= bitCounter + case (currentMode)
                                SINGLE_MODE: 1;
                                DUAL_MODE: 2;
                                QUAD_MODE: 4;
                                default: 0;
                            endcase;
                        end
                    end
                    else begin
                        readResponseFifo.enq(ReadResponse { data: truncate(shiftReg) });
                        readFifo.deq;
                        state <= 0;
                        csReg <= 1;
                        currentMode <= NO_TRANSACTION;
                        isFastRead <= False;
                    end
                end
            endcase
        end
    endrule
    
    // Start new transaction when enabled
    rule startWriteTransaction (state == 0 && writeFifo.notEmpty && transactionEnabled);
        state <= 1;
        csReg <= 0;
        addrReg <= writeFifo.first.addr;
        currentMode <= writeFifo.first.mode;
        // Load appropriate command
        shiftReg <= {getWriteCmd(writeFifo.first.mode), 24'h000000};
        bitCounter <= 0;
    endrule
    
    rule startReadTransaction (state == 0 && readFifo.notEmpty && transactionEnabled);
        state <= 1;
        csReg <= 0;
        addrReg <= readFifo.first.addr;
        currentMode <= readFifo.first.mode;
        isFastRead <= readFifo.first.isFastRead;
        // Load appropriate command
        shiftReg <= {getReadCmd(readFifo.first.mode, readFifo.first.isFastRead), 24'h000000};
        bitCounter <= 0;
    endrule
    
    // Interface implementations
    interface writeClient = toGPClient(writeFifo, writeResponseFifo);
    interface readClient = toGPClient(readFifo, readResponseFifo);
    
    method Action startTransaction(Bool enable);
        transactionEnabled <= enable;
        if (!enable) begin
            state <= 0;
            csReg <= 1;
            currentMode <= NO_TRANSACTION;
        end
    endmethod
    
    method Action io_in(Bit#(4) i);
        ioInReg <= i;
    endmethod
    
    method Bit#(1) sclk = sclkReg;
    method Bit#(1) cs_n = csReg;
    method Bit#(4) io = ioReg;
    method Bit#(4) io_oe = ioOeReg;
endmodule

endpackage
