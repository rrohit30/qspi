package QuadSPITestbench;

import StmtFSM::*;
import Vector::*;
import FIFO::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import DefaultValue::*;
import Assert::*;

// Import your QuadSPIController module
import QuadSPIController::*;

module mkQuadSPITestbench(Empty);
    // Instantiate the DUT
    QuadSPIController_Ifc spiCtrl <- mkQuadSPIController();
    
    // Flash memory model
    RegFile#(Bit#(32), Bit#(8)) flashMem <- mkRegFileFull();
    
    // Test counters
    Reg#(UInt#(32)) testCount <- mkReg(0);
    Reg#(UInt#(32)) passCount <- mkReg(0);
    Reg#(UInt#(32)) failCount <- mkReg(0);
    
    // Initialize flash memory with random data
    function Action initFlashMem(UInt#(32) idx);
    action
        Bit#(8) randVal = truncate(pack(idx) ^ (idx >> 8) ^ (idx >> 16) ^ (idx >> 24));
        flashMem.upd(truncate(pack(idx)), randVal);
    endaction
    endfunction
    
    Stmt initStmts = seq
        for (UInt#(32) i = 0; i < 1024; i = i + 1)
            initFlashMem(i);
        $display("[%0t] Flash memory initialized with random data", $time);
    endseq;
    FSM initFSM <- mkFSM(initStmts);
    
    // Helper function to read from flash memory
    function Bit#(32) readFlashMem(Bit#(32) addr);
        Bit#(32) data = {
            flashMem.sub(addr+3),
            flashMem.sub(addr+2),
            flashMem.sub(addr+1),
            flashMem.sub(addr)
        };
        return data;
    endfunction
    
    // Test sequence
    Stmt testStmts = seq
        // Start initialization
        initFSM.start();
        await(initFSM.done());
        
        // Test single mode reads
        $display("\n=== Testing SINGLE Mode ===");
        testSingleRead(32'h0000_0100);
        testSingleRead(32'h0000_0200);
        testSingleRead(32'h0000_0300);
        
        // Test dual mode reads
        $display("\n=== Testing DUAL Mode ===");
        testDualRead(32'h0001_0000);
        testDualRead(32'h0001_0100);
        testDualRead(32'h0001_0200);
        
        // Test quad mode reads
        $display("\n=== Testing QUAD Mode ===");
        testQuadRead(32'h0002_0000);
        testQuadRead(32'h0002_0100);
        testQuadRead(32'h0002_0200);
        
        // Test mixed mode operations
        $display("\n=== Testing MIXED Modes ===");
        testSingleRead(32'h0000_0400);
        testDualRead(32'h0001_0300);
        testQuadRead(32'h0002_0300);
        testSingleRead(32'h0000_0500);
        
        // Print summary
        $display("\n=== Test Summary ===");
        $display("Total tests: %0d", testCount);
        $display("Passed: %0d", passCount);
        $display("Failed: %0d", failCount);
        
        if (failCount == 0)
            $display("ALL TESTS PASSED!");
        else
            $display("SOME TESTS FAILED!");
        
        $finish();
    endseq;
    
    FSM testFSM <- mkFSM(testStmts);
    
    // Test tasks
    function Action testSingleRead(Bit#(32) addr);
    action
        let expected = readFlashMem(addr);
        
        $display("[%0t] Starting SINGLE read from address 0x%08h", $time, addr);
        spiCtrl.request.put(SPIRequest {
            addr: addr,
            data: ?,
            mode: SINGLE,
            isWrite: False
        });
        
        let response <- spiCtrl.response.get();
        testCount <= testCount + 1;
        
        if (response == expected) begin
            $display("[%0t] PASS: Read 0x%08h from 0x%08h (SINGLE)", $time, response, addr);
            passCount <= passCount + 1;
        end else begin
            $display("[%0t] FAIL: Expected 0x%08h, got 0x%08h (SINGLE)", $time, expected, response);
            failCount <= failCount + 1;
        end
    endaction
    endfunction
    
    function Action testDualRead(Bit#(32) addr);
    action
        let expected = readFlashMem(addr);
        
        $display("[%0t] Starting DUAL read from address 0x%08h", $time, addr);
        spiCtrl.request.put(SPIRequest {
            addr: addr,
            data: ?,
            mode: DUAL,
            isWrite: False
        });
        
        let response <- spiCtrl.response.get();
        testCount <= testCount + 1;
        
        if (response == expected) begin
            $display("[%0t] PASS: Read 0x%08h from 0x%08h (DUAL)", $time, response, addr);
            passCount <= passCount + 1;
        end else begin
            $display("[%0t] FAIL: Expected 0x%08h, got 0x%08h (DUAL)", $time, expected, response);
            failCount <= failCount + 1;
        end
    endaction
    endfunction
    
    function Action testQuadRead(Bit#(32) addr);
    action
        let expected = readFlashMem(addr);
        
        $display("[%0t] Starting QUAD read from address 0x%08h", $time, addr);
        spiCtrl.request.put(SPIRequest {
            addr: addr,
            data: ?,
            mode: QUAD,
            isWrite: False
        });
        
        let response <- spiCtrl.response.get();
        testCount <= testCount + 1;
        
        if (response == expected) begin
            $display("[%0t] PASS: Read 0x%08h from 0x%08h (QUAD)", $time, response, addr);
            passCount <= passCount + 1;
        end else begin
            $display("[%0t] FAIL: Expected 0x%08h, got 0x%08h (QUAD)", $time, expected, response);
            failCount <= failCount + 1;
        end
    endaction
    endfunction
    
    // Start the test sequence
    rule startTest;
        testFSM.start();
    endrule
    
    // Model the Quad SPI flash behavior
    rule handleSPIRequests;
        let req <- spiCtrl.spi.request.get();
        
        // Decode the command based on the mode
        case (req.mode)
            SINGLE: $display("[%0t] SPI Command: Single mode, Addr: 0x%08h", $time, req.addr);
            DUAL:   $display("[%0t] SPI Command: Dual mode, Addr: 0x%08h", $time, req.addr);
            QUAD:   $display("[%0t] SPI Command: Quad mode, Addr: 0x%08h", $time, req.addr);
        endcase
        
        // Read from flash memory
        Bit#(32) data = readFlashMem(req.addr);
        
        // Send response
        spiCtrl.spi.response.put(data);
        $display("[%0t] SPI Response: 0x%08h", $time, data);
    endrule
endmodule

endpackage
