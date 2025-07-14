package QuadSPITestbench;

import FIFO::*;
import GetPut::*;
import StmtFSM::*;
import QuadSPIController::*;

(* synthesize *)
module mkQuadSPITestbench(Empty);
    // Instantiate the controller
    QuadSPI spi <- mkQuadSPIController();
    
    // Test sequence
    Stmt test = seq
        $display("\n=== Starting SPI Controller Test ===");
        
        // ----------------------------------
        // Test Single SPI Mode
        // ----------------------------------
        $display("[SINGLE] Setting mode");
        spi.modeConfig.put(SINGLE);
        delay(10);
        
        $display("[SINGLE] Signal states:");
        $display("CS=%b SCLK=%b IO=%b IO_OE=%b", 
                spi.cs_n, spi.sclk, spi.io, spi.io_oe);
        
        // ----------------------------------
        // Test Dual SPI Mode 
        // ----------------------------------
        $display("\n[DUAL] Setting mode");
        spi.modeConfig.put(DUAL);
        delay(10);
        
        $display("[DUAL] Signal states:");
        $display("CS=%b SCLK=%b IO=%b IO_OE=%b",
                spi.cs_n, spi.sclk, spi.io, spi.io_oe);
        
        // ----------------------------------
        // Test Quad SPI Mode
        // ----------------------------------
        $display("\n[QUAD] Setting mode");
        spi.modeConfig.put(QUAD);
        delay(10);
        
        $display("[QUAD] Signal states:");
        $display("CS=%b SCLK=%b IO=%b IO_OE=%b",
                spi.cs_n, spi.sclk, spi.io, spi.io_oe);
        $display("WP=%b HOLD=%b", spi.wp_n, spi.hold_n);
        
        $display("\n=== Test Complete ===");
    endseq;
    
    mkAutoFSM(test);
endmodule

endpackage
