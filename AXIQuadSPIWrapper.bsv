package AXIQuadSPIWrapper;

import AXI4_Lite_Types::*;
import AXI4_Lite::*;
import Connectable::*;
import QuadSPIController::*;   // Your QSPI controller package
import QSPI::*;                // For QSPIIO definition

interface AXIQuadSPI_IFC;
  interface AXI4_Lite_Slave_IFC#(32, 32, 0) slave;
  interface QSPIIO io;
endinterface

(* synthesize *)
module mkAXIQuadSPIWrapper(AXIQuadSPI_IFC);

  // Instantiate your actual QSPI controller
  QuadSPI ctrl <- mkQuadSPIController;

  // Expose the interfaces exactly as your controller does
  interface slave = ctrl.reg_ifc;
  interface io = ctrl.io;

endmodule

endpackage

