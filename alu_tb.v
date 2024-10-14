`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 12.10.2024 12:11:08
// Design Name: 
// Module Name: alu_tb
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module alu_tb;

// Inputs
reg [31:0] n1;
reg [31:0] n2;
reg [1:0] oper;

// Outputs
wire [31:0] result;
wire Overflow;
wire Underflow;
wire Exception;

// Instantiate the Unit Under Test (UUT)
alu uut (
    .n1(n1), 
    .n2(n2), 
    .oper(oper), 
    .result(result), 
    .Overflow(Overflow), 
    .Underflow(Underflow), 
    .Exception(Exception)
);

initial begin
    // Initialize Inputs
    n1 = 32'b01000010001101101011000000000000;  // 45.678 in IEEE 754 format
    n2 = 32'b01000001101111000111000000000000;  // 23.56  in IEEE 754 format
    oper = 2'b00;       // Add operation
    #10;

    n1 = 32'b01000010001101101011000000000000;  // 45.678 in IEEE 754 format
    n2 = 32'b01000001101111000111000000000000;  // 23.56  in IEEE 754 format
    oper = 2'b01;       // Subtract operation
    #10;

    n1 = 32'b01000010001101101011000000000000;  // 45.678 in IEEE 754 format
    n2 = 32'b01000001101111000111000000000000;  // 23.56  in IEEE 754 format
    oper = 2'b10;       // Multiply operation
    #10;

    n1 = 32'b01000010001101101011000000000000;  // 45.678 in IEEE 754 format
    n2 = 32'b01000001101111000111000000000000;  // 23.56  in IEEE 754 format
    oper = 2'b11;       // Divide operation
    #10;

    n1 = 32'b01111111100000000000000000000000;  // Infinity in IEEE 754 format
    n2 = 32'b00111111100000000000000000000000;  // 1.0 in IEEE 754 format
    oper = 2'b00;       // Add operation (check overflow)
    #10;

    n1 = 32'b00000000000000000000000000000001;  // Smallest positive number in IEEE 754
    n2 = 32'b00000000000000000000000000000001;  // Smallest positive number in IEEE 754
    oper = 2'b00;       // Add operation (check underflow)
    #10;

    // Test Exception
    n1 = 32'b01000010001101101011000000000000;  // 45.678 in IEEE 754 format
    n2 = 32'b00000000000000000000000000000000;  // Zero in IEEE 754 format
    oper = 2'b11;       // Divide by zero (Exception)
    #10;

    // Stop simulation
    $finish;
end

endmodule
