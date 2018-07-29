module Compile.RegisterList (registers,eflagsRegisterNumber) where
-- Follow this list when using IReg and AReg!!
eflagsRegisterNumber = -123412341
registers :: [String]
registers = [
  "eax", -- 0
  "ebx", -- 1
  "ecx", -- 2
  "edx", -- 3
  "esi", -- 4
  "edi", -- 5
  "ebp", -- 6
  "r8d",  -- 7
  "r9d",  -- 8
  "r10d", -- 9
  "r11d", -- 10
  "r12d", -- 11
  "r13d", -- 12
  "r14d", -- 13
  "r15d"  -- 14
  ]
