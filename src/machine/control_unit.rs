use super::{datapath::*, MachineError};
use crate::isa::{instructions::*, RegisterId::*, *};
use log::debug;
use std::fmt::Display;

type CUResult = Result<ControlUnitState, MachineError>;

/// Блок декодирования и выполнения инструкций
#[derive(Default, Debug)]
struct InstructionDecoder<const M: usize> {
    /// Регистр для выполняемой инструкции
    pub instruction: MachineWord,
    /// Регистр для операнда инструкции (если таковой имеется и необходим для текущей инструкции).
    /// Иногда используется как обычный буфер
    pub operand_buf: MachineWord,
    /// Буфер-счётчик. Используется в делении
    pub count_buf: u32,
}
impl<const M: usize> InstructionDecoder<M> {
    pub fn decode(&self) -> CUResult {
        match &self.instruction {
            MachineWord::OpHigher(op) => Ok(match op {
                OpHigher::Math(math) => match math.args {
                    MathOpHigherArgs::MemToReg(_, _)
                    | MathOpHigherArgs::RegToMem(_, _)
                    | MathOpHigherArgs::RegImmed(_, _) => {
                        ControlUnitState::FetchOperand { tick_count: 1 }
                    },
                    _ => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                },
                OpHigher::Branch(_) => ControlUnitState::FetchOperand { tick_count: 1 },
                OpHigher::Alter(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                OpHigher::Io(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                OpHigher::Control(_) => ControlUnitState::ExecuteInstruction { tick_count: 1 },
                OpHigher::Stack(stack) => match stack.args {
                    StackOpHigherArgs::Register(_) | StackOpHigherArgs::None => {
                        ControlUnitState::ExecuteInstruction { tick_count: 1 }
                    },
                    StackOpHigherArgs::Immed(_) => ControlUnitState::FetchOperand { tick_count: 1 },
                },
            }),
            _ => Err(MachineError::InvalidInstruction(self.instruction.clone())),
        }
    }

    pub fn fetch_operand(&mut self, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        // 1 tick - instruction(lower) -> operand_buf(higher)
        // 2 tick - regs[eip] ADD 1 -> regs[eip]
        // 3 tick - regs[eip] ADD 0 -> mem_addr
        // 4 tick - memory[mem_addr] -> mem_out_buf
        // 5 tick - mem_out_buf STITCH operand_buf -> operand_buf
        match tick_count {
            1 => {
                let higher = self.instruction.as_number() as ImmedHigher;
                self.operand_buf = MachineWord::Data(Immed::of(higher, 0));
            },
            2 => {
                datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                datapath.signal_alu(AluSignal::ResetRight);
                datapath.signal_alu(AluSignal::AddRightOne);
                datapath.signal_alu_perform(
                    AluOpCode::Add,
                    false,
                    AluOutSel::Registers(InstructionPointer),
                );
            },
            3 => {
                datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                datapath.signal_alu(AluSignal::ResetRight);
                datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
            },
            4 => {
                datapath.signal_read(ReadOutSel::MemOutBuf);
            },
            5 => {
                datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                datapath.signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                datapath.signal_alu_perform(
                    AluOpCode::Stc,
                    false,
                    AluOutSel::Decoder { dest: &mut self.operand_buf },
                );
                return Ok(ControlUnitState::ExecuteInstruction { tick_count: 1 });
            },
            _ => return Err(MachineError::InvalidTickCall),
        };
        Ok(ControlUnitState::FetchOperand { tick_count: tick_count + 1 })
    }

    pub fn execute(&mut self, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        match &self.instruction.clone() {
            MachineWord::OpHigher(op) => match op {
                OpHigher::Math(math) => self.execute_math(math, tick_count, datapath),
                OpHigher::Branch(branch) => self.execute_branch(branch, tick_count, datapath),
                OpHigher::Alter(alter) => self.execute_alter(alter, tick_count, datapath),
                OpHigher::Io(io) => self.execute_io(io, tick_count, datapath),
                OpHigher::Control(control) => self.execute_control(control, tick_count, datapath),
                OpHigher::Stack(stack) => self.execute_stack(stack, tick_count, datapath),
            },
            _ => Err(MachineError::InvalidInstruction(self.instruction.clone())),
        }
    }

    fn execute_math(
        &self,
        math: &MathOpHigher,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        let call_alu = |datapath: &mut DataPath<M>, sel: AluOutSel| match math.opcode {
            MathOp::Mov => datapath.signal_alu_perform(AluOpCode::Mov, true, sel),
            MathOp::Add => datapath.signal_alu_perform(AluOpCode::Add, true, sel),
            MathOp::Sub => {
                datapath.signal_alu(AluSignal::InverseRight);
                datapath.signal_alu(AluSignal::AddRightOne);
                datapath.signal_alu_perform(AluOpCode::Add, true, sel)
            },
            MathOp::Cmp => {
                datapath.signal_alu(AluSignal::InverseRight);
                datapath.signal_alu(AluSignal::AddRightOne);
                datapath.signal_alu_perform(AluOpCode::Add, true, AluOutSel::None)
            },
            MathOp::Shl => datapath.signal_alu_perform(AluOpCode::Shl, true, sel),
            MathOp::Shr => datapath.signal_alu_perform(AluOpCode::Shr, true, sel),
        };
        match math.args {
            MathOpHigherArgs::RegToReg(dest, src) => {
                // 1 tick regs[dest] OP regs[src] -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        call_alu(datapath, AluOutSel::Registers(dest));
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            MathOpHigherArgs::RegToRegMem(dest, src) => {
                // 1 tick - regs[dest] -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - mem_out_buf OP src -> mem_in_buf
                // 4 tick - mem_in_buf -> memory[mem_addr]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        call_alu(datapath, AluOutSel::MemInBuf);
                    },
                    4 => {
                        datapath.signal_write();
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            MathOpHigherArgs::RegMemToReg(dest, src) => {
                // 1 tick - regs[src] -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - dest OP mem_out_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(src));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::MemOutBuf);
                        call_alu(datapath, AluOutSel::Registers(dest));
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            MathOpHigherArgs::MemToReg(dest, _) => {
                // 1 tick - operand_buf -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - dest OP mem_out_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath.signal_right_alu(AluRightMuxSel::MemOutBuf);
                        call_alu(datapath, AluOutSel::Registers(dest));
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            MathOpHigherArgs::RegToMem(src, _) => {
                // 1 tick - operand_buf -> mem_addr
                // 2 tick - memory[mem_addr] -> mem_out_buf
                // 3 tick - mem_out_buf OP regs[src] -> mem_in_buf
                // 4 tick - mem_in_buf -> memory[mem_addr]
                match tick_count {
                    1 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::MemOutBuf);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::MemOutBuf);
                        datapath.signal_right_alu(AluRightMuxSel::Registers(src));
                        call_alu(datapath, AluOutSel::MemInBuf);
                    },
                    4 => {
                        datapath.signal_write();
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            MathOpHigherArgs::RegImmed(dest, _) => {
                // 1 tick regs[dest] OP operand_buf -> regs[dest]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(dest));
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        call_alu(datapath, AluOutSel::Registers(dest));
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
        };
        Ok(ControlUnitState::ExecuteInstruction { tick_count: tick_count + 1 })
    }

    fn execute_branch(
        &self,
        branch: &BranchOpHigher,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match tick_count {
            1 => {
                let do_jump = match branch.opcode {
                    BranchOp::Jmp => true,
                    BranchOp::Jz => datapath.flags().zero,
                    BranchOp::Jnz => !datapath.flags().zero,
                    BranchOp::Js => datapath.flags().sign,
                    BranchOp::Jns => !datapath.flags().sign,
                };

                if do_jump {
                    datapath.signal_alu(AluSignal::ResetLeft);
                    datapath.signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                    datapath.signal_alu_perform(
                        AluOpCode::Mov,
                        false,
                        AluOutSel::Registers(InstructionPointer),
                    );
                    Ok(ControlUnitState::default())
                } else {
                    Ok(ControlUnitState::cycle_start())
                }
            },
            _ => Err(MachineError::InvalidTickCall),
        }
    }

    fn execute_alter(
        &self,
        alter: &AlterOpWord,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match alter.opcode {
            AlterOp::Inc => match tick_count {
                1 => {
                    datapath.signal_left_alu(AluLeftMuxSel::Registers(alter.arg));
                    datapath.signal_alu(AluSignal::ResetRight);
                    datapath.signal_alu(AluSignal::AddRightOne);
                    datapath.signal_alu_perform(
                        AluOpCode::Add,
                        true,
                        AluOutSel::Registers(alter.arg),
                    );
                    Ok(ControlUnitState::cycle_start())
                },
                _ => Err(MachineError::InvalidTickCall),
            },
            AlterOp::Dec => match tick_count {
                1 => {
                    datapath.signal_left_alu(AluLeftMuxSel::Registers(alter.arg));
                    datapath.signal_alu(AluSignal::ResetRight);
                    datapath.signal_alu(AluSignal::InverseRight);
                    datapath.signal_alu_perform(
                        AluOpCode::Add,
                        true,
                        AluOutSel::Registers(alter.arg),
                    );
                    Ok(ControlUnitState::cycle_start())
                },
                _ => Err(MachineError::InvalidTickCall),
            },
        }
    }

    fn execute_io(&self, io: &IoOpWord, tick_count: u8, datapath: &mut DataPath<M>) -> CUResult {
        match tick_count {
            1 => {
                let port_id = io.arg;
                match io.opcode {
                    IoOp::In => datapath.signal_ports(PortsMuxSel::In(port_id))?,
                    IoOp::Out => datapath.signal_ports(PortsMuxSel::Out(port_id))?,
                };
                Ok(ControlUnitState::cycle_start())
            },
            _ => Err(MachineError::InvalidTickCall),
        }
    }

    fn execute_control(
        &mut self,
        control: &ControlOpWord,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match control.0 {
            ControlOp::Exit => match tick_count {
                1 => Err(MachineError::Exit),
                _ => Err(MachineError::InvalidTickCall),
            },
            ControlOp::Div => {
                match tick_count {
                    // -----------PREPARATION-----------
                    // 1 tick - regs[ecx] -> operand_buf && count_buf++
                    // 2 tick - 0 -> regs[edx]
                    // 3 tick - -regs[ebx] -> regs[ecx]
                    // 4 tick - regs[ebx] -> regs[ebx] + flags
                    // 5 tick -
                    //      if !SF: count_buf++, ERR if count_buf > 32 (ebx = 0)
                    //      else: GOTO tick 8
                    // 6 tick - regs[ecx] << 1 -> regs[ecx]
                    // 7 tick - regs[ebx] << 1 -> regs[ebx]; GOTO tick 4
                    1 => {
                        self.count_buf += 1;

                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Count));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Decoder { dest: &mut self.operand_buf },
                        );
                    },
                    2 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Data),
                        );
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Base));
                        datapath.signal_alu(AluSignal::InverseLeft);
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Count),
                        );
                    },
                    4 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Base));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            true,
                            AluOutSel::Registers(Base),
                        );
                    },
                    5 => {
                        if !datapath.flags().sign {
                            self.count_buf += 1;
                            if self.count_buf > 32 {
                                return Err(MachineError::InvalidDivisionCall);
                            }
                        } else {
                            return Ok(ControlUnitState::ExecuteInstruction { tick_count: 8 });
                        }
                    },
                    6 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Count));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Shl,
                            false,
                            AluOutSel::Registers(Count),
                        );
                    },
                    7 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Base));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Shl,
                            true,
                            AluOutSel::Registers(Base),
                        );
                        return Ok(ControlUnitState::ExecuteInstruction { tick_count: 4 });
                    },
                    // ------------DIVISION------------
                    // 8 tick - regs[eax] + regs[ecx] -> regs[eax] + flags
                    // 9 tick -
                    //      IF !CF: regs[eax] + regs[ebx] -> regs[eax]
                    //      ELSE: regs[edx] + 1 -> regs[edx]
                    // 10 tick - count_buf--; IF count_buf == 0: GOTO tick 15
                    // 11 tick - regs[ecx] + 1 -> regs[ecx]
                    // 12 tick - regs[ecx] ROR 1 -> regs[ecx]
                    // 13 tick - regs[ebx] >> 1 -> regs[ebx]
                    // 14 tick - regs[edx] << 1 -> regs[edx]; GOTO tick 8
                    8 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Accumulator));
                        datapath.signal_right_alu(AluRightMuxSel::Registers(Count));
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            true,
                            AluOutSel::Registers(Accumulator),
                        );
                    },
                    9 => {
                        if !datapath.flags().carry {
                            datapath.signal_left_alu(AluLeftMuxSel::Registers(Accumulator));
                            datapath.signal_right_alu(AluRightMuxSel::Registers(Base));
                            datapath.signal_alu_perform(
                                AluOpCode::Add,
                                false,
                                AluOutSel::Registers(Accumulator),
                            );
                        } else {
                            datapath.signal_left_alu(AluLeftMuxSel::Registers(Data));
                            datapath.signal_alu(AluSignal::ResetRight);
                            datapath.signal_alu(AluSignal::AddRightOne);
                            datapath.signal_alu_perform(
                                AluOpCode::Add,
                                false,
                                AluOutSel::Registers(Data),
                            );
                        }
                    },
                    10 => {
                        self.count_buf -= 1;
                        if self.count_buf == 0 {
                            return Ok(ControlUnitState::ExecuteInstruction { tick_count: 15 });
                        }
                    },
                    11 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Count));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Count),
                        );
                    },
                    12 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Count));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Ror,
                            false,
                            AluOutSel::Registers(Count),
                        );
                    },
                    13 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Base));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Shr,
                            false,
                            AluOutSel::Registers(Base),
                        );
                    },
                    14 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Data));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Shl,
                            false,
                            AluOutSel::Registers(Data),
                        );
                        return Ok(ControlUnitState::ExecuteInstruction { tick_count: 8 });
                    },
                    // -----------CONCLUSION-----------
                    // 15 tick - operand_buf -> regs[ecx]
                    // 16 tick - regs[edx] -> operand_buf
                    // 17 tick - regs[eax] -> regs[edx]
                    // 18 tick - operand_buf -> regs[eax]
                    15 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Count),
                        );
                    },
                    16 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Data));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Decoder { dest: &mut self.operand_buf },
                        );
                    },
                    17 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(Accumulator));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Data),
                        );
                    },
                    18 => {
                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(Accumulator),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }

                Ok(ControlUnitState::ExecuteInstruction { tick_count: tick_count + 1 })
            },
        }
    }

    fn execute_stack(
        &self,
        stack: &StackOpHigher,
        tick_count: u8,
        datapath: &mut DataPath<M>,
    ) -> CUResult {
        match stack.opcode {
            StackOp::Push => {
                // 1 tick - regs[esp] - 1 -> regs[esp]
                // 2 tick - regs[esp] -> mem_addr
                // 3 tick -
                //  REG: regs[REG] -> mem_in_buf
                //  IMMED: operand_buf -> mem_in_buf
                // 4 tick - mem_in_buf -> memory[mem_addr]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::InverseRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(StackPointer),
                        );
                    },
                    2 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    3 => match stack.args {
                        StackOpHigherArgs::Register(id) => {
                            datapath.signal_left_alu(AluLeftMuxSel::Registers(id));
                            datapath.signal_alu(AluSignal::ResetRight);
                            datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemInBuf);
                        },
                        StackOpHigherArgs::Immed(_) => {
                            datapath.signal_alu(AluSignal::ResetLeft);
                            datapath.signal_right_alu(AluRightMuxSel::Decoder {
                                src: &self.operand_buf,
                            });
                            datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemInBuf);
                        },
                        StackOpHigherArgs::None => return Err(MachineError::InvalidTickCall),
                    },
                    4 => {
                        datapath.signal_write();
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            StackOp::Pop => {
                let id = match stack.args {
                    StackOpHigherArgs::Register(id) => id,
                    StackOpHigherArgs::Immed(_) | StackOpHigherArgs::None => {
                        return Err(MachineError::InvalidTickCall)
                    },
                };
                // 1 tick - regs[esp] -> mem_addr
                // 2 tick - (memory[mem_addr] -> regs[REG]) && regs[esp] + 1 -> regs[esp]
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::Registers(id));

                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(StackPointer),
                        );

                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            StackOp::Call => {
                // 1 tick - regs[esp] - 1 -> regs[esp]
                // 2 tick - regs[esp] -> mem_addr
                // 3 tick - regs[eip] -> mem_in_buf
                // 4 tick - (mem_in_buf -> memory[mem_addr]) && (operand_buf -> regs[eip])
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::InverseRight);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(StackPointer),
                        );
                    },
                    2 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    3 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemInBuf);
                    },
                    4 => {
                        datapath.signal_write();

                        datapath.signal_alu(AluSignal::ResetLeft);
                        datapath
                            .signal_right_alu(AluRightMuxSel::Decoder { src: &self.operand_buf });
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(InstructionPointer),
                        );
                        return Ok(ControlUnitState::default());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
            StackOp::Ret => {
                // 1 tick - regs[esp] -> mem_addr
                // 2 tick - (memory[mem_addr] -> eip) && (regs[esp] + 1 -> esp)
                match tick_count {
                    1 => {
                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
                    },
                    2 => {
                        datapath.signal_read(ReadOutSel::Registers(InstructionPointer));

                        datapath.signal_left_alu(AluLeftMuxSel::Registers(StackPointer));
                        datapath.signal_alu(AluSignal::ResetRight);
                        datapath.signal_alu(AluSignal::AddRightOne);
                        datapath.signal_alu_perform(
                            AluOpCode::Add,
                            false,
                            AluOutSel::Registers(StackPointer),
                        );
                        return Ok(ControlUnitState::cycle_start());
                    },
                    _ => return Err(MachineError::InvalidTickCall),
                }
            },
        }
        Ok(ControlUnitState::ExecuteInstruction { tick_count: tick_count + 1 })
    }
}

/// Состояние процессора. Является "заменой" микрокода.
#[derive(Debug)]
enum ControlUnitState {
    /// Цикл выборки инструкции
    FetchInstruction { tick_count: u8 },
    /// Цикл выборки операнда.
    FetchOperand { tick_count: u8 },
    /// Цикл исполнения инструкции
    ExecuteInstruction { tick_count: u8 },
}
impl ControlUnitState {
    pub fn cycle_start() -> Self {
        Self::FetchInstruction { tick_count: 1 }
    }
}
impl Default for ControlUnitState {
    fn default() -> Self {
        Self::FetchInstruction { tick_count: 2 }
    }
}

/// ```ignore
///                               ┌───────────────────────┬────────────────────────┬───────────────┐
///                               │                       │                        │               │
///                               │ latch                 │       latch            │   latch       │
///                ┌──────────────┴┐instruction buf  ┌────┴──────┐operand buf   ┌──┴──┐eip         │
///                │instruction buf│◄──────────────  │operand buf│◄──────────   │ eip │◄──         ▼
///                └───────────────┘                 └────────┬──┘              └┬───┬┘        ┌──────────────┐
///                 ▲                                 ▲       │                  │ ▲ │         │   DECODER    │
///                 │                                 │       ├──────────────────┘ │ │         │              │
///                 │                                 │       │                    │ │         │              │
///                 │                                 │       │                    │ │         ├────────┐     │
///                 └──────────────────────────┬──────┴───────┼────────────────────┘ │         │ cycle  │     │
///                                            │              │                      │         │ counter│     │
///                                        sel┌┴──┐           │                      │         └────────┴────┬┘
///                                        ──►│MUX│           │                      │                    ▲  │
///                                           └───┘           │     ┌────────────────┘                    │  │
///                                            ▲ ▲            │     │                                     │  │
///                                          ┌─┘ └───┐        │     │             ┌───────────────────────┘  │
///                                          │from   │from    │to   │ to          │from                      │
///                                          │ALU    │memory  ▼ALU  ▼ mem addr    │FLAGS                     │
///                                         ┌┴───────┴────────────────────────────┴─┐                        │
///                                         │                                       │                        │
///                                         │                 DATAPATH              │                        │
///                                         │                                       │                        │
///                                         │                                       │                        │
///                                         │                                       │SIGNALS                 │
///                                         │                                       │◄───────────────────────┘
/// ┌────────────────────┐         to/from  │                                       │
/// │  EXTERNAL DEVICES  │         regs     │                                       │
/// │  (PORTS)           │◄──────────────►  │                                       │
/// │                    │                  │                                       │
/// │                    │                  │                                       │
/// │                    │                  │                                       │
/// └────────────────────┘                  │                                       │
///                                         │                                       │
///                                         │                                       │
///                                         └───────────────────────────────────────┘
/// ```
#[derive(Default, Debug)]
pub struct ControlUnit<const MEM_SIZE: usize, const TICK_LIMIT: usize> {
    /// Блок обработки данных
    datapath: DataPath<MEM_SIZE>,
    /// Декодер инструкций
    decoder: InstructionDecoder<MEM_SIZE>,
    /// Текущее состояние
    state: ControlUnitState,
    /// Номер текущего такта
    tick: usize,
    /// Номер текущей инструкции
    instruction: usize,
}
impl<const MEM_SIZE: usize, const TICK_LIMIT: usize> ControlUnit<MEM_SIZE, TICK_LIMIT> {
    pub fn new(datapath: DataPath<MEM_SIZE>) -> Self {
        Self { datapath, ..Default::default() }
    }

    pub fn current_state(&self) -> (usize, usize) {
        (self.tick, self.instruction)
    }

    pub fn tick(&mut self) -> Result<(), MachineError> {
        self.tick += 1;
        if self.tick > TICK_LIMIT {
            return Err(MachineError::TickLimitReached(TICK_LIMIT));
        }
        self.state = match self.state {
            ControlUnitState::FetchInstruction { tick_count } => self.fetch_instruction(tick_count),
            ControlUnitState::FetchOperand { tick_count } => {
                self.decoder.fetch_operand(tick_count, &mut self.datapath)
            },
            ControlUnitState::ExecuteInstruction { tick_count } => {
                if tick_count == 1 {
                    self.instruction += 1;
                    debug!("{:#}", self);
                }
                self.decoder.execute(tick_count, &mut self.datapath)
            },
        }?;
        Ok(())
    }

    fn fetch_instruction(&mut self, tick_count: u8) -> CUResult {
        match tick_count {
            // 1 tick - regs[eip] + 1 -> regs[eip]
            // 2 tick - regs[eip] -> mem_addr
            // 3 tick - memory[mem_addr] -> instruction
            // 4 tick - decode instruction
            1 => {
                self.datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                self.datapath.signal_alu(AluSignal::ResetRight);
                self.datapath.signal_alu(AluSignal::AddRightOne);
                self.datapath.signal_alu_perform(
                    AluOpCode::Add,
                    false,
                    AluOutSel::Registers(InstructionPointer),
                );
            },
            2 => {
                self.datapath.signal_left_alu(AluLeftMuxSel::Registers(InstructionPointer));
                self.datapath.signal_alu(AluSignal::ResetRight);
                self.datapath.signal_alu_perform(AluOpCode::Add, false, AluOutSel::MemAddr);
            },
            3 => {
                self.datapath
                    .signal_read(ReadOutSel::Decoder { dest: &mut self.decoder.instruction });
            },
            4 => {
                return self.decoder.decode();
            },
            _ => return Err(MachineError::InvalidTickCall),
        };
        Ok(ControlUnitState::FetchInstruction { tick_count: tick_count + 1 })
    }
}
impl<const MEM_SIZE: usize, const TICK_LIMIT: usize> Display for ControlUnit<MEM_SIZE, TICK_LIMIT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "TICK: {tick:4}; {datapath}; INSTR: {instr:?}; STATE: {state:?}",
            tick = self.tick,
            datapath = self.datapath,
            instr = self.decoder.instruction,
            state = self.state
        ))
    }
}
