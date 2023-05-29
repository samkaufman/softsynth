use clap::Parser;
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

// 11 with 3 regs. took 211 seconds.
// Should be able to do 13 in 5 hours.
const MAX_PROG_SIZE: usize = 20;
const DATA_SIZE: usize = 3;
const REGISTERS: u8 = 3;
const LOOP_DATA_NEEDS_READ: bool = true;
const LOOP_REG: u8 = 0;
const LOOP_ITER_SETS_ACTIVE_REG: bool = true;
const INCLUDE_DATA_INST: bool = false;

// TODO: Actually check against tests other than the first.
const TESTS: [([Val; DATA_SIZE], [Val; DATA_SIZE]); 4] = [
    ([-1.0, 3.0, 2.0], [0.01321289, 0.7213992, 0.26538795]),
    ([-1.0, 3.0, 1000.0], [0.0, 0.0, 1.0]), // overflowing example
    ([0.0, 1.0, 2.0], [0.09003057, 0.24472846, 0.6652409]),
    ([0.0, 0.0, 0.0], [0.33333334, 0.33333334, 0.33333334]),
];

type Val = f32;

#[derive(Parser)]
#[command(long_about=None)]
struct Args {
    #[arg(long)]
    naive_checkpoints: bool,
    #[arg(long)]
    safe_checkpoints: bool,
    #[arg(long)]
    online_checkpoints: bool,
    prog_size: usize,
    data_budget: u8,
}

#[derive(Clone, Copy, Debug)]
enum Inst {
    SetReg(u8),
    Data,
    NegInf,
    Add(u8),
    Mult(u8),
    Neg,
    Max(u8),
    Copy(u8),
    Div(u8),
    Epow,
    Do,
    Loop,
}

enum CheckpointStatus {
    Hit,
    Unhit,
    Failed,
}

#[derive(Clone)]
struct Interpreter<const N: usize> {
    input_data: &'static [Val],
    expected_result: &'static [Val],
    outputs: [Val; DATA_SIZE],
    active_register: u8,
    registers: [Val; REGISTERS as usize],
    loop_block: [Inst; N],
    loop_block_size: usize,
    in_loop: bool,
    register_needs_read: [bool; REGISTERS as usize],
    data_budget: u8,
    last_instruction_run: Option<Inst>,
}

#[derive(Error, Debug)]
enum InterpretError {
    #[error("Data budget exhausted")]
    DataBudgetExhausted,
    #[error("Clobbering register needing read")]
    ClobbersRegister,
    #[error("Data given invalid index")]
    DataInvalidIndex,
    #[error("Empty loop")]
    EmptyLoop,
}

impl<const N: usize> Interpreter<N> {
    fn interpret(&mut self, inst: &Inst) -> Result<(), InterpretError> {
        if self.in_loop && !matches!(inst, Inst::Loop) {
            debug_assert!(self.loop_block_size < N, "Program: {:?}", self.loop_block);
            self.loop_block[self.loop_block_size] = *inst;
            self.loop_block_size += 1;
        }

        let data_len: u8 = self.input_data.len().try_into().unwrap();

        match inst {
            Inst::SetReg(i) => {
                debug_assert!(*i < REGISTERS);
                debug_assert_ne!(*i, self.active_register);
                self.active_register = *i;
            }
            Inst::Data => {
                debug_assert_ne!(self.data_budget, 0);
                if self.data_budget == 0 {
                    return Err(InterpretError::DataBudgetExhausted);
                }
                self.data_budget -= 1;
                let ar = usize::from(self.active_register);
                // Use nearest integer as index.
                if self.register_needs_read[ar] {
                    return Err(InterpretError::ClobbersRegister);
                }
                let nearest = self.registers[ar].round() as usize;
                if nearest >= self.input_data.len() {
                    return Err(InterpretError::DataInvalidIndex);
                }
                self.register_needs_read[ar] = true;
                self.registers[ar] = self.input_data[nearest];
            }
            Inst::NegInf => {
                let ar = usize::from(self.active_register);
                self.register_needs_read[ar] = true;
                self.registers[ar] = Val::NEG_INFINITY;
            }
            Inst::Add(other_reg) => {
                let ar = usize::from(self.active_register);
                let other_reg = usize::from(*other_reg);
                self.register_needs_read[other_reg] = false;
                self.register_needs_read[ar] = true;
                self.registers[ar] += self.registers[other_reg];
            }
            Inst::Mult(other_reg) => {
                let ar = usize::from(self.active_register);
                let other_reg = usize::from(*other_reg);
                self.register_needs_read[other_reg] = false;
                self.register_needs_read[ar] = true;
                self.registers[ar] *= self.registers[other_reg];
            }
            Inst::Max(other_reg) => {
                let ar = usize::from(self.active_register);
                let other_reg = usize::from(*other_reg);
                self.register_needs_read[other_reg] = false;
                self.register_needs_read[ar] = true;
                self.registers[ar] = self.registers[ar].max(self.registers[other_reg]);
            }
            Inst::Neg => {
                let ar = usize::from(self.active_register);
                self.register_needs_read[ar] = true;
                self.registers[ar] = -self.registers[ar];
            }
            Inst::Copy(src) => {
                let ar = usize::from(self.active_register);
                let src = usize::from(*src);
                debug_assert_ne!(ar, src);
                if self.register_needs_read[ar] {
                    return Err(InterpretError::ClobbersRegister);
                }
                self.register_needs_read[src] = false;
                self.register_needs_read[ar] = true;
                self.registers[ar] = self.registers[src];
            }
            Inst::Div(other_reg) => {
                let ar = usize::from(self.active_register);
                let other_reg = usize::from(*other_reg);
                let divisor = self.registers[other_reg];
                self.register_needs_read[ar] = true;
                self.registers[ar] /= divisor;
            }
            Inst::Epow => {
                let ar = usize::from(self.active_register);
                self.register_needs_read[ar] = true;
                self.registers[ar] = self.registers[ar].exp();
            }
            Inst::Do => {
                debug_assert!(!self.in_loop);
                if self.data_budget < data_len {
                    return Err(InterpretError::DataBudgetExhausted);
                }
                self.in_loop = true;
                self.data_budget -= data_len;
                self.init_loop_iteration(0);
            }
            Inst::Loop => {
                debug_assert!(self.in_loop);
                if self.loop_block_size == 0 {
                    return Err(InterpretError::EmptyLoop);
                }
                self.outputs[0] = self.registers[usize::from(self.active_register)];

                self.in_loop = false;
                for iteration in 1..self.input_data.len() {
                    self.init_loop_iteration(iteration);
                    let block_copy = Vec::from(&self.loop_block[..self.loop_block_size]);
                    for inst in &block_copy {
                        self.interpret(inst)?;
                    }
                    // Yield active register from this subsequent iteration.
                    self.outputs[iteration] = self.registers[usize::from(self.active_register)];
                }
                self.loop_block_size = 0;
            }
        }

        self.last_instruction_run = Some(*inst);
        Ok(())
    }

    fn init_loop_iteration(&mut self, iteration: usize) {
        let lr = usize::from(LOOP_REG);
        self.register_needs_read[lr] = LOOP_DATA_NEEDS_READ;
        self.registers[lr] = self.input_data[iteration];
        if LOOP_ITER_SETS_ACTIVE_REG {
            self.active_register = LOOP_REG;
        }
    }

    pub fn output_is_softmax(&self) -> bool {
        for (o, e) in self.outputs.iter().zip(self.expected_result) {
            if !approx::abs_diff_eq!(o, e) {
                return false;
            }
        }
        true
    }
}

fn possible_instructions(
    active_register: u8,
    remaining_insts: usize,
    in_loop: bool,
    last_instruction_run: Option<Inst>,
    body: &mut impl FnMut(Inst) -> bool,
) {
    if remaining_insts >= 3 && !in_loop && !body(Inst::Do) {
        return;
    }
    if in_loop {
        if !body(Inst::Loop) {
            return;
        }
        // If this is the last instruction and we're in a loop, we need to
        // finish the loop.
        if remaining_insts == 1 {
            return;
        }
    }

    if !matches!(last_instruction_run, Some(Inst::NegInf)) && !body(Inst::NegInf) {
        return;
    }

    if INCLUDE_DATA_INST && !body(Inst::Data) {
        return;
    }
    if !matches!(last_instruction_run, Some(Inst::Neg)) && !body(Inst::Neg) {
        return;
    }
    if !body(Inst::Epow) {
        return;
    }

    for offset in 1..=(REGISTERS - 1) {
        let offset_reg = (active_register + offset) % REGISTERS;

        if !matches!(last_instruction_run, Some(Inst::SetReg(_))) && !body(Inst::SetReg(offset_reg))
        {
            return;
        }
        if !body(Inst::Copy(offset_reg)) {
            return;
        }

        match last_instruction_run {
            Some(Inst::Add(r)) if r <= offset_reg => {}
            _ => {
                if !body(Inst::Add(offset_reg)) {
                    return;
                }
            }
        };

        match last_instruction_run {
            Some(Inst::Mult(r)) if r <= offset_reg => {}
            _ => {
                if !body(Inst::Mult(offset_reg)) {
                    return;
                }
            }
        };

        match last_instruction_run {
            Some(Inst::Max(r)) if r <= offset_reg => {}
            _ => {
                if !body(Inst::Max(offset_reg)) {
                    return;
                }
            }
        };

        if !body(Inst::Div(offset_reg)) {
            return;
        }
    }
}

fn iter_and_interp<const N: usize>(
    program_len: usize,
    last_interp: &Interpreter<N>,
    checkpoints: &SmallVec<[&dyn Fn(&Interpreter<N>, usize) -> CheckpointStatus; 2]>,
) -> Option<Vec<Inst>> {
    debug_assert!(program_len <= N);
    debug_assert!(program_len > 0);

    let mut result: Option<Vec<Inst>> = None;
    possible_instructions(
        last_interp.active_register,
        program_len,
        last_interp.in_loop,
        last_interp.last_instruction_run,
        &mut |inst| match inner_iter_and_interp(&inst, program_len, last_interp, checkpoints) {
            Ok(Some(v)) => {
                result = Some(v);
                false
            }
            Ok(None) | Err(_) => true,
        },
    );
    result
}

fn inner_iter_and_interp<const N: usize>(
    inst: &Inst,
    program_len: usize,
    last_interp: &Interpreter<N>,
    checkpoints: &SmallVec<[&dyn Fn(&Interpreter<N>, usize) -> CheckpointStatus; 2]>,
) -> Result<Option<Vec<Inst>>, InterpretError> {
    let mut new_interpreter = last_interp.clone();
    new_interpreter.interpret(inst)?;

    if program_len == 1 && new_interpreter.output_is_softmax() {
        return Ok(Some(vec![*inst]));
    }

    let mut remaining_checkpoints = checkpoints.clone();
    for (idx, check) in checkpoints.iter().enumerate().rev() {
        match check(&new_interpreter, program_len) {
            CheckpointStatus::Hit => {
                remaining_checkpoints.remove(idx);
            }
            CheckpointStatus::Unhit => {}
            CheckpointStatus::Failed => {
                return Ok(None);
            }
        }
    }

    match program_len {
        0 => unreachable!(),
        1 => {
            if new_interpreter.output_is_softmax() {
                return Ok(Some(vec![*inst]));
            }
        }
        _ => {
            if let Some(correct_program) =
                iter_and_interp(program_len - 1, &new_interpreter, &remaining_checkpoints)
            {
                let mut correct_program = correct_program;
                correct_program.insert(0, *inst);
                return Ok(Some(correct_program));
            }
        }
    }

    Ok(None)
}

fn make_check_register_values<const N: usize>(
    expected: Vec<Val>,
    deadline: usize,
) -> Box<dyn Fn(&Interpreter<N>, usize) -> CheckpointStatus> {
    Box::new(move |interp, program_len| {
        let predicate_hit = interp.registers.iter().any(|&r| {
            expected
                .iter()
                .any(|&e| r == e || approx::abs_diff_eq!(r, e))
        });
        if predicate_hit {
            CheckpointStatus::Hit
        } else if deadline < program_len {
            CheckpointStatus::Unhit
        } else {
            CheckpointStatus::Failed
        }
    })
}

fn main() {
    let args = Args::parse();
    let prog_size = args.prog_size;
    let data_budget = args.data_budget;
    assert!(prog_size <= MAX_PROG_SIZE);

    let base_interp = Interpreter::<MAX_PROG_SIZE> {
        input_data: &TESTS[0].0[..],
        expected_result: &TESTS[0].1[..],
        outputs: [0.0; DATA_SIZE],
        active_register: 0,
        registers: [0.0; REGISTERS as usize],
        loop_block: [Inst::SetReg(0); MAX_PROG_SIZE],
        loop_block_size: 0,
        in_loop: false,
        register_needs_read: [false; REGISTERS as usize],
        data_budget, // The naive version is 6.
        last_instruction_run: None,
    };


    let mut checkpoints: SmallVec<[_; 2]> = smallvec![];

    if args.naive_checkpoints {
        let input_e_sum = base_interp.input_data.iter().map(|v| v.exp()).sum();
        checkpoints.push(make_check_register_values(
            vec![input_e_sum, -input_e_sum],
            prog_size - 6,
        ));
    }

    if args.safe_checkpoints {
        let input_max = base_interp
            .input_data
            .iter()
            .cloned()
            .fold(Val::NEG_INFINITY, f32::max);
        checkpoints.push(make_check_register_values(
            vec![input_max, -input_max],
            prog_size - 5,
        ));

        let input_diff_e_sum = base_interp
            .input_data
            .iter()
            .map(|v| (v - input_max).exp())
            .sum();
        checkpoints.push(make_check_register_values(
            vec![input_diff_e_sum, -input_diff_e_sum],
            prog_size - 10,
        ));
    }

    if args.online_checkpoints {
        checkpoints.push(make_check_register_values(
            vec![-Val::NEG_INFINITY],
            prog_size - 2,
        ));
        checkpoints.push(make_check_register_values(
            vec![-base_interp.input_data[0]],
            prog_size - 5,
        ));
        checkpoints.push(make_check_register_values(vec![1.0, -1.0], prog_size - 8));
    }

    let start_time = std::time::Instant::now();
    if let Some(correct_program) = iter_and_interp(
        prog_size,
        &base_interp,
        &checkpoints.iter().map(|c| c.as_ref()).collect(),
    ) {
        println!("Program of length {} found.", prog_size);
        for inst in correct_program {
            println!("{:?}", inst);
        }
    } else {
        println!("No correct program of length {} found.", prog_size);
    }
    println!(
        "Time taken for prog. len {}: {:?}",
        prog_size,
        start_time.elapsed(),
    );
}
