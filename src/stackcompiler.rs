use crate::parser::{BinaryOp, ElseBranch, Expression, Literal, Statement, UnaryOp};

#[derive(Debug)]
pub enum Value {
    Number(f32)
}

#[derive(Debug)]
pub enum Instruction {
    Push(Value),
    LoadVar(String),
    StoreVar(String),

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    Negate,
    Bang,

    Call(usize),
    Return,

    JumpIfFalse(usize),
    Jump(usize),

    Pop,
    Print,
    Halt
}

pub struct Compiler {
    pub code: Vec<Instruction>
}

impl Compiler {
    pub fn new() -> Self { Self { code: Vec::new() } }

    pub fn compile_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal(Literal::Number(n)) => self.code.push(Instruction::Push(Value::Number(*n))),
            Expression::Variable(name) => self.code.push(Instruction::LoadVar(name.clone())),
            Expression::Unary { op, expression } => {
                self.compile_expression(expression);
                match op {
                    UnaryOp::Negate => self.code.push(Instruction::Negate),
                    UnaryOp::Bang => self.code.push(Instruction::Bang),
                    _ => {}
                }
            },
            Expression::Binary { left, op, right } => {
                self.compile_expression(left);
                self.compile_expression(right);

                let instr = match op {
                    BinaryOp::Add => Instruction::Add,
                    BinaryOp::Sub => Instruction::Sub,
                    BinaryOp::Mul => Instruction::Mul,
                    BinaryOp::Div => Instruction::Div,
                    BinaryOp::Mod => Instruction::Mod,
                    BinaryOp::Equal => Instruction::Equal,
                    BinaryOp::NotEqual => Instruction::NotEqual,
                    BinaryOp::Less => Instruction::Less,
                    BinaryOp::LessEqual => Instruction::LessEqual,
                    BinaryOp::GreaterEqual => Instruction::GreaterEqual,
                    BinaryOp::Greater => Instruction::Greater,
                };

                self.code.push(instr);
            },
            Expression::Call { callee, args } => {
                for arg in args {
                    self.compile_expression(arg);
                }

                self.compile_expression(callee);
                self.code.push(Instruction::Call(args.len()));
            }
        }
    }

    pub fn compile_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declaration { name, value } => {
                self.compile_expression(value);
                self.code.push(Instruction::StoreVar(name.clone()));
            },
            Statement::Assignment { name, value } => {
                self.compile_expression(value);
                self.code.push(Instruction::StoreVar(name.clone()));
            },
            Statement::Expression(expr) => {
                self.compile_expression(expr);
                self.code.push(Instruction::Pop);
            },
            Statement::Return(expr) => {
                self.compile_expression(expr);
                self.code.push(Instruction::Return);
            },
            Statement::If { condition, then_branch, else_branch } => {
                self.compile_expression(condition);
                let jump_if_false_pos = self.code.len();

                self.code.push(Instruction::JumpIfFalse(0));

                for stmt in then_branch {
                    self.compile_statement(stmt);
                }

                let jump_over_else_pos = self.code.len();
                self.code.push(Instruction::Jump(0));

                let else_start = self.code.len();

                match else_branch {
                    Some(ElseBranch::Else(stmts)) => {
                        for stmt in stmts {
                            self.compile_statement(&stmt);
                        }
                    },
                    Some(ElseBranch::If(stmt)) => {
                        self.compile_statement(stmt);
                    },
                    _ => {}
                }

                let end = self.code.len();

                self.code[jump_if_false_pos] = Instruction::JumpIfFalse(else_start);
                self.code[jump_over_else_pos] = Instruction::Jump(end);
            },
            Statement::While { condition, body } => {
                let loop_start = self.code.len();
                self.compile_expression(condition);
                let jump_exit_pos = self.code.len();
                self.code.push(Instruction::JumpIfFalse(0));
                
                for stmt in body {
                    self.compile_statement(stmt);
                }

                self.code.push(Instruction::Jump(loop_start));
                let loop_end = self.code.len();
                self.code[jump_exit_pos] = Instruction::JumpIfFalse(loop_end);
            },
            Statement::Function { .. } => { todo!() }
        }
    }

    pub fn compile_program(&mut self, program: &Vec<Statement>) {
        for stmt in program {
            self.compile_statement(stmt);
        }
    }
}
