use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{BinaryOp, ElseBranch, Expression, Literal, Statement, UnaryOp};

pub enum RuntimeError {
    NotEnoughArguments {
        expected: usize,
        got: usize
    },
    UndefinedVariable(String),
    DivisionByZero,
    NotCallable,
    NonBooleanCondition,
    InvalidUnary,
    InvalidBinary,

    // SPECIAL NEED TO FIX,
    Return(Value)
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotEnoughArguments { expected, got } => f.write_str(&format!("Not enough arguments, expected {expected} but got {got}")),
            Self::UndefinedVariable(name) => f.write_str(&format!("Undefined variable `{name}`")),
            Self::DivisionByZero => f.write_str("Division by zero"),
            Self::NotCallable => f.write_str("Not callable"),
            Self::NonBooleanCondition => f.write_str("Non boolean condition"),
            Self::InvalidUnary => f.write_str("Invalid unary"),
            Self::InvalidBinary => f.write_str("Invalid binary"),
            Self::Return(v) => f.write_str(&format!("Returned `{v}`")),
        }
    } 
}

pub type NativeFn = Rc<dyn Fn(Vec<Value>) -> Result<Value, RuntimeError>>;

#[derive(Clone)]
pub enum Function {
    UserDefined {
        params: Vec<String>,
        body: Vec<Statement>,
        closure: Environment
    },
    Native {
        arity: usize,
        func: NativeFn
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f32),
    Function(Function),
    Boolean(bool),
    Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => f.write_str(&format!("{n}")),
            Self::Function(_) => f.write_str(&format!("function")),
            Self::Boolean(b) => f.write_str(&format!("{b}")),
            Self::Unit => f.write_str("unit")
        }        
    }
}

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(parent) 
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().assign(name, value)
        } else {
            Err(RuntimeError::UndefinedVariable(name.to_string()))
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(val) = self.values.get(name) {
            Ok(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            Err(RuntimeError::UndefinedVariable(name.to_string()))
        }
    }
}

pub struct Interpreter {
    pub env: Rc<RefCell<Environment>>
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new()))
        }
    }

    pub fn eval_expression(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        match expr {
            Expression::Literal(Literal::Number(n)) => Ok(Value::Number(*n)),
            Expression::Variable(name) => self.env.borrow().get(name),
            Expression::Unary { op, expression } => {
                let val = self.eval_expression(expression)?;
                match (op, val) {
                    (UnaryOp::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
                    (UnaryOp::Bang, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                    _ => Err(RuntimeError::InvalidUnary)
                }
            },
            Expression::Binary { left, op, right } => {
                let l = self.eval_expression(left)?;
                let r = self.eval_expression(right)?;

                match (op, l, r) {
                    (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l+r)),
                    (BinaryOp::Sub, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l-r)),
                    (BinaryOp::Mul, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l*r)),
                    (BinaryOp::Div, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l/r)),
                    (BinaryOp::Mod, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l%r)),
                    (BinaryOp::Less, Value::Number(l), Value::Number(r))         => Ok(Value::Boolean(l < r)),
                    (BinaryOp::LessEqual, Value::Number(l), Value::Number(r))    => Ok(Value::Boolean(l <= r)),
                    (BinaryOp::GreaterEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l >= r)),
                    (BinaryOp::Greater, Value::Number(l), Value::Number(r))      => Ok(Value::Boolean(l > r)),
                    (BinaryOp::Equal, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l == r)),
                    (BinaryOp::Equal, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l == r)),
                    (BinaryOp::NotEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l != r)),
                    (BinaryOp::NotEqual, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l != r)),
                    _ => Err(RuntimeError::InvalidBinary),
                }
            },
            Expression::Call { callee, args } => {
                match self.eval_expression(callee)? {
                    Value::Function(Function::Native { arity, func }) => {
                        if arity != args.len() {
                            return Err(RuntimeError::NotEnoughArguments {
                                expected: arity,
                                got: args.len()
                            })
                        }

                        let evaluated_args = args
                            .iter()
                            .map(|arg| self.eval_expression(arg))
                            .collect::<Result<Vec<_>, _>>()?;

                        func(evaluated_args)
                    },
                    Value::Function(Function::UserDefined { params, body, .. }) => {
                        if params.len() != args.len() {
                            return Err(RuntimeError::NotEnoughArguments {
                                expected: params.len(),
                                got: args.len()
                            })
                        }

                        let mut new_env = Environment::with_parent(Rc::clone(&self.env));

                        for (param, arg) in params.iter().zip(args.iter()) {
                            let value = self.eval_expression(arg)?;
                            new_env.define(param.clone(), value);
                        }

                        let mut interpreter = Interpreter {
                            env: Rc::new(RefCell::new(new_env))
                        };

                        interpreter.interpret_program(body)
                    },
                    _ => Err(RuntimeError::NotCallable)
                }
            }
        }
    }

    pub fn eval_statement(&mut self, stmt: &Statement) -> Result<(), RuntimeError> {
        match stmt {
            Statement::Declaration { name, value } => {
                let val = self.eval_expression(value)?;
                self.env.borrow_mut().define(name.clone(), val);
                Ok(())
            },
            Statement::Assignment { name, value } => {
                let value = self.eval_expression(value)?;
                self.env.borrow_mut().assign(name, value)
            },
            Statement::Return(expr) => {
                let val = self.eval_expression(expr)?;
                Err(RuntimeError::Return(val))
            },
            Statement::Expression(expr) => {
                self.eval_expression(expr)?;
                Ok(())
            },
            Statement::If { condition, then_branch, else_branch } => {
                let cond = self.eval_expression(condition)?;

                match cond {
                    Value::Boolean(true) => {
                        for stmt in then_branch {
                            self.eval_statement(stmt)?;
                        };
                        Ok(())
                    },
                    Value::Boolean(false) => {
                        match else_branch {
                            Some(ElseBranch::If(stmt)) => self.eval_statement(stmt),
                            Some(ElseBranch::Else(stmts)) => {
                                for stmt in stmts {
                                    self.eval_statement(stmt)?;
                                }
                                Ok(())
                            },
                            _ => Ok(())
                        }
                    },
                    _ => return Err(RuntimeError::NonBooleanCondition),
                }
            },
            Statement::While { condition, body } => {
                while matches!(self.eval_expression(condition)?, Value::Boolean(true)) {
                    for stmt in body {
                        self.eval_statement(stmt)?;
                    }
                }
                Ok(())
            },
            Statement::Function { name, params, body } => {
                let func = Function::UserDefined {
                    params: params.clone(),
                    body: body.clone(), 
                    closure: Environment::new(),
                };
                self.env.borrow_mut().define(name.clone(), Value::Function(func));
                Ok(())
            },
        }
    }
    
    pub fn interpret_program(&mut self, program: Vec<Statement>) -> Result<Value, RuntimeError> {
        for stmt in &program {
            match self.eval_statement(stmt) {
                Err(RuntimeError::Return(val)) => return Ok(val),
                Err(e) => return Err(e),
                Ok(_) => {}
            }
        }

        Ok(Value::Unit)
    }
}
