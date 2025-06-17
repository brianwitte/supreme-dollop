use std::collections::HashMap;
use std::fmt;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Number(f64),
    Symbol(String),
    String(String),
    Bool(bool),
    List(Vec<Value>),
    Function(fn(&[Value]) -> Result<Value, String>),
    Lambda {
        params: Vec<String>,
        body: Box<Value>,
        closure: HashMap<String, Value>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Function(_) => write!(f, "#<function>"),
            Value::Lambda { .. } => write!(f, "#<lambda>"),
        }
    }
}

#[derive(Debug, Clone)]
struct Environment {
    vars: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Environment) -> Self {
        Self {
            vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        self.vars.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.get(name))
        })
    }
}

struct Interpreter {
    global_env: Environment,
}

impl Interpreter {
    fn new() -> Self {
        let mut global_env = Environment::new();

        // Built-in functions
        global_env.define("+".to_string(), Value::Function(builtin_add));
        global_env.define("-".to_string(), Value::Function(builtin_sub));
        global_env.define("*".to_string(), Value::Function(builtin_mul));
        global_env.define("/".to_string(), Value::Function(builtin_div));
        global_env.define("=".to_string(), Value::Function(builtin_eq));
        global_env.define("<".to_string(), Value::Function(builtin_lt));
        global_env.define(">".to_string(), Value::Function(builtin_gt));
        global_env.define("list".to_string(), Value::Function(builtin_list));
        global_env.define("car".to_string(), Value::Function(builtin_car));
        global_env.define("cdr".to_string(), Value::Function(builtin_cdr));
        global_env.define("cons".to_string(), Value::Function(builtin_cons));
        global_env.define("null?".to_string(), Value::Function(builtin_null));

        Self { global_env }
    }

    fn tokenize(&self, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut current = String::new();
        let mut in_string = false;
        let mut chars = input.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '"' if !in_string => {
                    if !current.is_empty() {
                        tokens.push(current);
                        current = String::new();
                    }
                    in_string = true;
                    current.push(ch);
                }
                '"' if in_string => {
                    current.push(ch);
                    tokens.push(current);
                    current = String::new();
                    in_string = false;
                }
                _ if in_string => {
                    current.push(ch);
                }
                '(' | ')' => {
                    if !current.is_empty() {
                        tokens.push(current);
                        current = String::new();
                    }
                    tokens.push(ch.to_string());
                }
                ch if ch.is_whitespace() => {
                    if !current.is_empty() {
                        tokens.push(current);
                        current = String::new();
                    }
                }
                _ => {
                    current.push(ch);
                }
            }
        }

        if !current.is_empty() {
            tokens.push(current);
        }

        tokens
    }

    fn parse(&self, tokens: &[String]) -> Result<(Value, usize), String> {
        if tokens.is_empty() {
            return Err("Unexpected end of input".to_string());
        }

        match tokens[0].as_str() {
            "(" => {
                let mut items = Vec::new();
                let mut i = 1;

                while i < tokens.len() && tokens[i] != ")" {
                    let (value, consumed) = self.parse(&tokens[i..])?;
                    items.push(value);
                    i += consumed;
                }

                if i >= tokens.len() {
                    return Err("Missing closing parenthesis".to_string());
                }

                Ok((Value::List(items), i + 1))
            }
            ")" => Err("Unexpected closing parenthesis".to_string()),
            token => {
                if token.starts_with('"') && token.ends_with('"') {
                    let s = token[1..token.len()-1].to_string();
                    Ok((Value::String(s), 1))
                } else if let Ok(n) = token.parse::<f64>() {
                    Ok((Value::Number(n), 1))
                } else if token == "#t" {
                    Ok((Value::Bool(true), 1))
                } else if token == "#f" {
                    Ok((Value::Bool(false), 1))
                } else {
                    Ok((Value::Symbol(token.to_string()), 1))
                }
            }
        }
    }

    fn eval(&self, expr: &Value, env: &mut Environment) -> Result<Value, String> {
        match expr {
            Value::Number(_) | Value::String(_) | Value::Bool(_) | Value::Function(_) => {
                Ok(expr.clone())
            }
            Value::Symbol(name) => {
                env.get(name)
                    .cloned()
                    .ok_or_else(|| format!("Unbound variable: {}", name))
            }
            Value::List(items) if items.is_empty() => {
                Ok(Value::List(vec![]))
            }
            Value::List(items) => {
                match &items[0] {
                    Value::Symbol(name) if name == "define" => {
                        if items.len() != 3 {
                            return Err("define requires exactly 2 arguments".to_string());
                        }
                        if let Value::Symbol(var_name) = &items[1] {
                            let value = self.eval(&items[2], env)?;
                            env.define(var_name.clone(), value.clone());
                            Ok(value)
                        } else {
                            Err("define requires a symbol as first argument".to_string())
                        }
                    }
                    Value::Symbol(name) if name == "if" => {
                        if items.len() != 4 {
                            return Err("if requires exactly 3 arguments".to_string());
                        }
                        let condition = self.eval(&items[1], env)?;
                        let is_true = match condition {
                            Value::Bool(false) => false,
                            _ => true,
                        };
                        if is_true {
                            self.eval(&items[2], env)
                        } else {
                            self.eval(&items[3], env)
                        }
                    }
                    Value::Symbol(name) if name == "lambda" => {
                        if items.len() != 3 {
                            return Err("lambda requires exactly 2 arguments".to_string());
                        }
                        if let Value::List(param_list) = &items[1] {
                            let mut params = Vec::new();
                            for param in param_list {
                                if let Value::Symbol(param_name) = param {
                                    params.push(param_name.clone());
                                } else {
                                    return Err("lambda parameters must be symbols".to_string());
                                }
                            }
                            Ok(Value::Lambda {
                                params,
                                body: Box::new(items[2].clone()),
                                closure: env.vars.clone(),
                            })
                        } else {
                            Err("lambda requires a parameter list".to_string())
                        }
                    }
                    Value::Symbol(name) if name == "quote" => {
                        if items.len() != 2 {
                            return Err("quote requires exactly 1 argument".to_string());
                        }
                        Ok(items[1].clone())
                    }
                    _ => {
                        // Function application
                        let func = self.eval(&items[0], env)?;
                        let mut args = Vec::new();
                        for arg in &items[1..] {
                            args.push(self.eval(arg, env)?);
                        }

                        match func {
                            Value::Function(f) => f(&args),
                            Value::Lambda { params, body, closure } => {
                                if args.len() != params.len() {
                                    return Err(format!("Wrong number of arguments: expected {}, got {}", params.len(), args.len()));
                                }

                                let mut new_env = Environment::with_parent(env.clone());
                                // Add closure variables
                                for (k, v) in closure {
                                    new_env.define(k, v);
                                }
                                // Bind parameters
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    new_env.define(param.clone(), arg.clone());
                                }

                                self.eval(&body, &mut new_env)
                            }
                            _ => Err("Not a function".to_string()),
                        }
                    }
                }
            }
            Value::Lambda { .. } => Ok(expr.clone()),
        }
    }

    fn run(&mut self, input: &str) -> Result<Value, String> {
        let tokens = self.tokenize(input);
        let (expr, _) = self.parse(&tokens)?;
        let mut env = self.global_env.clone();
        let result = self.eval(&expr, &mut env)?;
        // Update global environment with any new definitions
        self.global_env = env;
        Ok(result)
    }
}

// Built-in functions
fn builtin_add(args: &[Value]) -> Result<Value, String> {
    let mut sum = 0.0;
    for arg in args {
        if let Value::Number(n) = arg {
            sum += n;
        } else {
            return Err("+ requires numeric arguments".to_string());
        }
    }
    Ok(Value::Number(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("- requires at least one argument".to_string());
    }

    if let Value::Number(first) = &args[0] {
        if args.len() == 1 {
            Ok(Value::Number(-first))
        } else {
            let mut result = *first;
            for arg in &args[1..] {
                if let Value::Number(n) = arg {
                    result -= n;
                } else {
                    return Err("- requires numeric arguments".to_string());
                }
            }
            Ok(Value::Number(result))
        }
    } else {
        Err("- requires numeric arguments".to_string())
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, String> {
    let mut product = 1.0;
    for arg in args {
        if let Value::Number(n) = arg {
            product *= n;
        } else {
            return Err("* requires numeric arguments".to_string());
        }
    }
    Ok(Value::Number(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("/ requires at least one argument".to_string());
    }

    if let Value::Number(first) = &args[0] {
        if args.len() == 1 {
            Ok(Value::Number(1.0 / first))
        } else {
            let mut result = *first;
            for arg in &args[1..] {
                if let Value::Number(n) = arg {
                    if *n == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    result /= n;
                } else {
                    return Err("/ requires numeric arguments".to_string());
                }
            }
            Ok(Value::Number(result))
        }
    } else {
        Err("/ requires numeric arguments".to_string())
    }
}

fn builtin_eq(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("= requires exactly 2 arguments".to_string());
    }
    Ok(Value::Bool(args[0] == args[1]))
}

fn builtin_lt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("< requires exactly 2 arguments".to_string());
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
        _ => Err("< requires numeric arguments".to_string()),
    }
}

fn builtin_gt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("> requires exactly 2 arguments".to_string());
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
        _ => Err("> requires numeric arguments".to_string()),
    }
}

fn builtin_list(args: &[Value]) -> Result<Value, String> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_car(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("car requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(_) => Err("car of empty list".to_string()),
        _ => Err("car requires a list".to_string()),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cdr requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::List(_) => Err("cdr of empty list".to_string()),
        _ => Err("cdr requires a list".to_string()),
    }
}

fn builtin_cons(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cons requires exactly 2 arguments".to_string());
    }
    match &args[1] {
        Value::List(items) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(items.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Err("cons requires a list as second argument".to_string()),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("null? requires exactly 1 argument".to_string());
    }
    match &args[0] {
        Value::List(items) => Ok(Value::Bool(items.is_empty())),
        _ => Ok(Value::Bool(false)),
    }
}

fn main() {
    let mut interpreter = Interpreter::new();

    println!("Minimal Lisp-1 Interpreter");
    println!("Enter expressions or 'quit' to exit");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input == "quit" {
                    break;
                }
                if input.is_empty() {
                    continue;
                }

                match interpreter.run(input) {
                    Ok(result) => println!("{}", result),
                    Err(error) => println!("Error: {}", error),
                }
            }
            Err(error) => {
                println!("Error reading input: {}", error);
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.run("(+ 1 2 3)").unwrap(), Value::Number(6.0));
        assert_eq!(interp.run("(* 2 3 4)").unwrap(), Value::Number(24.0));
        assert_eq!(interp.run("(- 10 3)").unwrap(), Value::Number(7.0));
        assert_eq!(interp.run("(/ 12 3)").unwrap(), Value::Number(4.0));
    }

    #[test]
    fn test_variables() {
        let mut interp = Interpreter::new();
        interp.run("(define x 42)").unwrap();
        assert_eq!(interp.run("x").unwrap(), Value::Number(42.0));
    }

    #[test]
    fn test_lambda() {
        let mut interp = Interpreter::new();
        interp.run("(define square (lambda (x) (* x x)))").unwrap();
        assert_eq!(interp.run("(square 5)").unwrap(), Value::Number(25.0));
    }

    #[test]
    fn test_list_operations() {
        let mut interp = Interpreter::new();
        interp.run("(define lst (list 1 2 3))").unwrap();
        assert_eq!(interp.run("(car lst)").unwrap(), Value::Number(1.0));
        assert_eq!(interp.run("(car (cdr lst))").unwrap(), Value::Number(2.0));
    }
}
