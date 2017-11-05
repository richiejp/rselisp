#[macro_use]
use super::*;
use lambda::{EvalOption, Func};

/// Define a function which can be called from Lisp
///
/// It would be much nicer if we used a "Derive style" macro instead. This
/// needs to be implemented as a compiler plugin using
/// syntax::exit::base::ItemDecorator. This would also make it a lot easier to
/// call these functions from Rust because we can declare them normally with
/// regular arguments. So this would look something like:
///
/// #[defun(Unevaluated, Optional=[docstr, interactive], Rest=body)]
/// fn lambda(args: Sexp, docstr: &str, interactive: ???, body: Iter<LispObj>) -> UserFunc {
///   ...
/// }
#[macro_export]
macro_rules! def_builtin {
    ($name:expr, $rname:ident, $evaled:ident, $lsp:ident, $args:ident; $fn_body:block ) => (
        #[derive(Clone)]
        pub struct $rname { }
        impl Func for $rname {
            fn eval_args(&self) -> EvalOption {
                EvalOption::$evaled
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn call(&self, $lsp: &mut Lsp, $args: &mut Iter<LispObj>) -> Result<LispObj, String> {
                $fn_body
            }
        }
    )
}

def_builtin! { "-", MinusBuiltin, Evaluated, _lsp, args; {
    let mut res = 0;

    if let Some(arg) = args.next() {
        match arg {
            &LispObj::Int(i) => res = i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        }
    }

    if let Some(arg) = args.next() {
        match arg {
            &LispObj::Int(i) => res -= i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        }
    } else {
        res = -res;
    }

    while let Some(arg) = args.next() {
        match arg {
            &LispObj::Int(i) => res -= i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(LispObj::Int(res))
}}

def_builtin! { "+", PlusBuiltin, Evaluated, _lsp, args; {
    let mut res = 0;

    while let Some(arg) = args.next() {
        match arg {
            &LispObj::Int(i) => res += i,
            _ => return Err(format!("Expected int, but found {:?}", arg)),
        };
    }

    Ok(LispObj::Int(res))
}}

def_builtin! { "quote", QuoteBuiltin, Unevaluated, _lsp, args; {
    let argt = take2!(args);
    match argt {
        (Some(arg), None) => Ok(arg.clone()),
        _ => Err(format!("Wrong number of arguments; quote only accepts one")),
    }
}}

def_builtin! { "interactive", InteractiveBuiltin, Unevaluated, _lsp, _args; {
    Ok(LispObj::nil())
}}

def_builtin! { "defalias", DefaliasBuiltin, Evaluated, lsp, args; {
    let name = match args.next() {
        Some(&LispObj::Sym(ref name)) => name,
        _ => return Err(format!("defalias expected symbol")),
    };

    let mut fun = match args.next() {
        Some(&LispObj::Lambda(ref lmbda)) => lmbda.clone(),
        _ => return Err(format!("defalias expected lambda")),
    };

    lsp.globals.reg_fn(fun);
    Ok(LispObj::Sym(name.to_owned()))
}}

def_builtin! { "print", PrintBuiltin, Evaluated, _lsp, args; {
    let mut s = String::new();
    let _res = fmt_iter('(', args, &mut s);
    println!("{}", &s);
    Ok(LispObj::Str(s))
}}

def_builtin! { "exit", ExitBuiltin, Unevaluated, _lsp, _args; {
    Ok(LispObj::Sym("exit".to_owned()))
}}

def_builtin! { "progn", PrognBuiltin, Evaluated, _lsp, args; {
    if let Some(val) = args.last() {
        Ok(val.clone())
    } else {
        Err(format!("progn requires one or more arguments"))
    }
}}

def_builtin! { "if", IfBuiltin, Unevaluated, lsp, args; {
    if let Some(cond) = args.next() {
        let then = match args.next() {
            Some(then) => then,
            None => return Err(format!("if requires a THEN argument")),
        };

        if !lsp.eval_inner(cond)?.is_nil() {
                lsp.eval_inner(then)
        } else {
            let mut ret = if let Some(lelse) = args.next() {
                lsp.eval_inner(lelse)?
            } else {
                LispObj::nil()
            };

            while let Some(lelse) = args.next() {
                ret = lsp.eval_inner(lelse)?;
            }

            Ok(ret)
        }
    } else {
        Err(format!("if requires a COND argument"))
    }
}}

def_builtin! { "eq", EqBuiltin, Evaluated, _lsp, args; {
    if let (Some(left), Some(right)) = take2!(args) {
        if left == right {
            Ok(LispObj::t())
        } else {
            Ok(LispObj::nil())
        }
    } else {
        Err(format!("eq requires two arguments"))
    }
}}

def_builtin! { "cons", ConsBuiltin, Evaluated, _lsp, args; {
    if let (Some(car), Some(cdr)) = take2!(args) {
        let mut sxp = Sexp::new('(');
        sxp.push(car.clone());
        sxp.push(cdr.clone());
        Ok(LispObj::Sxp(sxp))
    } else {
        Err(format!("cons requires two arguments"))
    }
}}

def_builtin! { "car", CarBuiltin, Evaluated, _lsp, args; {
    if let Some(lst) = args.next() {
        match lst {
            &LispObj::Sxp(ref sxp) => Ok(sxp.car()),
            &LispObj::Ref(ref iref) => match &iref.borrow() as &LispObj {
                &LispObj::Sxp(ref sxp) => Ok(sxp.car()),
                &LispObj::Ref(_) => Err(format!("car: argument is a reference to a reference")),
                _ => Err(format!("car: argument is not a list")),
            },
            _ => Err(format!("car: argument is not a list")),
        }
    } else {
        Err(format!("car requires one argument"))
    }
}}

def_builtin! { "cdr", CdrBuiltin, Evaluated, _lsp, args; {
    if let Some(lst) = args.next() {
        match lst {
            &LispObj::Sxp(ref sxp) => Ok(sxp.cdr()),
            &LispObj::Ref(ref iref) => match &iref.borrow() as &LispObj {
                &LispObj::Sxp(ref sxp) => Ok(sxp.cdr()),
                &LispObj::Ref(_) => Err(format!("cdr: argument is a reference to a reference")),
                _ => Err(format!("cdr: argument is not a list")),
            },
            _ => Err(format!("cdr: argument is not a list")),
        }
    } else {
        Err(format!("cdr requires one argument"))
    }
}}

def_builtin! { "listp", ListpBuiltin, Evaluated, _lsp, args; {
    if let Some(lst) = args.next() {
        match lst {
            &LispObj::Sxp(_) => Ok(LispObj::t()),
            _ => Ok(LispObj::nil()),
        }
    } else {
        Err(format!("listp requires one argument"))
    }
}}

def_builtin! { "load", LoadBuiltin, Unevaluated, lsp, args; {
    if let Some(name) = args.next() {
        let biref;
        let name = if let &LispObj::Ref(ref iref) = name {
            biref = iref.borrow();
            &biref
        } else {
            name
        };

        match name {
            &LispObj::Sym(ref name) | &LispObj::Str(ref name) => {
                lsp.load(name)
            },
            thing => Err(format!("load expects a symbol or string not {}", thing))
        }
    } else {
        Err(format!("load requires one argument"))
    }
}}
