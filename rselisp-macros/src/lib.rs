#![feature(proc_macro)]
#![recursion_limit="128"]

extern crate proc_macro;
extern crate synom;
extern crate syn;
#[macro_use]
extern crate quote;

use std::str::FromStr;
use proc_macro::TokenStream;

mod func;
mod attr;

#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let attr_ts = attr_ts.to_string();
    let real_attr = if attr_ts.len() > 0 {
        format!("#[lisp_fn({})]", attr_ts)
    } else {
        "#[lisp_fn]".to_string()
    };

    let fn_item = syn::parse_item(&fn_ts.to_string()).unwrap();
    let attr = attr::parse(syn::parse_outer_attr(&real_attr.to_string()).unwrap()).unwrap();
    let func = func::parse(&fn_item).unwrap();

    let eval_opt_toks = match attr.eval_option {
        attr::EvalOption::Evaluated => {
            quote! { ::rselisp::lambda::EvalOption::Evaluated }
        }
        attr::EvalOption::Unevaluated => {
            quote! { ::rselisp::lambda::EvalOption::Unevaluated }
        }
    };

    let rname = {
        let name = format!("{}", func.name);
        let c = name.chars().next().map(|c| c.to_uppercase()).unwrap();
        syn::Ident::new(format!("{}{}Builtin", c, name.split_at(1).1))
    };

    let name = func.name.clone();

    let tokens = quote! {
        #[derive(Clone, Debug)]
        pub struct #rname {
            name: ::rselisp::symbols::Atom,
        }

        impl ::rselisp::lambda::Func for #rname {
            fn eval_args(&self) -> ::rselisp::lambda::EvalOption {
                #eval_opt_toks
            }

            fn name(&self) -> ::rselisp::symbols::Atom {
                self.name
            }

            fn call(&self, lsp: &mut ::rselisp::Lsp, args: &mut std::slice::Iter<::rselisp::LispObj>)
                -> Result<::rselisp::LispObj, String> {
                #name(lsp, args)
            }
        }

        #fn_item
    };

    TokenStream::from_str(tokens.as_str()).unwrap()
}
