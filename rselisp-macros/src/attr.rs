use syn;

#[derive(Debug)]
pub struct LispFn {
    /// Defines if the function is evaluated or unevaluated.
    pub eval_option: EvalOption,
}

#[derive(Debug)]
pub enum EvalOption {
    Evaluated,
    Unevaluated,
}

pub fn parse(attr: syn::Attribute) -> Result<LispFn, ()> {
    let mut lisp_fn = LispFn { eval_option: EvalOption::Evaluated };
    match attr.value {
        // #[lisp_fn] form
        syn::MetaItem::Word(_) => {},
        // #[lisp_fn(..)] form
        syn::MetaItem::List(_, meta_items) => {
            for entry in meta_items {
                match entry {
                    syn::NestedMetaItem::MetaItem(meta_item) => {
                        match meta_item {
                            syn::MetaItem::Word(ident) => {
                                match ident.as_ref() {
                                    "unevaluated" => {
                                        lisp_fn.eval_option = EvalOption::Unevaluated;
                                    }
                                    "evaluated" => {
                                        lisp_fn.eval_option = EvalOption::Evaluated;
                                    }
                                    _ => return Err(())
                                }
                            }
                            _ => return Err(()),
                        }
                    }
                    _ => return Err(()),
                }
            }
        }
        // not valid syntax
        _=> return Err(())
    }

    Ok(lisp_fn)
}
