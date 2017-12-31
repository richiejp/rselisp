use syn;

#[derive(Debug)]
pub struct Function {
    pub name: syn::Ident,
}

pub fn parse(item: &syn::Item) -> Result<Function, ()> {
    let name = item.ident.clone();

    match item.node {
        syn::ItemKind::Fn(_, _, _, _, _, _) => {}
        _ => return Err(())
    }

    Ok(Function {
        name,
    })
}
