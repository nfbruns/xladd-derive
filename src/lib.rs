use proc_macro::*;
use std::collections::BTreeMap;
use quote::quote;
use syn::{FnArg, ItemFn};



#[proc_macro_attribute]
pub fn xl_func(attr: TokenStream, input: TokenStream) -> TokenStream {
    // println!("{:?}", attr);
    // println!("{:?}", input);
    // println!("{:?}", input);

    let item = syn::parse::<ItemFn>(input).expect("Failed to parse.");
    let mut params = BTreeMap::new();
    let tree = attr.into_iter().collect::<Vec<TokenTree>>();
    for chunk in tree.as_slice().windows(3) {
        match chunk {
            [TokenTree::Ident(i),TokenTree::Punct(_),TokenTree::Literal(l)] => {
                let l = l.to_string();
                params.insert(i.to_string(),l[1..l.len()-1].to_string());
            },
            _ => (),
        }
    }
    let name = item.sig.ident.to_string();
    let category = if let Some(v) = params.get("category") { v } else { "" };
    let prefix = if let Some(v) = params.get("prefix") { v } else { "xl" };
    let rename = if let Some(v) = params.get("rename") { v } else { name.as_str() };
    let async_function = if let Some(_) = params.get("async") { true } else { false };
    let single_threaded = if let Some(_) = params.get("single_threaded") { true } else { false };
    // Use `quote` to convert the syntax tree back into tokens so we can return them. Note
    // that the tokens we're returning at this point are still just the input, we've simply
    // converted it between a few different forms.
    let output = &item.sig.output;
    let func = &item.sig.ident;

    let xl_function = proc_macro2::Ident::new(
        &format!("{}_{}", prefix, rename),
        proc_macro2::Span::call_site(),
    );
    let error_handler_function = proc_macro2::Ident::new(
        &format!("_error_hndlr_{}", func),
        proc_macro2::Span::call_site(),
    );

    let register_function = proc_macro2::Ident::new(
        &format!("register_{}", func),
        proc_macro2::Span::call_site(),
    );
    // From the signature, identify the types we handle
    // f32,f64,i32,i64,bool,&str,&[&str],&[f64]
    // and map them to the corresponding owned types

    // Optional arguments (Option<T>) are only allowed as the trailing arguments, following Excel's
    // convention. An omitted argument or an empty cell is passed to the function as None
    let mut seen_optional = false;
    for arg in item.sig.inputs.iter() {
        if let FnArg::Typed(typed_arg) = arg {
            let optional = option_inner(&typed_arg.ty).is_some();
            if seen_optional && !optional {
                panic!("{}: Option<T> arguments must come after all required arguments", func);
            }
            seen_optional |= optional;
        }
    }

    let typed_args = &item.sig.inputs.iter().map(|arg| match arg {
        FnArg::Typed(typed_arg) => {
            let arg_name = arg_name(typed_arg);
            // Owned type
            let owned_type = match option_inner(&typed_arg.ty) {
                Some(inner) => {
                    let convert = owned_conversion(&arg_name, inner);
                    quote!(
                        let #arg_name = if #arg_name.is_missing_or_null() {
                            log::trace!("{}:[None]",stringify!(#arg_name));
                            None
                        } else {
                            Some({ #convert #arg_name })
                        };
                    )
                }
                None => {
                    let missing_check = missing_check(&typed_arg.ty);
                    let convert = owned_conversion(&arg_name, &typed_arg.ty);
                    quote!(
                        if #arg_name.#missing_check() {
                            return Err(Box::new(xladd::variant::XLAddError::MissingArgument(stringify!(#func).to_string(),stringify!(#arg_name).to_string())));
                        }
                        #convert
                    )
                }
            };
            (arg_name, owned_type)
        }
        FnArg::Receiver(_) => panic!("Free functions only"),
    });

    // Rerefence args
    let reference_args = &item.sig.inputs.iter().map(|arg| match arg {
        FnArg::Typed(typed_arg) => {
            let arg_name = arg_name(typed_arg);
            let ref_type = match option_inner(&typed_arg.ty) {
                Some(inner) => reference_conversion(&arg_name, inner, true),
                None => reference_conversion(&arg_name, &typed_arg.ty, false),
            };
            (arg_name, ref_type)
        }
        FnArg::Receiver(_) => panic!("Free functions only"),
    });
    // Parse the doc comments

    let comments = &item.attrs.iter().filter_map(|attr: &syn::Attribute| {
        let segment = &attr.path.segments[0];
        if segment.ident == "doc" {
            Some(attr.tokens.to_string())
        } else {
            None
        }
    });
    let args = typed_args
        .clone()
        .filter_map(|(name, _)| {
            let name = name.to_string();
            comments.clone().find_map(|v| {
                if v.starts_with(&format!("= \" * {} -", name)) {
                    let v = &v[name.len() + 9..v.len() - 1];
                    Some(quote! {#v})
                } else {
                    None
                }
            })
        })
        .collect::<Vec<_>>();

    let ret = comments.clone().find_map(|v| {
        if v.starts_with("= \" * ret -") {
            let v = &v[12..v.len() - 1];
            Some(v.to_owned())
        } else {
            None
        }
    });
    let docs = comments.clone().find_map(|v| {
        if !v.starts_with("= \" *") {
            let v = &v[4..v.len() - 1];
            Some(v.to_owned())
        } else {
            None
        }
    });

    let docs_ret = 
        if docs.is_some() {
            docs.as_ref().unwrap()
        } else {
            ""
        };

    // Return type convert back to variant
    let output = {
        match output {
            syn::ReturnType::Default => quote! {},
            syn::ReturnType::Type(_, path) => match &**path {
                syn::Type::Path(path) => {
                    let segment = &path.path.segments[0];
                    if segment.ident == "Result" {
                        let args = &segment.arguments;
                        match args {
                            syn::PathArguments::AngleBracketed(generic_args) => {
                                let arg0 = &generic_args.args[0];
                                match &*arg0 {
                                    syn::GenericArgument::Type(path) => match path {
                                        syn::Type::Tuple(tuple) => {
                                            let elems = &tuple.elems[0];
                                            match &*elems {
                                                syn::Type::Path(path) => {
                                                    let segment = &path.path.segments[0];
                                                    if segment.ident == "Vec" {
                                                        let args = &segment.arguments;
                                                        match &*args {
                                                            syn::PathArguments::AngleBracketed(generic_args) => {
                                                                let arg0 = &generic_args.args[0];
                                                                match &*arg0 {
                                                                    syn::GenericArgument::Type(path) => {
                                                                        match path {
                                                                            syn::Type::Path(p) => {
                                                                                if p.path.segments[0].ident == "String" {
                                                                                    quote! {Ok(xladd::variant::Variant::from(&(res.0.iter().map(AsRef::as_ref).collect::<Vec<_>>().as_slice(),res.1)))}
                                                                                } else {
                                                                                    quote! {Ok(xladd::variant::Variant::from(&(res.0.as_slice(),res.1)))}
                                                                                }
                                                                            },
                                                                            _ => panic!("Expected a type of f64,u32,bool,String")
                                                                        }
 
                                                                    },
                                                                    _ =>  panic!("Expected a simple type after a vec"),

                                                                }
                                                            },
                                                                syn::PathArguments::Parenthesized(_) => {
                                                                    quote! {Ok(xladd::variant::Variant::from(true))}
                                                                },
                                                                syn::PathArguments::None => panic!("Unhandled type for result0"),
                                                        }
//                                                        quote! {Ok(Variant::from(&(res.0.as_slice(),res.1)))}
                                                    } else {
                                                        quote! {Ok(xladd::variant::Variant::from(res))}
                                                    }        
                                                },
                                                _ => panic!("Tuple returned must of <Vec<f64>,Dimension(usize)>")

                                            }
                                        }
                                        syn::Type::Path(_) => {
                                            quote! {Ok(xladd::variant::Variant::from(res))}
                                        },
                                        _ => panic!("XL functions must return a basic type of f64,i64,u32,i32,bool or a tuple of (Vec<f64>,Dimension(usize))")
                                    },
                                    _ => panic!("XL functions must return a basic type of f64,i64,u32,i32,bool or a tuple of (Vec<f64>,Dimension(usize))")
                                }
                            }
                            syn::PathArguments::Parenthesized(_) => {
                                quote! {Ok(xladd::variant::Variant::from(true))}
                            }
                            syn::PathArguments::None => quote! {} //panic!("XL functions must return a basic type of f64,i64,u32,i32,bool or a tuple of (Vec<f64>,Dimension(usize))")

                        }
                    } else {
                        panic!("XL functions must return a Result<TYPE,Error>. Error can be coerced into a Box<std::error::Error>")
                    }
                }
                _ => panic!("Unhandled type"),
            },
        }
    };
    // Now collate
    let lpx_oper_args = typed_args
        .clone()
        .map(|(name, _)| quote!(#name: xladd::xlcall::LPXLOPER12))
        .collect::<Vec<_>>();
    let variant_args = typed_args
        .clone()
        .map(|(name, _)| quote!(#name: xladd::variant::Variant))
        .collect::<Vec<_>>();
    let to_variant = typed_args
        .clone()
        .map(|(name, _)| quote!(let #name = xladd::variant::Variant::from(#name);))
        .collect::<Vec<_>>();
    let caller_args = typed_args
        .clone()
        .map(|(name, _)| quote!(#name))
        .collect::<Vec<_>>();
    let caller_args_str = item
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Typed(typed_arg) if option_inner(&typed_arg.ty).is_some() => {
                Some(format!("[{}]", arg_name(typed_arg)))
            }
            FnArg::Typed(typed_arg) => Some(arg_name(typed_arg).to_string()),
            FnArg::Receiver(_) => None,
        })
        .collect::<Vec<_>>()
        .join(", ");
    let mut q_args = typed_args
        .clone()
        .map(|(_, _)| "Q")
        .collect::<Vec<_>>()
        .join("");
    // Mark function as async
    if async_function {
        q_args.insert(0,'>');
        q_args.push('X');
    } else {
        // Return type is a variant
        q_args.push('Q');
    }
    // not_thread_safe
    if !(single_threaded || async_function) { q_args.push('$'); };
    let convert_to_owned_rust_types = typed_args
        .clone()
        .map(|(_, owned_type)| owned_type)
        .collect::<Vec<_>>();

    let convert_to_ref_rust_types = reference_args
        .clone()
        .map(|(_, owned_type)| owned_type)
        .collect::<Vec<_>>();

        let xl_function_str = xl_function.to_string();

    // Async function
    if async_function {
        let wrapper = quote! {
             // Error handler
             fn #error_handler_function(#(#variant_args),*, return_handle: xladd::xlcall::LPXLOPER12) -> Result<xladd::variant::Variant, Box<dyn std::error::Error>> {
                log::trace!("{} called [*ASYNC*] ..waiting for results",stringify!(#xl_function));
                #(#convert_to_owned_rust_types)*;
                let raw_ptr = xladd::variant::XLOPERPtr(return_handle);
                std::thread::spawn(move ||{
                    #(#convert_to_ref_rust_types)*;
                    match #func(#(#caller_args),*) {
                        Ok(v) => {
                            log::trace!("Results [{:?}]",v);
                            xladd::entrypoint::excel12(
                                                xladd::xlcall::xlAsyncReturn,
                                                &mut [xladd::variant::Variant::from(raw_ptr), v]);
                        }
                        Err(e) => {
                            log::error!("Error {:?}",e.to_string());
                            xladd::entrypoint::excel12(
                                                xladd::xlcall::xlAsyncReturn,
                                                &mut [xladd::variant::Variant::from(raw_ptr), xladd::variant::Variant::from(e.to_string())]);
                        }
                    }
                });
                Ok(xladd::variant::Variant::default())
            }
            // Excel function
            #[unsafe(no_mangle)]
            extern "stdcall" fn #xl_function(#(#lpx_oper_args),* ,return_handle: xladd::xlcall::LPXLOPER12) {
                #(#to_variant)*
                match #error_handler_function(#(#caller_args),*, return_handle) {
                    Ok(_) => (),
                    Err(e) => {
                        log::error!("{}",e.to_string());
                        let raw_ptr = xladd::variant::XLOPERPtr(return_handle);
                        xladd::entrypoint::excel12(
                            xladd::xlcall::xlAsyncReturn,
                            &mut [xladd::variant::Variant::from(raw_ptr), xladd::variant::Variant::from(e.to_string())],
                        );
                    },
                }
            }

            pub (crate) fn #register_function(reg: &xladd::registrator::Reg) {
                reg.add(#xl_function_str,#q_args,#caller_args_str,#category,#docs_ret,&[#(#args),*]);
            }
            // User function
            #item
        };
        wrapper.into()
    } else {
        let wrapper = quote! {

            // Error handler
            fn #error_handler_function(#(#variant_args),*) -> Result<xladd::variant::Variant, Box<dyn std::error::Error>> {
                log::trace!("{} called",stringify!(#xl_function));
                #(#convert_to_owned_rust_types)*;
                #(#convert_to_ref_rust_types)*;
                let res = std::panic::catch_unwind(|| #func(#(#caller_args),*));
                match res {
                    Ok(result) => {
                        let res = result?;
                        log::trace!("Results [{:?}]",res);
                        #output        
                    }
                    Err(_) => {
                        log::error!("Unexpected error while calling function"); 
                        Err("Error when trying to execute function, check for invalid values, ranges, or #n/a".into())
                    }
                }
            }
            // Excel function
            #[unsafe(no_mangle)]
            extern "stdcall" fn #xl_function(#(#lpx_oper_args),*)  -> xladd::xlcall::LPXLOPER12 {
                #(#to_variant)*
                match #error_handler_function(#(#caller_args),*) {
                    Ok(v) => xladd::xlcall::LPXLOPER12::from(v),
                    Err(e) => {
                        log::error!("{}",e.to_string());
                        xladd::xlcall::LPXLOPER12::from(xladd::variant::Variant::from(e.to_string().as_str()))
                    },
                }
            }

            pub (crate) fn #register_function(reg: &xladd::registrator::Reg) {
                
                reg.add(#xl_function_str,#q_args,#caller_args_str,#category,#docs_ret,&[#(#args),*]);
            }
            // User function
            #item
        };
    wrapper.into()
    }   
}

fn arg_name(typed_arg: &syn::PatType) -> proc_macro2::TokenStream {
    match &*typed_arg.pat {
        syn::Pat::Ident(ident) => quote!(#ident),
        _ => panic!("Type not covered"),
    }
}

/// Returns `T` if `ty` is `Option<T>`
fn option_inner(ty: &syn::Type) -> Option<&syn::Type> {
    let syn::Type::Path(p) = ty else { return None };
    let segment = p.path.segments.last()?;
    if segment.ident != "Option" {
        return None;
    }
    match &segment.arguments {
        syn::PathArguments::AngleBracketed(generic_args) => match generic_args.args.first() {
            Some(syn::GenericArgument::Type(inner)) => {
                if option_inner(inner).is_some() {
                    panic!("Nested Option<Option<T>> arguments are not supported");
                }
                Some(inner)
            }
            _ => None,
        },
        _ => None,
    }
}

/// Check used to reject a missing required argument. Empty cells are allowed for reference types (&str, &[T])
fn missing_check(ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Reference(_) => quote!(is_missing),
        _ => quote!(is_missing_or_null),
    }
}

/// Statements converting the Variant `arg_name` into the owned Rust type backing `ty`
/// f32,f64,i32,i64,bool,String,Vec<T>,Array2<T> are converted directly,
/// &str -> String, &[T] -> Vec<T>, &[&str] -> Vec<String>
fn owned_conversion(arg_name: &proc_macro2::TokenStream, ty: &syn::Type) -> proc_macro2::TokenStream {
    let owned_type = match ty {
        syn::Type::Path(p) => {
            let segment = &p.path.segments[0];
            let ident = &segment.ident;
            if ident == "str" {
                quote!(String)
            } else {
                quote!(#p)
            }
        }
        syn::Type::Reference(p) => {
            let elem = &p.elem;
            match &**elem {
                // Slice
                syn::Type::Slice(s) => {
                    let elem = &s.elem;
                    match &**elem {
                        syn::Type::Path(p) => {
                            let segment = &p.path.segments[0];
                            let ident = &segment.ident;
                            quote!(Vec<#ident>)
                        }
                        syn::Type::Reference(p) => {
                            let elem = &p.elem;
                            match &**elem {
                                syn::Type::Path(p) if p.path.segments[0].ident == "str" => quote!(Vec<String>),
                                syn::Type::Path(_) => panic!("Only slices of &[&str] supported"),
                                _ => panic!("Type not covered"),
                            }
                        }
                        _ => panic!("Type not covered"),
                    }
                }
                // or Path
                syn::Type::Path(s) => {
                    let segment = &s.path.segments[0];
                    let ident = &segment.ident;
                    if ident == "str" {
                        quote!(String)
                    } else {
                        quote!(#ident)
                    }
                }
                _ => panic!("Type not covered"),
            }
        }
        _ => panic!("Type not covered"),
    };
    quote!(
        let #arg_name = std::convert::TryInto::<#owned_type>::try_into(&#arg_name)?;
        log::trace!("{}:[{:?}]",stringify!(#arg_name),#arg_name);
    )
}

/// Statements borrowing the owned value produced by `owned_conversion` as the reference type `ty`.
/// For optional arguments the owned value is wrapped in an Option
fn reference_conversion(arg_name: &proc_macro2::TokenStream, ty: &syn::Type, optional: bool) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Path(_) => quote!(),
        syn::Type::Reference(p) => {
            let elem = &p.elem;
            match &**elem {
                // Slice
                syn::Type::Slice(s) => {
                    let elem = &s.elem;
                    match &**elem {
                        syn::Type::Path(_) if optional => quote!( let #arg_name = #arg_name.as_deref(); ),
                        syn::Type::Path(_) => quote!( let #arg_name = #arg_name.as_slice(); ),
                        // &[&str] needs an intermediate Vec<&str> borrowing the Vec<String>
                        syn::Type::Reference(_) if optional => quote!(
                            let #arg_name = #arg_name.as_ref().map(|v| v.iter().map(String::as_str).collect::<Vec<_>>());
                            let #arg_name = #arg_name.as_deref();
                        ),
                        syn::Type::Reference(_) => quote!(
                            let #arg_name = #arg_name.iter().map(String::as_str).collect::<Vec<_>>();
                            let #arg_name = #arg_name.as_slice();
                        ),
                        _ => panic!("Type not covered"),
                    }
                }
                // or Path
                syn::Type::Path(s) if s.path.segments[0].ident == "str" => {
                    if optional {
                        quote!( let #arg_name = #arg_name.as_deref(); )
                    } else {
                        quote!( let #arg_name = #arg_name.as_str(); )
                    }
                }
                syn::Type::Path(_) => quote!(),
                _ => panic!("Type not covered"),
            }
        }
        _ => panic!("Type not covered"),
    }
}
