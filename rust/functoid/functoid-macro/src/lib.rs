use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, FnArg, ItemFn, Pat, ReturnType, Attribute};

/// Procedural macro to convert a function into a Functoid
///
/// Example:
/// ```ignore
/// #[functoid]
/// fn add(x: i32, y: i32) -> i32 {
///     x + y
/// }
/// ```
///
/// With @Id support:
/// ```ignore
/// #[functoid]
/// fn greet(#[id("user-name")] name: String, #[id("greeting-msg")] msg: String) -> String {
///     format!("{} {}", msg, name)
/// }
/// ```
#[proc_macro_attribute]
pub fn functoid(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);

    let fn_name = &input.sig.ident;
    let fn_vis = &input.vis;
    let fn_block = &input.block;
    let fn_generics = &input.sig.generics;

    // Extract parameter information
    let mut param_idents = Vec::new();
    let mut param_name_strs = Vec::new();
    let mut param_types = Vec::new();
    let mut param_ids = Vec::new();

    for arg in &input.sig.inputs {
        if let FnArg::Typed(pat_type) = arg {
            // Extract parameter name
            let param_ident = if let Pat::Ident(pat_ident) = &*pat_type.pat {
                &pat_ident.ident
            } else {
                panic!("Functoid only supports simple parameter patterns");
            };

            // Extract parameter type
            let param_type = &*pat_type.ty;

            // Extract @id attribute if present
            let id = extract_id_attribute(&pat_type.attrs);

            param_idents.push(param_ident.clone());
            param_name_strs.push(param_ident.to_string());
            param_types.push(param_type.clone());
            param_ids.push(id);
        } else {
            panic!("Functoid does not support self parameters");
        }
    }

    // Extract return type
    let return_type = match &input.sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    };

    // Generate parameter info initialization
    let param_info_init = param_name_strs.iter().zip(&param_types).zip(&param_ids).map(|((name, ty), id)| {
        if let Some(id_str) = id {
            quote! {
                ::functoid::ParamInfo::with_id(
                    ::functoid::TypeInfo::of::<#ty>(),
                    #name,
                    #id_str
                )
            }
        } else {
            quote! {
                ::functoid::ParamInfo::new(
                    ::functoid::TypeInfo::of::<#ty>(),
                    #name
                )
            }
        }
    });

    // Generate struct name for the functoid wrapper (convert snake_case to CamelCase)
    let struct_name = syn::Ident::new(
        &format!("{}Functoid", to_camel_case(&fn_name.to_string())),
        fn_name.span()
    );

    // Generate argument names for function call
    let arg_usage = (0..param_types.len()).map(|i| {
        let ident = syn::Ident::new(&format!("arg_{}", i), proc_macro2::Span::call_site());
        quote! { #ident }
    });

    // Generate the full argument extraction and binding
    let full_arg_extraction = param_types.iter().enumerate().map(|(i, ty)| {
        let arg_ident = syn::Ident::new(&format!("arg_{}", i), proc_macro2::Span::call_site());
        quote! {
            let #arg_ident = args.get(#i)
                .expect(&format!("Missing argument at position {}", #i))
                .downcast_ref::<#ty>()
                .expect(&format!("Argument {} has wrong type, expected {}", #i, std::any::type_name::<#ty>()))
                .clone();
        }
    });

    let param_count = param_types.len();

    let expanded = quote! {
        // Keep the original function for direct calling
        #fn_vis fn #fn_name #fn_generics (#(#param_idents: #param_types),*) -> #return_type {
            #fn_block
        }

        // Generate the Functoid wrapper struct
        #fn_vis struct #struct_name;

        impl #struct_name {
            pub fn new() -> Self {
                Self
            }
        }

        impl ::functoid::Functoid for #struct_name {
            type Output = #return_type;

            fn param_info(&self) -> &[::functoid::ParamInfo] {
                use std::sync::OnceLock;
                static PARAM_INFO: OnceLock<Vec<::functoid::ParamInfo>> = OnceLock::new();
                PARAM_INFO.get_or_init(|| {
                    vec![
                        #(#param_info_init),*
                    ]
                })
            }

            fn return_type(&self) -> ::functoid::TypeInfo {
                ::functoid::TypeInfo::of::<#return_type>()
            }

            fn invoke(&self, args: Vec<Box<dyn std::any::Any>>) -> Self::Output {
                assert_eq!(args.len(), #param_count, "Expected {} arguments, got {}", #param_count, args.len());

                #(#full_arg_extraction)*

                #fn_name(#(#arg_usage),*)
            }
        }
    };

    TokenStream::from(expanded)
}

/// Extract the @id attribute value from parameter attributes
fn extract_id_attribute(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("id") {
            if let Ok(lit_str) = attr.parse_args::<syn::LitStr>() {
                return Some(lit_str.value());
            }
        }
    }
    None
}

/// Convert snake_case to CamelCase
fn to_camel_case(s: &str) -> String {
    s.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().chain(chars).collect(),
            }
        })
        .collect()
}
