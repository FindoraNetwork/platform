//! Implements the functionality to convert a struct type to a abci event type
//! in Rust through the use of a procedural macros.
#![recursion_limit = "128"]

use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Ident};

/// Findora transaction events parser.
///
/// Converts a given input struct into a abci event where the keys are the attribute names assigned to
/// the values of the entries.
#[proc_macro_derive(Event)]
pub fn derive_event(input_struct: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input_struct as DeriveInput);

    // check for struct type and parse out fields
    let fields = match ast.data {
        Data::Struct(st) => st.fields,
        _ => panic!("Implementation must be a struct"),
    };

    // parse out all the field names in the struct as `Ident`s
    let idents: Vec<&Ident> = fields
        .iter()
        .filter_map(|field| field.ident.as_ref())
        .collect::<Vec<&Ident>>();

    // convert all the field names into strings
    let keys: Vec<String> = idents
        .clone()
        .iter()
        .map(|ident| ident.to_string())
        .collect::<Vec<String>>();

    // get the name identifier of the struct input AST
    let name: &Ident = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    // start codegen for events functionality that converts a struct into a abci event
    let tokens = quote! {

        impl #impl_generics Event for #name #ty_generics #where_clause {

            fn emit_event(field_type: String, mut input_struct: #name) -> AbciEvent {
                let mut attributes = Vec::new();
                #(
                    let mut pair = AbciPair::new();
                    pair.key = #keys.to_string().as_bytes().into();
                    pair.value = format!("{:?}", input_struct.#idents).as_bytes().into();
                    attributes.push(pair);
                )*
                let mut event = AbciEvent::new();
                event.type_ = format!("{}_{}", field_type, stringify!(#name));
                event.attributes = attributes;
                event
            }

            fn emit_serde_event(field_type: String, mut input_struct: #name) -> AbciEvent {
                let mut attributes = Vec::new();
                #(
                    let mut pair = AbciPair::new();
                    pair.key = #keys.to_string().as_bytes().into();
                    pair.value = to_vec(&input_struct.#idents).unwrap_or(vec![]);
                    attributes.push(pair);
                )*
                let mut event = AbciEvent::new();
                event.type_ = format!("{}_{}", field_type, stringify!(#name));
                event.attributes = attributes;
                event
            }
        }
    };
    TokenStream::from(tokens)
}
