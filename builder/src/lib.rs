use paste::paste;
use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, AngleBracketedGenericArguments, Data, DeriveInput, Field,
    Fields, GenericArgument, Ident, Path, PathArguments, PathSegment, Type, TypePath,
};

/*
    impl StructName {
        pub fn builder() -> StructNameBuilder {
            return StructNameBuilder
        }

    }

    struct StructNameBuilder {
        field_name: Optional<FieldType>
        fn build() {
            if field_name.is_none() {
                panic!
            }

        }
    }

    impl StructNameBuilder {



    }


*/

fn ty_is_option(ty: &syn::Type) -> Option<&syn::Type> {
    match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: _,
                    segments: segments @ _,
                },
        }) => {
            if segments.len() > 1 {
                return None;
            }
            let segment = segments.first();
            if let None = segment {
                return None;
            }
            let segment = segment.unwrap();
            match segment {
                PathSegment {
                    ident: ident @ _,
                    arguments:
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            args: args @ _,
                            ..
                        }),
                } => {
                    if ident.to_string() != "Option" {
                        return None;
                    }
                    if args.len() > 1 {
                        return None;
                    }
                    let arg = args.first();
                    match arg {
                        Some(GenericArgument::Type(x @ _)) => return Some(x),
                        _ => return None,
                    }
                }
                _ => return None,
            }
        }
        _ => None,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input_derived = parse_macro_input!(input as DeriveInput);
    let struct_name = input_derived.ident;
    eprintln!("Struct name: {}", struct_name);

    let data = match input_derived.data {
        Data::Struct(data) => data,
        _ => unimplemented!(),
    };

    let named_fields = match data.fields {
        Fields::Named(fields) => fields,
        _ => unimplemented!(),
    };

    let new_field_type = |ty: &Type| {
        if ty_is_option(&ty).is_none() {
            quote!(Option<#ty>)
        } else {
            quote!(#ty)
        }
    };
    let field_decls = named_fields.named.iter().map(|field| {
        let field_type = &field.ty;
        let field_name = field.ident.as_ref().unwrap();
        let new_ty = new_field_type(field_type);
        quote!(
                #field_name: #new_ty
        )
    });

    let builder_struct_name = Ident::new(
        &format!("{}_Builder", struct_name),
        Span::call_site().into(),
    );
    let builder_func_impls = named_fields.named.iter().map(|field| {
        let field_type = &field.ty;
        let field_name = field.ident.as_ref().unwrap();
        let new_ty = ty_is_option(field_type).unwrap_or(field_type);
        quote!(
            pub fn #field_name(&mut self, v: #new_ty) -> &mut Self {
                self.#field_name = Some(v);
                self
            }
        )
    });

    let field_initializations = named_fields.named.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;
        if ty_is_option(field_type).is_none() {
            quote!(
                #field_name: self.#field_name.as_ref().cloned()?
            )
        } else {
            quote!(
                #field_name: self.#field_name.as_ref().cloned()
            )
        }
        // }
        // else{
        //     quote!(
        //         #field_name: if let Some(v) = self.#field_name {
        //             #field_name
        //         }
        //         else {
        //             None
        //         }
        //     )
        // }
    });

    let result = quote!(

            #[derive(Default)]
            struct #builder_struct_name {
                #(#field_decls),*
            }

            impl #builder_struct_name {
                pub fn build(&self) -> Option<#struct_name> {
                    Some(#struct_name {
                        #(#field_initializations),*
                    })
                }
                #(#builder_func_impls)*
            }

            impl #struct_name {
                pub fn builder() -> #builder_struct_name {
                    Default::default()
                }
            }
    )
    .into();

    eprintln!("Generated: {}", result);
    result
}

// Inject into the impl of the struct
//
